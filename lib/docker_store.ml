open Lwt.Syntax

(* Represents a persistent cache.
   You must hold a cache's lock when removing or updating its entry in
   "cache", and must assume this may happen at any time when not holding it.
   The generation counter is used to check whether the cache has been updated
   since being cloned. The counter starts from zero when the in-memory cache
   value is created (i.e. you cannot compare across restarts). *)
type cache = {
  lock : Lwt_mutex.t;
  mutable gen : int;
}

type t = {
  root : string;        (* The top-level directory (containing `result`, etc). *)
  caches : (string, cache) Hashtbl.t;
  mutable next : int;   (* Used to generate unique temporary IDs. *)
}

let ( / ) = Filename.concat
let strf = Printf.sprintf

module Path = struct
  (* A btrfs store contains several subdirectories:

     - result: completed builds, named by ID
     - result-tmp: in-progress builds
     - state: for sqlite DB, etc

     result-tmp is wiped at start-up. *)

  let result t id        = t.root / "result" / id
  let result_tmp t id    = t.root / "result-tmp" / id
  let state t            = t.root / "state"
end

(* The OBuilder persistent cache is implemented using a shared Docker
   volume. As there's no snapshotting in volumes, we implement
   poor-man's snapshots: take a lock and copy the source. If the build
   of the new cache entry succeeds, it replaces the old one.

   For security reasons, each build step should only have access to
   its cache, so we need one volume per cache entry. The copy happens
   in the host filesystem. *)
module Cache : sig
  val empty : t -> string
  val cache : string -> [> `Docker_volume of string]
  val cache_tmp : int -> string -> [> `Docker_volume of string]

  val name : [< `Docker_volume of string] -> string

  val exists : [< `Docker_volume of string] -> bool Lwt.t
  val create : [< `Docker_volume of string] -> unit Lwt.t
  val snapshot : src:[< `Docker_volume of string] -> [< `Docker_volume of string] -> unit Lwt.t
  val delete : [`Docker_volume of string] -> unit Lwt.t
end = struct
  let empty t = t.root / "empty"
  let cache name = Docker.docker_volume_cache (Escape.cache name)
  let cache_tmp i name = Docker.docker_volume_cache ~tmp:true (strf "%d-%s" i (Escape.cache name))

  let name (`Docker_volume name) = name

  let exists volume =
    let* r = Docker.exists volume in
    let b = Result.is_ok r in
    Lwt.return b

  let create volume =
    let* _ = Docker.volume (`Create volume) in
    Lwt.return_unit

  let snapshot ~src dst =
    let* () = create dst in
    let* src = Docker.mount_point src in
    let* dst = Docker.mount_point dst in
    Os.copy ~superuser:true ~src dst

  let delete volume =
    let* _ = Docker.volume (`Remove [volume]) in
    Lwt.return_unit
end

let purge path =
  Sys.readdir path |> Array.to_list |> Lwt_list.iter_s (fun item ->
      let item = path / item in
      Log.warn (fun f -> f "Removing left-over temporary item %S" item);
      Os.delete_recursively item
    )

let create ?(clean=false) root =
  let* () =
    if clean && Os.check_dir root = `Present then
      Os.delete_recursively root
    else Lwt.return_unit in
  Os.ensure_dir root;
  let hash = Unix.realpath root |> Sha256.string |> Sha256.to_hex in
  let hash = String.sub hash 0 7 in
  Docker.set_prefix (strf "obuilder-%s" hash);
  let t = { root; caches = Hashtbl.create 10; next = 0 } in
  Os.ensure_dir (Cache.empty t);
  Os.ensure_dir (root / "result");
  Os.ensure_dir (root / "result-tmp");
  Os.ensure_dir (root / "state");
  let* () = purge (root / "result-tmp") in
  (* Left-over caches (Docker volumes) are cleaned in {!Docker_sandbox.clean_docker}. *)
  Lwt.return t

let build t ?base ~id (fn:(string -> (unit, 'e) Lwt_result.t)) : (unit, 'e) Lwt_result.t =
  let result = Path.result t id in
  let result_tmp = Path.result_tmp t id in
  assert (not (Sys.file_exists result));
  Os.ensure_dir result_tmp;
  match base with
  | None ->
    Lwt.try_bind
      (fun () -> fn result_tmp)
      (fun r ->
         let* () = match r with
           | Ok () -> Lwt_unix.rename result_tmp result
           | Error _ -> Os.delete_recursively result_tmp
         in
         Lwt.return r)
      (fun exn ->
         Log.warn (fun f -> f "Uncaught exception from %S build function: %a" id Fmt.exn exn);
         let* () = Os.delete_recursively result_tmp in
         Lwt.fail exn)
  | Some base ->
    let base = Docker.docker_image base in
    let tmp_image = (Docker.docker_image ~tmp:true id) in
    let* () = Docker.tag base tmp_image in
    Lwt.try_bind
      (fun () -> fn result_tmp)
      (fun r ->
         let* () = match r with
           | Ok () -> Lwt_unix.rename result_tmp result
           | Error _ -> Os.delete_recursively result_tmp
         in
         (* As the cache is cleaned before this, the sandbox must take
            care of committing the container and removing it, otherwise
            the container still has a reference to the volume. *)
         let* () = Docker.image (`Remove tmp_image) in
         Lwt.return r)
      (fun exn ->
         Log.warn (fun f -> f "Uncaught exception from %S build function: %a" id Fmt.exn exn);
         let* () = Os.delete_recursively result_tmp in
         let* () = Docker.image (`Remove tmp_image) in
         Lwt.fail exn)

let delete t id =
  let path = Path.result t id in
  let* () = match Os.check_dir path with
    | `Present -> Os.delete_recursively path
    | `Missing -> Lwt.return_unit
  in
  let image = Docker.docker_image id in
  let* exists = Docker.exists image in
  match exists with
  | Ok () -> Docker.image (`Remove image)
  | Error _ -> Lwt.return_unit

let result t id =
  let dir = Path.result t id in
  let img = Docker.docker_image id in
  let* r = Docker.exists img in
  (* We want both the Docker image and the directory. If only one of
     them is present (should not happen), clean it. *)
  match r, Os.check_dir dir with
  | Ok _, `Present  -> Lwt.return_some dir
  | Error _, `Present ->
    let* () = Os.delete_recursively dir in
    Lwt.return_none
  | Ok _, `Missing ->
    let* () = Docker.rmi [img] in
    Lwt.return_none
  | _ -> Lwt.return_none

let state_dir = Path.state

let get_cache t name =
  match Hashtbl.find_opt t.caches name with
  | Some c -> c
  | None ->
    let c = { lock = Lwt_mutex.create (); gen = 0 } in
    Hashtbl.add t.caches name c;
    c

let cache ~user t name : (string * (unit -> unit Lwt.t)) Lwt.t =
  let cache = get_cache t name in
  Lwt_mutex.with_lock cache.lock @@ fun () ->
  let tmp = Cache.cache_tmp t.next name in
  t.next <- t.next + 1;
  let snapshot = Cache.cache name in
  (* Create cache if it doesn't already exist. *)
  let* () =
    let* exists = Cache.exists snapshot in
    if not exists then Cache.create snapshot
    else Lwt.return_unit
  in
  (* Create writeable clone. *)
  let gen = cache.gen in
  let* () = Cache.snapshot ~src:snapshot tmp in
  let* () = match user with
    | `Unix { Obuilder_spec.uid; gid } ->
      let* tmp = Docker.mount_point tmp in
      Os.sudo ["chown"; strf "%d:%d" uid gid; tmp]
    | `Windows _ -> Lwt.return_unit (* FIXME: does Windows need special treatment? *)
  in
  let release () =
    Lwt_mutex.with_lock cache.lock @@ fun () ->
    let* () =
      if cache.gen = gen then (
        (* The cache hasn't changed since we cloned it. Update it. *)
        (* todo: check if it has actually changed. *)
        cache.gen <- cache.gen + 1;
        let* () = Cache.delete snapshot in
        Cache.snapshot ~src:tmp snapshot
      ) else Lwt.return_unit
    in
    Cache.delete tmp
  in
  Lwt.return (Cache.name tmp, release)

let delete_cache t name =
  let cache = get_cache t name in
  Lwt_mutex.with_lock cache.lock @@ fun () ->
  cache.gen <- cache.gen + 1;   (* Ensures in-progress writes will be discarded *)
  let snapshot = Cache.cache name in
  let* exists = Cache.exists snapshot in
  if exists then
    let* () = Cache.delete snapshot in
    Lwt_result.return ()
  else Lwt_result.return ()

let complete_deletes t =
  ignore t;
  (* FIXME: how to implement this? *)
  Lwt.return_unit
