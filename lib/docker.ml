open Lwt.Syntax

type ids = [
  | `Docker_image of string | `Docker_container of string
  | `Docker_volume of string | `Obuilder_id of string
]

let prefix = ref "obuilder"
let set_prefix prefix' = prefix := prefix'

let image_prefix () = !prefix ^ "-image-"
let container_prefix () = !prefix ^ "-container-"
let cache_prefix () = !prefix ^ "-cache-"
let volume_prefix () = !prefix ^ "-copy-"

let obuilder_volume () = !prefix ^ "-volume"
let image_name ?(tmp=false) name = image_prefix () ^ (if tmp then "tmp-" else "") ^ name
let container_name name = container_prefix () ^ name
let volume_cache_name ?(tmp=false) name = cache_prefix () ^ (if tmp then "tmp-" else "") ^ name
let volume_copy_name ?(tmp=false) name = volume_prefix () ^ (if tmp then "tmp-" else "") ^ name

let docker_image ?(tmp=false) id = `Docker_image (image_name ~tmp id)
let docker_container id = `Docker_container (container_name id)
let docker_volume_cache ?(tmp=false) id = `Docker_volume (volume_cache_name ~tmp id)
let docker_volume_copy ?(tmp=false) id = `Docker_volume (volume_copy_name ~tmp id)

let ( / ) = Filename.concat
let mount_point_inside_unix = if Sys.win32 then "/cygdrive/c" else "/var/lib/obuilder"
let mount_point_inside_native = if Sys.win32 then {|C:/|} else mount_point_inside_unix

let bash_entrypoint obuilder_volume =
  [if Sys.win32 then mount_point_inside_native / obuilder_volume / "bash.exe" else "bash"; "-c"]

let default_entrypoint =
  if Sys.win32 then [{|C:\Windows\System32\cmd.exe|}; "/S"; "/C"]
  else ["/bin/sh"; "-c"]

let rec setup_command ~entp ~cmd =
  match entp with
  | hd :: tl -> hd, tl @ cmd
  | [] -> setup_command ~entp:default_entrypoint ~cmd

let extract_name = function `Docker_image name | `Docker_container name | `Docker_volume name -> name

let pread' ?stderr argv =
  Os.pread ?stderr  ("docker" :: argv)

let pread_result' ?stderr argv =
  let cmd = "docker" :: argv in
  let pp f = Os.pp_cmd f cmd in
  Os.pread_result ~pp ?stderr cmd

let exec' ?stdin ?stdout ?stderr ?is_success argv =
  Os.exec ?stdin ?stdout ?stderr ?is_success ("docker" :: argv)

let exec_result' ?stdin ?stdout ?stderr ?is_success argv =
  let cmd = "docker" :: argv in
  let pp f = Os.pp_cmd f cmd in
  Os.exec_result ?stdin ?stdout ?stderr ?is_success ~pp cmd

let create ?stderr (`Docker_image base) =
  pread' ?stderr ("create" :: ["--"; base])

let export ?stdout (`Docker_container id) =
  exec' ?stdout ["export"; "--"; id]

let image ?stdout (`Remove (`Docker_image id)) =
  exec' ?stdout ["image"; "rm"; id]

let rm ?stdout containers =
  exec' ?stdout ("rm" :: "--force" :: "--" :: (List.rev_map extract_name containers))

let tag ?stdout ?stderr (`Docker_image source) (`Docker_image target) =
  exec' ?stdout ?stderr ["tag"; source; target]

let commit ?stdout (`Docker_image base_image) (`Docker_container container) (`Docker_image target_image) =
  (* Restore CMD and ENTRYPOINT *)
  let* entrypoint = pread' ["inspect"; "--type=image"; "--format={{json .Config.Entrypoint }}"; "--"; base_image] in
  let* cmd = pread' ["inspect"; "--type=image"; "--format={{json .Config.Cmd }}"; "--"; base_image] in
  let entrypoint, cmd = String.trim entrypoint, String.trim cmd in
  let argv = [ "--"; container; target_image ] in
  let argv = if entrypoint = "null" then argv else ("--change=ENTRYPOINT " ^ entrypoint) :: argv in
  let argv = if cmd = "null" then argv else ("--change=CMD " ^ cmd) :: argv in
  exec' ?stdout ("commit" :: argv)

let pull ?stderr (`Docker_image base) =
  exec' ?stderr ["pull"; base]

let exists id =
  let argv = match id with
    | `Docker_container id -> ["inspect"; "--type=container"; "--"; id]
    | `Docker_image id -> ["inspect"; "--type=image"; "--"; id]
    | `Docker_volume id -> ["volume"; "inspect"; "--"; id]
  in
  exec_result' ~stdout:`Dev_null ~stderr:`Dev_null argv

let build docker_argv (`Docker_image image) context_path =
  exec' ("build" :: docker_argv @ ["-t"; image; context_path])

let run' ?stdin ?name ~rm ~docker_argv image argv =
  let docker_argv = if rm then "--rm" :: docker_argv else docker_argv in
  let docker_argv = match name with
    | Some (`Docker_container name) -> "--name" :: name :: docker_argv
    | None -> docker_argv in
  let docker_argv = match stdin with
    | Some (`FD_move_safely _) -> "-i" :: docker_argv
    | _ -> docker_argv in
  "run" :: docker_argv @ image :: argv

let run ?stdin ?stdout ?stderr ?is_success ?name ?(rm=false) docker_argv (`Docker_image image) argv =
  let argv = run' ?stdin ?name ~rm ~docker_argv image argv in
  exec' ?stdin ?stdout ?stderr ?is_success argv

let run_result ?stdin ?stdout ?stderr ?name ?(rm=false) docker_argv (`Docker_image image) argv =
  let argv = run' ?stdin ?name ~rm ~docker_argv image argv in
  exec_result' ?stdin ?stdout ?stderr argv

let run_pread_result ?stderr ?name ?(rm=false) docker_argv (`Docker_image image) argv =
  let argv = run' ?name ~rm ~docker_argv image argv in
  pread_result' ?stderr argv

let stop (`Docker_container name) =
  exec_result' ["stop"; name]

let volume = function
  | `Create (`Docker_volume name) -> pread' ("volume" :: "create" :: "--" :: name :: [])
  | `Inspect (volumes, `Mountpoint) ->
    let volumes = List.rev_map extract_name volumes in
    let format = "{{ .Mountpoint }}" in
    pread' ("volume" :: "inspect" :: "--format" :: format :: "--" :: volumes)
  | `List (filter) ->
    let filter = match filter with None -> [] | Some filter -> ["--filter"; filter] in
    pread' ("volume" :: "ls" :: "--quiet" :: filter)
  | `Remove volumes ->
    let volumes = List.rev_map extract_name volumes in
    pread' ("volume" :: "rm" :: "--" :: volumes)

let mount_point name =
  let* s = volume (`Inspect ([name], `Mountpoint)) in
  Lwt.return (String.trim s)

let rmi ?stdout images =
  exec' ?stdout ("rmi" :: (List.rev_map extract_name images))

let manifest ?stdout ?stderr = function
  | `Create (`Docker_image name, manifests) ->
    exec_result' ?stdout ?stderr ("manifest" :: "create" :: name :: (List.rev_map extract_name manifests))
  | `Inspect (`Docker_image name) ->
    exec_result' ?stdout ?stderr ["manifest"; "inspect"; name]
  | `Remove manifests ->
    exec_result' ?stdout ?stderr ("manifest" :: "rm" :: (List.rev_map extract_name manifests))

let obuilder_images () =
  let* images = pread' ["images"; "--format={{ .Repository }}"; !prefix ^ "*"] in
  String.split_on_char '\n' images
  |> List.filter_map (function "" -> None | id -> Some (`Docker_image id))
  |> Lwt.return

let obuilder_containers () =
  let* containers = pread' ["container"; "ls"; "--all"; "--filter"; "name=^" ^ !prefix; "-q"] in
  String.split_on_char '\n' containers
  |> List.filter_map (function "" -> None | id -> Some (`Docker_container id))
  |> Lwt.return

let obuilder_volumes ?(prefix=(!prefix)) () =
  let* volumes = volume (`List (Some ("name=^" ^ prefix))) in
  String.split_on_char '\n' volumes
  |> List.filter_map (function "" -> None | id -> Some (`Docker_volume id))
  |> Lwt.return

let obuilder_caches_tmp () =
  obuilder_volumes ~prefix:(cache_prefix () ^ "tmp-") ()

let with_container ~log base fn =
  let* cid = Os.with_pipe_from_child (fun ~r ~w ->
      (* We might need to do a pull here, so log the output to show progress. *)
      let copy = Build_log.copy ~src:r ~dst:log in
      let* cid = create ~stderr:(`FD_move_safely w) (`Docker_image base) in
      let+ () = copy in
      String.trim cid
    )
  in
  Lwt.finalize
    (fun () -> fn cid)
    (fun () -> rm ~stdout:`Dev_null [`Docker_container cid])

module Extract = struct
  let export_env base : Config.env Lwt.t =
    let+ env =
      pread' ["image"; "inspect";
              "--format"; {|{{range .Config.Env}}{{print . "\x00"}}{{end}}|};
              "--"; base] in
    String.split_on_char '\x00' env
    |> List.filter_map (function
        | "\n" -> None
        | kv ->
          match Astring.String.cut ~sep:"=" kv with
          | None -> Fmt.failwith "Invalid environment in Docker image %S (should be 'K=V')" kv
          | Some _ as pair -> pair
      )

  let fetch ~log ~rootfs base =
    let* () = with_container ~log base (fun cid ->
        Os.with_pipe_between_children @@ fun ~r ~w ->
        let exporter = export ~stdout:(`FD_move_safely w) (`Docker_container cid) in
        let tar = Os.sudo ~stdin:(`FD_move_safely r) ["tar"; "-C"; rootfs; "-xf"; "-"] in
        let* () = exporter in
        tar
      )
    in
    export_env base
end
