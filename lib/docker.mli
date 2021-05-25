(** Docker interface over the CLI tool  *)

type ids = [
  | `Docker_container of string | `Docker_image of string
  | `Docker_volume of string
  | `Obuilder_id of string
]

val set_prefix : string -> unit
(** Set the prefix for Docker images, containers, and volumes managed
    by the current OBuilder instance. *)

val obuilder_volume : unit -> string
val image_name : ?tmp:bool -> S.id -> string
val container_name : S.id -> string
val volume_copy_name : ?tmp:bool -> S.id -> string

val docker_image : ?tmp:bool -> S.id -> [> `Docker_image of string ]
val docker_container : S.id -> [> `Docker_container of string ]
val docker_volume_cache : ?tmp:bool -> S.id -> [> `Docker_volume of string ]
val docker_volume_copy : ?tmp:bool -> S.id -> [> `Docker_volume of string ]

val mount_point_inside_unix : string
(** Mount point of Docker volumes inside Docker containers, Unix path
    style. Use with Cygwin tools. *)

val mount_point_inside_native : string
(** Mount point of Docker volumes inside Docker containers, native
    path style. *)

val bash_entrypoint : string -> string list
(** Get a Bash entrypoint in a Docker container to execute Bash
    scripts. *)

val default_entrypoint : string list
(** Get the default entrypoint of Docker container according to the
    host (Windows is cmd, everywhere else is sh). *)

val setup_command : entp:string list -> cmd:string list -> string * string list
(** [setup_command ~entp ~cmd] returns the head of [entp], to be
    give to Docker's [--entrypoint], and the concatenation of the tail
    of [head] and [cmd] to be given to Docker command, as Docker
    [--entrypoint] takes only one argument. *)

val pull : ?stderr:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
  [< `Docker_image of string ] -> unit Lwt.t
val export : ?stdout:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
  [< `Docker_container of string ] -> unit Lwt.t
val image : ?stdout:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
  [< `Remove of [< `Docker_image of string ] ] -> unit Lwt.t
val rm : ?stdout:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
  [ `Docker_container of string ] list -> unit Lwt.t
val rmi : ?stdout:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
  [ `Docker_image of string ] list -> unit Lwt.t
val tag : ?stdout:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
  ?stderr:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
  [< `Docker_image of string ] -> [< `Docker_image of string ] ->
  unit Lwt.t
val commit : ?stdout:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
  [< `Docker_image of string ] ->
  [< `Docker_container of string ] ->
  [< `Docker_image of string ] ->
  unit Lwt.t
val volume :
  [< `Create of [< `Docker_volume of string ]
  | `Inspect of [< `Docker_volume of string ] list * [< `Mountpoint ]
  | `List of string option
  | `Remove of [< `Docker_volume of string ] list ] ->
  string Lwt.t
val mount_point : [< `Docker_volume of string ] -> string Lwt.t
val build : string list -> [< `Docker_image of string ] -> string -> unit Lwt.t
val run : ?stdin:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
  ?stdout:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
  ?stderr:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
  ?is_success:(int -> bool) ->
  ?name:[< `Docker_container of string ] ->
  ?rm:bool ->
  string list ->
  [< `Docker_image of string ] ->
  string list ->
  unit Lwt.t
val run_result : ?stdin:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
  ?stdout:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
  ?stderr:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
  ?name:[< `Docker_container of string ] ->
  ?rm:bool ->
  string list ->
  [< `Docker_image of string ] ->
  string list ->
  (unit, [> `Msg of string ]) result Lwt.t
val run_pread_result : ?stderr:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
  ?name:[< `Docker_container of string ] ->
  ?rm:bool ->
  string list ->
  [< `Docker_image of string ] ->
  string list ->
  (string, [> `Msg of string ]) result Lwt.t
val stop : [< `Docker_container of string ] ->
  (unit, [> `Msg of string ]) result Lwt.t

val manifest :?stdout:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
  ?stderr:[ `Dev_null | `FD_move_safely of Os.unix_fd ] ->
  [< `Create of [< `Docker_image of string ] * [< `Docker_image of string ] list
  | `Inspect of [< `Docker_image of string ]
  | `Remove of [< `Docker_image of string ] list ] ->
  (unit, [> `Msg of string ]) result Lwt.t

val exists : [< `Docker_container of string | `Docker_image of string
             | `Docker_volume of string ] ->
  (unit, [> `Msg of string ]) result Lwt.t

val obuilder_images : unit -> [ `Docker_image of string ] list Lwt.t
val obuilder_containers : unit -> [ `Docker_container of string ] list Lwt.t
val obuilder_volumes : ?prefix:string -> unit -> [ `Docker_volume of string ] list Lwt.t
val obuilder_caches_tmp : unit -> [ `Docker_volume of string ] list Lwt.t

(** Fetch (pull and extract) base images using Docker *)
module Extract : sig
  include S.FETCHER
end
