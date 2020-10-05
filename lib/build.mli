module Context : sig
  type t

  val v :
    ?switch:Lwt_switch.t ->
    ?env:Os.env ->
    ?user:Obuilder_spec.user ->
    ?workdir:string ->
    ?shell:string list ->
    log:S.logger ->
    src_dir:string ->
    unit -> t
  (** [context ~log ~src_dir] is a build context where copy operations read from the (host) directory [src_dir].
      @param switch Turn this off to cancel the build.
      @param env Environment in which to run commands.
      @param user Container user to run as.
      @param workdir Directory in the container namespace for cwd.
      @param shell The command used to run shell commands (default [["/bin/bash"; "-c"]]).
      @param log Function to receive log data.
  *)
end

module Make (Store : S.STORE) (Sandbox : S.SANDBOX) : sig
  include S.BUILDER with type context := Context.t

  val v : store:Store.t -> sandbox:Sandbox.t -> t
end
