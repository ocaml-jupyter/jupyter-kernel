
(* This file is free software. See file "license" for more details. *)

val main :
  ?args:(Arg.key * Arg.spec * Arg.doc) list ->
  usage:string ->
  kernel_init:(unit -> Client.Kernel.t Lwt.t) ->
  unit Lwt.t
(** [main ~usage kernel_init] will parse command line arguments, open a connection
    using {!Sockets}, and call the [init_kernel] function and run the returned kernel.

    - A connection file can be passed using [--connection-file <file>];
    - A log file through [--log <file>];
    - Individual connection parameters with [--ci-<foo> <bar>];
    - See [--help] for more details;
    - The parameter [args] can contain additional command line arguments.
*)

