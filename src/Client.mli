
(** Main Kernel Loop *)

(** {2 Prelude} *)

type 'a or_error = ('a, string) result
type 'a ivar = ('a, string) Ivar.t

type t

val make : ?key:string -> Sockets.t -> Kernel.t -> t

val trigger_restart : unit -> 'a
(** Raise an exception to ask Jupyter to restart the kernel *)

type run_result =
  | Run_stop
  | Run_restart
  | Run_fail of exn

val run : t -> run_result
