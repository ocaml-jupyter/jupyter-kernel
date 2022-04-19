
(** A set-once synchronous variable. *)

type ('a, 'err) t
type ('a, 'err) resolver

val on_done :
  ('a, 'err) t ->
  (('a, 'err) result -> unit) -> unit

val is_done : _ t -> bool

val create : unit -> ('a, 'err) t * ('a, 'err) resolver

val return : 'a -> ('a, _) t

val fail : 'err -> (_,'err) t

val resolve_ok : ('a, _) resolver -> 'a -> unit
(** @raise Invalid_arg if already set *)

val resolve_err : (_, 'err) resolver -> 'err -> unit
(** @raise Invalid_arg if already set *)
