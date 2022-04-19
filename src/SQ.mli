
type 'a t

val create : unit -> 'a t

val push : 'a t -> 'a -> unit

val take_block : 'a t -> 'a

val try_take : 'a t -> 'a option

val peek : 'a t -> 'a option
