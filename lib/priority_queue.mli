type 'a t

val create : int -> 'a -> 'a t
val add : 'a t -> 'a -> int -> unit
val remove_min : 'a t -> 'a
val is_empty : 'a t -> bool
val get_len : 'a t -> int
val get_repeat : 'a t -> int