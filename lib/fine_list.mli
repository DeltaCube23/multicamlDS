type 'a t

val create : unit -> 'a t
val add : 'a t -> int -> bool
val remove : 'a t -> int -> bool
val is_empty : 'a t -> bool
