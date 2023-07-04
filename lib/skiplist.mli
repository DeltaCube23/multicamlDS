type t

val create : ?max_height:int -> unit -> t
val contains : t -> int -> bool
val add : t -> int -> bool
val remove : t -> int -> bool
val find_mark_min : t -> int
