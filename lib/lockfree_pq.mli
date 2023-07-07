type t

val create : ?max_height:int -> unit -> t
val push : t -> int -> unit 
val pop : t -> int
val contains : t -> int -> bool