type 'a t

type 'a node

val create : 'a -> 'a t

val insert_after : 'a node -> 'a node -> bool

val insert_before : 'a node -> 'a node -> bool

val delete : 'a node -> bool

val get_next : 'a node -> 'a node option

val get_prev : 'a node -> 'a node option

val get_head : 'a t -> 'a node

val get_tail : 'a t -> 'a node

val get_val : 'a node -> 'a