type 'a node = { value : 'a; mutable next : 'a node option; mutable prev : 'a node option }
type 'a t = { head : 'a node; tail : 'a node }

let create_node a b c = {value = a; next = b; prev = c}
let create key = 
  let temp = { head = create_node key None None; tail = create_node key None None} in 
  temp.head.next <- Some temp.tail;
  temp.tail.prev <- Some temp.head;
  temp
  
let get_next cur_node = 
  cur_node.next

let get_prev cur_node = 
  cur_node.prev

(* no locking required since immutable *)  
let get_head t =
  t.head

(* no locking required since immutable *)  
let get_tail t =
  t.tail

(* fetch value of current node, needs locking *)
let get_val cur_node = 
  cur_node.value

(* delete current node from linked list, lock before, cur_node & after *)  
let delete cur_node =
  let before = Option.get cur_node.prev in 
  let after = Option.get cur_node.next in 
  before.next <- Some after;
  after.prev <- Some before;
  true

(* insert after current node *)
let insert_after cur_node new_key =
  let after = Option.get cur_node.next in 
  let new_node = create_node new_key (Some after) (Some cur_node) in
  after.prev <- Some new_node;
  cur_node.next <- Some new_node;
  true

(* insert before current node *)
let insert_before cur_node new_key =
  let before = Option.get cur_node.prev in 
  let new_node = create_node new_key (Some cur_node) (Some before) in
  before.next <- Some new_node;
  cur_node.prev <- Some new_node;
  true
