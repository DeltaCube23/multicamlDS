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

let get_head t =
  t.head

let get_tail t =
  t.tail

let get_val cur_node = 
  cur_node.value

let delete cur_node =
  let before = Option.get cur_node.prev in 
  let after = Option.get cur_node.next in 
  before.next <- Some after;
  after.prev <- Some before;
  true

(* insert after prev of current node *)
let insert_after cur_node new_node =
  let before = Option.get cur_node.prev in 
  before.next <- Some new_node;
  cur_node.prev <- Some new_node;
  true

(* insert before next of current node *)
let insert_before cur_node new_node =
  let after = Option.get cur_node.next in 
  after.prev <- Some new_node;
  cur_node.next <- Some new_node;
  true
