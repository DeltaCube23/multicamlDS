type 'a node = { value : 'a; mutable next : 'a node option; mutable prev : 'a node option; lock : Mutex.t; mutable active : bool }
type 'a t = { head : 'a node; tail : 'a node }
(* all locks are acquired from left to right *)

let create_node a b c = {value = a; next = b; prev = c; lock = Mutex.create (); active = true}
let create key = 
  let temp = { head = create_node key None None; tail = create_node key None None} in 
  temp.head.next <- Some temp.tail;
  temp.tail.prev <- Some temp.head;
  temp

(* get next of node if not tail *)  
let get_next cur_node = 
  Mutex.lock cur_node.lock;
  let res = Option.get cur_node.next in 
  Mutex.unlock cur_node.lock; 
  res

(* get prev of node if not head *)  
let get_prev cur_node = 
  Mutex.lock cur_node.lock;
  let res = Option.get cur_node.prev in 
  Mutex.unlock cur_node.lock; 
  res

(* no locking required since immutable *)  
let get_head t =
  t.head

(* no locking required since immutable *)  
let get_tail t =
  t.tail

(* fetch value of current node, needs locking *)
let get_val cur_node = 
  Mutex.lock cur_node.lock;
  let res = cur_node.value in 
  Mutex.unlock cur_node.lock; 
  res

(* delete current node from linked list, lock before, cur_node & after *)  
let delete cur_node =
  let remove x y =
    x.next <- Some y;
    y.prev <- Some x;
  in
  let rec validate before after =
    Mutex.lock before.lock;
    Mutex.lock cur_node.lock;
    Mutex.lock after.lock;
    (* cur_node already deleted case *)
    if cur_node.active = false then (
      Mutex.lock before.lock;
      Mutex.lock cur_node.lock;
      Mutex.lock after.lock;
      false)
    else if before == (Option.get cur_node.prev) && after == (Option.get cur_node.next) then (
      (* valid position, delete here *)
      remove before after;
      cur_node.active <- false;
      Mutex.unlock before.lock;
      Mutex.unlock cur_node.lock;
      Mutex.unlock after.lock;
      true)
    else ( (* retry with updated data *)
      Mutex.unlock before.lock;
      Mutex.unlock cur_node.lock;
      Mutex.unlock after.lock;
      let new_bf = Option.get cur_node.prev in 
      let new_af = Option.get cur_node.next in
      validate new_bf new_af)
  in
  let bf = Option.get cur_node.prev in 
  let af = Option.get cur_node.next in 
  validate bf af

(* insert after current node, not allowed for tail *)
let insert_after cur_node new_node =
  let insert x y = 
    new_node.prev <- Some x;
    new_node.next <- Some y;
    x.next <- Some new_node;
    y.prev <- Some new_node;
  in
  let rec validate after = 
    Mutex.lock cur_node.lock;
    Mutex.lock after.lock;
    (* cur_node already deleted case *)
    if cur_node.active = false then (
      Mutex.unlock cur_node.lock;
      Mutex.unlock after.lock;
      false)
    else if after == (Option.get cur_node.next) then (
      (* valid position, insert here *)
      insert cur_node after;
      Mutex.unlock cur_node.lock;
      Mutex.unlock after.lock;
      true)
    else ( (* retry with updated data *)
      Mutex.unlock cur_node.lock; 
      Mutex.unlock after.lock;
      let new_af = Option.get cur_node.next in 
      validate new_af)
  in
  let af = Option.get cur_node.next in 
  validate af

(* insert before current node, not allowed for head *)
let insert_before cur_node new_node =
  let insert x y = 
    new_node.prev <- Some x;
    new_node.next <- Some y;
    x.next <- Some new_node;
    y.prev <- Some new_node;
  in
  let rec validate before = 
    Mutex.lock before.lock;
    Mutex.lock cur_node.lock;
    (* cur_node already deleted case *)
    if cur_node.active = false then (
      Mutex.unlock before.lock;
      Mutex.unlock cur_node.lock;
      false)
    else if before == (Option.get cur_node.prev) then (
      (* valid position, insert here *)
      insert before cur_node;
      Mutex.unlock before.lock;
      Mutex.unlock cur_node.lock;
      true)
    else ( (* retry with updated data *) 
      Mutex.unlock before.lock;
      Mutex.unlock cur_node.lock; 
      let new_bf = Option.get cur_node.prev in 
      validate new_bf)
  in
  let bf = Option.get cur_node.prev in 
  validate bf
