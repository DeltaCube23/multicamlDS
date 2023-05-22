(*
  structure node t fvalue: data type, next: pointer to node tg
  structure queue t fHead: pointer to node t, Tail: pointer to node t, H lock: lock type, T lock: lock type
*)

type 'a node = Nil | Next of 'a option * 'a node Atomic.t

type 'a t = {
  mutable head : 'a node Atomic.t;
  mutable tail : 'a node Atomic.t;
  head_lock : Mutex.t;
  tail_lock : Mutex.t;
}

(* create and initialize the 2 lock queue *)
let init () =
  let dummy = Atomic.make Nil in
  {
    head = dummy;
    tail = dummy;
    head_lock = Mutex.create ();
    tail_lock = Mutex.create ();
  }

(* push value to the tail of queue *)
let push t value =
  let new_tail = Atomic.make Nil in
  let new_node = Next (Some value, new_tail) in
  Mutex.lock t.tail_lock;
  Atomic.set t.tail new_node;
  t.tail <- new_tail;
  Mutex.unlock t.tail_lock

(* pop from head of queue, return next value since first one is dummy node *)
let pop t =
  Mutex.lock t.head_lock;
  let popped =
    match Atomic.get t.head with
    | Nil -> None
    | Next (value, next) ->
        t.head <- next;
        value
  in
  Mutex.unlock t.head_lock;
  popped

(* check if q is empty if the node head is pointing to is Nil *)
let is_empty t =
  Mutex.lock t.head_lock;
  let empty = Atomic.get t.head = Nil in
  Mutex.unlock t.head_lock;
  empty

(* return element at head of queue *)
let peek t =
  Mutex.lock t.head_lock;
  let top =
    match Atomic.get t.head with Nil -> None | Next (value, _) -> value
  in
  Mutex.unlock t.head_lock;
  top
