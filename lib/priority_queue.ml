(* keep everything mutable to make it easy to swap *)
type 'a node = {
  status : int Atomic.t; (* EMPTY | AVAILABLE | BUSY *)
  mutable owner : Domain.id option;
  key : int;
  item : 'a;
}

type 'a t = {
  heap_lock : Mutex.t;
  heap : 'a node array;
  fine_lock : Mutex.t array;
  capacity : int;
  next : int ref;
}

let create_node it k = { status = Atomic.make 0; owner = None; key = k; item = it }

let create num def_it =
  {
    heap_lock = Mutex.create ();
    heap = Array.init num (fun _ -> create_node def_it 42);
    fine_lock = Array.init num (fun _ -> Mutex.create ());
    capacity = num;
    next = ref 1;
  }

(* add node to the first empty slot and then traverse up to reach correct position *)
let add t item key =
  (* Printf.printf "in add func \n%!"; *)
  Mutex.lock t.heap_lock;
  let child = ref !(t.next) in
  incr t.next;
  Mutex.lock t.fine_lock.(!child);
  t.heap.(!child) <- create_node item key;
  Atomic.set t.heap.(!child).status 2;
  t.heap.(!child).owner <- Some (Domain.self ());
  Mutex.unlock t.heap_lock;
  Mutex.unlock t.fine_lock.(!child);
  while !child > 1 do
    let par = !child / 2 in
    Mutex.lock t.fine_lock.(par);
    Mutex.lock t.fine_lock.(!child);
    (* Printf.printf "Locked\n%!"; *)
    let oldchild = !child in
    let par_node = t.heap.(par) in
    let child_node = t.heap.(!child) in
    if
      (Atomic.get par_node.status) = 1 && (Atomic.get child_node.status) = 2
      && child_node.owner = Some (Domain.self ())
    then
      if child_node.key < par_node.key then (
        t.heap.(par) <- child_node;
        t.heap.(!child) <- par_node;
        child := par)
      else (
        Atomic.set child_node.status 1;
        child_node.owner <- None;
        child := 0 (* to simulate early return *))
    else if
      not ((Atomic.get child_node.status) = 2 && child_node.owner = Some (Domain.self ()))
    then child := par
    else if (Atomic.get par_node.status) = 0 then child := 0;
    Mutex.unlock t.fine_lock.(oldchild);
    Mutex.unlock t.fine_lock.(par);
    (* Printf.printf "UnLocked\n%!"; *)
  done;

  if !child = 1 then (
    Mutex.lock t.fine_lock.(1);
    let root = t.heap.(1) in
    if (Atomic.get root.status) = 2 && root.owner = Some (Domain.self ()) then (
      Atomic.set root.status 1;
      root.owner <- None);
    Mutex.unlock t.fine_lock.(1))

(* delete root node and swap with last node, then traverse down to reach correct position *)
let remove_min t =
  (* Printf.printf "in remove func \n%!"; *)
  Mutex.lock t.heap_lock;
  decr t.next;
  let bottom = !(t.next) in
  match bottom with
  | 0 -> assert false
  | 1 ->
      (* Printf.printf "=1 case \n%!"; *)
      Mutex.lock t.fine_lock.(1);
      Mutex.unlock t.heap_lock;
      let it = t.heap.(1).item in
      Atomic.set t.heap.(1).status 0;
      t.heap.(1).owner <- None;
      Mutex.unlock t.fine_lock.(1);
      it
  | _ ->
      (* Printf.printf ">1 case \n%!"; *)
      Mutex.lock t.fine_lock.(bottom);
      Mutex.lock t.fine_lock.(1);
      Mutex.unlock t.heap_lock;
      let it = t.heap.(1).item in
      Atomic.set t.heap.(1).status 0;
      t.heap.(1).owner <- None;
      Atomic.set t.heap.(bottom).status 1;
      t.heap.(bottom).owner <- None;
      let root = t.heap.(1) in
      t.heap.(1) <- t.heap.(bottom);
      t.heap.(bottom) <- root;
      Mutex.unlock t.fine_lock.(bottom);
      let child = ref 0 in
      let par = ref 1 in
      let break = ref false in
      (* simulate breaking out of loop *)
      while !par < t.capacity / 2 && not !break do
        let left = !par * 2 in
        let right = left + 1 in
        Mutex.lock t.fine_lock.(left);
        Mutex.lock t.fine_lock.(right);
        if (Atomic.get t.heap.(left).status) = 0 then (
          Mutex.unlock t.fine_lock.(right);
          Mutex.unlock t.fine_lock.(left);
          break := true)
        else if
          (Atomic.get t.heap.(right).status) = 0 || t.heap.(left).key < t.heap.(right).key
        then (
          Mutex.unlock t.fine_lock.(right);
          child := left)
        else (
          Mutex.unlock t.fine_lock.(left);
          child := right);
        if not !break then
          let child_node = t.heap.(!child) in
          let par_node = t.heap.(!par) in
          if child_node.key < par_node.key then (
            t.heap.(!child) <- par_node;
            t.heap.(!par) <- child_node;
            Mutex.unlock t.fine_lock.(!par);
            par := !child)
          else (
            Mutex.unlock t.fine_lock.(!child);
            break := true)
      done;
      Mutex.unlock t.fine_lock.(!par);
      it

let is_empty t =
  Mutex.lock t.heap_lock;
  let res = !(t.next) = 1 in
  Mutex.unlock t.heap_lock;
  res

let get_len t =
  Mutex.lock t.heap_lock;
  let res = !(t.next) in
  Mutex.unlock t.heap_lock;
  res - 1
