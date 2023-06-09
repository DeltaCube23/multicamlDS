type 'a node = {
  mutable status : int; (* EMPTY | AVAILABLE | BUSY *)
  mutable owner : Domain.id option;
  key : int;
  item : 'a;
}

type 'a t = {
  heap_lock : Mutex.t;
  heap : 'a node array;
  fine_lock : Mutex.t array;
  capacity : int;
  next : Brc.t;
}

let create_node it k = { status = 0; owner = None; key = k; item = it }

let create num def_it =
  {
    heap_lock = Mutex.create ();
    heap = Array.init num (fun _ -> create_node def_it 42);
    fine_lock = Array.init num (fun _ -> Mutex.create ());
    capacity = num;
    next = Brc.create ();
  }

(* add node to the first empty slot and then traverse up to reach correct position *)
let add t item key =
  Mutex.lock t.heap_lock;
  let idx = Brc.increment t.next in
  match idx with
  | x when x = t.capacity ->
    Mutex.unlock t.heap_lock;
    Domain.cpu_relax ()
  | x ->
    let child = ref x in
    Mutex.lock t.fine_lock.(!child);
    t.heap.(!child) <- create_node item key;
    t.heap.(!child).status <- 2;
    t.heap.(!child).owner <- Some (Domain.self ());
    Mutex.unlock t.heap_lock;
    Mutex.unlock t.fine_lock.(!child);
    while !child > 1 do
      let par = !child / 2 in
      Mutex.lock t.fine_lock.(par);
      Mutex.lock t.fine_lock.(!child);
      let oldchild = !child in
      let par_node = t.heap.(par) in
      let child_node = t.heap.(!child) in
      if
        par_node.status = 1 && child_node.status = 2
        && child_node.owner = Some (Domain.self ())
      then
        if child_node.key < par_node.key then (
          t.heap.(par) <- child_node;
          t.heap.(!child) <- par_node;
          child := par)
        else (
          child_node.status <- 1;
          child_node.owner <- None; 
          child := 0 (* to simulate early return *))
      else if
        not (child_node.status = 2 && child_node.owner = Some (Domain.self ()) )
      then child := par
      else if par_node.status = 0 then child := 0;
      Mutex.unlock t.fine_lock.(oldchild);
      Mutex.unlock t.fine_lock.(par);
      if !child = oldchild then Domain.cpu_relax ();
    done;

    if !child = 1 then (
      Mutex.lock t.fine_lock.(1);
      let root = t.heap.(1) in
      if root.status = 2 && root.owner = Some (Domain.self ()) then (
        root.status <- 1;
        root.owner <- None );
      Mutex.unlock t.fine_lock.(1))

(* delete root node and swap with last node, then traverse down to reach correct position *)
let remove_min t =
  Mutex.lock t.heap_lock;
  let bottom = Brc.get_idx t.next in
  match bottom with
  | 0 -> (* return dummy node 42 for timebeing to pass STM tests, check if queue not empty before calling *) 
    Mutex.unlock t.heap_lock; 
    t.heap.(0).item
  | 1 ->
      (* Printf.printf "=1 case \n%!"; *)
      ignore @@ Brc.decrement t.next;
      Mutex.lock t.fine_lock.(1);
      Mutex.unlock t.heap_lock;
      let it = t.heap.(1).item in
      t.heap.(1).status <- 0;
      t.heap.(1).owner <- None;
      Mutex.unlock t.fine_lock.(1);
      it
  | _ ->
      (* Printf.printf ">1 case \n%!"; *)
      ignore @@ Brc.decrement t.next;
      Mutex.lock t.fine_lock.(bottom);
      Mutex.lock t.fine_lock.(1);
      Mutex.unlock t.heap_lock;
      let it = t.heap.(1).item in
      t.heap.(1).status <- 0;
      t.heap.(1).owner <- None;
      t.heap.(bottom).status <- 1;
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
        if t.heap.(left).status = 0 then (
          Mutex.unlock t.fine_lock.(right);
          Mutex.unlock t.fine_lock.(left);
          break := true)
        else if
          t.heap.(right).status = 0 || t.heap.(left).key < t.heap.(right).key
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
  let res = (Brc.get_size t.next) = 0 in
  Mutex.unlock t.heap_lock;
  res

let get_len t =
  Mutex.lock t.heap_lock;
  let res = Brc.get_size t.next in
  Mutex.unlock t.heap_lock;
  res
