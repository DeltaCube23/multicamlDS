(* type 'a priority_queue = {
     mutable heap : 'a array;
     mutable size : int;
     compare : 'a -> 'a -> int;
     lock : Mutex.t;
     nonempty : Condition.t;
   }

   let create compare =
     {
       heap = Array.make 10 (Obj.magic ());
       size = 0;
       compare;
       lock = Mutex.create ();
       nonempty = Condition.create ();
     }

   let resize pq =
     let new_heap = Array.make (pq.size * 2) (Obj.magic ()) in
     Array.blit pq.heap 0 new_heap 0 pq.size;
     pq.heap <- new_heap

   let enqueue pq value =
     Mutex.lock pq.lock;
     if pq.size >= Array.length pq.heap then resize pq;
     let hole = ref pq.size in
     pq.size <- pq.size + 1;
     while !hole > 0 do
       let parent = (!hole - 1) / 2 in
       if pq.compare pq.heap.(parent) value <= 0 then hole := !hole - 1
       else (
         pq.heap.(!hole) <- pq.heap.(parent);
         hole := parent)
     done;
     pq.heap.(!hole) <- value;
     Condition.signal pq.nonempty;
     Mutex.unlock pq.lock

   let dequeue pq =
     Mutex.lock pq.lock;
     while pq.size = 0 do
       Condition.wait pq.nonempty pq.lock
     done;
     pq.size <- pq.size - 1;
     let min = pq.heap.(0) in
     let value = pq.heap.(pq.size) in
     pq.heap.(pq.size) <- Obj.magic ();
     let hole = ref 0 in
     while (!hole * 2) + 1 < pq.size do
       let child1 = (!hole * 2) + 1 in
       let child2 = (!hole * 2) + 2 in
       let smaller_child =
         if child2 < pq.size && pq.compare pq.heap.(child2) pq.heap.(child1) < 0
         then child2
         else child1
       in
       if pq.compare pq.heap.(smaller_child) value < 0 then (
         pq.heap.(!hole) <- pq.heap.(smaller_child);
         hole := smaller_child)
       else hole := (!hole * 2) + 1
     done;
     pq.heap.(!hole) <- value;
     Mutex.unlock pq.lock;
     min

   let is_empty pq =
     Mutex.lock pq.lock;
     let result = pq.size = 0 in
     Mutex.unlock pq.lock;
     result
*)
