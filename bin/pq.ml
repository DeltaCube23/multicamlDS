open MulticamlDS;;

Random.self_init ();
  let len = Random.int 10_000 in
  let plen = 2*len in
  (* Creating a queue *)
  let lpush1 = List.init plen (fun _ -> Random.int 20_000) in
  let queue = Priority_queue.create 50_000 42 in
  List.iter (fun ele -> Priority_queue.add queue ele ele) lpush1;

  let c1 = ref 0 in
  let c2 = ref 0 in
  let d1 =
    Domain.spawn (fun () ->
      while !c1 < len do
        let relt = Random.int 30_000 in
        Priority_queue.add queue relt relt;
        ignore @@ Priority_queue.remove_min queue;
        ignore @@ Priority_queue.remove_min queue;
        incr c1;
      done;)
    in
  let d2 =
    Domain.spawn (fun () ->
      while !c2 < len do
        let relt = Random.int 30_000 in
        Priority_queue.add queue relt relt;
        ignore @@ Priority_queue.remove_min queue;
        ignore @@ Priority_queue.remove_min queue;
        incr c2;
      done;)
    in
  Domain.join d1;
  Domain.join d2;
  (* Testing property *)
  if Priority_queue.is_empty queue then print_endline "success"
  else print_endline "failure"

