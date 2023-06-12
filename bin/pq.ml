open MulticamlDS;;

Random.self_init ();
let len = 500_000 in
let queue = Priority_queue.create 10_000_000 42 in
for i = 1 to 500_000 do 
  Priority_queue.add queue i i
done;
let start_time = Unix.gettimeofday () in
let c1 = ref 0 in
let c2 = ref 0 in
let c3 = ref 0 in
let c4 = ref 0 in
let c5 = ref 0 in
let c6 = ref 0 in
let c7 = ref 0 in
let c8 = ref 0 in
let max_tt = ref Float.min_float in
let d1 =
  Domain.spawn (fun () ->
    while !c1 < len do
      let relt = Random.int 300_000 in
      let op_st = Unix.gettimeofday () in
      Priority_queue.add queue relt relt;
      let op_et = Unix.gettimeofday () in 
      let t_t = op_et -. op_st in
      if t_t > !max_tt then max_tt := t_t;
      (* ignore @@ Priority_queue.remove_min queue;
      ignore @@ Priority_queue.remove_min queue; *)
      incr c1;
    done;)
  in
let d2 =
  Domain.spawn (fun () ->
    while !c2 < len do
      let relt = Random.int 300_000 in
      Priority_queue.add queue relt relt;
      (* ignore @@ Priority_queue.remove_min queue;
      ignore @@ Priority_queue.remove_min queue; *)
      incr c2;
    done;)
  in
let d3 =
  Domain.spawn (fun () ->
    while !c3 < len do
      let relt = Random.int 300_000 in
      Priority_queue.add queue relt relt;
      (* ignore @@ Priority_queue.remove_min queue;
      ignore @@ Priority_queue.remove_min queue; *)
      incr c3;
    done;)
  in
let d4 =
  Domain.spawn (fun () ->
    while !c4 < len do
      let relt = Random.int 300_000 in
      Priority_queue.add queue relt relt;
      (* ignore @@ Priority_queue.remove_min queue;
      ignore @@ Priority_queue.remove_min queue; *)
      incr c4;
    done;)
  in
let d5 =
  Domain.spawn (fun () ->
    while !c5 < len do
      let relt = Random.int 300_000 in
      Priority_queue.add queue relt relt;
      (* ignore @@ Priority_queue.remove_min queue;
      ignore @@ Priority_queue.remove_min queue; *)
      incr c5;
    done;)
  in
let d6 =
  Domain.spawn (fun () ->
    while !c6 < len do
      let relt = Random.int 300_000 in
      Priority_queue.add queue relt relt;
      (* ignore @@ Priority_queue.remove_min queue;
      ignore @@ Priority_queue.remove_min queue; *)
      incr c6;
    done;)
  in
let d7 =
  Domain.spawn (fun () ->
    while !c7 < len do
      let relt = Random.int 300_000 in
      Priority_queue.add queue relt relt;
      (* ignore @@ Priority_queue.remove_min queue;
      ignore @@ Priority_queue.remove_min queue; *)
      incr c7;
    done;)
  in
let d8 =
  Domain.spawn (fun () ->
    while !c8 < len do
      let relt = Random.int 300_000 in
      Priority_queue.add queue relt relt;
      (* ignore @@ Priority_queue.remove_min queue;
      ignore @@ Priority_queue.remove_min queue; *)
      incr c8;
    done;)
  in
Domain.join d1;
Domain.join d2;
Domain.join d3;
Domain.join d4;
Domain.join d5;
Domain.join d6;
Domain.join d7;
Domain.join d8;

let end_time = Unix.gettimeofday () in
Format.printf "Completed in %f\n"
(end_time -. start_time);
Format.printf "longest in %f\n" !max_tt;
(* Testing property *)
if Priority_queue.get_len queue = 9*len then print_endline "success"
else print_endline "failure"


