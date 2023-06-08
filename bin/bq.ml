open MulticamlDS;;

let task = 1_000_000 in

print_endline "--- Simualtion Starts ---";

(* 1-both push and pop, 2-only producer, 3-only consumer *)
let worker q num_ops id =
  if id = 1 then
    for i = 1 to num_ops do
      let action = Random.int 2 in
      if action = 0 then Bounded_queue.push q i
      else ignore (Bounded_queue.pop q)
    done
  else if id = 2 then
    for i = 1 to num_ops do
      Bounded_queue.push q i
    done
  else
    for _ = 1 to num_ops do
      ignore (Bounded_queue.pop q)
    done
in

(* 2 producers 2 consumers *)
let q = Bounded_queue.init () in
let start_time = Unix.gettimeofday () in
let d1 = Domain.spawn (fun _ -> worker q task 2) in
let d2 = Domain.spawn (fun _ -> worker q task 2) in
let d3 = Domain.spawn (fun _ -> worker q task 3) in
let d4 = Domain.spawn (fun _ -> worker q task 3) in
Domain.join d1;
Domain.join d2;
Domain.join d3;
Domain.join d4;
let end_time = Unix.gettimeofday () in
Format.printf "Completed 2 producers 2 consumers in %f\n"
  (end_time -. start_time);

(* 4 producers 4 consumers *)
let q = Bounded_queue.init () in
let start_time = Unix.gettimeofday () in
let d1 = Domain.spawn (fun _ -> worker q task 2) in
let d2 = Domain.spawn (fun _ -> worker q task 2) in
let d3 = Domain.spawn (fun _ -> worker q task 2) in
let d4 = Domain.spawn (fun _ -> worker q task 2) in
let d5 = Domain.spawn (fun _ -> worker q task 3) in
let d6 = Domain.spawn (fun _ -> worker q task 3) in
let d7 = Domain.spawn (fun _ -> worker q task 3) in
let d8 = Domain.spawn (fun _ -> worker q task 3) in
Domain.join d1;
Domain.join d2;
Domain.join d3;
Domain.join d4;
Domain.join d5;
Domain.join d6;
Domain.join d7;
Domain.join d8;
let end_time = Unix.gettimeofday () in
Format.printf "Completed 4 producers 4 consumers in %f\n"
  (end_time -. start_time);

(* 1 producer 4 consumers *)
let q = Bounded_queue.init () in
let start_time = Unix.gettimeofday () in
let d1 = Domain.spawn (fun _ -> worker q (4 * task) 2) in
let d2 = Domain.spawn (fun _ -> worker q task 3) in
let d3 = Domain.spawn (fun _ -> worker q task 3) in
let d4 = Domain.spawn (fun _ -> worker q task 3) in
let d5 = Domain.spawn (fun _ -> worker q task 3) in
Domain.join d1;
Domain.join d2;
Domain.join d3;
Domain.join d4;
Domain.join d5;
let end_time = Unix.gettimeofday () in
Format.printf "Completed 1 producer 4 consumers in %f\n" (end_time -. start_time);

(* 4 producers 1 consumer *)
let q = Bounded_queue.init () in
let start_time = Unix.gettimeofday () in
let d1 = Domain.spawn (fun _ -> worker q task 2) in
let d2 = Domain.spawn (fun _ -> worker q task 2) in
let d3 = Domain.spawn (fun _ -> worker q task 2) in
let d4 = Domain.spawn (fun _ -> worker q task 2) in
let d5 = Domain.spawn (fun _ -> worker q (4 * task) 3) in
Domain.join d1;
Domain.join d2;
Domain.join d3;
Domain.join d4;
Domain.join d5;
let end_time = Unix.gettimeofday () in
Format.printf "Completed 4 producers 1 consumer in %f\n" (end_time -. start_time)
