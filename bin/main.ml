open MulticamlDS;;

print_endline "--- Simualtion Starts ---";

(* 2/3 push, 1/3 pop *)
let worker q num_ops =
  for i = 1 to num_ops do
    let action = Random.int 3 in
    if action mod 3 > 0 then Bounded_queue.push q i else ignore (Bounded_queue.pop q)
  done
in

let q = Bounded_queue.init () in
let start_time = Unix.gettimeofday () in
let d1 = Domain.spawn (fun _ -> worker q 1_000_000) in
let d2 = Domain.spawn (fun _ -> worker q 1_000_000) in
let d3 = Domain.spawn (fun _ -> worker q 1_000_000) in
let d4 = Domain.spawn (fun _ -> worker q 1_000_000) in
(* let d5 = Domain.spawn (fun _ -> worker q 1_000_000) in
let d6 = Domain.spawn (fun _ -> worker q 1_000_000) in
let d7 = Domain.spawn (fun _ -> worker q 1_000_000) in
let d8 = Domain.spawn (fun _ -> worker q 1_000_000) in
Domain.join d5;
Domain.join d6;
Domain.join d7;
Domain.join d8; *)
Domain.join d1;
Domain.join d2;
Domain.join d3;
Domain.join d4;
let end_time = Unix.gettimeofday () in
Format.printf "Completed in %f\n" (end_time -. start_time);

Gc.print_stat stdout
