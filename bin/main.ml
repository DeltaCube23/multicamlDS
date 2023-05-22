open MulticamlDS;;

print_endline "--- Simualtion Starts ---";

let worker q num_ops =
  for i = 1 to num_ops do
    let action = Random.int 2 in
    if action mod 2 = 0 then Ms_queue.push q i else ignore (Ms_queue.pop q)
  done
in

let q = Ms_queue.init () in
let start_time = Unix.gettimeofday () in
let d1 = Domain.spawn (fun _ -> worker q 1000_000) in
let d2 = Domain.spawn (fun _ -> worker q 1000_000) in
let d3 = Domain.spawn (fun _ -> worker q 1000_000) in
let d4 = Domain.spawn (fun _ -> worker q 1000_000) in
Domain.join d1;
Domain.join d2;
Domain.join d3;
Domain.join d4;
let end_time = Unix.gettimeofday () in
Format.printf "Completed in %f\n" (end_time -. start_time);

Gc.print_stat stdout
