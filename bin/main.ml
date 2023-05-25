open MulticamlDS;;

(* print_endline "--- Simualtion Starts ---";

   (* 1-both push and pop, 2-only producer, 3-only consumer *)
   let worker q num_ops id =
     if id = 1 then begin
       for i = 1 to num_ops do
         let action = Random.int 2 in
         if action = 0 then Bounded_queue.push q i
         else ignore (Bounded_queue.pop q)
       done
     end
     else if id = 2 then begin
       for i = 1 to num_ops do
         Bounded_queue.push q i
       done
     end
     else begin
       for _ = 1 to num_ops do
         ignore (Bounded_queue.pop q)
       done
     end
   in

   let q = Bounded_queue.init () in
   let start_time = Unix.gettimeofday () in
   let d1 = Domain.spawn (fun _ -> worker q 1_000_000 1) in
   let d2 = Domain.spawn (fun _ -> worker q 1_000_000 1) in
   let d3 = Domain.spawn (fun _ -> worker q 1_000_000 1) in
   let d4 = Domain.spawn (fun _ -> worker q 1_000_000 1) in
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

   Gc.print_stat stdout *)

let ll = Fine_list.create () in
let t1 = Fine_list.add ll 0 in
let t2 = Fine_list.remove ll 0 in
if t1 then print_endline "success";
if t2 then print_endline "failure"
