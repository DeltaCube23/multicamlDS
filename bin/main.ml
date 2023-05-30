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

(* seed = 907026367
let lpush = [-937365509245381732; 356968215213169086; -1417827251707620319; -901233158630615213; 3474625093963640403; -3899889420340277892; -663891703863524430] in 
let queue = Bounded_queue.init() in 
let producer =
  Domain.spawn (fun () -> List.iter (Bounded_queue.push queue) lpush)
in

(* each iteration will pop 1 element from queue *)
let count = ref 0 in
while !count < List.length lpush do
  incr count;
  print_int !count;
  ignore (Bounded_queue.pop queue)
done;

Domain.join producer;

print_int !count*)


let lpush1 = List.init 55 (fun i->i+1) in
let lpush2 = List.rev lpush1 in
let llist = Fine_list.create 0 in
let c1 = ref 0 in
let c2 = ref 0 in
let producer1 = 
  Domain.spawn (fun () -> List.iter (fun ele -> if (Fine_list.add llist ele) then incr c1) lpush1)
in
let producer2 = 
  Domain.spawn (fun () -> List.iter (fun ele -> if (Fine_list.add llist ele) then incr c2) lpush2)
in
Domain.join producer1;
Domain.join producer2;

print_int (!c1 + !c2)