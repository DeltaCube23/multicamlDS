open MulticamlDS;;
let lpush = [1;2;3;4] in
let queue = Priority_queue.create 10_000 42 in
List.iter (fun ele -> Priority_queue.add queue ele ele) lpush;

(* Popping until [is_empty q] is true *)
let out = ref [] in
let insert v = out := v :: !out in
let count = ref 0 in
while !count < 4 do
  incr count;
  insert (Priority_queue.remove_min queue);
done;

(* Testing property *)
if (Priority_queue.is_empty queue && List.rev lpush = !out) then print_endline "yes"
else print_endline "no"

open MulticamlDS;;