open MulticamlDS;;

Random.self_init ();
let sl = Skiplist.create ~max_height:20 () in
let lpush = [ 2; 3; 2 ] in
List.iter (fun ele -> ignore @@ Skiplist.add sl ele) lpush;

let lpop = ref [] in
for _ = 1 to 2 do
  let num = Skiplist.find_mark_min sl in
  lpop := num :: !lpop;
  ignore @@ Skiplist.remove sl num
done;

if List.rev !lpop = List.sort compare lpush then print_endline "success"
else print_endline "failure"
