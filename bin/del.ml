open MulticamlDS;;

Random.self_init ();
(*let arr = Array.make 1000 (Dll.create_node 42 None None) in
let dl = Dll.create 23 in 
let h = Dll.get_head dl in
let check = ref true in
for i = 1 to 100 do 
  let k = Random.int 1000 in
  arr.(i) <- (Dll.create_node k None None);
  check := (Dll.insert_after h arr.(i));
done;

for i = 1 to 99 do 
  let after = Dll.get_prev arr.(i) in 
  if after != arr.(i+1) then check := false
done;

for _ = 1 to 50 do 
  let k = Random.int 100 in 
  check := Dll.delete arr.(k+1)
done;

if !check then print_endline "success"
else print_endline "failure";*)

let arr = Array.make 1000 (Dll.create_node 42 None None) in
let dl = Dll.create 23 in 
let h = Dll.get_head dl in
let cntr = Atomic.make 0 in
for i = 1 to 100 do 
  let k = Random.int 1000 in
  arr.(i) <- (Dll.create_node k None None);
  if (Dll.insert_after h arr.(i)) then Atomic.incr cntr
done;

let d1 = Domain.spawn (fun _ ->
  for i = 101 to 200 do 
    let k = Random.int 1000 in
    let idx = Random.int 100 in
    arr.(i) <- (Dll.create_node k None None);
    if (Dll.insert_after arr.(idx + 1) arr.(i)) then Atomic.incr cntr
  done;  

  for j = 1 to 25 do 
    if Dll.delete arr.(j*2) then Atomic.decr cntr
  done;
) in

let d2 = Domain.spawn (fun _ ->
  for i = 201 to 300 do 
    let k = Random.int 1000 in
    let idx = Random.int 100 in
    arr.(i) <- (Dll.create_node k None None);
    if (Dll.insert_after arr.(idx + 1) arr.(i)) then Atomic.incr cntr
  done; 
  
  for j = 1 to 25 do 
    let l = 50 + (j*2) in
    if Dll.delete arr.(l) then Atomic.decr cntr
  done;
) in

Domain.join d1;
Domain.join d2;
(* the counter math will not be exact because some deletions will render future random insertions as false *)
print_int (Atomic.get cntr)