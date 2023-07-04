type t = { heap : int array; len : int ref }

let create sz = { heap = Array.init (sz + 1) (fun _ -> -1); len = ref 0 }

(* to add new element *)
let push t value =
  incr t.len;
  let temp = ref !(t.len) in
  while !temp > 0 do
    let par = !temp / 2 in
    if value < t.heap.(par) then (
      t.heap.(!temp) <- t.heap.(par);
      temp := par)
    else (
      t.heap.(!temp) <- value;
      temp := 0)
  done

(* to remove top element *)
let pop t =
  let bottom = !(t.len) in
  let x = t.heap.(1) in
  t.heap.(1) <- t.heap.(bottom);
  t.heap.(bottom) <- -1;
  decr t.len;
  let par = ref 1 in
  let child = ref 0 in
  while 2 * !par <= !(t.len) do
    let left = !par * 2 in
    let right = left + 1 in
    if t.heap.(right) = -1 then child := left
    else if t.heap.(left) < t.heap.(right) then child := left
    else child := right;
    if t.heap.(!par) > t.heap.(!child) then (
      let temp = t.heap.(!par) in
      t.heap.(!par) <- t.heap.(!child);
      t.heap.(!child) <- temp;
      par := !child)
    else par := !(t.len)
  done;
  x
