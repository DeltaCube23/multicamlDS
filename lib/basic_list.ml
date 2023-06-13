type 'a node = { value : 'a; mutable next : 'a node option }
type 'a t = { head : 'a node }

let create_node value nextptr = { value; next = nextptr }
let create key = { head = create_node key None }

(* find the previous node <= key *)
let find_previous_add t key =
  let rec aux prev next =
    match next with
    | Some node when node.value > key -> prev
    | Some node -> aux node node.next
    | None -> prev
  in
  aux t.head t.head.next

(* find the previous node < key *)
let find_previous_remove t key =
  let rec aux prev next =
    match next with
    | Some node when node.value >= key -> prev
    | Some node -> aux node node.next
    | None -> prev
  in
  aux t.head t.head.next

let add t value =
  let insert x =
    if x.value = value && x <> t.head then false
    else
      let new_node = create_node value x.next in
      x.next <- Some new_node;
      true
  in
  let start = find_previous_add t value in
  insert start

(* remove node from correct position *)
let remove t value =
  let erase x y =
    if y.value <> value then false
    else (
      x.next <- y.next;
      true)
  in
  let start = find_previous_remove t value in
  let to_remove =
    match start.next with Some node -> node | _ -> create_node value None
  in
  erase start to_remove

let is_empty t =
  let empty = t.head.next = None in
  empty
