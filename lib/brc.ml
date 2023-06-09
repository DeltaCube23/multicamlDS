type t = { mutable counter: int; mutable reversed: int; mutable highbit: int }

let create () = {counter = 0; reversed = 0; highbit = -1}

let increment c =
  c.counter <- c.counter + 1;
  match c.counter with
  | x when x = 1 ->
    c.reversed <- 1;
    c.highbit <- 1;
    c.reversed
  | _ ->
    let bit = ref (c.highbit lsr 1) in
    let break = ref true in
    while !bit <> 0 && !break do
      c.reversed <- c.reversed lxor !bit;
      if (c.reversed land !bit) <> 0 then
        break := false;
      if !break then bit := !bit lsr 1
    done;
    if !bit = 0 then (
      c.highbit <- c.highbit lsl 1;
      c.reversed <- c.highbit;
    );
    c.reversed
let decrement c =
  c.counter <- c.counter - 1;
  let bit = ref (c.highbit lsr 1) in
  let break = ref true in
  while !bit <> 0 && !break do
    c.reversed <- c.reversed lxor !bit;
    if (c.reversed land !bit) = 0 then
      break := false;
    if !break then bit := !bit lsr 1
  done;
  if !bit = 0 then (
    c.highbit <- c.highbit lsr 1;
    c.reversed <- c.counter;
  );
  c.reversed

let get_size c = 
  c.counter

let get_idx c =
  c.reversed