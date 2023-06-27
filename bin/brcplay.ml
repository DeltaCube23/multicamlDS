open MulticamlDS;;

let cntr = Brc.create () in
let st = Unix.gettimeofday () in
for _ = 1 to 100_000 do
  ignore @@ (Brc.increment cntr)
done;
for _ = 1 to 100_000 do
  ignore @@ (Brc.decrement cntr)
done;
let et = Unix.gettimeofday () in
Printf.printf "Completed in %f\n" (et -. st);


let cntr2 = ref 0 in
let st = Unix.gettimeofday () in
for _ = 1 to 100_000 do
  incr cntr2
done;
for _ = 1 to 100_000 do
  decr cntr2
done;
let et = Unix.gettimeofday () in
Printf.printf "Completed in %f\n" (et -. st)