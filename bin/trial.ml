(*let cntr = ref 1 in 
let highbit = ref 1 in
let border = ref 4 in
let miscount = ref 0 in
let base = ref 2 in
for _ = 1 to 50 do 
  cntr := !cntr + !highbit;
  if !cntr >= !border then (
    incr miscount;
    if !miscount >= !highbit then (
      miscount := 0;
      highbit := !highbit * 2;
      base := !border;
      cntr := !base;
      border := !border * 2;
    ) else (
      base := (!base + 1);
      cntr := !base;
    )
  ); 
  Printf.printf "num = %d\n" !cntr;
done


1001 -> remove high -> 001 -> reverse -> 100 -> 1100

2,3
+1
+1

4,6,5,7
+2, -1, +2

+1
8, 12, 10, 14, 9, 13, 11, 15 
+4, -2, +4, -5, +4, -2, +4
+1

16, 24, 20, 28, 18, 26, 22, 30, 17, 25, 21, 29, 19, 27, 23, 31
+8, -4, +8, -10, +8, -4, +8, -13, +8, -4, +8, -10, +8, -4, +8,
*)

open MulticamlDS;;

let cntr = Brc.create () in
let brclist = Array.init 100 (fun _ -> Brc.increment cntr) in 
for i = 0 to 99 do 
  print_int brclist.(i)
done;