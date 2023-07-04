open MulticamlDS;;
let temp_test = QCheck.Test.make ~count:100 ~name:"parallel_add_remove" QCheck.small_nat (fun len ->
Random.self_init ();
(* Creating a queue *)
let queue = Skiplist.create ~max_height:20 () in
let c1 = ref 0 in
let c2 = ref 0 in
let c3 = ref 0 in
let c4 = ref 0 in
let d1 =
  Domain.spawn (fun () ->
      for i = 1 to len do
        if Skiplist.add queue (4 * i) then incr c1;
        let num = Skiplist.find_mark_min queue in
        if num == Int.max_int then Format.printf "trouble%!";
        if Skiplist.remove queue num then decr c1
      done)
in
let d2 =
  Domain.spawn (fun () ->
      for i = 1 to len do
        if Skiplist.add queue ((4 * i) + 1) then incr c2;
        let num = Skiplist.find_mark_min queue in
        if num == Int.max_int then Format.printf "trouble%!";
        if Skiplist.remove queue num then decr c2
      done)
in
let d3 =
  Domain.spawn (fun () ->
      for i = 1 to len do
        if Skiplist.add queue ((4 * i) + 2) then incr c3;
        let num = Skiplist.find_mark_min queue in
        if num == Int.max_int then Format.printf "trouble%!";
        if Skiplist.remove queue num then decr c3
      done)
in
let d4 =
  Domain.spawn (fun () ->
      for i = 1 to len do
        if Skiplist.add queue (4 * i + 3) then incr c4;
        let num = Skiplist.find_mark_min queue in
        if num == Int.max_int then Format.printf "trouble%!";
        if Skiplist.remove queue num then decr c4
      done)
in
Domain.join d1;
Domain.join d2;
Domain.join d3;
Domain.join d4;
!c1 + !c2 + !c3 + !c4 = 0);;

QCheck_runner.set_seed 451380142;;
QCheck_runner.run_tests [temp_test];;