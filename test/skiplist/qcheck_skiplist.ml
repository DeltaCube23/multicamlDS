open MulticamlDS

let tests_sequential =
  QCheck.
    [
      (* TEST 1 - push, pop check order *)
      Test.make ~count:1000 ~name:"push_pop_check_order" small_nat (fun len ->
          assume (len <> 0);
          (* Building a random queue *)
          let lpush = List.init len (fun i -> i) in
          let queue = Skiplist.create ~max_height:20 () in
          List.iter (fun ele -> ignore @@ Skiplist.add queue ele) lpush;

          (* Popping until [is_empty q] is true *)
          let out = ref [] in
          let insert v = out := v :: !out in
          let count = ref 0 in
          while !count < len do
            incr count;
            let num = Skiplist.find_mark_min queue in
            insert num;
            ignore @@ Skiplist.remove queue num
          done;

          (* Testing property *)
          !out = List.rev lpush);
      (* TEST 2 - push, pop check order random *)
      Test.make ~count:1000 ~name:"push_pop_check_order_random" small_nat
        (fun len ->
          assume (len <> 0);
          (* Building a random queue *)
          Random.self_init ();
          let queue = Skiplist.create ~max_height:20 () in
          let lpush = ref [] in
          for _ = 1 to len do
            let x = Random.int 100_000 in
            if Skiplist.add queue x then lpush := x :: !lpush
          done;
          (* Popping until [is_empty q] is true *)
          let out = ref [] in
          let insert v = out := v :: !out in
          let count = ref 0 in
          while !count < List.length !lpush do
            incr count;
            let num = Skiplist.find_mark_min queue in
            insert num;
            ignore @@ Skiplist.remove queue num
          done;

          (* Testing property *)
          List.rev !out = List.sort compare !lpush);
    ]

let tests_domains =
  QCheck.
    [
      (* TEST 1 - 2 producers, check pop order *)
      Test.make ~count:100 ~name:"double_add_remove" small_nat (fun nlen ->
          assume (nlen <> 0);
          Random.self_init ();
          let rlen = Random.int 1000 in
          let len = nlen + rlen in
          (* Creating a queue *)
          let lpush1 = List.init len (fun i -> i) in
          let lpush2 = List.init len (fun i -> i + len) in
          let queue = Skiplist.create ~max_height:20 () in

          let producer1 =
            Domain.spawn (fun () ->
                List.iter (fun ele -> ignore @@ Skiplist.add queue ele) lpush1)
          in
          let producer2 =
            Domain.spawn (fun () ->
                List.iter (fun ele -> ignore @@ Skiplist.add queue ele) lpush2)
          in
          Domain.join producer1;
          Domain.join producer2;

          let out = ref [] in
          let insert v = out := v :: !out in
          let count = ref 0 in
          while !count < 2 * len do
            incr count;
            let num = Skiplist.find_mark_min queue in
            insert num;
            ignore @@ Skiplist.remove queue num
          done;
          (* Testing property *)
          List.rev !out = lpush1 @ lpush2);
      (* TEST 2 - 2 consumers, check order *)
      Test.make ~count:100 ~name:"add_double_remove" small_nat (fun nlen ->
          assume (nlen <> 0);
          Random.self_init ();
          let rlen = Random.int 1000 in
          let len = nlen + rlen in
          let plen = 2 * len in
          (* Creating a queue *)
          let lpush = List.init plen (fun i -> i) in
          let queue = Skiplist.create ~max_height:20 () in
          List.iter (fun ele -> ignore @@ Skiplist.add queue ele) lpush;
          let c1 = ref 0 in
          let c2 = ref 0 in
          let consumer1 =
            Domain.spawn (fun () ->
                while !c1 < len do
                  let num = Skiplist.find_mark_min queue in
                  ignore @@ Skiplist.remove queue num;
                  incr c1
                done)
          in
          let consumer2 =
            Domain.spawn (fun () ->
                while !c2 < len do
                  let num = Skiplist.find_mark_min queue in
                  ignore @@ Skiplist.remove queue num;
                  incr c2
                done)
          in
          Domain.join consumer1;
          Domain.join consumer2;
          !c1 + !c2 = plen);
      (* TEST 3 - Same domain add *)
      Test.make ~count:100 ~name:"parallel_add" small_nat (fun len ->
          assume (len <> 0);
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
                done)
          in
          let d2 =
            Domain.spawn (fun () ->
                for i = 1 to len do
                  if Skiplist.add queue ((4 * i) + 1) then incr c2;
                done)
          in
          let d3 =
            Domain.spawn (fun () ->
                for i = 1 to len do
                  if Skiplist.add queue ((4 * i) + 2) then incr c3;
                done)
          in
          let d4 =
            Domain.spawn (fun () ->
                for i = 1 to len do
                  if Skiplist.add queue ((4 * i) + 3) then incr c4;
                done)
          in
          Domain.join d1;
          Domain.join d2;
          Domain.join d3;
          Domain.join d4;
          !c1 + !c2 + !c3 + !c4 = (4 *len));
      (* TEST 4 - Same domain add remove *)
      Test.make ~count:100 ~name:"parallel_add_remove" small_nat (fun slen ->
        assume (slen <> 0);
        Random.self_init ();
        (* Creating a queue *)
        let queue = Skiplist.create ~max_height:20 () in
        let len = Random.int 10000 + slen in 
        let c1 = ref 0 in
        let c2 = ref 0 in
        let c3 = ref 0 in
        let c4 = ref 0 in
        let c5 = ref 0 in 
        for i = 1 to len do 
          if Skiplist.add queue (5 * i) then incr c5
        done;
        let d1 =
          Domain.spawn (fun () ->
              for i = 1 to len do
                if Skiplist.add queue ((5 * i) + 4) then incr c1;
                let num = Skiplist.find_mark_min queue in
                if num <> Int.max_int then(
                  if Skiplist.remove queue num then decr c1
                )
              done)
        in
        let d2 =
          Domain.spawn (fun () ->
              for i = 1 to len do
                if Skiplist.add queue ((5 * i) + 1) then incr c2;
                let num = Skiplist.find_mark_min queue in
                if num <> Int.max_int then(
                  if Skiplist.remove queue num then decr c2
                )
              done)
        in
        let d3 =
          Domain.spawn (fun () ->
              for i = len downto 1 do
                if Skiplist.add queue ((5 * i) + 2) then incr c3;
                let num = Skiplist.find_mark_min queue in
                if num <> Int.max_int then(
                  if Skiplist.remove queue num then decr c3
                )
              done)
        in
        let d4 =
          Domain.spawn (fun () ->
              for i = len downto 1 do
                if Skiplist.add queue ((5 * i) + 3) then incr c4;
                let num = Skiplist.find_mark_min queue in
                if num <> Int.max_int then(
                  if Skiplist.remove queue num then decr c4
                )
              done)
        in
        Domain.join d1;
        Domain.join d2;
        Domain.join d3;
        Domain.join d4;
        for _ = 1 to len do 
          let num = Skiplist.find_mark_min queue in
          if num <> Int.max_int then(
            if Skiplist.remove queue num then decr c5)
        done;
        !c1 = 0 && !c2 = 0 && !c3 = 0 && !c4 = 0 && !c5 = 0);
    ]

let main () =
  (* QCheck_base_runner.set_seed 124752466; *)
  let to_alcotest = List.map QCheck_alcotest.to_alcotest in
  Alcotest.run "Priority_queue"
    [
      ("test_sequential", to_alcotest tests_sequential);
      ("test_domains", to_alcotest tests_domains);
    ]
;;
main ()
