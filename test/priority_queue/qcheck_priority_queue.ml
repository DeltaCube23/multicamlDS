open MulticamlDS

let tests_sequential =
  QCheck.
    [
      (* TEST 1: push *)
      Test.make ~count:100 ~name:"push" (list small_nat) (fun lpush ->
          assume (lpush <> []);
          (* Building a random queue *)
          let len = List.length lpush in
          let queue = Priority_queue.create (len + 1) 42 in
          List.iter (Priority_queue.add queue 42) lpush;

          (* Testing property *)
          not (Priority_queue.is_empty queue));
      (* TEST 2 - push, pop until empty *)
      Test.make ~count:100 ~name:"push_pop_until_empty" (list small_nat) (fun lpush ->
          assume (lpush <> []);
          (* Building a random queue *)
          let len = List.length lpush in
          let queue = Priority_queue.create (len + 1) 42 in
          List.iter (Priority_queue.add queue 42) lpush;

          (* Popping until [is_empty q] is true *)
          let count = ref 0 in
          while !count < len do
            incr count;
            ignore (Priority_queue.remove_min queue)
          done;

          (* Testing property *)
          Priority_queue.is_empty queue);
      (* TEST 3 - push, pop check order *)
      Test.make ~count:100 ~name:"push_pop_check_order" small_nat (fun len ->
          assume (len <> 0);
          (* Building a random queue *)
          let lpush = List.init len (fun i -> i) in
          let queue = Priority_queue.create (len + 1) 42 in
          List.iter (fun ele -> Priority_queue.add queue ele ele) lpush;

          (* Popping until [is_empty q] is true *)
          let out = ref [] in
          let insert v = out := v :: !out in
          let count = ref 0 in
          while !count < len do
            incr count;
            insert (Priority_queue.remove_min queue)
          done;

          (* Testing property *)
          Priority_queue.is_empty queue && !out = List.rev lpush);
      (* TEST 4 - push, pop check order random *)
      Test.make ~count:100 ~name:"push_pop_check_order_random" (list small_nat)
        (fun lpush ->
          assume (lpush <> []);
          (* Building a random queue *)
          let len = List.length lpush in
          let queue = Priority_queue.create (len + 1) 42 in
          List.iter (fun ele -> Priority_queue.add queue ele ele) lpush;

          (* Popping until [is_empty q] is true *)
          let out = ref [] in
          let insert v = out := v :: !out in
          let count = ref 0 in
          while !count < len do
            incr count;
            insert (Priority_queue.remove_min queue)
          done;

          (* Testing property *)
          Priority_queue.is_empty queue
          && List.rev !out = List.sort compare lpush);
    ]

let tests_two_domains =
  QCheck.
    [
      (* TEST 1 - two producers *)
      Test.make ~count:100 ~name:"double_add" small_nat (fun nlen ->
          assume (nlen <> 0);
          Random.self_init ();
          let rlen = Random.int 10_000 in
          let len = nlen + rlen in
          (* Creating a queue *)
          let lpush1 = List.init len (fun i -> i) in
          let lpush2 = List.init len (fun i -> i + len) in
          let queue = Priority_queue.create 30_000 42 in

          let producer1 =
            Domain.spawn (fun () ->
                List.iter (fun ele -> Priority_queue.add queue ele ele) lpush1)
          in
          let producer2 =
            Domain.spawn (fun () ->
                List.iter (fun ele -> Priority_queue.add queue ele ele) lpush2)
          in
          Domain.join producer1;
          Domain.join producer2;
          (* Testing property *)
          Priority_queue.get_len queue = 2 * len);
      (* TEST 2 - two producers, check pop order *)
      Test.make ~count:100 ~name:"double_add_remove" small_nat (fun nlen ->
          assume (nlen <> 0);
          Random.self_init ();
          let rlen = Random.int 10_000 in
          let len = nlen + rlen in
          (* Creating a queue *)
          let lpush1 = List.init len (fun i -> i) in
          let lpush2 = List.init len (fun i -> i + len) in
          let queue = Priority_queue.create 30_000 42 in

          let producer1 =
            Domain.spawn (fun () ->
                List.iter (fun ele -> Priority_queue.add queue ele ele) lpush1)
          in
          let producer2 =
            Domain.spawn (fun () ->
                List.iter (fun ele -> Priority_queue.add queue ele ele) lpush2)
          in
          Domain.join producer1;
          Domain.join producer2;

          let out = ref [] in
          let insert v = out := v :: !out in
          let count = ref 0 in
          while !count < 2 * len do
            incr count;
            insert (Priority_queue.remove_min queue)
          done;
          (* Testing property *)
          Priority_queue.is_empty queue && List.rev !out = lpush1 @ lpush2);
      (* TEST 3 - parallel push pop *)
      Test.make ~count:10 ~name:"parallel_add_remove" small_nat (fun nlen ->
          assume (nlen <> 0);
          Random.self_init ();
          let rlen = Random.int 10_000 in
          let len = nlen + rlen in
          (* Creating a queue *)
          let lpush1 = List.init len (fun i -> i) in
          let lpush2 = List.init len (fun i -> i + len) in
          let queue = Priority_queue.create 30_000 42 in
          List.iter (fun ele -> Priority_queue.add queue ele ele) lpush1;
          let producer1 =
            Domain.spawn (fun () ->
                List.iter (fun ele -> Priority_queue.add queue ele ele) lpush2)
          in
          let out = ref [] in
          let insert v = out := v :: !out in
          let count = ref 0 in
          while !count < 2 * len do
            if Priority_queue.is_empty queue then Domain.cpu_relax ()
            else (
              insert (Priority_queue.remove_min queue);
              incr count)
          done;
          Domain.join producer1;
          (* Testing property *)
          Priority_queue.is_empty queue && List.rev !out = lpush1 @ lpush2);
      (* TEST 4 - add double remove *)
      Test.make ~count:10 ~name:"add_double_remove" small_nat (fun nlen ->
          assume (nlen <> 0);
          Random.self_init ();
          let rlen = Random.int 10_000 in
          let len = nlen + rlen in
          let plen = len * 2 in
          (* Creating a queue *)
          let lpush1 = List.init plen (fun _ -> Random.int 20_000) in
          let queue = Priority_queue.create 30_000 42 in
          List.iter (fun ele -> Priority_queue.add queue ele ele) lpush1;

          let c1 = ref 0 in
          let c2 = ref 0 in
          let consumer1 =
            Domain.spawn (fun () ->
                while !c1 < len do
                  ignore @@ Priority_queue.remove_min queue;
                  incr c1
                done)
          in
          let consumer2 =
            Domain.spawn (fun () ->
                while !c2 < len do
                  ignore @@ Priority_queue.remove_min queue;
                  incr c2
                done)
          in
          Domain.join consumer1;
          Domain.join consumer2;
          (* Testing property *)
          Priority_queue.is_empty queue);
      (* TEST 5 - same domain add remove *)
      Test.make ~count:10 ~name:"same_domain_add_remove" small_nat (fun nlen ->
          assume (nlen <> 0);
          Random.self_init ();
          let rlen = Random.int 1000 in
          let len = nlen + rlen in
          let plen = 2 * len in
          (* Creating a queue *)
          let lpush1 = List.init plen (fun _ -> Random.int 20_000) in
          let queue = Priority_queue.create 5000 42 in
          List.iter (fun ele -> Priority_queue.add queue ele ele) lpush1;

          let c1 = ref 0 in
          let c2 = ref 0 in
          let d1 =
            Domain.spawn (fun () ->
                while !c1 < len do
                  let relt = Random.int 30_000 in
                  Priority_queue.add queue relt relt;
                  ignore @@ Priority_queue.remove_min queue;
                  ignore @@ Priority_queue.remove_min queue;
                  incr c1
                done)
          in
          let d2 =
            Domain.spawn (fun () ->
                while !c2 < len do
                  let relt = Random.int 30_000 in
                  Priority_queue.add queue relt relt;
                  ignore @@ Priority_queue.remove_min queue;
                  ignore @@ Priority_queue.remove_min queue;
                  incr c2
                done)
          in
          Domain.join d1;
          Domain.join d2;
          (* Testing property *)
          Priority_queue.is_empty queue);
    ]

let main () =
  let to_alcotest = List.map QCheck_alcotest.to_alcotest in
  Alcotest.run "Priority_queue"
    [
      ("test_sequential", to_alcotest tests_sequential);
      ("test_two_domains", to_alcotest tests_two_domains);
    ]
;;

main ()
