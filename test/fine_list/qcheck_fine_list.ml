open MulticamlDS

let tests_sequential =
  QCheck.
    [
      (* TEST 1: insert *)
      Test.make ~name:"push" (list int) (fun lpush ->
          assume (lpush <> []);
          (* Building a random list *)
          let llist = Fine_list.create () in
          List.iter (fun ele -> ignore @@ Fine_list.add llist ele) lpush;

          (* Testing property *)
          not (Fine_list.is_empty llist));
      (* TEST 2 - insert, remove until empty *)
      Test.make ~name:"push_pop_until_empty" (list int) (fun lpush ->
          (* Building a random list *)
          let llist = Fine_list.create () in
          List.iter (fun ele -> ignore @@ Fine_list.add llist ele) lpush;

          (* Removing until [is_empty l] is true *)
          List.iter (fun ele -> ignore @@ Fine_list.remove llist ele) lpush;

          (* Testing property *)
          Fine_list.is_empty llist);
    ]

let main () =
  let to_alcotest = List.map QCheck_alcotest.to_alcotest in
  Alcotest.run "Fine_list" [ ("test_sequential", to_alcotest tests_sequential) ]
;;

main ()
