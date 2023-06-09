(** Sequential and Parallel model-based tests of priority_queue *)

open QCheck
open STM
module Priority_queue = MulticamlDS.Priority_queue

module PQConf = struct
  type cmd = Add of int | Remove_min | Is_empty

  (* possible operations *)
  let show_cmd c =
    match c with
    | Add i -> "Add " ^ string_of_int i
    | Remove_min -> "Remove_min"
    | Is_empty -> "Is_empty"

  (* model state and system state *)
  type state = int list
  type sut = int Priority_queue.t

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.map (fun i -> Add i) int_gen;
           Gen.return Remove_min;
           Gen.return Is_empty;
         ])

  let init_state = []
  let init_sut () = Priority_queue.create 100_001 42
  let cleanup _ = ()

  (* next stage after performing operation on current state *)
  let next_state c s =
    match c with
    | Add i ->
      let rec sortlist l e =
        match l with
        | [] -> [ e ]
        | x :: ys when List.length l < 100_000 ->
          if e <= x then e :: x :: ys
          else x :: sortlist ys e
        | x :: ys -> x :: ys
      in
      sortlist s i
    | Remove_min -> (match s with [] -> s | _ :: s' -> s')
    | Is_empty -> s

  let precond _ _ = true

  let run c d =
    match c with
    | Add i -> Res (unit, Priority_queue.add d i i)
    | Remove_min -> Res (int, Priority_queue.remove_min d)
    | Is_empty -> Res (bool, Priority_queue.is_empty d)

  let postcond c (s : state) res =
    match (c, res) with
    | Add _, Res ((Unit, _), _) -> true
    | Remove_min, Res ((Int, _), res) -> (match s with [] -> res = 42 | x :: _ -> res = x)
    | Is_empty, Res ((Bool, _), res) -> res = (s = [])
    | _, _ -> false
end

module PQ_seq = STM_sequential.Make (PQConf)
module PQ_dom = STM_domain.Make (PQConf)

let () =
  let count = 1000 in
  QCheck_base_runner.run_tests_main
    [
      PQ_seq.agree_test ~count ~name:"STM Priority_Queue test sequential";
      PQ_dom.agree_test_par ~count ~name:"STM Priority_Queue test parallel";
    ]
