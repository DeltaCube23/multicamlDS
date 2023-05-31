(** Sequential and Parallel model-based tests of michael_scott_queue *)

open QCheck
open STM
module Ms_queue = MulticamlDS.Ms_queue

module MSQConf = struct
  type cmd = Push of int | Pop | Is_empty

  (* possible operations *)
  let show_cmd c =
    match c with
    | Push i -> "Push " ^ string_of_int i
    | Pop -> "Pop"
    | Is_empty -> "Is_empty"

  (* model state and system state *)
  type state = int list
  type sut = int Ms_queue.t

  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.map (fun i -> Push i) int_gen;
           Gen.return Pop;
           Gen.return Is_empty;
         ])

  let init_state = []
  let init_sut () = Ms_queue.init ()
  let cleanup _ = ()

  (* next stage after performing operation on current state *)
  let next_state c s =
    match c with
    | Push i -> i :: s
    | Pop -> ( match List.rev s with [] -> s | _ :: s' -> List.rev s')
    | Is_empty -> s

  let precond _ _ = true

  let run c d =
    match c with
    | Push i -> Res (unit, Ms_queue.push d i)
    | Pop -> Res (option int, Ms_queue.pop d)
    | Is_empty -> Res (bool, Ms_queue.is_empty d)

  let postcond c (s : state) res =
    match (c, res) with
    | Push _, Res ((Unit, _), _) -> true
    | Pop, Res ((Option Int, _), res) -> (
        match List.rev s with [] -> res = None | j :: _ -> res = Some j)
    | Is_empty, Res ((Bool, _), res) -> res = (s = [])
    | _, _ -> false
end

module MSQ_seq = STM_sequential.Make (MSQConf)
module MSQ_dom = STM_domain.Make (MSQConf)

let () =
  let count = 500 in
  QCheck_base_runner.run_tests_main
    [
      MSQ_seq.agree_test ~count ~name:"STM Ms_queue test sequential";
      MSQ_dom.agree_test_par ~count ~name:"STM Ms_queue test parallel";
    ]
