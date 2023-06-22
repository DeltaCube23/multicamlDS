open MulticamlDS.Priority_queue

let test ninit nd nt =
  let barrier = Atomic.make nd in
  let max = (ninit + (nd * nt))*2 in
  let pq = create max 0 in
  for i = 0 to ninit - 1 do
    add pq i i
  done;

  let work _i =
    Atomic.decr barrier;
    while Atomic.get barrier <> 0 do
      Domain.cpu_relax ()
    done;

    for _i = 0 to nt - 1 do
      let i = Random.int (max*2) in
      add pq i i
    done
  in

  let t1 = Unix.gettimeofday () in
  let domains = Array.init nd (fun i -> Domain.spawn (fun () -> work i)) in
  Array.iter Domain.join domains;
  let t2 = Unix.gettimeofday () in
  t2 -. t1

let _ =
  Random.self_init ();
  let time = test 1_000_00 2 100_000 in
  Format.printf "Result : %f@." time