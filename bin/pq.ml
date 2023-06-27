open MulticamlDS;;
Random.self_init ();
let temp = ref 0 in
for _ = 1 to 8  do
  let len = 10_000 in
  let bound = 30_000_000 in
  let base = 100_000 in
  temp := (!temp + 1);
  let num_domains = !temp in
  let queue = Priority_queue.create 500_000 42 in
  for _ = 1 to base do
    let x = Random.int bound in
    ignore @@ Priority_queue.add queue x x
  done;

  let start_time = Unix.gettimeofday () in

  let dom_list =
    Array.init num_domains (fun _ ->
        Domain.spawn (fun () ->
            (*let tt_cntr = ref 0.0 in
            let tt_swap = ref 0 in
            let total = ref 0 in*)
            for _ = 1 to len do
              (*let relt = Random.int bound in
              Priority_queue.add queue relt relt;*)
              ignore @@ Priority_queue.remove_min queue
              (*let op_st = Unix.gettimeofday () in 
              let swap = Priority_queue.add queue relt relt in
              let op_et = Unix.gettimeofday () in
              total := (!total + swap);
              let t_t = op_et -. op_st in
              if swap > 8 then (tt_cntr := (!tt_cntr +. t_t); incr tt_swap)*)
            done;))
            (*Format.printf "Total swaps %d\n" !total;
            Format.printf "Costly %f swaps %d\n" !tt_cntr !tt_swap))*)
  in
  for i = 0 to num_domains - 1 do
    Domain.join dom_list.(i)
  done;

  let end_time = Unix.gettimeofday () in
  Format.printf "Completed %d in %f\n" num_domains (end_time -. start_time);
  (* Testing property *)
  Format.printf "%d\n" (Priority_queue.get_len queue)
done
