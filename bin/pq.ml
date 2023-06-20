open MulticamlDS;;
Random.self_init ();
let temp = ref 0 in
for _ = 1 to 4 do
  let len = 5_000 in
  let bound = 30_000_000 in
  let base = 100_000 in
  temp := (!temp + 2);
  let num_domains = !temp in
  let queue = Priority_queue.create 5_000_000 42 in
  for _ = 1 to base do
    let x = Random.int bound in
    Priority_queue.add queue x x
  done;

  let start_time = Unix.gettimeofday () in

  (*let tt_cntr = ref 0 in*)
  let dom_list =
    Array.init num_domains (fun _ ->
        Domain.spawn (fun () ->
            for _ = 1 to len do
              let relt = Random.int bound in
              (* let op_st = Unix.gettimeofday () in *)
              Priority_queue.add queue relt relt
              (* let op_et = Unix.gettimeofday () in
                let t_t = op_et -. op_st in
                if t_t > 0.00005 then incr tt_cntr *)
            done))
  in
  for i = 0 to num_domains - 1 do
    Domain.join dom_list.(i)
  done;

  let end_time = Unix.gettimeofday () in
  Format.printf "Completed in %f\n" (end_time -. start_time);
  (*Format.printf "%d ops\n" !tt_cntr;*)
  (* Testing property *)
  if Priority_queue.get_len queue = (base + num_domains * len) then print_endline "success"
  else print_endline "failure"
done
