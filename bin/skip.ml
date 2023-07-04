open MulticamlDS;;

Random.self_init ();

(*let sl = Skiplist.create ~max_height:20 () in
  for i = 1 downto 100_000 do
    ignore @@ Skiplist.add sl i
  done;

  for _ = 1 to 5 do
    let num = Skiplist.find_mark_min sl in
    Printf.printf "%d\n%!" num;
    ignore @@ Skiplist.remove sl num
  done;

  for i = 1 to 10 do
    if Skiplist.contains sl i then print_endline "success"
    else print_endline "failure"
  done *)
let temp = ref 0 in
let bound = 30_000_000 in
let ops = 10_000 in
for _ = 1 to 8 do
  let sl = Skiplist.create ~max_height:20 () in
  for _ = 1 to 100_000 do
    let x = Random.int bound in
    ignore @@ Skiplist.add sl x
  done;
  temp := !temp + 1;
  let num_domains = !temp in
  let start_time = Unix.gettimeofday () in
  let dom_list =
    Array.init num_domains (fun _ ->
        Domain.spawn (fun () ->
            for _ = 1 to ops do
              (*let ele = Random.int bound in
                ignore @@ Skiplist.add sl ele;*)
              let num = Skiplist.find_mark_min sl in
              ignore @@ Skiplist.remove sl num
            done))
  in
  for i = 0 to num_domains - 1 do
    Domain.join dom_list.(i)
  done;

  let end_time = Unix.gettimeofday () in
  Format.printf "Completed in %d domains in %f\n%!" num_domains
    (end_time -. start_time)
done
