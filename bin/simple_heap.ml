(*open MulticamlDS;;
  Random.self_init ();
  let queue = Basic_heap.create 100 in
  for _ = 1 to 100 do
    let x = Random.int 1000 in
    Format.printf "%d\n" x;
    ignore @@ Basic_heap.push queue x
  done;

  for _ = 1 to 100 do
    let y = Basic_heap.pop queue in
    Format.printf "%d\n" y
  done*)

open MulticamlDS;;

Random.self_init ();
let glock = Mutex.create () in
let temp = ref 0 in
for _ = 1 to 8 do
  let len = 10_000 in
  let bound = 30_000_000 in
  let base = 10_000 in
  temp := !temp + 1;
  let num_domains = !temp in
  let queue = Basic_heap.create 100_000 in
  for _ = 1 to base do
    let x = Random.int bound in
    ignore @@ Basic_heap.push queue x
  done;

  let start_time = Unix.gettimeofday () in

  let dom_list =
    Array.init num_domains (fun _ ->
        Domain.spawn (fun () ->
            for _ = 1 to len do
              let relt = Random.int bound in
              Mutex.lock glock;
              Basic_heap.push queue relt;
              Mutex.unlock glock
            done))
  in
  for i = 0 to num_domains - 1 do
    Domain.join dom_list.(i)
  done;

  let end_time = Unix.gettimeofday () in
  Format.printf "Completed %d in %f\n" num_domains (end_time -. start_time)
done
