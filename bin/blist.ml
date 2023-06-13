open MulticamlDS;;

Random.self_init ();
let len = 2000 in
let num_domains = 8 in
let glock = Mutex.create () in
let list = Basic_list.create 42 in
for _ = 1 to 10_000 do
  let relt = Random.int 30_000_000 in
  ignore @@ Basic_list.add list relt
done;

let start_time = Unix.gettimeofday () in

let dom_list =
  Array.init num_domains (fun _ ->
      Domain.spawn (fun () ->
          for _ = 1 to len do
            let relt = Random.int 30_000_000 in
            Mutex.lock glock;
            ignore @@ Basic_list.add list relt;
            Mutex.unlock glock
          done))
in
for i = 0 to num_domains - 1 do
  Domain.join dom_list.(i)
done;

let end_time = Unix.gettimeofday () in
Format.printf "Completed in %f\n" (end_time -. start_time)
