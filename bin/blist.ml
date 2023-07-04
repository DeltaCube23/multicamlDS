open MulticamlDS;;

Random.self_init ();
for num_domains = 1 to 8 do
  let len = 20_000 / num_domains in

  (*amount of work by each domain*)
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
  Format.printf "Completed %d domains in %f\n" num_domains
    (end_time -. start_time)
done
