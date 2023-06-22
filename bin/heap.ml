module E = struct
  type t = int

  let compare = Stdlib.compare
end

module H = Binary_heap.Make (E)

let dummy = 1729
let bound = 30_000_000
let ops = 10_000

let () =
  Random.self_init ();
  let temp = ref 0 in
  for _ = 1 to 4 do
    let h = H.create ~dummy 5_000_000 in
    let glock = Mutex.create () in
    for _ = 1 to 10_000 do
      let x = Random.int bound in
      H.add h x
    done;
    temp := (!temp + 2);
    let num_domains = !temp in
    let start_time = Unix.gettimeofday () in
    let dom_list =
      Array.init num_domains (fun _ ->
          Domain.spawn (fun () ->
              for _ = 1 to ops do
                let ele = Random.int bound in
                Mutex.lock glock;
                H.add h ele;
                Mutex.unlock glock
                (*Mutex.lock glock;
                  ignore @@ H.pop_minimum h;
                  Mutex.unlock glock;*)
              done))
    in
    for i = 0 to num_domains - 1 do
      Domain.join dom_list.(i)
    done;

    let end_time = Unix.gettimeofday () in
    Format.printf "Completed in %f\n" (end_time -. start_time);

    Format.printf "%d\n" (H.length h)
  done
