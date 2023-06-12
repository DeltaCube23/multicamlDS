module E = struct
  type t = int
  let compare = Stdlib.compare
end
module H = Binary_heap.Make(E)

let dummy = 1729

let () = 
  Random.self_init ();
  let h = H.create ~dummy 5_000_000 in
  let glock = Mutex.create () in
  for i = 1 to 500_000 do
    H.add h i;
  done;
  let start_time = Unix.gettimeofday () in
  let d1 = Domain.spawn (fun () -> for _ = 1 to 500_000 do 
    let ele = Random.int 300_000 in
    Mutex.lock glock;
    H.add h ele;
    Mutex.unlock glock;
  done;) in
  let d2 = Domain.spawn (fun () -> for _ = 1 to 500_000 do 
    let ele = Random.int 300_000 in
    Mutex.lock glock;
    H.add h ele;
    Mutex.unlock glock;
  done;) in
  let d3 = Domain.spawn (fun () -> for _ = 1 to 500_000 do 
    let ele = Random.int 300_000 in
    Mutex.lock glock;
    H.add h ele;
    Mutex.unlock glock;
  done;) in
  let d4 = Domain.spawn (fun () -> for _ = 1 to 500_000 do 
    let ele = Random.int 300_000 in
    Mutex.lock glock;
    H.add h ele;
    Mutex.unlock glock;
  done;) in
  let d5 = Domain.spawn (fun () -> for _ = 1 to 500_000 do 
    let ele = Random.int 300_000 in
    Mutex.lock glock;
    H.add h ele;
    Mutex.unlock glock;
  done;) in
  let d6 = Domain.spawn (fun () -> for _ = 1 to 500_000 do 
    let ele = Random.int 300_000 in
    Mutex.lock glock;
    H.add h ele;
    Mutex.unlock glock;
  done;) in
  let d7 = Domain.spawn (fun () -> for _ = 1 to 500_000 do 
    let ele = Random.int 300_000 in
    Mutex.lock glock;
    H.add h ele;
    Mutex.unlock glock;
  done;) in
  let d8 = Domain.spawn (fun () -> for _ = 1 to 500_000 do 
    let ele = Random.int 300_000 in
    Mutex.lock glock;
    H.add h ele;
    Mutex.unlock glock;
  done;) in
  Domain.join d1;
  Domain.join d2;
  Domain.join d3;
  Domain.join d4;
  Domain.join d5;
  Domain.join d6;
  Domain.join d7;
  Domain.join d8;
  let end_time = Unix.gettimeofday () in
  Format.printf "Completed in %f\n" (end_time -. start_time);

Format.eprintf "%d@." (H.length h)