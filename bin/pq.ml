Random.self_init ();
for _ = 1 to 100 do
  let x = Random.int 1000 in
  Printf.printf "%d\n" x
done
