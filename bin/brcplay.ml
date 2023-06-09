open MulticamlDS;;

let cntr = Brc.create () in 
for _ = 1 to 7 do
Printf.printf "%i\n%!" (Brc.increment cntr);
done;
for _ = 1 to 7 do
Printf.printf "%i\n%!" (Brc.decrement cntr);
done