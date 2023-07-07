(*
skiplist is currently unique elements only, to change it to have duplicates
there needs to be 2 minor changes
- in the add method need to remove if key is found return false part
- in the remove method if iMarkedIt failed instead of returning false you need to retry to delete that key again
  this case can happen if 2 threads have the same key to delete and they have logically marked different ones,
  but while iteratively checking they both try to physically remove the same one   
*)