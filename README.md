# multicamlDS

Collection of lock based data structures for Multicore OCaml

1) Ms_queue - With separate head and tail lock.
2) Bounded_queue - Improved version of michael scott queue with fixed capacity that waits till queue is not empty before popping and queue is not full before pushing.
3) Fine_list - Fine Grained List based on Optimistic Synchronization.
