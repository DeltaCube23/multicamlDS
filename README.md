# multicamlDS

Collection of lock based data structures for Multicore OCaml

1) `Ms_queue` - With separate head and tail lock.
2) `Bounded_queue` - Improved version of michael scott queue with fixed capacity that waits till queue is not empty before popping and queue is not full before pushing.
3) `Fine_list` - Fine Grained List based on Optimistic Synchronization.
4) `Priority_queue` - Fine grained priority queue based on heap structure
5) `Double_linked_list` - Fine grained list that support O(1) deletion and insertion to the left and right of a node
