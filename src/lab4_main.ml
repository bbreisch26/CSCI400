open Testing
open Lab4_lazy
open Lab4_stream
open Lab4_queue

let main () =
  print_string "Running Lab 4\n";
  print_string "=============\n";

  print_string "Lazy Evaluation\n";
  print_string "---------------\n";
  Testing.print_tests Lab4_lazy.lazy_delay_force_tests;
  Testing.print_tests Lab4_lazy.lazy_memoize_tests;

  print_string "Streams\n";
  print_string "-------\n";
  Testing.print_tests Lab4_stream.stream_fromto_list_tests;
  Testing.print_tests Lab4_stream.stream_seed_tests;
  Testing.print_tests Lab4_stream.stream_seed2_tests;
  Testing.print_tests Lab4_stream.stream_append_tests;
  Testing.print_tests Lab4_stream.stream_reverse_tests;
  Testing.print_tests Lab4_stream.stream_nth_tests;
  Testing.print_tests Lab4_stream.stream_prefix_tests;
  Testing.print_tests Lab4_stream.stream_suffix_tests;
  Testing.print_tests Lab4_stream.stream_map_tests;
  Testing.print_tests Lab4_stream.stream_map2_tests;
  Testing.print_tests Lab4_stream.stream_filter_tests;

  print_string "Queues\n";
  print_string "-------\n";
  Testing.print_tests Lab4_queue.queue_fromto_list_tests;
  Testing.print_tests Lab4_queue.queue_fromto_lists_tests;
  Testing.print_tests Lab4_queue.queue_head_tests;
  Testing.print_tests Lab4_queue.queue_check_tests;
  Testing.print_tests Lab4_queue.queue_snoc_tests;
  Testing.print_tests Lab4_queue.queue_tail_tests;

;;

main ()
