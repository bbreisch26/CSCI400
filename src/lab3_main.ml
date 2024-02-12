open Testing
open Lab3_exp
open Lab3_rbtree

let main () =
  print_string "Running Lab 3\n";
  print_string "=============\n";


  (* Expressions *)
  Testing.print_tests Lab3_exp.rpn_eval_tests;
  Testing.print_tests Lab3_exp.exp_string_tests;
  Testing.print_tests Lab3_exp.exp_eval_tests;
  Testing.print_tests Lab3_exp.exp_rpn_tests;

  (* Red-Black Trees *)
  Testing.print_tests Lab3_rbtree.rbt_is_invariant_int_tests;
  Testing.print_tests Lab3_rbtree.rbt_is_invariant_str_tests;
  Testing.print_tests Lab3_rbtree.rbt_is_sorted_int_tests;
  Testing.print_tests Lab3_rbtree.rbt_is_sorted_str_tests;
  Testing.print_tests Lab3_rbtree.rbt_search_int_tests;
  Testing.print_tests Lab3_rbtree.rbt_search_str_tests;
  Testing.print_tests Lab3_rbtree.rbt_balance_int_tests;
  Testing.print_tests Lab3_rbtree.rbt_balance_str_tests;
  Testing.print_tests Lab3_rbtree.rbt_insert_int_tests;
  Testing.print_tests Lab3_rbtree.rbt_insert_str_tests;
  Testing.print_tests Lab3_rbtree.rbt_map_inorder_tests;
  Testing.print_tests Lab3_rbtree.rbt_map_revorder_tests;

  (* Ropes *)
  Testing.print_tests Lab3_rope.to_string_tests;
  Testing.print_tests Lab3_rope.length_tests;
  Testing.print_tests Lab3_rope.height_tests;

  Testing.print_tests Lab3_rope.rot_right_tests;
  Testing.print_tests Lab3_rope.rot_right_left_tests;
  Testing.print_tests Lab3_rope.rot_left_tests;
  Testing.print_tests Lab3_rope.rot_left_right_tests;

  Testing.print_tests Lab3_rope.create_tests;
  Testing.print_tests Lab3_rope.bal_tests;
  Testing.print_tests Lab3_rope.cat_tests;

  Testing.print_tests Lab3_rope.get_tests;
  Testing.print_tests Lab3_rope.sub_tests;
;;

main ()
