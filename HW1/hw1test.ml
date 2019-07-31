let my_subset_test0 = subset [1;2;2] [2;1]  ;;
let my_subset_test1 = subset [] [2;1]  ;;
let my_subset_test0 = not (subset [3;2] [2;1])  ;;

let my_equal_sets_test0 = equal_sets [1;2;2] [2;1]  ;;
let my_equal_sets_test1 = equal_sets [3;2;1] [2;1;3]  ;;
let my_equal_sets_test2 = not (equal_sets [3;2;2] [2;1])  ;;

let my_set_union_test0 = equal_sets (set_union [1] [2]) [2;1]  ;;
let my_set_union_test1 = equal_sets (set_union [] [3;1;2]) [2;1;3]  ;;
let my_set_union_test2 = equal_sets (set_union [1;4] [2]) [2;1;4]  ;;

let my_set_intersection_test0 = equal_sets (set_intersection [1] [2]) []  ;;
let my_set_intersection_test1 = equal_sets (set_intersection [1;2;3;4] [2;1;5;6]) [1;2]  ;;
let my_set_intersection_test2 = equal_sets (set_intersection [1;2;3;4] [4;5;6;7]) [4]  ;;

let my_set_diff_test0 = equal_sets (set_diff [1] [2]) [1]  ;;
let my_set_diff_test1 = equal_sets (set_diff [1;2;3] [2]) [1;3]  ;;
let my_set_diff_test2 = equal_sets (set_diff [1;2] [2;1]) []  ;;

let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x *. 0.1) 1000000. = 0. ;;
let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> x *. 10.) 1. = infinity ;;
let my_computed_fixed_point_test2 = computed_fixed_point (=) (fun x -> x /. 0.1) 10. = infinity ;;

type weird_nonterminals =
  | S | A | B | C  ;;

let weird_rules =
  [S, [T"ZZZ"];
   S, [N A];
   A, [T"fff"];
   A, [T"ggg!"];
   B, [N A];
   B, [N B];
   C, [N A];
   C, [N B];
   C, [N C; T","; N S]] ;;

let weird_grammar = S, weird_rules ;;

let my_filter_reachable_test0 = filter_reachable weird_grammar = (S, [(S, [T "ZZZ"]); (S, [N A]); (A, [T "fff"]); (A, [T "ggg!"])]) ;;

let my_filter_reachable_test1 = filter_reachable ( C, weird_rules) = ( C, weird_rules) ;;

let my_filter_reachable_test2 = filter_reachable ( A, weird_rules) = (A, [(A, [T "fff"]); (A, [T "ggg!"])]) ;;
