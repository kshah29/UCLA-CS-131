
let accept_all string = Some string ;;
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x ;;

type giant_nonterminals =
  | Conversation | Sentence | Grunt | Snore | Shout;;

let giant_grammar =
( Conversation,
        function
          | Snore -> [ [T "ZZZ"] ]
          | Conversation -> [ [N Sentence; T ","; N Conversation];
                              [N Snore]; ]
          | Sentence -> [ [N Grunt];
                          [N Shout] ]
          | Grunt ->  [ [T"khrgh"] ]
          | Shout -> [ [T"aooogah!"] ] ) ;;



let make_matcher_test0 =
  ((make_matcher giant_grammar accept_all ["ouch"]) = None) ;;

let make_matcher_test1 =
  ((make_matcher giant_grammar accept_all ["ZZZ"])
   = Some [] ) ;;

let make_matcher_test2 =
     ((make_matcher giant_grammar accept_empty_suffix ["khrgh"; "aooogah!"; ","; "ZZZ"; "ZZZ"]) = None) ;;

let make_parser_test_0 = ((make_parser giant_grammar ["aooogah!"; ","; "ZZZ"]) =
                Some
                (Node (Conversation,
                              [Node (Sentence,
                                      [Node (Shout, [Leaf "aooogah!"])]);
                              Leaf ",";
                              Node (Conversation,
                                        [Node (Snore, [Leaf "ZZZ"])])]))    ) ;;

let make_parser_test_1 = match make_parser giant_grammar ["aooogah!"; ","; "ZZZ"] with
    | Some tree -> parse_tree_leaves tree = ["aooogah!"; ","; "ZZZ"]
    | _ -> false ;;

let make_parser_test_2 = ( (make_parser giant_grammar ["ZZZ"]) =
                    Some (Node (Conversation, [Node (Snore, [Leaf "ZZZ"])]))  ) ;;
