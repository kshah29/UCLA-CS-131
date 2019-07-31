open List ;;

(* Question 1 *)
type ('nonterminal, 'terminal) symbol =
      | N of 'nonterminal
      | T of 'terminal ;;

let rec convert_all_rules rules symbol new_rules = match rules with
      | [] -> new_rules
      | (a,b)::tl -> if symbol = a then convert_all_rules tl symbol (b::new_rules)
                  else convert_all_rules tl symbol new_rules;;

let convert_grammar (start, rules) = (start, fun symbol -> convert_all_rules rules symbol []) ;;

(* Question 2 *)
type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal ;;


(* Type of argument: (tree_list:(string, int) parse_tree list) *)
let rec parse_list tree_list = match tree_list with
  | [] -> []
  | hd::tl -> match hd with
            | Node (a,b) -> ( parse_list b ) @ (parse_list tl)
            | Leaf l -> [l] @ (parse_list tl) ;;

let parse_tree_leaves b =  parse_list [b] ;;


(* Question 3 *)
let rec match_non start rules_fn rule_list accept frag = match rule_list with
         | [] -> None
         | rule::tail -> match (match_rule rules_fn rule accept frag) with
                       | Some x -> Some x
                       | None -> match_non start rules_fn tail accept frag

   and

     match_rule rules_fn rule accept frag = match rule with
           | [] -> accept frag
           | rule_head::rule_tail -> match frag with
                       | [] -> None
                       | frag_head::frag_tail -> match rule_head with
                                   | N non_terminal ->  match_non non_terminal rules_fn (rules_fn non_terminal) (match_rule rules_fn rule_tail accept) frag
                                   | T terminal -> if frag_head = terminal then match_rule rules_fn rule_tail accept frag_tail
                                                   else None ;;

let make_matcher (start, rules_fn) accept frag = match_non start rules_fn (rules_fn start) accept frag ;;

(* Question 4 *)
let rec parse_match_non start rules_fn rule_list accept deduction_rules frag = match rule_list with
      | [] -> None
      | rule::tail -> match (parse_match_rule rules_fn rule accept (deduction_rules @ [start, rule]) frag) with
                    | Some x -> Some x
                    | None -> parse_match_non start rules_fn tail accept deduction_rules frag

and

  parse_match_rule rules_fn rule accept deduction_rules frag = match rule with
        | [] -> accept deduction_rules frag
        | rule_head::rule_tail -> match frag with
                    | [] -> None
                    | frag_head::frag_tail -> match rule_head with
                                | N non_terminal ->  parse_match_non non_terminal rules_fn (rules_fn non_terminal) (parse_match_rule rules_fn rule_tail accept) deduction_rules frag
                                | T terminal -> if frag_head = terminal then parse_match_rule rules_fn rule_tail accept deduction_rules frag_tail
                                                else None ;;

let parse_make_matcher (start, rules_fn) accept frag = parse_match_non start rules_fn (rules_fn start) accept [] frag ;;


let parse_tree_acceptor deduction_rules frag = Some(deduction_rules, frag) ;;

let rec give_rule_list deduction_rules = match deduction_rules with
                | [] -> []
                | (start, rule)::tail -> [rule] @ (give_rule_list tail) ;;


let rec number_nonterminals rule = match rule with
                                    | [] -> 0
                                    | head::tail -> match head with
                                                  | T terminal -> number_nonterminals tail
                                                  | N non_terminal -> 1 + number_nonterminals tail;;


let rec number_rules non_terminals_left rules = if non_terminals_left > 0 then match rules with
                                                    | [] -> 0
                                                    | head::tail -> (number_rules (non_terminals_left - 1 + (number_nonterminals head)) tail) + 1
                                            else  0;;


let rec remove_from_list tail_rules num = if num > 0 then match tail_rules with
                                              | [] -> []
                                              | head::tail -> remove_from_list tail (num - 1)
                                          else tail_rules ;;


let rec make_tree head_rules tail_rules = match head_rules with
    | [] -> []
    | hd_rule::tl_rule -> match hd_rule with
            | T terminal -> ( [Leaf terminal] @ (make_tree tl_rule tail_rules) )
            | N non_terminal -> match tail_rules with
                | [] -> []
                | hd_remain::tl_remain -> [Node (non_terminal, (make_tree hd_remain tl_remain))] @ (make_tree tl_rule (remove_from_list tail_rules (number_rules 1 tail_rules))) ;;


let make_parser (start, rules_fn) frag = let matcher_output = (parse_make_matcher (start, rules_fn) parse_tree_acceptor frag) in match matcher_output with
                                        	| Some (deduction_rules, frag) -> let rules = give_rule_list deduction_rules in
                                                                  Some ( Node ( (fst (hd deduction_rules)), (make_tree (hd rules) (tl rules))) )
                                          | None -> None ;;
