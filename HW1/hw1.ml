open List ;;

let rec belong x y = match x with
  | [] -> false
  | a::b -> if a = y then true else belong b y ;;


let rec subset x y = match x with
  | [] -> true
  | a::b -> if belong y a = true then subset b y else false ;;


let equal_sets a b = if subset a b && subset b a then true else false ;;


let rec set_union (a:'e list)  (b:'e list) = match b with
  | [] -> a
  | x::y -> if belong a x = false then set_union (a @ [x]) y else set_union a y;;


let rec rec_set_intersection (a:'e list) (b:'e list) (c:'e list) = match b with
  | [] -> c
  | x::y -> if belong a x = false then rec_set_intersection a y c else rec_set_intersection a y (c @ [x]) ;;

let set_intersection (a:'e list)  (b:'e list) = rec_set_intersection a b [] ;;


let rec rec_set_diff (a:'e list) (b:'e list) (c:'e list) = match a with
  | [] -> c
  | x::y -> if belong b x = false then rec_set_diff y b (x::c) else rec_set_diff y b c ;;

let set_diff (a:'e list) (b:'e list) = rec_set_diff a b [] ;;


let rec computed_fixed_point eq f x =
    if eq (f x) x = true then x else computed_fixed_point eq f (f x) ;;


type ('nonterminal, 'terminal) symbol =
      | N of 'nonterminal
      | T of 'terminal ;;


let rec non_terminal rule symbols = match rule with
    | [] -> symbols
    | x::y -> match x with
            | N a -> non_terminal y (a::symbols)
            | T b -> non_terminal y symbols ;;


let rec rec_add_rules symbol rules new_rules = match rules with
    | [] -> new_rules
    | x::y -> match x with
              | (a,b) -> if a = symbol then rec_add_rules symbol y (x::new_rules) else rec_add_rules symbol y new_rules ;;

let add_rules symbol rules = rec_add_rules symbol rules [] ;;


let rec rec_add_non_terminals symbol rules non_terminal_list = match rules with
    | [] -> non_terminal_list
    | x::y -> match x with
              | (a,b) -> if a = symbol
                                    then rec_add_non_terminals symbol y ((non_terminal b []) @ non_terminal_list)
                                    else rec_add_non_terminals symbol y non_terminal_list ;;

let add_non_terminals symbol rules = rec_add_non_terminals symbol rules [];;


let rec give_list rules non_terminals final_list seen= match non_terminals with
  | [] -> final_list
  | a::b -> if belong seen a = false then give_list rules ((add_non_terminals a rules) @ b) ((add_rules a rules) @ final_list) (a::seen)
                                      else give_list rules b final_list seen ;;

let filter_reachable (symbol, rules) = ( symbol  , set_intersection ( give_list rules (symbol::[]) [] []) rules ) ;;
