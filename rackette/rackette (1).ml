#use "read.ml" ;;
#use "CS17setup.ml" ;;

(* these type definitions are in comments
 * because they've already been defined in read.ml
 *
 * type raw_program = string
 *
 * type concrete_program_piece =
 *   Number of int
 * | Symbol of string
 * | List of concrete_program_piece list
 *
 * type concrete_program = concrete_program_piece list
 *)

(* an Rackette identifier *)
type identifier = ID of string ;;
(* (ID ("+"), (ID ("-")) *)

(* a Rackette expression *)
type expression =
| NumE of int
(* NumE 12, NumE 15 *)
| IdentE of identifier
(* IdentE (ID ("+")), IdentE (ID ("-")) *)
| AndE of expression * expression
(* AndE (IdentE (ID ("true")), IdentE (ID ("false")))  *)
| OrE of expression * expression
(* OrE (IdentE (ID ("true")), IdentE (ID ("false"))) *)
| IfE of expression * expression * expression
(* IfE (IdentE (ID ("true")), NumE 1, NumE 2) *)
| CondE of (expression * expression) list
(* CondE [(IdentE (ID ("true")), NumE 1);(IdentE (ID ("true")), NumE 3)]  *)
| QuoteE of concrete_program_piece
(* QuoteE Number 17, QuoteE Symbol "+" *)
| LambdaE of identifier list * expression
(* LambdaE [ID ("x"); ID ("y")], (IfE (IdentE (ID ("true")),
IdentE (ID ("false"))) *)
| ApplicationE of expression list ;;
(* (ApplicationE [IdentE (ID ("+")); NumE 5; NumE 6]) *)

type definition = identifier * expression ;;

(* a piece of Rackette that can be processed:
 * either a definition or an expression *)
type abstract_program_piece =
| Definition of definition
(* Definition (ID("x"), NumE 15) *)
| Expression of expression ;;
(* Expression (NumE 15), Expression (QuoteE (Symbol "no")) *)

(* a representation of a Rackette program --
 * any number of pieces *)
type abstract_program = abstract_program_piece list ;;
(* [Expression (AndE); Expression (IdentE (ID ("true"));
Expression IdentE (ID ("false"))] *)
(* [Definition (ID ("x"), NumE 15);
Expression (IfE (ApplicationE [(IdentE (ID ("zero?"))); IdentE (ID ("x"))]),
Expression (IdentE (ID ("true")), Expression IdentE (ID ("false")))) *)


(* a Rackette value: the result of
 * evaluating a Rackette expression *)
type value =
| VNum of int
(* VNum 17, VNum 15, VNum 1 *)
| VBool of bool
(* VBool true, VBool false *)
| VSymbol of string
(* VSymbol "yes", VSymbol "no" *)
| VList of value list
(* VList [],
   VList [VNum 0],
   VList [VBool true, VBool false],
   VList [VSymbol "hi", VSymbol "hey", VSymbol "hello"] *)
| VBuiltin of string * (value list -> value)
(* VBuiltin ("<builtin-proc-+>", plus_func),
   VBuiltin ("<builtin-proc-->", sub_func) *)
| VClosure of identifier list * expression * environment
and binding = identifier * value
and environment = binding list
(* VClosure (initial_tle_id_list, (ApplicationE [IdentE (ID ("+"));
NumE 5; NumE 6]), initial_tle) *)

let plus_func : (value list) -> value = function
  VNum a :: VNum b :: [] -> VNum (a + b)
  | _ -> failwith "Expects two arguments" ;;

let plus : value = VBuiltin ("<builtin-proc-+>", plus_func) ;;

(* test cases for plus *)
check_expect (plus_func ([VNum 1; VNum 3])) (VNum 4) ;;
check_error (function () -> (plus_func ([VNum 1; VNum 3; VNum 4])))
                            "Expects two arguments" ;;

let sub_func : (value list) -> value = function
  VNum a :: VNum b :: [] -> VNum (a - b)
  | _ -> failwith "Expects two arguments" ;;

let sub : value = VBuiltin ("<builtin-proc-->", sub_func) ;;

check_expect (sub_func [VNum 3; VNum 1]) (VNum 2) ;;
check_error (function () -> (sub_func [VNum 1])) "Expects two arguments" ;;

let mult_func : (value list) -> value = function
  VNum a :: VNum b :: []-> VNum (a * b)
  | _ -> failwith "Expects two arguments" ;;

let mult : value = VBuiltin ("<builtin-proc-*>", mult_func) ;;

check_expect (mult_func [VNum 4; VNum 5]) (VNum 20) ;;
check_error (function () -> (mult_func ([VNum 1]))) "Expects two arguments" ;;

let div_func : (value list) -> value = function
  VNum a :: VNum b :: [] -> VNum (a / b)
| _ -> failwith "Expects two arguments" ;;

let div : value = VBuiltin ("<builtin-proc-/>", div_func) ;;

(* test cases division *)
check_expect (div_func [VNum 4; VNum 4]) (VNum 1) ;;
check_error (function () -> (div_func ([VNum 1]))) "Expects two arguments" ;;

let less_than_func : (value list) -> value = function
  VNum a :: VNum b :: [] -> VBool (a < b)
  | _ -> failwith "Expects two arguments" ;;

let less_than : value = VBuiltin ("<builtin-proc-<>", less_than_func) ;;

(* test cases for less_than *)
check_expect (less_than_func [VNum 4; VNum 5]) (VBool true) ;;
check_expect (less_than_func [VNum 4; VNum 3]) (VBool false) ;;
check_error (function () -> (less_than_func ([VNum 1])))
                            "Expects two arguments" ;;

let zero_ques_func : (value list) -> value = function
  [VNum a] -> VBool (a = 0)
  | _ -> failwith "Expects a list of one VNum" ;;

let zero_ques : value = VBuiltin ("<builtin-proc-zero?>", zero_ques_func) ;;

(* test cases zero? *)
check_expect (zero_ques_func [VNum 0]) (VBool true) ;;
check_expect (zero_ques_func [VNum 4]) (VBool false) ;;
check_error (function () -> (zero_ques_func ([VNum 1; VNum 2])))
"Expects a list of one VNum" ;;

let empty_ques_func : (value list) -> value = function
  [VList []]  -> VBool true
  | [VList _] -> VBool false
  | _ -> failwith "Expects a list" ;;

let empty_ques : value = VBuiltin ("<builtin-proc-empty?>", empty_ques_func) ;;

(* test cases for empty? *)
check_expect (empty_ques_func [VList []]) (VBool true) ;;
check_expect (empty_ques_func [VList [VBool true]]) (VBool false) ;;
check_error (function () -> (empty_ques_func [VBool true])) "Expects a list" ;;

let cons_ques_func : (value list) -> value = function
   [VList []] -> VBool false
   | [VList _] -> VBool true
   | _ -> failwith "Expects a list" ;;

let cons_ques : value = VBuiltin ("<builtin-proc-cons?>", cons_ques_func) ;;

(* test cases for cons? *)
check_expect (cons_ques_func [VList []]) (VBool false) ;;
check_expect (cons_ques_func [VList [VBool true]]) (VBool true) ;;
check_error (function () -> (cons_ques_func [VBool true])) "Expects a list" ;;

let cons_func : (value list) -> value = function
  [x; VList y] -> VList (x :: y)
  | _ -> failwith "Expects a single element and a list" ;;

let cons : value = VBuiltin ("<builtin-proc-cons>", cons_func) ;;

(* test cases for cons *)
check_expect (cons_func [(VBool false); VList []]) (VList [VBool false]) ;;
check_error (function () -> (cons_func [(VBool true); (VBool false);
          VList [VBool true]])) "Expects a single element and a list" ;;

let car_func : (value list) -> value = function
   [VList []] -> failwith "Expects a non-empty list"
   | [VList (hd :: tl)] -> hd
   | _ -> failwith "failed to give a list" ;;

let car : value = VBuiltin ("<builtin-proc-car>", car_func) ;;

let cdr_func : (value list) -> value = function
 [] -> failwith "Expects a non-empty list"
 | [VList (h :: t)] -> VList t
 | _ -> failwith "failed to give a list" ;;

let cdr : value = VBuiltin ("<builtin-proc-cdr>", cdr_func) ;;

let equal_func: (value list) -> value = function
  [] -> failwith "Expects a non-empty list"
  | [VNum a; VNum b] -> VBool (a = b)
  | _ -> failwith "Expects a VNum, received something else" ;;

let equal : value = VBuiltin ("<builtin-proc-=>", equal_func) ;;

let equal_ques_func : (value list) -> value = function
  [] -> failwith "Expects a non-empty list"
  | [VNum a; VNum b] -> VBool (a = b)
  | [VBool a; VBool b] -> VBool (a = b)
  | [VSymbol a; VSymbol b] -> VBool (a = b)
  | [VList a; VList b] -> VBool (a = b)
  | _ -> failwith "Expects a two-element list" ;;

let equal_ques : value = VBuiltin ("<bultin-proc-equal?>", equal_ques_func) ;;

let remainder_func : (value list) -> value = function
  [] -> failwith "Expects a non-empty list"
  | [VNum a; VNum b] -> VNum (a mod b)
  | _ -> failwith "Expects two VNums" ;;

let remainder : value =
                    VBuiltin ("<builtin-proc-remainder>", remainder_func) ;;

check_expect (remainder_func [VNum 10; VNum 5]) (VNum 0) ;;
check_expect (remainder_func [VNum 10; VNum 4]) (VNum 2) ;;
check_error (function () -> (remainder_func [VBool false]))
                                                "Expects two VNums" ;;
check_error (function () -> (remainder_func [])) "Expects a non-empty list" ;;

let not_func : (value list) -> value = function
  [] -> failwith "Expects a non-empty list"
  | [VBool (x)] -> VBool (not x)
  | _ -> failwith "Expects a singular " ;;

let my_not : value = VBuiltin ("<builtin-proc-not", not_func) ;;

check_expect (not_func [VBool true]) (VBool false) ;;
check_expect (not_func [VBool false]) (VBool true) ;;

let number_ques_func : (value list) -> value = function
  [VNum a] -> VBool true
  | [VBool _] -> VBool false
  | [VSymbol _] -> VBool false
  | [VList _] -> VBool false
  | _ -> failwith "Expected a value" ;;

let number_ques : value =
  VBuiltin ("<builtin-proc-number?", number_ques_func) ;;

check_expect (number_ques_func [VNum 2]) (VBool true) ;;
check_expect (number_ques_func
  [VSymbol "I respect you actually read all of our check_expects"])
  (VBool false) ;;
check_error (function () -> (number_ques_func [VNum 2; VNum 3]))
                            "Expected a value" ;;

let initial_tle : environment =
    [(ID "true", VBool true);
    (ID "false", VBool false);
    (ID "+", plus);
    (ID "-", sub);
    (ID "*", mult);
    (ID "/", div);
    (ID "<", less_than);
    (ID "equal?", equal_ques);
    (ID "=", equal);
    (ID "remainder", remainder);
    (ID "zero?", zero_ques);
    (ID "empty?", empty_ques);
    (ID "cons?", cons_ques);
    (ID "cons", cons);
    (ID "car", car);
    (ID "cdr", cdr);
    (ID "not", my_not);
    (ID "number?", number_ques)] ;;

(* Input: a concrete_program_piece given by read that should match to one of
the potential types in racket
Output: the concrete_program_piece now represented as a type expression *)
let rec parse_expression : concrete_program_piece -> expression = function
  Number (x) -> NumE (x)
  | Symbol (x) -> IdentE (ID (x))
  | List [Symbol ("and"); x; y] -> AndE (parse_expression x,
                                         parse_expression y)
  | List [Symbol ("or"); x; y] -> OrE (parse_expression x,
                                       parse_expression y)
  | List [Symbol ("if"); x; y; z] -> IfE (parse_expression x,
                                          parse_expression y,
                                          parse_expression z)
  | List (Symbol ("cond") :: tl) -> CondE (List.map cond_help tl)
  | List [Symbol ("quote"); x] -> QuoteE (x)
  | List [Symbol ("lambda"); List(x); cpp] ->
                                      LambdaE ((List.map (function
                                        IdentE (x) -> x
              | _ -> failwith "Unexpected error (lambda match mapped function)")
                                        (List.map parse_expression x)),
                                        (parse_expression cpp))
  | List [] -> ApplicationE []
  | List (x) -> ApplicationE (List.map parse_expression x)
  and cond_help : concrete_program_piece -> expression * expression = function
    List[x; y] -> ((parse_expression x), (parse_expression y))
    | _ -> failwith "Unexpected error (cond_help)" ;;

(* test cases parse expression *)

check_expect (parse_expression
  (List [Symbol "quote"; List [Symbol "hello"]]))
  (QuoteE (List [Symbol "hello"]) ;;
check_expect (parse_expression
  (List [Symbol "cond"; List [Symbol "true"; Number 1]]))
  (CondE [(IdentE (ID "true"), NumE 1)]) ;;
check_expect (parse_expression
  (List [Symbol "if"; Symbol "true"; Number 1; Number 2]))
  (IfE (IdentE (ID "true"), NumE 1, NumE 2)) ;;
check_expect (parse_expression
  (List [Symbol "and"; Symbol "true"; Symbol "false"]))
  (AndE (IdentE (ID "true"), IdentE (ID "false"))) ;;
check_expect (parse_expression
  (List [Symbol "or"; Symbol "true"; Symbol "false"]))
  (OrE (IdentE (ID "true"), IdentE (ID "false"))) ;;
check_expect (parse_expression (List[Symbol "lambda";
  List[Symbol "x"; Symbol "y"]; List [Symbol "+"; Symbol "x"; Symbol "y"]]))
  (LambdaE ([ID "x"; ID "y"], ApplicationE [IdentE (ID "+"); IdentE (ID "x");
  IdentE (ID "y")])) ;;
check_expect (parse_expression (List[Symbol "and";
  List [Symbol "equal?"; Number 1; Number 2];
  List [Symbol "equal?"; Number 2; Number 2]]))
  (AndE (ApplicationE [IdentE (ID "equal?"); NumE 1; NumE 2],
  ApplicationE [IdentE (ID "equal?"); NumE 2; NumE 2])) ;;
check_expect (parse_expression (Number (5))) (NumE 5) ;;
check_expect (parse_expression (Symbol ("x"))) (IdentE (ID "x")) ;;

(* TODO: write the design recipe for and implement parse_define *)
let parse_definition : concrete_program_piece -> definition = function
| List [Symbol ("define"); Symbol (x); y] -> (ID (x), parse_expression y)
| _ -> failwith "Expects a definition" ;;

check_error (function () -> parse_definition (Symbol "true"))
  "Expects a definition" ;;
check_expect (parse_definition (List [Symbol "define"; Symbol "x"; Number 2]))
   (ID ("x"), NumE 2) ;;

let parse_piece : concrete_program_piece -> abstract_program_piece = function
  cpp -> match cpp with
  | List (Symbol "define" :: _) -> Definition (parse_definition cpp)
  | _ -> Expression (parse_expression cpp) ;;

(* Input: a concrete program which is a list of concrete program pieces which
is either Number, a Symbol, or a List of concrete program pieces
Output: an abstract program which is either a definition of definitions or an
expression of expressions *)
let parse : concrete_program -> abstract_program = function
  (* this will parse all of the pieces of this program,
   * giving us a list of pieces, our abstract syntax *)
  cp -> List.map parse_piece cp ;;

let rec lookup: environment * identifier -> value option = function
  ([], _) -> None
  | ((id1, v)::tl, id2) -> if id1 == id2 then (Some v) else lookup (tl, id2) ;;

check_expect (lookup ([], (ID "x"))) None ;;
check_expect (lookup ([((ID "x"), VNum 2)], (ID "x"))) (Some (VNum 2)) ;;
check_expect (lookup ([((ID "y"), VNum 1); ((ID "x"), VNum 2)], (ID "x")))
  (Some (VNum 2)) ;;

(* TODO: write the design recipe for and implement eval *)
(* Input: takes in a top level environment, a local envrionment, and an
expression and evluates the corresponding expression depending on whether its
bounded in the corresponding environment
Output: the value of the expression depending on how its defined in the
various envrionments. *)
(* NOTE: tle is the top level environment and env is the local environment *)
let rec eval : environment * environment * expression -> value = function
  (tle, env, expr) -> match expr with
  | NumE x -> VNum x
  | IdentE x -> (match (lookup ((env @ tle), x)) with
                 None -> failwith "Symbol is not bound to a value or procedure"
                 | Some (x) -> x)
  | AndE (first, second) -> (match (eval (tle, env, first)) with
                              VBool false -> VBool false
                              | VBool true ->
                                (match (eval (tle, env, second)) with
                                  VBool x -> VBool x
          | _ -> failwith "second argument of 'and' must evaluate to a boolean")
          | _ -> failwith "first argument of 'and' must evaluate to a boolean")
  | OrE (first, second) -> (match (eval (tle, env, first)) with
                              VBool true -> VBool true
                              | VBool false ->
                                (match (eval (tle, env, second)) with
                                  VBool x -> VBool x
          | _ -> failwith "second argument of 'or' must evaluate to a boolean")
          | _ -> failwith "first argument of 'or' must evaluate to a boolean")
  | IfE (cond, true_exp, false_exp) -> (match (eval (tle, env, cond)) with
                                  VBool true -> (eval (tle, env, true_exp))
                                  | VBool false -> (eval (tle, env, false_exp))
              | _ -> failwith "first argument of if must evaluate to a boolean")
  | CondE x -> (match x with
    [] -> failwith "No conditions in cond expression evaluate to true"
    | (cond, exp)::t -> (match (eval (tle, env, cond)) with
                        VBool a -> if a then (eval (tle, env, exp))
                                   else (eval (tle, env, (CondE t)))
| _ -> failwith "First element in argument of cond must evaluate to a boolean"))
  | QuoteE x -> (match x with
      Symbol ("true") -> VBool true
      | Symbol ("false") -> VBool false
      | Symbol x -> VSymbol x
      | Number b -> VNum b
      | List [] -> VList []
      | List (h::t) -> VList ((eval (tle, env, (QuoteE h))) ::
                             (match (eval (tle, env, (QuoteE (List t))))
                             with VList x -> x | _ ->
                              failwith "Unexpected error (QuoteE List match)")))
  | LambdaE (l, exp) -> VClosure (l, exp, env)
  | ApplicationE (h::t) -> (match eval (tle, env, h) with
                           VBuiltin (x, y) -> (y (List.map
                             (function x -> eval (env, tle, x)) t))
                           | VClosure (a, b, c) ->
                        (eval (tle, (closure_helper (a, t, tle, env) @ c), b))
                           | _ -> failwith
                             "Expects a built-in function or lambda expression")
  | ApplicationE [] -> VList []
  and closure_helper : (identifier list) * (expression list)
  * environment * environment -> environment = function
    ([], [], env1, env2) -> []
    | ((h1 :: t1), (h2 :: t2), env1, env2) -> ((h1, (eval (env1, env2, h2))) ::
                                      (closure_helper (t1, t2, env1, env2)))
    | _ -> failwith "Unexpected error (closure_helper)" ;;

check_expect (eval (initial_tle, [], OrE (IdentE (ID "true"),
  IdentE (ID "false")))) (VBool true) ;;
check_expect (eval (initial_tle, [], OrE (IdentE (ID "false"),
  ApplicationE [IdentE (ID ("number?")); NumE 2]))) (VBool true) ;;
check_expect (eval (initial_tle, [], AndE (IdentE (ID "true"),
  IdentE (ID "false")))) (VBool false) ;;
check_expect (eval (initial_tle, [], AndE (IdentE (ID "true"),
  OrE ((IdentE (ID "false")), (IdentE (ID "true")))))) (VBool true) ;;
check_expect (eval (initial_tle, [], OrE (IdentE (ID "false"),
  AndE ((IdentE (ID "true")), (IdentE (ID "false")))))) (VBool false) ;;
check_expect (eval (initial_tle, [], IfE (IdentE (ID "true"),
  ApplicationE [IdentE (ID ("+")); NumE 2; NumE 5], NumE 4)))
  (VNum 7) ;;
check_expect (eval (initial_tle, [], IfE (AndE (IdentE (ID "true"),
  IdentE (ID "false")),
  ApplicationE [IdentE (ID ("+")); NumE 2; NumE 5], NumE 4))) (VNum 4) ;;
check_expect (eval (initial_tle, [], CondE [(IdentE (ID "false"), NumE 4);
  (IdentE (ID "true"), ApplicationE [IdentE (ID ("+")); NumE 2; NumE 5])]))
  (VNum 7) ;;
check_expect (eval (initial_tle, [], QuoteE (Number 2))) (VNum 2) ;;
check_expect (eval (initial_tle, [], QuoteE (Symbol "hello")))
  (VSymbol "hello") ;;
check_expect (eval (initial_tle, [], QuoteE (Symbol "true"))) (VBool true) ;;
check_expect (eval (initial_tle, [],
  QuoteE (List [Symbol "true"; Symbol "false"])))
  (VList [VBool true; VBool false]) ;;
check_expect (eval (initial_tle, [], ApplicationE [LambdaE ([ID "x"; ID "y"],
  ApplicationE [IdentE (ID ("*")); IdentE (ID ("x")); IdentE (ID ("y"))]);
  NumE 2; NumE 3])) (VNum 6) ;;
check_expect (eval (initial_tle, [], ApplicationE [IdentE (ID ("cons")); NumE 0;
  QuoteE (List [])])) (VList [VNum 0]) ;;
check_expect (eval (initial_tle, [], ApplicationE [IdentE (ID ("cons"));
  ApplicationE [IdentE (ID ("+")); NumE 1; NumE 2];
  QuoteE (List [Number 1; Number 2; Number 3])]))
  (VList [VNum 3; VNum 1; VNum 2; VNum 3]) ;;
check_expect (eval (initial_tle, [], ApplicationE [IdentE (ID ("cons"));
  ApplicationE [IdentE (ID ("+")); NumE 7;
  (ApplicationE [LambdaE ([ID "x"; ID "y"],
  ApplicationE [IdentE (ID ("*")); IdentE (ID ("x")); IdentE (ID ("y"))]);
    NumE 2; NumE 3])]; QuoteE (List [Number 5; Number 4])]))
  (VList [VNum 13; VNum 5; VNum 4]) ;;

(* Input: Takes in an environment which is a binding list and a definition and
pattern matches the two to check if the definition is already defined in the
environment
Output: The environment with the new definition added or returns an error if
the ID it's trying to bind to already exists in the top level environment   *)
let add_definition : environment * definition -> environment = function
  | (tle, (x, y)) -> (match (lookup (tle, x)) with
                     None -> ((x, eval (tle, [], y)) :: tle)
      | Some (x) -> failwith "Symbol bound to multiple values or procedures") ;;

check_expect (add_definition ([], (ID "x", NumE 2)))
  [(ID "x", VNum 2)] ;;
check_error (function () -> (add_definition ([(ID "x", VNum 1)],
  (ID "x", NumE 1)))) "Symbol bound to multiple values or procedures" ;;
check_error (function () -> (add_definition ([(ID "x", VNum 1)],
  (ID "x", NumE 2)))) "Symbol bound to multiple values or procedures" ;;
check_error (function () -> (add_definition
  ([(ID "y", VNum 1); (ID "x", VNum 2)],
  (ID "x", NumE 2)))) "Symbol bound to multiple values or procedures" ;;
check_error (function () -> (add_definition
  ([(ID "y", VNum 1); (ID "x", VNum 3)],
  (ID "x", NumE 2)))) "Symbol bound to multiple values or procedures" ;;

(* TODO: write the design recipe for and implement string_of_value *)
let rec string_of_value : value -> string = function
  VNum (x) -> (string_of_int x)
  | VBool (y) -> (string_of_bool y)
  | VSymbol (z) -> z
  | VList (lst) -> "(" ^ (list_creator lst)
  | VBuiltin (s, f) -> s
  | VClosure (a, b, c) -> "<procedure>"
  and list_creator : (value list) -> string = function
      [] -> ")"
      | hd :: [] -> (string_of_value hd) ^ ")"
      | hd :: tl -> (string_of_value hd) ^ " " ^ (list_creator tl) ;;

check_expect (string_of_value (VNum 2)) "2" ;;
check_expect (string_of_value (VBool true)) "true" ;;
check_expect (string_of_value (VSymbol "hello")) "hello" ;;
check_expect (string_of_value (VList [VBool true; VBool false]))
  "(true false)" ;;
check_expect (string_of_value plus) "<builtin-proc-+>" ;;
check_expect (string_of_value (VClosure ([], NumE 2, []))) "<procedure>" ;;

(* process: this procedure processes the abstract_program
 * representation of a Rackette program following the
 * Rackette rules of processing
 * I/P: an abstract_program representation of a Rackette program
 * O/P: the list of values corresponding to
 * the evaluation of any expressions present in pieces *)
let rec process_helper: environment * abstract_program -> value list = function
  (tle, pieces) -> match pieces with
  | [] -> []
  | (Definition def) :: tl -> process_helper ((add_definition (tle, def)), tl)
  | (Expression expr) :: tl ->
                           eval (tle, [], expr) :: process_helper (tle, tl) ;;

let process : abstract_program -> value list = function
  pieces -> process_helper (initial_tle, pieces) ;;

(* these check-expects are not required, but I wanted to try them anyway *)
check_expect (process ([Expression (ApplicationE [LambdaE ([ID "x"; ID "y"],
  ApplicationE [IdentE (ID ("*")); IdentE (ID ("x")); IdentE (ID ("y"))]);
  NumE 2; NumE 3])])) [VNum 6] ;;
check_expect (process ([Expression (ApplicationE [IdentE (ID ("cons"));
  NumE 0; QuoteE (List [])])])) [VList [VNum 0]] ;;
check_expect (process ([Expression (ApplicationE [IdentE (ID ("cons"));
  ApplicationE [IdentE (ID ("+")); NumE 1; NumE 2];
  QuoteE (List [Number 1; Number 2; Number 3])])]))
  [VList [VNum 3; VNum 1; VNum 2; VNum 3]] ;;
check_expect (process ([Expression (ApplicationE [IdentE (ID ("cons"));
  ApplicationE [IdentE (ID ("+")); NumE 7;
  (ApplicationE [LambdaE ([ID "x"; ID "y"],
  ApplicationE [IdentE (ID ("*")); IdentE (ID ("x")); IdentE (ID ("y"))]);
    NumE 2; NumE 3])]; QuoteE (List [Number 5; Number 4])])]))
  [VList [VNum 13; VNum 5; VNum 4]] ;;

(* check_expect (process "(cons (+ 1 2) (quote (1 2 3)))")
  [VList [VNum 3; VNum 1; VNum 2; VNum 3]] ;;
check_expect (process
  "(cons (+ 7 ((lambda (x y) (* x y)) 2 3)) (quote (5 4)))")
  [VList [VNum 13; VNum 5; VNum 4]] ;; ;; *)

(* rackette: this procedure will interpret a Rackette program
 * and return its value as a string, if it has one
 * I/P: a Rackette program represented as a string, program
 * O/P: a list of the string representations of
 *      the evaluated Rackette expressions in programs *)

let rackette : string -> string list = function
  program -> List.map string_of_value (process (parse (read_all program))) ;;

(* these check-expects are not required, but I wanted to try them anyway *)
check_expect (rackette "((lambda (x y) (* x y)) 2 3)") ["6"] ;;
check_expect (rackette "(cons 0 (quote ()))") ["(0)"] ;;
check_expect (rackette "(cons (+ 1 2) (quote (1 2 3)))") ["(3 1 2 3)"] ;;
check_expect (rackette
  "(cons (+ 7 ((lambda (x y) (* x y)) 2 3)) (quote (5 4)))") ["(13 5 4)"] ;;
