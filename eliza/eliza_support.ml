(* IMPORTANT: You need not edit any code in this file! *)

(* SECTION ONE: TYPE DEFINITIONS*)
(* A single non-empty word or punctuation mark, with no whitespace. *)
type word = string ;;

(* examples of word *)
"example" ;;
"?" ;;

(* an element of a pattern that
 * matches some structure in a sequence of strings *)
type pattern_element =
| Lit of word
| One
| Any ;;

(* examples of pattern_element *)
Lit "example" ;;
One ;;
Any ;;

(* an element of a response template which
 * represents the form of a response,
 * given a sequence of extracted phrases *)
type response_element =
| Text of word list
| Place of int ;;

(* examples of response_element *)
Text(["Are" ; "you" ; "feeling"]) ;;
Place(2) ;;
Text(["today?"]) ;;
Place(1) ;;
Text([]) ;;

type phrase = word list ;;

(* examples of phrase *)
[] ;;
["Hakuna"; "Matata"] ;;

type pattern = pattern_element list ;;

(* examples of pattern *)
[] ;;
[One] ;;
[Any] ;;
[Lit "I"; Any; Lit "my"; Any] ;;
[Lit "Hi"; One; Any] ;;

type response_template = response_element list ;;

(* examples of response_template *)
[] ;;
[Text ["cat"]; Text ["dog"]] ;;
[Text ["Is"]; Place 2; Place 1; Text ["?"]] ;;

type rule = Rule of pattern * response_template ;;

(* examples of rule *)
Rule ([], []) ;;
Rule ([Lit "I"; Any; One], [Place 2; Text ["doesn't"]; Place 1; Text ["you."]]) ;;

(* SECTION TWO: PROCEDURE DEFINITIONS*)
(* is_punc
 * I/P: a string, s
 * O/P: the boolean true if s is a string representing a single punctuation
 *      mark from the set {? ! . , : ;}, false otherwise. *)
let is_punc(s : string) : bool =
  match s with
  | "?" | "!" | "." | "," | ":" | ";" -> true
  | _ -> false ;;

(* to_phrase
 * I/P: a string, input
 * O/P: the string that is the result of separating input into a list of
 *      the individual words it contains. *)
let to_phrase (input : string) : phrase =

  (* walks through the input string char by char, constructing the corresponding phrase *)
  let rec to_phrase_helper (input : string) (len : int) (pos : int): phrase =
	if (pos >= len) then [] (* reached end of string *)
    else let rest_phrase = to_phrase_helper input len (pos + 1)
    in match input.[pos], rest_phrase with
       | ' ', "" :: _ | '\t', "" :: _ -> rest_phrase
       | ' ', _ | '\t', _ -> if (pos == 0) then rest_phrase else "" :: rest_phrase
       | x, [] -> [Char.escaped x]
       | x, "" :: tl -> (Char.escaped x) :: tl
       | x, _ when is_punc(Char.escaped x) -> (Char.escaped x) :: rest_phrase
       | x, wd :: tl when is_punc(wd) -> (Char.escaped x) :: rest_phrase
       | x, wd :: tl (* wd is not punctuation *) -> (Char.escaped x ^ wd) :: tl

	in to_phrase_helper (String.trim input) (String.length (String.trim input)) 0 ;;


(* from_phrase
 * I/P: a phrase, p
 * O/P: the string that is the result of concatenating each string in p together
 *      using the space character as a separator, with the caveat that spaces do
 *      not go directly before punctuation marks. *)
let rec from_phrase (p : phrase) : string =
  match p with
  | [] -> ""
  | x :: [] -> x
  | x :: next :: tl ->
    x ^ (if is_punc(next) then "" else " ") ^ (from_phrase (next :: tl)) ;;
