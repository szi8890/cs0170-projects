#use "CS17setup.ml" ;;

#use "eliza_rules.ml" ;;

(* extract
 * I/P: a phrase * pattern tuple (input, pattern)
 * O/P: None, if input does not match pattern; or
 *	Some x, where x is the list of phrases which
 *	sequentially match the wildcards, if input
 *	matches pattern *)
let rec extract : phrase * pattern -> phrase list option = function
	(input, pattern) -> match input, pattern with
  | f :: r1, Lit(word) :: r2 -> if (f = word) then extract (r1, r2)
                                else None
  | f :: r1, One :: r2 -> (match extract (r1, r2) with
                           None -> None
	                         | Some(x) -> Some([f] :: x))
  | f :: r1, Any :: r2 -> (match extract (f :: r1, r2) with
                           None -> (match (extract (r1, Any :: r2)) with
                                    None -> (match (extract (r1, r2)) with
                                             None -> None
                                             | Some(x) -> Some([f] :: x))
                                    | Some(a :: x) -> Some((f :: a) :: x)
                                    | Some([]) -> Some([[f]]))
                           | Some(x) -> Some([] :: x))
  | [], Any :: r -> (match extract ([], r) with
                     None -> None
                     | Some(x) -> Some ([] :: x))
  | [], [] -> Some []
  | _, _ -> None ;;

(* test cases for extract go here *)
check_expect (extract (["bought"; "that"], [Any; Lit("bought"); Any]))
            (Some [[]; ["that"]]) ;;
check_expect (extract (["I";"bought"; "candy"], [Any; Lit("bought"); Any]))
            (Some [["I"]; ["candy"]]) ;;
check_expect (extract (["I"; "ate"; "that"], [Any; Lit("bought"); Any]))
            None ;;
check_expect (extract (["My"; "glasses"; "has"; "grown"],
            [Lit("My"); One; Lit("has"); Any]))
            (Some [["glasses"]; ["grown"]]);;
check_expect (extract (["My"; "glasses"; "has"; "grown"; "a"; "lot"],
            [Lit("My"); One; Lit("has"); Any]))
            (Some [["glasses"]; ["grown"; "a"; "lot"]]) ;;
check_expect (extract (["My"; "glasses"], [Lit("My"); One; Lit("has"); Any]))
              None ;;
check_expect (extract (["My"; "glasses"; "has";],
            [Lit("My"); One; Lit("has"); Any])) (Some [["glasses"];[]]) ;;
check_expect (extract (["Sorin"; "is"; "my"; "best"; "friend"],
            [One; Lit("is"); Lit("my"); Lit("best"); Lit("friend")]))
            (Some [["Sorin"]]) ;;
check_expect (extract (["Sorin"; "Cho"; "is"; "my"; "best"; "friend"],
              [One; Lit("is"); Lit("my"); Lit("best"); Lit("friend")]))
              None ;;
check_expect (extract
          (["Do"; "you"; "like"; "big"; "Sorin"; "or"; "little"; "Sorin"],
          [Lit("Do"); Lit("you"); Lit("like"); Any; One; Lit("or"); Any; One]))
          (Some [["big"]; ["Sorin"]; ["little"]; ["Sorin"]]) ;;
check_expect (extract (["Do"; "you"; "want"; "to"; "like"; "big";
                      "Sorin"; "or"; "little"; "Sorin"],
                      [Lit("Do"); Lit("you"); Lit("like");
                      Any; One; Lit("or"); Any; One])) None ;;
check_expect (extract
          (["Do"; "you"; "like"; "Sorin"; "or"; "Tim"],
          [Lit("Do"); Lit("you"); Lit("like"); Any; One; Lit("or"); Any; One]))
          (Some [[];["Sorin"]; []; ["Tim"]]) ;;
check_expect (extract (["I"; "want"; "to"; "hug"; "you"],
                      [Lit("I"); One; Any; Lit("you")]))
                      (Some [["want"]; ["to"; "hug"]]) ;;
check_expect (extract (["I"; "want"; "to"; "cry"],
                      [Lit("I"); One; Any; Lit("you")]))
                      None ;;
check_expect (extract (["Sorin"; "is"; "18"; "years"; "old"],
                       [Any; Lit("is"); One; Lit("years"); Lit("old")]))
                      (Some [["Sorin"]; ["18"]]) ;;
check_expect (extract (["Sorin"; "is"; "18"; "20"; "years"; "old"],
                       [Any; Lit("is"); One; Lit("years"); Lit("old")]))
                      None ;;
check_expect (extract (["Do"; "you"; "live"; "here"; "?"], [Any; Lit("?")]))
                      (Some [["Do"; "you"; "live"; "here"]]) ;;
check_expect (extract (["Do"; "you"; "?"; "me"], [Any; Lit("?")]))
                      None ;;
check_expect (extract (["I"; "enjoy"; "sleeping"; "a"; "ton"],
                      [Lit("I"); Lit("enjoy"); Lit("sleeping"); Any; Any]))
                      (Some [[]; ["a"; "ton"]]) ;;
check_expect (extract (["I"; "might"; "enjoy"; "sleeping"; "a"; "ton"],
                      [Lit("I"); Lit("enjoy"); Lit("sleeping"); Any; Any]))
                      None ;;
check_expect (extract (["I"; "lift"; "everyday"],
                      [Lit("I"); Any; Lit("everyday")])) (Some [["lift"]]) ;;
check_expect (extract (["I"; "lift"; "everyday"; "bro"],
                      [Lit("I"); Any; Lit("everyday")])) None ;;
check_expect (extract (["Are"; "you"; "a"; "bot"],
                    [Lit("Are"); Lit("you"); Lit("a"); Lit("bot")]))
                    (Some []) ;;
check_expect (extract (["Are"; "you"; "a"; "bot"; "now"],
                    [Lit("Are"); Lit("you"); Lit("a"); Lit("bot")]))
                    None ;;
check_expect (extract (["I'm"; "so"; "tired"], [One; One; One]))
                      (Some [["I'm"]; ["so"]; ["tired"]]) ;;
check_expect (extract (["I'm"; "so"; "tired"; "today"], [One; One; One]))
                      None ;;
check_expect (extract ([], [Any])) (Some [[]]) ;;
check_expect (extract (["derp"; "derp"], [Any]))
                      (Some [["derp"; "derp"]]) ;;


(* make_response
 * I/P: a phrase list * response_template tuple (extraction, temp)
 * O/P: a response constructed by substituting the
 *      extracted literals from extraction into
 *      their positions in the template*)

let rec make_response : phrase list * response_template -> phrase = function
	(extraction, temp) -> match extraction, temp with
	| pl, [] -> []
	| pl, Text(x) :: r ->  x @ (make_response (pl, r))
	| pl, Place(x) :: r ->  (List.nth pl (x - 1)) @ (make_response (pl, r)) ;;

check_expect (make_response ([["I"]; ["cookies"]],
             [Text(["How"; "much"; "did"]); Place(2); Text(["cost?"])]))
             ["How"; "much"; "did"; "cookies"; "cost?"] ;;
check_expect (make_response ([["I"]; []],
              [Text(["How"; "much"; "did"]); Place(2); Text(["cost?"])]))
              ["How"; "much"; "did"; "cost?"] ;;
check_expect (make_response ([["hair"]; ["grown"]],
            [Text(["Is"; "it"; "good"; "for"]); Place(1);
            Text(["to"; "have"]); Place(2); Text(["?"])]))
            ["Is"; "it"; "good"; "for"; "hair"; "to"; "have"; "grown"; "?"] ;;
check_expect (make_response ([["hair"]; ["grown"; "a"; "lot"]],
              [Text(["Is"; "it"; "good"; "for"]); Place(1);
              Text(["to"; "have"]); Place(2); Text(["?"])]))
              ["Is"; "it"; "good"; "for"; "hair";
              "to"; "have"; "grown"; "a"; "lot"; "?"] ;;

check_expect (make_response ([["Scott"]],
              [Text(["Tell"; "me"; "more"; "about"]); Place(1)]))
              ["Tell"; "me"; "more"; "about"; "Scott"] ;;
check_expect (make_response ([["big"]; ["Sorin"]; ["little"]; ["Sorin"]],
              [Text(["I"; "like"]); Place(3); Place(4); Text(["more"; "then"]);
              Place(1); Place(2)]))
            ["I"; "like"; "little"; "Sorin"; "more"; "then"; "big"; "Sorin"] ;;
check_expect (make_response ([[]; ["Sorin"]; []; ["Tim"]],
              [Text(["I"; "like"]); Place(3); Place(4); Text(["more"; "than"]);
              Place(1); Place(2)]))
            ["I"; "like"; "Tim"; "more"; "than"; "Sorin"] ;;
check_expect (make_response ([["want"]; ["to"; "hug"]],
              [Text(["Why"; "do"; "you"]); Place(1); Place(2); Text["me?"]]))
              ["Why"; "do"; "you"; "want"; "to"; "hug"; "me?"] ;;
check_expect (make_response ([["Scott"]; ["10"]],
              [Text(["WOW"; "that's"; "old"])])) ["WOW"; "that's"; "old"] ;;
check_expect (make_response ([["Do"; "you"; "like"; "me"]],
              [Text(["No"; "questions,"; "only"; "answers"])]))
              ["No"; "questions,"; "only"; "answers"] ;;
check_expect (make_response ([[]; ["a"; "ton"]],
              [Text(["Do"; "you"; "really"; "enjoy"; "sleeping"]);
              Place(1); Place(2)]))
              ["Do"; "you"; "really"; "enjoy"; "sleeping"; "a"; "ton"] ;;
check_expect (make_response ([["lift"]],
              [Text(["Do"; "you"; "like"; "to"]); Place(1);
              Text(["everyday?"])]))
              ["Do"; "you"; "like"; "to"; "lift"; "everyday?"] ;;
check_expect (make_response ([], [Text(["Not"; "a"; "robot."])]))
              ["Not"; "a"; "robot."] ;;
check_expect (make_response ([["derp"]; ["derp"]; ["derp"]],
              [Text(["Can"; "you"; "repeat"; "that?"])]))
              ["Can"; "you"; "repeat"; "that?"] ;;
check_expect (make_response ([[]],
              [Text(["Howdy" ; "partner,"; "how"; "are" ; "you?"])]))
              ["Howdy" ; "partner,"; "how"; "are" ; "you?"] ;;


(* eliza_respond
 * I/P: a phrase * rule list tuple (input, rules)
 * O/P: a phrase representing ELIZA's response to the input phrase
*)

let rec eliza_respond : phrase * rule list -> phrase = function
  (input, rules) -> match input, rules with
    input, [] -> []
  | input, (Rule(a, b) :: tl) -> match (extract (input, a)) with
                                 None -> eliza_respond (input, tl)
                                 | Some (x) -> make_response (x, b) ;;

check_expect (eliza_respond (["I"; "bought"; "a"; "bag"], my_rules))
              ["How"; "much"; "did"; "a"; "bag"; "cost?"] ;;
check_expect (eliza_respond (["My"; "plant"; "has"; "grown"], my_rules))
            ["Is"; "it"; "good"; "for"; "plant"; "to"; "have"; "grown"; "?"] ;;
check_expect (eliza_respond
              (["Scott"; "is"; "my"; "best"; "friend"], my_rules))
              ["Tell"; "me"; "more"; "about"; "Scott"] ;;
check_expect (eliza_respond
              (["Do"; "you"; "like"; "Kim"; "or"; "Tim"], my_rules))
              ["I"; "like"; "Tim"; "more"; "than"; "Kim"] ;;
check_expect (eliza_respond (["I"; "like"; "you"], my_rules))
              ["Why"; "do"; "you"; "like"; "me?"] ;;
check_expect (eliza_respond (["Scott"; "is"; "10"; "years"; "old"], my_rules))
              ["WOW"; "that's"; "old"] ;;
check_expect (eliza_respond (["Why"; "are"; "we"; "alive"; "?"], my_rules))
              ["No"; "questions,"; "only"; "answers"] ;;
check_expect (eliza_respond (["I"; "enjoy"; "sleeping"; "a"; "ton"], my_rules))
              ["Do"; "you"; "really"; "enjoy"; "sleeping"; "a"; "ton"] ;;
check_expect (eliza_respond (["I"; "cry"; "everyday"], my_rules))
              ["Do"; "you"; "like"; "to"; "cry"; "everyday?"] ;;
check_expect (eliza_respond (["Are"; "you";"a"; "bot"], my_rules))
              ["Not"; "a"; "robot."] ;;
check_expect (eliza_respond (["derp"; "Derp"; "derp"], my_rules))
              ["Can"; "you"; "repeat"; "that?"] ;;
check_expect (eliza_respond ([], my_rules))
              ["Howdy" ; "partner,"; "how" ; "are" ; "you?"] ;;

(* eliza
 * I/P: a string * rule list tuple (input, rules)
 *	consisting of a string of user input and a rule list
 * O/P: ELIZA's response to input based on the patterns contained
 *      in rules, converted to a string.
 * You need not test eliza, as it is already implemented. *)

let eliza : string * rule list -> string = function
  (input, rules) -> from_phrase (eliza_respond ((to_phrase input), rules)) ;;
