#use "sig_game.ml" ;;
#use "game.ml" ;;

module AIPlayer = functor (Game: GAME) ->
struct
  module PlayerGame = Connect4

let depth = 5

let rec min_estimate : float list -> float = function
       [] -> failwith "doesn't take in an empty list"
      | hd :: [] -> hd
      | hd :: hd2 :: tl -> if hd > hd2 then min_estimate (hd2 :: tl) else  min_estimate (hd::tl)

let rec max_estimate : float list -> float = function
      [] -> failwith "doesn't take in an empty list"
      | hd :: []  -> hd
      | hd :: hd2 :: tl -> if hd > hd2 then max_estimate (hd :: tl)
                                        else max_estimate (hd2 :: tl)


(* let rec possible_movestates : move list * State (s, l) -> (move * State (s, l))  list = function possible_states(s)  ->
        match ml  with
        | [] > []
        | hd::tl  -> (hd, (PlayerGame.estimate_value)  :: possible_states_value (State(s,l), tl) *)


let possible_states: state -> state list = function s ->
   (List.map  (Connect4.next_state s) (Connect4.legal_moves s))


(* let rec make_kv: 'a list * 'b list -> ('a * 'b) list = function lst, lst2 ->
    match lst, lst2 with
      [], [] -> []
    | (hd::tl), (hd1::tl1) -> (hd, hd1) :: make_kv (tl, tl1)

let state_pairs: state -> (state*move) list = function a ->
  make_kv ((possible_states a), (Connect4.legal_moves a))

let rec get_state_kv: (state * move) list -> state list = function
  [] -> []
  | (hd, hd1):: tl -> hd::(get_state_kv tl)

let rec get_move: state * (state * move) list -> move = function s, l ->
  match l with
  [] -> []
  | (hd, hd1):: tl -> if s = hd1 then hd1 else get_move (s, tl) *)


let rec minimax: state * int -> float  = function State(s, l), d  ->
        match d with
        1 -> estimate_value (State (s, l))
        | _ -> match s with
              | Win(_) | Draw -> estimate_value (State(s, l))
              | Ongoing(P1) ->  (max_estimate (List.map (function n -> (minimax (n, (d - 1))))
                                                        (possible_states (State (s,l)))))
              | Ongoing(P2)->  (min_estimate (List.map (function n -> (minimax (n, (d - 1))))
                                                       (possible_states (State (s,l)))))


let rec get_index : 'a * 'a list -> int = function s, lst ->
    match lst with
    | [] -> raise (Failure "Not Found")
    | h :: t -> if s = h then 0 else 1 + (get_index (s, t))

let next_move : state -> move = function State (s, l) ->
  match s with
  Win _ | Draw -> failwith "kill me right now"
  | Ongoing (player) -> if player = P1 then (List.nth (Connect4.legal_moves (State (s,l))) (get_index ((max_estimate (List.map (function n -> (minimax (n, depth))) (possible_states (State(s, l))))), (List.map (function n -> (minimax (n, depth))) (possible_states (State (s,l)))))))
                        else (List.nth (Connect4.legal_moves (State (s,l))) (get_index ((min_estimate (List.map (function n -> (minimax (n, depth))) (possible_states (State (s,l))))), (List.map (function n -> (minimax (n, depth))) (possible_states (State (s,l)))))))





end ;;

module Connect4AI = AIPlayer(Connect4) ;;

open Connect4AI;;
max_estimate [4.;5.;6.;3.];;
let test_board4 = [[0;0;0;0;2;1];
                  [0;1;1;1;1;2];
                  [0;0;0;2;1;1];
                  [0;0;0;1;1;2];
                  [0;0;2;2;2;1]] ;;



(* TODO: test cases for AIPlayer *)
