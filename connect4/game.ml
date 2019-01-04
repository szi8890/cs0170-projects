#use "CS17setup.ml" ;;

module Connect4 =
struct
  type which_player = P1 | P2

  type status =
  | Win of which_player
  | Draw
  | Ongoing of which_player

  type state = State of (status * (int list) list)

  let other_player = function player ->
    (match player with
       P1 -> P2
     | P2 -> P1)

  (* describes a game's moves that a player can make *)
  type move = Move of int

  (* PRINTING FUNCTIONS *)
  let rec make_col : int -> int list = function col ->
    match col with
    0 -> []
    | n -> 0 :: (make_col (n - 1))

  let rec make_board : int * int -> int list list = function (column, row) ->
    match column with
    0 -> []
    | n -> (make_col row) :: (make_board ((n-1), row))

  let initial_rows = 5
  let initial_columns = 7

  let initial_board = (make_board (initial_columns, initial_rows))
  let initial_state = State (Ongoing P1, initial_board)

  let rec transpose : 'a list list -> 'a list list = function
    [] | [] :: _ -> []
    | (hd1 :: []) :: tl -> [List.map List.hd ((hd1 :: []) :: tl)]
    | (hd1 :: tl1) :: tl -> (List.map List.hd ((hd1 :: tl1) :: tl)) ::
                         (transpose (List.map List.tl ((hd1 :: tl1) :: tl))) ;;

  let rec print_column : int list -> string = function l ->
  match l with
  [] -> ""
  | (hd :: tl) -> (match hd with
                  0 -> "_"
                  | 1 -> "○"
                  | 2 -> "●") ^ " | " ^ (print_column tl)
                  | _ -> ""

  let rec print_board : int list list -> string = function l ->
    match l with
    [] -> ""
    | hd :: tl -> " | " ^ (print_column hd) ^ "\n" ^ (print_board (tl))

  let string_of_player = function player ->
    if player = P1 then "Player 1" else "Player 2"

  let get_chip = function player ->
    if player = P1 then 1 else 2

  let rec create_numbers : int -> string = function n ->
  match n with
  0 -> ""
  | n -> (create_numbers (n - 1)) ^ " | " ^ string_of_int (n)

  let string_of_state = function State (s, l) ->
    match (s, l) with
      Win (p), _ -> (string_of_player p) ^ " wins with the board"
                    ^ "\n" ^ (print_board (transpose l))
    | Draw, _ -> "Draw"
    | Ongoing (p), l -> "It is " ^ string_of_player p ^
                         "'s turn with the board"
                          ^ "\n" ^ (create_numbers (initial_columns)) ^ " |"
                          ^ "\n" ^ (print_board (transpose l))

  let string_of_move = function (Move n) ->
          "by placing a piece in column " ^ string_of_int n ^ "!"

  (* GAME LOGIC *)

  let rec chip_connected : int list * int -> bool = function (l, s) ->
    match l, s with
    | hd:: hd2 ::hd3 :: hd4 :: tl, inp ->
                  ((hd = inp && hd2 = inp) && (hd3 = inp && hd4 = inp))
                  || (chip_connected (hd2 ::hd3 :: hd4 :: tl, inp))
    | _ -> false

  let rec check_column : int list list * int -> bool =
  function (board, chip) ->
    match board with
    [] -> false
    | hd :: tl -> (chip_connected (hd, chip)) || (check_column (tl, chip))


  let rec check_row : int list list * int -> bool = function board, chip ->
    (check_column ((transpose board), chip))

  let append_zero : int list list -> int list list = function (board)->
    if List.length board >= 4 then match board with
                                    hd :: hd2 :: hd3 :: hd4 :: tl ->
                                    ([(hd @ [0;0;0]); ([0] @ hd2 @ [0; 0]);
                                    ([0; 0] @ hd3 @ [0]); ([0; 0; 0] @ hd4)])
                              else []

  let rec check_diagonal : int list list * int -> bool =
  function (board, chip) ->
   match board with
   [] -> false
   | hd :: tl -> if (List.mem chip hd)
               then (check_row ((append_zero board), chip))
               || (check_row ((append_zero (List.map List.rev board)), chip))
               else check_diagonal ((tl), chip)


  let rec is_full : int list list * int -> move list =
  function board, move_int ->
    match board, move_int with
    [], _ -> []
    | (hd :: tl), y -> if (List.mem 0 hd)
                       then (Move (y)) :: (is_full (tl, (y + 1)))
                       else (is_full (tl, (y + 1)))

  let legal_moves = function State (s, board) ->
    (is_full (board, 1))

  (* returns the status of the game at the given state *)
  let game_status = function State (s, l) -> s

  (* given a state and a legal move, yields the next state
     Note that this procedure is curried. *)
  let rec add_chip : int list * int -> int list = function l, chip ->
    match l, chip with
    (hd :: hd2 :: tl), m -> if ((hd = 0) && (not (hd2 = 0)))
                              then ((m + hd) :: hd2 :: tl)
                              else hd :: (add_chip ((hd2::tl), m))
    | hd :: [], m -> [(m + hd)]
    | _ -> []

  let rec next_board : int list list * int * int -> int list list =
  function (l, m, chip) ->
    match (l, m) with
    [], _ -> []
    | hd :: tl, x -> if x = 1 then (add_chip (hd, chip)) :: tl
                     else hd :: next_board (tl, (m - 1), chip)

  let check_win : int list list * int * int -> bool = function (l, m, chip) ->
    (check_row ((next_board (l, m, chip)), chip)) ||
    (check_column ((next_board (l, m, chip)), chip) ||
    check_diagonal ((next_board (l, m, chip)), chip))

  let next_state = fun (State (s, l)) (Move m) ->
     match s with
     Win (_) | Draw -> State (s, l)
     | Ongoing (player) -> if check_win (l, m, (get_chip player))
                               then (State
                                  ((Win (player)),
                                  (next_board (l, m, (get_chip player)))))
                               else if ((legal_moves (State (s,
                                 next_board (l, m, (get_chip player))))) = [])
                               then (State (Draw,
                                    (next_board (l, m, (get_chip player)))))
                               else (State
                                    ((Ongoing (other_player player)),
                                    (next_board (l, m, (get_chip player)))))

  (* SPECIFIC TO HUMAN PLAYERS *)

  (* for transforming player input into
   * the internal representation of a move *)
   let move_of_string = function str -> Move (int_of_string str)


  (* SPECIFIC TO AI PLAYERS *)

  (* estimate the value (for player 1) of a given state *)
  (* use floats *)
  let rec assign_pts_col3 : int list * int * float -> float =
  function column, chip, counter ->
    match column, chip with
      hd :: hd2 :: hd3 :: hd4 :: tl, chip -> if ((hd = 0 && hd2 = chip) &&
                                               (hd3 = chip && hd4 = chip))
                                          then (counter +. 35.0)
                                          else (counter)
      | _ -> 0.0

  let rec get_all_col3 : int list list * int -> float = function board, chip ->
    if List.length board >= 4
      then match board with
                 hd :: tl -> (assign_pts_col3 (hd, chip, 0.0))
                             +. (get_all_col3 (tl, chip))
                 | _ -> 0.0
      else 0.0

  let assign_pts_row3 : int list * int * float -> float =
  function row, chip, counter ->
    match row with
    hd :: hd2 :: hd3 :: hd4 :: tl -> if ((hd = 0 && hd2 = chip) &&
                                        (hd3 = 0 && hd4 = chip)) ||
                                        ((hd = chip && hd2 = 0) &&
                                        (hd3 = chip && hd4 = 0)) ||
                                        ((hd = 0 && hd2 = chip) &&
                                        (hd3 = chip && hd4 = chip)) ||
                                        ((hd = 0 && hd2 = chip) &&
                                        (hd3 = chip && hd4 = chip)) ||
                                        ((hd = chip  && hd2 = chip) &&
                                        (hd3 = 0 && hd4 = chip)) ||
                                        ((hd = chip && hd2 = 0) &&
                                        (hd3 = chip && hd4 = chip))
                                      then (counter +. 50.0)
                                      else (counter)
    | _ -> 0.0

  let rec get_all_row3 : int list list * int -> float = function board, chip ->
    if List.length board >= 4
    then match board with
            hd :: tl -> (assign_pts_row3 (hd, chip, 0.0))
                        +. (get_all_row3 (tl, chip))
            | _ -> 0.0
    else 0.0

  let rec get_all_diag3 : int list list * int -> float =
  function board, chip ->
    if List.length board >= 4
    then match board with
          hd :: tl -> if (List.mem chip hd)
                      then get_all_row3 ((transpose (append_zero board)), chip)
                           +. get_all_row3 ((transpose (append_zero
                                           (List.map List.rev board))), chip)
                      else get_all_diag3 (tl, chip)
          | _ -> 0.0
    else 0.0



  let rec assign_pts_col2 : int list * int * float -> float =
  function column, chip, counter ->
    match column, chip with
    hd :: hd2 :: hd3 :: hd4 :: tl, chip -> if ((hd = 0 && hd2 = chip) &&
                                               (hd3 = chip))
                                               then (counter +. 20.0)
                                               else (counter)
    | _ -> 0.0

  let rec get_all_col2 : int list list * int -> float = function board, chip ->
    if List.length board >= 4
    then match board with
            hd :: tl -> (assign_pts_col2 (hd, chip, 0.0))
                                     +. (get_all_col2 (tl, chip))
            | _ -> 0.0
    else 0.0

  let assign_pts_row2: int list * int * float -> float =
  function row, chip, counter ->
    match row with
    hd :: hd2 :: hd3 :: hd4 :: tl -> if ((hd = chip && hd2 = chip) &&
                                 (hd3 = 0 && hd4 = 0)) ||
                                  ((hd = 0 && hd2 = chip) &&
                                  (hd3 = chip && hd4 = 0)) ||
                                  ((hd = 0 && hd2 = 0) &&
                                  (hd3 = chip && hd4 = chip))
                                  then (counter +. 20.0)
                                  else (counter)
    | _ -> 0.0

  let rec get_all_row2 : int list list * int -> float = function board, chip ->
    if List.length board >= 4 then match (transpose board) with
                              hd :: tl -> (assign_pts_row2 (hd, chip, 0.0))
                                          +. (get_all_row2 (tl, chip))
                              | _ -> 0.0
                              else 0.0

  let rec get_all_diag2 : int list list * int -> float = function board, chip ->
  if List.length board >= 4
  then match board with
          hd :: tl -> if (List.mem chip hd)
                      then get_all_row2 ((transpose (append_zero board)), chip)
                      +. get_all_row2 ((transpose (append_zero
                                      (List.map List.rev board))), chip)
                      else get_all_diag2 (tl, chip)
          | _ -> 0.0
  else 0.0

  let rec assign_pts_col1 : int list * int * float -> float =
  function column, chip, counter ->
    match column with
    hd :: hd2 :: tl -> if (hd = 0 && hd2 = chip)
                       then (counter +. 10.0)
                       else (counter)
    | _ -> 0.0

  let rec get_all_col1 : int list list * int  -> float =
  function board, chip ->
    if List.length board >= 4 then match board with
                                   hd :: tl -> (assign_pts_col1 (hd, chip, 0.0)
                                                 +. (get_all_col1 (tl, chip)))
                                    | _ -> 0.0
                              else 0.0

  let rec assign_pts_row1 : int list * int * float -> float =
  function row, chip, counter ->
    match row with
    hd :: hd2 :: tl -> if (hd = 0 && hd2 = chip) || (hd = chip && hd2 = 0)
                       then (counter +. 10.0)
                       else (counter)
    | _ -> 0.0

  let rec get_all_row1 : int list list * int -> float =
  function board, chip ->
    if List.length board >= 4 then match board with
                                 [] -> 0.0
                                 | hd :: tl -> (assign_pts_row1 (hd, chip, 0.0)
                                                +. (get_all_row1 (tl, chip)))
                              else 0.0

  let rec get_all_diag1 : int list list * int -> float =
  function board, chip ->
    if List.length board >= 4
    then match board with
          [] -> 0.0
         | hd :: tl -> if (List.mem chip hd)
                       then get_all_row1 ((transpose (append_zero board)), chip)
                            +. get_all_row1 ((transpose
                            (append_zero (List.map List.rev board))), chip)
                        else get_all_diag1 (tl, chip)
    else 0.0

  let all_row_pts : int list list * int -> float = function board, chip ->
    get_all_row1 ((transpose board), chip)
    +. get_all_row2 ((transpose board), chip)
     +. get_all_row3 ((transpose board), chip)

  let all_col_pts : int list list * int -> float = function board, chip ->
    get_all_col1 (board, chip)
    +. get_all_col2 (board, chip)
    +. get_all_col3 (board, chip)

  let all_diag_pts : int list list * int -> float = function board, chip ->
    get_all_diag1 (board, chip)
    +. get_all_diag2 (board, chip)
     +. get_all_diag3 (board, chip)

  let all_pts : int list list * int -> float = function board, chip ->
    all_diag_pts (board, chip)
    +. all_row_pts (board, chip)
    +. all_col_pts(board, chip)

  let estimate_value = function State (s, l) ->
    match s with
    Win (P1) -> 1000000000.0
    | Win (P2) -> -1000000000.0
    | Draw -> 0.0
    | Ongoing(player) -> all_pts (l, 1) -. all_pts (l, 2)


end ;;

open Connect4 ;;

(* test boards  *)
let test_board = [[1;1;1;1;1;1;1];
                  [0;0;0;1;1;2;2];
                  [0;0;0;0;1;2;1];
                  [0;0;0;0;0;1;1];
                  [0;0;0;0;0;1;1];
                  [0;0;0;0;0;2;2];
                  [0;0;0;0;0;1;1]];;

let test_board2 = [[1;1;1;2;1;1;1;1;2];
                  [0;0;0;0;0;0;2;2;1];
                  [0;0;0;0;0;0;1;1;2];
                  [0;0;0;0;0;0;1;1;1];
                  [0;0;0;0;0;0;0;1;1];
                  [0;0;0;0;0;0;0;2;2];
                  [0;0;0;0;0;0;0;1;1]] ;;

let draw_board = [[0; 1; 2; 1; 2];
                  [2; 1; 2; 1; 2];
                  [1; 2; 1; 2; 1];
                  [1; 2; 1; 2; 1];
                  [2; 1; 2; 1; 2];
                  [2; 1; 2; 1; 2];
                  [1; 2; 1; 2; 1]] ;;

let win_board = [[1; 2; 1; 1; 2];
                  [0; 2; 2; 2; 1];
                  [1; 2; 1; 2; 1];
                  [1; 2; 1; 2; 1];
                  [2; 1; 2; 1; 2];
                  [2; 1; 2; 1; 2];
                  [1; 2; 1; 2; 1]] ;;

(* test cases for other player *)
check_expect (other_player P1) (P2) ;;
check_expect (other_player P2) (P1) ;;

(* test cases for make col *)
check_expect (make_col 0) [] ;;
check_expect (make_col 4) [0; 0; 0; 0] ;;
check_expect (make_col 5) [0; 0; 0; 0; 0] ;;

(* test cases for make board *)
check_expect (make_board (7, 5))
([[0; 0; 0; 0; 0];
[0; 0; 0; 0; 0];
[0; 0; 0; 0; 0];
[0; 0; 0; 0; 0];
[0; 0; 0; 0; 0];
[0; 0; 0; 0; 0];
[0; 0; 0; 0; 0]]) ;;
check_expect (make_board (0, 5)) [] ;;

(* test cases for transpose *)
check_expect (transpose [[10; 12; 14]; [11; 13; 15]])
                        [[10; 11]; [12; 13]; [14; 15]] ;;

(* test cases for print_column *)
check_expect (print_column [0; 0; 0; 0]) ("_ | _ | _ | _ | ") ;;
check_expect (print_column [0; 0; 2; 1])
(* because of the pieces that we used, ocaml cant
exactly read it for some reason *)
("_ | _ | \226\151\143 | \226\151\139 | ") ;;
(* this is the only way to get it to work exactly *)
check_expect (print_board initial_board)
 (" | _ | _ | _ | _ | _ | \n | _ | _ | _ | _ | _ | \n | _ | _ | _ | _ | _ | \n | _ | _ | _ | _ | _ | \n | _ | _ | _ | _ | _ | \n | _ | _ | _ | _ | _ | \n | _ | _ | _ | _ | _ | \n") ;;

(* test cases for stirng of player *)
check_expect (string_of_player P1) ("Player 1") ;;
check_expect (string_of_player P2) ("Player 2") ;;

(* test cases for get chip *)
check_expect (get_chip P1) (1) ;;
check_expect (get_chip P2) (2) ;;

(* test cases for chip connected *)
check_expect (chip_connected ([0; 0; 0; 0], 1)) (false) ;;
check_expect (chip_connected ([1; 1; 1; 1], 1)) (true) ;;
check_expect (chip_connected ([0; 0; 0; 0; 2; 2; 2;], 2)) (false) ;;
check_expect (chip_connected ([0; 0; 1; 0; 2; 2; 2;], 2)) (false) ;;
check_expect (chip_connected ([0; 0; 2; 2; 2; 2; 1;], 2)) (true) ;;

(* test cases for add chip *)
check_expect (add_chip ([0; 0; 0; 1], 1)) ([0; 0; 1; 1]) ;;
check_expect (add_chip ([0; 1; 2; 2; 1], 2)) ([2; 1; 2; 2; 1]) ;;
check_expect (add_chip ([0; 0; 0; 0; 0;], 2)) ([0; 0; 0; 0; 2]) ;;

(* test cases for check win *)
check_expect (check_win (test_board, 3, 1)) (true) ;;
check_expect (check_win (test_board2, 4, 1)) (true) ;;
check_expect (check_win (win_board, 2, 1)) (true) ;;
check_expect (check_win (draw_board, 1, 1)) (false) ;;

(* test cases for legal moves *)
check_expect (legal_moves (State (Ongoing (P1), initial_board)))
([Move 1; Move 2; Move 3; Move 4; Move 5; Move 6; Move 7]) ;;
check_expect (legal_moves (State (Ongoing (P1), test_board)))
([Move 2; Move 3; Move 4; Move 5; Move 6; Move 7]) ;;
check_expect (legal_moves (State (Ongoing (P2), draw_board))) ([Move 1]) ;;

(* test cases for next_state *)
check_expect (next_state (State (Ongoing (P1), initial_board)) (Move 1))
(State
 (Ongoing P2,
  [[0; 0; 0; 0; 1]; [0; 0; 0; 0; 0]; [0; 0; 0; 0; 0]; [0; 0; 0; 0; 0];
   [0; 0; 0; 0; 0]; [0; 0; 0; 0; 0]; [0; 0; 0; 0; 0]])) ;;
check_expect (next_state (State (Ongoing (P2), draw_board)) (Move 1))
(State
 (Draw,
  [[2; 1; 2; 1; 2]; [2; 1; 2; 1; 2]; [1; 2; 1; 2; 1]; [1; 2; 1; 2; 1];
   [2; 1; 2; 1; 2]; [2; 1; 2; 1; 2]; [1; 2; 1; 2; 1]])) ;;
check_expect (next_state (State (Ongoing (P2), win_board)) (Move 2))
(State
 (Win P2,
  [[1; 2; 1; 1; 2]; [2; 2; 2; 2; 1]; [1; 2; 1; 2; 1]; [1; 2; 1; 2; 1];
   [2; 1; 2; 1; 2]; [2; 1; 2; 1; 2]; [1; 2; 1; 2; 1]])) ;;


(* test cases for move of string *)
check_expect (move_of_string "3") (Move 3) ;;
check_expect (move_of_string "2") (Move 2) ;;
check_expect (move_of_string "7") (Move 7) ;;

(* test cases for estimate value *)
(* because we wrote our other helper functions within estimate value,
calling the entirety of estimate value should work for testing the other
functions *)
check_expect (estimate_value (State (Ongoing (P1),
[[0; 0; 0; 0; 0]; [0; 0; 0; 0; 0]]))) (0.0) ;;
check_expect (estimate_value (State (Win(P1), test_board))) (1000000000.0) ;;
check_expect (estimate_value (State (Win(P2), test_board))) (-1000000000.0) ;;
check_expect (estimate_value (State (Draw, draw_board))) (0.0) ;;
check_expect (estimate_value (State (Ongoing (P1), test_board))) (130.0) ;;
check_expect (estimate_value (State (Ongoing (P2), test_board2))) (160.0) ;;
check_expect (estimate_value (State (Ongoing (P1), draw_board))) (0.0) ;;
check_expect (all_row_pts (test_board, 1)) (70.0) ;;
check_expect (all_col_pts (test_board, 1)) (0.0) ;;
check_expect (all_diag_pts (test_board, 1)) (70.0) ;;
check_expect (all_row_pts (test_board, 2)) (0.0) ;;
check_expect (all_col_pts (test_board, 2)) (0.0) ;;
check_expect (all_diag_pts (test_board, 2))  (10.0) ;;
