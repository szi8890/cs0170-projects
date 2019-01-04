#use "sig_game.ml" ;;
#use "sig_player.ml" ;;
#use "game.ml" ;;
#use "human_player.ml" ;;
#use "ai_player.ml" ;;

module Referee = functor (Game : GAME) ->
                functor (Player1: PLAYER with module PlayerGame = Game) ->
                functor (Player2: PLAYER with module PlayerGame = Game) ->
struct
  module CurrentGame = Game

  let play_game () : unit =
    let rec game_loop: CurrentGame.state -> unit = function s ->
      print_endline (CurrentGame.string_of_state s) ;
      match CurrentGame.game_status s with
        CurrentGame.Win player -> print_endline ((CurrentGame.string_of_player player) ^ " wins!")
      | CurrentGame.Draw -> print_endline "Draw..."
      | CurrentGame.Ongoing player ->
        print_endline ((CurrentGame.string_of_player player) ^ "'s turn.") ;
        let the_move = (match player with
                      CurrentGame.P1 -> Player1.next_move s
                    | CurrentGame.P2 -> Player2.next_move s) in
        print_endline ((CurrentGame.string_of_player player) ^
                        " makes the move " ^
                        (CurrentGame.string_of_move the_move)) ;
        game_loop (CurrentGame.next_state s the_move)
    in try game_loop CurrentGame.initial_state with
       | Failure message -> print_endline message
end ;;

module Ref = Referee (Connect4) (HumanPlayer(Connect4)) (HumanPlayer(
Connect4)) ;;

module Ref1 = Referee (Connect4) (HumanPlayer(Connect4)) (AIPlayer(
Connect4)) ;;

module Ref2 = Referee (Connect4) (AIPlayer(Connect4)) (HumanPlayer(
Connect4)) ;;
