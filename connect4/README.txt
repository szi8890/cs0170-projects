Connect4 ReadMe

logins: izhao3 & acagaana


__________________________________________________________________________________________
Instructions for use: 

Playing a game against a friend: The player would run all the files on Ocaml and use the 
referee, game, and human_player files from the document. Now, all the files have been loaded 
on the Ocaml Repl. From here, they would need to define the module where it takes in the referee,
and the two types of players (in this case human player)

module Ref = Referee (Connect4) (HumanPlayer(Connect4)) (HumanPlayer(
Connect4)).

From here, they would just type in corresponding numbers that represent the column that they want to
place the piece in. If it isn't in the domain, it should return an error.

For playing a game with AI, the user would instead type in AIPlayer for human player. As such, it would
instead run the AIPlayer in the corresponding turn.

___________________________________________________________________________________________
Overview of how the program functions: 

The program has various parsing abilities that takes the input given by the user and turns it 
into piece that is readable by the computer (Move 3 per se). From there, the program checks to see
if that corresponding column is full. If it isn't, it plays the move and checks if there is a win. If not
it moves onto Ongoing of the next_state. If there are two human players, it keeps doing this for each
state until one person has a four in a row where chip_connected is used to check the win.

For AI's, the AI's use estimate value and check a certain depth given by us. Once the depth is checked, the 
minimax algorithm is used to find which move will give the maximum float value or minimum float value and act
on that move. From there, the AI goes until the game is over.

_________________________________________________________________________________________
Hopefully there are no bugs :( there probably are im just too tired at this point

List of people:

Kushagra Agarwal
Timmy Wang
Sorin Cho
Amanda Ng


No extra features :( Sorry!