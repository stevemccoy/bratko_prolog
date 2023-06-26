/*
Prolog Workshop

Idea: Chess problem solver (mate in N moves).

1. Board state representation.
2. Move definition.
2a. Deciding if position is checkmate or not.
3. Turn taking.
4. Generate possible moves from a board position.
5. Search for mate for Colour in Num moves from position.

6. (optional) Board state evaluation - scoring heuristic.
7. (optional) Minimax search tree pruning.

*/

% rook_move(From, To, Board)
:- use_module(library(clpfd)).

% List utility functions.

% Delete an item from a list.
del(Item, [Item | List], List).
del(Item, [First | List1], [First | List2]) :-
	del(Item, List1, List2).

% Board state something like:
% 1. Piece positions.
% 2. Rooks and King moved flags?
% 
% board([[Col, Row, Player, Piece], ...])


% Move definition.
% 1. Board state (before, after)
% 2. Move taken.
% 
% Destination square must be empty before;
% Unless a knight move, all intervening squares must be empty.

empty_square([C, R], Board) :-
	not(member([C, R, _, _], Board)).

valid_coord(C) :-
	member(C, [1,2,3,4,5,6,7,8]).

valid_coord(C, R) :-
	on_board(C),
	on_board(R).

% ray(Board, From, Direction, To)
% On given Board, square To is reachable from square From by a straight line move 
% in the given Direction.
ray(Board, Limit, [C1, R1], [DC, DR], [C2, R2]) :-
	member(N, [1,2,3,4,5,6,7,8]),
	Limit >= N,
	C2 is C1 + DC * N,
	R2 is R1 + DR * N,
	valid_coord(C2, R2),
	empty_square([C2, R2], Board).

piece_directions(r, [[1,0], [-1,0], [0,1], [0,-1]]).
piece_directions(b, [[-1,-1], [-1,1], [1,-1], [1,1]]).
piece_directions(q, [[1,0], [-1,0], [0,1], [0,-1], [-1,-1], [-1,1], [1,-1], [1,1]]).

straight_move(Before, [Player, C1, R1, C2, R2], [[C2, R2, Player, Piece] | During]) :-
	del([C1, R1, Player, Piece], Before, During),
	piece_directions(Piece, Directions),
	member(Direction, Directions),
	ray(Before, 7, [C1, R1], Direction, [C2, R2]).

king_move(Before, [Player, C1, R1, C2, R2], [[C2, R2, Player, k] | During]) :-
	del([C1, R1, Player, k], Before, During),
	piece_directions(q, Directions),
	member(Direction, Directions),
	ray(Before, 1, [C1, R1], Direction, [C2, R2]).



% Something like:
% successor(Board1, Board2, Move).

successor(FromBoard, ToBoard, move())







% diagonal moves.
diag_move([C1, R1], [C2, R2], Bound) :-
	[C1,R1,C2,R2] ins 1..8,
	D in 1..Bound,
	[E,F] ins -1 \/ 1,
	C2 #= C1 + D * E,
	R2 #= R1 + D * F.

% horizontal moves.
move([C1, R], [C2, R], Bound) :-
	[C1,R,C2] ins 1..8,
	D in 1..Bound,
	E in -1 \/ 1,
	C2 #= C1 + D * E.

% vertical moves.
move([C, R1], [C, R2], Bound) :-
	[C, R1, R2] ins 1..8,
	D in 1..7,
	E in -1 \/ 1,
	R2 #= R1 + D * E.

% knights moves.
kmove([C1, R1], [C2, R2]) :-
	[C1,R1,C2,R2] ins 1..8,
	[DC, DR] ins 1..2,
	DC #\= DR,
	[SC, SR] ins -1 \/ 1,
	C2 #= C1 + DC * SC,
	R2 #= R1 + DR * SR.
