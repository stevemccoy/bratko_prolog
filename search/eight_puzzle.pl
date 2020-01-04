% Eight puzzle representation.
%
% [X/Y | Tail]
%
% where X/Y coordinates of squares, with first in list is the empty square.
%

% s(Node, SuccessorNode, Cost) 

s([Empty | Tiles], [Tile | Tiles1], 1) :-
	swap(Empty, Tile, Tiles, Tiles1).

swap(Empty, Tile, [Tile | Ts], [Empty | Ts]) :- 
	mandist(Empty, Tile, 1).

swap(Empty, Tile, [T1 | Ts], [T1 | Ts1]) :-
	swap(Empty, Tile, Ts, Ts1).

mandist(X/Y, X1/Y1, D) :-
	dif(X, X1, Dx),
	dif(Y, Y1, Dy),
	D is Dx + Dy.

dif(A, B, D) :-
	D is A - B, D >= 0, !
	;
	D is B - A.

% Heuristic estimate is the sum of distances of each Tile
% from its home square plus 3 times sequence score.

h([Empty | Tiles], H) :-
	goal([Empty1 | GoalSquares]),
	totdist(Tiles, GoalSquares, D),
	seq(Tiles, S),
	H is D + 3 * S.

totdist([], [], 0).
totdist([Tile | Tiles], [Square | Squares], D) :- 
	mandist(Tile, Square, D1),
	totdist(Tiles, Squares, D2),
	D is D1 + D2.

% seq(TilePositions, Score): sequence score.

seq([First | OtherTiles], S) :-
	seq([First | OtherTiles], First, S).

seq([Tile1, Tile2 | Tiles], First, S) :-
	score(Tile1, Tile2, S1),
	seq([Tile2 | Tiles], First, S2),
	S is S1 + S2.

seq([Last], First, S) :-
	score(Last, First, S).

score(2/2, _, 1) :- !.

score(1/3, 2/3, 0) :- !.
score(2/3, 3/3, 0) :- !.
score(3/3, 3/2, 0) :- !.
score(3/2, 3/1, 0) :- !.
score(3/1, 2/1, 0) :- !.
score(2/1, 1/1, 0) :- !.
score(1/1, 1/2, 0) :- !.
score(1/2, 1/3, 0) :- !.

score(_, _, 2).

goal([2/2, 1/3, 2/3, 3/3, 3/2, 3/1, 2/1, 1/1, 1/2]).


