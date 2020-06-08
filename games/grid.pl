% 
% Utility functions for square grids addressed by coordinates.
%

:- use_module(library(clpfd)).

coordinate(X) :-
	X #> 0, X #< 9.

% adjacent(Pos1, Pos2).
%
% Two positions are adjacent - Pos1 must be bound.

adjacent(X1/Y1, X2/Y2) :-
	dif(X1/Y1, X2/Y2),
	DX in -1..1,
	DY in -1..1,
	indomain(DX),
	indomain(DY),
	X2 is X1 + DX,
	Y2 is Y1 + DY.

% beam(Start, Heading, Positions)
% 
% Generate positions starting at X/Y and stepping by increments of DX/DY, while remaining on grid.

beam(X/Y, DX/DY, [X1/Y1 | Tail]) :-
	coordinate(X1),
	coordinate(Y1),
	X1 is X + DX,
	Y1 is Y + DY,
	!,
	beam(X1/Y1, DX/DY, Tail).
beam(_, _, []).

