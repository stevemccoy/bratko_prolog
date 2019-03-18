
% Reversi Game

% Start position 2 white and 2 black stones.
% Take turns, black first.
% Place stones so you can enclose stones of the opposing colour, in which case you turn them over to your colour.
% Straight lines only.

% Game position representation:
% Grid of elements from (b,w,s)

% Row and column coordinates from the following set:
coordinates([1,2,3,4,5,6,7,8,9]).

position([
	[s,s,s,s,s,s,s,s],
	[s,s,s,s,s,s,s,s],
	[s,s,s,s,s,s,s,s],
	[s,s,s,w,b,s,s,s],
	[s,s,s,b,w,s,s,s],
	[s,s,s,s,s,s,s,s],
	[s,s,s,s,s,s,s,s],
	[s,s,s,s,s,s,s,s]
]).

% Get and set indexed positions in a list.

get_indexed_item(1, [V | _], V).
get_indexed_item(N, [_ | T], V) :-
	N > 1,
	M is N - 1,
	get_indexed_item(M, T, V).

set_indexed_item(1, [_ | Tail], V, [V | Tail]).
set_indexed_item(N, [Head | Tail], V, [Head | After]) :-
	N > 1,
	M is N - 1,
	set_indexed_item(M, Tail, V, After).

% Get and set the value of a given square.

get_square(R/C, Grid, Value) :-
	get_indexed_item(R, Grid, Row),
	get_indexed_item(C, Row, Value).

set_square(R/C, Before, V, After) :-
	get_indexed_item(R, Before, Row),
	set_indexed_item(C, Row, V, NewRow),
	set_indexed_item(R, Before, NewRow, After).

% Where can I move?


% white_move(Position, R/C) -- R/C is a legal move for white in Position.
% successor(Position1, R/C, Position2) -- White moving R/C in Position1 results in Position2.
% score(Position, Score) -- Score is a numerical evaluation of Position (+ve for white, -ve for black).


% Find out squares converted by a single move.

beam_directions([-1/-1, -1/0, -1/1, 0/-1, 0/1, 1/-1, 1/0, 1/1]).


convert_all_beams(Position, StartRow/StartCol, Them, Us, TurnOverList) :-
	beam_directions(Directions),
	!,
	findall(BeamList, (
		member(DR/DC, Directions), 
		R is StartRow + DR,
		C is StartCol + DC,
		valid_coordinate(R/C),
		convert_beam(Position, R/C, Direction, Them, Us, BeamList)
	), BeamLists),
	flatten(BeamLists, TurnOverList).


% Going in one direction.

% Position, Start, Direction, TheirColour, OurColour, TurnOverList
% convert_beam(Position, StartRow/StartCol, DRow/DCol, Them/Us, TurnOverList) :-

convert_beam(Position, SRow/SCol, DRow/DCol, Them/Us, []) :-
	get_square(SRow/SCol, Position, Us),
	!.

convert_beam(Position, SRow/SCol, DRow/DCol, Them/Us, [SRow/SCol | Tail]) :-
	get_square(SRow/SCol, Position, Them),
	!,
	R is SRow + DRow,
	C is SCol + DCol,
	valid_coordinate(R/C),
	convert_beam(Position, R/C, DRow/DCol, Them/Us, Tail).


% Make move.

make_move(Position1, Row/Col, Us, TurnOverList, Position3) :-
	set_square(Row/Col, Position1, Us, Position2),
	set_all_squares(TurnOverList, Position2, Us, Position3).

set_all_squares([], Position, _, Position).
set_all_squares([R/C | Tail], Position1, Us, Position3) :-
	set_square(R/C, Position1, Us, Position2),
	!,
	set_all_squares(Tail, Position2, Us, Position3).



% Generate adjacent coordinates to R1/C1.

adjacent(R1/C1, R2/C2) :-
	beam_directions(Directions),
	!,
	member(DR/DC, Directions),
	R2 is R1 + DR,
	C2 is C1 + DC.


% Find squares with given value.

find_squares_with(Position, Value, Results) :-
	setof(R/C, (), Results)

% Need to scan through the rows in Position, keeping track of index positions, to capture matching entries.
% find_index_of(Value, List, Index)

% indexed_content(List, Index, Value)

indexed_content([H | Tail], 1, H).
indexed_content([_ | Tail], N, H) :-
	inde



find_index_of(Value, [Value | Tail], 1).
find_index_of(Value, [_ | Tail], N) :-
	find_index_of




% move(Position1, Us/Them, R/C, Position2)

possible_move(Position, Us/Them, R/C) :-
	% Square is empty (s)
	% Adjacent to Them
	% Non-empty conversion list.


