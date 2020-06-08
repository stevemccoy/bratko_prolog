
% Reversi (Othello) Game

% Start position 2 white and 2 black stones.
% Take turns, black first.
% Place stones so you can enclose stones of the opposing colour, in which case you turn them over to your colour.
% Straight lines only.

% Game position representation:
% Grid of elements from (b,w,s)

row_coordinate(R) :-
	member(R, [1,2,3,4,5,6,7,8]).

column_coordinate(C) :-
	member(C, [1,2,3,4,5,6,7,8]).

valid_coordinate(R/C) :-
	row_coordinate(R),
	column_coordinate(C).

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

set_indexed_item(1, [_ | Tail], V, [V | Tail]).
set_indexed_item(N, [Head | Tail], V, [Head | After]) :-
	N > 1,
	M is N - 1,
	set_indexed_item(M, Tail, V, After).


% Get value at an indexed position, or find the indexes of matches to the provided value, within a given list.
% List must be a finite list - procedure cannot be used to generate List.

indexed_item(Value, List, Index) :-
	not(var(List)),
	find_index_with_offset(Value, List, 0, Index).

find_index_with_offset(Value, [Value | _], Offset, N) :-
	N is Offset + 1.
find_index_with_offset(Value, [_ | Tail], Offset, N) :-
	NewOffset is Offset + 1,
	find_index_with_offset(Value, Tail, NewOffset, N).


% Get and set the value of a given square.

get_square(R/C, Grid, Value) :-
	indexed_item(Row, Grid, R),
	indexed_item(Value, Row, C).

set_square(R/C, Before, V, After) :-
	indexed_item(Row, Before, R),
	set_indexed_item(C, Row, V, NewRow),
	set_indexed_item(R, Before, NewRow, After).

% Where can I move?


% white_move(Position, R/C) -- R/C is a legal move for white in Position.
% successor(Position1, R/C, Position2) -- White moving R/C in Position1 results in Position2.
% score(Position, Score) -- Score is a numerical evaluation of Position (+ve for white, -ve for black).


% Find out squares converted by a single move.

direction(-1, -1).
direction(-1, 0).
direction(-1, 1).
direction(0, -1).
direction(0, 1).
direction(1, -1).
direction(1, 0).
direction(1, 1).

% beam_directions([-1/-1, -1/0, -1/1, 0/-1, 0/1, 1/-1, 1/0, 1/1]).


convert_all_beams(Position, StartRow/StartCol, Them, Us, TurnOverList) :-
	findall(BeamList, (
		direction(DR, DC),
		R is StartRow + DR,
		C is StartCol + DC,
		valid_coordinate(R/C),
		convert_beam(Position, R/C, DR/DC, Them, Us, BeamList)
	), BeamLists),
	flatten(BeamLists, TurnOverList).


% Going in one direction.

% Position, Start, Direction, TheirColour, OurColour, TurnOverList
% convert_beam(Position, StartRow/StartCol, DRow/DCol, Them/Us, TurnOverList) :-

convert_beam(Position, SRow/SCol, _/_, _/Us, []) :-
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
	direction(DR, DC),
	R2 is R1 + DR,
	C2 is C1 + DC.


% Find squares with given value.

/*
find_squares_with(Position, Value, Results) :-
	setof(R/C, (), Results).
*/

% Need to scan through the rows in Position, keeping track of index positions, to capture matching entries.
% find_index_of(Value, List, Index)

% indexed_content(List, Index, Value)

/*


% move(Position1, Us/Them, R/C, Position2)

possible_move(Position, Us/Them, R/C) :-
	% Square is empty (s)
	% Adjacent to Them
	% Non-empty conversion list.


*/
