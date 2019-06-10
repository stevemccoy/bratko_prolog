
% Sudoku Puzzle Solver

% Constraint satisfaction problem (CSP) with the following options in each cell: 
universe([1,2,3,4,5,6,7,8,9]).

% Row and column coordinates from the following set:
coordinates([1,2,3,4,5,6,7,8,9]).


% Populate an initial state for the puzzle.

make_grid(Grid) :-
	coordinates(C),
	length(C, N),
	make_rows(N, Grid).

make_rows(0, []).
make_rows(N, [Head | Tail]) :-
	N > 0,
	make_row(Head),
	M is N - 1,
	make_rows(M, Tail).

make_row(Row) :-
	coordinates(C),
	length(C, N),
	make_row(N, Row).

make_row(0, []).
make_row(N, [Head | Tail]) :-
	N > 0,
	make_cell(Head),
	M is N - 1,
	make_row(M, Tail).

make_cell(L1) :-
	universe(L1).


% Convert from (Row, Col) to (Group, Position) and back.

convert_position(Row/Col, Grp/Pos) :-
	integer(Row),
	integer(Col),
	!,
	ZR is Row - 1,
	ZC is Col - 1,
	divmod(ZR, 3, RQ, RR),
	divmod(ZC, 3, CQ, CR),
	Grp is RQ * 3 + CQ + 1,
	Pos is CR + RR * 3 + 1.

convert_position(Row/Col, Grp/Pos) :-
	integer(Grp),
	integer(Pos),
	!,
	ZG is Grp - 1,
	ZP is Pos - 1,
	divmod(ZG, 3, GQ, GR),
	divmod(ZP, 3, PQ, PR),
	Row is GQ * 3 + PQ + 1,
	Col is PR + GR * 3 + 1.


% Show the generated position conversions.

show_positions1() :-
	coordinates(CL),
	member(R, CL),
	member(C, CL),
	convert_position(R/C, Position),
	write(R/C),
	write(" ---> "),
	writeln(Position).

show_positions2() :-
	coordinates(CL),
	member(G, CL),
	member(P, CL),
	convert_position(Position, G/P),
	write(G/P),
	write(" <--- "),
	writeln(Position).


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

% Remove any instances of a value from a list.

remove_all(_, [], []).
remove_all(V, [V | Tail], TailAfter) :-
	!,
	remove_all(V, Tail, TailAfter).
remove_all(V, [H | Tail], [H | TailAfter]) :-
	remove_all(V, Tail, TailAfter).


% Get and set the value of a given square.

get_square(R/C, Grid, Value) :-
	get_indexed_item(R, Grid, Row),
	get_indexed_item(C, Row, Value).

set_square(R/C, Before, V, After) :-
	get_indexed_item(R, Before, Row),
	set_indexed_item(C, Row, V, NewRow),
	set_indexed_item(R, Before, NewRow, After).


% Propagate number setting from one square.


% Resolve given square to a single value.

resolve_square(R/C, Before, V, After, AllExtras) :-
	% Make sure V is still an option for this square.
	get_square(R/C, Before, Cell),
	member(V, Cell),
	!,
	remove_from_row(R, Before, V, A1, RowExtras),
	remove_from_column(C, A1, V, A2),
	scan_for_column_extras(A1, A2, C, V, ColExtras),
	convert_position(R/C, G/_),
	remove_from_group(G, A2, V, A3),
	scan_for_group_extras(A2, A3, G, V, GroupExtras),
	conc(RowExtras, ColExtras, X1),
	conc(X1, GroupExtras, AllExtras),
	% Having removed V from everywhere else, add it back where needed.
	set_square(R/C, A3, [V], After).

% propagate_in_row(R/C, Before, Value, After)

remove_from_row(R, GridBefore, V, GridAfter, Extras) :-
	get_indexed_item(R, GridBefore, RowBefore),
	remove_from_row(RowBefore, V, RowAfter),
	find_row_extras(R, RowBefore, RowAfter, V, Extras),
	set_indexed_item(R, GridBefore, RowAfter, GridAfter).

remove_from_row([], _, []).
remove_from_row([Cell1 | Tail1], Value, [Cell2 | Tail2]) :-
	remove_all(Value, Cell1, Cell2),
	remove_from_row(Tail1, Value, Tail2).


% remove_from_column(C, GridBefore, Value, GridAfter)

remove_from_column(_, [], _, []).
remove_from_column(C, [RowBefore | TailBefore], Value, [RowAfter | TailAfter]) :-
	remove_from_row_col(RowBefore, C, Value, RowAfter),
	remove_from_column(C, TailBefore, Value, TailAfter).

remove_from_row_col(RowBefore, ColIndex, Value, RowAfter) :-
	get_indexed_item(ColIndex, RowBefore, Cell1),
	remove_all(Value, Cell1, Cell2),
	set_indexed_item(ColIndex, RowBefore, Cell2, RowAfter).


% remove_from_group(G, GridBefore, Value, GridAfter)

remove_from_group(G, GridBefore, V, GridAfter) :-
	universe(Universe),
	remove_from_group_positions(G, Universe, V, GridBefore, GridAfter).

remove_from_group_positions(_, [], _, Grid, Grid).
remove_from_group_positions(G, [Pos1 | Others], V, Before, After) :-
	convert_position(R/C, G/Pos1),
	get_square(R/C, Before, Cell1),
	remove_all(V, Cell1, Cell2),
	set_square(R/C, Before, Cell2, Grid),
	remove_from_group_positions(G, Others, V, Grid, After).

% After reducing options in a row, column or group, find any other resolved cells.

find_row_extras(R, RowBefore, RowAfter, V, Extras) :-
	coordinates(Coords),
	findall(R/C/V2, (
		member(C, Coords),
		get_indexed_item(C, RowAfter, [V2]),
		integer(V2),
		V \== V2,
		not(member([V2], RowBefore))
	), Extras).

scan_for_column_extras(GridBefore, GridAfter, C, V, Extras) :-
	coordinates(Coords),
	findall(R/C/V2, (
		member(R, Coords),
		get_square(R/C, GridAfter, [V2]),
		integer(V2),
		V \== V2,
		not(get_square(R/C, GridBefore, [V2]))
	), Extras).

scan_for_group_extras(GridBefore, GridAfter, G, V, Extras) :-
	coordinates(Coords),
	findall(R/C/V2, (
		member(Pos, Coords),
		convert_position(R/C, G/Pos),
		get_square(R/C, GridAfter, [V2]),
		integer(V2),
		V \== V2,
		not(get_square(R/C, GridBefore, [V2]))
	), Extras).


% Find any reductions in any row of the grid, based on available positions for
% each universe value.

find_reductions_in_any_row(Grid, Reductions) :-
	coordinates(Coords),
	!,
	findall(R/C/V, (
		member(R, Coords),
		get_indexed_item(R, Grid, Row),
		find_reductions_in_row(R, Row, RowReductions),
		member(R/C/V, RowReductions)
	), Reductions).


% Find any further reductions possible in a single row, based on positions for
% each universe value.

find_reductions_in_row(R, Row, Reductions) :-
	unresolved_values_in_row(Row, Values),
	!,
	findall(R/C/V1, (
		member(V1, Values),
		indexed_value_options(V1, Row, [C])
	), Reductions).


% Which of the possible cell values are still not resolved in the given row?

unresolved_values_in_row(Row, Values) :-
	universe(Universe),
	!,
	setof(V, (
		member(V, Universe),
		not(member([V], Row))
	), Values).


% What are the indices of the cells in the row which contain the given value?

indexed_value_options(Value, Row, Indices) :-
	coordinates(Coords),
	!,
	findall(Index, (
		member(Index, Coords),
		get_indexed_item(Index, Row, Values),
		member(Value, Values)
	), Indices).


% List concatenation.
conc([], L, L).
conc([H | Tail], L1, [H | L2]) :-
	conc(Tail, L1, L2).

% String concatenation.

strcat([], "").
strcat(StringList, S) :-
	strcat_codes(StringList, CodeList),
	string_codes(S, CodeList).

strcat_codes([], []).
strcat_codes([H | T], L) :-
	string_codes(H, HL),
	strcat_codes(T, TL),
	conc(HL, TL, L).


to_console([]).
to_console([S | Tail]) :-
	write(S), nl,
	to_console(Tail).


% repeat_term(N, Term, List) :-

repeat_term(0, _, []).
repeat_term(N, T, [T | Tail]) :-
	N > 0,
	M is N - 1,
	repeat_term(M, T, Tail).


% Display grid options.

display_grid(Grid) :-
	display_grid(Grid, StringList),
	nl,
	to_console(StringList).


display_grid(Grid, StringList) :-
	findall(RS, (
		member(Row, Grid), 
		display_row(Row, RSL), 
		member(RS, RSL)
	), StringList).

% Display row.

display_row(Row, [RS1, RS2, RS3, ""]) :-
	display_row_slice(Row, [1,2,3], RS1),
	display_row_slice(Row, [4,5,6], RS2),
	display_row_slice(Row, [7,8,9], RS3).


display_row_slice([], _, "").
display_row_slice(Row, Slice, String) :-
	findall(CS, (member(Cell, Row), display_cell_slice(Cell, Slice, CS)), SL),
	!,
	strcat(SL, String).

% Display cell slice.

display_cell_slice(Cell, Slice, String) :-
	findall(E, (member(E, Slice), member(E, Cell)), L),
	!,
	length(Slice, NS),
	length(L, N),
	M is NS - N,
	repeat_term(M, " ", Padding),
	conc(Padding, L, L2),
	strcat([" " | L2], String).

position(_).

clear :-
	make_grid(Grid),
	assert(position(Grid)),
	display_grid(Grid).

move(R/C/V) :-
	position(Grid),
	resolve_square(R/C, Grid, V, After, _),
	!,
	retractall(position(_)),
	assert(position(After)),
	display_grid(After).

moves([]).
moves([R/C/V | Tail]) :-
	move(R/C/V),
	!,
	moves(Tail).


% Prime a puzzle from initial layout.
% setup_sudoku(Layout, Grid)

% *** EVERYTHING BELOW HERE NEEDS TESTING....   ***

% 1. DONE: Define structure for layout, moves, grid.
% 2. DONE: Capture initial state of the puzzle grid as a layout.
% 3. DONE: Convert layout to a set of moves, then use each move to reduce the 
% unconstrained grid and propagate the solution to the puzzle.
%
% 4. Display the state of the puzzle as a grid of options, or as a layout of fully
% resolved values per square.
%
% 5. Capture image of the initial layout from camera; detect grid lines and use to
% locate and rescale characters; OCR technology to classify each of the detected
% characters and populate an initial layout for puzzle; solve and produce an image
% overlay showing the solution.
%
% 6. DONE: Need to propagate extra resolutions through row and col and group
% eliminations, avoiding capture of previous resolutions in the extras list.


setup_sudoku(Layout, Grid) :-
	make_grid(Grid0),
	reduce_by_rows(Layout, Grid0, Grid).

reduce_by_rows(Layout, GridBefore, GridAfter) :-
	layout_moves(Layout, AllMoves),
	reduce_by_moves(GridBefore, AllMoves, GridAfter).

layout_moves(Layout, AllMoves) :-
	findall(RowIndex/ColumnIndex/Value, (
		coordinates(RowIndices),
		member(RowIndex, RowIndices),
		get_indexed_item(RowIndex, Layout, Row),
		coordinates(ColumnIndices),
		member(ColumnIndex, ColumnIndices),
		get_indexed_item(ColumnIndex, Row, Value),
		integer(Value)
	), AllMoves).

reduce_by_moves(Grid, [], Grid).
reduce_by_moves(GridBefore, [R/C/V | Tail], GridAfter) :-
	resolve_square(R/C, GridBefore, V, Grid2, Extras),
	!,
	reduce_by_moves(Grid2, Extras, Grid3),
	reduce_by_moves(Grid3, Tail, GridAfter).


empty_layout([
	[_,_,_, _,_,_, _,_,_],
	[_,_,_, _,_,_, _,_,_],
	[_,_,_, _,_,_, _,_,_],
	[_,_,_, _,_,_, _,_,_],
	[_,_,_, _,_,_, _,_,_],
	[_,_,_, _,_,_, _,_,_],
	[_,_,_, _,_,_, _,_,_],
	[_,_,_, _,_,_, _,_,_],
	[_,_,_, _,_,_, _,_,_]
]).

dummy_layout([
	[5,_,_, 9,_,_, _,_,_],
	[_,1,_, _,_,_, _,_,_],
	[_,_,_, _,5,_, _,_,_],
	[_,_,_, 4,_,_, _,8,_],
	[_,_,_, _,_,_, _,_,_],
	[_,_,_, _,_,_, _,_,_],
	[3,_,_, 7,_,_, _,_,_],
	[_,_,6, _,_,_, _,_,_],
	[_,_,_, _,_,_, 2,_,_]
]).


% Determine if the sudoku puzzle grid is a solved state.

is_solved_grid([]).
is_solved_grid([Row | TailRows]) :-
	is_solved_row(Row),
	is_solved_grid(TailRows).

is_solved_row([]).
is_solved_row([[Value] | Tail]) :-
	integer(Value),
	is_solved_row(Tail).

% Display move list.

display_moves([]).
display_moves([R/C/V | Moves]) :-
	write('row:'),
	write(R),
	write(', col:'),
	write(C),
	write(', value:'),
	writeln(V),
	display_moves(Moves).


% Convert from grid back to layout of fully resolved cells.

layout_from_grid(Grid, After) :-
	empty_layout(Before),
	findall(RowIndex/ColumnIndex/Value, (
		coordinates(RowIndices),
		member(RowIndex, RowIndices),
		get_indexed_item(RowIndex, Grid, Row),
		coordinates(ColumnIndices),
		member(ColumnIndex, ColumnIndices),
		get_indexed_item(ColumnIndex, Row, [Value]),
		integer(Value)
	), AllMoves),
	setup_layout_from_moves(Before, AllMoves, After).

setup_layout_from_moves(Layout, [], Layout).
setup_layout_from_moves(Before, [R/C/V | OtherMoves], After) :-
	set_square(R/C, Before, V, Layout),
	setup_layout_from_moves(Layout, OtherMoves, After).


% Display layout.

display_layout(Layout) :-
	to_console(Layout).


% Solve a Sudoku puzzle from an empty grid by provided set of square fillings.

solve(Moves, FinalGrid) :-
	make_grid(Grid0),
	reduce_by_moves(Grid0, Moves, Grid1),
	further_reductions(Grid1, FinalGrid).


% Iterate the reductions of a grid using various tactics.

futher_reductions(Grid1, FinalGrid) :-
	find_reductions_in_any_row(Grid1, Reductions),
	Reductions = [_ | _],
	!,
	reduce_by_moves(Grid1, Reductions, Grid2),
	further_reductions(Grid2, FinalGrid).

further_reductions(Grid, Grid).
