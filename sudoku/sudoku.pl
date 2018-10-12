
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

