use_module(library(clpfd)).

problem238 :-
	[A,B,C,D] ins 1..60,
	A + B + C + D #= 63,
	S #= A*B + B*C + C*D,
	labeling([max(S)], [A,B,C,D]),
	format('S = ~d, A = ~d, B = ~d, C = ~d, D = ~d\n', [S,A,B,C,D]).

