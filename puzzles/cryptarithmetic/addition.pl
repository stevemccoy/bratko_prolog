
% Cryptarithmetic solver for addition

% sum(N1, N2, N) -- N1, N2 and N are lists representing decimal numbers 
% such that N = N1 + N2.
sum(N1, N2, N) :-
	sum1(N1, N2, N, 0, 0, [0,1,2,3,4,5,6,7,8,9], _).

% sum1(N1, N2, N, Cin, Cout, DigIn, DigOut)
%
% N1 + N2 + Cin == N + carry Cout, where digits are chosen from DigIn and DigOut records 
% what digits are not used.
%
sum1([],[],[],0,0,Digs,Digs).
sum1([D1 | N1], [D2 | N2], [D | N], Cin, Cout, DigIn, DigOut) :-
	sum1(N1, N2, N, Cin, C2, DigIn, Digs),
	digitsum(D1, D2, C2, D, Cout, Digs, DigOut).


% Non-deterministic delete us
del(Item, List, List) :-
	nonvar(Item),!.					% Already instantiated.
del(Item, [Item | List], List).		% Delete the head.
del(Item, [A | List], [A | List1]) :-
	del(Item, List, List1).			% Delete item from the tail.

digitsum(D1, D2, Cin, D, Cout, DigIn, DigOut) :-
	del(D1, DigIn, Digs2),
	del(D2, Digs2, Digs3),
	del(D, Digs3, DigOut),
	S is D1 + D2 + Cin,
	D is S mod 10,
	Cout is S div 10.

puzzle1(
	[D,O,N,A,L,D], 
	[G,E,R,A,L,D], 
	[R,O,B,E,R,T]).

puzzle2(
	[0,S,E,N,D],
	[0,M,O,R,E],
	[M,O,N,E,Y]).


