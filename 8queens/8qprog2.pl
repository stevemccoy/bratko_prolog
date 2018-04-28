
% Solution to the 8 queens problem

solution(Queens) :- 
	permutation([1,2,3,4,5,6,7,8], Queens),
	safe(Queens).

permutation([], []).
permutation([Head | Tail], PermList) :-
	permutation(Tail, PermTail),
	del(Head, PermList, PermTail).

del(Item, [Item | List], List).
del(Item, [First | List1], [First | List2]) :-
	del(Item, List1, List2).
	
safe([]).
safe([Queen | Others]) :-
	safe(Others),
	noattack(Queen, Others, 1).
	
noattack(_, [], _).
noattack(Y, [Y1 | YList], XDist) :-
	Y1-Y =\= XDist,
	Y-Y1 =\= XDist,
	Dist1 is XDist + 1,
	noattack(Y, YList, Dist1).

