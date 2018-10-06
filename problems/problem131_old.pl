problem131_old(Solution) :-
	D1 = [1,2,3,4,5,6,7,8,9],
	permutation(D1, P1),
	P1 = [_,_,_, _,8,_, _,_,_],
	
	row1sum(P1, N),
	row2sum(P1, N),
	row3sum(P1, N),

	col1sum(P1, N),
	col2sum(P1, N),
	col3sum(P1, N),
	
	leaddiag(P1, N),
	otherdiag(P1, N),
	
	Solution = P1.

	
% Delete an item from a list.
del(Item, [Item | List], List).
del(Item, [First | List1], [First | List2]) :-
	del(Item, List1, List2).



% row1sum([_,_,_, _,_,_, _,_,_], N).

row1sum([X1,X2,X3, _,_,_, _,_,_], N) :- 
	N is X1 + X2 + X3.

row2sum([_,_,_, X1,X2,X3, _,_,_], N) :- 
	N is X1 + X2 + X3.

row3sum([_,_,_, _,_,_, X1,X2,X3], N) :- 
	N is X1 + X2 + X3.

col1sum([X1,_,_, X2,_,_, X3,_,_], N) :-
	N is X1 + X2 + X3.

col2sum([_,X1,_, _,X2,_, _,X3,_], N) :-
	N is X1 + X2 + X3.

col3sum([_,_,X1, _,_,X2, _,_,X3], N) :-
	N is X1 + X2 + X3.

leaddiag([X1,_,_, _,X2,_, _,_,X3], N) :-
	N is X1 + X2 + X3.

otherdiag([_,_,X1, _,X2,_, X3,_,_], N) :-
	N is X1 + X2 + X3.
	