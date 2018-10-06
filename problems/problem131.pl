
problem131(Solution, Sum) :-

	% Set up possible domain for problem.
	getDomain(D0),
	
	% Choose elements to satisfy same sum 4 ways.
	chooseTwo(D0, D1, X1, X9, N),	% Leading diagonal
	chooseTwo(D1, D2, X3, X7, N),	% Other diagonal
	chooseTwo(D2, D3, X4, X6, N),	% Row 2
	chooseTwo(D3, [], X2, X8, N),	% Column 2
	
	% Check the other 4 ways against the total sum.
	Sum is N + 8,
	Sum is X1 + X2 + X3,	% Row 1
	Sum is X7 + X8 + X9,	% Row 3
	Sum is X1 + X4 + X7,	% Column 1
	Sum is X3 + X6 + X9,	% Column 3

	% Present the solution.
	Solution = [X1,X2,X3, X4,8,X6, X7,X8,X9].


% Display the list as a solution on the console.
writeSolution([X1,X2,X3, X4,X5,X6, X7,X8,X9]) :-
	write([X1,X2,X3]),nl,
	write([X4,X5,X6]),nl,
	write([X7,X8,X9]),nl,
	nl.

% Sequential integer sequence from which 8 is removed.
getDomain(Domain) :-
	sequence(1, 8, StartPoints),
	del(Start, StartPoints, _),
	sequence(Start, 9, D0),
	del(8, D0, Domain).

% Choose 2 elements from the list, making sum N.	
chooseTwo(D0, D2, X1, X2, N) :-
	del(X1, D0, D1),
	del(X2, D1, D2),
	N is X1 + X2.
	
% Delete an item from a list.
del(Item, [Item | List], List).
del(Item, [First | List1], [First | List2]) :-
	del(Item, List1, List2).

% Sequence of integers length N starting at B.
sequence(B, 0, []) :-
	integer(B).	
sequence(B, N, [B | Tail]) :-
	integer(B),
	integer(N),
	N > 0,
	M is N - 1,
	C is B + 1,
	sequence(C, M, Tail).
	
	
