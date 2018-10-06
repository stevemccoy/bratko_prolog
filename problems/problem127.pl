% Problem 127: Order the numbers. 
% (Prolog Solution - Steve McCoy, 01 September 2018)

% Arrange the numbers 1 through 16 such that the sum of adjacent numbers
% equals a square.  You must include all numbers in sequence, but each number 
% must appear only once.

% Delete an item from a list.
del(Item, [Item | List], List).
del(Item, [First | List1], [First | List2]) :-
	del(Item, List1, List2).

% These should be all the squares needed for the problem.
square(1, 1).
square(2, 4).
square(3, 9).
square(4, 16).
square(5, 25).
square(6, 36).

% Result is a solution to the problem 127 definition as above.
% Write out solution and find all other solutions on backtracking.
solution(Result) :-
	solution([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16], Result),
	write(Result).

% Solver for arbitrary list permutation.
solution([], []).
solution([X], [X]).
solution(L1, [X1 | Tail]) :-
	del(X1, L1, L2),
	continuation(X1, L2, Tail).

% Given a starting number and list of remaining items,
% find possible continuations to solve the problem.	
continuation(_, [], []).
continuation(X1, L1, [Y1 | Tail]) :-
	square(_, S1),
	S1 > X1,
	Y1 is S1 - X1,
	del(Y1, L1, L2),
	continuation(Y1, L2, Tail).
