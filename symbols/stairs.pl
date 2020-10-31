/*
	Stairs problem - Prolog solution.
	Succeed if X is a sequence of digits each one adjacent to the ones next to it.
 */

stairs(X) :-
	M is mod(X, 10) + 1,
	stairs(X, M).

stairs(X, Neighbour) :-
	divmod(X, 10, Q, R),
	1 is abs(R - Neighbour),
	(Q = 0 ; stairs(Q, R)).


