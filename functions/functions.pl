
% Determine if the given list is a bound numerical vector.
bound_vector([]).
bound_vector([X | Tail]) :-
	number(X),
	bound_vector(Tail).

% Max and min of numerical arguments.
max(X, Y, Z) :-	
	bound_vector([X, Y]),
	max_number(X, Y, Z).

min(X, Y, Z) :-
	bound_vector([X, Y]),
	min_number(Y, X, Z).

% Supporting predicates for max and min.
max_number(X, Y, X) :-	
	Y =< X, !.
max_number(_, Y, Y).

min_number(X, Y, X) :-	
	Y >= X, !.
min_number(_, Y, Y).

