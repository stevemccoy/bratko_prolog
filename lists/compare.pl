
% list(L) - L is a list.

list([]).
list([_ | _]).

% term_type(Term, Type)

term_type(Term, 1) :- number(Term).
term_type(Term, 2) :- atom(Term).
term_type(Term, 3) :- string(Term).
term_type(Term, 4) :- list(Term).


% compare_terms(X1, X2, Delta) - Delta is +ve if X1 < X2, 0 if they are equal, -ve if X1 > X2.

compare_terms(X, X, 0) :-
	!.

compare_terms(X1, X2, Delta) :-
	number(X1),
	number(X2),
	!,
	Delta is X2 - X1.

compare_terms(A1, A2, Delta) :-
	atom(A1),
	atom(A2),
	!,
	atom_codes(A1, L1),
	atom_codes(A2, L2),
	list_compare(L1, L2, Delta).

compare_terms(S1, S2, Delta) :-
	string(S1),
	string(S2),
	!,
	string_compare(S1, S2, Delta).
	
compare_terms(L1, L2, Delta) :-
	list(L1),
	list(L2),
	!,
	list_compare(L1, L2, Delta).

	
compare_terms(X1, X2, Delta) :-
	term_type(X1, T1),
	term_type(X2, T2),
	!,
	Delta is T2 - T1.
	
	
% list_compare(L1, L2, Delta) - Delta is +ve if L1 < L2, 0 if they are equal, -ve if L1 > L2.

list_compare([], [], 0).
list_compare([], [_ | _], 1).
list_compare([_ | _], [], -1).

list_compare([X1 | Tail1], [X2 | Tail2], Delta) :-
	compare_terms(X1, X2, 0),
	!,
	list_compare(Tail1, Tail2, Delta).
	
list_compare([X1 | _], [X2 | _], Delta) :-
	compare_terms(X1, X2, Delta).
	

% string_compare(S1, S2, Delta)

string_compare(S1, S2, Delta) :-
	string_codes(S1, L1),
	string_codes(S2, L2),
	!,
	list_compare(L1, L2, Delta).
	

