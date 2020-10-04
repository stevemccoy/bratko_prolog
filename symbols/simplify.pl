% List concatenation.
conc([], L, L).
conc([H | Tail], L1, [H | L2]) :-
	conc(Tail, L1, L2).

% Arithmetic simplification.

% Rearrange symbols and numbers in expression.

% sum_terms_by_type(E, Terms, Atoms, Numbers).

% find_numbers(E, Numbers).

% find_terms(E, Op, List).

find_terms(E1, Op, Terms) :-
	E1 =.. [Op, E2, Term],
	!,
	find_terms(E2, Op, Rest),
	conc(Rest, [Term], Terms).
find_terms(E, Op, [E]).



simplify(E1 + E2, E) :-
	simplify(E1, E3),
	simplify(E2, E4),
	simplify(E3 + E4, E).



simplify(N1 + N2, N) :-
	number(N1),
	number(N2),
	N is N1 + N2,
	!.

simplify(N1 + E1, Expression) :-
	number(N1),
	simplify(E1, E2),
	simplify(E2 + N1, Expression).

simplify(S1 + S2, S2 + S1) :-
	atom(S1),
	atom(S2),
	sort([S1,S2],[S2,S1]),
	!.


	

simplify(E, E).

