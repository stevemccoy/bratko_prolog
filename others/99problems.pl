% 99 problems in Prolog.
% https://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/


% P01 - last element in a list.
my_last([A], A).
my_last([_ | T], A) :-
	my_last(T, A).

% P02 - last but one element of a list.
my_last_but_one([A, _], A).
my_last_but_one([_ | T], A) :-
	my_last_but_one(T, A).

% P03 - k-th element of a list.
element_at(H, [H | _], 1).
element_at(X, [_ | T], K) :-
	K > 1,
	J is K - 1,
	element_at(X, T, J).

% P04 - number of elements in a list.
my_count([], 0).
my_count([_ | T], N) :-
	my_count(T, M),
	N is M + 1.

% List concatenation.
conc([], L, L).
conc([H | Tail], L1, [H | L2]) :-
	conc(Tail, L1, L2).

% P05 - reverse a list.
my_reverse([], []).
my_reverse([H | T], Result) :-
	my_reverse(T, TR),
	conc(TR, [H], Result).

% P06 - list is a palindrome.
palindrome([]).
palindrome([_]) :- !.
palindrome([H | Tail]) :-
	conc(Mid, [H], Tail),
	palindrome(Mid).

% P07 - flatten nested list structure.

% P08 - eliminate successive duplicates of list elements.
my_compress([], []).
my_compress([A], [A]) :- !.
my_compress([A, A | Tail], Result) :-
	!,
	my_compress([A | Tail], Result).
my_compress([A, B | Tail], [A | Result]) :-
	my_compress([B | Tail], Result).

% P09 - pack consecutive duplicates of list elements into sublists.
my_pack([], []).
my_pack([A, A | Tail])
