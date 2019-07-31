% Utilities for manipulating lists and their contents.

% - Depends on compare.pl for compare/3


% Delete an item from a list.
del(Item, [Item | List], List).
del(Item, [First | List1], [First | List2]) :-
	del(Item, List1, List2).

% List concatenation.
conc([], L, L).
conc([H | Tail], L1, [H | L2]) :-
	conc(Tail, L1, L2).

lastItem1(Item, List) :-
	conc(_, [Item], List).

lastItem2(X, [X]).
lastItem2(X, [_ | Tail]) :-
	lastItem2(X, Tail).
	
append(X, L1, L2) :-
	conc(L1, [X], L2).

% S is a sub-sequence of L.	
sublist(S, L) :-
	conc(_, L2, L),
	conc(S, _, L2).
	

% Reorder the elements of one list into another.	
permutation([], []).
permutation([Head | Tail], PermList) :-
	permutation(Tail, PermTail),
	del(Head, PermList, PermTail).


% Partition list of numbers based on given pivot element.
% partition(InputList, Pivot, LowPart, HighPart).

partition([], _, [], []).

partition([X | Tail], Pivot, [X | LowPart], HighPart) :-
	compare_terms(X, Pivot, Delta),
	Delta > 0,
	!,
	partition(Tail, Pivot, LowPart, HighPart).

partition([X | Tail], Pivot, LowPart, [X | HighPart]) :-
	partition(Tail, Pivot, LowPart, HighPart).
	
	
% quicksort(List, SortedList) - Quicksort algorithm.
quicksort([], []).
quicksort([X | Tail], Sorted) :-
	partition(Tail, X, Small, Big),
	quicksort(Small, SortedSmall),
	quicksort(Big, SortedBig),
	conc(SortedSmall, [X | SortedBig], Sorted).


evenlength([]).
evenlength([_,_ | L]) :-
	evenlength(L).

oddlength([_]).
oddlength([_,_ | L]) :-
	oddlength(L).
	

% reverse(List1, List2) - List2 is the list of elements in List1 but in reversed order.

my_reverse([], []).
my_reverse(List1, [X | Tail]) :-
	conc(Start, [X], List1),
	my_reverse(Start, Tail),
	!.

palindrome(List) :-
	my_reverse(List, List).


shift([], []).
shift([X | Tail], List2) :-
	conc(Tail, [X], List2).


% translate(Digits, Names)

translate([], []).
translate([D | Tail], [DName | TailNames]) :-
	means(D, DName),
	translate(Tail, TailNames).

means(0, zero).
means(1, one).
means(2, two).
means(3, three).
means(4, four).
means(5, five).
means(6, six).
means(7, seven).
means(8, eight).
means(9, nine).


% dividelist(List, List1, List2) - Decompose List into List1 and List2.	

dividelist([], [], []).
dividelist([X | Tail], [X | Tail1], List2) :-
	dividelist(Tail, Tail1, List2).
dividelist([X | Tail], List1, [X | Tail2]) :-
	dividelist(Tail, List1, Tail2).

	
% S is a subset of list L.	This doesn't work yet for generating all subsets.

subset([], []).
subset([H | Tail], [H | Sub]) :-
	subset(Tail, Sub).
subset([_ | Tail], Sub) :-
	subset(Tail, Sub).


% Item in a list indexed by integer.

% get_item(Index, List, Item)

get_item(1, [Item | _], Item).
get_item(N, [_ | Tail], Item) :-
	N > 1,
	M is N -1,
	get_item(M, Tail, Item).

% set_item(Index, Before, Item, After)

set_item(1, [_ | Tail], Item, [Item | Tail]).
set_item(N, [Head | Tail], Item, [Head | Tail2]) :-
	N > 1,
	M is N - 1,
	set_item(M, Tail, Item, Tail2).


% Remove any instances of a value from a list.

remove_all(_, [], []).
remove_all(V, [V | Tail], TailAfter) :-
	!,
	remove_all(V, Tail, TailAfter).
remove_all(V, [H | Tail], [H | TailAfter]) :-
	remove_all(V, Tail, TailAfter).


% Zip the corresponding elements from two lists together.

zip_lists([], [], []).
zip_lists([Item1 | Tail1], [Item2 | Tail2], [[Item1, Item2] | ZippedTail]) :-
	length(Tail1, N),
	length(Tail2, N),
	zip_lists(Tail1, Tail2, ZippedTail).


% Flatten terms in list.

flatten([], []).
flatten([[] | T], Result) :-
	!,
	flatten(T, Result).
flatten([[H | T] | Rest], Result) :-
	!,
	flatten([H | T], HFlat),
	flatten(Rest, RestFlat),
	conc(HFlat, RestFlat, Result).
flatten([H | T], [H | TFlat]) :-
	flatten(T, TFlat).


% Map list elements from one list through a named function(X,Y), producing a second list.
my_map_list([], _, []).
my_map_list([X | InTail], Function, [Y | OutTail]) :-
	Goal =.. [Function, X, Y],
	Goal,
	my_map_list(InTail, Function, OutTail).

sum(X, Y, Z) :-
	Z is X + Y.


% Aggregate elements of a list successively by a named function(X,Y,Z), producing a scalar result.
my_aggregate_list([A], _, A).
my_aggregate_list([X, Y | InTail], Function, Result) :-
	Goal =.. [Function, X, Y, Z],
	Goal,
	my_aggregate_list([Z | InTail], Function, Result).

% Map adjacent pairs of list items onto the result of applying named function(X,Y,Z) to them.
my_map_adjacent_pairs([_], _, []).
my_map_adjacent_pairs([X, Y | InTail], Function, [Z | OutTail]) :-
	Goal =.. [Function, X, Y, Z],
	Goal,
	my_map_adjacent_pairs([Y | InTail], Function, OutTail).


maxlist(L, M) :-
	my_aggregate_list(L, max, M).

sumlist(L, S) :-
	my_aggregate_list(L, sum, S).

% Truth value of comparison between X and Y.
ordered_pair(X, Y, true) :- X =< Y, !.
ordered_pair(_, _, false).

% Generate a vector of repeat values gen_repeat_values(Value, Count, List).
gen_repeat_values(_, 0, []) :- !.
gen_repeat_values(Value, Count, [Value | Rest]) :-
	Count2 is Count - 1,	
	gen_repeat_values(Value, Count2, Rest).

% Test vector of repeated values.
test_repeat_values([], _, 0).
test_repeat_values([H | T], H, Count) :-
	test_repeat_values(T, H, Count2),
	Count is Count2 + 1.

% Test that a list is instantiated as a monotonic non-decreasing ordered sequence of values.
ordered(List) :-
	ground(List),
	my_map_adjacent_pairs(List, ordered_pair, Tests),
	test_repeat_values(Tests, true, _).

% subsum(Set, Sum, SubSum).
% SubSum is a subset of Set with the sum of values Sum.
subsum(_, 0, []).
subsum(Set, Sum, [H | T]) :-
	conc(_, [H | Remainder], Set),
	subsum(Remainder, Sum2, T),
	Sum is Sum2 + H.


% gcd(X, Y, Z) - Z is the greatest common divisor of X and Y.
gcd(X, X, X).
gcd(X, Y, D) :-
	X < Y,
	Y1 is Y - X,
	gcd(X, Y1, D).
gcd(X, Y, D) :-
	Y < X,
	gcd(Y, X, D).

% between(N1, N2, X) :- X is an integer between N1 and N2
between(N1, N2, N1) :-
	N1 =< N2.
between(N1, N2, X) :-
	N1 < N2,
	NewN1 is N1 + 1,
	between(NewN1, N2, X).


% split(List, Positives, Negatives).

split([], [], []).
split([X |Tail], Positives, [X | Negatives]) :-
	X < 0,
	!,
	split(Tail, Positives, Negatives).
split([X |Tail], [X | Positives], Negatives) :-
	split(Tail, Positives, Negatives).


% difference(Super, Sub, Diff) - Diff is the set of elements of Super which are not in Sub.
difference(S, [], S).
difference(Set, [H | T], Diff) :-
	remove_all(H, Set, D1),
	difference(D1, T, Diff).

% unifiable(List1, Term, List2) - List2 is the list of terms from List1 which can be unified with the given Term.
unifiable([], _, []).
unifiable([Term1 | Tail1], Term, List2) :-
	not(Term1 = Term),
	!,
	unifiable(Tail1, Term, List2).
unifiable([Term1 | Tail1], Term, [Term1 | Tail2]) :-
	unifiable(Tail1, Term, Tail2).


% Merge two sorted lists, producing a third list.
merge([], L, L).
merge(L, [], L).
merge([A | AT], [B | BT], [A | LT]) :-
	compare_terms(A, B, Delta),
	Delta >= 0,
	!,
	merge(AT, [B | BT], LT).
merge(AL, [B | BT], [B | LT]) :-
	merge(AL, BT, LT),
	!.

