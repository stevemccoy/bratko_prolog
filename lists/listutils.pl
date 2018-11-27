% Utilities for manipulating lists and their contents.

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
subset(L, S) :-
	setof(M, member(M, L), L1),
	quicksort(S, S1),
	quicksort(L1, L2),
	dividelist(L2, S1, _).
	

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

