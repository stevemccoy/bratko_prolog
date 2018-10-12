
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
	
	