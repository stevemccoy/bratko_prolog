
% Delete an item from a list.
del(Item, [Item | List], List).
del(Item, [First | List1], [First | List2]) :-
	del(Item, List1, List2).

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
	
sublist(S, L) :-
	conc(L1, L2, L),
	conc(S, L3, L2).
	

% Reorder the elements of one list into another.	
permutation([], []).
permutation([Head | Tail], PermList) :-
	permutation(Tail, PermTail),
	del(Head, PermList, PermTail).

	
% Partition list of numbers based on given pivot element.
% partition(InputList, Pivot, LowPart, HighPart).

partition([], _, [], []).

partition([X | Tail], Pivot, [X | LowPart], HighPart) :-
	X < Pivot, !,
	partition(Tail, Pivot, LowPart, HighPart).
	
partition([X | Tail], Pivot, LowPart, [X | HighPart]) :-
	partition(Tail, Pivot, LowPart, HighPart).
	

evenlength([]).
evenlength([_,_ | L]) :-
	evenlength(L).

oddlength([_]).
oddlength([_,_ | L]) :-
	oddlength(L).
	
