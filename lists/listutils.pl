
% Delete an item from a list.
del(Item, [Item | List], List).
del(Item, [First | List1], [First | List2]) :-
	del(Item, List1, List2).

% Reorder the elements of one list into another.	
permutation([], []).
permutation([Head | Tail], PermList) :-
	permutation(Tail, PermTail),
	del(Head, PermList, PermTail).

