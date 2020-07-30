
cities([atlanta, boston, chicago, dallas, detroit, losangeles, newyork]).

s(atlanta, boston, 936).
s(atlanta, chicago, 589).
s(atlanta, dallas, 719).
s(atlanta, detroit, 534).
s(atlanta, losangeles, 1932).
s(atlanta, newyork, 746).

s(boston, chicago, 849).
s(boston, dallas, 1549).
s(boston, detroit, 1038).
s(boston, losangeles, 2591).
s(boston, newyork, 190).

s(chicago, dallas, 805).
s(chicago, detroit, 223).
s(chicago, losangeles, 1742).
s(chicago, newyork, 711).

s(dallas, detroit, 583).
s(dallas, losangeles, 1238).
s(dallas, newyork, 1370).

s(detroit, losangeles, 1594).
s(detroit, newyork, 882).

s(losangeles, newyork, 2445).


% Delete an item from a list.
del(Item, [Item | List], List).
del(Item, [First | List1], [First | List2]) :-
	del(Item, List1, List2).

% Reorder the elements of one list into another.	
permutation([], []).
permutation([Head | Tail], PermList) :-
	permutation(Tail, PermTail),
	del(Head, PermList, PermTail).


tsp(Path, Cost) :-
    cities(CityList),
    


% Need:

1. definition of a solution and a starting state.
2. integration with bestfirst.pl logic.
3. estimator for longest hamiltonian path through the cities given (used to prime the best so far path cost).
4. heuristic function to estimate minimum remaining path distance --> admissible h(N).
5. 

