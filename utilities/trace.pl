% trace(Goal): Execute Prolog goal Goal displaying trace information.

trace(Goal) :-
	trace(Goal, 0).

trace(true, _) :- !.				% Red cut.

trace((Goal1, Goal2), Depth) :- !,		% Red cut.
	trace(Goal1, Depth),
	trace(Goal2, Depth).

trace(Goal, Depth) :-
	predicate_property(Goal, built_in),
	display('Call:', Goal, Depth),
	display('Exit:', Goal, Depth),
	display_redo(Goal, Depth).

trace(Goal, Depth) :-
	display('Call:', Goal, Depth),
	clause(Goal, Body),
	Depth1 is Depth + 1,
	trace(Body, Depth1),
	display('Exit:', Goal, Depth),
	display_redo(Goal, Depth).

trace(Goal, Depth) :-					% All alternatives exhausted.
	display('Fail:', Goal, Depth),
	fail.

display(Message, Goal, Depth) :-
	tab(Depth), write(Message),
	write(Goal), nl.

display_redo(Goal, Depth) :-
	true								% First succeed simply.
	;
	display('Redo:', Goal, Depth),		% Then report backtracking.
	fail.								% Force backtracking.
