%
% Problem 129
% 
% Your neighbor asks you to help cut his lawn.  You start at A and end at B and he starts at B and ends at A.  
% You or him must cut each square once and only once.  You can drive through a square or make a right turn, 
% but you are not allowed to cut a square diagonally.
%
% What is a possible route?
%

% Delete an item from a list.
del(Item, [Item | List], List).
del(Item, [First | List1], [First | List2]) :-
	del(Item, List1, List2).

% List concatenation.
concat([], L, L).
concat([H | Tail], L1, [H | L2]) :-
	concat(Tail, L1, L2).

% Append single item onto list.	
append(X, L1, L2) :-
	concat(L1, [X], L2).
	
% Generate all the coordinates allowable.
all_coordinates(L) :-
	Coords = [1,2,3,4,5,6,7,8,9],
	setof( X/Y, (member(X, Coords), member(Y, Coords)), L1),
	del(5/5, L1, L).


% Solve the problem by backtracking search.


% Find round trip tour using all available squares on the board, starting and returning to square 1/1.
problem129(Path) :-
	all_coordinates(Available1),
	Start = 1/1,
	!,
	search(Start, Available1, [Start], Path),
	Path = [Start | _].

% NEXT: Draw suggested route (ASCII?).


% move(Pos1, Pos2)

offset(-1, 0).
offset(1, 0).
offset(0, -1).
offset(0, 1).

move(X1/Y1, X2/Y2) :-
	offset(DX, DY),
	X2 is X1 + DX,
	X2 > 0,
	X2 < 10,
	Y2 is Y1 + DY,
	Y2 > 0,
	Y2 < 10.
	
% search(Position, Available, PathSoFar, FullPath)

search(_, [], Path, Path).
search(X1/Y1, Available, PathSoFar, FullPath) :-
	move(X1/Y1, X2/Y2),
	del(X2/Y2, Available, Available2),
	search(X2/Y2, Available2, [X2/Y2 | PathSoFar], FullPath).
	
% Path check property - path must not separate available spaces into two disconnected areas.
% path_splits_available(Path, Available) :-
	
% It seems like what we want here is a function to group the available points together into 
% mutually accessible groups, to determine the number of contiguous regions represented.


% Transfer an item from one list to another, if it is found, do nothing otherwise.
% transfer_item(Item, OldSource, OldDest, NewSource, NewDest)

transfer_item(Item, OldSource, OldDest, NewSource, [Item | OldDest]) :-
	del(Item, OldSource, NewSource),
	!.
transfer_item(Item, OldSource, OldDest, OldSource, OldDest).
	

% Transfer all items from a source to a destination.

transfer_all_items([], Source, Dest, Source, Dest).
transfer_all_items([Item | Tail], S1, D1, S2, D2) :-
	transfer_all_items(Tail, S1, D1, S3, D3),
	transfer_item(Item, S3, D3, S2, D2).


% extract_group(Available, Members, NonMembers)

extract_group([], [], []).
extract_group([P1 | Tail1], [P1 | Tail2], NonMembers) :-
	setof(P2, move(P1, P2), L1),
	transfer_all_items

	move
	
	% What about: setof( member(P1, AvailableCoords), move(P1,P2), ...
	% to map out all reaschable coordinates in the same group.

%

grow_group_from(Position, GroupBefore, AvailBefore, GroupAfter, AvailAfter) :-
	

