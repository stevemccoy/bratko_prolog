

% Definition of skills hierarchy.

% Skill area -- Parent 

% skill(Category, SkillName, [Alias, ... ], ParentSkill).


% Given: expressed a need for a specific named skill.
% Find: matching skills in the hierarchy based on name matching.



% Match name strings. Initially just do a case invariant string comparison.

matching_name(Name1, Name2) :-
	string_lower(Name1, LowerName),
	string_lower(Name2, LowerName).


simple_find_skill(Name, MatchList) :-
	setof(Match, (
		skill(_, Match),
		matching_name(Name, Match)
	), MatchList).


% matching_skill_under(Parent, Name, MatchList)

matching_skill_under(Parent, Name, MatchList) :-
	setof(Match, (
		skill(Parent, Match),
		matching_name(Name, Match)
	), MatchList).


matching_child_skill(Parent, Child) :-
	skill(Parent, Skill),
	matching_name(Skill, Child).


matching_skill_as_child(Name, Parent, Child) :-
	skill(Parent, Child),
	matching_name(Name, Child).

matching_skill_as_parent(Name, Parent, Child) :-
	skill(Parent, Child),
	matching_name(Name, Parent).


horizon_match(Name, 0, Parent, Child) :-
	matching_skill_as_child(Name, Parent, Child).

horizon_match(Name, Horizon, Ancestor, Child) :-
	Horizon > 0,
	NewHorizon is Horizon - 1,
	matching_skill_as_child(Name, Parent, Child),
	horizon_match(Name, NewHorizon, Ancestor, Parent).



% find_skill(SkillName, Horizon, UpList, MatchList, DownList)
% 
% 


find_skill(Name, 0, [], Match, []) :-
	matching_skill_under(_, Name, Match).

find_skills(Name, Horizon, Up, Match, Down) :-
	Horizon > 0.
	

% Find skills under given node 
% Children of the node.

children(Name, Children) :-
	setof(Child, (skill(Name, Child)), Children).

descendents(_, 0, []).
descendents(Name, Depth,  [Children | Descendents]) :-
	Depth > 0,
	NewDepth is Depth - 1,
	children(Name, Children),
	setof(Other, (
		member(Child, Children),
		descendents(Child, NewDepth, Others)
	), Descendents).
