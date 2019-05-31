

% Definition of skills hierarchy.

% Skill area -- Parent 

% skill(Category, SkillName, [Alias, ... ], ParentSkill).


% Match name strings. Initially just do a case invariant string comparison.

matching_name(Name1, Name2) :-
	string_lower(Name1, LowerName),
	string_lower(Name2, LowerName).


% matching_skill_under(Parent, Name, MatchList)

matching_skill_under(Parent, Name, MatchList) :-
	setof(Match, (
		skill(Parent, Match),
		matching_name(Name, Match)
	), MatchList).


% find_skill(SkillName, Horizon, UpList, MatchList, DownList)



find_skill(Name, 0, [], Match, []) :-
	matching_skill_under(_, Name, Match).

find_skills(Name, Horizon, Up, Match, Down) :-
	Horizon > 0,
	

