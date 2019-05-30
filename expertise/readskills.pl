:- cd('C:/Users/uc257579/Documents/Prolog/bratko_prolog/expertise').

:- use_module(library(http/json)).



% List concatenation.
conc([], L, L).
conc([H | Tail], L1, [H | L2]) :-
	conc(Tail, L1, L2).

append(X, L1, L2) :-
	conc(L1, [X], L2).


% Read the given named file (in JSON format), producing a list of skill definitions.

grab_skills(FileName, Skills) :-
	retractall(skill(_, _)),
	% Read the file contents.
	open(FileName, read, Stream),
	json_read(Stream, JsonSkills),
	close(Stream),
	!,
	extract_skills('Top', JsonSkills, Skills).


% extract_skills(Parent, JS, Skills)

extract_skills(_, [], []).
extract_skills(Parent, [json(KVL) | Tail], [Parent --> Title | OtherSkills]) :-
	member(title=Title, KVL),
	member(children=Children, KVL),
	extract_skills(Title, Children, ChildSkills),
	extract_skills(Parent, Tail, TailSkills),
	conc(ChildSkills, TailSkills, OtherSkills),
	!.


% Write out to console.

write_skills([]).
write_skills([Skill | Tail]) :-
	writeln(Skill),
	write_skills(Tail).


% assert_skills(Skills).

assert_skills([]).
assert_skills([Parent --> Child | Rest]) :-
	assertz(skill(Parent, Child)),
	assert_skills(Rest).


% Output skill definitions to a file.


write_skill_clauses(Stream) :-
	clause(skill(Parent, Child), true),
	portray_clause(Stream, skill(Parent, Child)),
	fail.
write_skill_clauses(_).


write_skills_file(FileName) :-
	open(FileName, write, Stream),
	write_skill_clauses(Stream),
	close(Stream).


