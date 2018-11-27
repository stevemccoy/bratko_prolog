digit(D) :- 
    member(D, [0,1,2,3,4,5,6,7,8,9]).

digit_sum(A, B, C, Sum, Carry) :-
    digit(A),
    digit(B),
    digit(C),
    X is A + B + C,
    Sum is X mod 10,
    Carry is X div 10.

palindrome_sum([A1,A2,A2,A1], [B1,B2,B2,B1], [S1,S2,S3,S2,S1]) :-
    digit(A1),
    A1 =\= 0,
    digit(B1),
    B1 =\= 0,
    digit_sum(A1, B1, 0, S1, C1),
    S1 =\= 0,
    digit(A2),
    digit(B2),
    digit_sum(A2, B2, C1, S2, C2),
    digit_sum(A2, B2, C2, S3, C3),
    digit_sum(A1, B1, C3, S2, S1),
    A3 is A1 * 10 + A2,
    B3 is B1 * 10 + B2,
    A3 >= B3.

write_digits([]).
write_digits([H | T]) :-
    write(H), write_digits(T).

write_sum(A, B, S) :-
    write_digits(A), write(" + "), write_digits(B), write(" = "), write_digits(S), nl.

problem133() :-
    setof(A/B/S, palindrome_sum(A, B, S), Solutions),
    length(Solutions, N),
    write(N), write(" solutions found: "), nl, !,
    member(A2/B2/S2, Solutions),
    write_sum(A2, B2, S2),
    fail.
