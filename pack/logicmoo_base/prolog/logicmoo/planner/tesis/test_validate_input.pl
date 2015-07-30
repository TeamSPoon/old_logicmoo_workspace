:- module(test_validate_input,_).

:- use_module(pddl_domain,[parse_domain/5]).
:- use_module(pddl_problem,[parse_problem/4]).


%parse_file(+File).
parse_file(F):-
	parse_domain(F,[a,b],P,A,D), nl, nl,
	write(F), nl, write('Preconditions: '), write(P), nl, nl, 
	write('Achieves: '), write(A), nl, nl,
	write('Deletes: '), write(D), nl, nl, nl.
%	parse_problem(F,O,I,G),!.

parse_file(F):-
	write('Parsing file failed. '), write('('), write(F), write(')'), nl.
	

test_colection:-
	parse_file('bkw1.pddl'),
	parse_file('bkw2.pddl'),
	parse_file('bkw3.pddl'),
	parse_file('bkwup1.pddl'),
	parse_file('bkwup2.pddl'),
	parse_file('bkwup3.pddl'),
	parse_file('bkwup4.pddl'),
	parse_file('bkwdp1.pddl'),
	parse_file('bkwdp2.pddl'),
	parse_file('bkwdp3.pddl'),
	parse_file('bkwdp4.pddl'),
	parse_file('bkwdp5.pddl'),
	parse_file('bkwdp6.pddl').

