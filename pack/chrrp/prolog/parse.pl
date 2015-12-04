% Parsing Module
%
% Part of the CHR-rp compiler.
%
% Author: Leslie De Koninck, K.U.Leuven, Belgium

:- module(parse,
	[
	    parse/2
	]).

:- use_module(database,
	[
	    constraint/3,
	    rule/7
	]).

% parse(Lines,Constraints,Rules,Remaining)

parse(Lines,Remaining) :- parse(Lines,1,Remaining).

parse([(:- chr_constraint(CDecl))|T],Counter,Rem) :- !,
	conj_to_list(CDecl,CList),
	parse_constraints(CList),
	parse(T,Counter,Rem).
parse([pragma(PL,PR)|T],Counter,Rem) :- !,
	parse_rule(PL,PR,Counter),
	NewCounter is Counter + 1,
	parse(T,NewCounter,Rem).
parse([H|T],Counter,[H|Rem]) :-
	parse(T,Counter,Rem).
parse([],_,[]).

parse_constraints([H|T]) :-
	(   H = F/A
	->  length(Modes,A),
	    maplist(=(?),Modes),
	    length(Types,A),
	    maplist(=(any),Types),
	    constraint(F/A,Modes,Types)
	;   H =.. [F|Args],
	    length(Args,A),
	    modes_and_types(Args,Modes,Types),
	    constraint(F/A,Modes,Types)
	),
	parse_constraints(T).
parse_constraints([]).

modes_and_types([+|As],[+|Ms],[any|Ts]) :- !, modes_and_types(As,Ms,Ts).
modes_and_types([-|As],[-|Ms],[any|Ts]) :- !, modes_and_types(As,Ms,Ts).
modes_and_types([?|As],[?|Ms],[any|Ts]) :- !, modes_and_types(As,Ms,Ts).
modes_and_types([+(T)|As],[+|Ms],[T|Ts]) :- !, modes_and_types(As,Ms,Ts).
modes_and_types([-(T)|As],[-|Ms],[T|Ts]) :- !, modes_and_types(As,Ms,Ts).
modes_and_types([?(T)|As],[?|Ms],[T|Ts]) :- !, modes_and_types(As,Ms,Ts).
modes_and_types([],[],[]).

% rule(Name,Priority,KeptHeads,RemovedHeads,Guard,Body)
parse_rule(==>(H,GB),priority(P),Counter) :-
	conj_to_list(H,KHL),
	(   GB = (G|B)
	->  conj_to_list(G,GL),
	    conj_to_list(B,BL)
	;   GL = [],
	    conj_to_list(GB,BL)
	),
	rule(Counter,no,P,KHL,[],GL,BL).
parse_rule(<=>(H,GB),priority(P),Counter) :-
	(   H = \(KH,RH)
	->  conj_to_list(KH,KHL),
	    conj_to_list(RH,RHL)
	;   conj_to_list(H,RHL),
	    KHL = []
	),
	(   GB = (G|B)
	->  conj_to_list(G,GL),
	    conj_to_list(B,BL)
	;   GL = [],
	    conj_to_list(GB,BL)
	),
	rule(Counter,no,P,KHL,RHL,GL,BL).
		

conj_to_list((L,R),[L|T]) :- !, conj_to_list(R,T).
conj_to_list(C,[C]).

	
