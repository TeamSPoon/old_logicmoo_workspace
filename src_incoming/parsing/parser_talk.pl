
:- module(parser_talk,[]).

:- op(500,xfy,&).
:- op(510,xfy,=>).
:- op(1200,xfx,-->).
:- op(100,fx,¹).
% exampels 
% bertrand wrote principia 
% is bertrand an author
% bertrand is a author 
% every author is a programmer
% what did bertrand write
% what is a book
% what is a author
% principia is a book
% bertrand is bertrand
% shrdlu halts
% every student wrote a program 
% terry writes a program that halts 
% an author of every book wrote a program 
% Bertand wrote a book about gottlob
% What did Alfred give to Bertrand
% Who did alfred give a book to 

m :- main_loop.

:- asserta(thlocal:into_form_code).

:-export(main_loop/0).

main_loop :-
	write('>> '),
	read_sent(Words),
	talk(Words,Reply),
	print_reply(Reply),
	main_loop.

talk(Sentence,Reply) :-
	parse(Sentence,LF,Type),
	write(LF),nl,
	write(Type),nl,
	write(Sentence),nl,
	clausify(LF,Clause,FreeVars),
	write(Clause),nl,
	write(FreeVars),nl,
	!,
	reply(Type,FreeVars,Clause,Reply).

talk(_,error('too difficult')).

% reply a question
reply(query,FreeVars,
	(answer(Answer) :- Condition),Reply) :-
	(setof(Answer,FreeVars^Condition,Answers)
	->  Reply = answer(Answers)
	;   (Answer == yes
		-> Reply = answer([no])
		;  Reply = answer([none]))),!.
		
% reply an assertion
reply(assertion,_,Assertion,asserted(Assertion)) :-
	assert(Assertion),
	!.
reply(_,_,_,error('unknown type')).

print_reply(error(ErrorType)) :-
	write('Error: '),
	write(ErrorType),
	write('.'),
	nl.
print_reply(asserted(Assertion)) :-
	write('Asserted '),
	write(Assertion),
	write('.'),
	nl.
print_reply(answer(Answers)) :-
	print_answers(Answers).

print_answers([Answer]) :-
	write(Answer),
	write('.'),nl.
print_answers([Answer|Rest]) :-
	write(Answer),
	write(','),
	print_reply(answer(Rest)).

parse(Sentence,LF,query) :-
	q(LF,Sentence,[]).
parse(Sentence,LF,assertion) :-
	s(LF,nogap,Sentence,[]).

% Universals
clausify(all(X,F0),F,[X|V]) :-
	clausify(F0,F,V).

% Implications 
clausify(A0=>C0,(C:-A),V) :-
	clausify_literal(C0,C),
	clausify_antecedent(A0,A,V).

% Literals
clausify(C0,C,[]):-
	clausify_literal(C0,C).

% Literals
clausify_antecedent(L0,L,[]):-
	clausify_literal(L0,L).

% Conjunctions
clausify_antecedent(E0&F0,(E,F),V) :-
	clausify_antecedent(E0,E,V0),
	clausify_antecedent(F0,F,V1),
	conc(V0,V1,V).
	
% Existentials
clausify_antecedent(exists(X,F0),F,[X|V]) :-
	clausify_antecedent(F0,F,V).

clausify_literal(L,L).

% Grammar
% Questions
q(S => answer(X)) -->
 			whpron,vp(finite,X^S,nogap).
q(S => answer(X)) -->
 			whpron,sinv(S,gap(np,X)). 
q(S => answer(yes)) --> 
			sinv(S,nogap).
q(S => answer(yes)) -->
    copula,
    np((X^SO)^S, nogap),
    np((X^true)^exists(X,SO & true),nogap).

q(S => answer(S)) -->  s(S,nogap),[?].

q(Q) -->  q(Q),[?].


% Declarative sentences
s(S,GapInfo) -->
 		np(VP^S,nogap),
 		vp(finite,VP,GapInfo).

% Inverted sentences
sinv(S,GapInfo) --> 
	aux(finite/Form,VP1^VP2),
	np(VP2^S,nogap),
	vp(Form,VP1,GapInfo).
 
% Noun Phrases
np(NP,nogap) -->
 	det(N2^NP),n(N1),optrel(N1^N2).
np(NP,nogap) --> pn(NP).
np((X^S)^S,gap(np,X)) --> [].

% Verb  phrases
vp(Form,X^S,GapInfo) -->
	tv(Form,X^VP),
	np(VP^S,GapInfo).
vp(Form,VP,nogap) -->
	iv(Form,VP).
vp(Form1,VP2,GapInfo) -->
	aux(Form1/Form2,VP1^VP2),
	vp(Form2,VP1,GapInfo).
vp(Form1,VP2,GapInfo) -->
  	rov(Form1/Form2,NP^VP1^VP2),
  	np(NP,GapInfo),
 	vp(Form2,VP1,nogap).
vp(Form2,VP2,GapInfo) -->
	rov(Form1/Form2,NP^VP1^VP2),
	np(NP,nogap),
	vp(Form1,VP1,GapInfo).
vp(finite,X^S,GapInfo) -->
	copula,
	np((X^P)^exists(X,S&P),GapInfo).


% relative clauses
optrel((X^S1)^(X^(S1&S2))) -->
	relpron,vp(finite,X^S2,nogap).
optrel((X^S1)^(X^(S1&S2))) -->
	relpron,s(S2,gap(np,X)).
optrel(N^N) --> [].

% Dictionary
% preterminals

det(LF) --> [D],{det(D,LF)}.
n(LF) 	--> [N],{n(N,LF)}.
pn((E^S)^S) --> [PN],{pn(PN,E)}.

aux(Form,LF) --> [Aux],{aux(Aux,Form,LF)}.
relpron --> [RP],{relpron(RP)}.
whpron --> [WH], {whpron(WH)}.

copula --> [C], {copula(C)}.

iv(nonfinite,		LF) --> [IV],{iv(IV,_,_,_,_,LF)}.
iv(finite,			LF) --> [IV],{iv(_,IV,_,_,_,LF)}.
iv(finite,			LF) --> [IV],{iv(_,_,IV,_,_,LF)}.
iv(past_participle,	LF) --> [IV],{iv(_,_,_,IV,_,LF)}.
iv(pres_participle,	LF) --> [IV],{iv(_,_,_,_,IV,LF)}.

tv(nonfinite,		LF) --> [TV],{tv(TV,_,_,_,_,LF)}.
tv(finite,			LF) --> [TV],{tv(_,TV,_,_,_,LF)}.
tv(finite,			LF) --> [TV],{tv(_,_,TV,_,_,LF)}.
tv(past_participle,	LF) --> [TV],{tv(_,_,_,TV,_,LF)}.
tv(pres_participle,	LF) --> [TV],{tv(_,_,_,_,TV,LF)}.


rov(nonfinite	/Requires,LF) 
	-->	[ROV], {rov(ROV,_,_,_,_,LF,Requires)}.
rov(finite		/Requires,LF) 
	-->	[ROV], {rov(_,ROV,_,_,_,LF,Requires)}.
rov(finite		/Requires,LF) 
	-->	[ROV], {rov(_,_,ROV,_,_,LF,Requires)}.
rov(past_participle	/Requires,LF) 
	-->	[ROV], {rov(_,_,_,ROV,_,LF,Requires)}.
rov(pres_participle	/Requires,LF) 
	-->	[ROV], {rov(_,_,_,_,ROV,LF,Requires)}.
	
% Lexical Items

relpron(that).
relpron(who).
relpron(whom).

whpron(who).
whpron(whom).
whpron(what).

copula(is).



det(every,	(X^S1)^(X^S2)^ all(X,S1=> S2)).
det(a,		(X^S1)^(X^S2)^ exists(X,S1& S2)).
det(some,	(X^S1)^(X^S2)^ exists(X,S1& S2)).

n(author, 		X^author(X)		).
n(book,	 		X^book(X)		).
n(professor, 		X^professor(X)	).
n(program, 		X^program(X)	).
n(programmer, 	X^programmer(X)	).
n(student, 		X^student(X)		).


pn(begriffsschrift	,begriffsschrift).
pn(bertrand		,bertrand		).
pn(bill			,bill			).
pn(gottlob		,gottlob		).
pn(lunar			,lunar		).
pn(principia		,principia		).
pn(shrdlu			,shrdlu		).
pn(terry			,terry		).

iv(	halt,		halts,	halted,
	halted,	halting,	X^halt(X)).

tv(	write,		writes,		wrote,
	written,		writing,		X^Y^writes(X,Y)).
tv(	meet,		meets,		met,
	met,			meeting,		X^Y^meets(X,Y)).
tv(	concern,		concerns,		concerned,
	concerned,	concerning,	X^Y^concerns(X,Y)).
tv(	run,			runs,		ran,
	run,			running,		X^Y^runs(X,Y)).

rov(want,	wants,	wanted,
	wanted,	wanting,
	((X^want(Y,X,Comp))^S) ^(X^Comp) ^Y ^S,infinitival).
%semantics is partially execution of 
% NP ^VP ^Y ^NP(X want(Y,X,VP(X)))
%((X^¹want(Y,X,Comp))^S) ^(X^Comp) ^Y ^S,
% form of VP required:
%infinitival).

aux(to , 		infinitival/nonfinite , 	VP^VP).
aux(does , 	finite/nonfinite , 		VP^VP).
aux(did , 		finite/nonfinite , 		VP^VP).

conc([],L,L).
conc([H|T],L,[H|R]) :- conc(T,L,R).

% input
read_sent(Words) :- 
				get0(Char),
				read_sent(Char,Words).
% Newlines
read_sent(C,[]) :- newline(C), !.

% Spaces are skipped
read_sent(C,Words) :- space(C),!,
			get0(Char),
			read_sent(Char,Words).

% The Rest
read_sent(Char,[Word|Words]) :- 
	read_word(Char,Chars,Next),
	name(Word,Chars) ,
	read_sent(Next,Words).

read_word(C,[],C) :- space(C),!.
read_word(C,[],C) :- newline(C),!.

read_word(Char,[Char|Chars],Last) :- 
	get0(Next),
	read_word(Next,Chars,Last).

space(32). % :- name(C,[32]).

newline(10). % :- name(C,[10]).


:- retract(thlocal:into_form_code).


:- context_module(CM),module_predicates_are_exported(CM).
:- context_module(CM),module_meta_predicates_are_transparent(CM).
% :- context_module(CM),module_property(CM, exports(List)),moo_hide_show_childs(List).












