:- module(pddltostrips_tr,[parser_ext/2]).

:- use_module(library(lists)).
:- use_module(pddl_domain,[parse_domain/5]).
:- use_module(pddl_problem,[parse_problem/4]).
:- use_module(module_lists).
%:- use_module(library(atom2term)).
%:- use_module(library(formulae)).
:- use_module(tokenizer,[tokenizer/2]).


parser_ext(0,0).

parser_ext((P,D),STRIPS_POP):-

	atom_codes(P,CH),
	atom_codes(D,CH2),

	tokenizer(CH,TokensProblem),
	tokenizer(CH2,TokensDomain),

	parse_problem(TokensProblem,Objects,Goals,Init),

	objects_pop(Objects,OBJ_POP),
	goals_pop(Goals,GOALS_POP),
	init_pop(Init,INIT_POP),
	append(OBJ_POP,GOALS_POP,PBL),
	append(INIT_POP,PBL,Problem),

	parse_domain(TokensDomain,OBJ_POP,Preconditions,Achieves,Deletes),

	achieves_pop(Achieves,ACHIEVES_POP),
	deletes_pop(Deletes,DELETES_POP),
%	preconditions_pop(Preconditions,PRE),
	append(ACHIEVES_POP,DELETES_POP,D1),
	append(Preconditions,D1,Domain),
	append(Domain,Problem,STRIPS_POP).


parser_ext(end_of_file,[(achieves(init,P) :- holds(P,init)),end_of_file]).


% Partial Order Planner (POP)

preconditions_pop([],[]).

preconditions_pop([preconditions(Name,Predicates)|PRE],PPP):-
	preconditions_pop_clause(preconditions(Name,Predicates),PEQ),
	difference(Predicates,PEQ,Predicates2),
	preconditions_pop_strips(preconditions(Name,Predicates2),PEQ,PP),
	preconditions_pop(PRE,PPT),
	append(PP,PPT,PPP).

preconditions_pop_clause([],[]).
preconditions_pop_clause(preconditions(_,[]),[]).

preconditions_pop_clause(preconditions(Name,[PSEQ|PSEQT]),[PSEQ|PSEQT2]):-
	PSEQ =..['=='|_],
	preconditions_pop_clause(preconditions(Name,PSEQT),PSEQT2).
		
preconditions_pop_clause(preconditions(Name,[PSNEQ|PSNEQT]),[PSNEQ|PSNEQT2]):-
	PSNEQ =..['\\=='|_],
	preconditions_pop_clause(preconditions(Name,PSNEQT),PSNEQT2).

preconditions_pop_clause(preconditions(_,[_|PSNT]),PSNT2):-
	preconditions_pop_clause(preconditions(_,PSNT),PSNT2).


preconditions_pop_strips(preconditions(N,P),[],[preconditions(N,P)]).

preconditions_pop_strips(preconditions(N,P),[PEQNEQ|[]],(preconditions(N,P):- PEQNEQ)).
	


achieves_pop([],[]).

achieves_pop([achieves(Name,Predicates)|ACH],Achieves_pop):-
	achieves_pop_strips(achieves(Name,Predicates),ACH1),
	achieves_pop(ACH,ACH2),!,
	append(ACH1,ACH2,Achieves_pop).


achieves_pop_strips([],[]).
achieves_pop_strips(achieves(_,[]),[]).

achieves_pop_strips(achieves(Name,[P|MP]),POP_STRIPS):-
	append([achieves(Name,P)],[],POP),!,
	achieves_pop_strips(achieves(Name,MP),POP2),
	append(POP,POP2,POP_STRIPS).


deletes_pop([],[]).

deletes_pop([deletes(Name,Predicates)|DL], Deletes_pop):-
	deletes_pop_strips(deletes(Name,Predicates),DL1),
	deletes_pop(DL,DL2),!,
	append(DL1,DL2,Deletes_pop).


deletes_pop_strips([],[]).

deletes_pop_strips(deletes(_Name,[]),[]).

deletes_pop_strips(deletes(Name,[P|MP]), POP_STRIPS):-
	append([deletes(Name,P)],[],POP),!,
	deletes_pop_strips(deletes(Name,MP),POP2),
	append(POP,POP2,POP_STRIPS).


objects_pop(Objects,OBJ_POP):-
	Objects =..[objects|OBJ_POP].


goals_pop(G,GOALS_POP):-
	G =..[goal|GOALS_POP].


init_pop(Init,HOLDS_POP):-
	Init =..[init|TAIL],
	holds(TAIL,HOLDS_POP).


holds([E|T],[H|T2]):-
	H =..[holds|[E,init]],
	holds(T,T2).

holds([],[]).




	




	

	
	

	

	



	

	

	









