:- module(continuous_pop,_).
%:- module(continuous_pop,[initialplan/2,continuouspop/2,perception/1,action/1,current_perceptions/1]).
:- use_module(pop,[solve/3,seq/2,solve_open_preconditions/3, add_these_preconds/4]).
:- use_module(goalgen,[find_new_goals/1,find_depth_bound/1]).
:- use_module(library(write)).
:- use_module(library(lists)).
:- use_module(soccer_strips,[achieves/2,preconditions/2,deletes/2,holds/2,'<-'/2,'<<'/2]).
:- use_module(library(concurrency)).



% Definitions:
% An agenda is a list of goals.
% A goal is of the form goal(P,NA) where P is a proposition 
%   that is a precondition of action instance index NA.
% A plan is of the form plan(As,Os,Ls) where 
%   As is a list of actions instances,
%      An action instance is of the form act(N,A) where N is an integer 
%      and A is an action (this is needed in case there are multiple 
%      instances of the same action). N is the action instance index.
%   Os is a list of ordering constraints (A1<A2) where A1 & A2 
%      are action instance indexes.
%   Ls is a list of causal links.
% A causal link is of the form cl(A0,P,A1) where A0 and A1 are action 
%   instance indexes, and P is a proposition that is a precondition of 
%   action A1 means that A0 is making P true for A1.
:- op(1200,xfx,[<-,<<]).

:- concurrent perception/1.
:- concurrent action/1.
:- data [perceptions/1, current_perceptions/1].

action(noop).

main:- 
	initlog('./cpop.log'),
	initperlist,
	initialplan(Plan,DB),
%	find_depth_bound(DB),
	eng_call(controlador,create,create),
	eng_call(continuouspop(Plan,DB),create,create).

initlog(NombreArchivo):-
	open(NombreArchivo,write,Stream),
	set_output(Stream).


initialplan(Plan,DB):-
	find_new_goals(Goals),
	display('la meta es : '), display(Goals), nl,
	find_depth_bound(DB),
	solve(Goals,Plan,DB).

initialplan_seq(Plan,Seq):-
	find_new_goals(Goals),
	find_depth_bound(DB),
	solve(Goals,Plan,DB),
	seq(Plan,Seq).


controlador:- 
	get_perceptions(Percepts),
	assertz_fact(perception(Percepts)),
	retract_fact(action(Act)),  % ojo
	display(Act), nl,!,
	controlador.


get_perceptions(Percepts):-
	retract_fact(perceptions(Percepts)).

%Para blocks
% initperlist:-
% 	%assertz_fact(perceptions([clear(table),handempty,on(a,table),on(b,e),on(e,table),on(c,f),on(f,table),on(d,g),on(g,table),clear(a),clear(b),clear(c),clear(d)])),
% 	%el agente llevando d
% 	%assertz_fact(perceptions([clear(table),holding(d),on(a,table),on(b,e),on(e,table),on(c,f),on(f,table),on(g,table),clear(a),clear(b),clear(c),clear(g)])),
% 	%otro agente pone on(d,b)
%         assertz_fact(perceptions([clear(table),on(a,table),on(b,e),on(e,table),on(c,f),on(f,table),on(d,b),on(g,table),clear(a),clear(g),clear(c),clear(d)])),
% 	% %el agente llevando el bloque c
%         % assertz_fact(perceptions([clear(table),on(a,table),on(b,e),on(e,table),on(f,table),on(d,b),on(g,table),clear(a),clear(g),clear(c),clear(d),clear(f),holding(c)])),
% 	%la accion falla y el bloque c cae sobre a, ie, on(c,a)
%         assertz_fact(perceptions([clear(table),on(a,table),on(b,e),on(e,table),on(c,a),on(f,table),on(d,b),on(g,table),clear(g),clear(c),clear(d),clear(f)])),	
% 	% %el agente llevando el bloque c
%         % assertz_fact(perceptions([clear(table),on(a,table),on(b,e),on(e,table),on(f,table),on(d,b),on(g,table),clear(a),clear(g),clear(c),clear(d),clear(f),holding(c)])),
% 	%todo ok
%         assertz_fact(perceptions([clear(table),on(a,table),on(e,table),on(b,e),on(d,b),on(c,d),on(f,table),on(g,table),clear(g),clear(f),clear(c),clear(a)])).

%Par soccer_strips:

initperlist:-
	% % INITIAL SITUATION
%        assertz_fact(perceptions([waiting_at(kula,field1),waiting_at(ball,cell(2,2))])),
%        assertz_fact(perceptions([waiting_at(kula,cell(2,2)),waiting_at(ball,cell(2,2))])),
%       assertz_fact(perceptions([waiting_at(kiñe,cell(3,2)),waiting_at(epu,cell(3,1)),waiting_at(kula,cell(2,2)),waiting_at(meli,cell(2,1)),waiting_at(kechu,cell(2,3)),waiting_at(ball,cell(2,2))])),
	assertz_fact(perceptions([waiting_at(kula,cell(25,19)),waiting_at(ball,cell(15,12))])),
	assertz_fact(perceptions([waiting_at(kula,cell(25,18)),waiting_at(ball,cell(15,12))])),
	assertz_fact(perceptions([waiting_at(kula,cell(25,18)),waiting_at(ball,cell(15,12))])).

%	assertz_fact(perceptions([waiting_at(kiñe,cell(28,12)),waiting_at(epu,cell(26,6)),waiting_at(kula,cell(25,19)),waiting_at(meli,cell(19,6)),waiting_at(kechu,cell(19,19)),waiting_at(ball,cell(15,12))])),
%	assertz_fact(perceptions([waiting_at(kiñe,cell(28,12)),waiting_at(epu,cell(26,6)),waiting_at(kula,cell(25,19)),waiting_at(meli,cell(19,6)),waiting_at(kechu,cell(19,19)),waiting_at(ball,cell(15,12))])).
%        assertz_fact(perceptions([waiting_at(kiñe,cell(3,2)),waiting_at(epu,cell(3,1)),waiting_at(kula,cell(2,2)),waiting_at(meli,cell(2,1)),waiting_at(kechu,cell(2,3)),waiting_at(ball,cell(2,2))])),
%        assertz_fact(perceptions([waiting_at(kiñe,cell(3,2)),waiting_at(epu,cell(3,1)),waiting_at(kula,cell(2,2)),waiting_at(meli,cell(2,1)),waiting_at(kechu,cell(2,3)),waiting_at(ball,cell(1,2))])),
%        assertz_fact(perceptions([waiting_at(kiñe,cell(3,2)),waiting_at(epu,cell(3,1)),waiting_at(kula,cell(2,2)),waiting_at(meli,cell(2,1)),waiting_at(kechu,cell(2,3)),waiting_at(ball,cell(1,2))])).





continuouspop(Plan,DB):-
% 	current_output(S),
%         write(S,'######## new cicle ###### \n'),
	display('++++ esperando percepciones : '),nl,
	retract_fact(perception(Percepts)),
	set_fact(current_perceptions(Percepts)),
	display('++++ obtiene percepciones : OK'),nl,
% 	writeln(['>>> Current perceptions: ', Percepts,' <<<']),
	update_effects(Percepts),!,
	remove_flaw(Plan,DB,NewPlan,Act),
%	solve_open_preconditions(Plan2,Plan3,DB),
%	unexecuted_action(Plan3,NewPlan,Act),
%	exect_if_pos(Plan2,Plan3),
	display('P |> asserta accion : '),display(Act),
	asserta_fact(action(Act)),
	display('OK'),nl,
	!,
	continuouspop(NewPlan,DB).

% Si el plan falla, asserto accion noop,
% actualizo las percepciones y trato de buscar
% un nuevo plan con nuevas metas.
continuouspop(_Plan,DB):-
	asserta_fact(action(noop)),
	retract_fact(perception(Percepts)), 
	update_effects(Percepts),
	initialplan(NewPlan,DB),
	!,
	continuouspop(NewPlan,DB).



iniciar :-
	%Percepts = [clear(table),handempty,on(a,table),on(e,table),on(b,e),on(d,b),on(c,d),on(f,table),on(g,table),clear(g),clear(f),clear(c),clear(a)],
        Percepts = [waiting_at(kula,cell(3,3)),waiting_at(ball,cell(2,2))],
	asserta_fact(perception(Percepts)).


trypop(Plan,S) :-
	iniciar,
	perception(P),
	update_effects(P),
	%solve([on(c,d),on(d,b)],Plan,2), 
        solve([waiting_at(ball,oppGoal)],Plan,7), 
	seq(Plan,S).

trycpop :-
	initperlist,
	get_perceptions(P1),assertz_fact(perception(P1)),
	get_perceptions(P2),assertz_fact(perception(P2)),
	get_perceptions(P3),assertz_fact(perception(P3)),
% 	get_perceptions(P4),assertz_fact(perception(P4)),
% 	get_perceptions(P5),assertz_fact(perception(P5)),
% 	get_perceptions(P6),assertz_fact(perception(P6)),
	update_effects(P1),
	retract_fact(perception(P1)),
	initialplan(Plan,DB),
	continuouspop(Plan,DB).


% exect_if_pos(_Plan2,_Plan3):-
%         %...
% 	asserta_fact(action(_Act)).
%         %...


agenda(plan(_,_,_,Agenda),Agenda).


%%

remove_flaw(plan(As,Os,Ls,Ag,DIC),DB,NewPlan,Act) :-
	remove_unsupported_links(Ls,L1s,Ag,Ag1),
	remove_redundant_actions(plan(As,Os,L1s,Ag1,DIC),plan(A1s,O1s,L2s,Ag2,DIC)),
	solve_open_preconditions(plan(A1s,O1s,L2s,Ag2,DIC),plan(A2s,O2s,L3s,Ag3,DIC),DB),
	unexecuted_action(plan(A2s,O2s,L3s,Ag3,DIC),Plan3,Act),
	remove_historical_goals(Plan3,NewPlan).

%si falla el plan, busco un plan inicial nuevamente.
% remove_flaw(_,_,NewPlan,noop) :-
% %	find_depth_bound(DB),
% %alternativa : assetar la accion noop, obtener nuevas percepciones, update effects, initial plan
% 	initialplan(NewPlan).
	

add_new_goals(Goals,Agenda1,Agenda) :-
   %agrega las metas como precondiciones de finish
   add_these_preconds(Goals,finish,Agenda1,Agenda).


update_effects(Percepts) :-
	retractall_fact(holds(_,init)),
	update_effects1(Percepts).

update_effects1([]).

%
update_effects1([Percept|RestPercepts]):-
	asserta_fact(holds(Percept,init)),
%	holds(Percept,init),
	update_effects1(RestPercepts).



remove_unsupported_links([],[],Ag,Ag).

remove_unsupported_links([cl(start,Precond,Action)|Cls],NewCL,Ag,Ag1) :-
	\+ holds(Precond,init),!,
%        writeln(['Remove unsupported links: cl(start,',Precond,',',Action,')']),
	remove_unsupported_links(Cls,NewCL,[goal(Precond,Action)|Ag],Ag1).
	%remover este link de la lista.

%    incorporate_causal_link1(CL, As,Os,NOs)
% ,   writeln([' Added causal link: ',CL,nl,'  As=',As,nl,'  Os=',NOs])
%.

remove_unsupported_links([Cl|Cls],[Cl|RestCL],Ag,Ag1) :-
%    writeln(['Remove unsupported links: ',nl]),
	%no remueve el link de la lista.
%	remove_unsupported_links([],NewCL).
	remove_unsupported_links(Cls,RestCL,Ag,Ag1).




% remove_redundant_actions
% 1) busco postcondiciones en start,ie holds(Precond,init)
% 2) busco si hay links causales del tipo cl(Act1,Precond,Act2), donde Act1 \== start
% 3) elimino link causal
% 4) elimino la accion Act1 redundante
% 5) Extiendo el link causal, agregando cl(start,Precond,Act2).

remove_redundant_actions(plan(As,Os,L1s,Ag1,DIC),plan(A1s,O1s,L3s,Ag2,DIC)):-
	extend_causal_links(L1s,L2s),
	remove_red_act(As,L2s,A1s,[],Removed),
%	writeln(['Actions  ', Removed,' where removed from the plan']),
	remove_open_precond(Ag1,Removed,Ag2),
        remove_cl_missing_act(L2s,Removed,L3s),
	remove_constr(Os,Removed,O1s).

remove_constr([],_,[]).
remove_constr([A<B|Os],Rem,O1s):-
	(
	    member(A,Rem)
	;
	    member(B,Rem)
	),!,
	remove_constr(Os,Rem,O1s).
remove_constr([A<B|Os],Rem,[A<B|O1s]):-
	remove_constr(Os,Rem,O1s).


remove_open_precond([],_,[]).
remove_open_precond([goal(_,NA)|Ag],Rem,Ag2):-
	member(NA,Rem),!,
	remove_open_precond(Ag,Rem,Ag2).
remove_open_precond([goal(P,NA)|Ag],Rem,[goal(P,NA)|Ag2]):-
	remove_open_precond(Ag,Rem,Ag2).


remove_cl_missing_act([],_,[]).
remove_cl_missing_act([cl(_,_,Act)|L2s],Rem,L3s):-
	member(Act,Rem),!,
	remove_cl_missing_act(L2s,Rem,L3s).
remove_cl_missing_act([cl(Act1,P,Act)|L2s],Rem,[cl(Act1,P,Act)|L3s]):-
	remove_cl_missing_act(L2s,Rem,L3s).
	
	
remove_red_act([],_,[],R,R).
remove_red_act([act(start,init)|As],Ls,[act(start,init)|A1s],R,Removed):-
	remove_red_act(As,Ls,A1s,R,Removed).
remove_red_act([act(Act,A)|As],Ls,[act(Act,A)|A1s],R,Removed):-
	(
	    member(cl(Act,_,_),Ls)
	;
	    Act == finish
	),!,
	remove_red_act(As,Ls,A1s,R,Removed).
remove_red_act([act(Act,_A)|As],Ls,A1s,R,Removed):-
	remove_red_act(As,Ls,A1s,[Act|R],Removed).



extend_causal_links([],[]).
extend_causal_links([cl(Act1,Precond,Act2)|Ls],[cl(start,Precond,Act2)|NLs]):-
	Act1 \== start,
	holds(Precond,init),!,
%        writeln(['Extend causal link: cl(',Act1,',',Precond,',',Act2,')',' to ','cl(start,',Precond,',',Act2,')',nl]),
	extend_causal_links(Ls,NLs).
extend_causal_links([Cl|Ls],[Cl|NLs]):-
	extend_causal_links(Ls,NLs).
	



%verifica si todos los links causales van de start a finish.
all_goals_satisfied([cl(start,_Precond,finish)]).
all_goals_satisfied([cl(start,_Precond,finish)|Ls]) :-
	all_goals_satisfied(Ls).

% remove_historical_goals
% Si no hay precondiciones abiertas (Agenda vacia), 
% y todos los links causales van de Start a Finish
% remueve las metas  
remove_historical_goals(plan(As,Os,Ls,[],Dic),plan(As,Os,[],[],Dic)):-
	%deberíamos buscar nuevas metas... find_new_goals
	all_goals_satisfied(Ls),
	writeln(['Current goal set achieved!!! You can look for new goals']).

remove_historical_goals(Plan,Plan).

% remove_historical_goals(
% 	plan(As,Os,[cl(start,_Precond,finish)|Cls],Ag,DIC),
% 	plan(As,Os,NewCL,Ag,DIC)) :-
% 	%find open preconditions, like pop
% 	remove_historical_goals(plan(As,Os,Cls,Ag,DIC),plan(As,Os,NewCL,Ag,DIC)).

% una accion act(N0,Act0), Act0 \== finish,
% tiene todas sus precondiciones satisfechas, ie 
% preconditions(Act0,Preconds),all_preconds_satisfied(Preconds) 
% no hay otras acciones ordenadas antes de N0: \+ (member(A<N0,Os),A \== start),
% 

unexecuted_action(plan(As,Os,Ls,Agenda,DIC),plan(NAs,NOs,NLs,NAgenda,DIC), Act0):-
	member(act(N0,Act0),As),
	N0 \== finish,
	preconditions(Act0,Preconds),
	all_preconds_satisfied(Preconds),
	\+ (member(A<N0,Os),A \== start),
%	writeln(['**** Action ', Act0, ' is ready for execution ****']),
	delete_action(As,Ls,Os,Agenda,act(N0,Act0),NAs,NLs,NOs,NAgenda).

unexecuted_action(P,P, noop).



delete_action(As,Ls,Os,Agenda,act(N0,Act0),NAs,NLs,NOs,NAgenda) :-
	delete_non_ground(As,act(N0,Act0),NAs),
	remove_unexec_links(Ls,Agenda,N0,NLs,NAgenda),
	delete_non_ground(Os,N0<_,O1s),
	delete_non_ground(O1s,start<N0,NOs).

remove_unexec_links([],Ag,_,[],Ag).
remove_unexec_links([cl(start,_,N0)|Ls],Ag,N0,NLs,NAg):-
	remove_unexec_links(Ls,Ag,N0,NLs,NAg).
remove_unexec_links([cl(N0,P,NAct)|Ls],Ag,N0,NLs,[goal(P,NAct)|NAg]):-
	remove_unexec_links(Ls,Ag,N0,NLs,NAg).
remove_unexec_links([Cl|Ls],Ag,N0,[Cl|NLs],NAg):-
	remove_unexec_links(Ls,Ag,N0,NLs,NAg).


%[sitting_at(kula,o109),sitting_at(parcel,lng)]
% [sitting_at(k1,mail)]
all_preconds_satisfied([]).
all_preconds_satisfied([P|Ps]) :-
   (P <- B),
   all_preconds_satisfied(B),
   all_preconds_satisfied(Ps).

all_preconds_satisfied([P|Ps]) :-
   (P << B),
   all_preconds_satisfied(B),
   all_preconds_satisfied(Ps).
	
all_preconds_satisfied([P|Ps]) :-
	achieves(init,P),
	all_preconds_satisfied(Ps).


% writeln(L) is true if L is a list of items to be written on a line, followed by a newline.
writeln(L) :- \+ \+ (numbervars(L,0,_), writelnw(L) ).
writelnw([]) :- nl.
writelnw([nl|T]) :- !,nl, writeln(T).
writelnw([H|T]) :- display(H), writeln(T).
