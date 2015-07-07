:- consult(verbosity).
:- dynamic constant/1,action/1,initially_true/1,poss/2,fluent/1,new_fluent/1,new_initially_true/1,new_poss/2,causes_true/3,causes_false/3, derived/2.



%% Domain wipe-out

clean_domain :-
	retractall(new_fluent(_)),
	retractall(new_goal(_)),
	retractall(fluent(_)),
	retractall(new_fluent(_,_)),
	retractall(constant(_)),
	retractall(action(_)),
	retractall(initially_true(_)),
	retractall(new_initially_true(_)),
	retractall(poss(_,_)),
	retractall(new_poss(_,_)),
	retractall(causes_true(_,_,_)),
	retractall(causes_false(_,_,_)),
	retractall(derived(_,_)).


%% Regression Functions

causes_true_real(Fluent,Action,Condition) :-
	causes_true(Fluent,Action,Condition),!.
causes_true_real(_,_,false).

causes_false_real(Fluent,Action,Condition) :-
	causes_false(Fluent,Action,Condition),!.
causes_false_real(_,_,false).


regress(true,_,true,false) :- !.

regress(not(F),A,TrueCond,FalseCond) :- !,
	regress(F,A,TC,FC),
	simplify(FC,TrueCond),
	simplify(TC,FalseCond).

regress(and(F,G),A,TrueCond,FalseCond) :- !,
	regress(F,A,Ftrue,Ffalse),
	regress(G,A,Gtrue,Gfalse),
	TC = or(and(Ftrue,Gtrue),
		or(and(F,and(not(Ffalse),Gtrue)),
		   and(G,and(not(Gfalse),Ftrue)))),
	FC = or(Ffalse,Gfalse),
	simplify(FC,FalseCond),
	simplify(TC,TrueCond).

regress(or(F,G),A,TrueCond,FalseCond) :- !,
	regress(not(and(not(F),not(G))),A,TrueCond,FalseCond).

regress(F,A,TrueCond,FalseCond) :-
	copy_term(A,Act),
	copy_term(A,Actp),
	causes_true_real(F,Act,TC),
	causes_false_real(F,Actp,FC),
	get_bindings(A,Act,TBind),
	get_bindings(A,Actp,FBind),
	to_and(TBind,TAnt),
	to_and(FBind,FAnt),
	TrueCondNS = and(TAnt,TC),
	FalseCondNS = and(FAnt,FC),
	simplify(TrueCondNS,TrueCond),
	simplify(FalseCondNS,FalseCond).

get_bindings(X,Y,Bin) :-
	X=..[Func|VArgs],
	Y=..[Func|IArgs],
	get_bindings_aux(VArgs,IArgs,Bin).

get_bindings_aux([],[],[]).
get_bindings_aux([V|Vs],[I|Is],[equal(V,I)|Bs]) :-
	nonvar(I),!,
	get_bindings_aux(Vs,Is,Bs).

get_bindings_aux([X|Vs],[X|Is],Bs) :-
	get_bindings_aux(Vs,Is,Bs).



simp(not(false),true) :- !.
simp(not(true),false) :- !.
simp(not(not(X)),Xp) :- !, simp(X,Xp).
simp(and(false,_),false) :- !.
simp(and(_,false),false) :- !.
simp(and(equal(A,B),equal(A,C)),false) :- B\==C,!. 
simp(equal(A,B),true) :- A==B,!.
simp(equal(A,B),false) :- nonvar(A),nonvar(B),A\==B,!.
simp(and(X,X),X) :- !.
simp(and(true,X),Xp) :- !,simp(X,Xp).
simp(and(X,true),Xp) :- !,simp(X,Xp).
simp(and(X,Y),and(Xp,Yp)) :- !,simp(X,Xp),simp(Y,Yp).

simp(or(true,_),true):-!.
simp(or(_,true),true):-!.
simp(or(X,X),X) :- !.
simp(or(false,X),Xp) :- !,simp(X,Xp).
simp(or(X,false),Xp) :- !,simp(X,Xp).
simp(or(X,Y),or(Xp,Yp)) :- !,simp(X,Xp),simp(Y,Yp).

simp(X,X).

simplify(X,Xp) :-
	simp(X,Xs),
	(X = Xs ->
	    Xp=Xs;
	    simplify(Xs,Xp)
	).

super_simplify(false,false):-!.
super_simplify(true,true):-!.
super_simplify(F,FS) :-
	writef("I've been called with %q\n",[F]),
	dnfequiv(F,DF),
	fla2list(DF,DFl),
	simp_dnf(DFl,DFlS),
	list2fla(DFlS,FS).

super_simplify(F,_) :-
	writef("I am failing for %q!!\n",[F]),
	fail.

%%%%%%%%%
% get_subformulae(Formula,UnInstFormula,SubForumlaList,VariableList)
%
% unifies SubFormulaList with the list of the biggest
% temporal subformulas in Formula
% In turn, UnInstGoal stores Formula but with those subformulae
% replaced by (new)variables. Finally those (new) variables are stored
% in VariableList

get_subformulae(and(X,Y),and(Xp,Yp),SL,VL) :- !,
	get_subformulae(X,Xp,SL1,VL1),
	get_subformulae(Y,Yp,SL2,VL2),
	append(SL1,SL2,SL),
	append(VL1,VL2,VL).

get_subformulae(or(X,Y),or(Xp,Yp),SL,VL) :- !,
	get_subformulae(X,Xp,SL1,VL1),
	get_subformulae(Y,Yp,SL2,VL2),
	append(SL1,SL2,SL),
	append(VL1,VL2,VL).

get_subformulae(not(X),not(Xp),SL,VL) :- !,
	get_subformulae(X,Xp,SL,VL).


get_subformulae(X,Y,[X],[Y]). % X is a temporal formula



% genaut_list(ListFla,ListAut)
%
% Unifies ListAut with a list of automata, one for each formula in ListFla

genaut_list([],[],X,X).
genaut_list([F|Fs],[A|As],NS,NStates) :-
	genaut(F,A),
	length(A,N),
	NSp is NS+N,
	genaut_list(Fs,As,NSp,NStates).

%% compute_new_domain(Goal)
%
% given a temporal goal formula, computes a new domain

compute_new_domain(Goal,Type) :-
	to_nnf(Goal,NGoal),
	simplify_nnf(NGoal,NGoalS),
	breakup(NGoalS,NGoal1),
	%NGoal1 = NGoalS, % omitting breakup!
	get_subformulae(NGoal1,UnInstGoal,SubGoalList,VarList),
	genaut_list(SubGoalList,AutList,0,TotalStates),
	writef('Total No. of states = %q\n',[TotalStates]),
	findall(Fact,initially_true(Fact),InitState), % we add original initial facts
	add_new_initial(InitState),                   % add new initial facts
				% leave preconditions intact
	forall(poss(Action, Cond), myassertz(new_poss(Action,Cond))), 	
	compute_new_domain(AutList,Type,1),
	add_final_goal(AutList,UnInstGoal,VarList,1).

compute_new_domain([],_,_).
compute_new_domain([Aut|List],Type,AutNumber) :-
%	writef('Automaton %q',[AutNumber]),
	(is_verbose ->
	    showaut(Aut);
	    true
	),  
	writef('Generating new fluents...\n'),
	aut2newfluents(Aut,AutNumber,Type),
	writef('New Initial State...\n'),
	aut2initial(Aut,AutNumber,Type), % New ground facts initially true
	writef('New Effect Axioms...\n'),
	aut2eff(Aut,AutNumber,Type),	    % Generation of new effect axioms
%	writef('New Preconditions...\n'),
%	aut2prec(Aut,AutNumber),    % Generation of new preconditions
	AutNumberp is AutNumber+1,
	compute_new_domain(List,Type,AutNumberp).

aut2newfluents(Aut,AutNumber,Type) :-
	forall((member([Id,_,_,_],Aut),id2fluent(Id,AutNumber,Fluent)),myassertz(new_fluent(Fluent))),
	(Type=dp -> %aditional prev_autstate predicates are declared
	    forall((member([Id,_,_,_],Aut),id2prevfluent(Id,AutNumber,Fluent)),myassertz(new_fluent(Fluent)));
	    true
	).

add_final_goal([],Goal,[],_) :- myassertz(new_goal(Goal)).
add_final_goal([Aut|AList],UnInstGoal,[Var|VList],N) :-
	aut2goal(Aut,N,Var),
	N1 is N+1,
	add_final_goal(AList,UnInstGoal,VList,N1).

aut2goal(Aut,AutNumber,GoalFla) :-
	findall(Fluent,(member([Id,1,_,_],Aut),
			id2fluent(Id,AutNumber,Fluent)),GFs),
	to_or(GFs,GoalFla).



aut2eff(Aut,AutNumber,Type) :- 
	findall([Id,PredList],
		(
		  member([Id,_,_,_],Aut),
		  findall([State,Label],
			  (
			    member([State,_,_,AdjList],Aut),
			    member([Id,Label],AdjList)
			  ),
			  PredList)
		),
	       StatePred),
	dwritef("\n\nCalling new_effects (%q)\n\n",[StatePred]),
	new_effects(StatePred,AutNumber,Type).


aut2prec(Aut,AutNumber) :-
	forall(action(Action),
	       (
		 findall(Cond,(member([Id,_,_,_],Aut),
			       id2fluent(Id,AutNumber,Fluent),
			       causes_false(Fluent,Action,CondOut),
			       Cond=or(not(Fluent),CondOut)
			      ),Conditions),
		 to_and(Conditions,ExtraCond),
%		 writef("WAS HERE EXTRA COND=%q",[ExtraCond]),
		 (poss(Action, Cond) ->	% assuming action is always possible if no precondition is declared
		     true;
		     Cond=true
		 ),
		 simplify(and(Cond,ExtraCond),SimpCond),

		 myassertz(new_poss(Action,Cond))	       
				% if you want to add extra preconditions to actions
				% change this line by the following 
%		 myassertz(new_poss(Action,SimpCond))

	       )).


	
aut2initial(Aut,AutNumber,cr) :-
	findall(Fluent,(member([_,_,1,AdjList],Aut),member([Node,Label],AdjList),
			all_true(Label),id2fluent(Node,AutNumber,Fluent)),TrueInit),
	list_to_set(TrueInit,TrueInitSet),
	add_new_initial(TrueInitSet).


aut2initial(Aut,AutNumber,dp) :-   %% prevautstate fluents are added to the initial state
	findall(Fluent,(
			member([Node,_,1,_],Aut),
			id2prevfluent(Node,AutNumber,Fluent)
		       ),TrueInit),
	add_new_initial(TrueInit).


all_true([]).

all_true([not(F)|R]) :-
	\+ initially_true(F),
	all_true(R).

all_true([F|R]) :-
	initially_true(F),
	all_true(R).


add_new_initial([]).
add_new_initial([Fact|R]) :-
	myassertz(new_initially_true(Fact)),
	add_new_initial(R).
	
	




	       

myassertz(X) :-
	dwritef("to assert: %q\n",[X]),
	assertz(X).

clear_aut_eff :-
	string_to_atom(Prefix,autstate_),
	findall(_,
		(
		  causes_true(F,_,_),
		  F=..[Predname|_],
		  string_to_atom(Str,Predname),
		  string_concat(Prefix,_,Str),
		  retractall(causes_true(F,_,_))
		),_),
	findall(_,
		(
		  causes_false(F,_,_),
		  F=..[Predname|_],
		  string_to_atom(Str,Predname),
		  string_concat(Prefix,_,Str),
		  retract(causes_false(F,_,_))
		),_).
	

add_positive_eff(Fluent,Action,Cond) :-
	(Cond\=false ->
	    simplify(Cond,SCond),
%	    Cond=SCond,
	    myassertz(causes_true(Fluent,Action,SCond))
	;
	    true).

add_negative_eff(Fluent,Action,Cond) :-
	(Cond\=false ->
	    simplify(Cond,SCond),
%	    SCond=Cond,
	    myassertz(causes_false(Fluent,Action,SCond))
	;
	    true).


new_effects([],_,_).
new_effects([[Id,PredList]|Rest],AutNumber,cr) :-

	% for the causal rules version we add causal rules
	% for the autstate predicates
	
	id2fluent(Id,AutNumber,ThisFluent),
	                    
				% we add positive effects
	
	findall([Action,TrueCond],
		(
		  action(Action),
		  my_find(Action,TrueC,
			  (
			    member([PredId,Label],PredList),
			    PredId\=Id,
			    to_and(Label,C),
			    regress(C,Action,TC,TF),
			    id2fluent(PredId,AutNumber,IdFluent),
			    simplify(and(IdFluent,or(TC,and(C,not(TF)))),TrueC)
			  ),
			  TrueCondList),
		  to_or(TrueCondList,TrueCondBS),
		  simplify(TrueCondBS,TrueCond),
		  add_positive_eff(ThisFluent,Action,TrueCond)
		), _),
				% now negative effects

		(member([Id,_],PredList) ->

				% there is a self transition

	    findall(SL,
		    (
		      member([Id,SLab],PredList),
		      to_and(SLab,SL)
		    ),SelfLabels),
			       
	    to_or(SelfLabels,FCondNS),
	    simplify(FCondNS,FCond),
	    dwritef("I'm here for state %q\nFCond=%q",[Id,FCond]),
	    findall(Action,
		    (
		      action(Action),
		      regress(FCond,Action,TC,TF),
				% Conditions under which self-transition is falsified
		      dwritef("TC=%q\n",[TC]),
		      causes_true_real(ThisFluent,Action,BecomeTrue),
				% Conditions under which it may become true
				% warning: previous line assumes the positive effect has been asserted!
		      simplify(and(not(BecomeTrue),
				   and(not(TC),
				       or(not(FCond),TF))),ActualCond),
		    
		      add_negative_eff(ThisFluent,Action,ActualCond)
		    ), _)

	    ;
				% there is no self-transition
	    findall(Action,
		    (
		      action(Action),
		      causes_true_real(ThisFluent,Action,BecomeTrue),
				% Conditions under which it may become true
				% warning: previous line assumes the positive effect has been asserted!
		      simplify(not(BecomeTrue),ActualCond),

		      add_negative_eff(ThisFluent,Action,ActualCond)
		    ), _)
	),	
	new_effects(Rest,AutNumber,cr).


new_effects([[Id,PredList]|Rest],AutNumber,dp) :-
	findall(and(Fluent,Condition),
		(
		  member([PredId,PredLabel],PredList),
		  id2prevfluent(PredId,AutNumber,Fluent),
		  to_and(PredLabel,Condition)
		),DefList),
	to_or(DefList,Def),
	simplify(Def,DefS),
	id2prevfluent(Id,AutNumber,PrevFluent),
	id2fluent(Id,AutNumber,F),
	(DefS \== false ->
	    myassertz(derived(F,DefS))	%%derived definition
	;
	    true
	),
	forall(action(A),
	       (
		 myassertz(causes_true(PrevFluent,A,F)),  %% causal rule for prev_autstate
		 myassertz(causes_false(PrevFluent,A,not(F)))
	       )),
	new_effects(Rest,AutNumber,dp).


to_and([],true).
to_and([X],X) :- !.
to_and([X|L],and(X,R)) :-
	to_and(L,R).

to_or([],false).
to_or([X],X) :- !.
to_or([X|L],or(X,R)) :-
	to_or(L,R).

id2fluent(Id,AutNumber,Term) :-
	concat_atom([autstate_,AutNumber,'_',Id],Term).

id2prevfluent(Id,AutNumber,Term) :-
	concat_atom([prev_autstate_,AutNumber,'_',Id],Term).


:- dynamic my_find_list/1.

my_find(Functor,X,C,L) :-
	retractall(my_find_list(_)),
	assert(my_find_list([[],Functor])),
	\+ my_find_aux(Functor,X,C),
	!,
	my_find_list([L,Functor]),
	retractall(my_find_list(_)).

my_find_aux(Functor,X,C) :-
	call(C),
	my_find_list([L,Functor]),
	retractall(my_find_list(_)),
	asserta(my_find_list([[X|L],Functor])),
	fail.






%% simp_dnf(Fla,Fla2) true iff Fla2 is obtained by simplifying Fla

simp_dnf(Fla,SFla) :-
	simp_dnf1(Fla,Flap),
	simp_dnf2(Flap,Flap,SFla).

simp_dnf1([],[]).
simp_dnf1([Term|Terms],Fla2) :-
	false_term(Term),!,
	simp_dnf1(Terms,Fla2).

simp_dnf1([T|Ts],L) :-
	simp_nots(T,T1),
	etoset(T1,T2),
	eremove(true,T2,T3),
	simp_term_eq(T3,U),
	(U\=[] ->
	    L=[U|Us]
	;
	    L=Us
	),
	simp_dnf1(Ts,Us).

simp_dnf2([],_,[]).
simp_dnf2([T|Ts],Fla,Us) :-
	member(Term,Fla),
	Term \== T,
	esubset(Term,T),!,
	simp_dnf2(Ts,Fla,Us).

simp_dnf2([T|Ts],Fla,[T|Us]) :-
	simp_dnf2(Ts,Fla,Us).


doublenot(not(not(X)),Y):-!,
	doublenot(X,Y).
doublenot(X,X).

simp_nots([],[]).

simp_nots([X|Xs],[Y|Ys]) :-
	doublenot(X,Xp),
	(Xp=not(true) ->
	    Y = false
	;
	    (Xp = not(false) ->
		Y = true
	    ;
		Xp = Y
	    )
	),
	simp_nots(Xs,Ys).

%% false_term(Term) true if Term is trivially false

false_term(Term) :-
	member(false,Term).

false_term(Term) :-
	member(Lit1,Term),
	emember(not(Lit1),Term).

false_term(Term) :-
	member(equal(X,Y),Term),
	nonvar(X),nonvar(Y),X\==Y.

false_term(Term) :-
	member(equal(X,Y),Term),
	member(equal(W,Z),Term),
	var(X),var(W),X==W,
	Y\==Z.

false_term(Term) :-
	member(equal(X,Y),Term),
	nonvar(X),nonvar(Y),X\==Y.

%% sim_term_eq(T1,T2) true if T2 is a simplified version of T1
%% the simplification is based on equality identities

simp_term_eq([],[]).

simp_term_eq([equal(X,Y)|L],Lp) :-
	nonvar(X),nonvar(Y),X==Y,!,
	simp_term_eq(L,Lp).

simp_term_eq([equal(X,Y)|L],[equal(X,Y)|Lp]) :-
	var(X),nonvar(Y),
	member(not(equal(W,Z)),L),
	var(W),W==X,
	nonvar(Z), Y\==Z,
	eremove(not(equal(W,Z)),L,Ls),!,
	simp_term_eq(Ls,Lp).

simp_term_eq([not(equal(X,Y))|L],Lp) :-
	var(X),nonvar(Y),
	member(equal(W,Z),L),
	var(W),W==X,
	nonvar(Z), Y\==Z,!,
	simp_term_eq(L,Lp).

simp_term_eq([X|L],[X|Lp]) :-
	simp_term_eq(L,Lp).

eremove(_,[],[]).
eremove(X,[Y|Z],Zp) :-
	X==Y,!,eremove(X,Z,Zp).

eremove(X,[Y|Z],[Y|Zp]):- eremove(X,Z,Zp).


%% etoset(T1,T2) T2 is T1 with duplicates removed

etoset([],[]).

etoset([X|Xs],Ys) :-
	emember(X,Xs),!,
	etoset(Xs,Ys).

etoset([X|Xs],[X|Ys]) :-
	etoset(Xs,Ys).

%% esubset(S1,S2) true iff S1 is a subset of S2

esubset([],_).
esubset([X|Xs],S2) :-
	emember(X,S2),
	esubset(Xs,S2).


%% "Exact"/literal member of a list
emember(X,[Y|L]) :- X==Y; emember(X,L).
	

%%% fla2list transform a formula in dnf to list representation

fla2list(or(X,Y),List) :- !,
	fla2list(X,L1),
	fla2list(Y,L2),
	append(L1,L2,List).

fla2list(X,[L]) :- fla2list2(X,L).

fla2list2(and(X,Y),List) :- !,
	fla2list2(X,L1),
	fla2list2(Y,L2),
	append(L1,L2,List).
fla2list2(X,[X]).
	


list2fla(L,F) :-
	list2fla_aux(L,Lp),
	to_or(Lp,F).

list2fla_aux([],[]).
list2fla_aux([T|Ts],[U|Us]) :-
	to_and(T,U),
	list2fla_aux(Ts,Us).

	
% to disjunctive normal form

dnfequiv(X,Y) :-
	transform(X,Z),!,
	(X \= Z ->
	    dnfequiv(Z,Y)
	;
	    Y=Z
	).

dnfequiv(X,X).


% transform is satisfied if its second argument is derived from its
 % first by applying one of the standard transformations. Given a query
 % in which the first argument is a boolean formula and the second is a
 % variable, it will instantiate the variable to a logically equivalent
 % formula to the first argument, which is nearer to being in conjunctive
 % normal form. If the first argument is already in conjunctive normal form
 % then the goal cannot be satisfied.

transform(not(not(X)),X).  % eliminate double negation

transform(not(and(X,Y)),or(not(X),not(Y))).  % De Morgan
transform(not(or(X,Y)),and(not(X),not(Y))).  %

transform(and(X,or(Y,Z)),or(and(X,Y),and(X,Z))).   % distribution
transform(and(or(X,Y),Z),or(and(X,Z),and(Y,Z))).   %

transform(or(X1,Y),or(X2,Y)) :- transform(X1,X2).  %
transform(or(X,Y1),or(X,Y2)) :- transform(Y1,Y2).  %
                                                 % transform subterms
transform(and(X1,Y),and(X2,Y)) :- transform(X1,X2).  %
transform(and(X,Y1),and(X,Y2)) :- transform(Y1,Y2).  %
                                                 %
transform(not(X1),not(X2)) :- transform(X1,X2).      %
