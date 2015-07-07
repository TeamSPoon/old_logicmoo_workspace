:- consult(genaut),consult(regress).
:- dynamic(indent_val/1).

%% Predicates for the generation of new domain/problem

indent_val(0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% topddl_dp(Path,DomainName,ProblemName)
%%
%% Generates a PDDL domain definition and a PDDL problem.
%% The translated domain includes a set of *derived predicates*
%% The files are written respectively in Path/DomainName
%% and Path/ProblemName

topddl_dp(Path,DomainName,ProblemName) :-
	topddl(Path,DomainName,ProblemName,dp).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% topddl_dp(Path,DomainName,ProblemName)
%%
%% Assuming that the domain has been loaded, generates
%% a PDDL domain definition and a PDDL problem.
%% The translated domain contain standard ADL operators
%% The files are written respectively in Path/DomainName
%% and Path/ProblemName

topddl_cr(Path,DomainName,ProblemName) :-
	topddl(Path,DomainName,ProblemName,cr).

topddl(Path,DomainName,ProblemName,Type) :-
	clean_domain,
	retractall(indent_val(_)),
	asserta(indent_val(0)),
	concat_atom([Path,'/',DomainName],DomainFile),
	concat_atom([Path,'/',ProblemName],ProblemFile),
	consult(DomainFile),
	consult(ProblemFile),
	goal(Goal),
	compute_new_domain(Goal,Type),
	write_pddl(Path,DomainName,ProblemName,Type).

write_pddl(Path,DomainName,ProblemName,Type) :-
	(is_verbose ->
	    listing(causes_true),
	    listing(causes_false),
	    listing(new_poss),
	    listing(new_initially_true),
	    listing(derived);
	    true
	),
	concat_atom([Path,'/',DomainName,'_',ProblemName,'_',Type,'.pddl'],PddlDomainFile),
	concat_atom([Path,'/',ProblemName,'_',Type,'.pddl'],PddlProblemFile),
	tell(PddlDomainFile),
	write_domain(DomainName),
	told,
	tell(PddlProblemFile),
	write_problem(DomainName,ProblemName),
	told.


write_problem(DomainName,ProblemName) :-
	iwritef("(define (problem %q)\n",[ProblemName]),nl,
	inc_indent,
	iwritef("(:domain %q)\n",[DomainName]),
	write_objects,
	write_initial_state,
	write_goal,
	writef(")"),
	dec_indent.

write_objects :-
	iwritef("(:objects "),
	findall(Obj,constant(Obj),Objects),
	write_list(Objects),writef(")\n").

write_initial_state :-
	iwritef("(:init \n"),
	inc_indent,
	forall(new_initially_true(F),(iwritef(""),write_term(F),nl)),
	dec_indent,
	iwritef(")\n").

write_goal :-
	iwritef("(:goal "),
	new_goal(Fla),
	write_formula(Fla),writef(")").
	

write_domain(DomainName) :-
	iwritef("(define (domain %q)\n",[DomainName]),
	inc_indent,
	write_domain_predicates,
	write_domain_effs,
	write_derived_predicates,
	writef(")"),
	dec_indent.

write_derived_predicates :-
	forall(derived(F,D),
	       (
		 writef("(:derived "),nl,
		 inc_indent,iwritef(""),
		 write_term(F),	%% assuming F has no variables
		 nl,
		 iwritef(""),
		 write_formula(D), %% assuming D has no variables
		 dec_indent,
		 nl,writef(")\n\n")
	       )).

write_domain_predicates :-
	iwritef("(:predicates\n"),
	inc_indent,
	findall(F, (
		     (fluent(F);new_fluent(F)),
		     lisp_vars(F),
		     iwritef(""),
		     write_term(F),
		     nl
		   ),
		_),
	dec_indent,
	iwritef(")"),nl.


write_domain_effs :-
	findall(Action,
		(
		  action(Action),
		  lisp_vars(Action,0,F1,ActionFunctor,ActionVars),
		  findall(Effect,
			  (
			    (causes_true(Fluent,Action,Condition)
			    ;
			    (causes_false(F,Action,Condition),Fluent=not(F))),
			    lisp_vars(Condition,F1,F2,_,ConditionVars),
			    lisp_vars(Fluent,F2,_,_,FluentVars),
			    (Condition = true ->
				Effect=forall(FluentVars,Fluent)
			    ;
				Effect=forall(ConditionVars,when(Condition,forall(FluentVars,Fluent)))
			    )
			  ),  
			  Effects),
		  new_poss(Action,PossCond),
		  lisp_vars(PossCond,F1,_,_,PossVars),
		  write_effects_action(ActionFunctor,ActionVars,Effects,exists(PossVars,PossCond))
		),
		_).

write_effects_action(ActionFunctor,ActionVars,Effects,PossCond) :-
	iwritef("(:action %q\n",[ActionFunctor]),
	inc_indent,
	iwritef(":parameters "),
	writef("("),write_list(ActionVars),writef(")\n"),
	iwritef(":precondition \n"),
	inc_indent,
	iwritef(""),
	write_formula(PossCond),nl,
	dec_indent,
	iwritef(":effect\n"),inc_indent,
	length(Effects,N),
	(N=1 ->
	    write_formula(Effects)
	;
	    iwritef("(and"),nl,
	    inc_indent,
	    forall(member(E,Effects),(iwritef(""),write_formula(E),nl)),
	    dec_indent,
	    iwritef(")\n")
	),
	dec_indent,
	iwritef(")\n"),
	dec_indent.

write_formula(forall([],Fla)) :- !,write_formula(Fla).
write_formula(forall(L,Fla)) :- 
	L\=[],!,
	writef("(forall ("),
	write_list(L),writef(")"),nl,
	inc_indent,
	iwritef(""),write_formula(Fla),
	writef(")"),nl,
	dec_indent.
	
write_formula(exists([],Fla)) :- !,write_formula(Fla).
write_formula(exists(L,Fla)) :- !,
	L\=[],
	writef("(exists ("),
	write_list(L),writef(")"),nl,
	inc_indent,
	iwritef(""),write_formula(Fla),
	writef(")"),nl,
	dec_indent.

write_formula(and(F1,F2)) :- !,
	writef("(and\n"),
	inc_indent,
	iwritef(""),write_formula(F1),nl,
	iwritef(""),write_formula(F2),
	writef(")"),
	dec_indent.

write_formula(or(F1,F2)) :- !,
 	writef("(or\n"),
	inc_indent,
	iwritef(""),write_formula(F1),nl,
	iwritef(""),write_formula(F2),
	writef(")"),
	dec_indent.

write_formula(when(F1,F2)) :- !,
	writef("(when\n"),
	inc_indent,
	iwritef(""),write_formula(F1),nl,
	iwritef(""),write_formula(F2),
	writef(")"),
	dec_indent.

write_formula(equal(X,Y)) :- !,
	writef("(= "),write_list([X,Y]),writef(")").

write_formula(not(F)) :- !,
	writef("(not \n"),
	inc_indent,
	iwritef(""),write_formula(F),
	writef(")"),
	dec_indent.

write_formula(F) :- %must be a term
	write_term(F).

write_term(F) :-
	F=..List,
	writef("("),
	write_list(List),
	writef(")").


write_list([]).
write_list([X|Xs]) :-
	write(X),
	(Xs \= [] ->
	    writef(" "),
	    write_list(Xs)
	;
	    true).


lisp_vars(Term) :-
	lisp_vars(Term,0,_,_,_).

lisp_vars(Term,Init,Fin,Functor,Vars) :-
	term_variables(Term,Vars),
	Term=..[Functor|_],
	lisp_inst_list(Vars,Init,Fin).

lisp_inst_list([],I,I).
lisp_inst_list([V|Vs],Init,Fin) :-
	concat_atom(['?x',Init],V),
	Ip is Init + 1,
	lisp_inst_list(Vs,Ip,Fin).
	

%% Predicates for pretty printing of formulae

inc_indent :-
	indent_val(X),
	retract(indent_val(_)),
	Xp is X+1,
	asserta(indent_val(Xp)).

dec_indent :-
	indent_val(X),
	retract(indent_val(_)),
	(X>0 -> Xp is X-1),
	asserta(indent_val(Xp)).

write_indent :-
	indent_val(X),
	write_indent(X).

write_indent(0).
write_indent(X) :-
	X>0,
	writef("  "),
	Xp is X-1,
	write_indent(Xp).


iwritef(String) :-
	write_indent,
	(String \= [] -> writef(String);true).

iwritef(String,List) :-
	write_indent,
	(String \= [] -> writef(String,List);true).



