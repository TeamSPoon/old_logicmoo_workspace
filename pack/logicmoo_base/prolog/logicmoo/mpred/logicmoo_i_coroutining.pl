/** <module> 
% ===================================================================
% File 'logicmoo_i_coroutining.pl'
% Purpose: For Emulation of OpenCyc for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface' 1.0.0
% Revision:  $Revision: 1.9 $
% Revised At:   $Date: 2002/06/27 14:13:20 $
% ===================================================================
% File used as storage place for all predicates which change as
% the world is run.
%
%
% Dec 13, 2035
% Douglas Miles
*/


% :- use_module(library(lists)).


:- include(logicmoo_i_header).


attribs_to_atoms(ListA,List):-map_subterms(attribs_to_atoms0,ListA,List).

map_subterms(Pred,I,O):-call(Pred,I,O).
map_subterms(Pred,I,O):-is_list(I),!,maplist(map_subterms(Pred),I,O).
map_subterms(Pred,I,O):-compound(I),!,I=..IL,maplist(map_subterms(Pred),IL,OL),O=..OL.
map_subterms(_Pred,IO,IO).

iza_to_isa(Iza,ftTerm):-var(Iza),!.
iza_to_isa((A,B),isAnd(ListO)):-!,conjuncts_to_list((A,B),List),list_to_set(List,Set),min_isa_l(Set,ListO).
iza_to_isa((A;B),isOr(Set)):-!,conjuncts_to_list((A,B),List),list_to_set(List,Set).
iza_to_isa(AA,AB):-must(AA=AB).


argisa:attribute_goals(_) --> [true].
argisa:attr_unify_hook(_, _).

attribs_to_atoms0(Var,Isa):-get_attr(Var,argisa,Iza),!,must(iza_to_isa(Iza,Isa)).
attribs_to_atoms0(O,O):-not(compound(O)).


min_isa_l(List,ListO):-isa_pred_l(lambda(Y,X,genls(X,Y)),List,ListO).
max_isa_l(List,ListO):-isa_pred_l(genls,List,ListO).


isa_pred_l(Pred,List,ListO):-isa_pred_l(Pred,List,List,ListO).

isa_pred_l(_Pred,[],_List,[]).
isa_pred_l(Pred,[X|L],List,O):-member(Y,List),X\=Y,call(Pred,X,Y),!,isa_pred_l(Pred,L,List,O).
isa_pred_l(Pred,[X|L],List,[X|O]):-isa_pred_l(Pred,L,List,O).

min_isa(HintA,HintA,HintA):- !.
min_isa(HintA,HintB,HintA):- genls(HintA,HintB),!.
min_isa(HintB,HintA,HintA):- genls(HintA,HintB),!.
min_isa((A,B),HintC,HintO):- min_isa(A,HintC,HintA),min_isa(B,HintC,HintB),conjoin(HintA,HintB,HintO).
min_isa(HintA,HintB,HintO):- conjoin(HintA,HintB,HintO).

max_isa(HintA,HintA,HintA):- !.
max_isa(HintA,HintB,HintB):- genls(HintA,HintB),!.
max_isa(HintB,HintA,HintB):- genls(HintA,HintB),!.
max_isa((A,B),HintC,HintO):- max_isa(A,HintC,HintA),max_isa(B,HintC,HintB),conjoin(HintA,HintB,HintO).
max_isa(HintA,HintB,HintO):- conjoin(HintA,HintB,HintO).


add_iza(Var,HintA):- var(Var),(get_attr(Var,argisa,HintB)->min_isa(HintA,HintB,Hint);Hint=HintA), put_attr(Var,argisa,Hint).
add_iza(Var,Hint):- ignore(show_call_failure(isa(Var,Hint))).

:- style_check(-singleton).


attempt_attribute_args(AndOr,Hint,Var):- var(Var),add_iza(Var,Hint),!.
attempt_attribute_args(AndOr,Hint,Grnd):-ground(Grnd),!.
attempt_attribute_args(AndOr,Hint,Term):-not(compound(Term)),!.
attempt_attribute_args(AndOr,Hint,+(A)):-!,attempt_attribute_args(AndOr,Hint,A).
attempt_attribute_args(AndOr,Hint,-(A)):-!,attempt_attribute_args(AndOr,Hint,A).
attempt_attribute_args(AndOr,Hint,?(A)):-!,attempt_attribute_args(AndOr,Hint,A).
attempt_attribute_args(AndOr,Hint,(A,B)):-!,attempt_attribute_args(AndOr,Hint,A),attempt_attribute_args(AndOr,Hint,B).
attempt_attribute_args(AndOr,Hint,[A|B]):-!,attempt_attribute_args(AndOr,Hint,A),attempt_attribute_args(AndOr,Hint,B).
attempt_attribute_args(AndOr,Hint,(A;B)):-!,attempt_attribute_args(';'(AndOr),Hint,A),attempt_attribute_args(';'(AndOr),Hint,B).
attempt_attribute_args(AndOr,Hint,Term):- use_was_isa(Term,I,C), add_iza(I,C).
attempt_attribute_args(AndOr,Hint,Term):- Term=..[F,A],tCol(F),!,attempt_attribute_args(AndOr,F,A).
attempt_attribute_args(AndOr,Hint,Term):- Term=..[F|ARGS],!,attempt_attribute_args(AndOr,Hint,F,1,ARGS).

attempt_attribute_args(AndOr,_Hint,_F,_N,[]):-!.
attempt_attribute_args(AndOr,Hint,t,1,[A]):-attempt_attribute_args(AndOr,callable,A).
attempt_attribute_args(AndOr,Hint,t,N,[A|ARGS]):-atom(A),!,attempt_attribute_args(AndOr,Hint,A,N,ARGS).
attempt_attribute_args(AndOr,Hint,t,N,[A|ARGS]):-not(atom(A)),!.
attempt_attribute_args(AndOr,Hint,F,N,[A|ARGS]):-attempt_attribute_one_arg(Hint,F,N,A),N2 is N+1,attempt_attribute_args(AndOr,Hint,F,N2,ARGS).

attempt_attribute_one_arg(Hint,F,N,A):-argIsa(F,N,Type),Type\=ftTerm,not(compound(Type)),!,attempt_attribute_args(AndOr,Type,A).
attempt_attribute_one_arg(Hint,F,N,A):-argQuotedIsa(F,N,Type),Type\=ftTerm,not(compound(Type)),!,attempt_attribute_args(AndOr,Type,A).
attempt_attribute_one_arg(Hint,F,N,A):-argIsa(F,N,Type),Type\=ftTerm,!,attempt_attribute_args(AndOr,Type,A).
attempt_attribute_one_arg(Hint,F,N,A):-attempt_attribute_args(AndOr,argi(F,N),A).



% mdif(A,B):- tlbugger:attributedVars,!,dif(A,B).
mdif(A,B):-A\==B.

:-export((samef/2,same/2)).
same(X,Y):- samef(X,Y),!.
same(X,Y):- compound(X),arg(1,X,Y),!.
same(X,Y):- compound(Y),arg(1,Y,X),!.


samef(X,Y):- hotrace(((to_functor(X,XF),to_functor(Y,YF),(XF=YF->true;string_equal_ci(XF,YF))))).

to_functor(A,O):-is_ftVar(A),!,A=O.
to_functor(A,O):-compound(A),get_functor(A,F),!,to_functor(F,O).
to_functor(A,A).

:-export(arg_to_var/3).
arg_to_var(_Type,_String,_Var).

:-export(same_arg/3).

same_arg(_How,X,Y):-var(X),var(Y),!,X=Y.
same_arg(equals,X,Y):-!,equals_call(X,Y).
same_arg(tCol(_Type),X,Y):-!, unify_with_occurs_check(X,Y).

same_arg(ftText,X,Y):-(var(X);var(Y)),!,X=Y.
same_arg(ftText,X,Y):-!, string_equal_ci(X,Y).

same_arg(same_or(equals),X,Y):- same_arg(equals,X,Y).
same_arg(same_or(genls),X,Y):- same_arg(equals,X,Y).
same_arg(same_or(genls),Sub,Sup):- holds_t(genls,Sub,Sup),!.
same_arg(same_or(isa),X,Y):- same_arg(equals,X,Y).
same_arg(same_or(isa),I,Sup):- !, holds_t(Sup,I),!.

same_arg(same_or(_Pred),X,Y):- same_arg(equals,X,Y).
same_arg(same_or(Pred),I,Sup):- holds_t(Pred,I,Sup),!.

% same_arg(I,X):- promp_yn('~nSame Objects: ~q==~q ?',[I,X]).
promp_yn(Fmt,A):- format(Fmt,A),get_single_char(C),C=121.

:- set_prolog_flag(generate_debug_info, true).
:- debug.

:- meta_predicate
	when_met(+, 0),
	suspend_list(+, 0),
	varcall:trigger(+, 0),
	trigger_disj(+, 0),
	trigger_conj(+, +, 0).


/** <module> Conditional coroutining

This library implements the when_met/2 constraint, delaying a goal until its
arguments are sufficiently instantiated.  For   example,  the  following
delayes the execution of =:=/2 until the expression is instantiated.

    ==
	...
	when_met(ground(Expr), 0 =:= Expr),
    ==

@author Tom Schrijvers (initial implementation)
@author Jan Wielemaker
*/

%%	when_met(+Condition, :Goal)
%
%	Execute Goal when_met Condition is satisfied. I.e., Goal is executed
%	as by call/1  if  Condition  is   true  when_met  when_met/2  is called.
%	Otherwise  Goal  is  _delayed_  until  Condition  becomes  true.
%	Condition is one of the following:
%
%	    * nonvar(X)
%	    * ground(X)
%	    * ?=(X,Y)
%	    * (Cond1,Cond2)
%	    * (Cond2;Cond2)
%
%	For example (note the order =a= and =b= are written):
%
%	    ==
%	    ?- when_met(nonvar(X), writeln(a)), writeln(b), X = x.
%	    b
%	    a
%	    X = x
%	    ==

when_met(Condition, Goal) :- 
	'$eval_when_condition'(Condition, Optimised),
	trigger_first(Optimised, Goal).


%%	'$eval_when_condition'(+Condition, -Optimised)
%
%	C-building block defined in pl-attvar.c.   It  pre-processes the
%	when_met-condition, checks it  for   errors  (instantiation  errors,
%	domain-errors and cyclic terms) and   simplifies it. Notably, it
%	removes already satisfied conditions   from  Condition, unifying
%	Optimised to =true= if  there  is   no  need  to suspend. Nested
%	disjunctions are reported as or(List).


trigger_first(true, Goal) :- !,
	call(Goal).
trigger_first(nonvar(X), Goal) :- !,
	'$suspend'(X, when_met, trigger_nonvar(X, Goal)).
trigger_first(Cond, Goal) :- 
	varcall:trigger(Cond, Goal).

varcall:trigger(nonvar(X),Goal) :- 
	trigger_nonvar(X,Goal).
varcall:trigger(ground(X),Goal) :- 
	trigger_ground(X,Goal).
varcall:trigger(?=(X,Y),Goal) :- 
	trigger_determined(X,Y,Goal).
varcall:trigger(tPred(X,Pred),Goal) :- 
	trigger_pred(X,Pred,Goal).
varcall:trigger((G1,G2),Goal) :- 
	trigger_conj(G1,G2,Goal).
varcall:trigger(or(GL),Goal) :- 
	trigger_disj(GL, check_disj(_DisjID,GL,Goal)).

varcall:trigger_nonvar(X, Goal) :- 
	(   nonvar(X)
	->  call(Goal)
	;   '$suspend'(X, when_met, trigger_nonvar(X, Goal))
	).

varcall:trigger_pred(X,Pred, Goal) :- 
	(   call(Pred, X)
	->  call(Goal)
	;   '$suspend'(X, when_met, trigger_pred(X,Pred, Goal))
	).

varcall:trigger_ground(X, Goal) :- 
	term_variables(X, Vs),
	(   Vs = [H]
	->  '$suspend'(H, when_met, trigger_ground(H, Goal))
	;   Vs = [H|_]
	->  T =.. [f|Vs],
	    '$suspend'(H, when_met, trigger_ground(T, Goal))
	;   call(Goal)
	).

varcall:trigger_determined(X, Y, Goal) :- 
	unifiable(X, Y, Unifier), !,
	(   Unifier == []
	->  call(Goal)
	;   put_attr(Det, when_met, det(trigger_determined(X,Y,Goal))),
	    suspend_list(Unifier, wake_det(Det))
	).
trigger_determined(_, _, Goal) :- 
	call(Goal).


wake_det(Det) :- 
	( var(Det) ->
		get_attr(Det,when_met,Attr),
		del_attr(Det,when_met),
		Det = (-),
		Attr = det(Goal),
		call(Goal)
	;
		true
	).

trigger_conj(G1,G2,Goal) :- 
	varcall:trigger(G1, varcall:trigger(G2,Goal)).

trigger_disj([],_).
trigger_disj([H|T], G) :- 
	varcall:trigger(H, G),
	trigger_disj(T, G).


%%	check_disj(DisjVar, Disj, Goal)
%
%	If there is a disjunctive condition, we share a variable between
%	the disjunctions. If the  goal  is  fired   due  to  one  of the
%	conditions, the shared variable is boud   to (-). Note that this
%	implies that the attributed  variable  is   left  in  place. The
%	predicate  when_goal//1  skips  such   goals    on   behalfe  of
%	copy_term/3.

check_disj(Disj,_,Goal) :- 
	(   Disj == (-)
	->  true
	;   Disj = (-),
	    call(Goal)
	).

suspend_list([],_Goal).
suspend_list([V=W|Unifier],Goal) :- 
	'$suspend'(V, when_met, Goal),
	(   var(W)
	->  '$suspend'(W, when_met, Goal)
	;   true
	),
	suspend_list(Unifier,Goal).

varcall:attr_unify_hook(call(Goal), Other) :- 
	(   get_attr(Other, when_met, call(GOTher))
	->  del_attr(Other, when_met),
	    Goal, GOTher
	;   Goal
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
varcall:attribute_goals(V) -->
	{ get_attr(V, when_met, Attr) },
	when_goals(Attr).

when_goals(det(trigger_determined(X, Y, G))) --> !,
	(   { disj_goal(G, Disj, DG) }
	->  disj_or(Disj, DG)
	;   { G = varcall:varcall:trigger(C, Goal) }
	->  [ when_met((?=(X,Y),C), Goal) ]
	;   [ when_met(?=(X,Y), G) ]
	).
when_goals(call(Conj)) -->
	when_conj_goals(Conj).

when_conj_goals((A,B)) --> !,
	when_conj_goals(A),
	when_conj_goals(B).
when_conj_goals(G) -->
	when_goal(G).

when_goal(trigger_nonvar(X, G)) -->
	(   { disj_goal(G, Disj, DG) }
	->  disj_or(Disj, DG)
	;   { G = varcall:trigger(C, Goal) }
	->  [ when_met((nonvar(X),C), Goal) ]
	;   [ when_met(nonvar(X),G) ]
	).
when_goal(trigger_ground(X, G)) -->
	(   { disj_goal(G, Disj, DG) }
	->  disj_or(Disj, DG)
	;   { G = varcall:trigger(C, Goal) }
	->  [ when_met((ground(X),C), Goal) ]
	;   [ when_met(ground(X),G) ]
	).
when_goal(wake_det(_)) -->
	[].

disj_goal(check_disj(X, _, _), [], -) :- X == (-).
disj_goal(check_disj(-, Or, DG), Or, DG).

disj_or([], _) --> [].
disj_or(List, DG) -->
	{ or_list(List, Or) },
	[when_met(Or, DG)].

or_list([H], H) :- !.
or_list([H|T], (H;OT)) :- 
	or_list(T, OT).






% :-swi_module(domain, [ domain/2  ]). % Var, ?Domain
:- use_module(library(ordsets)).
:-export(domain/2).
domain(X, Dom) :-
      var(Dom), !,
      get_attr(X, domain, Dom).
domain(X, List) :-
      list_to_ord_set(List, Domain),
      put_attr(Y, domain, Domain),
      X = Y.

:-export(extend_domain/2).
extend_domain(X, DomL):- init_dom(X, Dom2), ord_union(Dom2, DomL, NewDomain),put_attr( X, domain, NewDomain ).

:-export(extend_dom/2).
extend_dom(X, DomE):-  init_dom(X, Dom2),ord_add_element(Dom2, DomE, NewDomain),put_attr( X, domain, NewDomain ).

:-export(init_dom/2).
init_dom(X,Dom):-get_attr(X, domain, Dom),!.
init_dom(X,Dom):-Dom =[_], put_attr(X, domain, Dom),!.

% An attributed variable with attribute value Domain has been
% assigned the value Y
domain:attr_unify_hook(Domain, Y) :-
   ( get_attr(Y, domain, Dom2)
   -> ord_intersection(Domain, Dom2, NewDomain),
   ( NewDomain == []
   -> fail
   ; NewDomain = [Value]
   -> Y = Value
   ; put_attr(Y, domain, NewDomain)
   )
   ; var(Y)
   -> put_attr( Y, domain, Domain )
   ; (\+ \+ (cmp_memberchk(Y, Domain)))
).



% Translate attributes from this module to residual goals
domain:attribute_goals(X) -->
      { get_attr(X, domain, List) },
      [domain(X, List)].


cmp_memberchk(X,Y):-numbervars(X,0,_,[attvars(skip)]),member(X,Y),!.
cmp_memberchk0(Item, [X1,X2,X3,X4|Xs]) :- !,
	compare(R4, Item, X4),
	(   R4 = (>) -> cmp_memberchk0(Item, Xs)
	;   R4 = (<) ->
	    compare(R2, Item, X2),
	    (   R2 = (>) -> Item = X3
	    ;   R2 = (<) -> Item = X1
	    ;/* R2 = (=),   Item = X2 */ true
	    )
	;/* R4 = (=) */ true
	).
cmp_memberchk0(Item, [X1,X2|Xs]) :- !,
	compare(R2, Item, X2),
	(   R2 = (>) -> cmp_memberchk0(Item, Xs)
	;   R2 = (<) -> Item = X1
	;/* R2 = (=) */ true
	).
cmp_memberchk0(Item, [X1]) :-
	Item = X1.



:-export(isac/2).
isac(X, Dom) :-
      var(Dom), !,
      get_attr(X, isac, Dom).
isac(X, List) :-
      list_to_ord_set(List, Domain),
      put_attr(Y, isac, Domain),
      X = Y.

type_size(C,S):-isa(C,completeExtentKnown),!,setof(E,isa(E,C),L),length(L,S).
type_size(C,1000000):-isa(C,ttFormatType),!.
type_size(_,1000).

comp_type(Comp,Col1,Col2):-type_size(Col1,S1),type_size(Col2,S2),compare(Comp,S1,S2).

inst_isac(X, List):- predsort(comp_type,List,SList),isac_gen(X,SList).

% An attributed variable with attribute value Domain has been
% assigned the value Y
isac:attr_unify_hook(Domain, Y):-
   ( get_attr(Y, isac, Dom2)
   -> ord_union(Domain, Dom2, NewDomain),
   ( (fail,NewDomain == [])
   -> fail
   ; (fail,NewDomain = [Value])
   -> Y = Value
   ; put_attr(Y, isac, NewDomain)
   )
   ; var(Y)
   -> put_attr( Y, isac, Domain )
   ;  isac_chk(Y,Domain)).

isac_chk(E,Cs):-once(isac_gen(E,Cs)).

isac_gen(_, []).
isac_gen(Y, [H|List]):-isa(Y,H),!,isac_gen(Y, List).



% Translate attributes from this module to residual goals
isac:attribute_goals(X) -->
      { get_attr(X, isac, List) },
      [isac(X, List)].

