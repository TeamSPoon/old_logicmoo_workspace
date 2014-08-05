/** <module> 
% ===================================================================
% File 'dbase_i_coroutining.pl'
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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This module implements the dif_arg/2 constraint. It constraints two terms
% to be not identical.
%
%	Author: 	Tom Schrijvers, K.U.Leuven
% 	E-mail: 	Tom.Schrijvers@cs.kuleuven.ac.be
%	Copyright:	2003-2004, K.U.Leuven
%
% Update 7/3/2004:
%   Now uses unifiable/3. It enables dif_arg/2 to work with infinite terms.
% Update 11/3/2004:
%   Cleaned up code. Now uses just one or node for every call to dif_arg/2.
% Update Jul 8, 2005 (JW)
%   Fixed spelling unifyable --> unifiable
% Update Sep 4, 2007 (JW)
%   Added support for current_prolog_flag(occurs_check, error) case
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(lists)).
:- set_prolog_flag(generate_debug_info, false).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dif_arg(X,Y) :-
	dif_c_c(X,Y,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% types of attributes?
% 	vardif: X is a variable
%%	node(Parent,Children,Variables,Counter)

dif_c_c(X,Y,OrNode) :-
	(	( current_prolog_flag(occurs_check, error) ->
			catch(unifiable(X,Y,Unifier), error(occurs_check(_,_),_), fail)
		  ;
			unifiable(X,Y,Unifier)
		) ->
		( Unifier == [] ->
			or_one_fail(OrNode)
		;
			dif_c_c_l(Unifier,OrNode)
		)
	;
		or_succeed(OrNode)
	).


dif_c_c_l(Unifier,OrNode) :-
	length(Unifier,N),
	extend_ornode(OrNode,N,List,Tail),
	dif_c_c_l_aux(Unifier,OrNode,List,Tail).

extend_ornode(OrNode,N,List,Vars) :-
	( get_attr(OrNode,dif_arg,Attr) ->
		Attr = node(M,Vars),
		O is N + M - 1
	;
		O = N,
		Vars = []
	),
	put_attr(OrNode,dif_arg,node(O,List)).

dif_c_c_l_aux([],_,List,List).
dif_c_c_l_aux([X=Y|Unifier],OrNode,List,Tail) :-
	List = [X=Y|Rest],
	add_ornode(X,Y,OrNode),
	dif_c_c_l_aux(Unifier,OrNode,Rest,Tail).

add_ornode(X,Y,OrNode) :-
	add_ornode_var1(X,Y,OrNode),
	( var(Y) ->
		add_ornode_var2(X,Y,OrNode)
	;
		true
	).

add_ornode_var1(X,Y,OrNode) :-
	( get_attr(X,dif_arg,Attr) ->
		Attr = vardif(V1,V2),
		put_attr(X,dif_arg,vardif([OrNode-Y|V1],V2))
	;
		put_attr(X,dif_arg,vardif([OrNode-Y],[]))
	).

add_ornode_var2(X,Y,OrNode) :-
	( get_attr(Y,dif_arg,Attr) ->
		Attr = vardif(V1,V2),
		put_attr(Y,dif_arg,vardif(V1,[OrNode-X|V2]))
	;
		put_attr(Y,dif_arg,vardif([],[OrNode-X]))
	).

vardif:attr_unify_hook(vardif(V1,V2),Other) :-
	( var(Other) ->
		reverse_lookups(V1,Other,OrNodes1,NV1),
		or_one_fails(OrNodes1),
		get_attr(Other,dif_arg,OAttr),
		OAttr = vardif(OV1,OV2),
		reverse_lookups(OV1,Other,OrNodes2,NOV1),
		or_one_fails(OrNodes2),
		remove_obsolete(V2,Other,NV2),
		remove_obsolete(OV2,Other,NOV2),
		append(NV1,NOV1,CV1),
		append(NV2,NOV2,CV2),
		( CV1 == [], CV2 == [] ->
			del_attr(Other,dif_arg)
		;
			put_attr(Other,dif_arg,vardif(CV1,CV2))
		)
	;
		verify_compounds(V1,Other),
		verify_compounds(V2,Other)
	).

remove_obsolete([], _, []).
remove_obsolete([N-Y|T], X, L) :-
        (   Y==X ->
            remove_obsolete(T, X, L)
        ;   L=[N-Y|RT],
            remove_obsolete(T, X, RT)
        ).

reverse_lookups([],_,[],[]).
reverse_lookups([N-X|NXs],Value,Nodes,Rest) :-
	( X == Value ->
		Nodes = [N|RNodes],
		Rest = RRest
	;
		Nodes = RNodes,
		Rest = [N-X|RRest]
	),
	reverse_lookups(NXs,Value,RNodes,RRest).

verify_compounds([],_).
verify_compounds([Node-Y|Rest],X) :-
	( var(Y) ->
		true
	;
		dif_c_c(X,Y,Node)
	),
	verify_compounds(Rest,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
or_succeed(OrNode) :-
	( attvar(OrNode) ->
		get_attr(OrNode,dif_arg,Attr),
		Attr = node(_Counter,Pairs),
		del_attr(OrNode,dif_arg),
		OrNode = (-),
		del_or_dif(Pairs)
	;
		true
	).

or_one_fails([]).
or_one_fails([N|Ns]) :-
	or_one_fail(N),
	or_one_fails(Ns).

or_one_fail(OrNode) :-
	( attvar(OrNode) ->
		get_attr(OrNode,dif_arg,Attr),
		Attr = node(Counter,Pairs),
		NCounter is Counter - 1,
		( NCounter == 0 ->
			fail
		;
			put_attr(OrNode,dif_arg,node(NCounter,Pairs))
		)
	;
		fail
	).

del_or_dif([]).
del_or_dif([X=Y|Xs]) :-
	cleanup_dead_nodes(X),
	cleanup_dead_nodes(Y),
	del_or_dif(Xs).

cleanup_dead_nodes(X) :-
 	( attvar(X) ->
 		get_attr(X,dif_arg,Attr),
		Attr = vardif(V1,V2),
		filter_dead_ors(V1,NV1),
		filter_dead_ors(V2,NV2),
		( NV1 == [], NV2 == [] ->
			del_attr(X,dif_arg)
		;
			put_attr(X,dif_arg,vardif(NV1,NV2))
		)
	;
		true
	).

filter_dead_ors([],[]).
filter_dead_ors([Or-Y|Rest],List) :-
	( var(Or) ->
		List = [Or-Y|NRest]
	;
		List = NRest
	),
	filter_dead_ors(Rest,NRest).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   The attribute of a variable X is vardif/2. The first argument is a
   list of pairs. The first component of each pair is an OrNode. The
   attribute of each OrNode is node/2. The second argument of node/2
   is a list of equations A = B. If the LHS of the first equation is
   X, then return a goal, otherwise don't because someone else will.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

vardiff:attribute_goals(Var) -->
	(   { get_attr(Var, dif_arg, vardif(Ors,_)) } ->
	    or_nodes(Ors, Var)
	;   or_node(Var)
	).

or_node(O) -->
        (   { get_attr(O, dif_arg, node(_, Pairs)) } ->
            { eqs_lefts_rights(Pairs, As, Bs) },
            mydif(As, Bs),
            { del_attr(O, dif_arg) }
        ;   []
        ).

or_nodes([], _)       --> [].
or_nodes([O-_|Os], X) -->
	(   { get_attr(O, dif_arg, node(_, Eqs)) } ->
            (   { Eqs = [LHS=_|_], LHS == X } ->
                { eqs_lefts_rights(Eqs, As, Bs) },
                mydif(As, Bs),
                { del_attr(O, dif_arg) }
            ;   []
            )
        ;   [] % or-node already removed
        ),
	or_nodes(Os, X).

mydif([X], [Y]) --> !, [dif_arg(X, Y)].
mydif(Xs0, Ys0) --> [dif_arg(X,Y)],
        { reverse(Xs0, Xs), reverse(Ys0, Ys), % follow original order
          X =.. [f|Xs], Y =.. [f|Ys] }.

eqs_lefts_rights([], [], []).
eqs_lefts_rights([A=B|ABs], [A|As], [B|Bs]) :-
        eqs_lefts_rights(ABs, As, Bs).


same(X,Y):- samef(X,Y),!.
same(X,Y):- compound(X),arg(1,X,Y),!.
same(X,Y):- compound(Y),arg(1,Y,X),!.


samef(X,Y):- X=Y,!.
samef(X,Y):- hotrace(((functor_safe(X,XF,_),functor_safe(Y,YF,_),string_equal_ci(XF,YF)))).


:-export(arg_to_var/3).
arg_to_var(_Type,_String,_Var).

:-export(same_arg/3).

same_arg(_How,X,Y):-var(X),var(Y),!,grtrace,X=Y.
same_arg(equals,X,Y):-!, unify_with_occurs_check(X,Y).
same_arg(type(_Type),X,Y):-!, unify_with_occurs_check(X,Y).

same_arg(text,X,Y):-!, string_equal_ci(X,Y).

same_arg(same_or(equals),X,Y):- same_arg(equals,X,Y).
same_arg(same_or(subclass),X,Y):- same_arg(equals,X,Y).
same_arg(same_or(subclass),Sub,Sup):- holds_t(subclass,Sub,Sup),!.
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
	trigger(+, 0),
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
	trigger(Cond, Goal).

trigger(nonvar(X),Goal) :- 
	trigger_nonvar(X,Goal).
trigger(ground(X),Goal) :- 
	trigger_ground(X,Goal).
trigger(?=(X,Y),Goal) :- 
	trigger_determined(X,Y,Goal).
trigger(pred(X,Pred),Goal) :- 
	trigger_pred(X,Pred,Goal).
trigger((G1,G2),Goal) :- 
	trigger_conj(G1,G2,Goal).
trigger(or(GL),Goal) :- 
	trigger_disj(GL, check_disj(_DisjID,GL,Goal)).

trigger_nonvar(X, Goal) :- 
	(   nonvar(X)
	->  call(Goal)
	;   '$suspend'(X, when_met, trigger_nonvar(X, Goal))
	).

trigger_pred(X,Pred, Goal) :- 
	(   call(Pred, X)
	->  call(Goal)
	;   '$suspend'(X, when_met, trigger_pred(X,Pred, Goal))
	).

trigger_ground(X, Goal) :- 
	term_variables(X, Vs),
	(   Vs = [H]
	->  '$suspend'(H, when_met, trigger_ground(H, Goal))
	;   Vs = [H|_]
	->  T =.. [f|Vs],
	    '$suspend'(H, when_met, trigger_ground(T, Goal))
	;   call(Goal)
	).

trigger_determined(X, Y, Goal) :- 
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
	trigger(G1, trigger(G2,Goal)).

trigger_disj([],_).
trigger_disj([H|T], G) :- 
	trigger(H, G),
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
	;   { G = moo:trigger(C, Goal) }
	->  [ when_met((?=(X,Y),C), Goal) ]
	;   [ when_met(?=(X,Y), G) ]
	).
when_goals(call(Conj)) -->
	when_conj_goals(Conj).

when_conj_goals((A,B)) --> !,
	when_conj_goals(A),
	when_conj_goals(B).
when_conj_goals(moo:G) -->
	when_goal(G).

when_goal(trigger_nonvar(X, G)) -->
	(   { disj_goal(G, Disj, DG) }
	->  disj_or(Disj, DG)
	;   { G = moo:trigger(C, Goal) }
	->  [ when_met((nonvar(X),C), Goal) ]
	;   [ when_met(nonvar(X),G) ]
	).
when_goal(trigger_ground(X, G)) -->
	(   { disj_goal(G, Disj, DG) }
	->  disj_or(Disj, DG)
	;   { G = moo:trigger(C, Goal) }
	->  [ when_met((ground(X),C), Goal) ]
	;   [ when_met(ground(X),G) ]
	).
when_goal(wake_det(_)) -->
	[].

disj_goal(moo:check_disj(X, _, _), [], -) :- X == (-).
disj_goal(moo:check_disj(-, Or, DG), Or, DG).

disj_or([], _) --> [].
disj_or(List, DG) -->
	{ or_list(List, Or) },
	[when_met(Or, DG)].

or_list([H], H) :- !.
or_list([H|T], (H;OT)) :- 
	or_list(T, OT).


