/** <module> 
% ===================================================================
% File 'dbase'
% Purpose: Emulation of OpenCyc for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface' 1.0.0
% Revision:  $Revision: 1.9 $
% Revised At:   $Date: 2002/06/27 14:13:20 $
% ===================================================================
% File used as storage place for all predicates which_f change as
% the world is run.
%
% props(Obj,height(ObjHt))  == k(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt))  == padd(height,Obj,ObjHt,...) == add(QueryForm)
% kretract[all](Obj,height(ObjHt))  == kretract[all](Obj,height,ObjHt) == pretract[all](height,Obj,ObjHt) == del[all](QueryForm)
% keraseall(AnyTerm).
%
%
% Dec 13, 2035
% Douglas Miles
*/
:- module(dbase,[add/1,
argIsa_call/3,
assertThrough/1,
assertThrough/2,
balanceBinding/2,
clause_present_1/3, 
define_subtype/2,
do_expand_args/3,
call_no_cuts/1,
expand_head/2,
clr/1,
call_tabled/1,
cycAssert/1,
call_after_game_load/1,
cycAssert/2,
% db_query_lc/1,
cycInit/0,
cyclify/2,
cyclifyNew/2,
db_op0/2,
cycQuery/1,
cycQuery/2,
split_name_type/3,
cycRetract/1,
cycRetract/2,
cycRetractAll/1,
cycRetractAll/2,
cycStats/0,
db_query/2,
db_op/2,
non_assertable/1,
defaultMt/1,
db_op00/2,
del/1,
ensureMt/1,
findall_type_default_props/3,
finishCycConnection/3,
formatCyc/3,
getCycConnection/3,
getSurfaceFromChars/3,
holds_f/1,holds_f/2,holds_f/3,holds_f/4,holds_f/5,holds_f/6,holds_f/7,holds_f/8,holds_t/1,holds_t/2,holds_t/3,holds_t/4,holds_t/5,holds_t/6,holds_t/7,holds_t/8,
holds_tcall/1,
inside_clause_expansion/1,
invokeSubL/1,
invokeSubL/2,
invokeSubLRaw/2,
isCycPredArity/2,
isDebug/0,
isSlot/1,
list_to_term/2,
lowerCasePred/1,
makeConstant/1,
try_mud_body_expansion/2,
try_mud_head_expansion/2,
padd/2,
padd/3,
printSubL/2,
process_mworld/0,
prop/3,
prop_or/4,
props/2,
req/1,
retractAllThrough/1,
retractAllThrough/2,
scan_arities/0,
stringToWords/2,
term_listing/1,
toCycApiExpression/2,
toCycApiExpression/3,
use_term_listing/2,
useExternalDBs/0,
with_kb_assertions/2,
world_clear/1,
xcall_t/1,
xcall_t/2,
xcall_t/3,
xcall_t/4,
xcall_t/5,
xcall_t/6,
xcall_t/7,
xcall_f/1,
xcall_f/2,
xcall_f/3,
xcall_f/4,
xcall_f/5,
xcall_f/6,
xcall_f/7,         
testOpenCyc/0]).

:- set_prolog_flag(double_quotes, atom).
:- set_prolog_flag(double_quotes, string).

:- use_module(logicmoo(logicmoo_util/logicmoo_util_bugger)).
:- use_module(logicmoo(logicmoo_util/logicmoo_util_library)).
:- use_module(logicmoo(logicmoo_util/logicmoo_util_ctx_frame)).
:- use_module(logicmoo(logicmoo_util/logicmoo_util_strings)).
:- use_module(logicmoo(logicmoo_util/logicmoo_util_terms)).
:- use_module(logicmoo(logicmoo_util/logicmoo_util_dcg)).

:- include(logicmoo(vworld/dbase_i_cyc)).

/*

4 ?- predicate_property(assert(_),Z).
Z = visible ;
Z = built_in ;
Z = foreign ;
Z = imported_from(system) ;
Z = transparent ;
Z = (meta_predicate assert(:)) ;
Z = nodebug ;

5 ?- predicate_property(call(_),Z).
Z = interpreted ;
Z = visible ;
Z = built_in ;
Z = imported_from(system) ;
Z = transparent ;
Z = (meta_predicate call(0)) ;
Z = file('c:/pf/swipl/boot/init.pl') ;
Z = line_count(213) ;
Z = nodebug ;
Z = number_of_clauses(1) ;
Z = number_of_rules(1) ;
Z = noprofile ;
Z = iso ;

7 ?- predicate_property(forall(_,_),Z).
Z = interpreted ;
Z = visible ;
Z = built_in ;
Z = imported_from('$apply') ;
Z = transparent ;
Z = (meta_predicate forall(0, 0)) ;
Z = file('c:/pf/swipl/boot/apply.pl') ;
Z = line_count(48) ;
Z = nodebug ;
Z = number_of_clauses(1) ;
Z = number_of_rules(1) ;
Z = noprofile ;
*/

call_after_game_load(Code):-call_after(moodb:not_loading_game_file,Code).

:-decl_mpred((
     type/1, agent/1, item/1, region/1,

     verbOverride/3,named/2, determinerString/2, keyword/2 ,descriptionHere/2, mudToHitArmorClass0/2,

      thinking/1,
 weight/2,
 permanence/3,
      act_term/2,
      act_turn/2,
      agent_doing/2,
      agent_done/2,
      atloc/2,
      charge/2,
      damage/2,
      description/2,
      facing/2,
      failure/2,
      grid/4,
      height/2,
      memory/2,
      mtForPred/2,
      isa/2,
      pathName/3, 
      possess/2,
      score/2,
      stm/2,      
      str/2,
      wearing/2)).

:- moodb:dbase_mod(DBASE), dynamic(DBASE:inside_clause_expansion/1).


:- moodb:dbase_mod(DBASE), 
          DBASE:(dynamic((
          dbase_t/1,
          dbase_t/2,
          dbase_t/3,
          dbase_t/4,
          dbase_t/5,
          dbase_t/6,
          dbase_t/7,
          asserted_dbase_t/1,
          asserted_dbase_t/2,
          asserted_dbase_t/3,
          asserted_dbase_t/4,
          asserted_dbase_t/5,
          asserted_dbase_t/6,
          asserted_dbase_t/7,
          assertion_f/1,
          assertion_t/1,
          asserted_dbase_f/1,
          asserted_dbase_f/2,
          asserted_dbase_f/3,
          asserted_dbase_f/4,
          asserted_dbase_f/5,
          asserted_dbase_f/6,
          asserted_dbase_f/7,
          dbase_f/1,
          dbase_f/2,
          dbase_f/3,
          dbase_f/4,
          dbase_f/5,
          dbase_f/6,
          dbase_f/7))).

:- moodb:dbase_mod(DBASE), 
          DBASE:(export((
          dbase_t/1,
          dbase_t/2,
          dbase_t/3,
          dbase_t/4,
          dbase_t/5,
          dbase_t/6,
          dbase_t/7,
          asserted_dbase_t/1,
          asserted_dbase_t/2,
          asserted_dbase_t/3,
          asserted_dbase_t/4,
          asserted_dbase_t/5,
          asserted_dbase_t/6,
          asserted_dbase_t/7,
          assertion_f/1,
          assertion_t/1,
          asserted_dbase_f/1,
          asserted_dbase_f/2,
          asserted_dbase_f/3,
          asserted_dbase_f/4,
          asserted_dbase_f/5,
          asserted_dbase_f/6,
          asserted_dbase_f/7,
          dbase_f/1,
          dbase_f/2,
          dbase_f/3,
          dbase_f/4,
          dbase_f/5,
          dbase_f/6,
          dbase_f/7))).

:- moodb:dbase_mod(DBASE), 
          DBASE:(multifile((
          dbase_t/1,
          dbase_t/2,
          dbase_t/3,
          dbase_t/4,
          dbase_t/5,
          dbase_t/6,
          dbase_t/7,
          asserted_dbase_t/1,
          asserted_dbase_t/2,
          asserted_dbase_t/3,
          asserted_dbase_t/4,
          asserted_dbase_t/5,
          asserted_dbase_t/6,
          asserted_dbase_t/7,
          assertion_f/1,
          assertion_t/1,
          asserted_dbase_f/1,
          asserted_dbase_f/2,
          asserted_dbase_f/3,
          asserted_dbase_f/4,
          asserted_dbase_f/5,
          asserted_dbase_f/6,
          asserted_dbase_f/7,
          dbase_f/1,
          dbase_f/2,
          dbase_f/3,
          dbase_f/4,
          dbase_f/5,
          dbase_f/6,
          dbase_f/7))).

:- moodb:dbase_mod(DBASE), 
          DBASE:(discontiguous((
          dbase_t/1,
          dbase_t/2,
          dbase_t/3,
          dbase_t/4,
          dbase_t/5,
          dbase_t/6,
          dbase_t/7,
          asserted_dbase_t/1,
          asserted_dbase_t/2,
          asserted_dbase_t/3,
          asserted_dbase_t/4,
          asserted_dbase_t/5,
          asserted_dbase_t/6,
          asserted_dbase_t/7,
          assertion_f/1,
          assertion_t/1,
          asserted_dbase_f/1,
          asserted_dbase_f/2,
          asserted_dbase_f/3,
          asserted_dbase_f/4,
          asserted_dbase_f/5,
          asserted_dbase_f/6,
          asserted_dbase_f/7,
          dbase_f/1,
          dbase_f/2,
          dbase_f/3,
          dbase_f/4,
          dbase_f/5,
          dbase_f/6,
          dbase_f/7))).


:- meta_predicate mud_pred_expansion_2(3,?,?,*,*,*).
:- meta_predicate mud_pred_expansion_1(3,*,*,*).
:- meta_predicate trigger_determined(*,*,0).
:- meta_predicate trigger_first(*,0).
:- meta_predicate trigger_pred(?,1,0).
:- meta_predicate trigger_nonvar(*,0).
:- meta_predicate trigger_ground(*,0).
:- meta_predicate check_disj(*,*,0).
:- meta_predicate mud_pred_expansion_0(3,?,*,?).

:- meta_predicate clause_present(:), db_assert_mv(+,+,+,0), db_assert_sv(+,+,+,-), db_op_exact(+,+), db_quf(+,-,-).

%  argIsa_call/3, use_term_listing/2,world_clear/1.

:- dynamic db_prop_prolog/11.

% :- context_module(M),asserta(moodb:dbase_mod(M)),dmsg(assert_if_new(moodb:dbase_mod(M))).

:- dynamic_multifile_exported dbase_t/1.
:- dynamic_multifile_exported dbase_t/2.
:- dynamic_multifile_exported dbase_t/3.
:- dynamic_multifile_exported dbase_t/4.
:- dynamic_multifile_exported dbase_t/5.
:- dynamic_multifile_exported dbase_t/6.
:- dynamic_multifile_exported dbase_t/7.

:- discontiguous dbase_t/1.
:- discontiguous dbase_t/2.
:- discontiguous dbase_t/3.
:- discontiguous dbase_t/4.
:- discontiguous dbase_t/5.
:- discontiguous dbase_t/6.
:- discontiguous dbase_t/7.

:- meta_predicate isDebug(?).
:- meta_predicate ensure_db_predicate(+,+,-).
:- meta_predicate ensure_db_predicate_1(+,+,-).
:- meta_predicate ensure_db_predicate_2(+,+,-).
:- meta_predicate with_kb_assertions(?,?).
:- meta_predicate show_cgoal(?).
:- meta_predicate retractall_cloc(?).
:- meta_predicate retract_cloc(?).
:- meta_predicate assertz_cloc(?).
:- meta_predicate asserta_cloc(?).

:- def_meta_predicate(call_f,3,9).
:- def_meta_predicate(call_t,3,9).
:- def_meta_predicate(xcall_t,1,7).
:- def_meta_predicate(xcall_f,1,7).
:- def_meta_predicate(dbase_t,1,7).
:- def_meta_predicate(dbase_f,1,7).
:- def_meta_predicate(call_mt_f,5,11).
:- def_meta_predicate(call_mt_t,5,11).
:- def_meta_predicate(assertion_t,1,1).
:- def_meta_predicate(assertion_f,1,1).



callable_tf(P,2):-arity_pred(P),!,fail.
callable_tf(F,A):- functor_safe(P,F,A),predicate_property(P,_),!.
:-dynamic(useExternalDBs/0).
useExternalDBs:-fail.
useDBMts:- fail, useExternalDBs.

relax_term(P,P,Aic,Aic,Bic,Bic):-!.
/*
relax_term(P,P,A,A,Bi,Bc):-arg(_,v(subclass,isa),P),!,fail.
relax_term(P,P,Ai,Ac,Bic,Bic):- when_met(nonvar(Ac), same_arg(ic,Ac,Ai)),!.
relax_term(P,P,Ai,Ac,Bi,Bc):- defined_type(Ai),!,when_met(pred(nonvar,Ac), (same_arg(subclass,Ac,Ai),same_arg(i,Bc,Bi))),!.
relax_term(P,P,Ai,Ac,Bi,Bc):- when_met(pred(nonvar,Ac),when_met(pred(nonvar,Bc), (same_arg(ic,Ac,Ai),same_arg(i,Bc,Bi)))).
*/

% ?- member(R,[a,b,c]),when_met(nonvar(Re), dbase:same_arg(i,n,Re)),Re=R,write(chose(R)).

same_arg(i,X,X):-!.
same_arg(subclass,X,X):-!.
same_arg(subclass,Sub,Sup):-holds_t(subclass,Sub,Sup),!.
same_arg(ic,X,X):-!.
same_arg(ic,I,Sup):-holds_t(isa,I,Sup),!.

% same_arg(I,X):- promp_yn('~nSame Objects: ~q==~q ?',[I,X]).
promp_yn(Fmt,A):-format(Fmt,A),get_single_char(C),C=121.

:- set_prolog_flag(generate_debug_info, true).
:-debug.

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

attr_unify_hook(call(Goal), Other) :-
	(   get_attr(Other, when_met, call(GOTher))
	->  del_attr(Other, when_met),
	    Goal, GOTher
	;   Goal
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
attribute_goals(V) -->
	{ get_attr(V, when_met, Attr) },
	when_goals(Attr).

when_goals(det(trigger_determined(X, Y, G))) --> !,
	(   { disj_goal(G, Disj, DG) }
	->  disj_or(Disj, DG)
	;   { G = dbase:trigger(C, Goal) }
	->  [ when_met((?=(X,Y),C), Goal) ]
	;   [ when_met(?=(X,Y), G) ]
	).
when_goals(call(Conj)) -->
	when_conj_goals(Conj).

when_conj_goals((A,B)) --> !,
	when_conj_goals(A),
	when_conj_goals(B).
when_conj_goals(dbase:G) -->
	when_goal(G).

when_goal(trigger_nonvar(X, G)) -->
	(   { disj_goal(G, Disj, DG) }
	->  disj_or(Disj, DG)
	;   { G = dbase:trigger(C, Goal) }
	->  [ when_met((nonvar(X),C), Goal) ]
	;   [ when_met(nonvar(X),G) ]
	).
when_goal(trigger_ground(X, G)) -->
	(   { disj_goal(G, Disj, DG) }
	->  disj_or(Disj, DG)
	;   { G = dbase:trigger(C, Goal) }
	->  [ when_met((ground(X),C), Goal) ]
	;   [ when_met(ground(X),G) ]
	).
when_goal(wake_det(_)) -->
	[].

disj_goal(dbase:check_disj(X, _, _), [], -) :- X == (-).
disj_goal(dbase:check_disj(-, Or, DG), Or, DG).

disj_or([], _) --> [].
disj_or(List, DG) -->
	{ or_list(List, Or) },
	[when_met(Or, DG)].

or_list([H], H) :- !.
or_list([H|T], (H;OT)) :-
	or_list(T, OT).

% ================================================================================
% begin holds_t
% ================================================================================
which_t(dac(d,a,no_c,no_mt)).

holds_t(P,A1,A2,A3,A4,A5,A6,A7):- isCycPredArity_ignoreable(P,7),which_t(DBS),(call_t(DBS,P,A1,A2,A3,A4,A5,A6,A7);call_mt_t(DBS,P,A1,A2,A3,A4,A5,A6,A7,_,_);assertion_t([P,A1,A2,A3,A4,A5,A6,A7])).
holds_t(P,A1,A2,A3,A4,A5,A6):- isCycPredArity_ignoreable(P,6),which_t(DBS),(call_t(DBS,P,A1,A2,A3,A4,A5,A6);call_mt_t(DBS,P,A1,A2,A3,A4,A5,A6,_,_)).
holds_t(P,A1,A2,A3,A4,A5):-isCycPredArity_ignoreable(P,5),which_t(DBS),(call_t(DBS,P,A1,A2,A3,A4,A5);call_mt_t(DBS,P,A1,A2,A3,A4,A5,_,_)).
holds_t(P,A1,A2,A3,A4):- isCycPredArity_ignoreable(P,4),which_t(DBS),(call_t(DBS,P,A1,A2,A3,A4);call_mt_t(DBS,P,A1,A2,A3,A4,_,_)).
holds_t(P,A1,A2,A3):-isCycPredArity_ignoreable(P,3),which_t(DBS),(call_t(DBS,P,A1,A2,A3);call_mt_t(DBS,P,A1,A2,A3,_,_)).
holds_t(P,A1,A2):-holds_relaxed_t(P,A1,A2).
holds_t(P,A1):-!,req(isa(A1,P)).
holds_t(P,A1):-isCycPredArity_ignoreable(P,1),which_t(DBS),(call_t(DBS,P,A1);call_mt_t(DBS,P,A1,_,_)).

% holds_relaxed_t(Arity, P, A):- Arity == arity,!,isCycPredArity(P,A).
holds_relaxed_t(Mpred, FA, [Prop]):- Mpred==mpred,!,get_mpred_prop(FA,Prop).
holds_relaxed_t(P,A1,A2):- isCycPredArity_ignoreable(P,2),which_t(DBS),!,relax_term(P,PR,A1,R1,A2,R2),holds_relaxed_0_t(DBS,PR,R1,R2).
holds_relaxed_0_t(DBS,P,A1,A2):-call_t(DBS,P,A1,A2).
holds_relaxed_0_t(DBS,P,A1,A2):-call_mt_t(DBS,P,A1,A2,_,_).
holds_relaxed_0_t(DBS,P,A1,A2):-ground((P,A1)), TEMPL=..[P,T1,_],dbase_t(default_sv,TEMPL,A2),req(isa(A1,T1)),!.


holds_t([AH,P|LIST]):-is_holds_true(AH),!,holds_t_p2(P,LIST).
holds_t([AH,P|LIST]):-is_holds_false(AH),!,holds_f_p2(P,LIST).
holds_t([P|LIST]):-!,holds_t_p2(P,LIST).
holds_t(CALL):-CALL=..[P|LIST],holds_t([P|LIST]).
holds_t_p2(P,LIST):- CALL=..[holds_t,P|LIST],call(CALL).

dbase_t(List):-is_list(List),!,Call=..[dbase_t|List],Call.
dbase_t(List):-holds_t(List).

call_list_t(dac(d,_,_,_),CALL,_):-dbase_t(CALL).
call_list_t(dac(_,a,_,_),_,List):-assertion_t(List).
call_list_t(dac(_,_,c,_),CALL,_):-xcall_t(CALL).

call_t(DBS,P,A1,A2,A3,A4,A5,A6,A7):- callable_tf(P,7),List= [P,A1,A2,A3,A4,A5,A6,A7], CALL=..List, call_list_t(DBS,CALL,List).
call_t(dac(d,_,_,_),P,A1,A2,A3,A4,A5,A6):- dbase_t(P,A1,A2,A3,A4,A5,A6).
call_t(dac(_,a,_,_),P,A1,A2,A3,A4,A5,A6):- assertion_t([P,A1,A2,A3,A4,A5,A6]).
call_t(dac(_,_,c,_),P,A1,A2,A3,A4,A5,A6):- callable_tf(P,6),xcall_t(P,A1,A2,A3,A4,A5,A6).
call_t(dac(d,_,_,_),P,A1,A2,A3,A4,A5):- dbase_t(P,A1,A2,A3,A4,A5).
call_t(dac(_,a,_,_),P,A1,A2,A3,A4,A5):- assertion_t([P,A1,A2,A3,A4,A5]).
call_t(dac(_,_,c,_),P,A1,A2,A3,A4,A5):- callable_tf(P,5),xcall_t(P,A1,A2,A3,A4,A5).
call_t(dac(d,_,c,_),P,A1,A2,A3,A4):- dbase_t(P,A1,A2,A3,A4).
call_t(dac(_,a,_,_),P,A1,A2,A3,A4):- assertion_t([P,A1,A2,A3,A4]).
call_t(dac(_,_,c,_),P,A1,A2,A3,A4):- callable_tf(P,4),xcall_t(P,A1,A2,A3,A4).
call_t(dac(d,_,_,_),P,A1,A2,A3):- dbase_t(P,A1,A2,A3).
call_t(dac(_,a,_,_),P,A1,A2,A3):- assertion_t([P,A1,A2,A3]).
call_t(dac(_,_,c,_),P,A1,A2,A3):- callable_tf(P,3),xcall_t(P,A1,A2,A3).
call_t(dac(d,_,_,_),P,A1,A2):- dbase_t(P,A1,A2).
call_t(dac(_,a,_,_),P,A1,A2):- assertion_t([P,A1,A2]).
call_t(dac(_,_,c,_),P,A1,A2):- callable_tf(P,2),xcall_t(P,A1,A2).
call_t(dac(d,_,_,_),P,A1):- dbase_t(P,A1).
call_t(dac(_,a,_,_),P,A1):- assertion_t([P,A1]).
call_t(dac(_,_,c,_),P,A1):- callable_tf(P,1),xcall_t(P,A1).

call_mt_t(dac(_,_,_,mt),P,A1,A2,A3,A4,A5,A6,A7,A8,A9):- callable_tf(P,9),CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8,A9],xcall_t(CALL).
call_mt_t(dac(_,_,_,mt),P,A1,A2,A3,A4,A5,A6,A7,A8):- callable_tf(P,8),CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8],xcall_t(CALL).
call_mt_t(dac(_,_,_,mt),P,A1,A2,A3,A4,A5,A6,A7):- callable_tf(P,7),CALL=..[P,A1,A2,A3,A4,A5,A6,A7],xcall_t(CALL).
call_mt_t(dac(_,_,_,mt),P,A1,A2,A3,A4,A5,A6):- callable_tf(P,6),xcall_t(P,A1,A2,A3,A4,A5,A6).
call_mt_t(dac(_,_,_,mt),P,A1,A2,A3,A4,A5):- callable_tf(P,5),xcall_t(P,A1,A2,A3,A4,A5).
call_mt_t(dac(_,_,_,mt),P,A1,A2,A3,A4):- callable_tf(P,4),xcall_t(P,A1,A2,A3,A4).
call_mt_t(dac(_,_,_,mt),P,A1,A2,A3):- callable_tf(P,3),xcall_t(P,A1,A2,A3).
call_mt_t(dac(_,_,_,mt),P,A1,A2):- callable_tf(P,3),xcall_t(P,A1,A2).

xcall_t(P,A1,A2,A3,A4,A5,A6,A7,A8,A9):- CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8,A9],call(CALL).
xcall_t(P,A1,A2,A3,A4,A5,A6,A7,A8):- CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8],call(CALL).
xcall_t(P,A1,A2,A3,A4,A5,A6,A7):- CALL=..[P,A1,A2,A3,A4,A5,A6,A7],call(CALL).
xcall_t(P,A1,A2,A3,A4,A5,A6):- call(P,A1,A2,A3,A4,A5,A6).
xcall_t(P,A1,A2,A3,A4,A5):- call(P,A1,A2,A3,A4,A5).
xcall_t(P,A1,A2,A3,A4):- call(P,A1,A2,A3,A4).
xcall_t(P,A1,A2,A3):- call(P,A1,A2,A3).
xcall_t(P,A1,A2):- call(P,A1,A2).
xcall_t(P,A1):- call(P,A1).
xcall_t(P):- call(P).

assertion_t([AH,P|LIST]):-is_holds_true(AH),!,assertion_t([P|LIST]).
assertion_t([AH,P|LIST]):-is_holds_false(AH),!,assertion_f([P|LIST]).
% todo hook into loaded files!
assertion_t(_):-not(useExternalDBs),!,fail.
assertion_t([P|LIST]):- tiny_kb:'ASSERTION'(':TRUE-DEF',_,_UniversalVocabularyMt,_Vars,/*HL*/[P|LIST]).
assertion_t([P|LIST]):-tiny_kb:'ASSERTION'(':TRUE-MON',_,_UniversalVocabularyMt,_Vars,/*HL*/[P|LIST]).
assertion_t([P|LIST]):- append([assertion_holds_mworld0,P|LIST],[_,_],CallList),Call=..CallList, '@'(xcall_t(Call),mworld0).
assertion_t([P|LIST]):- Call=..[assertion_holds,P|LIST], '@'(xcall_t(Call),hl_holds).

% ================================================================================
% end holds_t
% ================================================================================


% ================================================================================
% begin holds_f
% ================================================================================
which_f(dac(d,no_a,no_c,no_mt)).

holds_f(P,A1,A2,A3,A4,A5,A6,A7):- isCycPredArity_ignoreable(P,7),which_f(DBS),(call_f(DBS,P,A1,A2,A3,A4,A5,A6,A7);call_mt_f(DBS,P,A1,A2,A3,A4,A5,A6,A7,_,_);assertion_f([P,A1,A2,A3,A4,A5,A6,A7])).
holds_f(P,A1,A2,A3,A4,A5,A6):- isCycPredArity_ignoreable(P,6),which_f(DBS),(call_f(DBS,P,A1,A2,A3,A4,A5,A6);call_mt_f(DBS,P,A1,A2,A3,A4,A5,A6,_,_)).
holds_f(P,A1,A2,A3,A4,A5):-isCycPredArity_ignoreable(P,5),which_f(DBS),(call_f(DBS,P,A1,A2,A3,A4,A5);call_mt_f(DBS,P,A1,A2,A3,A4,A5,_,_)).
holds_f(P,A1,A2,A3,A4):- isCycPredArity_ignoreable(P,4),which_f(DBS),(call_f(DBS,P,A1,A2,A3,A4);call_mt_f(DBS,P,A1,A2,A3,A4,_,_)).
holds_f(P,A1,A2,A3):-isCycPredArity_ignoreable(P,3),which_f(DBS),(call_f(DBS,P,A1,A2,A3);call_mt_f(DBS,P,A1,A2,A3,_,_)).
holds_f(P,A1,A2):-holds_relaxed_t(P,A1,A2).
holds_f(P,A1):-isCycPredArity_ignoreable(P,1),which_f(DBS),(call_f(DBS,P,A1);call_mt_f(DBS,P,A1,_,_)).


holds_relaxed_f(P,A1,A2):- isCycPredArity_ignoreable(P,2),which_f(DBS),!,relax_term(P,PR,A1,R1,A2,R2),holds_relaxed_0_f(DBS,PR,R1,R2).
holds_relaxed_0_f(DBS,P,A1,A2):-call_f(DBS,P,A1,A2).
holds_relaxed_0_f(DBS,P,A1,A2):-call_mt_f(DBS,P,A1,A2,_,_).


holds_f([AH,P|LIST]):-is_holds_true(AH),!,holds_f_p2(P,LIST).
holds_f([AH,P|LIST]):-is_holds_false(AH),!,holds_t_p2(P,LIST).
holds_f([P|LIST]):-!,holds_t_p2(P,LIST).
holds_f(CALL):-CALL=..[P|LIST],holds_f([P|LIST]).
holds_f_p2(P,LIST):- CALL=..[holds_f,P|LIST],call(CALL).

dbase_f(List):-is_list(List),!,Call=..[dbase_t|List],Call.
dbase_f(List):-holds_f(List).


call_f(_,P,A1,A2,A3,A4,A5,A6,A7):- callable_tf(P,7),List= [P,A1,A2,A3,A4,A5,A6,A7], CALL=..List,(assertion_f(List);dbase_f(CALL);xcall_f(CALL)).
call_f(dac(d,_,_,_),P,A1,A2,A3,A4,A5,A6):- dbase_f(P,A1,A2,A3,A4,A5,A6).
call_f(dac(_,a,_,_),P,A1,A2,A3,A4,A5,A6):- assertion_f([P,A1,A2,A3,A4,A5,A6]).
call_f(dac(_,_,c,_),P,A1,A2,A3,A4,A5,A6):- callable_tf(P,6),xcall_f(P,A1,A2,A3,A4,A5,A6).
call_f(dac(d,_,_,_),P,A1,A2,A3,A4,A5):- dbase_f(P,A1,A2,A3,A4,A5).
call_f(dac(_,a,_,_),P,A1,A2,A3,A4,A5):- assertion_f([P,A1,A2,A3,A4,A5]).
call_f(dac(_,_,c,_),P,A1,A2,A3,A4,A5):- callable_tf(P,5),xcall_f(P,A1,A2,A3,A4,A5).
call_f(dac(d,_,c,_),P,A1,A2,A3,A4):- dbase_f(P,A1,A2,A3,A4).
call_f(dac(_,a,_,_),P,A1,A2,A3,A4):- assertion_f([P,A1,A2,A3,A4]).
call_f(dac(_,_,c),c,P,A1,A2,A3,A4):- callable_tf(P,4),xcall_f(P,A1,A2,A3,A4).
call_f(dac(d,_,_,_),P,A1,A2,A3):- dbase_f(P,A1,A2,A3).
call_f(dac(_,a,_,_),P,A1,A2,A3):- assertion_f([P,A1,A2,A3]).
call_f(dac(_,_,c,_),P,A1,A2,A3):- callable_tf(P,3),xcall_f(P,A1,A2,A3).
call_f(dac(d,_,_,_),P,A1,A2):- dbase_f(P,A1,A2).
call_f(dac(_,a,_,_),P,A1,A2):- assertion_f([P,A1,A2]).
call_f(dac(_,_,c,_),P,A1,A2):- callable_tf(P,2),xcall_f(P,A1,A2).
call_f(dac(d,_,_,_),P,A1):- dbase_f(P,A1).
call_f(dac(_,a,_,_),P,A1):- assertion_f([P,A1]).
call_f(dac(_,_,c,_),P,A1):- callable_tf(P,1),xcall_f(P,A1).

call_mt_f(dac(_,_,_,mt),P,A1,A2,A3,A4,A5,A6,A7,A8,A9):- callable_tf(P,9),CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8,A9],xcall_f(CALL).
call_mt_f(dac(_,_,_,mt),P,A1,A2,A3,A4,A5,A6,A7,A8):- callable_tf(P,8),CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8],xcall_f(CALL).
call_mt_f(dac(_,_,_,mt),P,A1,A2,A3,A4,A5,A6,A7):- callable_tf(P,7),CALL=..[P,A1,A2,A3,A4,A5,A6,A7],xcall_f(CALL).
call_mt_f(dac(_,_,_,mt),P,A1,A2,A3,A4,A5,A6):- callable_tf(P,6),xcall_f(P,A1,A2,A3,A4,A5,A6).
call_mt_f(dac(_,_,_,mt),P,A1,A2,A3,A4,A5):- callable_tf(P,5),xcall_f(P,A1,A2,A3,A4,A5).
call_mt_f(dac(_,_,_,mt),P,A1,A2,A3,A4):- callable_tf(P,4),xcall_f(P,A1,A2,A3,A4).
call_mt_f(dac(_,_,_,mt),P,A1,A2,A3):- callable_tf(P,3),xcall_f(P,A1,A2,A3).
call_mt_f(dac(_,_,_,mt),P,A1,A2):- callable_tf(P,2),xcall_f(P,A1,A2).

xcall_f(P,A1,A2,A3,A4,A5,A6,A7,A8,A9):- CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8,A9],\+ xcall_t(CALL).
xcall_f(P,A1,A2,A3,A4,A5,A6,A7,A8):- CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8],\+ xcall_t(CALL).
xcall_f(P,A1,A2,A3,A4,A5,A6,A7):- CALL=..[P,A1,A2,A3,A4,A5,A6,A7],\+ xcall_t(CALL).
xcall_f(P,A1,A2,A3,A4,A5,A6):- \+ xcall_t(P,A1,A2,A3,A4,A5,A6).
xcall_f(P,A1,A2,A3,A4,A5):- \+ xcall_t(P,A1,A2,A3,A4,A5).
xcall_f(P,A1,A2,A3,A4):- \+ xcall_t(P,A1,A2,A3,A4).
xcall_f(P,A1,A2,A3):- \+ xcall_t(P,A1,A2,A3).
xcall_f(P,A1,A2):- \+ xcall_t(P,A1,A2).
xcall_f(P,A1):- \+ xcall_t(P,A1).
xcall_f(P):- \+ xcall_t(P).

assertion_f([AH,P|LIST]):-is_holds_true(AH),!,assertion_f([P|LIST]).
assertion_f([AH,P|LIST]):-is_holds_false(AH),!,assertion_f([P|LIST]).
% todo hook into loaded files!
assertion_f(_):-not(useExternalDBs),!,fail.
assertion_f([P|LIST]):-tiny_kb:'ASSERTION'(':FALSE-DEF',_,_UniversalVocabularyMt,_Vars,/*HL*/[P|LIST]).
assertion_f([P|LIST]):-tiny_kb:'ASSERTION'(':FALSE-MON',_,_UniversalVocabularyMt,_Vars,/*HL*/[P|LIST]).


% ================================================================================
% end holds_f 
% ================================================================================


is_creatable_type(Type):-arg(_,p(agent,item,region,concept),SubType).
is_creatable_type(Type):-holds_t([isa,Type,creatable_type]).

arityMatches(A,S-E):- !, catch((system:between(S,E,OTHER),A=OTHER),_,(trace,system:between(S,E,OTHER),A=OTHER)).
arityMatches(A,OTHER):-number(OTHER),!,A=OTHER.

isCycPredArity_ignoreable(P,A):-ignore(isCycPredArity(P,A)).

isCycPredArity_Check(P,A):-isCycPredArity(P,A),!.
isCycPredArity_Check(P,A):-get_mpred_prop(P/A,_).

isCycPredArity(P,A):-loop_check_throw(isCycPredArity_lc(P,A)).
isCycPredArity_lc(_:P,A):-nonvar(P),!,isCycPredArity(P,A).
isCycPredArity_lc(P,A):-nonvar(P),!,isCycPredArity0(P,OTHER),!,arityMatches(A,OTHER).
isCycPredArity_lc(P,A):- isCycPredArity0(P,OTHER),arityMatches(A,OTHER).

arity_pred(P):-nonvar(P),arg(_,a(arity,arityMax,arityMin),P).

isCycPredArity0(P,A):- arity_pred(P),!,A=2.
isCycPredArity0(P,A):- isRegisteredCycPred(_,P,A).
isCycPredArity0(P,A):- xcall_t(holds_t(arity,P,A)).
isCycPredArity0(P,A):- xcall_t(holds_t(arityMax,P,A)).
isCycPredArity0(holds,7):-!.
isCycPredArity0(P,_):- is_2nd_order_holds(P),!,fail.
isCycPredArity0(F,A):- integer(A),A>0,isa_type(F),!.
isCycPredArity0(P,A):- xcall_t(((holds_t(arityMin,P,A),not(holds_t(arityMax,P, _))))).


:- include(logicmoo(vworld/dbase_i_db_preds)).

:- style_check(+discontiguous).
:- style_check(+singleton).


scan_arities:- forall(holds_t(arity,F,A),moodb:decl_mpred(F,A)).

% logicmoo('vworld/dbase') compiled into dbase 11.97 sec, 2,140,309 clauses
%:- include(logicmoo('pldata/trans_header')).

process_mworld:-!. %forall(dynamicCyc2(C),moodb:registerCycPredPlus2(C)).

% logicmoo('pldata/mworld0.pldata') compiled into world 61.18 sec, 483,738 clauses




% withvars_988 loaded 9.46 sec, 4,888,433 clauses

:- process_mworld.

real_list_undefined(A):-
 merge_options(A, [module_class([user])], B),
        prolog_walk_code([undefined(trace), on_trace(found_undef)|B]),
        findall(C-D, retract(undef(C, D)), E),
        (   E==[]
        ->  true
        ;   print_message(warning, check(undefined_predicates)),
            keysort(E, F),
            group_pairs_by_key(F, G),
            maplist(check:report_undefined, G)
        ).

:- use_module(library(check)).
remove_undef_search:- ((
 redefine_system_predicate(check:list_undefined(_)),
 abolish(check:list_undefined/1),
 assert((check:list_undefined(A):- not(thread_self(main)),!, ignore(A=[]))),
 assert((check:list_undefined(A):- ignore(A=[]),real_list_undefined(A))))).

user_export(_):- moodb:dbase_mod(user),!.
user_export(_M:Prop/Arity):-!,user_export(Prop/Arity).
user_export(Prop/Arity):- 
   moodb:dbase_mod(M), '@'( ((export(Prop/Arity),dynamic(Prop/Arity))) , M).


:- meta_predicate hooked_asserta(^), hooked_assertz(^), hooked_retract(^), hooked_retractall(^).

:- meta_predicate del(+),clr(+),add(+),req(+), db_op(+,+).

% Found new meta-predicates in iteration 1 (0.281 sec)
%:- meta_predicate db_op_exact(?,?,?,0).

% :- include(dbase_types_motel).

% :- moodb:register_module_type(utility).


term_listing(Obj):- nonvar(var(Obj)),catch(listing(Obj),_,fail),fail.
term_listing(Obj):- 
   any_to_atom(Obj,HO),
   get_functor(Obj,F),
   doall((
   predicate_property(H,number_of_clauses(_)),
   functor(H,HF,_),
   HF\==F,
   clause(H,B),
   use_term_listing(Obj,HO,H,B),
   show_term_listing(H,B),
   fail)).

use_term_listing(Obj,HO):-
   clause(HO,B),
   use_term_listing(Obj,HO,HO,B).

use_term_listing(Obj,HO,H,B):- term_to_atom((H:-B),HO), (sub_atom_icasechk(HO,_,Obj);sub_atom_icasechk(Obj,_)),!.
use_term_listing(Obj,_HO,H,B):- not(not(((subst((H:-B),Obj,fov,H1B1), H1B1 \= (H:-B))))),!.


show_term_listing(H,true):- !, show_term_listing(H).
show_term_listing(H,B):- show_term_listing((H:-B)).

show_term_listing(H):- writeq(H),write('.'),nl,!.


:- dynamic(non_assertable/1).
non_assertable(WW,isVar):-var(WW),!.
non_assertable(_:WW,Why):-!,non_assertable(WW,Why).
non_assertable(WW,Why):-compound(WW),functor(WW,F,A),!,never_use_holds_db(F,A,Why).
% non_assertable(WW,Why):-db_prop_game_assert

% replaced the 1st with the 2nd and better version of retract
% del(C0):-db_op(retract,C0)
%% del(RetractOne)    <--  del(C0):- ignore((db_op(ask(Must),C0),!,db_op('retract',C0))).
del(C0):- (db_op('retract',C0) -> db_op('ra',C0) ; dmsg(failed(del(C0)))).
%% clr(Retractall)
clr(C0):-db_op(ra,C0).
%% req(Query)
req(C0):- db_op(ask(call),C0).
mreq(C0):- db_op(ask(must),C0).
%% props(Obj,QueryPropSpecs)
props(Obj,PropSpecs):-db_op(ask(query),props(Obj,PropSpecs)).
%% add(Assertion)
add(C0):- db_op(tell(_OldV), C0).
%% padd(Obj,PropSpecs)
padd(Obj,PropSpecs):-add(props(Obj,PropSpecs)).
%% padd(Obj,Prop,Value)
padd(Obj,Prop,Value):- must(nonvar(Prop)), PropValue=..[Prop,Value],!,padd(Obj,PropValue).
%% prop(Obj,Prop,Value)
prop(Obj,Prop,Value):- holds_t(Prop,Obj,Value).
%% prop_or(Obj,Prop,Value,OrElse)
prop_or(Obj,Prop,Value,OrElse):- atLeastOne0(Value=OrElse,prop(Obj,Prop,Value)).

% TODO: canonicalize clauses first!
with_kb_assertions([],Call):- !,Call.
with_kb_assertions([With|MORE],Call):-!,with_kb_assertions(With,with_kb_assertions(MORE,Call)).
with_kb_assertions(With,Call):- not(not(req(With))),!,Call,del(With).
with_kb_assertions(With,Call):-
   kb_update(With,OldV),
   setup_call_cleanup(add(With),Call,kb_update(OldV,With)).

kb_update(New,OldV):-req(New),!,OldV=New.
kb_update(New,OldV):-db_op(tell(OldV),New).

world_clear(Named):-dmsg('Clearing world database: ~q.',[Named]).

holds_tcall(C):- not(not(predicate_property(C,_PP))),!,call(C).
holds_tcall(Call):-req(Call).



% ?- dbase:do_expand_args(eachOf,subclass(eachOf(mpred_mv,mpred_flag,singleValued),mpred),FOO).



do_expand_args(Exp,Term,Out):-compound(Term),!,do_expand_args_c(Exp,Term,Out).
do_expand_args(_,Term,Term).

do_expand_args_c(Exp,[L|IST],Out):-!,do_expand_args_l(Exp,[L|IST],Out).
do_expand_args_c(Exp,Term,Out):- Term=..[P|ARGS],do_expand_args_pa(Exp,P,ARGS,Out).

do_expand_args_pa(Exp,Exp,ARGS,Out):-!,member(Out,ARGS).
do_expand_args_pa(Exp,P,ARGS,Out):- do_expand_args_l(Exp,ARGS,EARGS), Out=..[P|EARGS].

do_expand_args_l(_,A,A):-var(A),!.
do_expand_args_l(_,[],[]):-!.
do_expand_args_l(Exp,[A|RGS],[E|ARGS]):-do_expand_args(Exp,A,E),do_expand_args_l(Exp,RGS,ARGS).






:-decl_mpred(act_affect,3).


split_name_type(T,T,C):-compound(T),functor(T,C,_),!.
split_name_type(T,T,C):-atom(T),atom_codes(T,AC),last(AC,LC),is_digit(LC),append(Type,Digits,AC),catch(number_codes(_,Digits),_,fail),atom_codes(C,Type),!.
split_name_type(C,P,C):-atom(C),C==food,gensym(C,P),!.
split_name_type(C,P,C):-atom(C),trace,gensym(C,P),!.
split_name_type(C,P,C):-string(C),trace,gensym(C,P),!.

same(X,Y):- X=Y,!.
same(X,Y):- compound(X),arg(1,X,Y),!.
same(X,Y):- compound(Y),arg(1,Y,X),!.
same(X,Y):- samef(X,Y).

samef(X,Y):- X=Y,!.
samef(X,Y):- hotrace(((functor_safe(X,XF,_),functor_safe(Y,YF,_),string_equal_ci(XF,YF)))).

:-decl_mpred( type_max_charge/2).

define_subtype(O,T):- define_type(O),define_type(T),add(moo:subclass(O,T)).

:-decl_mpred( ft_info/2).
:-decl_mpred( subft/2).
:-decl_mpred( subclass/2).
:-decl_mpred( isa/2).

is_ft(S):-   holds_t(ft_info,S,_).
is_ft(S):-   holds_t(subft,S,_).
is_ft(S):-   holds_t(subclass,S,formattype).
is_ft(S):-   holds_t(isa,S,formattype).



:-export((
          samef/2,
          same/2,
          term_is_ft/2,
          is_ft/1,
          correctFormatType/4,
          any_to_value/2,
          any_to_number/2,
          atom_to_value/2,
          any_to_dir/2)).

expand_goal_correct_argIsa(A,B):-expand_goal(A,B).

isa_type(Type):-req(isa(Type,type)).

db_op_simpler(_,KB:Term,Term):-is_kb_module(KB).
db_op_simpler(_,KB:Term,Term):-dbase_mod(KB).
db_op_simpler(ask(_),MODULE:C0,holds_tcall(MODULE:C0)):- atom(MODULE), nonvar(C0),not(not(predicate_property(C0,_PP))),!. % , functor(C0,F,A), dmsg(todo(unmodulize(F/A))), %throw(module_form(MODULE:C0)),
                                                                             %    db_op(Op,C0).
db_op_simpler(_,C0,C1):- C0=..[svo,Obj,Prop|ARGS],!,C1=..[p,Prop,Obj|ARGS],!.
db_op_simpler(_,DBASE_T,DBASE):- DBASE_T=..[HOLDS,P,A|ARGS],atom(P),is_holds_true(HOLDS),DBASE=..[P,A|ARGS].

db_op_simpler(_,TypeTerm,props(Inst,[isa(Type),PROP1|PROPS])):-TypeTerm=..[Type,Inst,[PROP1|PROPS]],nonvar(PROP1),isa_type(Type),!.
db_op_simpler(_,TypeTerm,props(Inst,[isa(Type),PROP1|PROPS])):-TypeTerm=..[Type,Inst,PROP1|PROPS],isa_type(Type).

db_op_sentence(_Op,Prop,ARGS,C0):- must(atom(Prop)), C0=..[Prop|ARGS].


% ================================================
% db_tell_isa/2
% ================================================

db_tell_isa(T,type):-!, define_type(T).
db_tell_isa(type,type):-!.
db_tell_isa(_,SubType):- holds_t(subclass,SubType,formattype),!.
db_tell_isa(_,SubType):- holds_t(isa,SubType,formattype),!.
db_tell_isa(Term,mpred):-!,decl_mpred(Term).
db_tell_isa(A,SubType):- holds_t(subclass,SubType,Type),db_op(tell(_OldV),isa(A,Type)),fail.
% skip formatter types
db_tell_isa(_A,SubType):-member(SubType,[string,action,dir]),!.
db_tell_isa(_A,Fmt):-nonvar(Fmt), is_ft(Fmt),!,dmsg(todo(dont_assert_is_ft(Fmt))),!.
% one of 4 special types
db_tell_isa(Arg,SubType):-atom(SubType),is_creatable_type(SubType), call_after_game_load((world:create_instance(Arg,SubType,[]))),fail.
% sublass of 4 special types
db_tell_isa(Arg,SubType):- is_creatable_type(Type),holds_t(subclass,SubType,Type),call_after_game_load((world:create_instance(Arg,Type,[isa(SubType)]))),fail.


get_isa(A,Fmt):-get_isa_asserted(A,Fmt).
get_isa_asserted(A,Fmt):- nonvar(Fmt),is_ft(Fmt),!,correctFormatType(ask(once),A,Fmt,AA),A==AA,!.
get_isa_asserted(A,Fmt):- call_t(dac(d,a,no_c,no_mt),isa,A,Fmt).


:-decl_mpred(forwardRule/2).
:-decl_mpred(equivRule/2).

equivRule_call(A,B):-holds_t(equivRule,A,B).
equivRule_call(A,B):-holds_t(equivRule,B,A).
forwardRule_call(A,B):-holds_t(forwardRule,B,A).

call_must(must,Call):-!,must(Call).
call_must(Must,Call):-var(Must),!,Call.
call_must(once,Call):-!,once(Call).
call_must(!,Call):-!,once(Call).
call_must(_,Call):-call(Call).
% ================================================
% db_op/2
% ================================================
add_from_file(B,_):- contains_singletons(B),dmsg(todo(add_from_file_contains_singletons(B))),!,fail.
add_from_file(B,B):- db_op(tell(_OldV),B),!.

db_op(Op,Term):-notrace(do_all_of(dbase_module_loaded)),db_op00(Op,Term),notrace((do_all_of(dbase_module_loaded))).

db_op00(Op,isa(Term,Var)):-var(Var),!,db_op0(Op,get_isa(Term,Var)).

db_op00(ask(Must),isa(T,type)):-!,call_must(Must,defined_type(T)).
db_op00(tell(isa(T,type)),isa(T,type)):-!,db_tell_isa(T,type).

db_op00(tell(OldV),B):- !,loop_check(db_op0(tell(OldV),B),true). % true = we are already processing this assert
db_op00(ask(Must),createableType(SubType)):-!, call_must(Must,is_creatable_type(SubType)).
db_op00(ask(Must),Term):-!,loop_check(db_op0(ask(Must),Term),db_query_lc(Must,Term)).
db_op00(Op,Term):-!,loop_check_throw(db_op0(Op,Term)).

db_op0(Op,Term):-not(compound(Term)),!,throw_safe(nc(db_op0(Op,Term))).
db_op0(Op,[holds_t,P|Args]):-atom(P),!,Goal=..[P|Args],db_op0(Op,Goal).
db_op0(Op,KB:Term):-is_kb_module(KB),!,db_op(Op,Term).
db_op0(Op,KB:Term):-dbase_mod(KB),!,db_op(Op,Term).
db_op0(Op,(':-'(A))):- expand_goal_correct_argIsa(A,AA),goals_different(A,AA),!,db_op(Op, (':-'(A))).
db_op0(tell(AA),(':-'(A))):- expand_goal_correct_argIsa(A,AA),!, hotrace(AA).
db_op0(Op,(C1,C2)):-!,db_op(Op,C1),db_op(Op,C2).
db_op0(retract,(C1;C2)):-!,trace,once((db_op(retract,C1),db_op(retract,C2))).
db_op0(ra,(C1;C2)):-!,must(db_op(ra,C1)),must(db_op(ra,C2)).
db_op0(tell(OldV),(C1;C2)):-!,db_op(tell(OldV),C1),!,db_op(tell(OldV),C2),!.
db_op0(ask(_Must),props(Obj,Props)):-var(Props),!,findall(Prop,(dbase_t([P,Obj|REST]),Prop=..[P|REST]),Props).
db_op0(Op,props(Obj,Open)):-var(Open),!,throw_safe(db_op0(Op,props(Obj,Open))).
db_op0(_Op,props(_Obj,[])):-!.
db_op0(Op,props(Obj,[P])):- nonvar(P),!,db_op(Op,props(Obj,P)).
db_op0(Op,props(Obj,[P|ROPS])):-!,db_op(Op,props(Obj,P)),db_op(Op,props(Obj,ROPS)).
db_op0(Op,props(Obj,PropVal)):-PropVal=..[Prop,NonVar|Val],Obj==NonVar,!,db_op0(Op,[holds_t,Prop,Obj|Val]).
db_op0(Op,props(Obj,PropVal)):-PropVal=..[OP,Pred|Val],comparitiveOp(OP),not(comparitiveOp(Pred)),!,OPVAL=..[OP|Val],PropVal2=..[Pred,OPVAL],db_op0(Op,props(Obj,PropVal2)).
db_op0(Op,props(Obj,PropVal)):-PropVal=..[Prop|Val],not(infix_op(Prop,_)),!,db_op0(Op,[holds_t,Prop,Obj|Val]).
db_op0(Op,props(Obj,PropVal)):-PropVal=..[Prop|Val],!,grtrace,db_op0(Op,[holds_t,Prop,Obj|Val]).
db_op0(Op,expand_args(Exp,Term)):-!,forall(do_expand_args(Exp,Term,O),db_op0(Op,O)).
db_op0(Op,somethingIsa(A,List)):-!,forall_member(E,List,db_op(Op, isa(A,E))).
db_op0(Op,somethingDescription(A,List)):-!,forall_member(E,List,db_op(Op, description(A,E))).
db_op0(Op,objects(Type,List)):-!,forall_member(I,List,db_op(Op,isa(I,Type))).
db_op0(Op,sorts(Type,List)):-!,forall_member(I,List,db_op(Op, subclass(I,Type))).
db_op0(Op,predicates(List)):-!,forall_member(T,List,db_op(Op,mpred(T))).
db_op0(Op,db_op_exact(Term)):-!,db_op_exact(Op,Term).
db_op0(Op,EACH):-EACH=..[each|List],forall_member(T,List,db_op(Op,T)).
db_op0(tell(_),description(A,E)):-!,must(once(assert_description(A,E))).
db_op0(Op,nameString(A,S0)):- determinerRemoved(S0,String,S),!,db_op(Op, nameString(A,S)),db_op(tell(_OldV), determinerString(A,String)).

db_op0(Op,Term):- notrace((equivRule_call(Term,NewTerm),not(contains_singletons(NewTerm)))),db_op(Op,NewTerm).
db_op0(Op,Term):-  notrace((forwardRule_call(Term,NewTerm),not(contains_singletons(NewTerm)))),db_op(Op,NewTerm).


db_op0(tell(_OldV),singleValued(Term)):-!,add(mpred(Term)),add_mpred_prop(Term,singleValued).
db_op0(tell(OldV),multiValued(Term)):-!,functor_safe(Term,_F,A),db_op(tell(OldV),mpred(Term,multi(A))).
db_op0(tell(_OldV),isa(A,mpred)):- decl_mpred(A),!.

db_op0(ask(Must),get_isa(Term,Var)):-!,call_must(Must,get_isa_asserted(Term,Var)).
db_op0(ask(Must),isa(Term,Var)):-!,call_must(Must,get_isa(Term,Var)).

db_op0(Op,isa(A,SubType)):- holds_t(createableSubclassType,SubType,Type),db_op(Op,isa(A,Type)),db_op(Op,isa(A,SubType)).
db_op0(Op,Wild):-db_op_simpler(Op,Wild,Simpler),not(is_loop_checked(db_op0(Op,Simpler))),!,db_op(Op,Simpler).
db_op0(Op,C):- C=..[SubType,Arg],!,db_op(Op,isa(Arg,SubType)).


% some type to learn
db_op0(_Op,isa(_A,SubType)):- atom(SubType),
         define_type(SubType),
         dmsg(todo(ensure_creatable(SubType))),
         fail.

db_op0(retract,Term):-!,db_op(ask(once),Term),!,db_op(ra,Term).
db_op0(ask(Must),argIsa(P,N,T)):-db_op0(ask(Must),arity(P,A)),functor(ArgsIsa,P,A),req(argsIsa(ArgsIsa)),arg(N,ArgsIsa,T),must(nonvar(T)).

db_op0(Op,A):-must(once(correctArgsIsa(Op,A,AA))),goals_different(A,AA),!,must(once(db_op(Op,AA))),!.

db_op0(Op,C0):- !, C0=..[Prop|ARGS],db_op_unit(Op,C0,Prop,ARGS).

% ================================================
% db_op_unit/3
% ================================================

% impl/1
db_op_unit(Op,_C0,Prop,ARGS):-get_mpred_prop(Prop,impl(Other)),!,grtrace,must(atom(Other)),
        db_op_sentence(Op,Other,ARGS,Unit),!,db_op_exact(Op,Unit).

% alias/1
db_op_unit(Op,_C0,Prop,ARGS):-get_mpred_prop(Prop,alias(Other)),!,grtrace,must(atom(Other)),
	db_op_sentence(Op,Other,ARGS,Unit),!,db_op_exact(Op,Unit).

% inverse/1
db_op_unit(Op,_C0,Prop,ARGS):-
      get_mpred_prop(Prop,inverse(Other)),!,grtrace,must(atom(Other)),
      inverse_args(ARGS,Inverse),
      db_op_sentence(Op,Other,Inverse,Unit1),
      db_op_sentence(Op,Prop,ARGS,Unit2),
      db_op_exact(Op,(Unit1;Unit2)).

% assert_with_pred/1
db_op_unit(tell(_),C0,Prop,_RGS):- get_mpred_prop(Prop,assert_with_pred(How)),!,grtrace,must(atom(How)), call(run_database_hooks(assert(z)),C0), call(How,C0).

db_op_unit(Op,_C0,Prop,ARGS):-get_mpred_prop(Prop,assert_with_pred(How)),!,grtrace,must(atom(How)), throw(cant(db_op_unit(Op,Prop,ARGS),get_mpred_prop(Prop,assert_with_pred(How)))).

% plain prop
db_op_unit(Op,_C0,Prop,ARGS):- must((db_op_sentence(Op,Prop,ARGS,Unit),same_vars(ARGS,Unit))),db_op_exact(Op,Unit).

% ================================================
% db_op_exact/2
% ================================================

db_op_exact(tell(OldV),W):-non_assertable(W,Why),dumpST,trace,throw_safe(todo(db_op(tell(OldV), non_assertable(Why,W)))).
db_op_exact(ask(Must), Term):- !,db_query(Must,Term).
db_op_exact(query, Term):- !,db_query(findall,Term).
db_op_exact(u,C):- grtrace,db_quf(C,U,Template),U,Template,must(ground(Template)),!,ignore(hooked_retractall(Template)).
db_op_exact(ra,C):-db_quf(C,U,Template), doall((U,hooked_retractall(Template))).
db_op_exact(ra,C):- ignore(hooked_retractall(C)).
db_op_exact(tell(_OldVIn),C0):- db_quf(C0,U,C),must(U),functor(C,F,A),( get_mpred_prop(F/A,singleValued) -> must(db_assert_sv(C,F,A,_OldVOut1)) ; must(db_assert_mv(C,F,A,_OldVOut2))).
db_op_exact(tell(_OldVIn),C):- functor(C,F,A),!, functor(FA,F,A), must((get_mpred_prop(FA,singleValued) -> must(db_assert_sv(C,F,A,_OldVOut1)) ; must(db_assert_mv(C,F,A,_OldVOut2)))).
db_op_exact(retract,C):- ground(C),hooked_retractall(C),!.
db_op_exact(Op,C):-!,grtrace,throw(unhandled(db_op_exact(Op,C))).

% ================================================
% db_query/1
% ================================================
:-decl_mpred(needs_look/2).

db_query(Must,LC):-loop_check_throw(db_query_lc(Must,LC)).


db_query_lc(Must,(C0->C1;C2)):- !, (db_query(sf(!,!),C0) -> db_query(Must,C1) ; db_query(Must,C2)).
db_query_lc(Must,(C1;C2)):-!, db_query(Must,C1) ; db_query(Must,C2).
db_query_lc(Must,Term):-findall(a(Term),db_query0(Must,Term),List),list_to_set_safe(List,Set),!,member(a(Term),Set).

db_query0(Must,Term):- expand_goal(Term,Term2),!,call_must(Must,db_query1(Term2)).
% db_query1(C):- not(not(predicate_property(C,_PP))),!,call(C).
db_query1(C):- db_quf(C,Pretest,Template),!,call_expanded(Pretest), call_expanded(Template).

call_expanded(Goal):-expand_goal(Goal,Call),call(Call).

singletons_throw_or_fail(C):- contains_singletons(C),grtrace,throw(contains_singletons(C)).
nonground_throw_or_fail(C):- throw_if_true_else_fail(not(ground(C)),C).

% ================================================
% db_assert/1
% ================================================

% assert to tell(OldV) mutlivalue pred
db_assert_mv(C,F,A,OldV):- functor(FA,F,A),
   (clause_present(C,F,A) -> OldV=C; (get_mpred_prop(FA,ordered)-> hooked_assertz(C) ; hooked_asserta(C))).

% assert to tell(OldV) singlevalue pred
db_assert_sv(C,F,A,OldV):- throw_if_true_else_fail(contains_singletons(C),db_assert_sv(C,F,A,OldV)).
db_assert_sv(C,_,A,OldV):-   
   arg(A,C,Update),
   db_assert_sv_old_value(C,A,OLD,OldV),
   db_assert_sv_update(C,A,OLD,Update),!.

db_assert_sv_update(C,A,OLD,Update):-
   update_value(OLD,Update,NEW),
   nonvar(NEW),
   replace_arg(C,A,NEW,CC),
   hooked_asserta(CC),!.

db_assert_sv_old_value(C,A,OLD,CC):- must(var(OLD)),
   replace_arg(C,A,OLD,CC),
      % this binds or leave OLD
      ((req(CC) -> hooked_retract(CC) ; OLD= _ )),!.


replace_arg(C,A,OLD,CC):-
   C=..FARGS,
   replace_nth(FARGS,A,OLD,FARGO),
   CC=..FARGO.

replace_nth([],_,_,[]):-!.
replace_nth([_|ARGO],0,OLD,[OLD|ARGO]):-!.
replace_nth([T|FARGS],A,OLD,[T|FARGO]):-
    A2 is A-1,replace_nth(FARGS,A2,OLD,FARGO).

% ================================================
% hooked_assert*/1 hooked_retract*/1
% ================================================

% only place ever should actual game dbase be changed from
hooked_asserta(C):- singletons_throw_or_fail(hooked_asserta(C)).
hooked_asserta(C):- moodb:run_database_hooks(assert(tell(_OldV)),C),asserta_cloc(C).
hooked_assertz(C):- singletons_throw_or_fail(hooked_assertz(C)).
hooked_assertz(C):- run_database_hooks(assert(z),C), assertz_cloc(C).
hooked_retract(C):- nonground_throw_or_fail(hooked_retract(C)).
hooked_retract(C):- run_database_hooks(retract(one),C), must(retract_cloc(C)).
hooked_retractall(C):- run_database_hooks(retract(all),C), retractall_cloc(C).

ensure_db_predicate(Op,In,Out):-ensure_db_predicate_1(Op,In,Out),!.
ensure_db_predicate(Op,In,Out):-throw(ensure_db_predicate(Op,In,Out)).

ensure_db_predicate_1(AR,M:C,G):- nonvar(M), !,ensure_db_predicate_1(AR,C,G).
ensure_db_predicate_1(AR,CI,G):- force_head_expansion(CI,C),ensure_db_predicate_2(AR,C,G).

ensure_db_predicate_2(AR,M:C,G):- nonvar(M),ensure_db_predicate_2(AR,C,G).
ensure_db_predicate_2(_AR,CI,G):- force_head_expansion(CI,EG),expand_term(EG,G).

% ensure_db_predicate(tell(OldV),agent('NpcCol1000-Geordi684'),Out).

retract_cloc(C):- ensure_db_predicate(retract,C,G), show_cgoal(retract(G)).
retractall_cloc(C):- ensure_db_predicate(retract,C,G), show_cgoal(retractall(G)).
asserta_cloc(C):- ensure_db_predicate(tell(_OldV),C,G), show_cgoal(asserta_new(G)).
assertz_cloc(C):- ensure_db_predicate(tell(_OldV),C,G), debugOnError(show_cgoal(assertz_if_new(G))).

show_cgoal(G):- !, G.
show_cgoal(G):-dmsg(show_goal(G)),call(G).


% ================================================
% clause_present/1
% ================================================

:-thread_local clause_present_lookup_local/3.

clause_present(C):-hotrace((functor_safe(C,F,A),clause_present(C,F,A))).
clause_present(C,F,1):-C=..[F,A], is_ft(F), correctFormatType(ask(findall),A,F,_).
clause_present(C,_F,_A):- not(predicate_property(C,_)),!,fail.
clause_present(C,_F,_A):- not(ground(C)),!,fail.
clause_present(C,F,A):- clause_present_lookup_local(C,F,A),!,fail.
clause_present(C,_,1):- !, clause(C,true).
clause_present(C,F,A):- with_assertions(clause_present_lookup_local(C,F,A),clause_present_1(C,F,A)).

clause_present_1(C,_,_):- catch(C,_,false),!.
clause_present_1(C,_F,_A):-predicate_property(C,foreign),!,throw(predicate_property(C,foreign)),!,fail.
clause_present_1(C,_F,_A):-clause(C,true),!.
clause_present_1(C0,_F,A):- A>1, arg(A,C0,NEW),string(NEW),!,copy_term(C0,C),
   setarg(A,C,OLD),C,string_chars(NEW,[S|C1]),string_chars(OLD,[S|C2]),C1=C2,trace,dmsg(present(C)).

/*

clause_present_1(C,F,A):- A>1, arg(A,C,NEW),snonvar(NEW),!,setarg(A,C,OLD),clause_present(C,F,A),pl_arg_type(NEW,string),string_chars(NEW,[S|C1]),string_chars(OLD,[S|C2]),C1=C2,dmsg(present(C)).

*/
pl_arg_type(Arg,Type):-
      var(Arg) -> Type =var;
      integer(Arg) -> Type =integer;
      number(Arg) -> Type =float;
      string(Arg) -> Type =string;
      atom(Arg) -> Type =atom;
      is_list(Arg) -> Type =list;
      compound(Arg) -> Type =compound;
         Arg = Type.



compare_n(NewLast,Last):-NewLast=Last,!.
compare_n(NewLast,Last):-NewLast==unknown,!,NewLast==Last.
compare_n(NewLast,Last):-number(NewLast),not(number(Last)),throw(incomparable_terms(Last,NewLast)).
compare_n(Last,NewLast):-number(NewLast),not(number(Last)),throw(incomparable_terms(Last,NewLast)).
compare_n(NewLast,Last):-atomic(NewLast),not(atomic(Last)),throw(incomparable_terms(Last,NewLast)).
compare_n(Last,NewLast):-atomic(NewLast),not(atomic(Last)),throw(incomparable_terms(Last,NewLast)).


member_or_e(E,[L|List]):-!,member(E,[L|List]).
member_or_e(E,E).

is_single_valuedOrFail(F,A,Obj,ARGS):- get_mpred_prop(F/A,singleValued),!,valuedOrThrow(F,A,Obj,ARGS),!.
is_single_valuedOrFail(_,_,_,_):-fail.

valuedOrThrow(F,_,Obj,ARGS):- holds_t(isa,Obj,T), findall_type_default_props(Obj,T,Props),Props=[_|_],Prop=..[F|ARGS], member_or_e(Prop,Props),!.
valuedOrThrow(F,A,Obj,ARGS):-valuedOrThrow1(F,A,Obj,ARGS).
valuedOrThrow1(_F,_A,_Obj,ARGS):-last(ARGS,unknown),!.
valuedOrThrow1(F,A,Obj,ARGS):-throw(is_single_valuedOrFail(F,A,Obj,ARGS)).


findall_type_default_props(Inst,Type,TraitsO):-findall(Props,holds_t(type_default_props,Inst,Type,Props),Traits),flatten(Traits,TraitsM),!,subst(TraitsM,Inst,Inst,TraitsO).


replace_nth([],_N,_OldVar,_NewVar,[]):-!,throw(missed_the_boat).
replace_nth([OldVar|ARGS],1,OldVar,NewVar,[NewVar|ARGS]):-!.
replace_nth([Carry|ARGS],Which,OldVar,NewVar,[Carry|NEWARGS]):-
 Which1 is Which-1,
 replace_nth(ARGS,Which1,OldVar,NewVar,NEWARGS),!.

update_value(OLD,+X,NEW):-number(OLD),catch(NEW is OLD + X,_,fail),!.
update_value(OLD,-X,NEW):-number(OLD),catch(NEW is OLD - X,_,fail),!.
update_value(OLD,NEW,NEXT):-var(NEW),!,throw(logicmoo_bug(update_value(OLD,NEW,NEXT))).
update_value(_OLD,NEW,NEW).

insert_into(ARGS,0,Insert,[Insert|ARGS]):- !.
insert_into([Carry|ARGS],After,Insert,[Carry|NEWARGS]):-
   After1 is After - 1,
   insert_into(ARGS,After1,Insert,NEWARGS).


is_mpred_prolog(F,A):-get_mpred_prop(F/A,ask_module(_)), 
   not(get_mpred_prop(F/A,query(_))),!.

:-include(dbase_formattypes).

:-include(dbase_c_term_expansion).

:- catch(noguitracer,_,true).

% load_motel:- defrole([],time_state,restr(time,period)).

:-dynamic(call_tabled_list/2).

make_key(CC,Key):-copy_term(CC,Key),numbervars(Key,'$VAR',0,_),!.

expire_tabled_list(T):-CT= call_tabled_list(Key,List),doall(((CT,any_term_overlap(T,Key:List),retract(CT)))).

any_term_overlap(T1,T2):-atoms_of(T1,A1),atoms_of(T2,A2),!,member(A,A1),member(A,A2),!.

:-meta_predicate call_tabled(0).
:-module_transparent call_tabled/1.

call_tabled(findall(A,B,C)):-!,findall_tabled(A,B,C).
call_tabled(C):-copy_term(C,CC),numbervars(CC,'$VAR',0,_),call_tabled(C,C).
call_tabled(CC,C):-make_key(CC,Key),call_tabled0(Key,C,C,List),!,member(C,List).
call_tabled0(Key,_,_,List):-call_tabled_list(Key,List),!.
call_tabled0(Key,E,C,List):-findall(E,C,List1),list_to_set(List1,List),asserta_if_ground(call_tabled_list(Key,List)),!.

findall_tabled(Result,C,List):-make_key(Result^C,RKey),findall_tabled4(Result,C,RKey,List).
findall_tabled4(_,_,RKey,List):-call_tabled_list(RKey,List),!.
findall_tabled4(Result,C,RKey,List):-findall(Result,call_tabled(C),RList),list_to_set(RList,List),asserta_if_ground(call_tabled_list(RKey,List)).

asserta_if_ground(_):-!.
asserta_if_ground(G):-ground(G),asserta(G),!.
asserta_if_ground(_).

call_no_cuts(CALL):-clause(CALL,TEST),call_no_cuts_0(TEST).

call_no_cuts_0(true):-!.
call_no_cuts_0((!)):-!.
call_no_cuts_0((A,B)):-!,call_no_cuts_0(A),call_no_cuts_0(B).
call_no_cuts_0(CALL):-catch(clause(CALL,_),_,fail),!,clause(CALL,TEST),call_no_cuts_0(TEST).
call_no_cuts_0(C):-call(C).


:- meta_predicate compare_op(*,2,?,?).
:- meta_predicate holds_tcall(^).

:- meta_predicate call_tabled0(?,?,?,?).
:- meta_predicate call_no_cuts_0(?).
:- meta_predicate call_tabled(?).
:- meta_predicate findall_tabled4(?,?,?,?).


:- decl_mpred(mudMaxHitPoints(agent,int)).

% :- load_motel.

% :-include(moo_loadall).


:- include(logicmoo(vworld/moo_loader)).
:-export((          
          finish_processing_game/0, 
          correctType/4,
          gload/0,
          correctArgsIsa/3,
          load_game/1)).

:- meta_predicate show_call(0).
:- meta_predicate show_call0(0).


user:goal_expansion(G1,G3):- notrace((compound(G1), do_term_expansions)), once(try_mud_body_expansion(G1,G2)),goals_different(G1,G2),G2=G3.

user:term_expansion(G1,G3):- notrace((compound(G1), do_term_expansions)),  once(attempt_clause_expansion(G1,G2)),goals_different(G1,G2),G2=G3.


:- decl_mpred(ft_info,2).
:- decl_mpred(subft,2).


load_game(File):-absolute_file_name(File,Path),see(Path),
   world_clear(current),
   repeat,
   asserta(moodb:loading_game_file(File)),
   read_term(Term,[double_quotes(string)]),
   game_assert(Term),
   Term == end_of_file,seen,!,
   retractall(moodb:loading_game_file(File)),
   assert_if_new(moodb:loaded_game_file(File)),
   must(finish_processing_game).

game_assert(end_of_file):-!.
game_assert(Term):-once(must(add(Term))).

:-meta_predicate(call_after_game_load(+)).

:-dynamic(in_finish_processing_game/0).

finish_processing_game:- debug,in_finish_processing_game,!.
finish_processing_game:- assert(in_finish_processing_game),fail.
finish_processing_game:- do_all_of(dbase_module_loaded).
finish_processing_game:- retract(moodb:call_after_load(A)),once(must(A)),fail.
finish_processing_game:- call_after(moodb:not_loading_game_file,dbase:savedb),fail.
finish_processing_game:- retractall(in_finish_processing_game).
finish_processing_game.




% gload:- load_game(savedb),!.
gload:- load_game(logicmoo('rooms/startrek.all.pl')).

% savedb:-!.
savedb:-
 ignoreOnError((
   moodb:dbase_mod(DBM),
   make_directory('/tmp/lm/'),
   tell('/tmp/lm/savedb'),listing(DBM:dbase_t),told)).

/*
is_ft_except(S,List):-
   moo:ft_info(S,_);
   not((member(S,List), 
      ((get_subft(S,S2) ,
        is_ft_except(S2,[S|List]) ;
             ((moo:subft(S3,S) , is_ft_except(S3,[S,S2|List]))))))).
*/



p2c_dir2('s','South-Directly').
p2c_dir2('w','West-Directly').
p2c_dir2('u','Up-Directly').
p2c_dir2('d','Down-Directly').
p2c_dir2('e','East-Directly').
p2c_dir2('n','North-Directly').


moo:ft_info(atom,atom(self)).
moo:ft_info(apath(region,dir),formatted).
moo:ft_info(string,string(self)).
moo:ft_info(number,number(self)).
moo:ft_info(type,isa(self,type)).
moo:ft_info(dir,any_to_dir(self,_)).
moo:ft_info(dice(int,int,int),formatted).
moo:ft_info(xyz(region,int,int,int),formatted).
moo:ft_info(list(type),formatted).
moo:ft_info(term,nonvar(self)).
moo:ft_info(id,nonvar(self)).
moo:ft_info(prolog,true).
moo:ft_info(rest,true).
moo:ft_info(var,var(self)).
moo:ft_info(action(prolog),formatted).

moo:subft(var,prolog).
moo:subft(term,prolog).
moo:subft(atom,term).
moo:subft(string,term).
% moo:subft(number,term).
moo:subft(id,term).

moo:subft(int,integer).
moo:subft(integer,number).
moo:subft(dice,int).

moo:formattype(FormatType):-moo:subclass(FormatType,formattype).
moo:formattype(FormatType):-dbase:holds_t(isa, FormatType, formattype).

:-include(dbase_i_builtin).

:- begin_transform_moo_preds.
moo:database_hook(assert(_),C):-expire_tabled_list(C).
moo:database_hook(retract(_),C):-expire_tabled_list(C).
:- end_transform_moo_preds.



:- module_predicates_are_exported(dbase).
:- module_meta_predicates_are_transparent(dbase). 



/*
"Lieutenant",
"Commander",
"Human",
"Player",
"Explorer Player",
"ACT_NICE_THIEF",
"AWARE",
"NOBACKSTAB",
"ACT_STAY_ZONE",
"MEMORY",
"HELPER",
"ACT_FRIEND",
"NOCHARM",
"NOSUMMON",
"NOSLEEP",
"NOBASH",
"NOBLIND",
"NPC_DETECT_INVIS",
"NPC_NOTRACK",
"+mudToHitArmorClass0: 1",
"mudMaxHitPoints: 18d18+4000",
"#$PunchingSomething mudBareHandDamage: 10d10+75",
"Player",
"Player",
"Human",
"Logged on player character",
"burgandy",
"starfleet",
"command",
"uniform",
"a burgandy Starfleet command uniform",
"A neatly folded burgandy Starfleet command uniform is lying here",
"armorLevel: 10",
"These uniforms are worn by command officers on Federation starships. It's kind of tight, but it looks pretty good",
"Red Uniform",
"a burgandy Starfleet command uniform"

*/

