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
force_expand_goal/2,
force_expand_head/2,
argIsa_call/4,
assertThrough/1,
assertThrough/2,
balanceBinding/2,
clause_present_1/3, 
do_expand_args/3,
call_no_cuts/1,
call_no_cuts/2,
force_expand/1,
game_assert/1,
clr/1,
call_tabled/1,
cycAssert/1,
call_after_game_load/1,
cycAssert/2,
% db_query_lc/1,
cycInit/0,
cyclify/2,
record_on_thread/2,
cyclifyNew/2,
cycQuery/1,
cycQuery/2,
prolog_callable_expanded/1,
split_name_type/3,
cycRetract/1,
cycRetract/2,
cycRetractAll/1,
capturing_changes/2,
cycRetractAll/2,
cycStats/0,
non_assertable/1,
defaultMt/1,
db_op/2,
db_op0/2,
db_opp/2,
db_query/2,
del/1,
ensureMt/1,
findall_type_default_props/3,
finishCycConnection/3,
formatCyc/3,
getCycConnection/3,
getSurfaceFromChars/3,
holds_f/1,holds_f/2,holds_f/3,holds_f/4,holds_f/5,holds_f/6,holds_f/7,holds_f/8,
holds_t/1,holds_t/2,holds_t/3,holds_t/4,holds_t/5,holds_t/6,holds_t/7,holds_t/8,
call_expanded/1,
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
use_term_listing/3,
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
:- thread_local capturing_changes/2.
:- include(logicmoo(vworld/dbase_i_cyc)).

hook:decl_database_hook(AR,C):- record_on_thread(dbase_change,changing(AR,C)).
record_on_thread(Dbase_change,O):- thread_self(ID),capturing_changes(ID,Dbase_change),!,Z=..[Dbase_change,ID,O],assertz(Z).

:- decl_mpred(subft,2).

:- meta_predicate call_no_cuts(^).
:- meta_predicate call_no_cuts(+,^).


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

call_after_game_load(Code):- call_after(moo:not_loading_game_file,Code).

:- decl_mpred(act_affect,3).
:- decl_mpred( type_max_charge/2).
:- decl_mpred(isa/2).
:- decl_mpred(forwardRule/2).
:- decl_mpred(equivRule/2).
:- decl_mpred(subft(formattype,formattype)).
:- decl_mpred(subclass(type,type)).
:- decl_mpred(needs_look/2).
:- decl_mpred(mudMaxHitPoints(agent,int)).

:- decl_mpred((
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

:- moo:dbase_mod(DBASE), dynamic(DBASE:inside_clause_expansion/1).


:- moo:dbase_mod(DBASE), 
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

:- moo:dbase_mod(DBASE), 
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

:- moo:dbase_mod(DBASE), 
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

:- moo:dbase_mod(DBASE), 
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

:- meta_predicate clause_present(:), db_assert_mv(+,+,+,+,0), db_assert_sv(+,+,+,+,-), db_op_exact(+,+), db_quf(+,+,-,-).

%  argIsa_call/3, use_term_listing/2,world_clear/1.

:- dynamic db_prop_prolog/2.

% :- context_module(M),asserta(moo:dbase_mod(M)),dmsg(assert_if_new(moo:dbase_mod(M))).

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

:-include(dbase_i_coroutining).

/*
took(logicmoo_example1, success, 0.00019672599999998042).
took(logicmoo_example1_holds, success, 0.00019862799999992298).
took(logicmoo_example2, success, 0.00041965599999982395).
took(chang_lee_example1, success, 0.0003453970000002471).
took(chang_lee_example2, success, 25.950020179).
took(chang_lee_example3, success, 0.0001518709999999146).
took(chang_lee_example4, success, 0.00016846299999784264).
took(chang_lee_example5, success, 0.0004118619999999851).
took(chang_lee_example6, success, 20.269434864).
took(chang_lee_example7, success, 0.0005044489999974644).
XRay writing compiled clauses ... ERROR: assert/1: Cannot represent due to cyclic_term'

*/
%:- user_use_module(dbase_rules_xray).
:- user_use_module(dbase_rules_pttp).
/*
took(logicmoo_example1, success, 0.00020127899999988763).
took(logicmoo_example1_holds, success, 0.0001955599999998725).
took(logicmoo_example2, success, 0.000398248000000212).
took(logicmoo_example3_will_fail, failure, 0.490448118).
took(chang_lee_example1, success, 0.0007522150000001115).
took(chang_lee_example2, success, 1.1971676820000001).
took(chang_lee_example3, success, 0.0001870799999998951).
took(chang_lee_example4, success, 0.00019246799999983466).
took(chang_lee_example5, success, 0.0010531570000003043).
took(chang_lee_example6, success, 0.043870042).
took(chang_lee_example7, success, 0.0013306609999998997).
took(chang_lee_example8, success, 0.0003523059999999134).
took(chang_lee_example9, success, 0.3166319040000003).
*/


do_pttp_tests:-do_pttp_test(_ALL).
do_pttp_tests:-pttp_assert(test123).
do_pttp_tests:-pttp_query(test123).
% do_pttp_tests:-prolog.

% fast here
:-is_startup_file('run_debug.pl')->doall(do_pttp_tests);true.


callable_tf(P,2):- arity_pred(P),!,fail.
callable_tf(F,A):- functor_safe(P,F,A),predicate_property(P,_),!.
:- dynamic(useExternalDBs/0).
useExternalDBs:- fail.
useDBMts:- fail, useExternalDBs.

relax_term(P,P,Aic,Aic,Bic,Bic):- !.
/*
relax_term(P,P,A,A,Bi,Bc):- arg(_,v(subclass,isa),P),!,fail.
relax_term(P,P,Ai,Ac,Bic,Bic):- when_met(nonvar(Ac), same_arg(same_or(isa),Ac,Ai)),!.
relax_term(P,P,Ai,Ac,Bi,Bc):- is_type(Ai),!,when_met(pred(nonvar,Ac), (same_arg(same_or(subclass),Ac,Ai),same_arg(same_or(equals),Bc,Bi))),!.
relax_term(P,P,Ai,Ac,Bi,Bc):- when_met(pred(nonvar,Ac),when_met(pred(nonvar,Bc), (same_arg(same_or(subclass),Ac,Ai),same_arg(same_or(equals),Bc,Bi)))).
*/

% ?- member(R,[a,b,c]),when_met(nonvar(Re), dbase:same_arg(same_or(termOfUnit),n,Re)),Re=R,write(chose(R)).


% ================================================================================
% begin holds_t
% ================================================================================
holds_t(P,A1,A2,A3,A4,A5,A6,A7):- req(holds_t(P,A1,A2,A3,A4,A5,A6,A7)).
holds_t(P,A1,A2,A3,A4,A5,A6):- req(holds_t(P,A1,A2,A3,A4,A5,A6)).
holds_t(P,A1,A2,A3,A4,A5):- req(holds_t(P,A1,A2,A3,A4,A5)).
holds_t(P,A1,A2,A3,A4):- req(holds_t(P,A1,A2,A3,A4)).
holds_t(P,A1,A2,A3):- req(holds_t(P,A1,A2,A3)).
holds_t(P,A1,A2):- req(holds_t(P,A1,A2)).
holds_t(P,A1):- req(holds_t(P,A1)).
holds_t(G):- req(G).


% ================================================================================
% begin cholds_t
% ================================================================================
which_t(dac(d,a,no_c,no_mt)).
cholds_t(P,A1,A2,A3,A4,A5,A6,A7):- isCycPredArity_ignoreable(P,7),which_t(DBS),(call_t(DBS,P,A1,A2,A3,A4,A5,A6,A7);call_mt_t(DBS,P,A1,A2,A3,A4,A5,A6,A7,_,_);assertion_t([P,A1,A2,A3,A4,A5,A6,A7])).
cholds_t(P,A1,A2,A3,A4,A5,A6):- isCycPredArity_ignoreable(P,6),which_t(DBS),(call_t(DBS,P,A1,A2,A3,A4,A5,A6);call_mt_t(DBS,P,A1,A2,A3,A4,A5,A6,_,_)).
cholds_t(P,A1,A2,A3,A4,A5):- isCycPredArity_ignoreable(P,5),which_t(DBS),(call_t(DBS,P,A1,A2,A3,A4,A5);call_mt_t(DBS,P,A1,A2,A3,A4,A5,_,_)).
cholds_t(P,A1,A2,A3,A4):- isCycPredArity_ignoreable(P,4),which_t(DBS),(call_t(DBS,P,A1,A2,A3,A4);call_mt_t(DBS,P,A1,A2,A3,A4,_,_)).
cholds_t(P,A1,A2,A3):- isCycPredArity_ignoreable(P,3),which_t(DBS),(call_t(DBS,P,A1,A2,A3);call_mt_t(DBS,P,A1,A2,A3,_,_)).
cholds_t(P,A1,A2):- hotrace(cholds_relaxed_t(P,A1,A2)).
cholds_t(P,A1):- !,req(isa(A1,P)).
cholds_t(P,A1):- isCycPredArity_ignoreable(P,1),which_t(DBS),(call_t(DBS,P,A1);call_mt_t(DBS,P,A1,_,_)).

% holds_relaxed_t(Arity, P, A):- Arity == arity,!,isCycPredArity(P,A).
%holds_relaxed_t(Mpred, FA, [Prop]):- Mpred==mpred,!,get_mpred_prop(FA,Prop).
cholds_relaxed_t(P,A1,A2):- isCycPredArity_ignoreable(P,2),which_t(DBS),!,relax_term(P,PR,A1,R1,A2,R2),cholds_relaxed_0_t(DBS,PR,R1,R2).
cholds_relaxed_0_t(DBS,P,A1,A2):- call_t(DBS,P,A1,A2).
cholds_relaxed_0_t(DBS,P,A1,A2):- call_mt_t(DBS,P,A1,A2,_,_).
cholds_relaxed_0_t(DBS,P,A1,A2):- ground((P,A1)), TEMPL=..[P,T1,_],dbase_t(default_sv,TEMPL,A2),req(isa(A1,T1)),!.


cholds_t([AH,P|LIST]):- is_holds_true(AH),!,cholds_t_p2(P,LIST).
cholds_t([AH,P|LIST]):- is_holds_false(AH),!,holds_f_p2(P,LIST).
cholds_t([P|LIST]):- !,cholds_t_p2(P,LIST).
cholds_t(CALL):- safe_univ(CALL,[P|LIST]),holds_t([P|LIST]).
cholds_t_p2(P,LIST):- safe_univ(CALL,[holds_t,P|LIST]),call(CALL).


dbase_t(isa, I , C):- dbase_t(C, I).

dbase_t(List):- is_list(List),!,Call=..[dbase_t|List],Call.
dbase_t(List):- holds_t(List).

call_list_t(dac(d,_,_,_),CALL,_):- dbase_t(CALL).
call_list_t(dac(_,a,_,_),_,List):- assertion_t(List).
call_list_t(dac(_,_,c,_),CALL,_):- xcall_t(CALL).

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

assertion_t([AH,P|LIST]):- is_holds_true(AH),!,assertion_t([P|LIST]).
assertion_t([AH,P|LIST]):- is_holds_false(AH),!,assertion_f([P|LIST]).
% todo hook into loaded files!
assertion_t([P|LIST]):- Call=..[asserted_dbase_t,P|LIST],Call.
assertion_t(_):- not(useExternalDBs),!,fail.
assertion_t([P|LIST]):- tiny_kb:'ASSERTION'(':TRUE-DEF',_,_UniversalVocabularyMt,_Vars,/*HL*/[P|LIST]).
assertion_t([P|LIST]):- tiny_kb:'ASSERTION'(':TRUE-MON',_,_UniversalVocabularyMt,_Vars,/*HL*/[P|LIST]).
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
holds_f(P,A1,A2,A3,A4,A5):- isCycPredArity_ignoreable(P,5),which_f(DBS),(call_f(DBS,P,A1,A2,A3,A4,A5);call_mt_f(DBS,P,A1,A2,A3,A4,A5,_,_)).
holds_f(P,A1,A2,A3,A4):- isCycPredArity_ignoreable(P,4),which_f(DBS),(call_f(DBS,P,A1,A2,A3,A4);call_mt_f(DBS,P,A1,A2,A3,A4,_,_)).
holds_f(P,A1,A2,A3):- isCycPredArity_ignoreable(P,3),which_f(DBS),(call_f(DBS,P,A1,A2,A3);call_mt_f(DBS,P,A1,A2,A3,_,_)).
holds_f(P,A1,A2):- holds_relaxed_f(P,A1,A2).
holds_f(P,A1):- isCycPredArity_ignoreable(P,1),which_f(DBS),(call_f(DBS,P,A1);call_mt_f(DBS,P,A1,_,_)).


holds_relaxed_f(P,A1,A2):- isCycPredArity_ignoreable(P,2),which_f(DBS),!,relax_term(P,PR,A1,R1,A2,R2),holds_relaxed_0_f(DBS,PR,R1,R2).
holds_relaxed_0_f(DBS,P,A1,A2):- call_f(DBS,P,A1,A2).
holds_relaxed_0_f(DBS,P,A1,A2):- call_mt_f(DBS,P,A1,A2,_,_).


holds_f([AH,P|LIST]):- is_holds_true(AH),!,holds_f_p2(P,LIST).
holds_f([AH,P|LIST]):- is_holds_false(AH),!,cholds_t_p2(P,LIST).
holds_f([P|LIST]):- !,cholds_t_p2(P,LIST).
holds_f(CALL):- CALL=..[P|LIST],holds_f([P|LIST]).
holds_f_p2(P,LIST):- CALL=..[holds_f,P|LIST],call(CALL).

dbase_f(List):- is_list(List),!,Call=..[dbase_t|List],Call.
dbase_f(List):- holds_f(List).


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

assertion_f([AH,P|LIST]):- is_holds_true(AH),!,assertion_f([P|LIST]).
assertion_f([AH,P|LIST]):- is_holds_false(AH),!,assertion_f([P|LIST]).
% todo hook into loaded files!
assertion_f(_):- not(useExternalDBs),!,fail.
assertion_f([P|LIST]):- tiny_kb:'ASSERTION'(':FALSE-DEF',_,_UniversalVocabularyMt,_Vars,/*HL*/[P|LIST]).
assertion_f([P|LIST]):- tiny_kb:'ASSERTION'(':FALSE-MON',_,_UniversalVocabularyMt,_Vars,/*HL*/[P|LIST]).


% ================================================================================
% end holds_f 
% ================================================================================


is_creatable_type(Type):- arg(_,p(agent,item,region,concept),SubType).
is_creatable_type(Type):- holds_t([isa,Type,creatable_type]).

arityMatches(A,S-E):- !, catch((system:between(S,E,OTHER),A=OTHER),_,(trace,system:between(S,E,OTHER),A=OTHER)).
arityMatches(A,OTHER):- number(OTHER),!,A=OTHER.

isCycPredArity_ignoreable(P,A):- hotrace(ignore(isCycPredArity(P,A))).

isCycPredArity_Check(P,A):- isCycPredArity(P,A),!.
% isCycPredArity_Check(P,A):- mpred_arity(P,A),!. % get_mpred_prop(P,AA,_),!,integer(AA),A=AA.

isCycPredArity(P,A):- loop_check_throw(isCycPredArity_lc(P,A)).
isCycPredArity_lc(_:P,A):- nonvar(P),!,isCycPredArity(P,A).
isCycPredArity_lc(P,A):- nonvar(P),!,isCycPredArity0(P,OTHER),!,arityMatches(A,OTHER).
isCycPredArity_lc(P,A):- isCycPredArity0(P,OTHER),arityMatches(A,OTHER).

arity_pred(P):- nonvar(P),arg(_,a(arity,arityMax,arityMin),P).

isCycPredArity0(P,A):- arity_pred(P),!,A=2.
isCycPredArity0(P,A):- isRegisteredCycPred(_,P,A).
isCycPredArity0(P,A):- xcall_t(holds_t(arity,P,A)).
isCycPredArity0(P,A):- xcall_t(holds_t(arityMax,P,A)).
isCycPredArity0(holds,7):- !.
isCycPredArity0(P,_):- is_2nd_order_holds(P),!,fail.
isCycPredArity0(F,A):- integer(A),A>0,is_type(F),!.
isCycPredArity0(P,A):- xcall_t(((holds_t(arityMin,P,A),not(holds_t(arityMax,P, _))))).


:- include(logicmoo(vworld/dbase_i_db_preds)).
:- include(logicmoo(vworld/dbase_i_pldoc)).

:- style_check(+discontiguous).
:- style_check(+singleton).


scan_arities:- forall(holds_t(arity,F,A),moo:decl_mpred(F,A)).

% logicmoo('vworld/dbase') compiled into dbase 11.97 sec, 2,140,309 clauses
%:- include(logicmoo('pldata/trans_header')).

process_mworld:- !. %forall(dynamicCyc2(C),moo:registerCycPredPlus2(C)).

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

user_export(_):- moo:dbase_mod(user),!.
user_export(_M:Prop/Arity):- !,user_export(Prop/Arity).
user_export(Prop/Arity):- 
   moo:dbase_mod(M), '@'( ((export(Prop/Arity),dynamic(Prop/Arity))) , M).


:- meta_predicate hooked_asserta(+), hooked_assertz(+), hooked_retract(+), hooked_retractall(+).

:- meta_predicate del(+),clr(+),add(+),req(+), db_op(+,+).

% Found new meta-predicates in iteration 1 (0.281 sec)
%:- meta_predicate db_op_exact(?,?,?,0).

% :- include(dbase_types_motel).

% :- moo:register_module_type(utility).

% oncely later will throw an error if there where choice points left over by call
oncely(Call):-once(Call).

:- dynamic(non_assertable/1).
non_assertable(WW,isVar):- var(WW),!.
non_assertable(_:WW,Why):- !,non_assertable(WW,Why).
non_assertable(WW,Why):- compound(WW),functor(WW,F,A),!,never_use_holds_db(F,A,Why),!.
% non_assertable(WW,Why):- db_prop_game_assert

% replaced the 1st with the 2nd and better version of retract
% del(C0):- db_op(retract,C0)
%% del(RetractOne)    <--  del(C0):- ignore((db_op(ask(Must),C0),!,db_op('retract',C0))).
del(C0):- (db_op(ask(once),C0) -> db_op('retract',C0) ; dmsg(failed(del(C0)))).
%% clr(Retractall)
clr(C0):- db_op(ra,C0).
%% req(Query)
req(C0):- db_op(ask(call),C0).
mreq(C0):- db_op(ask(must),C0).
%% props(Obj,QueryPropSpecs)
props(Obj,PropSpecs):- db_op(ask(query),props(Obj,PropSpecs)).
%% add(Assertion)
add(C0):- oncely(db_op(tell(_OldV), C0)).
%% padd(Obj,PropSpecs)
padd(Obj,PropSpecs):- add(props(Obj,PropSpecs)).
%% padd(Obj,Prop,Value)
padd(Obj,Prop,Value):- must(nonvar(Prop)), PropValue=..[Prop,Value],!,padd(Obj,PropValue).
%% prop(Obj,Prop,Value)
prop(Obj,Prop,Value):- holds_t(Prop,Obj,Value).
%% prop_or(Obj,Prop,Value,OrElse)
prop_or(Obj,Prop,Value,OrElse):- one_must(prop(Obj,Prop,Value),Value=OrElse).

% TODO: canonicalize clauses first!
with_kb_assertions([],Call):- !,Call.
with_kb_assertions([With|MORE],Call):- !,with_kb_assertions(With,with_kb_assertions(MORE,Call)).
with_kb_assertions(With,Call):- not(not(req(With))),!,Call,del(With).
with_kb_assertions(With,Call):- 
   kb_update(With,OldV),
   setup_call_cleanup(add(With),Call,kb_update(OldV,With)).

kb_update(New,OldV):- req(New),!,OldV=New.
kb_update(New,OldV):- db_op(tell(OldV),New).

world_clear(Named):- dmsg('Clearing world database: ~q.',[Named]).


do_expand_args(Exp,Term,Out):- compound(Term),!,do_expand_args_c(Exp,Term,Out).
do_expand_args(_,Term,Term).

do_expand_args_c(Exp,[L|IST],Out):- !,do_expand_args_l(Exp,[L|IST],Out).
do_expand_args_c(Exp,Term,Out):- Term=..[P|ARGS],do_expand_args_pa(Exp,P,ARGS,Out).

do_expand_args_pa(Exp,Exp,ARGS,Out):- !,member(Out,ARGS).
do_expand_args_pa(Exp,P,ARGS,Out):- do_expand_args_l(Exp,ARGS,EARGS), Out=..[P|EARGS].

do_expand_args_l(_,A,A):- var(A),!.
do_expand_args_l(_,[],[]):- !.
do_expand_args_l(Exp,[A|RGS],[E|ARGS]):- do_expand_args(Exp,A,E),do_expand_args_l(Exp,RGS,ARGS).







split_name_type(T,T,C):- compound(T),functor(T,C,_),!.
split_name_type(T,T,C):- atom(T),atom_codes(T,AC),last(AC,LC),is_digit(LC),append(Type,Digits,AC),catch(number_codes(_,Digits),_,fail),atom_codes(C,Type),!.
split_name_type(C,P,C):- atom(C),C==food,gensym(C,P),!.
split_name_type(C,P,C):- atom(C),trace,gensym(C,P),!.
split_name_type(C,P,C):- string(C),trace,gensym(C,P),!.

is_ft(formattype).
is_ft(S):-  moo:ft_info(S,_).
is_ft(S):-  dbase_t(subft,S,_).
is_ft(S):-  dbase_t(subclass,S,formattype).
is_ft(S):-  holds_t(isa,S,formattype).


:- export((
          samef/2,
          same/2,
          term_is_ft/2,
          is_ft/1,
          correctFormatType/4,
          any_to_value/2,
          any_to_number/2,
          atom_to_value/2,
          any_to_dir/2)).

expand_goal_correct_argIsa(A,B):- force_expand_goal(A,B).

% db_op_simpler(ask(_),MODULE:C0,call_expanded(call,MODULE:C0)):- atom(MODULE), nonvar(C0),not(not(predicate_property(C0,_PP))),!. % , functor(C0,F,A), dmsg(todo(unmodulize(F/A))), %trace_or_throw(module_form(MODULE:C0)), %   db_op(Op,C0).
db_op_simpler(_,TypeTerm,props(Inst,[isa(Type)|PROPS])):- TypeTerm=..[Type,Inst|PROPS],nonvar(Inst),is_type(Type),!.


%db_op_manditory(_,KB:Term,Term):- is_kb_module(KB).
%db_op_manditory(_,KB:Term,Term):- dbase_mod(KB).
db_op_manditory(_,_:Term,Term).
db_op_manditory(_,C0,[Prop,Obj|ARGS]):- C0=..[svo,Obj,Prop|ARGS],!.
db_op_manditory(_,DBASE_T,[P,A|ARGS]):- DBASE_T=..[HOLDS,P,A|ARGS],is_holds_true_not_hilog(HOLDS).
db_op_manditory(_,[HOLDS,P,A|ARGS],[P,A|ARGS]):- is_holds_true_not_hilog(HOLDS).
db_op_manditory(_,[P,A|ARGS],DBASE):- var(P),!,hilog_functor(HILOG),DBASE=..[HILOG,P,A|ARGS].
db_op_manditory(_,[P,A|ARGS],DBASE):- atom(P),!,DBASE=..[P,A|ARGS].
db_op_manditory(_,[P,A|ARGS],DBASE):- nonvar(P),dtrace, DBASE=..[P,A|ARGS].
db_op_manditory(_,TypeTerm,props(Inst,[isa(Type)|PROPS])):- TypeTerm=..[Type,Inst|PROPS],nonvar(Inst),is_type(Type),!.


db_op_simpler_wlc(ask(Must),Wild,Simpler):- !,hotrace(db_op_simpler(ask(Must),Wild,Simpler)),not(is_loop_checked(req(Simpler))),!.
db_op_simpler_wlc(tell(Must),Wild,Simpler):- !,hotrace(db_op_simpler(tell(Must),Wild,Simpler)),not(is_loop_checked(add(Simpler))),!.
db_op_simpler_wlc(Op,Wild,Simpler):- !,hotrace(db_op_simpler(Op,Wild,Simpler)),not(is_loop_checked(db_op0(Op,Simpler))),!.


db_op_sentence(_Op,Prop,ARGS,C0):- must(atom(Prop)), C0=..[Prop|ARGS],!.
db_op_sentence(_Op,Prop,ARGS,C0):- grtrace, C0=..[holds_t,Prop|ARGS].


% ================================================
% db_tell_isa/2
% ================================================

db_tell_isa(T,type):- !, define_type(T).
db_tell_isa(type,type):- !.
db_tell_isa(Term,mpred):- !,decl_mpred(Term).
% skip formatter types
db_tell_isa(_I,T):- member(T,[string,action,dir]),!.
db_tell_isa(_I,Fmt):- notrace(( nonvar(Fmt), is_ft(Fmt))),!,dmsg(todo(dont_assert_is_ft(Fmt))),!.
db_tell_isa(I,T):- get_isa_asserted(I,T),!.
db_tell_isa(I,T):-doall(db_tell_isa_hooked(I,T)).

db_tell_isa_hooked(I,T):- not(ground(db_tell_isa(I,T))), trace_or_throw(not(ground(db_tell_isa(I,T)))).
% db_tell_isa_hooked(I,T):- once((define_type(T),hooked_asserta(isa(I,T)))).

db_tell_isa_hooked(I,T):-asserta_new(dbase:dbase_t(isa,I,T)).
% one of 4 special types
db_tell_isa_hooked(I,T):- atom(T),is_creatable_type(T),call_after_game_load((world:create_instance(I,T,[]))).
% sublass of 4 special types
db_tell_isa_hooked(I,T):- is_creatable_type(ST),holds_t(subclass,T,ST),call_after_game_load((world:create_instance(I,ST,[isa(T)]))).

%db_tell_isa_hooked(food5,'Weapon'):-trace_or_throw(db_tell_isa(food5,'Weapon')).
%db_tell_isa_hooked(I,T):-dmsg((told(db_tell_isa(I,T)))).
db_tell_isa_hooked(I,T):- dbase_t(subclass,T,ST),db_op(tell(_OldV),isa(I,ST)).

define_ft(FT):- db_tell_isa(FT,formattype).

get_isa_backchaing(A,T):-var(T),!,get_isa_asserted_0(A,T).
get_isa_backchaing(_,A):- not(is_type(A)),!,fail.
get_isa_backchaing(A,formattype):- !,is_ft(A).
get_isa_backchaing(A,type):- !, is_type(A).
get_isa_backchaing(A,Fmt):- nonvar(Fmt),is_ft(Fmt),!,correctType(ask(once),A,Fmt,AA),!,A==AA,!.
get_isa_backchaing(A,Fmt):- get_isa_asserted(A,Fmt).

get_isa_asserted(A,Fmt):-hotrace(get_isa_asserted_0(A,Fmt)).

get_isa_asserted_0(A,ArgsIsa):- nonvar(ArgsIsa),mpred_functor(ArgsIsa,A,Prop),!, get_mpred_prop(_,_,Prop).
get_isa_asserted_0(A,Fmt):- call_t(dac(d,a,no_c,no_mt),isa,A,Fmt).
get_isa_asserted_0(A,ArgsIsa):- mpred_functor(ArgsIsa,A,Prop),!, get_mpred_prop(_,_,Prop).

mpred_functor(argsIsa,A,argsIsa(A)).
mpred_functor(singleValued,_,singleValued).
mpred_functor(multi,_,multi(_)).
mpred_functor(multiValued,_,multiValued(_)).


equivRule_call(A,B):- holds_t(equivRule,A,B).
equivRule_call(A,B):- holds_t(equivRule,B,A).
forwardRule_call(A,B):- holds_t(forwardRule,B,A).

good_for_chaining(_,_):-!.
good_for_chaining(_Op,Term):-not(contains_singletons(Term)).
db_rewrite(_Op,Term,NewTerm):-equivRule_call(Term,NewTerm).
db_rewrite(_Op,Term,NewTerm):-forwardRule_call(Term,NewTerm).

call_must(query,Call):- !,call(Call).
call_must(must,Call):- !,must(Call).
% call_must(Must,Call):- var(Must),!,Call.
call_must(once,Call):- !,once(Call).
call_must(!,Call):- !,once(Call).
call_must(_,Call):- call(Call).
% ================================================
% db_op/2
% ================================================
add_from_file(B,_):- contains_singletons(B),grtrace,dmsg(todo(add_from_file_contains_singletons(B))),!,fail.
add_from_file(B,B):- db_op(tell(_OldV),B),!.

do_db_op_hooks:- hotrace(once(ignore(do_all_of(dbase_module_loaded)))).

db_op(tell(OldV),Term):- !, db_opp(tell(OldV),Term),!,do_db_op_hooks.
db_op(Op,Term):- do_db_op_hooks,db_opp(Op,Term),do_db_op_hooks.

% ================================================
% db_opp/2
% ================================================

db_opp(Op,isa(Term,Var)):- var(Var),!,db_op0(Op,get_isa(Term,Var)).

db_opp(Op,Wild):- db_op_manditory(Op,Wild,Simpler),Wild\=Simpler,!,db_op(Op,Simpler).

db_opp(ask(Must),isa(T,type)):- !,call_must(Must,is_type(T)).
db_opp(tell(_),isa(T,type)):- !,db_tell_isa(T,type).
db_opp(tell(_),isa(T,Type)):- !,db_tell_isa(T,Type).

db_opp(ask(Must),createableType(SubType)):- !, call_must(Must,is_creatable_type(SubType)).

db_opp(tell(OldV),B):- !,loop_check_term(db_op0(tell(OldV),B),add(B),true). % true = we are already processing this assert
db_opp(ask(Must),B):- !,loop_check_term(db_op0(ask(Must),B),req(B),db_query(Must,B)). % true = we are already processing this assert
db_opp(Op,Term):- loop_check_throw(db_op0(Op,Term)).

% ================================================
% db_op0/2
% ================================================

db_op0(Op,Term):- not(compound(Term)),!,grtrace,throw_safe(nc(db_op0(Op,Term))).

db_op0(Op,KB:Term):- is_kb_module(KB),!,db_op(Op,Term).
db_op0(Op,KB:Term):- dbase_mod(KB),!,db_op(Op,Term).

db_op0(Op,Term):- record_on_thread(dbase_opcall,db_op(Op,Term)),fail.

db_op0(Op,(':- '(A))):- must((expand_goal_correct_argIsa(A,AA))),expanded_different(A,AA),!,db_op(Op, (':- '(A))).

db_op0(tell(_OldV),(':-'(A))):- !, must((expand_goal_correct_argIsa(A,AA),call_expanded_for(call,AA))).
db_op0(retract,(C1;C2)):- !,dtrace,once((db_op(retract,C1),db_op(retract,C2))).
db_op0(tell(OldV),(C1;C2)):- !,db_op(tell(OldV),C1),!,db_op(tell(OldV),C2),!.
db_op0(ra,(C1;C2)):- !,must(db_op(ra,C1)),must(db_op(ra,C2)).
db_op0(Op,(C1,C2)):- !,db_op(Op,C1),db_op(Op,C2).

db_op0(ask(Must),props(Obj,Props)):- var(Props),!,findall(Prop,(call_must(Must,dbase_t([P,Obj|REST])),Prop=..[P|REST]),Props).
db_op0(Op ,props(Obj,Open)):- var(Open),!,throw_safe(db_op(Op,props(Obj,Open))).
db_op0(_Op,props(_Obj,[])):- !.
db_op0(Op,props(Obj,[P])):- nonvar(P),!,db_op(Op,props(Obj,P)).
db_op0(Op,props(Obj,[P|ROPS])):- !,db_op(Op,props(Obj,P)),db_op(Op,props(Obj,ROPS)).
db_op0(Op,props(Obj,PropVal)):- safe_univ(PropVal,[Prop,NonVar|Val]),Obj==NonVar,!,db_op(Op,[holds_t,Prop,Obj|Val]).
db_op0(Op,props(Obj,PropVal)):- PropVal=..[OP,Pred|Val],comparitiveOp(OP),not(comparitiveOp(Pred)),!,OPVAL=..[OP|Val],PropVal2=..[Pred,OPVAL],db_op(Op,props(Obj,PropVal2)).
db_op0(Op,props(Obj,PropVal)):- PropVal=..[Prop|Val],not(infix_op(Prop,_)),!,db_op(Op,[holds_t,Prop,Obj|Val]).
db_op0(Op,props(Obj,PropVal)):- PropVal=..[Prop|Val],!,grtrace,db_op(Op,[holds_t,Prop,Obj|Val]).

db_op0(Op,expand_args(Exp,Term)):- !,forall(do_expand_args(Exp,Term,O),db_op(Op,O)).
db_op0(Op,somethingIsa(A,List)):- !,forall_member(E,List,db_op(Op, isa(A,E))).
db_op0(Op,somethingDescription(A,List)):- !,forall_member(E,List,db_op(Op, description(A,E))).
db_op0(Op,objects(Type,List)):- !,forall_member(I,List,db_op(Op,isa(I,Type))).
db_op0(Op,sorts(Type,List)):- !,forall_member(I,List,db_op(Op, subclass(I,Type))).
db_op0(Op,predicates(List)):- !,forall_member(T,List,db_op(Op,mpred(T))).
db_op0(Op,EACH):- EACH=..[each|List],forall_member(T,List,db_op(Op,T)).

db_op0(Op,db_op_exact(Term)):- !,db_op_exact(Op,Term).
 
db_op0(tell(_),description(A,E)):- !,must(once(assert_description(A,E))).
db_op0(Op,nameString(A,S0)):- determinerRemoved(S0,String,S),!,db_op(Op, nameString(A,S)),db_op(tell(_OldV), determinerString(A,String)).

% db_op0(Op,Term):- hotrace(good_for_chaining(Op,Term)), db_rewrite(Op,Term,NewTerm),not(contains_singletons(NewTerm)),db_op(Op,NewTerm).

db_op0(tell(_OldV), subclass(I,T)):- (atomic(I)->define_type(I);true) ,  (atomic(T)->define_type(T);true), fail.
db_op0(tell(_OldV), subft(I,T)):- (atomic(I)->define_ft(I);true) ,  (atomic(T)->define_ft(T);true), fail.

db_op0(tell(_OldV),mpred(A)):- !,decl_mpred(A),!.
db_op0(tell(_OldV),isa(A,mpred)):- !,decl_mpred(A),!.

db_op0(ask(Must),get_isa(Term,Var)):- !,call_must(Must,get_isa_asserted(Term,Var)).
db_op0(ask(Must),isa(Term,Var)):- !,call_must(Must,hotrace(get_isa_backchaing(Term,Var))).
db_op0(Op,isa(A,SubType)):- holds_t(createableSubclassType,SubType,Type),!,db_op(Op,isa(A,Type)),db_op(Op,isa(A,SubType)).

db_op0(tell(_OldV),argsIsa(F,Term)):-!,hooked_asserta(dbase_t(argsIsa,F,Term)).
db_op0(Op,argsIsa(Term)):- !, fix_fa(Term,F,_),!,db_op(Op,argsIsa(F, Term)).
db_op0(tell(_OldV),singleValued(Term)):- !,decl_mpred(Term),add_mpred_prop(Term,singleValued).
db_op0(tell(_OldV),multiValued(Term)):- !,functor_safe(Term,_,A),decl_mpred(Term),add_mpred_prop(Term,[multiValued,multi(A)]).
db_op0(ask(Must),argIsa(P,N,T)):- call_must(Must,(get_mpred_prop(P,argsIsa(ArgsIsa)),arg(N,ArgsIsa,T),must(nonvar(T)))).


db_op0(Op,Wild):- db_op_simpler_wlc(Op,Wild,Simpler),!,db_op(Op,Simpler).

db_op0(Op,Term):- Term =..[Type,A],!,db_op(Op,isa(A,Type)).
db_op0(Op,A):- hotrace(must(once(correctArgsIsa(Op,A,AA)))),expanded_different(A,AA),!,must(db_op(Op,AA)).

db_op0(ask(Must),Goal):- prolog_callable(Goal),!,call_must(Must,call_expanded_for(call,Goal)).
db_op0(Op,C0):- C0=..[Prop|ARGS],db_op_unit(Op,C0,Prop,ARGS).

% ================================================
% db_op_unit/3
% ================================================

db_op_unit(Op,_C0,Prop,ARGS):-is_type(Prop),trace_or_throw(db_op_unit(Op,is_type(Prop),ARGS)).
db_op_unit(Op,C0,isa,ARGS):-trace_or_throw(db_op_unit(Op,isa(C0),ARGS)).

% impl/1
db_op_unit(Op,_C0,Prop,ARGS):- get_mpred_prop(Prop,impl(Other)),db_op_sentence(Op,Other,ARGS,Unit),db_op_loop(Op,Unit,fail).

% use_db_op/1
db_op_unit(Op,C0,Prop,_ARGS):- get_mpred_prop(Prop,use_db_op(Other)),!,call(Other,Op,C0).

% alias/1
db_op_unit(Op,_C0,Prop,ARGS):- get_mpred_prop(Prop,alias(Other)),!,
   db_op_sentence(Op,Other,ARGS,Unit),!,
   db_op_loop(Op,Unit,trace_or_throw(db_op_unit(Op,alias(Other),Prop,ARGS))).

% inverse/1
db_op_unit(Op,_C0,Prop,ARGS):- 
      get_mpred_prop(Prop,inverse(Other)),!,grtrace,must(atom(Other)),
      inverse_args(ARGS,Inverse),      
      db_op_sentence(Op,Other,Inverse,Unit1),
      db_op_sentence(Op,Prop,ARGS,Unit2),!,
      (db_op_loop(Op,Unit2,fail);db_op_exact(Op,Unit1)).
      

% assert_with_pred/1
db_op_unit(tell(_),C0,Prop,_RGS):- get_mpred_prop(Prop,assert_with_pred(How)),!,grtrace,must(atom(How)), call(run_database_hooks(assert(z)),C0), call(How,C0).

% plain prop
db_op_unit(Op,_C0,Prop,ARGS):- must((db_op_sentence(Op,Prop,ARGS,Unit),same_vars(ARGS,Unit))),!, db_op_exact(Op,Unit).

db_op_unit(Op,_C0,Prop,ARGS):- grtrace,must((db_op_sentence(Op,Prop,ARGS,Unit),same_vars(ARGS,Unit))),!, db_op_loop(Op,Unit,db_op_exact(Op,Unit)).
db_op_unit(Op,C0,_Prop,_ARGS):- db_op_loop(Op,C0,db_op_exact(Op,C0)).


db_op_loop(Op,Unit,Result):- is_loop_checked(db_op0(Op,Unit)),!,call(Result).
db_op_loop(Op,Unit,_Result):- db_op(Op,Unit).

% ================================================
% db_op_exact/2
% ================================================
db_op_exact(Op,C):- C=..[SubType,Arg],db_op_loop(Op,isa(Arg,SubType),fail),!.
db_op_exact(ask(Must), Term):- !,db_query(Must,Term).
db_op_exact(query, Term):- !,db_query(findall,Term).
db_op_exact(must, Term):- !,db_query(must,Term).
db_op_exact(u,C):- grtrace,db_quf(u,C,U,Template),call_expanded_for(quf,U),Template,must(ground(Template)),!,ignore(hooked_retractall(Template)).
db_op_exact(ra,C):- db_quf(ra,C,U,Template),!, doall((call_expanded_for(quf,U),hooked_retractall(Template))).
db_op_exact(retract,C):- must(db_quf(retract,C,U,Template)),!,call_expanded_for(quf,U),!,hooked_retract(Template).
db_op_exact(tell(OldV),W):- non_assertable(W,Why),dumpST,trace,throw_safe(todo(db_op(tell(OldV), non_assertable(Why,W)))).
db_op_exact(tell(Must),C0):- db_quf(tell(Must),C0,U,C),!,must(call_expanded_for(quf,U)),functor(C,F,A),( get_mpred_prop(F,A,singleValued) -> must(db_assert_sv(Must,C,F,A,_OldVOut1)) ; must(db_assert_mv(Must,C,F,A,_OldVOut2))).
db_op_exact(tell(Must),C):- grtrace, functor(C,F,A), must((get_mpred_prop(F,_,singleValued) -> must(db_assert_sv(Must,C,F,A,_OldVOut1)) ; must(db_assert_mv(Must,C,F,A,_OldVOut2)))).
db_op_exact(Op,C):- trace_or_throw(unhandled(db_op_exact(Op,C))).

% ================================================
% db_query/1
% ================================================

db_query(Must,LC):- loop_check(db_query_lc(Must,LC),call_expanded_for(req,LC)).

db_query_lc(Must,(C0->C1;C2)):- !, (db_query(once,C0) -> db_query(Must,C1) ; db_query(Must,C2)).
db_query_lc(Must,(C0->C1)):- !, (db_query(once,C0) -> db_query(Must,C1)).
db_query_lc(Must,(C1;C2)):- !, db_query(Must,C1) ; db_query(Must,C2).
db_query_lc(Must,(C1,C2)):- !, db_query(query,C1) , db_query(Must,C2).
db_query_lc(Must,Term):- findall(a(Term),db_query_quf(Must,Term),List),list_to_set_safe(List,Set),!,member(a(Term),Set).

db_query_quf(Must,C):- db_quf(ask(Must),C,Pretest,Template),!,call_tabled(call_expanded_for(quf,Pretest)),call_must(Must,call_expanded_for(req,Template)).


:- meta_predicate meta_interp(1,+).

meta_interp_signal(meta_call(V)):-!,nonvar(V).
meta_interp_signal(meta_callable(_,_)).
meta_interp_signal(_:meta_call(V)):-!,nonvar(V).
meta_interp_signal(_:meta_callable(_,_)).



meta_interp(_CE,A):- leash(+all),meta_interp_signal(A),!,fail.
meta_interp(CE,A):- var(A),!, throw(meta_interp(CE,A)).
meta_interp(_,_:true):-!.
%right thing i thought 
%meta_interp(CE,M:A):-!, '@'(meta_interp(CE,A),M).
meta_interp(CE,A):- call(CE, meta_callable(A,NewA)),!,NewA.
meta_interp(CE,not(A)):-!,not(meta_interp(CE,A)).
meta_interp(CE,once(A)):-!,once(meta_interp(CE,A)).
meta_interp(CE,(A;B)):-!,meta_interp(CE,A);meta_interp(CE,B).
meta_interp(CE,(A->B)):-!,meta_interp(CE,A)->meta_interp(CE,B).
meta_interp(CE,(A->B;C)):-!,(meta_interp(CE,A)->meta_interp(CE,B);meta_interp(CE,C)).
meta_interp(CE,(A,!)):-!,meta_interp(CE,A),!.
meta_interp(CE,(A,B)):-!,meta_interp(CE,A),meta_interp(CE,B).
meta_interp(CE,[A]):-!,meta_interp(CE,A).
meta_interp(CE,[A|B]):-!,meta_interp(CE,A),meta_interp(CE,B).
%meta_interp(_CE,!):- !, cut_block(!).
meta_interp(CE,_:A):- !, call(CE,meta_call(A)).
meta_interp(CE,_:A):-!, meta_interp(CE,A).
meta_interp(CE,A):- call(CE,meta_call(A)).


call_expanded(X):-with_assertions(hga_wrapper(dbase_t,holds_t,dbase_t),call_expanded_s(X)).
call_expanded_for(quf,X):-with_assertions(hga_wrapper(dbase_t,holds_t,dbase_t),call_expanded_s(X)).
call_expanded_for(call,X):-with_assertions(hga_wrapper(dbase_t,holds_t,dbase_t),call_expanded_s(X)).

call_expanded_for(req,X):-with_assertions(hga_wrapper(dbase_t,cholds_t,dbase_t),call_expanded_s(X)).


:- meta_predicate call_expanded(+).

call_expanded_s(X):- var(X),!, throw(var(call_expanded(X))).
call_expanded_s(meta_callable(_Goal,_Result)):-!,fail.
call_expanded_s(meta_call(X)):- !,call_expanded0(X).
call_expanded_s(Goal):- meta_interp(call_expanded,Goal).

call_expanded0(_:PreGoal):-!, call_expanded_s(PreGoal).
call_expanded0(meta_callable(_Goal,_Result)):-!,fail.
call_expanded0(Goal):- get_mpred_prop(Goal,ask_module(Module)),!,dtrace,Module:call(Goal).
call_expanded0(Goal):- prolog_callable(Goal),!,Goal.
call_expanded0(PreGoal):- force_expand(expand_goal(PreGoal,Goal)),PreGoal\=Goal,prolog_callable(Goal),!,Goal.
call_expanded0(PreGoal):- force_expand(expand_term(PreGoal,Goal)),PreGoal\=Goal,prolog_callable(Goal),!,Goal.
call_expanded0(PreGoal):- hilog_functor(HILOG),PreGoal=..[P|LIST], P \= HILOG,!,safe_univ(Goal,[HILOG,P|LIST]),!,Goal.
call_expanded0(Goal):- dtrace, dmsg(todo(failure(Goal))),!,dumpST,grtrace,req(Goal).
%call_expanded0(Goal):- meta_interp(call_expanded,Goal).



prolog_callable_expanded(Goal):- get_mpred_prop(Goal,ask_module(Module)),prolog_callable(Module:call(Goal)),!.
prolog_callable_expanded(Goal):- prolog_callable(Goal),!.
prolog_callable_expanded(PreGoal):- force_expand(expand_goal(PreGoal,Goal)),PreGoal\=Goal,prolog_callable(Goal),!.
prolog_callable_expanded(PreGoal):- force_expand(expand_term(PreGoal,Goal)),PreGoal\=Goal,prolog_callable(Goal),!.
prolog_callable_expanded(_:PreGoal):-prolog_callable_expanded(PreGoal).


prolog_callable(Goal):- notrace(( not(not(predicate_property(Goal,_PP))))).


singletons_throw_or_fail(C):- contains_singletons(C),trace_or_throw(contains_singletons(C)).
nonground_throw_or_fail(C):- throw_if_true_else_fail(not(ground(C)),C).

% ================================================
% db_assert/1
% ================================================

% assert to tell(OldV) mutlivalue pred
db_assert_mv(Must,C,F,A,_OldV):- call_must(Must, (get_mpred_prop(F/A,ordered) -> hooked_assertz(C) ; hooked_asserta(C))).

% assert to tell(OldV) singlevalue pred
db_assert_sv(Must,C,F,A,OldV):- throw_if_true_else_fail(contains_singletons(C),db_assert_sv(Must,C,F,A,OldV)).
db_assert_sv(Must,C,_,A,OldV):-
   arg(A,C,Update),
   db_assert_sv_old_value(C,A,OLD,OldV),
   db_assert_sv_update(Must,C,A,OLD,Update),!.

db_assert_sv_update(Must,C,A,OLD,Update):- 
   update_value(OLD,Update,NEW),
   nonvar(NEW),
   replace_arg(C,A,NEW,CC),
   call_must(Must,hooked_asserta(CC)),!.

db_assert_sv_old_value(C,A,OLD,CC):- must(var(OLD)),
   replace_arg(C,A,OLD,CC),
      % this binds or leave OLD
      ((req(CC) -> hooked_retract(CC) ; OLD= _ )),!.


replace_arg(C,A,OLD,CC):- 
   C=..FARGS,
   replace_nth(FARGS,A,OLD,FARGO),
   CC=..FARGO.

replace_nth([],_,_,[]):- !.
replace_nth([_|ARGO],0,OLD,[OLD|ARGO]):- !.
replace_nth([T|FARGS],A,OLD,[T|FARGO]):- 
    A2 is A-1,replace_nth(FARGS,A2,OLD,FARGO).

% ================================================
% hooked_assert*/1 hooked_retract*/1
% ================================================

% only place ever should actual game dbase be changed from
hooked_asserta(C):- singletons_throw_or_fail(hooked_asserta(C)).
hooked_asserta(C):- run_database_hooks(asserta(a),C),asserta_cloc(C).
hooked_assertz(C):- singletons_throw_or_fail(hooked_assertz(C)).
hooked_assertz(C):- run_database_hooks(assert(z),C), assertz_cloc(C).
hooked_retract(C):- nonground_throw_or_fail(hooked_retract(C)).
hooked_retract(C):- run_database_hooks(retract(one),C), must(retract_cloc(C)).
hooked_retractall(C):- run_database_hooks(retract(all),C), retractall_cloc(C).

ensure_db_predicate(Op,In,Out):- ensure_db_predicate_1(Op,In,Out),!.
ensure_db_predicate(Op,In,Out):- trace_or_throw(ensure_db_predicate(Op,In,Out)).

ensure_db_predicate_1(AR,M:C,G):- nonvar(M), !,ensure_db_predicate_1(AR,C,G).
ensure_db_predicate_1(AR,CI,G):- force_head_expansion(CI,C),ensure_db_predicate_2(AR,C,G).

ensure_db_predicate_2(AR,M:C,G):- nonvar(M),ensure_db_predicate_2(AR,C,G).
ensure_db_predicate_2(_AR,CI,G):- force_head_expansion(CI,EG),force_expand(expand_term(EG,G)).

% ensure_db_predicate(tell(OldV),agent('NpcCol1000-Geordi684'),Out).

retract_cloc(C):- ensure_db_predicate(retract,C,G), show_cgoal(retract(G)).
retractall_cloc(C):- ensure_db_predicate(retract,C,G), show_cgoal(retractall(G)).
asserta_cloc(C):- ensure_db_predicate(tell(_OldV),C,G), show_cgoal(asserta_new(G)).
assertz_cloc(C):- ensure_db_predicate(tell(_OldV),C,G), debugOnError(show_cgoal(assertz_if_new(G))).

show_cgoal(G):- !, G.
show_cgoal(G):- dmsg(show_goal(G)),call(G).


% ================================================
% clause_present/1
% ================================================

:- thread_local clause_present_lookup_local/3.

clause_present(C):- hotrace((functor_safe(C,F,A),clause_present(C,F,A))).
clause_present(C,F,1):- C=..[F,A], is_ft(F), correctFormatType(ask(findall),A,F,_).
clause_present(C,_F,_A):- not(predicate_property(C,_)),!,fail.
clause_present(C,_F,_A):- not(ground(C)),!,fail.
clause_present(C,F,A):- clause_present_lookup_local(C,F,A),!,fail.
clause_present(C,_,1):- !, clause(C,true).
clause_present(C,F,A):- with_assertions(clause_present_lookup_local(C,F,A),clause_present_1(C,F,A)).

clause_present_1(C,_,_):- catch(C,_,false),!.
clause_present_1(C,_F,_A):- predicate_property(C,foreign),!,trace_or_throw(predicate_prxoperty(C,foreign)),!,fail.
clause_present_1(C,_F,_A):- clause(C,true),!.
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



compare_n(NewLast,Last):- NewLast=Last,!.
compare_n(NewLast,Last):- NewLast==unknown,!,NewLast==Last.
compare_n(NewLast,Last):- number(NewLast),not(number(Last)),trace_or_throw(incomparable_terms(Last,NewLast)).
compare_n(Last,NewLast):- number(NewLast),not(number(Last)),trace_or_throw(incomparable_terms(Last,NewLast)).
compare_n(NewLast,Last):- atomic(NewLast),not(atomic(Last)),trace_or_throw(incomparable_terms(Last,NewLast)).
compare_n(Last,NewLast):- atomic(NewLast),not(atomic(Last)),trace_or_throw(incomparable_terms(Last,NewLast)).


member_or_e(E,[L|List]):- !,member(E,[L|List]).
member_or_e(E,E).

is_single_valuedOrFail(F,A,Obj,ARGS):- get_mpred_prop(F,A,singleValued),!,valuedOrThrow(F,A,Obj,ARGS),!.
is_single_valuedOrFail(_,_,_,_):- fail.

valuedOrThrow(F,_,Obj,ARGS):- holds_t(isa,Obj,T), findall_type_default_props(Obj,T,Props),Props=[_|_],Prop=..[F|ARGS], member_or_e(Prop,Props),!.
valuedOrThrow(F,A,Obj,ARGS):- valuedOrThrow1(F,A,Obj,ARGS).
valuedOrThrow1(_F,_A,_Obj,ARGS):- last(ARGS,unknown),!.
valuedOrThrow1(F,A,Obj,ARGS):- trace_or_throw(is_single_valuedOrFail(F,A,Obj,ARGS)).


findall_type_default_props(Inst,Type,TraitsO):- findall(Props,moo:type_default_props(Inst,Type,Props),Traits),flatten(Traits,TraitsM),!,subst(TraitsM,self,Inst,TraitsO).


replace_nth([],_N,_OldVar,_NewVar,[]):- !,trace_or_throw(missed_the_boat).
replace_nth([OldVar|ARGS],1,OldVar,NewVar,[NewVar|ARGS]):- !.
replace_nth([Carry|ARGS],Which,OldVar,NewVar,[Carry|NEWARGS]):- 
 Which1 is Which-1,
 replace_nth(ARGS,Which1,OldVar,NewVar,NEWARGS),!.

update_value(OLD,+X,NEW):- number(OLD),catch(NEW is OLD + X,_,fail),!.
update_value(OLD,-X,NEW):- number(OLD),catch(NEW is OLD - X,_,fail),!.
update_value(OLD,NEW,NEXT):- var(NEW),!,trace_or_throw(logicmoo_bug(update_value(OLD,NEW,NEXT))).
update_value(_OLD,NEW,NEW).

insert_into(ARGS,0,Insert,[Insert|ARGS]):- !.
insert_into([Carry|ARGS],After,Insert,[Carry|NEWARGS]):- 
   After1 is After - 1,
   insert_into(ARGS,After1,Insert,NEWARGS).


is_mpred_prolog(F,A):- get_mpred_prop(F,A,ask_module(_)), 
   not(get_mpred_prop(F,A,query(_))),!.

:- include(dbase_formattypes).

:- include(dbase_c_term_expansion).

:- catch(noguitracer,_,true).

% load_motel:- defrole([],time_state,restr(time,period)).

:- dynamic(call_tabled_list/2).

make_key(CC,Key):- copy_term(CC,Key),numbervars(Key,'$VAR',0,_),!.

expire_tabled_list(T):- CT= call_tabled_list(Key,List),doall(((CT,any_term_overlap(T,Key:List),retract(CT)))).

any_term_overlap(T1,T2):- atoms_of(T1,A1),atoms_of(T2,A2),!,member(A,A1),member(A,A2),!.

:- meta_predicate call_tabled(0).
:- module_transparent call_tabled/1.

call_tabled(findall(A,B,C)):- !,findall_tabled(A,B,C).
call_tabled(C):- copy_term(C,CC),numbervars(CC,'$VAR',0,_),call_tabled(C,C).
call_tabled(CC,C):- make_key(CC,Key),call_tabled0(Key,C,C,List),!,member(C,List).
call_tabled0(Key,_,_,List):- call_tabled_list(Key,List),!.
call_tabled0(Key,E,C,List):- findall(E,C,List1),list_to_set(List1,List),asserta_if_ground(call_tabled_list(Key,List)),!.

findall_tabled(Result,C,List):- make_key(Result^C,RKey),findall_tabled4(Result,C,RKey,List).
findall_tabled4(_,_,RKey,List):- call_tabled_list(RKey,List),!.
findall_tabled4(Result,C,RKey,List):- findall(Result,call_tabled(C),RList),list_to_set(RList,List),asserta_if_ground(call_tabled_list(RKey,List)).

asserta_if_ground(_):- !.
asserta_if_ground(G):- ground(G),asserta(G),!.
asserta_if_ground(_).

call_no_cuts(CALL):-call_no_cuts(1,CALL).

call_no_cuts(N,Var):- var(Var),!,trace_or_throw(call_no_cuts(N,Var)).
call_no_cuts(_,true):- !.
call_no_cuts(_,(!)):- !.
call_no_cuts(N,(A,B)):- !,call_no_cuts(N,A),call_no_cuts(N,B).
call_no_cuts(N,(A;B)):- !,call_no_cuts(N,A);call_no_cuts(N,B).
call_no_cuts(N,(A->B)):- !,(call_no_cuts(N,A)->call_no_cuts(N,B)).
call_no_cuts(N,(A->B;C)):- !, (call_no_cuts(N,A)->call_no_cuts(N,B);call_no_cuts(N,C)).
call_no_cuts(N, CALL):- N>0, predicate_property(CALL,number_of_clauses(_)), N2 is N-1 ,!, clause(CALL,TEST), call_no_cuts(N2, TEST).
call_no_cuts(_,C):- call_expanded(C).


:- meta_predicate compare_op(*,2,?,?).

:- meta_predicate call_tabled0(?,?,?,?).
:- meta_predicate call_tabled(?).
:- meta_predicate findall_tabled4(?,?,?,?).



% :- load_motel.

% :- include(moo_loadall).


:- include(logicmoo(vworld/moo_loader)).
:- export((          
          finish_processing_game/0, 
          correctType/4,
          gload/0,
          correctArgsIsa/3,
          load_game/1)).

:- meta_predicate show_call(0).
:- meta_predicate show_call0(0).

:-moo_hide_childs(user:goal_expansion/2).
:-moo_hide_childs(user:term_expansion/2).

user:goal_expansion(G1,G3):- notrace((compound(G1), do_term_expansions)), once(try_mud_body_expansion(G1,G2)),expanded_different(G1,G2),G2=G3.

user:term_expansion(G1,G3):- notrace((compound(G1), do_term_expansions)),  once(attempt_clause_expansion(G1,G2)),expanded_different(G1,G2),G2=G3.



load_game(File):- absolute_file_name(File,Path),see(Path),
   world_clear(current),
   repeat,
   asserta(moo:loading_game_file(File)),
   read_term(Term,[double_quotes(string)]),
   game_assert(Term),
   Term == end_of_file,seen,!,
   retractall(moo:loading_game_file(File)),
   assert_if_new(moo:loaded_game_file(File)),
   must_det(finish_processing_game).

game_assert(end_of_file):- !.
game_assert(Term):- once(must(add(Term))).

:- meta_predicate(call_after_game_load(+)).

:- dynamic(in_finish_processing_game/0).

finish_processing_game:- debug,in_finish_processing_game,!.
finish_processing_game:- assert(in_finish_processing_game),fail.
finish_processing_game:- do_all_of(dbase_module_loaded).
finish_processing_game:- retract(moo:call_after_load(A)),once(must(A)),fail.
finish_processing_game:- gftrace,call_after(moo:not_loading_game_file, savedb),fail.
finish_processing_game:- retractall(in_finish_processing_game).
finish_processing_game.




% gload:- load_game(savedb),!.
gload:- load_game(logicmoo('rooms/startrek.all.pl')).

% savedb:- !.
savedb:- 
 catch((   
   ignore(catch(make_directory('/tmp/lm/'),_,true)),
   tell('/tmp/lm/savedb'),make_db_listing,told),E,dmsg(savedb(E))).

make_db_listing:-
 % moo:dbase_mod(DBM),
%   listing(dbase_t),
 %  listing(dbase_f),
   listing(dbase:_),
 listing(moo:_),
 listing(dyn:_).

/*
is_ft_except(S,List):- 
   moo:ft_info(S,_);
   not((member(S,List), 
      ((get_subft(S,S2) ,
        is_ft_except(S2,[S|List]) ;
             ((dyn:subft(S3,S) , is_ft_except(S3,[S,S2|List]))))))).
*/


% dide some debugging
:-forall(member(F,[holds_t,holds_f]),doall((between(1,8,A),functor(Pred,F,A),moo_hide_childs(Pred)))).



p2c_dir2('s','South-Directly').
p2c_dir2('w','West-Directly').
p2c_dir2('u','Up-Directly').
p2c_dir2('d','Down-Directly').
p2c_dir2('e','East-Directly').
p2c_dir2('n','North-Directly').


:- begin_transform_moo_preds.
:- include(dbase_i_builtin).
:- end_transform_moo_preds.

:- begin_transform_moo_preds.
hook:decl_database_hook(assert(_),C):- expire_tabled_list(C).
hook:decl_database_hook(retract(_),C):- expire_tabled_list(C).
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


