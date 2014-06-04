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
% File used as storage place for all predicates which change as
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

:- module(dbase,[]).

:- export((
add/1,
argIsa_call/3,
assertThrough/1,
assertThrough/2,
balanceBinding/2,
clause_present_1/3,
clr/1,
cycAssert/1,
cycAssert/2,
cycInit/0,
cyclify/2,
cyclifyNew/2,
cycQuery/1,
cycQuery/2,
cycRetract/1,
cycRetract/2,
cycRetractAll/1,
cycRetractAll/2,
cycStats/0,
db_forall_query/1,
db_op/2,
db_prop_from_game_load/1,
db_prop_game_assert/1,
dbase_define_db_prop/2,
dbase_mod/1,
defaultMt/1,
del/1,
ensureMt/1,
findall_type_default_props/3,
finishCycConnection/3,
formatCyc/3,
getCycConnection/3,
getSurfaceFromChars/3,
holds_f/1,
holds_f/2,
holds_f/3,
holds_f/4,
holds_f/5,
holds_f/6,
holds_f/7,
holds_f/8,
holds_t/1,
holds_t/2,
holds_t/3,
holds_t/4,
holds_t/5,
holds_t/6,
holds_t/7,
holds_t/8,
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
scan_db_prop/0,
skip_qlfs/0,
stringToWords/2,
term_listing/1,
toCycApiExpression/2,
toCycApiExpression/3,
use_term_listing/2,
useExternalDBs/0,
with_kb_assertions/2,
world_clear/1,
xcall/1,
xcall/2,
xcall/3,
xcall/4,
xcall/5,
xcall/6,
xcall/7,
xcall_not/1,
xcall_not/2,
xcall_not/3,
xcall_not/4,
xcall_not/5,
xcall_not/6,
xcall_not/7,         
testOpenCyc/0)).

:-dynamic(dbase_mod/1).
dbase_mod(dbase).

:-decl_mpred((
     type/1, agent/1, item/1, region/1,

      thinking/1,

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
      mud_isa/2,
      ofclass/2,
      pathName/3, 
      possess/2,
      score/2,
      stm/2,      
      str/2,
      wearing/2)).

:- dbase_mod(DBASE), dynamic(DBASE:inside_clause_expansion/1).

:- dbase_mod(DBASE), 
          DBASE:(dynamic((
          dbase_t/1,
          dbase_t/2,
          dbase_t/3,
          dbase_t/4,
          dbase_t/5,
          dbase_t/6,
          dbase_t/7,
          assertion_f/1,
          assertion_t/1,
          dbase_f/1,
          dbase_f/2,
          dbase_f/3,
          dbase_f/4,
          dbase_f/5,
          dbase_f/6,
          dbase_f/7))).

:- dbase_mod(DBASE), 
          DBASE:(export((
          dbase_t/1,
          dbase_t/2,
          dbase_t/3,
          dbase_t/4,
          dbase_t/5,
          dbase_t/6,
          dbase_t/7,
          assertion_f/1,
          assertion_t/1,
          dbase_f/1,
          dbase_f/2,
          dbase_f/3,
          dbase_f/4,
          dbase_f/5,
          dbase_f/6,
          dbase_f/7))).

:- dbase_mod(DBASE), 
          DBASE:(multifile((
          dbase_t/1,
          dbase_t/2,
          dbase_t/3,
          dbase_t/4,
          dbase_t/5,
          dbase_t/6,
          dbase_t/7,
          assertion_f/1,
          assertion_t/1,
          dbase_f/1,
          dbase_f/2,
          dbase_f/3,
          dbase_f/4,
          dbase_f/5,
          dbase_f/6,
          dbase_f/7))).

:- dbase_mod(DBASE), 
          DBASE:(discontiguous((
          dbase_t/1,
          dbase_t/2,
          dbase_t/3,
          dbase_t/4,
          dbase_t/5,
          dbase_t/6,
          dbase_t/7,
          assertion_f/1,
          assertion_t/1,
          dbase_f/1,
          dbase_f/2,
          dbase_f/3,
          dbase_f/4,
          dbase_f/5,
          dbase_f/6,
          dbase_f/7))).

:- meta_predicate pred_term_parts(1,?,*).
:- meta_predicate mud_pred_expansion_2(3,?,?,*,*,*).
:- meta_predicate list_retain(*,1,*).
% Restarting analysis ...
% Found new meta-predicates in iteration 2 (12.070 sec)
:- meta_predicate mud_pred_expansion_1(3,*,*,*).
:- meta_predicate pred_term_parts_l(1,*,*).
% Restarting analysis ...
% Found new meta-predicates in iteration 3 (11.946 sec)
:- meta_predicate mud_pred_expansion_0(3,*,*,*).

:- meta_predicate clause_present(:), db_forall_assert_mv(+,+,+), db_forall_assert_sv(+,+,+), db_forall(+,+), db_forall_quf(+,-,-).

:- dynamic skip_qlfs/0, db_op/2,  db_prop_from_game_load/1, db_prop_game_assert/1, req/1, scan_db_prop/0, term_listing/1,
  argIsa_call/3, use_term_listing/2,world_clear/1.


:- dynamic(db_prop_prolog/1).

:- dynamic verbOverride/3,named/2, determinerString/2, keyword/2 ,descriptionHere/2, mudToHitArmorClass0/2.

% :- context_module(M),asserta(dbase_mod(M)),dmsg(assert_if_new(dbase_mod(M))).

% :- ensure_loaded(logicmoo(logicmoo_util/logicmoo_util_all)).
:- ensure_loaded(logicmoo(logicmoo_util/logicmoo_util_library)).
:- ensure_loaded(logicmoo(logicmoo_util/logicmoo_util_ctx_frame)).
:- ensure_loaded(logicmoo(logicmoo_util/logicmoo_util_bugger)).

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
:- meta_predicate mud_pred_expansion(+,+,+,-).
:- meta_predicate with_kb_assertions(?,?).
:- meta_predicate show_cgoal(?).
:- meta_predicate retractall_cloc(?).
:- meta_predicate retract_cloc(?).
:- meta_predicate assertz_cloc(?).
:- meta_predicate asserta_cloc(?).

:- def_meta_predicate(call_f,2,8).
:- def_meta_predicate(call_t,2,8).
:- def_meta_predicate(xcall,1,7).
:- def_meta_predicate(xcall_not,1,7).
:- def_meta_predicate(dbase_t,1,7).
:- def_meta_predicate(dbase_f,1,7).
:- def_meta_predicate(call_f_mt,4,10).
:- def_meta_predicate(call_t_mt,4,10).
:- def_meta_predicate(assertion_t,1,1).
:- def_meta_predicate(assertion_f,1,1).


callable_tf(P,2):-arity_pred(P),!,fail.
callable_tf(F,A):- functor_safe(P,F,A),predicate_property(P,_),!.
:-dynamic(useExternalDBs/0).
useExternalDBs:-fail.
useDBMts:- fail, useExternalDBs.


% ================================================================================
% begin holds_t
% ================================================================================
holds_t(P,A1,A2,A3,A4,A5,A6,A7):- isCycPredArity_NE(P,7),(call_t(P,A1,A2,A3,A4,A5,A6,A7);call_t_mt(P,A1,A2,A3,A4,A5,A6,A7,_,_);assertion_t([P,A1,A2,A3,A4,A5,A6,A7])).
holds_t(P,A1,A2,A3,A4,A5,A6):- isCycPredArity_NE(P,6),(call_t(P,A1,A2,A3,A4,A5,A6);call_t_mt(P,A1,A2,A3,A4,A5,A6,_,_)).
holds_t(P,A1,A2,A3,A4,A5):-isCycPredArity_NE(P,5),(call_t(P,A1,A2,A3,A4,A5);call_t_mt(P,A1,A2,A3,A4,A5,_,_)).
holds_t(P,A1,A2,A3,A4):- isCycPredArity_NE(P,4),(call_t(P,A1,A2,A3,A4);call_t_mt(P,A1,A2,A3,A4,_,_)).
holds_t(P,A1,A2,A3):-isCycPredArity_NE(P,3),(call_t(P,A1,A2,A3);call_t_mt(P,A1,A2,A3,_,_)).
holds_t(P,A1,A2):-isCycPredArity_NE(P,2),(call_t(P,A1,A2);call_t_mt(P,A1,A2,_,_)).
holds_t(P,A1):-isCycPredArity_NE(P,1),(call_t(P,A1);call_t_mt(P,A1,_,_)).


holds_t([AH,P|LIST]):-is_holds_true(AH),!,holds_t_p2(P,LIST).
holds_t([AH,P|LIST]):-is_holds_false(AH),!,holds_f_p2(P,LIST).
holds_t([P|LIST]):-!,holds_t_p2(P,LIST).
holds_t(CALL):-CALL=..[P|LIST],holds_t([P|LIST]).
holds_t_p2(P,LIST):- CALL=..[holds_t,P|LIST],call(CALL).


call_t(P,A1,A2,A3,A4,A5,A6,A7):- callable_tf(P,7),List= [P,A1,A2,A3,A4,A5,A6,A7], CALL=..List,(assertion_t(List);dbase_t(CALL);xcall(CALL)).
call_t(P,A1,A2,A3,A4,A5,A6):- dbase_t(P,A1,A2,A3,A4,A5,A6).
call_t(P,A1,A2,A3,A4,A5,A6):- assertion_t([P,A1,A2,A3,A4,A5,A6]).
call_t(P,A1,A2,A3,A4,A5,A6):- callable_tf(P,6),xcall(P,A1,A2,A3,A4,A5,A6).
call_t(P,A1,A2,A3,A4,A5):- dbase_t(P,A1,A2,A3,A4,A5).
call_t(P,A1,A2,A3,A4,A5):- assertion_t([P,A1,A2,A3,A4,A5]).
call_t(P,A1,A2,A3,A4,A5):- callable_tf(P,5),xcall(P,A1,A2,A3,A4,A5).
call_t(P,A1,A2,A3,A4):- dbase_t(P,A1,A2,A3,A4).
call_t(P,A1,A2,A3,A4):- assertion_t([P,A1,A2,A3,A4]).
call_t(P,A1,A2,A3,A4):- callable_tf(P,4),xcall(P,A1,A2,A3,A4).
call_t(P,A1,A2,A3):- dbase_t(P,A1,A2,A3).
call_t(P,A1,A2,A3):- assertion_t([P,A1,A2,A3]).
call_t(P,A1,A2,A3):- callable_tf(P,3),xcall(P,A1,A2,A3).
call_t(P,A1,A2):- dbase_t(P,A1,A2).
call_t(P,A1,A2):- assertion_t([P,A1,A2]).
call_t(P,A1,A2):- callable_tf(P,2),xcall(P,A1,A2).
call_t(P,A1):- dbase_t(P,A1).
call_t(P,A1):- assertion_t([P,A1]).
call_t(P,A1):- callable_tf(P,1),xcall(P,A1).

call_t_mt(P,A1,A2,A3,A4,A5,A6,A7,A8,A9):- useDBMts,callable_tf(P,9),CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8,A9],xcall(CALL).
call_t_mt(P,A1,A2,A3,A4,A5,A6,A7,A8):- useDBMts,callable_tf(P,8),CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8],xcall(CALL).
call_t_mt(P,A1,A2,A3,A4,A5,A6,A7):- useDBMts,callable_tf(P,7),CALL=..[P,A1,A2,A3,A4,A5,A6,A7],xcall(CALL).
call_t_mt(P,A1,A2,A3,A4,A5,A6):- useDBMts,callable_tf(P,6),xcall(P,A1,A2,A3,A4,A5,A6).
call_t_mt(P,A1,A2,A3,A4,A5):- useDBMts,callable_tf(P,5),xcall(P,A1,A2,A3,A4,A5).
call_t_mt(P,A1,A2,A3,A4):- useDBMts,callable_tf(P,4),xcall(P,A1,A2,A3,A4).
call_t_mt(P,A1,A2,A3):- useDBMts,callable_tf(P,3),xcall(P,A1,A2,A3).

xcall(P,A1,A2,A3,A4,A5,A6,A7,A8,A9):- CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8,A9],call(CALL).
xcall(P,A1,A2,A3,A4,A5,A6,A7,A8):- CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8],call(CALL).
xcall(P,A1,A2,A3,A4,A5,A6,A7):- CALL=..[P,A1,A2,A3,A4,A5,A6,A7],call(CALL).
xcall(P,A1,A2,A3,A4,A5,A6):- call(P,A1,A2,A3,A4,A5,A6).
xcall(P,A1,A2,A3,A4,A5):- call(P,A1,A2,A3,A4,A5).
xcall(P,A1,A2,A3,A4):- call(P,A1,A2,A3,A4).
xcall(P,A1,A2,A3):- call(P,A1,A2,A3).
xcall(P,A1,A2):- call(P,A1,A2).
xcall(P,A1):- call(P,A1).
xcall(P):- call(P).

assertion_t([AH,P|LIST]):-is_holds_true(AH),!,assertion_t([P|LIST]).
assertion_t([AH,P|LIST]):-is_holds_false(AH),!,assertion_f([P|LIST]).
% todo hook into loaded files!
assertion_t(_):-not(useExternalDBs),!,fail.
assertion_t([P|LIST]):- tiny_kb:'ASSERTION'(':TRUE-DEF',_,_UniversalVocabularyMt,_Vars,/*HL*/[P|LIST]).
assertion_t([P|LIST]):-tiny_kb:'ASSERTION'(':TRUE-MON',_,_UniversalVocabularyMt,_Vars,/*HL*/[P|LIST]).
assertion_t([P|LIST]):- append([assertion_holds_mworld0,P|LIST],[_,_],CallList),Call=..CallList, '@'(xcall(Call),mworld0).
assertion_t([P|LIST]):- Call=..[assertion_holds,P|LIST], '@'(xcall(Call),hl_holds).

% ================================================================================
% end holds_t
% ================================================================================


% ================================================================================
% begin holds_f
% ================================================================================
holds_f(P,A1,A2,A3,A4,A5,A6,A7):- isCycPredArity_NE(P,7),(call_f(P,A1,A2,A3,A4,A5,A6,A7);call_f_mt(P,A1,A2,A3,A4,A5,A6,A7,_,_);assertion_f([P,A1,A2,A3,A4,A5,A6,A7])).
holds_f(P,A1,A2,A3,A4,A5,A6):- isCycPredArity_NE(P,6),(call_f(P,A1,A2,A3,A4,A5,A6);call_f_mt(P,A1,A2,A3,A4,A5,A6,_,_)).
holds_f(P,A1,A2,A3,A4,A5):-isCycPredArity_NE(P,5),(call_f(P,A1,A2,A3,A4,A5);call_f_mt(P,A1,A2,A3,A4,A5,_,_)).
holds_f(P,A1,A2,A3,A4):- isCycPredArity_NE(P,4),(call_f(P,A1,A2,A3,A4);call_f_mt(P,A1,A2,A3,A4,_,_)).
holds_f(P,A1,A2,A3):-isCycPredArity_NE(P,3),(call_f(P,A1,A2,A3);call_f_mt(P,A1,A2,A3,_,_)).
holds_f(P,A1,A2):-isCycPredArity_NE(P,2),(call_f(P,A1,A2);call_f_mt(P,A1,A2,_,_)).
holds_f(P,A1):-isCycPredArity_NE(P,1),(call_f(P,A1);call_f_mt(P,A1,_,_)).


holds_f([AH,P|LIST]):-is_holds_true(AH),!,holds_f_p2(P,LIST).
holds_f([AH,P|LIST]):-is_holds_false(AH),!,holds_t_p2(P,LIST).
holds_f([P|LIST]):-!,holds_f_p2(P,LIST).
holds_f(CALL):-CALL=..[P|LIST],holds_f([P|LIST]).
holds_f_p2(P,LIST):- xcall(((CALL=..[holds_f,P|LIST],xcall(CALL)))).

call_f(P,A1,A2,A3,A4,A5,A6,A7):- xcall(((callable_tf(P,7),List= [P,A1,A2,A3,A4,A5,A6,A7], CALL=..List,(assertion_f(List);dbase_f(CALL);xcall_not(CALL))))).
call_f(P,A1,A2,A3,A4,A5,A6):- xcall(((dbase_f(P,A1,A2,A3,A4,A5,A6)))).
call_f(P,A1,A2,A3,A4,A5,A6):- xcall(((assertion_f([P,A1,A2,A3,A4,A5,A6])))).
call_f(P,A1,A2,A3,A4,A5,A6):- xcall(((callable_tf(P,6),xcall_not(P,A1,A2,A3,A4,A5,A6)))).
call_f(P,A1,A2,A3,A4,A5):- xcall(((dbase_f(P,A1,A2,A3,A4,A5)))).
call_f(P,A1,A2,A3,A4,A5):- xcall(((assertion_f([P,A1,A2,A3,A4,A5])))).
call_f(P,A1,A2,A3,A4,A5):- xcall(((callable_tf(P,5),xcall_not(P,A1,A2,A3,A4,A5)))).
call_f(P,A1,A2,A3,A4):- xcall((( dbase_f(P,A1,A2,A3,A4)))).
call_f(P,A1,A2,A3,A4):- xcall(((assertion_f([P,A1,A2,A3,A4])))).
call_f(P,A1,A2,A3,A4):- xcall(((callable_tf(P,4),xcall_not(P,A1,A2,A3,A4)))).
call_f(P,A1,A2,A3):- xcall(((dbase_f(P,A1,A2,A3)))).
call_f(P,A1,A2,A3):- xcall(((assertion_f([P,A1,A2,A3])))).
call_f(P,A1,A2,A3):- xcall(((callable_tf(P,3),xcall_not(P,A1,A2,A3)))).
call_f(P,A1,A2):- xcall(((dbase_f(P,A1,A2)))).
call_f(P,A1,A2):- xcall((( assertion_f([P,A1,A2])))).
call_f(P,A1,A2):- xcall(((callable_tf(P,2),xcall_not(P,A1,A2)))).
call_f(P,A1):- xcall(((dbase_f(P,A1)))).
call_f(P,A1):- xcall(((assertion_f([P,A1])))).
call_f(P,A1):- xcall(((callable_tf(P,1),xcall_not(P,A1)))).

call_f_mt(P,A1,A2,A3,A4,A5,A6,A7,A8,A9):- useDBMts, callable_tf(P,9),CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8,A9],xcall_not(CALL).
call_f_mt(P,A1,A2,A3,A4,A5,A6,A7,A8):- useDBMts,  callable_tf(P,8),CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8],xcall_not(CALL).
call_f_mt(P,A1,A2,A3,A4,A5,A6,A7):- useDBMts, callable_tf(P,7),CALL=..[P,A1,A2,A3,A4,A5,A6,A7],xcall_not(CALL).
call_f_mt(P,A1,A2,A3,A4,A5,A6):- useDBMts, callable_tf(P,6),xcall_not(P,A1,A2,A3,A4,A5,A6).
call_f_mt(P,A1,A2,A3,A4,A5):- useDBMts, callable_tf(P,5),xcall_not(P,A1,A2,A3,A4,A5).
call_f_mt(P,A1,A2,A3,A4):- useDBMts,  callable_tf(P,4),xcall_not(P,A1,A2,A3,A4).
call_f_mt(P,A1,A2,A3):- useDBMts, callable_tf(P,3),xcall_not(P,A1,A2,A3).

xcall_not(P,A1,A2,A3,A4,A5,A6,A7,A8,A9):- CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8,A9],\+ xcall(CALL).
xcall_not(P,A1,A2,A3,A4,A5,A6,A7,A8):- CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8],\+ xcall(CALL).
xcall_not(P,A1,A2,A3,A4,A5,A6,A7):- CALL=..[P,A1,A2,A3,A4,A5,A6,A7],\+ xcall(CALL).
xcall_not(P,A1,A2,A3,A4,A5,A6):- \+ xcall(P,A1,A2,A3,A4,A5,A6).
xcall_not(P,A1,A2,A3,A4,A5):- \+ xcall(P,A1,A2,A3,A4,A5).
xcall_not(P,A1,A2,A3,A4):- \+ xcall(P,A1,A2,A3,A4).
xcall_not(P,A1,A2,A3):- \+ xcall(P,A1,A2,A3).
xcall_not(P,A1,A2):- \+ xcall(P,A1,A2).
xcall_not(P,A1):- \+ xcall(P,A1).
xcall_not(P):- \+ xcall(P).

assertion_f([AH,P|LIST]):-is_holds_true(AH),!,assertion_f([P|LIST]).
assertion_f([AH,P|LIST]):-is_holds_false(AH),!,assertion_f([P|LIST]).
% todo hook into loaded files!
assertion_f(_):-not(useExternalDBs),!,fail.
assertion_f([P|LIST]):-tiny_kb:'ASSERTION'(':FALSE-DEF',_,_UniversalVocabularyMt,_Vars,/*HL*/[P|LIST]).
assertion_f([P|LIST]):-tiny_kb:'ASSERTION'(':FALSE-MON',_,_UniversalVocabularyMt,_Vars,/*HL*/[P|LIST]).


% ================================================================================
% end holds_f 
% ================================================================================



arityMatches(A,S-E):- !, system:between(S,E,OTHER), A=OTHER.
arityMatches(A,OTHER):-number(OTHER),!, A=OTHER.

isCycPredArity_NE(P,A):-ignore(isCycPredArity(P,A)).


isCycPredArity(P,A):-nonvar(P),!,isCycPredArity0(P,OTHER),!,arityMatches(A,OTHER).
isCycPredArity(P,A):- isCycPredArity0(P,OTHER),arityMatches(A,OTHER).

arity_pred(P):-nonvar(P),member(P,[arity,arityMax,arityMin]).

isCycPredArity0(P,A):- arity_pred(P),!,A=2.
isCycPredArity0(P,A):- isRegisteredCycPred(_,P,A).
isCycPredArity0(P,A):- xcall(holds_t(arity,P, A)).
isCycPredArity0(P,A):- xcall(holds_t(arityMax,P, A)).
isCycPredArity0(holds,7):-!.
isCycPredArity0(P,_):- is_2nd_order_holds(P),!,fail.
isCycPredArity0(P,A):- xcall(((holds_t(arityMin,P, A),not(holds_t(arityMax,P, _))))).

     
%list_to_term(X,Y):- balanceBinding(X,Y).
list_to_term(X,Y):-nonvar(X),var(Y),!,list_to_terms_lr(X,Y).
list_to_term(X,Y):-list_to_terms_rl(X,Y).
list_to_terms_rl(List,(A,B)):-list_to_terms_rl(A,AL),list_to_terms_rl(B,BL),append(AL,BL,List).
list_to_terms_rl(List,(A;B)):-list_to_terms_rl(A,AL),list_to_terms_rl(B,BL),append(AL,[or|BL],List).
list_to_terms_lr([],true):-!.
list_to_terms_lr([T],T):-!.
list_to_terms_lr([H|T],(H,TT)):-!,list_to_terms_lr(T,TT).


ah(P,A1,A2,A3,A4,A5,A6,A7):- dmsg(ah(P,7,A1,A2,A3,A4,A5,A6,A7)).
ah(P,A1,A2,A3,A4,A5,A6):- dmsg(ah(P,6,A1,A2,A3,A4,A5,A6)).
ah(P,A1,A2,A3,A4,A5):- dmsg(ah(P,5,A1,A2,A3,A4,A5)).
ah(P,A1,A2,A3,A4):- dmsg(ah(P,4,A1,A2,A3,A4)).
ah(P,A1,A2,A3):- dmsg(ah(P,3,A1,A2,A3)).
ah(P,A1,A2):- dmsg(ah(P,2,A1,A2)).
ah(P,A1):- dmsg(ah(P,1,A1)).



:- include(logicmoo(vworld/dbase_i_db_preds)).

%:- style_check(-singleton).
:- style_check(+discontiguous).
%:- style_check(-atom).
% :- style_check(-string).

 
scan_arities:- forall(holds_t(arity,F,A),moodb:decl_mpred(F,A)).

:- include(logicmoo(vworld/dbase_i_cyc)).

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

% done in 'user' to avoid reloading when we reload dbase
ensure_q_loaded(File):-
    expand_file_search_path(logicmoo('pldata/mworld0_declpreds.pl'),Path),exists_file(Path),!,                                 
   '@'(load_files(File,[if(not_loaded),qcompile(auto),expand(true),derived_from(Path)]),user).

make_qlfs:-
 ensure_q_loaded(logicmoo('pldata/tiny_kb')),
 ensure_q_loaded(logicmoo('pldata/nldata_freq_pdat')),
 ensure_q_loaded(logicmoo('pldata/nldata_BRN_WSJ_LEXICON')),
 ensure_q_loaded(logicmoo('pldata/nldata_colloc_pdat')),
 ensure_q_loaded(logicmoo('pldata/nldata_cycl_pos0')),
 ensure_q_loaded(logicmoo('pldata/nldata_dictionary_some01')),
 ensure_q_loaded(logicmoo('pldata/tt0_00022_cycl')),
 ensure_q_loaded(logicmoo('pldata/hl_holds')),
 ensure_q_loaded(logicmoo('pldata/mworld0')),
 ensure_q_loaded(logicmoo('pldata/mworld0_declpreds')),
 catch(ensure_q_loaded(logicmoo('pldata/withvars_988')),_,true).

:- catch(logicmoo('pldata/mworld0_declpreds.qlf'),_,make_qlfs).


/*

 First time you run this 2 million clauses are qcompiled 
 (I've excluded 7 million more clauses that are only available with spec ial C Y C  Liciens ing)

%     /devel/logicmoo/src_data/pldata/tiny_kb.pl *qcompiled* into tiny_kb 2.40 sec, 8,481 clauses
%     /devel/logicmoo/src_data/pldata/nldata_freq_pdat.pl *qcompiled* into nldata_freq_pdat 7.88 sec, 107,704 clauses
%     /devel/logicmoo/src_data/pldata/nldata_BRN_WSJ_LEXICON.pl *qcompiled* into nldata_BRN_WSJ_LEXICON 7.65 sec, 113,863 clauses
%     /devel/logicmoo/src_data/pldata/nldata_colloc_pdat.pl *qcompiled* into nldata_colloc_pdat 6.31 sec, 64,081 clauses
%     /devel/logicmoo/src_data/pldata/nldata_cycl_pos0.pl *qcompiled* into nldata_cycl_pos0 0.20 sec, 2,488 clauses
%     /devel/logicmoo/src_data/pldata/nldata_dictionary_some01.pl *qcompiled* into nldata_dictionary_some01 0.03 sec, 293 clauses
%     /devel/logicmoo/src_data/pldata/tt0_00022_cycl.pl *qcompiled* into tt0_00022_cycl 26.86 sec, 313,234 clauses
%     /devel/logicmoo/src_data/pldata/hl_holds.pl *qcompiled* into hl_holds 175.31 sec, 1,041,317 clauses
%     /devel/logicmoo/src_data/pldata/mworld0_declpreds.pl *qcompiled* into dbase 0.05 sec, 680 clauses
%     /devel/logicmoo/src_data/pldata/mworld0.pl *qcompiled* into mworld0 60.49 sec, 483,046 clauses

  It took several minutes on my 24 core machine with 128gb ram on all SSDs as you can see.. 

  But afterwards (the results next) .. it is able to load the system from .qlf in a mater of under 3 seconds!

  No other SQL clone has been able to beat this .. Prolog uses 80% less ram and 10x times faster than
    any SQL indexing strategy I've for a large database (wtf? secret is all atoms are keys)  
   (The atom table (pointers to strings) is of no interest/use during join ops obviouslly.. 
     in which i have to do millions of join ops per semantic parse)

%     logicmoo('pldata/tiny_kb') loaded into tiny_kb 0.02 sec, 9,016 clauses
%     logicmoo('pldata/nldata_freq_pdat') loaded into nldata_freq_pdat 0.10 sec, 107,709 clauses
%     logicmoo('pldata/nldata_BRN_WSJ_LEXICON') loaded into nldata_BRN_WSJ_LEXICON 0.09 sec, 113,868 clauses
%     logicmoo('pldata/nldata_colloc_pdat') loaded into nldata_colloc_pdat 0.06 sec, 64,086 clauses
%     logicmoo('pldata/nldata_cycl_pos0') loaded into nldata_cycl_pos0 0.00 sec, 2,479 clauses
%     logicmoo('pldata/nldata_dictionary_some01') loaded into nldata_dictionary_some01 0.00 sec, 264 clauses
%     logicmoo('pldata/tt0_00022_cycl') loaded into tt0_00022_cycl 0.28 sec, 313,287 clauses
%     logicmoo('pldata/hl_holds') loaded into hl_holds 1.31 sec, 1,041,321 clauses
%     logicmoo('pldata/mworld0_declpreds') loaded into dbase 0.00 sec, 679 clauses
%     logicmoo('pldata/mworld0') loaded into mworld0 0.60 sec, 483,058 clauses

*/

user_export(_):- dbase_mod(user),!.
user_export(_M:Prop/Arity):-!,user_export(Prop/Arity).
user_export(Prop/Arity):- 
   dbase_mod(M), '@'( ((export(Prop/Arity),dynamic(Prop/Arity))) , M).


:- meta_predicate hooked_asserta(^), hooked_assertz(^), hooked_retract(^), hooked_retractall(^).
% % :- meta_predicate del(0),clr(0),add(0),req(0), db_op(0,0,0),moo:db_prop(0,0).

% Found new meta-predicates in iteration 1 (0.281 sec)
%:- meta_predicate db_forall(?,?,?,0).

:- include(dbase_types_motel).

% :- moodb:register_module_type(utility).

dbase_define_db_prop(P):-dbase_define_db_prop(P,[]).
dbase_define_db_prop(M:ArgTypes,PropTypes):-!,
   dbase_define_db_prop(ArgTypes,[module(M)|PropTypes]),!.
   
dbase_define_db_prop(ArgTypes,PropTypes):-
   functor(ArgTypes,F,A),
   doall(define_db_prop_0(ArgTypes,F,A)),
   doall(define_db_prop_1(ArgTypes,F,A,PropTypes)),
   doall(define_db_prop_2(ArgTypes,F,A,PropTypes)),
   doall(define_db_prop_3(ArgTypes,F,A,PropTypes)).

define_argType(F,A,N,ArgType):-moodb:add_db_prop(F,A,argIsa(N,ArgType)).

% pass 0 = assert arity and argTypes
define_db_prop_0(ArgTypes,F,A):-moodb:add_db_prop(F,A,arity(F,A)),fail.
define_db_prop_0(ArgTypes,F,A):-moodb:add_db_prop(F,A,argTypes(ArgTypes)),fail.
define_db_prop_0(ArgTypes,F,A):-doall(forall(arg(N,ArgTypes,ArgType),define_argType(F,A,N,ArgType))),fail.

% pass 1 = assert misc properties
define_db_prop_1(W,F,A,PT):-moodb:add_db_prop(F,A,PT),!,forall(member_or_e(E,PT),define_db_prop_3(W,F,A,PT)).

% pass 2 = add extensional hooks
define_db_prop_2(W,F,A,_):- doall(forall(moodb:is_db_prop(F,A,CL),ignore(define_db_prop_3(W,F,A,CL)))).

% pass 3 = re-add extensional hooks
define_db_prop_3(_,Pred,Arity,isRegisteredCycPred(_,Pred,Arity)):- moodb:decl_mpred(Pred,Arity).
define_db_prop_3(_,Pred,Arity,cyc):-moodb:decl_mpred(Pred,Arity).
define_db_prop_3(_,F,A,module(Module)):-not(dbase_mod(Module)),functor(HEAD,F,A),must(predicate_property(Module:HEAD,_)),!.
define_db_prop_3(_,F,A,module(Module)):-dbase_mod(Module),!,declare_dbase_local(F,A).
define_db_prop_3(_,F,A,arity(F,A)):- 
   not((moodb:is_db_prop(F,A,module(Module)),not(dbase_mod(Module)))),!,
      declare_dbase_local(F,A).
define_db_prop_3(_,_,_,_).

declare_dbase_local(M:F,A):- dbase_mod(M),!,declare_dbase_local(F,A).
declare_dbase_local(_M:F,A):- !, trace, declare_dbase_local(F,A).
declare_dbase_local(F,A):- declare_dbase_local_1(F,A).


declare_dbase_local_1(F,A):- moodb:is_db_prop(F,A,hasStub),!.

declare_dbase_local_1(F,A):- once(moodb:add_db_prop(F,A,hasStub)),fail.

declare_dbase_local_1(F,A):- functor(Pred,F,A), get_module_of(Pred,Module),declare_dbase_local_1(Module, Pred, F,A).

declare_dbase_local_1(Module, Pred, F,A):-
   not(dbase_mod(Module)),!,
   dbase_mod(DBModule),
   rem_db_prop(F,A,module(DBModule)),
   moodb:add_db_prop(F,A,module(Module)).

declare_dbase_local_1(Module, Pred, F,A):- 
   dbase_mod(Module),!,decl_mpred(F,A).
   %%'@'(((  dynamic( F/A ), Module:export( F/A )   )),Module ),!. 


scan_db_prop:-
   dbase_mod(DBM),debug,
   ignore(('@'(forall((db_prop_argsIsa(ArgTypes),functor(ArgTypes,F,A)),call( dbase_define_db_prop(ArgTypes,[arity(F,A)]))),DBM))),
   ignore(('@'(forall(db_prop_findall(ArgTypes,PropTypes),call( dbase_define_db_prop(ArgTypes,PropTypes))),DBM))),
   !.
scan_db_prop:-!.




term_listing(Obj):- nonvar(var(Obj)),catch(listing(Obj),_,fail),fail.
term_listing(Obj):- 
    term_to_atom(Obj,HO),
   get_functor(Obj,F),
   doall((
   predicate_property(H,number_of_clauses(_)),
   functor(H,HF,_),
   HF\==F,
   clause(H,B),
   use_term_listing(Obj,HO,H,B),
   show_term_listing(H,B),
   fail)).

use_term_listing(Obj,HO,H,B):- term_to_atom((H:-B),HBO), sub_atom_icasechk(HBO,_,HO),!.
use_term_listing(Obj,HO,H,B):- not(not(((subst((H:-B),Obj,fov,H1B1), H1B1 \= (H:-B))))),!.


show_term_listing(H,true):- !, show_term_listing(H).
show_term_listing(H,B):- show_term_listing((H:-B)).

show_term_listing(H):- writeq(H),write('.'),nl,!.



% replaced the 1st with the 2nd and better version of retract
% del(C0):-db_op(r,C0)
%% del(RetractOne)    <--  del(C0):- ignore((db_op('q',C0),!,db_op('r',C0))).
del(C0):- (db_op('r',C0) -> db_op('ra',C0) ; dmsg(failed(del(C0)))).
%% clr(Retractall)
clr(C0):-db_op(ra,C0).
%% req(Query)
req(C0):- db_forall_query(C0).
%% props(Obj,QueryPropSpecs)
props(Obj,PropSpecs):-db_op('q',props(Obj,PropSpecs)).
%% add(Assertion)
add(C0):- db_op(a,C0).
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
with_kb_assertions(With,Call):-
   setup_call_cleanup(asserta(With,Ref),Call,erase(Ref)).



world_clear(Named):-dmsg('Clearing world database: ~q.',[Named]).

pred_as_is(F,A):-moodb:is_db_prop(F,A,flag),!.
pred_as_is(F,A):-moodb:is_db_prop(F,A,module(_)),!.
pred_as_is(p,_):-!,fail.
pred_as_is(k,_):-!,fail.


holds_tcall(Call):-req(Call).

db_op( q,Term):-!,db_op0(q,Term).
db_op( r,Term):-!,db_op0(q,Term),!,db_op0(ra,Term).
db_op(Op,Term):-must(db_op0(Op,Term)).

db_op0(_Op,props(_Obj,NotOpen)):-NotOpen==[],!.
db_op0(Op,props(Obj,Open)):-var(Open),throw((db_op0(Op,props(Obj,Open)))).
db_op0(Op,props(Obj,[P|ROPS])):-!,db_op(Op,props(Obj,P)),db_op(Op,props(Obj,ROPS)).
db_op0(Op,props(Obj,PropVal)):- !,PropVal=..[Prop|Vals],
	Call=..[Prop,Obj|Vals], db_op(Op,Call).

db_op0(Op,C0):-functor(C0,F,A),db_op_4(Op,F,A,C0),!.

db_op_4(Op,:,2,_MODULE:C0):-!,/*throw(module_form(MODULE:C0)),*/
  functor(C0,F,A),
  dmsg(todo(unmodulize(F/A))),
  db_op(Op,C0).
db_op_4(Op,k,_,C0):- C0=..[k,Prop|ARGS],atom(Prop), C1=..[Prop|ARGS],!,db_op(Op,C1).
db_op_4(Op,p,_,C0):- C0=..[p,Prop|ARGS],atom(Prop), C1=..[Prop|ARGS],!,db_op(Op,C1).
db_op_4(Op,k,_,C0):- C0=..[k,Prop|ARGS],atom(Prop),C1=..[Prop|ARGS],!,db_op(Op,C1).
db_op_4(Op,p,_,C0):- C0=..[p,Prop|ARGS],atom(Prop), C1=..[Prop|ARGS],!,db_op(Op,C1).
db_op_4(Op,svo,_,C0):- C0=..[svo,Obj,Prop|ARGS],atom(Prop),C1=..[Prop,Obj|ARGS],!,db_op(Op,C1).

db_op_4(q,Fmt,1,C0):-is_ft(Fmt),!,C0=..[_,A],format_complies(A,Fmt,_).
db_op_4(a,Fmt,1,_C0):-is_ft(Fmt),!,dmsg(todo(dont_assert_is_ft(Fmt))),!.
db_op_4(Op,Prop,_,C0):- 
   C0=..[Prop|ARGS],db_op_disj(Op,Prop,ARGS).

db_op_unit(_Op,Prop,ARGS,C0):- must(atom(Prop)), C0=..[Prop|ARGS].

% impl/1
db_op_disj(Op,Prop,ARGS):-moodb:is_db_prop(Prop,_,impl(Other)),!,
	db_op_unit(Op,Other,ARGS,Unit),!,db_forall(Op,Unit).
% alias/1
db_op_disj(Op,Prop,ARGS):-moodb:is_db_prop(Prop,_,alias(Other)),!,
	db_op_unit(Op,Other,ARGS,Unit),!,db_forall(Op,Unit).
% inverse/1
db_op_disj(Op,Prop,ARGS):-
      moodb:is_db_prop(Prop,_,inverse(Other)),!,
      inverse_args(ARGS,Inverse),
      db_op_unit(Op,Other,Inverse,Unit1),
      db_op_unit(Op,Prop,ARGS,Unit2),
      db_forall(Op,(Unit1;Unit2)).


moodb:is_db_prop(F,A,Prop):-isRegisteredCycPred(_,F,A),functor(P,F,A),dbase:db_prop_findall(P,Props),member(Prop,Props).

% assert/1
db_op_disj(a,Prop,ARGS):- moodb:is_db_prop(Prop,_,assert(How)),!,call_pa(How,Prop,ARGS).
db_op_disj(Op,Prop,ARGS):- moodb:is_db_prop(Prop,W,assert(How)),!,throw(cant(db_op_disj(Op,Prop,ARGS),moodb:is_db_prop(Prop,W,assert(How)))).

% plain prop
db_op_disj(Op,Prop,ARGS):-db_op_unit(Op,Prop,ARGS,Unit),db_forall(Op,Unit).

call_pa(How,Prop,ARGS):-PROPARGS=..[Prop|ARGS], Call=..[How,PROPARGS],must(Call).

db_forall('q', Term):- !,db_forall_query(Term).

db_forall(Op,(C1,C2)):-!,db_forall(Op,C1),db_forall(Op,C2).
db_forall(r,(C1;C2)):-!,trace,once((db_forall(r,C1),db_forall(r,C2))).
db_forall(ra,(C1;C2)):-!,must(db_forall(ra,C1)),must(db_forall(ra,C2)).
db_forall(a,(C1;C2)):-!,db_forall(a,C1),!,db_forall(a,C2),!.
db_forall(u,C):- grtrace,db_forall_quf(C,U,Template),U,Template,must(ground(Template)),!,ignore(retractall(Template)).
db_forall(ra,C):-db_forall_quf(C,U,Template), doall((U,retractall(Template))).
db_forall(ra,C):-ignore(retractall(C)).
db_forall(a,C0):- db_forall_quf(C0,U,C),must(U),functor(C,F,A),!, (moodb:is_db_prop(F,A,singleValued) -> must(db_forall_assert_sv(C,F,A)) ; must(db_forall_assert_mv(C,F,A))).

db_forall(a,C):- functor(C,F,A),!, (moodb:is_db_prop(F,A,singleValued) -> must(db_forall_assert_sv(C,F,A)) ; must(db_forall_assert_mv(C,F,A))).
db_forall(r,C):- ground(C),retractall(C),!.
db_forall(Op,C):-!,trace,throw(unhandled(db_forall(Op,C))).


db_forall_query((C0->C1;C2)):- !, (db_forall_query(C0) -> db_forall_query(C1) ; db_forall_query(C2)).
db_forall_query((C1;C2)):-!, db_forall_query(C1) ; db_forall_query(C2).
db_forall_query(Term):-findall(a(Term),db_forall_query0(Term),List),list_to_set_safe(List,Set),!,member(a(Term),Set).

db_forall_query0(Term):- expand_goal(Term,Term2),db_forall_query1(Term2).
db_forall_query1(C):- not(not(predicate_property(C,_PP))),!,call(C).
db_forall_query1(C):- grtrace, db_forall_quf(C,Pretest,Template),!,Pretest, call(Template).


singletons_throw_or_fail(C):- throw_if_true_else_fail(contains_singletons(C),C).
nonground_throw_or_fail(C):- throw_if_true_else_fail(not(ground(C)),C).

% only place ever should actual game dbase be changed from
hooked_asserta(C):- singletons_throw_or_fail(hooked_asserta(C)).
hooked_asserta(C):- moodb:run_database_hooks(assert(a),C),asserta_cloc(C).
hooked_assertz(C):- singletons_throw_or_fail(hooked_assertz(C)).
hooked_assertz(C):- run_database_hooks(assert(z),C), assertz_cloc(C).
hooked_retract(C):- nonground_throw_or_fail(hooked_retract(C)).
hooked_retract(C):- run_database_hooks(retract(one),C), must(retract_cloc(C)).
hooked_retractall(C):- run_database_hooks(retract(all),C), retractall_cloc(C).

ensure_db_predicate(Op,In,Out):-ensure_db_predicate_1(Op,In,Out),!.
ensure_db_predicate(Op,In,Out):-throw(ensure_db_predicate(Op,In,Out)).

ensure_db_predicate_1(AR,M:C,G):- nonvar(M), !,ensure_db_predicate_1(AR,C,G).
ensure_db_predicate_1(AR,CI,G):- expand_goal(CI,C),ensure_db_predicate_2(AR,C,G).

ensure_db_predicate_2(AR,M:C,G):- nonvar(M),ensure_db_predicate_2(AR,C,G).
ensure_db_predicate_2(_AR,CI,G):- expand_term(CI,G).

% ensure_db_predicate(a,agent('NpcCol1000-Geordi684'),Out).

prepend_module(_:C,M,M:C):-!.
prepend_module(C,M,M:C).

make_predicate(DBASE,F,A):-dbase_mod(DBASE),!,decl_mpred(F,A),!.
make_predicate(M,F,A):- T =  '@'((dynamic(F/A)),M),ground(T),!,T.
make_predicate(M,F,A):- dynamic(F/A).

retract_cloc(C):- ensure_db_predicate(r,C,G), show_cgoal(retract(G)).
retractall_cloc(C):- ensure_db_predicate(r,C,G), show_cgoal(retractall(G)).
asserta_cloc(C):- ensure_db_predicate(a,C,G), show_cgoal(asserta(G)).
assertz_cloc(C):- ensure_db_predicate(a,C,G), debugOnError(show_cgoal(assertz(G))).

show_cgoal(G):- !, G.
show_cgoal(G):-dmsg(show_goal(G)),call(G).

% assert to a mutlivalue pred
db_forall_assert_mv(C,F,A):-
   (clause_present(C,F,A) -> true; (moodb:is_db_prop(F,A,ordered)-> hooked_assertz(C) ; hooked_asserta(C))).

term_parts(A,[A]):- not(compound(A)),!.
term_parts([A|L],TERMS):-!,term_parts_l([A|L],TERMS).
term_parts(Comp,[P/A|TERMS]):- functor(Comp,P,A), Comp=..[P|List],term_parts_l(List,TERMS).

term_parts_l(Var,[open(Var),Var]):-var(Var),!.
term_parts_l([],[]):-!.
term_parts_l([A|L],TERMS):-!,term_parts(A,AP),term_parts_l(L,LP),append(AP,LP,TERMS).
term_parts_l(Term,[open(Term)|TERMS]):-term_parts(Term,TERMS),!.

pred_term_parts(Pred,A,[A]):- call(Pred,A),!.
pred_term_parts(Pred,[A|L],TERMS):-!,pred_term_parts_l(Pred,[A|L],TERMS).
pred_term_parts(Pred,Comp,TERMS):-Comp=..[P,A|List],pred_term_parts_l(Pred,[P,A|List],TERMS).
pred_term_parts(_,Term,[]).

pred_term_parts_l(_,NV,[]):-NV==[],!.
pred_term_parts_l(Pred,[A|L],TERMS):-!,pred_term_parts(Pred,A,AP),pred_term_parts_l(Pred,L,LP),append(AP,LP,TERMS).
pred_term_parts_l(Pred,Term,TERMS):-pred_term_parts(Pred,Term,TERMS),!.
pred_term_parts_l(_,Term,[]).

throw_if_true_else_fail(T,E):- once(hotrace(T)),throw(throw_if_true_else_fail(E:T)).

list_retain(PL,Pred,Result):- throw_if_true_else_fail(not(is_list(PL)),list_retain(PL,Pred,Result)).
list_retain([],_Pred,[]):-!.
list_retain([R|List],Pred,[R|Retained]):- call(Pred,R),!, list_retain(List,Pred,Retained).
list_retain([_|List],Pred,Retained):- list_retain(List,Pred,Retained).

identical_member(X,[Y|_])  :-
	X == Y,
	!.
identical_member(X,[_|L]) :-
	identical_member(X,L).

contains_singletons(Term):-hotrace(pred_term_parts(var,Term,Vars1)),
   term_variables(Term,Vars2),
   member(Var,Vars2),findall(Var,identical_member(Var,Vars1),List),length(List,1).

% assert to a singlevalue pred
db_forall_assert_sv(C,F,A):- throw_if_true_else_fail(contains_singletons(C),db_forall_assert_sv(C,F,A)).
db_forall_assert_sv(C,F,A):- clause_present(C,F,A),!.
db_forall_assert_sv(C0,F,A):- 
   copy_term(c(_,C0),c(_,C)),
      arg(A,C,Update),
      ignore(db_forall_assert_sv_old_value(C,A,OLD)),
      update_value(OLD,Update,NEW),
      must(nonvar(NEW)),
      setarg(A,C,NEW),      
      must_asserta(C).

db_forall_assert_sv_old_value(C,A,OLD):-
   copy_term(C,CC),
   must(var(OLD)),
      setarg(A,CC,OLD),
      % this binds or leave OLD
      ((CC -> hooked_retract(CC) ; OLD= _ )),!.

pl_arg_type(Arg,Type):-
      var(Arg) -> Type =var;
      integer(Arg) -> Type =integer;
      number(Arg) -> Type =float;
      string(Arg) -> Type =string;
      atom(Arg) -> Type =atom;
      is_list(Arg) -> Type =list;
      compound(Arg) -> Type =compound;
         Arg = Type.


:-thread_local clause_present_lookup_local/3.

clause_present(C):-hotrace((functor_safe(C,F,A),clause_present(C,F,A))).
clause_present(C,F,1):-C=..[F,A], is_ft(F), format_complies(A,F,_).
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
%clause_present_1(C,F,A):- A>1, arg(A,C,NEW),snonvar(NEW),!,setarg(A,C,OLD),clause_present(C,F,A),pl_arg_type(NEW,string),string_chars(NEW,[S|C1]),string_chars(OLD,[S|C2]),C1=C2,dmsg(present(C)).


must_asserta(C):- nonground_throw_or_fail(C).
must_asserta(C):- must(ground(C)), must(hooked_asserta(C)),!.
      
argIsa_call(Prop,N1,Type):-once((must(nonvar(Prop)),must(number(N1)))),fail.
argIsa_call(Prop,N1,Type):-argIsa_call_0(Prop,N1,Type),!.
argIsa_call(_:Prop,N1,Type):-!,argIsa_call(Prop,N1,Type).
argIsa_call(Prop,N1,Type):-argIsa_call_1(Prop,N1,Type),!.

argIsa_call_0(mud_isa,1,argIsaFn(mud_isa,1)):-!.
argIsa_call_0(mud_isa,2,type):-!.
argIsa_call_0(act,_,term):-!.
argIsa_call_0(ofclass,2,type):-!.
argIsa_call_0(memory,2,term):-!.
argIsa_call_0(Prop,N1,Type):- moodb:is_db_prop(Prop,_,argIsa(N1,Type)),!.

argIsa_call_1(Prop,N1,Type):- is_2nd_order_holds(Prop),trace,dmsg(todo(define(argIsa_call(Prop,N1,'Second_Order_TYPE')))),
   Type=argIsaFn(Prop,N1).
argIsa_call_1(Prop,N1,Type):-dmsg(todo(define(argIsa_call(Prop,N1,'_TYPE')))),
   Type=argIsaFn(Prop,N1).

db_forall_quf(C,Pretest,Template):- C=..[Prop,OBJ|ARGS],
      translate_args(Prop,OBJ,2,ARGS,NEWARGS,true,Pretest),
      Template =.. [Prop,OBJ|NEWARGS].

translate_args(_Prop,_OBJ,_N,[],[],GIN,GIN).
translate_args(Prop,OBJ,N1,[ARG|S],[NEW|ARGS],GIN,GOALS):-
   argIsa_call(Prop,N1,Type),
   translateOneArg(Prop,OBJ,Type,ARG,NEW,GIN,GMID),
   N2 is N1 +1,
   translate_args(Prop,OBJ,N2,S,ARGS,GMID,GOALS).

comparitiveOp((\=)).
comparitiveOp((\==)).
comparitiveOp((=)).
comparitiveOp((=:=)).
comparitiveOp((==)).
comparitiveOp((<)).
comparitiveOp((>)).
comparitiveOp((=<)).
comparitiveOp((>=)).

additiveOp((is)).
additiveOp((*)).
additiveOp(+).
additiveOp(-).
additiveOp((/)).

% var
translateOneArg(_Prop,_O,_Type,VAR,VAR,G,G):-var(VAR),!.

% not an expression
translateOneArg(_Prop,_O,_Type,VAR,VAR,G,G):-atomic(VAR),!.

% translateOneArg(_Prop,_O,Type,VAR,VAR,G,G):-ignore(mud_isa(VAR,Type)),!.

% props(Obj,size < 2).
translateOneArg(Prop,O,Type,ARG,OLD,G,(GETTER,COMPARE,G)):-
       functor(ARG,F,2), comparitiveOp(F),!,
       ARG=..[F,Prop,VAL],
       GETTER=..[Prop,O,OLD],
       COMPARE= compare_op(Type,F,OLD,VAL),!.

% props(Obj,oneOf(Sz,[size+1,2])).
translateOneArg(Prop,O,Type,oneOf(VAL,LIST),VAL,G,(GO,G)):-
   translateListOps(Prop,O,Type,VAL,LIST,G,GO).

% padd(Obj,size + 2).
translateOneArg(Prop,O,_Type,ARG,NEW,G,(GETTER,STORE,G)):-
       functor(ARG,F,2), additiveOp(F),!,
       ARG=..[F,Prop,VAL],
       GETTER=..[Prop,O,OLD],
       STORE= update_value(OLD,VAL,NEW),!.

translateListOps(_Prop,_O,_Type,_VAL,[],G,G).
translateListOps(Prop,O,Type,VAL,[L|LIST],G,GO2):-
   translateOneArg(Prop,O,Type,L,VAL,G,GO),
   translateListOps(Prop,O,Type,VAL,LIST,GO,GO2).

compare_op(Type,F,OLD,VAL):-nop(Type),call((trace,call(F,OLD,VAL))),!.

% start of database
% These will all be deleted at start of run

inverse_args([AR,GS],[GS,AR]):-!.
inverse_args([AR,G,S],[S,G,AR]):-!.
inverse_args([A,R,G,S],[S,R,G,A]):-!.
inverse_args([P,A,R,G,S],[S,A,R,G,P]):-!.


compare_n(NewLast,Last):-NewLast=Last,!.
compare_n(NewLast,Last):-NewLast==unknown,!,NewLast==Last.
compare_n(NewLast,Last):-number(NewLast),not(number(Last)),throw(incomparable_terms(Last,NewLast)).
compare_n(Last,NewLast):-number(NewLast),not(number(Last)),throw(incomparable_terms(Last,NewLast)).
compare_n(NewLast,Last):-atomic(NewLast),not(atomic(Last)),throw(incomparable_terms(Last,NewLast)).
compare_n(Last,NewLast):-atomic(NewLast),not(atomic(Last)),throw(incomparable_terms(Last,NewLast)).


member_or_e(E,[L|List]):-!,member(E,[L|List]).
member_or_e(E,E).

is_single_valuedOrFail(F,A,Obj,ARGS):-moodb:is_db_prop(F,A,singleValued),!,valuedOrThrow(F,A,Obj,ARGS),!.
is_single_valuedOrFail(_,_,_,_):-fail.

valuedOrThrow(F,_,Obj,ARGS):- holds_t(mud_isa,Obj,T), findall_type_default_props(Obj,T,Props),Props=[_|_],Prop=..[F|ARGS], member_or_e(Prop,Props),!.
valuedOrThrow(F,A,Obj,ARGS):-valuedOrThrow1(F,A,Obj,ARGS).
valuedOrThrow1(_F,_A,_Obj,ARGS):-last(ARGS,unknown),!.
valuedOrThrow1(F,A,Obj,ARGS):-throw(is_single_valuedOrFail(F,A,Obj,ARGS)).


findall_type_default_props(Inst,Type,TraitsO):-findall(Props,moo:type_default_props(Inst,Type,Props),Traits),flatten(Traits,TraitsO),!.



replace_nth([],_N,OldVar,_NewVar,[]):-!,throw(missed_the_boat).
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



:-include(dbase_c_term_expansion).

:-noguitracer.

load_motel:- defrole([],time_state,restr(time,period)).

:-include(dbase_i_builtin).

:- debug,scan_db_prop.

:- dbase_define_db_prop(mudMaxHitPoints(agent,int)).

:- load_motel.

:-include(moo_loadall).
