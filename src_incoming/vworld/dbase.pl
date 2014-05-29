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

:- module(dbase,
 [
add/1,
always_transform_moo_preds/0,
prevent_transform_moo_preds/0,
add0/1,
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
dbase_t/1,
dbase_t/2,
dbase_t/3,
dbase_t/4,
dbase_t/5,
dbase_t/6,
dbase_t/7,
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
is_holds_false/1,
is_holds_true/1,
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
testOpenCyc/0]).

:-registerCycPred((
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

is_holds_true(Prop):-once(notrace((atom(Prop),is_holds_true0(Prop)))).
is_holds_true0(Prop):-member(Prop,[k,p,holds,holds_t,dbase_t,res,assertion_t,assertion]).

is_2nd_order_holds(Prop):- is_holds_true(Prop) ; is_holds_false(Prop).

is_holds_false(Prop):-notrace((atom(Prop),once((is_holds_false0(Prop,Stem),is_holds_true0(Stem))))).
is_holds_false0(Prop,Stem):-atom_concat('not_',Stem,Prop).
is_holds_false0(Prop,Stem):-atom_concat(Stem,'_not',Prop).
is_holds_false0(Prop,Stem):-atom_concat(Stem,'_false',Prop).
is_holds_false0(Prop,Stem):-atom_concat(Stem,'_f',Prop).

:- meta_predicate get_module_of_4(0,+,+,-).
:- meta_predicate get_module_of(0,-).

:- dynamic
    inside_clause_expansion/1,   
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
          dbase_f/7.

:- meta_predicate clause_present(:), db_forall_assert_mv(+,+,+), db_forall_assert_sv(+,+,+), db_forall(+,+), db_forall_quf(+,+,+).

:- dynamic skip_qlfs/0, db_op/2, dbase_mod/1, db_prop_from_game_load/1, db_prop_game_assert/1, req/1, scan_db_prop/0, term_listing/1,
  argIsa_call/3, use_term_listing/2,world_clear/1.


:- dynamic(db_prop_prolog/1).

:- dynamic verbOverride/3,named/2, determinerString/2, keyword/2 ,descriptionHere/2, mudToHitArmorClass0/2.

% :- context_module(M),asserta(dbase_mod(M)),dmsg(assert_if_new(dbase_mod(M))).
dbase_mod(dbase).

% :- ensure_loaded(logicmoo(logicmoo_util/logicmoo_util_all)).
:- use_module(logicmoo(logicmoo_util/logicmoo_util_library)).
:- use_module(logicmoo(logicmoo_util/logicmoo_util_ctx_frame)).
:- use_module(logicmoo(logicmoo_util/logicmoo_util_bugger)).

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
:- meta_predicate ensure_db_predicate_2(?,?,?,?,?,?).
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


get_module_of(P,M):-predicate_property(P,imported_from(M)),!.
get_module_of(MM:_,M):-!,MM=M.
get_module_of(P,M):-functor(P,F,A),get_module_of_4(P,F,A,M).
get_module_of_4(_P,F,A,M):- current_predicate(M0:F0/A0),F0=F,A0=A,!,M=M0.
get_module_of_4(_P,F,A,M):- current_predicate(F0/A0),F0=F,A0=A,!,dbase_mod(M).
get_module_of_4(_P,F,A,_M):-trace, isCycPredArity(F,A),!,fail.
get_module_of_4(P,F,A,M):- trace, show_cgoal(get_module_of_4(P,F,A,M)).


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
isCycPredArity0(P,A):- moodb:isRegisteredCycPred(_,P,A).
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


     

:-multifile(user:goal_expansion/2).
   
set_list_len(List,A,NewList):-length(List,LL),A=LL,!,NewList=List.
set_list_len(List,A,NewList):-length(List,LL),A>LL,length(NewList,A),append(List,_,NewList),!.
set_list_len(List,A,NewList):-length(NewList,A),append(NewList,_,List),!.

use_holds_db(Builtin,_):-
   member(Builtin,[compound,var]),!,fail.

use_holds_db(F,_):-is_2nd_order_holds(F),!,fail.
use_holds_db(F,A):- isCycPredArity(F,A).

true2(F,A):- isCycPredArity(F,A),!.
%true2(F,A):- integer(A),isCycPredArity(F,A2),!,trace,throw(isCycPredArity(F,A-A2)).
true2(F,A):- integer(A),moodb:registerCycPred(F,A).

is_kb_module(Moo):-atom(Moo),member(Moo,[moo,opencyc]).

try_mud_body_expansion(G0,G2):- not_ended_transform_moo_preds, mud_goal_expansion_0(G0,G1),!,goals_different(G0, G1),!,prepend_module(G1,dbase,G2).
mud_goal_expansion_0(G1,G2):-notrace((mud_pred_expansion(use_holds_db, holds_t - holds_f,G1,G2))).

try_mud_head_expansion(G0,G2):- not_ended_transform_moo_preds, mud_clause_expansion_0(G0,G1),!,goals_different(G0, G1),!,prepend_module(G1,dbase,G2).
mud_clause_expansion_0(G1,G2):-notrace((mud_pred_expansion(use_holds_db, dbase_t - dbase_f,G1,G2))),!.


dmsg_p(_):-!.
dmsg_p(P):-once(dmsg(P)),!.
dmsg_p(_):-!.

goals_different(G0,G1):-G0==G1,!,fail.
goals_different(G0,G1):-goals_different_1(G0,G1),!.
goals_different(G0,G1):- G0\==G1.

% goals_different_0(G0,dbase:G1):-G0==G1,!.
% goals_different_0(G0,G1):-trace.
goals_different_1(NV:G0,G1):-nonvar(NV),!,goals_different_1(G0,G1).
goals_different_1(G0,NV:G1):-nonvar(NV),!,goals_different_1(G0,G1).
goals_different_1(G0,G1):- (var(G0);var(G1)),!,throw(goals_different(G0,G1)).
goals_different_1(G0,G1):- G0 \= G1,!.


attempt_clause_expansion(B,_):-not(compound(B)),!,fail.
attempt_clause_expansion(B,BR):-  copy_term(B,BC),numbervars(BC,0,_),!, attempt_clause_expansion(B,BC,BR).
attempt_clause_expansion(_,BC,_):-inside_clause_expansion(BC),!,fail.
attempt_clause_expansion(B,BC,BR):- 
    setup_call_cleanup(asserta(inside_clause_expansion(BC)),
    force_clause_expansion(B,BR),
    must(retract(inside_clause_expansion(BC)))).

force_clause_expansion(M:((H:-B)),R):- !, mud_rule_expansion(M:H,M:B,R),!.
force_clause_expansion(((M:H:-B)),R):- !, mud_rule_expansion(M:H,B,R),!.
force_clause_expansion(((H:-B)),R):-mud_rule_expansion(H,B,R),!.
force_clause_expansion(H,HR):- try_mud_head_expansion(H,HR),!.
force_clause_expansion(H,HR):- user:expand_term(H,HR).

mud_rule_expansion(H,B,((HR:-BR))):-force_clause_expansion(H,HR),user:expand_goal(B,BR),!.

is_term_head(H):- (( \+ \+ inside_clause_expansion(H))),!.
is_term_head(_):- inside_clause_expansion(_),!,fail.
is_term_head(H):-H=_, is_our_sources(H).

is_our_sources(_):- not(prolog_load_context(directory,_)).
is_our_sources(_):- prolog_load_context(directory,Dir0),absolute_file_name(Dir0,Dir),user:file_search_path(logicmoo,LM0),absolute_file_name(LM0,LM),!,
      atom_concat(LM,_,Dir),!.


:-dynamic(always_transform_moo_preds).
:-dynamic(prevent_transform_moo_preds).
not_ended_transform_moo_preds:- prevent_transform_moo_preds,!,fail.
not_ended_transform_moo_preds:- always_transform_moo_preds,!.
not_ended_transform_moo_preds:- not(moodb:ended_transform_moo_preds).



univ_left(Comp,[M:P|List]):- univ_left0(M, Comp, [P|List]),!.
univ_left(Comp,[H,M:P|List]):- univ_left0(M,Comp,[H,P|List]),!.
univ_left(Comp,[P|List]):- univ_left0(dbase,Comp,[P|List]),!.

univ_left0(M,M:Comp,List):- Comp=..List,!.


head_xform(G0,_HOLDSNHOLDS,G1):- 
      functor_safe(G0,F,A),
      true2(F,A),
      G0=G1,!.
head_xform(G0,_HOLDSNHOLDS,G1):- trace,G0=G1.
  
mud_pred_expansion(_Prd,_HOLDS,G1,_):-not(compound(G1)),!,fail.
mud_pred_expansion(_Prd,_HOLDS,G1,G1):- functor_safe(G1,xcall,_),!.
mud_pred_expansion(Pred,NHOLDS - HOLDS, not(G1) ,G2):-!,mud_pred_expansion(Pred,HOLDS - NHOLDS,G1,G2).
mud_pred_expansion(Pred,NHOLDS - HOLDS, \+(G1) ,G2):-!,mud_pred_expansion(Pred,HOLDS - NHOLDS,G1,G2).

mud_pred_expansion(Pred, HOLDSNHOLDS, G0 ,G2):-
 functor(G0,F,1),G1=..[F,MP],
 predicate_property(G0, meta_predicate(G1)),
 member(MP,[:,0,1,2,3,4,5,6,7,8,9]),!,
 G0=..[F,Term],
 mud_pred_expansion(Pred, HOLDSNHOLDS, Term ,G2).

mud_pred_expansion(_Pred,HOLDSNHOLDS, Moo:G0,G3):- nonvar(Moo),is_kb_module(Moo),
   head_xform(G0,HOLDSNHOLDS,G1),
   mud_pred_expansion_0(true2,HOLDSNHOLDS,G1,G2),!,G2=G3.
mud_pred_expansion(Pred,HOLDSNHOLDS, Moo:G1,G3):-  nonvar(Moo),!, mud_pred_expansion_0(Pred,HOLDSNHOLDS,Moo:G1,G2),!,G2=G3.
mud_pred_expansion(Pred,HOLDSNHOLDS,G1,G3):- mud_pred_expansion_0(Pred,HOLDSNHOLDS,G1,G2),!,G2=G3.




mud_pred_expansion_0(Pred,HOLDSNHOLDS,_:G1,G2):-!,compound(G1),
   mud_pred_expansion_1(Pred,HOLDSNHOLDS,G1,G2),!.
mud_pred_expansion_0(Pred,HOLDSNHOLDS,G1,G2):-!,compound(G1),
   mud_pred_expansion_1(Pred,HOLDSNHOLDS,G1,G2),!.

mud_pred_expansion_1(_,HoldsT-HoldsF,P,_):-functor(P,Holds,_),member(Holds,[HoldsT,HoldsF]),!,fail.
mud_pred_expansion_1(_,_,P,_):-functor(P,Holds,_),member(Holds,[',',';']),!,fail.
% mud_pred_expansion_1(_,_,P,_):-functor(P,Holds,_),is_2nd_order_holds(Holds),trace,!,fail.

mud_pred_expansion_1(Pred,HOLDS-NHOLDS,G1,G2):-
   functor_safe(G1,F,A),is_2nd_order_holds(F),!,
   (is_holds_true(F) -> HOLDSNHOLDS=HOLDS-NHOLDS ; HOLDSNHOLDS=NHOLDS-HOLDS),
   mud_pred_expansion_holds(F,A,Pred,HOLDSNHOLDS,G1,G2).

mud_pred_expansion_1(Pred,HOLDS-_NHOLDS,G1,G2):-             
            functor_safe(G1,F,_),
            notrace((call(Pred,F,_))),
            must(call(Pred,F,A)),!,            
            G1=..[F|List], set_list_len(List,A,NewList), 
            univ_left(G2,[HOLDS,F|NewList]),!,
            dmsg_p(mud_pred_expansion(Pred,HOLDS,G1,G2)).
%mud_pred_expansion_1(Pred,HOLDS,G1,G2):- fail, G1=G2, dmsg(once(mud_pred_expansion(Pred,HOLDS,G1,G2))),!,fail.


mud_pred_expansion_holds(HF,1,Pred,HOLDSNHOLDS,G0,G2):- !, 
  G0=..[HF,G1],compound(G1),not(is_list(G1)),!,
  mud_pred_expansion(Pred,HOLDSNHOLDS,G1,G2).

mud_pred_expansion_holds(HF,A1,Pred,HOLDS-_NHOLDS,G1,G2):-
            (A1>2 -> A is A1 - 1 ; true),
            G1=..[HF,F,Arg1|List],
            nonvar(F),
            must(call(Pred,F,A)),!,
            set_list_len([Arg1|List],A,NewList),
            univ_left(G2 , [HOLDS,F|NewList]),!,
            dmsg_p(mud_pred_expansion(Pred,HOLDS,G1,G2)).

 

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

 
scan_arities:- forall(holds_t(arity,F,A),moodb:registerCycPred(F,A)).

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
ensure_q_loaded(File):-'@'(load_files(File,[if(not_loaded),qcompile(auto),expand(true),derived_from('../src_data/pldata/mworld0_declpreds.pl')]),user).

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
% % :- meta_predicate del(0),clr(0),add(0),add0(0),req(0), db_op(0,0,0),moo:db_prop(0,0).

% Found new meta-predicates in iteration 1 (0.281 sec)
%:- meta_predicate db_forall(?,?,?,0).

:- include('dbase_types_motel').

% :- register_module_type(utility).

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
define_db_prop_3(_,Pred,Arity,isRegisteredCycPred(_,Pred,Arity)):- moodb:registerCycPred(Pred,Arity).
define_db_prop_3(_,Pred,Arity,cyc):-moodb:registerCycPred(Pred,Arity).
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
   dbase_mod(Module),!,
   '@'(((  dynamic( F/A ), Module:export( F/A )   )),Module ),!. 


scan_db_prop:-
   dbase_mod(DBM),debug,
   ignore(('@'(forall((db_prop_argsIsa(ArgTypes),functor(ArgTypes,F,A)),call( dbase_define_db_prop(ArgTypes,[arity(F,A)]))),DBM))),
   ignore(('@'(forall(db_prop_findall(ArgTypes,PropTypes),call( dbase_define_db_prop(ArgTypes,PropTypes))),DBM))),
   !.
scan_db_prop:-!.


term_listing(Obj):- nonvar(var(Obj)),catch(listing(Obj),_,fail),fail.
term_listing(Obj):- 
    term_to_atom(Obj,HO),
   functor_safe(Obj,F),
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
req(C0):- db_op(q,C0).
%% props(Obj,QueryPropSpecs)
props(Obj,PropSpecs):-db_op('q',props(Obj,PropSpecs)).
%% add(Assertion)
add(C0):- add0(C0).
add0(C0):- db_op(a,C0).
%% padd(Obj,PropSpecs)
padd(Obj,PropSpecs):-add0(props(Obj,PropSpecs)).
%% padd(Obj,Prop,Value)
padd(Obj,Prop,Value):- must(atom(Prop)), PropValue=..[Prop,Value],!,padd(Obj,PropValue).
%% prop(Obj,Prop,Value)
prop(Obj,Prop,Value):- must(atom(Prop)), C=..[Prop,Obj,Value],!,req(C).
%% prop_or(Obj,Prop,Value)
prop_or(Obj,Prop,Value,OrElse):- once(prop(Obj,Prop,Value);Value=OrElse).

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
db_forall(u,C):- trace,db_forall_quf(C,U,Template),U,Template,must(ground(Template)),!,ignore(retractall(Template)).
db_forall(ra,C):-db_forall_quf(C,U,Template), doall((U,retractall(Template))).
db_forall(ra,C):-ignore(retractall(C)).
db_forall(a,C0):- db_forall_quf(C0,U,C),must(U),functor(C,F,A),!, (moodb:is_db_prop(F,A,singleValued) -> must(db_forall_assert_sv(C,F,A)) ; must(db_forall_assert_mv(C,F,A))).

db_forall(a,C):- functor(C,F,A),!, (moodb:is_db_prop(F,A,singleValued) -> must(db_forall_assert_sv(C,F,A)) ; must(db_forall_assert_mv(C,F,A))).
db_forall(r,C):- ground(C),retractall(C),!.
db_forall(Op,C):-!,trace,throw(unhandled(db_forall(Op,C))).


db_forall_query((C0->C1;C2)):- !, (db_forall_query(C0) -> db_forall_query(C1) ; db_forall_query(C2)).
db_forall_query((C1;C2)):-!, db_forall_query(C1) ; db_forall_query(C2).
db_forall_query(Term):-findall(a(Term),db_forall_query0(Term),List),list_to_set_safe(List,Set),!,member(a(Term),Set).
db_forall_query0((C1,C2)):-!, db_forall_query0(C1) , db_forall_query0(C2).
db_forall_query0(Term):- once(expand_goal(Term,Term2)),goals_different(Term, Term2),!,db_forall_query0(Term2).
db_forall_query0(Term):-db_forall_query1(Term).
db_forall_query1(C):- predicate_property(C,_PP),!,call(C).
db_forall_query1(C):- trace, db_forall_quf(C,Pretest,Template),!,Pretest, call(Template).


must_no_singletons(C):- throw_if_true(contains_singletons(C),C).
must_ground(C):- throw_if_true(not(ground(C)),C).

% only place ever should actual game dbase be changed from
hooked_asserta(C):- must_no_singletons(hooked_asserta(C)).
hooked_asserta(C):- moodb:run_database_hooks(assert(a),C),asserta_cloc(C).
hooked_assertz(C):- must_no_singletons(hooked_assertz(C)).
hooked_assertz(C):- run_database_hooks(assert(z),C), assertz_cloc(C).
hooked_retract(C):- must_ground(hooked_retract(C)).
hooked_retract(C):- run_database_hooks(retract(one),C), must(retract_cloc(C)).
hooked_retractall(C):- run_database_hooks(retract(all),C), retractall_cloc(C).

ensure_db_predicate(AR,M:C,_,F,A,G):- nonvar(M), !,ensure_db_predicate(AR,C,M,F,A,G).
ensure_db_predicate(AR,CI,M,F,A,G):-try_mud_head_expansion(CI,C),goals_different(CI, C),!,ensure_db_predicate_2(AR,C,M,F,A,G).
ensure_db_predicate(AR,CI,M,F,A,G):-try_mud_body_expansion(CI,C),goals_different(CI, C),!,ensure_db_predicate(AR,C,M,F,A,G).
ensure_db_predicate(AR,CI,M,F,A,G):-ensure_db_predicate_2(AR,CI,M,F,A,G).

ensure_db_predicate_2(AR,CI,M,F,A,G):-try_mud_head_expansion(CI,C),goals_different(CI, C),!,ensure_db_predicate_2(AR,C,M,F,A,G).
ensure_db_predicate_2(AR,C,M,F,A,G):- get_module_of(C,M),!, functor(C,F,A), make_predicate(M,F,A),prepend_module(C,M,G).
ensure_db_predicate_2(AR,C,M,F,A,G):- 
   must(functor(C,F,A)),
   declare_dbase_local(M:F,A),trace,
   get_module_of(C,M),!,
   make_predicate(M,F,A),prepend_module(C,M,G).

% ensure_db_predicate(a,agent('NpcCol1000-Geordi684'),dbase,_G176791,_G176792,_G176793)

prepend_module(_:C,M,M:C):-!.
prepend_module(C,M,M:C).

make_predicate(dbase,F,A):-!,registerCycPred(F,A),!.
make_predicate(M,F,A):- T =  '@'((dynamic(F/A)),M),ground(T),!,T.
make_predicate(M,F,A):- dynamic(F/A).

retract_cloc(C):- ensure_db_predicate(r,C,M,F,A,G), show_cgoal(retract(G)).
retractall_cloc(C):- ensure_db_predicate(r,C,M,F,A,G), show_cgoal(retractall(G)).
asserta_cloc(C):- ensure_db_predicate(a,C,M,F,A,G), show_cgoal(asserta(G)).
assertz_cloc(C):- ensure_db_predicate(a,C,M,F,A,G), debugOnError(show_cgoal(assertz(G))).

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

throw_if_true(T,E):- once(T),throw(throw_if_true(E:T)).

list_retain(PL,Pred,Result):- throw_if_true(not(is_list(PL)),list_retain(PL,Pred,Result)).
list_retain([],_Pred,[]):-!.
list_retain([R|List],Pred,[R|Retained]):- call(Pred,R),!, list_retain(List,Pred,Retained).
list_retain([_|List],Pred,Retained):- list_retain(List,Pred,Retained).

identical_member(X,[Y|_])  :-
	X == Y,
	!.
identical_member(X,[_|L]) :-
	identical_member(X,L).

contains_singletons(Term):-pred_term_parts(var,Term,Vars1),
   term_variables(Term,Vars2),
   member(Var,Vars2),findall(Var,identical_member(Var,Vars1),List),length(List,1).

% assert to a singlevalue pred
db_forall_assert_sv(C,F,A):- throw_if_true(contains_singletons(C),db_forall_assert_sv(C,F,A)).
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



must_asserta(C):-
      must(ground(C)),
      must(hooked_asserta(C)),!.
      
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

valuedOrThrow(F,_,Obj,ARGS):-mud_isa(Obj,T), findall_type_default_props(Obj,T,Props),Props=[_|_],Prop=..[F|ARGS], member_or_e(Prop,Props),!.
valuedOrThrow(F,A,Obj,ARGS):-valuedOrThrow1(F,A,Obj,ARGS).
valuedOrThrow1(_F,_A,_Obj,ARGS):-last(ARGS,unknown),!.
valuedOrThrow1(F,A,Obj,ARGS):-throw(is_single_valuedOrFail(F,A,Obj,ARGS)).

:-asserta(always_transform_moo_preds).

user:goal_expansion(G1,G3):- compound(G1),  once(try_mud_body_expansion(G1,G2)),goals_different(G1,G2),G2=G3.
user:term_expansion(G1,G3):- compound(G1),  once(attempt_clause_expansion(G1,G2)),goals_different(G1,G2),G2=G3.

findall_type_default_props(Inst,Type,TraitsO):-findall(Props,moo:type_default_props(Inst,Type,Props),Traits),flatten(Traits,TraitsO),!.

% =================================================================================================
% BEGIN world English
% =================================================================================================

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


moo:term_anglify_last(Head,English):-compound(Head),
   functor(Head,F,A),A>1,
   not(ends_with_icase(F,"Fn")),not(starts_with_icase(F,"SKF-")),
   atom_codes(F,[C|_]),code_type(C,lower),
   Head=..[F|ARGS],
   term_anglify_args(Head,F,A,ARGS,singleValued,English).

moo:term_anglify(Head,English):-
      functor(Head,F,A),
      moodb:is_db_prop(F,A,Info),member(Info,[singleValued,multi(_)]),
      Head=..[F|ARGS],
      term_anglify_args(Head,F,A,ARGS,Info,English),fully_expand(English,EnglishO),!.


term_anglify_args(Head,F,A,ARGS,multi(Which),English):- !,replace_nth(ARGS,Which,OldVar,NewVar,NEWARGS),!,
      NewHead=..[F|NEWARGS], findall(NewVar,NewHead,ListNewVar),list_to_set_safe(ListNewVar,SetNewVar),NewVar=list(ListNewVar),
      term_anglify_args(Head,F,A,NewHead,singleValued,English).
term_anglify_args(Head,F,A,ARGS0,singleValued,English):- add_arg_parts_of_speech(F,1,ARGS0,ARGS),verb_after_arg(F,A,After),
   insert_into(ARGS,After,verbFn(F),NEWARGS),
   fully_expand(NEWARGS,English),!.

unCamelCase(S,String):-any_to_string(S,Str),S\=Str,!,unCamelCase(Str,String),!.
unCamelCase("",""):-!.
unCamelCase(S,String):-sub_string(S,0,1,_,Char),sub_string(S,1,_,0,Rest),unCamelCase(Rest,RestString),string_lower(Char,NewChar),
  (Char\=NewChar->atomics_to_string(['_',NewChar,RestString],String);atomics_to_string([Char,RestString],String)),!.

moo:term_anglify(verbFn(mud_isa),[is,a]):-!.
moo:term_anglify(verbFn(isa),[is,a]):-!.
moo:term_anglify(verbFn(F),[is|UL]):-not(string_lower(F,F)),unCamelCase(F,U),atomics_to_string(UL,"_",U).
moo:term_anglify(verbFn(F),[is,F]):-atom_concat(_,'ing',F).
moo:term_anglify(verbFn(F),[F,is]).
% moo:term_anglify(prolog(Term),String):-term_to_atom(Term,Atom),any_to_string(Atom,String).
moo:term_anglify(determinerString(Obj,Text),[np(Obj),is,uses,string(Text),as,a,determiner]).
moo:term_anglify(nameString(Obj,Text),[np(Obj),is,refered,to,as,string(Text)]).
moo:term_anglify(moo:term_anglify(Term,Text),[prolog(Term),is,converted,to,english,using,prolog(Text)]).


add_arg_parts_of_speech(_F,_N,[],[]).
add_arg_parts_of_speech(F,N,[A|ARGS0],[ARG|ARGS]):-argIsa_call_or_undressed(F,N,A,ARG),N1 is N+1, add_arg_parts_of_speech(F,N1,ARGS0,ARGS).

argIsa_call_or_undressed(F,N,Obj,fN(Obj,Type)):-argIsa_call_0(F,N,Type),!.
argIsa_call_or_undressed(F,N,Obj,Obj).

verb_after_arg(_,_,1).


:- style_check(+discontiguous).


db_prop_argsIsa(ArgTypes):-db_prop_findall(ArgTypes,_).


db_prop_findall(ArgTypes,PROPS):-moo:db_prop(ArgTypes,PROPS).
db_prop_findall(ArgTypes,PROPS):-db_prop_multi(ArgTypes,PROPS).
% db_prop_findall(ArgTypes,[]):-moo:db_prop(ArgTypes).
db_prop_findall(ArgTypes,[]):-db_prop_sv(ArgTypes).
db_prop_findall(ArgTypes,[]):-db_prop_multi(ArgTypes).


moo:db_prop(ArgTypes,[from_game_load]):-db_prop_from_game_load(ArgTypes).

% =================================================================================================
% BEGIN world database
% =================================================================================================

:- begin_transform_moo_preds.

dbase_t(inRegion,O,Region):-atloc(O,LOC),locationToRegion(LOC,Region).
dbase_t(inRegion,apath(Region,Dir),Region):- holds_t(pathBetween,Region,Dir,_To).

db_prop_format(apath(region,dir),areaPath).
db_prop_format(dice(int,int,int),int).
db_resultIsa(apath,areaPath).


% prolog code
moo:db_prop(CallSig,[module(M)]):-db_prop_prolog(M,CallSig).


% db_prop_prolog(world,nearby(object,object)).
db_prop_prolog(world,mud_isa(object,type)).
% db_prop_prolog(world,same(id,id)).

% game_assert
moo:db_prop(G,[assert(game_assert)]):- db_prop_game_assert(G).


db_prop_game_assert(somethingIsa(term,list(type))).
db_prop_game_assert(somethingDescription(term,list(string))).
db_prop_game_assert(objects(type,list(id))).
% db_prop_game_assert(predicates(list(functors))).
db_prop_game_assert(sorts(type,list(type))).

% multivalued
moo:db_prop(G,[multi(AT)|LIST]):-db_prop_multi(G,AT,LIST).

% singlevalued
moo:db_prop(ArgTypes,[singleValued]):-db_prop_sv(ArgTypes).

% flags
moo:db_prop(ArgTypes,[flag,singleValued]):-db_prop_flag(ArgTypes).
moo:db_prop(ArgTypes,[flag,singleValued]):-db_prop_flag(ArgTypes).

moo:db_prop(repl_writer(agent,term),[singleValued,default(look:default_repl_writer)]).
moo:db_prop(repl_to_string(agent,term),[singleValued,default(look:default_repl_obj_to_string)]).



moo:db_prop(look:get_feet(agent,list(spatial)),[]).
moo:db_prop(look:get_near(agent,list(spatial)),[module(look)]).
moo:db_prop(get_precepts(agent,list(spatial)),[module(look)]).



type(T):-moo:subclass(A,B),(T=B;T=A).
type(item).


dbase_t(nameString,apath(Region,Dir),Text):- holds_t(pathName,Region,Dir,Text).
description(apath(Region,Dir),Text):- holds_t(pathName, Region,Dir,Text).

dbase_t(type_action_help,agent,What,Help):- holds_t(action_help,What,Help).
dbase_t(action_help,What,text("command is: ",What)):- holds_t(action_info,What).

moo:action_info(list(term)).
moo:agent_call_command(_Gent,list(Obj)):- term_listing(Obj).

channel(A):-region(A).
channel(A):-agent(A).
channel(gossup).

moo:subclass(agent,spatialthing).
moo:subclass(region,spatialthing).
moo:subclass(object,spatialthing).
moo:subclass(item,spatialthing).


moo:subclass(drinkable,item).
moo:subclass(possessable,item).
moo:subclass(useable,item).
moo:subclass(eatable,item).
moo:subclass(chargeable,item).
moo:subclass(wearable,item).


moo:type_default_props(_,food,[height(0)]).

moo:specifier_text(Text,pred):- moodb:is_db_prop(Text,_,arity(_,_)).

% single valued
moo:subclass(agent,object).
moo:subclass(item,object).


moo:db_prop(pathName(region,dir,string),[]).
db_prop_sv(verbOverride(term,action,action)).

db_prop_sv(atloc(object,xyz(region,int,int,int))).
db_prop_sv(act_turn(agent,int)).
db_prop_sv(armorLevel(possessable,int)).
db_prop_sv(attack(agent,int)).
db_prop_sv(charge(agent,int)).
db_prop_sv(chargeCapacity(chargable,int)).
db_prop_sv(chargeRemaining(chargable,int)).
db_prop_sv(damage(agent,int)).
db_prop_sv(defence(agent,int)).
db_prop_sv(facing(agent,dir)).
db_prop_sv(height(agent,int)).
% db_prop_sv(id(object,term)).
db_prop_sv(inRegion(term,region)).
db_prop_sv(last_command(agent,command)).
db_prop_sv(location_center(region,xyz(region,int,int,int))).
db_prop_sv(movedist(agent,number)).
db_prop_sv(mudBareHandDamage(agent,dice)).
db_prop_sv(mudLevelOf(possessable,int)).
db_prop_sv(mudMaxHitPoints(agent,int)).
db_prop_sv(mudToHitArmorClass0(agent,int)).
db_prop_sv(pathBetween(region,dir,region)).
db_prop_sv(permanence(item,verb,int)).
db_prop_sv(score(object,int)).
db_prop_sv(spawn_rate(moo:subclass(object),int)).
db_prop_sv(spd(agent,int)).
db_prop_sv(stm(agent,int)).
db_prop_sv(str(agent,int)).
% db_prop_sv(type_grid(regiontype,int,list(term))).
db_prop_sv(weight(object,int)).

db_prop_sv(needs_look(agent,boolean)). 

moo:subclass(areaPath,door).
moo:subclass(door,item).

moo:subclass(dir,string).

% flags
db_prop_flag(agent(id)).
db_prop_flag(item(id)).
db_prop_flag(region(id)).
db_prop_flag(type(id)).

db_prop_flag(thinking(agent)).
db_prop_flag(deleted(id)).


% multivalued
db_prop_multi(G,AT,[ordered|LIST]):- db_prop_multi(G,LIST),functor_safe(G,_,AT).

% db_prop_multi(named(term,term),[genlpreds(id)]).
db_prop_multi(ofclass(term,type),[alias(mud_isa)]).
db_prop_multi(G,[]):-db_prop_multi(G).

db_prop_multi(verbAsWell(term,action,action)).
db_prop_multi(failure(agent,action)).
db_prop_multi(nameString(term,string)).
db_prop_multi(determinerString(term,string)).
db_prop_multi(descriptionHere(term,string)).
db_prop_multi(description(term,string)).
db_prop_multi(keyword(term,string)).
db_prop_multi(act(term,term,term)).
db_prop_multi(memory(agent,term)).
db_prop_multi(wearing(agent,wearable)).
db_prop_multi(grid(region,int,int,object)).
db_prop_multi(possess(agent,item)).
db_prop_multi(moo:subclass(type,type)).
db_prop_multi(mud_isa(term,type)).

:-end_transform_moo_preds.
% =================================================================================================
% END world database
% =================================================================================================

:-noguitracer.

load_motel:- defrole([],time_state,restr(time,period)).

:- debug,scan_db_prop.

:- dbase_define_db_prop(mudMaxHitPoints(agent,int)).

:- load_motel.

%:- include(logicmoo(vworld/moo_footer)).

