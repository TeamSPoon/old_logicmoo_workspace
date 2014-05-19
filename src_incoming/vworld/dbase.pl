/** <module> 
% ===================================================================
% File 'dbase.pl'
% Purpose: Emulation of OpenCyc for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface.pl' 1.0.0
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

:- module(dbase, [

 add/1, add0/1, agent/1, agent_doing/2, agent_done/2, argIsa_call/3, charge/2, ofclass/2, clr/1, damage/2, db_op/2, atloc/2, 
 db_prop_from_game_load/1, db_prop_game_assert/1, del/1, failure/2, grid/4, mud_isa/2, item/1, holds_tcall/1,
 memory/2, padd/2, padd/3, possess/2, prop/3, prop_or/4, props/2, region/1, req/1, scan_db_prop/0, score/2, stm/2, term_listing/1,  facing/2,
 thinking/1, type/1, use_term_listing/2, wearing/2, world_clear/1, str/2 ,facing/2, height/2, act_term/2, description/2, act_turn/2,
 dbase_mod/1, dbase_define_db_prop/2,

          is_holds_false/1,
          is_holds_true/1,

          db_forall_query/1,

         clause_present_1/3,
         with_kb_assertions/2,

         make_list/3,

          getSurfaceFromChars/3,
          isSlot/1,
          assertion_holds/7,
          assertion_holds/6,
          assertion_holds/5,
          assertion_holds/4,
          assertion_holds/3,
          assertion_holds/2,
           scan_arities/0,
          'ASSERTION'/5,
          'ASSERTION'/6,
           cycInit/0,
           getCycConnection/3,
           finishCycConnection/3,
           invokeSubL/1,
           invokeSubL/2,
           invokeSubLRaw/2,
           cycStats/0,
           printSubL/2,
           formatCyc/3,
           toCycApiExpression/2,
           toCycApiExpression/3,
           cycQuery/1,
           cycQuery/2,
           cycAssert/1,
           cycAssert/2,
           cycRetract/1,
           cycRetract/2,
           cycRetractAll/1,
           cycRetractAll/2,
           isDebug/0,
           makeConstant/1,
           ensureMt/1,
           cyclify/2,
           cyclifyNew/2,
           defaultMt/1,
           mtForPred/2,

          dbase_true/1,
          dbase_true/2,
          dbase_true/3,
          dbase_true/4,
          dbase_true/5,
          dbase_true/6,
          dbase_true/7,

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

           isRegisteredCycPred/3,
           registerCycPred/1,
           registerCycPredPlus2/1,
           registerCycPred/2,
           registerCycPred/3,
           assertThrough/1,
           assertThrough/2,
           retractAllThrough/1,
           retractAllThrough/2,
           begin_transform_cyc_preds/0,
         end_transform_cyc_preds/0,
         cyc_goal_expansion/2,
         list_to_term/2, 
         lowerCasePred/1,   
         stringToWords/2, 
          balanceBinding/2,

         holds_t/8,
         holds_t/7,
         holds_t/6,
         holds_t/5,
         holds_t/4,
         holds_t/3,
         holds_t/2,
         holds_t/1,

         holds_f/8,
         holds_f/7,
         holds_f/6,
         holds_f/5,
         holds_f/4,
         holds_f/3,
         holds_f/2,
         holds_f/1,

         isCycPredArity/2,         
         testOpenCyc/0]).

:-dynamic(isRegisteredCycPred/3).

:- meta_predicate get_module_of(0,+,+,-).
:- meta_predicate get_module_of(0,-).

:- dynamic
          assertion_holds/7,
          assertion_holds/6,
          assertion_holds/5,
          assertion_holds/4,
          assertion_holds/3,
          assertion_holds/2,
          dbase_true/1,
          dbase_true/2,
          dbase_true/3,
          dbase_true/4,
          dbase_true/5,
          dbase_true/6,
          dbase_true/7,

          assertion_holds_not/7,
          assertion_holds_not/6,
          assertion_holds_not/5,
          assertion_holds_not/4,
          assertion_holds_not/3,
          assertion_holds_not/2,
          dbase_false/1,
          dbase_false/2,
          dbase_false/3,
          dbase_false/4,
          dbase_false/5,
          dbase_false/6,
          dbase_false/7.

:- meta_predicate clause_present(:), db_forall_assert_mv(+,+,+), db_forall_assert_sv(+,+,+), db_forall(+,+), db_forall_quf(+,+,+).

:- dynamic 
 dbase_mod/1,
  agent/1, agent_doing/2, agent_done/2, argIsa_call/3, charge/2, ofclass/2,  damage/2, db_op/2,atloc/2, 
 db_prop_from_game_load/1, db_prop_game_assert/1, failure/2, grid/4, mud_isa/2, item/1, 
 memory/2, pathName/3, possess/2, region/1, req/1, scan_db_prop/0, score/2, stm/2, term_listing/1, facing/2,
 thinking/1, type/1, use_term_listing/2, wearing/2, world_clear/1, str/2 ,facing/2, height/2, act_term/2,  description/2, act_turn/2.

:- dynamic(db_prop_prolog/1).

:- dynamic verbOverride/3,named/2, determinerString/2, keyword/2 ,descriptionHere/2, mudToHitArmorClass0/2.

% :- context_module(M),asserta(dbase_mod(M)),dmsg(assert_if_new(dbase_mod(M))).
dbase_mod(dbase).

:- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_bugger.pl')).
:- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_all.pl')).
:- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_terms.pl')).
:- ensure_loaded(logicmoo('vworld/moo.pl')).

:- dynamic_multifile_exported dbase_true/1.
:- dynamic_multifile_exported dbase_true/2.
:- dynamic_multifile_exported dbase_true/3.
:- dynamic_multifile_exported dbase_true/4.
:- dynamic_multifile_exported dbase_true/5.
:- dynamic_multifile_exported dbase_true/6.
:- dynamic_multifile_exported dbase_true/7.

make_list(E,1,[E]):-!.
make_list(E,N,[E|List]):- M1 is N - 1, make_list(E,M1,List),!.

def_meta_predicate(F,S,E):-doall(((between(S,E,N),make_list('?',N,List),CALL=..[F|List],meta_predicate(CALL)))).
:- def_meta_predicate(call_f,2,8).
:- def_meta_predicate(call_t,2,8).
:- def_meta_predicate(xcall,1,7).
:- def_meta_predicate(xcall_not,1,7).
:- def_meta_predicate(dbase_true,1,7).
:- def_meta_predicate(dbase_false,1,7).
:- def_meta_predicate(call_f_mt,4,10).
:- def_meta_predicate(call_t_mt,4,10).
:- def_meta_predicate(assertion_holds,2,7).
:- def_meta_predicate(assertion_holds_not,2,7).

%:- register_module_type(utility).

add_db_prop(_,_,Var):- var(Var),!.
add_db_prop(_,_,[]):- !.
add_db_prop(F,A,[C|L]):-!, add_db_prop(F,A,C),add_db_prop(F,A,L),!.
add_db_prop(F,A,CL):- assert_if_new(moo:is_db_prop(F,A,CL)).

rem_db_prop(_,_,Var):- var(Var),!.
rem_db_prop(_,_,[]):- !.
rem_db_prop(F,A,[C|L]):-!, rem_db_prop(F,A,C),rem_db_prop(F,A,L),!.
rem_db_prop(F,A,CL):- retractall(moo:is_db_prop(F,A,CL)).

% ============================================
% Prolog to Cyc Predicate Mapping
%
%  the following will all do the same things:
%
% ?- registerCycPred('BaseKB':isa/2). 
% ?- registerCycPred('BaseKB':isa(_,_)). 
% ?- registerCycPred(isa(_,_),'BaseKB'). 
% ?- registerCycPred('BaseKB',isa,2). 
%
%  Will make calls 
% ?- isa(X,Y)
%  Query into #$BaseKB for (#$isa ?X ?Y) 
%
% ============================================
:-dynamic(isRegisteredCycPred/3).

% ?- registerCycPred('BaseKB':isa/2). 
registerCycPred(Mt:Pred/Arity):- !,
   registerCycPred(Mt,Pred,Arity).

% ?- registerCycPred('BaseKB':isa(_,_)). 
registerCycPred(Mt:Term):-
   functor(Term,Pred,Arity),
   registerCycPred(Mt,Pred,Arity).

registerCycPred(M:F):-!, '@'(registerCycPred(F), M).
registerCycPred(F/A):- reallyRegisterCycPred(F,A).
registerCycPred([A]):-!,registerCycPred(A).
registerCycPred([A|L]):-!,registerCycPred(A),registerCycPred(L).
registerCycPred((A,L)):-!,registerCycPred(A),registerCycPred(L).

registerCycPredPlus2(M:F):-!, '@'(registerCycPredPlus2(F), M).
registerCycPredPlus2(F/A):- A2 is A -2, registerCycPred(F/A2).
registerCycPredPlus2([A]):-!,registerCycPredPlus2(A).
registerCycPredPlus2([A|L]):-!,registerCycPredPlus2(A),registerCycPredPlus2(L).
registerCycPredPlus2((A,L)):-!,registerCycPredPlus2(A),registerCycPredPlus2(L).

reallyRegisterCycPred(F,A) :- registerCycPred(_,F,A).

% ?- registerCycPred(isa(_,_),'BaseKB'). 
registerCycPred(Term,Mt):-
   functor(Term,Pred,Arity),
   registerCycPred(Mt,Pred,Arity).
   
% ?- registerCycPred('BaseKB',isa,2). 
registerCycPred(Mt,_:Pred,Arity):-!,registerCycPred(Mt,Pred,Arity).
registerCycPred(Mt,Pred,0):-!,registerCycPred(Mt,Pred,2).
registerCycPred(Mt,Pred,Arity):-isRegisteredCycPred(Mt,Pred,Arity),!.
registerCycPred(Mt,Pred,Arity):-
      M = dbase,
      assertz(M:isRegisteredCycPred(Mt,Pred,Arity)),
      add_db_prop(Pred,Arity,isRegisteredCycPred(Mt,Pred,Arity)),
      '@'(((
      % functor(Term,Pred,Arity),
      F = Pred,
      A is Arity,
    %  A1 is Arity + 1,
      A2 is Arity + 2,
      dynamic(F/A), multifile(F/A), export(F/A),
      dynamic(F/A2), multifile(F/A2), export(F/A2),
    %  dynamic(assertion_holds/A1), multifile(assertion_holds/A1), export(assertion_holds/A1),
      !
      )),  M).


get_module_of(P,M):-predicate_property(P,imported_from(M)),!.
get_module_of(MM:_,M):-!,MM=M.
get_module_of(P,M):-functor(P,F,A),get_module_of_4(P,F,A,M).
get_module_of_4(_P,F,A,M):- current_predicate(M0:F0/A0),F0=F,A0=A,!,M=M0.
get_module_of_4(_P,F,A,M):- current_predicate(F0/A0),F0=F,A0=A,!,dbase_mod(M).
get_module_of_4(_P,F,A,_M):-trace, isCycPredArity(F,A),!,fail.
get_module_of_4(P,F,A,M):- trace, show_cgoal(get_module_of_4(P,F,A,M)).


callable(F,A):- functor_safe(P,F,A),predicate_property(P,_),!.

% ================================================================================
% begin holds_t
% ================================================================================
holds_t(P,A1,A2,A3,A4,A5,A6,A7):- isCycPredArity_NE(P,7),(call_t(P,A1,A2,A3,A4,A5,A6,A7);call_t_mt(P,A1,A2,A3,A4,A5,A6,A7,_,_);assertion_true([P,A1,A2,A3,A4,A5,A6,A7])).
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


call_t(P,A1,A2,A3,A4,A5,A6,A7):- callable(P,7),CALL=..[P,A1,A2,A3,A4,A5,A6,A7],(dbase:dbase_true(CALL);call(CALL)).
call_t(P,A1,A2,A3,A4,A5,A6):- dbase:dbase_true(P,A1,A2,A3,A4,A5,A6).
call_t(P,A1,A2,A3,A4,A5,A6):- assertion_holds(P,A1,A2,A3,A4,A5,A6).
call_t(P,A1,A2,A3,A4,A5,A6):- callable(P,6),call(P,A1,A2,A3,A4,A5,A6).
call_t(P,A1,A2,A3,A4,A5):- dbase:dbase_true(P,A1,A2,A3,A4,A5).
call_t(P,A1,A2,A3,A4,A5):- assertion_holds(P,A1,A2,A3,A4,A5).
call_t(P,A1,A2,A3,A4,A5):- callable(P,5),call(P,A1,A2,A3,A4,A5).
call_t(P,A1,A2,A3,A4):- dbase:dbase_true(P,A1,A2,A3,A4).
call_t(P,A1,A2,A3,A4):- assertion_holds(P,A1,A2,A3,A4).
call_t(P,A1,A2,A3,A4):- callable(P,4),call(P,A1,A2,A3,A4).
call_t(P,A1,A2,A3):- dbase:dbase_true(P,A1,A2,A3).
call_t(P,A1,A2,A3):- assertion_holds(P,A1,A2,A3).
call_t(P,A1,A2,A3):- callable(P,3),call(P,A1,A2,A3).
call_t(P,A1,A2):- dbase:dbase_true(P,A1,A2).
call_t(P,A1,A2):- assertion_holds(P,A1,A2).
call_t(P,A1,A2):- callable(P,2),call(P,A1,A2).
call_t(P,A1):- dbase:dbase_true(P,A1).
call_t(P,A1):- assertion_holds(P,A1).
call_t(P,A1):- callable(P,1),call(P,A1).

call_t_mt(P,A1,A2,A3,A4,A5,A6,A7,A8,A9):- callable(P,9),CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8,A9],call(CALL).
call_t_mt(P,A1,A2,A3,A4,A5,A6,A7,A8):- callable(P,8),CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8],call(CALL).
call_t_mt(P,A1,A2,A3,A4,A5,A6,A7):- callable(P,7),CALL=..[P,A1,A2,A3,A4,A5,A6,A7],call(CALL).
call_t_mt(P,A1,A2,A3,A4,A5,A6):- callable(P,6),call(P,A1,A2,A3,A4,A5,A6).
call_t_mt(P,A1,A2,A3,A4,A5):- callable(P,5),call(P,A1,A2,A3,A4,A5).
call_t_mt(P,A1,A2,A3,A4):- callable(P,4),call(P,A1,A2,A3,A4).
call_t_mt(P,A1,A2,A3):- callable(P,3),call(P,A1,A2,A3).

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

assertion_true([AH,P|LIST]):-is_holds_true(AH),!,assertion_true([P|LIST]).
assertion_true([AH,P|LIST]):-is_holds_false(AH),!,assertion_false([P|LIST]).
assertion_true([P|LIST]):-'ASSERTION'(':TRUE-DEF',_,_UniversalVocabularyMt,_Vars,/*HL*/[P|LIST]).
assertion_true([P|LIST]):-'ASSERTION'(':TRUE-MON',_,_UniversalVocabularyMt,_Vars,/*HL*/[P|LIST]).
assertion_true([P|LIST]):-!, dbase:dbase_true([P|LIST]).

% ================================================================================
% end holds_t
% ================================================================================


% ================================================================================
% begin holds_f
% ================================================================================
holds_f(P,A1,A2,A3,A4,A5,A6,A7):- isCycPredArity_NE(P,7),(call_f(P,A1,A2,A3,A4,A5,A6,A7);call_f_mt(P,A1,A2,A3,A4,A5,A6,A7,_,_);assertion_false([P,A1,A2,A3,A4,A5,A6,A7])).
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


call_f(P,A1,A2,A3,A4,A5,A6,A7):- xcall(((callable(P,7),CALL=..[P,A1,A2,A3,A4,A5,A6,A7],(dbase:dbase_false(CALL);xcall_not(CALL))))).
call_f(P,A1,A2,A3,A4,A5,A6):- xcall(((dbase:dbase_false(P,A1,A2,A3,A4,A5,A6)))).
call_f(P,A1,A2,A3,A4,A5,A6):- xcall(((assertion_holds_not(P,A1,A2,A3,A4,A5,A6)))).
call_f(P,A1,A2,A3,A4,A5,A6):- xcall(((callable(P,6),xcall_not(P,A1,A2,A3,A4,A5,A6)))).
call_f(P,A1,A2,A3,A4,A5):- xcall(((dbase:dbase_false(P,A1,A2,A3,A4,A5)))).
call_f(P,A1,A2,A3,A4,A5):- xcall(((assertion_holds_not(P,A1,A2,A3,A4,A5)))).
call_f(P,A1,A2,A3,A4,A5):- xcall(((callable(P,5),xcall_not(P,A1,A2,A3,A4,A5)))).
call_f(P,A1,A2,A3,A4):- xcall((( dbase:dbase_false(P,A1,A2,A3,A4)))).
call_f(P,A1,A2,A3,A4):- xcall(((assertion_holds_not(P,A1,A2,A3,A4)))).
call_f(P,A1,A2,A3,A4):- xcall(((callable(P,4),xcall_not(P,A1,A2,A3,A4)))).
call_f(P,A1,A2,A3):- xcall(((dbase:dbase_false(P,A1,A2,A3)))).
call_f(P,A1,A2,A3):- xcall(((assertion_holds_not(P,A1,A2,A3)))).
call_f(P,A1,A2,A3):- xcall(((callable(P,3),xcall_not(P,A1,A2,A3)))).
call_f(P,A1,A2):- xcall(((dbase:dbase_false(P,A1,A2)))).
call_f(P,A1,A2):- xcall((( assertion_holds_not(P,A1,A2)))).
call_f(P,A1,A2):- xcall(((callable(P,2),xcall_not(P,A1,A2)))).
call_f(P,A1):- xcall(((dbase:dbase_false(P,A1)))).
call_f(P,A1):- xcall(((assertion_holds_not(P,A1)))).
call_f(P,A1):- xcall(((callable(P,1),xcall_not(P,A1)))).

call_f_mt(P,A1,A2,A3,A4,A5,A6,A7,A8,A9):- callable(P,9),CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8,A9],xcall_not(CALL).
call_f_mt(P,A1,A2,A3,A4,A5,A6,A7,A8):- callable(P,8),CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8],xcall_not(CALL).
call_f_mt(P,A1,A2,A3,A4,A5,A6,A7):- callable(P,7),CALL=..[P,A1,A2,A3,A4,A5,A6,A7],xcall_not(CALL).
call_f_mt(P,A1,A2,A3,A4,A5,A6):- callable(P,6),xcall_not(P,A1,A2,A3,A4,A5,A6).
call_f_mt(P,A1,A2,A3,A4,A5):- callable(P,5),xcall_not(P,A1,A2,A3,A4,A5).
call_f_mt(P,A1,A2,A3,A4):- callable(P,4),xcall_not(P,A1,A2,A3,A4).
call_f_mt(P,A1,A2,A3):- callable(P,3),xcall_not(P,A1,A2,A3).

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

assertion_false([AH,P|LIST]):-is_holds_true(AH),!,assertion_false([P|LIST]).
assertion_false([AH,P|LIST]):-is_holds_false(AH),!,assertion_true([P|LIST]).
assertion_false([P|LIST]):-'ASSERTION'(':FALSE-DEF',_,_UniversalVocabularyMt,_Vars,/*HL*/[P|LIST]).
assertion_false([P|LIST]):-'ASSERTION'(':FALSE-MON',_,_UniversalVocabularyMt,_Vars,/*HL*/[P|LIST]).
assertion_false([P|LIST]):-!, dbase:dbase_false([P|LIST]).

% ================================================================================
% end holds_f 
% ================================================================================



arityMatches(A,S-E):- !, system:between(S,E,OTHER), A=OTHER.
arityMatches(A,OTHER):-number(OTHER),!, A=OTHER.

isCycPredArity_NE(P,A):-ignore(isCycPredArity(P,A)).


isCycPredArity(P,A):-nonvar(P),!,isCycPredArity0(P,OTHER),!,arityMatches(A,OTHER).
isCycPredArity(P,A):- isCycPredArity0(P,OTHER),arityMatches(A,OTHER).


isCycPredArity0(P,A):- nonvar(P),member(P,[arity,arityMax,arityMin]),!,A=2.
isCycPredArity0(P,A):- isRegisteredCycPred(_,P,A).
isCycPredArity0(P,A):- xcall(holds_t(arity,P, A)).
isCycPredArity0(P,A):- xcall(holds_t(arityMax,P, A)).
isCycPredArity0(P,A):- xcall(((holds_t(arityMin,P, A),not(holds_t(arityMax,P, _))))).




%:- register_module_type(utility).

     
%list_to_term(X,Y):- balanceBinding(X,Y).
list_to_term(X,Y):-nonvar(X),var(Y),!,list_to_terms_lr(X,Y).
list_to_term(X,Y):-list_to_terms_rl(X,Y).
list_to_terms_rl(List,(A,B)):-list_to_terms_rl(A,AL),list_to_terms_rl(B,BL),append(AL,BL,List).
list_to_terms_rl(List,(A;B)):-list_to_terms_rl(A,AL),list_to_terms_rl(B,BL),append(AL,[or|BL],List).
list_to_terms_lr([],true):-!.
list_to_terms_lr([T],T):-!.
list_to_terms_lr([H|T],(H,TT)):-!,list_to_terms_lr(T,TT).


     

:-dynamic(ended_transform_cyc_preds/0).
:-multifile(user:goal_expansion/2).

ended_transform_cyc_preds.

begin_transform_cyc_preds:- retractall((ended_transform_cyc_preds)).

end_transform_cyc_preds:-
   asserta(ended_transform_cyc_preds),
   retractall((user:goal_expansion(G1,G2):- not(ended_transform_cyc_preds), compound(G1), cyc_goal_expansion(G1,G2))).

set_list_len(List,A,NewList):-length(List,LL),A=LL,!,NewList=List.
set_list_len(List,A,NewList):-length(List,LL),A>LL,length(NewList,A),append(List,_,NewList),!.
set_list_len(List,A,NewList):-length(NewList,A),append(NewList,_,List),!.


functor_safe(P,F,A):- strip_module(P,P0),var(F),!,functor(P0,F0,A),strip_module(F0,F),!.
functor_safe(P,F,A):- strip_module(P,P0),strip_module(F,F0),!,functor(P0,F0,A).

strip_module(_:P,P):-nonvar(P),!.
strip_module(P,P).

use_holds_db(F,A):-isCycPredArity(F,A).

cyc_goal_expansion(G1,G2):-cyc_pred_expansion(use_holds_db, holds_t:holds_f,G1,G2).
cyc_head_expansion(G1,G2):-cyc_pred_expansion(use_holds_db, dbase_true:dbase_false,G1,G2).


cyc_pred_expansion(_Prd,_HOLDS,G1,_):-not(compound(G1)),!,fail.
cyc_pred_expansion(_Prd,_HOLDS,G1,G1):- functor(G1,xcall,_).
cyc_pred_expansion(Pred,NHOLDS:HOLDS,not(G1),G2):-!,cyc_pred_expansion(Pred,HOLDS:NHOLDS,G1,G2).
cyc_pred_expansion(Pred,NHOLDS:HOLDS, \+(G1) ,G2):-!,cyc_pred_expansion(Pred,HOLDS:NHOLDS,G1,G2).
cyc_pred_expansion(Pred,HOLDS:_NHOLDS,G1,G2):- 
            functor_safe(G1,HF,_),is_holds_true(HF),
            G1=..[HF,F|List],call(Pred,F,A),!,
            set_list_len(List,A,NewList),
            G2=..[HOLDS,F|NewList],!,
            dmsg_p(cyc_pred_expansion(Pred,HOLDS,G1,G2)).
cyc_pred_expansion(Pred,_HOLDS:NHOLDS,G1,G2):- 
            functor_safe(G1,HF,_),is_holds_false(HF),
            G1=..[HF,F|List],call(Pred,F,A),!,
            set_list_len(List,A,NewList),
            G2=..[NHOLDS,F|NewList],!,
            dmsg_p(cyc_pred_expansion(Pred,NHOLDS,G1,G2)).
cyc_pred_expansion(Pred,HOLDS:_NHOLDS,G1,G2):- 
            functor_safe(G1,F,_),call(Pred,F,A),!,
            G1=..[F|List], set_list_len(List,A,NewList), 
            G2=..[HOLDS,F|NewList],!,
            dmsg_p(cyc_pred_expansion(Pred,HOLDS,G1,G2)).
cyc_pred_expansion(Pred,HOLDS,G1,G2):- fail, G1=G2, dmsg(once(cyc_pred_expansion(Pred,HOLDS,G1,G2))),!,fail.


dmsg_p(_):-!.
dmsg_p(P):-once(dmsg(P)),!.
dmsg_p(_):-!.

cyc_term_expansion(G1,_):-not(compound(G1)),!,fail.
cyc_term_expansion(((H:-B)),R):-!,cyc_rule_expansion(H,B,R),!.
% cyc_term_expansion(H,HR):-trace,cyc_head_expansion(H,HR),!.

cyc_rule_expansion(H,B,((HR:-BR))):-cyc_head_expansion(H,HR),!,must(expand_goal(B,BR)).

cyc_body_expansion(B,BR):-user:goal_expansion(B,BR).

user:goal_expansion(G1,G2):- compound(G1),  not(ended_transform_cyc_preds), cyc_goal_expansion(G1,G2),!.
user:term_expansion(G1,G2):- compound(G1),  not(ended_transform_cyc_preds), cyc_term_expansion(G1,G2),!.

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

 
:- dynamic_multifile_exported(dbase,
 ((
  %dbase:inRegion/2,
  %dbase:pathBetween/3,
  dbase:'ASSERTION'/5,
  dbase:'ASSERTION'/6,
  dbase:transprob/4,
  dbase:transprob2/3,
  dbase:indexCyc/1,
  dbase:bcr/1,
  %dbase:((dynamicCyc))/1,
  dbase:brillPos/1,
  dbase:mws/2 
  )) ).

scan_arities:- forall(holds_t(arity,F,A),registerCycPred(F,A)).

:- include(logicmoo(vworld/dbase_i_cyc)).

% :- include(logicmoo('pldata/trans_header.pl')).

% logicmoo('pldata/mworld0.pldata') compiled into world 61.18 sec, 483,738 clauses
:- dmsg(loading(kb0)).
:- ensure_loaded(logicmoo('pldata/tiny_kb')).

% this next line delays loading of NL content
% ensure_NL_loaded(File):- current_prolog_flag(version,V),V>70111,!,dmsg(delay_loading(File)),!.
ensure_NL_loaded(File):-dmsg(loading(File)),load_files(File,[if(not_loaded),qcompile(auto)]).

:- ensure_loaded(logicmoo('database/logicmoo_nldata_freq.pdat.txt')).
:- ensure_loaded(logicmoo('database/logicmoo_nldata_BRN_WSJ_LEXICON.txt')).
:- ensure_loaded(logicmoo('database/logicmoo_nldata_colloc.pdat.txt')).
:- ensure_loaded(logicmoo('database/logicmoo_cyc_pos_data')).
:- ensure_loaded(logicmoo('database/logicmoo_nl_dictionary')).

:- ensure_NL_loaded(logicmoo('pldata/tt0_00022_cycl')).
:- ensure_NL_loaded(logicmoo('pldata/mworld0')).
:- ensure_NL_loaded(logicmoo('pldata/hl_holds')).

/*

;;    loading(kb0)
%     logicmoo('pldata/tiny_kb') compiled into dbase 6.54 sec, 9,019 clauses
%     logicmoo('database/logicmoo_nl_dictionary') compiled into dbase 0.09 sec, 267 clauses
;;     loading(logicmoo('database/logicmoo_nldata_freq.pdat.txt'))
%     logicmoo('database/logicmoo_nldata_freq.pdat.txt') compiled into dbase 15.24 sec, 107,710 clauses
;;     loading(logicmoo('database/logicmoo_nldata_BRN_WSJ_LEXICON.txt'))
%     logicmoo('database/logicmoo_nldata_BRN_WSJ_LEXICON.txt') compiled into dbase 12.26 sec, 113,869 clauses
;;     loading(logicmoo('database/logicmoo_nldata_colloc.pdat.txt'))
%     logicmoo('database/logicmoo_nldata_colloc.pdat.txt') compiled into dbase 9.50 sec, 64,087 clauses
;;     loading(logicmoo('database/logicmoo_cyc_pos_data'))
%     logicmoo('database/logicmoo_cyc_pos_data') compiled into dbase 0.39 sec, 2,480 clauses
;;     loading(logicmoo('pldata/tt0_00022_cycl'))
%     logicmoo('pldata/tt0_00022_cycl') compiled into dbase 54.46 sec, 313,290 clauses
;;     loading(logicmoo('pldata/mworld0'))
%     logicmoo('pldata/mworld0') compiled into dbase 109.01 sec, 483,066 clauses
;;     loading(logicmoo('pldata/hl_holds'))
%     logicmoo('pldata/hl_holds') compiled into dbase 324.81 sec, 1,041,327 clauses

*/
user_export(_):- dbase_mod(user),!.
user_export(_M:Prop/Arity):-!,user_export(Prop/Arity).
user_export(Prop/Arity):- 
   dbase_mod(M), '@'( ((export(Prop/Arity),dynamic(Prop/Arity))) , M).


:- meta_predicate hooked_asserta(^), hooked_assertz(^), hooked_retract(^), hooked_retractall(^).
% % :- meta_predicate del(0),clr(0),add(0),add0(0),req(0), db_op(0,0,0),moo:db_prop(0,0),moo:db_prop(0).

% Found new meta-predicates in iteration 1 (0.281 sec)
%:- meta_predicate db_forall(?,?,?,0).

:- ensure_loaded(logicmoo('vworld/moo')).

% :- include(logicmoo('vworld/moo_header')).

:- include('dbase_types_motel').

%r:- register_module_type(utility).

dbase_define_db_prop(P):-dbase_define_db_prop(P,[]).
dbase_define_db_prop(M:ArgTypes,PropTypes):-!,
   dbase_define_db_prop(ArgTypes,[module(M)|PropTypes]),!.
   
dbase_define_db_prop(ArgTypes,PropTypes):-
   functor(ArgTypes,F,A),
   doall(define_db_prop_0(ArgTypes,F,A)),
   doall(define_db_prop_1(ArgTypes,F,A,PropTypes)),
   doall(define_db_prop_2(ArgTypes,F,A,PropTypes)),
   doall(define_db_prop_3(ArgTypes,F,A,PropTypes)).

define_argType(F,A,N,ArgType):-add_db_prop(F,A,argIsa(N,ArgType)).

% pass 0 = assert arity and argTypes
define_db_prop_0(ArgTypes,F,A):-add_db_prop(F,A,arity(F,A)),fail.
define_db_prop_0(ArgTypes,F,A):-add_db_prop(F,A,argTypes(ArgTypes)),fail.
define_db_prop_0(ArgTypes,F,A):-doall(forall(arg(N,ArgTypes,ArgType),define_argType(F,A,N,ArgType))),fail.

% pass 1 = assert misc properties
define_db_prop_1(W,F,A,PT):-add_db_prop(F,A,PT),!,forall(member_or_e(E,PT),define_db_prop_3(W,F,A,PT)).

% pass 2 = add extensional hooks
define_db_prop_2(W,F,A,_):- doall(forall(moo:is_db_prop(F,A,CL),ignore(define_db_prop_3(W,F,A,CL)))).

% pass 3 = re-add extensional hooks
define_db_prop_3(_,Pred,Arity,isRegisteredCycPred(_,Pred,Arity)):-registerCycPred(Pred,Arity).
define_db_prop_3(_,Pred,Arity,cyc):-registerCycPred(Pred,Arity).
define_db_prop_3(_,F,A,module(Module)):-not(dbase_mod(Module)),functor(HEAD,F,A),must(predicate_property(Module:HEAD,_)),!.
define_db_prop_3(_,F,A,module(Module)):-dbase_mod(Module),!,declare_dbase_local(F,A).
define_db_prop_3(_,F,A,arity(F,A)):- 
   not((moo:is_db_prop(F,A,module(Module)),not(dbase_mod(Module)))),!,
      declare_dbase_local(F,A).
define_db_prop_3(_,_,_,_).

declare_dbase_local(M:F,A):- dbase_mod(M),!,declare_dbase_local(F,A).
declare_dbase_local(_M:F,A):- !, trace, declare_dbase_local(F,A).
declare_dbase_local(F,A):- declare_dbase_local_1(F,A).


declare_dbase_local_1(F,A):- moo:is_db_prop(F,A,hasStub),!.

declare_dbase_local_1(F,A):- once(add_db_prop(F,A,hasStub)),fail.

declare_dbase_local_1(F,A):- functor(Pred,F,A), get_module_of(Pred,Module),declare_dbase_local_1(Module, Pred, F,A).

declare_dbase_local_1(Module, Pred, F,A):-
   not(dbase_mod(Module)),!,
   dbase_mod(DBModule),
   rem_db_prop(F,A,module(DBModule)),
   add_db_prop(F,A,module(Module)).

declare_dbase_local_1(Module, Pred, F,A):- 
   dbase_mod(Module),!,
   '@'(((  dynamic( F/A ), Module:export( F/A )   )),Module ),!. 


scan_db_prop:-
   dbase_mod(DBM),debug,
   ignore(('@'(forall((db_prop_argsIsa(ArgTypes),functor(ArgTypes,F,A)),call( dbase_define_db_prop(ArgTypes,[arity(F,A)]))),DBM))),
   ignore(('@'(forall(db_prop_findall(ArgTypes,PropTypes),call( dbase_define_db_prop(ArgTypes,PropTypes))),DBM))),
   !.
scan_db_prop:-!.

functor_safe(Obj,Obj):-not(compound(Obj)),!.
functor_safe(_:Obj,F):-!,functor_safe(Obj,F).
functor_safe(Obj,F):-functor(Obj,F,_).

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



world_clear(Named):-fmt('Clearing world database: ~q.',[Named]).

pred_as_is(F,A):-moo:is_db_prop(F,A,flag),!.
pred_as_is(F,A):-moo:is_db_prop(F,A,module(_)),!.
pred_as_is(p,_):-!,fail.
pred_as_is(k,_):-!,fail.


holds_tcall(Call):-req(Call).

db_op( q,Term):-!,db_op0(q,Term).
db_op( r,Term):-!,db_op0(q,Term),!,db_op0(ra,Term).
db_op(Op,Term):-must(db_op0(Op,Term)).

db_op0(_Op,props(_Obj,Open)):-Open==[],!.
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

db_op_4(q,Fmt,1,C0):-is_decl_ft(Fmt),!,C0=..[_,A],format_complies(A,Fmt,_).
db_op_4(a,Fmt,1,_C0):-is_decl_ft(Fmt),!,dmsg(todo(dont_assert_is_decl_ft(Fmt))),!.
db_op_4(Op,Prop,_,C0):- 
   C0=..[Prop|ARGS],db_op_disj(Op,Prop,ARGS).

db_op_unit(_Op,Prop,ARGS,C0):- must(atom(Prop)), C0=..[Prop|ARGS].

% impl/1
db_op_disj(Op,Prop,ARGS):-moo:is_db_prop(Prop,_,impl(Other)),!,
	db_op_unit(Op,Other,ARGS,Unit),!,db_forall(Op,Unit).
% alias/1
db_op_disj(Op,Prop,ARGS):-moo:is_db_prop(Prop,_,alias(Other)),!,
	db_op_unit(Op,Other,ARGS,Unit),!,db_forall(Op,Unit).
% inverse/1
db_op_disj(Op,Prop,ARGS):-
      moo:is_db_prop(Prop,_,inverse(Other)),!,
      inverse_args(ARGS,Inverse),
      db_op_unit(Op,Other,Inverse,Unit1),
      db_op_unit(Op,Prop,ARGS,Unit2),
      db_forall(Op,(Unit1;Unit2)).

% assert/1
db_op_disj(a,Prop,ARGS):- moo:is_db_prop(Prop,_,assert(How)),!,call_pa(How,Prop,ARGS).
db_op_disj(Op,Prop,ARGS):- moo:is_db_prop(Prop,W,assert(How)),!,throw(cant(db_op_disj(Op,Prop,ARGS),moo:is_db_prop(Prop,W,assert(How)))).

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
db_forall(a,C0):- db_forall_quf(C0,U,C),must(U),functor(C,F,A),!, (moo:is_db_prop(F,A,singleValued) -> must(db_forall_assert_sv(C,F,A)) ; must(db_forall_assert_mv(C,F,A))).

db_forall(a,C):- functor(C,F,A),!, (moo:is_db_prop(F,A,singleValued) -> must(db_forall_assert_sv(C,F,A)) ; must(db_forall_assert_mv(C,F,A))).
db_forall(r,C):- ground(C),retractall(C),!.
db_forall(Op,C):-!,trace,throw(unhandled(db_forall(Op,C))).


db_forall_query((C0->C1;C2)):- !, (db_forall_query(C0) -> db_forall_query(C1) ; db_forall_query(C2)).
db_forall_query((C1;C2)):-!, db_forall_query(C1) ; db_forall_query(C2).
db_forall_query(Term):-findall(a(Term),db_forall_query0(Term),List),list_to_set_safe(List,Set),!,member(a(Term),Set).
db_forall_query0((C1,C2)):-!, db_forall_query0(C1) , db_forall_query0(C2).
db_forall_query0(Term):- once(expand_goal(Term,Term2)),Term\==Term2,!,db_forall_query0(Term2).
db_forall_query0(Term):-db_forall_query1(Term).
db_forall_query1(C):- predicate_property(C,_PP),!,call(C).
db_forall_query1(C):- trace, db_forall_quf(C,Pretest,Template),!,Pretest, call(Template).



% only place ever should actual game dbase be changed from
hooked_asserta(C):-moo:run_database_hooks(assert(a),C),asserta_cloc(C).
hooked_assertz(C):- moo:run_database_hooks(assert(z),C), assertz_cloc(C).
hooked_retract(C):- moo:run_database_hooks(retract(one),C), must(retract_cloc(C)).
hooked_retractall(C):- moo:run_database_hooks(retract(all),C), retractall_cloc(C).

ensure_db_predicate(AR,M:C,_,F,A,G):- nonvar(M), !,ensure_db_predicate(AR,C,M,F,A,G).
ensure_db_predicate(AR,CI,M,F,A,G):-cyc_goal_expansion(CI,C),CI\== C,!,ensure_db_predicate(AR,C,M,F,A,G).
ensure_db_predicate(AR,CI,M,F,A,G):-cyc_head_expansion(CI,C), CI\== C,!,ensure_db_predicate_2(AR,C,M,F,A,G).
ensure_db_predicate(AR,CI,M,F,A,G):-ensure_db_predicate_2(AR,CI,M,F,A,G).

ensure_db_predicate_2(AR,CI,M,F,A,G):-cyc_head_expansion(CI,C),CI\==C,!,ensure_db_predicate_2(AR,C,M,F,A,G).
ensure_db_predicate_2(AR,C,M,F,A,G):- get_module_of(C,M),!, functor(C,F,A), make_predicate(M,F,A),prepend_module(C,M,G).
ensure_db_predicate_2(AR,C,M,F,A,G):- 
   must(functor(C,F,A)),
   declare_dbase_local(M:F,A),trace,
   get_module_of(C,M),!,
   make_predicate(M,F,A),prepend_module(C,M,G).

% ensure_db_predicate(a,agent('NpcCol1000-Geordi684'),dbase,_G176791,_G176792,_G176793)

prepend_module(_:C,M,M:C):-!.
prepend_module(C,M,M:C).

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
   (clause_present(C,F,A) -> true; (moo:is_db_prop(F,A,ordered)-> hooked_assertz(C) ; hooked_asserta(C))).

% assert to a singlevalue pred
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
clause_present(C,F,1):-C=..[F,A], is_decl_ft(F), format_complies(A,F,_).
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

is_holds_true(Prop):-hotrace((atom(Prop),is_holds_true0(Prop))).
is_holds_true0(Prop):-member(Prop,[k,p,holds,holds_t,dbase_true,res,assertion_holds,assertion]).

is_2nd_order_holds(Prop):- is_holds_true(Prop) ; is_holds_false(Prop).

is_holds_false(Prop):-hotrace((atom(Prop),is_holds_true0(Stem),is_holds_false0(Prop,Stem))).
is_holds_false0(Prop,Stem):-atom_concat('not_',Stem,Prop).
is_holds_false0(Prop,Stem):-atom_concat(Stem,'_not',Prop).
is_holds_false0(Prop,Stem):-atom_concat(Stem,'_false',Prop).
is_holds_false0(Prop,Stem):-atom_concat(Stem,'_f',Prop).


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
argIsa_call_0(Prop,N1,Type):- moo:is_db_prop(Prop,_,argIsa(N1,Type)),!.

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

is_single_valuedOrFail(F,A,Obj,ARGS):-moo:is_db_prop(F,A,singleValued),!,valuedOrThrow(F,A,Obj,ARGS),!.
is_single_valuedOrFail(_,_,_,_):-fail.

valuedOrThrow(F,_,Obj,ARGS):-mud_isa(Obj,T),findall_type_default_props(Obj,T,Props),Props=[_|_],Prop=..[F|ARGS], member_or_e(Prop,Props),!.
valuedOrThrow(F,A,Obj,ARGS):-valuedOrThrow1(F,A,Obj,ARGS).
valuedOrThrow1(_F,_A,_Obj,ARGS):-last(ARGS,unknown),!.
valuedOrThrow1(F,A,Obj,ARGS):-throw(is_single_valuedOrFail(F,A,Obj,ARGS)).



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
      moo:is_db_prop(F,A,Info),member(Info,[singleValued,multi(_)]),
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
moo:term_anglify(moo:term_anglify(Term,List),[prolog(Term),is,converted,to,english,using,prolog(Text)]).


add_arg_parts_of_speech(_F,_N,[],[]).
add_arg_parts_of_speech(F,N,[A|ARGS0],[ARG|ARGS]):-argIsa_call_or_undressed(F,N,A,ARG),N1 is N+1, add_arg_parts_of_speech(F,N1,ARGS0,ARGS).

argIsa_call_or_undressed(F,N,Obj,fN(Obj,Type)):-argIsa_call_0(F,N,Type),!.
argIsa_call_or_undressed(F,N,Obj,Obj).

verb_after_arg(_,_,1).


db_prop_argsIsa(ArgTypes):-db_prop_findall(ArgTypes,_).


db_prop_findall(ArgTypes,PROPS):-moo:db_prop(ArgTypes,PROPS).
db_prop_findall(ArgTypes,PROPS):-db_prop_multi(ArgTypes,PROPS).
db_prop_findall(ArgTypes,[]):-moo:db_prop(ArgTypes).
db_prop_findall(ArgTypes,[]):-db_prop_sv(ArgTypes).
db_prop_findall(ArgTypes,[]):-db_prop_multi(ArgTypes).


moo:db_prop(ArgTypes):-db_prop_from_game_load(ArgTypes).

:- style_check(+discontiguous).

% =================================================================================================
% BEGIN world database
% =================================================================================================

:- begin_transform_cyc_preds.

inRegion(O,Region):-atloc(O,LOC),locationToRegion(LOC,Region).
inRegion(apath(Region,Dir),Region):- pathBetween(Region,Dir,_To).

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



moo:db_prop(look:get_feet(agent,list(spatial))).
moo:db_prop(look:get_near(agent,list(spatial)),[module(look)]).
moo:db_prop(get_precepts(agent,list(spatial)),[module(look)]).



type(T):-moo:subclass(A,B),(T=B;T=A).
type(item).


nameString(apath(Region,Dir),Text):- pathName(Region,Dir,Text).
description(apath(Region,Dir),Text):- holds_t(pathName(Region,Dir,Text)).

moo:decl_action(list(term)).
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

moo:specifier_text(Text,pred):- moo:is_db_prop(Text,_,arity(_,_)).

% single valued
moo:decl_subclass(agent,object).
moo:decl_subclass(item,object).


moo:db_prop(pathName(region,dir,string)).
moo:db_prop_multi(verbAsWell(term,action,action)).
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
db_prop_sv(id(object,id)).
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

moo:decl_subclass(areaPath,door).
moo:decl_subclass(door,item).

moo:decl_subclass(dir,string).

% flags
db_prop_flag(agent(id)).
db_prop_flag(item(id)).
db_prop_flag(region(id)).
db_prop_flag(type(id)).

db_prop_flag(thinking(agent)).
db_prop_flag(deleted(id)).


% multivalued
db_prop_multi(G,AT,[ordered|LIST]):-db_prop_multi(G,LIST),functor_safe(G,_,AT).

db_prop_multi(named(term,term),[genlpreds(id)]).
db_prop_multi(ofclass(term,type),[alias(mud_isa)]).
db_prop_multi(G,[]):-db_prop_multi(G).

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

:-end_transform_cyc_preds.
% =================================================================================================
% END world database
% =================================================================================================

load_motel:- defrole([],time_state,restr(time,period)).

:- scan_db_prop.

:- dbase_define_db_prop(mudMaxHitPoints(agent,int)).

:- load_motel.

% :- include(logicmoo('vworld/moo_footer')).




