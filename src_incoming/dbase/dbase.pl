/** <module> 
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

:-op(1150,fx,export).

:- '@'(use_module(logicmoo(vworld/moo)),'user').
% :- '@'(use_module(dbase_formattypes),'user').

:-include(dbase_formattypes).

%:- trace, (dynamic_multifile_exported  moo:obj/1). 
%:- trace, (dynamic_multifile_exported  obj/1). 


:- export(( 
   add/1, 
   clr/1,
   db_op/2,
  del/1,  
  padd/2, padd/3, prop/3, prop_or/4, props/2, req/1, term_listing/1, 
  use_term_listing/2,  world_clear/1,  
   with_kb_assertions/2
  )).


:- dynamic_multifile_exported 
   agent/1, agent_doing/2, agent_done/2, charge/2,damage/2, atloc/2, failure/2, grid/4, isa/2, item/1, 
  memory/2,  pathName/3, possess/2,  region/1, score/2, stm/2,   facing/2,
   % type/1,
   inRegion/2,
   
  thinking/1,   wearing/2, 
  %str/2,
  facing/2, height/2, act_term/2, nameStrings/2, description/2, pathBetween/3, agent_turnnum/2.


:- dbase_mod(M),dynamic_multifile_exported((
          % M:dbase_t/1,
          M:dbase_t/2,
          M:dbase_t/3,
          M:dbase_t/4,
          M:dbase_t/5,
          M:dbase_t/6,
          M:dbase_t/7)).

:-dynamic(weight/2).
:-dynamic(movedist/2).
:-export(movedist/2).
% :-dynamic(subclass/2).
:-export(subclass/2).

:- dynamic_multifile_exported mtForPred/2.

:- decl_mpred_hybrid((
     type/1, agent/1, item/1, region/1,
     verbOverride/3,named/2, determinerString/2, keyword/2 ,descriptionHere/2, 
     mudToHitArmorClass0/2,

      thinking/1,
 weight/2,
 permanence/3,
      act_term/2,
      agent_turnnum/2,
      agent_doing/2,
      agent_done/2,
      atloc/2,
      charge/2,
      damage/2,
      description/2,
      facing/2,
      failure/2,
      spd/2,
      grid/4,
      height/2,
      memory/2,
      
      isa/2,
      pathName/3, 
      possess/2,
      score/2,
      stm/2,      
      str/2,
      wearing/2)).

logical_functor(X):-atom(X),member(X,[',',';']).


:- decl_mpred_hybrid((
stat_total/2,
armorLevel/2,
mudLevelOf/2,
mudToHitArmorClass0/2,
mudBareHandDamage/2,
chargeCapacity/2,
chargeRemaining/2,
     type/1, agent/1, item/1, region/1,
     verbOverride/3,named/2, determinerString/2, keyword/2 ,descriptionHere/2, 

      thinking/1,
 weight/2,
 permanence/3,
      act_term/2,
      agent_turnnum/2,
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
      isa/2,
      pathName/3, 
      possess/2,
      score/2,
      stm/2,      
      str/2,
      wearing/2)).

:-multifile inRegion/2.
/*

dbase_mod(moo).
:- context_module(M),
   asserta(dbase_mod(M)),
   dmsg(assert_if_new(dbase_mod(M))).

*/

%:- multifile argsIsa/2.

:- meta_predicate man:with_assertions(:,0).
:- meta_predicate world:intersect(?,0,?,0,0,-).

%% :- meta_predicate del(0),clr(0),add(0),add0(0),req(0), db_op(0,0,0).

% Found new meta-predicates in iteration 1 (0.281 sec)
%:- meta_predicate db_forall(?,?,?,0).

:- include(logicmoo('vworld/moo_header.pl')).

% :- include('dbase_types_motel').

% :- user_use_module(dbase_rules_pttp).

:- register_module_type(utility).

moo:actiontype(list(term)).
moo:agent_call_command(_Gent,list(Obj)):- term_listing(Obj).

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
:-export((holds_t/1,holds_t/2,holds_t/3,holds_t/4,holds_t/5,holds_t/6,holds_t/7,holds_t/8)).
holds_t(P,A1,A2,A3,A4,A5,A6,A7):- req(dbase_t(P,A1,A2,A3,A4,A5,A6,A7)).
holds_t(P,A1,A2,A3,A4,A5,A6):- req(dbase_t(P,A1,A2,A3,A4,A5,A6)).
holds_t(P,A1,A2,A3,A4,A5):- req(dbase_t(P,A1,A2,A3,A4,A5)).
holds_t(P,A1,A2,A3,A4):- req(dbase_t(P,A1,A2,A3,A4)).
holds_t(P,A1,A2,A3):- req(dbase_t(P,A1,A2,A3)).
holds_t(P,A1,A2):- req(dbase_t(P,A1,A2)).
holds_t(P,A1):- req(dbase_t(P,A1)).
holds_t(G):- req(G).


isCycPredArity_ignoreable(P,A):- mpred_prop(P,useCycPred), hotrace(ignore(mpred_arity(P,A))).

dbase_which_next(dac(no_d,a,no_c,no_mt)).

dbase_t(P,A1,A2,A3,A4,A5,A6,A7):- isCycPredArity_ignoreable(P,7),dbase_which_next(DBS),(call_t(DBS,P,A1,A2,A3,A4,A5,A6,A7);call_mt_t(DBS,P,A1,A2,A3,A4,A5,A6,A7,_,_);assertion_t([P,A1,A2,A3,A4,A5,A6,A7])).
dbase_t(P,A1,A2,A3,A4,A5,A6):- isCycPredArity_ignoreable(P,6),dbase_which_next(DBS),(call_t(DBS,P,A1,A2,A3,A4,A5,A6);call_mt_t(DBS,P,A1,A2,A3,A4,A5,A6,_,_)).
dbase_t(P,A1,A2,A3,A4,A5):- isCycPredArity_ignoreable(P,5),dbase_which_next(DBS),(call_t(DBS,P,A1,A2,A3,A4,A5);call_mt_t(DBS,P,A1,A2,A3,A4,A5,_,_)).
dbase_t(P,A1,A2,A3,A4):- isCycPredArity_ignoreable(P,4),dbase_which_next(DBS),(call_t(DBS,P,A1,A2,A3,A4);call_mt_t(DBS,P,A1,A2,A3,A4,_,_)).
dbase_t(P,A1,A2,A3):- isCycPredArity_ignoreable(P,3),dbase_which_next(DBS),(call_t(DBS,P,A1,A2,A3);call_mt_t(DBS,P,A1,A2,A3,_,_)).
dbase_t(P,A1,A2):- hotrace(cholds_relaxed_t(P,A1,A2)).
% dbase_t(P,A1):- !,dbase_t(isa,A1,P).
dbase_t(P,A1):- isCycPredArity_ignoreable(P,1),dbase_which_next(DBS),(call_t(DBS,P,A1);call_mt_t(DBS,P,A1,_,_)).

% ================================================================================
% begin cholds_t
% ================================================================================
which_t(dac(no_d,a,no_c,no_mt)).
cholds_t(P,A1,A2,A3,A4,A5,A6,A7):- isCycPredArity_ignoreable(P,7),which_t(DBS),(call_t(DBS,P,A1,A2,A3,A4,A5,A6,A7);call_mt_t(DBS,P,A1,A2,A3,A4,A5,A6,A7,_,_);assertion_t([P,A1,A2,A3,A4,A5,A6,A7])).
cholds_t(P,A1,A2,A3,A4,A5,A6):- isCycPredArity_ignoreable(P,6),which_t(DBS),(call_t(DBS,P,A1,A2,A3,A4,A5,A6);call_mt_t(DBS,P,A1,A2,A3,A4,A5,A6,_,_)).
cholds_t(P,A1,A2,A3,A4,A5):- isCycPredArity_ignoreable(P,5),which_t(DBS),(call_t(DBS,P,A1,A2,A3,A4,A5);call_mt_t(DBS,P,A1,A2,A3,A4,A5,_,_)).
cholds_t(P,A1,A2,A3,A4):- isCycPredArity_ignoreable(P,4),which_t(DBS),(call_t(DBS,P,A1,A2,A3,A4);call_mt_t(DBS,P,A1,A2,A3,A4,_,_)).
cholds_t(P,A1,A2,A3):- isCycPredArity_ignoreable(P,3),which_t(DBS),(call_t(DBS,P,A1,A2,A3);call_mt_t(DBS,P,A1,A2,A3,_,_)).
cholds_t(P,A1,A2):- hotrace(cholds_relaxed_t(P,A1,A2)).
% cholds_t(P,A1):- !,cholds_t(isa,A1,P).
cholds_t(P,A1):- isCycPredArity_ignoreable(P,1),which_t(DBS),(call_t(DBS,P,A1);call_mt_t(DBS,P,A1,_,_)).

cholds_relaxed_t(P,A1,A2):- isCycPredArity_ignoreable(P,2),which_t(DBS),
      relax_term(P,PR,A1,R1,A2,R2),
         cholds_relaxed_0_t(DBS,PR,R1,R2).

cholds_relaxed_0_t(DBS,P,A1,A2):- call_t(DBS,P,A1,A2).
cholds_relaxed_0_t(DBS,P,A1,A2):- call_mt_t(DBS,P,A1,A2,_,_).

/*
cholds_relaxed_0_t(dac(_,a,_,_),P,A1,A2):- assertion_t([P,A1,A2]).
cholds_relaxed_0_t(dac(d,_,_,_),P,A1,A2):- dbase_t(P,A1,A2).
cholds_relaxed_0_t(dac(_,_,_,h),P,A1,A2):- call_t(DBS,P,A1,A2).
cholds_relaxed_0_t(DBS,P,A1,A2):- call_mt_t(DBS,P,A1,A2,_,_).
cholds_relaxed_0_t(_DBS,P,A1,A2):- ground((P,A1)), TEMPL=..[P,T1,_],dbase_t(default_sv,TEMPL,2,A2),req(isa(A1,T1)),!.
*/

cholds_t([AH,P|LIST]):- is_holds_true(AH),!,cholds_t_p2(P,LIST).
cholds_t([AH,P|LIST]):- is_holds_false(AH),!,holds_f_p2(P,LIST).
cholds_t([P|LIST]):- !,cholds_t_p2(P,LIST).
cholds_t(not(CALL)):-cholds_f(CALL).
cholds_t(CALL):- safe_univ(CALL,[P|LIST]),cholds_t([P|LIST]).

cholds_t_p2(P,LIST):- safe_univ(CALL,[cholds_t,P|LIST]),call(CALL).


call_list_t(dac(d,_,_,_),CALL,_):- dbase_t(CALL).
call_list_t(dac(_,a,_,_),_,List):- assertion_t(List).
call_list_t(dac(_,_,c,_),CALL,_):- xcall_t(CALL).
call_list_t(dac(_,_,_,h),CALL,_):- holds_t(CALL).

call_t(DBS,P,A1,A2,A3,A4,A5,A6,A7):- callable_tf(P,7),List= [P,A1,A2,A3,A4,A5,A6,A7], CALL=..List, call_list_t(DBS,CALL,List).
call_t(dac(_,_,_,h),P,A1,A2,A3,A4,A5,A6,A7):- holds_t(P,A1,A2,A3,A4,A5,A6,A7).

call_t(dac(d,_,_,_),P,A1,A2,A3,A4,A5,A6):- dbase_t(P,A1,A2,A3,A4,A5,A6).
call_t(dac(_,a,_,_),P,A1,A2,A3,A4,A5,A6):- assertion_t([P,A1,A2,A3,A4,A5,A6]).
call_t(dac(_,_,c,_),P,A1,A2,A3,A4,A5,A6):- callable_tf(P,6),xcall_t(P,A1,A2,A3,A4,A5,A6).
call_t(dac(_,_,_,h),P,A1,A2,A3,A4,A5,A6):- holds_t(P,A1,A2,A3,A4,A5,A6).

call_t(dac(d,_,_,_),P,A1,A2,A3,A4,A5):- dbase_t(P,A1,A2,A3,A4,A5).
call_t(dac(_,a,_,_),P,A1,A2,A3,A4,A5):- assertion_t([P,A1,A2,A3,A4,A5]).
call_t(dac(_,_,c,_),P,A1,A2,A3,A4,A5):- callable_tf(P,5),xcall_t(P,A1,A2,A3,A4,A5).
call_t(dac(_,_,_,h),P,A1,A2,A3,A4,A5):- holds_t(P,A1,A2,A3,A4,A5).

call_t(dac(d,_,c,_),P,A1,A2,A3,A4):- dbase_t(P,A1,A2,A3,A4).
call_t(dac(_,a,_,_),P,A1,A2,A3,A4):- assertion_t([P,A1,A2,A3,A4]).
call_t(dac(_,_,c,_),P,A1,A2,A3,A4):- callable_tf(P,4),xcall_t(P,A1,A2,A3,A4).
call_t(dac(_,_,_,h),P,A1,A2,A3,A4):- holds_t(P,A1,A2,A3,A4).

call_t(dac(d,_,_,_),P,A1,A2,A3):- dbase_t(P,A1,A2,A3).
call_t(dac(_,a,_,_),P,A1,A2,A3):- assertion_t([P,A1,A2,A3]).
call_t(dac(_,_,c,_),P,A1,A2,A3):- callable_tf(P,3),xcall_t(P,A1,A2,A3).
call_t(dac(_,_,_,h),P,A1,A2,A3):- holds_t(P,A1,A2,A3).

call_t(dac(d,_,_,_),P,A1,A2):- dbase_t(P,A1,A2).
call_t(dac(_,a,_,_),P,A1,A2):- assertion_t([P,A1,A2]).
call_t(dac(_,_,c,_),P,A1,A2):- callable_tf(P,2),xcall_t(P,A1,A2).
call_t(dac(_,_,_,h),P,A1,A2):- holds_t(P,A1,A2).

call_t(dac(d,_,_,_),P,A1):- dbase_t(P,A1).
call_t(dac(_,a,_,_),P,A1):- assertion_t([P,A1]).
call_t(dac(_,_,c,_),P,A1):- callable_tf(P,1),xcall_t(P,A1).
call_t(dac(_,_,_,h),P,A1):- holds_t(P,A1).

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
% assertion_t([P|LIST]):- Call=..[dbase_t,P|LIST],Call.
assertion_t(_):- not(useExternalDBs),!,fail.
assertion_t(C):-dmsg(enter_assertion_t(C)),fail.
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

dbase_f(List):- is_list(List),!,Call=..[dbase_f|List],Call.
dbase_f(List):- holds_f(List).


call_f(_,P,A1,A2,A3,A4,A5,A6,A7):- callable_tf(P,7),List= [P,A1,A2,A3,A4,A5,A6,A7], CALL=..List,(assertion_f(List);dbase_f(CALL);xcall_f(CALL)).
call_f(dac(d,_,_,_),P,A1,A2,A3,A4,A5,A6):- dbase_f(P,A1,A2,A3,A4,A5,A6).
call_f(dac(_,a,_,_),P,A1,A2,A3,A4,A5,A6):- assertion_f([P,A1,A2,A3,A4,A5,A6]).
call_f(dac(_,_,c,_),P,A1,A2,A3,A4,A5,A6):- callable_tf(P,6),xcall_f(P,A1,A2,A3,A4,A5,A6).
call_f(dac(d,_,_,_),P,A1,A2,A3,A4,A5):- dbase_f(P,A1,A2,A3,A4,A5).
call_f(dac(_,a,_,_),P,A1,A2,A3,A4,A5):- assertion_f([P,A1,A2,A3,A4,A5]).
call_f(dac(_,_,c,_),P,A1,A2,A3,A4,A5):- callable_tf(P,5),xcall_f(P,A1,A2,A3,A4,A5).
call_f(dac(d,_,_,_),P,A1,A2,A3,A4):- dbase_f(P,A1,A2,A3,A4).
call_f(dac(_,a,_,_),P,A1,A2,A3,A4):- assertion_f([P,A1,A2,A3,A4]).
call_f(dac(_,_,c,_),P,A1,A2,A3,A4):- callable_tf(P,4),xcall_f(P,A1,A2,A3,A4).
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



:-declare_dbase_local_dynamic(atloc,2).

:-include(dbase_i_pldoc).
:-include(dbase_i_coroutining).
%:-discontiguous(singleValued/2).
%:-discontiguous(multiValued/1).
% =================================================================================================
% world database
% =================================================================================================

:-export(add_deduction/3).
quiet_fact(Fact):-functor(Fact,F,A),quiet_fact(F,A).
quiet_fact(isa,_).
quiet_fact(argIsa,_).
quiet_fact(mpred_prop,_).

add_deduction(_Type,[],_How):-!.
add_deduction(Type,[Fact|S],How):-!,add_deduction(Type,Fact,How),add_deduction(Type,S,How),!.
add_deduction(Type,Fact,How):- ignore(loop_check_term(add_deduction_lc(Type,Fact,How),add_deduction_lc(Type,Fact),true)),!.

add_deduction_lc(Type,Fact,_How):-quiet_fact(Fact),!,do_deduction_type(Type,Fact),!.
add_deduction_lc(Type,Fact,How):-dmsg(add_deduction(Type,Fact,'_________from________',How)),do_deduction_type(Type,Fact),!.

do_deduction_type(assert(_),Fact):-add(Fact).
do_deduction_type(retract(_),Fact):-clr(Fact).


% TODO: canonicalize clauses first!
with_kb_assertions([],Call):- !,Call.
with_kb_assertions([With|MORE],Call):-!,with_kb_assertions(With,with_kb_assertions(MORE,Call)).
with_kb_assertions(With,Call):-
   setup_call_cleanup(asserta(With,Ref),Call,erase(Ref)).



world_clear(Named):-fmt('Clearing world database: ~q.~n',[Named]).

% pred_as_is(F,_):-mpred_prop(F,flag),!.
pred_as_is(F,A):-get_mpred_prop(F,A,as_is(_Why)),!.
pred_as_is(F,A):-get_mpred_prop(F,A,external(_)),!.
pred_as_is(p,_):-!,fail.
pred_as_is(dbase_t,_):-!,fail.
pred_as_is(k,_):-!,fail.

extreme_debug(P):-nop(P).


:- meta_predicate hooked_asserta(^), hooked_assertz(^), hooked_retract(^), hooked_retractall(^).

:- meta_predicate del(-),clr(-),add(-),req(-), db_op(-,-).

% Found new meta-predicates in iteration 1 (0.281 sec)
%:- meta_predicate db_op_exact(?,?,?,0).

% movedist(X,Y):-callStub_moo(holds_t,movedist(X,Y)).

:-decl_mpred_prolog(move:movedist/2).

% :- moo:register_module_type(utility).

% oncely later will throw an error if there where choice points left over by call
oncely(:-(Call)):-!,Call,!.
oncely(:-(Call)):-!,call_expanded(Call).
oncely(Call):-once(Call).

:- dynamic(non_assertable/1).
non_assertable(WW,isVar(WW)):- var(WW),!.
non_assertable(_:WW,Why):- !,non_assertable(WW,Why).
non_assertable(WW,notAssertable(Why)):- compound(WW),functor_catch(WW,F,_),mpred_prop(F,notAssertable(Why)),!.
% non_assertable(WW,as_is(Why)):- compound(WW),functor_catch(WW,F,_),!,mpred_prop(F,as_is(Why)),!.
% non_assertable(WW,Why):- db_prop_game_assert

% replaced the 1st with the 2nd and better version of retract
% del(C0):- db_op_int(retract,C0)
%% del(RetractOne)    <--  del(C0):- ignore((db_op(query(HLDS,Must),C0),!,db_op('retract',C0))).
del(C0):- db_op_int('retract',C0).
del(C0):- dmsg(failed(del(C0))),!,fail.
%% clr(Retractall)
clr(C0):- db_op_int(ra,C0).
%% req(Query)
req(C0):- db_op_int(query(dbase_t,req),C0).
mreq(C0):- must(req(C0)).
%% props(Obj,QueryPropSpecs)
props(Obj,PropSpecs):- req(props(Obj,PropSpecs)).
%% add(Assertion)
add(C0):- must_det((db_op_int(tell(add), C0), extreme_debug(req(C0)))),!.
%% padd(Obj,PropSpecs)
padd(Obj,PropSpecs):- add(props(Obj,PropSpecs)).
%% padd(Obj,Prop,Value)
padd(Obj,Prop,Value):- add(dbase_t(Prop,Obj,Value)).
%% prop(Obj,Prop,Value)
prop(Obj,Prop,Value):- req(dbase_t(Prop,Obj,Value)).
%% prop_or(Obj,Prop,Value,OrElse)
prop_or(Obj,Prop,Value,OrElse):- one_must(dbase_t(Prop,Obj,Value),Value=OrElse).


kb_update(New,OldV):- req(New),!,OldV=New.
kb_update(New,OldV):- db_op_int(tell(OldV),New).

:-thread_local((record_on_thread/2)).

hook:decl_database_hook(assert(_),ft_info(FT,_)):- define_ft(FT).
hook:decl_database_hook(assert(_),subft(FT,OFT)):- define_ft(OFT),define_ft(FT).
% hook:decl_database_hook(assert(_),subclass(FT,OFT)):- formattype(OFT),dmsg(warning(subclass_of_define_ft(FT))).


hook:dmsg_hook(transform_holds(dbase_t,_What,props(creatableType,[isa(isa),isa]))):-dtrace.

% expand_goal_correct_argIsa(A,A):-simple_code,!.
expand_goal_correct_argIsa(A,B):- expand_goal(A,B).

% db_op_simpler(query(HLDS,_),MODULE:C0,call_expanded(call,MODULE:C0)):- atom(MODULE), nonvar(C0),not(not(predicate_property(C0,_PP))),!. % , functor_catch(C0,F,A), dmsg(todo(unmodulize(F/A))), %trace_or_throw(module_form(MODULE:C0)), %   db_op(Op,C0).
db_op_simpler(_,TypeTerm,props(Inst,[isa(Type)|PROPS])):- TypeTerm=..[Type,Inst|PROPS],nonvar(Inst),type(Type),!.

foreach_arg(ARGS,_N,_ArgIn,_ArgN,_ArgOut,_Call,ARGS):-not(compound(ARGS)),!.
foreach_arg([ArgIn1|ARGS],ArgN1,ArgIn,ArgN,ArgOut,Call1,[ArgOut1|ARGSO]):-
     copy_term( a(ArgIn1,ArgOut1,ArgN1,Call1), a(ArgIn,ArgOut,ArgN,Call) ),
      call(Call),
      ArgN2 is ArgN + 1,
      foreach_arg(ARGS,ArgN2,ArgIn,ArgN,ArgOut,Call,ARGSO).


transform_functor_holds(_,F,ArgInOut,N,ArgInOut):- once(argIsa_ft(F,N,FT)),FT=term,!.
transform_functor_holds(Op,_,ArgIn,_,ArgOut):- transform_holds(Op,ArgIn,ArgOut),!.

transform_holds(H,In,Out):- hotrace((transform_holds_3(H,In,Out))),!,ignore((In\=Out,fail,dmsg(transform_holds(H,In,Out)))).

transform_holds_3(_,A,A):-not(compound(A)),!.
transform_holds_3(_,A,A):-functor_catch(A,F,N), predicate_property(A,_),mpred_prop(F,arity(N)),!.
transform_holds_3(_,props(Obj,Props),props(Obj,Props)).
transform_holds_3(Op,M:Term,OUT):-atom(M),!,transform_holds_3(Op,Term,OUT).
transform_holds_3(HLDS,[P,A|ARGS],DBASE):- var(P),!,DBASE=..[HLDS,P,A|ARGS].
transform_holds_3(HLDS, ['[|]'|ARGS],DBASE):- trace_or_throw(list_transform_holds_3(HLDS,['[|]'|ARGS],DBASE)).
transform_holds_3(Op,[SVOFunctor,Obj,Prop|ARGS],OUT):- is_svo_functor(SVOFunctor),!,transform_holds_3(Op,[Prop,Obj|ARGS],OUT).
transform_holds_3(_,[P|ARGS],[P|ARGS]):- not(atom(P)),!,dmsg(transform_holds_3),dtrace.
transform_holds_3(Op,[HOLDS,P,A|ARGS],OUT):- is_holds_true(HOLDS),!,transform_holds_3(Op,[P,A|ARGS],OUT).
transform_holds_3(HLDS,[HOLDS,P,A|ARGS],OUT):- HLDS==HOLDS, !, transform_holds_3(HLDS,[P,A|ARGS],OUT).

transform_holds_3(_,[Type,Inst],isa(Inst,Type)):-must_det(type(Type)).
transform_holds_3(_,HOLDS,isa(I,C)):- was_isa(HOLDS,I,C),!.
transform_holds_3(_,HOLDS,isa(I,C)):- holds_args(HOLDS,[ISA,I,C]),ISA==isa,!.

transform_holds_3(Op,[Logical|ARGS],OUT):- 
         hotrace(logical_functor(Logical)),!,must(not(is_svo_functor(Logical))),
         must_det(foreach_arg(ARGS,1,ArgIn,ArgN,ArgOut,transform_functor_holds(Op,Logical,ArgIn,ArgN,ArgOut),LARGS)),
         OUT=..[Logical|LARGS].

transform_holds_3(_,[props,Obj,Props],props(Obj,Props)).
transform_holds_3(_,[Type,Inst|PROPS],props(Inst,[isa(Type)|PROPS])):- nonvar(Inst), not(Type=props), cached_isa(Type,type),must_det(not(never_type(Type))),!.
transform_holds_3(_,[P,A|ARGS],DBASE):- atom(P),!,DBASE=..[P,A|ARGS].
transform_holds_3(_,[P,A|ARGS],DBASE):- !, nonvar(P),dumpST,dtrace, DBASE=..[P,A|ARGS].
transform_holds_3(Op,DBASE_T,OUT):- DBASE_T=..[P,A|ARGS],!,transform_holds_3(Op,[P,A|ARGS],OUT).


db_op_simpler_wlc(query(HLDS,Must),Wild,Simpler):- !,hotrace(db_op_simpler(query(HLDS,Must),Wild,Simpler)),not(is_loop_checked(req(Simpler))),!.
db_op_simpler_wlc(tell(Must),Wild,Simpler):- !,hotrace(db_op_simpler(tell(Must),Wild,Simpler)),not(is_loop_checked(add(Simpler))),!.
db_op_simpler_wlc(Op,Wild,Simpler):- !,hotrace(db_op_simpler(Op,Wild,Simpler)),not(is_loop_checked(db_op0(Op,Simpler))),!.


db_op_sentence(_Op,Prop,ARGS,C0):- must_det(atom(Prop)), C0=..[Prop|ARGS],!.
db_op_sentence(_Op,Prop,ARGS,C0):- C0=..[dbase_t,Prop|ARGS].

hook:decl_database_hook(AR,C):-smart_decl_database(AR,C).

smart_decl_database(AR,svo(S,V,O)):- !,dbase2pred2svo(DBASE,PRED,svo(S,V,O)),!,smart_db_op(AR,DBASE,PRED,svo(S,V,O)).
smart_decl_database(AR,DBASE):- functor_catch(DBASE,dbase_t,_),!,dbase2pred2svo(DBASE,PRED,SVO),!,smart_db_op(AR,DBASE,PRED,SVO).
smart_decl_database(AR,PRED):- dbase2pred2svo(DBASE,PRED,SVO),!,smart_db_op(AR,DBASE,PRED,SVO).

smart_db_op(retract(AR),A,B,C):- retract_ar_fact(AR,A), retract_ar_fact(AR,B),  retract_ar_fact(AR,C).

retract_ar_fact(all,What):- predicate_property(What,dynamic), !, doall((retract_ar_fact(one,What),fail)).
retract_ar_fact(all,What):- not(predicate_property(What,_)),!.
retract_ar_fact(all,What):- copy_term(What,WO),ignore(once(WO)),must_det(What=@=WO).

retract_ar_fact(one,What):- predicate_property(What,dynamic),!, clause(What,true),retract(What:-true).
retract_ar_fact(one,What):- predicate_property(What,_),!, clause_safe(What,true),!.
retract_ar_fact(one,What):- dmsg(mssing(retract_ar_fact(one,What))).
% ================================================
% assert_isa/2
% ================================================
:-export assert_isa/2.

assert_isa(I,T):- not(ground(I:T)),trace_or_throw(not(ground(assert_isa(I,T)))).
assert_isa(I,T):- hotrace((loop_check(assert_isa_lc(I,T),true))).

:-export assert_isa_lc/2.
% skip formatter types
assert_isa_lc(_I,T):- member(T,[string,action,dir]),!.
assert_isa_lc(I,T):- hotrace(formattype(T)),!,dmsg(todo(dont_assert_is_ft(I,T))),!.
assert_isa_lc(_,T):- once(define_type(T)),fail.
assert_isa_lc(I,type):- define_type(I),!.
assert_isa_lc(I,formattype):- define_ft(I),!.
assert_isa_lc(I,T):- cannot_table_call(get_isa_asserted(I,T)),!.
assert_isa_lc(I,T):- is_stable,!, hooked_asserta(isa(I,T)).
assert_isa_lc(I,T):- must_det((hooked_asserta(isa(I,T)),logOnFailureIgnore(get_isa_backchaing(I,T)))).

% ================================================
% assert_isa HOOKS
% ================================================
hook:decl_database_hook(assert(_),DATA):-into_mpred_form(DATA,O),!,O=isa(I,T),hotrace(doall(assert_isa_hooked(I,T))).
% hook:decl_database_hook(retract(_),isa(I,T)):-doall(db_retract_isa_hooked(I,T)).

assert_isa_hooked(I,T):- not(ground(assert_isa(I,T))),!, trace_or_throw(not(ground(assert_isa(I,T)))).
assert_isa_hooked(I,T):- asserta_if_new(moo:dbase_t(T,I)),fail.
assert_isa_hooked(T,type):-!,define_type(T),!.
assert_isa_hooked(T,formattype):-!,define_ft(T),!.
assert_isa_hooked(Term,mpred):-!,decl_mpred(Term).
assert_isa_hooked(Term,prologHybrid):-!,decl_mpred_hybrid(Term).
assert_isa_hooked(Term,prologOnly):-!,decl_mpred_prolog(Term).
assert_isa_hooked(I,_):- glean_pred_props_maybe(I),fail.
assert_isa_hooked(I,T):- argsIsaProps(T),decl_mpred(I,T),fail.
assert_isa_hooked(food5,'Weapon'):-trace_or_throw(assert_isa(food5,'Weapon')).

% assert_isa_hooked(I,T):- motel:defconcept(I,and([lexicon,T])).
% assert_isa_hooked(I,T):- motel:defprimconcept(I,T).
% assert_isa_hooked(I,T):-dmsg((told(assert_isa(I,T)))).


hook:decl_database_hook(assert(_),isa(I,T)):- doall(assert_isa_hooked_after(I,T)).

assert_isa_hooked_after(_,type):-!.
assert_isa_hooked_after(_,formattype):-!.
assert_isa_hooked_after(_,T):-argsIsaProps(T),!.
assert_isa_hooked_after(I,T):- is_creatable_type(T),!,assert_isa_hooked_creation(I,T).
assert_isa_hooked_after(I,T):- not(extentKnown(T)),impliedSubClass(T,ST),extentKnown(ST),assert_isa(I,ST).
%assert_isa_hooked_after(I,T):- assert_isa_hooked_creation(I,T).

extentKnown(Ext):- arg(_,vv(type,dir,formattype,string),Ext).
extentKnown(F):- is_creatable_type(F).
extentKnown(F):- argsIsaProps(F).
extentKnown(F):- is_asserted(isa(F,extentKnown)).


% one of 4 special types
assert_isa_hooked_creation(I,T):- is_creatable_type(T),!,call_after_game_load((world:create_instance(I,T,[]))).
% sublass of 4 special types
assert_isa_hooked_creation(I,T):- doall((is_creatable_type(ST),impliedSubClass(T,ST),call_after_game_load((world:create_instance(I,ST,[isa(T)]))))).

:-export transitive_subclass_tst/2.
transitive_subclass_tst(_,_):-!,fail.


:-export(get_isa_backchaing/2).
% get_isa_backchaing(A,T):- stack_depth(Level),Level>650,trace_or_throw(skip_dmsg_nope(failing_stack_overflow(get_isa_backchaing(A,T)))),!,fail.


get_isa_backchaing(A,T):- hotrace((fact_loop_checked(isa(A,T),get_isa_backchaing_0(A,T)))).

get_isa_backchaing_0(A,T):-  var(A),!,nonvar(T),setof(A,AT^(asserted_or_trans_subclass(AT,T),get_isa_asserted(A,AT)),List),!,member(A,List).
get_isa_backchaing_0(A,T):-  var(T),!,setof(TT,AT^(get_isa_asserted(A,AT),asserted_or_trans_subclass(AT,TT)),List),!,member(T,List).
get_isa_backchaing_0(A,T):-  get_isa_asserted(A,AT),asserted_or_trans_subclass(AT,T).

not_ft(T):-asserted_or_trans_subclass(T,obj).

:-export(get_isa_asserted/2).
get_isa_asserted(A,T):- fact_loop_checked(isa(A,T),get_isa_asserted_0(A,T)).

get_isa_asserted_0(A,T):- alt_dbase_t(T,A).
get_isa_asserted_0(A,T):- nonvar(A),nonvar(T),cached_isa(T,formattype),!,term_is_ft(A,T).
% get_isa_asserted_0(A,T):- compound(A),functor(A,F,_),!,get_isa_backchaing_1(F,T).
get_isa_asserted_0(_,T):- not(atom(T)),!,fail.
get_isa_asserted_0(A,T):- argsIsaProps(T),mpred_prop(A,T).
get_isa_asserted_0(A,T):- atom(A),mud_isa_atom(A,T),!.


mud_isa_atom(O,T):- atomic_list_concat_catch([T,_|_],'-',O),!.
mud_isa_atom(O,T):- atom_concat(T,Int,O),catch(atom_number(Int,_),_,fail),!.

cached_isa(I,T):-hotrace(get_isa_backchaing(I,T)).

generated_fact(Fact):- ground(Fact),!,req(Fact).
generated_fact(Fact):- (compound(Fact)-> true ; mpred_arity(F,A)), functor(Fact,F,A),Fact=..[F|Args],
   generate_args(F,1,A,Args).

generate_args(F,N,N,[Arg|_]):-!,generate_fact_arg(F,N,Arg).
generate_args(F,N,Until,[Arg|More]):- generate_fact_arg(F,N,Arg), N2 is N+1, generate_args(F,N2,Until,More).

generate_fact_arg(F,N,Arg):-call_argIsa(F,N,Type),!,get_isa_arg_test(Arg,Type).

get_isa_arg_test(Arg,Type):-ground(Arg),!,show_call(get_isa_backchaing(Arg,Type)).
get_isa_arg_test(Arg,Type):-get_isa_backchaing(Arg,Type).

equivRule_call(A,B):- is_asserted(holds_t(equivRule,A,B)).
equivRule_call(A,B):- is_asserted(holds_t(equivRule,B,A)).

forwardRule_call(A,B):- is_asserted(holds_t(forwardRule,B,A)).

:-dynamic_multifile_exported(equivRule/2).

good_for_chaining(_,_):-!.
good_for_chaining(_Op,Term):-not(contains_singletons(Term)).
db_rewrite(_Op,Term,NewTerm):-equivRule_call(Term,NewTerm).
db_rewrite(_Op,Term,NewTerm):-forwardRule_call(Term,NewTerm).

:-export(simply_functors/3).
simply_functors(Db_pred,query(HLDS,Must),Wild):- once(transform_holds(HLDS,Wild,Simpler)),Wild\=Simpler,!,call(Db_pred,query(HLDS,Must),Simpler).
simply_functors(Db_pred,Op,Wild):- hilog_functor(HILOG),once(transform_holds(HILOG,Wild,Simpler)),Wild\=Simpler,!,call(Db_pred,Op,Simpler).


%% hook:dmsg_hook(db_op(query(HLDS,call),moo:holds_t(ft_info,type,'$VAR'(_)))):-dtrace.

% ================================================
% db_op_int/2
% ================================================
add_from_file(B,_):- contains_singletons(B),grtrace,dmsg(todo(add_from_file_contains_singletons(B))),!,fail.
add_from_file(B,B):- db_op(tell(_OldV),B),!.

% do_db_op_hooks:-!.
do_db_op_hooks:-hotrace(loop_check(do_db_op_hooks0,true)).
:-export(do_db_op_hooks0/0).
do_db_op_hooks0:- do_all_of(dbase_module_loaded),ignore(do_after_game_file).

:-export(do_after_game_file/0).
do_after_game_file:- moo:not_loading_game_file, loop_check(call_after(moo:not_loading_game_file, true),true).

db_op_int(tell(Op),Term):- !, db_op(tell(Op),Term).
db_op_int(Op,Term):-do_db_op_hooks,db_op(Op,Term),do_db_op_hooks.


univ_left(Comp,[M:P|List]):- nonvar(M),univ_left0(M, Comp, [P|List]),!.
univ_left(Comp,[H,M:P|List]):- nonvar(M),univ_left0(M,Comp,[H,P|List]),!.
univ_left(Comp,[P|List]):-moo:dbase_mod(DBASE), univ_left0(DBASE,Comp,[P|List]),!.
univ_left0(M,M:Comp,List):- Comp=..List,!.

hook:decl_database_hook(AR,C):- record_on_thread(dbase_change,changing(AR,C)).

record_on_thread(Dbase_change,O):- thread_self(ID),capturing_changes(ID,Dbase_change),!,Z=..[Dbase_change,ID,O],assertz(Z).

holds_args([H|LIST],LISTO):- !, is_holds_true(H),!,LIST=LISTO.
holds_args(HOLDS,LIST):- compound(HOLDS),HOLDS=..[H|LIST],is_holds_true(H),!.

% ================================================
% db_op/2
% ================================================

% db_op(query(HLDS,Must),creatableType(SubType)):- !, call_must(Must,is_creatable_type(SubType)).
db_op(tell(_),props(_Obj,Props)):- Props ==[], !.

db_op(Op,isa(Term,Var)):- var(Var),!,db_op(Op,isa(Term,Var)).
db_op(query(Dbase_t, Req), must(Call)):-!,must(db_op(query(Dbase_t, Req), Call)).

db_op(a,Wild):-!,db_op(tell(assertion),Wild).
db_op(query(_HLDS,Must),isa(I,Type)):- !,call_must(Must,get_isa_backchaing(I,Type)).
db_op(tell(_),isa(T,Type)):- !,assert_isa(T,Type),!.
db_op(query(HLDS,call),Wild):- hotrace(transform_holds(HLDS,Wild,Simpler)),Wild\=Simpler,!,db_op(query(HLDS,call),Simpler).
db_op(Op,Wild):- hilog_functor(HILOG),hotrace(transform_holds(HILOG,Wild,Simpler)),Wild\=Simpler,!,db_op(Op,Simpler).

db_op(tell(OldV),B):- !,loop_check_term(db_op0(tell(OldV),B),add(B),true),!. % true = we are already processing this assert
db_op(query(HLDS,Must),B):- !,loop_check_term(db_op0(query(HLDS,Must),B),req(B),is_asserted(B)),!. % false = we are already processing this assert

db_op(Op,Term):- loop_check_throw(db_op0(Op,Term)).

% ================================================
% db_op0/2
% ================================================
:-moo_hide_childs(db_op0/2).

db_op0(tell(assertion),end_of_file):-!.
db_op0(Op,Term):- not(compound(Term)),!,trace_or_throw(nc(db_op0(Op,Term))).

db_op0(Op,KB:Term):- is_kb_module(KB),!,db_op(Op,Term).
db_op0(Op,KB:Term):- dbase_mod(KB),!,db_op(Op,Term).


% db_op0(Op,(':-'(A))):- must((expand_goal_correct_argIsa(A,AA))),expanded_different(A,AA),!,db_op(Op, (':-'(AA))).

db_op0(Op,[dbase_t,Class,Inst]):-!,db_op0(Op,isa(Inst,Class)).
db_op0(Op,[cholds_f,Class,Inst]):-!,db_op0(Op,isnt(Inst,Class)).

db_op0(Op,[dbase_t,P|List]):-nonvar(P),univ_left(G2,[P|List]),!,db_op(Op,G2).
db_op0(Op,[cholds_f,P|List]):-nonvar(P),univ_left(G2,[P|List]),!,db_op(Op,not(G2)).

db_op0(Op,G1):- functor_check_univ(G1,F,[P|ListL]),List=[P|ListL],
      (is_holds_true(F) -> (nonvar(P) -> (univ_left(G2,List),db_op(Op,G2)); db_op(Op,[dbase_t|List])) ;
      (is_holds_false(F) -> (nonvar(P) -> (univ_left(G2,List),db_op(Op,not(G2))); db_op(Op,[cholds_f|List]));
      fail)).

db_op0(Op,Term):- hotrace(record_on_thread(dbase_opcall,db_op(Op,Term))),fail.

db_op0(tell(_OldV),(':-'(A))):- !, must((expand_goal_correct_argIsa(A,AA),call_expanded(AA))).
db_op0(retract,(C1;C2)):- !,dtrace,once((db_op(retract,C1),db_op(retract,C2))).
db_op0(tell(OldV),(C1;C2)):- !,db_op(tell(OldV),C1),!,db_op(tell(OldV),C2),!.
db_op0(ra,(C1;C2)):- !,must_det(db_op(ra,C1)),must_det(db_op(ra,C2)).
db_op0(Op,and(C1,C2)):- !,db_op(Op,C1),db_op(Op,C2).
db_op0(Op,(C1,C2)):- !,db_op(Op,C1),db_op(Op,C2).


db_op0(query(HLDS,Must),props(Obj,Props)):- var(Props),!,findall(Prop,(call_must(query(HLDS,Must),dbase_t([P,Obj|REST])),Prop=..[P|REST]),Props).
db_op0(Op ,props(Obj,Open)):- var(Open),!,trace_or_throw(db_op(Op,props(Obj,Open))).
db_op0(_Op,props(_Obj,[])):- !.
db_op0(Op,props(Obj,[P])):- nonvar(P),!,db_op(Op,props(Obj,P)).
db_op0(Op,props(Obj,[P|ROPS])):- !,db_op(Op,props(Obj,P)),db_op(Op,props(Obj,ROPS)).
db_op0(Op,props(Obj,PropVal)):- atom(PropVal),!,Call=..[PropVal,Obj],!,db_op(Op,Call).
db_op0(Op,props(Obj,PropVal)):- safe_univ(PropVal,[Prop,NonVar|Val]),Obj==NonVar,!,db_op(Op,[dbase_t,Prop,Obj|Val]).
db_op0(Op,props(Obj,PropVal)):- PropVal=..[OP,Pred|Val],comparitiveOp(OP),not(comparitiveOp(Pred)),!,OPVAL=..[OP|Val],PropVal2=..[Pred,OPVAL],db_op(Op,props(Obj,PropVal2)).
db_op0(Op,props(Obj,PropVal)):- PropVal=..[Prop|Val],not(infix_op(Prop,_)),!,db_op(Op,[dbase_t,Prop,Obj|Val]).
db_op0(Op,props(Obj,PropVal)):- PropVal=..[Prop|Val],!,grtrace,db_op(Op,[dbase_t,Prop,Obj|Val]).

db_op0(query(HLDS,Must),expand_args(Exp,Term)):- !, forall(do_expand_args(Exp,Term,O),outside_loop_check(req(O),db_op(query(HLDS,Must),O))).
db_op0(Op,expand_args(Exp,Term)):- !,forall(do_expand_args(Exp,Term,O),db_op(Op,O)).
db_op0(Op,somethingIsa(A,List)):- !,forall_member(E,List,db_op(Op, isa(A,E))).
db_op0(Op,somethingDescription(A,List)):- !,forall_member(E,List,db_op(Op, description(A,E))).
db_op0(Op,objects(Type,List)):- !,forall_member(I,List,db_op(Op,isa(I,Type))).
db_op0(Op,sorts(Type,List)):- !,forall_member(I,List,db_op(Op, subclass(I,Type))).
db_op0(Op,predicates(List)):- !,forall_member(T,List,db_op(Op,mpred(T))).
db_op0(Op,EACH):- EACH=..[each|List],forall_member(T,List,db_op(Op,T)).

db_op0(Op,db_op_exact(Term)):- !,db_op_exact(Op,Term).
 
db_op0(tell(_),description(A,E)):- once(must(add_description(A,E))),!,db_op0(tell(_),descriptionHere(A,E)).
db_op0(Op,nameStrings(A,S0)):- nonvar(S0),determinerRemoved(S0,String,S),!,db_op(Op, nameStrings(A,S)),db_op(tell(_OldV), determinerString(A,String)).

% db_op0(Op,Term):- hotrace(good_for_chaining(Op,Term)), db_rewrite(Op,Term,NewTerm),not(contains_singletons(NewTerm)),db_op(Op,NewTerm).

db_op0(tell(_OldV),argsIsa(Term)):-!,decl_mpred(Term),!.
db_op0(tell(_OldV),Term):-compound(Term),functor(Term,F,_),argsIsaProps(F),decl_mpred(Term),!.
db_op0(tell(_OldV),argsIsa(F,Term)):-compound(Term),!,must_det(functor(Term,F,_)),decl_mpred(Term),!.
db_op0(tell(_OldV),mpred(A)):- !,decl_mpred(A),!.
db_op0(tell(_OldV),isa(A,mpred)):- !,decl_mpred(A),!.
db_op0(tell(_OldV),isa(A,P)):- nonvar(P),functor(P,F,_),argsIsaProps(F),!,decl_mpred(A,P),!.

db_op0(query(HLDS,Must),isa(Term,Var)):- !,call_must(query(HLDS,Must),hotrace(get_isa_backchaing(Term,Var))).
db_op0(Op,isa(A,SubType)):- dbase_t(createableSubclassType,SubType,Type),!,db_op(Op,isa(A,Type)),db_op(Op,isa(A,SubType)).

%db_op0(tell(_OldV),singleValued(Term)):- !,decl_mpred(Term),decl_mpred(Term,singleValued).
%db_op0(tell(_OldV),multiValued(Term)):- !,functor_safe(Term,_,A),decl_mpred(Term),decl_mpred(Term,[multiValued,multi(A)]).
db_op0(query(HLDS,Must),argIsa(P,N,T)):- call_must(query(HLDS,Must),(get_mpred_prop(P,argsIsa(ArgsIsa)),arg(N,ArgsIsa,T),must(nonvar(T)))).


db_op0(Op,A):- hotrace(must(once(correctArgsIsa(Op,A,AA)))), not(A=@=AA), !, db_op(Op,AA).

db_op0(tell(_),Term):- glean_pred_props_maybe(Term),fail.

% db_op0(Op,Wild):- dsfdf db_op_simpler_wlc(Op,Wild,Simpler),!,db_op(Op,Simpler).

db_op0(Op,Term):- type_error_checking,!, Term =..[Type,A],!,db_op(Op,isa(A,Type)).

db_op0(Op,C0):- C0=..[Prop|ARGS],db_op_unit(Op,C0,Prop,ARGS).


% ================================================
% db_op_unit/3
% ================================================

db_op_unit(query(HLDS,Must),_C0,Prop,ARGS):- get_mpred_prop(Prop,extentKnown),!,call_must(query(HLDS,Must),dbase_t_p2(Prop,ARGS)).

db_op_unit(Op,_C0,Prop,ARGS):- type_error_checking,!, cached_isa(Prop,type),trace_or_throw(db_op_unit(Op,type(Prop),ARGS)).

db_op_unit(Op,C0,isa,ARGS):- type_error_checking,!, trace_or_throw(db_op_unit(Op,isa(C0),ARGS)).

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
      
% query_with_pred/1
db_op_unit(query(Must,HLDS),C0,Prop,_RGS):- get_mpred_prop(Prop,query_with_pred(How)),!, call_must(query(Must,HLDS),call(How,C0)).

% assert_with_pred/1
db_op_unit(tell(_),C0,Prop,_RGS):- get_mpred_prop(Prop,assert_with_pred(How)),!, must(nonvar(How)), once(ignore((call(How,C0), run_database_hooks(assert(z),C0)))).

% plain prop
db_op_unit(Op,_C0,Prop,ARGS):- must_det((db_op_sentence(Op,Prop,ARGS,Unit),same_vars(ARGS,Unit))),!, db_op_exact(Op,Unit).

/*
 cant get here
db_op_unit(Op,_C0,Prop,ARGS):- grtrace,must_det((db_op_sentence(Op,Prop,ARGS,Unit),same_vars(ARGS,Unit))),!, db_op_loop(Op,Unit,db_op_exact(Op,Unit)).
db_op_unit(Op,C0,_Prop,_ARGS):- db_op_loop(Op,C0,db_op_exact(Op,C0)).
*/

db_op_loop(Op,Unit,Result):- is_loop_checked(db_op0(Op,Unit)),!,call(Result).
db_op_loop(Op,Unit,_Result):- db_op(Op,Unit).

% ================================================
% db_op_exact/2
% ================================================
db_op_exact(Op,G):-dmsg(db_op_exact(Op,G)),fail.
db_op_exact(Op,G):- G=..[SubType,Arg],must_det(type(SubType)),db_op_loop(Op,isa(Arg,SubType),fail),!.
db_op_exact(query(HLDS,Must),Term):- !,call_expanded_for(query(HLDS,Must),Term).
db_op_exact(query, Term):- !,call_expanded_for(findall,Term).
db_op_exact(must, Term):- !,call_expanded_for(must,Term).
db_op_exact(u,C):- grtrace,dtrace,db_quf(u,C,U,Template),call_expanded(U),Template,must(ground(Template)),!,ignore(hooked_retractall(Template)).
db_op_exact(ra,C):- db_quf(ra,C,U,Template),!, doall((call_expanded(U),hooked_retractall(Template))).
db_op_exact(retract,C):- must(db_quf(retract,C,U,Template)),!,call_expanded(U),!,hooked_retract(Template).
db_op_exact(tell(OldV),W):- non_assertable(W,Why),trace_or_throw(todo(db_op(tell(OldV), non_assertable(Why,W)))).
db_op_exact(tell(Must),C0):- db_quf(tell(Must),C0,U,C),!,must(call_expanded(U)),functor_catch(C,F,A),( get_mpred_prop(F,singleValued) -> must(db_assert_sv(Must,C,F,A)) ; must(db_assert_mv(Must,C,F,A))).
db_op_exact(tell(Must),C):- grtrace, functor_catch(C,F,A), must_det((get_mpred_prop(F,singleValued) -> must_det(db_assert_sv(tell(Must),C,F,A)) ; must(db_assert_mv(tell(Must),C,F,A)))).
%db_op_exact(Must, Term):- !,call_expanded_for(Must,Term).
db_op_exact(Op,C):- trace_or_throw(unhandled(db_op_exact(Op,C))).

:-export(call_expanded_for/2).
call_expanded_for(Must,Call):- call_must(Must,Call).

/*

call_expanded_for_sv(WhatNot,F,A,G,OUT):- nonvar(OUT),replace_arg(G,A,NEW,CC),!,call_expanded_for_sv_2(WhatNot,F,A,CC,NEW),!,NEW=OUT.
call_expanded_for_sv(WhatNot,F,A,G,OUT):- call_expanded_for_sv_2(WhatNot,F,A,G,OUT),!.

call_expanded_for_sv_2(WhatNot,F,A,G,_OUT):-call_expanded([whatnot(WhatNot)],G,F,A),!.
call_expanded_for_sv_2(_WhatNot,F,A,G,OUT):- defaultArgValue(G,F,A,OUT),!.

*/

:-export(call_expanded/1).
%:-export(call_expanded/1).
%call_expanded(G):-debugOnError(G).
call_expanded(G) :- functor_catch(G,F,A),call_expanded([],G,F,A).

:-export(call_expanded/4).
call_expanded(_Doing,V,_F,_A):-var(V),!,trace_or_throw(var_call_expanded(V)).
call_expanded(_Doing,true,_F,_A):-!.
call_expanded(Doing,G,F,A):-not(member(ask_module(_),Doing)),mpred_prop(F,ask_module(M)),!,'@'(M:call(call_expanded([ask_module(M)|Doing],M:G,F,A)),M).
call_expanded(Doing,G,F,A):-not(member(query(_),Doing)),mpred_prop(F,query(M)),!,call_expanded([query(M)|Doing],call(M,G),F,A).
call_expanded(Doing,G,F,A):-not(member(is_asserted,Doing)),!,is_asserted(G);call_expanded([is_asserted|Doing],G,F,A).
call_expanded(Doing,G,F,A):-not(member(call_mpred,Doing)),!,(call_mpred(F,G);call_expanded([call_mpred|Doing],G,F,A)).


:-export((dbase_t/1,dbase_t/2)).
:- dynamic_multifile_exported((
         % dbase_t/1,
         % dbase_t/2,
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
          dbase_f/7)).


dbase_t(C,I):- fail,loop_check_term(get_isa_backchaing(I,C),dbase_t(C,I),fail).

dbase_t([P|LIST]):- !,dbase_t_p2(P,LIST).
%dbase_t(naf(CALL)):-!,not(dbase_t(CALL)).
%dbase_t(not(CALL)):-!,dbase_f(CALL).
dbase_t(CALL):- compound(CALL),!,CALL =..[P|LIST],dbase_t_p2(P,LIST).

dbase_t_p2(P,[]):-!,dbase_t(P).
dbase_t_p2(dbase_t,LIST):-!, CALL=..[dbase_t|LIST],call(CALL).
dbase_t_p2(P,[L|IST]):-is_holds_true(P),!,dbase_t_p2(L,IST).
dbase_t_p2(P,LIST):-is_holds_false(P),!,dbase_f(LIST).
dbase_t_p2(P,LIST):- CALL=..[dbase_t,P|LIST],call(CALL).

% ================================================
% call_must/2
% ================================================
:-export((call_must/2)).

call_must(Must,M:Call):- atom(M),!,call_must(Must,Call).
call_must(M:Must,Call):- atom(M),!,call_must(Must,Call).
call_must(query(Must,dbase_t),Call):- !,call_must(Must,Call).
call_must(query(dbase_t,Must),Call):- !,call_must(Must,Call).
call_must(query(Must,call),Call):- !,call_must(Must,Call).
call_must(query(call,Must),Call):- !,call_must(Must,Call).
call_must(query(_HOLDS,Must),Call):- !,call_must(Must,Call).
call_must(tell(Must),Call):- !,call_must(Must,Call).
call_must(ask(Must),Call):- !,call_must(Must,Call).
call_must(!,Call):- !,call_must(once,Call).
call_must(Must,Call):- functor_catch(Call,F,_),term_variables(Call,Vs),!,call_must_vars(Must,F,Vs,Call).

call_must_vars(once,F,Vs,Call):- !,once(call_must_all(once,F,Vs,Call)).
call_must_vars(must,F,Vs,Call):- !,must(call_must_all(must,F,Vs,Call)).
call_must_vars(Op,F,[],Call):- !,once(call_must_all(Op,F,[],Call)).
call_must_vars(Op,F,Vs,Call):- call_must_all(Op,F,Vs,Call).

sc1(F,C):-(mpred_prop(F,nonGroundOK);in_prolog_source_code),!,C.
sc1(_,C):-C, ignore((not(ground(C)),dmsg(non_ground_sc1(C)))).

% call_must_all(Must,F,Vs,Call):- loop_check(sc1(F,call_must_all0(Must,F,Vs,Call)),(dmsg(failed_looped(call_must_all0(Must,F,Vs,Call))),!,fail)).
call_must_all(Must,F,Vs,Call):- loop_check(sc1(F,call_must_all0(Must,F,Vs,Call)),fail).

call_must_all0(_,subclass,_Vs,C):-!,is_asserted(subclass,C).
call_must_all0(_,get_isa_backchaing,_Vs,C):-!,C.

% call_must_all0(Op, F,Vs,Wild):-dmsg(call_must(Op,Wild)),fail.
call_must_all0(assertedOnly,F,_Vs,C):-!,asserted_clause(F,C).
call_must_all0(_ ,F,[],Call):-!,call_mpred(F,Call),!.
call_must_all0(_ ,F,_Vs,Call):-not(arg(_,Call,vvar)),!,call_mpred(F,Call),!.
call_must_all0(_ ,F,Vs,Call):- setof(Vs,call_mpred(F,Call),VVs),member(Vs,VVs).

call_mpred(M:C):-atom(M),!,call_mpred(C).
call_mpred(C):- compound(C),functor(C,F,_),!,call_mpred(F,C).
call_mpred(_,C):- predicate_property(C,_),debugOnError(C),check_was_known_false(C).
call_mpred(F,C):- not(mpred_prop(F,hasStub(body_req))),is_asserted(F,C).


naf(Goal):-not(req(Goal)).

:-decl_mpred_prolog(naf/1).
:-decl_mpred_prolog(call_mpred/1).

% ================================================
% db_assert_[mv|sv]/3
% ================================================


% assert_with to tell(OldV) mutlivalue pred
:-export((db_assert_mv/4)).
db_assert_mv(_Must,end_of_file,_,_):-!.
% db_assert_mv(_Must,C,_F,_A):- hooked_assertz(C),!.
db_assert_mv(_Must,C,F,_A):- (mpred_prop(F,ordered) -> hooked_assertz(C) ; hooked_asserta(C)).


% assert_with to tell(OldV) singlevalue pred
:-export((db_assert_sv/4)).
%db_assert_sv(_Must,C,F,A):- throw_if_true_else_fail(contains_singletons(C),db_assert_sv(C,F,A)).
db_assert_sv(Must,C,F,A):- ignore(( loop_check(db_assert_sv_lc(Must,C,F,A),true))).

:-export((db_assert_sv_lc/4)).
db_assert_sv_lc(Must,C,F,A):- arg(A,C,UPDATE),db_assert_sv_now(Must,C,F,A,UPDATE),!.

:-export(db_assert_sv_now/5).
db_assert_sv_now(Must,C,F,A, UPDATE):- number(UPDATE),UPDATE<0, db_assert_sv_update(Must,C,F,A,UPDATE).
db_assert_sv_now(Must,C,F,A,+UPDATE):-!, db_assert_sv_update(Must,C,F,A,+UPDATE).
db_assert_sv_now(Must,C,F,A,-UPDATE):-!, db_assert_sv_update(Must,C,F,A,-UPDATE).
db_assert_sv_now(Must,C,F,A, REPLACE):- db_assert_sv_replace(Must,C,F,A, REPLACE).

:-export(db_assert_sv_update/5).
db_assert_sv_update(Must,C,F,A,UPDATE):-
   replace_arg(C,A,OLD,COLD),
   must_det_l([qreq(COLD),   
   update_value(OLD,UPDATE,NEW),!,
   db_assert_sv_replace(Must,C,F,A,NEW)]),!.

:-export(db_assert_sv_replace/5).

:-style_check(-singleton).
% db_assert_sv_replace_noisey_so_disabled
db_assert_sv_replace(_Must,C,_,A,NEW):- fail,
   replace_arg(C,A,_,CBLANK),
   hooked_retractall(CBLANK),
   replace_arg(C,A,NEW,CNEW),
   must_det(hooked_asserta_confirmed(CNEW,A,NEW)),!.

db_assert_sv_replace(Must,C,F,A,NEW):-
   replace_arg(C,A,OLD,COLD),
   replace_arg(C,A,NEW,CNEW),
   ignore(qreq(COLD)),
   must_det(db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW)),!.

db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW):- var(OLD),!,
   dmsg(db_assert_sv(COLD,'__add__',CNEW)),
   % replace_arg(C,A,_,CBLANK),hooked_retractall(CBLANK),
   must_det(hooked_asserta_confirmed(CNEW,A,NEW)),!.
db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW):- same_arg(equals,OLD,NEW),!. %,dmsg(db_assert_sv_same(COLD,'__same__',CNEW)).
db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW):-
   dmsg(db_assert_sv(COLD,'__replace__',CNEW)),
   must_det(hooked_retract(COLD)),
   must_det(hooked_asserta_confirmed(CNEW,A,NEW)),!.

qreq(COLD):-functor(COLD,F,A),functor(COLD_AT_ALL,F,A),
   with_assertions(noDefaultTypeValues(COLD), 
     with_assertions(noDefaultValues(COLD),
                   (no_loop_check(  %listing(bugger:inside_loop_check/1),
                     call_expanded([whatnot(COLD)],COLD,F,A)))) ),!.

qreq2(COLD):-qreq(COLD),!.
qreq2(COLD):-functor(COLD,F,A),functor(COLD_AT_ALL,F,A),
   listing(bugger:inside_loop_check/1),
   retractall(bugger:inside_loop_check(_)),
   with_assertions(noDefaultTypeValues(COLD), 
     with_assertions(noDefaultValues(COLD),
                   ((  %listing(bugger:inside_loop_check/1),
                     call_expanded([whatnot(COLD)],COLD,F,A)))) ),!.

:-style_check(+singleton).

confirm_hook(CNEW:NEW=@=CNOW:NOW):-
   var(NOW),               
   qreq(CNOW),
   show_call(CNEW:NEW=@=CNOW:NOW),!.

% Expect CNEW to be what is found
hooked_asserta_confirmed(CNEW,A,NEW):-
   replace_arg(CNEW,A,NOW,CNOW),
   must(ground(CNEW)),
   hooked_asserta(CNEW),
   confirm_hook(CNEW:NEW=@=CNOW:NOW),!.

hooked_asserta_confirmed(CNEW,A,NEW):-
   replace_arg(CNEW,A,NOW,CNOW),
   dtrace,
   show_call((req(CNOW),CNEW:NEW=@=CNOW:NOW)),!.
hooked_asserta_confirmed(CNEW,A,NEW):-dmsg(unconfirmed(hooked_asserta_confirmed(CNEW,A,NEW))).

:-export((noDefaultValues/1)).
:-thread_local noDefaultValues/1.
:-export((noDefaultTypeValues/1)).
:-thread_local noDefaultTypeValues/1.


no_fallback(subclass,2).
no_fallback(P,2):-not(mpred_prop(P,singleValued)).


:-export(fallback_value/3).
fallback_value(Prop,_Obj,_Value):-no_fallback(Prop,2),!,fail.
fallback_value(_Prop,Obj,_Value):-var(Obj),!,fail.
fallback_value(Prop,Obj,Value):-Call=..[Prop,Obj,_], with_assertions(noDefaultValues(Call),defaultArgValue(Call,Prop,2,ValueR)),!,Value=ValueR.

:-export(defaultArgValue/4).
defaultArgValue(Call,_,_,_):- stack_check(1000),noDefaultValues(Call),!,fail.
defaultArgValue(_,F,_,_):- noDefaultValues(F),!,fail.
defaultArgValue(Call,F,A,OLD):- mpred_prop(F,default_sv(A,OLD)),!,dmsg(fallback_value(Call,F,default_sv(A,OLD))).
defaultArgValue(facing(_,_),_,2,"n"):-!.
defaultArgValue(change(_,_),_,2,200):-!.
defaultArgValue(damage(_,_),_,2,500):-!.
defaultArgValue(Call,F,LastPlus1,Value):- arg(1,Call,I),nonvar(I),isa(I,Type),catch(get_type_props(Type,PropList),_,fail),Last is LastPlus1 - 1,
      functor(Prop,F,Last),member(Prop,PropList),show_call(arg(Last,Prop,Value)),!.

defaultArgValue(Call,F,A,OLD):- argIsa_call(F,A,Type),defaultTypeValue(Call,Type,OLD),!,dmsg(using_defaultArgValue(Call,F,A,OLD)).


defaultTypeValue(Call,_,_):- noDefaultTypeValues(Call),!,fail.
defaultTypeValue(_,Type,_):- noDefaultTypeValues(Type),!,fail.
defaultTypeValue(_Info,dir,"n").
defaultTypeValue(_Info,int,0).
defaultTypeValue(Call,Type,Out):- create_random(Type,ROut,nonvar(ROut)),dmsg(fake_defaultValue(Call,Type,ROut=Out)),!,Out=ROut.

replace_arg(C,A,OLD,CC):- 
   C=..FARGS,
   replace_nth(FARGS,A,OLD,FARGO),!,
   CC=..FARGO.

replace_nth([],_,_,[]):- !.
replace_nth([_|ARGO],0,OLD,[OLD|ARGO]):- !.
replace_nth([T|FARGS],A,OLD,[T|FARGO]):- 
    A2 is A-1,replace_nth(FARGS,A2,OLD,FARGO).



member_or_e(E,[L|List]):-!,member(E,[L|List]).
member_or_e(E,E).


replace_nth([],_N,_OldVar,_NewVar,[]):- !,trace_or_throw(missed_the_boat).
replace_nth([OldVar|ARGS],1,OldVar,NewVar,[NewVar|ARGS]):- !.
replace_nth([Carry|ARGS],Which,OldVar,NewVar,[Carry|NEWARGS]):- 
 Which1 is Which-1,
 replace_nth(ARGS,Which1,OldVar,NewVar,NEWARGS),!.

update_value(OLD,NEW,NEXT):- var(NEW),!,trace_or_throw(logicmoo_bug(update_value(OLD,NEW,NEXT))).
update_value(OLD,NEW,NEWV):- var(OLD),!,compute_value_no_dice(NEW,NEWV).
update_value(OLD,X,NEW):- is_list(OLD),!,list_update_op(OLD,X,NEW),!.
update_value(OLDI,+X,NEW):- compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD + X,_,fail),!.
update_value(OLDI,-X,NEW):- compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD - X,_,fail),!.
update_value(OLDI,X,NEW):- number(X),X<0,compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD + X,_,fail),!.
update_value(_,NEW,NEWV):-compute_value_no_dice(NEW,NEWV),!.

list_update_op(OLDI,+X,NEW):-flatten_append(OLDI,X,NEW),!.
list_update_op(OLDI,-X,NEW):-flatten([OLDI],OLD),flatten([X],XX),!,list_difference_eq(OLD,XX,NEW),!.

compute_value_no_dice(NEW,NEW):- compound(NEW),functor_catch(NEW,dice,_),!.
compute_value_no_dice(NEW,NEWV):-compute_value(NEW,NEWV).

compute_value(NEW,NEWV):-catch(NEWV is NEW,_,fail),!.
compute_value(NEW,NEWV):-catch(any_to_value(NEW,NEWV),_,fail),!.
compute_value(NEW,NEW).

insert_into(ARGS,0,Insert,[Insert|ARGS]):- !.
insert_into([Carry|ARGS],After,Insert,[Carry|NEWARGS]):- 
   After1 is After - 1,
   insert_into(ARGS,After1,Insert,NEWARGS).

moo:term_specifier_text(Text,pred):- mpred_prop(Text,arity(_)).

:- multifile(mudToHitArmorClass0 / 2).

:- dynamic_safe(mudToHitArmorClass0 / 2).

:- user_use_module(dbase_rules_pttp).


/*
:-export(makeConstant/1).
makeConstant(X):-trace_or_throw(makeConstant(X)).
:-export(cycAssert/2).
cycAssert(A,B):-trace_or_throw(cycAssert(A,B)).
*/

:- include(dbase_c_term_expansion).

:- include(dbase_i_db_preds).

:- include(dbase_i_cyc).

:-decl_mpred(expand_args,2).
:-decl_mpred(objid,2).

% flags
:-decl_mpred(agent(id),[flag]).
:-decl_mpred(item(id),[flag]).
:-decl_mpred(region(id),[flag]).
:-decl_mpred(type(id),[flag]).
:-decl_mpred(thinking(agent),[flag]).
:-decl_mpred(deleted(id),[flag]).

:- decl_mpred(needs_look/2,[extentKnown]).
:- decl_mpred(mudMaxHitPoints(agent,int)).

:- rescan_mpred_props.


:-export(get_type_props/2).

get_type_props(Type,PropList):- loop_check(get_type_props_lc(Type,PropList),fail).
get_type_props_lc(Type,PropList):- call_tabled(type(Type)),
   findall(PropU,(findall_type_default_props(Inst,Type,Prop),subst(Prop,Inst,self,PropU)),PropS),flatten_set([PropS],PropList),PropList\=[].


:-export(instance_missing_props/3).
instance_missing_props(I,LPS,PS):- findall(P,(member(P,LPS),inst_missing_prop(I,P)),PS),!.

:-export(get_inst_default_props/3).
get_inst_default_props(I,PropListL,Missing):-
    findall(PropList,(get_type_props(Type,PropList),isa(I,Type)),PropListS),flatten_set(PropListS,PropListL),
       instance_missing_props(I,PropListL,Missing).
      
inst_missing_prop(I,P):-P=..[F|Args],inst_missing_prop(I,F,Args).

inst_missing_prop(_I,F,_Args):-not(mpred_prop(F,singleValued)),!,fail.
inst_missing_prop(_I,F,_Args):-mpred_prop(F,flag),!,fail.
inst_missing_prop(I,F,Args):-C=..[F,I|Args],get_sv_argnum(F,[I|Args],A),arg(A,C,_Default),replace_arg(C,A,BLANK,COLD),ignore(qreq(COLD)),!,var(BLANK).


get_sv_argnum(F,Args,ArgNum):-once(mpred_prop(F,functionalArg(ArgNum));length(Args,ArgNum)).

:-export(forall_setof/2).
forall_setof(ForEach,Call):-
   findall(ForEach,ForEach,ForEachAll),
   list_to_set(ForEachAll,Set),!,
   ignore(forall(member(ForEach,Set),Call)).

:-export(scan_default_props/0).

scan_default_props:- loop_check(scan_default_props_lc,true).
scan_default_props_lc:- dmsg(todo(fix(scan_default_props,"to not set atloc/2"))), fail,
   once(forall_setof(get_type_props(Type,PropList),
    forall_setof(isa(I,Type), 
         ignore((not(Type == I),
         once(instance_missing_props(I,PropList,Missing)),Missing\=[],
         dmsg(scan_default_props(I,Type,missing_from(Missing,PropList))),
         padd(I,Missing)))))),fail.
scan_default_props_lc:-ignore(rescan_duplicated_facts).

:-dynamic_multifile_exported((moo:was_imported_kb_content/1)).


is_clause_moo_special((Head :- Body)):-!, is_clause_moo_special(Head,Body).
is_clause_moo_special(C):- is_clause_moo_special(C,true).

:-export(game_assert_later/1).
game_assert_later(Goal):-assert_if_new(moo:call_after_load(Goal)).

:-thread_local game_assert_thread_override/1.
% game_assert_thread_override(A):-game_assert_from_macropred(A),!.

:-thread_local in_prolog_source_code/0.
:-export(in_prolog_source_code/0).
:-export(game_assert/1).
:-'$hide'(game_assert/1).
game_assert(A):-A==end_of_file,!.
game_assert(A):- in_prolog_source_code,!, must_det(assertz_local_game_clause(A)).
game_assert(A):- dmsg(game_assert(A)),fail.
game_assert(A):- not(compound(A)),!,trace_or_throw(not_compound(game_assert(A))).
game_assert(Call):- loop_check(game_assert_thread_override(Call),fail),!.
game_assert(Call):-game_assert_from_macropred(Call),!.
game_assert(M:HB):-atom(M),!, must_det(game_assert(HB)).
game_assert(A):-must_det(correctArgsIsa(tell(game_assert),A,AA)),must_det(game_assert_handler(AA)),!.

game_assert_handler(M:HB):-atom(M),!,game_assert_handler(HB).
game_assert_handler(Call):-loop_check(game_assert_handler_lc(Call),true).

game_assert_handler_lc(Call):- loop_check(game_assert_thread_override(Call),fail),!.  
game_assert_handler_lc(type(A)):- must_det(define_type(A)),!.
game_assert_handler_lc(mpred_prop(A)):- decl_mpred(A),!.
game_assert_handler_lc(mpred_prop(A,B)):- decl_mpred(A,B),!.
game_assert_handler_lc(A):- A=..[Type,_], formattype(Type), trace_or_throw(formattype_ensure_skippable(A)),!.
game_assert_handler_lc(A):- A=..[Type,_], not(type(Type)), dmsg(todo(ensure_creatabe(Type))),fail.
game_assert_handler_lc(Call):- game_assert_from_macropred(Call),!.
game_assert_handler_lc(W):-must_det(game_assert_fast(W)),!.
game_assert_handler_lc(A):-trace_or_throw('game_assert_handler_lc is skipping ~q.',[A]).

:-export(begin_prolog_source/0).
:-export(end_prolog_source/0).
begin_prolog_source:- must_det(asserta(in_prolog_source_code)).
end_prolog_source:- must_det(retract(in_prolog_source_code)).

:-export(game_assert_from_macropred/1).
game_assert_from_macropred(C):- loop_check(game_assert_from_macropred_lc(C),((dmsg(loopING_game_assert_from_macropred(C)),!,fail))).
game_assert_from_macropred_lc(A):-A==end_of_file,!.
game_assert_from_macropred_lc(A):- not(compound(A)),!,trace_or_throw(not_compound(game_assert_from_macropred_lc(A))).
game_assert_from_macropred_lc(':-'(A)):- predicate_property(A,_),!,must(logOnFailure(A)),!.
game_assert_from_macropred_lc(':-'(A)):- trace_or_throw(missing_directive(A)),!.
game_assert_from_macropred_lc(':-'(Head,Body)):- must_det(assertz_local_game_clause(Head,Body)),!.
game_assert_from_macropred_lc(RDF):- RDF=..[SVO,S,V,O],is_svo_functor(SVO),!,must_det(game_assert(dbase_t(V,S,O))).
game_assert_from_macropred_lc(somethingIsa(A,List)):-forall_member(E,List,game_assert(isa(A,E))).
game_assert_from_macropred_lc(somethingDescription(A,List)):-forall_member(E,List,game_assert(description(A,E))).
game_assert_from_macropred_lc(objects(Type,List)):-forall_member(I,List,game_assert(isa(I,Type))).
game_assert_from_macropred_lc(sorts(Type,List)):-forall_member(I,List,game_assert(subclass(I,Type))).
game_assert_from_macropred_lc(predicates(List)):-forall_member(T,List,game_assert(mpred_prop(T))).
game_assert_from_macropred_lc(description(A,E)):- add_description(A,E).
game_assert_from_macropred_lc(nameStrings(A,S0)):- determinerRemoved(S0,String,S),!,game_assert(nameStrings(A,S)),game_assert(determinerString(A,String)).
game_assert_from_macropred_lc(Call):- fail, predicate_property(Call, number_of_rules(_)),not(predicate_property(Call, dynamic)),nop(dmsg(assert_to_static(Call))),fail.
game_assert_from_macropred_lc(d(s)):-dumpST, dtrace.
game_assert_from_macropred_lc(M:HB):-atom(M),!,game_assert_from_macropred_lc(HB).

:-export((assertz_local_game_clause/1)).
assertz_local_game_clause((':-'(Body))):-!,must_det(show_call(Body)).
assertz_local_game_clause((Head :- Body)):-!, assertz_local_game_clause(Head,Body).
assertz_local_game_clause(C):- assertz_local_game_clause(C,true).

assertz_local_game_clause(H,B):- '@'(assertz_local_game_clause0(H,B),'moo').

assertz_local_game_clause0(Head,Body):- (var(Head);var(Body)),!,trace_or_throw(var_assertz_local_game_clause0(Head,Body)).
assertz_local_game_clause0(Head,Body):- clause_asserted((':-'(Head,Body))),!.
assertz_local_game_clause0(Head,BodyIn):- once(make_body_clause(Head,BodyIn,Body)),assertz_if_new_clause_here(Head,Body),dmsg(made_specal_clause(Head,Body)).

assertz_if_new_clause_here(Head,true):-functor(Head,_,N),N<3, not(last_arg_ground(Head)),singletons_throw_or_fail((Head)).
assertz_if_new_clause_here(Head,true):-add(Head),!.
assertz_if_new_clause_here(Head,Body):-assertz_if_new_clause(Head,Body).

special_wrapper_body(W):-get_body_functor(W,F,_),!,special_wrapper_functor(F).

special_wrapper_functor(game_call_head_body).
special_wrapper_functor(body_req).
special_wrapper_functor(loop_check).
special_wrapper_functor(loop_check_term).
special_wrapper_functor(loop_check_clauses).

make_body_clause(_Head,Body,Body):-atomic(Body),!.
make_body_clause(_Head,Body,Body):-special_wrapper_body(Body),!.
make_body_clause(Head,Body,moo:game_call_head_body(Head,Body)).

:-export(game_call_head_body/2).
game_call_head_body(Head,Body):-loop_check_term(Body,body_call(Head,Body),fail).


:-export((game_assert_fast/1)).
game_assert_fast(M:HB):-atom(M),!,game_assert_fast(HB).

game_assert_fast(C0):- ignore(assert_deduced_arg_isa_facts(C0)),add(C0),run_database_hooks_local(assert(z),C0).


mpred_prop(G,assert_with(game_assert)):- moo:assertionMacroHead(G).

assertOnLoad(ClassTemplate):- compound(ClassTemplate), ClassTemplate=..[class_template,Type|Props],
      flatten(Props,AllProps), !, assertOnLoad(default_type_props(self,Type,AllProps)).
assertOnLoad(X):-game_assert(X).

setTemplate(X):-game_assert(X).

onSpawn(ClassFact):- ClassFact=..[Funct,InstA,DeclB],onSpawn(Funct,InstA,DeclB).

onSpawn(ClassFact):- ClassFact=..[Funct,InstA],createByNameMangle(InstA,Inst),assert_isa(Inst,Funct).

englishServerInterface(SomeEnglish):-dmsg(todo(englishServerInterface(SomeEnglish))).

onSpawn(Funct,DeclA,DeclB):- createByNameMangle(DeclA,IDA),!, createByNameMangle(DeclB,IDB),!, game_assert(dbase_t(Funct,IDA,IDB)).

createByNameMangle(InstA,Inst):- compound(InstA),!,functor_catch(InstA,Type,A),must(A==1),assert_isa(InstA,Type),InstA=Inst.
createByNameMangle(InstA,_):- not(atom(InstA)),!,trace_or_throw(todo(not_atom_createByNameMangle(InstA))).
createByNameMangle(Suggest,InstA):-split_name_type(Suggest,InstA,Type),assert_isa(InstA,Type).
createByNameMangle(Type,InstA):- atom_concat(Type,'777',InstA),must_det(assert_isa(InstA,Type)).
createByNameMangle(InstA,IDA):- gensym(InstA,IDA), englishServerInterface([create,InstA,IDA]).

wfAssert(X):-game_assert(X). %  game_assert_later(X).


add_arg_parts_of_speech(_F,_N,[],[]).
add_arg_parts_of_speech(F,N,[A|ARGS0],[ARG|ARGS]):-argIsa_call_or_undressed(F,N,A,ARG),N1 is N+1, add_arg_parts_of_speech(F,N1,ARGS0,ARGS).

argIsa_call_or_undressed(F,N,Obj,fN(Obj,Type)):- argIsa_call_0(F,N,Type),!.
argIsa_call_or_undressed(_F,_N,Obj,Obj).

verb_after_arg(_,_,1).

:- style_check(+discontiguous).
:- style_check(-discontiguous).

:- decl_mpred(default_sv, 3).
:- decl_mpred(ask_module, 2).


is_clause_moo_special(M:H,B):-atomic(M),!,is_clause_moo_special(H,B).
is_clause_moo_special(H,_B):-compound(H),functor_catch(H,F,_),not(get_mpred_prop(F,prologBuiltin)),!,special_head(H,F),!.
is_clause_moo_special(H,M:B):-atomic(M),!,is_clause_moo_special(H,B).

special_head(_,F):-get_mpred_prop(F,prologOnly),!,fail.
special_head(_,F):-get_mpred_prop(F,prologHybrid).
special_head(_,F):-get_mpred_prop(F,hasStub(_)).


user:term_expansion(CL,moo:was_imported_kb_content(CL)):- is_clause_moo_special(CL),not(into_form_code),
   dmsg(assertz_local_game_clause(CL)),ignore(is_compiling_sourcecode),must_det(game_assert(CL)),!.

% load_motel:- defrole([],time_state,restr(time,period)).
% :-load_motel.

:- include(logicmoo('vworld/moo_footer.pl')).


