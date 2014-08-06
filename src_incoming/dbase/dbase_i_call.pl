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


% oncely later will throw an error if there where choice points left over by call
oncely(:-(Call)):-!,Call,!.
oncely(:-(Call)):-!,call_expanded(Call).
oncely(Call):-once(Call).


:-thread_local assertedOnly/1.
:-thread_local with_callMPred/1.

% ================================================
% call_expanded_for/2
% ================================================
:-export((call_expanded_for/2)).

call_expanded_for(Must,M:Call):- atom(M),!,call_expanded_for(Must,Call).
call_expanded_for(M:Must,Call):- atom(M),!,call_expanded_for(Must,Call).
call_expanded_for(query(Must,dbase_t),Call):- !,call_expanded_for(Must,Call).
call_expanded_for(query(dbase_t,Must),Call):- !,call_expanded_for(Must,Call).
call_expanded_for(query(Must,call),Call):- !,call_expanded_for(Must,Call).
call_expanded_for(query(call,Must),Call):- !,call_expanded_for(Must,Call).
call_expanded_for(query(_HOLDS,Must),Call):- !,call_expanded_for(Must,Call).
call_expanded_for(tell(Must),Call):- !,call_expanded_for(Must,Call).
call_expanded_for(ask(Must),Call):- !,call_expanded_for(Must,Call).
call_expanded_for(!,Call):- !,call_expanded_for(once,Call).
call_expanded_for(Must,Call):- functor_catch(Call,F,_),term_variables(Call,Vs),!,call_expanded_for_vars(Must,F,Vs,Call).

call_expanded_for_vars(once,F,Vs,Call):- !,once(call_expanded_for_all(once,F,Vs,Call)).
call_expanded_for_vars(must,F,Vs,Call):- !,must(call_expanded_for_all(must,F,Vs,Call)).
call_expanded_for_vars(Op,F,[],Call):- !,once(call_expanded_for_all(Op,F,[],Call)).
call_expanded_for_vars(Op,F,Vs,Call):- call_expanded_for_all(Op,F,Vs,Call).

sc1(F,C):-(mpred_prop(F,nonGroundOK);in_prolog_source_code),!,C.
sc1(_,C):-C, ignore((not(ground(C)),dmsg(non_ground_sc1(C)))).

% call_expanded_for_all(Must,F,Vs,Call):- loop_check(sc1(F,call_expanded_for_all0(Must,F,Vs,Call)),(dmsg(failed_looped(call_expanded_for_all0(Must,F,Vs,Call))),!,fail)).
call_expanded_for_all(Must,F,Vs,Call):- loop_check(sc1(F,call_expanded_for_all0(Must,F,Vs,Call)),fail).

call_expanded_for_all0(_,subclass,_Vs,C):-!,is_asserted(subclass,C).
call_expanded_for_all0(_,get_isa_backchaing,_Vs,C):-!,C.

% call_expanded_for_all0(Op, F,Vs,Wild):-dmsg(call_expanded_for(Op,Wild)),fail.
call_expanded_for_all0(assertedOnly,_,_Vs,C):-!,dtrace,is_asserted(C).
call_expanded_for_all0(_ ,F,[],Call):-!,call_mpred(F,Call),!.
call_expanded_for_all0(_ ,F,_Vs,Call):-not(arg(_,Call,vvar)),!,call_mpred(F,Call),!.
call_expanded_for_all0(_ ,F,Vs,Call):- setof(Vs,call_mpred(F,Call),VVs),member(Vs,VVs).

/*

call_expanded_for_sv(WhatNot,F,A,G,OUT):- nonvar(OUT),replace_arg(G,A,NEW,CC),!,call_expanded_for_sv_2(WhatNot,F,A,CC,NEW),!,NEW=OUT.
call_expanded_for_sv(WhatNot,F,A,G,OUT):- call_expanded_for_sv_2(WhatNot,F,A,G,OUT),!.

call_expanded_for_sv_2(WhatNot,F,A,G,_OUT):-call_expanded([whatnot(WhatNot)],G,F,A),!.
call_expanded_for_sv_2(_WhatNot,F,A,G,OUT):- defaultArgValue(G,F,A,OUT),!.

*/

:-export(call_expanded/1).
%:-export(call_expanded/1).
call_expanded(true):-!.
call_expanded(G):-compound(G),!,call_expanded([],G).
call_expanded(G):- trace_or_throw(var_call_expanded(G)).
:-export(call_expanded/2).
call_expanded(Doing,G):- functor(G,F,A),call_expanded(Doing,G,F,A).
:-export(call_expanded/4).
call_expanded(_Doing,true,_F,_A):-!.
call_expanded(Doing,G,dbase_t,A):-G=..[dbase_t,P|ARGS],!,into_mpred_form(G,dbase_t,P,A,ARGS,O),!,call_expanded(Doing,O).
call_expanded(_Doing,G,F,_A):-predicate_property(G,visible),not(mpred_prop(F,_)),!,debugOnError(G).
call_expanded(Doing,G,F,A):-not(member(singleValued,Doing)),mpred_prop(F,singleValued),arg(1,G,NonVar),nonvar(NonVar),!,once(call_expanded([singleValued|Doing],G,F,A)).
call_expanded(Doing,G,F,A):-not(member(ask_module(_),Doing)),mpred_prop(F,ask_module(M)),!,'@'(M:call(call_expanded([ask_module(M)|Doing],M:G,F,A)),M).
call_expanded(Doing,G,F,A):-not(member(query_with_pred(_),Doing)),mpred_prop(F,query_with_pred(M)),!,call_expanded([query_with_pred(M)|Doing],call(M,G),F,A).
call_expanded(Doing,G,F,A):-not(member(assertedOnly,Doing)),!,is_asserted(G);call_expanded([assertedOnly|Doing],G,F,A).
call_expanded(_Doing,G,F,_A):-assertedOnly(F),!,is_asserted(G).
call_expanded(_Doing,G,F,_A):-with_callMPred(F),!,call_mpred(F,G).
call_expanded(Doing,G,F,A):-not(member(with_callMPred,Doing)),!,(call_mpred(F,G);call_expanded([with_callMPred|Doing],G,F,A)).


call_mpred(M:C):-atom(M),!,call_mpred(C).
call_mpred(C):- compound(C),functor(C,F,_),!,call_mpred(F,C).
call_mpred(_,C):- predicate_property(C,_),debugOnError(C),check_was_known_false(C).
call_mpred(F,C):- not(mpred_prop(F,hasStub(body_req))),is_asserted(F,C).


naf(Goal):-not(req(Goal)).

:-decl_mpred_prolog(naf/1).
:-decl_mpred_prolog(call_mpred/1).





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



