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



% ================================================
% call_collect/2,4
% ================================================
call_collect(Ops,Call):-call_collect(Ops,Ops,Call,Call).

call_collect(Ops,Flags,Call,Vars):-ground(Call),not(member(ground,Flags)),!,call_collect(Ops,[ground|Flags],Call,Vars).
call_collect(Ops,Flags,Call,Vars):-is_callable(Call),not(member(defined,Flags)),!,call_collect(Ops,[defined|Flags],Call,Vars).
call_collect(Ops,Flags,Call,Vars):-call_collect_op(Ops,Flags,Call,Vars,[],Results),!,member(Vars,Results).

% complete
call_collect_op([],_Flags,_Call,_,Results,Results):-!.

% early complete
call_collect_op(_,Flags,_Call,_Vars,ResultsIn,ResultsOut):- member(firstValue,Flags),length(ResultsIn,L),L>0, member(Minus,[-(_),?(_)]), not(member(Minus,Flags)),!,
   must_det(ResultsIn=ResultsOut),!.

% push flag
call_collect_op([flag(+Flag)|Op2],FlagsIn,Call,Vars,ResultsIn,ResultsOut):- !,
   ord_union(FlagsIn,[Flag],NewFlags),
   call_collect_op(Op2,NewFlags,Call,Vars,ResultsIn,ResultsOut).

% remove flag
call_collect_op([flag(-Flag)|Op2],FlagsIn,Call,Vars,ResultsIn,ResultsOut):- !,
   ord_subtract(FlagsIn,[Flag],NewFlags),
   call_collect_op(Op2,NewFlags,Call,Vars,ResultsIn,ResultsOut).

% replace flags
call_collect_op([flags(NewFlags)|Op2],_OldFlags,Call,Vars,ResultsIn,ResultsOut):- !,
   call_collect_op(Op2,NewFlags,Call,Vars,ResultsIn,ResultsOut).

% sequential opers
call_collect_op([Op1|Op2],Flags,Call,Vars,ResultsIn,ResultsOut):- !,
   call_collect_op(Op1,[doing(Op,Call)|Flags],Call,Vars,ResultsIn,ResultsMid),   
   call_collect_op(Op2,[done(Op,Call)|Flags],Call,Vars,ResultsMid,ResultsOut).

% with_assertions
call_collect_op(with_assertions(Assertions,Op2),Flags,Call,Vars,ResultsIn,ResultsOut):- !,
   with_assertions(Assertions, call_collect_op(Op2,Flags,Call,Vars,ResultsIn,ResultsOut)).

% with_assertions
call_collect_op(with_no_assertions(Assertions,Op2),Flags,Call,Vars,ResultsIn,ResultsOut):- !,
   with_no_assertions(Assertions, call_collect_op(Op2,Flags,Call,Vars,ResultsIn,ResultsOut)).

% add to set
call_collect_op(+Op,Flags,Call,Vars,ResultsIn,ResultsOut):- !,
   setof(Vars,call_one(Op,Flags,Call,Vars),ResultsN), 
   ord_union(ResultsIn,ResultsN,ResultsOut).

% remove from set
call_collect_op(-Op,Flags,Call,Vars,ResultsIn,ResultsOut):- !,
   setof(Vars,(member(Vars,ResultsIn),not(call_one(Op,Flags,Call,Vars))),ResultsOut).

% retain only
call_collect_op('?'(Op),Flags,Call,Vars,ResultsIn,ResultsOut):- !,
   setof(Vars,(member(Vars,ResultsIn),call_one(Op,Flags,Call,Vars)),ResultsOut).

% unknown option
call_collect_op(Op,Flags,Call,Vars,ResultsIn,ResultsOut):- 
   dmsg(error_call_collect(Op,Flags,Call,Vars,ResultsIn,ResultsOut)),
     ResultsIn=ResultsOut.

% ================================================
% call_one/4
% ================================================
% and
call_one((Op1,Op2),Flags,Call,Vars):-  call_collect(Op1,Flags,Call,Vars), call_collect(Op2,Flags,Call,Vars).
% or
call_one((Op1;Op2),Flags,Call,Vars):- call_collect(Op1,Flags,Call,Vars), call_collect(Op2,Flags,Call,Vars).
% basic call
call_one(call,_Flags,Call,_Vars):- call(Call).
% not
call_one(not(Op1),Flags,Call,Vars):-  not(call_collect(Op1,Flags,Call,Vars)).
% succeed vars
call_one(v(Vars),_Flags,_Call,Vars):-!.
% succeed call
call_one(c(Call),_Flags,Call,_Vars):-!.
% hook:deduce_facts
call_one(deducedSimply,_Flags,Call,_Vars):-!, call_maybe_backchain(deducedSimply(Call)).
% is_asserted
call_one(asserted,_Flags,Call,_Vars):-!, is_asserted(Call).
% macro/1
call_one(macro(Macro),_Flags,Call,Vars):- subst(Macro,c,Call,Macro1),subst(Macro1,v,Vars,Macro2),call(Macro2).
% call something else
call_one(Other,_Flags,Call,_Vars):- call(Other,Call).



deducedSimply(Call):- clause(hook:deduce_facts(Fact,Call),Body),not(is_asserted(Call)),nonvar(Fact),Body,dmsg((deducedSimply2(Call):-Fact)),!,show_call((is_asserted(Fact),ground(Call))).

% deducedSimply(Call):- clause(hook:deduce_facts(Fact,Call),Body),nonvar(Fact),Body,ground(Call),dmsg((deducedSimply1(Call):-Fact)),show_call((is_asserted(Fact),ground(Call))).


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

sc1(dbase_t_p2,C):-dtrace,trace_or_throw(sc1(dbase_t_p2,C)),!,C.
sc1(F,C):-(mpred_prop(F,nonGroundOK);test_tl(in_prolog_source_code)),!,C.
sc1(_,C):-C, ignore((not(ground(C)),dmsg(non_ground_sc1(C)))).

% call_expanded_for_all(Must,F,Vs,Call):- loop_check(sc1(F,call_expanded_for_all0(Must,F,Vs,Call)),(dmsg(failed_looped(call_expanded_for_all0(Must,F,Vs,Call))),!,fail)).
call_expanded_for_all(Must,F,Vs,Call):- loop_check(sc1(F,call_expanded_for_all0(Must,F,Vs,Call)),fail).

%call_expanded_for_all0(_,subclass,_Vs,C):-!,is_asserted(subclass,C).
%call_expanded_for_all0(_,isa_backchaing,_Vs,C):-!,C.

% call_expanded_for_all0(Op, F,Vs,Wild):-dmsg(call_expanded_for(Op,Wild)),fail.
call_expanded_for_all0(assertedOnly,F,_Vs,Call):-!,with_assertions(thlocal:insideIREQ(F),call_mpred_real(F,Call)).
call_expanded_for_all0(_ ,F,[],Call):-!,call_mpred_real(F,Call),!.
call_expanded_for_all0(_ ,F,_Vs,Call):-not(has_free_args(Call)),!,call_mpred_real(F,Call),!.
call_expanded_for_all0(_ ,F,Vs,Call):- setof(Vs,call_mpred_real(F,Call),VVs),member(Vs,VVs).

/*

call_expanded_for_sv(WhatNot,F,A,G,OUT):- nonvar(OUT),replace_arg(G,A,NEW,CC),!,call_expanded_for_sv_2(WhatNot,F,A,CC,NEW),!,NEW=OUT.
call_expanded_for_sv(WhatNot,F,A,G,OUT):- call_expanded_for_sv_2(WhatNot,F,A,G,OUT),!.

call_expanded_for_sv_2(WhatNot,F,A,G,_OUT):-call_expanded([whatnot(WhatNot)],G,F,A),!.
call_expanded_for_sv_2(_WhatNot,F,A,G,OUT):- defaultArgValue(G,F,A,OUT),!.

*/

:-meta_predicate_transparent(is_callable(0)).
is_callable(C):-predicate_property(C,_),!.

:-meta_predicate_transparent(call_expanded(0)).
call_expanded(C):-is_callable(C),!,debugOnError(C).
call_expanded(C):-call_mpred_real(C).
:-export(call_mpred/1).
call_mpred(C):- call_mpred_real(C).
:-export(call_mpred/2).
call_mpred(F,C):- call_mpred_real(F,C).


check_mcall_ok(_):-!.
check_mcall_ok(C):-functor(C,F,_),not(mpred_prop(F,_)),!,ignore(check_was_known_false(C)),!.
check_mcall_ok(C):-checkNoArgViolation(C),check_was_known_false(C),!.
check_mcall_ok(_).

call_mpred_real(G):- var(G),!,trace_or_throw(var_call_expanded(G)).
call_mpred_real(M:C):-atom(M),!,call_mpred_real(C).
call_mpred_real(true):-!.
call_mpred_real(C):- compound(C),functor(C,F,_),!,call_mpred_real(F,C).
call_mpred_real(C):- debugOnError(C),!.

% if this next line compians .. that means it is borken so should be ocmmented out
call_mpred_real(dbase_t,C):- into_mpred_form(C,MP),!,(call_mpred_real(MP)->must_det(call_mpred_real0(dbase_t,C));call_mpred_real0(dbase_t,C)).
call_mpred_real(F,C):-call_mpred_real0(F,C).

call_mpred_real0(F,C):- mpred_prop(F,hasStub(body_req)),!,call_mpred_to_module(F,C).
call_mpred_real0(F,C):- mpred_prop(F,prologOnly),!,call_mpred_to_module_checked(F,C).

call_mpred_real0(F,C):- (ground(C);mpred_prop(F,singleValued)),!,call_mpred_real_g(F,C),!.

% nonground
call_mpred_real0(F,C):- not(mpred_prop(F,_)),!,call_mpred_to_module_checked(F,C).
call_mpred_real0(F,C):- not(is_callable(C)),!,is_asserted(F,C).
call_mpred_real0(F,C):- setof(C,is_asserted(F,C),Results),call_mpred_real_w_results(C,F,Results).

call_mpred_real_w_results(_F,C,Results):-member(C,Results).
call_mpred_real_w_results( F,C,Results):-is_callable(C),!,call_mpred_to_module(F,C),not(member(C,Results)),check_mcall_ok(C).

% ground
call_mpred_real_g(F,C):- not(mpred_prop(F,_)),!,call_mpred_to_module_checked(F,C).
call_mpred_real_g(F,C):- not(is_callable(C)),!,is_asserted(F,C).
call_mpred_real_g(F,C):- call_mpred_to_module_checked(F,C).
call_mpred_real_g(F,C):- is_asserted(F,C).


call_mpred_to_module_checked(F,C):-call_mpred_to_module(F,C),check_mcall_ok(C).



call_mpred_to_module(F,C):- predicate_property(C,visible),not(mpred_prop(F,_)),!,call_maybe_backchain(C).
call_mpred_to_module(F,C):- mpred_prop(F,query_with_pred(P)),PC=..[P,C],!,call_maybe_backchain(PC).
% call_mpred_to_module(F,C):- mpred_prop(F,ask_module(M2)),context_module(M),M\=M2,!,dmsg(calling_in_other_module(M:call_maybe_backchain(M2:C))),'@'(M:call_maybe_backchain(M2:C),M).
call_mpred_to_module(F,C):- not(is_callable(C)),!,functor(C,F,A),dmsg(todo(non_existent(F/A,C))),!,A>1,
   decl_mpred_hybrid(F/A),!,call_mpred_real(F,C).
call_mpred_to_module(_,C):- call_maybe_backchain(C).


call_maybe_backchain(C):- loop_check(call_maybe_backchain_lc(C)).

call_maybe_backchain_lc(C):- predicate_property(C,number_of_rules(N)),N>0,!,clause(C,Body),body_no_backchains(C,Body).
call_maybe_backchain_lc(C):- debugOnError(C).

body_no_backchains(_,true):-!.
body_no_backchains(H,B):- body_no_backchains_match(B),!,not(test_tl(thlocal:insideIREQ,H)),B.
body_no_backchains(H,_):- test_tl(thlocal:insideIREQ,H),!,fail.
body_no_backchains(H,B):- call_mpred_body(H,B).

body_no_backchains_match((!,hook:body_req(_, _, _, _))).

naf(Goal):-not(req(Goal)).

:-decl_mpred_prolog(naf/1).
:-decl_mpred_prolog(call_mpred_real/1).






callable_tf(P,2):- mpred_arity_pred(P),!,fail.
callable_tf(F,A):- functor_safe(P,F,A),predicate_property(P,_),!.

useDBMts:- loaded_external_kbs, test_tl(useOnlyExternalDBs),!.

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


isCycPredArity_ignoreable(P,A):- ignore(mpred_prop(P,cycPred(A))),ignore(mpred_arity(P,A)).

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

% cholds_relaxed_t(P,A1,A2):-var(A1),var(A2),!,dbase_t(P,A1,A2).
cholds_relaxed_t(P,A1,A2):-
  isCycPredArity_ignoreable(P,2),which_t(DBS),
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



into_plist(PLIST,PLIST):- var(PLIST),!,between(2,19,X),length(PLIST,X).
into_plist([P|LIST],[P|LIST]).
into_plist(Call,PLIST):-Call=..PLIST.

:- export(kb_f/1).
kb_f(X):-assertion_f(X).

% todo hook into loaded files!
:- export(assertion_t/1).

assertion_t(Call):- thlocal:useOnlyExternalDBs,!,thglobal:use_cyc_database,with_no_assertions(useOnlyExternalDBs, kb_t(Call),fail).
assertion_t(Call):- thglobal:use_cyc_database,with_assertions(thlocal:useOnlyExternalDBs,kb_t(Call)).
% assertion_t(Call):- with_assertions(thlocal:useOnlyExternalDBs,loop_check(req(Call))).


tiny_kb_ASSERTION(_):- not(is_callable('ASSERTION'(_,_,_,_,_))),!,fail.
tiny_kb_ASSERTION(PLIST):- 'ASSERTION'(':TRUE-DEF',_,_UniversalVocabularyMt,_Vars,/*HL*/PLIST).
tiny_kb_ASSERTION(PLIST):- 'ASSERTION'(':TRUE-MON',_,_UniversalVocabularyMt,_Vars,/*HL*/PLIST).

:-export((kb_t/1)).
kb_t(Call):- into_plist(Call,PLIST),[AH|LIST]=PLIST,!, kb_t(AH,LIST,PLIST).

kb_t(AH,_,PLIST):-var(AH),!,kbp_t(PLIST).
kb_t(dbase_t,PLIST,_):- !,kbp_t(PLIST).
kb_t(subclass,PLIST,_):- !,kbp_t([genls|PLIST]).
kb_t(AH,PLIST,_):- is_holds_true(AH),!,kb_t(PLIST).
kb_t(AH,PLIST,_):- is_holds_false(AH),!,kb_f(PLIST).
kb_t(_,_,PLIST):- kbp_t(PLIST).

:- export(kbp_t/1).
kbp_t(_):- not(useDBMts),!,fail.
% kbp_t(PLIST):- tiny_kb_ASSERTION(PLIST).
kbp_t(PLIST):- append([assertion_holds_mworld0|PLIST],[_,_],CallList),Call=..CallList,mworld:is_callable(Call), mworld0:Call.  % '@'(Call,mworld0).
kbp_t(PLIST):- Call=..[assertion_holds|PLIST],hl_holds:is_callable(Call), hl_holds:Call. % '@'(Call,hl_holds).
% kbp_t(PLIST):- Call=..[ttholds|PLIST],tt0_00022_cycl:is_callable(Call), tt0_00022_cycl:Call. % '@'(Call,hl_holds).


predicate_property_chk(P,PP):-predicate_property(P,PP),!.
predicate_property_chk(P,PP):-warn(predicate_property_chk(P,PP)).

callr(R):-retract(R).

assert_to_dbase_t([isa,X,Y]):- Call=..[dbase_t,Y,X],assert_if_new(Call).
assert_to_dbase_t(PLIST):- xform_plist(PLIST,PLISTO), Call=..[dbase_t|PLISTO],assert_if_new(Call).

xform_plist([implied|PLIST],[implied,ALIST,Last]):-append(ALIST,[Last],PLIST),!.
xform_plist(PLIST,PLIST):-!.

:-export(kbp_to_dbase_t/0).
kbp_to_dbase_t:-!.
kbp_to_dbase_t:-forall(kbp2_t(PLIST),assert_to_dbase_t(PLIST)),retractall(thglobal:use_cyc_database),tell('a.txt'),listing(dbase_t),listing('ASSERTION'),told,dmsg(done_dbase_t).

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
assertion_f(_):- not(useDBMts),!,fail.
assertion_f([P|LIST]):- tiny_kb:'ASSERTION'(':FALSE-DEF',_,_UniversalVocabularyMt,_Vars,/*HL*/[P|LIST]).
assertion_f([P|LIST]):- tiny_kb:'ASSERTION'(':FALSE-MON',_,_UniversalVocabularyMt,_Vars,/*HL*/[P|LIST]).


% ================================================================================
% end holds_f 
% ================================================================================



