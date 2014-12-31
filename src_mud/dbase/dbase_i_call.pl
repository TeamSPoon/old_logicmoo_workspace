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

:-meta_predicate_transparent(call_mpred(0)).
:-meta_predicate_transparent(call_mpred(+,+)).
:-decl_mpred_prolog(call_mpred/1).

% oncely later will throw an error if there where choice points left over by call
oncely(:-(Call)):-!,Call,!.
oncely(:-(Call)):-!,req(Call).
oncely(Call):-once(Call).



% ================================================
% call_typelect/2,4
% ================================================
call_typelect(Ops,Call):-call_typelect(Ops,Ops,Call,Call).

call_typelect(Ops,Flags,Call,Vars):-ground(Call),not(member(ground,Flags)),!,call_typelect(Ops,[ground|Flags],Call,Vars).
call_typelect(Ops,Flags,Call,Vars):-is_callable(Call),not(member(defined,Flags)),!,call_typelect(Ops,[defined|Flags],Call,Vars).
call_typelect(Ops,Flags,Call,Vars):-call_typelect_op(Ops,Flags,Call,Vars,[],Results),!,member(Vars,Results).

% complete
call_typelect_op([],_Flags,_Call,_,Results,Results):-!.

% early complete
call_typelect_op(_,Flags,_Call,_Vars,ResultsIn,ResultsOut):- member(firstValue,Flags),length(ResultsIn,L),L>0, member(Minus,[-(_),?(_)]), not(member(Minus,Flags)),!,
   must_det(ResultsIn=ResultsOut),!.

% push flag
call_typelect_op([flag(+Flag)|Op2],FlagsIn,Call,Vars,ResultsIn,ResultsOut):- !,
   ord_union(FlagsIn,[Flag],NewFlags),
   call_typelect_op(Op2,NewFlags,Call,Vars,ResultsIn,ResultsOut).

% remove flag
call_typelect_op([flag(-Flag)|Op2],FlagsIn,Call,Vars,ResultsIn,ResultsOut):- !,
   ord_subtract(FlagsIn,[Flag],NewFlags),
   call_typelect_op(Op2,NewFlags,Call,Vars,ResultsIn,ResultsOut).

% replace flags
call_typelect_op([flags(NewFlags)|Op2],_OldFlags,Call,Vars,ResultsIn,ResultsOut):- !,
   call_typelect_op(Op2,NewFlags,Call,Vars,ResultsIn,ResultsOut).

% sequential opers
call_typelect_op([Op1|Op2],Flags,Call,Vars,ResultsIn,ResultsOut):- !,
   call_typelect_op(Op1,[doing(Op,Call)|Flags],Call,Vars,ResultsIn,ResultsMid),   
   call_typelect_op(Op2,[done(Op,Call)|Flags],Call,Vars,ResultsMid,ResultsOut).

% with_assertions
call_typelect_op(with_assertions(Assertions,Op2),Flags,Call,Vars,ResultsIn,ResultsOut):- !,
   with_assertions(Assertions, call_typelect_op(Op2,Flags,Call,Vars,ResultsIn,ResultsOut)).

% with_assertions
call_typelect_op(with_no_assertions(Assertions,Op2),Flags,Call,Vars,ResultsIn,ResultsOut):- !,
   with_no_assertions(Assertions, call_typelect_op(Op2,Flags,Call,Vars,ResultsIn,ResultsOut)).

% add to set
call_typelect_op(+Op,Flags,Call,Vars,ResultsIn,ResultsOut):- !,
   findall_nodupes(Vars,call_one(Op,Flags,Call,Vars),ResultsN), 
   ord_union(ResultsIn,ResultsN,ResultsOut).

% remove from set
call_typelect_op(-Op,Flags,Call,Vars,ResultsIn,ResultsOut):- !,
   findall_nodupes(Vars,(member(Vars,ResultsIn),not(call_one(Op,Flags,Call,Vars))),ResultsOut).

% retain only
call_typelect_op('?'(Op),Flags,Call,Vars,ResultsIn,ResultsOut):- !,
   findall_nodupes(Vars,(member(Vars,ResultsIn),call_one(Op,Flags,Call,Vars)),ResultsOut).

% unknown option
call_typelect_op(Op,Flags,Call,Vars,ResultsIn,ResultsOut):- 
   dmsg(error_call_typelect(Op,Flags,Call,Vars,ResultsIn,ResultsOut)),
     ResultsIn=ResultsOut.

% ================================================
% call_one/4
% ================================================
% and
call_one((Op1,Op2),Flags,Call,Vars):-  call_typelect(Op1,Flags,Call,Vars), call_typelect(Op2,Flags,Call,Vars).
% or
call_one((Op1;Op2),Flags,Call,Vars):- call_typelect(Op1,Flags,Call,Vars), call_typelect(Op2,Flags,Call,Vars).
% basic call
call_one(call,_Flags,Call,_Vars):- call(Call).
% not
call_one(not(Op1),Flags,Call,Vars):-  not(call_typelect(Op1,Flags,Call,Vars)).
% succeed vars
call_one(v(Vars),_Flags,_Call,Vars):-!.
% succeed call
call_one(c(Call),_Flags,Call,_Vars):-!.
% deduce_facts
call_one(deducedSimply,_Flags,Call,_Vars):-!, deducedSimply(Call).
% is_asserted
call_one(asserted,_Flags,Call,_Vars):-!, is_asserted(Call).
% macro/1
call_one(macro(Macro),_Flags,Call,Vars):- subst(Macro,c,Call,Macro1),subst(Macro1,v,Vars,Macro2),call(Macro2).
% call something else
call_one(Other,_Flags,Call,_Vars):- call(Other,Call).



deducedSimply(Call):- clause(deduce_facts(Fact,Call),Body),not(is_asserted(Call)),nonvar(Fact),Body,dmsg((deducedSimply2(Call):-Fact)),!,show_call((is_asserted(Fact),ground(Call))).

% deducedSimply(Call):- clause(deduce_facts(Fact,Call),Body),nonvar(Fact),Body,ground(Call),dmsg((deducedSimply1(Call):-Fact)),show_call((is_asserted(Fact),ground(Call))).


% ================================================
% call_expanded_for/2
% ================================================
:-swi_export((call_expanded_for/2)).

call_expanded_for(req,Call):- !,call_mpred(Call).
call_expanded_for(must,Call):- !,must(call_mpred(Call)).
call_expanded_for(assertedOnly,Call):- !,with_assertions(thlocal:insideIREQ(F),call_mpred(F,Call)).
call_expanded_for(once,Call):- !,once(call_mpred(Call)).
call_expanded_for(_Req,Call):- !,call_mpred(Call).


:-meta_predicate_transparent(is_callable(0)).
is_callable(C):-predicate_property(C,_),!.

:-swi_export(call_mpred/1).
:-swi_export(call_mpred/2).

check_mcall_ok(_):-!.
check_mcall_ok(C):-functor(C,F,_),not(mpred_prop(F,_)),!,ignore(check_was_known_false(C)),!.
check_mcall_ok(C):-checkNoArgViolation(C),check_was_known_false(C),!.
check_mcall_ok(_).

call_mpred_fast(true):-!.
call_mpred_fast(C):-call_mpred(C).

call_mpred(G):- var(G),!,trace_or_throw(var_call_expanded(G)).
call_mpred(true):-!.
call_mpred(M:C):-atom(M),!,with_assertions(thlocal:caller_module(prolog,M),call_mpred_0(C)).
call_mpred(C):-call_mpred_0(C).

call_mpred_0(show_room_grid(W)):-!,show_room_grid(W).
call_mpred_0(C):- compound(C),!,functor(C,F,_),!,call_mpred(F,C).
call_mpred_0(C):- debugOnError(C),!.  % just atoms

% call_mpred_1(F,G):-loop_check(call_mpred(F,G),trace_or_throw(call_mpred(F,G))).

call_mpred(_,C):- into_mpred_form(C,MP),MP\=@=C,!,call_mpred(MP).
call_mpred(_,dbase_t([H|T])):- !,dbase_t([H|T]).
call_mpred(dbase_t,C):- trace_or_throw(not_into_mpred_form(C)),!.
call_mpred(F,C):- mpred_prop(F,prologOnly),!,must_det(is_callable(C)),!,debugOnError(C).

% lazy hooking up of new preds
call_mpred(F,C):- not(is_callable(C)),!,functor(C,F,A),dmsg(todo(non_existent(F/A,C))),!,A>1,
   decl_mpred_hybrid(F/A),!,call_mpred(F,C).

call_mpred(F,C):- ground(C),!,call_mpred_real_g(F,C),!.

% nonground
call_mpred(F,C):- findall(C,is_asserted(F,C),Results),no_repeats(call_merge_asserted_results(F,C,Results)).

call_merge_asserted_results(_F,C,Results):-member(C,Results).
call_merge_asserted_results( F,C,Results):-call_only_backchain(F,C),not(member(C,Results)),check_mcall_ok(C).

% ground
% call_mpred_real_g(F,C):- mpred_prop(F,prologOnly),!,debugOnError(C),!.
call_mpred_real_g(F,C):- is_asserted(F,C).
call_mpred_real_g(F,C):- call_only_backchain_checked(F,C).
% for now the above line calls C as well
% call_mpred_real_g(_,C):- loop_check(C,fail).


call_only_backchain_checked(F,C):-call_only_backchain(F,C),check_mcall_ok(C).



% call_only_backchain(F,C):- mpred_prop(F,ask_module(M2)),context_module(M),M\=M2,!,dmsg(calling_in_other_module(M:call_only_backchain(M2:C))),'@'(M:call_only_backchain(M2:C),M).

call_only_backchain(F,C):-  no_repeats(call_only_backchain_0(F,C)).

call_only_backchain_0(F,C):- loop_check(C,call_only_backchain_lc(F,C)).

call_only_backchain_lc(F,C):- mpred_prop(F,query_with_pred(P)),PC=..[P,C],!,req(PC).
call_only_backchain_lc(F,C):- mpred_prop(F,prologOnly),!,predicate_property(C,number_of_rules(N)),N>0,!,clause(C,Body),body_no_backchains(C,Body).
call_only_backchain_lc(_,C):- hybrid_rule(C,BODY),call_mpred_body(C,BODY).
% TODO call_only_backchain_lc(_,_,_,dbase_t(F,Obj,LValue)):-  choose_val(F,Obj,LValue).


body_no_backchains(_,true):-!.
% body_no_backchains(H,_):- test_tl(thlocal:insideIREQ,H),!,fail.
body_no_backchains(_,B):- body_no_backchains_match(B),!,B.
body_no_backchains(H,B):- call_mpred_body(H,B).

body_no_backchains_match((!,body_req(_, _, _, _))).

:-decl_mpred_prolog(naf/1).

naf(Goal):-not(req(Goal)).




callable_tf(P,2):- mpred_arity_pred(P),!,fail.
callable_tf(F,A):- functor_safe(P,F,A),predicate_property(P,_),!.


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


dbase_t(C,I):- trace_or_throw(dbase_t(C,I)),fail,loop_check_term(isa_backchaing(I,C),hasInstance(C,I),fail).

%dbase_t([P|LIST]):- !,dbase_plist_t(P,LIST).
%dbase_t(naf(CALL)):-!,not(dbase_t(CALL)).
%dbase_t(not(CALL)):-!,dbase_f(CALL).
dbase_t(CALL):- into_plist(CALL,[P|LIST]),dbase_plist_t(P,LIST).
dbase_plist_t(P,[]):-!,dbase_t(P).
dbase_plist_t(P,LIST):-var(P),!,CALL=..[dbase_t,P|LIST],debugOnError((CALL)).
dbase_plist_t(dbase_t,LIST):-!, CALL=..[dbase_t|LIST],call(CALL),debugOnError((CALL)).
dbase_plist_t(mpred_prop,[C,I]):-!,ground(I:C),mpred_prop(C,I).
dbase_plist_t(isa,[I,C]):-!,hasInstance(C,I).
dbase_plist_t(P,_):-never_dbase_mpred(P),!,fail.
dbase_plist_t(P,[L|IST]):-is_holds_true(P),!,dbase_plist_t(L,IST).
dbase_plist_t(P,LIST):-is_holds_false(P),!,dbase_f(LIST).
dbase_plist_t(P,LIST):- CALL=..[dbase_t,P|LIST],call(CALL),debugOnError((CALL)).

loop_check_mpred(Call):- !, fail,not(thlocal:insideIREQ(_)),loop_check_local(ireq(Call),fail).
% loop_check_mpred(Call):-loop_check_local(call_mpred(dbase_t,Call),fail).

dbase_t(P,A1,A2,A3,A4,A5,A6,A7):- loop_check_mpred(dbase_t(P,A1,A2,A3,A4,A5,A6,A7)).
dbase_t(P,A1,A2,A3,A4,A5,A6):- loop_check_mpred(dbase_t(P,A1,A2,A3,A4,A5,A6)).
dbase_t(P,A1,A2,A3,A4,A5):- loop_check_mpred(dbase_t(P,A1,A2,A3,A4,A5)).
dbase_t(P,A1,A2,A3,A4):- loop_check_mpred(dbase_t(P,A1,A2,A3,A4)).
dbase_t(P,A1,A2,A3):- loop_check_mpred(dbase_t(P,A1,A2,A3)).
dbase_t(P,A1,A2):- loop_check_mpred(dbase_t(P,A1,A2)).

isCycPredArity_ignoreable(F,A):- ignore(mpred_prop(F,cycPred(A))),ignore(mpred_arity(F,A)).

which_t(dac(d,a_notnow,c,no_fallback)).

holds_t(P,A1,A2,A3,A4,A5,A6,A7):- isCycPredArity_ignoreable(P,7),which_t(DBS),(call_which_t(DBS,P,A1,A2,A3,A4,A5,A6,A7);call_mt_t(DBS,P,A1,A2,A3,A4,A5,A6,A7,_,_);assertion_t([P,A1,A2,A3,A4,A5,A6,A7])).
holds_t(P,A1,A2,A3,A4,A5,A6):- isCycPredArity_ignoreable(P,6),which_t(DBS),(call_which_t(DBS,P,A1,A2,A3,A4,A5,A6);call_mt_t(DBS,P,A1,A2,A3,A4,A5,A6,_,_)).
holds_t(P,A1,A2,A3,A4,A5):- isCycPredArity_ignoreable(P,5),which_t(DBS),(call_which_t(DBS,P,A1,A2,A3,A4,A5);call_mt_t(DBS,P,A1,A2,A3,A4,A5,_,_)).
holds_t(P,A1,A2,A3,A4):- isCycPredArity_ignoreable(P,4),which_t(DBS),(call_which_t(DBS,P,A1,A2,A3,A4);call_mt_t(DBS,P,A1,A2,A3,A4,_,_)).
holds_t(P,A1,A2,A3):- isCycPredArity_ignoreable(P,3),which_t(DBS),(call_which_t(DBS,P,A1,A2,A3);call_mt_t(DBS,P,A1,A2,A3,_,_)).
%holds_t(P,A1,A2):- hotrace(holds_relaxed_t(P,A1,A2)).
holds_t(P,A1,A2):- isCycPredArity_ignoreable(P,2),which_t(DBS),(call_which_t(DBS,P,A1,A2);call_mt_t(DBS,P,A1,A2,_,_)).
holds_t(P,A1):- isCycPredArity_ignoreable(P,1),which_t(DBS),(call_which_t(DBS,P,A1);call_mt_t(DBS,P,A1,_,_)).


% holds_relaxed_t(P,A1,A2):-var(A1),var(A2),!,dbase_t(P,A1,A2).
holds_relaxed_t(P,A1,A2):-
  isCycPredArity_ignoreable(P,2),which_t(DBS),
      relax_term(P,PR,A1,R1,A2,R2),
         holds_relaxed_0_t(DBS,PR,R1,R2).

holds_relaxed_0_t(DBS,P,A1,A2):- call_which_t(DBS,P,A1,A2).
holds_relaxed_0_t(DBS,P,A1,A2):- call_mt_t(DBS,P,A1,A2,_,_).

/*
holds_relaxed_0_t(dac(_,a,_,_),P,A1,A2):- assertion_t([P,A1,A2]).
holds_relaxed_0_t(dac(d,_,_,_),P,A1,A2):- dbase_t(P,A1,A2).
holds_relaxed_0_t(dac(_,_,_,h),P,A1,A2):- call_which_t(DBS,P,A1,A2).
holds_relaxed_0_t(DBS,P,A1,A2):- call_mt_t(DBS,P,A1,A2,_,_).
holds_relaxed_0_t(_DBS,P,A1,A2):- ground((P,A1)), TEMPL=..[P,T1,_],dbase_t(default_sv,TEMPL,2,A2),req(isa(A1,T1)),!.
*/

holds_t([AH,P|LIST]):- is_holds_true(AH),!,holds_plist_t(P,LIST).
holds_t([AH,P|LIST]):- is_holds_false(AH),!,holds_f_p2(P,LIST).
holds_t([P|LIST]):- !,holds_plist_t(P,LIST).
holds_t(not(CALL)):- !, holds_f(CALL).
holds_t(CALL):- '=..'(CALL,PLIST),holds_t(PLIST).

holds_plist_t(P,LIST):- apply(holds_t,[P|LIST]).


call_whichlist_t(dac(d,_,_,_),CALL,_):- dbase_t(CALL).
call_whichlist_t(dac(_,a,_,_),_,List):- assertion_t(List).
call_whichlist_t(dac(_,_,c,_),CALL,_):- xcall_t(CALL).
call_whichlist_t(dac(_,_,_,holds_t),CALL,_):- holds_t(CALL).

call_which_t(DBS,P,A1,A2,A3,A4,A5,A6,A7):- callable_tf(P,7),List= [P,A1,A2,A3,A4,A5,A6,A7], CALL=..List, call_whichlist_t(DBS,CALL,List).
call_which_t(dac(_,_,_,h),P,A1,A2,A3,A4,A5,A6,A7):- holds_t(P,A1,A2,A3,A4,A5,A6,A7).

call_which_t(dac(d,_,_,_),P,A1,A2,A3,A4,A5,A6):- dbase_t(P,A1,A2,A3,A4,A5,A6).
call_which_t(dac(_,a,_,_),P,A1,A2,A3,A4,A5,A6):- assertion_t([P,A1,A2,A3,A4,A5,A6]).
call_which_t(dac(_,_,c,_),P,A1,A2,A3,A4,A5,A6):- callable_tf(P,6),xcall_t(P,A1,A2,A3,A4,A5,A6).
call_which_t(dac(_,_,_,holds_t),P,A1,A2,A3,A4,A5,A6):- holds_t(P,A1,A2,A3,A4,A5,A6).

call_which_t(dac(d,_,_,_),P,A1,A2,A3,A4,A5):- dbase_t(P,A1,A2,A3,A4,A5).
call_which_t(dac(_,a,_,_),P,A1,A2,A3,A4,A5):- assertion_t([P,A1,A2,A3,A4,A5]).
call_which_t(dac(_,_,c,_),P,A1,A2,A3,A4,A5):- callable_tf(P,5),xcall_t(P,A1,A2,A3,A4,A5).
call_which_t(dac(_,_,_,holds_t),P,A1,A2,A3,A4,A5):- holds_t(P,A1,A2,A3,A4,A5).

call_which_t(dac(d,_,c,_),P,A1,A2,A3,A4):- dbase_t(P,A1,A2,A3,A4).
call_which_t(dac(_,a,_,_),P,A1,A2,A3,A4):- assertion_t([P,A1,A2,A3,A4]).
call_which_t(dac(_,_,c,_),P,A1,A2,A3,A4):- callable_tf(P,4),xcall_t(P,A1,A2,A3,A4).
call_which_t(dac(_,_,_,holds_t),P,A1,A2,A3,A4):- holds_t(P,A1,A2,A3,A4).

call_which_t(dac(d,_,_,_),P,A1,A2,A3):- dbase_t(P,A1,A2,A3).
call_which_t(dac(_,a,_,_),P,A1,A2,A3):- assertion_t([P,A1,A2,A3]).
call_which_t(dac(_,_,c,_),P,A1,A2,A3):- callable_tf(P,3),xcall_t(P,A1,A2,A3).
call_which_t(dac(_,_,_,holds_t),P,A1,A2,A3):- holds_t(P,A1,A2,A3).

call_which_t(dac(d,_,_,_),P,A1,A2):- dbase_t(P,A1,A2).
call_which_t(dac(_,a,_,_),P,A1,A2):- assertion_t([P,A1,A2]).
call_which_t(dac(_,_,c,_),P,A1,A2):- callable_tf(P,2),xcall_t(P,A1,A2).
call_which_t(dac(_,_,_,holds_t),P,A1,A2):- holds_t(P,A1,A2).

call_which_t(dac(d,_,_,_),P,A1):- hasInstance(P,A1).
call_which_t(dac(_,a,_,_),P,A1):- assertion_t([P,A1]).
call_which_t(dac(_,_,c,_),P,A1):- callable_tf(P,1),xcall_t(P,A1).
call_which_t(dac(_,_,_,holds_t),P,A1):- holds_t(P,A1).

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

% todo hook into loaded files!
:- swi_export(assertion_t/1).

% assertion_t(Call):- thlocal:useOnlyExternalDBs,!,thglobal:use_cyc_database,with_no_assertions(thlocal:useOnlyExternalDBs,kb_t(Call)).
assertion_t(Call):- thglobal:use_cyc_database,!,with_assertions(thlocal:useOnlyExternalDBs,kb_t(Call)).
% assertion_t(Call):- with_assertions(thlocal:useOnlyExternalDBs,loop_check(req(Call))).

% ================================================================================
% end holds_t
% ================================================================================

:-user_ensure_loaded(dbase_i_call_kb).

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
holds_f([AH,P|LIST]):- is_holds_false(AH),!,holds_plist_t(P,LIST).
holds_f([P|LIST]):- !, holds_f_p2(P,LIST).
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
assertion_f(_):- not(loaded_external_kbs),!,fail.
assertion_f([P|LIST]):- 'TINYKB-ASSERTION'(':FALSE-DEF',_,_UniversalVocabularyMt,_Vars,/*HL*/[P|LIST]).
assertion_f([P|LIST]):- 'TINYKB-ASSERTION'(':FALSE-MON',_,_UniversalVocabularyMt,_Vars,/*HL*/[P|LIST]).


% ================================================================================
% end holds_f 
% ================================================================================



