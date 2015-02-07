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
:-dynamic_multifile_exported((call_expanded_for/2)).

call_expanded_for(req,Call):- !,call_mpred(Call).
call_expanded_for(must,Call):- !,must(call_mpred(Call)).
call_expanded_for(assertedOnly,Call):- !,with_assertions(thlocal:infInstanceOnly(F),call_mpred(F,Call)).
call_expanded_for(once,Call):- !,once(call_mpred(Call)).
call_expanded_for(_Req,Call):- !,call_mpred(Call).


:-meta_predicate_transparent(is_callable(0)).
is_callable(C):-predicate_property(C,_),!.

:-dynamic_multifile_exported(call_mpred/1).
:-dynamic_multifile_exported(call_mpred/2).

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

call_mpred_0(cmdShowRoomGrid(W)):-!,cmdShowRoomGrid(W).
call_mpred_0(C):- compound(C),!,functor(C,F,_),!,call_mpred(F,C).
call_mpred_0(C):- debugOnError(C),!.  % just atoms

% call_mpred_1(F,G):-loop_check(call_mpred(F,G),trace_or_throw(call_mpred(F,G))).

% call_mpred(_,Goal):-mcall(Goal).
call_mpred(call,C):- !,debugOnError(C).
call_mpred(_,C):- into_mpred_form(C,MP),MP\=@=C,!,call_mpred(MP).
call_mpred(_,dbase_t([H|T])):- !,dbase_t([H|T]).
call_mpred(F,C):- mpred_prop(F,prologPTTP),!,debugOnError(pttp_call(C)).
call_mpred(F,C):- mpred_prop(F,prologOnly),!,must_det(is_callable(C)),!,debugOnError(C).
call_mpred(dbase_t,C):- trace_or_throw(not_into_mpred_form(C)),!.

% lazy hooking up of new preds
call_mpred(F,C):- not(is_callable(C)),!,functor(C,F,A),dmsg(todo(non_existent(F/A,C))),!,A>1,
   decl_mpred_hybrid(F/A),!,call_mpred_hybrid(F,C).
call_mpred(F,C):-call_mpred_hybrid(F,C).


call_mpred_hybrid(_,C):- (use_pttp;true),!,call_provided_mpred_storage_op(call(conjecture),C,prologHybrid).
call_mpred_hybrid(F,C):- ground(C),!,call_mpred_real_g(F,C),!.

% nonground
call_mpred_hybrid(F,C):- findall(C,is_asserted(F,C),Results),no_repeats(call_merge_asserted_results(F,C,Results)).

call_merge_asserted_results(_F,C,Results):-member(C,Results).
call_merge_asserted_results( F,C,Results):-call_only_backchain(F,C),not(member(C,Results)),check_mcall_ok(C).

% ground
% call_mpred_real_g(F,C):- mpred_prop(F,prologOnly),!,debugOnError(C),!.
call_mpred_real_g(F,C):- is_asserted(F,C).
call_mpred_real_g(F,C):- call_only_backchain_checked(F,C).
% for now the above line calls C as well
% call_mpred_real_g(_,C):- loop_check(C,fail).


call_only_backchain_checked(F,C):-call_only_backchain(F,C),check_mcall_ok(C).



% call_only_backchain(F,C):- mpred_prop(F,predModule(M2)),context_module(M),M\=M2,!,dmsg(calling_in_other_module(M:call_only_backchain(M2:C))),'@'(M:call_only_backchain(M2:C),M).

call_only_backchain(F,C):-  no_repeats(call_only_backchain_0(F,C)).

call_only_backchain_0(F,C):- loop_check(C,call_only_backchain_lc(F,C)).

call_only_backchain_lc(F,C):- mpred_prop(F,predProxyQuery(P)),PC=..[P,C],!,req(PC).
call_only_backchain_lc(F,C):- mpred_prop(F,prologOnly),!,predicate_property(C,number_of_rules(N)),N>0,!,clause(C,Body),body_no_backchains(C,Body).
call_only_backchain_lc(F,C):- mpred_prop(F,prologPTTP),!,pttp_call(C).
call_only_backchain_lc(_,C):- ruleHybridChain(C,BODY),call_mpred_body(C,BODY).
% TODO call_only_backchain_lc(_,_,_,dbase_t(F,Obj,LValue)):-  choose_val(F,Obj,LValue).


body_no_backchains(_,true):-!.
% body_no_backchains(H,_):- test_tl(thlocal:infInstanceOnly,H),!,fail.
body_no_backchains(_,B):- body_no_backchains_match(B),!,B.
body_no_backchains(H,B):- call_mpred_body(H,B).

body_no_backchains_match((!,body_req(_, _, _, _))).

naf(Goal):-not(req(Goal)).

prologCall(Call):-call(Call).


callable_tf(P,2):- mpred_arity_pred(P),!,fail.
callable_tf(F,A):- functor_safe(P,F,A),predicate_property(P,_),!.
