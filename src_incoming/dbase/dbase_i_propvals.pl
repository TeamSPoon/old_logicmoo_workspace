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

:- dynamic_multifile_exported hook:create_random_fact/1.
:- dynamic_multifile_exported hook:create_random_instance/3.
:- dynamic_multifile_exported hook:fact_is_false/2.
:- dynamic_multifile_exported((thlocal:noDefaultValues/1)).
:- dynamic_multifile_exported((thlocal:noRandomValues/1)).
:- dynamic_multifile_exported((thlocal:insideIREQ/1)).

:-meta_predicate_transparent(with_fallbacks(0)).
with_fallbacks(Fact):-with_no_assertions(thlocal:noDefaultValues(_),Fact).

:-meta_predicate_transparent(with_fallbacksg(0)).
with_fallbacksg(Fact):-with_no_assertions(thlocal:noRandomValues(_),Fact).

:-meta_predicate_transparent(with_no_fallbacksg(0)).
with_no_fallbacksg(Fact):-with_assertions(thlocal:noRandomValues(_),Fact).

:-meta_predicate_transparent(with_no_fallbacks(0)).
with_no_fallbacks(Fact):-with_assertions(thlocal:noDefaultValues(_),Fact).

:-meta_predicate_transparent(fallback/0).
fallback:- not(thlocal:insideIREQ(_)).

:- dynamic_multifile_exported(transitive_other/4).

choose_val(Prop,Obj,Value):- thlocal:useOnlyExternalDBs,!, body_call_cyckb(dbase_t(Prop,Obj,Value)).
choose_val(Prop,Obj,Value):- var(Obj),nonvar(Value),!,mdif(Obj,Value),is_asserted(dbase_t(Prop,Obj,Value)).
choose_val(Prop,Obj,Value):- mdif(Obj,Value),choose_right(Prop,Obj,Value).

generate_candidate_arg_values(Prop,N,Obj):-call_vars_tabled(Obj,generate_candidate_arg_values0(Prop,N,Obj)).

generate_candidate_arg_values0(Prop,N,R):- cached_isa(Prop,completeExtentAsserted),arg(N,vv(Obj,Value),R),!,is_asserted(dbase_t(Prop,Obj,Value)).
generate_candidate_arg_values0(Prop,N,Obj):- once((argIsa_asserted(Prop,N,Type),type_has_instances(Type))),!,cached_isa(Obj,Type).
generate_candidate_arg_values0(Prop,N,R):- arg(N,vv(Obj,Value),R),!,is_asserted(dbase_t(Prop,Obj,Value)).

type_has_instances(Type):-  atom(Type),Type\=term,Type\=type,not_ft(Type),isa(_,Type),!.

choose_right(Prop,Obj,Value):- thlocal:useOnlyExternalDBs,!, body_call_cyckb(dbase_t(Prop,Obj,Value)).
choose_right(Prop,Obj,Value):- nonvar(Obj),!,choose_for(Prop,Obj,RValue),RValue=Value.
choose_right(Prop,Obj,Value):- cached_isa(Prop,completeExtentAsserted),not(cached_isa(Prop,singleValued)),!,is_asserted(dbase_t(Prop,Obj,Value)).
choose_right(Prop,Obj,Value):- findall(Obj,generate_candidate_arg_values(Prop,1,Obj),Objs),Objs\=[],!,member(Obj,Objs),nonvar(Obj),choose_for(Prop,Obj,Value).
choose_right(Prop,Obj,Value):- dmsg(var_choose_right(Prop,Obj,Value)),!,dtrace,is_asserted(dbase_t(Prop,Obj,Value)).
choose_right(Prop,Obj,Value):- choose_for(Prop,Obj,RValue),RValue=Value.

:-export(choose_for/3).

choose_for(Prop,Obj,Value):- var(Obj),trace_or_throw(var_choose_for(Prop,Obj,Value)).
choose_for(Prop,Obj,Value):- not(is_fact_consistent(dbase_t(Prop,Obj,Value))),!,fail.
choose_for(Prop,Obj,Value):- mpred_prop(Prop,singleValued),!,choose_one(Prop,Obj,Value),!.
choose_for(Prop,Obj,Value):- nonvar(Value),!,choose_each(Prop,Obj,RValue),!,RValue=Value.
choose_for(Prop,Obj,Value):- no_repeats(choose_each(Prop,Obj,Value)).

choose_one(Prop,Obj,Value):- choose_asserted(Prop,Obj,RValue),!,Value=RValue.
% was choose_one(Prop,Obj,Value):- with_fallbacks(with_fallbacksg(fallback_value(Prop,Obj,RValue))),checkNoArgViolation(Prop,Obj,RValue),!,Value = RValue,maybe_save(Obj,Prop,Value).
choose_one(Prop,Obj,_Value):- Fact=.. [Prop,Obj,_],thlocal:insideIREQ(Fact),!,fail.
choose_one(Prop,Obj,Value):- with_fallbacks(fallback_value(Prop,Obj,RValue)),ground(choose_one(Prop,Obj,RValue)),checkNoArgViolation(Prop,Obj,RValue),!,Value = RValue,save_fallback(Obj,Prop,Value).
choose_one(Prop,Obj,Value):- create_someval(Prop,Obj,RValue),ground(create_someval(Prop,Obj,RValue)),ground(create_someval(Prop,Obj,RValue)),checkNoArgViolation(Prop,Obj,RValue),!,Value = RValue,save_fallback(Obj,Prop,Value).

choose_each(Prop,Obj,Value):- mpred_prop(Prop, completeExtentAsserted),!,choose_asserted(Prop,Obj,Value).
choose_each(Prop,Obj,Value):- one_must(choose_asserted(Prop,Obj,Value),(fallback_value(Prop,Obj,Value),maybe_cache(Prop,Obj,Value,Obj))).

% choose_asserted(Prop,Obj,Value):- dbase_t(Prop,Obj,Value). % ,must_det(is_asserted(dbase_t(Prop,Obj,Value))).
% choose_asserted(Prop,Obj,Value):- is_asserted(dbase_t(Prop,Obj,Value)).
choose_asserted(Prop,Obj,Value):- choose_asserted_mid_order(Prop,Obj,Value).
choose_asserted(Prop,Obj,Value):- nonvar(Obj),transitive_other(Prop,1,Obj,What),choose_asserted_mid_order(Prop,Obj,Value),maybe_cache(Prop,Obj,Value,What).

choose_asserted_mid_order(Prop,Obj,Value):-loop_check(choose_asserted_mid_order_all(Prop,Obj,Value),fail).
choose_asserted_mid_order_all(Prop,Obj,Value):- call_mpred(dbase_t(Prop,Obj,Value)).
choose_asserted_mid_order_all(Prop,Obj,_Value):- Fact=.. [Prop,Obj,_],thlocal:insideIREQ(Fact),!,fail.
choose_asserted_mid_order_all(Prop,Obj,Value):- is_asserted(genlPreds(Prop,Other)),choose_asserted(Other,Obj,Value).
choose_asserted_mid_order_all(Prop,Obj,Value):- is_asserted(genlInverse(Prop,Other)),choose_val(Other,Value,Obj).

:-export(create_someval/3).
create_someval(Prop,Obj,Value):- ground(Prop-Obj-Value),!,dmsg(error_create_someval(Prop,Obj,Value)).
create_someval(Prop,Obj,Value):- into_mpred_form(dbase_t(Prop,Obj,Value),Fact),asserted_or_deduced(Fact),!.
create_someval(Prop,Obj,Value):- into_mpred_form(dbase_t(Prop,Obj,Value),Fact),not(test_tl(thlocal:noRandomValues,Fact)),hook:create_random_fact(Fact),!.
create_someval(Prop,Obj,_):- Fact=.. [Prop,Obj,_],test_tl(thlocal:noDefaultValues,Fact),!,fail.
create_someval(Prop,Obj,Value):- fallback_value(Prop,Obj,DValue),!,Value=DValue.
create_someval(Pred,_Arg1,Value):- must_det_l([moo:mpred_arity(Pred,Last),argIsa_call(Pred,Last,Type),create_random(Type,Value,nonvar(Value))]).

asserted_or_deduced(Fact):- is_asserted(Fact),!.
asserted_or_deduced(Fact):- hook:fact_always_true(Fact),must_det(is_fact_consistent(Fact)),!,add(Fact).
asserted_or_deduced(Fact):- test_tl(thlocal:noDefaultValues,Fact),!,fail.
asserted_or_deduced(Fact):- hook:fact_maybe_deduced(Fact),is_fact_consistent(Fact),add(Fact).
asserted_or_deduced(Fact):- deducedSimply(Fact),is_fact_consistent(Fact),add(Fact).

:-export(my_random_member/2).
my_random_member(LOC,LOCS):- length(LOCS,Len),Len>0, X is random(Len),nth0(X,LOCS,LOC).
randomize_list(LOCS,[LOC|LOCS]):- length(LOCS,Len),Len>0, X is random(Len),nth0(X,LOCS,LOC).

:-export(create_random/3).
create_random(Type,Value,Test):- copy_term(create_random(Type,Value,Test),create_random(RType,RValue,RTest)),
   hook:create_random_instance(RType,RValue,RTest),
   checkAnyType(query(_,_),RValue,Type,Value),must_det(Test).
create_random(Type,Value,Test):- atom(Type),atom_concat('random_',Type,Pred),Fact=..[Pred,Value],predicate_property(Fact,_),Fact,Test,!.
create_random(Type,Value,Test):- compound(Type),get_functor(Type,F),isa(F,_),atom_concat('random_',F,Pred),Fact=..[Pred,Value],predicate_property(Fact,_),Fact,Test,!.
create_random(Type,Value,Test):- findall(V,(isa_backchaing(V,Type)),Possibles),Possibles\=[],randomize_list(Possibles,Randomized),!,member(Value,Randomized),Test,!.
create_random(Type,Value,Test):- trace_or_throw(failed(create_random(Type,Value,Test))).

save_fallback(Fact):-not(ground(Fact)),trace_or_throw(var_save_fallback(Fact)).
save_fallback(Fact):-is_fact_consistent(Fact),add(Fact).

save_fallback(Obj,Prop,Value):-not(ground(padd(Obj,Prop,Value))),trace_or_throw(var_save_fallback(Obj,Prop,Value)).
save_fallback(Obj,Prop,Value):-is_fact_consistent(dbase_t(Prop,Obj,Value)),padd(Obj,Prop,Value).
maybe_cache(Prop,Obj,Value,What):-not(not(maybe_cache_0(Prop,Obj,Value,What))).

:-export(checkNoArgViolation/1).
checkNoArgViolation(_).
checkNoArgViolation(_):- not(bad_idea),!.
checkNoArgViolation(Fact):-Fact=..[dbase_t,Prop|ObjValue],!,checkNoArgViolation_p_args(Prop,ObjValue),!.
checkNoArgViolation(Fact):-Fact=..[Prop|ObjValue],!,checkNoArgViolation_p_args(Prop,ObjValue),!.
checkNoArgViolation(_).

dont_check_args(Fact):-functor(Fact,F,A),dont_check_args(F,A).
dont_check_args(isa,2).
dont_check_args(mpred_prop,2).
dont_check_args(mpred_arity,2).
dont_check_args(A,1):-atom(A).


checkNoArgViolation_p_args(isa,_).
checkNoArgViolation_p_args(F,List):-is_list(List),length(List,A),dont_check_args(F,A),!.
checkNoArgViolation_p_args(_,_):- test_tl(no_arg_type_error_checking),!.
checkNoArgViolation_p_args(Prop,[Obj,Value]):-!,checkNoArgViolation(Prop,Obj,Value).
checkNoArgViolation_p_args(Prop,[Obj,Value|_More]):-checkNoArgViolation(Prop,Obj,Value).
checkNoArgViolation_p_args(_,_).

:-thread_local deduceArgTypes/1.

checkNoArgViolation(isa,_,_):-!.
checkNoArgViolation(Prop,__,Value):-checkNoArgViolationOrDeduceInstead(Prop,2,Value),fail.
checkNoArgViolation(Prop,Obj,__):-checkNoArgViolationOrDeduceInstead(Prop,1,Obj),fail.
checkNoArgViolation(_,_,_):-!.

checkNoArgViolationOrDeduceInstead(Prop,N,Obj):-argIsa_call(Prop,N,Type),not(unverifiableType(Type)),findall(OT,isa(Obj,OT),OType),checkNoArgViolationOrDeduceInstead(Prop,N,Obj,OType,Type).


checkNoArgViolationOrDeduceInstead(_Prop,_,Obj,_OType,_Type):-var(Obj),!.
checkNoArgViolationOrDeduceInstead(Prop,N,[Obj],OType,Type):-!,checkNoArgViolationOrDeduceInstead(Prop,N,Obj,OType,Type).
checkNoArgViolationOrDeduceInstead(Prop,N,Obj,OType,Type):- not(thlocal:deduceArgTypes(Prop)),!,reallyCheckArgViolation(Prop,N,Obj,OType,Type).
checkNoArgViolationOrDeduceInstead(Prop,N,Obj,OType,Type):- deduce_argN(Prop,N,Obj,OType,Type),fail.
checkNoArgViolationOrDeduceInstead(Prop,N,Obj,_,_):- argIsa_call(Prop,N,Type),findall(OT,isa(Obj,OT),OType),reallyCheckArgViolation(Prop,N,Obj,OType,Type).

reallyCheckArgViolation(Prop,N,_Obj,_OType,argIsaFn(Prop,N)):-!.
reallyCheckArgViolation(_,_,_,List,Type):-memberchk(Type,List),!.
reallyCheckArgViolation(Prop,N,Obj,OType,Type):- violatesType(Obj,Type),trace_or_throw(violatesType_maybe_cache(Prop,N,Obj,OType\=Type)).


assert_argIsa(Prop,N,Type):-add(argIsa(Prop,N,Type)).

assert_subclass_on_argIsa(Prop,N,argIsaFn(Prop,N)):-!.
assert_subclass_on_argIsa(Prop,N,_OType):-argIsa_call(Prop,N,PropType),PropType=argIsaFn(Prop,N),!. % , assert_argIsa(Prop,N,OType).
assert_subclass_on_argIsa(Prop,N,OType):-argIsa_call(Prop,N,PropType),assert_subclass_safe(OType,PropType).

deduce_argN(Prop,2,Obj,OType,argIsaFn(Prop, 2)):-atom_concat(Prop,'_value',Type),decl_type(Type),assert_argIsa(Prop,2,Type),deduce_argN(Prop,2,Obj,OType,Type).
deduce_argN(Prop,N,Obj,[],Type):- Type \= argIsaFn(Prop,N), type(Type), assert_isa(Obj,Type),!.
deduce_argN(Prop,N,_Obj,[OType|_],_Type):-assert_subclass_on_argIsa(Prop,N,OType),!.

maybe_cache_0(Prop,Obj,Value,_What):- checkNoArgViolation(Prop,Obj,Value), is_asserted(dbase_t(Prop,Obj,Value)),!.
maybe_cache_0(Prop,Obj,Value,What):- padd(Obj,Prop,Value),
  ignore((What\=Obj,
   into_mpred_form(dbase_t(Prop,What,_),Trigger),hooked_asserta(on_change_once(retract(_),Trigger,del(dbase_t(Prop,Obj,Value)))))).

:-dynamic_multifile_exported(on_change_once/3).
:-dynamic_multifile_exported(on_change_always/3).

unverifiableType(term).
unverifiableType(voprop).
unverifiableType(id).
unverifiableType(mpred).
unverifiableType(text).
unverifiableType(fpred).
unverifiableType(dir).
unverifiableType(string).
unverifiableType(formattype).
unverifiableType(type).
unverifiableType(term(_)).
unverifiableType(list(_)).

violatesType(Value,Type):-var(Value),!,Type=var.
violatesType(_,Type):- unverifiableType(Type),!,fail.
% violatesType(_,type):-!,fail.
violatesType(Value,int):-number(Value),!,fail.
violatesType(Value,Type):-atom(Type),isa_backchaing(Value,Type),!,fail.
violatesType(Value,string):-string(Value),!,fail.
%violatesType(apath(_,_),Type):-!,(Type\=areaPath,Type\=obj).
violatesType(Value,Type):- compound(Type),!,not(term_is_ft(Value,Type)),!.
violatesType(Value,Type):- once((isa_backchaing(Value,_))), no_loop_check(not(isa_backchaing(Value,Type))).

hook:decl_database_hook(Type,Changer):- retract(on_change_once(Type,Changer,Fact)),Fact.
hook:decl_database_hook(Type,Changer):- forall(on_change_always(Type,Changer,Fact),Fact).

/*
% = falbacks come from...

*/
is_fact_consistent(Fact):-is_asserted(Fact),!.
is_fact_consistent(Fact):-into_mpred_form(Fact,MForm), not(hook:fact_is_false(MForm,_Why)).

hook:decl_database_hook(assert(_),Fact):- hook:fact_is_false(Fact,Why),trace_or_throw(hook:fact_is_false(Fact,Why)).

hook:decl_database_hook(assert(_),Fact):- ignore((not(dont_check_args(Fact)),slow_kb_op(checkNoArgViolation(Fact)))).


:-export(fallback_value/3).
fallback_value(Prop,Obj,Value):- is_asserted(dbase_t(Prop,Obj,Value)),!.
fallback_value(_Prop,Obj,_Value):-var(Obj),!,fail.
fallback_value(Prop,_Obj,_Value):-no_fallback(Prop,2),!,fail.
fallback_value(Prop,Obj,Value):-Fact=..[Prop,Obj,Value], 
   with_assertions(thlocal:noDefaultValues(Fact),defaultArgValue(Fact,Prop,2,ValueR)),!,
   checkNoArgViolation(Prop,Obj,ValueR),is_fact_consistent(Fact),
   Value=ValueR.

:-dmsg_hide(defaultArgValue).

no_fallback(subclass,2).
no_fallback(P,2):-not(mpred_prop(P,singleValued)).

:-export(defaultArgValue/4).
defaultArgValue(Fact,F,A,OLD):- stack_check(1000), mpred_prop(F,default_sv(A,OLD)),!,dmsg(defaultArgValue(fallback_value(Fact,F,default_sv(A,OLD)))).
defaultArgValue(facing(_,_),_,2,"n"):-!.
defaultArgValue(change(_,_),_,2,200):-!.
defaultArgValue(damage(_,_),_,2,500):-!.
defaultArgValue(Fact,F,A,Value):- Fact=..[F,P|Args],is_fact_consistent(Fact),defaultArgValue(Fact,F,A,P,Args,Value).

defaultArgValue(Fact,F,A,P,_Args,Value):-var(P),!,argIsa_call(F,A,Type),defaultTypeValue(Fact,Type,Value),!,dmsg(defaultArgValue(using_defaultTypeValue1(Fact,Type,Value))).
defaultArgValue(_Call,F,2,P,[Arg],Arg):-create_someval(F,P,Arg),!. 
defaultArgValue(Fact,F,LastPlus1,I,_Args,Value):- get_instance_default_props(I,PropList),Last is LastPlus1 - 1,
      functor(Prop,F,Last),member(Prop,PropList),arg(Last,Prop,Value),!,dmsg(defaultArgValue(defaultArgValue_get_type_props(Fact))).

defaultArgValue(Fact,F,A,_P,_Args,Value):-argIsa_call(F,A,Type),is_fact_consistent(Fact),defaultTypeValue(Fact,Type,Value),!.


defaultTypeValue(Fact,_,_):- thlocal:noRandomValues(Fact),!,fail.
defaultTypeValue(_,Type,_):- thlocal:noRandomValues(Type),!,fail.
defaultTypeValue(_Info,dir,"n").
defaultTypeValue(_Info,int,0).
defaultTypeValue(Fact,Type,Out):- create_random(Type,ROut,nonvar(ROut)),dmsg(defaultArgValue(create_random(Fact,Type,ROut=Out))),!,Out=ROut.


:-export(get_instance_default_props/2).

get_instance_default_props(Inst,TraitsO):- must_det(nonvar(Inst)),!,
   findall(Props,((type_w_default_props(Type),isa(Inst,Type),each_default_inst_type_props(Inst,Type,Props))),Traits),flatten_set(Traits,TraitsO),!.

:-export((get_type_default_props/2)).

get_type_default_props(Type,TraitsO):- nonvar(Type),!, Inst = self,
   findall(Props,((type_w_default_props(DefType),transitive_subclass_or_same(Type,DefType),each_default_inst_type_props(Inst,DefType,Props))),Traits),flatten_set(Traits,TraitsO),!.
get_type_default_props(DefType,TraitsO):- type_w_default_props(DefType), Inst = self,
   findall(Props,((each_default_inst_type_props(Inst,DefType,Props))),Traits),flatten_set(Traits,TraitsO),!.



type_w_default_props(Type):-call_tabled_can(type_w_defaults_asserted(Type)).

type_w_defaults_asserted(Type):- is_asserted(moo:default_inst_props(_,Type,_)),nonvar(Type).
type_w_defaults_asserted(Type):- is_asserted(moo:default_type_props(Type,_)),nonvar(Type).
type_w_defaults_asserted(Type):- is_asserted(moo:label_type_props(_,Type,_)),nonvar(Type).

each_default_inst_type_props(Inst,Type,Props):-call_no_cuts(moo:default_inst_props(Inst,Type,TProps)),subst(TProps,self,Inst,Prop),flatten([Prop],Props).
each_default_inst_type_props(Inst,Type,Props):-call_no_cuts(moo:default_type_props(Type,TProps)),subst(TProps,self,Inst,Prop),flatten([Prop],Props).
each_default_inst_type_props(_,Type,[kwLabel(Lbl)|Props]):-call_no_cuts(moo:label_type_props(Lbl,Type,Props)).

moo:default_inst_props(apath(Region,_Dir),areaPath,[localityOfObject(Region)]).


:-export((add_missing_instance_defaults/1)).
add_missing_instance_defaults(P):-
   loop_check(add_missing_instance_defaults_lc(P)).
add_missing_instance_defaults_lc(P):-
   get_inst_default_props(P,_PropListL,Missing),
   once(Missing=[];show_call(padd(P,Missing))).

:-export(gather_props_for/3).
gather_props_for(Op,Obj,Props):-findall(Prop,(length(REST,L),L<9,call_expanded_for(Op,dbase_t([P,Obj|REST])),Prop=..[P|REST]),Props).

:-export(get_inst_default_props/3).
get_inst_default_props(I,PropListL,Missing):-
   get_instance_default_props(I,PropListL),
   instance_missing_props(I,PropListL,Missing).


:-export(instance_missing_props/3).
instance_missing_props(I,LPS,PS):-
       must_det(once((nonvar(LPS);get_instance_default_props(I,LPS)))),
       findall(P,(member(P,LPS),inst_missing_prop(I,P)),PS),!.

inst_missing_prop(I,P):- P=..[F|Args], MP=..[F,I|Args],inst_missing_prop(I,MP,F).
inst_missing_prop(_,_,F):- mpred_prop(F,flag),!,fail.
inst_missing_prop(_,MP,F):- must_det((MP=..[F|Args],get_sv_argnum(F,Args,A),replace_arg(MP,A,BLANK,COLD))), ignore(ireq(COLD)),!,var(BLANK).

get_sv_argnum(F,Args,ArgNum):-once(mpred_prop(F,functionalArg(ArgNum));length(Args,ArgNum)).


:-export(rescan_default_props/0).

rescan_default_props:- loop_check(rescan_default_props_lc,true).
% rescan_default_props_lc:- dmsg(todo(fix(rescan_default_props,"to not set atloc/2"))),!,
rescan_default_props_lc:-
   once((forall_setof(get_type_default_props(Type,PropList),
    once((dmsg(get_type_default_props(Type,PropList)),
     ignore((fail,forall_setof(isa(I,Type), 
       ignore((
         not(Type == I),
         once(instance_missing_props(I,PropList,Missing)),
          Missing \= [],
          dmsg(rescan_default_props_for(I,Type,missing_from(Missing))),
          padd(I,Missing))))))))))),fail.

rescan_default_props_lc:-ignore(rescan_duplicated_facts).


% :- include(logicmoo(parsing/parser_chat80)). 

