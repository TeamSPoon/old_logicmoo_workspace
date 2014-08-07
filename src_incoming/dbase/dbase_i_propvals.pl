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


:-export((noDefaultValues/1)).
:-thread_local noDefaultValues/1.
:-export((noRandomValues/1)).
:-thread_local noRandomValues/1.

:-meta_predicate_transparent(with_fallbacks(0)).
with_fallbacks(Call):-with_no_assertions(noDefaultValues(_),Call).

:-meta_predicate_transparent(with_fallbacksg(0)).
with_fallbacksg(Call):-with_no_assertions(noRandomValues(_),Call).

:-meta_predicate_transparent(with_no_fallbacksg(0)).
with_no_fallbacksg(Call):-with_assertions(noRandomValues(_),Call).

:-meta_predicate_transparent(with_no_fallbacks(0)).
with_no_fallbacks(Call):-with_assertions(noDefaultValues(_),Call).



:- dynamic_multifile_exported(transitive_other/4).

choose_val(Prop,Obj,Value):- var(Obj),nonvar(Value),!,is_asserted(dbase_t(Prop,Obj,Value)).
choose_val(Prop,Obj,Value):- choose_right(Prop,Obj,Value).

generate_candidate_arg_values(Prop,N,Obj):-call_vars_tabled(Obj,generate_candidate_arg_values0(Prop,N,Obj)).

generate_candidate_arg_values0(Prop,N,R):- cached_isa(Prop,extentKnown),arg(N,vv(Obj,Value),R),!,is_asserted(dbase_t(Prop,Obj,Value)).
generate_candidate_arg_values0(Prop,N,Obj):- once((argIsa_asserted(Prop,N,Type),type_has_instances(Type))),!,cached_isa(Obj,Type).
generate_candidate_arg_values0(Prop,N,R):- arg(N,vv(Obj,Value),R),!,is_asserted(dbase_t(Prop,Obj,Value)).

type_has_instances(Type):-  atom(Type),Type\=term,Type\=type,not_ft(Type),isa(_,Type),!.

choose_right(Prop,Obj,Value):- var(Obj),cached_isa(Prop,extentKnown),not(cached_isa(Prop,singleValued)),!,is_asserted(dbase_t(Prop,Obj,Value)).
choose_right(Prop,Obj,Value):- var(Obj),findall(Obj,generate_candidate_arg_values(Prop,1,Obj),Objs),Objs\=[],!,member(Obj,Objs),nonvar(Obj),choose_for(Prop,Obj,Value).
choose_right(Prop,Obj,Value):- var(Obj),dmsg(var_choose_right(Prop,Obj,Value)),!,is_asserted(dbase_t(Prop,Obj,Value)).
choose_right(Prop,Obj,Value):- choose_for(Prop,Obj,RValue),RValue=Value.

:-export(choose_for/3).

choose_for(Prop,Obj,Value):-var(Obj),trace_or_throw(var_choose_for(Prop,Obj,Value)).
choose_for(Prop,Obj,Value):- mpred_prop(Prop,singleValued),!,choose_one(Prop,Obj,Value),!.
choose_for(Prop,Obj,Value):- nonvar(Value),!,choose_each(Prop,Obj,RValue),!,RValue=Value.
choose_for(Prop,Obj,Value):- findall(choose_each(Prop,Obj,Value),choose_each(Prop,Obj,Value),Results),Results \== [],!,
   member(choose_each(Prop,Obj,Value),Results).

choose_one(Prop,Obj,Value):- choose_asserted(Prop,Obj,RValue),!,Value=RValue.
choose_one(Prop,Obj,Value):- with_fallbacks(with_fallbacksg(fallback_value(Prop,Obj,RValue))),checkNoArgViolation(Prop,Obj,RValue),!,Value = RValue,padd(Obj,Prop,Value).
choose_one(Prop,Obj,Value):- create_someval(Prop,Obj,RValue),checkNoArgViolation(Prop,Obj,RValue),!,Value = RValue,padd(Obj,Prop,Value).

choose_each(Prop,Obj,Value):- mpred_prop(Prop, extentKnown),!,choose_asserted(Prop,Obj,Value).
choose_each(Prop,Obj,Value):- one_must(choose_asserted(Prop,Obj,Value),(fallback_value(Prop,Obj,Value),maybe_cache(Prop,Obj,Value,Obj))).

% choose_asserted(Prop,Obj,Value):- dbase_t(Prop,Obj,Value). % ,must_det(is_asserted(dbase_t(Prop,Obj,Value))).
% choose_asserted(Prop,Obj,Value):- is_asserted(dbase_t(Prop,Obj,Value)).
choose_asserted(Prop,Obj,Value):- var(Obj),!,choose_asserted_mid_order(Prop,Obj,Value).
choose_asserted(Prop,Obj,Value):- transitive_other(Prop,1,Obj,What),choose_asserted_mid_order(Prop,Obj,Value),maybe_cache(Prop,Obj,Value,What).

choose_asserted_mid_order(Prop,Obj,Value):-loop_check(choose_asserted_mid_order_all(Prop,Obj,Value),fail).
choose_asserted_mid_order_all(Prop,Obj,Value):- call_mpred(dbase_t(Prop,Obj,Value)).
choose_asserted_mid_order_all(Prop,Obj,Value):- is_asserted(genlPreds(Prop,Other)),choose_asserted(Other,Obj,Value).
choose_asserted_mid_order_all(Prop,Obj,Value):- is_asserted(genlInverse(Prop,Other)),choose_val(Other,Value,Obj).


maybe_cache(Prop,Obj,Value,What):-not(not(maybe_cache_0(Prop,Obj,Value,What))).

:-export(checkNoArgViolation/1).
checkNoArgViolation(Fact):-Fact=..[dbase_t,Prop|ObjValue],!,checkNoArgViolation_p_args(Prop,ObjValue),!.
checkNoArgViolation(Fact):-Fact=..[Prop|ObjValue],!,checkNoArgViolation_p_args(Prop,ObjValue),!.
checkNoArgViolation(_).

checkNoArgViolation_p_args(isa,_).
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
checkNoArgViolationOrDeduceInstead(Prop,N,Obj,OType,Type):- not(deduceArgTypes(Prop)),!,reallyCheckArgViolation(Prop,N,Obj,OType,Type).
checkNoArgViolationOrDeduceInstead(Prop,N,Obj,OType,Type):- deduce_argN(Prop,N,Obj,OType,Type),fail.
checkNoArgViolationOrDeduceInstead(Prop,N,Obj,_,_):- argIsa_call(Prop,N,Type),findall(OT,isa(Obj,OT),OType),reallyCheckArgViolation(Prop,N,Obj,OType,Type).

reallyCheckArgViolation(Prop,N,_Obj,_OType,argIsaFn(Prop,N)):-!.
reallyCheckArgViolation(Prop,N,Obj,OType,Type):- violatesType(Obj,Type),trace_or_throw(violatesType_maybe_cache(Prop,N,Obj,OType\=Type)).


assert_argIsa(Prop,N,Type):-add(argIsa(Prop,N,Type)).

assert_subclass_on_argIsa(Prop,N,argIsaFn(Prop,N)):-!.
assert_subclass_on_argIsa(Prop,N,_OType):-argIsa_call(Prop,N,PropType),PropType=argIsaFn(Prop,N),!. % , assert_argIsa(Prop,N,OType).
assert_subclass_on_argIsa(Prop,N,OType):-argIsa_call(Prop,N,PropType),assert_subclass_safe(OType,PropType).

deduce_argN(Prop,2,Obj,OType,argIsaFn(Prop, 2)):-atom_concat(Prop,'_value',Type),decl_type(Type),assert_argIsa(Prop,2,Type),deduce_argN(Prop,2,Obj,OType,Type).
deduce_argN(Prop,N,Obj,[],Type):- Type \= argIsaFn(Prop,N), type(Type), assert_isa(Obj,Type),!.
deduce_argN(Prop,N,_Obj,[OType|_],_Type):-assert_subclass_on_argIsa(Prop,N,OType),!.

hook:decl_database_hook(assert(_),Fact):-checkNoArgViolation(Fact).

maybe_cache_0(Prop,Obj,Value,_What):- checkNoArgViolation(Prop,Obj,Value).
maybe_cache_0(Prop,Obj,Value,_What):-is_asserted(dbase_t(Prop,Obj,Value)),!.
maybe_cache_0(Prop,Obj,Value,What):- padd(Obj,Prop,Value),
   into_mpred_form(dbase_t(Prop,What,_),Trigger),hooked_asserta(on_change_once(retract(_),Trigger,del(dbase_t(Prop,Obj,Value)))).

:-dynamic_multifile_exported(on_change_once/3).
:-dynamic_multifile_exported(on_change_always/3).

unverifiableType(term).
unverifiableType(voprop).
unverifiableType(id).
unverifiableType(mpred).
unverifiableType(text).
unverifiableType(fpred).
unverifiableType(formattype).
unverifiableType(type).
unverifiableType(term(_)).
unverifiableType(list(_)).

violatesType(Value,Type):-var(Value),!,Type=var.
violatesType(_,Type):- unverifiableType(Type),!,fail.
% violatesType(_,type):-!,fail.
violatesType(Value,int):-number(Value),!,fail.
%violatesType(apath(_,_),Type):-!,(Type\=areaPath,Type\=obj).
violatesType(Value,Type):- compound(Type),!,not(term_is_ft(Value,Type)),!.
violatesType(Value,Type):- once((isa_backchaing(Value,_))), no_loop_check(not(isa_backchaing(Value,Type))).

hook:decl_database_hook(Type,Changer):- retract(on_change_once(Type,Changer,Call)),Call.
hook:decl_database_hook(Type,Changer):- forall(on_change_always(Type,Changer,Call),Call).


:-export(fallback_value/3).
fallback_value(Prop,Obj,Value):- is_asserted(dbase_t(Prop,Obj,Value)),!.
fallback_value(_Prop,Obj,_Value):-var(Obj),!,fail.
fallback_value(Prop,_Obj,_Value):-no_fallback(Prop,2),!,fail.
fallback_value(Prop,Obj,Value):-Call=..[Prop,Obj,Value], 
   with_assertions(noDefaultValues(Call),defaultArgValue(Call,Prop,2,ValueR)),!,
   checkNoArgViolation(Prop,Obj,ValueR),
   Value=ValueR.

:-dmsg_hide(defaultArgValue).

no_fallback(subclass,2).
no_fallback(P,2):-not(mpred_prop(P,singleValued)).

:-export(defaultArgValue/4).
defaultArgValue(Call,F,A,OLD):- stack_check(1000), mpred_prop(F,default_sv(A,OLD)),!,dmsg(defaultArgValue(fallback_value(Call,F,default_sv(A,OLD)))).
defaultArgValue(facing(_,_),_,2,"n"):-!.
defaultArgValue(change(_,_),_,2,200):-!.
defaultArgValue(damage(_,_),_,2,500):-!.
defaultArgValue(Call,F,A,Value):-Call=..[F,P|Args],defaultArgValue(Call,F,A,P,Args,Value).

defaultArgValue(Call,F,A,P,_Args,Value):-var(P),!,argIsa_call(F,A,Type),defaultTypeValue(Call,Type,Value),!,dmsg(defaultArgValue(using_defaultTypeValue1(Call,Type,Value))).
defaultArgValue(_Call,F,2,P,[Arg],Arg):-create_someval(F,P,Arg),!. 
defaultArgValue(Call,F,LastPlus1,I,_Args,Value):- isa(I,Type),get_type_props(I,Type,PropList),Last is LastPlus1 - 1,
      functor(Prop,F,Last),member(Prop,PropList),arg(Last,Prop,Value),!,dmsg(defaultArgValue(defaultArgValue_get_type_props(Call))).

defaultArgValue(Call,F,A,_P,_Args,Value):-argIsa_call(F,A,Type),defaultTypeValue(Call,Type,Value),!.


defaultTypeValue(Call,_,_):- noRandomValues(Call),!,fail.
defaultTypeValue(_,Type,_):- noRandomValues(Type),!,fail.
defaultTypeValue(_Info,dir,"n").
defaultTypeValue(_Info,int,0).
defaultTypeValue(Call,Type,Out):- create_random(Type,ROut,nonvar(ROut)),dmsg(defaultArgValue(create_random(Call,Type,ROut=Out))),!,Out=ROut.


:-export(findall_type_default_props/3).

findall_type_default_props(Inst,Type,TraitsO):-nonvar(Inst),!,
   findall(Props,each_default_inst_type_props(Inst,Type,Props),Traits),flatten_set(Traits,TraitsO),!.

findall_type_default_props(Inst,Type,TraitsO):- % Inst=self, 
        findall(Props,each_default_inst_type_props(Inst,Type,Props),Traits),flatten_set(Traits,TraitsO),!.


get_type_with_default_props(Type):-call_tabled(get_type_with_default_props_0(Type)).

get_type_with_default_props_0(Type):- is_asserted_clause(moo:one_default_type_prop(_,Type,_),_),nonvar(Type).
get_type_with_default_props_0(Type):- is_asserted_clause(moo:default_type_props(Type,_),_),nonvar(Type).
get_type_with_default_props_0(Type):- is_asserted_clause(moo:default_inst_type_props(_,Type,_),_),nonvar(Type).
get_type_with_default_props_0(Type):- is_asserted_clause(moo:label_type_props(_,Type,_),_),nonvar(Type).

each_default_inst_type_props(Inst,Type,[Prop]):-call_no_cuts(moo:one_default_type_prop(Inst,Type,Prop)).
each_default_inst_type_props(Inst,Type,Props):-call_no_cuts(moo:default_inst_type_props(Inst,Type,Props)).
each_default_inst_type_props(Inst,Type,Props):-call_no_cuts(moo:default_type_props(Type,TProps)),subst(TProps,self,Inst,Props).
each_default_inst_type_props(Inst,Type,[named(Inst),kwLabel(Lbl)|Props]):-call_no_cuts(moo:label_type_props(Lbl,Type,Props)).

moo:one_default_type_prop(apath(Region,_Dir),areaPath,inRegion(Region)).

:-export((get_type_props/3)).
get_type_props(Inst,Type,PropList):- with_no_fallbacks(with_no_fallbacksg(loop_check(get_type_props_lc(Inst,Type,PropList),fail))).
get_type_props_lc(Inst,Type,PropList):- call_tabled(type(Type)),
   findall(Prop,findall_type_default_props(Inst,Type,Prop),PropS),flatten_set([PropS],PropList),PropList\=[].

:-export((add_missing_instance_defaults/1)).
add_missing_instance_defaults(P):-
   get_inst_default_props(P,_PropListL,Missing),
   once(Missing=[];show_call(padd(P,Missing))).

:-export(gather_props_for/3).
gather_props_for(Op,Obj,Props):-findall(Prop,(length(REST,L),L<9,call_expanded_for(Op,dbase_t([P,Obj|REST])),Prop=..[P|REST]),Props).

:-export(instance_missing_props/3).
instance_missing_props(I,LPS,PS):- findall(P,(member(P,LPS),inst_missing_prop(I,P)),PS),!.

:-export(get_inst_default_props/3).
get_inst_default_props(I,PropListL,Missing):-
    findall(PropList,(get_type_props(I,Type,PropList),isa(I,Type)),PropListS),flatten_set(PropListS,PropListL),
       instance_missing_props(I,PropListL,Missing).
      
inst_missing_prop(I,P):-P=..[F|Args],inst_missing_prop(I,F,Args).

inst_missing_prop(_I,F,_Args):-not(mpred_prop(F,singleValued)),!,fail.
inst_missing_prop(_I,F,_Args):-mpred_prop(F,flag),!,fail.
inst_missing_prop(I,F,Args):-C=..[F,I|Args],get_sv_argnum(F,[I|Args],A),arg(A,C,_Default),replace_arg(C,A,BLANK,COLD),ignore(qreq(COLD)),!,var(BLANK).

get_sv_argnum(F,Args,ArgNum):-once(mpred_prop(F,functionalArg(ArgNum));length(Args,ArgNum)).


:-export(scan_default_props/0).

scan_default_props:- loop_check(scan_default_props_lc,true).
scan_default_props_lc:- dmsg(todo(fix(scan_default_props,"to not set atloc/2"))), fail,
   once(forall_setof(get_type_with_default_props(Type),
    forall_setof(isa(I,Type), 
         ignore((not(Type == I),
         once(instance_missing_props(I,PropList,Missing)),Missing\=[],
         dmsg(scan_default_props(I,Type,missing_from(Missing,PropList))),
         padd(I,Missing)))))),fail.
scan_default_props_lc:-ignore(rescan_duplicated_facts).

