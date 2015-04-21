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



:- include(logicmoo_i_header).


end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.

assert_argIsa(Prop,N,Type):-show_call_failure(add_fast(argIsa(Prop,N,Type))).


% CANT :-export(transitive_other/4).

choose_val(Prop,Obj,Value):- thlocal:useOnlyExternalDBs,!, body_call_cyckb(dbase_t(Prop,Obj,Value)).
choose_val(Prop,Obj,Value):- var(Obj),nonvar(Value),!,mdif(Obj,Value),is_asserted(dbase_t(Prop,Obj,Value)).
choose_val(Prop,Obj,Value):- mdif(Obj,Value),choose_right(Prop,Obj,Value).

generate_candidate_arg_values(Prop,N,Obj):-call_vars_tabled(Obj,generate_candidate_arg_values0(Prop,N,Obj)).

generate_candidate_arg_values0(Prop,N,R):- cached_isa(Prop,completelyAssertedCollection),arg(N,vv(Obj,Value),R),!,is_asserted(dbase_t(Prop,Obj,Value)).
generate_candidate_arg_values0(Prop,N,Obj):- once((argIsa_known(Prop,N,Type),type_has_instances(Type))),!,cached_isa(Obj,Type).
generate_candidate_arg_values0(Prop,N,R):- arg(N,vv(Obj,Value),R),!,is_asserted(dbase_t(Prop,Obj,Value)).

type_has_instances(Type):-  atom(Type),Type\=ftTerm,Type\=tCol,not_ft(Type),isa(_,Type),!.

choose_right(Prop,Obj,Value):- thlocal:useOnlyExternalDBs,!, body_call_cyckb(dbase_t(Prop,Obj,Value)).
choose_right(Prop,Obj,Value):- nonvar(Obj),!,choose_for(Prop,Obj,RValue),RValue=Value.
choose_right(Prop,Obj,Value):- cached_isa(Prop,completelyAssertedCollection),not(cached_isa(Prop,prologSingleValued)),!,is_asserted(dbase_t(Prop,Obj,Value)).
choose_right(Prop,Obj,Value):- findall(Obj,generate_candidate_arg_values(Prop,1,Obj),Objs),Objs\=[],!,member(Obj,Objs),nonvar(Obj),choose_for(Prop,Obj,Value).
choose_right(Prop,Obj,Value):- dmsg(var_choose_right(Prop,Obj,Value)),!,dtrace,is_asserted(dbase_t(Prop,Obj,Value)).
choose_right(Prop,Obj,Value):- choose_for(Prop,Obj,RValue),RValue=Value.

:-export(choose_for/3).

choose_for(mudAtLoc,Obj,_):- nonvar(Obj),isa_asserted(Obj,tRegion),!,fail.
choose_for(Prop,Obj,Value):- var(Obj),trace_or_throw(var_choose_for(Prop,Obj,Value)).
choose_for(Prop,Obj,Value):- not(is_fact_consistent(dbase_t(Prop,Obj,Value))),!,fail.
choose_for(Prop,Obj,Value):- nonvar(Value),!,choose_for(Prop,Obj,RValue),!,RValue=Value.
choose_for(Prop,Obj,Value):- user:mpred_prop(Prop,prologSingleValued),!,must(choose_one(Prop,Obj,Value)),!.
choose_for(Prop,Obj,Value):- no_repeats(choose_each(Prop,Obj,Value)).

choose_one(mudAtLoc,Obj,_):-nonvar(Obj),isa_asserted(Obj,tRegion),!,fail.
choose_one(Prop,Obj,Value):- choose_asserted(Prop,Obj,RValue),!,Value=RValue.
% was choose_one(Prop,Obj,Value):- with_fallbacks(with_fallbacksg(fallback_value(Prop,Obj,RValue))),checkNoArgViolation(Prop,Obj,RValue),!,Value = RValue,maybe_save(Obj,Prop,Value).
choose_one(Prop,Obj,_Value):- Fact=.. [Prop,Obj,_],thlocal:infInstanceOnly(Fact),!,fail.
choose_one(Prop,Obj,Value):- with_fallbacks(fallback_value(Prop,Obj,RValue)),ground(choose_one(Prop,Obj,RValue)),checkNoArgViolation(Prop,Obj,RValue),!,Value = RValue,save_fallback(Obj,Prop,Value).
choose_one(Prop,Obj,Value):- create_someval(Prop,Obj,RValue),ground(create_someval(Prop,Obj,RValue)),checkNoArgViolation(Prop,Obj,RValue),!,Value = RValue,save_fallback(Obj,Prop,Value).

choose_each(Prop,Obj,Value):- hasInstance(completeExtentAsserted, Prop),!,choose_asserted(Prop,Obj,Value).
choose_each(Prop,Obj,Value):- one_must(choose_asserted(Prop,Obj,Value),(fallback_value(Prop,Obj,Value),maybe_cache(Prop,Obj,Value,Obj))).

% choose_asserted(Prop,Obj,Value):- dbase_t(Prop,Obj,Value). % ,must_det(is_asserted(dbase_t(Prop,Obj,Value))).
choose_asserted(Prop,Obj,Value):- is_asserted(dbase_t(Prop,Obj,Value)),!.
choose_asserted(Prop,Obj,Value):- choose_asserted_mid_order(Prop,Obj,Value),!.
% CANT choose_asserted(Prop,Obj,Value):- nonvar(Obj),transitive_other(Prop,1,Obj,What),choose_asserted_mid_order(Prop,Obj,Value),maybe_cache(Prop,Obj,Value,What).

choose_asserted_mid_order(Prop,Obj,Value):-loop_check(choose_asserted_mid_order_all(Prop,Obj,Value),fail).
choose_asserted_mid_order_all(Prop,Obj,Value):- mpred_call(dbase_t(Prop,Obj,Value)),!.
choose_asserted_mid_order_all(Prop,Obj,_Value):- atom(Prop), Fact=.. [Prop,Obj,_],thlocal:infInstanceOnly(Fact),!,fail.
choose_asserted_mid_order_all(Prop,Obj,Value):- is_asserted(genlPreds(Other,Prop)),choose_asserted(Other,Obj,Value).
% choose_asserted_mid_order_all(Prop,Obj,Value):- is_asserted(genlInverse(Prop,Other)),choose_val(Other,Value,Obj).

:-export(create_someval/3).
create_someval(Prop,Obj,Value):- ground(Prop-Obj-Value),!,dmsg(error_create_someval(Prop,Obj,Value)).
create_someval(Prop,Obj,Value):- into_mpred_form(dbase_t(Prop,Obj,Value),Fact),asserted_or_deduced(Fact),!.
create_someval(Prop,Obj,Value):- into_mpred_form(dbase_t(Prop,Obj,Value),Fact),not(test_tl(thlocal:noRandomValues,Fact)),create_random_fact(Fact),!.
create_someval(Prop,Obj,_):- Fact=.. [Prop,Obj,_],test_tl(thlocal:infAssertedOnly,Fact),!,fail.
create_someval(Prop,Obj,Value):- fallback_value(Prop,Obj,DValue),!,Value=DValue.
create_someval(Pred,_Arg1,Value):- must_det_l([arity(Pred,Last),argIsa(Pred,Last,Type),random_instance(Type,Value,nonvar(Value))]).

asserted_or_deduced(Fact):- is_asserted(Fact),!.
asserted_or_deduced(Fact):- user:fact_always_true(Fact),must_det(is_fact_consistent(Fact)),!,add(Fact).
asserted_or_deduced(Fact):- test_tl(thlocal:infAssertedOnly,Fact),!,fail.
asserted_or_deduced(Fact):- fact_maybe_deduced(Fact),is_fact_consistent(Fact),add(Fact).
asserted_or_deduced(Fact):- deducedSimply(Fact),is_fact_consistent(Fact),add(Fact).


guess_arity(F,GA,A):-GA=0,!,must(arity(F,A)).
guess_arity(F,GA,A):-arity(F,A);A=GA.

save_fallback(Fact):-not(ground(Fact)),trace_or_throw(var_save_fallback(Fact)).
save_fallback(Fact):-is_fact_consistent(Fact),add(Fact).

save_fallback(Obj,Prop,Value):-not(ground(padd(Obj,Prop,Value))),trace_or_throw(var_save_fallback(Obj,Prop,Value)).
save_fallback(Obj,Prop,Value):-is_fact_consistent(dbase_t(Prop,Obj,Value)),padd(Obj,Prop,Value).
maybe_cache(_Prop,_Obj,_Value,_What):-!.
maybe_cache(Prop,Obj,Value,What):-not(not(maybe_cache_0(Prop,Obj,Value,What))).

:-export(checkNoArgViolation/1).
checkNoArgViolation(_).
checkNoArgViolation(_):- (bad_idea),!.
checkNoArgViolation(Fact):-get_prop_args(Fact,Prop,ARGS),checkNoArgViolation_p_args(Prop,ARGS),!.
checkNoArgViolation(_).

get_prop_args(Fact,Prop,ARGS):-Fact=..[dbase_t,Prop|ARGS],!.
get_prop_args(Fact,Prop,ARGS):-Fact=..[Prop|ARGS],!.

dont_check_args(Fact):-functor(Fact,F,A),dont_check_args(F,A).
dont_check_args(isa,2).
dont_check_args(user:mpred_prop,3).
dont_check_args(arity,2).
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

checkNoArgViolationOrDeduceInstead(_,_,_):-!.
checkNoArgViolationOrDeduceInstead(Prop,N,Obj):-argIsa(Prop,N,Type),
   not(isa(Type,ttUnverifiableType)),
   findall(OT,isa(Obj,OT),OType),
   checkNoArgViolationOrDeduceInstead(Prop,N,Obj,OType,Type).

user:hook_coerce(Text,tPred,Pred):- user:mpred_prop(Pred,arity(_)),name_text(Pred,Text).


subft_or_subclass_or_same(C,C):-!.
subft_or_subclass_or_same(S,C):-genls(S,C),!.
checkNoArgViolationOrDeduceInstead(_Prop,_,Obj,_OType,_Type):-var(Obj),!.
checkNoArgViolationOrDeduceInstead(_Prop,_N,_Obj,[H|T],Type):-nonvar(T),!,member(E,[H|T]),subft_or_subclass_or_same(E,Type),!.
checkNoArgViolationOrDeduceInstead(Prop,N,[H|T],OType,Type):-!,forall(member(Obj,[H|T]),checkNoArgViolationOrDeduceInstead(Prop,N,Obj,OType,Type)).
checkNoArgViolationOrDeduceInstead(Prop,N,Obj,OType,Type):- not(thlocal:deduceArgTypes(Prop)),!,reallyCheckArgViolation(Prop,N,Obj,OType,Type).
checkNoArgViolationOrDeduceInstead(Prop,N,Obj,OType,Type):- must_det(deduce_argN(Prop,N,Obj,OType,Type)),fail.
checkNoArgViolationOrDeduceInstead(Prop,N,Obj,_,_):- argIsa(Prop,N,Type),findall(OT,isa(Obj,OT),OType),reallyCheckArgViolation(Prop,N,Obj,OType,Type).

openSubClass(tSpatialThing).
openSubClass(tObj).
openSubClass(tRegion).

reallyCheckArgViolation(Prop,N,_Obj,_OType,argIsaFn(Prop,N)):-!.
reallyCheckArgViolation(_,_,_,List,Type):-memberchk(Type,List),!.
reallyCheckArgViolation(_Prop,_N,_Obj,[OType|_],OpenSubClass):- openSubClass(OpenSubClass), atom(OType),show_call_failure(assert_subclass_safe(OType,OpenSubClass)),!.
reallyCheckArgViolation(Prop,N,Obj,OType,Type):- violatesType(Obj,Type),trace_or_throw(violatesType_maybe_cache(Prop,N,Obj,OType\=Type)).
reallyCheckArgViolation(_,_,_,_,_).

guessed_mpred_arity(F,A):-arity(F,AA),!,A=AA.
guessed_mpred_arity(_,2).

suggestedType(Prop,N,_,argIsaFn(Prop, N),FinalType):- sanity(Prop\=props),guessed_mpred_arity(Prop,N),i_name('vt',Prop,FinalType),!,must((decl_type(FinalType),assert_isa(FinalType,tInferInstanceFromArgType))).
suggestedType(Prop,N,_,_,FinalType):- guessed_mpred_arity(Prop,N),i_name('vt',Prop,FinalType),!,must((decl_type(FinalType),assert_isa(FinalType,tInferInstanceFromArgType))).
suggestedType( _ ,_,_ ,FinalType,FinalType):-atom(FinalType),tCol(FinalType),not(ttFormatType(FinalType)),!.
suggestedType( _ ,_,Possibles,_ ,FinalType):- member(FinalType,[tPred,tCol,ttFormatType,ftText,tRegion,tAgentGeneric,tItem,tObj,tSpatialThing]),member(FinalType,Possibles),!.

deduce_argN(Prop,N,_,ObjectTypes,Type):- suggestedType(Prop,N,ObjectTypes,Type,FinalType),FinalType\=Type,assert_argIsa(Prop,N,FinalType).
deduce_argN(_ ,_ ,Obj,[],Type):- tCol(Type), assert_isa(Obj,Type),!.
deduce_argN(Prop,N,_,[OType|_],_Type):-assert_subclass_on_argIsa(Prop,N,OType),!.

maybe_cache_0(Prop,Obj,Value,_What):- checkNoArgViolation(Prop,Obj,Value), is_asserted(dbase_t(Prop,Obj,Value)),!.
maybe_cache_0(Prop,Obj,Value,What):- padd(Obj,Prop,Value),
  ignore((What\=Obj,
   into_mpred_form(dbase_t(Prop,What,_),Trigger),hooked_asserta(on_change_once(change(retract,_),Trigger,del(dbase_t(Prop,Obj,Value)))))).

:-export(on_change_once/3).
:-export(on_change_always/3).


violatesType(Value,Type):-var(Value),!,Type=var.
violatesType(_,Type):- ttUnverifiableType(Type),!,fail.
% violatesType(_,col):-!,fail.
violatesType(Value,ftInt):-number(Value),!,fail.
violatesType(Value,Type):-atom(Type),isa_backchaing(Value,Type),!,fail.
violatesType(Value,ftString):-string(Value),!,fail.
%violatesType(apath(_,_),Type):-!,(Type\=tPathway,Type\=obj).
violatesType(Value,Type):- compound(Type),!,ttFormatType(Type),not(term_is_ft(Value,Type)),!.
violatesType(Value,Type):- once((isa_backchaing(Value,_))), no_loop_check(not(isa_backchaing(Value,Type))).

%OLD user:decl_database_hook(Type,Changer):- retract(on_change_once(Type,Changer,Fact)),Fact.
%OLD user:decl_database_hook(Type,Changer):- current_predicate(on_change_always/3),forall(on_change_always(Type,Changer,Fact),Fact).

/*
% = falbacks come from...

*/
is_fact_consistent(Fact):-is_asserted(Fact),!.
is_fact_consistent(Fact):-into_mpred_form(Fact,MForm), not(fact_is_false(MForm,_Why)).

%OLD user:decl_database_hook(change(assert,_),Fact):- fact_is_false(Fact,Why),trace_or_throw(fact_is_false(Fact,Why)).

%OLD user:decl_database_hook(change(assert,_),Fact):- ignore((not(dont_check_args(Fact)),agenda_slow_op_enqueue(checkNoArgViolation(Fact)))).


:-export(fallback_value/3).
fallback_value(Prop,Obj,Value):- isa(Obj,ObjType),is_asserted(dbase_t(Prop,isTypeFn(ObjType),Value)),!.
fallback_value(_Prop,Obj,_Value):-var(Obj),!,fail.
fallback_value(Prop,_Obj,_Value):-no_fallback(Prop,2),!,fail.
fallback_value(Prop,Obj,Value):-Fact=..[Prop,Obj,Value], 
   with_assertions(thlocal:infAssertedOnly(Fact),defaultArgValue(Fact,Prop,2,ValueR)),!,
   checkNoArgViolation(Prop,Obj,ValueR),is_fact_consistent(Fact),
   Value=ValueR.

%:-dmsg_hide(defaultArgValue).

no_fallback(genls,2).
no_fallback(P,2):-not(user:mpred_prop(P,prologSingleValued)).

:-export(defaultArgValue/4).
defaultArgValue(Fact,F,A,OLD):- stack_check, user:mpred_prop(F,argSingleValueDefault(A,OLD)),!,dmsg(defaultArgValue(fallback_value(Fact,F,argSingleValueDefault(A,OLD)))).
defaultArgValue(mudFacing(_,_),_,2,vNorth):-!.
defaultArgValue(mudEnergy(_,_),_,2,200):-!.
defaultArgValue(mudHealth(_,_),_,2,500):-!.
defaultArgValue(Fact,F,A,Value):- Fact=..[F,P|Args],is_fact_consistent(Fact),defaultArgValue(Fact,F,A,P,Args,Value).

defaultArgValue(Fact,F,A,P,_Args,Value):-var(P),!,argIsa(F,A,Type),defaultTypeValue(Fact,Type,Value),!,dmsg(defaultArgValue(using_defaultTypeValue1(Fact,Type,Value))).
defaultArgValue(_Call,F,2,P,[Arg],Arg):-create_someval(F,P,Arg),!. 
defaultArgValue(mudShape(Like,V1),mudShape,2,Like,[V1],_):- isa(Like,Type),V1 = isLikeFn(mudShape,Type).
% OLD FALLBACK SYSTEM: defaultArgValue(Fact,F,LastPlus1,I,_Args,Value):- get_instance_default_props(I,PropList),Last is LastPlus1 - 1, functor(Prop,F,Last),member(Prop,PropList),arg(Last,Prop,Value),!,dmsg(defaultArgValue(defaultArgValue_get_type_props(Fact))).

defaultArgValue(Fact,F,A,_P,_Args,Value):-argIsa(F,A,Type),is_fact_consistent(Fact),defaultTypeValue(Fact,Type,Value),!.


defaultTypeValue(Fact,_,_):- thlocal:noRandomValues(Fact),!,fail.
defaultTypeValue(_,Type,_):- thlocal:noRandomValues(Type),!,fail.
defaultTypeValue(_Info,vtDirection,"n").
defaultTypeValue(_Info,ftInt,0).
defaultTypeValue(Fact,Type,Out):- random_instance(Type,ROut,nonvar(ROut)),dmsg(defaultArgValue(random_instance(Fact,Type,ROut=Out))),!,Out=ROut.

% NEW FALLBACK SYSTEM:
add_missing_instance_defaults(P):- hooked_asserta(tNewlyCreated(P)).
/*

:-export(get_instance_default_props/2).

% OLD FALLBACK SYSTEM:
get_instance_default_props(Inst,TraitsO):- must_det(nonvar(Inst)),!,
   findall(Props,((type_w_default_props(Type),isa(Inst,Type),each_default_inst_type_props(Inst,Type,Props))),Traits),flatten_set(Traits,TraitsO),!.

:-export((get_type_default_props/2)).

% OLD FALLBACK SYSTEM:
get_type_default_props(Type,TraitsO):- nonvar(Type),!, Inst = isSelf,
   findall(Props,((type_w_default_props(DefType),transitive_subclass_or_same(Type,DefType),each_default_inst_type_props(Inst,DefType,Props))),Traits),flatten_set(Traits,TraitsO),!.
get_type_default_props(DefType,TraitsO):- type_w_default_props(DefType), Inst = isSelf,
   findall(Props,((each_default_inst_type_props(Inst,DefType,Props))),Traits),flatten_set(Traits,TraitsO),!.



% OLD FALLBACK SYSTEM:
type_w_default_props(Type):-call_tabled(type_w_defaults_asserted(Type)).

% OLD FALLBACK SYSTEM:
type_w_defaults_asserted(Type):- is_asserted(instTypeProps(_,Type,_)),nonvar(Type).
type_w_defaults_asserted(Type):- is_asserted(typeProps(Type,_)),nonvar(Type).
type_w_defaults_asserted(Type):- is_asserted(mudLabelTypeProps(_,Type,_)),nonvar(Type).

*/
/*
% OLD FALLBACK SYSTEM:
each_default_inst_type_props(Inst,Type,Props):-dmsg((warn_each_default_inst_type_props(Inst,Type,Props))),fail.
each_default_inst_type_props(Inst,Type,Props):-call_no_cuts(instTypeProps(Inst,Type,TProps)),subst(TProps,isSelf,Inst,Prop),flatten([Prop],Props).
each_default_inst_type_props(Inst,Type,Props):-call_no_cuts(typeProps(Type,TProps)),subst(TProps,isSelf,Inst,Prop),flatten([Prop],Props).
each_default_inst_type_props(_,Type,[typeHasGlyph(Lbl)|Props]):-call_no_cuts(mudLabelTypeProps(Lbl,Type,Props)).

% OLD FALLBACK SYSTEM:
:-export((add_missing_instance_defaults/1)).
add_missing_instance_defaults(_P):-dontAssertTypeProps,!.
add_missing_instance_defaults(P):-
   loop_check_local(add_missing_instance_defaults_ilc(P),true).
add_missing_instance_defaults_ilc(P):-
   get_inst_default_props(P,_PropListL,Missing),
   once(Missing=[];show_call(padd(P,Missing))).

% OLD FALLBACK SYSTEM:
:-export(gather_props_for/3).
gather_props_for(_Op,Obj,Props):-setof(Prop,(between(1,7,L),length(REST,L),(dbase_t([P,Obj|REST])),Prop=..[P|REST]),Props).
*/

/*
% OLD FALLBACK SYSTEM:
:-export(get_inst_default_props/3).
get_inst_default_props(I,PropListL,Missing):-
   get_instance_default_props(I,PropListL),
   instance_missing_props(I,PropListL,Missing).

% OLD FALLBACK SYSTEM:
:-export(instance_missing_props/3).
instance_missing_props(I,LPS,PS):-
       must_det(once((nonvar(LPS);get_instance_default_props(I,LPS)))),
       findall(P,(member(P,LPS),inst_missing_prop(I,P)),PS),!.

% OLD FALLBACK SYSTEM:
inst_missing_prop(I,P):- P=..[F|Args], MP=..[F,I|Args],inst_missing_prop(I,MP,F).
inst_missing_prop(_,_,F):- user:mpred_prop(F,flag),!,fail.
inst_missing_prop(_,MP,F):- must_det((MP=..[F|Args],get_sv_argnum(F,Args,A),replace_arg(MP,A,BLANK,COLD))), ignore(ireq(COLD)),!,var(BLANK).
*/

get_sv_argnum(F,Args,ArgNum):-once(user:mpred_prop(F,functionalArg(ArgNum));length(Args,ArgNum)).

dontAssertTypeProps:-!.

:-export(agenda_rescan_sim_objects/0).

agenda_rescan_sim_objects:- loop_check_local(agenda_rescan_sim_objects_ilc,true).
% agenda_rescan_sim_objects_ilc:- dmsg(todo(fix(agenda_rescan_sim_objects,"to not set atloc/2"))),!,
agenda_rescan_sim_objects_ilc:-dontAssertTypeProps,!.
agenda_rescan_sim_objects_ilc:-
   once((forall_setof(get_type_default_props(Type,PropList),
    once((dmsg(get_type_default_props(Type,PropList)),
     ignore((fail,forall_setof(isa(I,Type), 
       ignore((
         not(Type == I),
         once(instance_missing_props(I,PropList,Missing)),
          Missing \= [],
          dmsg(agenda_rescan_sim_objects_for(I,Type,missing_from(Missing))),
          padd(I,Missing))))))))))),fail.

agenda_rescan_sim_objects_ilc:-ignore(loop_check_local(rescan_duplicated_facts,true)).


% :- include(logicmoo(parsing/parser_chat80)). 
