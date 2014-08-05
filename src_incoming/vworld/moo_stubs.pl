





call_body_req(HEAD):- functor(HEAD,F,A),HEAD_T=..[F|ARGS],HEAD_T=..[dbase_t,F|ARGS],hook_body_req(F,A,HEAD,HEAD_T).


ztrace:-throw(dtrace).

% =====================================
% = body_req
% =====================================
/*
:-export(hook:body_req/4).
not_dupe(HEAD):-must_det(last_arg_ground(HEAD)),predicate_property(HEAD,number_of_clauses(N)),N>1,not(clause_safe(HEAD,true)).
*/
:-export(last_arg_ground/1).
last_arg_ground(HEAD):-compound(HEAD),functor(HEAD,F,A),last_arg_ground(F,A,HEAD),!.
last_arg_ground(mud_test,_,_).
last_arg_ground(_,A,_):-A>2,!.
last_arg_ground(_,A,HEAD):-arg(A,HEAD,Arg),!,ground(Arg).

:- dynamic_multifile_exported(transitive_other/3).

choose_val(Prop,Obj,Value):- choose_right(Prop,Obj,Value).

generate_candidate_arg_values(Prop,N,Obj):-call_vars_tabled(Obj,generate_candidate_arg_values0(Prop,N,Obj)).

generate_candidate_arg_values0(Prop,N,R):- cached_isa(Prop,extentKnown),arg(N,vv(Obj,Value),R),!,is_asserted(dbase_t(Prop,Obj,Value)).
generate_candidate_arg_values0(Prop,N,Obj):- once((argIsa_asserted(Prop,N,Type),type_has_instances(Type))),!,cached_isa(Obj,Type).
generate_candidate_arg_values0(Prop,1,R):- arg(N,vv(Obj,Value),R),!,is_asserted(dbase_t(Prop,Obj,Value)).

type_has_instances(Type):-  atom(Type),Type\=term,Type\=type,not_ft(Type),isa(_,Type),!.

choose_right(Prop,Obj,Value):- var(Obj),cached_isa(Prop,extentKnown),not(cached_isa(Prop,singleValued)),!,is_asserted(dbase_t(Prop,Obj,Value)).
choose_right(Prop,Obj,Value):- var(Obj),findall(Obj,generate_candidate_arg_values(Prop,1,Obj),Objs),Objs\=[],!,member(Obj,Objs),nonvar(Obj),choose_for(Prop,Obj,Value).
choose_right(Prop,Obj,Value):- var(Obj),dmsg(choose_right(Prop,Obj,Value)),!,is_asserted(dbase_t(Prop,Obj,Value)).
choose_right(Prop,Obj,Value):- choose_for(Prop,Obj,Value).

:-export(choose_for/3).

choose_for(Prop,Obj,Value):-var(Value),!,choose_current(Prop,Obj,Value),maybe_cache(Prop,Obj,Value,Obj).
choose_for(Prop,Obj,Value):-choose_current(Prop,Obj,RValue),Value=RValue,maybe_cache(Prop,Obj,Value,Obj).

choose_current(Prop,Obj,Value):- mpred_prop(Prop,singleValued),!,choose_one(Prop,Obj,Value),!.
choose_current(Prop,Obj,Value):- nonvar(Value),!,choose_each(Prop,Obj,Value),!.
choose_current(Prop,Obj,Value):- findall(choose_each(Prop,Obj,Value),choose_each(Prop,Obj,Value),Results),Results \== [],!,
   member(choose_each(Prop,Obj,Value),Results).

choose_one(Prop,Obj,Value):- choose_asserted(Prop,Obj,RValue),!,Value=RValue.
choose_one(Prop,Obj,    _):- Call=.. [Prop,Obj,_],noDefaultValues(Call),!,fail.
choose_one(Prop,Obj,Value):- fallback_value(Prop,Obj,DValue),!,Value = DValue,padd(Obj,Prop,Value).
choose_one(Prop,Obj,Value):- var(Value),create_someval(Prop,Obj,Value),padd(Obj,Prop,Value).

choose_each(Prop,Obj,Value):- mpred_prop(Prop, extentKnown),!,choose_asserted(Prop,Obj,Value).
choose_each(Prop,Obj,Value):- one_must(choose_asserted(Prop,Obj,Value),(fallback_value(Prop,Obj,Value),maybe_cache(Prop,Obj,Value,Obj))).

choose_asserted(Prop,Obj,Value):- dbase_t(Prop,Obj,Value). % ,must_det(is_asserted(dbase_t(Prop,Obj,Value))).
choose_asserted(Prop,Obj,Value):- is_asserted(dbase_t(Prop,Obj,Value)).
choose_asserted(Prop,Obj,Value):- transitive_other(Prop,Obj,What),call(Prop,What,Value),maybe_cache(Prop,Obj,Value,What).
      
maybe_cache(Prop,Obj,Value,_What):-is_asserted(dbase_t(Prop,Obj,Value)),!.
maybe_cache(Prop,Obj,Value,What):-padd(Obj,Prop,Value),into_mpred_form(dbase_t(Prop,What,_),Trigger),hooked_asserta(on_change_once(retract(_),Trigger,del(dbase_t(Prop,Obj,Value)))).

:-dynamic_multifile_exported(on_change_once/3).
:-dynamic_multifile_exported(on_change_always/3).

hook:decl_database_hook(Type,Changer):- retract(on_change_once(Type,Changer,Call)),Call.
hook:decl_database_hook(Type,Changer):- forall(on_change_always(Type,Changer,Call),Call).

% ========================================================================================
% BODY STUB
% ========================================================================================

body_req_isa(I,C):- loop_check(body_req_isa_lc(I,C),dbase_t(C,I)).

body_req_isa_lc(I,C):-catch(get_isa_backchaing(I,C),_,alt_dbase_t(C,I)).

alt_dbase_t(C,I):-clause(dbase_t(C,I),true).
alt_dbase_t(C,I):-clause(isa(I,C),true).
alt_dbase_t(C,I):-atom(C),alt_dbase_atom(C,I).

alt_dbase_atom(C,I):-clause(mpred_prop(I,C),true).
alt_dbase_atom(formattype,I):- clause(ft_info(I,_),true).
alt_dbase_atom(formattype,I):- clause(subft(I,_),true).
alt_dbase_atom(C,I):- G=..[C,I],predicate_property(G,number_of_clauses(_)),!,clause(G,true).

hook:body_req(F,A,HEAD,HEAD_T):-hook_body_req(F,A,HEAD,HEAD_T).

hook_body_req(F,_A,_HEAD,_HEAD_T):- mpred_prop(F,prologOnly),!,fail.
hook_body_req(_,_,isa(I,C),_):- !, body_req_isa(I,C).
hook_body_req(_,_,_,dbase_t(C,I)):- !, body_req_isa(I,C).
% % % hook_body_req(F,A,HEAD,HEAD_T):- predicate_property(HEAD,number_of_clauses(1)),!,body_req_1(F,A,HEAD,HEAD_T).
hook_body_req(F,A,HEAD,HEAD_T):- fail, ignore((clause(HEAD,(hook:body_req(F,A,HEAD,HEAD_T)), Ref),nth_clause(_, Nth, Ref), Nth > 1, 
      asserta(':-'(HEAD,(hook:body_req(F,A,HEAD,HEAD_T)))))),fail. % ,erase(Ref))),fail.


hook_body_req(_,_,_,dbase_t(Prop,Obj,LValue)):-!,choose_val(Prop,Obj,LValue).

hook_body_req(F,A,HEAD,HEAD_T):- body_req_0(F,A,HEAD,HEAD_T).

:-export(body_req_0/4).
body_req_0(F,A,HEAD,HEAD_T):- not(ground(HEAD)),!,body_req_1(F,A,HEAD,HEAD_T).
body_req_0(F,A,HEAD,HEAD_T):- body_req_3(F,A,HEAD,HEAD_T),!. % ,ignore((not(last_arg_ground(HEAD)),rtrace(body_req_0(F,A,HEAD,HEAD_T)))),not_dupe(HEAD).

:-export(body_req_1/4).
body_req_1(F,A,HEAD,HEAD_T):- mpred_prop(F,call_tabled),!, call_tabled(body_req_2(F,A,HEAD,HEAD_T)).
body_req_1(F,A,HEAD,HEAD_T):- body_req_2(F,A,HEAD,HEAD_T).

body_req_2(F,A,HEAD,HEAD_T):- body_req_3(F,A,HEAD,HEAD_T).

body_req_3(_,_,_    ,HEAD_T):- clause(HEAD_T,true).
body_req_3(F,_,HEAD,  _):- mpred_prop(F,external(Module)),!,call(Module:HEAD).
body_req_3(_,_,HEAD,_HEAD_T):- clause(HEAD,true).
% body_req_3(_,_,HEAD,  _):- predicate_property(HEAD,number_of_clauses(N)),N>100,!,fail. % body_req is possibly unneeded now
body_req_3(_F,_,HEAD,  _):- not(save_in_dbase_t),req(HEAD).








foo_b(b1).
foo_b(b2):-!.
foo_b(b3):-!.

:-must_det((findall(R,call_no_cuts(foo_b(R)),List),length(List,3))).
% ========================================================================================
% DEDUCE FACTS
% ========================================================================================

hook:decl_database_hook(Type,Fact):- predicate_property(add_deduction(_,_),_),run_deduce_facts_from(Type,Fact).

run_deduce_facts_from(Type,M:Fact):-atom(M),!,run_deduce_facts_from(Type,Fact).
run_deduce_facts_from(Type,Fact):-loop_check(run_deduce_facts_from_lc(Type,Fact),true).
run_deduce_facts_from_lc(Type,Fact):-doall((call_no_cuts(hook:deduce_facts(Fact,Deduction)),add_deduction(Type,Deduction,Fact))).


hook:decl_database_hook(assert(_),atloc(R,W)):- isa(R,region),trace_or_throw(atloc(R,W)).


hook:deduce_facts(atloc(Obj,LOC),inRegion(Obj,Region)):-locationToRegion(LOC,Region).
hook:deduce_facts(inRegion(_,Region),isa(Region,region)).
hook:deduce_facts(inRegion(Obj,_),isa(Obj,obj)).
hook:deduce_facts(inRegion(Obj,_),atloc(Obj,LOC)):- put_in_world(Obj),must_det(atloc(Obj,LOC)).

hook:deduce_facts(Fact,mpred_prop(AF,[argsIsa(ArgTs)|PROPS])):-compound(Fact),Fact=..[F,ArgTs|PROPS],argsIsaProps(F),compound(ArgTs),functor(ArgTs,AF,N),N>0,
                ArgTs=..[AF|ARGS],!,must_det(ground(ARGS)).

hook:deduce_facts(mpred_prop(F,argsIsa(ArgTs)),argsIsa(ArgTs)):-functor(F,ArgTs,_).

hook:deduce_facts(argsIsa(ArgTs),argIsa(F,A,Type)):-ztrace,functor(ArgTs,F,_),arg(A,ArgTs,Type).
hook:deduce_facts(mpred_prop(F,argsIsa(ArgTs)),argIsa(F,A,Type)):-arg(A,ArgTs,Type).

hook:deduce_facts(argIsa(F,_A,Type),[isa(Type,type),isa(F,relation)]):-atom(Type),not(dbase_t(Type,formattype)).

%hook:deduce_facts(B,A):- is_asserted(equivRule(B,A)),not(contains_singletons(A)).
%hook:deduce_facts(B,A):- is_asserted(equivRule(A,B)),not(contains_singletons(A)).
hook:deduce_facts(Term,NewTerm):- hotrace(good_for_chaining(Op,Term)), db_rewrite(Op,Term,NewTerm),not(contains_singletons(NewTerm)).


fix_argIsa(F,N,dir(Val),dir):-add(mpred_prop(F,default_sv(N,Val))),!.
fix_argIsa(_,_,list(Type),list(Type)):-!.
fix_argIsa(F,N,Type,F):-compound(Type),Type=..[F,Val],get_isa_backchaing(Val,F),add(mpred_prop(F,default_sv(N,Val))),!.
fix_argIsa(_,_,Arg,Arg).

fix_argsIsas(_,_,[],[]):-!.
fix_argsIsas(F,N,[Arg|TList],[G|List]):-
   fix_argIsa(F,N,Arg,G),
   N1 is N + 1,
   fix_argsIsas(F,N1,TList,List).

fix_argsIsas(F,N,ArgTList,GList):-
hook:decl_database_hook(assert(_),argsIsa(ArgTs)):-
   ArgTs=..[F|ArgTList],
   fix_argsIsas(F,1,ArgTList,GList),
   Good=..[F|GList],
   Good\=ArgTs,!,del(mpred_prop(F,argsIsa(ArgTs))),decl_mpred(F,argsIsa(Good)).

:-rescan_mpred_props.


