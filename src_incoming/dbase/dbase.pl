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

:- op(1120,fx,decl_mpred_prolog).
:- op(1150,fx,decl_mpred_hybrid).


ztrace:-dmsg(ztrace),dtrace.

slow_kb_op(_):-true.

% ========================================
% decl_mpred_hybrid database
% ========================================

:- dynamic_multifile_exported action_info/2.
:- dynamic_multifile_exported action_rules/4.
:- dynamic_multifile_exported agent_call_command/2.
:- dynamic_multifile_exported agent_text_command/4.
:- dynamic_multifile_exported call_after_load/1.
:- dynamic_multifile_exported check_permanence/4.
:- dynamic_multifile_exported decl_mud_test/2.
:- dynamic_multifile_exported default_type_props/2.
:- dynamic_multifile_exported default_inst_type_props/3.
:- dynamic_multifile_exported one_default_type_prop/3.
:- dynamic_multifile_exported label_type/2.
:- dynamic_multifile_exported label_type_props/3.
:- dynamic_multifile_exported mud_test/2.
:- dynamic_multifile_exported now_unused/1.
:- dynamic_multifile_exported term_specifier_text/2.
:- dynamic_multifile_exported type_action_info/3.
:- dynamic_multifile_exported update_charge/2.
:- dynamic_multifile_exported update_stats/2.
:- dynamic_multifile_exported use_usable/4.
:- dynamic_multifile_exported verb_alias/2.
:- dynamic_multifile_exported world_agent_plan/3.
:- dynamic_multifile_exported((actiontype/1,action_info/2,action_rules/4,type_action_info/3,term_specifier_text/2,action_verb_useable/4)).
:- dynamic_multifile_exported((decl_coerce)/3).
:- dynamic_multifile_exported((term_anglify/2,term_anglify_last/2, term_anglify_np/3,term_anglify_np_last/3)).
:- dynamic_multifile_exported((update_charge/2,update_stats/2)).
:- dynamic_multifile_exported(moo:ft_info/2).

% ================================================
% Thread Locals
% ================================================
:- thread_local thlocal:dbase_capture/2.
:- thread_local thlocal:dbase_change/2.
:- thread_local thlocal:dbase_opcall/2.
:- thread_local thlocal:repl_to_string/2.
:- thread_local thlocal:repl_writer/2.
:- thread_local thlocal:session_agent/2.

:- dynamic_multifile_exported thlocal:dbase_capture/2.
:- dynamic_multifile_exported thlocal:session_agent/2.

% ================================================
% DBASE_T System
% ================================================

:-include(dbase_i_kb_store).

% ================================================
% MPRED_PROP System
% ================================================

:-include(dbase_i_mpred_props).
:-include(dbase_i_mpred_stubs).

:- op(1120,fx,decl_mpred_prolog).
:- op(1150,fx,decl_mpred_hybrid).

:- decl_mpred_hybrid last_command/2.
:- decl_mpred_hybrid named/2, spd/2.
:- decl_mpred_hybrid str/2. 
:- decl_mpred_hybrid subclass/2.
:- decl_mpred_hybrid type_grid/3.
:- decl_mpred_hybrid(moo:subft/2).
:- decl_mpred_hybrid(singleValued/1).
:- discontiguous(singleValued/1).

:-include(dbase_i_deduce).

% ================================================
% ISA/GENLS Typesystem
% ================================================

:-dynamic_multifile_exported((loading_game_file/2, loaded_game_file/2)).
:-export(not_loading_game_file/0).
not_loading_game_file:- not(moo:loading_game_file(_,_)),moo:loaded_game_file(_,_),!.

:-include(dbase_i_isa_subclass).

% ================================================
% A tiny bit of TMS
% ================================================

hook:decl_database_hook(assert(_),Fact):- check_was_known_false(Fact).

was_known_false(Fact):-is_known_false(Fact),retractall((is_known_false(_):-true)),dmsg(trace_or_throw(error+was_known_false(Fact))).

check_was_known_false(Fact):-ignore(((is_known_false(Fact),was_known_false(Fact)))).

hook:decl_database_hook(assert(_A_or_Z),label_type_props(Lbl,T,Props)):- add_w_hooks(default_type_props(T,[label(Lbl)|Props])).

hook:decl_database_hook(assert(_A_or_Z),default_type_props(T,_)):- define_type_if_atom(T).

hook:decl_database_hook(assert(_A_or_Z),mpred_prop(F,arity(A))):- ignore((A==1,define_type(F))) , ignore((atom(P),define_type(P))).


% ========================================
% enter_term_anglify(MASK)
% ========================================

enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).
enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify_np(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).
enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify_last(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).
enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify_np_last(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).



:- dynamic_multifile_exported(decl_coerce/3).


decl_coerce(_,_,_):-fail.

:-dynamic_multifile_exported(coerce/3).

coerce(What,Type,NewThing):-decl_coerce(What,Type,NewThing),!.
coerce(What,_Type,NewThing):-NewThing = What.




:- meta_predicate tick_every(*,*,0).
:- meta_predicate register_timer_thread(*,*,0).

:- decl_mpred_hybrid((nonCreatableType/1, type/1, subclass/2, argsIsa/1 ,creatableType/1, createableSubclassType/2)).

/*
:- decl_mpred_hybrid((createableSubclassType(type,type))).
:- decl_mpred_hybrid((creatableType(type))).
:- decl_mpred_hybrid type_grid/3.
*/
:- decl_mpred_hybrid(((formatted/1,
                       contains/2))).



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
   areq/1,
  del/1,  
  padd/2, padd/3, prop/3, prop_or/4, props/2,aprops/2, req/1, term_listing/1, 
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

%:- meta_predicate man:with_assertions(:,0).
%:- meta_predicate world:intersect(?,0,?,0,0,-).

%% :- meta_predicate del(0),clr(0),add(0),add0(0),req(0), db_op(0,0,0).

% Found new meta-predicates in iteration 1 (0.281 sec)
%:- meta_predicate db_forall(?,?,?,0).

:- include(logicmoo('vworld/moo_header.pl')).

% :- include('dbase_types_motel').

% :- user_use_module(dbase_rules_pttp).

:- register_module_type(utility).

moo:actiontype(list(term)).
moo:agent_call_command(_Gent,list(Obj)):- term_listing(Obj).


:-declare_dbase_local_dynamic(atloc,2).

:-include(dbase_i_pldoc).
:-include(dbase_i_coroutining).
%:-discontiguous(singleValued/2).
%:-discontiguous(multiValued/1).
% =================================================================================================
% world database
% =================================================================================================


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


% replaced the 1st with the 2nd and better version of retract
% del(C0):- db_op_int(retract,C0)
%% del(RetractOne)    <--  del(C0):- ignore((db_op(query(HLDS,Must),C0),!,db_op('retract',C0))).
del(C0):- db_op_int('retract',C0).
del(C0):- dmsg(failed(del(C0))),!,fail.
%% clr(Retractall)
clr(C0):- db_op_int(ra,C0).
%% req(Query)
req(C0):- db_op_int(query(dbase_t,req),C0).
areq(C0):- with_assertions(assertedOnly(_),db_op_int(query(dbase_t,assertedOnly),C0)).
mreq(C0):- must(req(C0)).
%% props(Obj,QueryPropSpecs)
props(Obj,PropSpecs):- req(props(Obj,PropSpecs)).
aprops(Obj,PropSpecs):- areq(props(Obj,PropSpecs)).
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

record_on_thread(Dbase_change,O):- thread_self(ID),thlocal:dbase_capture(ID,Dbase_change),!,Z=..[Dbase_change,ID,O],assertz(Z).

holds_args([H|LIST],LISTO):- !, is_holds_true(H),!,LIST=LISTO.
holds_args(HOLDS,LIST):- compound(HOLDS),HOLDS=..[H|LIST],is_holds_true(H),!.

% ================================================
% db_op/2
% ================================================

% db_op(query(HLDS,Must),creatableType(SubType)):- !, call_expanded_for(Must,is_creatable_type(SubType)).
db_op(tell(_),props(_Obj,Props)):- Props ==[], !.

db_op(query(Dbase_t, Req), must(Call)):-!,must(db_op(query(Dbase_t, Req), Call)).

db_op(a,Wild):-!,db_op(tell(assertion),Wild).
db_op(query(_HLDS,Must),isa(I,Type)):- !,call_expanded_for(Must,get_isa_backchaing(I,Type)).
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
db_op0(Op,Term):- stack_check(1000),not(compound(Term)),!,trace_or_throw(nc(db_op0(Op,Term))).
db_op0(Op,props(Obj,nameStrings(Str))):-!, db_op0(Op,nameStrings(Obj,Str)).

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


db_op0(query(HLDS,Must),props(Obj,Props)):- var(Props),!,findall(Prop,(call_expanded_for(query(HLDS,Must),dbase_t([P,Obj|REST])),Prop=..[P|REST]),Props).
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

db_op0(query(HLDS,Must),isa(Term,Var)):- !,call_expanded_for(query(HLDS,Must),hotrace(get_isa_backchaing(Term,Var))).
db_op0(Op,isa(A,SubType)):- dbase_t(createableSubclassType,SubType,Type),!,db_op(Op,isa(A,Type)),db_op(Op,isa(A,SubType)).

%db_op0(tell(_OldV),singleValued(Term)):- !,decl_mpred(Term),decl_mpred(Term,singleValued).
%db_op0(tell(_OldV),multiValued(Term)):- !,functor_safe(Term,_,A),decl_mpred(Term),decl_mpred(Term,[multiValued,multi(A)]).
db_op0(query(HLDS,Must),argIsa(P,N,T)):- call_expanded_for(query(HLDS,Must),(get_mpred_prop(P,argsIsa(ArgsIsa)),arg(N,ArgsIsa,T),must(nonvar(T)))).


db_op0(Op,A):- hotrace(must(once(correctArgsIsa(Op,A,AA)))), not(A=@=AA), !, db_op(Op,AA).

db_op0(tell(_),Term):- glean_pred_props_maybe(Term),fail.

% db_op0(Op,Wild):- dsfdf db_op_simpler_wlc(Op,Wild,Simpler),!,db_op(Op,Simpler).

db_op0(Op,Term):- type_error_checking,!, Term =..[Type,A],!,db_op(Op,isa(A,Type)).

db_op0(Op,C0):- C0=..[Prop|ARGS],db_op_unit(Op,C0,Prop,ARGS).


% ================================================
% db_op_unit/3
% ================================================

db_op_unit(query(HLDS,Must),_C0,Prop,ARGS):- get_mpred_prop(Prop,extentKnown),!,call_expanded_for(query(HLDS,Must),dbase_t_p2(Prop,ARGS)).

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
db_op_unit(query(Must,HLDS),C0,Prop,_RGS):- get_mpred_prop(Prop,query_with_pred(How)),!, call_expanded_for(query(Must,HLDS),call(How,C0)).

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
% db_assert_[mv|sv]/3
% ================================================

:-dmsg_hide(db_assert_mv).
:-dmsg_hide(db_assert_sv).
:-dmsg_hide(db_op_exact).

% assert_with to tell(OldV) mutlivalue pred
:-export((db_assert_mv/4)).
db_assert_mv(_Must,end_of_file,_,_):-!.
% db_assert_mv(_Must,C,_F,_A):- hooked_assertz(C),!.
db_assert_mv(Must,C,F,A):- dmsg(db_assert_mv(Must,C,F,A)), must_det(mpred_prop(F,ordered) -> hooked_assertz(C) ; hooked_asserta(C)).


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
   replace_arg(C,A,_,CBLANK),hooked_retractall(CBLANK),
   must_det(hooked_asserta_confirmed(CNEW,A,NEW)),!.

qreq(COLD):-functor(COLD,F,A),functor(COLD_AT_ALL,F,A),
   with_assertions(noRandomValues(COLD), 
     with_assertions(noDefaultValues(COLD),
                   (no_loop_check(  %listing(bugger:inside_loop_check/1),
                     call_expanded([whatnot(COLD)],COLD,F,A)))) ),!.

qreq2(COLD):-qreq(COLD),!.
qreq2(COLD):-functor(COLD,F,A),functor(COLD_AT_ALL,F,A),
   listing(bugger:inside_loop_check/1),
   retractall(bugger:inside_loop_check(_)),
   dtrace,qreq(COLD).

:-style_check(+singleton).

confirm_hook(CNEW:NEW=@=CNOW:NOW):-
   var(NOW),               
   qreq(CNOW),
   logOnFailure(CNEW:NEW=@=CNOW:NOW),!.

% Expect CNEW to be what is found
hooked_asserta_confirmed(CNEW,A,NEW):-
   replace_arg(CNEW,A,NOW,CNOW),
   must(ground(CNEW)),
   hooked_asserta(CNEW),
   confirm_hook(CNEW:NEW=@=CNOW:NOW),!.

hooked_asserta_confirmed(CNEW,A,NEW):-
   replace_arg(CNEW,A,NOW,CNOW),
   dtrace,
   logOnFailure((req(CNOW),CNEW:NEW=@=CNOW:NOW)),!.
hooked_asserta_confirmed(CNEW,A,NEW):-dmsg(unconfirmed(hooked_asserta_confirmed(CNEW,A,NEW))).

:-include(dbase_i_propvals).
:-include(dbase_i_call).

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


:-export(forall_setof/2).
forall_setof(ForEach,Call):-
   findall(ForEach,ForEach,ForEachAll),
   list_to_set(ForEachAll,Set),!,
   ignore(forall(member(ForEach,Set),Call)).

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
game_assert(A):- into_mpred_form(A,F),not(A=@=F),!,game_assert(F).
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
      flatten(Props,AllProps), !, assertOnLoad(default_type_props(Type,AllProps)).
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

agent_text_command(_Agent,_Text,_AgentTarget,_Cmd):-fail.
/*
    coerce/3,
    rescan_dbase_t_once/0,
    rescan_dbase_t/0,
          enter_term_anglify/2,
           run_database_hooks/2,
           agent_text_command/4,         
*/
:- rescan_mpred_props.


