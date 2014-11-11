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
:- dynamic_multifile_exported hook:fact_is_false/2.
:- dynamic_multifile_exported hook:kbp_t_list_prehook/2.

:- op(1120,fx,decl_mpred_prolog).
:- op(1150,fx,decl_mpred_hybrid).

:- include(logicmoo('vworld/moo_header.pl')).


ztrace:-dmsg(ztrace),trace_or_throw(dtrace).

:-meta_predicate_transparent(when_debugging(+,0)).
when_debugging(What,Call):-debugging(What),!,Call.
when_debugging(_,_).

:-export(listprolog/0).
listprolog:-listing(mpred_prop(_,prologOnly)).


shrink_clause( (H:-true),H):-!.
shrink_clause( HB,HB).

% ================================================
% Thread Locals
% ================================================

:- decl_thlocal adding_from_srcfile/0.
:- decl_thlocal agent_current_action/2.
:- decl_thlocal dbase_capture/2.
:- decl_thlocal thlocal:caller_module/2.
:- decl_thlocal dbase_change/2.
:- decl_thlocal dbase_opcall/2.
:- decl_thlocal deduceArgTypes/1.
:- decl_thlocal do_slow_kb_op_now/0.
:- decl_thlocal enable_src_loop_checking/0.
:- decl_thlocal in_prolog_source_code/0.
:- decl_thlocal insideIREQ/1.
:- decl_thlocal into_form_code/0.
:- decl_thlocal no_arg_type_error_checking/0.
:- decl_thlocal noDefaultValues/1.
:- decl_thlocal noRandomValues/1.
:- decl_thlocal repl_to_string/2.
:- decl_thlocal repl_writer/2.
:- decl_thlocal session_agent/2.
:- decl_thlocal thlocal:useOnlyExternalDBs/0.
:- decl_thlocal with_callMPred/1.
:- decl_thlocal skip_db_op_hooks/0.
:- decl_thlocal thlocal:in_dynamic_reader/1.
:- decl_thlocal thlocal:tracing80/0.
:- decl_thlocal thlocal:usePlTalk/0.
:- decl_thlocal thlocal:useAltPOS/0.

:- dynamic_multifile_exported(thlocal:tracing80/0).
:- dynamic_multifile_exported(thlocal:usePlTalk/0).

:- dynamic_multifile_exported hybrid_rule/2.

:- dynamic_multifile_exported loaded_external_kbs/0.


% ========================================
% decl_mpred_hybrid database
% ========================================

:- dynamic_multifile_exported action_info/2.
:- dynamic_multifile_exported action_rules/4.
:- dynamic_multifile_exported agent_call_command/2.
:- dynamic_multifile_exported agent_text_command/4.
:- dynamic_multifile_exported check_permanence/4.
:- dynamic_multifile_exported decl_mud_test/2.
:- dynamic_multifile_exported default_type_props/2.
:- dynamic_multifile_exported default_inst_props/3.
:- dynamic_multifile_exported label_type/2.
:- dynamic_multifile_exported label_type_props/3.
:- dynamic_multifile_exported mud_test/2.
:- dynamic_multifile_exported mud_test/1.
:- dynamic_multifile_exported mud_test/0.
:- dynamic_multifile_exported mud_test_local/2.
:- dynamic_multifile_exported mud_test_local/1.
:- dynamic_multifile_exported mud_test_local/0.
:- dynamic_multifile_exported now_unused/1.
:- dynamic_multifile_exported term_specifier_text/2.
:- dynamic_multifile_exported type_action_info/3.
:- dynamic_multifile_exported update_charge/2.
:- dynamic_multifile_exported update_stats/2.
:- dynamic_multifile_exported use_usable/4.
:- dynamic_multifile_exported movedist/2.
:- dynamic_multifile_exported verb_alias/2.
:- dynamic_multifile_exported world_agent_plan/3.
:- dynamic_multifile_exported contains/2.
:- dynamic_multifile_exported longitude/2.
:- dynamic_multifile_exported latitude/2.
:- dynamic_multifile_exported region/1.
:- dynamic_multifile_exported((actiontype/1,action_info/2,action_rules/4,type_action_info/3,term_specifier_text/2,action_verb_useable/4)).
:- dynamic_multifile_exported((decl_coerce)/3).
:- dynamic_multifile_exported((term_anglify/2,term_anglify_last/2, term_anglify_np/3,term_anglify_np_last/3)).
:- dynamic_multifile_exported((update_charge/2,update_stats/2)).
:- dynamic_multifile_exported(ft_info/2).

:- dynamic_multifile_exported thglobal:use_cyc_database/0.

% TODO uncomment the next line without breaking it all!
% thglobal:use_cyc_database.

% ================================================
% DBASE_T System
% ================================================

:-ensure_loaded(dbase_i_kb_store).

% ================================================
% MPRED_PROP System
% ================================================

:-ensure_loaded(dbase_i_mpred_props).
:-ensure_loaded(dbase_i_mpred_stubs).

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

:-ensure_loaded(dbase_i_deduce).

% :-ensure_loaded(dbase_ext_was).

:-ensure_loaded(dbase_i_isa_subclass).

% ================================================
% A tiny bit of TMS
% ================================================

hook:decl_database_hook(assert(_),Fact):- check_was_known_false(Fact).

was_known_false(Fact):-is_known_false(Fact),retractall((is_known_false(_):-true)),dmsg(trace_or_throw(error+was_known_false(Fact))).

check_was_known_false(Fact):- ignore(((is_known_false(Fact),was_known_false(Fact)))).

hook:decl_database_hook(assert(_A_or_Z),label_type_props(Lbl,T,Props)):- hooked_asserta(default_type_props(T,[kwLabel(Lbl)|Props])).

hook:decl_database_hook(assert(_A_or_Z),default_type_props(T,_)):- decl_type_safe(T).

hook:decl_database_hook(assert(_A_or_Z),mpred_prop(F,_)):- must_det(atom(F)).


% ================================================
% Agenda system - source file loading
% ================================================

:-dynamic_multifile_exported((thglobal:loading_game_file/2, thglobal:loaded_game_file/2)).

moo:after_game_load:- not(thglobal:loading_game_file(_,_)),thglobal:loaded_game_file(_,_),!.

% when all previous tasks have completed
moo:after_game_load_pass2:- not(thglobal:will_call_after(moo:after_game_load,_)).
:- meta_predicate(call_after_game_load(0)).
% call_after_game_load(Code):- moo:after_game_load,!, call_after_next(moo:after_game_load_pass2,Code).
call_after_game_load(Code):- call_after_next(moo:after_game_load,Code).

:-export(rescan_game_loaded/0).
rescan_game_loaded:- ignore((moo:after_game_load, loop_check(call_after(moo:after_game_load, true ),true))).

:-export(rescan_game_loaded/0).
rescan_game_loaded_pass2:- ignore((moo:after_game_load, loop_check(call_after(moo:after_game_load_pass2,  dmsg(rescan_game_loaded_pass2_comlpete)),true))).

% ================================================
% Agenda system - standard database
% ================================================

% do_db_op_hooks:-!.
do_db_op_hooks:- loop_check_local(rescan_dbase_ops,true).

:-export(rescan_slow_kb_ops/0).

rescan_slow_kb_ops:-!.
rescan_slow_kb_ops:- loop_check(forall(retract(do_slow_kb_op_later(Slow)),must_det(Slow)),true).

:-export(rescan_dbase_ops/0).
rescan_dbase_ops:- test_tl(skip_db_op_hooks),!.
rescan_dbase_ops:- rescan_module_ready.

:-thread_local thlocal:in_rescan_module_ready/0.
rescan_module_ready:- thlocal:in_rescan_module_ready,!.
rescan_module_ready:- with_assertions(thlocal:in_rescan_module_ready,loop_check_local(do_all_of(dbase_module_ready),true)).

:- dynamic_multifile_exported do_slow_kb_op_later/1.
slow_kb_op(Slow):- test_tl(do_slow_kb_op_now),!,debugOnError(Slow).
slow_kb_op(Slow):- asserta_if_new(do_slow_kb_op_later(Slow)),!.


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

:- decl_mpred_hybrid((nonCreatableType/1, type/1, subclass/2, argsIsaInList/1 ,createableType/1, createableSubclassType/2)).

/*
:- decl_mpred_hybrid((createableSubclassType(type,type))).
:- decl_mpred_hybrid((createableType(type))).
:- decl_mpred_hybrid type_grid/3.
*/
:- decl_mpred_hybrid(((formatted/1,
                       contains/2))).



:-op(1150,fx,export).

:- '@'(use_module(logicmoo(vworld/moo)),'user').
% :- '@'(use_module(dbase_formattypes),'user').

:-ensure_loaded(dbase_formattypes).

%:- trace, (dynamic_multifile_exported  moo:obj/1). 
%:- trace, (dynamic_multifile_exported  obj/1). 


:- export(( 
   (add)/1, 
   clr/1,
   db_op/2,
   ireq/1,
  del/1,  
  padd/2, padd/3, prop/3, prop_or/4, props/2, iprops/2, upprop/2,add_fast/1, ireq/1, mreq/1, upprop/1, req/1, term_listing/1, 
  use_term_listing/2,  world_clear/1,  
   with_kb_assertions/2
  )).


:- dynamic_multifile_exported 
   agent/1, agent_doing/2, agent_done/2, charge/2,damage/2, atloc/2, failure/2, grid/4, isa/2, item/1, 
  memory/2,  pathName/3, possess/2,  region/1, score/2, stm/2,   facing/2,
   % type/1,
   localityOfObject/2,
   
  thinking/1,   wearing/2, 
  %str/2,
  facing/2, height/2, act_term/2, nameStrings/2, description/2, pathBetween/3, agent_turnnum/2.

:- must(dbase_mod(moo)).

:- dbase_mod(M),dynamic_multifile_exported((
          % M:dbase_t/1,
          M:dbase_t/2,
          M:dbase_t/3,
          M:dbase_t/4,
          M:dbase_t/5,
          M:dbase_t/6,
          M:dbase_t/7,
          M:dbase_t/8,
          M:dbase_t/9,
          M:dbase_t/10,
          M:dbase_t/11)).

:- dbase_mod(M),dynamic_multifile_exported((
          % M:holds_t/1,
          M:holds_t/2,
          M:holds_t/3,
          M:holds_t/4,
          M:holds_t/5,
          M:holds_t/6,
          M:holds_t/7,
          M:holds_t/8,
          M:holds_t/9,
          M:holds_t/10,
          M:holds_t/11)).

% :-dynamic((weight/2)).
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
      cmdfailure/2,
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
      wearsClothing/2)).

logical_functor(X):-atom(X),member(X,[',',';']).

:- dynamic stat_total/2.
:- decl_mpred_prolog(stat_total/2).

:- decl_mpred_hybrid((
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

:-multifile localityOfObject/2.
/*

:- context_module(M),
   asserta(dbase_mod(M)),
   dmsg(assert_if_new(dbase_mod(M))).

*/

%:- multifile argsIsaInList/2.

%:- meta_predicate man:with_assertions(:,0).
%:- meta_predicate world:intersect(?,0,?,0,0,-).

% -  :- meta_predicate del(0),clr(0),add(0),add0(0),req(0), db_op(0,0,0).

% Found new meta-predicates in iteration 1 (0.281 sec)
%:- meta_predicate db_forall(?,?,?,0).


% :- ensure_loaded('dbase_types_motel').

% :- user_use_module(dbase_rules_pttp).

:- register_module_type(utility).

moo:actiontype(list(term)).
moo:agent_call_command(_Gent,list(Obj)):- term_listing(Obj).


:-declare_dbase_local_dynamic(atloc,2).

:-ensure_loaded(dbase_i_pldoc).
:-ensure_loaded(dbase_i_coroutining).
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
pred_as_is(F,_):-get_mpred_prop(F,as_is(_Why)),!.
pred_as_is(F,_):-get_mpred_prop(F,external(_)),!.
pred_as_is(p,_):-!,fail.
pred_as_is(dbase_t,_):-!,fail.
pred_as_is(k,_):-!,fail.

xtreme_debug(P):- is_release,!,nop(P).
xtreme_debug(P):- not_is_release, verify_sanity(P).
verify_sanity(P):- is_release,!,nop(P).
verify_sanity(P):- debugOnError(P),!.
verify_sanity(P):- dmsg('$ERROR_incomplete_SANITY'(P)),!.


:- meta_predicate hooked_asserta(^), hooked_assertz(^), hooked_retract(^), hooked_retractall(^).

:- meta_predicate del(-),clr(-),add(-),req(-), db_op(-,-).

% Found new meta-predicates in iteration 1 (0.281 sec)
%:- meta_predicate db_op_exact(?,?,?,0).

:- decl_mpred_prolog(movedist/2).
:- export(movedist/2).
:- dynamic(movedist/2).
:- decl_mpred(movedist/2,[argsIsaInList(movedist(agent,int)),singleValued,ask_module(move),query(call),default_sv(2,1)]).

% movedist(X,Y):-callStub_moo(holds_t,movedist(X,Y)).


% :- moo:register_module_type(utility).
% ================================================
% db_reop/2  RE-CURSED and CHECKED 
% ================================================
db_reop(query(_HLDS,Must),Call) :- !,preq(Must,Call).
db_reop(OP,DATA):-no_loop_check(db_reop0(OP,DATA)).
db_reop0(change(assert,_),Call) :- !,add_fast(Call).
db_reop0(change(retract,one),Call) :- !,del(Call).
db_reop0(change(retract,all),Call) :- !,clr(Call).
db_reop0(OP,Call) :- dmsg(warn(db_reop(OP,Call))),db_op0(OP,Call).


% -  del(RetractOne) 
del(C0):- ireq(C0),!,idel(C0),!.
del(C0):- mreq(C0),!,mdel(C0),!.

idel(C0):- db_op_int(change(retract,one),C0), verify_sanity(ireq(C0)->(dmsg(warn(incomplete_I_DEL(C0))),fail);true),!.
idel(C0):- dmsg(warn(failed(idel(C0)))),!,fail.

mdel(C0):- db_op_int(change(retract,one),C0), verify_sanity(mreq(C0)->(dmsg(warn(incomplete_M_DEL(C0))),fail);true),!.
mdel(C0):- dmsg(warn(failed(mdel(C0)))),!,fail.

% -  clr(Retractall)
clr(C0):- db_op_int(change(retract,all),C0),verify_sanity(ireq(C0)->(dmsg(warn(incomplete_CLR(C0))));true).

% -  preq(Query) = query with P note
preq(P,C0):- db_op_int(query(dbase_t,P),C0).

% -  req(Query) = Normal query
req(C0):- preq(req,C0).

% -  mreq(Query) = Forced Full query
mreq(C0):- rescan_module_ready,no_loop_check(with_assertions([-insideIREQ(_),-thlocal:noDefaultValues(_),-thlocal:noRandomValues(_)],preq(must,C0))).


% -  ireq(Query) = Normal query (May not use second order logic) (must be asserted on isntance) (used mainly by 2nd order logic to avoid looping)
ireq(C0):- rescan_module_ready,no_loop_check(with_assertions([+insideIREQ(_), +thlocal:noDefaultValues(_),+thlocal:noRandomValues(_)],preq(ireq,C0))).


% -  props(Obj,QueryPropSpecs)
props(Obj,PropSpecs):- req(props(Obj,PropSpecs)).
iprops(Obj,PropSpecs):- ireq(props(Obj,PropSpecs)).
% -  add_fast(Assertion)
% add_fast(C0):- must_det((add_fast_unchecked(C0), xtreme_debug(once(ireq(C0);(with_all_dmsg((debug(blackboard),show_call(add_fast_unchecked(C0)),rtrace(add_fast_unchecked(C0)),dtrace(ireq(C0))))))))),!.
add_fast(C0):- must_det((add_fast_unchecked(C0), nop(xtreme_debug(ireq(C0)->true;dmsg(warn(failed_ireq(C0))))))),!.

:-export(add_fast_unchecked/1).
add_fast_unchecked(C0):-must_det(db_op(change(assert,add), C0)).

% -  upprop(Obj,PropSpecs) update the properties
upprop(Obj,PropSpecs):- upprop(props(Obj,PropSpecs)).
upprop(C0):- add(C0).
% -  padd(Obj,Prop,Value)
padd(Obj,PropSpecs):- add(props(Obj,PropSpecs)).
% -  padd(Obj,Prop,Value)
padd(Obj,Prop,Value):- add(dbase_t(Prop,Obj,Value)).
% -  prop(Obj,Prop,Value)
prop(Obj,Prop,Value):- req(dbase_t(Prop,Obj,Value)).
% -  prop_or(Obj,Prop,Value,OrElse)
prop_or(Obj,Prop,Value,OrElse):- one_must(dbase_t(Prop,Obj,Value),Value=OrElse).


kb_update(New,OldV):- req(New),!,OldV=New.
kb_update(New,OldV):- db_op_int(change(assert,OldV),New).

:-decl_thlocal((record_on_thread/2)).

hook:decl_database_hook(assert(_),ft_info(FT,_)):- define_ft(FT).
hook:decl_database_hook(assert(_),subft(FT,OFT)):- define_ft(OFT),define_ft(FT).
% hook:decl_database_hook(assert(_),subclass(FT,OFT)):- formattype(OFT),dmsg(warning(subclass_of_define_ft(FT))).

hook:dmsg_hook(transform_holds(dbase_t,_What,props(createableType,[isa(isa),isa]))):-trace_or_throw(dtrace).

% expand_goal_correct_argIsa(A,A):-simple_code,!.
expand_goal_correct_argIsa(A,B):- expand_goal(A,B).

% db_op_simpler(query(HLDS,_),MODULE:C0,req(call,MODULE:C0)):- atom(MODULE), nonvar(C0),not(not(predicate_property(C0,_PP))),!. % , functor_catch(C0,F,A), dmsg(todo(unmodulize(F/A))), %trace_or_throw(module_form(MODULE:C0)), %   db_op(Op,C0).
db_op_simpler(_,TypeTerm,props(Inst,[isa(Type)|PROPS])):- TypeTerm=..[Type,Inst|PROPS],nonvar(Inst),typeDeclarer(Type),!.




db_op_simpler_wlc(query(HLDS,Must),Wild,Simpler):- !,call(call,db_op_simpler(query(HLDS,Must),Wild,Simpler)),not(is_loop_checked(req(Simpler))),!.
db_op_simpler_wlc(Op,Wild,Simpler):- !,call(call,db_op_simpler(Op,Wild,Simpler)),not(is_loop_checked(db_op0(Op,Simpler))),!.


db_op_sentence(_Op,Prop,ARGS,C0):- atom(Prop),!, C0=..[Prop|ARGS].
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

generate_fact_arg(F,N,Arg):-argIsa_call(F,N,Type),!,isa_arg_test(Arg,Type).

isa_arg_test(Arg,Type):-ground(Arg),!,show_call(isa_backchaing(Arg,Type)).
isa_arg_test(Arg,Type):-isa_backchaing(Arg,Type).

equivRule_call(A,B):- is_asserted(holds_t(equivRule,A,B)).
equivRule_call(A,B):- is_asserted(holds_t(equivRule,B,A)).

forwardRule_call(A,B):- is_asserted(holds_t(forwardRule,B,A)).

:-dynamic_multifile_exported(equivRule/2).

good_for_chaining(_,_):-!.
good_for_chaining(_Op,Term):-not(contains_singletons(Term)).
db_rewrite(_Op,Term,NewTerm):-equivRule_call(Term,NewTerm).
db_rewrite(_Op,Term,NewTerm):-forwardRule_call(Term,NewTerm).

:-export(simply_functors/3).
simply_functors(Db_pred,query(HLDS,Must),Wild):- once(into_mpred_form(Wild,Simpler)),Wild\=@=Simpler,!,call(Db_pred,query(HLDS,Must),Simpler).
simply_functors(Db_pred,Op,Wild):- once(into_mpred_form(Wild,Simpler)),Wild\=@=Simpler,!,call(Db_pred,Op,Simpler).


% -  hook:dmsg_hook(db_op(query(HLDS,call),moo:holds_t(ft_info,type,'$VAR'(_)))):-trace_or_throw(dtrace).

% ================================================
% db_op_int/2
% ================================================
add_from_file(B,_):- contains_singletons(B),trace_or_throw(dtrace),dmsg(todo(add_from_file_contains_singletons(B))),!,fail.
add_from_file(B,B):- db_op(change(assert,_OldV),B),!.

% runs previously required ops fisrt
db_op_int(change(Assert,Op),Term):- !, db_op(change(Assert,Op),Term).
db_op_int(Op,Term):- do_db_op_hooks,db_op(Op,Term),do_db_op_hooks.

univ_left(Comp,[M:P|List]):- nonvar(M),univ_left0(M, Comp, [P|List]),!.
univ_left(Comp,[H,M:P|List]):- nonvar(M),univ_left0(M,Comp,[H,P|List]),!.
univ_left(Comp,[P|List]):-moo:dbase_mod(DBASE), univ_left0(DBASE,Comp,[P|List]),!.
univ_left0(M,M:Comp,List):- Comp=..List,!.

hook:decl_database_hook(AR,C):- record_on_thread(dbase_change,changing(AR,C)).

record_on_thread(Dbase_change,O):- thread_self(ID),thlocal:dbase_capture(ID,Dbase_change),!,Z=..[Dbase_change,ID,O],assertz(Z).


% ================================================
% db_op/2
% ================================================

% db_op(query(HLDS,Must),createableType(SubType)):- !, call_expanded_for(Must,is_creatable_type(SubType)).
db_op(change(_,_),props(_Obj,Props)):- Props ==[], !.
db_op(query(_,Must),NC):- not(compound(NC)),!,call_expanded_for(Must,NC).
db_op(query(Dbase_t, Req), must(Call)):-!, must(db_op(query(Dbase_t, Req), Call)).
db_op(query(HLDS,Must),B):-!, loop_check_term(db_op0(query(HLDS,Must),B),req(B),((loop_check_term(call_mpred(B),req2(B),show_call(is_asserted(B)))))). 
db_op(change(assert,CB2),B):- !,loop_check(db_op0(change(assert,CB2),B),must((dmsg(warn(looping(db_op(change(assert,CB2),B)))),hooked_asserta(B)))),!. % true = we are already processing this assert
db_op(change(retract,CB2),B):- !,loop_check(db_op0(change(retract,CB2),B),must((dmsg(warn(looping(db_op(change(retract,CB2),B)))),hooked_retract(B)))),!. % true = we are already processing this retract
db_op(Op,Term):- loop_check_local(db_op0(Op,Term),trace_or_throw(loop_check(db_op0(Op,Term)))).

% ================================================
% db_op0/2  SIMPLISTIC REWRITE (this is not the PRECANONICALIZER)
% ================================================
:-moo_hide_childs(db_op0/2).

db_op0(Op,G):- when_debugging(blackboard,dmsg(db_op0(Op,G))),fail.
db_op0(query(_HLDS,Must),isa(I,Type)):- !,call_expanded_for(Must,isa_backchaing(I,Type)).
db_op0(_,end_of_file):-!.
db_op0(Op,Term):- stack_check,var(Term),!,trace_or_throw(nc(db_op0(Op,Term))).
%db_op0(Op,props(Obj,nameStrings(Str))):-!, db_op0(Op,nameStrings(Obj,Str)).

db_op0(Op,MT:Term):- is_kb_module(MT),!,with_assertions(thlocal:caller_module(kb,MT),db_reop(Op,Term)).
db_op0(Op,DB:Term):- dbase_mod(DB),!,with_assertions(thlocal:caller_module(db,DB),db_reop(Op,Term)).
db_op0(Op,KB:Term):- atom(KB),!,with_assertions(thlocal:caller_module(prolog,KB),db_reop(Op,Term)).

% db_op0(Op,(':-'(A))):- must((expand_goal_correct_argIsa(A,AA))),expanded_different(A,AA),!,db_reop(Op, (':-'(AA))).

db_op0(Op,[dbase_t,Class,Inst]):-!,db_reop(Op,isa(Inst,Class)).
db_op0(Op,[holds_f,Class,Inst]):-!,db_reop(Op,isnt(Inst,Class)).

db_op0(Op,[dbase_t,P|List]):-nonvar(P),univ_left(G2,[P|List]),!,db_reop(Op,G2).
db_op0(Op,[holds_f,P|List]):-nonvar(P),univ_left(G2,[P|List]),!,db_reop(Op,not(G2)).

db_op0(Op,G1):- functor_check_univ(G1,F,[P|ListL]),List=[P|ListL],
      (is_holds_true(F) -> (nonvar(P) -> (univ_left(G2,List),db_reop(Op,G2)); db_reop(Op,[dbase_t|List])) ;
      (is_holds_false(F) -> (nonvar(P) -> (univ_left(G2,List),db_reop(Op,not(G2))); db_reop(Op,[holds_f|List]));
      fail)).

db_op0(Op,Term):- call(call,record_on_thread(dbase_opcall,db_reop(Op,Term))),fail.

db_op0(change(Dir,_OldV),(':-'(A))):- must(Dir=assert), !, must((expand_goal_correct_argIsa(A,AA),req(AA))).
db_op0(change(CA1,CB2),(C1;C2)):- !, (db_reop(change(CA1,CB2),C1);db_reop(change(CA1,CB2),C2)),!.
db_op0(Op,and(C1,C2)):- !,db_reop(Op,C1),db_reop(Op,C2).
db_op0(Op,(C1,C2)):- !,db_reop(Op,C1),db_reop(Op,C2).
db_op0(Op,(C1;C2)):- !,db_reop(Op,C1);db_reop(Op,C2).


db_op0(query(HLDS,Must),props(Obj,Props)):- nonvar(Obj),var(Props),!,gather_props_for(query(HLDS,Must),Obj,Props).


db_op0(Op,props(Obj,Open)):- var(Open),!,trace_or_throw(db_reop(Op,props(Obj,Open))).
db_op0(_Op,props(_Obj,[])):- !.
db_op0(Op,props(Obj,[P])):- nonvar(P),!,db_reop(Op,props(Obj,P)).
db_op0(Op,props(Obj,[P|ROPS])):- !,db_reop(Op,props(Obj,P)),db_op0(Op,props(Obj,ROPS)).
db_op0(Op,props(Obj,PropVal)):- atom(PropVal),!,Call=..[PropVal,Obj],!,db_reop(Op,Call).
db_op0(Op,props(Obj,PropVal)):- safe_univ(PropVal,[Prop,NonVar|Val]),Obj==NonVar,!,db_reop(Op,[dbase_t,Prop,Obj|Val]).
db_op0(Op,props(Obj,PropVal)):- PropVal=..[OP,Pred|Val],comparitiveOp(OP),not(comparitiveOp(Pred)),!,OPVAL=..[OP|Val],PropVal2=..[Pred,OPVAL],db_reop(Op,props(Obj,PropVal2)).
db_op0(Op,props(Obj,PropVal)):- PropVal=..[Prop|Val],not(infix_op(Prop,_)),!,db_reop(Op,[dbase_t,Prop,Obj|Val]).
db_op0(Op,props(Obj,PropVal)):- PropVal=..[Prop|Val],!,trace_or_throw(dtrace),db_reop(Op,[dbase_t,Prop,Obj|Val]).

db_op0(query(HLDS,Must),expand_args(Exp,Term)):- !, forall(do_expand_args(Exp,Term,O),no_loop_check(db_reop(query(HLDS,Must),O))).
db_op0(Op,expand_args(Exp,Term)):- !,forall(do_expand_args(Exp,Term,O),db_reop(Op,O)).
db_op0(Op,somethingIsa(A,List)):- !,forall_member(E,List,must(db_reop(Op, isa(A,E)))).
db_op0(Op,somethingDescription(A,List)):- !,forall_member(E,List, must(db_reop(Op, description(A,E)))).
db_op0(Op,objects(Type,List)):- !,forall_member(I,List,must(db_reop(Op,isa(I,Type)))).
db_op0(Op,sorts(Type,List)):- !,forall_member(I,List,must(db_reop(Op, subclass(I,Type)))).
db_op0(Op,predicates(List)):- !,forall_member(T,List,must(db_reop(Op,mpred(T)))).
db_op0(Op,EACH):- EACH=..[each|List],forall_member(T,List,must(db_reop(Op,T))).
db_op0(change(assert,_),mpred_prop(F,A)):- !,must(decl_mpred(F,A)).

db_op0(Op,db_op_exact(Term)):- !,db_op_exact(Op,Term).
 
% use assert_with_pred db_op0(change(assert,_)),description(A,E)):- once(must(add_description(A,E))),fail. % db_reop(change(CA1,CB2),descriptionHere(A,E)).
db_op0(Op,nameStrings(A,S0)):- nonvar(S0),determinerRemoved(S0,String,S),!,db_reop(Op, nameStrings(A,S)),add(determinerString(A,String)).


db_op0(Op,props(Obj,nameStrings(Str))):-!,db_reop(Op,nameStrings(Obj,Str)).
db_op0(query(HLDS,Must),isa(Term,Var)):- !,call_expanded_for(query(HLDS,Must),call(call,isa_backchaing(Term,Var))).
db_op0(Op,isa(A,SubType)):- dbase_t(createableSubclassType,SubType,Type),!,db_reop(Op,isa(A,Type)),db_reop(Op,isa(A,SubType)).
db_op0(query(HLDS,Must),argIsa(P,N,T)):- call_expanded_for(query(HLDS,Must),(get_mpred_prop(P,argsIsaInList(ArgsIsa)),arg(N,ArgsIsa,T),must(nonvar(T)))).

db_op0(change(_,_),Term):- glean_pred_props_maybe(Term),fail.



db_op0(Op,Wild):- into_mpred_form(Wild,Simpler), acceptable_xform( Wild , Simpler),dmsg(into_mpred_form(Op,Wild->Simpler)), !,db_reop(Op,Simpler).
db_op0(Op,A):- must_det(once(correctArgsIsa(Op,A,AA))),acceptable_xform(  A , AA),dmsg(correctArgsIsa(Op,A->AA)), !, db_reop(Op,AA).
db_op0(Op,Wild):- transform_holds(dbase_t,Wild,Simpler),acceptable_xform(  Wild , Simpler),!,dmsg(transform_holds(Op,Wild->Simpler)),db_reop(Op,Simpler).
% db_op0(Op,Term):- call(call,good_for_chaining(Op,Term)), db_rewrite(Op,Term,NewTerm),not(contains_singletons(NewTerm)),db_reop(Op,NewTerm).
% db_op0(Op,Wild):- dsfdf db_op_simpler_wlc(Op,Wild,Simpler),!,db_reop(Op,Simpler).

db_op0(change(DIR,_),argsIsaInList(Term)):- must(DIR==assert),!,show_call(decl_mpred(Term)),!.

db_op0(change(DIR,_),isa(T,Type)):- must(DIR==assert),!,assert_isa(T,Type),!.

db_op0(Op,Term):- type_error_checking,!, Term =..[Type,A],!,db_reop(Op,isa(A,Type)).

db_op0(Op,C0):- C0=..[Prop|ARGS],db_op_unit(Op,C0,Prop,ARGS).


% ================================================
% db_op_unit/3
% ================================================

db_op_unit(Op,_C0,Prop,ARGS):- type_error_checking,!, cached_isa(Prop,type),trace_or_throw(db_op_unit(Op,type(Prop),ARGS)).

db_op_unit(Op,C0,isa,ARGS):- type_error_checking,!, trace_or_throw(db_op_unit(Op,isa(C0),ARGS)).

% use_db_op/1
db_op_unit(Op,C0,Prop,_ARGS):- get_mpred_prop(Prop,use_db_op(Other)),!,call(Other,Op,C0).

% assert_with_pred/1
db_op_unit(change(assert,A),C0,Prop,_RGS):- get_mpred_prop(Prop,assert_with_pred(How)),!, must(nonvar(How)), once(ignore((call(How,C0), run_database_hooks(assert(A),C0)))).

% retract_with_pred/1
db_op_unit(change(retract,A),C0,Prop,_RGS):- get_mpred_prop(Prop,retract_with_pred(How)),!, must(nonvar(How)), once(ignore((call(How,C0), run_database_hooks(retract(A),C0)))).


% db_op_unit(query(HLDS,Must),_C0,Prop,ARGS):- bad_idea, get_mpred_prop(Prop,extentAsserted),!,call_expanded_for(query(HLDS,Must),dbase_t_p2(Prop,ARGS)).

% query_with_pred/1
db_op_unit(query(Must,HLDS),C0,Prop,_RGS):- get_mpred_prop(Prop,query_with_pred(How)),!, call_expanded_for(query(Must,HLDS),call(How,C0)).


% plain prop
db_op_unit(Op,_C0,Prop,ARGS):- once((db_op_sentence(Op,Prop,ARGS,Unit),same_vars(ARGS,Unit))), db_op_exact(Op,Unit).

% if is IREQ then fail
db_op_unit(Op,C0,_Prop,_ARGS):- test_tl(thlocal:insideIREQ,C0),!,must_det(query(_Must,_HLDS)=Op),fail.

% genlInverse/2
db_op_unit(Op,_C0,Prop,ARGS):- dbase_t(genlInverse,Prop,Other), inverse_args(ARGS,Inverse), db_op_sentence(Op,Other,Inverse,Unit1), db_op_exact(Op,Unit1).

% genlPreds/2
db_op_unit(Op,_C0,Prop,ARGS):- dbase_t(genlPreds,Prop,Other),  db_op_sentence(Op,Other,ARGS,Unit1), db_op_exact(Op,Unit1).

db_op_loop(Op,Unit,Result):- is_loop_checked(db_op0(Op,Unit)),!,call(Result).
db_op_loop(Op,Unit,_Result):- db_reop(Op,Unit).

% ================================================
% db_op_exact/2
% ================================================
db_op_exact(Op,G):- G=..[_SubType,_Arg],not(prolog_side_effects(G)),dmsg(todo(ensure_Usable(Op,G))),fail.
db_op_exact(query(HLDS,Must),Term):- !,call_expanded_for(query(HLDS,Must),Term).
db_op_exact(query, Term):- !,call_expanded_for(findall,Term).
db_op_exact(must, Term):- !,call_expanded_for(must,Term).
db_op_exact(u,C):- trace_or_throw(dtrace),db_quf(u,C,U,Template),call_mpred_fast(U),Template,must(ground(Template)),!,ignore(hooked_retractall(Template)).
db_op_exact(Op,G):- when_debugging(blackboard,dmsg(db_op_exact(Op,G))),fail.
db_op_exact(change(retract,all),C):- !, db_quf(change(retract,all),C,U,Template),!, when_debugging(retract,dtrace), doall((call_mpred_fast(U),hooked_retractall(Template))).
db_op_exact(change(retract,A),C):- must(db_quf(change(retract,A),C,U,Template)),!,  when_debugging(retract,dtrace), call_mpred_fast(U),!,hooked_retract(Template).
db_op_exact(change(Assert,OldV),W):- non_assertable(W,Why),trace_or_throw(todo(db_op(change(Assert,OldV), non_assertable(Why,W)))).
db_op_exact(change(assert,Must),C0):- db_quf(change(assert,Must),C0,U,C),!,must(call_mpred_fast(U)),functor_catch(C,F,A),( get_mpred_prop(F,singleValued) -> must(db_assert_sv(Must,C,F,A)) ; must(db_assert_mv(Must,C,F,A))).
db_op_exact(change(assert,Must),C):- trace_or_throw(dtrace),functor_catch(C,F,A), must_det((get_mpred_prop(F,singleValued) -> must_det(db_assert_sv(assert(Must),C,F,A)) ; must(db_assert_mv(assert(Must),C,F,A)))).
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



never_dbase_mpred(mpred_prop).
never_dbase_mpred(mpred_arity).


% ================================================
% db_assert_[mv|sv]/3
% ================================================

:-dmsg_hide(db_assert_mv).
:-dmsg_hide(db_assert_sv).

:-dmsg_hide(db_op_exact).
:-dmsg_hide(add).
:-dmsg_hide(assert_isa).


% assert_with to change(CA1,CB2) mutlivalue pred
:-export((db_assert_mv/4)).
db_assert_mv(_Must,end_of_file,_,_):-!.
% db_assert_mv(_Must,C,_F,_A):- hooked_assertz(C),!.
db_assert_mv(Must,C,F,A):- test_tl(thlocal:adding_from_srcfile), dmsg(db_assert_mv(Must,C,F,A)), hooked_assertz(C).
db_assert_mv(Must,C,F,A):- dmsg(db_assert_mv(Must,C,F,A)), must_det(mpred_prop(F,ordered) -> hooked_assertz(C) ; hooked_asserta(C)).


% assert_with to change(CA1,CB2) singlevalue pred
:-export((db_assert_sv/4)).
%db_assert_sv(_Must,C,F,A):- throw_if_true_else_fail(contains_singletons(C),db_assert_sv(C,F,A)).
db_assert_sv(Must,C,F,A):- ignore(( loop_check(db_assert_sv_lc(Must,C,F,A),true))).

:-export((db_assert_sv_lc/4)).
db_assert_sv_lc(Must,C,F,A):- arg(A,C,UPDATE),db_assert_sv_now(Must,C,F,A,UPDATE),!.

:-export(db_assert_sv_now/5).
db_assert_sv_now(Must,C,F,A, UPDATE):- has_free_args(db_assert_sv_now(Must,C,F,A, UPDATE)),!,trace_or_throw(var_db_assert_sv_now(Must,C,F,A, UPDATE)).
db_assert_sv_now(Must,C,F,A, UPDATE):- number(UPDATE),UPDATE<0, !,db_assert_sv_update(Must,C,F,A,UPDATE).
db_assert_sv_now(Must,C,F,A,+UPDATE):-!, db_assert_sv_update(Must,C,F,A,+UPDATE).
db_assert_sv_now(Must,C,F,A,-UPDATE):-!, db_assert_sv_update(Must,C,F,A,-UPDATE).
db_assert_sv_now(Must,C,F,A, REPLACE):- db_assert_sv_replace(Must,C,F,A, REPLACE).

:-export(db_assert_sv_update/5).
db_assert_sv_update(Must,C,F,A,UPDATE):-
   replace_arg(C,A,OLD,COLD),
   % prefer updated values to come from instances but will settle with anything legal
   must_det((once(ireq(COLD);mreq(COLD)),ground(COLD))),
   update_value(OLD,UPDATE,NEW),!,
   db_assert_sv_replace(Must,C,F,A,NEW),!.

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
   ignore(ireq(COLD)),
   must_det(db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW)),!.

db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW):- var(OLD),!,
   dmsg(db_assert_sv(COLD,'__add__',CNEW)),
   % replace_arg(C,A,_,CBLANK),hooked_retractall(CBLANK),
   must_det(hooked_asserta_confirmed(CNEW,A,NEW)),!.

db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW):- OLD =@= NEW,!.
db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW):- unify_with_occurs_check(OLD,NEW),!,dmsg(db_assert_sv_same(COLD,'__unify_with_occurs_check__',CNEW)).
db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW):- equals_call(OLD,NEW),!,dmsg(db_assert_sv_same(COLD,'__same__',CNEW)),trace_or_throw(dtrace).
db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW):-
   dmsg(db_assert_sv(COLD,'__replace__',CNEW)),
   ignore(show_call_failure((del(COLD), not(ireq(COLD))))),
   %replace_arg(C,A,_,CBLANK),must_det(clr(CBLANK)),hooked_retractall(CBLANK),   
   must_det(hooked_asserta_confirmed(CNEW,A,NEW)),!.

:-style_check(+singleton).


equals_call(X,Y):-unify_with_occurs_check(X,Y),!.
equals_call(X,Y):-once((any_to_string(X,XX),any_to_string(Y,YY))),unify_with_occurs_check(XX,YY),!.
equals_call(X,Y):-once((to_word_list(X,XX),to_word_list(Y,YY))),unify_with_occurs_check(XX,YY),!.
equals_call(X,Y):-compound(X),compound(Y),once((correctArgsIsa(X,XX),correctArgsIsa(Y,YY))),unify_with_occurs_check(XX,YY),!.

confirm_hook(CNEW:NEW=@=CNOW:NOW):-
   must_det(var(NOW)),               
   once(ireq(CNOW)),
   CNEW:NEW=@=CNOW:NOW,!.

confirm_hook(CNEW:NEW=@=CNOW:NOW):-
   dmsg(warn(failed_i_a_req(CNOW,expected(CNEW)))),
   verify_sanity((mreq(CNOW),(CNEW:NEW=@=CNOW:NOW))),!.



% Expect CNEW to be what is found
hooked_asserta_confirmed(CNEW,A,NEW):-
   replace_arg(CNEW,A,NOW,CNOW),
   must_det(ground(CNEW)),
   hooked_asserta(CNEW),!,
   verify_sanity(confirm_hook(CNEW:NEW=@=CNOW:NOW)),!.

hooked_asserta_confirmed(CNEW,A,NEW):-dmsg(unconfirmed(hooked_asserta_confirmed(CNEW,A,NEW))).

:-ensure_loaded(dbase_i_propvals).
:-ensure_loaded(dbase_i_call).

:-moo_hide_childs(replace_arg/4).
replace_arg(C,A,OLD,CC):- 
   C=..FARGS,
   replace_nth(FARGS,A,OLD,FARGO),!,
   CC=..FARGO.

:-moo_hide_childs(replace_nth/4).
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

:-moo_hide_childs(update_value/4).
update_value(OLD,NEW,NEXT):- var(NEW),!,trace_or_throw(logicmoo_bug(update_value(OLD,NEW,NEXT))).
update_value(OLD,NEW,NEWV):- var(OLD),!,compute_value_no_dice(NEW,NEWV).
update_value(OLD,X,NEW):- is_list(OLD),!,list_update_op(OLD,X,NEW),!.
update_value(OLDI,+X,NEW):- compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD + X,_,fail),!.
update_value(OLDI,-X,NEW):- compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD - X,_,fail),!.
update_value(OLDI,X,NEW):- number(X),X<0,compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD + X,_,fail),!.
update_value(_,NEW,NEWV):-compute_value_no_dice(NEW,NEWV),!.


% - 	list_difference_eq(+List, -Subtract, -Rest)
%
%	Delete all elements of Subtract from List and unify the result
%	with Rest.  Element comparision is done using ==/2.

list_difference_eq([],_,[]).
list_difference_eq([X|Xs],Ys,L) :-
	(   memberchk_eq(X,Ys)
	->  list_difference_eq(Xs,Ys,L)
	;   L = [X|T],
	    list_difference_eq(Xs,Ys,T)
	).

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

:- ensure_loaded(dbase_c_term_expansion).

:- ensure_loaded(dbase_i_db_preds).

:- ensure_loaded(dbase_i_cyc).

:- decl_mpred_hybrid(expand_args,2). 

:- decl_mpred(objid,2).

% flags
:-decl_mpred(agent(id),[flag]).
:-decl_mpred(item(id),[flag]).
:-decl_mpred(region(id),[flag]).
:-decl_mpred(type(id),[flag]).
:-decl_mpred(thinking(agent),[flag]).
:-decl_mpred(deleted(id),[flag]).

:- decl_mpred(needs_look/2,[completeExtentAsserted]).
:- decl_mpred(mudMaxHitPoints(agent,int)).



:-export(forall_setof/2).
forall_setof(ForEach,Call):-
   findall(ForEach,ForEach,ForEachAll),
   list_to_set(ForEachAll,Set),!,
   ignore(forall(member(ForEach,Set),Call)).

:-dynamic_multifile_exported((moo:was_imported_kb_content/2)).

:-decl_mpred_prolog(moo:was_imported_kb_content/2).

is_clause_moo_special((Head :- Body)):-!, is_clause_moo_special(Head,Body).
is_clause_moo_special(C):- is_clause_moo_special(C,true).

:-export(add_later/1).
add_later(Fact):- call_after_game_load(add(Fact)).

:-decl_thlocal add_thread_override/1.
% thlocal:add_thread_override(A):-add_from_macropred(A),!.

:-export((add/1)).
:-moo_hide_childs(add/1).
add(A):- A==end_of_file,!.
add(A):- not(compound(A)),!,trace_or_throw(not_compound(add(A))),!.
add(Call):- loop_check(thlocal:add_thread_override(Call)),!.
add(Call):- add_from_macropred(Call),!.
add(M:HB):-atom(M),!, must_det(add(HB)),!.
add(type(A)):- must_det(decl_type(A)),!.
add(A):-must(add_fast(A)),!.
add(A):-trace_or_throw(fmt('add is skipping ~q.',[A])).

:-export(begin_prolog_source/0).
:-export(end_prolog_source/0).
begin_prolog_source:- must_det(asserta(thlocal:in_prolog_source_code)).
end_prolog_source:- must_det(retract(thlocal:in_prolog_source_code)).

:-export(add_from_macropred/1).
add_from_macropred(C):- loop_check(add_from_macropred_lc(C),((dmsg(loopING_add_from_macropred(C)),dtrace,add_fast(C)))).
add_from_macropred_lc(A):-A==end_of_file,!.
add_from_macropred_lc(A):- not(compound(A)),!,trace_or_throw(not_compound(add_from_macropred_lc(A))).
add_from_macropred_lc(':-'(A)):- predicate_property(A,_),!,must(logOnFailure(A)),!.
add_from_macropred_lc(':-'(ensure_loaded(A))):- add(':-'(load_data_file(A))),!.
add_from_macropred_lc(':-'(A)):- dmsg(trace_or_throw(missing_directive(A))),!.
add_from_macropred_lc(':-'(Head,true)):- !, add(Head).
add_from_macropred_lc(':-'(Head,Body)):- must_det(assertz_local_game_clause(Head,Body)),!.
add_from_macropred_lc(RDF):- RDF=..[SVO,S,V,O],is_svo_functor(SVO),!,must_det(add(dbase_t(V,S,O))).
add_from_macropred_lc(A):- into_mpred_form(A,F), A \=@=F,!,add_from_macropred(F). 
add_from_macropred_lc(ClassTemplate):- compound(ClassTemplate), ClassTemplate=..[item_template,Type|Props],
   assert_isa(Type,createableType),
   assert_isa(Type,type),
   add(subclass(Type,item)),   
   flatten(Props,AllProps),!,
   show_call(add(default_type_props(Type,AllProps))).
/*
add_from_macropred_lc(somethingIsa(A,List)):-forall_member(E,List,add(isa(A,E))).
add_from_macropred_lc(somethingDescription(A,List)):-forall_member(E,List,add(description(A,E))).
add_from_macropred_lc(objects(Type,List)):-forall_member(I,List,add(isa(I,Type))).
add_from_macropred_lc(sorts(Type,List)):-forall_member(I,List,add(subclass(I,Type))).
add_from_macropred_lc(predicates(List)):-forall_member(T,List,add(mpred_prop(T))).
add_from_macropred_lc(description(A,E)):- add_description(A,E).
add_from_macropred_lc(nameStrings(A,S0)):- determinerRemoved(S0,String,S),!,add(nameStrings(A,S)),add(determinerString(A,String)).
add_from_macropred_lc(Call):- fail, predicate_property(Call, number_of_rules(_)),not(predicate_property(Call, dynamic)),nop(dmsg(assert_to_static(Call))),fail.
add_from_macropred_lc(d(s)):-dumpST, trace_or_throw(dtrace).
*/
add_from_macropred_lc(M:HB):-atom(M),!,add_from_macropred_lc(HB).

:-export((assertz_local_game_clause/1)).
assertz_local_game_clause(Before):- expand_term(Before,Replaced),Before \=@= Replaced,!, assertz_local_game_clause(Replaced).
assertz_local_game_clause((':-'(Body))):-!,must_det(show_call(Body)),!.
assertz_local_game_clause((Head :- Body)):- !,assertz_local_game_clause(Head,Body),!.
assertz_local_game_clause(C):- assertz_local_game_clause(C,true),!.

assertz_local_game_clause(Head,Body):- (var(Head);var(Body)),!,trace_or_throw(var_assertz_local_game_clause(Head,Body)).
assertz_local_game_clause(Head,Body):- clause_asserted((':-'(Head,Body))),!.
assertz_local_game_clause(Head,Body):- ExpIn = (Head:-Body),  expand_term(ExpIn,Exp),Exp \=@= ExpIn,!,assertz_local_game_clause(Exp),!.
assertz_local_game_clause(Head,Body):- get_mpred_type(Head,Type),!,must_det(assertz_local_game_clause(Type,Head,Body)),!.


assertz_local_game_clause(callable(prologOnly),Head,Body):- must_det(assertz_if_new_clause(Head,Body)),dmsg(used_clause_as_prologOnly(Head,Body)).
assertz_local_game_clause(callable(static),Head,Body):- must_det(assertz_if_new_clause(Head,Body)),trace_or_throw(eRROR_maybe_used_clause_as_prologOnly(Head,Body)).
assertz_local_game_clause(_,Head,true):- !,with_assertions(thlocal:adding_from_srcfile,add(Head)),!.
assertz_local_game_clause(callable(prologHybrid),Head,Body):-!, assertz_if_new_clause(moo:hybrid_rule(Head,Body),true),dmsg(used_clause_as_hybridRule(Head,Body)),
   decl_mpred_hybrid(F/A),!,declare_dbase_local_dynamic_really(moo,F,A).
assertz_local_game_clause(Type,Head,BodyIn):- once(make_body_clause(Head,BodyIn,Body)),must_det(assertz_if_new_clause(Head,Body)),dmsg(used_clause_as_unknown(Type,Head,Body)).
assertz_local_game_clause(Type,Head,Body):- must_det(assertz_if_new_clause(Head,Body)),dmsg(used_clause_as(Type,Head,Body)).

special_wrapper_body(W):-get_body_functor(W,F,_),!,special_wrapper_functor(F).

get_mpred_type(Head,Type):-functor_h(Head,F,A),!,get_mpred_type(Head,F,A,Type).
get_mpred_type(F,A,Type):-functor(P,F,A),get_mpred_type(P,F,A,Type).

get_mpred_type(Head,F,A,Type):-atom(Head),mpred_arity(Head,A),!,dmsg(get_mpred_type(Head,F,A,Type)),get_mpred_type(Head,A,Type).
get_mpred_type(Head,_,_,Type):-compound(Head),!,functor_h(Head,F,A),get_mpred_type4(Head,F,A,Type).
get_mpred_type(_,F,A,Type):-atom(F),number(A),!,functor(Head,F,A),get_mpred_type4(Head,F,A,Type).
get_mpred_type(Head,F,A,Type):-must(mpred_arity(F,A)),functor(Head,F,A),get_mpred_type4(Head,F,A,Type).

get_mpred_type4(P,F,A,T):-get_mpred_type5(P,F,A,T),!.

get_mpred_type5(_,F,_,callable(Type)):-member(Type,[prologOnly,prologHybrid,type]),mpred_prop(F,Type).
get_mpred_type5(P,_,_,W):-compound(P),!,pp_has(P,W).
get_mpred_type5(_,F,A,W):-atom(F),current_predicate(F/A),functor(P,F,A),!,pp_has(P,W).
get_mpred_type5(F,_,A,W):-atom(F),current_predicate(F/A),functor(P,F,A),!,pp_has(P,W).
get_mpred_type5(_P,_F,_A,funknown):-!. % dmsg(warn_pp(not(predicate_property(P,F,A)))).

pp_has(P,callable(dynamic)):-predicate_property(P,dynamic),!.
pp_has(P,callable(static)):-predicate_property(P,dynamic),!.
pp_has(_,unknown).

special_wrapper_functor(call_mpred_body).
special_wrapper_functor(body_req).
special_wrapper_functor(loop_check).
special_wrapper_functor(loop_check_term).
special_wrapper_functor(loop_check_clauses).

make_body_clause(_Head,Body,Body):-atomic(Body),!.
make_body_clause(_Head,Body,Body):-special_wrapper_body(Body),!.
make_body_clause(Head,Body,moo:call_mpred_body(Head,Body)).


assertOnLoad(X):-add_later(X).

setTemplate(X):-add(X).

englishServerInterface(SomeEnglish):-dmsg(todo(englishServerInterface(SomeEnglish))).

:-export(onLoad/1).
onLoad(C):-call_after_game_load(C).

onSpawn(ClassFact):- ClassFact=..[Funct,InstA],createByNameMangle(InstA,Inst,Type2),assert_isa(Type2,createableType),assert_isa(Inst,Funct),assert_isa(Inst,Type2),!.
onSpawn(ClassFact):- ClassFact=..[Funct|InstADeclB],must_det(onSpawn_f_args(Funct,InstADeclB)).

onSpawn_f_args(Funct,List):-
  with_assertions(deduceArgTypes(Funct),
   (convertSpawnArgs(Funct,1,List,NewList),
     Later =.. [dbase_t,Funct|NewList],
   call_after_game_load(with_assertions(deduceArgTypes(Funct), add(Later))))),!.

convertSpawnArgs(_,_,[],[]).
convertSpawnArgs(Funct,N,[A|List],[O|NewList]):-
  convertOneSpawnArg(Funct,N,A,O),!,
  N2 is N + 1,
  convertSpawnArgs(Funct,N2,List,NewList).

convertOneSpawnArg(_,_,O,O):-string(O),!.
convertOneSpawnArg(_,_,O,O):-number(O),!.
convertOneSpawnArg(_,_,nospawn(O),O):-!.
convertOneSpawnArg(Funct,N,spawn(A),O):-spawnOneSpawnArg(Funct,N,A,O).
convertOneSpawnArg(Funct,N,A,O):-spawnOneSpawnArg(Funct,N,A,O).

spawnOneSpawnArg(Funct,N,A,O):-
  createByNameMangle(A,O,TypeA),
  assert_subclass_on_argIsa(Funct,N,TypeA),
  assert_isa(TypeA,createableType).


createByNameMangle(InstA,InstA,Type):-compound(InstA),InstA=..[Type|Props],assert_isa(InstA,Type),with_assertions(deduceArgTypes(_),padd(InstA,Props)).
createByNameMangle(InstA,Inst,Type):- compound(InstA),!,functor_catch(InstA,Type,A),must(A==1),assert_isa(InstA,Type),InstA=Inst.
createByNameMangle(InstA,_,_Type):- not(atom(InstA)),!,trace_or_throw(todo(not_atom_createByNameMangle(InstA))).
createByNameMangle(Suggest,InstA,Type):- once(split_name_type(Suggest,InstA,Type)),Suggest==InstA,assert_isa(InstA,Type).
createByNameMangle(Type,InstA,Type):- atom_concat(Type,'777',InstA),must_det(assert_isa(InstA,Type)), call_after_game_load(create_instance(InstA)).
createByNameMangle(InstA,IDA,InstA):- gensym(InstA,IDA), englishServerInterface([create,InstA,IDA]).

wfAssert(X):-add(X). %  add_later(X).


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

special_head(_,F):-mpred_prop(F,prologOnly),!,fail.
special_head(_,F):-mpred_prop(F,prologHybrid).
special_head(_,F):-mpred_prop(F,hasStub(_)).

:-export(thlocal:in_dynamic_reader/1).

:-export(begin_dynamic_reader/0).
begin_dynamic_reader:-  dynamic_multifile_exported((moo:was_imported_kb_content/2)),
  !. %  must_det(( prolog_load_context(file,Source),asserta(thlocal:in_dynamic_reader(Source)))).
:-export(end_dynamic_reader/0).
end_dynamic_reader:-  
  !. % must_det(( prolog_load_context(file,Source),retract(thlocal:in_dynamic_reader(Source)))).

:-export(inside_dynamic_reader/0).
inside_dynamic_reader :- prolog_load_context(file,Source),test_tl(thlocal:in_dynamic_reader(Source)),!.
inside_dynamic_reader :- prolog_load_context(source,Source),test_tl(thlocal:in_dynamic_reader(Source)),!.

user:term_expansion(CL,moo:was_imported_kb_content(inside_dynamic_reader,CL)):-not(thlocal:into_form_code), not((functor_h(CL,F),F=was_imported_kb_content)),
 % ==== why we assert
   not(is_clause_moo_special(CL)),inside_dynamic_reader,
% ==== do it
   dmsg(assertz_inside_dynamic_reader(CL)),ignore(is_compiling_sourcecode),with_assertions(thlocal:adding_from_srcfile,must_det(add(CL))),!.

user:term_expansion(CL,moo:was_imported_kb_content(is_clause_moo_special,CL)):-not(thlocal:into_form_code), not((functor_h(CL,F),F=was_imported_kb_content)),
% ==== why we assert
   is_clause_moo_special(CL),  not(inside_dynamic_reader), 
% ==== do it
   dmsg(addingGaf(CL)),ignore(is_compiling_sourcecode),with_assertions(thlocal:adding_from_srcfile,must_det(add(CL))),!.

user:term_expansion((H:-B),Out):- fail,test_tl(enable_src_loop_checking),
   with_assertions(thlocal:adding_from_srcfile,
    once((once(make_body_clause(H,B,NewBody)),(B \=@= NewBody),!,dmsg(thlocal:enable_src_loop_checking((H:-NewBody)))))),Out=(H:-NewBody).

% load_motel:- defrole([],time_state,restr(time,period)).
% :-load_motel.

:- ensure_loaded(logicmoo('vworld/moo_footer.pl')).

agent_text_command(_Agent,_Text,_AgentTarget,_Cmd):-fail.
/*
    coerce/3,
    rescan_dbase_facts/0,
          enter_term_anglify/2,
           agent_text_command/4,         
*/

:- ensure_loaded(logicmoo(vworld/world)).
:-'$hide'(rescan_dbase_ops/0).
:-'$hide'(do_db_op_hooks/0).
%:- rescan_missing_stubs.
%:- rescan_mpred_props.



:-ensure_loaded(logicmoo(planner/dbase_i_hyhtn)).

% ================================================
% MPRED_PROP System
% ================================================

