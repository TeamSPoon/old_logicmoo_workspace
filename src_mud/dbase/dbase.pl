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
:- use_module(library(semweb/turtle)).

:- dynamic_multifile_exported fact_is_false/2.
:- dynamic_multifile_exported kbp_t_list_prehook/2.
:- dynamic_multifile_exported get_mpred_storage_provider/3.
:- dynamic_multifile_exported get_mpred_storage_provider/4.

:- include(logicmoo('vworld/moo_header.pl')).

ztrace:-dmsg(ztrace),trace_or_throw(dtrace).

:-meta_predicate_transparent(when_debugging(+,0)).
when_debugging(What,Call):-debugging(What),!,Call.
when_debugging(_,_).

:-dynamic_multifile_exported(listprolog/0).
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
:- decl_thlocal into_form_code/0.
:- decl_thlocal repl_to_string/2.
:- decl_thlocal repl_writer/2.
:- decl_thlocal thlocal:useOnlyExternalDBs/0.
:- decl_thlocal with_callMPred/1.
:- decl_thlocal skip_db_op_hooks/0.
:- decl_thlocal thlocal:in_dynamic_reader/1.
:- decl_thlocal thlocal:tracing80/0.
:- decl_thlocal thlocal:usePlTalk/0.
:- decl_thlocal thlocal:useAltPOS/0.

:- decl_thlocal infInstanceOnly/1.
:- decl_thlocal no_arg_type_error_checking/0.
:- decl_thlocal infAssertedOnly/1.
:- decl_thlocal noRandomValues/1.


:- decl_thlocal thlocal:session_agent/2.

:- dynamic thglobal:global_session_agent/2.

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
:-ensure_loaded(dbase_i_mpred_prolog).
:-ensure_loaded(dbase_i_mpred_dbase_t).

:- op(1120,fx,decl_mpred_prolog).
:- op(1150,fx,decl_mpred_hybrid).


toUpperCamelcase(Type,TypeUC):-toCamelcase(Type,TypeC),toPropercase(TypeC,TypeUC),!.
:-export(i_name/2).
i_name(OType,IType):-typename_to_iname0('',OType,IOType),!,IOType=IType.
:-export(i_name/3).
i_name(I,OType,IType):-typename_to_iname0(I,OType,IOType),!,IOType=IType.

typename_to_iname0(I,OType,IType):-type_prefix(Prefix,_),atom_concat(Prefix,Type,OType),capitalized(Type),!,typename_to_iname0(I,Type,IType).
typename_to_iname0(I,Type,IType):-nonvar(Type),toUpperCamelcase(Type,UType),atom_concat(I,UType,IType).


% ================================================
% A tiny bit of TMS
% ================================================

:-ensure_loaded(dbase_i_isa_subclass).

:-ensure_loaded(dbase_i_deduce).

% ================================================
% A tiny bit of TMS
% ================================================

user:decl_database_hook(assert(_),Fact):- check_was_known_false(Fact).

was_known_false(Fact):-is_known_false(Fact),doall(retract((is_known_false(_):-true))),dmsg(trace_or_throw(error+was_known_false(Fact))).

check_was_known_false(Fact):- ignore(((is_known_false(Fact),was_known_false(Fact)))).

user:decl_database_hook(assert(_A_or_Z),mudLabelTypeProps(Lbl,T,Props)):- hooked_asserta(typeProps(T,[mudKwLabel(Lbl)|Props])).

user:decl_database_hook(assert(_A_or_Z),typeProps(T,_)):- decl_type_safe(T).

user:decl_database_hook(assert(_A_or_Z),mpred_prop(F,_)):- must_det(atom(F)).


% ================================================
% Agenda system - source file loading
% ================================================

:-dynamic_multifile_exported((thglobal:loading_game_file/2, thglobal:loaded_game_file/2)).

thglobal:after_game_load:- not(thglobal:loading_game_file(_,_)),thglobal:loaded_game_file(_,_),!.

% when all previous tasks have completed
after_game_load_pass2:- not(thglobal:will_call_after(thglobal:after_game_load,_)).
:- meta_predicate(call_after_game_load(0)).
% call_after_game_load(Code):- thglobal:after_game_load,!, call_after_next(after_game_load_pass2,Code).
call_after_game_load(Code):- call_after_next(thglobal:after_game_load,Code).

:-dynamic_multifile_exported(rescan_game_loaded/0).
rescan_game_loaded:- ignore((thglobal:after_game_load, loop_check(call_after(thglobal:after_game_load, true ),true))).

:-dynamic_multifile_exported(rescan_game_loaded_pass2/0).
rescan_game_loaded_pass2:- ignore((thglobal:after_game_load, loop_check(call_after(after_game_load_pass2,  dmsg(rescan_game_loaded_pass2_comlpete)),true))).

% ================================================
% Agenda system - standard database
% ================================================

% do_db_op_hooks:-!.
do_db_op_hooks:- loop_check_local(rescan_dbase_ops,true).

:-dynamic_multifile_exported(rescan_slow_kb_ops/0).

% rescan_slow_kb_ops:-!.
rescan_slow_kb_ops:- loop_check(forall(retract(do_slow_kb_op_later(Slow)),must_det(Slow)),true).

:-dynamic_multifile_exported(rescan_dbase_ops/0).
rescan_dbase_ops:- test_tl(skip_db_op_hooks),!.
rescan_dbase_ops:- rescan_module_ready.

:-decl_thlocal thlocal:in_rescan_module_ready/0.
rescan_module_ready:- thlocal:in_rescan_module_ready,!.
rescan_module_ready:- with_assertions(thlocal:in_rescan_module_ready,loop_check_local(do_all_of(dbase_module_ready),true)).

:- dynamic_multifile_exported do_slow_kb_op_later/1.
slow_kb_op(Slow):- test_tl(do_slow_kb_op_now),!,debugOnError(Slow).
slow_kb_op(Slow):- asserta_if_new(do_slow_kb_op_later(Slow)),!.


% ========================================
% enter_term_anglify(MASK)
% ========================================

enter_term_anglify(X,Y):-findall(X-Y-Body,clause( mudTermAnglify(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).
enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify_np(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).
enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify_last(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).
enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify_np_last(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).


:-dynamic_multifile_exported(coerce/4).

coerce(What,Type,NewThing,_Else):-coerce(What,Type,NewThing),!.
coerce(_ ,_,     NewThing,Else):- NewThing = Else.




:- meta_predicate tick_every(*,*,0).
:- meta_predicate register_timer_thread(*,*,0).



:-op(1150,fx,export).

:- '@'(ensure_loaded(logicmoo(vworld/moo)),'user').
:- '@'(ensure_loaded(dbase_i_formattypes),'user').
:-decl_type(ttFormatType).
:-decl_type(ttValueType).



% :- if_file_exists(user_ensure_loaded(logicmoo(../externalsdbase/dbase_rules_pttp))).


% :-ensure_loaded(dbase_i_formattypes).

%:- trace, (dynamic_multifile_exported  obj/1). 
%:- trace, (dynamic_multifile_exported  obj/1). 


:- dynamic_multifile_exported(( 
   (add)/1, 
   clr/1,
   db_op/2,
   ireq/1,
  del/1,  
  padd/2, padd/3, prop/3, prop_or/4, props/2, iprops/2, upprop/2,add_fast/1, ireq/1, mreq/1, upprop/1, req/1, 
  use_term_listing/2,  world_clear/1,  
   with_kb_assertions/2
  )).


:- dynamic_multifile_exported 
   tAgentGeneric/1,  mudEnergy/2,mudHealth/2, mudAtLoc/2, failure/2, typeGrid/3, gridValue/4, mudIsa/2, tItem/1, 
  mudMemory/2,  pathName/3, mudPossess/2,  tRegion/1, mudScore/2, mudStm/2,   mudFacing/2,
   % col/1,
   % localityOfObject/2,
   
  tThinking/1,   mudWearing/2, 
  %str/2,
  mudFacing/2, mudHeight/2, act_term/2, nameStrings/2, mudDescription/2, pathBetween/3, mudAgentTurnnum/2.

:- must(dbase_mod(user)).


% :-dynamic((weight/2)).
% :-dynamic(mudSubclass/2).
:-dynamic_multifile_exported(mudSubclass/2).

:- dynamic_multifile_exported mtForPred/2.


logical_functor(X):-atom(X),member(X,[',',';']).

:- dynamic stat_total/2.


%:- multifile predArgTypes/2.

%:- meta_predicate man:with_assertions(:,0).
%:- meta_predicate intersect(?,0,?,0,0,-).

% -  :- meta_predicate del(0),clr(0),add(0),add0(0),req(0), db_op(0,0,0).

% Found new meta-predicates in iteration 1 (0.281 sec)
%:- meta_predicate db_forall(?,?,?,0).


:- xperimental->if_file_exists(ensure_loaded(logicmoo('dbase/dbase_types_motel')));true.


:- register_module_type(utility).

user:agent_call_command(_Gent,actGrep(Obj)):- term_listing(Obj).


:-ensure_loaded(dbase_i_pldoc).
:-ensure_loaded(dbase_i_coroutining).
%:-discontiguous(prologSingleValued/2).
%:-discontiguous(prologMultiValued/1).
% =================================================================================================
% world database
% =================================================================================================


% TODO: canonicalize clauses first!
with_kb_assertions([],Call):- !,Call.
with_kb_assertions([With|MORE],Call):-!,with_kb_assertions(With,with_kb_assertions(MORE,Call)).
with_kb_assertions(With,Call):-
   setup_call_cleanup(asserta(With,Ref),Call,erase(Ref)).



world_clear(Named):-fmt('Clearing world database: ~q.~n',[Named]).


xtreme_debug(P):- is_release,!,nop(P).
xtreme_debug(P):- not_is_release, verify_sanity(P).
verify_sanity(P):- is_release,!,nop(P).
verify_sanity(P):- debugOnError(P),!.
verify_sanity(P):- dmsg('$ERROR_incomplete_SANITY'(P)),!.


:- meta_predicate hooked_asserta(^), hooked_assertz(^), hooked_retract(^), hooked_retractall(^).

:- meta_predicate del(-),clr(-),add(-),req(-), db_op(-,-).

% Found new meta-predicates in iteration 1 (0.281 sec)
%:- meta_predicate db_op_exact(?,?,?,0).

:- dynamic_multifile_exported(mudMoveDist/2).
:- dynamic(mudMoveDist/2).


% :- register_module_type(utility).
% ================================================
% db_reop/2  RE-CURSED and CHECKED 
% ================================================
expands_on(EachOf,Term):-subst(Term,EachOf,foooz,Term2),!,Term2\=Term,not((do_expand_args(EachOf,Term,O),O = Term)).
if_expands_on(EachOf,Term,Call):- expands_on(EachOf,Term),subst(Call,Term,O,OCall),!, forall(do_expand_args(EachOf,Term,O),OCall).

db_reop(WhatNot,Call) :- into_mpred_form(Call,NewCall),NewCall\=@=Call,!,db_reop(WhatNot,NewCall).

db_reop(Op,Term):- expands_on(isEach,Term), !,forall(do_expand_args(isEach,Term,O),db_reop_l(Op,O)).
db_reop(Op,Term):-db_reop_l(Op,Term).

db_reop_l(query(_HLDS,Must),Call) :- !,preq(Must,Call).
db_reop_l(OP,DATA):-no_loop_check(db_reop0(OP,DATA)).
db_reop0(change(assert,_),Call) :- !,add_fast(Call).
db_reop0(change(retract,one),Call) :- !,del(Call).
db_reop0(change(retract,all),Call) :- !,clr(Call).
db_reop0(OP,Call) :- dmsg(warn(db_reop(OP,Call))),db_op0(OP,Call).


% -  del(RetractOne) 
del(C0):- ireq(C0),!,idel(C0),!.
del(C0):- mreq(C0),!,mdel(C0),!.

idel(C0):- expire_dont_add,dmsg(idel(C0)),db_op_int(change(retract,one),C0), verify_sanity(ireq(C0)->(dmsg(warn(incomplete_I_DEL(C0))),fail);true),!.
idel(C0):- dmsg(warn(failed(idel(C0)))),!,fail.

mdel(C0):- expire_dont_add,dmsg(mdel(C0)),db_op_int(change(retract,one),C0), verify_sanity(mreq(C0)->(dmsg(warn(incomplete_M_DEL(C0))),fail);true),!.
mdel(C0):- dmsg(warn(failed(mdel(C0)))),!,fail.

% -  clr(Retractall)
clr(C0):- expire_dont_add,dmsg(clr(C0)),db_op_int(change(retract,all),C0),verify_sanity(ireq(C0)->(dmsg(warn(incomplete_CLR(C0))));true).

% -  preq(Query) = query with P note
preq(P,C0):- db_op_int(query(dbase_t,P),C0).

% -  req(Query) = Normal query
req(C0):- dmsg(req(C0)), preq(req,C0).

% -  mreq(Query) = Forced Full query
mreq(C0):- dmsg(mreq(C0)), rescan_module_ready,no_loop_check(with_assertions([-infInstanceOnly(_),-thlocal:infAssertedOnly(_),-thlocal:noRandomValues(_)],preq(must,C0))).


% -  ireq(Query) = Normal query (May not use second order logic) (must be asserted on isntance) (used mainly by 2nd order logic to avoid looping)
ireq(C0):- dmsg(ireq(C0)), rescan_module_ready,no_loop_check(with_assertions([+infInstanceOnly(_), +thlocal:infAssertedOnly(_),+thlocal:noRandomValues(_)],preq(ireq,C0))).

:-dmsg_hide(req).
:-dmsg_hide(ireq).

% -  props(Obj,QueryPropSpecs)
props(Obj,PropSpecs):- req(props(Obj,PropSpecs)).
iprops(Obj,PropSpecs):- ireq(props(Obj,PropSpecs)).


expire_dont_add:-dmsg(expire_dont_add),retractall(implied_dont_add(_)).

% -  add_fast(Assertion)
% add_fast(CO):- must_det((add_fast_unchecked(CO), xtreme_debug(once(ireq(CO);(with_all_dmsg((debug(blackboard),show_call(add_fast_unchecked(CO)),rtrace(add_fast_unchecked(CO)),dtrace(ireq(CO))))))))),!.
add_fast(CO:-BODY):- dmsg(add_fast(CO:-BODY)),must_det((add_fast_unchecked((CO:-BODY)))),!.
add_fast(Skipped):- ground(Skipped),implied_skipped(Skipped),!. %, dmsg(skip_add_fast(mudSubclass(CO,CO))).
add_fast(grid_key(KW=COL)):- add_fast(typeProps(COL,[mudKwLabel(KW)])).
add_fast(CO):- dmsg(add_fast(CO)),ignore((ground(CO),asserta(implied_dont_add(CO)))),!,must_det((add_fast_unchecked(CO), nop(xtreme_debug(ireq(CO)->true;dmsg(warn(failed_ireq(CO))))))),!.

implied_skipped(mudSubclass(CO,CO)).
implied_skipped(props(_,[])).
implied_skipped(Skipped):-implied_dont_add(Skipped).

:-dynamic(implied_dont_add/1).



:-dynamic_multifile_exported(add_fast_unchecked/1).
add_fast_unchecked((CO:-BODY)):-BODY==true,!,add_fast_unchecked(CO).
add_fast_unchecked((CO:-BODY)):-!,must_det(assertz_clause(CO,BODY)).
add_fast_unchecked(CO):-must_det(db_op(change(assert,add), CO)).

% -  upprop(Obj,PropSpecs) update the properties
upprop(Obj,PropSpecs):- upprop(props(Obj,PropSpecs)).
upprop(CO):- add(CO).
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

user:decl_database_hook(assert(_),mudFtInfo(FT,_)):- define_ft(FT).
% user:decl_database_hook(assert(_),mudSubclass(FT,OFT)):- define_ft(OFT),define_ft(FT).
% user:decl_database_hook(assert(_),mudSubclass(FT,OFT)):- formattype(OFT),dmsg(warning(subclass_of_define_ft(FT))).

dmsg_hook(transform_holds(dbase_t,_What,props(ttCreateable,[mudIsa(mudIsa),mudIsa]))):-trace_or_throw(dtrace).

% expand_goal_correct_argIsa(A,A):-simple_code,!.
expand_goal_correct_argIsa(A,B):- expand_goal(A,B).

% db_op_simpler(query(HLDS,_),MODULE:C0,req(call,MODULE:C0)):- atom(MODULE), nonvar(C0),not(not(predicate_property(C0,_PP))),!. % , functor_catch(C0,F,A), dmsg(todo(unmodulize(F/A))), %trace_or_throw(module_form(MODULE:C0)), %   db_op(Op,C0).
db_op_simpler(_,TypeTerm,props(Inst,[mudIsa(Type)|PROPS])):- TypeTerm=..[Type,Inst|PROPS],nonvar(Inst),hasInstance(macroDeclarer,Type),!.



db_op_simpler_wlc(query(HLDS,Must),Wild,Simpler):- !,call(call,db_op_simpler(query(HLDS,Must),Wild,Simpler)),not(is_loop_checked(req(Simpler))),!.
db_op_simpler_wlc(Op,Wild,Simpler):- !,call(call,db_op_simpler(Op,Wild,Simpler)),not(is_loop_checked(db_op0(Op,Simpler))),!.


db_op_sentence(_Op,Prop,ARGS,C0):- atom(Prop),!, C0=..[Prop|ARGS].
db_op_sentence(_Op,Prop,ARGS,C0):- C0=..[dbase_t,Prop|ARGS].


generated_fact(Fact):- ground(Fact),!,req(Fact).
generated_fact(Fact):- (compound(Fact)-> true ; mpred_arity(F,A)), functor(Fact,F,A),Fact=..[F|Args],
   generate_args(F,1,A,Args).

generate_args(F,N,N,[Arg|_]):-!,generate_fact_arg(F,N,Arg).
generate_args(F,N,Until,[Arg|More]):- generate_fact_arg(F,N,Arg), N2 is N+1, generate_args(F,N2,Until,More).

generate_fact_arg(F,N,Arg):-argIsa_call(F,N,Type),!,isa_arg_test(Arg,Type).

isa_arg_test(Arg,Type):-ground(Arg),!,show_call(isa_backchaing(Arg,Type)).
isa_arg_test(Arg,Type):-isa_backchaing(Arg,Type).

equivRule_call(A,B):- is_asserted(holds_t(ruleEquiv,A,B)).
equivRule_call(A,B):- is_asserted(holds_t(ruleEquiv,B,A)).

forwardRule_call(A,B):- is_asserted(holds_t(forwardRule,B,A)).

:-dynamic_multifile_exported(ruleEquiv/2).

good_for_chaining(_,_):-!.
good_for_chaining(_Op,Term):-not(contains_singletons(Term)).
db_rewrite(_Op,Term,NewTerm):-equivRule_call(Term,NewTerm).
db_rewrite(_Op,Term,NewTerm):-forwardRule_call(Term,NewTerm).

:-dynamic_multifile_exported(simply_functors/3).
simply_functors(Db_pred,query(HLDS,Must),Wild):- once(into_mpred_form(Wild,Simpler)),Wild\=@=Simpler,!,call(Db_pred,query(HLDS,Must),Simpler).
simply_functors(Db_pred,Op,Wild):- once(into_mpred_form(Wild,Simpler)),Wild\=@=Simpler,!,call(Db_pred,Op,Simpler).


% -  dmsg_hook(db_op(query(HLDS,call),holds_t(ft_info,col,'$VAR'(_)))):-trace_or_throw(dtrace).

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
univ_left(Comp,[P|List]):-dbase_mod(DBASE), univ_left0(DBASE,Comp,[P|List]),!.
univ_left0(M,M:Comp,List):- Comp=..List,!.

user:decl_database_hook(AR,C):- record_on_thread(dbase_change,changing(AR,C)).

record_on_thread(Dbase_change,O):- thread_self(ID),thlocal:dbase_capture(ID,Dbase_change),!,Z=..[Dbase_change,ID,O],assertz(Z).


% ================================================
% db_op/2
% ================================================

db_op(Op,Term):- expands_on(isEach,Term),!,forall(do_expand_args(isEach,Term,O),db_op(Op,O)).
db_op(change(_,_),props(_Obj,Props)):- Props ==[], !.
db_op(query(_,Must),NC):- not(compound(NC)),!,call_expanded_for(Must,NC).
db_op(query(Dbase_t, Req), must(Call)):-!, must(db_op(query(Dbase_t, Req), Call)).
db_op(query(_Dbase_t, _Req), prologCall(Call)):-!, call(Call).
db_op(query(HLDS,Must),B):-!, loop_check_term(db_op0(query(HLDS,Must),B),req(B),((loop_check_term(call_mpred(B),req2(B),show_call(is_asserted(B)))))). 
db_op(change(assert,CB2),B):- !,loop_check(db_op0(change(assert,CB2),B),must((dmsg(warn(looping(db_op(change(assert,CB2),B)))),hooked_asserta(B)))),!. % true = we are already processing this assert
db_op(change(retract,CB2),B):- !,loop_check(db_op0(change(retract,CB2),B),must((dmsg(warn(looping(db_op(change(retract,CB2),B)))),hooked_retract(B)))),!. % true = we are already processing this retract
db_op(Op,Term):- loop_check_local(db_op0(Op,Term),trace_or_throw(loop_check(db_op0(Op,Term)))).

% ================================================
% db_op0/2  SIMPLISTIC REWRITE (this is not the PRECANONICALIZER)
% ================================================
:-moo_hide_childs(db_op0/2).

db_op0(query(HLDS,Must),Term):- expands_on(isEach,Term), !, forall(do_expand_args(isEach,Term,O),no_loop_check(db_reop(query(HLDS,Must),O))).
db_op0(Op,Term):- expands_on(isEach,Term), !,forall(do_expand_args(isEach,Term,O),db_op0(Op,O)).

db_op0(change(assert,add),mudIsa(ArgTypes,PrologMultiValued)):- compound(ArgTypes), 
   atom(PrologMultiValued), 
   hasInstance(macroDeclarer,PrologMultiValued),
   get_functor(ArgTypes,F,A),
   assert_arity(F,A),
   must(not(((PrologMultiValued=predProxyRetract)))),
   decl_mpred(F,[predArity(A),PrologMultiValued,predArgTypes(ArgTypes)]),!.

db_op0(Op,A):- ground(A),A=mudIsa(I,C), must_det(once(correctArgsIsa(Op,I,II))),I\=II,!,db_op0(Op,mudIsa(II,C)).

db_op0(Op,G):- when_debugging(blackboard,dmsg(db_op0(Op,G))),fail.
db_op0(query(_HLDS,Must),mudIsa(I,Type)):- !,call_expanded_for(Must,isa_backchaing(I,Type)).
db_op0(_,end_of_file):-!.
db_op0(Op,Term):- stack_check,var(Term),!,trace_or_throw(nc(db_op0(Op,Term))).
%db_op0(Op,props(Obj,nameStrings(Str))):-!, db_op0(Op,nameStrings(Obj,Str)).

db_op0(Op,MT:Term):- is_kb_module(MT),!,with_assertions(thlocal:caller_module(kb,MT),db_reop(Op,Term)).
db_op0(Op,DB:Term):- dbase_mod(DB),!,with_assertions(thlocal:caller_module(db,DB),db_reop(Op,Term)).
db_op0(Op,KB:Term):- atom(KB),!,with_assertions(thlocal:caller_module(prolog,KB),db_reop(Op,Term)).

% db_op0(Op,(':-'(A))):- must((expand_goal_correct_argIsa(A,AA))),expanded_different(A,AA),!,db_reop(Op, (':-'(AA))).

db_op0(Op,[dbase_t,Class,Inst]):-!,db_reop(Op,mudIsa(Inst,Class)).
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

db_op0(change(assert,add),mudIsa(ArgTypes,PrologMultiValued)):- compound(ArgTypes), 
   atom(PrologMultiValued), 
   hasInstance(macroDeclarer,PrologMultiValued),
   get_functor(ArgTypes,F,A),must(not(((PrologMultiValued=predProxyRetract)))),
   decl_mpred(F,[predArity(A),PrologMultiValued,predArgTypes(ArgTypes)]),!.

db_op0(Op,props(Obj,Open)):- var(Open),!,trace_or_throw(db_reop(Op,props(Obj,Open))).
db_op0(_Op,props(_Obj,[])):- !.
db_op0(Op,props(Obj,[P])):- nonvar(P),!,db_reop(Op,props(Obj,P)).
db_op0(Op,props(Obj,[P|ROPS])):- !,db_reop(Op,props(Obj,P)),db_op0(Op,props(Obj,ROPS)).
db_op0(Op,props(Obj,PropVal)):- atom(PropVal),!,Call=..[PropVal,Obj],!,db_reop(Op,Call).
db_op0(Op,props(Obj,PropVal)):- safe_univ(PropVal,[Prop,NonVar|Val]),Obj==NonVar,!,db_reop(Op,[dbase_t,Prop,Obj|Val]).
db_op0(Op,props(Obj,PropVal)):- PropVal=..[OP,Pred|Val],comparitiveOp(OP),not(comparitiveOp(Pred)),!,OPVAL=..[OP|Val],PropVal2=..[Pred,OPVAL],db_reop(Op,props(Obj,PropVal2)).
db_op0(Op,props(Obj,PropVal)):- PropVal=..[Prop|Val],not(infix_op(Prop,_)),!,db_reop(Op,[dbase_t,Prop,Obj|Val]).
db_op0(Op,props(Obj,PropVal)):- PropVal=..[Prop|Val],!,trace_or_throw(dtrace),db_reop(Op,[dbase_t,Prop,Obj|Val]).

db_op0(Op,pddlSomethingIsa(A,List)):- !,forall_member(E,List,must(db_reop(Op, mudIsa(A,E)))).
db_op0(Op,pddlDescription(A,List)):- !,forall_member(E,List, must(db_reop(Op, mudDescription(A,E)))).
db_op0(Op,pddlObjects(Type,List)):- !,forall_member(I,List,must(db_reop(Op,mudIsa(I,Type)))).
db_op0(Op,pddlSorts(Type,List)):- !,forall_member(I,List,must(db_reop(Op, mudSubclass(I,Type)))).
db_op0(Op,pddlTypes(List)):- !,forall_member(I,List,must(db_reop(Op, mudIsa(I,tCol)))).
db_op0(Op,pddlPredicates(List)):- !,forall_member(T,List,must(db_reop(Op,tPred(T)))).
db_op0(Op,typeProps(Type,List)):- fail,addTypeProps_getOverlap(Type,List,Overlap),!,db_op(Op,typePropSet(Type,Overlap)).
db_op0(Op,EACH):- EACH=..[each|List],forall_member(T,List,must(db_reop(Op,T))).
db_op0(change(assert,_),mpred_prop(F,A)):- !,must(decl_mpred(F,A)).

db_op0(Op,db_op_exact(Term)):- !,db_op_exact(Op,Term).
 
% use assert_with_pred db_op0(change(assert,_)),description(A,E)):- once(must(add_description(A,E))),fail. % db_reop(change(CA1,CB2),descriptionHere(A,E)).
db_op0(Op,nameStrings(A,S0)):- nonvar(S0),determinerRemoved(S0,String,S),!,db_reop(Op, nameStrings(A,S)),add(determinerString(A,String)).

db_op0(Op,A):- must_det(once(correctArgsIsa(Op,A,AA))),
 acceptable_xform(  A , AA),dmsg(correctArgsIsa(Op,A->AA)), !, db_reop(Op,AA).

db_op0(Op,props(Obj,nameStrings(Str))):-!,db_reop(Op,nameStrings(Obj,Str)).
db_op0(query(HLDS,Must),mudIsa(Term,Var)):- !,call_expanded_for(query(HLDS,Must),call(call,isa_backchaing(Term,Var))).
% db_op0(Op,mudIsa(A,SubType)):- dbase_t( cre ateableSubclassType,SubType,Type),!,db_reop(Op,mudIsa(A,Type)),db_reop(Op,mudIsa(A,SubType)).
db_op0(query(HLDS,Must),argIsa(P,N,T)):- call_expanded_for(query(HLDS,Must),(get_mpred_prop(P,predArgTypes(ArgsIsa)),arg(N,ArgsIsa,T),must(nonvar(T)))).

db_op0(change(_,_),Term):- glean_pred_props_maybe(Term),fail.


db_op0(Op,Wild):- into_mpred_form(Wild,Simpler), acceptable_xform( Wild , Simpler),dmsg(into_mpred_form(Op,Wild->Simpler)), !,db_reop(Op,Simpler).

db_op0(Op,Wild):- transform_holds(dbase_t,Wild,Simpler),acceptable_xform(  Wild , Simpler),!,dmsg(transform_holds(Op,Wild->Simpler)),db_reop(Op,Simpler).
% db_op0(Op,Term):- call(call,good_for_chaining(Op,Term)), db_rewrite(Op,Term,NewTerm),not(contains_singletons(NewTerm)),db_reop(Op,NewTerm).
% db_op0(Op,Wild):- dsfdf db_op_simpler_wlc(Op,Wild,Simpler),!,db_reop(Op,Simpler).

db_op0(change(DIR,_),predArgTypes(Term)):- must(DIR==assert),!,show_call(decl_mpred(Term)),!.

db_op0(change(DIR,_),mudIsa(T,Type)):- must(DIR==assert),!,assert_isa(T,Type),!.

db_op0(Op,Term):- type_error_checking,!, Term =..[Type,A],!,db_reop(Op,mudIsa(A,Type)).

db_op0(Op,C0):- C0=..[Prop|ARGS],db_op_unit(Op,C0,Prop,ARGS).


% ================================================
% db_op_unit/3
% ================================================

db_op_unit(Op,_C0,Prop,ARGS):- type_error_checking,!, cached_isa(Prop,tCol),trace_or_throw(db_op_unit(Op,tCol(Prop),ARGS)).

db_op_unit(Op,C0,mudIsa,ARGS):- type_error_checking,!, trace_or_throw(db_op_unit(Op,mudIsa(C0),ARGS)).

% use_db_op/1
db_op_unit(Op,C0,Prop,_ARGS):- get_mpred_prop(Prop,use_db_op(Other)),!,call(Other,Op,C0).

% assert_with_pred/1
db_op_unit(change(assert,A),C0,Prop,_RGS):- get_mpred_prop(Prop,predProxyAssert(How)),!, must(nonvar(How)), once(ignore((call(How,C0), run_database_hooks(assert(A),C0)))).

% predProxyRetract/1
db_op_unit(change(retract,A),C0,Prop,_RGS):- get_mpred_prop(Prop,predProxyRetract(How)),!, must(nonvar(How)), once(ignore((call(How,C0), run_database_hooks(retract(A),C0)))).


% db_op_unit(query(HLDS,Must),_C0,Prop,ARGS):- bad_idea, get_mpred_prop(Prop,extentAsserted),!,call_expanded_for(query(HLDS,Must),dbase_t_p2(Prop,ARGS)).

% predProxyQuery/1
db_op_unit(query(Must,HLDS),C0,Prop,_RGS):- get_mpred_prop(Prop,predProxyQuery(How)),!, call_expanded_for(query(Must,HLDS),call(How,C0)).


% plain prop
db_op_unit(Op,_C0,Prop,ARGS):- once((db_op_sentence(Op,Prop,ARGS,Unit),same_vars(ARGS,Unit))), db_op_exact(Op,Unit).

% if is IREQ then fail
db_op_unit(Op,C0,_Prop,_ARGS):- test_tl(thlocal:infInstanceOnly,C0),!,must_det(query(_Must,_HLDS)=Op),fail.

% genlInverse/2
%db_op_unit(Op,_C0,Prop,ARGS):- dbase_t(genlInverse,Prop,Other), inverse_args(ARGS,Inverse), db_op_sentence(Op,Other,Inverse,Unit1), db_op_exact(Op,Unit1).

% genlPreds/2
%db_op_unit(Op,_C0,Prop,ARGS):- dbase_t(genlPreds,Prop,Other),  db_op_sentence(Op,Other,ARGS,Unit1), db_op_exact(Op,Unit1).

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
db_op_exact(change(assert,Must),C0):- db_quf(change(assert,Must),C0,U,C),!,must(call_mpred_fast(U)),functor_catch(C,F,A),( get_mpred_prop(F,prologSingleValued) -> must(db_assert_sv(Must,C,F,A)) ; must(db_assert_mv(Must,C,F,A))).
db_op_exact(change(assert,Must),C):- trace_or_throw(dtrace),functor_catch(C,F,A), must_det((get_mpred_prop(F,prologSingleValued) -> must_det(db_assert_sv(assert(Must),C,F,A)) ; must(db_assert_mv(assert(Must),C,F,A)))).
%db_op_exact(Must, Term):- !,call_expanded_for(Must,Term).
db_op_exact(Op,C):- trace_or_throw(unhandled(db_op_exact(Op,C))).

addTypeProps_getOverlap(_Type,List,Overlap):-!,List=Overlap.


% ================================================
% db_assert_[mv|sv]/3
% ================================================

:-dmsg_hide(db_assert_mv).
:-dmsg_hide(db_assert_sv).

:-dmsg_hide(db_op_exact).
:-dmsg_hide(add).

:-dmsg_hide(into_mpred_form).

% assert_with to change(CA1,CB2) mutlivalue pred
:-dynamic_multifile_exported((db_assert_mv/4)).
db_assert_mv(_Must,end_of_file,_,_):-!.
% db_assert_mv(_Must,C,_F,_A):- hooked_assertz(C),!.
db_assert_mv(Must,C,F,A):- test_tl(thlocal:adding_from_srcfile), dmsg(db_assert_mv(Must,C,F,A)), hooked_assertz(C).
db_assert_mv(Must,C,F,A):- dmsg(db_assert_mv(Must,C,F,A)), must_det((mpred_prop(F,prologOrdered) -> hooked_assertz(C) ; hooked_asserta(C))).


% assert_with to change(CA1,CB2) singlevalue pred
:-dynamic_multifile_exported((db_assert_sv/4)).
%db_assert_sv(_Must,C,F,A):- throw_if_true_else_fail(contains_singletons(C),db_assert_sv(C,F,A)).
db_assert_sv(Must,C,F,A):- ignore(( loop_check(db_assert_sv_lc(Must,C,F,A),true))).

:-dynamic_multifile_exported((db_assert_sv_lc/4)).
db_assert_sv_lc(Must,C,F,A):- arg(A,C,UPDATE),db_assert_sv_now(Must,C,F,A,UPDATE),!.

:-dynamic_multifile_exported(db_assert_sv_now/5).
db_assert_sv_now(Must,C,F,A, UPDATE):- has_free_args(db_assert_sv_now(Must,C,F,A, UPDATE)),!,trace_or_throw(var_db_assert_sv_now(Must,C,F,A, UPDATE)).
db_assert_sv_now(Must,C,F,A, UPDATE):- number(UPDATE),UPDATE<0, !,db_assert_sv_update(Must,C,F,A,UPDATE).
db_assert_sv_now(Must,C,F,A,+UPDATE):-!, db_assert_sv_update(Must,C,F,A,+UPDATE).
db_assert_sv_now(Must,C,F,A,-UPDATE):-!, db_assert_sv_update(Must,C,F,A,-UPDATE).
db_assert_sv_now(Must,C,F,A, REPLACE):- db_assert_sv_replace(Must,C,F,A, REPLACE).

:-dynamic_multifile_exported(db_assert_sv_update/5).
db_assert_sv_update(Must,C,F,A,UPDATE):-
   replace_arg(C,A,OLD,COLD),
   % prefer updated values to come from instances but will settle with anything legal
   must_det((once(ireq(COLD);mreq(COLD)),ground(COLD))),
   update_value(OLD,UPDATE,NEW),!,
   db_assert_sv_replace(Must,C,F,A,NEW),!.

:-dynamic_multifile_exported(db_assert_sv_replace/5).

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
:-ensure_loaded(dbase_i_call_kb).

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

compute_value_no_dice(NEW,NEW):- compound(NEW),functor_catch(NEW,ftDice,_),!.
compute_value_no_dice(NEW,NEWV):-compute_value(NEW,NEWV).

compute_value(NEW,NEWV):-catch(NEWV is NEW,_,fail),!.
compute_value(NEW,NEWV):-catch(any_to_value(NEW,NEWV),_,fail),!.
compute_value(NEW,NEW).

insert_into(ARGS,0,Insert,[Insert|ARGS]):- !.
insert_into([Carry|ARGS],After,Insert,[Carry|NEWARGS]):- 
   After1 is After - 1,
   insert_into(ARGS,After1,Insert,NEWARGS).

user:hook_coerce(Text,tPred,Pred):- mpred_prop(Pred,predArity(_)),name_text(Pred,Text).

:- multifile(mudToHitArmorClass0 / 2).

:- dynamic_safe(mudToHitArmorClass0 / 2).


/*
:-dynamic_multifile_exported(makeConstant/1).
makeConstant(X):-trace_or_throw(makeConstant(X)).
:-dynamic_multifile_exported(cycAssert/2).
cycAssert(A,B):-trace_or_throw(cycAssert(A,B)).
*/

:- ensure_loaded(dbase_i_term_expansion).

:- ensure_loaded(dbase_i_db_preds).

:- ensure_loaded(dbase_i_cyc).



:-dynamic_multifile_exported(forall_setof/2).
forall_setof(ForEach,Call):-
   findall(ForEach,ForEach,ForEachAll),
   list_to_set(ForEachAll,Set),!,
   ignore(forall(member(ForEach,Set),Call)).

:-dynamic_multifile_exported((was_imported_kb_content/2)).

:-dynamic_multifile_exported(add_later/1).
add_later(Fact):- call_after_game_load(add(Fact)).

:-decl_thlocal add_thread_override/1.
% thlocal:add_thread_override(A):-add_from_macropred(A),!.

:-dynamic_multifile_exported(((add)/1)).
:-moo_hide_childs((add)/1).
add(A):- A==end_of_file,!.
add(A):- not(compound(A)),!,trace_or_throw(not_compound(add(A))),!.
add(Call) :- into_mpred_form(Call,NewCall),NewCall\=@=Call,!,add(NewCall).
add(Term):- expands_on(isEach,Term), !,forall(do_expand_args(isEach,Term,O),add(O)),!.
add(Call):- loop_check(thlocal:add_thread_override(Call)),!.
add(Call):- add_from_macropred(Call),!.
add(M:HB):-atom(M),!, must_det(add(HB)),!.
add(tCol(A)):- must_det(decl_type(A)),!.
add(A):-must(add_fast(A)),!.
add(A):-trace_or_throw(fmt('add is skipping ~q.',[A])).

:-dynamic_multifile_exported(begin_prolog_source/0).
:-dynamic_multifile_exported(end_prolog_source/0).
begin_prolog_source:- must_det(asserta(thlocal:in_prolog_source_code)).
end_prolog_source:- must_det(retract(thlocal:in_prolog_source_code)).

:-dynamic_multifile_exported(add_from_macropred/1).
add_from_macropred(C):- loop_check(add_from_macropred_lc(C),((dmsg(loopING_add_from_macropred(C)),add_fast(C)))).
add_from_macropred_lc(A):-A==end_of_file,!.
add_from_macropred_lc(Term):- expands_on(isEach,Term), !,forall(do_expand_args(isEach,Term,O),add_from_macropred_lc(O)).
add_from_macropred_lc(A):- not(compound(A)),!,trace_or_throw(not_compound(add_from_macropred_lc(A))).
add_from_macropred_lc(':-'(A)):- predicate_property(A,_),!,must(logOnFailure(show_call(A))),!.
add_from_macropred_lc(':-'(ensure_loaded(A))):- add(':-'(load_data_file(A))),!.
add_from_macropred_lc(':-'(A)):- dmsg(trace_or_throw(missing_directive(A))),!.
add_from_macropred_lc(':-'(Head,true)):- !, add(Head).
add_from_macropred_lc(':-'(Head,Body)):- must_det(assertz_clause(Head,Body)),!.
add_from_macropred_lc(RDF):- RDF=..[SVO,S,V,O],is_svo_functor(SVO),!,must_det(add(dbase_t(V,S,O))).
add_from_macropred_lc(A):- into_mpred_form(A,F), A \=@=F,!,add_from_macropred(F). 
add_from_macropred_lc(ClassTemplate):- compound(ClassTemplate), ClassTemplate=..[typeProps,Type|Props],
   assert_isa(Type,ttCreateable),
   assert_isa(Type,tCol),
   add(mudSubclass(Type,tItem)),   
   flatten(Props,AllProps),!,
   show_call(add(typeProps(Type,AllProps))).

add_from_macropred_lc(M:HB):-atom(M),!,add_from_macropred_lc(HB).





assertOnLoad(X):-add_later(X).

setTemplate(X):-add(X).

englishServerInterface(SomeEnglish):-dmsg(todo(englishServerInterface(SomeEnglish))).

:-multifile(user:call_OnEachLoad/1).
:-export(user:call_OnEachLoad/1).
:-dynamic(user:call_OnEachLoad/1).

:-dynamic_multifile_exported(onLoad/1).
onLoad(C):-call_after_game_load(C).
:-dynamic_multifile_exported(user:onEachLoad/1).
onEachLoad(C):-assert_if_new(user:call_OnEachLoad(C)).

call_OnEachLoad:-forall(call_OnEachLoad(C),doall(C)).

onSpawn(ClassFact):- ClassFact=..[Funct,InstA],createByNameMangle(InstA,Inst,Type2),assert_isa(Type2,ttCreateable),assert_isa(Inst,Funct),assert_isa(Inst,Type2),!.
onSpawn(ClassFact):- ClassFact=..[Funct|InstADeclB],must_det(onSpawn_f_args(Funct,InstADeclB)).

onSpawn_f_args(Funct,List):-
  with_assertions(deduceArgTypes(Funct),
   (convertSpawnArgs(Funct,1,List,NewList),
     Later =.. [dbase_t,Funct|NewList],
     add(Later),
   call_after_game_load(with_assertions(deduceArgTypes(Funct), add(Later))))),!.

convertSpawnArgs(_,_,[],[]).
convertSpawnArgs(Funct,N,[A|List],[O|NewList]):-
  convertOneSpawnArg(Funct,N,A,O),!,
  N2 is N + 1,
  convertSpawnArgs(Funct,N2,List,NewList).

convertOneSpawnArg(_,_,O,O):-string(O),!.
convertOneSpawnArg(_,_,O,O):-number(O),!.
convertOneSpawnArg(_,_,nospawn(O),O):-!.
convertOneSpawnArg(Funct,N,actSpawn(A),O):-spawnOneSpawnArg(Funct,N,A,O).
convertOneSpawnArg(Funct,N,A,O):-spawnOneSpawnArg(Funct,N,A,O).

spawnOneSpawnArg(Funct,N,A,O):-
  createByNameMangle(A,O,TypeA),
  assert_subclass_on_argIsa(Funct,N,TypeA),
  assert_isa(TypeA,ttCreateable).


createByNameMangle(InstA,IDA,InstAO):-must(createByNameMangle0(InstA,IDA,InstAO)),!.

createByNameMangle0(InstA,InstA,Type):-compound(InstA),InstA=..[Type|Props],assert_isa(InstA,Type),with_assertions(deduceArgTypes(_),padd(InstA,Props)).
createByNameMangle0(InstA,Inst,Type):- compound(InstA),!,functor_catch(InstA,Type,A),must(A==1),assert_isa(InstA,Type),InstA=Inst.
createByNameMangle0(InstA,_,_Type):- not(atom(InstA)),!,trace_or_throw(todo(not_atom_createByNameMangle(InstA))).
createByNameMangle0(Suggest,InstA,Type):- once(split_name_type(Suggest,InstA,Type)),Suggest==InstA,assert_isa(InstA,Type).
createByNameMangle0(OType,InstA,Type):- must(var(InstA)),i_name(t,OType,Type),atom_concat(Type,'7',InstA7),i_name(i,InstA7,InstA),must_det(assert_isa(InstA,Type)), 
 call_after_game_load(create_instance(InstA,Type)).
createByNameMangle0(InstA,IDA,InstA):- gensym(InstA,IDA), englishServerInterface([actCreate,InstA,IDA]).

wfAssert(X):-add(X). %  add_later(X).


add_arg_parts_of_speech(_F,_N,[],[]).
add_arg_parts_of_speech(F,N,[A|ARGS0],[ARG|ARGS]):-argIsa_call_or_undressed(F,N,A,ARG),N1 is N+1, add_arg_parts_of_speech(F,N1,ARGS0,ARGS).

argIsa_call_or_undressed(F,N,Obj,fN(Obj,Type)):- argIsa_call_0(F,N,Type),!.
argIsa_call_or_undressed(_F,_N,Obj,Obj).

verb_after_arg(_,_,1).


:- with_no_term_expansions(if_file_exists(user_ensure_loaded(logicmoo(dbase/dbase_i_rdf_store)))).

:- style_check(+discontiguous).
:- style_check(-discontiguous).

:-dynamic_multifile_exported(thlocal:in_dynamic_reader/1).

:-dynamic_multifile_exported(begin_dynamic_reader/0).
begin_dynamic_reader:-  dynamic_multifile_exported((was_imported_kb_content/2)),
  !. %  must_det(( prolog_load_context(file,Source),asserta(thlocal:in_dynamic_reader(Source)))).
:-dynamic_multifile_exported(end_dynamic_reader/0).
end_dynamic_reader:-  
  !. % must_det(( prolog_load_context(file,Source),retract(thlocal:in_dynamic_reader(Source)))).

:-dynamic_multifile_exported(inside_dynamic_reader/0).
inside_dynamic_reader :- prolog_load_context(file,Source),test_tl(thlocal:in_dynamic_reader(Source)),!.
inside_dynamic_reader :- prolog_load_context(source,Source),test_tl(thlocal:in_dynamic_reader(Source)),!.

user:term_expansion(CL,EXP):-
 not(thlocal:adding_from_srcfile),
 not(thlocal:into_form_code), not((get_functor(CL,F),F=was_imported_kb_content)),
 % ==== why we assert
   not(requires_storage(CL)),inside_dynamic_reader,
% ==== do it
   dmsg(assertz_inside_dynamic_reader(CL)),ignore(is_compiling_sourcecode),with_assertions(thlocal:adding_from_srcfile,must_det(add(CL))),!,
   must(EXP=was_imported_kb_content(inside_dynamic_reader,CL)).

user:term_expansion(CL,EXP):-not(thlocal:into_form_code),not(thlocal:adding_from_srcfile),
   not((get_functor(CL,F),F=was_imported_kb_content)),
% ==== why we assert
   requires_storage(CL),  not(inside_dynamic_reader), 
% ==== do it
   dmsg(addingGaf(CL)),ignore(is_compiling_sourcecode),with_assertions(thlocal:adding_from_srcfile,must_det(add(CL))),!,
   must(EXP=was_imported_kb_content(requires_storage,CL)).

user:term_expansion((H:-B),Out):- fail,test_tl(enable_src_loop_checking),
   with_assertions(thlocal:adding_from_srcfile,
    once((once(make_body_clause(H,B,NewBody)),(B \=@= NewBody),!,dmsg(thlocal:enable_src_loop_checking((H:-NewBody)))))),Out=(H:-NewBody).

% load_motel:- defrole([],time_state,restr(time,period)).
% :-load_motel.

:- ensure_loaded(logicmoo('vworld/moo_footer.pl')).

user:agent_text_command(_Agent,_Text,_AgentTarget,_Cmd):-fail.
/*
    coerce/3,
    rescan_dbase_facts/0,
          enter_term_anglify/2,
           user:agent_text_command/4,         
*/



:- with_no_term_expansions(if_file_exists(user_ensure_loaded(logicmoo(mobs/planner/dbase_i_hyhtn)))).


:-'$hide'(rescan_dbase_ops/0).
:-'$hide'(do_db_op_hooks/0).
%:- rescan_missing_stubs.
%:- rescan_mpred_props.

