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

:- '@'(use_module(logicmoo(vworld/moo)),'user').
:- '@'(use_module(dbase_formattypes),'user').

:- export((
 add/1, add0/1, agent/1, agent_doing/2, agent_done/2, argIsa_call/3, charge/2, ofclass/2, clr/1, damage/2, db_op/2, atloc/2, 
 mpred_prop_g/1, mpred_prop_game_assert/1, del/1, failure/2, grid/4, inRegion/2, isa/2, item/1, 
 memory/2, padd/2, padd/3, pathName/3, possess/2, prop/3, prop_or/4, props/2, region/1, req/1, scan_mpred_prop/0, score/2, stm/2, term_listing/1,  facing/2,
 thinking/1, type/1, use_term_listing/2, wearing/2, world_clear/1, str/2 ,facing/2, height/2, act_term/2, nameStrings/2, description/2, pathBetween/3, agent_turnnum/2,
 dbase_mod/1,
 clause_present_1/3,
 object/1,
 with_kb_assertions/2
    )).

:- dynamic 
 dbase_mod/1, object/1,
   add/1, add0/1, agent/1, agent_doing/2, agent_done/2, argIsa_call/3, charge/2, ofclass/2, clr/1, damage/2, db_op/2, atloc/2,
 mpred_prop_g/1, mpred_prop_game_assert/1, del/1, failure/2, grid/4, isa/2, item/1, 
 memory/2, padd/2, padd/3, pathName/3, possess/2, prop/3, prop_or/4, props/2, region/1, req/1, scan_mpred_prop/0, score/2, stm/2, term_listing/1, facing/2,
 thinking/1, type/1, use_term_listing/2, wearing/2, world_clear/1, str/2 ,facing/2, height/2, act_term/2, nameStrings/2, description/2, pathBetween/3, agent_turnnum/2.

:- dbase_mod(M),dynamic_multifile_exported((
          M:dbase_t/1,
          M:dbase_t/2,
          M:dbase_t/3,
          M:dbase_t/4,
          M:dbase_t/5,
          M:dbase_t/6,
          M:dbase_t/7)).

:-dynamic(weight/2).
:-dynamic(movedist/2).
:-export(movedist/2).
:-dynamic(subclass/2).
:-export(subclass/2).

:-multifile inRegion/2.
dbase_mod(moo).
/*

:- context_module(M),
   asserta(dbase_mod(M)),
   dmsg(assert_if_new(dbase_mod(M))).

*/

%:- multifile argsIsa/2.

:- meta_predicate man:with_assertions(:,0).
:- meta_predicate world:intersect(?,0,?,0,0,-).
:- meta_predicate clause_present(:), db_assert_mv(+,+,+), db_assert_sv(+,+,+), db_forall(+,+), db_forall_quf(+,+,+).

:- meta_predicate hooked_asserta(^), hooked_assertz(^), hooked_retract(^), hooked_retractall(^).
%% :- meta_predicate del(0),clr(0),add(0),add0(0),req(0), db_op(0,0,0).

% Found new meta-predicates in iteration 1 (0.281 sec)
%:- meta_predicate db_forall(?,?,?,0).

:- include(logicmoo('vworld/moo_header.pl')).

% :- include('dbase_types_motel').

:- user_use_module('dbase_rules_pttp').

:- register_module_type(utility).

moo:action_info(list(term)).
moo:agent_call_command(_Gent,list(Obj)):- term_listing(Obj).

% ================================================================================
% begin holds_t
% ================================================================================
:-export((holds_t/1,holds_t/2,holds_t/3,holds_t/4,holds_t/5,holds_t/6,holds_t/7,holds_t/8)).
holds_t(P,A1,A2,A3,A4,A5,A6,A7):- req(p(P,A1,A2,A3,A4,A5,A6,A7)).
holds_t(P,A1,A2,A3,A4,A5,A6):- req(p(P,A1,A2,A3,A4,A5,A6)).
holds_t(P,A1,A2,A3,A4,A5):- req(p(P,A1,A2,A3,A4,A5)).
holds_t(P,A1,A2,A3,A4):- req(p(P,A1,A2,A3,A4)).
holds_t(P,A1,A2,A3):- req(p(P,A1,A2,A3)).
holds_t(P,A1,A2):- req(p(P,A1,A2)).
holds_t(P,A1):- req(p(P,A1)).
holds_t(G):- req(G).


:-include(dbase_i_pldoc).
:-include(dbase_i_coroutining).
:-export(makeConstant/1).
makeConstant(X):-trace_or_throw(makeConstant(X)).
:-export(cycAssert/2).
cycAssert(A,B):-trace_or_throw(cycAssert(A,B)).
:-discontiguous(singleValued/2).
:-discontiguous(multiValued/1).
% =================================================================================================
% world database
% =================================================================================================


channel(A):-region(A).
channel(A):-agent(A).
channel(gossup).

moo:subclass(agent,spatialthing).
moo:subclass(region,spatialthing).
moo:subclass(object,spatialthing).
moo:subclass(item,spatialthing).


% replaced the 1st with the 2nd and better version of retract
% del(C0):-db_op(r,C0)
%% del(RetractOne)    <--  del(C0):- ignore((db_op('q',C0),!,db_op('r',C0))).
del(C0):- db_op('q',C0) -> db_op('ra',C0) ; dmsg(failure(del(C0))).
%% clr(Retractall)
clr(C0):-db_op(ra,C0).
%% req(Query)
req(C0):- db_op(q,C0).
%% props(Obj,QueryPropSpecs)
props(Obj,PropSpecs):-db_op0('q',props(Obj,PropSpecs)).
%% add(Assertion)
add0(C0):- must(db_op(a,C0)).
add(C0):- add0(C0).
%% padd(Obj,PropSpecs)
padd(Obj,PropSpecs):-add0(props(Obj,PropSpecs)).
%% padd(Obj,Prop,Value)
padd(Obj,Prop,Value):- must(atom(Prop)), PropValue=..[Prop,Value],!,padd(Obj,PropValue).
%% prop(Obj,Prop,Value)
prop(Obj,Prop,Value):- must(atom(Prop)), C=..[Prop,Obj,Value],!,req(C).
%% prop_or(Obj,Prop,Value)
prop_or(Obj,Prop,Value,OrElse):- once(prop(Obj,Prop,Value);Value=OrElse).

% TODO: canonicalize clauses first!
with_kb_assertions([],Call):- !,Call.
with_kb_assertions([With|MORE],Call):-!,with_kb_assertions(With,with_kb_assertions(MORE,Call)).
with_kb_assertions(With,Call):-
   setup_call_cleanup(asserta(With,Ref),Call,erase(Ref)).



world_clear(Named):-fmt('Clearing world database: ~q.~n',[Named]).

pred_as_is(F,_):-mpred_prop(F,flag),!.
pred_as_is(F,_):-mpred_prop(F,external(_)),!.
pred_as_is(p,_):-!,fail.
pred_as_is(k,_):-!,fail.

type(T):-moo:subclass(A,B),(T=B;T=A).
type(item).


moo:subclass(drinkable,item).
moo:subclass(possessable,item).
moo:subclass(useable,item).
moo:subclass(eatable,item).
moo:subclass(chargeable,item).
moo:subclass(wearable,item).

comparitiveOp((\=)).
comparitiveOp((\==)).
comparitiveOp((=)).
comparitiveOp((=:=)).
comparitiveOp((==)).
comparitiveOp((<)).
comparitiveOp((>)).
comparitiveOp((=<)).
comparitiveOp((>=)).

additiveOp((is)).
additiveOp((*)).
additiveOp(+).
additiveOp(-).
additiveOp((/)).


db_op(q,Term):-!, db_op0(q,Term).
db_op(Op,Term):-must(db_op0(Op,Term)).



db_op0(Op,expand_args(Exp,Term)):- !,forall(do_expand_args(Exp,Term,O),db_op(Op,O)).
db_op0(Op,somethingIsa(A,List)):- !,forall_member(E,List,db_op(Op, isa(A,E))).
db_op0(Op,somethingDescription(A,List)):- !,forall_member(E,List,db_op(Op, description(A,E))).
db_op0(Op,objects(Type,List)):- !,forall_member(I,List,db_op(Op,isa(I,Type))).
db_op0(Op,sorts(Type,List)):- !,forall_member(I,List,db_op(Op, subclass(I,Type))).
db_op0(Op,predicates(List)):- !,forall_member(T,List,db_op(Op,mpred(T))).
db_op0(Op,EACH):- EACH=..[each|List],forall_member(T,List,db_op(Op,T)).

db_op0(_Op,props(_Obj,Open)):-Open==[],!.
db_op0(Op,props(Obj,[P|ROPS])):-!,db_op(Op,props(Obj,P)),db_op(Op,props(Obj,ROPS)).
/*
db_op(Op,props(Obj, Compare)):- 
   getCompare(Op,Obj,Compare,PreCall,Condition,OnTrue,OnFalse),!,PreCall, (Condition -> OnTrue ; OnFalse).
*/
db_op0('q',props(Obj,ofclass(X))):-!,db_op0('q',props(Obj,isa(X))).
db_op0('q',props(Obj,isa(verb))):-var(Obj),!,get_term_specifier_text(Obj,verb).
db_op0('q',props(Obj,isa(ItemOrVerb))):-var(Obj),!,get_term_specifier_text(Obj,ItemOrVerb).

db_op0('q',props(Obj,PropVal)):- var(Obj),!,dtrace,
   throw(db_op0('q',props(Obj,PropVal))).

db_op0(Op,props(Obj,PropVal)):-   
   !,PropVal=..[Prop|Vals],
	Call=..[Prop,Obj|Vals],
	db_op(Op,Call).
db_op0(Op,C0):-functor(C0,F,A),db_op_4(Op,F,A,C0),!.

db_op_4(Op,:,2,_MODULE:C0):-!,/*throw(module_form(MODULE:C0)),*/
  functor(C0,F,A),
  dmsg(todo(unmodulize(F/A))),
  db_op(Op,C0).
db_op_4(Op,k,_,C0):- C0=..[k,Prop|ARGS],C1=..[Prop|ARGS],!,db_op(Op,C1),!.
db_op_4(Op,p,_,C0):- C0=..[p,Prop|ARGS],C1=..[Prop|ARGS],!,db_op(Op,C1),!.
db_op_4(Op,k,_,C0):- C0=..[k,Prop|ARGS],C1=..[Prop|ARGS],!,db_op(Op,C1),!.
db_op_4(Op,p,_,C0):- C0=..[p,Prop|ARGS],C1=..[Prop|ARGS],!,db_op(Op,C1),!.
db_op_4(Op,svo,_,C0):- C0=..[svo,Obj,Prop|ARGS],C1=..[Prop,Obj|ARGS],!,db_op(Op,C1),!.
db_op_4(q,Fmt,1,C0):-formattype(Fmt),!,C0=..[_,A],term_is_ft(A,Fmt).
db_op_4(a,Fmt,1,_C0):-formattype(Fmt),!,dmsg(todo(dont_assert_is_decl_ft(Fmt))),!.
db_op_4(Op,Prop,_,C0):- 
   C0=..[Prop|ARGS],db_op_disj(Op,Prop,ARGS).

db_op_unit(_Op,Prop,ARGS,C0):-C0=..[Prop|ARGS].

% impl/1
db_op_disj(Op,Prop,ARGS):-mpred_prop(Prop,impl(Other)),!,
	db_op_unit(Op,Other,ARGS,Unit),!,db_forall(Op,Unit).
% alias/1
db_op_disj(Op,Prop,ARGS):-mpred_prop(Prop,alias(Other)),!,
	db_op_unit(Op,Other,ARGS,Unit),!,db_forall(Op,Unit).
% inverse/1
db_op_disj(Op,Prop,ARGS):-
      mpred_prop(Prop,inverse(Other)),!,
      inverse_args(ARGS,Inverse),
      db_op_unit(Op,Other,Inverse,Unit1),
      db_op_unit(Op,Prop,ARGS,Unit2),
      db_forall(Op,(Unit1;Unit2)).

% assert_with/1
db_op_disj(a,Prop,ARGS):- mpred_prop(Prop,assert_with(How)),!,call_pa(How,Prop,ARGS).
db_op_disj(Op,Prop,ARGS):- mpred_prop(Prop,assert_with(How)),!,throw(cant(db_op_disj(Op,Prop,ARGS),mpred_prop(Prop,assert_with(How)))).

% plain prop
db_op_disj(Op,Prop,ARGS):-db_op_unit(Op,Prop,ARGS,Unit),db_forall(Op,Unit).

call_pa(How,Prop,ARGS):-PROPARGS=..[Prop|ARGS], Call=..[How,PROPARGS],must(Call).

db_forall(Op,(C1,C2)):-!,db_forall(Op,C1),db_forall(Op,C2).
db_forall(r,(C1;C2)):-!,trace,once((db_forall(r,C1),db_forall(r,C2))).
db_forall(ra,(C1;C2)):-!,must(db_forall(ra,C1)),must(db_forall(ra,C2)).
db_forall('q',(C1;C2)):-!, db_forall('q',C1) ; db_forall('q',C2).
db_forall(a,(C1;C2)):-!,db_forall(a,C1),!,db_forall(a,C2),!.
db_forall('q',C):-!,db_forall_quf(C,U,Template),!,U, debugOnError(Template).
db_forall(u,C):- trace,db_forall_quf(C,U,Template),U,Template,must(ground(Template)),!,ignore(hooked_retractall(Template)).
db_forall(ra,C):-db_forall_quf(C,U,Template), doall((U,hooked_retractall(Template))).
db_forall(ra,C):-ignore(hooked_retractall(C)).
db_forall(a,C0):- db_forall_quf(C0,U,C),must(U),functor(C,F,A),!, (mpred_prop(F,singleValued) -> must(db_assert_sv(C,F,A)) ; must(db_assert_mv(C,F,A))).

db_forall(a,C):- functor(C,F,A),!, (mpred_prop(F,singleValued) -> must(db_assert_sv(C,F,A)) ; must(db_assert_mv(C,F,A))).
db_forall(r,C):- ground(C),hooked_retractall(C),!.
db_forall(Op,C):-!,trace,throw(unhandled(db_forall(Op,C))).

call_expanded_for_sv(WhatNot,F,A,G,OUT):- nonvar(OUT),replace_arg(G,A,NEW,CC),!,call_expanded_for_sv_2(WhatNot,F,A,CC,NEW),!,NEW=OUT.
call_expanded_for_sv(WhatNot,F,A,G,OUT):- call_expanded_for_sv_2(WhatNot,F,A,G,OUT),!.

call_expanded_for_sv_2(WhatNot,F,A,G,_OUT):-call_expanded([whatnot(WhatNot)],G,F,A),!.
call_expanded_for_sv_2(WhatNot,F,A,G,OUT):- defaultArgValue(WhatNot:G,F,A,OUT),!.

:-export(call_expanded/1).
%:-export(call_expanded/1).
call_expanded(G):-debugOnError(G).
%call_expanded(G) :- functor(G,F,A),call_expanded([],G,F,A).

:-export(call_expanded/4).
call_expanded(_Doing,V,_F,_A):-var(V),!,trace_or_throw(var_call_expanded(V)).
call_expanded(_Doing,true,_F,_A):-!.
call_expanded(Doing,G,F,A):-not(member(ask_module(_),Doing)),mpred_prop(F,ask_module(M)),!,'@'(M:call(call_expanded([ask_module(M)|Doing],M:G,F,A)),M).
call_expanded(Doing,G,F,A):-not(member(query(_),Doing)),mpred_prop(F,query(M)),!,call_expanded([query(M)|Doing],call(M,G),F,A).
call_expanded(Doing,G,F,A):-not(member(singleValued,Doing)),mpred_prop(F,singleValued),!,call_expanded([singleValued|Doing],G,F,A),!.
call_expanded(Doing,G,F,A):-not(member(dbase_t,Doing)),mpred_prop(F,dbase_t),!,call_expanded([dbase_t|Doing],dbase_t(G),F,A).

% call_expanded(Doing,G,_,_):-predicate_property(M:G,_),!,call(M:G).
call_expanded(_Doing,G,_,_):-call_asserted(G).
% call_expanded(Doing,G,F,A):-add_mpred_prop(F,A,dbase_t),!,call_expanded(G).
% call_expanded(Doing,G,_,_):-dtrace,!,G.

:- dynamic_multifile_exported((
          dbase_t/1,
          dbase_t/2,
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

dbase_t([AH,P|LIST]):- is_holds_true(AH),!,dbase_t_p2(P,LIST).
dbase_t([AH,P|LIST]):- is_holds_false(AH),!,dbase_f([P|LIST]).
dbase_t([P|LIST]):- !,dbase_t_p2(P,LIST).
dbase_t(not(CALL)):-dbase_f(CALL).
dbase_t(CALL):- safe_univ(CALL,[P|LIST]),dbase_t([P|LIST]).
dbase_t_p2(P,LIST):- safe_univ(CALL,[dbase_t,P|LIST]),call(CALL).

% ================================================
% hooked_assert/1 hooked_retract/1
% ================================================

ensure_predicate_reachable(M,C):-once((predicate_property(C,imported_from(Other)),M\=Other,
                                       context_module(CM),
                                       dmsg(wrong_import_module(M,Other:C,from(CM))),
                                       ignore(delete_import_module(CM,Other)),
                                       '@'((M:dynamic(C),M:export(C)),M),user:import(M:C))),fail.
ensure_predicate_reachable(_,_).


singletons_throw_or_fail(C):- contains_singletons(C),trace_or_throw(contains_singletons(C)).
nonground_throw_or_fail(C):- throw_if_true_else_fail(not(ground(C)),C).


into_assertion_form_trans(G,was_asserted_gaf(G)):- functor(G,F,_),mpred_prop(F,isStatic).

/*
into_assertion_form(M:H,G):-atom(M),!,into_assertion_form(H,G).
into_assertion_form(H,G):-into_assertion_form_trans(H,G),!.
into_assertion_form(H,G):-expand_term( (H :- true) , C ), reduce_clause(C,G).
*/
%into_mpred_form(was_asserted_gaf(H),G):-!,into_mpred_form(H,G).
%into_mpred_form(H,G):-expand_term( (H :- true) , C ), reduce_clause(C,G).

asserted_clause(G):- clause_asserted(G).
asserted_clause(G):- is_asserted_gaf(G).


call_asserted(G):- clause_asserted(G).
call_asserted(G):- is_asserted_gaf(G).
call_asserted(G):- dbase_t(G).
call_asserted(G):- loop_check_fail(catch(G,_,fail)).

run_database_hooks_local(Type,C):- once((run_database_hooks(Type,C))),ignore((into_assertion_form(C,G),differnt_assert(C,G),run_database_hooks(Type,G))).

% only place ever should actual game dbase be changed from

into_mpred_aform(C,CP,CA):-into_mpred_form(C,CP),into_assertion_form(C,CA),!.

hooked_asserta(C):- into_mpred_aform(C,CP,CA),hooked_asserta(CP,CA).
hooked_assertz(C):- into_mpred_aform(C,CP,CA),hooked_assertz(CP,CA).
hooked_retract(C):- into_mpred_aform(C,CP,CA),hooked_retract(CP,CA).
hooked_retractall(C):- into_mpred_aform(C,CP,CA),hooked_retractall(CP,CA).


hooked_asserta(CP,_CA):- singletons_throw_or_fail(hooked_asserta(CP)).
hooked_asserta(_CP,CA):- asserted_clause(CA),!.
hooked_asserta(CP,CA):- asserta_cloc(CA),run_database_hooks_local(assert(a),CP).


hooked_assertz(CP,_CA):- singletons_throw_or_fail(hooked_assertz(CP)).
hooked_assertz(_CP,CA):- asserted_clause(CA),!.
hooked_assertz(CP,CA):- assertz_cloc(CA),run_database_hooks_local(assert(z),CP).

hooked_retract(CP,_CA):- nonground_throw_or_fail(hooked_retract(CP)).
hooked_retract(CP,CA):- must_det(asserted_clause(CA)),!,run_database_hooks_local(retract(one),CP), ignore(retract_cloc(CA)).
hooked_retract(CP,CA):- run_database_hooks_local(retract(one),CP), ignore(retract_cloc(CA)).

hooked_retractall(CP,CA):-  retractall_cloc(CA), run_database_hooks_local(retract(all),CP).

differnt_assert(G1,G2):- notrace(differnt_assert1(G1,G2)),dmsg(differnt_assert(G1,G2)),dtrace.

differnt_assert1(M:G1,G2):-atom(M),!, differnt_assert1(G1,G2).
differnt_assert1(G1,M:G2):-atom(M),!, differnt_assert1(G1,G2).
differnt_assert1(G1,G2):- once(into_mpred_form(G1,M1)),G1\=M1,!, differnt_assert1(M1,G2).
differnt_assert1(G1,G2):- once(into_mpred_form(G2,M2)),G2\=M2,!, differnt_assert1(G1,M2).
differnt_assert1(G1,G2):- not((G1 =@= G2)).

show_cgoal(G):- stack_check(300,dmsg(warning(maybe_overflow(stack_lvl)))),!,call(G).
show_cgoal(G):- % dmsg(show_cgoal(G)),
               call(G).


% only place ever should actual game database be changed from
asserta_cloc(M:C):-atom(M),!,asserta_cloc(M,C),!.
asserta_cloc( C ):-dbase_mod(M),asserta_cloc(M,C),!.
asserta_cloc(M,C):-ensure_predicate_reachable(M,C),fail.
asserta_cloc(M,C):-clause_asserted(M:C,true),!.
asserta_cloc(M,C):-database_real(asserta,M:C).


assertz_cloc(M:C):-atom(M),!,assertz_cloc(M,C),!.
assertz_cloc( C ):-dbase_mod(M),assertz_cloc(M,C),!.
assertz_cloc(M,C):-ensure_predicate_reachable(M,C),fail.
assertz_cloc(M,C):-clause_asserted(M:C,true),!.
assertz_cloc(M,C):-database_real(assertz,M:C).

retract_cloc(M:C):-atom(M),!,retract_cloc(M,C),!.
retract_cloc( C ):-dbase_mod(M),retract_cloc(M,C),!.
retract_cloc(M,C):-ensure_predicate_reachable(M,C),fail.
retract_cloc(M,C):-clause_asserted(M:C,true),!.
retract_cloc(M,C):-database_real(retract,M:C).

retractall_cloc(M:C):-atom(M),!,retractall_cloc(M,C),!.
retractall_cloc( C ):-dbase_mod(M),retractall_cloc(M,C),!.
retractall_cloc(M,C):-ensure_predicate_reachable(M,C),fail.
retractall_cloc(M,C):-clause_asserted(M:C,true),!.
retractall_cloc(M,C):-database_real(retractall,M:C).

database_real(P,C):- debugOnError(call(P,C)).

% ================================================
% call_must/2
% ================================================
:-export((call_must/2)).
call_must(query,Call):- !,call(Call).
call_must(must,Call):- !,must(Call).
% call_must(Must,Call):- var(Must),!,Call.
call_must(once,Call):- !,once(Call).
call_must(!,Call):- !,once(Call).
call_must(_,Call):- call(Call).

% ================================================
% db_assert_[mv|sv]/3
% ================================================


% assert_with to tell(OldV) mutlivalue pred
db_assert_mv(end_of_file,_,_):-!.
db_assert_mv(C,F,_A):- (mpred_prop(F,ordered) -> hooked_assertz(C) ; hooked_asserta(C)).


% assert_with to tell(OldV) singlevalue pred
%db_assert_sv(C,F,A):- throw_if_true_else_fail(contains_singletons(C),db_assert_sv(C,F,A)).
db_assert_sv(C,F,A):- ignore(( loop_check(db_assert_sv_lc(C,F,A),true))).

:-export((db_assert_sv_lc/3)).

db_assert_sv_lc(C,F,A):-
   arg(A,C,UPDATE),
   replace_arg(C,A,OLD,COLD),
   replace_arg(COLD,A,NEW,CNEW),
   replace_arg(CNEW,A,_,CBLANK),!,
   call_expanded_for_sv(forAssert,F,A,COLD,OLD),
   update_value(OLD,UPDATE,NEW),
   ((NEW==OLD)-> dmsg(no_updatedb_assert_sv_lc(C));
   ((
      ignore((hooked_retractall(CBLANK))),
      ignore((nonvar(COLD),hooked_retractall(COLD))),   
      hooked_asserta(CNEW)))),!.

defaultArgValue(facing(_,_),_,_,"n"):-!.
defaultArgValue(damage(_,_),_,_,500):-!.

defaultArgValue(Info,F,_A,OLD):- mpred_prop(F,default(OLD)),!,dmsg(real_defaultValue(Info,F,default(OLD))).
defaultArgValue(Info,F,A,OLD):- argIsa_call(F,A,Type),defaultTypeValue(Info,Type,OLD),!.

defaultTypeValue(_Info,dir,"n").
defaultTypeValue(_Info,int,0).
defaultTypeValue(Info,Type,Out):-dmsg(fake_defaultValue(Info,Type,0)),!,Out=0.

replace_arg(C,A,OLD,CC):- 
   C=..FARGS,
   replace_nth(FARGS,A,OLD,FARGO),!,
   CC=..FARGO.

replace_nth([],_,_,[]):- !.
replace_nth([_|ARGO],0,OLD,[OLD|ARGO]):- !.
replace_nth([T|FARGS],A,OLD,[T|FARGO]):- 
    A2 is A-1,replace_nth(FARGS,A2,OLD,FARGO).



pl_arg_type(Arg,Type):-
      var(Arg) -> Type =var;
      integer(Arg) -> Type =integer;
      number(Arg) -> Type =float;
      string(Arg) -> Type =string;
      atom(Arg) -> Type =atom;
      is_list(Arg) -> Type =list;
      compound(Arg) -> Type =compound;
         Arg = Type.


:-thread_local clause_present_lookup_local/3.

clause_present(C):-notrace((functor(C,F,A),clause_present(C,F,A))).
clause_present(C,F,1):-C=..[F,A], formattype(F),!, term_is_ft(A,F).
clause_present(C,_F,_A):- not(predicate_property(C,_)),!,fail.
clause_present(C,_F,_A):- predicate_property(C,foreign),!,fail.
clause_present(C,_F,_A):- not(ground(C)),!,fail.
clause_present(C,F,A):- clause_present_lookup_local(C,F,A),!,fail.
clause_present(C,_,1):- !, clause(C,true).
clause_present(C,F,A):- with_assertions(clause_present_lookup_local(C,F,A),clause_present_1(C,F,A)).

clause_present_1(C,_,_):- debugOnError(C).
% clause_present_1(C,_F,_A):-predicate_property(C,foreign),!,trace_or_throw(predicate_property(C,foreign)),!,fail.
% clause_present_1(C,_F,_A):-clause(C,true),!.
clause_present_1(C0,_F,A):- A>1, arg(A,C0,NEW),string(NEW),!,copy_term(C0,C),
   setarg(A,C,OLD),C,string_chars(NEW,[S|C1]),string_chars(OLD,[S|C2]),C1=C2,dtrace,dmsg(present(C)).
%clause_present_1(C,F,A):- A>1, arg(A,C,NEW),snonvar(NEW),!,setarg(A,C,OLD),clause_present(C,F,A),pl_arg_type(NEW,string),string_chars(NEW,[S|C1]),string_chars(OLD,[S|C2]),C1=C2,dmsg(present(C)).


must_asserta(C):-
      must(ground(C)),
      must(hooked_asserta(C)),!.

argIsa_call(isa,1,argIsaFn(isa,1)):-!.
argIsa_call(isa,2,type):-!.
argIsa_call(act_affect,_,term):-!.
argIsa_call(ofclass,2,type):-!.
argIsa_call(memory,2,term):-!.
argIsa_call(Prop,N1,Type):- mpred_prop(Prop,argIsa(N1, Type)),!.
argIsa_call(F,N,Type):- mpred_prop(F,argsIsa(Types)),arg(N,Types,Type),nonvar(Type),!.
argIsa_call(F,N,Type):- mpred_prop_game_assert(Types),functor(Types,F,_),arg(N,Types,Type),nonvar(Type),!.
argIsa_call(Prop,N1,Type):- dmsg(todo(define(argIsa_call(Prop,N1,'_TYPE')))),
   Type=argIsaFn(Prop,N1).

db_forall_quf(C,Pretest,Template):- C=..[Prop,OBJ|ARGS],
      translate_args(Prop,OBJ,2,ARGS,NEWARGS,true,Pretest),
      Template =.. [Prop,OBJ|NEWARGS].

translate_args(_Prop,_OBJ,_N,[],[],GIN,GIN).
translate_args(Prop,OBJ,N1,[ARG|S],[NEW|ARGS],GIN,GOALS):-
   argIsa_call(Prop,N1,Type),
   translateOneArg(Prop,OBJ,Type,ARG,NEW,GIN,GMID),
   N2 is N1 +1,
   translate_args(Prop,OBJ,N2,S,ARGS,GMID,GOALS).

% var
translateOneArg(_Prop,_O,_Type,VAR,VAR,G,G):-var(VAR),!.

% not an expression
translateOneArg(_Prop,_O,_Type,VAR,VAR,G,G):-atomic(VAR),!.

% translateOneArg(_Prop,_O,Type,VAR,VAR,G,G):-ignore(mud_isa(VAR,Type)),!.

% props(Obj,size < 2).
translateOneArg(Prop,O,Type,ARG,OLD,G,(GETTER,COMPARE,G)):-
       functor(ARG,F,2), comparitiveOp(F),!,
       ARG=..[F,Prop,VAL],
       GETTER=..[Prop,O,OLD],
       COMPARE= compare_op(Type,F,OLD,VAL),!.

% props(Obj,oneOf(Sz,[size+1,2])).
translateOneArg(Prop,O,Type,oneOf(VAL,LIST),VAL,G,(GO,G)):-
   translateListOps(Prop,O,Type,VAL,LIST,G,GO).

% padd(Obj,size + 2).
translateOneArg(Prop,O,_Type,ARG,NEW,G,(GETTER,STORE,G)):-
       functor(ARG,F,2), additiveOp(F),!,
       ARG=..[F,Prop,VAL],
       GETTER=..[Prop,O,OLD],
       STORE= update_value(OLD,VAL,NEW),!.

translateListOps(_Prop,_O,_Type,_VAL,[],G,G).
translateListOps(Prop,O,Type,VAL,[L|LIST],G,GO2):-
   translateOneArg(Prop,O,Type,L,VAL,G,GO),
   translateListOps(Prop,O,Type,VAL,LIST,GO,GO2).

compare_op(Type,F,OLD,VAL):-nop(Type),debugOnError((trace,call(F,OLD,VAL))),!.

% start of database
% These will all be deleted at start of run

inverse_args([AR,GS],[GS,AR]):-!.
inverse_args([AR,G,S],[S,G,AR]):-!.
inverse_args([A,R,G,S],[S,R,G,A]):-!.
inverse_args([P,A,R,G,S],[S,A,R,G,P]):-!.


compare_n(NewLast,Last):-NewLast=Last,!.
compare_n(NewLast,Last):-NewLast==unknown,!,NewLast==Last.
compare_n(NewLast,Last):- number(NewLast),not(number(Last)),trace_or_throw(incomparable_terms(Last,NewLast)).
compare_n(Last,NewLast):- number(NewLast),not(number(Last)),trace_or_throw(incomparable_terms(Last,NewLast)).
compare_n(NewLast,Last):- atomic(NewLast),not(atomic(Last)),trace_or_throw(incomparable_terms(Last,NewLast)).
compare_n(Last,NewLast):- atomic(NewLast),not(atomic(Last)),trace_or_throw(incomparable_terms(Last,NewLast)).

:- assert_if_new(mpred_prop(inRegion,isStatic)).
moo:inRegion(O,Region):-atloc(O,LOC),locationToRegion(LOC,Region).
moo:inRegion(apath(Region,Dir),Region):-pathBetween(Region,Dir,_To).
moo:inRegion(O,Region):-is_asserted_gaf(inRegion(O,Region)).

:-dynamic(was_asserted_gaf/1).

is_asserted_gaf(Fact):-was_asserted_gaf(Fact).

member_or_e(E,[L|List]):-!,member(E,[L|List]).
member_or_e(E,E).


replace_nth([],_N,_OldVar,_NewVar,[]):- !,trace_or_throw(missed_the_boat).
replace_nth([OldVar|ARGS],1,OldVar,NewVar,[NewVar|ARGS]):- !.
replace_nth([Carry|ARGS],Which,OldVar,NewVar,[Carry|NEWARGS]):- 
 Which1 is Which-1,
 replace_nth(ARGS,Which1,OldVar,NewVar,NEWARGS),!.

update_value(OLD,NEW,NEXT):- var(NEW),!,trace_or_throw(logicmoo_bug(update_value(OLD,NEW,NEXT))).
update_value(OLD,NEW,NEWV):- (OLD=@=NEW;var(OLD)),!,compute_value_no_dice(NEW,NEWV).
update_value(OLDI,+X,NEW):- compute_value(OLDI,OLD),catch(NEW is OLD + X,_,fail),!.
update_value(OLDI,-X,NEW):- compute_value(OLDI,OLD),catch(NEW is OLD - X,_,fail),!.
update_value(_OLD,NEW,NEWV):-compute_value(NEW,NEWV).

compute_value_no_dice(NEW,NEW):-functor(NEW,dice,_),!.
compute_value_no_dice(NEW,NEWV):-compute_value(NEW,NEWV).

compute_value(NEW,NEWV):-catch(NEWV is NEW,_,fail),!.
compute_value(NEW,NEWV):-catch(any_to_value(NEW,NEWV),_,fail),!.
compute_value(NEW,NEW).

insert_into(ARGS,0,Insert,[Insert|ARGS]):- !.
insert_into([Carry|ARGS],After,Insert,[Carry|NEWARGS]):- 
   After1 is After - 1,
   insert_into(ARGS,After1,Insert,NEWARGS).

moo:term_specifier_text(Text,pred):- mpred_prop(Text,arity(_)).

moo:subclass(agent,object).
moo:subclass(item,object).

% single valued
argsIsa(pathName(region,dir,string)).
argsIsa(verbOverride(term,action,action),[dynamic_in_module]).

moo:singleValued(atloc(object,xyz(region,int,int,int))).
moo:singleValued(agent_turnnum(agent,int)).
moo:singleValued(armorLevel(possessable,int)).
moo:singleValued(attack(agent,int)).
moo:singleValued(charge(agent,int),[default(500)]).
moo:singleValued(chargeCapacity(chargable,int)).
moo:singleValued(chargeRemaining(chargable,int)).
moo:singleValued(damage(agent,int)).
moo:singleValued(defence(agent,int)).
moo:singleValued(facing(agent,dir)).
moo:singleValued(height(agent,int)).
moo:singleValued(id(object,id)).
moo:singleValued(inRegion(term,region)).
moo:singleValued(last_command(agent,command)).
moo:singleValued(location_center(region,xyz(region,int,int,int))).
moo:singleValued(movedist(agent,number)).
moo:singleValued(mudBareHandDamage(agent,dice)).
moo:singleValued(mudLevelOf(possessable,int)).
moo:singleValued(mudMaxHitPoints(agent,int),[dynamic_in_module]).
moo:singleValued(mudToHitArmorClass0(agent,int)).
moo:singleValued(permanence(item,verb,int)).
moo:singleValued(score(object,int)).
moo:singleValued(spawn_rate(propFn(subclass(object)),int)).
moo:singleValued(spd(agent,int)).
moo:singleValued(stm(agent,int)).
moo:singleValued(str(agent,int)).
moo:singleValued(type_grid(regiontype,int,list(term))).
moo:singleValued(weight(object,int)).
moo:singleValued(ArgTypes):-mpred_prop_g(ArgTypes).

:-dynamic(spawn_rate/2).

singleValued(type_max_charge(type,int)).
singleValued(max_charge(term,int)).
singleValued(type_max_damage(type,int)).
singleValued(max_damage(term,int)).



:-dynamic(mudToHitArmorClass0/2).
% :- include('dbase_i_builtin').

:- multifile((
     type/1, agent/1, item/1, region/1,
     verbOverride/3,named/2, determinerString/2, keyword/2 ,descriptionHere/2, mudToHitArmorClass0/2,

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
      mtForPred/2,
      isa/2,
      pathName/3, 
      possess/2,
      score/2,
      stm/2,      
      str/2,
      wearing/2)).

:- dynamic((
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
      mtForPred/2,
      isa/2,
      pathName/3, 
      possess/2,
      score/2,
      stm/2,      
      str/2,
      wearing/2)).

mpred_prop_format(apath(region,dir),areaPath).
mpred_prop_format(dice(int,int,int),int).


moo:resultIsa(apath,areaPath).
moo:subclass(areaPath,door).
moo:subclass(door,item).

moo:subclass(dir,string).

% flags
:-decl_mpred(agent(id),[flag]).
:-decl_mpred(item(id),[flag]).
:-decl_mpred(region(id),[flag]).
:-decl_mpred(type(id),[flag]).
:-decl_mpred(thinking(agent),[flag]).
:-decl_mpred(deleted(id),[flag]).

% multivalued
%mpred(G,[multi(AT)|LIST]):-multiValued(G,AT,LIST).

:-dynamic(mpred_prop/2).

mpred_prop(G,assert_with(game_assert)):- mpred_prop_game_assert(G).

mpred_prop_game_assert(somethingIsa(term,list(type))).
mpred_prop_game_assert(somethingDescription(term,list(string))).
mpred_prop_game_assert(objects(type,list(id))).
mpred_prop_game_assert(sorts(type,list(type))).

%mpred(ArgTypes,[singleValued]):-moo:singleValued(ArgTypes).
%mpred(CallSig,[external(M)]):-mpred_prop_prolog(M:CallSig).
:-dynamic(mpred_prop_prolog/1).
mpred_prop_prolog(world:nearby(object,object)).
%mpred_prop_prolog(world:isa(object,type)).
%mpred_prop_prolog(world:same(id,id)).


% multiValued
%multiValued(G,AT,[ordered|LIST]):-multiValued(G,LIST),functor(G,_,AT).

moo:multiValued(pathBetween(region,dir,region)).
multiValued(named(term,term),[genlpreds(id)]).
multiValued(ofclass(term,type),[alias(isa)]).

multiValued(failure(agent,action)).
multiValued(nameStrings(term,string)).
multiValued(determinerString(term,string)).
multiValued(descriptionHere(term,string)).
multiValued(description(term,string)).
multiValued(keyword(term,string)).
multiValued(act_affect(term,term,term)).
multiValued(memory(agent,term)).
multiValued(wearing(agent,wearable)).
multiValued(grid(region,int,int,object)).
multiValued(possess(agent,item)).
multiValued(subclass(type,type)).
multiValued(isa(term,type)).
moo:argsIsa(somethingIsa(term,list(type))).
moo:argsIsa(somethingDescription(term,list(string))).

:-decl_mpred(repl_writer(agent,term),[singleValued,default(look:default_repl_writer)]).
:-decl_mpred(repl_to_string(agent,term),[singleValued,default(look:default_repl_obj_to_string)]).

%mpred(ArgTypes,PropTypes):-moo:decl_mpred_prop(ArgTypes,PropTypes).
% somethingIsa('NpcCol1012-Ensign732',['NpcCol1012',actor,'MaleAnimal']).

:-dynamic_multifile_exported((needs_look/2)).

ensure_clause(HEAD,_,_,_BODY):-not(not((numbervars(clause(HEAD,BODY),'$VAR',0,_),clause(HEAD,BODY)))),!.
% ensure_clause(HEAD,F,A,_):-pred_as_is(F,A), !.
ensure_clause(HEAD,_F,_A,BODY):-assertz((HEAD:-BODY)),
   % this is just to catch asserts at these predicates that are supposed to be contained.. We dont really want them compiled
   nop(compile_predicates([HEAD])).


nameStrings(apath(Region,Dir),Text):- pathName(Region,Dir,Text).
description(apath(Region,Dir),Text):- pathName(Region,Dir,Text).

:- decl_mpred(needs_look/2,[extentKnown]).
:- decl_mpred(mudMaxHitPoints(agent,int)).

:-export(scan_mpred_prop/0).
scan_mpred_prop:-forall(mpred_prop(Pred,Prop),hooked_assertz(mpred_prop(Pred,Prop))),fail.
scan_mpred_prop:-remove_duplicated_facts,fail.
scan_mpred_prop:-forall(mpred_prop(Pred,Prop),run_database_hooks(assert(z),mpred_prop(Pred,Prop))),fail.
scan_mpred_prop:-remove_duplicated_facts.


:- scan_mpred_prop.


:-export(get_type_props/2).
get_type_props(Type,PropList):-call_tabled(type(Type)),findall(PropU,(findall_type_default_props(Inst,Type,Prop),subst(Prop,Inst,self,PropU)),PropS),flatten_set(PropS,PropList).


instance_missing_props(I,LPS,PS):- findall(P,(member(P,LPS),inst_missing_prop(I,P)),PS),!.
      
inst_missing_prop(I,P):-P=..[F|Args],inst_missing_prop(I,F,Args).

inst_missing_prop(_I,F,_Args):-not(mpred_prop(F,singleValued)),!,fail.
inst_missing_prop(_I,F,_Args):-mpred_prop(F,flag),!,fail.
inst_missing_prop(I,F,Args):-C=..[F,I|Args],get_sv_argnum(F,[I|Args],A),arg(A,C,_Default),replace_arg(C,A,BLANK,COLD),ignore(req(COLD)),!,nonvar(BLANK).


get_sv_argnum(F,Args,ArgNum):-once(mpred_prop(F,functionalArg(ArgNum));length(Args,ArgNum)).

forall_setof(ForEach,Call):-
   findall(ForEach,ForEach,ForEachAll),
   list_to_set(ForEachAll,Set),!,
   ignore(forall(member(ForEach,Set),Call)).

:-export(scan_default_props/0).

scan_default_props:- dmsg(todo(fix(scan_default_props,"to not set atloc/2"))),!.
scan_default_props:- 
 forall_setof(get_type_props(Type,PropList),
    forall_setof(isa(I,Type), 
         ignore((not(Type == I),instance_missing_props(I,PropList,Missing),dmsg(scan_default_props(I,Type,missing_from(Missing,PropList))),padd(I,Missing))))),!,
 remove_duplicated_facts.
scan_default_props.

      

% load_motel:- defrole([],time_state,restr(time,period)).
% :-load_motel.

:- include(logicmoo('vworld/moo_footer.pl')).



