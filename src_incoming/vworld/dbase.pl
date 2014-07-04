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

:- user_use_module(dbase_formattypes).

:- export((
 add/1, add0/1, agent/1, agent_doing/2, agent_done/2, argIsa_call/3, charge/2, ofclass/2, clr/1, damage/2, db_op/2, mpred_prop/2, mpred_prop/1, atloc/2, is_mpred_prop/3,
 mpred_prop_g/1, mpred_prop_game_assert/1, del/1, failure/2, grid/4, inRegion/2, is_mpred_prop/2, is_mpred_prop/3, isa/2, item/1, 
 memory/2, padd/2, padd/3, pathName/3, possess/2, prop/3, prop_or/4, props/2, region/1, req/1, scan_mpred_prop/0, score/2, stm/2, term_listing/1,  facing/2,
 thinking/1, type/1, use_term_listing/2, wearing/2, world_clear/1, str/2 ,facing/2, height/2, act_term/2, nameStrings/2, description/2, pathBetween/3, act_turn/2,
 dbase_mod/1, define_mpred_prop/2,
 clause_present_1/3,
 object/1,
 with_kb_assertions/2
    )).

:- dynamic 
 dbase_mod/1, object/1,
 add/1, add0/1, agent/1, agent_doing/2, agent_done/2, argIsa_call/3, charge/2, ofclass/2, clr/1, damage/2, db_op/2, mpred_prop/2, mpred_prop/1, atloc/2, is_mpred_prop/2, is_mpred_prop/3,
 mpred_prop_g/1, mpred_prop_game_assert/1, del/1, failure/2, grid/4, is_mpred_prop/3, isa/2, item/1, 
 memory/2, padd/2, padd/3, pathName/3, possess/2, prop/3, prop_or/4, props/2, region/1, req/1, scan_mpred_prop/0, score/2, stm/2, term_listing/1, facing/2,
 thinking/1, type/1, use_term_listing/2, wearing/2, world_clear/1, str/2 ,facing/2, height/2, act_term/2, nameStrings/2, description/2, pathBetween/3, act_turn/2.

:-multifile inRegion/2.
dbase_mod(moo).
/*

:- context_module(M),
   asserta(dbase_mod(M)),
   dmsg(assert_if_new(dbase_mod(M))).

*/

user_export(_):- dbase_mod(user),!.
user_export(Prop/Arity):- 
   dbase_mod(M), '@'( M:export(Prop/Arity) , M).

:- multifile mpred_prop/2.

:- meta_predicate man:with_assertions(:,0).
:- meta_predicate world:intersect(?,0,?,0,0,-).
:- meta_predicate clause_present(:), db_assert_mv(+,+,+), db_assert_sv(+,+,+), db_forall(+,+), db_forall_quf(+,+,+).

:- meta_predicate hooked_asserta(^), hooked_assert(^), hooked_retract(^), hooked_retractall(^).
%% :- meta_predicate del(0),clr(0),add(0),add0(0),req(0), db_op(0,0,0),mpred_prop(0,0),mpred_prop(0).

% Found new meta-predicates in iteration 1 (0.281 sec)
%:- meta_predicate db_forall(?,?,?,0).

:- include(logicmoo('vworld/moo_header.pl')).

% :- include('dbase_types_motel').

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

:-export(call_expanded/1).
call_expanded(G):-debugOnError(G).

:-include(dbase_i_pldoc).
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



world_clear(Named):-fmt('Clearing world database: ~q.',[Named]).

pred_as_is(F,A):-is_mpred_prop(F,A,flag),!.
pred_as_is(F,A):-is_mpred_prop(F,A,external(_)),!.
pred_as_is(p,_):-!,fail.
pred_as_is(k,_):-!,fail.

/*
getCompare(Op, O, P > N,PreCall,VAL>N,db_op(Op,PreCall),false):-
	atom(P),integer(N),PreCall=..[P,O,VAL],!.
getCompare(Op, Obj, Compare,PreCall,Condition,db_op(Op,PreCall),false):-
      functor(Compare,F,_),(additiveOp(F);comparitiveOp(F)),!,
      arg(1,Compare,Prop),
      PreCall=..[Prop,Obj,VAL],
      copy_term(Compare,Condition),trace,
      nb_setarg(1,Condition,VAL),
      throw(props(Obj, Compare)).
*/

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
db_op_4(q,Fmt,1,C0):-formattype(Fmt),!,C0=..[_,A],format_complies(A,Fmt,_).
db_op_4(a,Fmt,1,_C0):-formattype(Fmt),!,dmsg(todo(dont_assert_is_decl_ft(Fmt))),!.
db_op_4(Op,Prop,_,C0):- 
   C0=..[Prop|ARGS],db_op_disj(Op,Prop,ARGS).

db_op_unit(_Op,Prop,ARGS,C0):-C0=..[Prop|ARGS].

% impl/1
db_op_disj(Op,Prop,ARGS):-is_mpred_prop(Prop,_,impl(Other)),!,
	db_op_unit(Op,Other,ARGS,Unit),!,db_forall(Op,Unit).
% alias/1
db_op_disj(Op,Prop,ARGS):-is_mpred_prop(Prop,_,alias(Other)),!,
	db_op_unit(Op,Other,ARGS,Unit),!,db_forall(Op,Unit).
% inverse/1
db_op_disj(Op,Prop,ARGS):-
      is_mpred_prop(Prop,_,inverse(Other)),!,
      inverse_args(ARGS,Inverse),
      db_op_unit(Op,Other,Inverse,Unit1),
      db_op_unit(Op,Prop,ARGS,Unit2),
      db_forall(Op,(Unit1;Unit2)).

% assert_with/1
db_op_disj(a,Prop,ARGS):- is_mpred_prop(Prop,_,assert_with(How)),!,call_pa(How,Prop,ARGS).
db_op_disj(Op,Prop,ARGS):- is_mpred_prop(Prop,W,assert_with(How)),!,throw(cant(db_op_disj(Op,Prop,ARGS),is_mpred_prop(Prop,W,assert_with(How)))).

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
db_forall(a,C0):- db_forall_quf(C0,U,C),must(U),functor(C,F,A),!, (is_mpred_prop(F,A,singleValued) -> must(db_assert_sv(C,F,A)) ; must(db_assert_mv(C,F,A))).

db_forall(a,C):- functor(C,F,A),!, (is_mpred_prop(F,A,singleValued) -> must(db_assert_sv(C,F,A)) ; must(db_assert_mv(C,F,A))).
db_forall(r,C):- ground(C),hooked_retractall(C),!.
db_forall(Op,C):-!,trace,throw(unhandled(db_forall(Op,C))).

% ================================================
% hooked_assert*/1 hooked_retract*/1
% ================================================

ensure_predicate_reachable(M,C):-once((predicate_property(C,imported_from(Other)),M\=Other,
                                       context_module(CM),
                                       dmsg(wrong_import_module(M,Other:C,from(CM))),
                                       ignore(delete_import_module(CM,Other)),
                                       '@'((M:dynamic(C),M:export(C)),M),user:import(M:C))),fail.
ensure_predicate_reachable(_,_).


singletons_throw_or_fail(C):- contains_singletons(C),trace_or_throw(contains_singletons(C)).
nonground_throw_or_fail(C):- throw_if_true_else_fail(not(ground(C)),C).


into_assertion_form_trans(G,was_asserted_gaf(G)):- functor(G,F,A),is_mpred_prop(F,A,isStatic).

into_assertion_form(M:H,G):-atom(M),!,into_assertion_form(H,G).
into_assertion_form(H,G):-into_assertion_form_trans(H,G),!.
into_assertion_form(H,G):-expand_term( (H :- true) , C ), reduce_clause(C,G).

into_mpred_form(was_asserted_gaf(H),G):-!,into_mpred_form(H,G).
into_mpred_form(H,G):-expand_term( (H :- true) , C ), reduce_clause(C,G).

asserted_clause(G):- clause_asserted(G).
asserted_clause(G):- is_asserted_gaf(G).

run_database_hooks_local(Type,C):- once((run_database_hooks(Type,C))),ignore((into_assertion_form(C,G),differnt_assert(C,G),run_database_hooks(Type,G))).

% only place ever should actual game dbase be changed from

into_mpred_aform(C,CP,CA):-into_mpred_form(C,CP),into_assertion_form(C,CA),!.

hooked_asserta(C):- into_mpred_aform(C,CP,CA),hooked_asserta(CP,CA).
hooked_assertz(C):- into_mpred_aform(C,CP,CA),hooked_assertz(CP,CA).
hooked_retract(C):- into_mpred_aform(C,CP,CA),hooked_retract(CP,CA).
hooked_retractall(C):- into_mpred_aform(C,CP,CA),hooked_retractall(CP,CA).


hooked_asserta(CP,_CA):- singletons_throw_or_fail(hooked_asserta(CP)).
hooked_asserta(_CP,CA):- asserted_clause(CA),!.
hooked_asserta(CP,CA):- run_database_hooks_local(assert_with(a),CP),asserta_cloc(CA).


hooked_assertz(CP,_CA):- singletons_throw_or_fail(hooked_assertz(CP)).
hooked_assertz(_CP,CA):- asserted_clause(CA),!.
hooked_assertz(CP,CA):- run_database_hooks_local(assert_with(z),CP),assertz_cloc(CA).

hooked_retract(CP,_CA):- nonground_throw_or_fail(hooked_retract(CP)).
hooked_retract(CP,CA):- must_det(asserted_clause(CA)),!,run_database_hooks_local(retract(one),CP), ignore(retract_cloc(CA)).
hooked_retract(CP,CA):- run_database_hooks_local(retract(one),CP), ignore(retract_cloc(CA)).

hooked_retractall(CP,CA):- run_database_hooks_local(retract(all),CP), retractall_cloc(CA).

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
db_assert_mv(C,F,A):-db_assert_mv(must,C,F,A,_OldV).
db_assert_mv(Must,C,F,A,_OldV):- call_must(Must, (is_mpred_prop(F,A,ordered) -> hooked_assertz(C) ; hooked_asserta(C))).


% assert_with to tell(OldV) singlevalue pred
db_assert_sv(C,F,A):-db_assert_sv(must,C,F,A,_OldV).
db_assert_sv(Must,C,F,A,ROldCC):- throw_if_true_else_fail(contains_singletons(C),db_assert_sv(Must,C,F,A,ROldCC)).
db_assert_sv(Must,C,_,A,ROldCCOut):-
   arg(A,C,Update),
   must_det(db_assert_sv_old_value(C,A,OLD,ROldCC)),
   must_det(db_assert_sv_update(Must,C,A,OLD,Update,_NEW)),!,must(ROldCCOut=ROldCC),
   !. %, dmsg(db_assert_sv(C,OLD,Update,NEW)).

db_assert_sv_update(Must,C,A,OLD,Update,NEW):- 
   update_value(OLD,Update,NEW),
   nonvar(NEW),
   replace_arg(C,A,NEW,CC),
   call_must(Must,hooked_asserta(CC)),!.

db_assert_sv_old_value(C,A,OLD,CC):- must(var(OLD)),must(var(CC)),
   replace_arg(C,A,OLD,CC),
   % this binds or leave OLD
   ignore(req(CC)),
   ignore((nonvar(OLD),hooked_retractall(CC))).


replace_arg(C,A,OLD,CC):- 
   C=..FARGS,
   replace_nth(FARGS,A,OLD,FARGO),
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
clause_present(C,F,1):-C=..[F,A], formattype(F), format_complies(A,F,_).
clause_present(C,_F,_A):- not(predicate_property(C,_)),!,fail.
clause_present(C,_F,_A):- predicate_property(C,foreign),!,fail.
clause_present(C,_F,_A):- not(ground(C)),!,fail.
clause_present(C,F,A):- clause_present_lookup_local(C,F,A),!,fail.
clause_present(C,_,1):- !, clause(C,true).
clause_present(C,F,A):- with_assertions(clause_present_lookup_local(C,F,A),clause_present_1(C,F,A)).

clause_present_1(C,_,_):- debugOnError(C).
% clause_present_1(C,_F,_A):-predicate_property(C,foreign),!,throw(predicate_property(C,foreign)),!,fail.
% clause_present_1(C,_F,_A):-clause(C,true),!.
clause_present_1(C0,_F,A):- A>1, arg(A,C0,NEW),string(NEW),!,copy_term(C0,C),
   setarg(A,C,OLD),C,string_chars(NEW,[S|C1]),string_chars(OLD,[S|C2]),C1=C2,trace,dmsg(present(C)).
%clause_present_1(C,F,A):- A>1, arg(A,C,NEW),snonvar(NEW),!,setarg(A,C,OLD),clause_present(C,F,A),pl_arg_type(NEW,string),string_chars(NEW,[S|C1]),string_chars(OLD,[S|C2]),C1=C2,dmsg(present(C)).


must_asserta(C):-
      must(ground(C)),
      must(hooked_asserta(C)),!.

argIsa_call(isa,1,argIsaFn(isa,1)):-!.
argIsa_call(isa,2,type):-!.
argIsa_call(act_affect,_,term):-!.
argIsa_call(ofclass,2,type):-!.
argIsa_call(memory,2,term):-!.
argIsa_call(Prop,N1,Type):-is_mpred_prop(Prop,N1,argIsa(Type)),!.
argIsa_call(Prop,N1,Type):-dmsg(todo(define(argIsa_call(Prop,N1,'_TYPE')))),
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

:- assert_if_new(is_mpred_prop(inRegion,2,isStatic)).
moo:inRegion(O,Region):-atloc(O,LOC),locationToRegion(LOC,Region).
moo:inRegion(apath(Region,Dir),Region):-pathBetween(Region,Dir,_To).
moo:inRegion(O,Region):-is_asserted_gaf(inRegion(O,Region)).

:-dynamic(was_asserted_gaf/1).

is_asserted_gaf(Fact):-was_asserted_gaf(Fact).

member_or_e(E,[L|List]):-!,member(E,[L|List]).
member_or_e(E,E).

is_single_valuedOrFail(F,A,Obj,ARGS):- is_mpred_prop(F,A,singleValued),!,valuedOrThrow(F,A,Obj,ARGS),!.
is_single_valuedOrFail(_,_,_,_):- fail.

valuedOrThrow(F,_,Obj,ARGS):- holds_t(isa,Obj,T), findall_type_default_props(Obj,T,Props),Props=[_|_],Prop=..[F|ARGS], member_or_e(Prop,Props),!.
valuedOrThrow(F,A,Obj,ARGS):- valuedOrThrow1(F,A,Obj,ARGS).
valuedOrThrow1(_F,_A,_Obj,ARGS):- last(ARGS,unknown),!.
valuedOrThrow1(F,A,Obj,ARGS):- trace_or_throw(is_single_valuedOrFail(F,A,Obj,ARGS)).


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

moo:default_type_props(_,food,[height(0)]).

moo:term_specifier_text(Text,pred):- is_mpred_prop(Text,_,arity(_,_)).

% single valued
moo:subclass(agent,object).
moo:subclass(item,object).


mpred_prop(pathName(region,dir,string)).
mpred_prop(verbOverride(term,action,action)).

moo:singleValued(atloc(object,xyz(region,int,int,int))).
moo:singleValued(act_turn(agent,int)).
moo:singleValued(armorLevel(possessable,int)).
moo:singleValued(attack(agent,int)).
moo:singleValued(charge(agent,int)).
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
moo:singleValued(mudMaxHitPoints(agent,int)).
moo:singleValued(mudToHitArmorClass0(agent,int)).
moo:singleValued(pathBetween(region,dir,region)).
moo:singleValued(permanence(item,verb,int)).
moo:singleValued(score(object,int)).
moo:singleValued(spawn_rate(subclass(object),int)).
moo:singleValued(spd(agent,int)).
moo:singleValued(stm(agent,int)).
moo:singleValued(str(agent,int)).
moo:singleValued(type_grid(regiontype,int,list(term))).
moo:singleValued(weight(object,int)).
moo:singleValued(ArgTypes):-mpred_prop_g(ArgTypes).

mpred_prop_format(apath(region,dir),areaPath).
mpred_prop_format(dice(int,int,int),int).


db_resultIsa(apath,areaPath).
moo:subclass(areaPath,door).
moo:subclass(door,item).

moo:subclass(dir,string).

% flags
mpred_prop(agent(id),[flag]).
mpred_prop(item(id),[flag]).
mpred_prop(region(id),[flag]).
mpred_prop(type(id),[flag]).


mpred_prop(thinking(agent),[flag]).
mpred_prop(deleted(id),[flag]).

% multivalued
mpred_prop(G,[multi(AT)|LIST]):-mpred_prop_multi(G,AT,LIST).

mpred_prop(G,[assert_with(game_assert)]):-mpred_prop_game_assert(G).

mpred_prop_game_assert(somethingIsa(term,list(type))).
mpred_prop_game_assert(somethingDescription(term,list(string))).
mpred_prop_game_assert(objects(type,list(id))).
mpred_prop_game_assert(sorts(type,list(type))).

mpred_prop(ArgTypes,[singleValued]):-moo:singleValued(ArgTypes).

mpred_prop(CallSig,[external(M)]):-mpred_prop_prolog(M:CallSig).
:-dynamic(mpred_prop_prolog/1).
%mpred_prop_prolog(world:nearby(object,object)).
%mpred_prop_prolog(world:mud_isa(object,type)).
%mpred_prop_prolog(world:same(id,id)).


% multivalued
mpred_prop_multi(G,AT,[ordered|LIST]):-mpred_prop_multi(G,LIST),functor(G,_,AT).

mpred_prop_multi(named(term,term),[genlpreds(id)]).
mpred_prop_multi(ofclass(term,type),[alias(isa)]).
mpred_prop_multi(G,[]):-mpred_prop_multi(G).

mpred_prop_multi(failure(agent,action)).
mpred_prop_multi(nameStrings(term,string)).
mpred_prop_multi(determinerString(term,string)).
mpred_prop_multi(descriptionHere(term,string)).
mpred_prop_multi(description(term,string)).
mpred_prop_multi(keyword(term,string)).
mpred_prop_multi(act_affect(term,term,term)).
mpred_prop_multi(memory(agent,term)).
mpred_prop_multi(wearing(agent,wearable)).
mpred_prop_multi(grid(region,int,int,object)).
mpred_prop_multi(possess(agent,item)).
mpred_prop_multi(subclass(type,type)).
mpred_prop_multi(isa(term,type)).

mpred_prop(repl_writer(agent,term),[singleValued,default(look:default_repl_writer)]).
mpred_prop(repl_to_string(agent,term),[singleValued,default(look:default_repl_obj_to_string)]).

%mpred_prop(ArgTypes,PropTypes):-moo:decl_mpred_prop(ArgTypes,PropTypes).
% somethingIsa('NpcCol1012-Ensign732',['NpcCol1012',actor,'MaleAnimal']).


define_mpred_prop(ArgTypes,PropTypes):-
   functor(ArgTypes,F,A),
      doall(define_mpred_prop_0(ArgTypes,F,A)),
      doall((member_or_e(PT,PropTypes),define_mpred_prop_1(ArgTypes,F,A,PT))),
      doall(define_mpred_prop_1(ArgTypes,F,A,interArgIsa)),
      doall((member_or_e(PT,PropTypes),define_mpred_prop_2(ArgTypes,F,A,PT))),
      doall(define_mpred_prop_2(ArgTypes,F,A,interArgIsa)),
      doall((member_or_e(PT,PropTypes),define_mpred_prop_3(ArgTypes,F,A,PT))),
      doall(define_mpred_prop_3(ArgTypes,F,A,interArgIsa)),!.


define_argType(F,N,ArgType):-assert_if_new(is_mpred_prop(F,N,argIsa(ArgType))).

% pass 0
define_mpred_prop_0(_ArgTypes,F,A):-assert_if_new(is_mpred_prop(F,A,arity(F,A))),fail.
define_mpred_prop_0(ArgTypes,F,_):-doall((arg(N,ArgTypes,ArgType),define_argType(F,N,ArgType))),fail.

% pass 1
define_mpred_prop_1(_,F,A,PT):-assert_if_new(is_mpred_prop(F,A,PT)).

% pass 2
define_mpred_prop_2(_,F,A,external(Module)):-not(dbase_mod(Module)),!,length(ARGS,A),HEAD=..[F|ARGS],must(predicate_property(Module:HEAD,_)),!.
define_mpred_prop_2(_,F,A,interArgIsa):- not((is_mpred_prop(F,A,external(Module)),not(dbase_mod(Module)))), declare_dbase_local(F,A).
define_mpred_prop_2(_,_,_,_).

% pass 3
define_mpred_prop_3(ArgTypes,F,A,PT):- nop(dmsg(define_mpred_prop_3(ArgTypes,F,A,PT))).


declare_dbase_local(F,A):- is_mpred_prop(F,A,hasStub),!.
declare_dbase_local(F,A):- assert_if_new(is_mpred_prop(F,A,hasStub)),fail.
declare_dbase_local(F,A):- is_mpred_prop(F,A,isStatic),!.
declare_dbase_local(F,A):- dynamic(F/A),user_export(F/A),
      functor(HEAD,F,A),HEAD=..[F|_ARGS],ensure_clause(HEAD,F,A,body_req(F,A,HEAD)),
      assert_if_new(is_mpred_prop(F,A,hasStub)).


body_req(F,A,HEAD):-is_mpred_prop(F,A,external(Module)),!,call(Module:HEAD).
%body_req(isa,2,_):-!,fail.
%body_req(_,_,HEAD):-req(Head).
body_req(F,A,HEAD):-is_mpred_prop(F,A,default(V)),arg(A,HEAD,V).

ensure_clause(HEAD,_,_,_BODY):-not(not((numbervars(clause(HEAD,BODY),'$VAR',0,_),clause(HEAD,BODY)))),!.
% ensure_clause(HEAD,F,A,_):-pred_as_is(F,A), !.
ensure_clause(HEAD,_F,_A,BODY):-assertz((HEAD:-BODY)),
   % this is just to catch asserts at these predicates that are supposed to be contained.. We dont really want them compiled
   nop(compile_predicates([HEAD])).


nameStrings(apath(Region,Dir),Text):- pathName(Region,Dir,Text).
description(apath(Region,Dir),Text):- pathName(Region,Dir,Text).

scan_mpred_prop:-
   dbase_mod(DBM),
   '@'(forall(mpred_prop(ArgTypes,PropTypes),debugOnError0( define_mpred_prop(ArgTypes,PropTypes))),DBM).

load_motel:- defrole([],time_state,restr(time,period)).

:- scan_mpred_prop.

% :-load_motel.

:- include(logicmoo('vworld/moo_footer.pl')).



