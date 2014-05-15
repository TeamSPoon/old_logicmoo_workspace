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

:- module(dbase, [

 add/1, add0/1, agent/1, agent_doing/2, agent_done/2, argIsa_call/3, charge/2, ofclass/2, clr/1, damage/2, db_op/2, atloc/2, 
 db_prop_g/1, db_prop_game_assert/1, del/1, failure/2, grid/4, mud_isa/2, item/1, 
 memory/2, padd/2, padd/3, pathName/3, possess/2, prop/3, prop_or/4, props/2, region/1, req/1, scan_db_prop/0, score/2, stm/2, term_listing/1,  facing/2,
 thinking/1, type/1, use_term_listing/2, wearing/2, world_clear/1, str/2 ,facing/2, height/2, act_term/2, nameString_call/2, description/2, act_turn/2,
 dbase_mod/1, dbase_define_db_prop/2,
 clause_present_1/3,
 with_kb_assertions/2
    ]).




:- dynamic 
 dbase_mod/1,
 add/1, add0/1, agent/1, agent_doing/2, agent_done/2, argIsa_call/3, charge/2, ofclass/2,  damage/2, db_op/2,atloc/2, 
 db_prop_g/1, db_prop_game_assert/1, del/1, failure/2, grid/4, mud_isa/2, item/1, 
 memory/2, pathName/3, possess/2, region/1, req/1, scan_db_prop/0, score/2, stm/2, term_listing/1, facing/2,
 thinking/1, type/1, use_term_listing/2, wearing/2, world_clear/1, str/2 ,facing/2, height/2, act_term/2, nameString_call/2, description/2, act_turn/2.

dbase_mod(dbase).
/*

:- context_module(M),
   asserta(dbase_mod(M)),
   dmsg(assert_if_new(dbase_mod(M))).

*/

user_export(_):- dbase_mod(user),!.
user_export(_M:Prop/Arity):-!,user_export(Prop/Arity).
user_export(Prop/Arity):- 
   dbase_mod(M), '@'( M:export(Prop/Arity) , M).


:- meta_predicate man:with_assertions(:,0).
:- meta_predicate world:intersect(?,0,?,0,0,-).
:- meta_predicate clause_present(:), db_forall_assert_mv(+,+,+), db_forall_assert_sv(+,+,+), db_forall(+,+), db_forall_quf(+,+,+).

:- meta_predicate hooked_asserta(^), hooked_assert(^), hooked_retract(^), hooked_retractall(^).
% % :- meta_predicate del(0),clr(0),add(0),add0(0),req(0), db_op(0,0,0),moo:db_prop(0,0),moo:db_prop(0).

% Found new meta-predicates in iteration 1 (0.281 sec)
%:- meta_predicate db_forall(?,?,?,0).

:- include(logicmoo('vworld/moo_header.pl')).

:- include('dbase_types_motel').

:- register_module_type(utility).

moo:decl_action(list(term)).
moo:agent_call_command(_Gent,list(Obj)):- term_listing(Obj).

term_listing(Obj):- catch(listing(Obj),_,fail),fail.
term_listing(Obj):-
   doall((
   predicate_property(H,number_of_clauses(_)),
   clause(H,B),
   use_term_listing(Obj,((H:-B))),
   show_term_listing(H,B),
   fail)).


use_term_listing(Obj,HB):- once((subst(HB,Obj,fov,H1B1), H1B1 \= HB)),!.
use_term_listing(Obj,HB):- term_to_atom(Obj,HO), term_to_atom(HB,HBO), sub_atom_icasechk(HBO,_,HO),!.


show_term_listing(H,true):- !, show_term_listing(H).
show_term_listing(H,B):- show_term_listing((H:-B)).

show_term_listing(H):- writeq(H),write('.'),nl,!.

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
add0(C0):- db_op(a,C0).
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

pred_as_is(F,A):-moo:is_db_prop(F,A,flag),!.
pred_as_is(F,A):-moo:is_db_prop(F,A,external(_)),!.
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
db_op0('q',props(Obj,PropVal)):- var(Obj),!,
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
db_op_4(Op,k,_,C0):- C0=..[k,Prop|ARGS],atom(Prop), C1=..[Prop|ARGS],!,db_op(Op,C1),!.
db_op_4(Op,p,_,C0):- C0=..[p,Prop|ARGS],atom(Prop), C1=..[Prop|ARGS],!,db_op(Op,C1),!.
db_op_4(Op,k,_,C0):- C0=..[k,Prop|ARGS],atom(Prop),C1=..[Prop|ARGS],!,db_op(Op,C1),!.
db_op_4(Op,p,_,C0):- C0=..[p,Prop|ARGS],atom(Prop), C1=..[Prop|ARGS],!,db_op(Op,C1),!.
db_op_4(Op,svo,_,C0):- C0=..[svo,Obj,Prop|ARGS],atom(Prop),C1=..[Prop,Obj|ARGS],!,db_op(Op,C1),!.
db_op_4(q,Fmt,1,C0):-is_decl_ft(Fmt),!,C0=..[_,A],format_complies(A,Fmt,_).
db_op_4(a,Fmt,1,_C0):-is_decl_ft(Fmt),!,dmsg(todo(dont_assert_is_decl_ft(Fmt))),!.
db_op_4(Op,Prop,_,C0):- 
   C0=..[Prop|ARGS],db_op_disj(Op,Prop,ARGS).

db_op_unit(_Op,Prop,ARGS,C0):- must(atom(Prop)), C0=..[Prop|ARGS].

% impl/1
db_op_disj(Op,Prop,ARGS):-moo:is_db_prop(Prop,_,impl(Other)),!,
	db_op_unit(Op,Other,ARGS,Unit),!,db_forall(Op,Unit).
% alias/1
db_op_disj(Op,Prop,ARGS):-moo:is_db_prop(Prop,_,alias(Other)),!,
	db_op_unit(Op,Other,ARGS,Unit),!,db_forall(Op,Unit).
% inverse/1
db_op_disj(Op,Prop,ARGS):-
      moo:is_db_prop(Prop,_,inverse(Other)),!,
      inverse_args(ARGS,Inverse),
      db_op_unit(Op,Other,Inverse,Unit1),
      db_op_unit(Op,Prop,ARGS,Unit2),
      db_forall(Op,(Unit1;Unit2)).

% assert/1
db_op_disj(a,Prop,ARGS):- moo:is_db_prop(Prop,_,assert(How)),!,call_pa(How,Prop,ARGS).
db_op_disj(Op,Prop,ARGS):- moo:is_db_prop(Prop,W,assert(How)),!,throw(cant(db_op_disj(Op,Prop,ARGS),moo:is_db_prop(Prop,W,assert(How)))).

% plain prop
db_op_disj(Op,Prop,ARGS):-db_op_unit(Op,Prop,ARGS,Unit),db_forall(Op,Unit).

call_pa(How,Prop,ARGS):-PROPARGS=..[Prop|ARGS], Call=..[How,PROPARGS],must(Call).

db_forall(Op,(C1,C2)):-!,db_forall(Op,C1),db_forall(Op,C2).
db_forall(r,(C1;C2)):-!,trace,once((db_forall(r,C1),db_forall(r,C2))).
db_forall(ra,(C1;C2)):-!,must(db_forall(ra,C1)),must(db_forall(ra,C2)).
db_forall('q',(C1;C2)):-!, db_forall('q',C1) ; db_forall('q',C2).
db_forall(a,(C1;C2)):-!,db_forall(a,C1),!,db_forall(a,C2),!.
db_forall('q',C):-!,db_forall_quf(C,U,Template),!,U, debugOnError(Template).
db_forall(u,C):- trace,db_forall_quf(C,U,Template),U,Template,must(ground(Template)),!,ignore(retractall(Template)).
db_forall(ra,C):-db_forall_quf(C,U,Template), doall((U,retractall(Template))).
db_forall(ra,C):-ignore(retractall(C)).
db_forall(a,C0):- db_forall_quf(C0,U,C),must(U),functor(C,F,A),!, (moo:is_db_prop(F,A,singleValued) -> must(db_forall_assert_sv(C,F,A)) ; must(db_forall_assert_mv(C,F,A))).

db_forall(a,C):- functor(C,F,A),!, (moo:is_db_prop(F,A,singleValued) -> must(db_forall_assert_sv(C,F,A)) ; must(db_forall_assert_mv(C,F,A))).
db_forall(r,C):- ground(C),retractall(C).
db_forall(Op,C):-!,trace,throw(unhandled(db_forall(Op,C))).

% only place ever should actual gaem desbase be changed from
hooked_asserta(C):-moo:run_database_hooks(assert(a),C),asserta(C).
hooked_assert(C):- moo:run_database_hooks(assert(z),C), assert(C).
hooked_retract(C):- moo:run_database_hooks(retract(one),C), must(retract(C)).
hooked_retractall(C):- moo:run_database_hooks(retract(all),C), retractall(C).


% assert to a mutlivalue pred
db_forall_assert_mv(C,F,A):-
   (clause_present(C,F,A) -> true; (moo:is_db_prop(F,A,ordered)-> hooked_assert(C) ; hooked_asserta(C))).

% assert to a singlevalue pred
db_forall_assert_sv(C,F,A):- clause_present(C,F,A),!.
db_forall_assert_sv(C0,_Prop,A):- 
   copy_term(c(_,C0),c(_,C)),
      arg(A,C,Update),
      db_forall_assert_sv_old_value(C,A,OLD),
      update_value(OLD,Update,NEW),
      must(nonvar(NEW)),
      setarg(A,C,NEW),      
      must_asserta(C).

db_forall_assert_sv_old_value(C,A,OLD):-
   copy_term(C,CC),
   must(var(OLD)),
      setarg(A,CC,OLD),
      % this binds or leave OLD
      (CC -> must(hooked_retract(CC)) ; OLD= _ ).

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
clause_present(C,F,1):-C=..[F,A], is_decl_ft(F), format_complies(A,F,_).
clause_present(C,_F,_A):- not(predicate_property(C,_)),!,fail.
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


is_2nd_order_holds(Prop):-member(Prop,[call,req,k,p,holds,not_k,not_p,not_holds]).

must_asserta(C):-
      must(ground(C)),
      must(hooked_asserta(C)),!.
      
argIsa_call(Prop,N1,Type):-once((must(nonvar(Prop)),must(number(N1)))),fail.
argIsa_call(Prop,N1,Type):-argIsa_call_0(Prop,N1,Type),!.
argIsa_call(Prop,N1,Type):-argIsa_call_1(Prop,N1,Type),!.

argIsa_call_0(mud_isa,1,argIsaFn(mud_isa,1)):-!.
argIsa_call_0(mud_isa,2,type):-!.
argIsa_call_0(act,_,term):-!.
argIsa_call_0(ofclass,2,type):-!.
argIsa_call_0(memory,2,term):-!.
argIsa_call_0(Prop,N1,Type):-moo:is_db_prop(Prop,N1,argIsa(Type)),!.

argIsa_call_1(Prop,N1,Type):-is_2nd_order_holds(Prop),dmsg(todo(define(argIsa_call(Prop,N1,'Second_Order_TYPE')))),
   Type=argIsaFn(Prop,N1).
argIsa_call_1(Prop,N1,Type):-dmsg(todo(define(argIsa_call(Prop,N1,'_TYPE')))),
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
compare_n(NewLast,Last):-number(NewLast),not(number(Last)),throw(incomparable_terms(Last,NewLast)).
compare_n(Last,NewLast):-number(NewLast),not(number(Last)),throw(incomparable_terms(Last,NewLast)).
compare_n(NewLast,Last):-atomic(NewLast),not(atomic(Last)),throw(incomparable_terms(Last,NewLast)).
compare_n(Last,NewLast):-atomic(NewLast),not(atomic(Last)),throw(incomparable_terms(Last,NewLast)).

e2c_data:inRegion(O,Region):-atloc(O,LOC),locationToRegion(LOC,Region).
e2c_data:inRegion(apath(Region,Dir),Region):-e2c_data:pathBetween(Region,Dir,_To).


member_or_e(E,[L|List]):-!,member(E,[L|List]).
member_or_e(E,E).

is_single_valuedOrFail(F,A,Obj,ARGS):-moo:is_db_prop(F,A,singleValued),!,valuedOrThrow(F,A,Obj,ARGS),!.
is_single_valuedOrFail(_,_,_,_):-fail.

valuedOrThrow(F,_,Obj,ARGS):-mud_isa(Obj,T),findall_type_default_props(Obj,T,Props),Props=[_|_],Prop=..[F|ARGS], member_or_e(Prop,Props),!.
valuedOrThrow(F,A,Obj,ARGS):-valuedOrThrow1(F,A,Obj,ARGS).
valuedOrThrow1(_F,_A,_Obj,ARGS):-last(ARGS,unknown),!.
valuedOrThrow1(F,A,Obj,ARGS):-throw(is_single_valuedOrFail(F,A,Obj,ARGS)).

moo:type_default_props(_,food,[height(0)]).

update_value(OLD,+X,NEW):-number(OLD),catch(NEW is OLD + X,_,fail),!.
update_value(OLD,-X,NEW):-number(OLD),catch(NEW is OLD - X,_,fail),!.
update_value(_OLD,NEW,NEW).

moo:specifier_text(Text,pred):- moo:is_db_prop(Text,_,arity(_,_)).

% single valued
moo:decl_subclass(agent,object).
moo:decl_subclass(item,object).


moo:db_prop(pathName(region,dir,string)).
moo:db_prop(verbOverride(term,action,action)).

db_prop_sv(atloc(object,xyz(region,int,int,int))).
db_prop_sv(act_turn(agent,int)).
db_prop_sv(armorLevel(possessable,int)).
db_prop_sv(attack(agent,int)).
db_prop_sv(charge(agent,int)).
db_prop_sv(chargeCapacity(chargable,int)).
db_prop_sv(chargeRemaining(chargable,int)).
db_prop_sv(damage(agent,int)).
db_prop_sv(defence(agent,int)).
db_prop_sv(facing(agent,dir)).
db_prop_sv(height(agent,int)).
db_prop_sv(id(object,id)).
db_prop_sv(e2c_data:inRegion(term,region)).
db_prop_sv(last_command(agent,command)).
db_prop_sv(location_center(region,xyz(region,int,int,int))).
db_prop_sv(movedist(agent,number)).
db_prop_sv(mudBareHandDamage(agent,dice)).
db_prop_sv(mudLevelOf(possessable,int)).
db_prop_sv(mudMaxHitPoints(agent,int)).
db_prop_sv(mudToHitArmorClass0(agent,int)).
db_prop_sv(e2c_data:pathBetween(region,dir,region)).
db_prop_sv(permanence(item,verb,int)).
db_prop_sv(score(object,int)).
db_prop_sv(spawn_rate(moo:subclass(object),int)).
db_prop_sv(spd(agent,int)).
db_prop_sv(stm(agent,int)).
db_prop_sv(str(agent,int)).
% db_prop_sv(type_grid(regiontype,int,list(term))).
db_prop_sv(weight(object,int)).
db_prop_sv(ArgTypes):-db_prop_g(ArgTypes).

db_prop_format(apath(region,dir),areaPath).
db_prop_format(dice(int,int,int),int).


db_resultIsa(apath,areaPath).
moo:decl_subclass(areaPath,door).
moo:decl_subclass(door,item).

moo:decl_subclass(dir,string).

% flags
moo:db_prop(agent(id),[flag]).
moo:db_prop(item(id),[flag]).
moo:db_prop(region(id),[flag]).
moo:db_prop(type(id),[flag]).


moo:db_prop(thinking(agent),[flag]).
moo:db_prop(deleted(id),[flag]).

% multivalued
moo:db_prop(G,[multi(AT)|LIST]):-db_prop_multi(G,AT,LIST).

moo:db_prop(G,[assert(game_assert)]):-db_prop_game_assert(G).

db_prop_game_assert(somethingIsa(term,list(type))).
db_prop_game_assert(somethingDescription(term,list(string))).
db_prop_game_assert(objects(type,list(id))).
db_prop_game_assert(sorts(type,list(type))).

moo:db_prop(ArgTypes,[singleValued]):-db_prop_sv(ArgTypes).

moo:db_prop(CallSig,[external(M)]):-db_prop_prolog(M:CallSig).
:-dynamic(db_prop_prolog/1).
%db_prop_prolog(world:nearby(object,object)).
%db_prop_prolog(world:mud_isa(object,type)).
%db_prop_prolog(world:same(id,id)).

replace_nth([],_N,OldVar,_NewVar,[]):-!,throw(missed_the_boat).
replace_nth([OldVar|ARGS],1,OldVar,NewVar,[NewVar|ARGS]):-!.
replace_nth([Carry|ARGS],Which,OldVar,NewVar,[Carry|NEWARGS]):-
 Which1 is Which-1,
 replace_nth(ARGS,Which1,OldVar,NewVar,NEWARGS),!.


insert_into(ARGS,0,Insert,[Insert|ARGS]):- !.
insert_into([Carry|ARGS],After,Insert,[Carry|NEWARGS]):-
   After1 is After - 1,
   insert_into(ARGS,After1,Insert,NEWARGS).

moo:term_anglify_last(Head,English):-compound(Head),
   functor(Head,F,A),A>1,
   not(ends_with_icase(F,"Fn")),not(starts_with_icase(F,"SKF-")),
   atom_codes(F,[C|_]),code_type(C,lower),
   Head=..[F|ARGS],
   term_anglify_args(Head,F,A,ARGS,singleValued,English).

moo:term_anglify(Head,English):-
      functor(Head,F,A),
      moo:is_db_prop(F,A,Info),member(Info,[singleValued,multi(_)]),
      Head=..[F|ARGS],
      term_anglify_args(Head,F,A,ARGS,Info,English),fully_expand(English,EnglishO),!.



term_anglify_args(Head,F,A,ARGS,multi(Which),English):- !,replace_nth(ARGS,Which,OldVar,NewVar,NEWARGS),!,
      NewHead=..[F|NEWARGS], findall(NewVar,NewHead,ListNewVar),list_to_set_safe(ListNewVar,SetNewVar),NewVar=list(ListNewVar),
      term_anglify_args(Head,F,A,NewHead,singleValued,English).
term_anglify_args(Head,F,A,ARGS0,singleValued,English):- add_arg_parts_of_speech(F,1,ARGS0,ARGS),verb_after_arg(F,A,After),
   insert_into(ARGS,After,verbFn(F),NEWARGS),
   fully_expand(NEWARGS,English),!.

unCamelCase(S,String):-any_to_string(S,Str),S\=Str,!,unCamelCase(Str,String),!.
unCamelCase("",""):-!.
unCamelCase(S,String):-sub_string(S,0,1,_,Char),sub_string(S,1,_,0,Rest),unCamelCase(Rest,RestString),string_lower(Char,NewChar),
  (Char\=NewChar->atomics_to_string(['_',NewChar,RestString],String);atomics_to_string([Char,RestString],String)),!.

moo:term_anglify(verbFn(mud_isa),[is,a]):-!.
moo:term_anglify(verbFn(isa),[is,a]):-!.
moo:term_anglify(verbFn(F),[is|UL]):-not(string_lower(F,F)),unCamelCase(F,U),atomics_to_string(UL,"_",U).
moo:term_anglify(verbFn(F),[is,F]):-atom_concat(_,'ing',F).
moo:term_anglify(verbFn(F),[F,is]).
% moo:term_anglify(prolog(Term),String):-term_to_atom(Term,Atom),any_to_string(Atom,String).
moo:term_anglify(determinerString(Obj,Text),[np(Obj),is,uses,string(Text),as,a,determiner]).
moo:term_anglify(nameString_call(Obj,Text),[np(Obj),is,refered,to,as,string(Text)]).
moo:term_anglify(moo:term_anglify(Term,List),[prolog(Term),is,converted,to,english,using,prolog(Text)]).


add_arg_parts_of_speech(_F,_N,[],[]).
add_arg_parts_of_speech(F,N,[A|ARGS0],[ARG|ARGS]):-argIsa_call_or_undressed(F,N,A,ARG),N1 is N+1, add_arg_parts_of_speech(F,N1,ARGS0,ARGS).

argIsa_call_or_undressed(F,N,Obj,fN(Obj,Type)):-argIsa_call_0(F,N,Type),!.
argIsa_call_or_undressed(F,N,Obj,Obj).

verb_after_arg(_,_,1).

% multivalued
db_prop_multi(G,AT,[ordered|LIST]):-db_prop_multi(G,LIST),functor(G,_,AT).

db_prop_multi(named(term,term),[genlpreds(id)]).
db_prop_multi(ofclass(term,type),[alias(mud_isa)]).
db_prop_multi(G,[]):-db_prop_multi(G).

db_prop_multi(failure(agent,action)).
db_prop_multi(nameString_call(term,string)).
db_prop_multi(determinerString(term,string)).
db_prop_multi(descriptionHere(term,string)).
db_prop_multi(description(term,string)).
db_prop_multi(keyword(term,string)).
db_prop_multi(act(term,term,term)).
db_prop_multi(memory(agent,term)).
db_prop_multi(wearing(agent,wearable)).
db_prop_multi(grid(region,int,int,object)).
db_prop_multi(possess(agent,item)).
db_prop_multi(moo:subclass(type,type)).
db_prop_multi(mud_isa(term,type)).

moo:db_prop(repl_writer(agent,term),[singleValued,default(look:default_repl_writer)]).
moo:db_prop(repl_to_string(agent,term),[singleValued,default(look:default_repl_obj_to_string)]).

% somethingIsa('NpcCol1012-Ensign732',['NpcCol1012',actor,'MaleAnimal']).

dbase_define_db_prop(_M:ArgTypes,PropTypes):-!,
   dbase_define_db_prop(ArgTypes,PropTypes),!.
   
dbase_define_db_prop(ArgTypes,PropTypes):-
   functor(ArgTypes,F,A),
      doall(define_db_prop_0(ArgTypes,F,A)),
      doall((member_or_e(PT,PropTypes),define_db_prop_1(ArgTypes,F,A,PT))),
      doall(define_db_prop_1(ArgTypes,F,A,interArgIsa)),
      doall((member_or_e(PT,PropTypes),define_db_prop_2(ArgTypes,F,A,PT))),
      doall(define_db_prop_2(ArgTypes,F,A,interArgIsa)),
      doall((member_or_e(PT,PropTypes),define_db_prop_3(ArgTypes,F,A,PT))),
      doall(define_db_prop_3(ArgTypes,F,A,interArgIsa)),!.


define_argType(F,N,ArgType):-assert_if_new(moo:is_db_prop(F,N,argIsa(ArgType))).

% pass 0
define_db_prop_0(ArgTypes,F,A):-assert_if_new(moo:is_db_prop(F,A,arity(F,A))),fail.
define_db_prop_0(ArgTypes,F,_):-doall((arg(N,ArgTypes,ArgType),define_argType(F,N,ArgType))),fail.

% pass 1
define_db_prop_1(_,F,A,PT):-assert_if_new(moo:is_db_prop(F,A,PT)).

% pass 2
define_db_prop_2(_,F,A,external(Module)):-not(dbase_mod(Module)),!,length(ARGS,A),HEAD=..[P|ARGS],must(predicate_property(Module:HEAD,_)),!.
define_db_prop_2(_,F,A,interArgIsa):- not((moo:is_db_prop(F,A,external(Module)),not(dbase_mod(Module)))), declare_dbase_local(F,A).
define_db_prop_2(_,_,_,_).

% pass 3
define_db_prop_3(ArgTypes,F,A,PT):- nop(dmsg(define_db_prop_3(ArgTypes,F,A,PT))).

declare_dbase_local(_M:F,A):-!, declare_dbase_local(F,A).
declare_dbase_local(F,A):- moo:is_db_prop(F,A,hasStub),!.
declare_dbase_local(F,A):- functor(Pred,F,A),predicate_property(Pred,imported_from(Module)),not(dbase_mod(Module)),!.
declare_dbase_local(F,A):- dynamic(F/A),user_export(F/A),
      functor(HEAD,F,A),HEAD=..[F|ARGS],ensure_clause(HEAD,F,A,body_req(F,A,HEAD)),
      assert_if_new(moo:is_db_prop(F,A,hasStub)).


body_req(F,A,HEAD):-moo:is_db_prop(F,A,external(Module)),!,call(Module:HEAD).
%body_req(mud_isa,2,_):-!,fail.
%body_req(_,_,HEAD):-req(Head).
body_req(F,A,HEAD):-moo:is_db_prop(F,A,default(V)),arg(A,HEAD,V).

ensure_clause(_M:HEAD,_,_,_BODY):-!,ensure_clause(HEAD,_,_,_BODY).
ensure_clause(HEAD,_,_,_BODY):-not(not((numbervars(clause(HEAD,BODY),'$VAR',0,_),clause(HEAD,BODY)))),!.
% ensure_clause(HEAD,F,A,_):-pred_as_is(F,A), !.
ensure_clause(HEAD,F,A,BODY):-assertz((HEAD:-BODY)),
   % this is just to catch asserts at these predicates that are supposed to be contained.. We dont really want them compiled
   nop(compile_predicates([HEAD])).


nameString_call(apath(Region,Dir),Text):- pathName(Region,Dir,Text).
description(apath(Region,Dir),Text):- pathName(Region,Dir,Text).

scan_db_prop:-
   dbase_mod(DBM),debug,
   '@'(forall(moo:db_prop(ArgTypes,PropTypes),debugOnError0( dbase_define_db_prop(ArgTypes,PropTypes))),DBM).

load_motel:- defrole([],time_state,restr(time,period)).

:- scan_db_prop.


:-load_motel.


:- include(logicmoo('vworld/moo_footer.pl')).

end_of_file.



:-dmsg(loading(kb3)).
call_assertion_holds(P,A):- 'ASSERTION'(_TRUTH,_NNF,_MT,_,/*HL*/[P,A]).
call_assertion_holds(P,A,B):- 'ASSERTION'(_TRUTH,_NNF,_MT,_,/*HL*/[P,A,B]).
call_assertion_holds(P,A,B,C):- 'ASSERTION'(_TRUTH,_NNF,_MT,_,/*HL*/[P,A,B,C]).
call_assertion_holds(P,A,B,C,D):- 'ASSERTION'(_TRUTH,_NNF,_MT,_,/*HL*/[P,A,B,C,D]).
call_assertion_holds(P,A,B,C,D,E):- 'ASSERTION'(_TRUTH,_NNF,_MT,_,/*HL*/[P,A,B,C,D,E]).
call_assertion_holds(P,A,B,C,D,E,F):- 'ASSERTION'(_TRUTH,_NNF,_MT,_,/*HL*/[P,A,B,C,D,E,F]).

call_assertion_holds(P,A):-assertion_holds(P,A).
call_assertion_holds(P,A,B):-assertion_holds(P,A,B).
call_assertion_holds(P,A,B,C):-assertion_holds(P,A,B,C).
call_assertion_holds(P,A,B,C,D):-assertion_holds(P,A,B,C,D).
call_assertion_holds(P,A,B,C,D,E):-assertion_holds(P,A,B,C,D,E).
call_assertion_holds(P,A,B,C,D,E,F):-assertion_holds(P,A,B,C,D,E,F).


end_of_file.




