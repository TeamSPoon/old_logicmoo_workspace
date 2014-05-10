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

 add/1, add0/1, agent/1, agent_doing/2, agent_done/2, argIsa_call/3, charge/2, ofclass/2, clr/1, damage/2, db_op/2, db_prop/2, db_prop/1, atloc/2, is_db_prop/3,
 db_prop_g/1, db_prop_game_assert/1, del/1, failure/2, grid/4, inRegion/2, is_db_prop/2, is_db_prop/3, isa/2, item/1, 
 memory/2, padd/2, padd/3, pathName/3, possess/2, prop/3, prop_or/4, props/2, region/1, req/1, scan_db_prop/0, score/2, stm/2, term_listing/1,  facing/2,
 thinking/1, type/1, use_term_listing/2, wearing/2, world_clear/1, str/2 ,facing/2, height/2, act_term/2, nameStrings/2, description/2, pathBetween/3, act_turn/2,
 dbase_mod/1, define_db_prop/2,
 clause_present_1/3,
 with_kb_assertions/2
    ]).

:- dynamic 
 dbase_mod/1,
 add/1, add0/1, agent/1, agent_doing/2, agent_done/2, argIsa_call/3, charge/2, ofclass/2, clr/1, damage/2, db_op/2, db_prop/2, db_prop/1, atloc/2, is_db_prop/2, is_db_prop/3,
 db_prop_g/1, db_prop_game_assert/1, del/1, failure/2, grid/4, inRegion/2, is_db_prop/3, isa/2, item/1, 
 memory/2, padd/2, padd/3, pathName/3, possess/2, prop/3, prop_or/4, props/2, region/1, req/1, scan_db_prop/0, score/2, stm/2, term_listing/1, facing/2,
 thinking/1, type/1, use_term_listing/2, wearing/2, world_clear/1, str/2 ,facing/2, height/2, act_term/2, nameStrings/2, description/2, pathBetween/3, act_turn/2.

dbase_mod(dbase).
/*

:- context_module(M),
   asserta(dbase_mod(M)),
   dmsg(assert_if_new(dbase_mod(M))).

*/

user_export(_):- dbase_mod(user),!.
user_export(Prop/Arity):- 
   dbase_mod(M), '@'( M:export(Prop/Arity) , M).

:- multifile db_prop/2.

:- meta_predicate man:with_assertions(:,0).
:- meta_predicate world:intersect(?,0,?,0,0,-).
:- meta_predicate clause_present(:), db_forall_assert_mv(+,+,+), db_forall_assert_sv(+,+,+), db_forall(+,+), db_forall_quf(+,+,+).

:- meta_predicate hooked_asserta(^), hooked_assert(^), hooked_retract(^), hooked_retractall(^).
%% :- meta_predicate del(0),clr(0),add(0),add0(0),req(0), db_op(0,0,0),db_prop(0,0),db_prop(0).

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

pred_as_is(F,A):-is_db_prop(F,A,flag),!.
pred_as_is(F,A):-is_db_prop(F,A,external(_)),!.
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
db_op_disj(Op,Prop,ARGS):-is_db_prop(Prop,_,impl(Other)),!,
	db_op_unit(Op,Other,ARGS,Unit),!,db_forall(Op,Unit).
% alias/1
db_op_disj(Op,Prop,ARGS):-is_db_prop(Prop,_,alias(Other)),!,
	db_op_unit(Op,Other,ARGS,Unit),!,db_forall(Op,Unit).
% inverse/1
db_op_disj(Op,Prop,ARGS):-
      is_db_prop(Prop,_,inverse(Other)),!,
      inverse_args(ARGS,Inverse),
      db_op_unit(Op,Other,Inverse,Unit1),
      db_op_unit(Op,Prop,ARGS,Unit2),
      db_forall(Op,(Unit1;Unit2)).

% assert/1
db_op_disj(a,Prop,ARGS):- is_db_prop(Prop,_,assert(How)),!,call_pa(How,Prop,ARGS).
db_op_disj(Op,Prop,ARGS):- is_db_prop(Prop,W,assert(How)),!,throw(cant(db_op_disj(Op,Prop,ARGS),is_db_prop(Prop,W,assert(How)))).

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
db_forall(a,C0):- db_forall_quf(C0,U,C),must(U),functor(C,F,A),!, (is_db_prop(F,A,singleValued) -> must(db_forall_assert_sv(C,F,A)) ; must(db_forall_assert_mv(C,F,A))).

db_forall(a,C):- functor(C,F,A),!, (is_db_prop(F,A,singleValued) -> must(db_forall_assert_sv(C,F,A)) ; must(db_forall_assert_mv(C,F,A))).
db_forall(r,C):- ground(C),retractall(C).
db_forall(Op,C):-!,trace,throw(unhandled(db_forall(Op,C))).

% only place ever should actual gaem desbase be changed from
hooked_asserta(C):-moo:run_database_hooks(assert(a),C),asserta(C).
hooked_assert(C):- moo:run_database_hooks(assert(z),C), assert(C).
hooked_retract(C):- moo:run_database_hooks(retract(one),C), must(retract(C)).
hooked_retractall(C):- moo:run_database_hooks(retract(all),C), retractall(C).


% assert to a mutlivalue pred
db_forall_assert_mv(C,F,A):-
   (clause_present(C,F,A) -> true; (is_db_prop(F,A,ordered)-> hooked_assert(C) ; hooked_asserta(C))).

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
      
argIsa_call(Prop,N1,_Type):-once((must(nonvar(Prop)),must(number(N1)))),fail.
argIsa_call(isa,1,argIsaFn(isa,1)):-!.
argIsa_call(isa,2,type):-!.
argIsa_call(act,_,term):-!.
argIsa_call(ofclass,2,type):-!.
argIsa_call(memory,2,term):-!.
argIsa_call(Prop,N1,Type):-is_db_prop(Prop,N1,argIsa(Type)),!.
argIsa_call(Prop,N1,Type):-is_2nd_order_holds(Prop),dmsg(todo(define(argIsa_call(Prop,N1,'Second_Order_TYPE')))),
   Type=argIsaFn(Prop,N1).
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
compare_n(NewLast,Last):-number(NewLast),not(number(Last)),throw(incomparable_terms(Last,NewLast)).
compare_n(Last,NewLast):-number(NewLast),not(number(Last)),throw(incomparable_terms(Last,NewLast)).
compare_n(NewLast,Last):-atomic(NewLast),not(atomic(Last)),throw(incomparable_terms(Last,NewLast)).
compare_n(Last,NewLast):-atomic(NewLast),not(atomic(Last)),throw(incomparable_terms(Last,NewLast)).

inRegion(O,Region):-atloc(O,LOC),locationToRegion(LOC,Region).
inRegion(apath(Region,Dir),Region):-pathBetween(Region,Dir,_To).


member_or_e(E,[L|List]):-!,member(E,[L|List]).
member_or_e(E,E).

is_single_valuedOrFail(F,A,Obj,ARGS):-is_db_prop(F,A,singleValued),!,valuedOrThrow(F,A,Obj,ARGS),!.
is_single_valuedOrFail(_,_,_,_):-fail.

valuedOrThrow(F,_,Obj,ARGS):-mud_isa(Obj,T),findall_type_default_props(Obj,T,Props),Props=[_|_],Prop=..[F|ARGS], member_or_e(Prop,Props),!.
valuedOrThrow(F,A,Obj,ARGS):-valuedOrThrow1(F,A,Obj,ARGS).
valuedOrThrow1(_F,_A,_Obj,ARGS):-last(ARGS,unknown),!.
valuedOrThrow1(F,A,Obj,ARGS):-throw(is_single_valuedOrFail(F,A,Obj,ARGS)).

moo:type_default_props(_,food,[height(0)]).

update_value(OLD,+X,NEW):-number(OLD),catch(NEW is OLD + X,_,fail),!.
update_value(OLD,-X,NEW):-number(OLD),catch(NEW is OLD - X,_,fail),!.
update_value(_OLD,NEW,NEW).

moo:specifier_text(Text,pred):- is_db_prop(Text,_,arity(_,_)).

% single valued
moo:decl_subclass(agent,object).
moo:decl_subclass(item,object).


db_prop(pathName(region,dir,string)).
db_prop(verbOverride(term,action,action)).

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
db_prop_sv(inRegion(term,region)).
db_prop_sv(last_command(agent,command)).
db_prop_sv(location_center(region,xyz(region,int,int,int))).
db_prop_sv(movedist(agent,number)).
db_prop_sv(mudBareHandDamage(agent,dice)).
db_prop_sv(mudLevelOf(possessable,int)).
db_prop_sv(mudMaxHitPoints(agent,int)).
db_prop_sv(mudToHitArmorClass0(agent,int)).
db_prop_sv(pathBetween(region,dir,region)).
db_prop_sv(permanence(item,verb,int)).
db_prop_sv(score(object,int)).
db_prop_sv(spawn_rate(subclass(object),int)).
db_prop_sv(spd(agent,int)).
db_prop_sv(stm(agent,int)).
db_prop_sv(str(agent,int)).
db_prop_sv(type_grid(regiontype,int,list(term))).
db_prop_sv(weight(object,int)).
db_prop_sv(ArgTypes):-db_prop_g(ArgTypes).

db_prop_format(apath(region,dir),areaPath).
db_prop_format(dice(int,int,int),int).


db_resultIsa(apath,areaPath).
moo:decl_subclass(areaPath,door).
moo:decl_subclass(door,item).

moo:decl_subclass(dir,string).

% flags
db_prop(agent(id),[flag]).
db_prop(item(id),[flag]).
db_prop(region(id),[flag]).
db_prop(type(id),[flag]).


db_prop(thinking(agent),[flag]).
db_prop(deleted(id),[flag]).

% multivalued
db_prop(G,[multi(AT)|LIST]):-db_prop_multi(G,AT,LIST).

db_prop(G,[assert(game_assert)]):-db_prop_game_assert(G).

db_prop_game_assert(somethingIsa(term,list(type))).
db_prop_game_assert(somethingDescription(term,list(string))).
db_prop_game_assert(objects(type,list(id))).
db_prop_game_assert(sorts(type,list(type))).

db_prop(ArgTypes,[singleValued]):-db_prop_sv(ArgTypes).

db_prop(CallSig,[external(M)]):-db_prop_prolog(M:CallSig).
:-dynamic(db_prop_prolog/1).
%db_prop_prolog(world:nearby(object,object)).
%db_prop_prolog(world:mud_isa(object,type)).
%db_prop_prolog(world:same(id,id)).


% multivalued
db_prop_multi(G,AT,[ordered|LIST]):-db_prop_multi(G,LIST),functor(G,_,AT).

db_prop_multi(named(term,term),[genlpreds(id)]).
db_prop_multi(ofclass(term,type),[alias(isa)]).
db_prop_multi(G,[]):-db_prop_multi(G).

db_prop_multi(failure(agent,action)).
db_prop_multi(nameStrings(term,string)).
db_prop_multi(determinerString(term,string)).
db_prop_multi(descriptionHere(term,string)).
db_prop_multi(description(term,string)).
db_prop_multi(keyword(term,string)).
db_prop_multi(act(term,term,term)).
db_prop_multi(memory(agent,term)).
db_prop_multi(wearing(agent,wearable)).
db_prop_multi(grid(region,int,int,object)).
db_prop_multi(possess(agent,item)).
db_prop_multi(subclass(type,type)).
db_prop_multi(isa(term,type)).

db_prop(repl_writer(agent,term),[singleValued,default(look:default_repl_writer)]).
db_prop(repl_to_string(agent,term),[singleValued,default(look:default_repl_obj_to_string)]).

db_prop(ArgTypes,PropTypes):-moo:decl_db_prop(ArgTypes,PropTypes).
% somethingIsa('NpcCol1012-Ensign732',['NpcCol1012',actor,'MaleAnimal']).


define_db_prop(ArgTypes,PropTypes):-
   functor(ArgTypes,F,A),
      doall(define_db_prop_0(ArgTypes,F,A)),
      doall((member_or_e(PT,PropTypes),define_db_prop_1(ArgTypes,F,A,PT))),
      doall(define_db_prop_1(ArgTypes,F,A,interArgIsa)),
      doall((member_or_e(PT,PropTypes),define_db_prop_2(ArgTypes,F,A,PT))),
      doall(define_db_prop_2(ArgTypes,F,A,interArgIsa)),
      doall((member_or_e(PT,PropTypes),define_db_prop_3(ArgTypes,F,A,PT))),
      doall(define_db_prop_3(ArgTypes,F,A,interArgIsa)),!.


define_argType(F,N,ArgType):-assert_if_new(is_db_prop(F,N,argIsa(ArgType))).

% pass 0
define_db_prop_0(ArgTypes,F,A):-assert_if_new(is_db_prop(F,A,arity(F,A))),fail.
define_db_prop_0(ArgTypes,F,_):-doall((arg(N,ArgTypes,ArgType),define_argType(F,N,ArgType))),fail.

% pass 1
define_db_prop_1(_,F,A,PT):-assert_if_new(is_db_prop(F,A,PT)).

% pass 2
define_db_prop_2(_,F,A,external(Module)):-not(dbase_mod(Module)),!,length(ARGS,A),HEAD=..[P|ARGS],must(predicate_property(Module:HEAD,_)),!.
define_db_prop_2(_,F,A,interArgIsa):- not((is_db_prop(F,A,external(Module)),not(dbase_mod(Module)))), declare_dbase_local(F,A).
define_db_prop_2(_,_,_,_).

% pass 3
define_db_prop_3(ArgTypes,F,A,PT):- nop(dmsg(define_db_prop_3(ArgTypes,F,A,PT))).


declare_dbase_local(F,A):- is_db_prop(F,A,hasStub),!.
declare_dbase_local(F,A):- dynamic(F/A),user_export(F/A),
      functor(HEAD,F,A),HEAD=..[F|ARGS],ensure_clause(HEAD,F,A,body_req(F,A,HEAD)),
      assert_if_new(is_db_prop(F,A,hasStub)).


body_req(F,A,HEAD):-is_db_prop(F,A,external(Module)),!,call(Module:HEAD).
%body_req(isa,2,_):-!,fail.
%body_req(_,_,HEAD):-req(Head).
body_req(F,A,HEAD):-is_db_prop(F,A,default(V)),arg(A,HEAD,V).

ensure_clause(HEAD,_,_,_BODY):-not(not((numbervars(clause(HEAD,BODY),'$VAR',0,_),clause(HEAD,BODY)))),!.
% ensure_clause(HEAD,F,A,_):-pred_as_is(F,A), !.
ensure_clause(HEAD,F,A,BODY):-assertz((HEAD:-BODY)),
   % this is just to catch asserts at these predicates that are supposed to be contained.. We dont really want them compiled
   nop(compile_predicates([HEAD])).


nameStrings(apath(Region,Dir),Text):- pathName(Region,Dir,Text).
description(apath(Region,Dir),Text):- pathName(Region,Dir,Text).

scan_db_prop:-
   dbase_mod(DBM),
   '@'(forall(db_prop(ArgTypes,PropTypes),debugOnError0( define_db_prop(ArgTypes,PropTypes))),DBM).

load_motel:- defrole([],time_state,restr(time,period)).

:- scan_db_prop.

:-load_motel.

:- include(logicmoo('vworld/moo_footer.pl')).


