% database.pl
% July 7, 1996
% John Eikenberry
% Dec 13, 2035
% Douglas Miles
%
/** <module> % File used as storage place for all predicates which change as
% the world is run.
%
% props(Obj,height(ObjHt))  == k(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt))  == padd(height,Obj,ObjHt,...) == add(QueryForm)
% kretract[all](Obj,height(ObjHt))  == kretract[all](Obj,height,ObjHt) == pretract[all](height,Obj,ObjHt) == del[all](QueryForm)
% keraseall(AnyTerm).
%
*/
:- module(dbase, [
          scan_db_prop/0,
    world_clear/1,
    del/1,
    clr/1,
    classof/2,
    add/1,
    req/1,
    props/2,
    prop/3,
    prop_or/4,
    padd/3,
    type/1,
    padd/2,
    db_op/2,
    pathName/3,
    argIsa_call/3,
    db_prop_game_assert/1,
    db_prop/2,
    atloc/2, charge/2, score/2, damage/2, failure/2, thinking/1, possess/2,  grid/4, wearing/2,
  %  p/3,p/4,p/5,p/6,p/7
  % k/3,k/4    
    db_prop_g/1
    ]).


:- dynamic p/3,p/4,p/5,p/6,p/7,db_prop_g/1,is_db_prop/2,is_db_prop/3,pathName/3,type/1,classof/2,isa/2.

:- meta_predicate man:with_assertions(:,0).
:- meta_predicate world:intersect(?,0,?,0,0,-).
:- meta_predicate clause_present(:), db_forall_assert_mv(+,+,+), db_forall_assert_sv(+,+,+), db_forall(+,+), db_forall_quf(+,+,+).

:- meta_predicate hooked_asserta(^), hooked_assert(^), hooked_retract(^), hooked_retractall(^).
%% :- meta_predicate del(0),clr(0),add(0),req(0), db_op(0,0,0),db_prop(0,0),db_prop(0).

:-dynamic(dbase:inRegion/2).
:-export(inRegion/2).

% Found new meta-predicates in iteration 1 (0.281 sec)
:- multifile db_prop/1,db_prop/2.
:- dynamic db_prop/2. db_prop/1.
%:- meta_predicate db_forall(?,?,?,0).

:- include(logicmoo('vworld/vworld_header.pl')).

:- register_module_type(utility).


% =================================================================================================
% world database
% =================================================================================================

% replaced the 1st with the 2nd and better version of retract
% del(C0):-db_op(r,C0)
%% del(RetractOne)
del(C0):- db_op('q',C0),!,db_op('r',C0).
%% clr(Retractall)
clr(C0):-db_op(ra,C0).
%% req(Query)
req(C0):- db_op(q,C0).
%% props(Obj,QueryPropSpecs)
props(Obj,PropSpecs):-req(props(Obj,PropSpecs)).
%% add(Assertion)
add(C0):- db_op(a,C0).
%% padd(Obj,PropSpecs)
padd(Obj,PropSpecs):-add(props(Obj,PropSpecs)).
%% padd(Obj,Prop,Value)
padd(Obj,Prop,Value):- must(atom(Prop)), PropValue=..[Prop,Value],!,padd(Obj,PropValue).
%% prop(Obj,Prop,Value)
prop(Obj,Prop,Value):- must(atom(Prop)), C=..[Prop,Obj,Value],!,req(C).
%% prop_or(Obj,Prop,Value)
prop_or(Obj,Prop,Value,OrElse):- once(prop(Obj,Prop,Value);Value=OrElse).


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

dbase:type(T):-moo:subclass(A,B),(T=B;T=A).

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
db_op0(Op,props(Obj,PropVal)):-!,PropVal=..[Prop|Vals],
	Call=..[Prop,Obj|Vals],
	db_op(Op,Call).
db_op0(Op,C0):-functor(C0,F,A),db_op_4(Op,F,A,C0),!.

db_op_4(Op,:,2,_MODULE:C0):-!,/*throw(module_form(MODULE:C0)),*/
  functor(C0,F,A),
  dmsg(todo(unmodulize(F/A))),
  db_op(Op,C0).
db_op_4(Op,p,_,C0):- C0=..[p,Prop|ARGS],C1=..[Prop|ARGS],db_op(Op,C1),!.
db_op_4(Op,svo,_,C0):- C0=..[svo,Obj,Prop|ARGS],C1=..[Prop,Obj|ARGS],db_op(Op,C1),!.
db_op_4(q,Fmt,1,C0):-is_decl_ft(Fmt),!,C0=..[_,A],format_complies(A,Fmt,_).
db_op_4(a,Fmt,1,_C0):-is_decl_ft(Fmt),!,dmsg(todo(dont_assert_is_decl_ft(Fmt))),!.
db_op_4(Op,Prop,_,C0):- C0=..[Prop|ARGS],db_op_disj(Op,Prop,ARGS).

db_op_unit(_Op,Prop,ARGS,C1):-C1=..[Prop|ARGS].


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
db_forall(a,C):- functor(C,F,A),!, (is_db_prop(F,A,singleValued) -> must(db_forall_assert_sv(C,F,A)) ; must(db_forall_assert_mv(C,F,A))).
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


clause_present(C):-functor(C,F,A),clause_present(C,F,A).
clause_present(C,F,1):-C=..[F,A],format_complies(A,F,_).
clause_present(C,_F,_A):-not(predicate_property(C,_)),!,fail.
clause_present(C,_F,_A):-not(ground(C)),!,fail.
clause_present(C,_F,_A):- debugOnError(C).
% clause_present(C,_F,_A):-predicate_property(C,foreign),!,throw(predicate_property(C,foreign)),!,fail.
% clause_present(C,_F,_A):-clause(C,true),!.
clause_present(C0,_F,A):- A>1, arg(A,C0,NEW),string(NEW),!,copy_term(C0,C),
   setarg(A,C,OLD),C,string_chars(NEW,[S|C1]),string_chars(OLD,[S|C2]),C1=C2,trace,dmsg(present(C)).
%clause_present(C,F,A):- A>1, arg(A,C,NEW),snonvar(NEW),!,setarg(A,C,OLD),clause_present(C,F,A),pl_arg_type(NEW,string),string_chars(NEW,[S|C1]),string_chars(OLD,[S|C2]),C1=C2,dmsg(present(C)).


must_asserta(C):-
      must(ground(C)),
      must(hooked_asserta(C)),!.

argIsa_call(isa,1,argIsaFn(isa,1)):-!.
argIsa_call(isa,2,type).
argIsa_call(classof,2,type).
argIsa_call(memory,2,term).
argIsa_call(Prop,N1,Type):-is_db_prop(Prop,A),functor(P,Prop,A),db_prop(P,_),arg(N1,P,Type).
argIsa_call(Prop,N1,Type):- dmsg(todo(define(argIsa_call(Prop,N1,'_TYPE')))),Type=argIsaFn(Prop,N1).

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

is_single_valuedOrFail(F,A,Obj,ARGS):-is_db_prop(F,A,singleValued),!,valuedOrThrow(F,A,Obj,ARGS),!.
is_single_valuedOrFail(_,_,_,_):-fail.

valuedOrThrow(F,_,Obj,ARGS):-mud_isa(Obj,T),findall_type_default_props(Obj,T,Props),Props=[_|_],Prop=..[F|ARGS], member(Prop,Props),!.
valuedOrThrow(F,A,Obj,ARGS):-valuedOrThrow1(F,A,Obj,ARGS).
valuedOrThrow1(_F,_A,_Obj,ARGS):-last(ARGS,unknown),!.
valuedOrThrow1(F,A,Obj,ARGS):-throw(is_single_valuedOrFail(F,A,Obj,ARGS)).

moo:type_default_props(_,food,[height(0)]).

update_value(OLD,X,NEW):-number(OLD),catch(NEW is OLD + X,_,fail),!.
update_value(_OLD,NEW,NEW).


% single valued
moo:decl_subclass(agent,object).
moo:decl_subclass(item,object).

db_prop(spawn_rate(subclass(object),int)).
db_prop(charge(agent,int)).
db_prop(score(object,int)).
db_prop(damage(agent,int)).
db_prop(defence(agent,int)).
db_prop(id(object,id)).
db_prop(str(agent,int)).
db_prop(spd(agent,int)).
db_prop(stm(agent,int)).
db_prop(attack(agent,int)).
db_prop(height(agent,int)).
db_prop(act_turn(agent,int)).
db_prop(weight(object,int)).
db_prop(permanence(item,verb,int)).
db_prop(weight(object,int)).
db_prop(last_command(agent,command)).
db_prop(mudMaxHitPoints(agent,int)).
db_prop(mudToHitArmorClass0(agent,int)).
db_prop(mudBareHandDamage(agent,dice)).
db_prop(armorLevel(possessable,int)).
db_prop(chargeRemaining(chargable,int)).
db_prop(chargeCapacity(chargable,int)).
db_prop(mudLevelOf(possessable,int)).
db_prop(inRegion(term,region)).

db_prop(type_grid(regiontype,int,list(term))).

db_prop(verbOverride(term,action,action)).

db_prop(facing(agent,dir),singleValued).
% db_prop(inRegion(object,region)).
db_prop(atloc(object,xyz(region,int,int,int)),singleValued).

db_prop(pathBetween(region,dir,region)).
db_prop(pathName(region,dir,string)).
db_prop(location_center(region,xyz(region,int,int,int)),singleValued).


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

db_prop(ArgTypes):-db_prop_g(ArgTypes).
db_prop(ArgTypes,[singleValued]):-db_prop(ArgTypes).

db_prop(CallSig,[external(M)]):-db_prop_prolog(M:CallSig).
:-dynamic(db_prop_prolog/1).
%db_prop_prolog(world:nearby(object,object)).
%db_prop_prolog(world:mud_isa(object,type)).
%db_prop_prolog(world:same(id,id)).


% multivalued
db_prop_multi(G,AT,[ordered|LIST]):-db_prop_multi(G,LIST),functor(G,_,AT).

db_prop_multi(named(term,term),[genlpreds(id)]).
db_prop_multi(classof(term,type),[alias(isa)]).
db_prop_multi(G,[]):-db_prop_multi(G).

db_prop_multi(failure(agent,action)).
db_prop_multi(nameStrings(term,string)).
db_prop_multi(determinerString(term,string)).
db_prop_multi(descriptionHere(term,string)).
db_prop_multi(description(term,string)).
db_prop_multi(keyword(term,string)).
db_prop_multi(act(object,verb,list(props))).
db_prop_multi(memory(agent,term)).
db_prop_multi(wearing(agent,wearable)).
db_prop_multi(grid(region,int,int,object)).
db_prop_multi(possess(agent,item)).
db_prop_multi(subclass(type,type)).
db_prop_multi(isa(term,type)).
% somethingIsa('NpcCol1012-Ensign732',['NpcCol1012',actor,'MaleAnimal']).


scan_db_prop:-
   '@'((forall(db_prop(ArgTypes,PropTypes),define_db_prop(ArgTypes,PropTypes))),dbase).

define_db_prop(ArgTypes,_):-doall((arg(_,ArgTypes,ArgType),functor(ArgType,F,_),define_type(F))),fail.
define_db_prop(ArgTypes,_):- once((functor(ArgTypes,F,A),dbase:dynamic(F/A),dbase:export(F/A))),fail.

define_db_prop(ArgTypes,PropTypes):-functor(ArgTypes,Prop,Arity),asserta(is_db_prop(Prop,Arity)),member(PT,PropTypes),asserta(is_db_prop(Prop,Arity,PT)),is_db_prop_decl2(Prop,Arity,PT),fail.
define_db_prop(ArgTypes,PropTypes):-functor(ArgTypes,P,A),member(external(Module),PropTypes),Module \== dbase,!,length(ARGS,A),HEAD=..[P|ARGS],must(predicate_property(Module:HEAD,_)),!.
define_db_prop(ArgTypes,PT):-functor(ArgTypes,F,A),length(ARGS,A),HEAD=..[F|ARGS],BODY=req(HEAD),is_db_prop_decl2(F,A,PT), ensure_clause(HEAD,BODY),!.

is_db_prop_decl2(Prop,Arity,_/*flag*/):- dynamic(Prop/Arity),dbase:export(Prop/Arity).

% this is just to catch asserts at these predicates that are supposed to be contained.. We dont really want them compiled
%ensure_clause(HEAD,_BODY):-clause(HEAD,_),!.
% ensure_clause(HEAD,_):-functor(HEAD,F,A),pred_as_is(F,A), !.
ensure_clause(HEAD,BODY):-nop(asserta((HEAD:-BODY))),nop(compile_predicates([HEAD])),functor(HEAD,F,A),dynamic(F/A),dbase:export(F/A).


:- scan_db_prop.


:- include(logicmoo('vworld/vworld_footer.pl')).



end_of_file.



































kwith(DbOp,Obj,PropSpecs):-
   PropSpecs =.. [Prop,Obj2|Change],
   length([Obj2|Change],ChangeLen),
   is_db_prop(Prop,ChangeLen),!,
   must(compare_n(Obj,Obj2)),
   kwith_n(DbOp,PropSpecs).

kwith(DbOp,Obj,PropSpecs):- nonvar(Obj),
   PropSpecs=..[Prop|Change],
   length([Obj2|Change],ChangeLen),
   is_db_prop(Prop,ChangeLen),!,
   must(compare_n(Obj,Obj2)),
   kwith_n(DbOp,[Prop,Obj|Change]),!.

kwith(DbOp,Obj,PropSpecs):-
   PropSpecs =.. [Prop,Change],
   Damage_6 =.. [Prop,Obj,Change],!,
   kwith_n(DbOp,Damage_6).


kwith_n(q,[Prop,Obj|Change]):-
   append(Given,[Last],[Prop,Obj|Change]),
   append(Given,[NewLast],NewQuery),
   NEW =.. [p|NewQuery],
   is_db_prop(Prop,_,singleValued),!,
   must(req(NEW)),!,
   once(compare_n(NewLast,Last)),!.

kwith_n(q,[Prop,Obj|Change]):-!,
   append(Given,[Last],[Prop,Obj|Change]),
   append(Given,[NewLast],NewQuery),!,
   NEW =.. [p|NewQuery],!,
   req(NEW),
   once(compare_n(NewLast,Last)).

kwith_n(a,[Prop,Obj,Change]):-
   is_db_prop(Prop,2,singleValued),
   must(kwith_n(u,[Prop,Obj,Change])).

kwith_n(a,[Prop,Obj,A2,Change]):-
   is_db_prop(Prop,3,singleValued),
   must(kwith_n(u,[Prop,Obj,A2,Change])).

kwith_n(a,[Prop,Obj|Change]):-!,
   NEW =.. [p,Prop,Obj|Change],
   add(NEW),!.

kwith_n(u,[Prop,Obj,Change]):-
   ignore(k(Prop,Obj,OLD)),
   (OLD==Change -> true;
   ((
      once(var(OLD);clr(p(Prop,Obj,OLD))),
      add(p(Prop,Obj,NEW))))),!.

kwith_n(u,[Prop,Obj,A2,Change]):-
   is_db_prop(Prop,3,singleValued),
   ignore(k(Prop,Obj,A2,OLD)),
   update_value(OLD,Change,NEW),
   (OLD==Change -> true;
   ((
      once(var(OLD);clr(p(Prop,Obj,A2,OLD))),
      add(p(Prop,Obj,A2,NEW))))),!.

kwith_n(u,[Prop,Obj|Change]):-!,
   NEW =.. [p,Prop,Obj|Change],
   add(NEW),!.


k(Prop,Obj,OLD):- req(Prop,Obj,OLD).
k(Prop,Obj,OLD,S):- p(current,Prop,Obj,OLD,S).


/*
p(current,Prop,Obj,A):-is_single_valuedOrFail(Prop,2,Obj,[A]).
p(current,Prop,Obj,A,B):-is_single_valuedOrFail(Prop,3,Obj,[A,B]).
p(current,Prop,Obj,A,B,C):-is_single_valuedOrFail(Prop,4,Obj,[A,B,C]).
p(current,Prop,Obj,A,B,C,D):-is_single_valuedOrFail(Prop,5,Obj,[A,B,C,D]).
*/

