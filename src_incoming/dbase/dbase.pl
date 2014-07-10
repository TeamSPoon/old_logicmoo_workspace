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
:-dynamic(multiValued/1).
:-dynamic(multiValued/2).

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
:- meta_predicate clause_present(:). %, db_assert_mv(+,+,+,+), db_assert_sv(-,+,+,+), db_forall(+,+), db_forall_quf(+,+,+).

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
holds_t(P,A1,A2,A3,A4,A5,A6,A7):- req(dbase_t(P,A1,A2,A3,A4,A5,A6,A7)).
holds_t(P,A1,A2,A3,A4,A5,A6):- req(dbase_t(P,A1,A2,A3,A4,A5,A6)).
holds_t(P,A1,A2,A3,A4,A5):- req(dbase_t(P,A1,A2,A3,A4,A5)).
holds_t(P,A1,A2,A3,A4):- req(dbase_t(P,A1,A2,A3,A4)).
holds_t(P,A1,A2,A3):- req(dbase_t(P,A1,A2,A3)).
holds_t(P,A1,A2):- req(dbase_t(P,A1,A2)).
holds_t(P,A1):- req(dbase_t(P,A1)).
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




% TODO: canonicalize clauses first!
with_kb_assertions([],Call):- !,Call.
with_kb_assertions([With|MORE],Call):-!,with_kb_assertions(With,with_kb_assertions(MORE,Call)).
with_kb_assertions(With,Call):-
   setup_call_cleanup(asserta(With,Ref),Call,erase(Ref)).



world_clear(Named):-fmt('Clearing world database: ~q.~n',[Named]).

pred_as_is(F,_):-mpred_prop(F,flag),!.
pred_as_is(F,_):-mpred_prop(F,external(_)),!.
pred_as_is(p,_):-!,fail.
pred_as_is(dbase_t,_):-!,fail.
pred_as_is(k,_):-!,fail.

type(T):-moo:subclass(A,B),(T=B;T=A).
type(item).


moo:subclass(drinkable,item).
moo:subclass(possessable,item).
moo:subclass(useable,item).
moo:subclass(eatable,item).
moo:subclass(chargeable,item).
moo:subclass(wearable,item).




:- meta_predicate hooked_asserta(^), hooked_assertz(^), hooked_retract(^), hooked_retractall(^).

:- meta_predicate del(-),clr(-),add(-),req(-), db_op(-,-).

% Found new meta-predicates in iteration 1 (0.281 sec)
%:- meta_predicate db_op_exact(?,?,?,0).

% movedist(X,Y):-callStub_moo(holds_t,movedist(X,Y)).

% :-decl_dynamic_prolog(move:movedist/2).

% :- moo:register_module_type(utility).

% oncely later will throw an error if there where choice points left over by call
oncely(:-(Call)):-!,Call,!.
oncely(:-(Call)):-!,call_expanded(Call).
oncely(Call):-once(Call).

:- dynamic(non_assertable/1).
non_assertable(WW,isVar(WW)):- var(WW),!.
non_assertable(_:WW,Why):- !,non_assertable(WW,Why).
non_assertable(WW,Why):- compound(WW),functor(WW,F,_),!,mpred_prop(F,as_is(Why)),!.
% non_assertable(WW,Why):- db_prop_game_assert

% replaced the 1st with the 2nd and better version of retract
% del(C0):- db_op_int(retract,C0)
%% del(RetractOne)    <--  del(C0):- ignore((db_op(query(HLDS,Must),C0),!,db_op('retract',C0))).
del(C0):- db_op_int(query(holds_t,once),C0),db_op('ra',C0).
del(C0):- dmsg(failed(del(C0))),!,fail.
%% clr(Retractall)
clr(C0):- db_op_int(ra,C0).
%% req(Query)
req(C0):- db_op_int(query(dbase_t,call),C0).
mreq(C0):- db_op_int(query(dbase_t,must),C0).
%% props(Obj,QueryPropSpecs)
props(Obj,PropSpecs):- db_op_int(query(dbase_t,query),props(Obj,PropSpecs)).
%% add(Assertion)
add(C0):- must_det(db_op_int(tell(add), C0)).
%% padd(Obj,PropSpecs)
padd(Obj,PropSpecs):- add(props(Obj,PropSpecs)).
%% padd(Obj,Prop,Value)
padd(Obj,Prop,Value):- must(nonvar(Prop)), PropValue=..[Prop,Value],!,padd(Obj,PropValue).
%% prop(Obj,Prop,Value)
prop(Obj,Prop,Value):- req(svo(Obj,Prop,Value)).
%% prop_or(Obj,Prop,Value,OrElse)
prop_or(Obj,Prop,Value,OrElse):- one_must(prop(Obj,Prop,Value),Value=OrElse).


kb_update(New,OldV):- req(New),!,OldV=New.
kb_update(New,OldV):- db_op_int(tell(OldV),New).

:-thread_local((record_on_thread/2)).

hook:decl_database_hook(assert(_),ft_info(FT,_)):- define_ft(FT).
% hook:decl_database_hook(assert(_),subft(FT,OFT)):- formattype(OFT),define_ft(FT).


% expand_goal_correct_argIsa(A,A):-simple_code,!.
expand_goal_correct_argIsa(A,B):- expand_goal(A,B).

% db_op_simpler(query(HLDS,_),MODULE:C0,call_expanded(call,MODULE:C0)):- atom(MODULE), nonvar(C0),not(not(predicate_property(C0,_PP))),!. % , functor(C0,F,A), dmsg(todo(unmodulize(F/A))), %trace_or_throw(module_form(MODULE:C0)), %   db_op(Op,C0).
db_op_simpler(_,TypeTerm,props(Inst,[isa(Type)|PROPS])):- TypeTerm=..[Type,Inst|PROPS],nonvar(Inst),is_type(Type),!.

foreach_arg(ARGS,_N,_ArgIn,_ArgN,_ArgOut,_Call,ARGS):-not(compound(ARGS)),!.
foreach_arg([ArgIn1|ARGS],ArgN1,ArgIn,ArgN,ArgOut,Call1,[ArgOut1|ARGSO]):-
     copy_term( a(ArgIn1,ArgOut1,ArgN1,Call1), a(ArgIn,ArgOut,ArgN,Call) ),
      call(Call),
      ArgN2 is ArgN + 1,
      foreach_arg(ARGS,ArgN2,ArgIn,ArgN,ArgOut,Call,ARGSO).


transform_functor_holds(_,F,ArgInOut,N,ArgInOut):- once(argIsa_ft(F,N,FT)),FT=term,!.
transform_functor_holds(Op,_,ArgIn,_,ArgOut):- transform_holds(Op,ArgIn,ArgOut),!.


transform_holds(_,A,A):-not(compound(A)),!.
transform_holds(Op,M:Term,OUT):-atom(M),!,transform_holds(Op,Term,OUT).
transform_holds(HLDS,[P,A|ARGS],DBASE):- var(P),!,DBASE=..[HLDS,P,A|ARGS].
transform_holds(_,[P|ARGS],[P|ARGS]):- not(atom(P)),!,dmsg(transform_holds),dtrace.

transform_holds(Op,[HOLDS,P,A|ARGS],OUT):- is_holds_true(HOLDS),!,transform_holds(Op,[P,A|ARGS],OUT).
transform_holds(HLDS,[HOLDS,P,A|ARGS],OUT):- HLDS==HOLDS, !, transform_holds(HLDS,[P,A|ARGS],OUT).
transform_holds(Op,[svo,Obj,Prop|ARGS],OUT):- !,transform_holds(Op,[Prop,Obj|ARGS],OUT).
transform_holds(_,[Type,Inst],isa(Inst,Type)).

transform_holds(_,HOLDS,isa(I,C)):- holds_args(HOLDS,[C,I]),!.
transform_holds(_,HOLDS,isa(I,C)):- holds_args(HOLDS,[ISA,I,C]),ISA==isa,!.

transform_holds(Op,[Logical|ARGS],OUT):- 
         notrace(logical_functor(Logical)),!,
         must_det(foreach_arg(ARGS,1,ArgIn,ArgN,ArgOut,transform_functor_holds(Op,Logical,ArgIn,ArgN,ArgOut),LARGS)),
         OUT=..[Logical|LARGS].

transform_holds(_,[props,Obj,Props],props(Obj,Props)).
transform_holds(_,props(Obj,Props),props(Obj,Props)).
transform_holds(_,[Type,Inst|PROPS],props(Inst,[isa(Type)|PROPS])):- nonvar(Inst), not(Type=props), is_type(Type),!.
transform_holds(_,[P,A|ARGS],DBASE):- atom(P),!,DBASE=..[P,A|ARGS].
transform_holds(_,[P,A|ARGS],DBASE):- !, nonvar(P),DBASE=..[P,A|ARGS].
transform_holds(Op,DBASE_T,OUT):- DBASE_T=..[P,A|ARGS],!,transform_holds(Op,[P,A|ARGS],OUT).
transform_holds(_,A,A):-dtrace.


db_op_simpler_wlc(query(HLDS,Must),Wild,Simpler):- !,hotrace(db_op_simpler(query(HLDS,Must),Wild,Simpler)),not(is_loop_checked(req(Simpler))),!.
db_op_simpler_wlc(tell(Must),Wild,Simpler):- !,hotrace(db_op_simpler(tell(Must),Wild,Simpler)),not(is_loop_checked(add(Simpler))),!.
db_op_simpler_wlc(Op,Wild,Simpler):- !,hotrace(db_op_simpler(Op,Wild,Simpler)),not(is_loop_checked(db_op0(Op,Simpler))),!.


db_op_sentence(_Op,Prop,ARGS,C0):- must(atom(Prop)), C0=..[Prop|ARGS],!.
db_op_sentence(_Op,Prop,ARGS,C0):- grtrace, C0=..[holds_t,Prop|ARGS].


% ================================================
% db_tell_isa/2
% ================================================
db_tell_isa(I,T):-not(ground(I:T)),trace_or_throw(not(ground(db_tell_isa(I,T)))).

% skip formatter types
db_tell_isa(_I,T):- member(T,[string,action,dir]),!.
db_tell_isa(I,Fmt):- Fmt\=type, hotrace(( nonvar(Fmt), dbase_t(isa(Fmt,formattype)))),!,dmsg(todo(dont_assert_is_ft(I,Fmt))),!.
db_tell_isa(I,T):- is_asserted(isa(I,T)),!.
db_tell_isa(I,T):- must_det((hooked_asserta(isa(I,T)),is_asserted(isa(I,T)))).

% ================================================
% db_tell_isa HOOKS
% ================================================
hook:decl_database_hook(assert(_),isa(I,T)):- doall(db_tell_isa_hooked(I,T)).
hook:decl_database_hook(assert(_),dbase_t(T,I)):- doall(db_tell_isa_hooked(I,T)).
% hook:decl_database_hook(retract(_),isa(I,T)):-doall(db_retract_isa_hooked(I,T)).

db_tell_isa_hooked(I,T):- not(ground(db_tell_isa(I,T))),!, trace_or_throw(not(ground(db_tell_isa(I,T)))).

db_tell_isa_hooked(I,T):- asserta_if_new(moo:dbase_t(isa,I,T)).
db_tell_isa_hooked(type,type):- !.
db_tell_isa_hooked(T,type):-  !,define_type(T).
db_tell_isa_hooked(Term,mpred):- !,decl_mpred(Term).
db_tell_isa_hooked(_,T):- define_type(T).
db_tell_isa_hooked(I,T):- extentKnown(T),asserta_if_new(moo:dbase_t(T,I)).

db_tell_isa_hooked(I,_):- glean_pred_props_maybe(I),fail.
db_tell_isa_hooked(I,T):- argsIsaProps(T),decl_mpred(I,T).

% db_tell_isa_hooked(I,T):- motel:defconcept(I,and([lexicon,T])).
% db_tell_isa_hooked(I,T):- motel:defprimconcept(I,T).

db_tell_isa_hooked(food5,'Weapon'):-trace_or_throw(db_tell_isa(food5,'Weapon')).

% db_tell_isa_hooked(I,T):-dmsg((told(db_tell_isa(I,T)))).


clauses_of(F,H,B):-current_predicate(F/A),functor(H,F,A),clause(H,B).

dbase_clauses(H,B):-clauses_of(dbase_t,H,B).

hook:decl_database_hook(assert(_),isa(I,T)):- doall(db_tell_isa_hooked_after(I,T)).

db_tell_isa_hooked_after(I,T):- is_creatable_type(T),!, db_tell_isa_hooked_creation(I,T).
db_tell_isa_hooked_after(I,T):- extentKnown(ST),impliedSubClass(T,ST),db_op_int(tell(asap),isa(I,ST)).
db_tell_isa_hooked_after(I,T):- db_tell_isa_hooked_creation(I,T).

extentKnown(Ext):- arg(_,dbase_t(type,dir,formattype,string),Ext).
extentKnown(ST):-is_creatable_type(ST).
extentKnown(F):-argsIsaProps(F).
extentKnown(F):-get_isa_asserted(F,extentKnown).

impliedSubClass(T,ST):-stack_check(300),req(subclass(T,ST)).

% one of 4 special types
db_tell_isa_hooked_creation(I,T):- is_creatable_type(T),!,call_after_game_load((world:create_instance(I,T,[]))).
% sublass of 4 special types
db_tell_isa_hooked_creation(I,T):- doall((is_creatable_type(ST),impliedSubClass(T,ST),call_after_game_load((world:create_instance(I,ST,[isa(T)]))))).



define_ft(FT):- db_tell_isa(FT,formattype).

get_isa_backchaing(A,T):-var(T),!,get_isa_asserted_0(A,T).
get_isa_backchaing(A,T):- extentKnown(T),!,get_isa_asserted_0(A,T).
get_isa_backchaing(_,A):- not(is_type(A)),!,fail.
%get_isa_backchaing(A,formattype):- !,formattype(A).
%get_isa_backchaing(A,type):- !, is_type(A).
get_isa_backchaing(A,Fmt):- nonvar(Fmt),formattype(Fmt),!,term_is_ft(A,Fmt).
get_isa_backchaing(A,Fmt):- get_isa_asserted(A,Fmt).


get_isa_asserted(A,Fmt):-hotrace(get_isa_asserted_0(A,Fmt)).

get_isa_asserted_0(A,Fmt):- is_asserted(dbase_t(isa,A,Fmt)).
get_isa_asserted_0(A,ArgsIsa):- nonvar(ArgsIsa),argsIsaProps(ArgsIsa),!,mpred_prop(A,ArgsIsa).


% subclass(Sub,Sup):-add(subclass(Sub,Sup)).

equivRule_call(A,B):- holds_t(equivRule,A,B).
equivRule_call(A,B):- holds_t(equivRule,B,A).
forwardRule_call(A,B):- holds_t(forwardRule,B,A).

:-dynamic_multifile_exported(equivRule/2).

good_for_chaining(_,_):-!.
good_for_chaining(_Op,Term):-not(contains_singletons(Term)).
db_rewrite(_Op,Term,NewTerm):-equivRule_call(Term,NewTerm).
db_rewrite(_Op,Term,NewTerm):-forwardRule_call(Term,NewTerm).


%% hook:dmsg_hook(db_op(query(HLDS,call),moo:holds_t(ft_info,type,'$VAR'(_)))):-dtrace.

% ================================================
% db_op_int/2
% ================================================
add_from_file(B,_):- contains_singletons(B),grtrace,dmsg(todo(add_from_file_contains_singletons(B))),!,fail.
add_from_file(B,B):- db_op(tell(_OldV),B),!.

% do_db_op_hooks:-!.
do_db_op_hooks:- ignore((hotrace(((do_all_of(dbase_module_loaded)),call_after(moo:not_loading_game_file, true))))).

db_op_int(Op,A):- hotrace(must(once(correctArgsIsa(Op,A,AA)))),!,db_op_int2(Op,AA).

db_op_int2(query(HLDS,Must),Term):- !,do_db_op_hooks, db_op(query(HLDS,Must),Term).
db_op_int2(Op,Term):- do_db_op_hooks,db_op(Op,Term),do_db_op_hooks.

holds_args([H|LIST],LISTO):- !, is_holds_true(H),!,LIST=LISTO.
holds_args(HOLDS,LIST):- compound(HOLDS),HOLDS=..[H|LIST],is_holds_true(H),!.

% ================================================
% db_op/2
% ================================================

% db_op(query(HLDS,Must),createableType(SubType)):- !, call_must(Must,is_creatable_type(SubType)).
db_op(Op,isa(Term,Var)):- var(Var),!,db_op(Op,get_isa(Term,Var)).

db_op(a,Wild):-!,db_op(tell(assertion),Wild).
db_op(query(_HLDS,Must),isa(T,type)):- !,call_must(Must,is_type(T)).
db_op(query(_HLDS,Must),isa(I,Type)):- !,call_must(Must,get_isa_backchaing(I,Type)).
db_op(tell(_),isa(T,type)):- !,db_tell_isa(T,type),!.
db_op(tell(_),isa(T,Type)):- !,db_tell_isa(T,Type),!.
db_op(query(HLDS,call),Wild):- once(transform_holds(HLDS,Wild,Simpler)),Wild\=Simpler,!,db_op(query(HLDS,call),Simpler).
db_op(Op,Wild):- hilog_functor(HILOG),once(transform_holds(HILOG,Wild,Simpler)),Wild\=Simpler,!,db_op(Op,Simpler).

% db_op(query(HLDS,call),subclass(Var,formattype)):-var(Var),!,fail.
db_op(tell(OldV),B):- !,loop_check_term(db_op0(tell(OldV),B),add(B),true),!. % true = we are already processing this assert
% call_expanded_for(Must,B)
db_op(query(HLDS,Must),B):- !,loop_check_term(db_op0(query(HLDS,Must),B),req(B),fail). % fail = we are already processing this query
db_op(Op,Term):- loop_check_throw(db_op0(Op,Term)).

% ================================================
% db_op0/2
% ================================================
:-moo_hide_childs(db_op0/2).

db_op0(tell(assertion),end_of_file):-!.
db_op0(Op,Term):- not(compound(Term)),!,trace_or_throw(nc(db_op0(Op,Term))).

db_op0(Op,KB:Term):- is_kb_module(KB),!,db_op(Op,Term).
db_op0(Op,KB:Term):- dbase_mod(KB),!,db_op(Op,Term).


% db_op0(Op,(':-'(A))):- must((expand_goal_correct_argIsa(A,AA))),expanded_different(A,AA),!,db_op(Op, (':-'(AA))).

db_op0(Op,[dbase_t,Class,Inst]):-!,db_op0(Op,isa(Inst,Class)).
db_op0(Op,[cholds_f,Class,Inst]):-!,db_op0(Op,isnt(Inst,Class)).

db_op0(Op,[dbase_t,P|List]):-nonvar(P),univ_left(G2,[P|List]),!,db_op0(Op,G2).
db_op0(Op,[cholds_f,P|List]):-nonvar(P),univ_left(G2,[P|List]),!,db_op0(Op,not(G2)).

db_op0(Op,G1):- functor_check_univ(G1,F,[P|ListL]),List=[P|ListL],
      (is_holds_true(F) -> (nonvar(P) -> (univ_left(G2,List),db_op0(Op,G2)); db_op0(Op,[dbase_t|List])) ;
      (is_holds_false(F) -> (nonvar(P) -> (univ_left(G2,List),db_op0(Op,not(G2))); db_op0(Op,[cholds_f|List]));
      fail)).

db_op0(Op,Term):- record_on_thread(dbase_opcall,db_op(Op,Term)),fail.

db_op0(tell(_OldV),(':-'(A))):- !, must((expand_goal_correct_argIsa(A,AA),call_expanded(AA))).
db_op0(retract,(C1;C2)):- !,dtrace,once((db_op(retract,C1),db_op(retract,C2))).
db_op0(tell(OldV),(C1;C2)):- !,db_op(tell(OldV),C1),!,db_op(tell(OldV),C2),!.
db_op0(ra,(C1;C2)):- !,must(db_op(ra,C1)),must(db_op(ra,C2)).
db_op0(Op,(C1,C2)):- !,db_op(Op,C1),db_op(Op,C2).


db_op0(query(HLDS,Must),props(Obj,Props)):- var(Props),!,findall(Prop,(call_must(query(HLDS,Must),dbase_t([P,Obj|REST])),Prop=..[P|REST]),Props).
db_op0(Op ,props(Obj,Open)):- var(Open),!,trace_or_throw(db_op(Op,props(Obj,Open))).
db_op0(_Op,props(_Obj,[])):- !.
db_op0(Op,props(Obj,[P])):- nonvar(P),!,db_op(Op,props(Obj,P)).
db_op0(Op,props(Obj,[P|ROPS])):- !,db_op(Op,props(Obj,P)),db_op(Op,props(Obj,ROPS)).
db_op0(Op,props(Obj,PropVal)):- safe_univ(PropVal,[Prop,NonVar|Val]),Obj==NonVar,!,db_op(Op,[dbase_t,Prop,Obj|Val]).
db_op0(Op,props(Obj,PropVal)):- PropVal=..[OP,Pred|Val],comparitiveOp(OP),not(comparitiveOp(Pred)),!,OPVAL=..[OP|Val],PropVal2=..[Pred,OPVAL],db_op(Op,props(Obj,PropVal2)).
db_op0(Op,props(Obj,PropVal)):- PropVal=..[Prop|Val],not(infix_op(Prop,_)),!,db_op(Op,[dbase_t,Prop,Obj|Val]).
db_op0(Op,props(Obj,PropVal)):- PropVal=..[Prop|Val],!,grtrace,db_op(Op,[dbase_t,Prop,Obj|Val]).

db_op0(Op,expand_args(Exp,Term)):- !,forall(do_expand_args(Exp,Term,O),db_op(Op,O)).
db_op0(Op,somethingIsa(A,List)):- !,forall_member(E,List,db_op(Op, isa(A,E))).
db_op0(Op,somethingDescription(A,List)):- !,forall_member(E,List,db_op(Op, description(A,E))).
db_op0(Op,objects(Type,List)):- !,forall_member(I,List,db_op(Op,isa(I,Type))).
db_op0(Op,sorts(Type,List)):- !,forall_member(I,List,db_op(Op, subclass(I,Type))).
db_op0(Op,predicates(List)):- !,forall_member(T,List,db_op(Op,mpred(T))).
db_op0(Op,EACH):- EACH=..[each|List],forall_member(T,List,db_op(Op,T)).

db_op0(Op,db_op_exact(Term)):- !,db_op_exact(Op,Term).
 
db_op0(tell(_),description(A,E)):- once(must(assert_description(A,E))),!,db_op0(tell(_),descriptionHere(A,E)).
db_op0(Op,nameStrings(A,S0)):- nonvar(S0),determinerRemoved(S0,String,S),!,db_op(Op, nameStrings(A,S)),db_op(tell(_OldV), determinerString(A,String)).

% db_op0(Op,Term):- hotrace(good_for_chaining(Op,Term)), db_rewrite(Op,Term,NewTerm),not(contains_singletons(NewTerm)),db_op(Op,NewTerm).

db_op0(tell(_OldV),mpred(A)):- !,decl_mpred(A),!.
db_op0(tell(_OldV),isa(A,mpred)):- !,decl_mpred(A),!.

db_op0(query(HLDS,Must),get_isa(Term,Var)):- !,call_must(query(HLDS,Must),get_isa_asserted(Term,Var)).
db_op0(query(HLDS,Must),isa(Term,Var)):- !,call_must(query(HLDS,Must),hotrace(get_isa_backchaing(Term,Var))).
db_op0(Op,isa(A,SubType)):- dbase_t(createableSubclassType,SubType,Type),!,db_op(Op,isa(A,Type)),db_op(Op,isa(A,SubType)).

db_op0(tell(_OldV),argsIsa(F,Term)):-!,hooked_asserta(dbase_t(argsIsa,F,Term)),!,decl_mpred(Term),!.
db_op0(tell(_OldV),singleValued(Term)):- !,decl_mpred(Term),decl_mpred(Term,singleValued).
db_op0(tell(_OldV),multiValued(Term)):- !,functor_safe(Term,_,A),decl_mpred(Term),decl_mpred(Term,[multiValued,multi(A)]).
db_op0(query(HLDS,Must),argIsa(P,N,T)):- call_must(query(HLDS,Must),(get_mpred_prop(P,argsIsa(ArgsIsa)),arg(N,ArgsIsa,T),must(nonvar(T)))).

db_op0(tell(_),Term):- glean_pred_props_maybe(Term),fail.

db_op0(Op,Wild):- db_op_simpler_wlc(Op,Wild,Simpler),!,db_op(Op,Simpler).

db_op0(Op,Term):- Term =..[Type,A],!,db_op(Op,isa(A,Type)).

db_op0(Op,C0):- C0=..[Prop|ARGS],db_op_unit(Op,C0,Prop,ARGS).

% ================================================
% db_op_unit/3
% ================================================

db_op_unit(Op,_C0,Prop,ARGS):-is_type(Prop),trace_or_throw(db_op_unit(Op,is_type(Prop),ARGS)).

db_op_unit(query(HLDS,Must),_C0,Prop,ARGS):- get_mpred_prop(Prop,extentKnown),!,call_must(query(HLDS,Must),dbase_t_p2(Prop,ARGS)).

db_op_unit(Op,C0,isa,ARGS):-trace_or_throw(db_op_unit(Op,isa(C0),ARGS)).

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
      
% assert_with_pred/1
db_op_unit(tell(_),C0,Prop,_RGS):- get_mpred_prop(Prop,assert_with_pred(How)),!,grtrace,must(atom(How)), call(run_database_hooks(assert(z)),C0), call(How,C0).

% plain prop
db_op_unit(Op,_C0,Prop,ARGS):- must((db_op_sentence(Op,Prop,ARGS,Unit),same_vars(ARGS,Unit))),!, db_op_exact(Op,Unit).

db_op_unit(Op,_C0,Prop,ARGS):- grtrace,must((db_op_sentence(Op,Prop,ARGS,Unit),same_vars(ARGS,Unit))),!, db_op_loop(Op,Unit,db_op_exact(Op,Unit)).
db_op_unit(Op,C0,_Prop,_ARGS):- db_op_loop(Op,C0,db_op_exact(Op,C0)).


db_op_loop(Op,Unit,Result):- is_loop_checked(db_op0(Op,Unit)),!,call(Result).
db_op_loop(Op,Unit,_Result):- db_op(Op,Unit).

% ================================================
% db_op_exact/2
% ================================================
db_op_exact(Op,C):- C=..[SubType,Arg],db_op_loop(Op,isa(Arg,SubType),fail),!.
db_op_exact(query(HLDS,Must), Term):- !,call_expanded_for(query(HLDS,Must),Term).
db_op_exact(query, Term):- !,call_expanded_for(findall,Term).
db_op_exact(must, Term):- !,call_expanded_for(must,Term).
db_op_exact(u,C):- grtrace,db_quf(u,C,U,Template),call_expanded(U),Template,must(ground(Template)),!,ignore(hooked_retractall(Template)).
db_op_exact(ra,C):- db_quf(ra,C,U,Template),!, doall((call_expanded(U),hooked_retractall(Template))).
db_op_exact(retract,C):- must(db_quf(retract,C,U,Template)),!,call_expanded(U),!,hooked_retract(Template).
db_op_exact(tell(OldV),W):- non_assertable(W,Why),trace_or_throw(todo(db_op(tell(OldV), non_assertable(Why,W)))).
db_op_exact(tell(Must),C0):- db_quf(tell(Must),C0,U,C),!,must(call_expanded(U)),functor(C,F,A),( get_mpred_prop(F,singleValued) -> must(db_assert_sv(Must,C,F,A)) ; must(db_assert_mv(Must,C,F,A))).
db_op_exact(tell(Must),C):- grtrace, functor(C,F,A), must((get_mpred_prop(F,singleValued) -> must(db_assert_sv(tell(Must),C,F,A)) ; must(db_assert_mv(tell(Must),C,F,A)))).
db_op_exact(Must, Term):- !,call_expanded_for(Must,Term).
db_op_exact(Op,C):- trace_or_throw(unhandled(db_op_exact(Op,C))).


:-export(call_expanded_for/2).
call_expanded_for(Must,Call):- call_must(Must,Call).



call_expanded_for_sv(WhatNot,F,A,G,OUT):- nonvar(OUT),replace_arg(G,A,NEW,CC),!,call_expanded_for_sv_2(WhatNot,F,A,CC,NEW),!,NEW=OUT.
call_expanded_for_sv(WhatNot,F,A,G,OUT):- call_expanded_for_sv_2(WhatNot,F,A,G,OUT),!.

call_expanded_for_sv_2(WhatNot,F,A,G,_OUT):-call_expanded([whatnot(WhatNot)],G,F,A),!.
call_expanded_for_sv_2(WhatNot,F,A,G,OUT):- defaultArgValue(WhatNot:G,F,A,OUT),!.

:-export(call_expanded/1).
%:-export(call_expanded/1).
call_expanded(G):-debugOnError(G).
%call_expanded(G) :- functor_catch(G,F,A),call_expanded([],G,F,A).

:-export(call_expanded/4).
call_expanded(_Doing,V,_F,_A):-var(V),!,trace_or_throw(var_call_expanded(V)).
call_expanded(_Doing,true,_F,_A):-!.
call_expanded(Doing,G,F,A):-not(member(ask_module(_),Doing)),mpred_prop(F,ask_module(M)),!,'@'(M:call(call_expanded([ask_module(M)|Doing],M:G,F,A)),M).
call_expanded(Doing,G,F,A):-not(member(query(_),Doing)),mpred_prop(F,query(M)),!,call_expanded([query(M)|Doing],call(M,G),F,A).
call_expanded(Doing,G,F,A):-not(member(singleValued,Doing)),mpred_prop(F,singleValued),!,call_expanded([singleValued|Doing],G,F,A),!.
call_expanded(Doing,G,F,A):-not(member(dbase_t,Doing)),mpred_prop(F,dbase_t),!,call_expanded([dbase_t|Doing],dbase_t(G),F,A).

% call_expanded(Doing,G,_,_):-predicate_property(M:G,_),!,call(M:G).
call_expanded(_Doing,G,_,_):-call_asserted(G).
% call_expanded(Doing,G,F,A):-decl_mpred(F,dbase_t),!,call_expanded(G).
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
dbase_t(not(CALL)):-!,dbase_f(CALL).
dbase_t(CALL):- compound(CALL),!,CALL =..[P|LIST],dbase_t([P|LIST]).
dbase_t_p2(P,[L|IST]):-atom(P),is_holds_true(P),!,dbase_t_p2(L,IST).
dbase_t_p2(P,LIST):- CALL=..[dbase_t,P,LIST],call(CALL).

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


into_assertion_form_trans(G,was_asserted_gaf(G)):- functor_catch(G,F,_),mpred_prop(F,isStatic).

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

call_must(Must,M:Call):- atom(M),!,call_must(Must,Call).
call_must(M:Must,Call):- atom(M),!,call_must(Must,Call).
call_must(query(Must,call),Call):- !,call_must(Must,Call).
call_must(query(call,Must),Call):- !,call_must(Must,Call).
call_must(query(Must,HOLDS),Call):- !,call_must(HOLDS,call_must(Must,Call)).
call_must(tell(Must),Call):- !,call_must(Must,Call).
call_must(ask(Must),Call):- !,call_must(Must,Call).
call_must(!,Call):- !,call_must(once,Call).
call_must(Must,Call):- functor(Call,F,_),term_variables(Call,Vs),call_must(Must,F,Vs,Call).

call_must(singleValued,F,Vs,Call):- !,once(call_must_all(singleValued,F,Vs,Call)).
call_must(once,F,Vs,Call):- !,once(call_must_all(once,F,Vs,Call)).
call_must(must,F,Vs,Call):- !,must(call_must_all(must,F,Vs,Call)).
call_must(Op,F,[],Call):- !,once(call_must_all(Op,F,[],Call)).
call_must(Op,F,Vs,Call):- call_must_all(Op,F,Vs,Call).


call_must_all(_,subclass,_Vs,C):-!,dbase_and_is_asserted(subclass,C).
call_must_all(_,get_isa_backchaing,Vs,C):-!,callm(get_isa_backchaing,C).

% call_must_all(Op, F,Vs,Wild):-dmsg(call_must(Op,Wild)),fail.
call_must_all(assertedOnly,F,_Vs,C):-!,dbase_and_is_asserted(F,C).
call_must_all(req, _ ,[],C):- !, req(C),!.
call_must_all(_ ,F,[],Call):-!,callm(F,Call),!.
% call_must_all(_ ,F,_Vs,C):-dbase_and_is_asserted(F,C).
call_must_all(_ ,F,Vs,Call):- setof(Vs,callm(F,Call),VVs),member(Vs,VVs).

callm(F,C):-not(predicate_property(C,_)),!,dbase_t_only(F,C).
callm(_,C):-C.

dbase_and_is_asserted(_,C):-clause(C,true).
dbase_and_is_asserted(dbase_t,C):-C=..[_,L|IST],!,nonvar(L),CC=..[L|IST],clause(CC,true).
dbase_and_is_asserted(_,C):-C=..[L|IST],CC=..[dbase_t,L|IST],clause(CC,true).

dbase_t_only(dbase_t,C):-!,clause(C,true).
dbase_t_only(_,C):- dbase_t(C).


% ================================================
% db_assert_[mv|sv]/3
% ================================================


% assert_with to tell(OldV) mutlivalue pred
:-export((db_assert_mv/4)).
db_assert_mv(_Must,end_of_file,_,_):-!.
db_assert_mv(_Must,C,F,_A):- (mpred_prop(F,ordered) -> hooked_assertz(C) ; hooked_asserta(C)).


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
   with_assertions(noDefaultValues(_),
      must_det(call_expanded([whatnot(Must:C)],COLD,F,A))),
   update_value(OLD,UPDATE,NEW),!,
   hooked_retract(COLD),
   replace_arg(C,A,NEW,CNEW),
   hooked_asserta_confirmed(CNEW,A,NEW).

:-export(db_assert_sv_replace/5).
db_assert_sv_replace(_Must,C,_,A,NEW):-
   replace_arg(C,A,_,CBLANK),
   hooked_retractall(CBLANK),
   replace_arg(C,A,NEW,CNEW),
   hooked_asserta_confirmed(CNEW,A,NEW).


hooked_asserta_confirmed(CNEW,A,NEW):-
   hooked_asserta(CNEW),
   replace_arg(CNEW,A,NOW,CNOW),
   req(CNOW),
   must_det(NEW=NOW).

:-thread_local noDefaultValues/1.

:-export(defaultArgValue/4).
defaultArgValue(_,F,_,_):- noDefaultValues(F),!,fail.
defaultArgValue(facing(_,_),_,_,"n"):-!.
defaultArgValue(damage(_,_),_,_,500):-!.
defaultArgValue(Info,F,_A,OLD):- mpred_prop(F,default(OLD)),!,dmsg(real_defaultValue(Info,F,default(OLD))).
defaultArgValue(Info,F,A,OLD):- argIsa_call(F,A,Type),defaultTypeValue(Info,Type,OLD),!.

defaultTypeValue(_,Type,_):- noDefaultValues(Type),!,fail.
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

clause_present(C):-notrace((functor_catch(C,F,A),clause_present(C,F,A))).
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
argIsa_call(F,N,Type):- mpred_prop_game_assert(Types),functor_catch(Types,F,_),arg(N,Types,Type),nonvar(Type),!.
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
       functor_catch(ARG,F,2), comparitiveOp(F),!,
       ARG=..[F,Prop,VAL],
       GETTER=..[Prop,O,OLD],
       COMPARE= compare_op(Type,F,OLD,VAL),!.

% props(Obj,oneOf(Sz,[size+1,2])).
translateOneArg(Prop,O,Type,oneOf(VAL,LIST),VAL,G,(GO,G)):-
   translateListOps(Prop,O,Type,VAL,LIST,G,GO).

% padd(Obj,size + 2).
translateOneArg(Prop,O,_Type,ARG,NEW,G,(GETTER,STORE,G)):-
       functor_catch(ARG,F,2), additiveOp(F),!,
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
update_value(OLD,NEW,NEWV):- var(OLD),!,compute_value_no_dice(NEW,NEWV).
update_value(OLD,X,NEW):- is_list(OLD),!,list_update_op(OLD,X,NEW),!.
update_value(OLDI,+X,NEW):- compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD + X,_,fail),!.
update_value(OLDI,-X,NEW):- compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD - X,_,fail),!.
update_value(OLDI,X,NEW):- number(X),X<0,compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD + X,_,fail),!.
update_value(_,NEW,NEWV):-compute_value_no_dice(NEW,NEWV),!.

list_update_op(OLDI,+X,NEW):-flatten_append(OLDI,X,NEW),!.
list_update_op(OLDI,-X,NEW):-flatten([OLDI],OLD),flatten([X],XX),!,list_difference_eq(OLD,XX,NEW),!.

compute_value_no_dice(NEW,NEW):- compound(NEW),functor(NEW,dice,_),!.
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
:- include(dbase_i_builtin).

:- user_use_module(dbase_rules_pttp).

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


% database backing impls
% builtin = prolog native
% dynamic = prolog dynamic assert/call
% dbase_t = a dbase_t/N
% pttp = pttp _int compiled
% cyc = 
% type =

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
%multiValued(G,AT,[ordered|LIST]):-multiValued(G,LIST),functor_catch(G,_,AT).

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

:-export(instance_missing_props/3).
instance_missing_props(I,LPS,PS):- findall(P,(member(P,LPS),inst_missing_prop(I,P)),PS),!.

:-export(get_inst_default_props/3).
get_inst_default_props(I,PropListL,Missing):-
    findall(PropList,(get_type_props(Type,PropList),isa(I,Type)),PropListS),flatten_set(PropListS,PropListL),
       instance_missing_props(I,PropListL,Missing).
      
inst_missing_prop(I,P):-P=..[F|Args],inst_missing_prop(I,F,Args).

inst_missing_prop(_I,F,_Args):-not(mpred_prop(F,singleValued)),!,fail.
inst_missing_prop(_I,F,_Args):-mpred_prop(F,flag),!,fail.
inst_missing_prop(I,F,Args):-C=..[F,I|Args],get_sv_argnum(F,[I|Args],A),arg(A,C,_Default),replace_arg(C,A,BLANK,COLD),ignore(req(COLD)),!,nonvar(BLANK).


get_sv_argnum(F,Args,ArgNum):-once(mpred_prop(F,functionalArg(ArgNum));length(Args,ArgNum)).

:-export(forall_setof/2).
forall_setof(ForEach,Call):-
   findall(ForEach,ForEach,ForEachAll),
   list_to_set(ForEachAll,Set),!,
   ignore(forall(member(ForEach,Set),Call)).

:-export(scan_default_props/0).

% scan_default_props:- dmsg(todo(fix(scan_default_props,"to not set atloc/2"))),!.
scan_default_props:- 
 forall_setof(get_type_props(Type,PropList),
    forall_setof(isa(I,Type), 
         ignore((not(Type == I),
         once(instance_missing_props(I,PropList,Missing)),Missing\=[],
         dmsg(scan_default_props(I,Type,missing_from(Missing,PropList))),
         padd(I,Missing))))),!,remove_duplicated_facts.
scan_default_props.

      

% load_motel:- defrole([],time_state,restr(time,period)).
% :-load_motel.

:- include(logicmoo('vworld/moo_footer.pl')).



