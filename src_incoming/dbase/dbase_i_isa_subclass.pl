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


never_type(Var):-var(Var),!,trace_or_throw(var_never_type(Var)).
never_type('Area1000').
never_type(subft).
never_type(must).
never_type(mpred_prop).
never_type(ft_info).
never_type(F):- mpred_arity(F,A),!, A > 1.

define_type(Spec):- never_type(Spec),!,trace_or_throw(never_type(Spec)).
define_type(M:F):-!, '@'(define_type(F), M).
define_type([]):-!.
define_type([A]):-!,define_type(A).
define_type([A|L]):-!,define_type(A),define_type(L).
define_type((A,L)):-!,define_type(A),define_type(L).


define_type(Spec):- compound(Spec),must_det(define_compound_as_type(Spec)).
define_type(Spec):- decl_mpred(Spec,1),declare_dbase_local_dynamic(Spec,1), define_type_0(Spec).

define_type_0(Spec):- dbase_t(type,Spec),!.
define_type_0(Spec):- add_w_hooks(dbase_t(type,Spec),isa(Spec,type)).

define_compound_as_type(Spec):- dbase_t(F,Spec),dmsg(once(define_compound_as_type(Spec,F))).
define_compound_as_type(Spec):- add(resultIsa(Spec,type)).
define_compound_as_type(Spec):- assertz_if_new(dbase_t(formattype,Spec)),dmsg(once(define_compound_as_type(Spec,formattype))).
define_compound_as_type(Spec):- compound(Spec),trace_or_throw(never_compound_define_type(Spec)).

:-forall(argsIsaProps(F),define_type(F)).



:-dynamic_multifile_exported(define_ft/1).
define_ft(Spec):- never_type(Spec),!,trace_or_throw(never_ft(Spec)).
define_ft(M:F):- !, '@'(define_ft(F), M).
define_ft(Spec):- compound(Spec),functor(Spec,F,_),!,define_ft_0(F),define_ft_0(Spec).
define_ft(Spec):- define_ft_0(Spec).

define_ft_0(Spec):- dbase_t(formattype,Spec),!.
define_ft_0(Spec):- dbase_t(type,Spec),dmsg(once(maybe_converting_plain_type_to_formattype(Spec))),fail.
define_ft_0(Spec):- add_w_hooks(dbase_t(formattype,Spec),isa(Spec,formattype)).

%type(Spec):- is_asserted(isa(Spec,type)).

define_type_if_atom(T):- compound(T),!.
define_type_if_atom(T):- ignore((atom(T),not(number(T)),define_type(T))).

hook:decl_database_hook(assert(_A_or_Z),subclass(S,C)):-define_type_if_atom(S),define_type_if_atom(C).

:- define_type(type).
:- define_type(extentKnown).
:- define_type(creatableType).
:- forall(argsIsaProps(Prop),(define_type(Prop),asserta(is_known_true(subclass(Prop,mpred))))).

is_creatable_type(Type):- arg(_,vv(agent,item,region,concept),Type).
is_creatable_type(Type):- atom(Type),call(is_asserted(isa(Type,creatableType))).

% ================================================
% assert_isa/2
% ================================================
:-export assert_isa/2.

assert_isa(I,T):- not(ground(I:T)),trace_or_throw(not(ground(assert_isa(I,T)))).
assert_isa(I,T):- hotrace((loop_check(assert_isa_lc(I,T),true))).

:-export assert_isa_lc/2.
% skip formatter types
assert_isa_lc(_I,T):- member(T,[string,action,dir]),!.
assert_isa_lc(I,T):- hotrace(formattype(T)),!,dmsg(todo(dont_assert_is_ft(I,T))),!.
assert_isa_lc(_,T):- once(define_type(T)),fail.
assert_isa_lc(I,type):- define_type(I),!.
assert_isa_lc(I,formattype):- define_ft(I),!.
assert_isa_lc(I,T):- cannot_table_call(get_isa_asserted(I,T)),!.
assert_isa_lc(I,T):- is_stable,!, hooked_asserta(isa(I,T)).
assert_isa_lc(I,T):- must_det((hooked_asserta(isa(I,T)),logOnFailureIgnore(get_isa_backchaing(I,T)))).

% ================================================
% assert_isa HOOKS
% ================================================
hook:decl_database_hook(assert(_),DATA):-into_mpred_form(DATA,O),!,O=isa(I,T),hotrace(doall(assert_isa_hooked(I,T))).
% hook:decl_database_hook(retract(_),isa(I,T)):-doall(db_retract_isa_hooked(I,T)).

assert_isa_hooked(I,T):- not(ground(assert_isa(I,T))),!, trace_or_throw(not(ground(assert_isa(I,T)))).
assert_isa_hooked(I,T):- asserta_if_new(moo:dbase_t(T,I)),fail.
assert_isa_hooked(T,type):-!,define_type(T),!.
assert_isa_hooked(T,formattype):-!,define_ft(T),!.
assert_isa_hooked(Term,mpred):-!,decl_mpred(Term).
assert_isa_hooked(Term,prologHybrid):-!,decl_mpred_hybrid(Term).
assert_isa_hooked(Term,prologOnly):-!,decl_mpred_prolog(Term).
assert_isa_hooked(I,_):- glean_pred_props_maybe(I),fail.
assert_isa_hooked(I,T):- argsIsaProps(T),decl_mpred(I,T),fail.
assert_isa_hooked(food5,'Weapon'):-trace_or_throw(assert_isa(food5,'Weapon')).

% assert_isa_hooked(I,T):- motel:defconcept(I,and([lexicon,T])).
% assert_isa_hooked(I,T):- motel:defprimconcept(I,T).
% assert_isa_hooked(I,T):-dmsg((told(assert_isa(I,T)))).


hook:decl_database_hook(assert(_),isa(I,T)):- doall(assert_isa_hooked_after(I,T)).

assert_isa_hooked_after(_,type):-!.
assert_isa_hooked_after(_,formattype):-!.
assert_isa_hooked_after(_,T):-argsIsaProps(T),!.
assert_isa_hooked_after(I,T):- is_creatable_type(T),!,assert_isa_hooked_creation(I,T).
assert_isa_hooked_after(I,T):- not(extentKnown(T)),impliedSubClass(T,ST),extentKnown(ST),assert_isa(I,ST).
%assert_isa_hooked_after(I,T):- assert_isa_hooked_creation(I,T).

extentKnown(Ext):- arg(_,vv(type,dir,formattype,string),Ext).
extentKnown(F):- is_creatable_type(F).
extentKnown(F):- argsIsaProps(F).
extentKnown(F):- is_asserted(isa(F,extentKnown)).


% one of 4 special types
assert_isa_hooked_creation(I,T):- is_creatable_type(T),!,call_after_game_load((world:create_instance(I,T,[]))).
% sublass of 4 special types
assert_isa_hooked_creation(I,T):- doall((is_creatable_type(ST),impliedSubClass(T,ST),call_after_game_load((world:create_instance(I,ST,[isa(T)]))))).

:-export transitive_subclass_tst/2.
transitive_subclass_tst(_,_):-!,fail.


:-export(get_isa_backchaing/2).
% get_isa_backchaing(A,T):- stack_depth(Level),Level>650,trace_or_throw(skip_dmsg_nope(failing_stack_overflow(get_isa_backchaing(A,T)))),!,fail.


get_isa_backchaing(A,T):- hotrace((fact_loop_checked(isa(A,T),get_isa_backchaing_0(A,T)))).

get_isa_backchaing_v_nv(A,term):-nonvar(A),!.
get_isa_backchaing_v_nv(_,var):-!.
get_isa_backchaing_v_nv(A,T):-setof(A,AT^(asserted_or_trans_subclass(AT,T),get_isa_asserted(A,AT)),List),!,member(A,List).

get_isa_backchaing_0(A,T):-  var(A),nonvar(T),!,get_isa_backchaing_v_nv(A,T).
get_isa_backchaing_0(A,T):-  var(T),!,setof(TT,AT^(get_isa_asserted(A,AT),asserted_or_trans_subclass(AT,TT)),List),!,member(T,List).
get_isa_backchaing_0(A,T):-  nonvar(A),get_isa_backchaing_nv_nv(A,T).
get_isa_backchaing_0(A,T):-  get_isa_asserted(A,AT),asserted_or_trans_subclass(AT,T).

get_isa_backchaing_nv_nv(A,argsIsa):-!,compound(A).

not_ft(T):-asserted_or_trans_subclass(T,obj).

:-export(get_isa_asserted/2).
get_isa_asserted(A,T):- fact_loop_checked(isa(A,T),get_isa_asserted_0(A,T)).

get_isa_asserted_0(A,T):- nonvar(A),nonvar(T),cached_isa(T,formattype),!,term_is_ft(A,T).
get_isa_asserted_0(A,T):- alt_dbase_t(T,A).
% get_isa_asserted_0(A,T):- compound(A),functor(A,F,_),!,get_isa_backchaing_1(F,T).
get_isa_asserted_0(_,T):- not(atom(T)),!,fail.
get_isa_asserted_0(A,T):- argsIsaProps(T),mpred_prop(A,T).
get_isa_asserted_0(A,T):- atom(A),mud_isa_atom(A,T),!.


mud_isa_atom(O,T):- atomic_list_concat_catch([T,_|_],'-',O),!.
mud_isa_atom(O,T):- atom_concat(T,Int,O),catch(atom_number(Int,_),_,fail),!.

cached_isa(I,T):-hotrace(get_isa_backchaing(I,T)).




% ============================================
% define_type/1
% ============================================
:- decl_mpred_hybrid type/1.
:- dynamic_multifile_exported never_type/1.
:- dynamic_multifile_exported define_type/1.

type(dir).
type(type).
type(mpred).
type(fpred).
type(relation).
type(creatableType).
type(argsIsa).
type(ArgsIsa):-argsIsaProps(ArgsIsa).
type(formattype).
type(actiontype).
type(region).
type(container).
dbase_t(formattype,string).
dbase_t(type,container).

mpred_prop(dbase_t,prologOnly).
mpred_prop(mpred_prop,prologOnly).



:- export(impliedSubClass/2).
impliedSubClass(T,ST):-ground(T:ST),is_known_false(subclass(T,ST)),!,fail.
impliedSubClass(T,ST):-predicate_property(transitive_subclass(T,ST),_),!,call_tabled(transitive_subclass(T,ST)).

:- export(asserted_subclass/2).
asserted_subclass(T,ST):-is_asserted(subclass(T,ST)).

:- export((transitive_subclass/2)).
transitive_subclass(A,T):-fact_loop_checked(subclass(A,T),transitive_subclass0(A,T)).

transitive_subclass0(FT,Sub):-asserted_subclass(FT,Sub).
transitive_subclass0(FT,Sub):-asserted_subclass(FT,A),asserted_subclass(A,Sub).
transitive_subclass0(FT,Sub):-asserted_subclass(FT,A),asserted_subclass(A,B),asserted_subclass(B,Sub).
transitive_subclass0(FT,Sub):-asserted_subclass(FT,A),asserted_subclass(A,B),asserted_subclass(B,C),asserted_subclass(C,Sub).
% transitive_subclass0(A,T):- dmsg(failed(transitive_subclass(A,T))),!,fail.


asserted_or_trans_subclass(A,A).
asserted_or_trans_subclass(A,B):-transitive_subclass(A,B).

:-dynamic_multifile_exported(not_mud_isa/2).
not_mud_isa(agent,formattype).
not_mud_isa(item,formattype).
not_mud_isa(type,formattype).
not_mud_isa(obj, extentKnown).
not_mud_isa(obj, creatableType).
not_mud_isa(obj, formattype).
not_mud_isa(formattype,formattype).
not_mud_isa(subft,type).
not_mud_isa('TemporallyExistingThing', 'TemporallyExistingThing').
not_mud_isa(creatableType,'TemporallyExistingThing').
not_mud_isa(X,type):-never_type(X).


:-dynamic_multifile_exported(type_isa/2).

type_isa(Type,creatableType):-arg(_,vv(agent,item,obj,region),Type),!.
type_isa(ArgIsa,mpredtype):-argsIsaProps(ArgIsa),!.
type_isa(Type,valuetype):-arg(_,vv(type,formattype,itemtype,valuetype,ppred,fpred),Type),!.
type_isa(string,formattype):-!.
type_isa(Type,valuetype):-arg(_,vv(concept,channel,place,dir,region),Type),!.
type_isa(Type,formattype):-formattype(Type),!. % text
%type_isa(_,valuetype).

:-dynamic_multifile_exported(is_known_true/1).

is_known_true(C):-has_free_args(C),!,fail.
is_known_true(isa(container,extentKnown)).
is_known_true(isa(extentKnown,extentKnown)).
is_known_true(isa(type,extentKnown)).
is_known_true(isa(gossup,channel)).
is_known_true(subclass(region,channel)).
is_known_true(subclass(agent,channel)).
is_known_true(isa(agent,creatableType)).
is_known_true(isa(region,creatableType)).
is_known_true(isa(_,id)).
is_known_true(isa(_,term)).
is_known_true(isa(singleValued, extentKnown)).
is_known_true(isa(creatableType,extentKnown)).
is_known_true(isa(formattype,extentKnown)).
is_known_true(isa(int,nonCreatableType)).
is_known_true(isa(type,type)).
is_known_true(isa(singleValued, type)).
is_known_true(isa(extentKnown, type)).
is_known_true(subclass(extentKnown, extentDecidable)).
is_known_true(subclass(singleValued, extentDecidable)).
is_known_true(subclass('MaleAnimal',agent)).
is_known_true(subclass(X,X)).
is_known_true(subclass(formattype,type)).
is_known_true(isa(type,nonCreatableType)).
is_known_true(isa(item,creatableType)).
is_known_true(subclass(item,creatableType)).
is_known_true(isa(formattype,nonCreatableType)).
is_known_true(subclass(formattype,nonCreatableType)).
is_known_true(isa('TemporallyExistingThing', 'creatableType')).
is_known_true(isa(term,nonCreatableType)).
is_known_true(subclass(argsIsa,relation)).
is_known_true(subclass(fpred,relation)).
is_known_true(subclass(F,mpred)):-argsIsaProps(F).
is_known_true(subclass(F,fpred)):-argsIsaProps(F).
is_known_true(subclass(F,relation)):-argsIsaProps(F).

has_free_args(C):-not(compound(C));not(not(arg(_,C,var))).

:-dynamic_multifile_exported(is_known_false/1).
% :-dynamic(is_known_false/1).
is_known_false(C):-has_free_args(C),!,fail.
is_known_false(isa(extentKnown,creatableType)).
is_known_false(isa(X,Y)):-not_mud_isa(X,Y).
is_known_false(Fact):-is_known_true(Fact),!,fail.
is_known_false(subclass(spatialthing,'MaleAnimal')).
is_known_false(subclass(Type,_)):-arg(_,vv(type,relation,spatialthing,formattype),Type).

is_known_false(subclass(A,B)):-disjointWith(A,B).
is_known_false(subclass(B,A)):-disjointWith(A,B).

disjointWith(agent,item).
disjointWith(region,obj).
disjointWith(formattype,item).
disjointWith(formattype,obj).
disjointWith(formattype,region).
disjointWith(creatableType,nonCreatableType).
disjointWith(A,B):- A=B,!,fail.
disjointWith(A,B):- asserted_or_trans_subclass(A,AS),asserted_or_trans_subclass(B,BS),(is_asserted(disjointWith(AS,BS));is_asserted(disjointWith(BS,AS))).
disjointWith(A,B):- once((type_isa(A,AT),type_isa(B,BT))),AT \= BT.


