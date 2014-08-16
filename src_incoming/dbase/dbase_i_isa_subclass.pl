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

% nart_to_atomic(L,L):-!,atom(L).
nart_to_atomic(L,L).

never_type(Var):-var(Var),!,trace_or_throw(var_never_type(Var)).
never_type('Area1000').
never_type(subft).
never_type(must).
never_type(mpred_prop).
never_type(ft_info).
never_type(F):- mpred_arity(F,A),!, A > 1.

decl_type(Spec):- never_type(Spec),!,trace_or_throw(never_type(Spec)).
decl_type(M:F):-!, '@'(decl_type(F), M).
decl_type([]):-!.
decl_type([A]):-!,decl_type(A).
decl_type([A|L]):-!,decl_type(A),decl_type(L).
decl_type((A,L)):-!,decl_type(A),decl_type(L).


decl_type(Spec):- compound(Spec),must_det(define_compound_as_type(Spec)).
decl_type(Spec):- decl_mpred(Spec,1),declare_dbase_local_dynamic(Spec,1), decl_type_unsafe(Spec).

decl_type_unsafe(Spec):- dbase_t(type,Spec),!.
decl_type_unsafe(Spec):- add_w_hooks(dbase_t(type,Spec),isa(Spec,type)).

define_compound_as_type(Spec):- dbase_t(F,Spec),dmsg(once(define_compound_as_type(Spec,F))).
define_compound_as_type(Spec):- add(resultIsa(Spec,type)).
define_compound_as_type(Spec):- assertz_if_new(dbase_t(formattype,Spec)),dmsg(once(define_compound_as_type(Spec,formattype))).
define_compound_as_type(Spec):- compound(Spec),trace_or_throw(never_compound_define_type(Spec)).

:-forall(argsIsaProps(F),decl_type(F)).

:-dynamic_multifile_exported(define_ft/1).
define_ft(Spec):- never_type(Spec),!,trace_or_throw(never_ft(Spec)).
define_ft(M:F):- !, '@'(define_ft(F), M).
define_ft(Spec):- compound(Spec),functor(Spec,F,_),!,define_ft_0(F),define_ft_0(Spec).
define_ft(Spec):- define_ft_0(Spec).

define_ft_0(Spec):- dbase_t(formattype,Spec),!.
define_ft_0(Spec):- dbase_t(type,Spec),dmsg(once(maybe_converting_plain_type_to_formattype(Spec))),fail.
define_ft_0(Spec):- add_w_hooks(dbase_t(formattype,Spec),isa(Spec,formattype)).

%type(Spec):- is_asserted(isa(Spec,type)).

:-export(decl_type_safe/1).
decl_type_safe(T):- compound(T),!.
decl_type_safe(T):- ignore((atom(T),not(number(T)),decl_type(T))).

:-export(assert_subclass/2).
assert_subclass(O,T):-assert_subclass_safe(O,T).

:-export(assert_subclass_safe/2).
assert_subclass_safe(O,T):- ignore((nonvar(O),decl_type_safe(O))), ignore((nonvar(T),decl_type_safe(T),nonvar(O),not(formattype(O)),not(formattype(T)),add(subclass(O,T)))).

:-export(assert_isa_safe/2).
assert_isa_safe(O,T):- ignore((nonvar(O),nonvar(T),decl_type_safe(T),assert_isa(O,T))).

hook:decl_database_hook(assert(_A_or_Z),subclass(S,C)):-decl_type_safe(S),decl_type_safe(C).

:- decl_type(type).
:- decl_type(completeExtentAsserted).
:- decl_type(createableType).
:- forall(argsIsaProps(Prop),(decl_type(Prop),asserta(is_known_true(subclass(Prop,mpred))))).

is_creatable_type(Type):- arg(_,vv(agent,item,region,concept),Type).
is_creatable_type(Type):- atom(Type),call(is_asserted(isa(Type,createableType))).

hook:decl_database_hook(assert(_A_or_Z),isa(W,createableType)):-decl_type_safe(W),call_after_game_load(forall(isa(I,W),create_instance(I,W))).
hook:decl_database_hook(assert(_A_or_Z),isa(W,type)):-atom(W),atomic_list_concat(List,'_',W),!,length(List,2),!,
   append(FirstPart,[Last],List),atom_length(Last,AL),AL>3,not(member(flagged,FirstPart)),atomic_list_concat(FirstPart,'_',_NewCol),show_call(assert_subclass_safe(W,Last)).

% ================================================
% assert_isa/2
% ================================================
:-export assert_isa/2.

assert_isa(I,T):- not(ground(I:T)),trace_or_throw(not(ground(assert_isa(I,T)))).
assert_isa(I,T):- ((loop_check(assert_isa_lc(I,T),true))).

:-export assert_isa_lc/2.
% skip formatter types
assert_isa_lc(_I,T):- member(T,[string,action,dir]),!.
assert_isa_lc(I,T):- hotrace(formattype(T)),!,dmsg(once(dont_assert_is_ft(I,T))),!.
assert_isa_lc(_,T):- once(decl_type(T)),fail.
assert_isa_lc(I,type):- decl_type(I),!.
assert_isa_lc(I,formattype):- define_ft(I),!.
assert_isa_lc(I,T):- cannot_table_call(isa_asserted(I,T)),!.
assert_isa_lc(I,T):- is_stable,!, hooked_asserta(isa(I,T)).
assert_isa_lc(I,T):- must_det((hooked_asserta(isa(I,T)),logOnFailureIgnore(isa_backchaing(I,T)))).

% ================================================
% assert_isa HOOKS
% ================================================
hook:decl_database_hook(assert(_),DATA):-into_mpred_form(DATA,O),!,O=isa(I,T),hotrace(doall(assert_isa_hooked(I,T))).
% hook:decl_database_hook(retract(_),isa(I,T)):-doall(db_retract_isa_hooked(I,T)).

assert_isa_hooked(I,T):- not(ground(assert_isa(I,T))),!, trace_or_throw(not(ground(assert_isa(I,T)))).
assert_isa_hooked(I,T):- asserta_if_new(moo:dbase_t(T,I)),fail.
assert_isa_hooked(T,type):-!,decl_type(T),!.
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
assert_isa_hooked_after(I,T):- not(completeExtentAsserted(T)),impliedSubClass(T,ST),completeExtentAsserted(ST),assert_isa(I,ST).
%assert_isa_hooked_after(I,T):- assert_isa_hooked_creation(I,T).

completeExtentAsserted(Ext):- arg(_,vv(type,dir,formattype,string),Ext).
completeExtentAsserted(F):- is_creatable_type(F).
completeExtentAsserted(F):- argsIsaProps(F).
completeExtentAsserted(F):- is_asserted(isa(F,completeExtentAsserted)).


% one of 4 special types
assert_isa_hooked_creation(I,T):- is_creatable_type(T),!,call_after_game_load((world:create_instance(I,T,[]))).
% sublass of 4 special types
assert_isa_hooked_creation(I,T):- doall((is_creatable_type(ST),impliedSubClass(T,ST),call_after_game_load((world:create_instance(I,ST,[isa(T)]))))).

:-export transitive_subclass_tst/2.
transitive_subclass_tst(_,_):-!,fail.


:-export(isa_backchaing/2).
% isa_backchaing(A,T):- stack_depth(Level),Level>650,trace_or_throw(skip_dmsg_nope(failing_stack_overflow(isa_backchaing(A,T)))),!,fail.


isa_backchaing(A,T):- fact_loop_checked(isa(A,T),isa_backchaing_0(A,T)).

isa_backchaing_v_nv(A,term):-nonvar(A),!.
isa_backchaing_v_nv(_,var):-!.
isa_backchaing_v_nv(A,T):-setof(A,AT^(transitive_subclass_or_same(AT,T),isa_asserted(A,AT)),List),!,member(A,List).

isa_backchaing_0(A,T):-  var(A),nonvar(T),!,isa_backchaing_v_nv(A,T).
isa_backchaing_0(A,T):-  var(T),!,setof(TT,AT^(isa_asserted(A,AT),transitive_subclass_or_same(AT,TT)),List),!,member(T,List).
isa_backchaing_0(A,T):-  nonvar(A),isa_backchaing_nv_nv(A,T).
isa_backchaing_0(A,T):-  transitive_subclass_or_same(AT,T),isa_asserted(A,AT).


isa_backchaing_nv_nv(A,argsIsaInList):-!,compound(A).

not_ft(T):-transitive_subclass_or_same(T,spatialthing).

:-export(isa_asserted/2).
isa_asserted(A,T):- stack_check(1000),once_if_ground(fact_loop_checked(isa(A,T),isa_asserted_0(A,T)),_).

isa_asserted_0(I,T):-clause(dbase_t(T,I),true).
isa_asserted_0(I,T):-clause(isa(I,T),true).
isa_asserted_0(I,T):- fail, thglobal:use_cyc_database,kbp_t([isa,I,T]).
isa_asserted_0(I,T):-string(I),member(T,[string,text]).
isa_asserted_0(I,T):-atom(I),isa_w_inst_atom(I,T).
isa_asserted_0(_,T):-var(T),!,fail.
isa_asserted_0(I,formattype):-!,isa_w_type_atom(I,formattype).
isa_asserted_0(I,type):-!,isa_w_type_atom(I,type).
isa_asserted_0(I,T):-atom(T),isa_w_type_atom(I,T).
isa_asserted_0(I,T):-nonvar(I),isa_asserted(T,formattype),!,term_is_ft(I,T).
isa_asserted_0(I,'&'(T1 , T2)):-nonvar(T1),var(T2),!,dif:dif(T1,T2),isa_backchaing(I,T1),transitive_subclass(T1,T2),isa_backchaing(I,T2).
isa_asserted_0(I,'&'(T1 , T2)):-nonvar(T1),!,dif:dif(T1,T2),isa_backchaing(I,T1),isa_backchaing(I,T2).
isa_asserted_0(I,(T1 ; T2)):-nonvar(T1),!,dif:dif(T1,T2),isa_backchaing(I,T1),isa_backchaing(I,T2).

% isa_asserted_0(I,T):- compound(I),functor(I,F,_),!,isa_backchaing_1(F,T).

isa_w_inst_atom(O,T):- atomic_list_concat_catch([T,_|_],'-',O),!.
isa_w_inst_atom(O,T):- atom_concat(T,Int,O),catch(atom_number(Int,_),_,fail),!.
isa_w_inst_atom(_,T):- T==atom. 

isa_w_type_atom(I,formattype):- clause(ft_info(I,_),true).
isa_w_type_atom(I,formattype):-!, clause(subft(I,_),true).
isa_w_type_atom(I,T):- argsIsaProps(T),!,mpred_prop(I,T).
isa_w_type_atom(I,T):- clause(mpred_prop(I,T),true).
isa_w_type_atom(I,T):- G=..[T,I],once_if_ground(isa_atom_call(T,G),_).

dont_call_type_arity_one(type).
dont_call_type_arity_one(formattype).
dont_call_type_arity_one(agenttype).

isa_atom_call(T,G):-loop_check(isa_atom_call_lc(T,G),fail).

isa_atom_call_lc(_,G):- predicate_property(G,builtin),!,G.
isa_atom_call_lc(Type,_):-dont_call_type_arity_one(Type),!,fail.
isa_atom_call_lc(_,G):- predicate_property(G,number_of_rules(R)),R>0,!,G.
isa_atom_call_lc(_,G):- predicate_property(G,number_of_clauses(_)),!,clause(G,true).


cached_isa(I,T):-hotrace(isa_backchaing(I,T)).




% ============================================
% decl_type/1
% ============================================
:- decl_mpred_hybrid type/1.
:- dynamic_multifile_exported never_type/1.
:- dynamic_multifile_exported decl_type/1.

type(dir).
type(type).
type(mpred).
type(fpred).
type(relation).
type(createableType).
type(typeDeclarer).
% type(argsIsaInList).
% TODO decide if OK
type(ArgsIsa):-argsIsaProps(ArgsIsa).
type(F):-dbase_t(typeDeclarer,F).
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
asserted_subclass(T,ST):-dbase_t(subclass,T,ST).


into_single_class(Var,VV):-var(Var),!, (nonvar(VV)->into_single_class(VV,Var);Var=VV).
into_single_class(A,B):- compound(B),!, (compound(A) -> (into_single_class(A,AB),into_single_class(B,AB)) ; into_single_class(B,A) ).
into_single_class('&'(A,Var),VV):-var(Var),!,into_single_class(A,VV).
into_single_class('&'(A,B),VV):-!, into_single_class((B),VV);into_single_class((A),VV).
into_single_class(A,A).

:- export((transitive_subclass/2)).
transitive_subclass(_,T):-T==formattype,!,fail.
transitive_subclass(A,_):-A==formattype,!,fail.

transitive_subclass(A,T):- bad_idea,!,
      into_single_class(A,AA),
      into_single_class(T,TT),
      fact_loop_checked(subclass(A,T),transitive_subclass_l_r(AA,TT)).

transitive_subclass(A,T):- !,stack_check(1000),
      fact_loop_checked(subclass(A,T),transitive_subclass_l_r(A,T)).

transitive_subclass_l_r(FT,Sub):-dbase_t(subclass,FT,Sub).
transitive_subclass_l_r(FT,Sub):-var(FT),!,transitive_subclass_r_l(FT,Sub).
transitive_subclass_l_r(FT,Sub):-dbase_t(subclass,FT,A),dbase_t(subclass,A,Sub).
transitive_subclass_l_r(FT,Sub):-dbase_t(subclass,FT,A),dbase_t(subclass,A,B),dbase_t(subclass,B,Sub).
transitive_subclass_l_r(_,Sub):-var(Sub),!,fail.
transitive_subclass_l_r(FT,Sub):-dbase_t(subclass,FT,A),dbase_t(subclass,A,B),dbase_t(subclass,B,C),dbase_t(subclass,C,Sub).

transitive_subclass_r_l(FT,Sub):-dbase_t(subclass,A,Sub),dbase_t(subclass,FT,A).
transitive_subclass_r_l(FT,Sub):-dbase_t(subclass,B,Sub),dbase_t(subclass,A,B),dbase_t(subclass,FT,A).
transitive_subclass_r_l(_,Sub):-var(Sub),!,fail.
transitive_subclass_r_l(FT,Sub):-dbase_t(subclass,C,Sub),dbase_t(subclass,B,C),dbase_t(subclass,A,B),dbase_t(subclass,FT,A).

      
transitive_subclass_or_same(A,A).
% transitive_subclass_or_same(A,AA):-compound(A),into_single_class(A,AA).
transitive_subclass_or_same(A,B):-transitive_subclass(A,B).

:- export((transitive_bp/3)).
transitive_bp(P,A,T):-(atom(P)->Fact =..[P,A,T];Fact =..[dbase_t,P,A,T]),fact_loop_checked(Fact,transitive_P_l_r(P,A,T)).

transitive_P_l_r(P,FT,Sub):-dbase_t(P,FT,Sub).
transitive_P_l_r(P,FT,Sub):-var(FT),!,transitive_P_r_l(P,FT,Sub).
transitive_P_l_r(P,FT,Sub):-dbase_t(P,FT,A),dbase_t(P,A,Sub).
transitive_P_l_r(P,FT,Sub):-dbase_t(P,FT,A),dbase_t(P,A,B),dbase_t(P,B,Sub).
transitive_P_l_r(_P,_FT,Sub):-var(Sub),!,fail.
transitive_P_l_r(P,FT,Sub):-dbase_t(P,FT,A),dbase_t(P,A,B),dbase_t(P,B,C),dbase_t(P,C,Sub).

transitive_P_r_l(P,FT,Sub):-dbase_t(P,A,Sub),dbase_t(P,FT,A).
transitive_P_r_l(P,FT,Sub):-dbase_t(P,B,Sub),dbase_t(P,A,B),dbase_t(P,FT,A).
transitive_P_r_l(_P,_FT,Sub):-var(Sub),!,fail.
transitive_P_r_l(P,FT,Sub):-dbase_t(P,C,Sub),dbase_t(P,B,C),dbase_t(P,A,B),dbase_t(P,FT,A).


:-dynamic_multifile_exported(not_mud_isa/2).
not_mud_isa(agent,formattype).
not_mud_isa(item,formattype).
not_mud_isa(type,formattype).
not_mud_isa(obj, completeExtentAsserted).
not_mud_isa(obj, createableType).
not_mud_isa(assertionMacroHead, formattype).
not_mud_isa(obj, formattype).
not_mud_isa(formattype,formattype).
not_mud_isa(subft,type).
not_mud_isa('TemporallyExistingThing', 'TemporallyExistingThing').
not_mud_isa(createableType,'TemporallyExistingThing').
not_mud_isa(X,type):-never_type(X).


:-dynamic_multifile_exported(type_isa/2).

type_isa(Type,createableType):-arg(_,vv(agent,item,obj,region),Type),!.
type_isa(ArgIsa,mpredtype):-argsIsaProps(ArgIsa),!.
type_isa(Type,valuetype):-arg(_,vv(type,formattype,itemtype,valuetype,ppred,fpred),Type),!.
type_isa(string,formattype):-!.
type_isa(Type,valuetype):-arg(_,vv(concept,channel,place,dir,region),Type),!.
type_isa(Type,formattype):-formattype(Type),!. % text
%type_isa(_,valuetype).

:-dynamic_multifile_exported(is_known_true/1).

is_known_true(C):-has_free_args(C),!,fail.
is_known_true(isa(container,completeExtentAsserted)).
is_known_true(isa(completeExtentAsserted,completeExtentAsserted)).
is_known_true(isa(type,completeExtentAsserted)).
is_known_true(isa(gossup,channel)).
is_known_true(subclass(region,channel)).
is_known_true(subclass(agent,channel)).
is_known_true(isa(agent,createableType)).
is_known_true(isa(region,createableType)).
is_known_true(isa(_,id)).
is_known_true(isa(_,term)).
is_known_true(isa(singleValued, completeExtentAsserted)).
is_known_true(isa(createableType,completeExtentAsserted)).
is_known_true(isa(formattype,completeExtentAsserted)).
is_known_true(isa(int,nonCreatableType)).
is_known_true(isa(type,type)).
is_known_true(isa(singleValued, type)).
is_known_true(isa(completeExtentAsserted, type)).
is_known_true(subclass(completeExtentAsserted, extentDecidable)).
is_known_true(subclass(singleValued, extentDecidable)).
is_known_true(subclass('MaleAnimal',agent)).
is_known_true(subclass(X,X)).
is_known_true(subclass(formattype,type)).
is_known_true(isa(type,nonCreatableType)).
is_known_true(isa(item,createableType)).
is_known_true(subclass(item,createableType)).
is_known_true(isa(formattype,nonCreatableType)).
is_known_true(subclass(formattype,nonCreatableType)).
is_known_true(isa('TemporallyExistingThing', 'createableType')).
is_known_true(isa(term,nonCreatableType)).
is_known_true(subclass(argsIsaInList,relation)).
is_known_true(subclass(fpred,relation)).
is_known_true(subclass(F,mpred)):-argsIsaProps(F).
is_known_true(subclass(F,fpred)):-argsIsaProps(F).
is_known_true(subclass(F,relation)):-argsIsaProps(F).
is_known_true(disjointWith(A,B)):-disjointWithT(A,B).


:-dynamic_multifile_exported(disjointWith/2).

:-export(has_free_args/1).
has_free_args(C):- not(ground(C)), compound(C),not(not(arg(_,C,var))),!.

:-dynamic_multifile_exported(is_known_false/1).
% :-dynamic(is_known_false/1).
is_known_false(C):-has_free_args(C),!,fail.
is_known_false(isa(completeExtentAsserted,createableType)).
is_known_false(isa(X,Y)):-not_mud_isa(X,Y).
is_known_false(Fact):-is_known_true(Fact),!,fail.
is_known_false(subclass(spatialthing,'MaleAnimal')).
is_known_false(subclass(Type,_)):-arg(_,vv(type,relation,spatialthing,formattype),Type).

% is_known_false(subclass(A,B)):-disjointWith(A,B).

disjointWith0(agent,item).
disjointWith0(region,obj).
disjointWith0(formattype,item).
disjointWith0(formattype,obj).
disjointWith0(formattype,region).
disjointWith0(createableType,nonCreatableType).

disjointWithT(A,B):-disjointWith0(A,B).
disjointWithT(B,A):-disjointWith0(A,B).

disjointWith(A,B):- A=B,!,fail.
disjointWith(A,B):-disjointWithT(A,B).
disjointWith(A,B):-disjointWithT(AS,BS),transitive_subclass_or_same(A,AS),transitive_subclass_or_same(B,BS).
disjointWith(A,B):- once((type_isa(A,AT),type_isa(B,BT))),AT \= BT.


