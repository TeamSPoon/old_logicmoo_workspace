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

:- dynamic_multifile_exported fact_always_true/1.

% ============================================
:- [dbase_i_isa_subclass_impl].
/*
:- use_module(dbase_i_isa_subclass_impl,
 [decl_type_motel/1,subclass_motel/2,isa_asserted_motel/2,isa_backchaing_motel/2,assert_subclass_motel/2,assert_disjoint_motel/2,assert_isa_motel/2]). 
*/
% ============================================


% ============================================
% decl_type/1, decl_type_safe/1, never_type/1.
% ============================================
:- dynamic_multifile_exported decl_type/1.

decl_type(Spec):- never_type(Spec),!,trace_or_throw(never_type(Spec)).
decl_type(M:F):-!, '@'(decl_type(F), M).
decl_type([]):-!.
decl_type([A]):-!,decl_type(A).
decl_type([A|L]):-!,decl_type(A),decl_type(L).
decl_type((A,L)):-!,decl_type(A),decl_type(L).
decl_type(Spec):- compound(Spec),must_det(define_compound_as_type(Spec)).
decl_type(Spec):- decl_mpred(Spec,1),declare_dbase_local_dynamic(Spec,1), decl_type_motel(Spec).

:- decl_mpred_hybrid type/1.
:- dynamic_multifile_exported never_type/1.

never_type(explorer(player2)).

% ============================================
% decl_type_safe/1
% ============================================
:-swi_export(decl_type_safe/1).
decl_type_safe(T):- compound(T),!.
decl_type_safe(T):- ignore((atom(T),not(never_type(T)),not(number(T)),decl_type(T))).


% ============================================
% define_compound_as_type/1
% ============================================

define_compound_as_type(Spec):- dbase_t(F,Spec),dmsg(once(define_compound_as_type(Spec,F))).
define_compound_as_type(Spec):- add(resultIsa(Spec,type)).
define_compound_as_type(Spec):- assertz_if_new(dbase_t(formattype,Spec)),dmsg(once(define_compound_as_type(Spec,formattype))).
define_compound_as_type(Spec):- compound(Spec),trace_or_throw(never_compound_define_type(Spec)).


% ============================================
% define_ft/1
% ============================================

:-dynamic_multifile_exported(define_ft/1).
define_ft(Spec):- never_type(Spec),!,trace_or_throw(never_ft(Spec)).
define_ft(M:F):- !, '@'(define_ft(F), M).
define_ft(Spec):- isa_asserted(Spec,type),dmsg(once(maybe_converting_plain_type_to_formattype(Spec))),fail.
define_ft(Spec):- once((compound(Spec),functor(Spec,F,_),define_ft(F))),fail.
define_ft(Spec):- assert_isa(Spec,formattype).



decl_database_hook(assert(_A_or_Z),subclass(S,C)):-decl_type_safe(S),decl_type_safe(C).
decl_database_hook(assert(_A_or_Z),isa(W,createableType)):-decl_type_safe(W),call_after_game_load(forall(isa(I,W),create_instance(I,W))).
decl_database_hook(assert(_A_or_Z),isa(W,type)):-atom(W),atomic_list_concat(List,'_',W),!,length(List,2),!,
   append(FirstPart,[Last],List),atom_length(Last,AL),AL>3,not(member(flagged,FirstPart)),atomic_list_concat(FirstPart,'_',_NewCol),show_call_failure(assert_subclass_safe(W,Last)).



% ============================================
:-swi_export(assert_subclass/2).
% ============================================
assert_subclass(O,T):-disjointWith(O,T),!,trace_or_throw(never_subclass(O,T)).
assert_subclass(O,T):-subclass_asserted(O,T),!.
assert_subclass(O,T):-assert_subclass_motel(O,T).


% ============================================
:- swi_export(subclass_asserted/2).
% ============================================
subclass_asserted(I,T):- ((thlocal:useOnlyExternalDBs,!);thglobal:use_cyc_database),(kbp_t([genls,I,T])).
subclass_asserted(T,ST):-subclass_asserted_motel(T,ST).


into_single_class(Var,VV):-var(Var),!, (nonvar(VV)->into_single_class(VV,Var);Var=VV).
into_single_class(A,B):- compound(B),!, (compound(A) -> (into_single_class(A,AB),into_single_class(B,AB)) ; into_single_class(B,A) ).
into_single_class('&'(A,Var),VV):-var(Var),!,into_single_class(A,VV).
into_single_class('&'(A,B),VV):-!, into_single_class((B),VV);into_single_class((A),VV).
into_single_class(A,A).

:- swi_export(subclass_backchaing/2).
subclass_backchaing(T,ST):-ground(T:ST),is_known_false(subclass(T,ST)),!,fail.
subclass_backchaing(_,T):-T==formattype,!,fail.
subclass_backchaing(A,_):-A==formattype,!,fail.
subclass_backchaing(A,T):- bad_idea,!, into_single_class(A,AA), into_single_class(T,TT), fact_loop_checked(subclass(A,T),transitive_P_l_r(dbase_t,subclass,AA,TT)).
subclass_backchaing(I,T):- stack_check,((thlocal:useOnlyExternalDBs,!);thglobal:use_cyc_database),
   fact_loop_checked(subclass(I,T),transitive_P_l_r(cyckb_t,genls,I,T)).
subclass_backchaing(I,T):-subclass_backchaing_motel(I,T).


% ============================================
:-swi_export(assert_isa_safe/2).
% ============================================
assert_isa_safe(O,T):- ignore((nonvar(O),nonvar(T),decl_type_safe(T),assert_isa(O,T))).

% ============================================
% isa_asserted/1
% ============================================
isa_asserted(I,T):-isa_asserted_motel(I,T).
isa_asserted(I,T):-type_isa(I,T).
isa_asserted(I,T):-isa_asserted_ft(I,T).

:-dynamic_multifile_exported(type_isa/2).

type_isa(Type,createableType):-arg(_,vv(agent,item,obj,region),Type),!.
type_isa(ArgIsa,mpredtype):-argsIsaProps(ArgIsa),!.
type_isa(Type,valuetype):-arg(_,vv(type,formattype,itemtype,valuetype,ppred,fpred),Type),!.
type_isa(string,formattype):-!.
type_isa(Type,valuetype):-arg(_,vv(concept,channel,place,dir,region),Type),!.
type_isa(Type,formattype):-formattype(Type),!. % text
%type_isa(_,valuetype).


isa_asserted_ft(A,T):- T==var,!,var(A).
isa_asserted_ft(I,T):-string(I),!,member(T,[string,text]).
isa_asserted_ft(I,T):-integer(I),!,member(T,[int,number,value]).
isa_asserted_ft(I,T):-number(I),!,member(T,[number,float,value]).



:-dynamic_multifile_exported(disjointWith/2).

:-swi_export(has_free_args/1).
has_free_args(C):- not(ground(C)), compound(C),not(not(arg(_,C,var))),!.



% ================================================
% assert_isa/2
% ================================================
:-swi_export(assert_isa/2).
:-dynamic_multifile_exported i_countable/1.

assert_isa(I,T):- not(ground(I:T)),trace_or_throw(not(ground(assert_isa(I,T)))).
assert_isa(_,FT):-formattype(FT),!.
assert_isa(I,T):- loop_check(assert_isa_lc(I,T),true).

:-swi_export(assert_isa_lc/2).
% skip formatter types
assert_isa_lc(_,T):- once(decl_type(T)),fail.
assert_isa_lc(I,type):- decl_type(I),!.
assert_isa_lc(I,T):- isa_asserted_motel(I,T),!.
assert_isa_lc(I,formattype):- define_ft(I),!.
assert_isa_lc(I,_):- not(mpred_prop(I,_)),not(type(I)),show_call(assert_if_new(i_countable(I))),fail.
assert_isa_lc(I,T):- assert_isa_motel(I,T), must_det((hooked_asserta(isa(I,T)),
   logOnFailureIgnore(isa_backchaing(I,T);((trace,isa_backchaing(I,T)))))).

% ================================================
% cached_isa/2
% ================================================
cached_isa(I,T):-hotrace(isa_backchaing(I,T)).

decl_database_hook(assert(_A_or_Z),isa(S,typeDeclarer)):-decl_type(S).

:- decl_type(formattype).
:- decl_type(dir).
:- decl_type(type).
:- decl_type(mpred).
:- decl_type(fpred).
:- decl_type(relation).
:- decl_type(createableType).
:- decl_type(typeDeclarer).
% type(argsIsaInList).
% TODO decide if OK

:- decl_type(actiontype).
:- decl_type(region).
:- decl_type(container).
:- assert_isa(string,formattype).
:- assert_isa(container,createableType).
:- forall(argsIsaProps(F),decl_type(F)).
:- decl_type(type).
:- decl_type(completeExtentAsserted).
:- decl_type(createableType).
:- forall(argsIsaProps(Prop),(decl_type(Prop),asserta(is_known_trew(subclass(Prop,mpred))))).
:- forall(arg(_,vv(agent,item,region,concept),Type),(decl_type(Type),assert_isa(Type,createableType))).




