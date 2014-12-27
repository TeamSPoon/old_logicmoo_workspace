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

decl_type(Spec):- never_type(Spec),!,trace_or_throw(never_type(Spec)).
decl_type(M:F):-!, '@'(decl_type(F), M).
decl_type([]):-!.
decl_type([A]):-!,decl_type(A).
decl_type([A|L]):-!,decl_type(A),decl_type(L).
decl_type((A,L)):-!,decl_type(A),decl_type(L).


decl_type(Spec):- compound(Spec),must_det(define_compound_as_type(Spec)).
decl_type(Spec):- decl_mpred(Spec,1),declare_dbase_local_dynamic(Spec,1), decl_type_unsafe(Spec).

decl_type_unsafe(Spec):- hasInstance(col,Spec),!.
decl_type_unsafe(Spec):- hooked_asserta(isa(Spec,col)),assert_hasInstance(col,Spec).

:- decl_mpred_hybrid(colDeclarer/1).

define_compound_as_type(Spec):- hasInstance(F,Spec),dmsg(once(define_compound_as_type(Spec,F))).
define_compound_as_type(Spec):- add(resultIsa(Spec,col)).
define_compound_as_type(Spec):- (assert_hasInstance(formattype,Spec)),dmsg(once(define_compound_as_type(Spec,formattype))).
define_compound_as_type(Spec):- compound(Spec),trace_or_throw(never_compound_define_type(Spec)).

:-forall(argsIsaProps(F),decl_type(F)).

:-dynamic_multifile_exported(define_ft/1).
define_ft(Spec):- never_type(Spec),!,trace_or_throw(never_ft(Spec)).
define_ft(M:F):- !, '@'(define_ft(F), M).
define_ft(Spec):- compound(Spec),functor(Spec,F,_),!,define_ft_0(F),define_ft_0(Spec).
define_ft(Spec):- define_ft_0(Spec).

define_ft_0(Spec):- hasInstance(formattype,Spec),!.
define_ft_0(Spec):- hasInstance(col,Spec),dmsg(once(maybe_converting_plain_type_to_formattype(Spec))),fail.
define_ft_0(Spec):- hooked_asserta(isa(Spec,formattype)),(assert_hasInstance(formattype,Spec)).

%col(Spec):- (isa_asserted(Spec,col)).

:-swi_export(decl_type_safe/1).
decl_type_safe(T):- compound(T),!.
decl_type_safe(T):- ignore((atom(T),not(never_type(T)),not(number(T)),decl_type(T))).

:-swi_export(assert_subclass/2).
assert_subclass(O,T):-assert_subclass_safe(O,T).

:-swi_export(assert_subclass_safe/2).
assert_subclass_safe(O,T):- ignore((nonvar(O),decl_type_safe(O),nonvar(T),decl_type_safe(T),nonvar(O),not(formattype(O)),not(formattype(T)),add(subclass(O,T)))).

:-swi_export(assert_isa_safe/2).
assert_isa_safe(O,T):- ignore((nonvar(O),nonvar(T),decl_type_safe(T),assert_isa(O,T))).

decl_database_hook(assert(_A_or_Z),subclass(S,C)):-decl_type_safe(S),decl_type_safe(C).

:- decl_type(col).
:- decl_type(completeExtentAsserted).
:- decl_type(createableType).
:- forall(argsIsaProps(Prop),(decl_type(Prop),asserta(is_known_trew(subclass(Prop,mpred))))).

is_creatable_type(Type):- arg(_,vv(agent,item,region,concept),Type).
is_creatable_type(Type):- atom(Type),call((isa_asserted(Type,createableType))).

decl_database_hook(assert(_A_or_Z),isa(W,createableType)):-decl_type_safe(W),call_after_game_load(forall(isa(I,W),create_instance(I,W))).
decl_database_hook(assert(_A_or_Z),isa(W,col)):-atom(W),atomic_list_concat(List,'_',W),!,length(List,2),!, append(FirstPart,[Last],List),atom_length(Last,AL),AL>3,not(member(flagged,FirstPart)),atomic_list_concat(FirstPart,'_',_NewCol),show_call_failure(assert_subclass_safe(W,Last)).


% ================================================
% assert_isa/2
% ================================================
:-swi_export(assert_isa/2).
:-dynamic_multifile_exported i_countable/1.

assert_isa(I,T):- not(ground(I:T)),trace_or_throw(not(ground(assert_isa(I,T)))).
assert_isa(_,term):-!.
assert_isa(_,term(_)):-!.
assert_isa(I,T):- loop_check(assert_isa_lc(I,T),true).

:-swi_export(assert_isa_lc/2).
% skip formatter cols
assert_isa_lc(_I,T):- member(T,[string,action,dir,apath]),!.
assert_isa_lc(I,T):- hotrace(formattype(T)),(compound(I)->true;dmsg(once(dont_assert_is_ft(I,T)))).
assert_isa_lc(I,T):- cannot_table_call(isa_asserted(I,T)),!.
assert_isa_lc(_,T):- once(decl_type(T)),fail.
assert_isa_lc(I,col):- decl_type(I),!.
assert_isa_lc(I,formattype):- define_ft(I),!.
assert_isa_lc(I,_):- not(mpred_prop(I,_)),not(col(I)),show_call(assert_if_new(i_countable(I))),fail.
assert_isa_lc(I,T):- is_release,!, hooked_asserta(isa(I,T)).
assert_isa_lc(I,T):- not_is_release, must_det((hooked_asserta(isa(I,T)),(isa_backchaing(I,T)))).
assert_isa_lc(I,T):- must_det((hooked_asserta(isa(I,T)),logOnFailureIgnore(isa_backchaing(I,T)))).


% ================================================
% assert_isa HOOKS
% ================================================
decl_database_hook(_,subclass(_,_)):-retractall(dbase_t(_,isa,_,_)),retractall(dbase_t(_,subclass,_,_)).
decl_database_hook(assert(_),DATA):-into_mpred_form(DATA,O),!,O=isa(I,T),hotrace(doall(assert_isa_hooked(I,T))).
% decl_database_hook(retract(_),isa(I,T)):-doall(db_retract_isa_hooked(I,T)).

assert_isa_hooked(A,_):-retractall(dbase_t(cache_I_L,isa,A,_)),fail.
assert_isa_hooked(F,T):-argsIsaProps(T),decl_mpred(F,T),fail.
assert_isa_hooked(I,T):- not(ground(assert_isa(I,T))),!, trace_or_throw(not(ground(assert_isa(I,T)))).
assert_isa_hooked(I,T):- assert_hasInstance(T,I),fail.

assert_isa_hooked(T,col):-!,decl_type(T),!.
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


decl_database_hook(assert(_),isa(I,T)):- doall(assert_isa_hooked_after(I,T)).

assert_isa_hooked_after(F,T):-argsIsaProps(T),!,decl_mpred(F,T).
assert_isa_hooked_after(_,col):-!.
assert_isa_hooked_after(_,formattype):-!.
assert_isa_hooked_after(I,T):- is_creatable_type(T),!,assert_isa_hooked_creation(I,T).
assert_isa_hooked_after(I,T):- not(completeExtentAsserted(T)),impliedSubClass(T,ST),completeExtentAsserted(ST),assert_isa(I,ST).
%assert_isa_hooked_after(I,T):- assert_isa_hooked_creation(I,T).

completeExtentAsserted(Ext):- arg(_,vv(col,dir,formattype,string),Ext).
completeExtentAsserted(F):- is_creatable_type(F).
completeExtentAsserted(F):- argsIsaProps(F).
completeExtentAsserted(F):- (isa_asserted(F,completeExtentAsserted)).




% one of 4 special cols
assert_isa_hooked_creation(I,T):- is_creatable_type(T),!,call_after_game_load((create_instance(I,T,[]))).
% sublass of 4 special cols
assert_isa_hooked_creation(I,T):- doall((is_creatable_type(ST),impliedSubClass(T,ST),call_after_game_load((create_instance(I,ST,[isa(T)]))))).

:-swi_export( transitive_subclass_tst/2).
transitive_subclass_tst(_,_):-!,fail.


% isa_backchaing(I,T):- stack_depth(Level),Level>650,trace_or_throw(skip_dmsg_nope(failing_stack_overflow(isa_backchaing(I,T)))),!,fail.

:-decl_mpred_prolog(isa_backchaing/2).
isa_backchaing(I,T):- fact_loop_checked(isa(I,T),no_repeats(isa_backchaing_0(I,T))).

isa_backchaing_v_nv(I,term):-nonvar(I),!.
isa_backchaing_v_nv(_,var):-!.
isa_backchaing_v_nv(I,T):-no_repeats([I],(transitive_subclass_or_same(AT,T),isa_asserted(I,AT))).

:-swi_export(isa_backchaing_0/2).
isa_backchaing_0(I,T):- T==var,!,var(I).
isa_backchaing_0(I,T):-  var(I),nonvar(T),!,isa_backchaing_v_nv(I,T).
isa_backchaing_0(I,T):-  var(T),!,setof(TT,AT^(isa_asserted(I,AT),transitive_subclass_or_same(AT,TT)),List),!,member(T,List).
isa_backchaing_0(I,T):-  nonvar(I),isa_backchaing_nv_nv(I,T),!.
isa_backchaing_0(I,T):-  transitive_subclass_or_same(AT,T),isa_asserted(I,AT).

% ==========================
% taxonomicPair(isa,subclass)
% ==========================

% A->TL = (isa_asserted_0(A,AT),transitive_subclass_or_same(AT,TL))
build_isa_inst_list_cache(A,Isa,B,(!,dbase_t(cache_I_I,Isa,A,B))):- Isa=isa, nonvar(A),once(dbase_t(cache_I_I,Isa,A,_)),!.
build_isa_inst_list_cache(A,Isa,B,(!,dbase_t(cache_I_I,Isa,A,B))):- Isa=isa, nonvar(A),
      forall(((isa_asserted_0(A,AT),transitive_subclass_or_same(AT,T))),assertz_if_new(dbase_t(cache_I_I,Isa,A,T))).
      

% A->TL = transitive_subclass(T,TL).
build_genls_inst_list_cache(A,Subclass,B,dbase_t(cache_I_I,Subclass,A,B)):- Subclass=subclass, nonvar(A),once(dbase_t(cache_I_I,Subclass,A,_)),!.
build_genls_inst_list_cache(A,Subclass,B,dbase_t(cache_I_I,Subclass,A,B)):- Subclass=subclass, nonvar(A),forall(transitive_subclass(A,T),assertz_if_new(dbase_t(cache_I_I,Subclass,A,T))).




isa_backchaing_nv_nv(A,argsIsaInList):-!,compound(A).
isa_backchaing_nv_nv(I,T):-compound(I),functor(I,F,_),isa_backchaing(F,T),!.
isa_backchaing_nv_nv(I,T):-atom(T),!,catch(call(T,I),_,fail).

not_ft(T):-transitive_subclass_or_same(T,spatialthing).



% ============================================
% isa_asserted/1
% ============================================
isa_asserted(I,T):-isa_asserted_motel(I,T).
isa_asserted(I,T):-type_isa(I,T).
isa_asserted(I,T):-isa_asserted_ft(I,T).

:-dynamic_multifile_exported(type_isa/2).

type_isa(Type,createableType):-arg(_,vv(agent,item,obj,region),Type),!.
type_isa(ArgIsa,mpredcol):-argsIsaProps(ArgIsa),!.
type_isa(Type,valuetype):-arg(_,vv(col,formattype,itemcol,valuetype,ppred,fpred),Type),!.
type_isa(string,formattype):-!.
type_isa(Type,valuetype):-arg(_,vv(concept,channel,place,dir,region),Type),!.
type_isa(Type,formattype):-formattype(Type),!. % text
%type_isa(_,valuetype).


isa_asserted_ft(I,T):- T==var,!,var(I).
isa_asserted_ft(I,term):-nonvar(I).
isa_asserted_ft(I,T):-string(I),!,member(T,[string,text]).
isa_asserted_ft(I,T):-integer(I),!,member(T,[int,integer,number,value]).
isa_asserted_ft(I,T):-number(I),!,member(T,[number,float,value]).
isa_asserted_ft(I,T):-atomic(I),!,member(T,[atom,id,name]).
isa_asserted_ft(_,compound).
isa_asserted_ft(I,T):-compound(I),functor(I,T,_),formattype(T).

isa_asserted_motel(I,T):-no_repeats(isa_asserted_nr(I,T)).

% isa_asserted_nr(I,T):- nonvar(I),fail,build_isa_inst_list_cache(I,_,T,CALL),CALL.
isa_asserted_nr(I,T):- stack_check,fact_loop_checked(isa(I,T),isa_asserted_0(I,T)).

isa_asserted_0(I,T):-atom(I),isa_w_inst_atom(I,T).
isa_asserted_0(I,T):-fact_always_true(isa(I,T)).

isa_asserted_0(I,T):- ((thlocal:useOnlyExternalDBs,!);thglobal:use_cyc_database),(kbp_t([isa,I,T]);kbp_t([T,I])).
isa_asserted_0(I,T):- hasInstance(T,I).   %isa_asserted_0(I,T):-clause(hasInstance(T,I),true). %isa_asserted_0(I,T):-clause(isa(I,T),true).

isa_asserted_0(I,T):-nonvar(T),isa_asserted_1(I,T).

isa_asserted_1(I,T):-T\=mped_type(_),mpred_prop(I,T).
isa_asserted_1(I,'&'(T1 , T2)):-nonvar(T1),var(T2),!,dif:dif(T1,T2),isa_backchaing(I,T1),impliedSubClass(T1,T2),isa_backchaing(I,T2).
isa_asserted_1(I,'&'(T1 , T2)):-nonvar(T1),!,dif:dif(T1,T2),isa_backchaing(I,T1),isa_backchaing(I,T2).
isa_asserted_1(I,(T1 ; T2)):-nonvar(T1),!,dif:dif(T1,T2),isa_backchaing(I,T1),isa_backchaing(I,T2).
isa_asserted_1(I,formattype):-!,isa_w_type_atom(I,formattype).
isa_asserted_1(I,col):-!,isa_w_type_atom(I,col).
isa_asserted_1(I,T):-atom(T),isa_w_type_atom(I,T).
isa_asserted_1(I,T):-nonvar(I),isa_asserted(T,formattype),!,term_is_ft(I,T).
% isa_asserted_1(I,T):- compound(I),functor(I,F,_),!,isa_backchaing_1(F,T).

%isa_w_inst_atom(O,T):- atomic_list_concat_catch([T,_|_],'-',O),!.
%isa_w_inst_atom(O,T):- atom_concat(T,Int,O),catch(atom_number(Int,_),_,fail),!.
isa_w_inst_atom(_,T):- T==atom. 

isa_w_type_atom(I,formattype):- clause(ft_info(I,_),true).
isa_w_type_atom(I,formattype):-!, clause(subft(I,_),true).
isa_w_type_atom(I,T):- argsIsaProps(T),!,mpred_prop(I,T).
isa_w_type_atom(I,T):- clause(mpred_prop(I,T),true).
isa_w_type_atom(I,T):- G=..[T,I],once_if_ground(isa_atom_call(T,G),_).

dont_call_type_arity_one(col).
dont_call_type_arity_one(formattype).
dont_call_type_arity_one(agentcol).
dont_call_type_arity_one(F):-mpred_prop(F,stubType(prologHybrid)),!.

isa_atom_call(T,G):-loop_check(isa_atom_call_lc(T,G),fail).

isa_atom_call_lc(_,G):- predicate_property(G,builtin),!,G.
isa_atom_call_lc(Type,_):-dont_call_type_arity_one(Type),!,fail.
isa_atom_call_lc(_,G):- predicate_property(G,number_of_clauses(_)),!,clause(G,B),call_mpred_body(G,B).
isa_atom_call_lc(_,G):- predicate_property(G,number_of_rules(R)),R>0,!,G.

cached_isa(I,T):-hotrace(isa_backchaing(I,T)).


% ============================================
% decl_type/1
% ============================================
:- decl_mpred_hybrid col/1.
:- dynamic_multifile_exported never_type/1.
:- dynamic_multifile_exported decl_type/1.

never_type(explorer(player2)).

col(dir).
col(col).
col(mpred).
col(fpred).
col(relation).
col(createableType).
col(colDeclarer).
% col(argsIsaInList).
% TODO decide if OK
col(ArgsIsa):-argsIsaProps(ArgsIsa).
col(F):-hasInstance(colDeclarer,F).
col(formattype).
col(actiontype).
col(region).
col(container).

mpred_prop(dbase_t,prologOnly).
mpred_prop(mpred_prop,prologOnly).



:- swi_export(impliedSubClass/2).
impliedSubClass(T,ST):-ground(T:ST),is_known_false(subclass(T,ST)),!,fail.
impliedSubClass(T,ST):-predicate_property(transitive_subclass(T,ST),_),!,call_tabled(transitive_subclass(T,ST)).

:- swi_export(asserted_subclass/2).
asserted_subclass(I,T):- ((thlocal:useOnlyExternalDBs,!);thglobal:use_cyc_database),(kbp_t([genls,I,T])).
asserted_subclass(T,ST):-dbase_t(subclass,T,ST).


into_single_class(Var,VV):-var(Var),!, (nonvar(VV)->into_single_class(VV,Var);Var=VV).
into_single_class(A,B):- compound(B),!, (compound(A) -> (into_single_class(A,AB),into_single_class(B,AB)) ; into_single_class(B,A) ).
into_single_class('&'(A,Var),VV):-var(Var),!,into_single_class(A,VV).
into_single_class('&'(A,B),VV):-!, into_single_class((B),VV);into_single_class((A),VV).
into_single_class(A,A).

:- swi_export((transitive_subclass_or_same/2)).
transitive_subclass_or_same(A,B):-var(A),!,A=B.
transitive_subclass_or_same(A,A):-nonvar(A),!.
transitive_subclass_or_same(A,B):-transitive_subclass(A,B).

:- swi_export((transitive_subclass/2)).
transitive_subclass(_,T):-T==formattype,!,fail.
transitive_subclass(A,_):-A==formattype,!,fail.
transitive_subclass(A,T):- fail, bad_idea,!, into_single_class(A,AA), into_single_class(T,TT), fact_loop_checked(subclass(A,T),transitive_P_l_r(dbase_t,subclass,AA,TT)).
transitive_subclass(I,T):- fail,stack_check,((thlocal:useOnlyExternalDBs,!);thglobal:use_cyc_database),
   fact_loop_checked(subclass(I,T),transitive_P_l_r(cyckb_t,genls,I,T)).
transitive_subclass(A,T):- fact_loop_checked(subclass(A,T),transitive_P(dbase_t,subclass,A,T)).

transitive_P(DB,P,L,R):-call(DB,P,L,R).
transitive_P(DB,P,L,R):-var(L),!,transitive_P_r_l(DB,P,L,R).
transitive_P(DB,P,L,R):-transitive_P_l_r(DB,P,L,R).

transitive_P_l_r(DB,P,L,R):-call(DB,P,L,A1),(call(DB,P,A1,R);call(DB,P,A1,A2),call(DB,P,A2,R)).
transitive_P_l_r(DB,P,L,R):-nonvar(R),call(DB,P,L,A1),call(DB,P,A1,A2),call(DB,P,A2,A3),call(DB,P,A3,R).

transitive_P_r_l(DB,P,L,R):-call(DB,P,A1,R),(call(DB,P,L,A1);call(DB,P,A2,A1),call(DB,P,L,A2)).
transitive_P_r_l(DB,P,L,R):-nonvar(L),call(DB,P,A3,R),call(DB,P,A2,A3),call(DB,P,A1,A2),call(DB,P,L,A1).



:-dynamic_multifile_exported(type_isa/2).

type_isa(Type,createableType):-arg(_,vv(agent,item,obj,region),Type),!.
type_isa(ArgIsa,mpredcol):-argsIsaProps(ArgIsa),!.
type_isa(Type,valuetype):-arg(_,vv(col,formattype,itemcol,valuetype,ppred,fpred),Type),!.
type_isa(string,formattype):-!.
type_isa(Type,valuetype):-arg(_,vv(concept,channel,place,dir,region),Type),!.
type_isa(Type,formattype):-formattype(Type),!. % text
%type_isa(_,valuetype).

:-dynamic_multifile_exported(is_known_trew/1).
:-dynamic_multifile_exported(is_known_true/1).

is_known_true(C):-has_free_args(C),!,trace_or_throw(has_free_args(is_known_trew,C)).
is_known_true(F):-is_known_false0(F),!,fail.
is_known_true(isa(X,spatialthing)):- (isa_asserted(X,_)),is_known_false0(isa(X,col)),is_known_false0(isa(X,formattype)),is_known_false0(isa(X,mpred)).
is_known_true(subclass(X,X)).
is_known_true(isa(_,id)).
is_known_true(isa(apath(_,_),areaPath)).
is_known_true(isa(apath(_,_),apath)).
is_known_true(isa(_,term)).


is_known_trew(isa(container,completeExtentAsserted)).
% is_known_trew(isa(formattype,metaclass)).
is_known_trew(isa(completeExtentAsserted,completeExtentAsserted)).
is_known_trew(isa(col,completeExtentAsserted)).
is_known_trew(isa(gossup,channel)).
is_known_trew(subclass(region,channel)).
is_known_trew(subclass(agent,channel)).
is_known_trew(isa(agent,createableType)).
is_known_trew(isa(region,createableType)).
is_known_trew(isa(singleValued, completeExtentAsserted)).
is_known_trew(isa(createableType,completeExtentAsserted)).
is_known_trew(isa(formattype,completeExtentAsserted)).
is_known_trew(isa(int,nonCreatableType)).
is_known_trew(isa(col,col)).
is_known_trew(isa(singleValued, col)).
is_known_trew(isa(completeExtentAsserted, col)).
is_known_trew(subclass(completeExtentAsserted, extentDecidable)).
is_known_trew(subclass(singleValued, extentDecidable)).
is_known_trew(subclass('MaleAnimal',agent)).
is_known_trew(subclass(formattype,col)).
is_known_trew(isa(col,nonCreatableType)).
is_known_trew(isa(item,createableType)).
is_known_trew(subclass(item,createableType)).
is_known_trew(isa(formattype,nonCreatableType)).
is_known_trew(subclass(formattype,nonCreatableType)).
is_known_trew(isa('TemporallyExistingThing', 'createableType')).
is_known_trew(isa(term,nonCreatableType)).
is_known_trew(subclass(argsIsaInList,relation)).
is_known_trew(subclass(fpred,relation)).


is_known_trew(subclass(F,mpred)):-argsIsaProps(F).
is_known_trew(subclass(F,fpred)):-argsIsaProps(F).
is_known_trew(subclass(F,relation)):-argsIsaProps(F).
is_known_trew(disjointWith(A,B)):-disjointWithT(A,B).




:-dynamic_multifile_exported(is_known_false/1).
% :-dynamic(is_known_false/1).
is_known_false(C):-has_free_args(C),!,fail.
is_known_false(F):-is_known_trew(F),!,fail.
is_known_false(F):-is_known_false0(F),!.

:-dynamic_multifile_exported(is_known_false0/1).
is_known_false0(isa(regioncol,formattype)).
is_known_false0(isa(formattype,formattype)).
is_known_false0(isa(X,spatialthing)):- col(X);formattype(X);mpred(X).
is_known_false0(isa(completeExtentAsserted,createableType)).
is_known_false0(isa(X,Y)):-!,not_mud_isa(X,Y).
is_known_false0(subclass(Type,_)):-arg(_,vv(col,relation,formattype),Type).

:-dynamic_multifile_exported(not_mud_isa/2).
not_mud_isa(agent,formattype).
not_mud_isa(item,formattype).
not_mud_isa(col,formattype).
not_mud_isa(obj, completeExtentAsserted).
not_mud_isa(obj, createableType).
not_mud_isa(assertionMacroHead, formattype).
not_mud_isa(obj, formattype).
not_mud_isa(formattype,formattype).
not_mud_isa(subft,col).
not_mud_isa('TemporallyExistingThing', 'TemporallyExistingThing').
not_mud_isa(createableType,'TemporallyExistingThing').
not_mud_isa(Type,formattype):- \+ (hasInstance(formattype, Type)).
not_mud_isa(Type, assertionMacroHead):- \+ (mpred_prop(Type, assertionMacroHead)).
not_mud_isa(Type, completeExtentAsserted):- \+ (mpred_prop(Type, completeExtentAsserted)).
not_mud_isa(X,col):-never_type(X).


:-dynamic_multifile_exported(disjointWith/2).

:-swi_export(has_free_args/1).
has_free_args(C):- not(ground(C)), compound(C),not(not(arg(_,C,var))),!.

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

:- assert_hasInstance(formattype,string).
:- assert_hasInstance(col,container).

