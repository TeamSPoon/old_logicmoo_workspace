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

decl_type_unsafe(Spec):- hasInstance(tCol,Spec),!.
decl_type_unsafe(Spec):- hooked_asserta(mudIsa(Spec,tCol)),assert_hasInstance(tCol,Spec).

:- decl_mpred_hybrid(macroDeclarer/1).

define_compound_as_type(Spec):- hasInstance(F,Spec),dmsg(once(define_compound_as_type(Spec,F))).
define_compound_as_type(Spec):- add(resultIsa(Spec,tCol)).
define_compound_as_type(Spec):- (assert_hasInstance(ttFormatType,Spec)),dmsg(once(define_compound_as_type(Spec,ttFormatType))).
define_compound_as_type(Spec):- compound(Spec),trace_or_throw(never_compound_define_type(Spec)).

:-forall(ttDeclarer(F),decl_type(F)).

:-dynamic_multifile_exported(define_ft/1).
define_ft(Spec):- never_type(Spec),!,trace_or_throw(never_ft(Spec)).
define_ft(M:F):- !, '@'(define_ft(F), M).
define_ft(Spec):- compound(Spec),functor(Spec,F,_),!,define_ft_0(F),define_ft_0(Spec).
define_ft(Spec):- define_ft_0(Spec).

define_ft_0(Spec):- hasInstance(ttFormatType,Spec),!.
define_ft_0(Spec):- hasInstance(tCol,Spec),dmsg(once(maybe_converting_plain_type_to_formattype(Spec))),fail.
define_ft_0(Spec):- hooked_asserta(mudIsa(Spec,ttFormatType)),(assert_hasInstance(ttFormatType,Spec)).

%col(Spec):- (isa_asserted(Spec,col)).

:-swi_export(decl_type_safe/1).
decl_type_safe(T):- compound(T),!.
decl_type_safe(T):- ignore((atom(T),not(never_type(T)),not(number(T)),decl_type(T))).

:-swi_export(assert_subclass/2).
assert_subclass(O,T):-assert_subclass_safe(O,T).

:-swi_export(assert_subclass_safe/2).
assert_subclass_safe(O,T):- ignore((nonvar(O),decl_type_safe(O),nonvar(T),decl_type_safe(T),nonvar(O),not(ttFormatType(O)),not(ttFormatType(T)),add(mudSubclass(O,T)))).

:-swi_export(assert_isa_safe/2).
assert_isa_safe(O,T):- ignore((nonvar(O),nonvar(T),decl_type_safe(T),assert_isa(O,T))).

decl_database_hook(assert(_A_or_Z),mudSubclass(S,C)):-decl_type_safe(S),decl_type_safe(C).

:- decl_type(tCol).
:- decl_type(ttFormatType).
:- decl_type(ttCompleteExtentAsserted).
:- decl_type(ttCreateable).
:- forall(ttDeclarer(Prop),(decl_type(Prop),asserta(is_known_trew(mudSubclass(Prop,tPred))))).

:- decl_thlocal(infSupertypeName).

ttCreateable(Type):- arg(_,vv(tAgentGeneric,tItem,tRegion),Type).

decl_database_hook(assert(_A_or_Z),mudIsa(W,ttCreateable)):-decl_type_safe(W),call_after_game_load(forall(mudIsa(I,W),create_instance(I,W))).
decl_database_hook(assert(_A_or_Z),mudIsa(W,tCol)):- (test_tl(infSupertypeName);true),guess_supertypes(W).

guess_supertypes(W):-atom(W),atomic_list_concat(List,'_',W),length(List,S),S>2,!, append(FirstPart,[Last],List),atom_length(Last,AL),AL>3,not(member(flagged,FirstPart)),atomic_list_concat(FirstPart,'_',_NewCol),show_call_failure(assert_subclass_safe(W,Last)).
guess_supertypes(W):-atom(W),to_first_break(W,lower,tt,_,upper),!,assert_isa(W,ttTypeType),!.
guess_supertypes(W):-atom(W),T=t,to_first_break(W,lower,T,All,upper),!,to_first_break(All,upper,_,Super,Rest),
   atom_length(Rest,L),!,L>2,i_name(T,Rest,Super),show_call_failure(assert_subclass_safe(W,Super)),!.


% ================================================
% assert_isa/2
% ================================================
:-swi_export(assert_isa/2).
:-dynamic_multifile_exported i_countable/1.

assert_isa(I,T):- not(ground(I:T)),trace_or_throw(not(ground(assert_isa(I,T)))).
assert_isa(_,ftTerm):-!.
assert_isa(_,ftTerm(_)):-!.
assert_isa(I,T):- loop_check(assert_isa_lc(I,T),true).

:-swi_export(assert_isa_lc/2).
% skip formatter cols
assert_isa_lc(_I,T):- member(T,[ftString,ftAction,vtDirection,apathFn]),!.
assert_isa_lc(I,T):- hotrace(ttFormatType(T)),(compound(I)->true;dmsg(once(dont_assert_is_ft(I,T)))).
assert_isa_lc(I,T):- cannot_table_call(isa_asserted(I,T)),!.
assert_isa_lc(_,T):- once(decl_type(T)),fail.
assert_isa_lc(I,tCol):- decl_type(I),!.
assert_isa_lc(I,ttFormatType):- define_ft(I),!.
assert_isa_lc(I,_):- not(mpred_prop(I,_)),not(tCol(I)),show_call(assert_if_new(i_countable(I))),fail.
assert_isa_lc(I,T):- is_release,!, hooked_asserta(mudIsa(I,T)).
assert_isa_lc(I,T):- not_is_release, must_det((hooked_asserta(mudIsa(I,T)),(isa_backchaing(I,T)))).
assert_isa_lc(I,T):- must_det((hooked_asserta(mudIsa(I,T)),logOnFailureIgnore(isa_backchaing(I,T)))).


% ================================================
% assert_isa HOOKS
% ================================================
decl_database_hook(_,mudSubclass(_,_)):-retractall(dbase_t(_,mudIsa,_,_)),retractall(dbase_t(_,mudSubclass,_,_)).
decl_database_hook(assert(_),DATA):-into_mpred_form(DATA,O),!,O=mudIsa(I,T),hotrace(doall(assert_isa_hooked(I,T))).
% decl_database_hook(retract(_),isa(I,T)):-doall(db_retract_isa_hooked(I,T)).

assert_isa_hooked(A,_):-retractall(dbase_t(cache_I_L,mudIsa,A,_)),fail.
assert_isa_hooked(F,T):-ttDeclarer(T),decl_mpred(F,T),fail.
assert_isa_hooked(I,T):- not(ground(assert_isa(I,T))),!, trace_or_throw(not(ground(assert_isa(I,T)))).
assert_isa_hooked(I,T):- assert_hasInstance(T,I),fail.

assert_isa_hooked(T,tCol):-!,decl_type(T),!.
assert_isa_hooked(T,ttFormatType):-!,define_ft(T),!.
assert_isa_hooked(Term,tPred):-!,decl_mpred(Term).
assert_isa_hooked(Term,prologHybrid):-!,decl_mpred_hybrid(Term).
assert_isa_hooked(Term,prologOnly):-!,decl_mpred_prolog(Term).
assert_isa_hooked(I,_):- glean_pred_props_maybe(I),fail.
assert_isa_hooked(I,T):- ttDeclarer(T),decl_mpred(I,T),fail.
assert_isa_hooked(food5,tWeapon):-trace_or_throw(assert_isa(food5,tWeapon)).

% assert_isa_hooked(I,T):- motel:defconcept(I,isAnd([lexicon,T])).
% assert_isa_hooked(I,T):- motel:defprimconcept(I,T).
% assert_isa_hooked(I,T):-dmsg((told(assert_isa(I,T)))).


decl_database_hook(assert(_),mudIsa(I,T)):- doall(assert_isa_hooked_after(I,T)).

assert_isa_hooked_after(F,T):-ttDeclarer(T),!,decl_mpred(F,T).
assert_isa_hooked_after(_,tCol):-!.
assert_isa_hooked_after(_,ttFormatType):-!.
assert_isa_hooked_after(I,T):- ttCreateable(T),!,assert_isa_hooked_creation(I,T).
assert_isa_hooked_after(I,T):- not(ttCompleteExtentAsserted(T)),impliedSubClass(T,ST),ttCompleteExtentAsserted(ST),assert_isa(I,ST).
%assert_isa_hooked_after(I,T):- assert_isa_hooked_creation(I,T).

ttCompleteExtentAsserted(Ext):- arg(_,vv(tCol,vtDirection,ttFormatType,ftString),Ext).
ttCompleteExtentAsserted(F):- ttCreateable(F).
ttCompleteExtentAsserted(F):- ttDeclarer(F).
ttCompleteExtentAsserted(F):- (isa_asserted(F,ttCompleteExtentAsserted)).




% one of 4 special cols
assert_isa_hooked_creation(I,T):- ttCreateable(T),!,call_after_game_load((create_instance(I,T,[]))).
% sublass of 4 special cols
assert_isa_hooked_creation(I,T):- doall((ttCreateable(ST),impliedSubClass(T,ST),call_after_game_load((create_instance(I,ST,[mudIsa(T)]))))).

:-swi_export( transitive_subclass_tst/2).
transitive_subclass_tst(_,_):-!,fail.


% isa_backchaing(I,T):- stack_depth(Level),Level>650,trace_or_throw(skip_dmsg_nope(failing_stack_overflow(isa_backchaing(I,T)))),!,fail.

:-decl_mpred_prolog(isa_backchaing/2).
isa_backchaing(I,T):- fact_loop_checked(mudIsa(I,T),no_repeats(isa_backchaing_0(I,T))).

isa_backchaing_v_nv(I,ftTerm):-nonvar(I),!.
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
build_isa_inst_list_cache(A,Isa,B,(!,dbase_t(cache_I_I,Isa,A,B))):- Isa=mudIsa, nonvar(A),once(dbase_t(cache_I_I,Isa,A,_)),!.
build_isa_inst_list_cache(A,Isa,B,(!,dbase_t(cache_I_I,Isa,A,B))):- Isa=mudIsa, nonvar(A),
      forall(((isa_asserted_0(A,AT),transitive_subclass_or_same(AT,T))),assertz_if_new(dbase_t(cache_I_I,Isa,A,T))).
      

% A->TL = transitive_subclass(T,TL).
build_genls_inst_list_cache(A,Subclass,B,dbase_t(cache_I_I,Subclass,A,B)):- Subclass=mudSubclass, nonvar(A),once(dbase_t(cache_I_I,Subclass,A,_)),!.
build_genls_inst_list_cache(A,Subclass,B,dbase_t(cache_I_I,Subclass,A,B)):- Subclass=mudSubclass, nonvar(A),forall(transitive_subclass(A,T),assertz_if_new(dbase_t(cache_I_I,Subclass,A,T))).




isa_backchaing_nv_nv(A,predArgTypes):-!,compound(A).
isa_backchaing_nv_nv(I,T):-compound(I),functor(I,F,_),isa_backchaing(F,T),!.
isa_backchaing_nv_nv(I,T):-atom(T),!,catch(call(T,I),_,fail).

not_ft(T):-transitive_subclass_or_same(T,tSpatialThing).



% ============================================
% isa_asserted/1
% ============================================
isa_asserted(I,T):-no_repeats(isa_asserted_nr(I,T)).
isa_asserted_motel(I,T):-no_repeats(isa_asserted_nr(I,T)).

:-dynamic_multifile_exported(type_isa/2).

type_isa(Type,ttCreateable):-arg(_,vv(tAgentGeneric,tItem,tObj,tRegion),Type),!.
type_isa(ArgIsa,ttPredType):-ttDeclarer(ArgIsa),!.
type_isa(ftString,ttFormatType):-!.
type_isa(Type,ttFormatType):-ttFormatType(Type),!. % text
%  from name


atom_prefix_other(Inst,Prefix,Other):-atom_type_prefix_other(Inst,_,Prefix,Other).
atom_type_prefix_other(Inst,Type,Prefix,Other):-type_prefix(Prefix,Type),current_atom(Inst),atom_concat(Prefix,Other,Inst),capitalized(Other).
atom_type_prefix_other(Inst,Type,Suffix,Other):-type_suffix(Suffix,Type),current_atom(Inst),atom_concat(Other,Suffix,Inst),!.


isa_from_morphology(Inst,Type):-type_prefix(Prefix,Type),current_atom(Inst),atom_concat(Prefix,Other,Inst),capitalized(Other).
isa_from_morphology(Inst,Type):-type_suffix(Suffix,Type),current_atom(Inst),atom_concat(_,Suffix,Inst),!.

type_suffix('Fn',ftFunctional).
type_suffix('Type',ttTypeType).
type_suffix('able',ttTypeByAction).


type_prefix(is,ftSyntaxOperator).
type_prefix(txt,ftTextType).
type_prefix(sk,ftSkolemFunction).
type_prefix(fn,ftSkolemFunction).
type_prefix(mud,tPred).
type_prefix(prop,tPred).
type_prefix(pred,tPred).
type_prefix(act,ftAction).

type_prefix(prolog,ttPredType).
type_prefix(vt,ttValueType).
type_prefix(ft,ttFormatType).
type_prefix(pred,ttPredType).
type_prefix(tt,ttTypeType).
type_prefix(t,ttObjectType).
type_prefix(a,ttAnyType).

type_prefix(v,ftValue).
type_prefix(i,ftID).

callOr(Pred,I,T):-(call(Pred,I);call(Pred,T)),!.

type_deduced(I,T):-atom(T),i_name(mud,T,P),!,dbase_t(P,_,I).
type_deduced(I,T):-nonvar(I),not(number(I)),dbase_t(P,_,I),(argIsa_known(P,2,AT)->T=AT;i_name(vt,P,T)).


isa_asserted_ft(I,T):- T==ftVar,!,var(I).
isa_asserted_ft(I,T):- var(I),!,T=ftVar.
isa_asserted_ft(I,ftTerm):-nonvar(I).
isa_asserted_ft(I,T):-is_string(I),member(T,[ftText]).
isa_asserted_ft(I,T):-string(I),!,member(T,[ftString,ftText]).
isa_asserted_ft(I,T):-integer(I),!,member(T,[ftInt,ftInteger,ftNumber]).
isa_asserted_ft(I,T):-number(I),!,member(T,[ftNumber,ftFloat]).
isa_asserted_ft(I,ftCallable):-is_callable(I).
isa_asserted_ft(I,T):-atomic(I),!,member(T,[ftAtom,ftID]).
isa_asserted_ft(_,ftCompound).
isa_asserted_ft(I,T):-compound(I),functor(I,T,_),ttFormatType(T).

% isa_asserted_nr(I,T):- nonvar(I),fail,build_isa_inst_list_cache(I,_,T,CALL),CALL.
isa_asserted_nr(I,T):- stack_check,fact_loop_checked(mudIsa(I,T),isa_asserted_0(I,T)).

isa_asserted_0(I,T):-type_deduced(I,T).
isa_asserted_0(I,T):-isa_from_morphology(I,T).
isa_asserted_0(I,T):-type_isa(I,T).
isa_asserted_0(I,T):-nonvar(T),type_prefix(_Prefix,T),atom(I),!,isa_from_morphology(I,T).
isa_asserted_0(I,T):-nonvar(T),append_term(T,I,HEAD),ruleHybridChain(HEAD,BODY),call_mpred_body(HEAD,BODY).
isa_asserted_0(I,T):-isa_asserted_ft(I,T).

isa_asserted_0(I,T):-fact_always_true(mudIsa(I,T)).

isa_asserted_0(I,T):- ((thlocal:useOnlyExternalDBs,!);thglobal:use_cyc_database),(kbp_t([mudIsa,I,T]);kbp_t([T,I])).
isa_asserted_0(I,T):- hasInstance(T,I).   %isa_asserted_0(I,T):-clause(hasInstance(T,I),true). %isa_asserted_0(I,T):-clause(isa(I,T),true).

isa_asserted_0(I,T):-nonvar(T),isa_asserted_1(I,T).

isa_asserted_1(I,T):-T\=mped_type(_),mpred_prop(I,T).
isa_asserted_1(I,'&'(T1 , T2)):-nonvar(T1),var(T2),!,dif:dif(T1,T2),isa_backchaing(I,T1),impliedSubClass(T1,T2),isa_backchaing(I,T2).
isa_asserted_1(I,'&'(T1 , T2)):-nonvar(T1),!,dif:dif(T1,T2),isa_backchaing(I,T1),isa_backchaing(I,T2).
isa_asserted_1(I,(T1 ; T2)):-nonvar(T1),!,dif:dif(T1,T2),isa_backchaing(I,T1),isa_backchaing(I,T2).
isa_asserted_1(I,tCol):-!,isa_w_type_atom(I,tCol).
isa_asserted_1(I,T):-atom(T),isa_w_type_atom(I,T).
isa_asserted_1(I,T):-nonvar(I),ttFormatType(T),term_is_ft(I,T).
% isa_asserted_1(I,T):- compound(I),functor(I,F,_),!,isa_backchaing_1(F,T).

isa_w_type_atom(I,T):- ttDeclarer(T),!,mpred_prop(I,T).
isa_w_type_atom(I,T):- clause(mpred_prop(I,T),true).
isa_w_type_atom(I,T):- G=..[T,I],once_if_ground(isa_atom_call(T,G),_).

dont_call_type_arity_one(tCol).
dont_call_type_arity_one(ttFormatType).
dont_call_type_arity_one(ttAgentType).
dont_call_type_arity_one(F):-mpred_prop(F,predStubType(prologHybrid)),!.

isa_atom_call(T,G):-loop_check(isa_atom_call_lc(T,G),fail).

isa_atom_call_lc(_,G):- predicate_property(G,builtin),!,G.
isa_atom_call_lc(Type,_):-dont_call_type_arity_one(Type),!,fail.
isa_atom_call_lc(_,G):- predicate_property(G,number_of_clauses(_)),!,clause(G,B),call_mpred_body(G,B).
isa_atom_call_lc(_,G):- predicate_property(G,number_of_rules(R)),R>0,!,G.

cached_isa(I,T):-hotrace(isa_backchaing(I,T)).


% ============================================
% decl_type/1
% ============================================
:- decl_mpred_hybrid tCol/1.
:- dynamic_multifile_exported never_type/1.
:- dynamic_multifile_exported decl_type/1.

never_type(iPlayer2).

tCol(vtDirection).
tCol(tCol).
tCol(tPred).
tCol(tFunction).
tCol(tRelation).
tCol(ttCreateable).
tCol(macroDeclarer).
% col(predArgTypes).
% TODO decide if OK
tCol(ArgsIsa):-ttDeclarer(ArgsIsa).
tCol(F):-hasInstance(macroDeclarer,F).
tCol(ttFormatType).
tCol(vtActionTemplate).
tCol(tRegion).
tCol(tContainer).

mpred_prop(dbase_t,prologOnly).
mpred_prop(mpred_prop,prologOnly).



:- swi_export(impliedSubClass/2).
impliedSubClass(T,ST):-ground(T:ST),is_known_false(mudSubclass(T,ST)),!,fail.
impliedSubClass(T,ST):-predicate_property(transitive_subclass(T,ST),_),!,call_tabled(transitive_subclass(T,ST)).

:- swi_export(asserted_subclass/2).
asserted_subclass(I,T):- ((thlocal:useOnlyExternalDBs,!);thglobal:use_cyc_database),(kbp_t([genls,I,T])).
asserted_subclass(T,ST):-dbase_t(mudSubclass,T,ST).


into_single_class(Var,VV):-var(Var),!, (nonvar(VV)->into_single_class(VV,Var);Var=VV).
into_single_class(A,B):- compound(B),!, (compound(A) -> (into_single_class(A,AB),into_single_class(B,AB)) ; into_single_class(B,A) ).
into_single_class('&'(A,Var),VV):-var(Var),!,into_single_class(A,VV).
into_single_class('&'(A,B),VV):-!, into_single_class((B),VV);into_single_class((A),VV).
into_single_class(A,A).

:- swi_export((transitive_subclass_or_same/2)).
transitive_subclass_or_same(A,B):- (var(A),var(B)),!,A=B.
transitive_subclass_or_same(A,A):-nonvar(A).
transitive_subclass_or_same(A,B):-transitive_subclass(A,B).

:- swi_export((transitive_subclass/2)).
transitive_subclass(_,T):-T==ttFormatType,!,fail.
transitive_subclass(A,_):-A==ttFormatType,!,fail.
transitive_subclass(A,T):- fail, bad_idea,!, into_single_class(A,AA), into_single_class(T,TT), fact_loop_checked(mudSubclass(A,T),transitive_P_l_r(dbase_t,mudSubclass,AA,TT)).
transitive_subclass(I,T):- fail,stack_check,((thlocal:useOnlyExternalDBs,!);thglobal:use_cyc_database),
   fact_loop_checked(mudSubclass(I,T),transitive_P_l_r(cyckb_t,genls,I,T)).
% transitive_subclass(A,T):- mudSubclass(A,T).
transitive_subclass(A,T):- fact_loop_checked(mudSubclass(A,T),transitive_P(dbase_t,mudSubclass,A,T)).

transitive_P(DB,P,L,R):-call(DB,P,L,R).
transitive_P(DB,P,L,R):-var(L),!,transitive_P_r_l(DB,P,L,R).
transitive_P(DB,P,L,R):-transitive_P_l_r(DB,P,L,R).

transitive_P_l_r(DB,P,L,R):-call(DB,P,L,A1),(call(DB,P,A1,R);call(DB,P,A1,A2),call(DB,P,A2,R)).
transitive_P_l_r(DB,P,L,R):-nonvar(R),call(DB,P,L,A1),call(DB,P,A1,A2),call(DB,P,A2,A3),call(DB,P,A3,R).
transitive_P_l_r(DB,P,L,R):-ground(L:R),call(DB,P,L,A1),call(DB,P,A1,A2),call(DB,P,A2,A3),call(DB,P,A3,A4),call(DB,P,A4,R).

transitive_P_r_l(DB,P,L,R):-nonvar(R),(call(DB,P,A1,R),(call(DB,P,L,A1);call(DB,P,A2,A1),call(DB,P,L,A2))).
transitive_P_r_l(DB,P,L,R):-nonvar(R),call(DB,P,A3,R),call(DB,P,A2,A3),call(DB,P,A1,A2),call(DB,P,L,A1).
transitive_P_r_l(DB,P,L,R):-ground(L:R),call(DB,P,A3,R),call(DB,P,A2,A3),call(DB,P,A1,A2),call(DB,P,A0,A1),call(DB,P,L,A0).


:-dynamic_multifile_exported(is_known_trew/1).
:-dynamic_multifile_exported(is_known_true/1).

is_known_true(C):-has_free_args(C),!,trace_or_throw(has_free_args(is_known_trew,C)).
is_known_true(F):-is_known_false0(F),!,fail.
is_known_true(mudIsa(X,tSpatialThing)):- (isa_asserted(X,_)),is_known_false0(mudIsa(X,tCol)),is_known_false0(mudIsa(X,ttFormatType)),is_known_false0(mudIsa(X,tPred)).
is_known_true(mudSubclass(X,X)).
is_known_true(mudIsa(_,ftID)).
is_known_true(mudIsa(apathFn(_,_),areaPath)).
is_known_true(mudIsa(apathFn(_,_),apathFn)).
is_known_true(mudIsa(_,ftTerm)).


is_known_trew(mudIsa(tContainer,ttCompleteExtentAsserted)).
% is_known_trew(isa(formattype,metaclass)).
is_known_trew(mudIsa(ttCompleteExtentAsserted,ttCompleteExtentAsserted)).
is_known_trew(mudIsa(tCol,ttCompleteExtentAsserted)).
is_known_trew(mudIsa(actGossup,tChannel)).
is_known_trew(mudSubclass(tRegion,tChannel)).
is_known_trew(mudSubclass(tAgentGeneric,tChannel)).
is_known_trew(mudIsa(tAgentGeneric,ttCreateable)).
is_known_trew(mudIsa(tRegion,ttCreateable)).
is_known_trew(mudIsa(prologSingleValued, ttCompleteExtentAsserted)).
is_known_trew(mudIsa(ttCreateable,ttCompleteExtentAsserted)).
is_known_trew(mudIsa(ttFormatType,ttCompleteExtentAsserted)).
is_known_trew(mudIsa(ftInt,ttNotCreatableType)).
is_known_trew(mudIsa(tCol,tCol)).
is_known_trew(mudIsa(prologSingleValued, tCol)).
is_known_trew(mudIsa(ttCompleteExtentAsserted, tCol)).
is_known_trew(mudSubclass(ttCompleteExtentAsserted, extentDecidable)).
is_known_trew(mudSubclass(prologSingleValued, extentDecidable)).
is_known_trew(mudSubclass('MaleAnimal',tAgentGeneric)).
is_known_trew(mudSubclass(ttFormatType,tCol)).
is_known_trew(mudIsa(tCol,ttNotCreatableType)).
is_known_trew(mudIsa(tItem,ttCreateable)).
is_known_trew(mudSubclass(tItem,ttCreateable)).
is_known_trew(mudIsa(ttFormatType,ttNotCreatableType)).
is_known_trew(mudSubclass(ttFormatType,ttNotCreatableType)).
is_known_trew(mudIsa(tTemporallyExistingThing, 'ttCreateable')).
is_known_trew(mudIsa(ftTerm,ttNotCreatableType)).
is_known_trew(mudSubclass(predArgTypes,tRelation)).
is_known_trew(mudSubclass(tFunction,tRelation)).


is_known_trew(mudSubclass(F,tPred)):-ttDeclarer(F).
% is_known_trew(mudSubclass(F,tFunction)):-ttDeclarer(F).
is_known_trew(mudSubclass(F,tRelation)):-ttDeclarer(F).
is_known_trew(disjointWith(A,B)):-disjointWithT(A,B).




:-dynamic_multifile_exported(is_known_false/1).
% :-dynamic(is_known_false/1).
is_known_false(C):-has_free_args(C),!,fail.
is_known_false(F):-is_known_trew(F),!,fail.
is_known_false(F):-is_known_false0(F),!.

:-dynamic_multifile_exported(is_known_false0/1).
is_known_false0(mudIsa(regioncol,ttFormatType)).
is_known_false0(mudIsa(ttFormatType,ttFormatType)).
is_known_false0(mudIsa(X,tSpatialThing)):- tCol(X);ttFormatType(X);tPred(X).
is_known_false0(mudIsa(ttCompleteExtentAsserted,ttCreateable)).
is_known_false0(mudIsa(X,Y)):-!,not_mud_isa(X,Y).
is_known_false0(mudSubclass(Type,_)):-arg(_,vv(tCol,tRelation,ttFormatType),Type).

:-dynamic_multifile_exported(not_mud_isa/2).
not_mud_isa(tAgentGeneric,ttFormatType).
not_mud_isa(tItem,ttFormatType).
not_mud_isa(tCol,ttFormatType).
not_mud_isa(tObj, ttCompleteExtentAsserted).
not_mud_isa(tObj, ttCreateable).
not_mud_isa(prologMacroHead, ttFormatType).
not_mud_isa(tObj, ttFormatType).
not_mud_isa(ttFormatType,ttFormatType).
not_mud_isa(mudSubft,tCol).
not_mud_isa(tTemporallyExistingThing, tTemporallyExistingThing).
not_mud_isa(ttCreateable,tTemporallyExistingThing).
not_mud_isa(Type,ttFormatType):- \+ (hasInstance(ttFormatType, Type)).
not_mud_isa(Type, prologMacroHead):- \+ (mpred_prop(Type, prologMacroHead)).
not_mud_isa(Type, ttCompleteExtentAsserted):- \+ (mpred_prop(Type, ttCompleteExtentAsserted)).
not_mud_isa(X,tCol):-never_type(X).


:-dynamic_multifile_exported(disjointWith/2).

:-swi_export(has_free_args/1).
has_free_args(C):- not(ground(C)), compound(C),not(not(arg(_,C,var))),!.

% is_known_false(subclass(A,B)):-disjointWith(A,B).

disjointWith0(tAgentGeneric,tItem).
disjointWith0(tRegion,tObj).
disjointWith0(ttFormatType,tItem).
disjointWith0(ttFormatType,tObj).
disjointWith0(ttFormatType,tRegion).
disjointWith0(ttCreateable,ttNotCreatableType).

disjointWithT(A,B):-disjointWith0(A,B).
disjointWithT(B,A):-disjointWith0(A,B).

disjointWith(A,B):- A=B,!,fail.
disjointWith(A,B):-disjointWithT(A,B).
disjointWith(A,B):-disjointWithT(AS,BS),transitive_subclass_or_same(A,AS),transitive_subclass_or_same(B,BS).
disjointWith(A,B):- once((type_isa(A,AT),type_isa(B,BT))),AT \= BT.

:- assert_hasInstance(ttFormatType,ftString).
:- assert_hasInstance(tCol,tContainer).

ttFormatType(I):- clause(mudFtInfo(I,_),true).
ttFormatType(I):- clause(mudSubft(I,_),true).

