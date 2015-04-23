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

:- include(logicmoo_i_header).

% ============================================
% inital hasInstance/2 database
% ============================================

user:hasInstance_dyn(tCol,tCol).
user:hasInstance_dyn(tCol,completelyAssertedCollection).
user:hasInstance_dyn(tCol,ttFormatType).
user:hasInstance_dyn(tCol,tSpatialThing).
user:hasInstance_dyn(tCol,tPred).
user:hasInstance_dyn(tCol,ftTerm).

user:hasInstance_dyn(completelyAssertedCollection,completelyAssertedCollection).
user:hasInstance_dyn(completelyAssertedCollection,prologSingleValued).
user:hasInstance_dyn(completelyAssertedCollection,tCol).
user:hasInstance_dyn(completelyAssertedCollection,ttFormatType).
user:hasInstance_dyn(completelyAssertedCollection,ttValueType).
user:hasInstance_dyn(completelyAssertedCollection,ttSpatialType).
user:hasInstance_dyn(completelyAssertedCollection,tRelation).
user:hasInstance_dyn(completelyAssertedCollection,tPred).
user:hasInstance_dyn(completelyAssertedCollection,defnSufficient).
user:hasInstance_dyn(completelyAssertedCollection,genlPreds).

user:hasInstance_dyn(ttNotSpatialType,ftInt).
%user:hasInstance_dyn(ttNotSpatialType,ftTerm).
user:hasInstance_dyn(ttNotSpatialType,tCol).
user:hasInstance_dyn(ttNotSpatialType,ttFormatType).
user:hasInstance_dyn(ttNotSpatialType,ttValueType).

user:hasInstance_dyn(ttSpatialType,tAgentGeneric).
user:hasInstance_dyn(ttSpatialType,tItem).
user:hasInstance_dyn(ttSpatialType,tObj).
user:hasInstance_dyn(ttSpatialType,tRegion).
user:hasInstance_dyn(ttSpatialType,tSpatialThing).

user:hasInstance_dyn(W,SS):-nonvar(W),nonvar(SS),SS=isKappaFn(_,S),nonvar(S),!.


hasInstance(T,I):- user:hasInstance_dyn(T,I).
hasInstance(T,I):- thglobal:pfcManageHybrids,clause_safe(isa(I,T),true).

/*
disabled hasInstance(T,I):- not(current_predicate(deduce_M/1)),!,user:hasInstance_dyn(T,I).
disabled hasInstance(T,I):- !, (mudIsa_motel(I,T) *-> true ; (((atom(I),must(not(user:hasInstance_dyn(T,I)))),fail))).
disabled hasInstance(T,I):- rdf_x(I,rdf:type,T).
*/

assert_hasInstance(T,I):- sanity(ground(T:I)),hasInstance(T,I),!.
assert_hasInstance(T,I):- assert_if_new(user:hasInstance_dyn(T,I)),!,expire_tabled_list(all).

% ========================================
% is_typef(F).
% Checks F for isa(F,tCol).
% ========================================
:-export(is_typef/1).
is_typef(C):-var(C),!,fail.
is_typef(prologSingleValued):-!.
is_typef(F):- isa_backchaing(F,tCol),!.
is_typef(F):- (hasInstance(functorDeclares,F);hasInstance(tCol,F);clause(asserted_mpred_prop(F,tCol),true)),!.
is_typef(F):- atom(F),current_predicate(isa_from_morphology/2),isa_from_morphology(F,TT),!,atom_concat(_,'Type',TT).

% ========================================
% is_never_type(F).
% Checks F for not_mudIsa(F,tCol).
% ========================================
:-export is_never_type/1.

is_never_type(V):-var(V),!,fail.
is_never_type(V):-never_type_why(V,_),!.


never_type_f(Var):-is_ftVar(Var),!,trace_or_throw(var_never_type(Var)).
never_type_f(F):-hasInstance(T,F),T==tCol,!,fail.
never_type_f(_:W):-!,never_type_f(W).
never_type_f(':-').
never_type_f('include').
never_type_f('tCol'):-!,fail.
never_type_f('onSpawn').
never_type_f('ensure_loaded').
never_type_f('declare_load_dbase').
never_type_f('user:ensure_loaded').
never_type_f('meta_predicate').
never_type_f('Area1000').
never_type_f(iPlayer2).
never_type_f(genls).
never_type_f(must).
never_type_f(mpred_prop).
never_type_f(defnSufficient).
never_type_f(backchainForbidden).

noncol_type('LogicalConnective').


never_type_why(V,ftVar(isSelf)):-is_ftVar(V),!.
never_type_why(mpred_call,mpred_call(isSelf)):-!.
never_type_why(C,_):-hasInstance(tCol,C),!,fail. % already declared to be a type
never_type_why(C,T):- noncol_type(T),hasInstance(T,C),!.
never_type_why(F,Why):-decided_not_was_isa(F,W),!,Why=(decided_not_was_isa(F,W)).
never_type_why(F,_):-asserted_mpred_prop(F,tCol),!,fail.
%never_type_why(C):- compound(C),functor(C,F,1),isa_asserted(F,tCol).
never_type_why(F,Why):-atom(F),functor(G,F,1),real_builtin_predicate(G),!,Why=(whynot( real_builtin_predicate(G) )).
never_type_why(M:C,Why):-atomic(M),!,never_type_why(C,Why).
% never_type_why(F):-dmsg(never_type_why(F)),!,asserta_if_new(asserted_mpred_prop(F,prologOnly)).
never_type_why(F,Why):-never_type_f(F),Why=is_never_type(F).
never_type_why(F,Why):- atom(F), arity(F,A),!,F\==isa, asserted_mpred_prop(F,_), A > 1,Why=(whynot( arity(F,A) )).


% ========================================
% isa_from_morphology(F).
% Checks F's name for isa(F,*).
% ========================================

isa_from_morphology(Inst,Type):-atom(Inst),type_suffix(Suffix,Type),atom_concat(Base,Suffix,Inst),!,atom_length(Base,BL),BL>2.
isa_from_morphology(Inst,Type):-atom(Inst),type_prefix(Prefix,Type),atom_concat(Prefix,Other,Inst),capitalized(Other),!.

type_suffix('Fn',ftFunctional).
type_suffix('Type',ttTypeType).
type_suffix('Able',ttTypeByAction).
type_suffix('able',ttTypeByAction).


type_prefix(vt,ttValueType).
type_prefix(tt,ttTypeType).
type_prefix(t,tCol).
type_prefix(v,ftValue).
type_prefix(i,ftID).
type_prefix(pred,tPred).
type_prefix(act,ftAction).

type_prefix(format,tPred).
type_prefix(is,ftSyntaxOperator).
type_prefix(dcg,ftSyntaxOperator).
type_prefix(dcg,ftTextType).
type_prefix(txt,ftTextType).
type_prefix(sk,ftSkolemFunction).
type_prefix(is,tFunction).
type_prefix(a,tFunction).
type_prefix(fn,tFunction).
type_prefix(mud,tPred).
type_prefix(prop,tPred).
type_prefix(prolog,ttPredType).
type_prefix(ft,ttFormatType).
type_prefix(pred,ttPredType).
type_prefix(macro,ttMacroType).

:-call_after(mpred_module_ready,((forall(type_prefix(_,T),add(isa(T,tCol)))))).
:-call_after(mpred_module_ready,fmt(passed_mpred_module_ready)).

% ========================================
% was_isa(Goal,I,C) recognises isa/2 and its many alternative forms
% ========================================
:- dynamic decided_not_was_isa/2.
:-export(was_isa/3).
was_isa(G,I,C):-compound(G),functor(G,F,_),notrace(((not(decided_not_was_isa(F,_)),once(was_isa0(G,I,C)-> true;((functor(G,F,1),get_when(When),asserta_if_new(decided_not_was_isa(F,When)),!,fail)))))).

% get_when(When)
get_when(F:L):-source_location(F,L),!.
get_when(F:L):-source_location(file,F),current_input(S),line_position(S,L),!.
get_when(When):-current_input(S),findall(NV,stream_property(S,NV),When),!.
get_when(M):-context_module(M),!.
get_when(user):-!.


was_isa0('$VAR'(_),_,_):-!,fail.
was_isa0(isa(I,C),I,C):-!.
was_isa0(is_typef(_),_,_):-!,fail.
was_isa0(notrace(_),_,_):-!,fail.
was_isa0(call(_),_,_):-!,fail.
was_isa0(trace(_),_,_):-!,fail.
was_isa0(not(_),_,_):-!,fail.
% was_isa0(hasInstance(tCol,I),I,tCol).
was_isa0(ttNotSpatialType(I),I,ttNotSpatialType).
was_isa0(tChannel(I),I,tChannel).
was_isa0(tAgentGeneric(I),I,tAgentGeneric).
was_isa0(t(C,_),_,_):-never_type_why(C,_),!,fail.
was_isa0(t(C,I),I,C).
was_isa0(t(P,I,C),I,C):-!,P==isa.
was_isa0(isa(I,C),I,C).
was_isa0(M:G,I,C):-atom(M),!,was_isa0(G,I,C).
was_isa0(G,I,C):-G=..[C,I],!,is_typef(C),!,not(is_never_type(C)).
% was_isa0(hasInstance(C,I),I,C).

not_ft(T):-nonvar(T),not_ft_quick(T),not(hasInstance(ttFormatType,T)).

not_ft_quick(T):-nonvar(T),(T=tItem;T=tRegion;T=tCol;T=completelyAssertedCollection;transitive_subclass_or_same(T,tSpatialThing)).

:-export(asserted_subclass/2).
asserted_subclass(I,T):- ((thlocal:useOnlyExternalDBs,!);thglobal:use_cyc_database),(kbp_t([genls,I,T])).
asserted_subclass(T,ST):-t(genls,T,ST).

chk_ft(T):- not_ft_quick(T),!,fail.
%chk_ft(I):- thlocal:infForward, t(defnSufficient,I,_),!.
%chk_ft(I):- thlocal:infForward, asserted_subclass(I,FT),I\=FT,chk_ft(FT),!.
chk_ft(I):- thlocal:infForward, !,hasInstance(ttFormatType,I).



into_single_class(Var,VV):-var(Var),!, (nonvar(VV)->into_single_class(VV,Var);Var=VV).
into_single_class(A,B):- compound(B),!, (compound(A) -> (into_single_class(A,AB),into_single_class(B,AB)) ; into_single_class(B,A) ).
into_single_class('&'(A,Var),VV):-var(Var),!,into_single_class(A,VV).
into_single_class('&'(A,B),VV):-!, into_single_class((B),VV);into_single_class((A),VV).
into_single_class(A,A).

% ==========================
% taxonomicPair(isa,genls)
% ==========================

:-export((transitive_subclass_or_same/2)).
transitive_subclass_or_same(A,B):- (var(A),var(B)),!,A=B.
transitive_subclass_or_same(A,A):-nonvar(A).
transitive_subclass_or_same(A,B):-genls(A,B).

transitive_P(DB,P,L,R):-call(DB,P,L,R).
transitive_P(DB,P,L,R):-var(L),!,transitive_P_r_l(DB,P,L,R).
transitive_P(DB,P,L,R):-transitive_P_l_r(DB,P,L,R).

transitive_P_l_r(DB,P,L,R):-call(DB,P,L,A1),(call(DB,P,A1,R);call(DB,P,A1,A2),call(DB,P,A2,R)).
transitive_P_l_r(DB,P,L,R):-nonvar(R),call(DB,P,L,A1),call(DB,P,A1,A2),call(DB,P,A2,A3),call(DB,P,A3,R).
transitive_P_l_r(DB,P,L,R):-ground(L:R),call(DB,P,L,A1),call(DB,P,A1,A2),call(DB,P,A2,A3),call(DB,P,A3,A4),call(DB,P,A4,R).

transitive_P_r_l(DB,P,L,R):-nonvar(R),(call(DB,P,A1,R),(call(DB,P,L,A1);call(DB,P,A2,A1),call(DB,P,L,A2))).
transitive_P_r_l(DB,P,L,R):-nonvar(R),call(DB,P,A3,R),call(DB,P,A2,A3),call(DB,P,A1,A2),call(DB,P,L,A1).
transitive_P_r_l(DB,P,L,R):-ground(L:R),call(DB,P,A3,R),call(DB,P,A2,A3),call(DB,P,A1,A2),call(DB,P,A0,A1),call(DB,P,L,A0).


is_known_true(C):-has_free_args(C),!,trace_or_throw(has_free_args(is_known_trew,C)).
is_known_true(F):-is_known_false0(F),!,fail.
is_known_true(F):-is_known_trew(F),!.
%is_known_true(isa(G,tSpatialThing)):- hasInstance(_,G),not_mud_isa(G,tCol),not_mud_isa(G,tPred).
is_known_true(genls(G,G)).
is_known_true(isa(apathFn(_,_),tPathway)).
is_known_true(isa(_,ftTerm)).
is_known_true(isa(_,ftID)).


:-export(is_known_trew/1).
is_known_trew(genls(tRegion,tChannel)).
is_known_trew(genls('MaleAnimal',tAgentGeneric)).
is_known_trew(genls(prologSingleValued, extentDecidable)).
is_known_trew(genls(tAgentGeneric,tChannel)).
is_known_trew(genls(completelyAssertedCollection, extentDecidable)).
is_known_trew(genls(ttFormatType,tCol)).
is_known_trew(genls(ttFormatType,ttNotSpatialType)).
is_known_trew(genls(pred_argtypes,tRelation)).
is_known_trew(genls(tFunction,tRelation)).
is_known_trew(genls(F,tPred)):-is_pred_declarer(F).
is_known_trew(disjointWith(A,B)):-disjointWithT(A,B).

pfcNeverTrue(P):-is_known_false(P).

is_known_false(C):-has_free_args(C),!,fail.
is_known_false(F):-is_known_trew(F),!,fail.
is_known_false(F):-is_known_false0(F),!.

:-export(is_known_false0/1).
is_known_false0(isa(G,Why)):-!,catch(not_mud_isa(G,Why),_,fail).
% is_known_false0(genls(Type,_)):-arg(_,vv(tCol,tRelation,ttFormatType),Type).


:-export(has_free_args/1).
has_free_args(C):- not(ground(C)), compound(C),not(not(arg(_,C,var))),!.

% is_known_false(genls(A,B)):-disjointWith(A,B).

disjointWith0(tAgentGeneric,tItem).
disjointWith0(tRegion,tObj).
disjointWith0(ttFormatType,tItem).
disjointWith0(ttFormatType,tObj).
disjointWith0(ttFormatType,tRegion).
disjointWith0(ttSpatialType,ttNotSpatialType).

disjointWithT(A,B):-disjointWith0(A,B).
disjointWithT(B,A):-disjointWith0(A,B).

:-export(not_mud_isa/3).
%not_mud_isa0(tObj, completelyAssertedCollection).
%not_mud_isa0(tObj, ttSpatialType).
%not_mud_isa0(tSpatialThing,ttSpatialType).
not_mud_isa0(I,T):-(var(I);var(T)),trace_or_throw(var_not_mud_isa(I,T)).
not_mud_isa0(actGossup,tChannel).
not_mud_isa0(_, blah):-!.
not_mud_isa0(I,pred_argtypes):-!,not(compound(I)).
not_mud_isa0(I,ttFormatted):-!,not(compound(I)).
not_mud_isa0(_,prologHybrid):-!,fail.
not_mud_isa0(prologMacroHead, ttFormatType).
not_mud_isa0(tAgentGeneric,ttFormatType).
not_mud_isa0(tCol,ttFormatType).
not_mud_isa0(tItem,ttFormatType).
not_mud_isa0(tObj, ttFormatType).
not_mud_isa0(tSpatialThing, tSpatialThing).
not_mud_isa0(completelyAssertedCollection,ttSpatialType).
not_mud_isa0(ttFormatType,ttFormatType).
not_mud_isa0(ttSpatialType,tSpatialThing).
%not_mud_isa0(I,tCol):- compound(I),!, \+ hasInstance(tCol,I).
%not_mud_isa0(I,C):- compound(I),!, \+ hasInstance(C,I).
not_mud_isa(I,C):-not_mud_isa(I,C,_).

not_mud_isa(I,C,Why):-not_mud_isa0(I,C),Why=not_mud_isa0(I,C).
not_mud_isa(G,tSpatialThing,Why):- ((hasInstance(tCol,G),Why=hasInstance(tCol,G));(tPred(G),Why=tPred(G))).
not_mud_isa(G,tCol,Why):-never_type_why(G,Why).

% ==========================
% isa_backchaing(i,c)
% ==========================
:-export(isa_backchaing/2).
isa_backchaing(I,T):- (atom(I);var(I)),!, no_repeats(isa_backchaing_0(I,T)).

:-export(isa_backchaing_0/2).
isa_backchaing_0(I,T):-  T==ftVar,!,is_ftVar(I).
isa_backchaing_0(I,T):-  var(I),nonvar(T),!,no_repeats_old(transitive_subclass_or_same(AT,T)),isa_asserted(I,AT).
isa_backchaing_0(I,T):-  var(I),   var(T),!,setof(TT,AT^(isa_asserted(I,AT),transitive_subclass_or_same(AT,TT)),List),!,member(T,List).
isa_backchaing_0(I,T):-  nonvar(T),!,isa_backchaing_i_i(I,T),!.
isa_backchaing_0(I,T):-  isa_asserted(I,AT),transitive_subclass_or_same(AT,T).

isa_backchaing_i_i(I,T):- not_mud_isa(I,T),!,fail.
isa_backchaing_i_i(I,T):-isa_asserted(I,T),!.
isa_backchaing_i_i(I,T):-not(hasInstance(completelyAssertedCollection,T)),!,no_repeats_old(transitive_subclass_or_same(AT,T)),isa_asserted(I,AT).


% ============================================
% isa_asserted/1
% ============================================

:-export(type_isa/2).

type_isa(Type,ttSpatialType):-arg(_,vv(tAgentGeneric,tItem,tObj,tRegion),Type),!.
type_isa(ArgIsa,ttPredType):-is_pred_declarer(ArgIsa),!.
type_isa(ftString,ttFormatType):-!.
type_isa(Type,ttFormatType):-chk_ft(Type),!. % text
%  from name


atom_prefix_other(Inst,Prefix,Other):-atom_type_prefix_other(Inst,_,Prefix,Other).
atom_type_prefix_other(Inst,Type,Prefix,Other):-atom(Inst),type_prefix(Prefix,Type),atom_concat(Prefix,Other,Inst),capitalized(Other).
atom_type_prefix_other(Inst,Type,Suffix,Other):-atom(Inst),type_suffix(Suffix,Type),atom_concat(Other,Suffix,Inst),!.

user:mpred_prop(F,tCol):-isa_from_morphology(F,Col),atom_concat(_,'Type',Col),arity(F,1).


onLoadPfcRule('=>'(hasInstance(tCol,Inst), {isa_from_morphology(Inst,Type)} , isa(Inst,Type))).


callOr(Pred,I,T):-(call(Pred,I);call(Pred,T)),!.

% type_deduced(I,T):-atom(T),i_name(mud,T,P),!,clause(t(P,_,I),true).
type_deduced(I,T):-nonvar(I),not(number(I)),clause(t(P,_,I),true),(argIsa_known(P,2,AT)->T=AT;i_name(vt,P,T)).

compound_isa(F,_,T):- mpred_call(resultIsa(F,T)).
compound_isa(_,I,T):- mpred_call(formatted_resultIsa(I,T)).

isa_asserted([I],T):-nonvar(I),!,hasInstance(T,I).
isa_asserted(I,T):-hasInstance(T,I).
isa_asserted(isInstFn(I),T):-nonvar(I),!,T=I.
isa_asserted(I,T):-nonvar(I),nonvar(T),not_mud_isa(I,T),!,fail.
isa_asserted(I,T):-is_known_trew(isa(I,T)).
isa_asserted(I,T):-compound(I),!,get_functor(I,F),compound_isa(F,I,T).
isa_asserted(I,T):-atom(I),isa_from_morphology(I,T).
isa_asserted(I,T):-(atom(I);atom(T)),type_isa(I,T).
isa_asserted(I,T):- ((thlocal:useOnlyExternalDBs,!);thglobal:use_cyc_database),(kbp_t([isa,I,T]);kbp_t([T,I])).
isa_asserted(I,T):- var(I),member(T,[ftVar,ftProlog]).
isa_asserted(I,T):- asserted_mpred_prop(I,T).
isa_asserted(I,T):- nonvar(I),/*not(not_ft(T)),*/(  ((var(T);chk_ft(T)),term_is_ft(I,T))*->true;type_deduced(I,T) ).
% isa_asserted(I,T):- HEAD= isa(I, T),ruleBackward(HEAD,BODY),call_mpred_body(HEAD,BODY).
isa_asserted(I,T):- nonvar(T),isa_asserted_1(I,T).

% isa_asserted_1(I,T):- T\=predStub(_),asserted_mpred_prop(I,T).
isa_asserted_1(I,T):- atom(T),isa_w_type_atom(I,T).
isa_asserted_1(_,T):- hasInstance(completelyAssertedCollection,T),!,fail.
%isa_asserted_1(I,T):- append_term(T,I,HEAD),ruleBackward(HEAD,BODY),call_mpred_body(HEAD,BODY).
isa_asserted_1(I,'&'(T1 , T2)):-!,nonvar(T1),var(T2),!,dif:dif(T1,T2),isa_backchaing(I,T1),subclass(T1,T2),isa_backchaing(I,T2).
isa_asserted_1(I,'&'(T1 , T2)):-!,nonvar(T1),!,dif:dif(T1,T2),isa_backchaing(I,T1),isa_backchaing(I,T2).
isa_asserted_1(I,(T1 ; T2)):-!,nonvar(T1),!,dif:dif(T1,T2),isa_backchaing(I,T1),isa_backchaing(I,T2).

isa_w_type_atom(I,T):- is_pred_declarer(T),!,asserted_mpred_prop(I,T).
isa_w_type_atom(_,T):- dont_call_type_arity_one(T),!,fail.
isa_w_type_atom(I,T):- G=..[T,I],once_if_ground(isa_atom_call(T,G),_).

dont_call_type_arity_one(tCol).
dont_call_type_arity_one(ttFormatType).
dont_call_type_arity_one(ttAgentType).
dont_call_type_arity_one(F):-asserted_mpred_prop(F,prologHybrid),!.

isa_atom_call(T,G):-fail,loop_check(isa_atom_call_ilc(T,G)).

isa_atom_call_ilc(_,G):- real_builtin_predicate(G),!,G.
%isa_atom_call_ilc(_,G):- predicate_property(G,number_of_clauses(_)),!,clause(G,B),call_mpred_body(G,B).
isa_atom_call_ilc(_,G):- predicate_property(G,number_of_rules(R)),R>0,!,G.


cached_isa(I,T):-hotrace(isa_backchaing(I,T)).

% '$toplevel':isa(I,C):-isa_backchaing(I,C).




% ============================================
% decl_type/1
% ============================================
:-export(decl_type_safe/1).
decl_type_safe(T):- compound(T),!.
decl_type_safe(T):- ignore((atom(T),not(never_type_why(T,_)),not(number(T)),decl_type(T))).


:-export decl_type/1.
decl_type(Var):- var(Var),!,trace_or_throw(var_decl_type(Var)).
decl_type(Spec):- never_type_why(Spec,Why),!,trace_or_throw(never_type_why(Spec,Why)).
decl_type(M:F):-!, '@'(decl_type(F), M).
decl_type([]):-!.
decl_type([A]):-!,decl_type(A).
decl_type([A|L]):-!,decl_type(A),decl_type(L).
decl_type((A,L)):-!,decl_type(A),decl_type(L).


decl_type(Spec):- compound(Spec),must_det(define_compound_isa(Spec,tCol)),!.
decl_type(Spec):- decl_type_unsafe(Spec),!.

decl_type_unsafe(Spec):- never_type_why(Spec,Why),!,trace_or_throw(never_type_why(Spec,Why)).
decl_type_unsafe(Spec):- !, must((with_assertions([thlocal:infSkipArgIsa,thlocal:infSkipFullExpand],(add(isa(Spec,tCol)),add(arity(Spec,1)))))).

decl_type_unsafe(Spec):-
 with_assertions([thlocal:infSkipArgIsa,thlocal:infSkipFullExpand], 
   ((hooked_asserta(isa(Spec,tCol)),assert_hasInstance(tCol,Spec),hooked_asserta(user:mpred_prop(Spec,tCol)),decl_mpred_hybrid(Spec/1)))),!.


define_compound_isa(Spec,T):- get_functor(Spec,F),define_compound_isa(F,Spec,T).
define_compound_isa(F,Spec,T):- add(formatted_resultIsa(Spec,T)),add(resultIsa(F,T)).


% ========================================
% is_typef(F).
% Checks F for isa(F,tCol).
% ========================================


:-export(define_ft/1).
% define_ft(ftListFn(Spec)):- nonvar(Spec),never_type_why(Spec,Why),!,trace_or_throw(never_ft(ftListFn(Spec),Why)).
define_ft(ftListFn(_)):-!.
define_ft(Spec):- never_type_why(Spec,Why),!,trace_or_throw(never_ft(never_type_why(Spec,Why))).
define_ft(M:F):- !, '@'(define_ft(F), M).
define_ft(Spec):- loop_check(define_ft_0(Spec),true).



define_ft_0(xyzFn):-!.
define_ft_0(Spec):- hasInstance(ttFormatType,Spec),!.
define_ft_0(Spec):- hasInstance(tCol,Spec),dmsg(once(maybe_coierting_plain_type_to_formattype(Spec))),fail.
define_ft_0(Spec):- hooked_asserta(isa(Spec,ttFormatType)),(compound(Spec)->hooked_asserta(isa(Spec,ttFormatted));true).

:-export(assert_subclass/2).
assert_subclass(O,T):-assert_subclass_safe(O,T).

:-export(assert_subclass_safe/2).
assert_subclass_safe(O,T):-
  ignore((nonvar(O),decl_type_safe(O),nonvar(T),decl_type_safe(T),nonvar(O),nop((not(chk_ft(O)),not(chk_ft(T)))),add(genls(O,T)))).

:-export(assert_isa_safe/2).
assert_isa_safe(O,T):- ignore((nonvar(O),nonvar(T),decl_type_safe(T),assert_isa(O,T))).

%OLD user:decl_database_hook(change(assert,_A_or_Z),genls(S,C)):-decl_type_safe(S),decl_type_safe(C).


%OLD user:decl_database_hook(change(assert,_A_or_Z),isa(W,ttSpatialType)):-decl_type_safe(W). %,call_after_mpred_load(forall(isa(I,W),create_instance(I,W))).
%OLD user:decl_database_hook(change(assert,_A_or_Z),isa(W,tCol)):- (test_tl(infSupertypeName);true),guess_supertypes(W).

guess_supertypes(W):-atom(W),atomic_list_concat(List,'_',W),length(List,S),S>2,!, append(FirstPart,[Last],List),atom_length(Last,AL),AL>3,not(member(flagged,FirstPart)),
            atomic_list_concat(FirstPart,'_',_NewCol),show_call_failure(assert_subclass_safe(W,Last)).
guess_supertypes(W):-atom(W),to_first_break(W,lower,tt,_,upper),!,assert_isa(W,ttTypeType),!.
guess_supertypes(W):-atom(W),T=t,to_first_break(W,lower,T,All,upper),!,to_first_break(All,upper,_,Super,Rest),
   atom_length(Rest,L),!,L>2,i_name(T,Rest,Super),show_call_failure(assert_subclass_safe(W,Super)),!.

/*
system:term_expansion(isa(Compound,PredArgTypes),
  (:-dmsg(pfc_add(wizza(Compound,PredArgTypes))))):-
  user:isa_pred_now_locked,
   ground(Compound:PredArgTypes),show_call(pfc_add(isa(Compound,PredArgTypes))),!.
*/

isa_provide_mpred_storage_op(_,_):-!,fail.
% ISA MODIFY
isa_provide_mpred_storage_op(change(assert,_),G):- was_isa(G,I,C),!,dmsg(assert_isa_from_op(I,C)),!, assert_isa(I,C).
% ISA MODIFY
isa_provide_mpred_storage_op(change(retract,How),G):- was_isa(G,I,C),!,show_call((with_assert_op_override(change(retract,How),((dmsg(retract_isa(G,I,C)),!, assert_isa(I,C)))))).
% ISA CALL
isa_provide_mpred_storage_op(call(_),G):- was_isa(G,I,C),!, (isa_backchaing(I,C);hasInstance(C,I)).
% ISA CLAUSES
isa_provide_mpred_storage_clauses(isa(I,C),true,hasInstanceIC):-hasInstance(C,I).
isa_provide_mpred_storage_clauses(H,true,hasInstanceCI):-
   (compound(H)-> 
      ((functor(H,C,1)-> H=..[C,I]; H=isa(I,C)), hasInstance(C,I)) ; 
      (hasInstance(C,I),(nonvar(C)->append_term(C,I,H);H=isa(I,C)))).

%isa_provide_mpred_storage_clauses(isa(I,C),B,W):-mpred_t_mpred_storage_clauses_rules(isa(I,C),B,W).
%isa_provide_mpred_storage_clauses(isa(I,C),B,W):-nonvar(C),append_term(C,I,H),mpred_t_mpred_storage_clauses_rules(H,B,W).


user:provide_mpred_storage_clauses(H,B,(What)):-isa_provide_mpred_storage_clauses(H,B,What).

% isa_backchaing(I,T):- stack_depth(Level),Level>650,trace_or_throw(skip_dmsg_nope(failing_stack_overflow(isa_backchaing(I,T)))),!,fail.


% ================================================
% assert_isa/2
% ================================================
:-meta_predicate(assert_isa(+,+)).

assert_isa([I],T):-nonvar(I),!,assert_isa(I,T).
assert_isa(I,T):-assert_isa_i(I,T),sanity(show_call_failure(is_asserted(isa(I,T)));show_call_failure(isa_asserted(I,T))).

assert_isa_i(I,T):- sanity(not(singletons_throw_else_fail(assert_isa(I,T)))),fail.
assert_isa_i(_,ftTerm):-!.
assert_isa_i(_,ftTerm(_)):-!.
assert_isa_i(I,T):-not_mud_isa(I,T,Why),!,throw(Why).
assert_isa_i(I,PT):- nonvar(PT),is_pred_declarer(PT),!,decl_mpred(I,PT),assert_hasInstance(PT,I).
assert_isa_i(I,T):- skipped_table_call(loop_check(assert_isa_ilc(I,T),loop_check(assert_hasInstance(T,I),dtrace(assert_isa_ilc(I,T))))).

:-export(assert_isa_ilc/2).
% skip formatter cols
assert_isa_ilc(isKappaFn(_,_),_):-!.
assert_isa_ilc(_I,T):- member(T,[ftString]),!.
assert_isa_ilc(I,T):- is_list(I),!,maplist(assert_isa_reversed(T),I).
assert_isa_ilc(I,T):- not(not(hasInstance(T,I))),!.
assert_isa_ilc(I,T):- hotrace(chk_ft(T)),(compound(I)->dmsg(once(dont_assert_c_is_ft(I,T)));dmsg(once(dont_assert_is_ft(I,T)))),rtrace((chk_ft(T))).
assert_isa_ilc(I,T):- once(decl_type(T)),
  skipped_table_call(must((assert_isa_ilc_unchecked(I,T),show_call_failure(isa_backchaing(I,T))))),
  assert_hasInstance(T,I).

assert_isa_ilc_unchecked(I,T):- compound(I),!,must((get_functor(I,F),assert_compound_isa(I,T,F))),!.
assert_isa_ilc_unchecked(I,tCol):- must(show_call(decl_type(I))).
assert_isa_ilc_unchecked(I,ttFormatType):- must(show_call(define_ft(I))).
assert_isa_ilc_unchecked(I,_):- not(asserted_mpred_prop(I,_)),not(hasInstance(tCol,I)),show_call_failure(assert_if_new(user:hasInstance_dyn(tCountable,I))),fail.
assert_isa_ilc_unchecked(I,T):-
  with_assertions([thlocal:infSkipArgIsa,thlocal:infSkipFullExpand],((  hooked_asserta(isa(I,T)),assert_hasInstance(T,I)))).

assert_compound_isa(I,_,_):- compound(I), I\=resultIsaFn(_),glean_pred_props_maybe(I),fail.
assert_compound_isa(I,T,_):- hotrace(chk_ft(T)),dmsg(once(dont_assert_is_ft(I,T))),rtrace((chk_ft(T))).
%assert_compound_isa(I,T,F):- is_Template(I),!,assert_hasInstance(T,I),show_call(add(resultIsa(I,T))),assert_hasInstance(T,resultIsaFn(F)).
assert_compound_isa(I,T,F):- ignore((is_Template(I),with_assertions(infConfidence(vWeak),assert_predArgTypes(I)))),
   hooked_asserta(isa(I,T)),assert_hasInstance(T,I),show_call(add(resultIsa(F,T))),assert_hasInstance(T,resultIsaFn(F)),!.
   

get_mpred_arg(N,_:C,E):-!,compound(C),arg(N,C,E).
get_mpred_arg(N,C,E):-!,compound(C),arg(N,C,E).

is_Template(I):- get_mpred_arg(_,I,Arg1),hasInstance(tCol,Arg1).


assert_isa_reversed(T,I):-assert_isa(I,T).

% ================================================
% assert_isa HOOKS
% ================================================
%OLD user:decl_database_hook(_,genls(_,_)):-retractall(t(_,isa,_,_)),retractall(t(_,genls,_,_)).
%OLD user:decl_database_hook(change(assert,_),DATA):-into_mpred_form(DATA,O),!,O=isa(I,T),hotrace(doall(assert_isa_hooked(I,T))).
%OLD user:decl_database_hook(change(assert,_),isa(I,T)):- assert_hasInstance(T,I),fail.

%OLD user:decl_database_hook(change( retract,_),isa(I,T)):-doall(db_retract_isa_hooked(I,T)).

assert_isa_hooked(A,_):-retractall(t(cache_I_L,isa,A,_)),fail.
assert_isa_hooked(F,T):- is_pred_declarer(T),decl_mpred(F,T),fail.
assert_isa_hooked(I,T):- assert_isa(I,T).
assert_isa_hooked(I,T):- not(ground(assert_isa(T))),!, trace_or_throw(not(ground(assert_isa(I,T)))).
assert_isa_hooked(I,T):- assert_hasInstance(T,I),fail.

assert_isa_hooked(T,tCol):-!,decl_type(T),!.
assert_isa_hooked(T,ttFormatType):-!,define_ft(T),!.
assert_isa_hooked(Term,tPred):-!,decl_mpred(Term).
assert_isa_hooked(Term,prologHybrid):-!,decl_mpred_hybrid(Term).
assert_isa_hooked(Term,prologOnly):-!,export(Term).
assert_isa_hooked(Term,prologPTTP):-!,decl_mpred_hybrid(Term,prologPTTP).
assert_isa_hooked(Term,prologSNARK):-!,decl_mpred_hybrid(Term,prologSNARK).
assert_isa_hooked(I,_):- I\=prologHybrid(_),glean_pred_props_maybe(I),fail.
assert_isa_hooked(food5,tWeapon):-trace_or_throw(assert_isa(food5,tWeapon)).

% assert_isa_hooked(I,T):- motel:defconcept(I,isAnd([lexicon,T])).
% assert_isa_hooked(I,T):- motel:defprimconcept(I,T).
% assert_isa_hooked(I,T):-dmsg((told(assert_isa(I,T)))).


%OLD user:decl_database_hook(change(assert,_),isa(I,T)):- doall(assert_isa_hooked_after(I,T)).

assert_isa_hooked_after(F,T):-is_pred_declarer(T),!,decl_mpred(F,T).
assert_isa_hooked_after(_,tCol):-!.
assert_isa_hooked_after(_,ttFormatType):-!.
%assert_isa_hooked_after(I,T):- ttSpatialType(T),!,assert_isa_hooked_creation(I,T).

%assert_isa_hooked_after(I,T):- assert_isa_hooked_creation(I,T).

/*
assert_isa_hooked_after(I,T):- not(completelyAssertedCollection(T)),impliedSubClass(T,ST),completelyAssertedCollection(ST),assert_isa(I,ST).
:-export(impliedSubClass/2).
impliedSubClass(T,ST):-ground(T:ST),is_known_false(genls(T,ST)),!,fail.
impliedSubClass(T,ST):-predicate_property(transitive_subclass(T,ST),_),!,call_tabled(transitive_subclass(T,ST)).
*/

% one of 4 special cols
% assert_isa_hooked_creation(I,T):- ttSpatialType(T),!,call_after_mpred_load((create_instance(I,T,[]))).
% sublass of 4 special cols
% assert_isa_hooked_creation(I,T):- doall((ttSpatialType(ST),impliedSubClass(T,ST),call_after_mpred_load((create_instance(I,ST,[isa(T)]))))).

:- asserta((user:isa(I,C):-loop_check(isa_backchaing(I,C)))).
:- asserta(('$toplevel':isa(I,C):-user:isa(I,C))).


:- forall(is_pred_declarer(Prop),decl_type(Prop)).
:- dynamic(isa/2).
:- decl_mpred_hybrid(isa/2).
:- decl_mpred_hybrid(argIsa/3).
:- decl_type(tPred).
:- decl_type(predIsFlag).
:- decl_type(prologOnly).
