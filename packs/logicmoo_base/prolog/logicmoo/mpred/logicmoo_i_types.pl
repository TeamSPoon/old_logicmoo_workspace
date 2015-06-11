/** <module> 
% File used as storage place for all predicates which change as
% the world is run.
%
% props(Obj,height(ObjHt))  == k(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt))  == padd(height,Obj,ObjHt,...) == pfc_add(QueryForm)
% kretract[all](Obj,height(ObjHt))  == kretract[all](Obj,height,ObjHt) == pretract[all](height,Obj,ObjHt) == del[all](QueryForm)
% keraseall(AnyTerm).
%
%
% Dec 13, 2035
% Douglas Miles
*/

:- include(logicmoo_i_header).

% ============================================
% inital a/2 database
% ============================================
:-dynamic(tSet/1).

% user:hasInstance_dyn(W,SS):-nonvar(W),nonvar(SS),SS=isKappaFn(_,S),nonvar(S),!.



/*
disabled a(T,I):- not(current_predicate(deduce_M/1)),!,user:hasInstance_dyn(T,I).
disabled a(T,I):- !, (mudIsa_motel(I,T) *-> true ; (((atom(I),must(not(user:hasInstance_dyn(T,I)))),fail))).
disabled a(T,I):- rdf_x(I,rdf:type,T).
*/

assert_hasInstance(T,I):-  sanity(ground(T:I)),pfc_add(isa(I,T)),!,expire_tabled_list(all).

% ========================================
% is_typef(F).
% Checks F for isa(F,tCol).
% ========================================
:-export(is_typef/1).
is_typef(C):-var(C),!,fail.
is_typef(prologSingleValued):-!.
is_typef(prologSideEffects):-!.
is_typef(F):- isa_backchaing(F,tCol),!.
is_typef(F):- (a(functorDeclares,F);a(tCol,F);clause(isa(F,tCol),true)),!.
is_typef(F):- atom(F),current_predicate(isa_from_morphology/2),isa_from_morphology(F,TT),!,atom_concat(_,'Type',TT).

% ========================================
% is_never_type(F).
% Checks F for not_mudIsa(F,tCol).
% ========================================
:-export(is_never_type/1).

is_never_type(V):-var(V),!,fail.
is_never_type(V):-never_type_why(V,_),!.

a(C,I):-must(atom(C)),G=..[C,I], clause_asserted(G).

never_type_f(Var):-is_ftVar(Var),!,trace_or_throw(var_never_type(Var)).
never_type_f(F):-a(tCol,F),!,fail.
never_type_f(_:W):-!,never_type_f(W).
never_type_f(':-').
never_type_f('include').
never_type_f('tCol'):-!,fail.
never_type_f('clause_asserted').
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

never_type_why(V,ftVar(isThis)):-is_ftVar(V),!.
never_type_why(M:C,Why):-atomic(M),!,never_type_why(C,Why).
never_type_why(mpred_call,mpred_call(isThis)):-!.
never_type_why(C,_):-a(tCol,C),!,fail. % already declared to be a type
never_type_why(C,_):-isa(C,tCol),!,fail.
never_type_why(C,noncol_type(T)):- noncol_type(T),a(T,C),!.
never_type_why(F,decided_not_was_isa(F,W)):-decided_not_was_isa(F,W),!.
%never_type_why(C):- compound(C),functor(C,F,1),isa_asserted(F,tCol).
never_type_why(F,Why):-atom(F),functor(G,F,1),real_builtin_predicate(G),!,Why=(whynot( real_builtin_predicate(G) )).
% never_type_why(F):-dmsg(never_type_why(F)),!,asserta_if_new(isa(F,prologOnly)).
never_type_why(F,Why):-never_type_f(F),Why=is_never_type(F).
never_type_why(F,Why):-prologSideEffects(F),Why=prologSideEffects(F).
never_type_why(F,Why):- atom(F), arity(F,A),!,F\==isa, isa(F,_), A > 1,Why=(whynot( arity(F,A) )).


% ========================================
% isa_from_morphology(F).
% Checks F's name for isa(F,*).
% ========================================

isa_from_morphology(Inst,Type):-atom(Inst),type_suffix(Suffix,Type),atom_concat(Base,Suffix,Inst),!,atom_length(Base,BL),BL>2.
isa_from_morphology(Inst,Type):-atom(Inst),type_prefix(Prefix,Type),atom_concat(Prefix,Other,Inst),capitalized(Other),!.

type_suffix('Fn',ftFunctional).
type_suffix('Type',ttTypeType).
type_suffix('Able',ttTypeByAction).


type_prefix(vt,ttValueType).
type_prefix(tt,ttTypeType).
type_prefix(t,tCol).
type_prefix(v,vtValue).
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
type_prefix(t,tFunction).
type_prefix(fn,tFunction).
type_prefix(mud,tMudPred).
type_prefix(mud,tPred).
type_prefix(prop,tPred).
type_prefix(prolog,ttPredType).
type_prefix(ft,ttFormatType).
type_prefix(pred,tPred).
type_prefix(macro,ttMacroType).

% ========================================
% was_isa(Goal,I,C) recognises isa/2 and its many alternative forms
% ========================================
:- dynamic decided_not_was_isa/2.
:-export(was_isa/3).
was_isa(G,I,C):- fail, \+(current_predicate(_,G)),
  compound(G),functor(G,F,_),hotrace(((not(decided_not_was_isa(F,_)),once(was_isa0(G,I,C)-> true;((functor(G,F,1),get_when(When),asserta_if_new(decided_not_was_isa(F,When)),!,fail)))))).

% get_when(When)
get_when(F:L):-source_location(file,F),current_input(S),line_position(S,L),!.
get_when(F:L):-source_location(F,L),!.
get_when(When):-current_input(S),findall(NV,stream_property(S,NV),When),!.
get_when(M):-context_module(M),!.
get_when(user):-!.

to_isa_out(I,C,OUT):-new_was_isa,!,(atom(C)->OUT=..[C,I];OUT=a(C,I)).
to_isa_out(I,C,isa(I,C)).


was_isa0('$VAR'(_),_,_):-!,fail.
was_isa0(isa(I,C),I,C):-!.
was_isa0(is_typef(_),_,_):-!,fail.
was_isa0(hotrace(_),_,_):-!,fail.
was_isa0(call(_),_,_):-!,fail.
was_isa0(trace(_),_,_):-!,fail.
was_isa0(not(_),_,_):-!,fail.
% was_isa0(a(tCol,I),I,tCol).
was_isa0(ttNotTemporalType(I),I,ttNotTemporalType).
was_isa0(tChannel(I),I,tChannel).
was_isa0(tAgent(I),I,tAgent).
was_isa0(t(C,_),_,_):- \+ new_was_isa, never_type_why(C,_),!,fail.
was_isa0(t(C,I),I,C).
was_isa0(t(P,I,C),I,C):-!,P==isa.
was_isa0(isa(I,C),I,C).
was_isa0(M:G,I,C):-atom(M),!,was_isa0(G,I,C).
was_isa0(G,I,C):-G=..[C,I],!,is_typef(C),!,not(is_never_type(C)).
was_isa0(t(C,I),I,C):- new_was_isa, atom(C),!.

not_ft(T):-nonvar(T),not_ft_quick(T),not(a(ttFormatType,T)).

not_ft_quick(T):-nonvar(T),(T=tItem;T=tRegion;T=tCol;T=completelyAssertedCollection;transitive_subclass_or_same(T,tTemporalThing)).

:-export(asserted_subclass/2).
asserted_subclass(I,T):- ((thlocal:useOnlyExternalDBs,!);thglobal:use_cyc_database),(kbp_t([genls,I,T])).
asserted_subclass(T,ST):-a(genls,T,ST).

chk_ft(T):- not_ft_quick(T),!,fail.
%chk_ft(I):- thlocal:infForward, a(defnSufficient,I,_),!.
%chk_ft(I):- thlocal:infForward, asserted_subclass(I,FT),I\=FT,chk_ft(FT),!.
chk_ft(I):- thlocal:infForward, !,a(ttFormatType,I).



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
is_known_true(isa(tPred, ttPredType)).
is_known_true(F):-is_known_false0(F),!,fail.
is_known_true(F):-is_known_trew(F),!.
%is_known_true(isa(G,tTemporalThing)):- a(_,G),not_mud_isa(G,tCol),not_mud_isa(G,tPred).
is_known_true(genls(G,G)).
is_known_true(isa(apathFn(_,_),tPathway)).

is_known_true(isa(_,ftTerm)).
is_known_true(isa(_,ftID)).


:-export(is_known_trew/1).
is_known_trew(genls(tRegion,tChannel)).
is_known_trew(genls('MaleAnimal',tAgent)).
is_known_trew(genls(prologSingleValued, extentDecidable)).
is_known_trew(genls(tAgent,tChannel)).
is_known_trew(genls(completelyAssertedCollection, extentDecidable)).
is_known_trew(genls(ttFormatType,tCol)).
is_known_trew(genls(ttFormatType,ttNotTemporalType)).
is_known_trew(genls(meta_argtypes,tRelation)).
is_known_trew(genls(tFunction,tRelation)).
is_known_trew(genls(F,tPred)):-a(ttPredType,F).
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

disjointWith0(tAgent,tItem).
disjointWith0(tRegion,tObj).
disjointWith0(ttFormatType,tItem).
disjointWith0(ttFormatType,tObj).
disjointWith0(ttFormatType,tRegion).
disjointWith0(ttTemporalType,ttNotTemporalType).

disjointWithT(A,B):-disjointWith0(A,B).
disjointWithT(B,A):-disjointWith0(A,B).

:-export(not_mud_isa/3).
%not_mud_isa0(tObj, completelyAssertedCollection).
%not_mud_isa0(tObj, ttTemporalType).
%not_mud_isa0(tTemporalThing,ttTemporalType).
not_mud_isa0(I,T):-(var(I);var(T)),trace_or_throw(var_not_mud_isa(I,T)).
not_mud_isa0(ttTypeByAction,ttTypeByAction).
not_mud_isa0(meta_argtypes,ttPredType).
not_mud_isa0(isa,ttPredType).
not_mud_isa0(props,ttPredType).
not_mud_isa0(F, functorDeclares):-not(clause_asserted(functorDeclares(F))).
not_mud_isa0(actGossup,tChannel).
not_mud_isa0(_, blah):-!.
not_mud_isa0(I,meta_argtypes):-!,not(compound(I)).
not_mud_isa0(I,meta_argtypes):-!,not(compound(I)).
not_mud_isa0(_,prologHybrid):-!,fail.
not_mud_isa0(prologMacroHead, ttFormatType).
not_mud_isa0(tAgent,ttFormatType).
not_mud_isa0(tCol,ttFormatType).
not_mud_isa0(tItem,ttFormatType).
not_mud_isa0(tObj, ttFormatType).
not_mud_isa0(tTemporalThing, tTemporalThing).
not_mud_isa0(completelyAssertedCollection,ttTemporalType).
not_mud_isa0(ttFormatType,ttFormatType).
not_mud_isa0(ttTemporalType,tTemporalThing).
%not_mud_isa0(I,tCol):- compound(I),!, \+ a(tCol,I).
%not_mud_isa0(I,C):- compound(I),!, \+ a(C,I).

not_mud_isa(I,C):-loop_check(not_mud_isa(I,C,_)).

not_mud_isa(F, CAC,Why):- completelyAssertedCollection(CAC),!,atom(CAC),current_predicate(CAC/1),G=..[CAC,F],\+((G)),!,Why=completelyAssertedCollection(CAC).
not_mud_isa(I,C,Why):-not_mud_isa0(I,C),Why=not_mud_isa0(I,C).
not_mud_isa(G,tTemporalThing,Why):- ((a(tCol,G),Why=a(tCol,G));(tPred(G),Why=tPred(G))).
not_mud_isa(G,tCol,Why):-never_type_why(G,Why).


tCol_gen(T):- no_repeats(T,(ttTemporalType(T);completelyAssertedCollection(T);tSet(T);tCol(T))),atom(T).
% ==========================
% isa_backchaing(i,c)
% ==========================
isa(I,T):- cwc, (nonvar(T)->true;tCol_gen(T)), loop_check(isa_backchaing(I,T)).
%a(P,F):-loop_check(isa(F,P)).
%a(T,I):- thglobal:pfcManageHybrids,clause_safe(isa(I,T),true).
:-export(isa_backchaing/2).

isa_backchaing(I,T):- (call_tabled(no_repeats(loop_check(isa_backchaing_0(I,T))))).

:-export(isa_backchaing_0/2).
isa_backchaing_0(I,T):- T==ftVar,!,is_ftVar(I).
isa_backchaing_0(_,T):- T==ftProlog,!.
isa_backchaing_0(I,T):-  I==T,I=ttTypeByAction,!,fail.
isa_backchaing_0(I,T):-  nonvar(I),nonvar(T),not_mud_isa(I,T),!,fail.
isa_backchaing_0(I,T):-  var(I),nonvar(T),!,no_repeats_old(transitive_subclass_or_same(AT,T)),isa_asserted(I,AT).
%isa_backchaing_0(I,T):-  var(I),   var(T),!,fail,setof(TT,AT^(isa_asserted(I,AT),transitive_subclass_or_same(AT,TT)),List),!,member(T,List).
%isa_backchaing_0(I,T):-  var(T),!,isa_asserted(I,AT),transitive_subclass_or_same(AT,T).
% isa_backchaing_0(I,T):-  isa_asserted(I,T),!.


isa_backchaing_0(I,T):-  nonvar(T),completelyAssertedCollection(T),!,isa_asserted(I,T).
isa_backchaing_0(I,T):-  isa_asserted(I,AT),transitive_subclass_or_same(AT,T).


% ============================================
% isa_asserted/1
% ============================================

:-export(type_isa/2).

type_isa(Type,ttTemporalType):-arg(_,vv(tAgent,tItem,tObj,tRegion),Type),!.
type_isa(ArgIsa,ttPredType):-a(ttPredType,ArgIsa),!.
type_isa(ftString,ttFormatType):-!.
type_isa(Type,ttFormatType):-chk_ft(Type),!. % text
%  from name


atom_prefix_other(Inst,Prefix,Other):-atom_type_prefix_other(Inst,_,Prefix,Other).
atom_type_prefix_other(Inst,Type,Prefix,Other):-atom(Inst),type_prefix(Prefix,Type),atom_concat(Prefix,Other,Inst),capitalized(Other).
atom_type_prefix_other(Inst,Type,Suffix,Other):-atom(Inst),type_suffix(Suffix,Type),atom_concat(Other,Suffix,Inst),!.


onLoadPfcRule('=>'(a(tCol,Inst), {isa_from_morphology(Inst,Type)} , isa(Inst,Type))).


callOr(Pred,I,T):-(call(Pred,I);call(Pred,T)),!.

% type_deduced(I,T):-atom(T),i_name(mud,T,P),!,clause(a(P,_,I),true).
type_deduced(I,T):-nonvar(I),not(number(I)),clause(a(P,_,I),true),(argIsa_known(P,2,AT)->T=AT;i_name(vt,P,T)).

compound_isa(F,_,T):- mpred_call(resultIsa(F,T)).
compound_isa(_,I,T):- mpred_call(formatted_resultIsa(I,T)).

isa_asserted(I,C):-call_tabled(no_repeats(loop_check(isa_asserted_0(I,C)))).
%isa_asserted(I,CC):-no_repeats((isa_asserted_0(I,C),genls(C,CC))).

:-dynamic(isa_asserted_0/2).
isa_asserted_0(I,T):-is_known_trew(isa(I,T)).
isa_asserted_0(F,tCol):-isa_from_morphology(F,Col),atom_concat(_,'Type',Col),arity(F,1).
%isa_asserted_0([I],T):-nonvar(I),!,isa_asserted_0(I,T).
isa_asserted_0(isInstFn(I),T):-nonvar(I),trace,!,T=I.
isa_asserted_0(aRelatedFn(T,_),I):-nonvar(T),!,T=I.
isa_asserted_0(I,T):- ((thlocal:useOnlyExternalDBs,!);thglobal:use_cyc_database),(kbp_t([isa,I,T]);kbp_t([T,I])).
isa_asserted_0(ttPredType, completelyAssertedCollection):-!.
isa_asserted_0(I,T):- atom(I),isa_from_morphology(I,T).
isa_asserted_0(I,T):- (atom(I);atom(T)),type_isa(I,T).
isa_asserted_0(I,T):- atom(I), I = ttTypeByAction, T=ttTypeByAction,!,fail.
isa_asserted_0(I,T):- nonvar(I),nonvar(T),not_mud_isa(I,T),!,fail.
% isa_asserted_0(I,T):- HEAD= isa(I, T),ruleBackward(HEAD,BODY),trace,call_mpred_body(HEAD,BODY).

isa_asserted_0(I,T):- var(T),!,tCol_gen(T),isa(I,T).
isa_asserted_0(I,T):- atom(T),current_predicate(T/1),G=..[T,I],(predicate_property(G,number_of_clauses(_))->clause(G,true);ignoreOnError(G)).
isa_asserted_0(I,T):- nonvar(I),(  ((var(T);chk_ft(T)),if_defined(term_is_ft(I,T)))*->true;type_deduced(I,T) ).
isa_asserted_0(I,T):- compound(I),!,get_functor(I,F),compound_isa(F,I,T).
isa_asserted_0(I,T):- nonvar(T),!,isa_asserted_1(I,T).

% isa_asserted_1(I,T):- T\=predStub(_),isa(I,T).
isa_asserted_1(_, ttPredType):-!,fail.
isa_asserted_1(_, functorDeclares):-!,fail.
isa_asserted_1(_, prologHybrid):-!,fail.
isa_asserted_1(I,T):- atom(T),loop_check(isa_w_type_atom(I,T)).
isa_asserted_1(_,T):- a(completelyAssertedCollection,T),!,fail.
%isa_asserted_1(I,T):- append_term(T,I,HEAD),ruleBackward(HEAD,BODY),call_mpred_body(HEAD,BODY).
isa_asserted_1(I,'&'(T1 , T2)):-!,nonvar(T1),var(T2),!,dif:dif(T1,T2),isa_backchaing(I,T1),subclass(T1,T2),isa_backchaing(I,T2).
isa_asserted_1(I,'&'(T1 , T2)):-!,nonvar(T1),!,dif:dif(T1,T2),isa_backchaing(I,T1),isa_backchaing(I,T2).
isa_asserted_1(I,(T1 ; T2)):-!,nonvar(T1),!,dif:dif(T1,T2),isa_backchaing(I,T1),isa_backchaing(I,T2).

isa_w_type_atom(I,T):- a(ttPredType,T),!,isa(I,T).
isa_w_type_atom(_,T):- dont_call_type_arity_one(T),!,fail.
isa_w_type_atom(I,T):- G=..[T,I],once_if_ground(isa_atom_call(T,G),_).

dont_call_type_arity_one(tCol).
dont_call_type_arity_one(ttFormatType).
dont_call_type_arity_one(ttAgentType).
dont_call_type_arity_one(F):-isa(F,prologHybrid),!.

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


:-export(decl_type/1).
decl_type(Var):- var(Var),!,trace_or_throw(var_decl_type(Var)).
decl_type([]):-!.
decl_type([A]):-!,decl_type(A).
decl_type([A|L]):-!,decl_type(A),decl_type(L).
decl_type((A,L)):-!,decl_type(A),decl_type(L).
% decl_type(Spec):- compound(Spec),must_det(define_compound_isa(Spec,tCol)),!.
decl_type(Spec):- decl_type_unsafe(Spec),!.

decl_type_unsafe(Spec):- never_type_why(Spec,Why),!,trace_or_throw(never_type_why(Spec,Why)).
decl_type_unsafe(Spec):- tCol(Spec)->true;(show_call(pfc_add(tCol(Spec))),guess_supertypes(Spec)).



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
define_ft_0(Spec):- a(ttFormatType,Spec),!.
define_ft_0(Spec):- a(tCol,Spec),dmsg(once(maybe_coierting_plain_type_to_formattype(Spec))),fail.
define_ft_0(Spec):- hooked_asserta(isa(Spec,ttFormatType)),(compound(Spec)->hooked_asserta(isa(Spec,meta_argtypes));true).

:-export(assert_subclass/2).
assert_subclass(O,T):-assert_subclass_safe(O,T).

:-export(assert_p_safe/3).
assert_p_safe(P,O,T):-
  ignore((nonvar(O),nonvar(T),nonvar(P),nop((not(chk_ft(O)),not(chk_ft(T)))),pfc_assert_guess(t(P,O,T)))).

:-export(assert_subclass_safe/2).
assert_subclass_safe(O,T):-
  ignore((nonvar(O),decl_type_safe(O),nonvar(T),decl_type_safe(T),nonvar(O),nop((not(chk_ft(O)),not(chk_ft(T)))),pfc_assert_guess(genls(O,T)))).

:-export(assert_isa_safe/2).
assert_isa_safe(O,T):- ignore((nonvar(O),nonvar(T),decl_type_safe(T),assert_isa(O,T))).

%OLD user:decl_database_hook(change(assert,_A_or_Z),genls(S,C)):-decl_type_safe(S),decl_type_safe(C).


%OLD user:decl_database_hook(change(assert,_A_or_Z),isa(W,ttTemporalType)):-decl_type_safe(W). %,call_after_mpred_load(forall(isa(I,W),create_instance(I,W))).
%OLD user:decl_database_hook(change(assert,_A_or_Z),isa(W,tCol)):- (test_tl(infSupertypeName);true),guess_supertypes(W).

:-dynamic(tried_guess_types_from_name/1).
:-dynamic(did_learn_from_name/1).

guess_types(W):- tried_guess_types_from_name(W),!.
guess_types(W):- isa_from_morphology(W,What),!,ignore(guess_types(W,What)).

guess_types(W,tCol):- !, guess_supertypes(W).
guess_types(W,What):- asserta(tried_guess_types_from_name(W)),ignore((atom(W),guess_types_0(W,What))).
guess_types_0(W,ftID):-hotrace((atom(W),atom_concat(i,T,W), 
   atom_codes(T,AC),last(AC,LC),is_digit(LC),append(Type,Digits,AC),catch(number_codes(_,Digits),_,fail),atom_codes(CC,Type),!,i_name(t,CC,NewType))),
   decl_type_safe(NewType),(tCol(NewType)->(assert_isa_safe(W,NewType),asserta(did_learn_from_name(W)),guess_supertypes(NewType));true).


guess_supertypes(W):- tried_guess_types_from_name(W),!.
guess_supertypes(W):- asserta(tried_guess_types_from_name(W)),ignore((atom(W),guess_supertypes_0(W))).

guess_supertypes_0(W):-atom(W),atomic_list_concat(List,'_',W),length(List,S),S>2,!, append(FirstPart,[Last],List),atom_length(Last,AL),AL>3,not(member(flagged,FirstPart)),
            atomic_list_concat(FirstPart,'_',_NewCol),pfc_assert_guess(genls(W,Last)),asserta(did_learn_from_name(W)).
guess_supertypes_0(W):-T=t,to_first_break(W,lower,T,All,upper),to_first_break(All,upper,Super,Rest,_),
   atom_length(Rest,L),!,L>2,i_name(tt,Rest,NewSuper),atom_concat(NewSuper,'Type',SuperTT),pfc_assert_guess(isa(SuperTT,ttTypeType)),
  pfc_assert_guess(isa(W,SuperTT)),asserta(did_learn_from_name(W)),!,guess_typetypes(SuperTT).

pfc_assert_guess(G):-show_call(pfc_assert(G,(d,d))).

guess_typetypes(W):- tried_guess_types_from_name(W),!.
guess_typetypes(W):- asserta(tried_guess_types_from_name(W)),ignore((atom(W),guess_typetypes_0(W))).

guess_typetypes_0(TtTypeType):-atom_concat(tt,TypeType,TtTypeType),atom_concat(Type,'Type',TypeType),
 atom_concat(t,Type,TType),pfc_assert_guess((isa(T,TtTypeType)=>genls(T,TType))).

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
isa_provide_mpred_storage_op(call(_),G):- was_isa(G,I,C),!, (isa_backchaing(I,C);a(C,I)).
% ISA CLAUSES
isa_provide_mpred_storage_clauses(isa(I,C),true,hasInstanceIC):-a(C,I).
isa_provide_mpred_storage_clauses(H,true,hasInstanceCI):-
   (compound(H)-> 
      ((functor(H,C,1)-> H=..[C,I]; H=isa(I,C)), a(C,I)) ; 
      (a(C,I),(nonvar(C)->append_term(C,I,H);H=isa(I,C)))).

%isa_provide_mpred_storage_clauses(isa(I,C),B,W):-mpred_t_mpred_storage_clauses_rules(isa(I,C),B,W).
%isa_provide_mpred_storage_clauses(isa(I,C),B,W):-nonvar(C),append_term(C,I,H),mpred_t_mpred_storage_clauses_rules(H,B,W).


user:provide_mpred_storage_clauses(H,B,(What)):-fail,isa_provide_mpred_storage_clauses(H,B,What).

% isa_backchaing(I,T):- stack_depth(Level),Level>650,trace_or_throw(skip_dmsg_nope(failing_stack_overflow(isa_backchaing(I,T)))),!,fail.


% ================================================
% assert_isa/2
% ================================================
:-meta_predicate(assert_isa(+,+)).

assert_isa([I],T):-nonvar(I),!,assert_isa(I,T).
assert_isa(I,T):-assert_isa_i(I,T),must(sanity((must(is_asserted(isa(I,T)));must(isa_asserted(I,T))))).

%assert_isa_i(I,T):- once(sanity(not(singletons_throw_else_fail(assert_isa(I,T))))),fail.
assert_isa_i(_,ftTerm):-!.
assert_isa_i(_,ftTerm(_)):-!.
%assert_isa_i(I,PT):- nonvar(PT),a(ttPredType,PT),!,decl_mpred(I,PT),assert_hasInstance(PT,I).
assert_isa_i(I,T):- skipped_table_call(loop_check(assert_isa_ilc(I,T),loop_check(assert_hasInstance(T,I),dtrace(assert_isa_ilc(I,T))))).

:-export(assert_isa_ilc/2).
% skip formatter cols
assert_isa_ilc(isKappaFn(_,_),_):-!.
assert_isa_ilc(_I,T):- member(T,[ftString]),!.
assert_isa_ilc(I,T):- is_list(I),!,maplist(assert_isa_reversed(T),I).
assert_isa_ilc(I,T):- not(not(a(T,I))),!.
assert_isa_ilc(I,T):- hotrace(chk_ft(T)),(compound(I)->dmsg(once(dont_assert_c_is_ft(I,T)));dmsg(once(dont_assert_is_ft(I,T)))),rtrace((chk_ft(T))).
assert_isa_ilc(I,T):- once(decl_type(T)),!,G=..[T,I], pfc_assert(G),(not_mud_isa(I,T,Why)->throw(Why);true),!.
assert_isa_ilc(I,T):- 
  skipped_table_call(must((assert_isa_ilc_unchecked(I,T),!,show_call_failure(isa_backchaing(I,T))))).
  

assert_isa_ilc_unchecked(I,T):- compound(I),!,must((get_functor(I,F),assert_compound_isa(I,T,F))),!.
assert_isa_ilc_unchecked(I,tCol):- must(show_call(decl_type(I))).
assert_isa_ilc_unchecked(I,ttFormatType):- must(show_call(define_ft(I))).
assert_isa_ilc_unchecked(I,T):-   (( \+(isa(I,_)),\+(a(tCol,I))) -> add(tCountable(I)) ; true),
  with_assertions([thlocal:infSkipArgIsa,thlocal:infSkipFullExpand],assert_hasInstance(T,I)).

assert_compound_isa(I,_,_):- compound(I), I\=resultIsaFn(_),glean_pred_props_maybe(I),fail.
assert_compound_isa(I,T,_):- hotrace(chk_ft(T)),dmsg(once(dont_assert_is_ft(I,T))),rtrace((chk_ft(T))).
%assert_compound_isa(I,T,F):- is_Template(I),!,assert_hasInstance(T,I),show_call(pfc_add(resultIsa(I,T))),assert_hasInstance(T,resultIsaFn(F)).
assert_compound_isa(I,T,F):- ignore((is_Template(I),with_assertions(infConfidence(vWeak),assert_predArgTypes(I)))),
   hooked_asserta(isa(I,T)),assert_hasInstance(T,I),show_call(pfc_add(resultIsa(F,T))),assert_hasInstance(T,resultIsaFn(F)),!.
   

get_mpred_arg(N,_:C,E):-!,compound(C),arg(N,C,E).
get_mpred_arg(N,C,E):-!,compound(C),arg(N,C,E).

is_Template(I):- get_mpred_arg(_,I,Arg1),a(tCol,Arg1).


pfc_univ(C,I,Head):-atom(C),!,Head=..[C,I],predicate_property(Head,number_of_clauses(_)).

assert_isa_reversed(T,I):-assert_isa(I,T).

% ================================================
% assert_isa HOOKS
% ================================================
%OLD user:decl_database_hook(_,genls(_,_)):-retractall(a(_,isa,_,_)),retractall(a(_,genls,_,_)).
%OLD user:decl_database_hook(change(assert,_),DATA):-into_mpred_form(DATA,O),!,O=isa(I,T),hotrace(doall(assert_isa_hooked(I,T))).
%OLD user:decl_database_hook(change(assert,_),isa(I,T)):- assert_hasInstance(T,I),fail.

%OLD user:decl_database_hook(change( retract,_),isa(I,T)):-doall(db_retract_isa_hooked(I,T)).

assert_isa_hooked(A,_):-retractall(a(cache_I_L,isa,A,_)),fail.
assert_isa_hooked(F,T):- a(ttPredType,T),decl_mpred(F,T),fail.
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

assert_isa_hooked_after(F,T):-a(ttPredType,T),!,decl_mpred(F,T).
assert_isa_hooked_after(_,tCol):-!.
assert_isa_hooked_after(_,ttFormatType):-!.
%assert_isa_hooked_after(I,T):- ttTemporalType(T),!,assert_isa_hooked_creation(I,T).

%assert_isa_hooked_after(I,T):- assert_isa_hooked_creation(I,T).

/*
assert_isa_hooked_after(I,T):- not(completelyAssertedCollection(T)),impliedSubClass(T,ST),completelyAssertedCollection(ST),assert_isa(I,ST).
:-export(impliedSubClass/2).
impliedSubClass(T,ST):-ground(T:ST),is_known_false(genls(T,ST)),!,fail.
impliedSubClass(T,ST):-predicate_property(transitive_subclass(T,ST),_),!,call_tabled(transitive_subclass(T,ST)).
*/

% one of 4 special cols
% assert_isa_hooked_creation(I,T):- ttTemporalType(T),!,call_after_mpred_load((create_instance(I,T,[]))).
% sublass of 4 special cols
% assert_isa_hooked_creation(I,T):- doall((ttTemporalType(ST),impliedSubClass(T,ST),call_after_mpred_load((create_instance(I,ST,[isa(T)]))))).


% :- asserta((user:isa(I,C):-loop_check(isa_backchaing(I,C)))).
:- asserta(('$toplevel':isa(I,C):-user:isa(I,C))).

mpred_types_loaded.

% ISA QUERY
system:goal_expansion(ISA,GO) :- \+ thlocal:disable_mpred_term_expansions_locally, \+current_predicate(_,ISA),
  once((compound(ISA),was_isa(ISA,I,C))),thlocal:is_calling,show_call(GO=no_repeats(isa(I,C))).
% ISA GOAL
% pfc_system_goal_expansion(G,GO):-G\=isa(_,_),was_isa(G,I,C),GO=isa(I,C).
% ISA EVER
%pfc_term_expansion(G,GO):-  \+ thlocal:disable_mpred_term_expansions_locally,was_isa(G,I,C),GO=isa(I,C).

:-decl_type(tCol).
:-decl_type(ttPredType).

