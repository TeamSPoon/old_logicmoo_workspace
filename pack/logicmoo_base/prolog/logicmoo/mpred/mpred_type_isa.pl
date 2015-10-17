/** <module> 
% File used as storage place for all predicates which change as
% the world is run.
%
% props(Obj,height(ObjHt))  == k(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt))  == padd(height,Obj,ObjHt,...) == mpred_add(QueryForm)
% kretract[all](Obj,height(ObjHt))  == kretract[all](Obj,height,ObjHt) == pretract[all](height,Obj,ObjHt) == del[all](QueryForm)
% keraseall(AnyTerm).
%
%
% Dec 13, 2035
% Douglas Miles
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_isa.pl
:- module(mpred_type_isa,
          [ a/2,
            assert_compound_isa/3,
            assert_hasInstance/2,
            assert_isa/2,
            assert_isa_hooked/2,
            assert_isa_hooked_after/2,
            assert_isa_i/2,
            assert_isa_ilc/2,
            assert_isa_ilc_unchecked/2,
            assert_isa_reversed/2,
            assert_isa_safe/2,
            assert_p_safe/3,
            assert_subclass/2,
            assert_subclass_safe/2,
            asserted_subclass/2,
            atom_prefix_other/3,
            atom_type_prefix_other/4,
            cached_isa/2,
            callOr/3,
            chk_ft/1,
            compound_isa/3,
            decl_type/1,
            decl_type_safe/1,
            decl_type_unsafe/1,
            define_ft/1,
            define_ft_0/1,
            disjointWith0/2,
            disjointWithT/2,
            dont_call_type_arity_one/1,
            get_mpred_arg/3,
            guess_supertypes/1,
            guess_supertypes_0/1,
            guess_types/1,
            guess_types/2,
            guess_types_0/2,
            guess_typetypes/1,
            guess_typetypes_0/1,
            has_free_args/1,
            into_single_class/2,
            is_Template/1,
            is_known_false/1,
            is_known_false0/1,
            is_known_trew/1,
            is_known_true/1,
            is_never_type/1,
            is_non_skolem/1,
            is_non_unit/1,
            is_sk_functor/1,
            is_sk_unit/1,
            is_typef/1,
            isa_asserted/2,
            isa_asserted_1/2,
            isa_atom_call/2,
            isa_atom_call_ilc/2,
            isa_backchaing/2,
            isa_backchaing_0/2,
            isa_from_morphology/2,
            isa_w_type_atom/2,
            mpred_add_guess/1,
            mpred_types_loaded/0,            
            never_type_f/1,
            never_type_why/2,
            noncol_type/1,
            not_ft/1,
            not_ft_quick/1,
            not_mud_isa/2,
            not_mud_isa/3,
            not_mud_isa0/2,
            onLoadPfcRule/1,
            pfcNeverTrue/1,
            tCol_gen/1,
            to_isa_out/3,
            transitive_P/4,
            transitive_P_l_r/4,
            transitive_P_r_l/4,
            transitive_subclass_or_same/2,
            type_deduced/2,
            type_isa/2,
            was_isa/3,
            was_isa0/3,
         %    decided_not_was_isa/2,
         % did_learn_from_name/1,
         % isa_pred_now_locked/0,
         isa_asserted_0/2,
         new_was_isa/0,
          type_prefix/2,
          type_suffix/2
          ]).

:- use_module(logicmoo(util/logicmoo_util_preddefs)).

% autoloading user:portray_clause_pi/2 from /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_first
:- meta_predicate callOr(1,?,?),
transitive_P(3,?,?,?),
transitive_P_l_r(3,?,?,?),
transitive_P_r_l(3,?,?,?),
        assert_isa(+, +).

:- include('mpred_header.pi').




% ============================================
% inital a/2 database
% ============================================

% lmconf:hasInstance_dyn(W,SS):-nonvar(W),nonvar(SS),SS=isKappaFn(_,S),nonvar(S),!.



/*
disabled a(T,I):- not(current_predicate(deduce_M/1)),!,lmconf:hasInstance_dyn(T,I).
disabled a(T,I):- !, (mudIsa_motel(I,T) *-> true ; (((atom(I),must(not(lmconf:hasInstance_dyn(T,I)))),fail))).
disabled a(T,I):- rdf_x(I,rdf:type,T).
*/

assert_hasInstance(T,I):-  sanity(ground(T:I)),mpred_add(isa(I,T)),!,expire_tabled_list(all).

% ========================================
% is_typef(F).
% Checks F for isa(F,tCol).
% ========================================
:- was_export(is_typef/1).
is_typef(C):-is_ftVar(C),!,fail.
is_typef(prologSingleValued):-!.
is_typef(prologSideEffects):-!.
is_typef(F):- isa_backchaing(F,tCol),!.
is_typef(F):- (a(functorDeclares,F);a(tCol,F);clause(isa(F,tCol),true)),!.
is_typef(F):- atom(F),current_predicate(isa_from_morphology/2),isa_from_morphology(F,TT),!,atom_concat(_,'Type',TT).

% ========================================
% is_never_type(F).
% Checks F for not_mudIsa(F,tCol).
% ========================================
:- was_export(is_never_type/1).

is_never_type(V):-is_ftVar(V),!,fail.
is_never_type(V):-never_type_why(V,_),!.

a(C,I):- atom(C),G=..[C,I], clause_asserted(G).

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
never_type_f('use_module').
never_type_f('meta_predicate').
never_type_f('Area1000').
never_type_f(iPlayer2).
never_type_f(genls).
never_type_f(must).
never_type_f(mpred_isa).
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
%never_type_why(C):- is_ftCompound(C),functor(C,F,1),isa_asserted(F,tCol).
never_type_why(F,Why):-atom(F),functor(G,F,1),real_builtin_predicate(G),!,Why=(whynot( real_builtin_predicate(G) )).
% never_type_why(F):-dmsg(never_type_why(F)),!,asserta_if_new(isa(F,prologDynamic)).
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
:- was_dynamic decided_not_was_isa/2.
:- was_export(was_isa/3).
was_isa(G,I,C):- fail, \+(current_predicate(_,G)),
  is_ftCompound(G),functor(G,F,_),hotrace(((not(decided_not_was_isa(F,_)),once(was_isa0(G,I,C)-> true;((functor(G,F,1),
  current_source_location(When),asserta_if_new(decided_not_was_isa(F,When)),!,fail)))))).

% current_source_location(When)

to_isa_out(I,C,OUT):-new_was_isa,!,(atom(C)->OUT=..[C,I];OUT=a(C,I)).
to_isa_out(I,C,isa(I,C)).

new_was_isa:-fail.

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

:- was_export(asserted_subclass/2).
asserted_subclass(I,T):- ((t_l:useOnlyExternalDBs,!);lmconf:use_cyc_database),(kbp_t([genls,I,T])).
asserted_subclass(T,ST):- t(genls,T,ST).

chk_ft(T):- not_ft_quick(T),!,fail.
%chk_ft(I):- t_l:infForward, a(defnSufficient,I,_),!.
%chk_ft(I):- t_l:infForward, asserted_subclass(I,FT),I\=FT,chk_ft(FT),!.
chk_ft(I):- t_l:infForward, !,a(ttFormatType,I).



into_single_class(Var,VV):-is_ftVar(Var),!, (nonvar(VV)->into_single_class(VV,Var);Var=VV).
into_single_class(A,B):- is_ftCompound(B),!, (is_ftCompound(A) -> (into_single_class(A,AB),into_single_class(B,AB)) ; into_single_class(B,A) ).
into_single_class('&'(A,Var),VV):-is_ftVar(Var),!,into_single_class(A,VV).
into_single_class('&'(A,B),VV):-!, into_single_class((B),VV);into_single_class((A),VV).
into_single_class(A,A).

% ==========================
% taxonomicPair(isa,genls)
% ==========================

:- was_export((transitive_subclass_or_same/2)).
transitive_subclass_or_same(A,B):- (is_ftVar(A),is_ftVar(B)),!,A=B.
transitive_subclass_or_same(A,A):-nonvar(A).
transitive_subclass_or_same(A,B):-genls(A,B).

transitive_P(DB,P,L,R):-call(DB,P,L,R).
transitive_P(DB,P,L,R):-is_ftVar(L),!,transitive_P_r_l(DB,P,L,R).
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


:- was_export(is_known_trew/1).
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

:- was_export(is_known_false0/1).
is_known_false0(isa(G,Why)):-!,catch(not_mud_isa(G,Why),_,fail).
% is_known_false0(genls(Type,_)):-arg(_,vv(tCol,tRelation,ttFormatType),Type).


is_non_unit(C):- \+ is_unit(C).
is_non_skolem(C):- \+ is_sk_unit(C).

is_sk_unit(C):-is_ftCompound(C), C\=(_:-_),get_functor(C,F),is_sk_functor(F),!.
is_sk_functor(F):- (\+ atom(F)),!,fail.
is_sk_functor(F):-atom_concat('sk',_,F).



:- was_export(has_free_args/1).
has_free_args(C):- sanity(is_ftCompound(C)),arg(_,C,E),is_ftVar(E),!.

% is_known_false(genls(A,B)):-disjointWith(A,B).

disjointWith0(tAgent,tItem).
disjointWith0(tRegion,tObj).
disjointWith0(ttFormatType,tItem).
disjointWith0(ttFormatType,tObj).
disjointWith0(ttFormatType,tRegion).
disjointWith0(ttTemporalType,ttNotTemporalType).

disjointWithT(A,B):-disjointWith0(A,B).
disjointWithT(B,A):-disjointWith0(A,B).

:- was_export(not_mud_isa/3).
%not_mud_isa0(tObj, completelyAssertedCollection).
%not_mud_isa0(tObj, ttTemporalType).
%not_mud_isa0(tTemporalThing,ttTemporalType).
not_mud_isa0(I,T):-(is_ftVar(I);is_ftVar(T)),trace_or_throw(var_not_mud_isa(I,T)).
not_mud_isa0(I,_):- is_sk_unit(I),!,fail.
not_mud_isa0(ttTypeByAction,ttTypeByAction).
not_mud_isa0(meta_argtypes,ttPredType).
not_mud_isa0(isa,ttPredType).
not_mud_isa0(props,ttPredType).
not_mud_isa0(F, functorDeclares):-not(clause_asserted(functorDeclares(F))).
not_mud_isa0(actGossup,tChannel).
not_mud_isa0(_, blah):-!.
not_mud_isa0(I,meta_argtypes):-!,not(is_ftCompound(I)).
not_mud_isa0(I,meta_argtypes):-!,not(is_ftCompound(I)).
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
%not_mud_isa0(I,tCol):- is_ftCompound(I),!, \+ a(tCol,I).
%not_mud_isa0(I,C):- is_ftCompound(I),!, \+ a(C,I).

not_mud_isa(I,C):-loop_check(not_mud_isa(I,C,_)).

not_mud_isa(F, CAC,Why):- completelyAssertedCollection(CAC),!,atom(CAC),current_predicate(_:CAC/1),G=..[CAC,F],\+((G)),!,Why=completelyAssertedCollection(CAC).
not_mud_isa(I,C,Why):-not_mud_isa0(I,C),Why=not_mud_isa0(I,C).
not_mud_isa(G,tTemporalThing,Why):- ((a(tCol,G),Why=a(tCol,G));(tPred(G),Why=tPred(G))).
not_mud_isa(G,tCol,Why):-never_type_why(G,Why).


tCol_gen(T):- no_repeats(T,(ttTemporalType(T);completelyAssertedCollection(T);tSet(T);tCol(T))). % ,atom(T).
% ==========================
% isa_backchaing(i,c)
% ==========================
lmconf:module_local_init:- mpred_add((isa(I,T):- cwc,isa_backchaing(I,T))).
%a(P,F):-loop_check(isa(F,P)).
%a(T,I):- lmconf:pfcManageHybrids,clause_safe(isa(I,T),true).
:- was_export(isa_backchaing/2).


:- was_export(isa_backchaing_0/2).
isa_backchaing(I,T):- T==ftVar,!,is_ftVar(I).
isa_backchaing(I,T):- nonvar(I),is_ftVar(I),!,T=ftVar.
isa_backchaing(_,T):- T==ftProlog,!.
isa_backchaing(I,T):-  I==T,I=ttTypeByAction,!,fail.


isa_backchaing(I,T):- is_ftVar(I),is_ftVar(T),!,tCol_gen(T),nonvar(T),isa_backchaing(I,T).
isa_backchaing(I,T):- call_tabled(isa(I,T),no_repeats(loop_check(isa_backchaing_0(I,T)))).

isa_backchaing_0(I,T):- nonvar(T),is_ftVar(T),!,trace_or_throw(var_isa_backchaing(I,T)).
isa_backchaing_0(I,T):-  nonvar(T),completelyAssertedCollection(T),!,isa_asserted(I,T).
isa_backchaing_0(I,T):-  nonvar(I),nonvar(T),!,no_repeats_old(transitive_subclass_or_same(AT,T)),isa_asserted(I,AT).
isa_backchaing_0(I,T):-  is_ftVar(I),nonvar(T),!,no_repeats_old(transitive_subclass_or_same(AT,T)),isa_asserted(I,AT).
isa_backchaing_0(I,T):-  sanity(nonvar(I)),isa_asserted(I,AT),transitive_subclass_or_same(AT,T).


% ============================================
% isa_asserted/1
% ============================================

:- was_export(type_isa/2).

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
compound_isa(_,I,T):- mpred_call(isa_asserted(I,T)).

isa_asserted(I,C):- ((call_tabled(isa(I,C),no_repeats(loop_check(isa_asserted_0(I,C)))))).
%isa_asserted(I,CC):-no_repeats((isa_asserted_0(I,C),genls(C,CC))).

isa_asserted_0(I,T):-is_known_trew(isa(I,T)).
isa_asserted_0(F,tCol):-isa_from_morphology(F,Col),atom_concat(_,'Type',Col),arity(F,1).
%isa_asserted_0([I],T):-nonvar(I),!,isa_asserted_0(I,T).

isa_asserted_0(isInstFn(I),T):-nonvar(I),trace,!,T=I.
isa_asserted_0(aRelatedFn(T,_),I):-nonvar(T),!,T=I.
isa_asserted_0(aRelatedFn(T,_,_),I):-nonvar(T),!,T=I.
isa_asserted_0(I,T):- ((t_l:useOnlyExternalDBs,!);lmconf:use_cyc_database),(kbp_t([isa,I,T]);kbp_t([T,I])).
isa_asserted_0(ttPredType, completelyAssertedCollection):-!.
isa_asserted_0(I,T):- atom(I),isa_from_morphology(I,T).
isa_asserted_0(I,T):- (atom(I);atom(T)),type_isa(I,T).
isa_asserted_0(I,T):- atom(I), I = ttTypeByAction, T=ttTypeByAction,!,fail.
isa_asserted_0(I,T):- nonvar(I),nonvar(T),not_mud_isa(I,T),!,fail.
% isa_asserted_0(I,T):- HEAD= isa(I, T),ruleBackward(HEAD,BODY),trace,call_mpred_body(HEAD,BODY).

isa_asserted_0(I,T):- is_ftVar(T),!,tCol_gen(T),isa(I,T).
isa_asserted_0(I,T):- atom(T),current_predicate(T,_:G),G=..[T,I],(predicate_property(G,number_of_clauses(_))->clause(G,true);on_x_cont(G)).
isa_asserted_0(I,T):- nonvar(I),(  ((is_ftVar(T);chk_ft(T)),if_defined(term_is_ft(I,T)))*->true;type_deduced(I,T) ).
isa_asserted_0(I,T):- is_ftCompound(I),is_non_unit(I),is_non_skolem(I),!,get_functor(I,F),compound_isa(F,I,T).
isa_asserted_0(I,T):- nonvar(T),!,isa_asserted_1(I,T).

% isa_asserted_1(I,T):- T\=predStub(_),isa(I,T).
isa_asserted_1(_, ttPredType):-!,fail.
isa_asserted_1(_, functorDeclares):-!,fail.
isa_asserted_1(_, prologHybrid):-!,fail.
isa_asserted_1(I,T):- atom(T),loop_check(isa_w_type_atom(I,T)).
isa_asserted_1(_,T):- a(completelyAssertedCollection,T),!,fail.
%isa_asserted_1(I,T):- append_term(T,I,HEAD),ruleBackward(HEAD,BODY),call_mpred_body(HEAD,BODY).
isa_asserted_1(I,'&'(T1 , T2)):-!,nonvar(T1),is_ftVar(T2),!,dif:dif(T1,T2),isa_backchaing(I,T1),genls(T1,T2),isa_backchaing(I,T2).
isa_asserted_1(I,'&'(T1 , T2)):-!,nonvar(T1),!,dif:dif(T1,T2),isa_backchaing(I,T1),isa_backchaing(I,T2).
isa_asserted_1(I,(T1 ; T2)):-!,nonvar(T1),!,dif:dif(T1,T2),isa_backchaing(I,T1),isa_backchaing(I,T2).

isa_w_type_atom(I,T):- a(ttPredType,T),!,isa(I,T).
isa_w_type_atom(_,T):- dont_call_type_arity_one(T),!,fail.
isa_w_type_atom(I,T):- G=..[T,I],once_if_ground(isa_atom_call(T,G),_).

dont_call_type_arity_one(_):-!,fail.
dont_call_type_arity_one(tCol).
dont_call_type_arity_one(ttFormatType).
dont_call_type_arity_one(ttAgentType).
dont_call_type_arity_one(F):-isa(F,prologHybrid),!.

isa_atom_call(T,G):-loop_check(isa_atom_call_ilc(T,G)).

isa_atom_call_ilc(_,G):- real_builtin_predicate(G),!,G.
%isa_atom_call_ilc(_,G):- predicate_property(G,number_of_clauses(_)),!,clause(G,B),call_mpred_body(G,B).
isa_atom_call_ilc(_,G):- predicate_property(G,number_of_rules(R)),R>0,!,G.


cached_isa(I,T):-hotrace(isa_backchaing(I,T)).

% '$toplevel':isa(I,C):-isa_backchaing(I,C).



% ============================================
% decl_type/1
% ============================================
:- was_export(decl_type_safe/1).
decl_type_safe(T):- is_ftCompound(T),!.
decl_type_safe(T):- ignore((atom(T),not(never_type_why(T,_)),not(number(T)),decl_type(T))).


:- was_export(decl_type/1).
decl_type(Var):- is_ftVar(Var),!,trace_or_throw(var_decl_type(Var)).
decl_type([]):-!.
decl_type([A]):-!,decl_type(A).
decl_type([A|L]):-!,decl_type(A),decl_type(L).
decl_type((A,L)):-!,decl_type(A),decl_type(L).
% decl_type(Spec):- is_ftCompound(Spec),must_det(define_compound_isa(Spec,tCol)),!.
decl_type(Spec):- decl_type_unsafe(Spec),!.

decl_type_unsafe(Spec):- never_type_why(Spec,Why),!,trace_or_throw(never_type_why(Spec,Why)).
decl_type_unsafe(Spec):- tCol(Spec)->true;(show_call(mpred_add(tCol(Spec))),guess_supertypes(Spec)).



% ========================================
% is_typef(F).
% Checks F for isa(F,tCol).
% ========================================


:- was_export(define_ft/1).
% define_ft(ftListFn(Spec)):- nonvar(Spec),never_type_why(Spec,Why),!,trace_or_throw(never_ft(ftListFn(Spec),Why)).
define_ft(ftListFn(_)):-!.
define_ft(Spec):- never_type_why(Spec,Why),!,trace_or_throw(never_ft(never_type_why(Spec,Why))).
define_ft(M:F):- !, '@'(define_ft(F), M).
define_ft(Spec):- loop_check(define_ft_0(Spec),true).



define_ft_0(xyzFn):-!.
define_ft_0(Spec):- a(ttFormatType,Spec),!.
define_ft_0(Spec):- a(tCol,Spec),dmsg(once(maybe_coierting_plain_type_to_formattype(Spec))),fail.
define_ft_0(Spec):- hooked_asserta(isa(Spec,ttFormatType)),(is_ftCompound(Spec)->hooked_asserta(isa(Spec,meta_argtypes));true).

:- was_export(assert_subclass/2).
assert_subclass(O,T):-assert_subclass_safe(O,T).

:- was_export(assert_p_safe/3).
assert_p_safe(P,O,T):-
  ignore((nonvar(O),nonvar(T),nonvar(P),nop((not(chk_ft(O)),not(chk_ft(T)))),mpred_add_guess(t(P,O,T)))).

:- was_export(assert_subclass_safe/2).
assert_subclass_safe(O,T):-
  ignore((nonvar(O),decl_type_safe(O),nonvar(T),decl_type_safe(T),nonvar(O),nop((not(chk_ft(O)),not(chk_ft(T)))),mpred_add_guess(genls(O,T)))).

:- was_export(assert_isa_safe/2).
assert_isa_safe(O,T):- ignore((nonvar(O),nonvar(T),decl_type_safe(T),assert_isa(O,T))).

%OLD lmconf:decl_database_hook(change(assert,_A_or_Z),genls(S,C)):-decl_type_safe(S),decl_type_safe(C).


%OLD lmconf:decl_database_hook(change(assert,_A_or_Z),isa(W,ttTemporalType)):-decl_type_safe(W). %,call_after_mpred_load(forall(isa(I,W),create_instance(I,W))).
%OLD lmconf:decl_database_hook(change(assert,_A_or_Z),isa(W,tCol)):- (test_tl(infSupertypeName);true),guess_supertypes(W).

:- was_dynamic(tried_guess_types_from_name/1).
:- was_dynamic(did_learn_from_name/1).

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
            atomic_list_concat(FirstPart,'_',_NewCol),mpred_add_guess(genls(W,Last)),asserta(did_learn_from_name(W)).
guess_supertypes_0(W):-T=t,to_first_break(W,lower,T,All,upper),to_first_break(All,upper,_UnusedSuper,Rest,_),
   atom_length(Rest,L),!,L>2,i_name(tt,Rest,NewSuper),atom_concat(NewSuper,'Type',SuperTT),mpred_add_guess(isa(SuperTT,ttTypeType)),
  mpred_add_guess(isa(W,SuperTT)),asserta(did_learn_from_name(W)),!,guess_typetypes(SuperTT).

mpred_add_guess(G):-show_call(mpred_add(G,(d,d))).

guess_typetypes(W):- tried_guess_types_from_name(W),!.
guess_typetypes(W):- asserta(tried_guess_types_from_name(W)),ignore((atom(W),guess_typetypes_0(W))).

guess_typetypes_0(TtTypeType):-atom_concat(tt,TypeType,TtTypeType),atom_concat(Type,'Type',TypeType),
 atom_concat(t,Type,TType),mpred_add_guess((isa(T,TtTypeType)=>genls(T,TType))).

/*
system:term_expansion(isa(Compound,PredArgTypes),
  (:-dmsg(mpred_add(wizza(Compound,PredArgTypes))))):-
  lmconf:isa_pred_now_locked,
   ground(Compound:PredArgTypes),show_call(mpred_add(isa(Compound,PredArgTypes))),!.
*/

isa_lmconf:mpred_provide_storage_op(_,_):-!,fail.
% ISA MODIFY
isa_lmconf:mpred_provide_storage_op(change(assert,_),G):- was_isa(G,I,C),!,dmsg(assert_isa_from_op(I,C)),!, assert_isa(I,C).
% ISA MODIFY
isa_lmconf:mpred_provide_storage_op(change(retract,How),G):- was_isa(G,I,C),!,show_call((with_assert_op_override(change(retract,How),((dmsg(retract_isa(G,I,C)),!, assert_isa(I,C)))))).
% ISA CALL
isa_lmconf:mpred_provide_storage_op(call(_),G):- was_isa(G,I,C),!, (isa_backchaing(I,C);a(C,I)).
% ISA CLAUSES
isa_lmconf:mpred_provide_storage_clauses(isa(I,C),true,hasInstanceIC):-a(C,I).
isa_lmconf:mpred_provide_storage_clauses(H,true,hasInstanceCI):-
   (is_ftCompound(H)-> 
      ((functor(H,C,1)-> H=..[C,I]; H=isa(I,C)), a(C,I)) ; 
      (a(C,I),(nonvar(C)->append_term(C,I,H);H=isa(I,C)))).

%isa_lmconf:mpred_provide_storage_clauses(isa(I,C),B,W):-mpred_t_mpred_storage_clauses_rules(isa(I,C),B,W).
%isa_lmconf:mpred_provide_storage_clauses(isa(I,C),B,W):-nonvar(C),append_term(C,I,H),mpred_t_mpred_storage_clauses_rules(H,B,W).


lmconf:mpred_provide_storage_clauses(H,B,(What)):-fail,isa_lmconf:mpred_provide_storage_clauses(H,B,What).

% isa_backchaing(I,T):- stack_depth(Level),Level>650,trace_or_throw(skip_dmsg_nope(failing_stack_overflow(isa_backchaing(I,T)))),!,fail.


% ================================================
% assert_isa/2
% ================================================
:- meta_predicate(assert_isa(+,+)).

assert_isa([I],T):-nonvar(I),!,assert_isa(I,T).
assert_isa(I,T):-assert_isa_i(I,T),must(sanity((must(is_asserted(isa(I,T)));must(isa_asserted(I,T))))).

%assert_isa_i(I,T):- once(sanity(not(singletons_throw_else_fail(assert_isa(I,T))))),fail.
assert_isa_i(_,ftTerm):-!.
assert_isa_i(_,ftTerm(_)):-!.
%assert_isa_i(I,PT):- nonvar(PT),a(ttPredType,PT),!,decl_mpred(I,PT),assert_hasInstance(PT,I).
assert_isa_i(I,T):- skipped_table_call(loop_check(assert_isa_ilc(I,T),loop_check(assert_hasInstance(T,I),dtrace(assert_isa_ilc(I,T))))).

:- was_export(assert_isa_ilc/2).
% skip formatter cols
assert_isa_ilc(isKappaFn(_,_),_):-!.
assert_isa_ilc(_I,T):- member(T,[ftString]),!.
assert_isa_ilc(I,T):- is_list(I),!,maplist(assert_isa_reversed(T),I).
assert_isa_ilc(I,T):- not(not(a(T,I))),!.
assert_isa_ilc(I,T):- hotrace(chk_ft(T)),(is_ftCompound(I)->dmsg(once(dont_assert_c_is_ft(I,T)));dmsg(once(dont_assert_is_ft(I,T)))),rtrace((chk_ft(T))).
assert_isa_ilc(I,T):- once(decl_type(T)),!,G=..[T,I], mpred_add(G),(not_mud_isa(I,T,Why)->throw(Why);true),!.
assert_isa_ilc(I,T):- 
  skipped_table_call(must((assert_isa_ilc_unchecked(I,T),!,show_call_failure(isa_backchaing(I,T))))).
  

assert_isa_ilc_unchecked(I,T):- is_ftCompound(I),is_non_skolem(I),!,must((get_functor(I,F),assert_compound_isa(I,T,F))),!.
assert_isa_ilc_unchecked(I,tCol):- must(show_call(decl_type(I))).
assert_isa_ilc_unchecked(I,ttFormatType):- must(show_call(define_ft(I))).
assert_isa_ilc_unchecked(I,T):-   (( \+(isa(I,_)),\+(a(tCol,I))) -> add(tCountable(I)) ; true),
  w_tl([t_l:infSkipArgIsa,t_l:infSkipFullExpand],assert_hasInstance(T,I)).

assert_compound_isa(I,_,_):- is_ftCompound(I), I\=resultIsaFn(_),glean_pred_props_maybe(I),fail.
assert_compound_isa(I,T,_):- hotrace(chk_ft(T)),dmsg(once(dont_assert_is_ft(I,T))),rtrace((chk_ft(T))).
%assert_compound_isa(I,T,F):- is_Template(I),!,assert_hasInstance(T,I),show_call(mpred_add(resultIsa(I,T))),assert_hasInstance(T,resultIsaFn(F)).
assert_compound_isa(I,T,F):- ignore((is_Template(I),w_tl(infConfidence(vWeak),assert_predArgTypes(I)))),
   hooked_asserta(isa(I,T)),assert_hasInstance(T,I),show_call(mpred_add(resultIsa(F,T))),assert_hasInstance(T,resultIsaFn(F)),!.
   

get_mpred_arg(N,_:C,E):-!,is_ftCompound(C),arg(N,C,E).
get_mpred_arg(N,C,E):-!,is_ftCompound(C),arg(N,C,E).

is_Template(I):- get_mpred_arg(_,I,Arg1),a(tCol,Arg1).


assert_isa_reversed(T,I):-assert_isa(I,T).

% ================================================
% assert_isa HOOKS
% ================================================
%OLD lmconf:decl_database_hook(_,genls(_,_)):-retractall(a(_,isa,_,_)),retractall(a(_,genls,_,_)).
%OLD lmconf:decl_database_hook(change(assert,_),DATA):-into_mpred_form(DATA,O),!,O=isa(I,T),hotrace(doall(assert_isa_hooked(I,T))).
%OLD lmconf:decl_database_hook(change(assert,_),isa(I,T)):- assert_hasInstance(T,I),fail.

%OLD lmconf:decl_database_hook(change( retract,_),isa(I,T)):-doall(db_retract_isa_hooked(I,T)).

assert_isa_hooked(A,_):-retractall(a(cache_I_L,isa,A,_)),fail.
assert_isa_hooked(F,T):- a(ttPredType,T),decl_mpred(F,T),fail.
assert_isa_hooked(I,T):- assert_isa(I,T).
assert_isa_hooked(I,T):- not(ground(assert_isa(T))),!, trace_or_throw(not(ground(assert_isa(I,T)))).
assert_isa_hooked(I,T):- assert_hasInstance(T,I),fail.

assert_isa_hooked(T,tCol):-!,decl_type(T),!.
assert_isa_hooked(T,ttFormatType):-!,define_ft(T),!.
assert_isa_hooked(Term,tPred):-!,decl_mpred(Term).
assert_isa_hooked(Term,prologHybrid):-!,decl_mpred_hybrid(Term).
assert_isa_hooked(Term,prologDynamic):-!,export(Term).
assert_isa_hooked(Term,prologPTTP):-!,decl_mpred_hybrid(Term,prologPTTP).
assert_isa_hooked(Term,prologKIF):-!,decl_mpred_hybrid(Term,prologKIF).
assert_isa_hooked(I,_):- I\=prologHybrid(_),glean_pred_props_maybe(I),fail.
assert_isa_hooked(food5,tWeapon):-trace_or_throw(assert_isa(food5,tWeapon)).

% assert_isa_hooked(I,T):- motel:defconcept(I,isAnd([lexicon,T])).
% assert_isa_hooked(I,T):- motel:defprimconcept(I,T).
% assert_isa_hooked(I,T):-dmsg((told(assert_isa(I,T)))).


%OLD lmconf:decl_database_hook(change(assert,_),isa(I,T)):- doall(assert_isa_hooked_after(I,T)).

assert_isa_hooked_after(F,T):-a(ttPredType,T),!,decl_mpred(F,T).
assert_isa_hooked_after(_,tCol):-!.
assert_isa_hooked_after(_,ttFormatType):-!.
%assert_isa_hooked_after(I,T):- ttTemporalType(T),!,assert_isa_hooked_creation(I,T).

%assert_isa_hooked_after(I,T):- assert_isa_hooked_creation(I,T).

/*
assert_isa_hooked_after(I,T):- not(completelyAssertedCollection(T)),impliedSubClass(T,ST),completelyAssertedCollection(ST),assert_isa(I,ST).
:- was_export(impliedSubClass/2).
impliedSubClass(T,ST):-ground(T:ST),is_known_false(genls(T,ST)),!,fail.
impliedSubClass(T,ST):-predicate_property(transitive_subclass(T,ST),_),!,call_tabled(isa(T,ST),transitive_subclass(T,ST)).
*/

% one of 4 special cols
% assert_isa_hooked_creation(I,T):- ttTemporalType(T),!,call_after_mpred_load((create_instance(I,T,[]))).
% sublass of 4 special cols
% assert_isa_hooked_creation(I,T):- doall((ttTemporalType(ST),impliedSubClass(T,ST),call_after_mpred_load((create_instance(I,ST,[isa(T)]))))).


% :- asserta((lmconf:isa(I,C):-loop_check(isa_backchaing(I,C)))).
lmconf:module_local_init:- asserta(('$toplevel':isa(I,C):-lmconf:isa(I,C))).

mpred_types_loaded.

% ISA QUERY
lmconf:module_local_init:- asserta_if_new((system:goal_expansion(ISA,GO) :- \+ t_l:disable_px, \+current_predicate(_,ISA),
  once((is_ftCompound(ISA),was_isa(ISA,I,C))),t_l:is_calling,show_call(GO=no_repeats(isa(I,C))))).
% ISA GOAL
% mpred_system_goal_expansion(G,GO):-G\=isa(_,_),was_isa(G,I,C),GO=isa(I,C).
% ISA EVER
%mpred_term_expansion(G,GO):-  \+ t_l:disable_px,was_isa(G,I,C),GO=isa(I,C).

lmconf:module_local_init:-mpred_add(tCol(tCol)).
lmconf:module_local_init:-mpred_add(tCol(ttPredType)).


:- source_location(S,_),forall(source_file(H,S),(functor(H,F,A),export(F/A),module_transparent(F/A))).

:- lmconf:module_local_init.


