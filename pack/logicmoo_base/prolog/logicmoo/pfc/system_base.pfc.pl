/* <module>
% =============================================
% File 'system_base.pfc'
% Purpose: Agent Reactivity for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface' 1.0.0
% Revision: $Revision: 1.9 $
% Revised At: $Date: 2002/06/27 14:13:20 $
% =============================================
%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
%
% props(Obj,[height(ObjHt)]) == t(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == t(height(Obj,ObjHt)).
% pain(Obj,[height(ObjHt)]) == prop_set(height,Obj,ObjHt,...) == ain(height(Obj,ObjHt))
% [pdel/pclr](Obj,[height(ObjHt)]) == [del/clr](height,Obj,ObjHt) == [del/clr]svo(Obj,height,ObjHt) == [del/clr](height(Obj,ObjHt))
% keraseall(AnyTerm).
%
%                      ANTECEEDANT                                   CONSEQUENT
%
%         P =         test nesc true                         assert(P),retract(~P) , enable(P).
%       ~ P =         test nesc false                        assert(~P),retract(P), disable(P)
%
%   ~ ~(P) =         test possible (via not impossible)      retract( ~(P)), enable(P).
%  \+ ~(P) =         test impossiblity is unknown            retract( ~(P))
%   ~ \+(P) =        same as P                               same as P
%     \+(P) =        test naf(P)                             retract(P)
%
% Dec 13, 2035
% Douglas Miles
*/


:- mpred_unload_file.

:- if(current_predicate(initEnvironment/0)).
% :- must(initEnvironment).
:- endif.
:- '$set_source_module'(baseKB).

:- set_prolog_flag(logicmoo_speed,3).

:- set_prolog_flag(logicmoo_speed, 0).
:- set_prolog_flag(logicmoo_safety, 2).
:- set_prolog_flag(logicmoo_debug, 2).
:- set_prolog_flag(unsafe_speedups, false).

:- set_module(baseKB:class(development)).

:- dynamic(baseKB:col_as_isa/1).
:- dynamic(baseKB:col_as_unary/1).
:- dynamic(baseKB:col_as_static/1).

col_as_isa(tCol).
col_as_isa(tSet).

col_as_isa(ttTypeType).
col_as_isa(ttRelationType).
col_as_isa(ttExpressionType).
col_as_isa(ttItemType).
col_as_isa(ttAgentType).
col_as_isa(ttMicrotheoryType).
col_as_isa(ttRegionType).
col_as_isa(ttValueType).
col_as_isa(ttTopicType).
col_as_isa(ttSituationType).
col_as_isa(ttActionType).
col_as_isa(ttEventType).
col_as_isa(ttSpatialType).
col_as_isa(ttTemporalType).

%denotesTypeType(FT,CT)==>prefered_collection(FT,CT).
%prefered_collection(ftSubLString,ftString).
%prefered_collection(rtCycLPredicator,tPred).


:- set_prolog_flag(lm_expanders,true).
% :- set_prolog_flag(read_attvars,false).
%:- set_prolog_flag(mpred_te,true).

:- set_prolog_flag(logicmoo_motel,false).

% :- '$set_source_module'(baseKB).
% :- defprimconcept(naf(tDeleted),tExisting).
:- abolish(isa,2).

do_and_undo(A,U):-atom(A),atom_concat('assert',Suffix,A),!,atom_concat('delete',Suffix,U),current_predicate(U/_).
do_and_undo(A,U):-atom(A),atom_concat('def',_,A),atom_concat('un',A,U),current_predicate(U/_).
do_and_undo(A,U):-strip_module(A,M,P),compound(P),P=..[F|ARGS],lookup_u(do_and_undo(F,UF)),UA=..[UF|ARGS], U = (M:UA).
ll:- listing([isa/2,mtCycL/1,col_as_unary,col_as_isa,tRRP2/1,tRR/1,tRRP/1]).


:- asserta_if_new(baseKB:mtCycL(baseKB)).

ttTypeType(col_as_isa).
ttTypeType(col_as_unary).
ttTypeType(col_as_static).
col_as_unary(col_as_isa).
col_as_unary(col_as_unary).
col_as_unary(col_as_static).
rtQuotedPred(argsQuoted).
col_as_unary(mtProlog).
col_as_unary(mtExact).
col_as_unary(mtCycL).

% :- rtrace((ain_expanded(tCol(tCol)))).

%prologHybrid(C)==>{must(callable(C))}.
%pfcControlled(C)==>{must(callable(C))}.
typeCheckDecl(prologHybrid(C),callable(C)).
typeCheckDecl(pfcControlled(C),callable(C)).

% :- break.

tSet(rtQuotedPred).
ttRelationType(rtQuotedPred).


%:- start_rtrace,trace.
:- ain_expanded(baseKB:mtCycL(baseKB)).
col_as_isa(tSet).
col_as_isa(ttSpatialType).

tSet(ttRelationType).
% ~ ttRelationType(col_as_unary).

col_as_isa(ttRelationType).
%col_as_isa(completelyAssertedCollection).

rtQuotedPred(completeExtentAsserted).
col_as_unary(completelyAssertedCollection).

%:- rtrace.
% rtQuotedPred(P)==> ~tCol(P).
col_as_unary(Col)==>tCol(Col).
col_as_isa(Col)==>tCol(Col).
%:- nortrace.
%:-break.



==>((ttRelationType(isEach(
                  prologBuiltin,
                  prologDynamic,
                  prologHybrid,
                  prologIsFlag,
                  prologKIF,
                  prologListValued,
                  prologMultiValued,
                  prologNegByFailure,
                  prologOrdered,
                  prologPTTP,
                  prologSideEffects,
                  prologSingleValued,

                  rtAvoidForwardChain,
                  predCanHaveSingletons,

                  pfcControlled,
                  pfcWatched,
                  pfcCreates,
                  pfcNegTrigger,
                  pfcPosTrigger,
                  pfcBcTrigger,
                  pfcRHS,
                  pfcLHS,
                  pfcCallCodeBody,
                  pfcCallCodeTst,
                  pfcMustFC,
                  pfcDatabaseTerm)))).


ttRelationType(P)==>(tCol(P),completelyAssertedCollection(P),completeExtentAsserted(P)).

% ((prologHybrid(C),{must(callable(C)),get_functor(C,F,A),C\=F}) ==> arity(F,A)).


%% t(?Collection, ?VALUE1) is semidet.
%
% Completely Asserted Collection.
%

==> t(completeExtentAsserted,pm).
==> t(completeExtentAsserted,functorIsMacro).
==> t(completelyAssertedCollection,tMicrotheory).
==> t(completelyAssertedCollection,mtCycL).

:-assert((t(T,I):- cwc, I==T,completelyAssertedCollection==I,!)).
:-assert((t(T,I):- cwc, I==T,completeExtentAsserted==I,!)).
:-assert((t(T,I):- ((cwc, I==T,ttExpressionType==I,!,fail)))).

completeExtentAsserted(pm).
completeExtentAsserted(functorIsMacro).
completelyAssertedCollection(tMicrotheory).
completelyAssertedCollection(mtCycL).



% :- assert_if_new((isa(I,T):- cwc, visit_isa(I,T))).

% :- break.

genls(ttRelationType,completelyAssertedCollection).

:- do_gc.

%:- set_fileAssertMt(baseKB).

:- dynamic(baseKB:agent_call_command/2).
:- export(baseKB:agent_call_command/2).
:- system:import(baseKB:agent_call_command/2).


:- dynamic(decided_not_was_isa/2).
:- kb_dynamic(baseKB:mtCycL/1).
:- kb_dynamic(baseKB:mtExact/1).
:- kb_dynamic(baseKB:predicateConventionMt/2).
:- dynamic(baseKB:mtCycL/1).
:- dynamic(baseKB:mtExact/1).
:- dynamic(baseKB:predicateConventionMt/2).


:- listing(spft/3).
baseKB:mtCycL(baseKB).
:- mpred_run.
%baseKB:mtExact(baseKB).

:- nortrace.

:-  abolish(yall:'/' / 2).

% :- expand_file_search_path(pack(logicmoo_nlu/prolog/pldata),X),exists_directory(X),!,assert_if_new(user:file_search_path(pldata,X)).

%^ :- ensure_loaded(logicmoo(logicmoo_plarkc)).




%:- rtrace.
:- dynamic(mpred_mark/3).
:- kb_dynamic(mpred_mark/3).
%:- nortrace.


tAtemporalNecessarilyEssentialCollectionType(ttModule).

/*
:- dynamic(collectionConventionMt/2).
:- kb_dynamic(collectionConventionMt/2).
tAtemporalNecessarilyEssentialCollectionType(ANECT)==> collectionConventionMt(ANECT,baseKB).
*/

tAtemporalNecessarilyEssentialCollectionType(ANECT)==>
       decontextualizedCollection(ANECT).

tAtemporalNecessarilyEssentialCollectionType(ANECT)==> 
        collectionConventionMt(ANECT,baseKB).


:- dynamic(ttModule/1).
:- dynamic(marker_supported/2).
:- dynamic(pass2/0).
:- dynamic(sometimesSlow/0).
:- dynamic(sometimesBuggy/0).
:- dynamic(sometimesUseless/0).
:- kb_dynamic(ttModule/1).
:- kb_dynamic(marker_supported/2).
:- kb_dynamic(pass2/0).
:- kb_dynamic(sometimesSlow/0).
:- kb_dynamic(sometimesBuggy/0).
:- kb_dynamic(sometimesUseless/0).


% NEVER (P/mpred_non_neg_literal(P) ==> { remove_negative_version(P) } ).

:- dynamic(mpred_mark_C/1).
:- kb_dynamic(mpred_mark_C/1).
:- kb_dynamic(tCol/1).

:- kb_dynamic(subFormat/2).

:- kb_dynamic(singleValuedInArg/2).
:- kb_dynamic(ptReformulatorDirectivePredicate/1).
:- kb_dynamic(support_hilog/2).
:- kb_dynamic(mpred_undo_sys/3).
:- kb_dynamic(arity/2).


:- dynamic(arity/2).
:- abolish(system:arity,2).
:- system:import(arity/2).
:- dynamic(disjointWith/2).
:- dynamic(genlsFwd/2).
:- kb_dynamic(arity/2).
:- kb_dynamic(disjointWith/2).
:- kb_dynamic(genlsFwd/2).
arity(comment,2).

% prologHybrid(arity/2).

:- begin_pfc.
:- sanity(get_lang(pfc)).
:- set_file_lang(pfc).
% :- mpred_ops.

arity(alwaysGaf,1).
alwaysGaf(alwaysGaf).
alwaysGaf(pfcRHS).
alwaysGaf(pfcLHS).


% ttExpressionType(A)/atom(A)==> ~tIndividual(A),tCol(A),{decl_type(A), kb_dynamic(A/1)}.
tSet(A)/atom(A)==> ~tIndividual(A),tCol(A),{decl_type(A), kb_dynamic(A/1)}.
% tCol(C)/(\+ never_isa_syntax(C))==>{decl_as_isa(C)}.
:- mpred_notrace_exec.

% tCol(C)/atom(C)==> functorDeclares(C), ~tRelation(C),{nop(decl_type_unsafe(C)), nop(kb_dynamic(C/1)),\+ ttExpressionType(C)},tSet(C).

/*
Unneeded yet

ttExpressionType(C)==>col_as_unary(C).
col_as_unary(C)==> \+ col_as_isa(C).
col_as_isa(C)==> \+ col_as_unary(C).
col_as_isa(C)/( is_never_type(C) ; decided_not_was_isa(C,W)) ==> (conflict((col_as_isa(C)/( decided_not_was_isa(C,W);is_never_type(C))))).
*/

tCol(tCol).
tCol(tPred).
tCol(tFunction).
tCol(tRelation).
tCol(ttTemporalType).
tCol(ttExpressionType).
~tCol(functorDeclares).
functorDeclares(ttModule).

ttExpressionType(ftList(ftInt)).

%:- sanity((fix_mp(clause(assert,sanity),arity(apathFn,2),M,O),M:O=baseKB:arity(apathFn,2))).

:- dynamic(ttRelationType/1).

arity(xyzFn,4).
arity(Prop,1):- cwc, clause_b(ttRelationType(Prop)).
arity(prologSingleValued,1).
arity(meta_argtypes,1).
arity(isKappaFn,2).
arity(isInstFn,1).
arity(is_never_type,1).
arity(ftListFn,1).
arity(arity,2).
arity(argsIsa, 2).
arity(argIsa, 3).
arity(apathFn,2).
arity('<=>',2).
%arity('$VAR',_).
arity(F,A):- cwc, is_ftNameArity(F,A), current_predicate(F/A),A>1.
arity(F,1):- cwc, is_ftNameArity(F,1), current_predicate(F/1), (col_as_unary(F);col_as_isa(F)), \+((call((dif:dif(Z,1))), arity(F,Z))).

% mtCycL(baseKB).

tCol(ttModule).
arity(tCol,1).
tCol(ftAssertable).
tCol(ftCallable).
tCol(ftAskable).
tCol(tRelation).
tCol(ftListFn(Atom)):-callable(Atom),tCol(Atom).
ftSpec(ftListFn(Atom)):-callable(Atom),ftSpec(Atom).
ttExpressionType(ftListFn(Atom)):-callable(Atom).

tSet(ftListFn(Atom)):-callable(Atom),!,fail.

ttExpressionType(ftAssertable).
ttExpressionType(ftCallable).
ttExpressionType(ftAskable).
tCol(ftString).
tCol(ftAtom).
tCol(ftProlog).
tCol(rtAvoidForwardChain).

tSet(ttModule,mudToCyc('MicrotheoryType')).

arity(argsQuoted,1).
arity(quasiQuote,1).
argsQuoted(spft).

:-call(asserta_if_new, baseKB:mtCycL(baseKB)).

% this mean to leave terms at EL:  foo('xQuoteFn'([cant,touch,me])).

quasiQuote('xQuoteFn').

argsQuoted(argsQuoted).

argsQuoted('loop_check_term').
argsQuoted('loop_check_term_key').
argsQuoted('xQuoteFn').
argsQuoted('$VAR').

argsQuoted(ain).
argsQuoted(meta_argtypes).
argsQuoted(ttFormated).
argsQuoted(ruleRewrite).
argsQuoted(mpred_action).
argsQuoted(ain).
argsQuoted(mpred_rem).
argsQuoted(added).
argsQuoted(call).
argsQuoted(call_u).
argsQuoted(member).
argsQuoted(=..).
argsQuoted({}).
argsQuoted(second_order).
argsQuoted(ftSpec).
argsQuoted(vtActionTemplate).
% argsQuoted((':-')).

:- dynamic((==>)/2).
:- kb_dynamic((==>)/2).
%doing_slow_rules,
%:-rtrace(ain(((prologBuiltin(F),{atom(F)},arity(F,A),{sanity(integer(A))})==>{make_builtin(F/A)}))).
%((prologBuiltin(P),{compound(P),get_arity(P,F,A)},arity(F,A),{sanity(integer(A))})==>{make_builtin(F/A)}).


meta_argtypes(support_hilog(tRelation,ftInt)).

:- dynamic(codeTooSlow/0).

((codeTooSlow,((tPred(F),
 arity(F,A)/
  (is_ftNameArity(F,A),A>1, 
      \+ prologBuiltin(F), 
      % sanity(mpred_must(\+ arity(F,1))),
      sanity(mpred_must(\+ tCol(F)))))) )
   ==> (~(tCol(F)),support_hilog(F,A))).

:- kb_dynamic(support_hilog/2).


((codeTooSlow,(support_hilog(F,A)
  /(is_ftNameArity(F,A),
    \+ is_static_predicate(F/A), \+ prologDynamic(F)))) ==>
   (hybrid_support(F,A), 
    {% functor(Head,F,A) ,Head=..[F|TTs], TT=..[t,F|TTs],
    %  (CL = (Head :- cwc, call(second_order(TT,CuttedCall)), ((CuttedCall=(C1,!,C2)) -> (C1,!,C2);CuttedCall)))
    CL = arity(F,A)
    },
   (CL))).


%:- kb_dynamic(hybrid_support/2).
%prologBuiltin(resolveConflict/1).

:- dynamic(bt/2).
:- kb_dynamic(bt/2).
bt(P,_)/nonvar(P) ==> (P:- mpred_bc_only(P)).

((sometimesUseless,prologHybrid(F),arity(F,A))==>hybrid_support(F,A)).
(hybrid_support(F,A))==>prologHybrid(F),arity(F,A).

==>((mpred_mark(pfcRHS,F,A)/(A\=0)) ==> {kb_dynamic(F/A)}).
% ((mpred_mark(_,F,A)/(A\=0)) ==> {shared_multifile(F/A)}).


(pass2,pfcControlled(X)/get_pifunctor(X,C))==>({kb_dynamic(C),get_functor(C,F,A)},arity(F,A),pfcControlled(F),support_hilog(F,A)).
%pfcControlled(X)/get_pifunctor(X,C)==>({shared_multifile(C),get_functor(C,F,A)},arity(F,A),pfcControlled(F),support_hilog(F,A)).

(pass2,prologHybrid(X)/get_pifunctor(X,C))==>({\+ is_static_predicate(C), shared_multifile(C),get_functor(C,F,A)},arity(F,A),prologHybrid(F)).
%prologHybrid(X)/get_pifunctor(X,C)==>({\+ is_static_predicate(C), kb_dynamic(C),get_functor(C,F,A)},arity(F,A),prologHybrid(F)).


%(pass2,prologBuiltin(X)/get_pifunctor(X,C))==>({nop(decl_mpred_prolog(C)),get_functor(C,F,A)},arity(F,A),prologBuiltin(F)).

% prologDynamic(X)/get_pifunctor(X,C)==>({kb_dynamic(C),decl_mpred_prolog(C),get_functor(C,F,A)},arity(F,A),prologDynamic(F)).

pfcMustFC(F) ==> pfcControlled(F).



pfcControlled(C)==>prologHybrid(C).

:- dynamic(hybrid_support/2).
:- dynamic(type_checking/1).
:- kb_dynamic(hybrid_support/2).
:- kb_dynamic(type_checking/1).

/*
% catching of misinterpreations
type_checking, mpred_mark(pfcPosTrigger,F,A)==>{warn_if_static(F,A)}.
type_checking, mpred_mark(pfcNegTrigger,F,A)==>{warn_if_static(F,A)}.
type_checking, mpred_mark(pfcBcTrigger,F,A)==>{warn_if_static(F,A)}.
*/

%'==>'((mpred_mark(S1, F, A)/(ground(S1),is_ftNameArity(F,A))==>(tCol(S1),arity(F,A), ==>(isa(F,S1))))).
% ((mpred_mark(S1, F, A)/(ground(S1),is_ftNameArity(F,A))==>(tCol(S1),arity(F,A),t(S1,F)))).
((mpred_mark(S1, F, A)/(ground(S1),is_ftNameArity(F,A),A==1)==>((tCol(S1),arity(F,A),{ASSERT=..[S1,F]},ASSERT)))).

mpred_mark(pfcPosTrigger,F, A)/(\+ ground(F/A))==>{trace_or_throw(mpred_mark(pfcPosTrigger,F, A))}.
mpred_mark(pfcPosTrigger,F, A)==>marker_supported(F,A).
mpred_mark(pfcNegTrigger,F, A)==>marker_supported(F,A).
mpred_mark(pfcBcTrigger,F, A)==>marker_supported(F,A).
mpred_mark(pfcLHS,F, A)==>arity(F,A),functorIsMacro(F).
mpred_mark(pfcRHS,F, A)==>
  {functor(P,F,A),make_dynamic(P),kb_dynamic(P),
    create_predicate_istAbove(abox,F,A)},
    marker_supported(F,A).

mpred_mark(pfcCallCode,F, A)/predicate_is_undefined_fa(F,A)
    ==> marker_supported(F,A).
/*
mpred_mark(pfcCallCodeTst,F, A)/predicate_is_undefined_fa(F,A)
    ==> marker_supported(F,A).
*/


tCol(P)==>functorIsMacro(P).
functorIsMacro(functorIsMacro).

% (marker_supported(F,A))==>(prologHybrid(F),hybrid_support(F,A)).
%mpred_mark(pfcPosTrigger,F,A)/(integer(A),functor(P,F,A)) ==> pfcTriggered(F/A),afterAdding(F,lambda(P,mpred_enqueue(P,(m,m)))).
%mpred_mark(pfcNegTrigger,F,A)/(integer(A),functor(P,F,A)) ==> pfcTriggered(F/A), afterRemoving(F,lambda(P,mpred_enqueue(~P,(m,m)))).

/*
mpred_mark(pfcRHSF,1)/(fail,atom(F),functor(Head,F,1), 
 \+ argsQuoted(F),
 \+ prologDynamic(F),
 \+ ~(tCol(F)),
 \+ specialFunctor(F),
 \+ predicate_property(Head,built_in))==>completelyAssertedCollection(F).
*/
% mpred_mark(Type,F,A)/(integer(A),A>1,F\==arity,Assert=..[Type,F])==>arity(F,A),Assert.

mpred_mark_C(G) ==> {map_mpred_mark_C(G)}.
map_mpred_mark_C(G) :-  map_literals(lambda(P,(get_functor(P,F,A),ain([pfcControlled(F),arity(F,A)]))),G).
mpred_mark(pfcRHS,F,A)/(is_ftNameArity(F,A),F\==arity)==>(tPred(F),arity(F,A),pfcControlled(F)).

% (hybrid_support(F,A) ==>{\+ is_static_predicate(F/A), must(kb_dynamic(F/A))}).


%:- meta_predicate(mp_test_agr(?,+,-,*,^,:,0,1,5,9)).
%mp_test_agr(_,_,_,_,_,_,_,_,_,_).
%:- mpred_test(predicate_property(mp_test_agr(_,_,_,_,_,_,_,_,_,_),meta_predicate(_))).
% becomes         mp_test_agr(+,+,-,?,^,:,0,1,0,0)


((marker_supported(F,A)/is_ftNameArity(F,A),prologHybrid(F)))==>hybrid_support(F,A).
((marker_supported(F,A)/(is_ftNameArity(F,A),correct_module(abox,_,F,A,Mt),Mt\=abox,
   \+ predicateConventionMt(F,_), mtExact(Mt))))==>predicateConventionMt(F,Mt).

% hybrid_support(F,A) ==>{ nop(must(kb_dynamic(F/A)))}.

% mtExact(Mt)/module_predicate(Mt,F,A)==>predicateConventionMt(F,Mt),arity(F,A).


% NAUTs
tSet(tUnreifiableFunction,
genls(tFunction),
comment("
A specialization of Function-Denotational instances of which are such that their values 
are not reified in the Cyc system. More precisely, an instance of UnreifiableFunction 
is such that closed \"NA[R|U]Ts\" (see CycLNonAtomicTerm) 
built from its standard CycL name are _not_ instances of #$HLReifiedDenotationalTerm. 
Constrast with ReifiableFunction. Usually it is more efficient to make functions reifiable; 
but it is not desirable to reify every non-atomic term, such as those built from (names of) 
instances of MathematicalFunctionOnScalars. For example, it would be cumbersome to
 reify every term of the form (Inch N) that happened to appear in a CycL assertion."
)).

% NARTs
tSet(tReifiableFunction,comment("A specialization of Function-Denotational. Each instance of ReifiableFunction is denoted by a 
CycL constant that can stand in the 0th (or \"arg0\") position in a CycLReifiableNonAtomicTerm (q.v.). For example, GovernmentFn is a 
reifiable function, so the term `(GovernmentFn France)' is a reifiable non-atomic term (or \"NAT\"). And since this particular 
term actually _is_ reified in the Cyc Knowledge Base, it is, more specifically, a CycLNonAtomicReifiedTerm 
(or \"NART\"). The NART `(GovernmentFn France)' is treated more or less the same as if it were a CycL constant 
(named, say, `GovernmentOfFrance'). Similary, the constant for GovernmentFn can be applied to the constant (or other reified or 
reifiable term) for _any_ instance of GeopoliticalEntity to form a reifiable NAT that denotes that region's government; and should 
 this NAT appear in a sentence that is asserted to the KB, it will thereby become a NART. Note, however, that not all NATs are such that it 
is desireable that they should become reified (i.e. become NARTs) if they appear in assertions; for more on this see UnreifiableFunction."
),
genls(tFunction)).


tSet(vtLinguisticObject).
vtLinguisticObject(vtVerb).

tReifiableFunction(aVerbFn).
conceptuallyRelated("go",actMove).
resultIsa(aVerbFn(ftString),vtVerb).



/*
genlMt(Mt1,Mt2),mtCycL(Mt1),mtProlog(Mt2) ==> 
  {maybe_add_module_import(Mt1,Mt2)}.
*/
/*
genlMt(Mt1,Mt2),mtProlog(Mt1),mtCycL(Mt2) ==> 
  {trace_or_throw(oddly_genlMt(Mt1,Mt2))}.
*/

% baseKB:mtCycL(baseKB).

baseKB:predicateConventionMt(predicateConventionMt,baseKB).
baseKB:predicateConventionMt(collectionConventionMt,baseKB).

predicateConventionMt(genlMt,baseKB).
predicateConventionMt(regression_test,baseKB).

functorDeclares(tSet).
tSet(tMicrotheory,mudToCyc('Microtheory')).

baseKB:collectionConventionMt(tMicrotheory,baseKB).
collectionConventionMt(mtCycL,baseKB).
collectionConventionMt(mtExact,baseKB).
collectionConventionMt(Col,Where) ==> predicateConventionMt(Col,Where).

% mtExact(Mt)==>{kb_dynamic(Mt)}.


mtProlog(Mt),predicateConventionMt(F,Mt)/(Mt\==baseKB)==>prologBuiltin(F).

% genlsFwd(Sub,Super)==> (isa(I,Super) :- isa(I,Sub)). 
% :- ain_expanded((genlsFwd(Sub,Super)==> (t(Sub,I) ==> t(Super,I)))).

ttModule(M)==>tCol(M).

ttModule(MtType)==>genls(MtType,tMicrotheory).
ttModule(mtProlog).

:- sanity(get_lang(pfc)).

tCol(Decl)==>functorDeclares(Decl).

:- sanity(( fully_expand(cuz,==>((ttModule(mtCycL,
  comment("yada....................."),
  genlsFwd(tMicrotheory)))),
  OO),dmsg(full_transform=OO),OO=(_,_))).

% :- rtrace((trace,fully_expand(zzz,==>ttModule(mtCycL777One,comment("hi there"),genlsFwd(tMicrotheory)),O))),nl,writeq(O),nl,notrace.
% :- break.

:- ain_expanded(ttModule(mtCycL,
  comment("mtCycL(?Mt) Mts like baseKB that contain mainly assertions written in CycL"),
  genlsFwd(tMicrotheory))).

:- ain_expanded(
 ttModule(mtProlog,comment("Real Prolog modules loaded with :-use_module/1 such as 'lists' or 'apply'"),
  genls(tMicrotheory))).

:- sanity(arity(ttModule,1)).
:- sanity(functorDeclares(ttModule)).
:- sanity(\+ arity(ttModule,3)).
:- sanity(\+ predicate_property(ttModule(_,_,_),_)).

:- ain_expanded(ttModule(mtProlog,comment("Builtin Prolog code modules such as 'lists' or 'apply' and PFC system like 'mpred_loader' or 'mpred_type_wff'"),
  genls(mtCore))).


% ttModule(mtLocal,comment("mtLocal(?Mt) is always scoped underneath baseKB")).

ttModule(mtGlobal,comment("mtGlobal(?Mt) states the Mt is always findable during inheritance")).
mtGlobal(baseKB).
mtGlobal(system).

ttModule(mtExact,
  comment("mtExact(?Mt) states that all predicates the Mt specifies should be called and asserted using only this ?Mt.")).
mtExact(baseKB).
mtExact(lmcache).
mtExact(t_l).
mtExact(Mt)==> mtGlobal(Mt).


ttModule(mtCore,comment("mtCore(?Mt) states Mt specified is builtin")).
mtCore(user).
mtCore(everythingPCS).
mtCore(inferencePCS).
genls(mtCore,tMicrotheory).


mtCycL(O)==>({find_and_call(ensure_abox(O))},~mtProlog(O),\+ mtProlog(O)).

:- sanity(functorDeclares(ttModule)).
:- sanity(arity(ttModule,1)).

:- sanity(\+ arity(ttModule,3)).
:- sanity(\+ predicate_property(ttModule(_,_,_),_)).


:- dynamic(nondet/0).
:- kb_dynamic(nondet/0).

/*
% These rules break the loader 
% to test 
% swipl -f sanity_base/mt_01.pl
% whereas this would work: 
% swiplb -f sanity_base/mt_01.pl

*/
{module_property(Mt,class(user)),
   (atom_concat('common_logic_',_,Mt);atom_concat('logicmoo_',_,Mt);atom_concat('mpred_',_,Mt))} 
    ==>  mtProlog(Mt).
{module_property(Mt,class(microtheory))} ==> mtCycL(Mt).
{module_property(Mt,class(library))} ==> mtProlog(Mt).
{module_property(Mt,class(system))} ==> mtProlog(Mt).



% TODO: stop next line from killing mtCycL(baseKB)
%  (tMicrotheory(Mt), ~ mtCycL(Mt)) <==> mtProlog(Mt).

% mtCycL(Mt)==>{skip_user(Mt),set_prolog_flag(Mt:unknown,warning)},genlMt(Mt,baseKB).
codeRule(mtGlobal(Mt)==>genlMt(baseKB,Mt)).

baseKB:isRegisteredCycPred(apply,maplist,3).

/*
(genlMt(Child,Parent), \+ mtCore(Child)) ==>
   {ignore((system:delete_import_module(Parent,user))),
    ignore((system:delete_import_module(Parent,Child))),
    system:add_import_module(Child,Parent,start)}.
*/

:- dynamic(baseKB:isRegisteredCycPred/3).
:- kb_dynamic(baseKB:isRegisteredCycPred/3).

/*
:- ((rtrace, dtrace)).

(({fail,current_module(Mt),
   predicate_property(Mt:P,defined), 
 \+ predicate_property(Mt:P,imported_from(_)),
 functor(P,F,A)})
  ==>baseKB:isRegisteredCycPred(Mt,F,A)).
*/

/* prolog_listing:listing */
% :- printAll(baseKB:isRegisteredCycPred/3).

% ~(tCol({})).

:- unload_file(library(yall)).



% Unneeded yet
% pass2


% :- ain(mpred_database_term(F,_,_)==> ~predicateConventionMt(F,_)).


:- ain((mpred_database_term(F,_,_)==> ~ predicateConventionMt(F,baseKB))).

% :- ain(((predicateConventionMt(F,abox),\+predicateConventionMt(F,baseKB)) ==> ~ predicateConventionMt(F,baseKB))).


/*

doRemoveMe ==> ~ removeMe(_,_).

removeMe(1,2).
removeMe(1,3).

doRemoveMe.



doRedelMe ==>  {redelMe(A,B)}, \+ redelMe(A,B).

redelMe(1,2).
redelMe(1,3).

doRedelMe.

:-listing(removeMe/2).
:-listing(redelMe/2).

:- dbreak.
*/

nondet.

% :- set_prolog_flag(dialect_pfc,false).
:- mpred_trace_exec.

% isa(I,C)==>{wdmsg(isa(I,C))}.


do_and_undo(mpred_post_exactly,mpred_remove_exactly).

%:- if( \+ flag_call(logicmoo_speed==true)).
%(((CI,{was_mpred_isa(CI,I,C)},\+ ~isa(I,C)) ==> actn(mpred_post_exactly(isa(I,C))))).
%:- endif.

:- abolish(system:arity,2).
:- system:import(arity/2).

:- mpred_notrace_exec.

