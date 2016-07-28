/* 
% ===================================================================
% File 'logicmoo_i_cyc_kb.pl'
% Purpose: Emulation of OpenCyc for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface.pl' 1.0.0
% Revision:  $Revision: 1.9 $
% Revised At:   $Date: 2002/06/27 14:13:20 $
% ===================================================================
% File used as storage place for all predicates which make us more like Cyc
% special module hooks into the logicmoo engine allow
% syntax to be recocogized via our CycL/KIF handlers 
%
% Dec 13, 2035
% Douglas Miles
*/
%:- module(tiny_kb,['TINYKB-ASSERTION'/5, 'TINYKB-ASSERTION'/6]).
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/plarkc/logicmoo_i_cyc_kb.pl
:- module(logicmoo_i_cyc_kb,
          [ addCycL/1,
            addCycL0/1,
            addCycL1/1,
            addTinyCycL/1,
            addTiny_added/1,
            as_cycl/2,
            atom_concatM/3,
            atom_concatR/3,
            call_el_stub/3,
            cycLToMpred/2,
            cycLToMpred0/2,
            % baseKB:cycPrepending/2,
            cyc_to_clif/2,
            cyc_to_clif_notify/2,
            cyc_to_mpred_idiom/2,
            cyc_to_mpred_idiom1/2,
            cyc_to_mpred_idiom_unused/2,
            cyc_to_mpred_sent_idiom_2/3,
            %baseKB:cyc_to_plarkc/2,
            finish_asserts/0,
            expT/1,
            %lmcache:isCycAvailable_known/0,
            %lmcache:isCycUnavailable_known/1,
            isF/1,
            isFT/1,
            isPT/1,
            isRT/1,
            isV/1,
            isVT/1,
            is_better_backchained/1,
            is_simple_arg/1,
            is_simple_gaf/1,
            isa_db/2,
            ist_tiny/2,
            kw_to_vars/2,
            label_args/3,
            list_to_ops/3,
            loadTinyKB/0,
            ltkb1/0,
            ltkb1_complete/0,
            make_el_stub/4,
            make_functor_h/3,
            make_kw_functor/3,
            make_kw_functor/4,
            maybe_ruleRewrite/2,
            mpred_postpend_type/2,
            mpred_prepend_type/2,
         %   baseKB:mpred_to_cyc/2,
            mwkb1/0,
            needs_canoncalization/1,
            needs_indexing/1,
            notFormatType/1,
            print_assertion/3,
            sent_to_conseq/2,
            tDressedMt/1,
            tEscapeFunction/1,
            tUndressedMt/1,
            tinyAssertion/3,
            tinyAssertion0/3,
            tinyKB/1,
            tinyKB/3,
            tinyKB1/1,
            tinyKB2/1,
            tinyKB_All/3,
            tinyKB_wstr/1,
            tiny_support/3,
            % vtUnreifiableFunction/1,
            wkb0/0,
            wkb01/0,
            wkb02/0,
            wkb2/0,
            wkbe/0
          ]).

:- op(500,fx,'~').
:- op(1050,xfx,('==>')).
:- op(1050,xfx,'<==>').
:- op(1050,xfx,('<-')).
:- op(1100,fx,('==>')).
:- op(1150,xfx,('::::')).
:- 
 system:((
 op(1199,fx,('==>')), 
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'))).


:- set_prolog_flag(lm_expanders,false).

:- dynamic logicmoo_i_cyc_kb:argIsa/3.
:- multifile logicmoo_i_cyc_kb:argIsa/3.
:- public logicmoo_i_cyc_kb:argIsa/3.
:- module_transparent logicmoo_i_cyc_kb:argIsa/3.

logicmoo_i_cyc_kb:argIsa(6,6,6):-trace_or_throw(logicmoo_i_cyc_kb:argIsa(6,6,6)).
argIsa(6,6,6):-trace_or_throw(argIsa(7,7,7)).
:- set_prolog_flag(lm_expanders,true).

:- compile_predicates([ logicmoo_i_cyc_kb:argIsa/3]).

% :- shared_multifile((  argGenl/3,argIsa/3,argQuotedIsa/3)).

:- dynamic((
  
        % argGenl/3,argIsa/3,argQuotedIsa/3,

        addTiny_added/1,
        baseKB:cycPrepending/2,
        baseKB:cyc_to_plarkc/2,
        lmcache:isCycUnavailable_known/1,
        baseKB:mpred_to_cyc/2)).

:- volatile(lmcache:isCycAvailable_known/0).

isa_db(I,C):-clause(isa(I,C),true).

:- dynamic((exactlyAssertedEL/4,exactlyAssertedEL/5,exactlyAssertedEL/6,exactlyAssertedEL/7)).
:- dynamic((exactlyAssertedEL_next/4,exactlyAssertedEL_next/5,exactlyAssertedEL_next/6,exactlyAssertedEL_next/7)).
:- dynamic((exactlyAssertedEL_first/4,exactlyAssertedEL_first/5,exactlyAssertedEL_first/6,exactlyAssertedEL_first/7)).
:- dynamic(assertedTinyKB_implies_first/4).
:- dynamic(assertedTinyKB_not_first/3).
:- dynamic((exactlyAssertedEL_first/5,exactlyAssertedEL_with_vars/5,exactlyAssertedEL_with_vars/6,assertedTinyKB_implies_Already/4)).


:- dynamic(baseKB:cycPrepending/2).
baseKB:cycPrepending(ft,'AssertedAssertion').
baseKB:cycPrepending(ft,'Assertion').
baseKB:cycPrepending(ft,'Atom').
baseKB:cycPrepending(ft,'AtomicAssertion').
baseKB:cycPrepending(ft,'AtomicSentence').
baseKB:cycPrepending(ft,'AtomicTerm').
baseKB:cycPrepending(ft,'Character').
baseKB:cycPrepending(ft,'Constant').
baseKB:cycPrepending(ft,'DeducedAssertion').
baseKB:cycPrepending(ft,'DenotationalTerm').
baseKB:cycPrepending(ft,'DenotationalTerm-Assertible').
baseKB:cycPrepending(ft,'DocumentationConstant').
baseKB:cycPrepending(ft,'Expression').
baseKB:cycPrepending(ft,'ExpressionAskable').
baseKB:cycPrepending(ft,'ExpressionAssertible').
baseKB:cycPrepending(ft,'GAFAssertion').
%baseKB:cycPrepending(ft,'GenericRelationFormula').
baseKB:cycPrepending(ft,'HLPrototypicalTerm').
baseKB:cycPrepending(ft,'IndeterminateTerm').
baseKB:cycPrepending(ft,'IndexedTerm').
baseKB:cycPrepending(ft,'InferenceDataStructure').
baseKB:cycPrepending(ft,'NonNegativeScalarInterval').
baseKB:cycPrepending(ft,'InferenceSupportedTerm').
baseKB:cycPrepending(ft,'KBDatastructure').
baseKB:cycPrepending(ft,'Keyword').
baseKB:cycPrepending(ft,'List').
baseKB:cycPrepending(ft,'NonAtomicReifiedTerm').
baseKB:cycPrepending(ft,'NonAtomicTerm').
baseKB:cycPrepending(ft,'NonAtomicTerm-Askable').
baseKB:cycPrepending(ft,'NonAtomicTerm-Assertible').
baseKB:cycPrepending(ft,'NonNegativeInteger').
baseKB:cycPrepending(ft,'NonVariableNonKeywordSymbol').
baseKB:cycPrepending(ft,'NonVariableSymbol').
/*
baseKB:cycPrepending(ft,'NonAtomicTerm-ClosedFunctor').
baseKB:cycPrepending(ft,'OpenDenotationalTerm').
baseKB:cycPrepending(ft,'OpenExpression').
baseKB:cycPrepending(ft,'OpenFormula').
baseKB:cycPrepending(ft,'OpenNonAtomicTerm').
baseKB:cycPrepending(ft,'OpenSentence').
baseKB:cycPrepending(ft,'ClosedAtomicSentence').
% baseKB:cycPrepending(ft,'ClosedAtomicTerm').
baseKB:cycPrepending(ft,'ClosedDenotationalTerm').
baseKB:cycPrepending(ft,'ClosedExpression').
baseKB:cycPrepending(ft,'ClosedFormula').
baseKB:cycPrepending(ft,'ClosedNonAtomicTerm').
baseKB:cycPrepending(ft,'ClosedSentence').
*/

baseKB:cycPrepending(ft,'PositiveInteger').
baseKB:cycPrepending(ft,'PropositionalSentence').
baseKB:cycPrepending(ft,'RealNumber').
baseKB:cycPrepending(ft,'ReifiableDenotationalTerm').
baseKB:cycPrepending(ft,'ReifiableNonAtomicTerm').
baseKB:cycPrepending(ft,'ReifiedDenotationalTerm').
baseKB:cycPrepending(ft,'RepresentedAtomicTerm').
baseKB:cycPrepending(ft,'RepresentedTerm').
baseKB:cycPrepending(ft,'RuleAssertion').
baseKB:cycPrepending(ft,'ScalarIntegralValue').
baseKB:cycPrepending(ft,'Sentence').
baseKB:cycPrepending(ft,'SentenceAskable').
baseKB:cycPrepending(ft,'SentenceAssertible').
baseKB:cycPrepending(ft,'String').
baseKB:cycPrepending(ft,'Symbol').

baseKB:cycPrepending(ft,'SupportDatastructure').
baseKB:cycPrepending(ft,'TheTerm').
baseKB:cycPrepending(ft,'TruthValueSentence').

baseKB:cycPrepending(v,'SingleEntry').
baseKB:cycPrepending(v,'SetTheFormat').

baseKB:cycPrepending(tt,'TransformationModuleSupportedCollection').
baseKB:cycPrepending(tt,'RemovalModuleSupportedCollection-Generic').

baseKB:cycPrepending(pt,'UnaryPredicate').
baseKB:cycPrepending(pt,'RemovalModuleSupportedPredicate-Generic').
baseKB:cycPrepending(pt,'RemovalModuleSupportedPredicate-Specific').
baseKB:cycPrepending(pt,'InferenceSupportedPredicate').
baseKB:cycPrepending(pt,'SentenceClosedPredicate').
baseKB:cycPrepending(pt,'TransformationModuleSupportedPredicate').
baseKB:cycPrepending(pt,'ArgGenlQuantityBinaryPredicate').


baseKB:cycPrepending(v,'Forward-AssertionDirection').
baseKB:cycPrepending(v,'Code-AssertionDirection').
baseKB:cycPrepending(v,'Backward-AssertionDirection').
baseKB:cycPrepending(vt,'AssertionDirection').

baseKB:cycPrepending(vt,'BinaryFunction').
baseKB:cycPrepending(vt,'UnreifiableFunction').
baseKB:cycPrepending(vt,'UnaryFunction').


baseKB:cycPrepending(tt,'InferenceSupportedCollection').

baseKB:cycPrepending(pt,A):-atom(A),atom_contains(A,'Predicate').



:- dynamic(baseKB:cyc_to_plarkc/2).


baseKB:cyc_to_plarkc('between', cycBetween).
baseKB:cyc_to_plarkc('forall', cycforAll).

baseKB:cyc_to_plarkc('equals', mudEquals).
baseKB:cyc_to_plarkc('termOfUnit',skolem ).


baseKB:cyc_to_plarkc('BaseKB', baseKB).
baseKB:cyc_to_plarkc(Was, baseKB):-tUndressedMt(Was).
baseKB:cyc_to_plarkc('NART', aNARTFn).
baseKB:cyc_to_plarkc('RelationAllExistsFn', aRelationAllExistsFn).
baseKB:cyc_to_plarkc('RelationExistsAllFn', aRelationExistsAllFn).
baseKB:cyc_to_plarkc('RelationExistsInstanceFn', aRelationExistsInstanceFn).
baseKB:cyc_to_plarkc('RelationInstanceExistsFn', aRelationInstanceExistsFn).
baseKB:cyc_to_plarkc('Cyclist', tAuthor).
%baseKB:cyc_to_plarkc('and', '&').
baseKB:cyc_to_plarkc('forAll', 'all').
baseKB:cyc_to_plarkc('thereExists', 'exists').
baseKB:cyc_to_plarkc('thereExistsAtLeast', 'atleast').
baseKB:cyc_to_plarkc('thereExistsAtMost', 'atmost').
baseKB:cyc_to_plarkc('CycLClosedAtomicTerm', 'ftAtomicTerm').
baseKB:cyc_to_plarkc('UnitOfMeasure', 'tUnitOfMeasure').
baseKB:cyc_to_plarkc('CharacterString', 'ftString').
baseKB:cyc_to_plarkc('SetOrCollection', 'tCol').
baseKB:cyc_to_plarkc('ScalarInterval', 'tScalarInterval').


%baseKB:cyc_to_plarkc('or', 'v').
baseKB:cyc_to_plarkc('holds', 't').
baseKB:cyc_to_plarkc('dot_holds', 't').

% for SUMO
baseKB:cyc_to_plarkc('domain', 'argIsa').
baseKB:cyc_to_plarkc('range', 'resultIsa').
baseKB:cyc_to_plarkc('domainSubclass', 'argGenl').
baseKB:cyc_to_plarkc('rangeSubclass', 'resultGenl').
baseKB:cyc_to_plarkc('instance', 'isa').
baseKB:cyc_to_plarkc(subrelation,genlPreds).
baseKB:cyc_to_plarkc(documentation,comment).
baseKB:cyc_to_plarkc('Class',tCol).
baseKB:cyc_to_plarkc('SetOrClass', 'tCol').


:- dynamic(baseKB:mpred_to_cyc/2).

baseKB:mpred_to_cyc(P,C):- baseKB:cyc_to_plarkc(C,P),!.
baseKB:mpred_to_cyc(ftInt,'Integer').
baseKB:mpred_to_cyc(ftSentence,'CycLFormulaicSentence').
baseKB:mpred_to_cyc(ftSentence,'FormulaicSentence').
% baseKB:mpred_to_cyc(ftNonAtomicTerm,'CycLGenericRelationFormula').

baseKB:mpred_to_cyc(ftSentence,'SubLFormulaicSentence').
baseKB:mpred_to_cyc(ftSentence,'icSentenceSentence').
baseKB:mpred_to_cyc(ftVar,'CycLVariable').
baseKB:mpred_to_cyc(ftVar,'Variable').

baseKB:mpred_to_cyc(ttExpressionType,'CycLExpressionType').
baseKB:mpred_to_cyc(ttExpressionType,'ExpressionType').

baseKB:mpred_to_cyc(tAgent,'Agent-Generic').
baseKB:mpred_to_cyc(tCol,'Collection').
baseKB:mpred_to_cyc(tFunction,'Function-Denotational').
baseKB:mpred_to_cyc(tHumanCyclist, 'HumanCyclist').
baseKB:mpred_to_cyc(tKnowledgeBase, 'KnowledgeBase').
baseKB:mpred_to_cyc(tMicrotheory, 'Microtheory').
baseKB:mpred_to_cyc(tPred,'Predicate').
baseKB:mpred_to_cyc(tProblemStore, 'CycProblemStore').
baseKB:mpred_to_cyc(tRuleTemplate, 'RuleTemplate').
baseKB:mpred_to_cyc(tThing, 'Thing').
baseKB:mpred_to_cyc(tIndividual, 'Individual').

baseKB:mpred_to_cyc(vtDayOfWeekType, 'DayOfWeekType').
baseKB:mpred_to_cyc(vtMonthOfYearType, 'MonthOfYearType').
baseKB:mpred_to_cyc(vtFormat, 'Format').
baseKB:mpred_to_cyc(vtInferenceProblemLinkStatus, 'CycInferenceProblemLinkStatus').
baseKB:mpred_to_cyc(vtHLTruthValue, 'CycHLTruthValue').
baseKB:mpred_to_cyc(vtProvabilityStatus, 'CycProvabilityStatus').
baseKB:mpred_to_cyc(vtTruthValue, 'TruthValue').
baseKB:mpred_to_cyc(vtCanonicalizerDirective, 'CanonicalizerDirective').

baseKB:mpred_to_cyc(aCollectionSubsetFn,'CollectionSubsetFn').
baseKB:mpred_to_cyc(vTrue, 'True').
baseKB:mpred_to_cyc(vFalse, 'False').
baseKB:mpred_to_cyc(vGuest, 'Guest').
baseKB:mpred_to_cyc(vAdministrator, 'CycAdministrator').

baseKB:mpred_to_cyc(vIntervalEntry, 'IntervalEntry').
baseKB:mpred_to_cyc(vSingleEntry, 'SingleEntry').


baseKB:mpred_to_cyc(ftAtomicTerm, 'CycLClosedAtomicTerm').
baseKB:mpred_to_cyc(vSetTheFormat, 'SetTheFormat').

baseKB:mpred_to_cyc(vAssertedFalseDefault, 'AssertedFalseDefault').
baseKB:mpred_to_cyc(vAssertedFalseMonotonic, 'AssertedFalseMonotonic').
baseKB:mpred_to_cyc(vAssertedTrueDefault, 'AssertedTrueDefault').
baseKB:mpred_to_cyc(vAssertedTrueMonotonic, 'AssertedTrueMonotonic').
baseKB:mpred_to_cyc(vMonotonicallyFalse, 'MonotonicallyFalse').
baseKB:mpred_to_cyc(vMonotonicallyTrue, 'MonotonicallyTrue').
baseKB:mpred_to_cyc(vDefaultFalse, 'DefaultFalse').
baseKB:mpred_to_cyc(vDefaultTrue, 'DefaultTrue').

baseKB:mpred_to_cyc('vGood-ProblemProvabilityStatus', 'Good-ProblemProvabilityStatus').
baseKB:mpred_to_cyc('vNeutral-ProblemProvabilityStatus', 'Neutral-ProblemProvabilityStatus').
baseKB:mpred_to_cyc('vNoGood-ProblemProvabilityStatus', 'NoGood-ProblemProvabilityStatus').
baseKB:mpred_to_cyc('vUnknown-HLTruthValue', 'Unknown-HLTruthValue').
baseKB:mpred_to_cyc('vExistentialQuantifier-Bounded', 'ExistentialQuantifier-Bounded').
baseKB:mpred_to_cyc(vAllowGenericArgVariables, 'AllowGenericArgVariables').
baseKB:mpred_to_cyc(vAllowKeywordVariables, 'AllowKeywordVariables').
baseKB:mpred_to_cyc(vRelaxArgTypeConstraintsForVariables, 'RelaxArgTypeConstraintsForVariables').
baseKB:mpred_to_cyc(vLeaveSomeTermsAtEL, 'LeaveSomeTermsAtEL').
baseKB:mpred_to_cyc(vLeaveSomeTermsAtELAndAllowKeywordVariables, 'LeaveSomeTermsAtELAndAllowKeywordVariables').
baseKB:mpred_to_cyc(vLeaveVariablesAtEL, 'LeaveVariablesAtEL').
baseKB:mpred_to_cyc(vDontReOrderCommutativeTerms, 'DontReOrderCommutativeTerms').

baseKB:mpred_to_cyc(vReformulationBackwardDirection, 'ReformulationBackwardDirection').
baseKB:mpred_to_cyc(vReformulationForwardDirection, 'ReformulationForwardDirection').
baseKB:mpred_to_cyc(vReformulationNeitherDirection, 'ReformulationNeitherDirection').
baseKB:mpred_to_cyc(ftSentenceAssertible, 'CycLSentence-ClosedPredicate').
baseKB:mpred_to_cyc(ftNonAtomicTerm, 'CycLNonAtomicTerm-ClosedFunctor').


baseKB:mpred_to_cyc(vApril, 'April').
baseKB:mpred_to_cyc(vAugust, 'August').
baseKB:mpred_to_cyc(vDecember, 'December').
baseKB:mpred_to_cyc(vFebruary, 'February').
baseKB:mpred_to_cyc(vJanuary, 'January').
baseKB:mpred_to_cyc(vJuly, 'July').
baseKB:mpred_to_cyc(vJune, 'June').
baseKB:mpred_to_cyc(vMarch, 'March').
baseKB:mpred_to_cyc(vMay, 'May').
baseKB:mpred_to_cyc(vNovember, 'November').
baseKB:mpred_to_cyc(vOctober, 'October').
baseKB:mpred_to_cyc(vSeptember, 'September').

baseKB:mpred_to_cyc(vSunday, 'Sunday').
baseKB:mpred_to_cyc(vMonday, 'Monday').
baseKB:mpred_to_cyc(vTuesday, 'Tuesday').
baseKB:mpred_to_cyc(vWednesday, 'Wednesday').
baseKB:mpred_to_cyc(vThursday, 'Thursday').
baseKB:mpred_to_cyc(vFriday, 'Friday').
baseKB:mpred_to_cyc(vSaturday, 'Saturday').



:- forall(baseKB:cycPrepending(AT,A),((atom_concat(AT,A,FT),asserta(baseKB:mpred_to_cyc(FT,A))))).


notFormatType(tThing).
notFormatType(tIndividual).
notFormatType(tInferenceSupportedFunction).

:- forall(notFormatType(NFT),ain(tCol(NFT))).


expT('SubLExpressionType').
expT('SubLExpression').
expT('CycLExpression').
expT('ttExpressionType').


isF(X):- atom_concat(_,'Fn',X).
isF(X):- tinyKB1(resultIsa(X,_)).
isF(X):- tinyKB1(resultQuotedIsa(X,_)).
isF(X):- tinyKB1(resultGenl(X,_)).
isF(X):- tinyKB1(isa(X,C)),atom_contains(C,'Function').

isFT(X):- expT(FT),tinyKB1(isa(X,FT)),!.
isFT(X):- expT(FT),tinyKB1(genls(X,FT)),!.
isFT(X):- tinyKB1(resultIsa(X,FT)),expT(FT),!.
isFT(X):- tinyKB1(resultGenl(X,FT)),expT(FT),!.
isFT(X):- tinyKB1(resultQuotedIsa(X,_)),!.

isV(X):- tinyKB1(isa(X,VT)),isVT(VT).
isVT(X):- tinyKB1(genls(X,'Individual')).

isPT(X):- atom_concat(_,'Predicate',X).
isPT(X):- tinyKB1(genls(X,'tPred')).

isRT(X):- atom_concat(_,'Function',X).
isRT(X):- atom_concat(_,'Relation',X).
isRT(X):- tinyKB1(genls(X,'tRelation')).
isRT(X):- tinyKB1(genls(X,'tFunction')).

:-dynamic(tinyKB0/1).

maybe_ruleRewrite(I,O):-ruleRewrite(I,O),!.
maybe_ruleRewrite(IO,IO).

:- dynamic(baseKB:cyc_to_plarkc/2).

mpred_prepend_type(X,_):- \+ atom(X),!,fail.
mpred_prepend_type(X,PP):- baseKB:cycPrepending(PP,X),!.
mpred_prepend_type(X,_):- name(X,[S|_]),char_type(S,lower),!,fail.
mpred_prepend_type(X,_):-upcase_atom(X,U),X==U,!,fail.


mpred_prepend_type(X,t):- tinyKB1(genls(X,'tMicrotheory')),!.
mpred_prepend_type(X,ft):- isFT(X),!.
mpred_prepend_type(X,a):- isF(X),!.
mpred_prepend_type(X,pt):- isPT(X),!.
mpred_prepend_type(X,vt):- isRT(X),!.
mpred_prepend_type(X,v):- isV(X),!.
mpred_prepend_type(X,v):- isVT(X),!.
mpred_prepend_type(X,tt):- tinyKB1(genls(X,'tCol')),!.
mpred_prepend_type(X,tt):- tinyKB1(isa(X,'AtemporalNecessarilyEssentialCollectionType')),!.
mpred_prepend_type(X,t):- tinyKB1(isa(X,'tCol')),!.
mpred_prepend_type(X,t):- tinyKB1(isa(_,X)),!.
mpred_prepend_type(X,v):- name(X,[C|_]),char_type(C,upper),!.

mpred_postpend_type(X,_):- name(X,[S|_]),char_type(S,lower),!,fail.
mpred_postpend_type(C,'Fn'):-isF(C).

tinyKB_wstr(P):-tUndressedMt(MT),tinyKB(P,MT,_).
tinyKB_wstr(ist(MT,P)):-tDressedMt(MT),tinyKB(P,MT,_).

mwkb1:- tell(fooooo0),
      ignore(( tinyKB(D), maybe_ruleRewrite(D,E),format('~q.~n',[tinyKB0(E)]),attvar_op(baseKB:asserta_if_new,tinyKB0(E)),fail)),
      told.

ltkb1:- check_clause_counts,
 defaultAssertMt(MT),
 must(logicmoo_i_cyc_kb\==MT),
 with_current_why(mfl(MT, ltkb1, _ ),
 ( MT: must_det_l(( mwkb1,forall(find_and_call(tinyKB0(D)), MT:cycAssert(D)))),
         check_clause_counts,
         finish_asserts,
  ltkb1_complete)).


finish_asserts:- call_u(forall(find_and_call(tinyKB8(Fact)),mpred_post(baseKB:Fact,(tinyKB8(Fact),ax)))).

ltkb1_complete:- 
  finish_asserts,
  doall((filematch(logicmoo('plarkc/logicmoo_i_cyc_kb_tinykb.pfc'),F),
  source_file(X,F),
  predicate_property(X,dynamic),retract(X:-_))).


wkb0:- tell(fooooo0),
      forall(tinyKB_All(V,MT,STR),format('~q.~n',[tinyKB_All(V,MT,STR)])),
      told.
wkb01:- tell(fooooo0),
      forall(tinyKB_All(V,MT,STR),format('~q.~n',[tinyKB_All(V,MT,STR)])),
      told.


wkbe:- statistics(cputime,S),tell(foof),
   ignore((el_assertions:el_holds_pred_impl(F),between(2,16,A),
          current_predicate(F/A),functor(P,F,A),forall(P,format('~q.~n',[P])),fail)),told,
   statistics(cputime,E),Total is E - S, writeln(Total).

wkb02:- tell(fooooo0),
      forall(tinyKB_All(V,MT,STR),once((cyc_to_clif(tinyKB_All(V,MT,STR),KB),format('~q.~n',[KB])))),
      told.

wkb2:- tell(fooooo2),
      ignore(( tinyKB(D,MT,Str),cyc_to_clif(D,KB),format('~N~q.~N',[proof(KB,D,MT,Str)]),fail)),
      told.

:- was_export(cyc_to_mpred_idiom/2).
%cyc_to_mpred_idiom(different,dif).
cyc_to_mpred_idiom(X,X):- \+ atom(X),!,fail.
cyc_to_mpred_idiom(C,P):-baseKB:cyc_to_plarkc(C,P),!.
cyc_to_mpred_idiom(M,P):-baseKB:mpred_to_cyc(P,M),!.
cyc_to_mpred_idiom(equiv,(<=>)).
cyc_to_mpred_idiom(implies,(=>)). 
cyc_to_mpred_idiom(not,(~)).
cyc_to_mpred_idiom(X,X):- name(X,[S|_]),char_type(S,lower),!.
cyc_to_mpred_idiom(X,X):-upcase_atom(X,U),X==U,!.
cyc_to_mpred_idiom(C,PM):- atom(C),  
  transitive_lc(cyc_to_mpred_idiom1,C,M),!,
  m_to_pm(M,C,PM),
   ignore(( % \+ current_prolog_flag(logicmoo_debug,true),
   asserta(baseKB:cyc_to_plarkc(C,PM)),
   asserta(baseKB:mpred_to_cyc(PM,C)))),!.

m_to_pm(P,C,PM):- name(P,[S|_]),char_type(S,lower),!,
   (mpred_postpend_type(C,PPT)->(atom_concat(_,PPT,P)-> PM=P; (atom_concat(P,PPT,PM)));PM=P),!.
m_to_pm(M,C,PM):-
   (mpred_prepend_type(C,PT)->(atom_concat(PT,_,M)-> P=M; (atom_concat(PT,M,P)));P=M),
   (mpred_postpend_type(C,PPT)->(atom_concat(_,PPT,P)-> PM=P; (atom_concat(P,PPT,PM)));PM=P),!.


cyc_to_mpred_idiom1(C,P):-nonvar(C),baseKB:mpred_to_cyc(P,C),!.
cyc_to_mpred_idiom1('CycLTerm','CycLExpression').
cyc_to_mpred_idiom1(C,P):-atom_concatM('CycLSentence-',Type,C),!,atom_concat('Sentence',Type,P).
cyc_to_mpred_idiom1(C,P):-atom_concatM('Expression-',Type,C),!,atom_concat('Expression',Type,P).

% TODO remove these next two simplifcations one day
cyc_to_mpred_idiom1(C,P):-atom_concatM('CycLOpen',P,C).
cyc_to_mpred_idiom1(C,P):-atom_concatM('CycLClosed',P,C).
 

cyc_to_mpred_idiom1(C,P):-atom_concatM('SubL',P,C).
cyc_to_mpred_idiom1(C,P):-atom_concatM('Cyclist',Type,C),!,atom_concat('Author',Type,P).
cyc_to_mpred_idiom1(C,P):-atom_concatM('CycL',P,C).
cyc_to_mpred_idiom1(C,P):-atom_concatM('Cyc',P,C).
cyc_to_mpred_idiom1(C,P):-atom_concatM('FormulaicSenten',Type,C),!,atom_concat('Senten',Type,P).
cyc_to_mpred_idiom1(C,P):-atom_concatM('SExpressi',Type,C),!,atom_concat('Expressi',Type,P).
cyc_to_mpred_idiom1(C,P):-atom_concatR(C,Type,'-Assertible'),!,atom_concat(Type,'Assertible',P).
cyc_to_mpred_idiom1(C,P):-atom_concatR(C,Type,'-Askable'),!,atom_concat(Type,'Askable',P).
cyc_to_mpred_idiom1(C,P):-atom_concatR(C,Type,'FormulaicSentence'),!,atom_concat(Type,'Sentence',P).

cyc_to_mpred_idiom_unused([Conj|MORE],Out):-fail, not(is_ftVar(Conj)),!,cyc_to_mpred_sent_idiom_2(Conj,Pred,_),
  w_tl(thocal:outer_pred_expansion(Conj,MORE),
    ( maplist(cyc_to_clif,MORE,MOREL), 
       w_tl(thocal:outer_pred_expansion(Pred,MOREL),       
         list_to_ops(Pred,MOREL,Out)))),!.

atom_concatM(L,M,R):-atom(L),nonvar(R),atom_concat(L,M,R),atom_length(M,N),!,N > 1.
atom_concatR(L,M,R):-atom(R),nonvar(L),atom_concat(L,M,R),atom_length(M,N),!,N > 1.

cyc_to_mpred_sent_idiom_2(and,(','),trueSentence).

list_to_ops(_,V,V):-is_ftVar(V),!.
list_to_ops(Pred,[],Out):-cyc_to_mpred_sent_idiom_2(_,Pred,Out),!.
list_to_ops(Pred,In,Out):-not(is_list(In)),!,cyc_to_clif(In,Mid),cyc_to_mpred_sent_idiom_2(_,Pred,ArityOne),Out=..[ArityOne,Mid].
list_to_ops(_,[In],Out):-!,cyc_to_clif(In,Out).
list_to_ops(Pred,[H,T],Body):-!,
    cyc_to_clif(H,HH),
    cyc_to_clif(T,TT),
    (is_list(TT)-> Body=..[Pred,HH|TT]; Body=..[Pred,HH,TT]).

list_to_ops(Pred,[H|T],Body):-!,
    list_to_ops(Pred,H,HH),
    list_to_ops(Pred,T,TT),
    (is_list(TT)-> Body=..[Pred,HH|TT]; Body=..[Pred,HH,TT]).

kw_to_vars(KW,VARS):-subsT_each(KW,[':ARG1'=_ARG1,':ARG2'=_ARG2,':ARG3'=_ARG3,':ARG4'=_ARG4,':ARG5'=_ARG5,':ARG6'=_ARG6],VARS).
make_kw_functor(F,A,CYCL):-make_kw_functor(F,A,CYCL,':ARG'),!.
make_kw_functor(F,A,CYCL,PREFIX):-make_functor_h(CYCL,F,A),CYCL=..[F|ARGS],label_args(PREFIX,1,ARGS).

label_args(_PREFIX,_,[]).
label_args(PREFIX,N,[ARG|ARGS]):-atom_concat(PREFIX,N,TOARG),ignore(TOARG=ARG),!,N2 is N+1,label_args(PREFIX,N2,ARGS).

:- thread_local thocal:outer_pred_expansion/2.

cyc_to_clif_notify(B,A):- cyc_to_clif(B,A) -> B\=@=A, nop(dmsg(B==A)).
%cyc_to_clif_entry(I,O):-fail,cyc_to_clif(I,M),!,must((functor(I,FI,_),functor(M,MF,_),FI==MF)),O=M.

cyc_to_clif(V,V):-is_ftVar(V),!.
cyc_to_clif(I,O):-atom(I),must(cyc_to_mpred_idiom(I,O)),!.
cyc_to_clif(V,V):-not(compound(V)),!.
cyc_to_clif('SubLQuoteFn'(V),V):-atom(V),!.
cyc_to_clif(isa(I,C),O):-atom(C),M=..[C,I],!,cyc_to_clif(M,O).
cyc_to_clif(I,O):-ruleRewrite(I,M),I\=@=M,!,cyc_to_clif(M,O).
cyc_to_clif([H|T],[HH|TT]):-!,cyc_to_clif(H,HH),cyc_to_clif(T,TT),!.
cyc_to_clif(HOLDS,HOLDSOUT):-HOLDS=..[F|HOLDSL],
  w_tl(thocal:outer_pred_expansion(F,HOLDSL),cyc_to_clif([F|HOLDSL],[C|HOLDSOUTL])),!,
  ((is_list([C|HOLDSOUTL]), atom(C))-> must(HOLDSOUT=..[C|HOLDSOUTL]) ; HOLDSOUT=[C|HOLDSOUTL]),!.


/*
:- dynamic(argIsa/3).
:- shared_multifile(argIsa/3).
:- dynamic(argGenl/3).
:- shared_multifile(argGenl/3).
:- dynamic(argQuotedIsa/3).
:- shared_multifile(argQuotedIsa/3).
isa(I,C):-exactlyAssertedEL(isa,I,C,_,_).
genls(I,C):-exactlyAssertedEL(genls,I,C,_,_).
arity(I,C):-exactlyAssertedEL(arity,I,C,_,_).
argIsa(P,N,C):-exactlyAssertedEL(argIsa,P,N,C,_,_).
argGenl(P,N,C):-exactlyAssertedEL(argGenl,P,N,C,_,_).
argQuotedIsa(P,N,C):-exactlyAssertedEL(argQuotedIsa,P,N,C,_,_).
*/
% queuedTinyKB(CycL,MT):- (tUndressedMt(MT);tDressedMt(MT)),(STR=vStrMon;STR=vStrDef),  tinyKB_All(CycL,MT,STR),\+ clause(exactlyAssertedEL(CycL,_,_,_),true).
% queuedTinyKB(CycL):-tUndressedMt(MT),queuedTinyKB(CycL,MT).
% queuedTinyKB(ist(MT,CycL)):-tDressedMt(MT),queuedTinyKB(CycL,MT).


ist_tiny(MT,P):-tinyKB(P,MT,vStrMon).
ist_tiny(MT,P):-tinyKB(P,MT,vStrDef).

%TODO ADD BACK AFTER OPTIZING
tinyKB(P):- tinyKB(P,_MT,_).
tinyKB(ist(MT,P)):-!,tDressedMt(MT),tinyKB(P,MT,_).
tinyKB(D):-find_and_call(tinyKB0(D)).

tinyKB1(D):-no_repeats(tinyKB2(D)).
tinyKB2(P):- tinyKB(P,_MT,_).
tinyKB2(D):-tinyKB0(D).
tinyKB2(isa(C1,C3)):-nonvar(C1),!,tinyKB0(isa(C1,C2)),tinyKB2(genls(C2,C3)).
tinyKB2(genls(C1,C3)):-nonvar(C1),tinyKB0(genls(C1,C2)),tinyKB2(genls(C2,C3)).
/*
tinyKB2(genls(C1,C3)):-nonvar(C1),tinyKB0(genls(C1,C2)),tinyKB0(genls(C2,C3)).
tinyKB2(genls(C1,C4)):-nonvar(C1),tinyKB0(genls(C1,C2)),tinyKB0(genls(C2,C3)),tinyKB0(genls(C3,C4)).
tinyKB2(genls(C1,C5)):-nonvar(C1),tinyKB0(genls(C1,C2)),tinyKB0(genls(C2,C3)),tinyKB0(genls(C3,C4)),tinyKB0(genls(C4,C5)).
*/
%TODO ADD BACK AFTER OPTIZING tinyKB(P):-nonvar(P),if_defined(P).

tinyKB(PO,MT,STR):- %fwc,  
  (tUndressedMt(MT);tDressedMt(MT)),(STR=vStrMon;STR=vStrDef), 
  tinyKB_All(PO,MT,STR).

tinyKB_All(V,MT,STR):- tinyAssertion(V,MT,STR).
tinyKB_All(PO,MT,STR):- current_predicate(_:'TINYKB-ASSERTION'/5),!,
    tiny_kb_ASSERTION(PLISTIn,PROPS),
        once((sexpr_sterm_to_pterm(PLISTIn,P),
               memberchk(amt(MT),PROPS),
               memberchk(str(STR),PROPS), 
              (member(vars(VARS),PROPS)->(nput_variable_names( []),fixvars(P,0,VARS,PO),nput_variable_names( PO));PO=P ))).

loadTinyKB:-forall(tinyKB(P,MT,STR),((print_assertion(P,MT,STR),ain(P)))).
% ssveTinyKB:-tinyKB_All(tinyKB(P,MT,STR),tell((print_assertion(P,MT,STR),ain(P)))).

print_assertion(P,MT,STR):- P=..PL,append([exactlyAssertedEL|PL],[MT,STR],PPL),PP=..PPL, portray_clause(current_output,PP,[numbervars(false)]).


tUndressedMt('UniversalVocabularyImplementationMt').
tUndressedMt('LogicalTruthImplementationMt').
tUndressedMt('CoreCycLImplementationMt').
tUndressedMt('UniversalVocabularyMt').
tUndressedMt('LogicalTruthMt').
tUndressedMt('CoreCycLMt').
tUndressedMt('BaseKB').

tDressedMt('BookkeepingMt').
tDressedMt('EnglishParaphraseMt').
tDressedMt('TemporaryEnglishParaphraseMt').

call_el_stub(V,MT,STR):-into_mpred_form(V,M),!,M=..ML,((ML=[t|ARGS]-> true; ARGS=ML)),CALL=..[exactlyAssertedEL|ARGS],!,call(CALL,MT,STR).
make_el_stub(V,MT,STR,CALL):-into_mpred_form(V,M),!,M=..ML,((ML=[t|ARGS]-> true; ARGS=ML)),append(ARGS,[MT,STR],CARGS),CALL=..[exactlyAssertedEL|CARGS],!.

tinyAssertion(V,MT,STR):- 
 nonvar(V) -> call_el_stub(V,MT,STR);
  (tinyAssertion0(W,MT,STR),once(into_mpred_form(W,V))).

tinyAssertion0(t(A,B,C,D,E),MT,STR):-exactlyAssertedEL(A,B,C,D,E,MT,STR).
tinyAssertion0(t(A,B,C,D),MT,STR):-exactlyAssertedEL(A,B,C,D,MT,STR).
tinyAssertion0(t(A,B,C),MT,STR):-exactlyAssertedEL(A,B,C,MT,STR).
tinyAssertion0(t(A,B),MT,STR):-exactlyAssertedEL(A,B,MT,STR).


addTinyCycL(CycLIn):- into_mpred_form(CycLIn,CycL),
  ignore((tiny_support(CycL,_MT,CALL),must(retract(CALL)))),!,
  addCycL(CycL),!.



tiny_support(CycLIn,MT,CALL):- compound(CycLIn),!,into_mpred_form(CycLIn,CycL), CycL=..[F|Args], append(Args,[MT,_STR],WMT),CCALL=..[exactlyAssertedEL,F|WMT],!,
  ((clause(CCALL,true), CCALL=CALL) ; clause(CCALL,(CALL,_))).
tiny_support(CycLOut,MT,CALL):- between(4,7,Len),functor(CCALL,exactlyAssertedEL,Len),CCALL=..[exactlyAssertedEL,F|WMT],append(Args,[MT,_STR],WMT),
 CCALL,(atom(F)->CycL=..[F|Args];append_termlist(F,Args,CycL)),((clause(CCALL,true), CCALL=CALL) ; clause(CCALL,(CALL,_))), fully_expand(CycL,CycLOut).

make_functor_h(CycL,F,A):- length(Args,A),CycL=..[F|Args].

is_simple_gaf(V):-not(compound(V)),!.
is_simple_gaf(V):-needs_canoncalization(V),!,fail.
is_simple_gaf(V):-functor(V,F,A),member(F/A,[isa/2,genls/2,argQuotedIsa/3,afterAdding/2,afterRemoving/2]),!.
is_simple_gaf(V):-needs_indexing(V),!,fail.
is_simple_gaf(_).

needs_indexing(V):-compound(V),arg(_,V,A),not(is_simple_arg(A)),!,fail.

is_simple_arg(A):-not(compound(A)),!.
is_simple_arg(A):-functor(A,Simple,_),tEscapeFunction(Simple).

% :- dynamic(vtUnreifiableFunction/1).
'tEscapeFunction'('TINYKB-ASSERTION').
'tEscapeFunction'('aQuoteFn').
'tEscapeFunction'(X):- clause_b('vtUnreifiableFunction'(X)).

needs_canoncalization(CycL):-is_ftVar(CycL),!,fail.
needs_canoncalization(CycL):-functor(CycL,F,_),isa_db(F,'SentenceOperator').
needs_canoncalization(CycL):-needs_indexing(CycL).

is_better_backchained(CycL):-is_ftVar(CycL),!,fail.
is_better_backchained(CycL):-functor(CycL,F,_),isa_db(F,'SentenceOperator').
is_better_backchained(V):-unnumbervars(V,FOO),(((each_subterm(FOO,SubTerm),nonvar(SubTerm),isa_db(SubTerm,tAvoidForwardChain)))),!.


as_cycl(VP,VE):-subst(VP,('-'),(~),V0),subst(V0,('v'),(or),V1),subst(V1,('exists'),(thereExists),V2),subst(V2,('&'),(and),VE),!.


:- dynamic(addTiny_added/1).
addCycL(V):-addTiny_added(V),!.
addCycL(V):-into_mpred_form(V,M),V\=@=M,!,addCycL(M),!.
addCycL(V):-defunctionalize('implies',V,VE),V\=@=VE,!,addCycL(VE).
addCycL(V):-cyc_to_clif(V,VE),V\=@=VE,!,addCycL(VE).
addCycL(V):-is_simple_gaf(V),!,addCycL0(V),!.
addCycL(V):-kif_to_boxlog(V,VB),boxlog_to_pfc(VB,VP),V\=@=VP,!,as_cycl(VP,VE),show_call(why,addCycL0(VE)).
addCycL(V):-addCycL0(V),!.

addCycL0(V):-addCycL1(V).

addCycL1(V):-into_mpred_form(V,M),V\=@=M,!,addCycL0(M),!.
addCycL1(V):-cyc_to_clif(V,VE),V\=@=VE,!,addCycL0(VE).
addCycL1((TRUE=>V)):-is_true(TRUE),addCycL0(V),!.
addCycL1(<=(V , TRUE)):-is_true(TRUE),addCycL0(V),!.
addCycL1((V :- TRUE)):-is_true(TRUE),addCycL0(V),!.
addCycL1((V :- A)):- show_call(why,addCycL0((A => V))).
addCycL1((A => (V1 , V2))):-not(is_ftVar(V1)),!,show_call(why,addCycL0((A => V1))) , show_call(why,addCycL0((A => V2))).
addCycL1((V1 , V2)):-!,addCycL0(V1),addCycL0(V2),!.
addCycL1([V1 | V2]):-!,addCycL0(V1),addCycL0(V2),!.
addCycL1(V):-addTiny_added(V),!.
addCycL1(V):-asserta(addTiny_added(V)),unnumbervars(V,VE),
  % must(nop(remQueuedTinyKB(VE))),
  ain(VE).


sent_to_conseq(CycLIn,Consequent):- into_mpred_form(CycLIn,CycL), ignore((tiny_support(CycL,_MT,CALL),retract(CALL))),must(cycLToMpred(CycL,Consequent)),!.

:- dynamic(addTiny_added/1).

cycLToMpred(V,CP):-into_mpred_form(V,M),V\=@=M,!,cycLToMpred(M,CP),!.
cycLToMpred(V,CP):-cyc_to_clif(V,VE),V\=@=VE,!,cycLToMpred(VE,CP).
cycLToMpred(V,CP):-is_simple_gaf(V),!,cycLToMpred0(V,CP),!.
cycLToMpred(V,CP):-defunctionalize('implies',V,VE),V\=@=VE,!,cycLToMpred(VE,CP).
cycLToMpred(V,CP):-kif_to_boxlog(V,VB),boxlog_to_pfc(VB,VP),V\=@=VP,!,as_cycl(VP,VE),show_call(why,cycLToMpred0(VE,CP)).
cycLToMpred(V,CP):-cycLToMpred0(V,CP),!.

cycLToMpred0(V,CP):-into_mpred_form(V,M),V\=@=M,!,cycLToMpred0(M,CP),!.
cycLToMpred0(V,CP):-cyc_to_clif(V,VE),V\=@=VE,!,cycLToMpred0(VE,CP).
cycLToMpred0((TRUE => V),CP):-is_true(TRUE),cycLToMpred0(V,CP),!.
cycLToMpred0((V <=> TRUE),CP):-is_true(TRUE),cycLToMpred0(V,CP),!.
cycLToMpred0((V :- TRUE),CP):-is_true(TRUE),cycLToMpred0(V,CP),!.
cycLToMpred0((V :- A),CP):- show_call(why,cycLToMpred0((A => V),CP)).
cycLToMpred0((A => (V1 , V2)),CP):-not(is_ftVar(V1)),!,cycLToMpred0((A=> (V1/consistent(V2))),V1P),cycLToMpred0((A=> (V2/consistent(V1))),V2P) ,!,conjoin(V1P,V2P,CP).
cycLToMpred0((V1 , V2),CP):-!,cycLToMpred0(V1,V1P),cycLToMpred0(V2,V2P),!,conjoin(V1P,V2P,CP).
cycLToMpred0([V1 | V2],CP):-!,cycLToMpred0(V1,V1P),cycLToMpred0(V2,V2P),!,conjoin(V1P,V2P,CP).
cycLToMpred0(V,V).

%  cycLToMpred( (grandparent('$VAR'('G'),'$VAR'('C')) => thereExists('$VAR'('P'), and(parent('$VAR'('G'),'$VAR'('P')),parent('$VAR'('P'),'$VAR'('C'))))),O).



% :-onEachLoad(loadTinyAssertions2).

% ============================================
% DBASE to Cyc Predicate Mapping
% ============================================
/*
arity('abbreviationString-PN', 2).

typical_mtvars([_,_]).

% arity 1 person
make_functorskel(Person,1,fskel(Person,t(Person,A),Call,A,[],MtVars,Call2)):-typical_mtvars(MtVars),Call=..[Person,A],Call2=..[Person,A|MtVars]. 
% arity 2 likes
make_functorskel(Likes,2,fskel(Likes,t(Likes,A,B),Call,A,B,MtVars,Call2)):- typical_mtvars(MtVars),Call=..[Likes,A,B],Call2=..[Likes,A,B|MtVars]. 
% arity 3 between
make_functorskel(Between,3,fskel(Between,t(Between,A,B,C),Call,A,[B,C],MtVars,Call2)):- typical_mtvars(MtVars),Call=..[Between,A,B,C],Call2=..[Between,A,B,C|MtVars]. 
% arity 4 xyz
make_functorskel(Xyz,4,fskel(Xyz,t(Xyz,I,X,Y,Z),Call,I,[X,Y,Z],MtVars,Call2)):- typical_mtvars(MtVars),Call=..[Xyz,I,X,Y,Z],Call2=..[Xyz,I,X,Y,Z|MtVars]. 
% arity 5 rxyz
make_functorskel(RXyz,5,fskel(RXyz,t(RXyz,I,R,X,Y,Z),Call,I,[R,X,Y,Z],MtVars,Call2)):-typical_mtvars(MtVars),Call=..[RXyz,I,R,X,Y,Z],Call2=..[RXyz,I,R,X,Y,Z|MtVars]. 
% arity >6 
make_functorskel(F,N,fskel(F,DBASE,Call,I,NList,MtVars,Call2)):-typical_mtvars(MtVars),functor(Call,F,N),Call=..[F,I|NList],DBASE=..[t,F,I|NList],append([F,I|NList],MtVars,CALL2List),Call2=..CALL2List.

*/

% ============================================
% Prolog to Cyc Predicate Mapping
%
%  the following will all do the same things:
%
% :- decl_mpred('BaseKB':isa/2). 
% :- decl_mpred('BaseKB':isa(_,_)). 
% :- decl_mpred(isa(_,_),'BaseKB'). 
% :- decl_mpred('BaseKB',isa,2). 
%
%  Will make calls 
% :- isa(X,Y)
%  Query into #$BaseKB for (#$isa ?X ?Y) 
%
% decl_mpred/N
%
% ============================================

:- dynamic(lmcache:isCycUnavailable_known/1).
:- dynamic(lmcache:isCycAvailable_known/0).

/*
:- was_export(isCycAvailable/0).
isCycAvailable:-lmcache:isCycUnavailable_known(_),!,fail.
isCycAvailable:-lmcache:isCycAvailable_known,!.
isCycAvailable:-checkCycAvailablity,isCycAvailable.

:- was_export(isCycUnavailable/0).
isCycUnavailable:-lmcache:isCycUnavailable_known(_),!.
isCycUnavailable:-lmcache:isCycAvailable_known,!,fail.
isCycUnavailable:-checkCycAvailablity,isCycUnavailable.

:- was_export(checkCycAvailablity/0).
checkCycAvailablity:- (lmcache:isCycAvailable_known;lmcache:isCycUnavailable_known(_)),!.
checkCycAvailablity:- catchv((current_predicate(invokeSubL/2),ignore((invokeSubL("(+ 1 1)",R))),(R==2->assert_if_new(lmcache:isCycAvailable_known);assert_if_new(lmcache:isCycUnavailable_known(R)))),E,assert_if_new(lmcache:isCycUnavailable_known(E))),!.
*/
% :- dmsg("Loading tinyKB should take under a minute 666").

% :-must((asserta((user:term_expansion(A,B):-cyc_to_clif_notify(A,B),!),CLREF),asserta(at_eof_action(erase(CLREF))))).
% :- gripe_time(60,qcompile(logicmoo(plarkc/'logicmoo_i_cyc_kb_tinykb.pfc'))).
:- disable_mpred_expansion.
:- set_prolog_flag(lm_expanders,false).
:- (include(logicmoo(plarkc/'logicmoo_i_cyc_kb_preds.pfc'))).

:- ensure_loaded(logicmoo(plarkc/'logicmoo_i_cyc_kb_tinykb.pfc')).

:- enable_mpred_expansion.
:- gripe_time(60,ensure_loaded(logicmoo(plarkc/'logicmoo_i_cyc_xform.pfc'))).
%:-must(forall(retract(at_eof_action(CALL)),must(CALL))).
:- set_prolog_flag(lm_expanders,true).



% :-prolog.
