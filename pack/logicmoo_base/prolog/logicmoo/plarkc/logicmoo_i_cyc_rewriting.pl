/* 
% ===================================================================
% File 'logicmoo_i_cyc_rewriting.pl'
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

:- set_prolog_flag(lm_expanders,false).

:- dynamic(baseKB:cycPrepending/2).
baseKB:cycPrepending(ft,'Atom').
baseKB:cycPrepending(ft,'AtomicAssertion').
baseKB:cycPrepending(ft,'AtomicSentence').
baseKB:cycPrepending(ft,'AtomicTerm').
baseKB:cycPrepending(ft,'Character').
baseKB:cycPrepending(ft,'Constant').
baseKB:cycPrepending(ft,'Expression').
baseKB:cycPrepending(ft,'ExpressionAskable').
baseKB:cycPrepending(ft,'ExpressionAssertible').
% BAD?  baseKB:cycPrepending(ft,'GenericRelationFormula').
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
baseKB:cycPrepending(ft,'PositiveInteger').
baseKB:cycPrepending(ft,'PropositionalSentence').
baseKB:cycPrepending(ft,'RealNumber').
baseKB:cycPrepending(ft,'ReifiableDenotationalTerm').
baseKB:cycPrepending(ft,'ReifiableNonAtomicTerm').
baseKB:cycPrepending(ft,'ReifiedDenotationalTerm').
baseKB:cycPrepending(ft,'RepresentedAtomicTerm').
baseKB:cycPrepending(ft,'Sentence').
baseKB:cycPrepending(ft,'String').
baseKB:cycPrepending(ft,'Symbol').

/*
baseKB:cycPrepending(ft,'NonAtomicTerm-ClosedFunctor').
baseKB:cycPrepending(ft,'OpenDenotationalTerm').
baseKB:cycPrepending(ft,'OpenExpression').
baseKB:cycPrepending(ft,'OpenFormula').
baseKB:cycPrepending(ft,'OpenNonAtomicTerm').
baseKB:cycPrepending(ft,'OpenSentence').
baseKB:cycPrepending(ft,'ClosedAtomicSentence').
% BAD?  baseKB:cycPrepending(ft,'ClosedAtomicTerm').
baseKB:cycPrepending(ft,'ClosedDenotationalTerm').
baseKB:cycPrepending(ft,'ClosedExpression').
baseKB:cycPrepending(ft,'ClosedFormula').
baseKB:cycPrepending(ft,'ClosedNonAtomicTerm').
baseKB:cycPrepending(ft,'ClosedSentence').
*/

/*
baseKB:cycPrepending(ft,'RepresentedTerm').
baseKB:cycPrepending(ft,'RuleAssertion').
baseKB:cycPrepending(ft,'ScalarIntegralValue').
baseKB:cycPrepending(ft,'SentenceAskable').
baseKB:cycPrepending(ft,'SentenceAssertible').
baseKB:cycPrepending(ft,'SupportDatastructure').
baseKB:cycPrepending(ft,'TheTerm').
baseKB:cycPrepending(ft,'AssertedAssertion').
baseKB:cycPrepending(ft,'Assertion').
baseKB:cycPrepending(ft,'DeducedAssertion').
baseKB:cycPrepending(ft,'DenotationalTerm').
baseKB:cycPrepending(ft,'DenotationalTerm-Assertible').
baseKB:cycPrepending(ft,'DocumentationConstant').
baseKB:cycPrepending(ft,'GAFAssertion').
baseKB:cycPrepending(ft,'HLPrototypicalTerm').
baseKB:cycPrepending(ft,'IndeterminateTerm').
baseKB:cycPrepending(ft,'IndexedTerm').
baseKB:cycPrepending(ft,'TruthValueSentence').
*/

baseKB:cycPrepending(v,'SingleEntry').
baseKB:cycPrepending(v,'SetTheFormat').

baseKB:cycPrepending(tt,'TransformationModuleSupportedCollection').
baseKB:cycPrepending(tt,'RemovalModuleSupportedCollection-Generic').

baseKB:cycPrepending(rt,'UnaryPredicate').
baseKB:cycPrepending(rt,'RemovalModuleSupportedPredicate-Generic').
baseKB:cycPrepending(rt,'RemovalModuleSupportedPredicate-Specific').
baseKB:cycPrepending(rt,'InferenceSupportedPredicate').
baseKB:cycPrepending(rt,'SentenceClosedPredicate').
baseKB:cycPrepending(rt,'TransformationModuleSupportedPredicate').
baseKB:cycPrepending(rt,'ArgGenlQuantityBinaryPredicate').
baseKB:cycPrepending(rt,'BinaryFunction').
baseKB:cycPrepending(rt,'UnreifiableFunction').
baseKB:cycPrepending(rt,'UnaryFunction').


baseKB:cycPrepending(v,'Forward-AssertionDirection').
baseKB:cycPrepending(v,'Code-AssertionDirection').
baseKB:cycPrepending(v,'Backward-AssertionDirection').
baseKB:cycPrepending(vt,'AssertionDirection').

baseKB:cycPrepending(tt,'InferenceSupportedCollection').
% BAD?  baseKB:cycPrepending(rt,A):-atom(A),atom_contains(A,'Predicate').

prepender(Pre,C):- best_rename(C,P),atom_concat(Pre,C,P).


% for SUMO
:- dynamic(baseKB:sumo_to_plarkc/2).
baseKB:sumo_to_plarkc('domain', 'argIsa').
baseKB:sumo_to_plarkc('range', 'resultIsa').
baseKB:sumo_to_plarkc('domainSubclass', 'argGenl').
baseKB:sumo_to_plarkc('rangeSubclass', 'resultGenl').
baseKB:sumo_to_plarkc('instance', 'isa').
baseKB:sumo_to_plarkc(subrelation,genlPreds).
baseKB:sumo_to_plarkc(documentation,comment).
baseKB:sumo_to_plarkc('Class','tSet').
baseKB:sumo_to_plarkc('SetOrClass', 'tCol').

renamed_surely(C):- rn(C,_);rn(_,C).

ftCycOnlyTerm(C):- rn(C,_), \+ rename(_,C).
ftCycOnlyTerm(C):- rename(_,C).
ftLarkcOnlyTerm(P):- rn(_,P), \+ rename(P,_).
ftLarkcOnlyTerm(P):- rename(P,_).

cyc_renames(C,P1,P2):- ftCycOnlyTerm(C),atomic(C),
   ((rename(P2,C),atomic(P2))->true;P1=P2),
   gripe_time(0.1,once((cyc_to_mpred_idiom(C,P1);P1=P2))).

remove_renames(T):- retractall(rn(_,T)).

:- doall((findall(P,((rename(P,_);rn(P,_);rename(_,P);rn(_,P)), \+ atom(P)),L),
          member(P,L),maplist(retractall,[rename(_,P),rename(P,_),rn(_,P),rn(P,_)]))).

:- remove_renames('ftExpression').
:- remove_renames('ftSentence').

:- call((doall((findall(P,(ftLarkcOnlyTerm(P), atom_concat('xt',_,P)),L), 
          member(P,L),remove_renames(P))))).

:- call((doall((findall(P,(ftLarkcOnlyTerm(P), atom_concat('vt',_,P)),L), 
          member(P,L),remove_renames(P))))).

:- call((doall((findall(P,(ftLarkcOnlyTerm(P),starts_hungarian('v',P)),L), 
          member(P,L),remove_renames(P))))).

starts_hungarian(V,P):-atom_concat(V,Rest,P),name(Rest,[A|_]),char_type(A,upper).

conflicted(C,P1,P2):- cyc_renames(C,P1,P2), P1\==P2.

is_merge0(C1,C2):- ftLarkcOnlyTerm(P),rn(C1,P),dif(C1,C2),rn(C2,P).
is_merge(O1,O2):- no_repeats([O1,O2],(is_merge0(C1,C2), sort([C1,C2],[O1,O2]))).

best_rename(C,P):- baseKB:cyc_to_plarkc(C,P),!.
best_rename(C,P):- rn(C,P),nonvar(C),retractall(rename(_,C)),!.
best_rename(C,P):- cyc_renames(C,P1,P2), \+ rn(C,_), once(best_rename(C,P1,P2,P)),!.

best_rename(_C,P,P,P):-!.
best_rename(C,C,P2,P):-!,P=P2.
best_rename(C,P1,C,P):-!,P=P1.
best_rename(_C,P1,P2,P):-atom_prefix(P1,ft),atom_prefix(P2,xt),!,P=P1.
best_rename(_C,P1,P2,P):-atom_contains(P1,P2),!,P=P1.
best_rename(_C,P1,P2,P):-atom_contains(P2,P1),!,P=P2.
best_rename(_C,_,P2,P):-starts_lower(P2),!,P=P2.
% best_rename(_C,P1,_,P):-starts_lower(P1),!,P=P1.
best_rename(_C,P1,_,P1).

:- multifile(baseKB:cyc_to_plarkc/2).
:- dynamic(baseKB:cyc_to_plarkc/2).


% BAD? baseKB:cyc_to_plarkc('BaseKB', baseKB).
baseKB:cyc_to_plarkc('between', cycBetween).
baseKB:cyc_to_plarkc('forall', cycforAll).

% BAD?  baseKB:cyc_to_plarkc('equals', mudEquals).
% BAD?  baseKB:cyc_to_plarkc('termOfUnit',skolem ).

% BAD? baseKB:cyc_to_plarkc('ScalarInterval', 'tScalarInterval').
% BAD? baseKB:cyc_to_plarkc('SetOrCollection',tSpec).
% BAD? baseKB:cyc_to_plarkc(Was, baseKB):-mtUndressedMt(Was).
% BAD?  baseKB:cyc_to_plarkc('or', 'v').
% BAD?  baseKB:cyc_to_plarkc('and', '&').
baseKB:cyc_to_plarkc('SiblingDisjointCollectionType',tSet).
baseKB:cyc_to_plarkc('forAll', 'all').
baseKB:cyc_to_plarkc('thereExists', 'exists').
baseKB:cyc_to_plarkc('thereExistsAtLeast', 'atleast').
baseKB:cyc_to_plarkc('thereExistsAtMost', 'atmost').
baseKB:cyc_to_plarkc('CycLClosedAtomicTerm', 'ftAtomicTerm').
baseKB:cyc_to_plarkc('UnitOfMeasure', 'tUnitOfMeasure').
baseKB:cyc_to_plarkc('CharacterString', ftString).
baseKB:cyc_to_plarkc('Collection',tCol).
baseKB:cyc_to_plarkc('CollectionType',ttTypeType).
baseKB:cyc_to_plarkc('SiblingDisjointCollectionType',tSet).
baseKB:cyc_to_plarkc('ObjectType',ttValueType).
baseKB:cyc_to_plarkc('ObjectType',tSet).
baseKB:cyc_to_plarkc('AspatialThing',vtValue).
baseKB:cyc_to_plarkc('RelationshipType',ttRelationType).
baseKB:cyc_to_plarkc('Predicate',tPred).
baseKB:cyc_to_plarkc('SubLExpressionType',ttExpressionType).

baseKB:cyc_to_plarkc('holds', 't').
baseKB:cyc_to_plarkc('dot_holds', 't').
baseKB:cyc_to_plarkc('ArgGenlQuantityTernaryPredicate',rtArgGenlQuantityTernaryPredicate).
baseKB:cyc_to_plarkc('ELRelation',rtELRelation).
baseKB:cyc_to_plarkc('InterArgFormatPredicate',rtInterArgFormatPredicate).
baseKB:cyc_to_plarkc('ArgTypePredicate',rtArgTypePredicate).
baseKB:cyc_to_plarkc('ArgIsaPredicate',rtArgIsaPredicate).
baseKB:cyc_to_plarkc('SententialRelation',rtSententialRelation).
baseKB:cyc_to_plarkc('ThePrototypicalBinaryPredicate',rtThePrototypicalBinaryPredicate).
baseKB:cyc_to_plarkc('ThePrototypicalTransitiveBinaryPredicate',rtThePrototypicalTransitiveBinaryPredicate).
baseKB:cyc_to_plarkc('TruthFunction',rtTruthFunction).
baseKB:cyc_to_plarkc('ArgTypeBinaryPredicate',rtArgTypeBinaryPredicate).
baseKB:cyc_to_plarkc('ArgGenlTernaryPredicate',rtArgGenlTernaryPredicate).
baseKB:cyc_to_plarkc('ArgIsaTernaryPredicate',rtArgIsaTernaryPredicate).
baseKB:cyc_to_plarkc('ArgGenlBinaryPredicate',rtArgGenlBinaryPredicate).
baseKB:cyc_to_plarkc('ArgIsaBinaryPredicate',rtArgIsaBinaryPredicate).
baseKB:cyc_to_plarkc('ArgTypeTernaryPredicate',rtArgTypeTernaryPredicate).
baseKB:cyc_to_plarkc('InterArgIsaPredicate',rtInterArgIsaPredicate).
baseKB:cyc_to_plarkc('AntiTransitiveBinaryPredicate',rtAntiTransitiveBinaryPredicate).
baseKB:cyc_to_plarkc('BookkeepingPredicate',rtBookkeepingPredicate).
baseKB:cyc_to_plarkc('DistributingMetaKnowledgePredicate',rtDistributingMetaKnowledgePredicate).
baseKB:cyc_to_plarkc('DocumentationPredicate',rtDocumentationPredicate).
baseKB:cyc_to_plarkc('AntiSymmetricBinaryPredicate',rtAntiSymmetricBinaryPredicate).
baseKB:cyc_to_plarkc('EvaluatableFunction',rtEvaluatableFunction).
baseKB:cyc_to_plarkc('EvaluatableRelation',rtEvaluatableRelation).
baseKB:cyc_to_plarkc('ArgConstraintPredicate',rtArgConstraintPredicate).
baseKB:cyc_to_plarkc('ScopingRelation',rtScopingRelation).
baseKB:cyc_to_plarkc('MicrotheoryDesignatingRelation',rtMicrotheoryDesignatingRelation).
baseKB:cyc_to_plarkc('TransitiveBinaryPredicate',rtTransitiveBinaryPredicate).
baseKB:cyc_to_plarkc('ArgQuotedIsaBinaryPredicate',rtArgQuotedIsaBinaryPredicate).
baseKB:cyc_to_plarkc('ArgQuotedIsaPredicate',rtArgQuotedIsaPredicate).
baseKB:cyc_to_plarkc('ArgQuotedIsaTernaryPredicate',rtArgQuotedIsaTernaryPredicate).
baseKB:cyc_to_plarkc('ArgSometimesIsaPredicate',rtArgSometimesIsaPredicate).
baseKB:cyc_to_plarkc('AssociativeRelation',rtAssociativeRelation).
baseKB:cyc_to_plarkc('CollectionDenotingFunction',rtCollectionDenotingFunction).
baseKB:cyc_to_plarkc('EvaluatablePredicate',rtEvaluatablePredicate).
baseKB:cyc_to_plarkc('ExceptionPredicate',rtExceptionPredicate).
baseKB:cyc_to_plarkc('IndeterminateTermDenotingFunction',rtIndeterminateTermDenotingFunction).
baseKB:cyc_to_plarkc('QuaternaryRelation',rtQuaternaryRelation).
baseKB:cyc_to_plarkc('QuintaryRelation',rtQuintaryRelation).
baseKB:cyc_to_plarkc('TernaryRelation',rtTernaryRelation).
baseKB:cyc_to_plarkc('AsymmetricBinaryPredicate',rtAsymmetricBinaryPredicate).
baseKB:cyc_to_plarkc('SymmetricBinaryPredicate',rtSymmetricBinaryPredicate).
baseKB:cyc_to_plarkc('BinaryRelation',rtBinaryRelation).
baseKB:cyc_to_plarkc('CommutativeRelation',rtCommutativeRelation).
baseKB:cyc_to_plarkc('UnaryRelation',rtUnaryRelation).
baseKB:cyc_to_plarkc('PartiallyCommutativeRelation',rtPartiallyCommutativeRelation).
baseKB:cyc_to_plarkc('IrreflexiveBinaryPredicate',rtIrreflexiveBinaryPredicate).
baseKB:cyc_to_plarkc('ReflexiveBinaryPredicate',rtReflexiveBinaryPredicate).
baseKB:cyc_to_plarkc('QuaternaryPredicate',rtQuaternaryPredicate).
baseKB:cyc_to_plarkc('QuintaryPredicate',rtQuintaryPredicate).
baseKB:cyc_to_plarkc('TernaryPredicate',rtTernaryPredicate).
baseKB:cyc_to_plarkc('BinaryPredicate',rtBinaryPredicate).
baseKB:cyc_to_plarkc('CycLReformulationRulePredicate',rtReformulationRulePredicate).
baseKB:cyc_to_plarkc('DefaultMonotonicPredicate',rtDefaultMonotonicPredicate).
baseKB:cyc_to_plarkc('InferenceRelatedBookkeepingPredicate',rtInferenceRelatedBookkeepingPredicate).
baseKB:cyc_to_plarkc('FixedArityRelation',rtFixedArityRelation).
baseKB:cyc_to_plarkc('VariableArityRelation',rtVariableArityRelation).
baseKB:cyc_to_plarkc('SkolemFunction',rtSkolemFunction).
baseKB:cyc_to_plarkc('VariableAritySkolemFunction',rtVariableAritySkolemFunction).
baseKB:cyc_to_plarkc('FixedAritySkolemFunction',rtFixedAritySkolemFunction).
baseKB:cyc_to_plarkc('ReifiableFunction',rtReifiableFunction).
baseKB:cyc_to_plarkc('InferenceSupportedCollection',ttInferenceSupportedCollection).
baseKB:cyc_to_plarkc('AssertionDirection',vtAssertionDirection).
baseKB:cyc_to_plarkc('Backward-AssertionDirection',vBackwardAssertionDirection).
baseKB:cyc_to_plarkc('Code-AssertionDirection',vCodeAssertionDirection).
baseKB:cyc_to_plarkc('Forward-AssertionDirection',vForwardAssertionDirection).
baseKB:cyc_to_plarkc('UnaryFunction',rtUnaryFunction).
baseKB:cyc_to_plarkc('UnreifiableFunction',rtUnreifiableFunction).
baseKB:cyc_to_plarkc('BinaryFunction',rtBinaryFunction).
baseKB:cyc_to_plarkc('ArgGenlQuantityBinaryPredicate',rtArgGenlQuantityBinaryPredicate).
baseKB:cyc_to_plarkc('TransformationModuleSupportedPredicate',rtTransformationModuleSupportedPredicate).
baseKB:cyc_to_plarkc('SentenceClosedPredicate',rtSentenceClosedPredicate).
baseKB:cyc_to_plarkc('InferenceSupportedPredicate',rtInferenceSupportedPredicate).
baseKB:cyc_to_plarkc('RemovalModuleSupportedPredicate-Specific',rtRemovalModuleSupportedPredicateSpecific).
baseKB:cyc_to_plarkc('RemovalModuleSupportedPredicate-Generic',rtRemovalModuleSupportedPredicateGeneric).
baseKB:cyc_to_plarkc('UnaryPredicate',rtUnaryPredicate).
baseKB:cyc_to_plarkc('RemovalModuleSupportedCollection-Generic',ttRemovalModuleSupportedCollectionGeneric).
baseKB:cyc_to_plarkc('TransformationModuleSupportedCollection',ttTransformationModuleSupportedCollection).
baseKB:cyc_to_plarkc('SetTheFormat',vSetTheFormat).
baseKB:cyc_to_plarkc('SingleEntry',vSingleEntry).
baseKB:cyc_to_plarkc('Symbol',ftSymbol).
baseKB:cyc_to_plarkc('String',ftString).
baseKB:cyc_to_plarkc('Sentence',ftSentence).
baseKB:cyc_to_plarkc('RepresentedAtomicTerm',ftRepresentedAtomicTerm).
baseKB:cyc_to_plarkc('ReifiedDenotationalTerm',ftReifiedDenotationalTerm).
baseKB:cyc_to_plarkc('ReifiableNonAtomicTerm',ftReifiableNonAtomicTerm).
baseKB:cyc_to_plarkc('ReifiableDenotationalTerm',ftReifiableDenotationalTerm).
baseKB:cyc_to_plarkc('RealNumber',ftRealNumber).
baseKB:cyc_to_plarkc('PropositionalSentence',ftPropositionalSentence).
baseKB:cyc_to_plarkc('PositiveInteger',ftPositiveInteger).
baseKB:cyc_to_plarkc('NonVariableSymbol',ftNonVariableSymbol).
baseKB:cyc_to_plarkc('NonVariableNonKeywordSymbol',ftNonVariableNonKeywordSymbol).
baseKB:cyc_to_plarkc('NonNegativeInteger',ftNonNegativeInteger).
baseKB:cyc_to_plarkc('NonAtomicTerm-Assertible',ftNonAtomicTermAssertible).
baseKB:cyc_to_plarkc('NonAtomicTerm-Askable',ftNonAtomicTermAskable).
baseKB:cyc_to_plarkc('NonAtomicTerm',ftNonAtomicTerm).
baseKB:cyc_to_plarkc('NonAtomicReifiedTerm',ftNonAtomicReifiedTerm).
baseKB:cyc_to_plarkc('List',ftList).
baseKB:cyc_to_plarkc('Keyword',ftKeyword).
baseKB:cyc_to_plarkc('KBDatastructure',ftKBDatastructure).
baseKB:cyc_to_plarkc('InferenceSupportedTerm',ftInferenceSupportedTerm).
baseKB:cyc_to_plarkc('NonNegativeScalarInterval',ftNonNegativeScalarInterval).
baseKB:cyc_to_plarkc('InferenceDataStructure',ftInferenceDataStructure).
baseKB:cyc_to_plarkc('ExpressionAssertible',ftExpressionAssertible).
baseKB:cyc_to_plarkc('ExpressionAskable',ftExpressionAskable).
baseKB:cyc_to_plarkc('Expression',ftExpression).
baseKB:cyc_to_plarkc('Constant',ftConstant).
baseKB:cyc_to_plarkc('Character',ftCharacter).
baseKB:cyc_to_plarkc('AtomicTerm',ftAtomicTerm).
baseKB:cyc_to_plarkc('AtomicSentence',ftAtomicSentence).
baseKB:cyc_to_plarkc('AtomicAssertion',ftAtomicAssertion).
baseKB:cyc_to_plarkc('Atom',ftAtom).
baseKB:cyc_to_plarkc('Integer',ftInt).
baseKB:cyc_to_plarkc('CycLFormulaicSentence',ftSentence).
baseKB:cyc_to_plarkc('FormulaicSentence',ftSentence).
baseKB:cyc_to_plarkc('SubLFormulaicSentence',ftSentence).
baseKB:cyc_to_plarkc(icSentenceSentence,ftSentence).
baseKB:cyc_to_plarkc('CycLVariable',ftVar).
baseKB:cyc_to_plarkc('Variable',ftVar).
baseKB:cyc_to_plarkc('CycLExpressionType',ttExpressionType).
baseKB:cyc_to_plarkc('ExpressionType',ttExpressionType).
baseKB:cyc_to_plarkc('Agent-Generic',tAgent).
baseKB:cyc_to_plarkc('Collection',tCol).
baseKB:cyc_to_plarkc('Function-Denotational',tFunction).
baseKB:cyc_to_plarkc('HumanCyclist',tHumanCyclist).
baseKB:cyc_to_plarkc('KnowledgeBase',tKnowledgeBase).
baseKB:cyc_to_plarkc('Microtheory',tMicrotheory).
baseKB:cyc_to_plarkc('Predicate',tPred).
baseKB:cyc_to_plarkc('CycProblemStore',tProblemStore).
baseKB:cyc_to_plarkc('RuleTemplate',tRuleTemplate).
baseKB:cyc_to_plarkc('Thing',tThing).
baseKB:cyc_to_plarkc('Individual',tIndividual).
baseKB:cyc_to_plarkc('DayOfWeekType',vtDayOfWeekType).
baseKB:cyc_to_plarkc('MonthOfYearType',vtMonthOfYearType).
baseKB:cyc_to_plarkc('Format',vtFormat).
baseKB:cyc_to_plarkc('CycInferenceProblemLinkStatus',vtInferenceProblemLinkStatus).
baseKB:cyc_to_plarkc('CycHLTruthValue',vtHLTruthValue).
baseKB:cyc_to_plarkc('CycProvabilityStatus',vtProvabilityStatus).
baseKB:cyc_to_plarkc('TruthValue',vtTruthValue).
baseKB:cyc_to_plarkc('CanonicalizerDirective',vtCanonicalizerDirective).
baseKB:cyc_to_plarkc('CollectionSubsetFn',tColOfCollectionSubsetFn).
baseKB:cyc_to_plarkc('True',vTrue).
baseKB:cyc_to_plarkc('False',vFalse).
baseKB:cyc_to_plarkc('Guest',vGuest).
baseKB:cyc_to_plarkc('CycAdministrator',vAdministrator).
baseKB:cyc_to_plarkc('IntervalEntry',vIntervalEntry).
baseKB:cyc_to_plarkc('SingleEntry',vSingleEntry).
baseKB:cyc_to_plarkc('CycLClosedAtomicTerm',ftAtomicTerm).
baseKB:cyc_to_plarkc('SetTheFormat',vSetTheFormat).
baseKB:cyc_to_plarkc('AssertedFalseDefault',vAssertedFalseDefault).
baseKB:cyc_to_plarkc('AssertedFalseMonotonic',vAssertedFalseMonotonic).
baseKB:cyc_to_plarkc('AssertedTrueDefault',vAssertedTrueDefault).
baseKB:cyc_to_plarkc('AssertedTrueMonotonic',vAssertedTrueMonotonic).
baseKB:cyc_to_plarkc('MonotonicallyFalse',vMonotonicallyFalse).
baseKB:cyc_to_plarkc('MonotonicallyTrue',vMonotonicallyTrue).
baseKB:cyc_to_plarkc('DefaultFalse',vDefaultFalse).
baseKB:cyc_to_plarkc('DefaultTrue',vDefaultTrue).
baseKB:cyc_to_plarkc('Good-ProblemProvabilityStatus',vGoodProblemProvabilityStatus).
baseKB:cyc_to_plarkc('Neutral-ProblemProvabilityStatus',vNeutralProblemProvabilityStatus).
baseKB:cyc_to_plarkc('NoGood-ProblemProvabilityStatus',vNoGoodProblemProvabilityStatus).
baseKB:cyc_to_plarkc('Unknown-HLTruthValue',vUnknownHLTruthValue).
baseKB:cyc_to_plarkc('ExistentialQuantifier-Bounded',vExistentialQuantifierBounded).
baseKB:cyc_to_plarkc('AllowGenericArgVariables',vAllowGenericArgVariables).
baseKB:cyc_to_plarkc('AllowKeywordVariables',vAllowKeywordVariables).
baseKB:cyc_to_plarkc('RelaxArgTypeConstraintsForVariables',vRelaxArgTypeConstraintsForVariables).
baseKB:cyc_to_plarkc('LeaveSomeTermsAtEL',vLeaveSomeTermsAtEL).
baseKB:cyc_to_plarkc('LeaveSomeTermsAtELAndAllowKeywordVariables',vLeaveSomeTermsAtELAndAllowKeywordVariables).
baseKB:cyc_to_plarkc('LeaveVariablesAtEL',vLeaveVariablesAtEL).
baseKB:cyc_to_plarkc('DontReOrderCommutativeTerms',vDontReOrderCommutativeTerms).
baseKB:cyc_to_plarkc('ReformulationBackwardDirection',vReformulationBackwardDirection).
baseKB:cyc_to_plarkc('ReformulationForwardDirection',vReformulationForwardDirection).
baseKB:cyc_to_plarkc('ReformulationNeitherDirection',vReformulationNeitherDirection).
baseKB:cyc_to_plarkc('CycLSentence-ClosedPredicate',ftSentenceAssertible).
baseKB:cyc_to_plarkc('CycLNonAtomicTerm-ClosedFunctor',ftNonAtomicTerm).
baseKB:cyc_to_plarkc('April',vApril).
baseKB:cyc_to_plarkc('August',vAugust).
baseKB:cyc_to_plarkc('December',vDecember).
baseKB:cyc_to_plarkc('February',vFebruary).
baseKB:cyc_to_plarkc('January',vJanuary).
baseKB:cyc_to_plarkc('July',vJuly).
baseKB:cyc_to_plarkc('June',vJune).
baseKB:cyc_to_plarkc('March',vMarch).
baseKB:cyc_to_plarkc('May',vMay).
baseKB:cyc_to_plarkc('November',vNovember).
baseKB:cyc_to_plarkc('October',vOctober).
baseKB:cyc_to_plarkc('September',vSeptember).
baseKB:cyc_to_plarkc('Sunday',vSunday).
baseKB:cyc_to_plarkc('Monday',vMonday).
baseKB:cyc_to_plarkc('Tuesday',vTuesday).
baseKB:cyc_to_plarkc('Wednesday',vWednesday).
baseKB:cyc_to_plarkc('Thursday',vThursday).
baseKB:cyc_to_plarkc('Friday',vFriday).
baseKB:cyc_to_plarkc('Saturday',vSaturday).
baseKB:cyc_to_plarkc('ArgGenlQuantityTernaryPredicate',rtArgGenlQuantityTernaryPredicate).

baseKB:cyc_to_plarkc(X,Y):- starts_lower(X),!,dehyphenize_const(X,Y).
baseKB:cyc_to_plarkc(C,P):- atom(C), once(cyc_to_mpred_idiom1(C,I)), C\==I, loop_check(baseKB:cyc_to_plarkc(I,P)).
% baseKB:cyc_to_plarkc(C,P):- atom(C), transitive_lc(cyc_to_mpred_idiom1,C,I),baseKB:cyc_to_plarkc(I,P).
% BAD?  baseKB:cyc_to_plarkc(C,P):- rename(P,C).



baseKB:mpred_to_cyc(P,C):- loop_check(baseKB:cyc_to_plarkc(C,P)),!.

dehyphenize_const(PM,PMO):-tokenize_atom(PM,[_,T1|Toks]),member(E,[T1|Toks]),number(E),E<0,!,atomic_list_concat(List,'-',PM),atomic_list_concat(List,'_',PMO),!.
dehyphenize_const(PM,PMO):-atomic_list_concat([P,F|List],'-',PM),maplist(toPropercase,[F|List],ListO),atomic_list_concat([P|ListO],PMO),!.
dehyphenize_const(PM,PM).

:- must(dehyphenize_const('a-b','aB')).
:- must(dehyphenize_const('a-2b','a_2b')).

%:- forall(baseKB:cycPrepending(AT,A),((atom_concat(AT,A,FT),dehyphenize_const(FT,FFT)))).


notFormatType(tThing).
notFormatType(tIndividual).
notFormatType(rtInferenceSupportedFunction).

:- forall(notFormatType(NFT),ain(tSet(NFT))).


expT('SubLExpressionType').
expT('SubLExpression').
expT('CycLExpression').
expT('ttExpressionType').


isF(X):- atom_concat(_,'Fn',X).
isF(X):- tinyKB1(resultIsa(X,_)).
isF(X):- tinyKB1(resultQuotedIsa(X,_)).
isF(X):- tinyKB1(resultGenl(X,_)).
isF(X):- tinyKB1(isa(X,C)),atom_contains(C,'Function').

isFT(X):- atom_concatR(_,'Expression',X).
isFT(X):- tinyKB1(resultIsa(X,FT)),expT(FT),!.
isFT(X):- tinyKB1(resultGenl(X,FT)),expT(FT),!.
isFT(X):- tinyKB1(resultQuotedIsa(X,_)),!.
isFT(X):- expT(FT),tinyKB1(isa(X,FT)),!.
isFT(X):- expT(FT),tinyKB1(genls(X,FT)),!.

isV(X):- tinyKB1(isa(X,VT)),isVT(VT).
isVT(X):- tinyKB1(genls(X,'Individual')).

isPT(X):- atom_concat(_,'Predicate',X).
isPT(X):- tinyKB1(genls(X,'tPred')).

isRT(X):- atom_contains(X,'Functor').
isRT(X):- atom_concatR(_,'Operator',X).
isRT(X):- atom_concatR(_,'Slot',X).
isRT(X):- atom_concatR(_,'Dimension',X).
isRT(X):- atom_concatR(_,'Function',X).
isRT(X):- atom_concatR(_,'Relation',X).
isRT(X):- atom_concatR(_,'Quantifier',X).
isRT(X):- tinyKB1(genls(X,'tRelation')).
isRT(X):- tinyKB1(genls(X,'tFunction')).


mpred_prepend_type(X,_):- \+ atom(X),!,fail.
mpred_prepend_type(X,PP):- baseKB:cycPrepending(PP,X),!.
%mpred_prepend_type(X,PrePend):- mpred_prepend_type_via(X,PrePend).
mpred_prepend_type(X,_):- starts_lower(X),!,fail.



% mpred_prepend_type(X,t):- tinyKB1(genls(X,'tMicrotheory')),!.
mpred_prepend_type(X,rt):- isPT(X),!.
mpred_prepend_type(X,rt):- isRT(X),!.
mpred_prepend_type(X,ft):- isFT(X), \+ isF(X).


mpred_prepend_type(X,tt):- atom_concatR(_,'Collection',X).
mpred_prepend_type(X,tt):- atom_concatR(_,'Type',X).



%mpred_prepend_type(X,v):- isV(X), \+ isF(X).
% ..                                    ""
%mpred_prepend_type(X,v):- isVT(X),!.
%mpred_prepend_type(X,tt):- tinyKB1(genls(X,'tCol')),!.
%mpred_prepend_type(X,tt):- tinyKB1(isa(X,'AtemporalNecessarilyEssentialCollectionType')),!.
%mpred_prepend_type(X,t):- tinyKB1(isa(X,'tCol')),!.
%mpred_prepend_type(X,t):- tinyKB1(isa(_,X)),!.
%mpred_prepend_type(X,v):- name(X,[C|_]),char_type(C,upper),!.

mpred_postpend_type(X,_):- starts_lower(X),!,fail.
mpred_postpend_type(C,'Fn'):-isF(C).

% mpred_prepend_type_via(C,Pre):-rename(P,C),dehyphenize_const(C,H),atom_concat(Pre,H,P).

prepend_constant(PT,C,_,P):- transitive_lc(cyc_to_mpred_idiom1,C,PM),dehyphenize_const(PM,PMH),!, atom_concat(PT,PMH,P).

:- was_export(cyc_to_mpred_idiom/2).
%cyc_to_mpred_idiom(different,dif).

starts_lower(X):-name(X,[S|_]),char_type(S,lower).

never_idiom((:-)).
never_idiom((,)).
never_idiom(Atom):-atom_length(Atom,Len),Len<3.
never_idiom(A):- upcase_atom(A,U),downcase_atom(A,U).

cyc_to_mpred_idiom(X,_):- \+ atom(X),!,fail.
cyc_to_mpred_idiom(X,X):- never_idiom(X),!.
cyc_to_mpred_idiom(KW,SYMBOL):-name(KW,[58,LETTER|REST]),char_type(LETTER,alpha),!,name(SYMBOL,[LETTER|REST]).
cyc_to_mpred_idiom(KW,'$VAR'(VAR)):-name(KW,[63,LETTER|REST]),char_type(LETTER,alpha),!,name(SYMBOL,[LETTER|REST]),fix_var_name(SYMBOL,VAR),!.
cyc_to_mpred_idiom(C,P):- rn(C,P),!.
cyc_to_mpred_idiom(C,P):- baseKB:cyc_to_plarkc(C,P),!.
%cyc_to_mpred_idiom(C,P):-baseKB:mpred_to_cyc(P,C),!.
cyc_to_mpred_idiom(equiv,(<=>)).
cyc_to_mpred_idiom(implies,(=>)). 
%cyc_to_mpred_idiom(not,(~)).
% cyc_to_mpred_idiom(X,Y):- starts_lower(X), rename(X,Y),!.
cyc_to_mpred_idiom(X,Y):- starts_lower(X),!,dehyphenize_const(X,Y).
cyc_to_mpred_idiom(C,PM):- 
  cyc_to_mpred_idiom_did(C,PM),
  C\==PM,
  asserta(baseKB:cyc_to_plarkc(C,PM)),
  asserta(baseKB:mpred_to_cyc(PM,C)),!.
% cyc_to_mpred_idiom(C,PM):- rename(PM,C),!.
cyc_to_mpred_idiom(C,PM):- transitive_lc(cyc_to_mpred_idiom1,C,PM),!.
cyc_to_mpred_idiom(C,P):- atom_concat(it,C,P).

cyc_to_mpred_idiom_did(C,PM):- atom(C),  transitive_lc(cyc_to_mpred_idiom1,C,M),!,
 m_to_pm(M,C,PM),!.

m_to_pm(M,C,P):- mpred_prepend_type(C,PT), (atom_concat(PT,_,M)-> P=M; prepend_constant(PT,C,M,P)).
% m_to_pm(M,C,P):- mpred_postpend_type(C,PT), (atom_concat(_,PT,M)-> P=M; atom_concat(M,PT,P)).


cyc_to_mpred_idiom1('CycLTerm','CycLExpression').
cyc_to_mpred_idiom1(C,P):-atom_concatM('CycLSentence-',Type,C),!,atom_concat('Sentence',Type,P).
cyc_to_mpred_idiom1(C,P):-atom_concatM('Expression-',Type,C),!,atom_concat('Expression',Type,P).
% cyc_to_mpred_idiom1(C,P):-nonvar(C),baseKB:mpred_to_cyc(P,C),!.

% TODO remove these next two simplifcations one day
/*
cyc_to_mpred_idiom1(C,P):-atom_concatM('CycLOpen',P,C).
cyc_to_mpred_idiom1(C,P):-atom_concatM('CycLClosed',P,C).
% cyc_to_mpred_idiom1(C,P):-atom_concatM('Open',P,C).
cyc_to_mpred_idiom1(C,P):-atom_concatM('Closed',P,C).

cyc_to_mpred_idiom1(C,P):-atom_concatM('HL',P,C).

*/

cyc_to_mpred_idiom1(C,P):-atom_concatM('SubL',P,C).
cyc_to_mpred_idiom1(C,P):-atom_concatM('CycSystem',P,C).
cyc_to_mpred_idiom1(C,P):-atom_concatM('Lisp',P,C).

% cyc_to_mpred_idiom1(C,P):-atom_concatM('Cyclist',Type,C),!,atom_concat('Author',Type,P).
cyc_to_mpred_idiom1(C,P):-atom_concatM('CycL',P,C).
cyc_to_mpred_idiom1(C,P):-atom_concatM('Cyc',P,C).
cyc_to_mpred_idiom1(C,P):-atom_concatM('FormulaicSenten',Type,C),!,atom_concat('Senten',Type,P).
cyc_to_mpred_idiom1(C,P):-atom_concatM('SExpressi',Type,C),!,atom_concat('Expressi',Type,P).
cyc_to_mpred_idiom1(C,P):-atom_concatR(C,Type,'-Assertible'),!,atom_concat(Type,'Assertible',P).
cyc_to_mpred_idiom1(C,P):-atom_concatR(C,Type,'-Askable'),!,atom_concat(Type,'Askable',P).
cyc_to_mpred_idiom1(C,P):-atom_concatR(C,Type,'FormulaicSentence'),!,atom_concat(Type,'Sentence',P).
cyc_to_mpred_idiom1(B,A):-startsLower(B),dehyphenize_const(B,A).

cyc_to_mpred_idiom_unused([Conj|MORE],Out):-fail, not(is_ftVar(Conj)),!,cyc_to_mpred_sent_idiom_2(Conj,Pred,_),
  w_tl(thocal:outer_pred_expansion(Conj,MORE),
    ( maplist(cyc_to_clif,MORE,MOREL), 
       w_tl(thocal:outer_pred_expansion(Pred,MOREL),       
         list_to_ops(Pred,MOREL,Out)))),!.


atom_concatM(L,M,R):-atom(L),nonvar(R),atom_concat(L,M,R),atom_length(M,N),!,N > 2.
atom_concatR(L,M,R):-atom(R),nonvar(L),atom_concat(L,M,R),atom_length(M,N),!,N > 2.
atom_concatR(L,M,R):-atom(R),nonvar(M),atom_concat(L,M,R),atom_length(R,N),!,N > 2.



kw_to_vars(KW,VARS):-subsT_each(KW,[':ARG1'=_ARG1,':ARG2'=_ARG2,':ARG3'=_ARG3,':ARG4'=_ARG4,':ARG5'=_ARG5,':ARG6'=_ARG6],VARS).
make_kw_functor(F,A,CYCL):-make_kw_functor(F,A,CYCL,':ARG'),!.
make_kw_functor(F,A,CYCL,PREFIX):-make_functor_h(CYCL,F,A),CYCL=..[F|ARGS],label_args(PREFIX,1,ARGS).

label_args(_PREFIX,_,[]).
label_args(PREFIX,N,[ARG|ARGS]):-atom_concat(PREFIX,N,TOARG),ignore(TOARG=ARG),!,N2 is N+1,label_args(PREFIX,N2,ARGS).

:- thread_local thocal:outer_pred_expansion/2.

cyc_to_clif_notify(B,A):- cyc_to_clif(B,A) -> B\=@=A, nop(dmsg(B==A)).
%cyc_to_clif_entry(I,O):-fail,cyc_to_clif(I,M),!,must((functor(I,FI,_),functor(M,MF,_),FI==MF)),O=M.

cyc_to_clif(V,V):-is_ftVar(V),!.
cyc_to_clif([],[]):-!.
cyc_to_clif([H],HH):- string(H),convert_to_cycString(H,HH),!.
cyc_to_clif(H,HH):- string(H),convert_to_cycString(H,HH),!.
cyc_to_clif(I,O):- \+ (compound(I)),do_renames(I,O),!.
cyc_to_clif('uSubLQuoteFn'(V),V):-atom(V),!.
% cyc_to_clif(isa(I,C),O):-atom(C),M=..[C,I],!,cyc_to_clif(M,O).
cyc_to_clif(I,O):-ruleRewrite(I,M),I\=@=M,!,cyc_to_clif(M,O).


cyc_to_clif([H|T],[HH|TT]):-!,cyc_to_clif(H,HH),cyc_to_clif(T,TT),!.
cyc_to_clif(I,O):-stack_check,do_renames(I,O),!.
cyc_to_clif(HOLDS,HOLDSOUT):-HOLDS=..[F|HOLDSL],
  w_tl(thocal:outer_pred_expansion(F,HOLDSL),maplist( cyc_to_clif,[F|HOLDSL],[C|HOLDSOUTL])),!,
  ((is_list([C|HOLDSOUTL]), atom(C))-> must(HOLDSOUT=..[C|HOLDSOUTL]) ; HOLDSOUT=[C|HOLDSOUTL]),!.

:-export(do_renames/2).

fix_var_name(A,B):- atom_concat(':',_,A), atomic_list_concat(AB,'-',A),atomic_list_concat(AB,'_',B).
fix_var_name(A,B):- atom_concat('?',QB,A),!,atom_concat('_',QB,B).
fix_var_name(A,B):- atomic_list_concat(AB,'-',A),atomic_list_concat(AB,'_',B).

% rename_atom(A,B):- atom_contains(A,'~'),!,convert_to_cycString(A,B),nb_setval('$has_quote',t),!.
rename_atom(A,B):- atom_contains(A,' '),!,convert_to_cycString(A,B),nb_setval('$has_quote',t),!.
rename_atom(A,B):- best_rename(A,B),!.
rename_atom(A,B):- cyc_to_mpred_idiom(A,B),!.
rename_atom(A,A):- upcase_atom(A,A),!.
rename_atom(A,A):- dmsg(didnt_rename_atom(A)).

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


do_renames(A,B):- var(A),!,A=B.
do_renames(uU('SubLQuoteFn',A),uSubLQuoteFn(A)):-var(A),!,nb_setval('$has_var',t),!.
do_renames(uU('SubLQuoteFn','$VAR'(A)),uSubLQuoteFn(A)):-!,nb_setval('$has_quote',t),!.
do_renames('$VAR'(A),'$VAR'(B)):- catch((fix_var_name(A,B),!,nb_setval('$has_var',t)),E,(dtrace(dmsg(E)))),!.
%do_renames('$VAR'(A),B):- catch((fix_var_name(A,B),!,nb_setval('$has_var',t)),E,(dtrace(dmsg(E)))),!.
do_renames(A,B):- string(A),!,logicmoo_util_strings:convert_to_cycString(A,B).
do_renames(A,B):- atom(A),must(rename_atom(A,B)),!.
do_renames(A,B):- \+ compound(A),!,A=B.
do_renames([A|Rest],BList):- is_list([A|Rest]),!,maplist(do_renames,[A|Rest],BList).
do_renames([A|Rest],[B|List]):- !, do_renames(A,B),do_renames(Rest,List).
do_renames(uN(P,ARGS),B):-!,maplist(do_renames,[P|ARGS],List),compound_name_arguments(B,uT,List).
do_renames(A,B):- compound_name_arguments(A,P,ARGS),do_renames_compound_name_arguments(A,P,ARGS,B).
do_renames_compound_name_arguments(A,P,_,A):- is_builtin_like(P),!.
do_renames_compound_name_arguments(_A,P,ARGS,B):- maplist(do_renames,[P|ARGS],[T|L]),do_renames_pass2(T,L,B).

is_builtin_like(format).
is_builtin_like(sformat).
is_builtin_like(F):-exact_args_f(F).


compute_argIsa(ARG1ISA,NN,ARGISA):-
  atom(ARG1ISA),
  atom_concat('arg',REST,ARG1ISA),
  member(E,['Genl','Isa','SometimesIsa','Format','QuotedIsa']),atom_concat(N,E,REST),
  atom_number(N,NN),
  atom_concat('arg',E,ARGISA),!.

do_renames_pass2(forward,[MT,C,ARG1ISA,P,ID],OUT):- compute_argIsa(ARG1ISA,NN,ARGISA),!, 
  do_renames_pass2(forward,[MT,P,ARGISA,NN,C,ID],OUT).
do_renames_pass2(backward,[MT,C,ARG1ISA,P,ID],OUT):- compute_argIsa(ARG1ISA,NN,ARGISA),!, 
  do_renames_pass2(backward,[MT,P,ARGISA,NN,C,ID],OUT).
do_renames_pass2(code,[MT,C,ARG1ISA,P,ID],OUT):- compute_argIsa(ARG1ISA,NN,ARGISA),!, 
  do_renames_pass2(code,[MT,P,ARGISA,NN,C,ID],OUT).

do_renames_pass2(t,[ARG1ISA,P,C],OUT):- compute_argIsa(ARG1ISA,NN,ARGISA),  OUT = t(ARGISA,P,NN,C).

do_renames_pass2(P,[],B):-!,do_renames(P,B).
do_renames_pass2('NART',[P|ARGS],(B)):-atom(P),!,compound_name_arguments(B,P,ARGS).
do_renames_pass2(nartR,[P|ARGS],(B)):-atom(P),!,compound_name_arguments(B,P,ARGS).
do_renames_pass2(nartR,ARGS,B):-!,compound_name_arguments(B,nartR,ARGS).
do_renames_pass2(t,[P,I,C],B):- P==isa,atom(C),!,B=..[C,I].
do_renames_pass2(t,[P|IC],B):- intrinsicPred(P),!,B=..[P|IC].
do_renames_pass2(t,ARGS,B):- compound_name_arguments(B,t,ARGS).
do_renames_pass2(uU,ARGS,B):-!,compound_name_arguments(B,u,ARGS).
do_renames_pass2(P,IC,B):- intrinsicPred(P),!,B=..[P|IC].
do_renames_pass2(P,ARGS,B):-!,compound_name_arguments_safe(B,P,ARGS).

compound_name_arguments_safe(B,P,ARGS):-atom(P),!,compound_name_arguments(B,P,ARGS).
compound_name_arguments_safe(B,P,ARGS):- must(append_term(P,ARGS,B)),!.

intrinsicPred(genlMt).
intrinsicPred(ist).
intrinsicPred(and).
intrinsicPred(~).
intrinsicPred(not).
intrinsicPred(=).
intrinsicPred(or).
intrinsicPred(genlMt).
intrinsicPred(&).
intrinsicPred(v).
intrinsicPred(t).
intrinsicPred(=>).
intrinsicPred(<=>).
intrinsicPred(implies).
intrinsicPred(equiv).
intrinsicPred(forAll).
intrinsicPred('[|]').
intrinsicPred(;).
intrinsicPred(termOfUnit).

intrinsicPred(argSometimesIsa).
intrinsicPred(argQuotedIsa).
intrinsicPred(argIsa).
intrinsicPred(argGenl).
intrinsicPred(argFormat).

intrinsicPred(A):-atom(A),atom_concat('thereE',_,A).

:- (current_prolog_flag(lm_expanders,PrevValue)->true;PrevValue=false),
   call(assert,on_fin(set_prolog_flag(lm_expanders,PrevValue))),
   set_prolog_flag(lm_expanders,false).

/*
:- (current_prolog_flag(double_quotes,PrevValue)->true;PrevValue=false),
   call(assert,on_fin(set_prolog_flag(double_quotes,PrevValue))),
   set_prolog_flag(double_quotes,atom).
*/

:- if(current_prolog_flag(logicmoo_simplify_te,true)).
:- (call(asserta,((system:term_expansion(I, (:- true)):- !, I\=(:- _), call(assert,I))),Ref),call(assert,on_fin(erase(Ref)))),!.
:- (call(asserta,((user:term_expansion(I, (:- true)):- !, I\=(:- _), call(assert,I))),Ref),call(assert,on_fin(erase(Ref)))),!.
:- (call(asserta,((term_expansion(I, (:- true)):- !, I\=(:- _), call(assert,I))),Ref),call(assert,on_fin(erase(Ref)))),!.
:- endif.


% unt_ify(TGaf,PGaf):- \+ current_prolog_flag(logicmoo_untify,true),!,TGaf=PGaf.
unt_ify(TGaf,PGaf):- notrace(unt_ify_a(TGaf,PGaf)).

unt_ify_a(TGaf,PGaf):- compound(TGaf),functor(TGaf,UR,_),arg(_,v(uU,nartR),UR),!,TGaf=PGaf.
unt_ify_a(TGaf,PGaf):- term_variables(TGaf,TVars),
  freeze_pvars(TVars,unt_ify0(TGaf,PGaf)).
unt_ify0(TGaf,PGaf):- 
 \+ compound(TGaf)->PGaf=TGaf ;
  (is_list(TGaf) -> maplist(t_ify0,TGaf,PGaf);
    ((TGaf=..[t,P|TARGS]) -> (maplist(unt_ify,TARGS,ARGS)-> apply_term(P,ARGS,PGaf) ; PGaf=TGaf))).

% t_ify(PGaf,TGaf):- \+ current_prolog_flag(logicmoo_untify,true),!,TGaf=PGaf.
t_ify(PGaf,TGaf):- 
 notrace((term_variables(PGaf,PVars),
  freeze_pvars(PVars,t_ify0(PGaf,TGaf)))).

t_ify0(PGaf,TGaf):- 
 (\+ compound(PGaf)-> PGaf=TGaf;
  (is_list(PGaf) -> maplist(t_ify0,PGaf,TGaf);
    (PGaf=..[P|ARGS], (P==t -> PGaf=TGaf ; (maplist(t_ify0,ARGS,TARGS)->TGaf=..[t,P|TARGS]))))).


makeRenames:- 
 w_tl(set_prolog_flag(logicmoo_load_state,making_renames),
     forall(makeRenames0,true)),!.

makeRenames0:- exists_file('./rn2.pl'),must(ensure_loaded('./rn2.pl')),!.
makeRenames0:-
  tell('./rn2.pl'),
   writeln('

:- multifile(baseKB:cyc_to_plarkc/2).
:- dynamic(baseKB:cyc_to_plarkc/2). 

            '
            ), 
    forall((ftCycOnlyTerm(C)),
       ignore((best_rename(C,P),
         \+ rn(C,P),
         \+ baseKB:rename(P,C),
       ( \+ baseKB:rename(_,C)-> true;
          ((baseKB:rename(WasP,C),
            retractall((rename(WasP,C))),
            retractall((rn(C,WasP))),
            format('~n~q.',[:- retractall(rn(C,WasP))]),
            format('~n~q.',[:- retractall(rename(WasP,C))])
            ))),
          format('~n~q.',[baseKB:cyc_to_plarkc(C,P)]),
         assert(rn(C,P))))).
makeRenames0:- nl,told.
makeRenames0:- makeCycRenames.

makeCycRenames:- forall(makeCycRenames0,true).

actual_cyc_renames0(C,P):-rn(C,P).
actual_cyc_renames0(C,P):-baseKB:cyc_to_plarkc(C,P), \+ rn(C,P).

actual_cyc_renames(C,P):-actual_cyc_renames0(C,P).
actual_cyc_renames(C,P):- ftCycOnlyTerm(C),\+actual_cyc_renames0(C,_),best_rename(C,P).

makeCycRenames0:- 
  tell('e2c/renames.lisp'),
   writeln('

(define safely-rename-or-merge (Before After)
  (clet ((b (find-constant Before)) (a (find-constant After)))
     (pwhen b
       (punless a
         (ret (cyc-rename b After)))
   ;; purposely doesnt do anything
   (ret (quote (cyc-merge a b))))))
 '
           ), 
    forall(actual_cyc_renames(C,P),format('(safely-rename-or-merge "~w" "~w")~n',[C,P])).
makeCycRenames0:- nl,told.

:- gripe_time(7.0,makeRenames).


