/** <module> 
% ===================================================================
% File 'logicmoo_i_cyc.pl'
% Purpose: Emulation of OpenCyc for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface.pl' 1.0.0
% Revision:  $Revision: 1.9 $
% Revised At:   $Date: 2002/06/27 14:13:20 $
% ===================================================================
% File used as storage place for all predicates which make us more like Cyc
%
% Dec 13, 2035
% Douglas Miles
*/
%:- module(tiny_kb,['TINYKB-ASSERTION'/5, 'TINYKB-ASSERTION'/6]).

:-if_file_exists(user:ensure_loaded(logicmoo(ext/moo_ext_cyc_new))).

isa_db(I,C):-clause(isa(I,C),true).

:-dynamic(cycPrepending/2).
cycPrepending(ft,'AssertedAssertion').
cycPrepending(ft,'Assertion').
cycPrepending(ft,'Atom').
cycPrepending(ft,'AtomicAssertion').
cycPrepending(ft,'AtomicSentence').
cycPrepending(ft,'AtomicTerm').
cycPrepending(ft,'Character').
cycPrepending(ft,'ClosedAtomicSentence').
cycPrepending(ft,'ClosedAtomicTerm').
cycPrepending(ft,'ClosedDenotationalTerm').
cycPrepending(ft,'ClosedExpression').
cycPrepending(ft,'ClosedFormula').
cycPrepending(ft,'ClosedNonAtomicTerm').
cycPrepending(ft,'ClosedSentence').
cycPrepending(ft,'Constant').
cycPrepending(ft,'DeducedAssertion').
cycPrepending(ft,'DenotationalTerm').
cycPrepending(ft,'DenotationalTerm-Assertible').
cycPrepending(ft,'DocumentationConstant').
cycPrepending(ft,'Expression').
cycPrepending(ft,'ExpressionAskable').
cycPrepending(ft,'ExpressionAssertible').
cycPrepending(ft,'GAFAssertion').
cycPrepending(ft,'GenericRelationFormula').
cycPrepending(ft,'HLPrototypicalTerm').
cycPrepending(ft,'IndeterminateTerm').
cycPrepending(ft,'IndexedTerm').
cycPrepending(ft,'InferenceDataStructure').
cycPrepending(ft,'InferenceSupportedCollection').
cycPrepending(ft,'InferenceSupportedTerm').
cycPrepending(ft,'KBDatastructure').
cycPrepending(ft,'Keyword').
cycPrepending(ft,'List').
cycPrepending(ft,'NonAtomicReifiedTerm').
cycPrepending(ft,'NonAtomicTerm').
cycPrepending(ft,'NonAtomicTerm-Askable').
cycPrepending(ft,'NonAtomicTerm-Assertible').
cycPrepending(ft,'NonAtomicTerm-ClosedFunctor').
cycPrepending(ft,'NonNegativeInteger').
cycPrepending(ft,'NonVariableNonKeywordSymbol').
cycPrepending(ft,'NonVariableSymbol').
cycPrepending(ft,'OpenDenotationalTerm').
cycPrepending(ft,'OpenExpression').
cycPrepending(ft,'OpenFormula').
cycPrepending(ft,'OpenNonAtomicTerm').
cycPrepending(ft,'OpenSentence').
cycPrepending(ft,'PositiveInteger').
cycPrepending(ft,'PropositionalSentence').
cycPrepending(ft,'RealNumber').
cycPrepending(ft,'ReifiableDenotationalTerm').
cycPrepending(ft,'ReifiableNonAtomicTerm').
cycPrepending(ft,'ReifiedDenotationalTerm').
cycPrepending(ft,'RemovalModuleSupportedCollection-Generic').
cycPrepending(ft,'RemovalModuleSupportedPredicate-Generic').
cycPrepending(ft,'RemovalModuleSupportedPredicate-Specific').
cycPrepending(ft,'RepresentedAtomicTerm').
cycPrepending(ft,'RepresentedTerm').
cycPrepending(ft,'RuleAssertion').
cycPrepending(ft,'ScalarIntegralValue').
cycPrepending(ft,'Sentence').
cycPrepending(ft,'SentenceAskable').
cycPrepending(ft,'SentenceAssertible').
cycPrepending(ft,'String').
cycPrepending(ft,'SupportDatastructure').
cycPrepending(ft,'Symbol').
cycPrepending(ft,'TheTerm').
cycPrepending(ft,'TransformationModuleSupportedCollection').
cycPrepending(ft,'TruthValueSentence').
cycPrepending(v,'SingleEntry').
cycPrepending(v,'SetTheFormat').
cycPrepending(t,'UnreifiableFunction').
cycPrepending(t,'UnaryFunction').
cycPrepending(t,'UnaryPredicate').
cycPrepending(t,'BinaryFunction').


:-dynamic(mpred_to_cyc/2).

mpred_to_cyc(tCol,'Collection').
mpred_to_cyc(ttFormatType,'CycLExpressionType').
mpred_to_cyc(ttFormatType,'ExpressionType').
mpred_to_cyc(tPred,'Predicate').
mpred_to_cyc(tFunction,'Function-Denotational').

mpred_to_cyc(ftInt,'Integer').

mpred_to_cyc(tAgent,'Agent-Generic').
mpred_to_cyc(ftSentence,'icSentenceSentence').
mpred_to_cyc(ftSentence,'CycLFormulaicSentence').
mpred_to_cyc(ftSentence,'SubLFormulaicSentence').
mpred_to_cyc(ftSentence,'FormulaicSentence').
mpred_to_cyc(ftVar,'CycLVariable').
mpred_to_cyc(ftVar,'Variable').
:-forall(cycPrepending(ft,A),((atom_concat('ft',A,FT),assert(mpred_to_cyc(FT,A))))).


notFormatType(tIndividual).
notFormatType(tInferenceSupportedFunction).
notFormatType(tInferenceSupportedPredicate).
notFormatType(tNonNegativeScalarInterval).
notFormatType(tScalarInterval).
notFormatType(tScalarPointValue).
notFormatType(tSentenceClosedPredicate).
notFormatType(tThing).
notFormatType(tTransformationModuleSupportedPredicate).
:-forall(notFormatType(NFT),pfc_add(tSet(NFT))).



mpred_prepend_type(X,_):- \+ atom(X),!,fail.
mpred_prepend_type(X,PP):-cycPrepending(PP,X),!.
mpred_prepend_type(X,_):- name(X,[S|_]),char_type(S,lower),!,fail.
mpred_prepend_type(X,_):-atom_concat('ft',_,X),!,fail.
mpred_prepend_type(X,_):-atom_concat('t',_,X),!,fail.
mpred_prepend_type(X,ft):- tinyKB(isa(X,ttFormatType)).
mpred_prepend_type(X,ft):- tinyKB(isa(X,'SubLExpressionType')).
mpred_prepend_type(X,ft):- tinyKB(genls(X,'SubLExpression')).
mpred_prepend_type(X,ft):- tinyKB(genls(X,'CycLExpression')).
mpred_prepend_type(X,ft):- tinyKB(genls(X,ftExpression)).
mpred_prepend_type(X,tt):- tinyKB(genls(X,'tCol')).
% mpred_prepend_type(X,i):-atom_concat(_,'Fn',X).
mpred_prepend_type(X,t):-atom_concat(_,'Function',X).
mpred_prepend_type(X,t):-atom_concat(_,'Relation',X).
mpred_prepend_type(X,t):-atom_concat(_,'Predicate',X).
mpred_prepend_type(X,t):- tinyKB(isa(X,'tCol')).
mpred_prepend_type(X,t):- tinyKB(isa(_,X)).
mpred_prepend_type(X,tt):- tinyKB(isa(X,'AtemporalNecessarilyEssentialCollectionType')).
% mpred_prepend_type(X,v):- name(X,[C|_]),char_type(C,upper),!.

tinyKB_wstr(P):-tUndressedMt(MT),tinyKB(P,MT,_).
tinyKB_wstr(ist(MT,P)):-tDressedMt(MT),tinyKB(P,MT,_).

wkb0:- tell(fooooo0),
      forall(tinyKB_All(V,MT,STR),format('~q.~n',[tinyKB_All(V,MT,STR)])),
      told.

wkbe:- statistics(cputime,S),tell(foof),ignore((el_holds_pred_impl(F),between(2,16,A),current_predicate(F/A),functor(P,F,A),forall(P,format('~q.~n',[P])),fail)),told,
   statistics(cuptime,E),Total is E - S, writeln(Total).

wkb01:- tell(fooooo0),
      forall(tinyKB_All(V,MT,STR),format('~q.~n',[tinyKB_All(V,MT,STR)])),
      told.

wkb1:- tell(fooooo1),
      ignore(( tinyKB(D),cyc_to_pfc_expansion(D,KB),format('~N~q.~N',[KB]),fail)),
      told.
wkb2:- tell(fooooo2),
      ignore(( tinyKB(D,MT,Str),cyc_to_pfc_expansion(D,KB),format('~N~q.~N',[proof(KB,D,MT,Str)]),fail)),
      told.


:-export(cyc_to_pfc_idiom/2).
%cyc_to_pfc_idiom(different,dif).
cyc_to_pfc_idiom(equiv,(<=>)).
cyc_to_pfc_idiom(implies,(=>)). 
cyc_to_pfc_idiom(not,(neg)).
cyc_to_pfc_idiom(C,P):-
  transitive_lc(cyc_to_pfc_idiom1,C,M),!,
   ((mpred_prepend_type(C,PT),not(atom_concat(PT,_,M)))
     -> (atom_concat(PT,M,P),asserta(mpred_to_cyc(P,M))); M=P).

cyc_to_pfc_idiom1(C,P):-nonvar(C),mpred_to_cyc(P,C),!.
cyc_to_pfc_idiom1('CycLTerm','CycLExpression').
cyc_to_pfc_idiom1(C,P):-atom_concatM('CycLSentence-',Type,C),!,atom_concat('Sentence',Type,P).
cyc_to_pfc_idiom1(C,P):-atom_concatM('Expression-',Type,C),!,atom_concat('Expression',Type,P).
cyc_to_pfc_idiom1(C,P):-atom_concatM('CycL',P,C).
cyc_to_pfc_idiom1(C,P):-atom_concatM('SubL',P,C).
cyc_to_pfc_idiom1(C,P):-atom_concatM('Cyclist',Type,C),!,atom_concat('Author',Type,P).
cyc_to_pfc_idiom1(C,P):-atom_concatM('Cyc',P,C).
cyc_to_pfc_idiom1(C,P):-atom_concatM('FormulaicSenten',Type,C),!,atom_concat('Senten',Type,P).
cyc_to_pfc_idiom1(C,P):-atom_concatM('SExpressi',Type,C),!,atom_concat('Expressi',Type,P).
cyc_to_pfc_idiom1(C,P):-atom_concatR(C,Type,'-Assertible'),!,atom_concat(Type,'Assertible',P).
cyc_to_pfc_idiom1(C,P):-atom_concatR(C,Type,'-Askable'),!,atom_concat(Type,'Askable',P).
cyc_to_pfc_idiom1(C,P):-atom_concatR(C,Type,'FormulaicSentence'),!,atom_concat(Type,'Sentence',P).

cyc_to_pfc_idiom_unused([Conj|MORE],Out):-fail, not(is_ftVar(Conj)),!,cyc_to_pfc_sent_idiom_2(Conj,Pred,_),
  with_assertions(thocal:outer_pred_expansion(Conj,MORE),
    ( maplist(cyc_to_pfc_expansion,MORE,MOREL), 
       with_assertions(thocal:outer_pred_expansion(Pred,MOREL),       
         list_to_ops(Pred,MOREL,Out)))),!.

atom_concatM(L,M,R):-atom(L),nonvar(R),atom_concat(L,M,R),atom_length(M,N),!,N > 1.
atom_concatR(L,M,R):-atom(R),nonvar(L),atom_concat(L,M,R),atom_length(M,N),!,N > 1.

cyc_to_pfc_sent_idiom_2(and,(','),trueSentence).

list_to_ops(_,V,V):-is_ftVar(V),!.
list_to_ops(Pred,[],Out):-cyc_to_pfc_sent_idiom_2(_,Pred,Out),!.
list_to_ops(Pred,In,Out):-not(is_list(In)),!,cyc_to_pfc_expansion(In,Mid),cyc_to_pfc_sent_idiom_2(_,Pred,ArityOne),Out=..[ArityOne,Mid].
list_to_ops(_,[In],Out):-!,cyc_to_pfc_expansion(In,Out).
list_to_ops(Pred,[H,T],Body):-!,
    cyc_to_pfc_expansion(H,HH),
    cyc_to_pfc_expansion(T,TT),
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

:-thread_local thocal:outer_pred_expansion/2.

cyc_to_pfc_expansion_notify(B,A):-cyc_to_pfc_expansion(B,A)->B\=@=A,nop(dmsg(A<=B)).
cyc_to_pfc_expansion_entry(I,O):-fail,cyc_to_pfc_expansion(I,M),!,must((functor(I,FI,_),functor(M,MF,_),FI==MF)),O=M.

cyc_to_pfc_expansion(V,V):-is_ftVar(V),!.
cyc_to_pfc_expansion(I,O):-atom(I),must(cyc_to_pfc_idiom(I,O)),!.
cyc_to_pfc_expansion(V,V):-not(compound(V)),!.
cyc_to_pfc_expansion([H|T],[HH|TT]):-!,cyc_to_pfc_expansion(H,HH),cyc_to_pfc_expansion(T,TT),!.
cyc_to_pfc_expansion(HOLDS,HOLDSOUT):-HOLDS=..[F|HOLDSL],
  with_assertions(thocal:outer_pred_expansion(F,HOLDSL),cyc_to_pfc_expansion([F|HOLDSL],[C|HOLDSOUTL])),!,
  ((is_list([C|HOLDSOUTL]), atom(C))-> must(HOLDSOUT=..[C|HOLDSOUTL]) ; HOLDSOUT=[C|HOLDSOUTL]),!.

clip_us(A,AO):-concat_atom(L,'-',A),concat_atom(L,'_',AO).
clip_qm(QA,AO):-atom_concat('??',A1,QA),!,atom_concat('_',A1,A),clip_us(A,AO).
clip_qm(QA,AO):-atom_concat('?',A,QA),!,clip_us(A,AO).
clip_qm(A,AO):-clip_us(A,AO).

fixvars(P,_,[],P):-!.
fixvars(P,N,[V|VARS],PO):-  
     atom_string(Name,V),clip_qm(Name,NB),Var = '$VAR'(NB),
     subst(P,'$VAR'(N),Var,PM0),
     subst(PM0,'$VAR'(Name),Var,PM),
   %  nb_getval('$variable_names', Vs),
  %   append(Vs,[Name=Var],NVs),
  %   nb_setval('$variable_names', NVs),
     N2 is N + 1,fixvars(PM,N2,VARS,PO).


:-dynamic(argIsa/3).
:-multifile(argIsa/3).
:-dynamic(argGenl/3).
:-multifile(argGenl/3).
:-dynamic(argQuotedIsa/3).
:-multifile(argQuotedIsa/3).
/*
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

tinyKB(P):-tUndressedMt(MT),tinyKB(P,MT,_).
tinyKB(ist(MT,P)):-tDressedMt(MT),tinyKB(P,MT,_).


tinyKB(PO,MT,STR):- %fwc,  
  (tUndressedMt(MT);tDressedMt(MT)),(STR=vStrMon;STR=vStrDef), 
  tinyKB_All(PO,MT,STR).

tinyKB_All(V,MT,STR):- tinyAssertion(V,MT,STR).
tinyKB_All(PO,MT,STR):- current_predicate('TINYKB-ASSERTION'/5),!,
    tiny_kb_ASSERTION(PLISTIn,PROPS),
        once((sterm_to_pterm(PLISTIn,P),
               memberchk(amt(MT),PROPS),
               memberchk(str(STR),PROPS), 
              (member(vars(VARS),PROPS)->(nb_setval('$variable_names', []),fixvars(P,0,VARS,PO));PO=P ))).

loadTinyKB:-forall(tinyKB(P,MT,STR),((print_assertion(P,MT,STR),pfc_add(P)))).
% ssveTinyKB:-tinyKB_All(tinyKB(P,MT,STR),tell((print_assertion(P,MT,STR),pfc_add(P)))).

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


tiny_support(CycL,MT,CALL):- compound(CycL),!, CycL=..[F|Args], append(Args,[MT,_STR],WMT),CCALL=..[exactlyAssertedEL,F|WMT],!,
  ((clause(CCALL,true), CCALL=CALL) ; clause(CCALL,(CALL,_))).
tiny_support(CycL,MT,CALL):- between(4,7,Len),functor(CCALL,exactlyAssertedEL,Len),CCALL=..[exactlyAssertedEL,F|WMT],append(Args,[MT,_STR],WMT),
 CCALL,(atom(F)->CycL=..[F|Args];append_termlist(F,Args,CycL)),((clause(CCALL,true), CCALL=CALL) ; clause(CCALL,(CALL,_))).

make_functor_h(CycL,F,A):- length(Args,A),CycL=..[F|Args].

is_simple_gaf(V):-not(compound(V)),!.
is_simple_gaf(V):-needs_canoncalization(V),!,fail.
is_simple_gaf(V):-functor(V,F,A),member(F/A,[isa/2,genls/2,argQuotedIsa/3,afterAdding/2,afterRemoving/2]),!.
is_simple_gaf(V):-needs_indexing(V),!,fail.
is_simple_gaf(_).

needs_indexing(V):-compound(V),arg(_,V,A),not(is_simple_arg(A)),!,fail.

is_simple_arg(A):-not(compound(A)),!.
is_simple_arg(A):-functor(A,Simple,_),tEscapeFunction(Simple).

'tEscapeFunction'('TINYKB-ASSERTION').
'tEscapeFunction'('QuoteFn').
'tEscapeFunction'(X):- 'tUnreifiableFunction'(X).

needs_canoncalization(CycL):-is_ftVar(CycL),!,fail.
needs_canoncalization(CycL):-functor(CycL,F,_),isa_db(F,'SentenceOperator').
needs_canoncalization(CycL):-needs_indexing(CycL).

is_better_backchained(CycL):-is_ftVar(CycL),!,fail.
is_better_backchained(CycL):-functor(CycL,F,_),isa_db(F,'SentenceOperator').
is_better_backchained(V):-unnumbervars(V,FOO),(((each_subterm(FOO,SubTerm),nonvar(SubTerm),isa_db(SubTerm,tAvoidForwardChain)))),!.


as_cycl(VP,VE):-subst(VP,('-'),(neg),V0),subst(V0,('v'),(or),V1),subst(V1,('exists'),(thereExists),V2),subst(V2,('&'),(and),VE),!.


:-dynamic(addTiny_added/1).
addCycL(V):-addTiny_added(V),!.
addCycL(V):-into_mpred_form(V,M),V\=@=M,!,addCycL(M),!.
addCycL(V):-defunctionalize('implies',V,VE),V\=@=VE,!,addCycL(VE).
addCycL(V):-cyc_to_pfc_expansion(V,VE),V\=@=VE,!,addCycL(VE).
addCycL(V):-is_simple_gaf(V),!,addCycL0(V),!.
addCycL(V):-kif_to_boxlog(V,VB),boxlog_to_prolog(VB,VP),V\=@=VP,!,as_cycl(VP,VE),show_call(addCycL0(VE)).
addCycL(V):-addCycL0(V),!.

addCycL0(V):-addCycL1(V).

addCycL1(V):-into_mpred_form(V,M),V\=@=M,!,addCycL0(M),!.
addCycL1(V):-cyc_to_pfc_expansion(V,VE),V\=@=VE,!,addCycL0(VE).
addCycL1((TRUE=>V)):-is_true(TRUE),addCycL0(V),!.
addCycL1((V<=TRUE)):-is_true(TRUE),addCycL0(V),!.
addCycL1((V :- TRUE)):-is_true(TRUE),addCycL0(V),!.
addCycL1((V :- A)):- show_call(addCycL0((A => V))).
addCycL1((A => (V1 , V2))):-not(is_ftVar(V1)),!,show_call(addCycL0((A => V1))) , show_call(addCycL0((A => V2))).
addCycL1((V1 , V2)):-!,addCycL0(V1),addCycL0(V2),!.
addCycL1([V1 | V2]):-!,addCycL0(V1),addCycL0(V2),!.
addCycL1(V):-addTiny_added(V),!.
addCycL1(V):-asserta(addTiny_added(V)),unnumbervars(V,VE),pfc_add(VE),remQueuedTinyKB(VE).


sent_to_conseq(CycLIn,Consequent):- into_mpred_form(CycLIn,CycL), ignore((tiny_support(CycL,_MT,CALL),retract(CALL))),must(cycLToMpred(CycL,Consequent)),!.

:-dynamic(addTiny_added/1).

cycLToMpred(V,CP):-into_mpred_form(V,M),V\=@=M,!,cycLToMpred(M,CP),!.
cycLToMpred(V,CP):-cyc_to_pfc_expansion(V,VE),V\=@=VE,!,cycLToMpred(VE,CP).
cycLToMpred(V,CP):-is_simple_gaf(V),!,cycLToMpred0(V,CP),!.
cycLToMpred(V,CP):-defunctionalize('implies',V,VE),V\=@=VE,!,cycLToMpred(VE,CP).
cycLToMpred(V,CP):-kif_to_boxlog(V,VB),boxlog_to_prolog(VB,VP),V\=@=VP,!,as_cycl(VP,VE),show_call(cycLToMpred0(VE,CP)).
cycLToMpred(V,CP):-cycLToMpred0(V,CP),!.

cycLToMpred0(V,CP):-into_mpred_form(V,M),V\=@=M,!,cycLToMpred0(M,CP),!.
cycLToMpred0(V,CP):-cyc_to_pfc_expansion(V,VE),V\=@=VE,!,cycLToMpred0(VE,CP).
cycLToMpred0((TRUE=>V),CP):-is_true(TRUE),cycLToMpred0(V,CP),!.
cycLToMpred0((V<=TRUE),CP):-is_true(TRUE),cycLToMpred0(V,CP),!.
cycLToMpred0((V :- TRUE),CP):-is_true(TRUE),cycLToMpred0(V,CP),!.
cycLToMpred0((V :- A),CP):- show_call(cycLToMpred0((A => V),CP)).
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

:-dynamic(isCycUnavailable_known/1).
:-dynamic(isCycAvailable_known/0).

:-export(isCycAvailable/0).
isCycAvailable:-isCycUnavailable_known(_),!,fail.
isCycAvailable:-isCycAvailable_known,!.
isCycAvailable:-checkCycAvailablity,isCycAvailable.

:-export(isCycUnavailable/0).
isCycUnavailable:-isCycUnavailable_known(_),!.
isCycUnavailable:-isCycAvailable_known,!,fail.
isCycUnavailable:-checkCycAvailablity,isCycUnavailable.

:-export(checkCycAvailablity/0).
checkCycAvailablity:- (isCycAvailable_known;isCycUnavailable_known(_)),!.
checkCycAvailablity:- ccatch((ignore((invokeSubL("(+ 1 1)",R))),(R==2->assert_if_new(isCycAvailable_known);assert_if_new(isCycUnavailable_known(R)))),E,assert_if_new(isCycUnavailable_known(E))),!.

:- dmsg("Loading ehe tinyKB should take under a minute").
:- gripe_time(60,user:ensure_loaded(logicmoo(plarkc/logicmoo_i_cyc_kb_tinykb))).


:-prolog.
