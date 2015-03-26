/** <module>
% ===================================================================
% File 'dbase_i_builtin.pl'
% Purpose: Emulation of OpenCyc for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface' 1.0.0
% Revision: $Revision: 1.9 $
% Revised At: $Date: 2002/06/27 14:13:20 $
% ===================================================================
% File used as storage place for all predicates which change as
% the world is run.
%
% props(Obj,height(ObjHt)) == holds(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt)) == padd(height,Obj,ObjHt,...) == moo(QueryForm)
% kretract[all](Obj,height(ObjHt)) == kretract[all](Obj,height,ObjHt) == pretract[all](height,Obj,ObjHt) == del[all](QueryForm)
% keraseall(AnyTerm).
%
%
% Dec 13, 2035
% Douglas Miles
*/

:- op(500,fx,'~').
:- op(1050,xfx,('=>')).
:- op(1050,xfx,'<=>').
:- op(1050,xfx,('<=')).
:- op(1100,fx,('=>')).
:- op(1150,xfx,('::::')).
:-dynamic(subFormat/2).

tCol(X)=>{decl_type(X)}.
:-decl_type(tBird).

:- meta_predicate(neg(0)).
:- dynamic(neg(0)).


:- meta_predicate(mp_test_agr(?,+,-,*,^,:,0,1,5,9)).
% becomes         mp_test_agr(+,+,-,?,^,:,0,1,0,0)

mp_test_agr(_,_,_,_,_,_,_,_,_,_).

:-show_call(predicate_property(mp_test_agr(_,_,_,_,_,_,_,_,_,_),meta_predicate(_))).

:-pfcTrace.

:- include(dbase_i_header).

:-must(assert_argIsa(tPred,1,tPred)).


:-dynamic(pfcDefault/1).
% -*-Prolog-*-
% here is an example which defines pfcDefault facts and rules.  Will it work?



:-dynamic(conflict/1).
% a conflict triggers a Prolog action to resolve it.
((conflict(C) => {resolveConflict(C)})).

:-dynamic(resolveConflict/1).
% this isnt written yet.
resolveConflict(C) :-
  format("~NHalting with conflict ~w", [C]),
  pfcHalt.

% meta rules to schedule inferencing.

% resolve conflicts asap
((pfcSelect(conflict(X)) :- pfcQueue(conflict(X)))).
  
% a pretty basic conflict.
(({pfcLiteral(P)}, neg(P), P => conflict(P))).

/*
% reflexive equality
equal(A,B) => equal(B,A).
equal(A,B),{ \+ (A=B}),equal(B,C),{ \+ (A=C)} => equal(A,C).

notequal(A,B) <= notequal(B,A).
notequal(C,B) <= equal(A,C),notequal(A,B).
*/

% is this how to define constraints?
% either(P,Q) => (neg(P) => Q), (neg(Q) => P).
% (P,Q => false) => (P => neg(Q)), (Q => neg(P)).


:-export(member/2).
:-export(arg/3).
%:-export(mpred_call/1).
:-decl_mpred_prolog(cycAssert/2).
:-export(integer/1).
% :-export(makeConstant/1).
% :-export(naf/1).
:-export(number/1).
:-export(string/1).
:-export(var/1).

:- decl_type(completelyAssertedCollection).
:- decl_type(completeExtentAsserted).
:- decl_type(ttFormatType).
:- decl_type(functorDeclares).
:- decl_mpred_hybrid isa/2.

% :- decl_mpred_pfc neg/1.
:- decl_mpred_hybrid genls/2.
:- decl_mpred_hybrid(( tCol/1, genls/2, predArgTypes/1)).
:- decl_mpred_hybrid(typeProps/2).
:- must(arity(typeProps,2)).
:- add((argIsa(isEach(tPred,prologMultiValued,prologOrdered,prologNegByFailure,predArgTypes,prologHybrid,prologPTTP,predCanHaveSingletons,prologOnly,prologMacroHead,prologListValued,prologSingleValued),1,tPred))).
:- add((argIsa(isEach(tPred,prologMultiValued,prologOrdered,prologNegByFailure,predArgTypes,prologHybrid,prologPTTP,prologOnly,prologMacroHead,prologListValued,prologSingleValued),2,ftListFn(ftVoprop)))).
:- add((isa(isEach(prologMultiValued,prologOrdered,prologNegByFailure,predArgTypes,prologPTTP,prologHybrid,predCanHaveSingletons,prologOnly,prologOnly,prologMacroHead,prologListValued,prologSingleValued),functorDeclares))).
:- add((genls(isEach(prologMultiValued,prologOrdered,prologNegByFailure,predArgTypes,prologHybrid,prologPTTP,prologOnly,prologMacroHead,prologListValued,prologSingleValued),tPred))).
:- assert_hasInstance(tCol,tCol).
:- begin_transform_moo_preds.
:- debug.
%TODO FIX :- decl_mpred(tDeleted(ftID),[predIsFlag]).
:- decl_mpred_hybrid disjointWith/2.
:- decl_mpred_hybrid(( ttNotSpatialType/1,ttSpatialType/1 )).
:- decl_mpred_hybrid((genlInverse/2,genlPreds/2)).
:- decl_mpred_hybrid(argIsa/3).
:- decl_mpred_hybrid(argSingleValueDefault, 3).
:- decl_mpred_hybrid(disjointWith/2).
:- decl_mpred_hybrid(instTypeProps/3).
:- decl_mpred_hybrid(predModule, 2).
:- decl_mpred_hybrid(predProxyAssert,2).
:- decl_mpred_hybrid(predProxyQuery, 2).
:- decl_mpred_hybrid(predProxyRetract, 2).
:- decl_mpred_hybrid(predTypeMax/3).
:- decl_mpred_hybrid(prologSingleValued/1).
:- decl_mpred_hybrid(resultIsa/2).
:- decl_mpred_hybrid(genls/2).
:- decl_mpred_hybrid(isa/2).
:- decl_mpred_hybrid(genls/2).
:- decl_mpred_hybrid(typeGenls/2).
:- decl_mpred_prolog(arg/3).
:- decl_type(predArgTypes).
:- decl_type(prologMultiValued).
:- decl_type(prologSingleValued).
:- decl_type(tCol).
:- decl_type(tFunction).
:- decl_type(tInferInstanceFromArgType).
:- decl_type(tPred).
:- decl_type(tRelation).
:- decl_type(ttFormatted).
:- decl_type(ttSpatialType).
:- decl_type(ttTypeType).
:- decl_type(tPathway).

:- decl_type(ttValueType).
:- decl_type(vtActionTemplate).
:- define_ft(ftString).
:- define_ft(ftVar).
%:-export(repl_to_string(tAgentGeneric,ftTerm)).
%:-export(repl_writer/2).
%:-export(repl_writer(tAgentGeneric,ftTerm)).
%prologHybrid(typeProps(tCol,ftVoprop)).
:- discontiguous(prologSingleValued/1).
:- do_gc.
:- forall(is_pred_declarer(F),must((decl_type(F),add(isa(F,functorDeclares)),add(genls(F,tPred))))).
:- export mtForPred/2.
:- user:decl_mpred_hybrid((argIsa/3, formatted_resultIsa/2, localityOfObject/2, subFormat/2, isa/2, mudLabelTypeProps/3, genls/2, pddlSomethingIsa/2, resultIsa/2, subFormat/2, tCol/1, tRegion/1, completelyAssertedCollection/1, ttFormatType/1, typeProps/2)).
:-add(isa(tObj,ttSpatialType)).
:-add(isa(tRegion,ttSpatialType)).
:-add(isa(ttFormatType,ttAbstractType)).
:-add(predArgTypes(typeGenls(ttTypeType,tCol))).


subFormat(ftDeplictsFn(tCol),ftSpec).
subFormat(ftDeplictsFn(ttFormatted),ftSpec).
subFormat(ftVoprop,ftSpec).

ttFormatted(isEach(ftRest(ftTerm))).
ttFormatted(isRandom(tCol)).
ttFormatted(isAnd(ftRest(ftSpec))).
ttFormatted(isMost(ftRest(ftSpec))).
ttFormatted(isOneOf(ftRest(ftSpec))).
ttFormatted(isNot(ftSpec)).
ttFormatted(isOptional(ftSpec,ftTerm)).
ttFormatted(isOptionalStr(ftString)).
ttFormatted(exactStr(ftString)).

resultIsa(ftDeplictsFn,ftSpec).

user:mpred_prop(defnSufficient,prologOnly).

isa(argIsa,prologHybrid).
isa(determinerString, prologMultiValued).
isa(defnSufficient, completeExtentAsserted).
isa(ftInt,ttFormatType).
isa(ftNumber,ttFormatType).
isa(ftString,ttFormatType).
isa(isInstFn,tFunction).
isa(isKappaFn,tFunction).
isa(prologMultiValued, tCol).
arity(ftListFn,1).
arity(isLikeFn,2).
arity(ftDeplictsFn,1).
ttFormatted(ftDice(ftInt,ftInt,ftInt)).
ttFormatted(ftListFn(ftRest)).
ttFormatted(ftDeplictsFn(tCol)).



tCol(completeIsaAsserted).
%completelyAssertedCollection(Ext):- fwc, arg(_,vv(tCol,vtDirection,ttFormatType,tRegion,ftString,genlPreds),Ext).
completeExtentAsserted(formatted_resultIsa).
completeExtentAsserted(defnSufficient).
completelyAssertedCollection(completelyAssertedCollection).
ttFormatType(ftString).
ttFormatType(ftVar).
ttFormatType(ftVoprop).


:- pfcAdd(((isa(Compound,prologMacroHead)/compound_functor(Compound,F)) => functorDeclares(F))).
:- pfcAdd((isa(_,ArgsIsa)=>tCol(ArgsIsa))).

:- pfcTrace.
%:- pfcWatch.
:- pfcWarn.
% next_test :- sleep(1),pfcReset.


% :-dynamic((disjointWith/2,genls/2)).


predArgTypes(argQuotedIsa(tRelation,ftInt,ttFormatType)).
predArgTypes(argIsa(tRelation,ftInt,tCol)).
predArgTypes(argSingleValueDefault(prologSingleValued,ftInt,ftTerm)).
predArgTypes(formatted_resultIsa(ttFormatType,tCol)).
predArgTypes(defnSufficient(ttFormatType,ftTerm)).
predArgTypes(isLikeFn(tPred,tCol)).
predArgTypes('=>'(ftAskable,ftAssertable)).
predArgTypes('<='(ftAssertable,ftAskable)).
prologHybrid(instTypeProps(ftID,tCol,ftRest(ftVoprop))).
prologHybrid(subFormat(ttFormatType,ttFormatType)).
prologMacroHead(macroSomethingDescription(ftTerm,ftListFn(ftString))).
prologMacroHead(pddlObjects(tCol,ftListFn(ftID))).
prologMacroHead(pddlPredicates(ftListFn(ftVoprop))).
prologMacroHead(pddlSomethingIsa(ftTerm,ftListFn(tCol))).
prologMacroHead(pddlSorts(tCol,ftListFn(tCol))).
prologMacroHead(pddlTypes(ftListFn(tCol))).
prologMultiValued(comment(ftTerm,ftString)).
prologMultiValued(genlInverse(tPred,tPred)).
prologMultiValued(genlPreds(tPred,tPred)).
prologMultiValued(predModule(tRelation,ftAtom)).
prologMultiValued(predProxyAssert(prologMultiValued,ftTerm)).
prologMultiValued(predProxyQuery(prologMultiValued,ftTerm)).
% prologMultiValued('<=>'(ftTerm,ftTerm)).
prologMultiValued('<='(ftAssertable,ftAskable)).
prologMultiValued('=>'(ftAskable,ftAssertable)).
prologNegByFailure(predArgMulti(prologMultiValued,ftInt)).
prologNegByFailure(tDeleted(ftID)).
prologSingleValued(predInstMax(ftID,prologSingleValued,ftInt),prologHybrid).
prologSingleValued(predTypeMax(prologSingleValued,tCol,ftInt),prologHybrid).
resultIsa(txtFormatFn,ftText).
%'<=>'(prologMultiValued(CallSig,[predProxyAssert(hooked_asserta),predProxyRetract(hooked_retract),predProxyQuery(call)]),prologOnly(CallSig)).
%'<=>'(prologMultiValued(CallSig,[predProxyAssert(pttp_tell),predProxyRetract(pttp_retract),predProxyQuery(pttp_ask)]),prologPTTP(CallSig)).
subFormat(ftAtom,ftTerm).
subFormat(ftCallable,ftProlog).
resultIsa(ftDice,ftInt).
subFormat(ftID,ftTerm).
subFormat(ftInt,ftNumber).
subFormat(ftInteger,ftNumber).
subFormat(ftNumber,ftPercent).
subFormat(ftPercent,ftNumber).
subFormat(ftString,ftTerm).
subFormat(ftString,ftText).
subFormat(ftTerm,ftProlog).
subFormat(ftText,ftTerm).
subFormat(ftVar,ftProlog).
subFormat(ftVoprop,ftRest(ftVoprop)).
subFormat(ftVoprop,ftTerm).

tCol(tChannel).
tChannel(A):- tAgentGeneric(A).
tChannel(A):- tRegion(A).
tChannel(iGossupChannel).

:-decl_type(ttNewlyCreated).

typeGenls(tAgentGeneric,ttAgentType).
typeGenls(tItem,ttItemType).
typeGenls(tObj,ttObjectType).
typeGenls(tPred,ttPredType).
typeGenls(tRegion,ttRegionType).
typeGenls(ttAgentType,tAgentGeneric).
typeGenls(ttFormatTypeType,ttFormatType).
typeGenls(ttItemType,tItem).
typeGenls(ttObjectType,tObj).
typeGenls(ttPredType,tPred).
typeGenls(ttRegionType,tRegion).
typeGenls(ttSpatialType,tSpatialThing).
typeGenls(ttTypeFacet,tCol).
typeGenls(ttTypeType,tCol).

ttTypeFacet(ttUnverifiableType).
ttUnverifiableType(ftDice).
ttUnverifiableType(ftID).
ttUnverifiableType(ftListFn(ftTerm)).
ttUnverifiableType(ftString).
ttUnverifiableType(ftTerm).
ttUnverifiableType(ftText).
ttUnverifiableType(ftVoprop).
ttUnverifiableType(tCol).
ttUnverifiableType(tFunction).
ttUnverifiableType(tPred).
ttUnverifiableType(ttFormatType).
ttUnverifiableType(vtDirection).


is_pred_declarer(ArgsIsa)=>isa(ArgsIsa,tCol).
%TODO isa(_,ArgsIsa)=>tCol(ArgsIsa).
cycAssert(A,B):- trace_or_throw(cycAssert(A,B)).

/*
disjointWith(A,B):- A=B,!,fail.
disjointWith(A,B):- disjointWithT(A,B).
disjointWith(A,B):- disjointWithT(AS,BS),transitive_subclass_or_same(A,AS),transitive_subclass_or_same(B,BS).
disjointWith(A,B):- once((type_isa(A,AT),type_isa(B,BT))),AT \= BT.
*/
disjointWith(Sub, Super) => disjointWith( Super, Sub).
disjointWith(tObj,tRegion).
disjointWith(tRegion,tObj).
disjointWith(ttSpatialType,ttAbstractType).

prologHybrid(dividesBetween(tCol,tCol,tCol)).

dividesBetween(S,C1,C2) => (disjointWith(C1,C2) , genls(C1,S) ,genls(C2,S)).
dividesBetween(tItem,tMassfull,tMassless).
dividesBetween(tObj,tItem,tAgentGeneric).
dividesBetween(tObj,tMassfull,tMassless).
dividesBetween(tSpatialThing,tObj,tRegion).
formatted_resultIsa(ftDice(ftInt,ftInt,ftInt),ftInt).

defnSufficient(ftInt,integer).
defnSufficient(ftFloat,float).
defnSufficient(ftAtom,atom).
defnSufficient(ftString,string).
defnSufficient(ftCallable,is_callable).
defnSufficient(ftCompound,compound).
defnSufficient(ftGround,ground).
defnSufficient(ftID,is_id).
defnSufficient(ftTerm,nonvar).
defnSufficient(ftVar,var).
defnSufficient(ftNonvar,nonvar).
defnSufficient(ftNumber,number).
defnSufficient(ftRest,is_rest).
defnSufficient(ftRest(Type),is_rest_of(Type)).
defnSufficient(ftListFn(Type),is_list_of(Type)).
defnSufficient(ftBoolean,is_boolean).
defnSufficient(ftText,is_string).
defnSufficient(ftCodeIs(SomeCode),SomeCode):-nonvar(SomeCode).

isa(arity,tBinaryPredicate).

(arity(Pred,2),tPred(Pred)) <=> isa(Pred,tBinaryPredicate).
prologHybrid(relationMostInstance(tBinaryPredicate,tCol,ftValue)).
relationMostInstance(BP,_,_)=>tBinaryPredicate(BP).
prologHybrid(relationAllInstance(tBinaryPredicate,tCol,ftValue)).
relationAllInstance(BP,_,_)=>tBinaryPredicate(BP).

(isa(Inst,ttSpatialType), tCol(Inst)) => genls(Inst,tSpatialThing).
% (isa(Inst,Type), tCol(Inst)) => isa(Type,ttTypeType).
% (isa(TypeType,ttTypeType) , isa(Inst,TypeType), genls(SubInst,Inst)) => isa(SubInst,TypeType).

(ttFormatType(FT),{compound(FT)})=>ttFormatted(FT).

=> tCol(vtDirection).

disjointWith(Sub, Super) => disjointWith( Super, Sub).
disjointWith(tObj,tRegion).
disjointWith(ttSpatialType,ttAbstractType).


genls(tPartOfobj,tItem).

:-decl_mpred_hybrid(dividesBetween(tCol,tCol,tCol)).
(dividesBetween(tAgentGeneric,tMale,tFemale)).

% dividesBetween(tItem,tPathways).
dividesBetween(tItem,tMassfull,tMassless).
dividesBetween(tObj,tItem,tAgentGeneric).
dividesBetween(tObj,tMassfull,tMassless).
dividesBetween(tSpatialThing,tObj,tRegion).
dividesBetween(tAgentGeneric,tPlayer,tNpcPlayer).

dividesBetween(S,C1,C2) => (disjointWith(C1,C2) , genls(C1,S) ,genls(C2,S)).

% disjointWith(P1,P2) => ((neg(isa(C,P1))) <=> isa(C,P2)).

% isa(Col1, ttObjectType) => neg(isa(Col1, ttFormatType)).

=> tCol(tCol).
=> tCol(tPred).
=> tCol(tFunction).
=> tCol(tRelation).
=> tCol(ttSpatialType).
=> tCol(ttFormatType).
=> tCol(functorDeclares).
% tCol(ArgsIsa):-is_pred_declarer(ArgsIsa).
% TODO decide if OK
%tCol(F):-hasInstance(functorDeclares,F).
=> tCol(ttFormatType).
=> tCol(vtActionTemplate).
=> tCol(tRegion).
=> tCol(tContainer).

(mpred_prop(_,predArgTypes(ArgTypes)),{is_declarations(ArgTypes)}) => ({is_declarations(ArgTypes)}, predArgTypes(ArgTypes)).


% tCol(Type),(tBinaryPredicate(Pred)/(functor(G,Pred,2),G=..[Pred,isInstFn(Type),Value])), G => relationMostInstance(Pred,Type,Value).


isa(tRegion,ttSpatialType).
isa(tRelation,ttAbstractType).


% rtrace(Goal):- Goal. % (notrace((visible(+all),visible(+unify),visible(+exception),leash(-all),leash(+exception))),(trace,Goal),leash(+all)).

% :- gutracer.

tCol(tFly).

neg(isa(I,Super)) <= (isa(I,Sub), disjointWith(Sub, Super)).

(tCol(Inst), {isa_from_morphology(Inst,Type)}) => isa(Inst,Type).

% HOW TO MAKE THIS FAST? isa(Inst,Type) <= {isa_from_morphology(Inst,Type)}.

%((disjointWith(P1,P2) , genls(C1,P1), {dif:dif(C1,P1)}) =>    disjointWith(C1,P2)).
% (disjointWith(C1,P2) <= (genls(C1,P1), {dif:dif(C1,P1)}, disjointWith(P1,P2))).

tCol(ttPreAssertedCollection).
tCol(completeIsaAsserted).
% genls(completeIsaAsserted,tSpatialThing).
genls(ttPreAssertedCollection,tCol).
ttPreAssertedCollection(ttPreAssertedCollection).
ttPreAssertedCollection(tPred).
ttPreAssertedCollection(tCol).
ttPreAssertedCollection(ttFormatType).
ttPreAssertedCollection(ttTypeType).
ttPreAssertedCollection(tItem).
ttPreAssertedCollection(tRegion).
ttPreAssertedCollection(tObj).
ttPreAssertedCollection(tAgentGeneric).
ttPreAssertedCollection(tCarryAble).
ttPreAssertedCollection(vtVerb).
genls(ttTypeByAction,ttPreAssertedCollection).
genls(ttPreAssertedCollection,completelyAssertedCollection).

((completeIsaAsserted(I), isa(I,Sub), genls(Sub, Super),{ground(Sub:Super)}) => ({dif:dif(Sub, Super)}, isa(I,Super))).
% TODO ADD THIS :-pfcAdd((ttPreAssertedCollection(Super), isa(I,Sub), genls(Sub, Super)) => ({ground(I:Sub:Super),\==(Sub, Super)}, isa(I,Super))).
% (isa(I,Sub), genls(Sub, Super),{ground(Sub:Super)}, ~neg(completelyAssertedCollection(Super))) => ({dif:dif(Sub, Super)}, isa(I,Super)).

( ttFormatted(FT), {dif:dif(FT,COL)}, genls(FT, COL),tCol(COL),{not(isa(COL,ttFormatType))}) => formatted_resultIsa(FT,COL).

% {dif:dif(I,Super),nonvar(I),nonvar(Sub),nonvar(Super)},
(genls(I,Sub),genls(Sub, Super)) => ({nonvar(I),nonvar(Sub),nonvar(Super),\==(I,Super),\==(Sub,Super),\==(I,Sub)},genls(I,Super) , completeExtentAsserted(genls)).


neg(isa(I,Super)) <= (isa(I,Sub), disjointWith(Sub, Super)).

genls(tPartOfobj,tItem).

((isa(Pred,prologSingleValued),arity(Pred,Arity))
  =>
  {
   dynamic(Pred/Arity),
   length(AfterList,Arity),
   append(Left,[A],AfterList),
   append(Left,[B],BeforeList),
  After =.. [Pred|AfterList],
  Before =.. [Pred|BeforeList]},
  (After,{Before, \==(A , B)} => {pfcRem2(Before)})).


% dividesBetween(tItem,tPathway).
dividesBetween(tItem,tMassfull,tMassless).
dividesBetween(tObj,tItem,tAgentGeneric).
dividesBetween(tObj,tMassfull,tMassless).
dividesBetween(tSpatialThing,tObj,tRegion).
dividesBetween(tAgentGeneric,tPlayer,tNpcPlayer).

dividesBetween(S,C1,C2) => (disjointWith(C1,C2) , genls(C1,S) ,genls(C2,S)).

disjointWith(P1,P2) => (not(isa(C,P1)) <=> isa(C,P2)).

isa(Col1, ttObjectType) => ~isa(Col1, ttFormatType).

=> tCol(tCol).
=> tCol(tPred).
=> tCol(tFunction).
=> tCol(tRelation).
=> tCol(ttSpatialType).
=> tCol(ttFormatType).
/*

% this isn't written yet.
resolveConflict(C) :- dtrace,
  format("~NHalting with conflict ~w", [C]),
  pfcHalt.


% a conflict triggers a Prolog action to resolve it.
conflict(C) => {resolveConflict(C)}.

% meta rules to schedule inferencing.

% resolve conflicts asap
pfcSelect(conflict(X)) :- pfcQueue(conflict(X)).
  
% a pretty basic conflict.
((neg(P), P ) => conflict(P)).

*/

tCol(tFly).
tCol(tCanary).
tCol(tPenguin).
tCol(tBird).
=> genls(tCanary,tBird).
=> genls(tPenguin,tBird).
%(isa(A, tBird) =>isa(A, tFly)).
%(isa(A, tBird), ~neg(isa(A, tFly))=>isa(A, tFly)).

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 
% % % These next two have been comnbined with the two following % % %
(((pfcDefault(P)/pfcLiteral(P))  =>  (~neg(P) => P))).
((pfcDefault((P => Q))/pfcLiteral(Q) => (P, ~neg(Q) => Q))).

%((pfcDefault(P)/pfcLiteral(P), {pfcVerifyMissing(P,F)})) =>  ((F, ~neg(P)) => P).
%((pfcDefault((P => Q))/pfcLiteral(Q), {pfcVerifyMissing(Q,F)})) => ((P, F, ~neg(Q)) => Q).
% % % 
(pfcDefault((Q <= P))/pfcLiteral(Q)) => (Q <=(P, ~neg(Q))).
%(pfcDefault((P => Q))/pfcLiteral(Q)) => (Q <=(P, ~neg(Q))).
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 

 neg(P) <= {pfcVerifyMissing(P,F,Test)},Test,{F\=P}.

% is this how to define constraints?
either(P,Q) => (neg(P) => Q), (neg(Q) => P).
% (P,Q => false) => (P => neg(Q)), (Q => neg(P)).

% rembmer the tCol rule points to isa/2
tCol(C)=>{atom(C),P=..[C,I],assertz_if_new(P:-infoF(isa(I,C)))}.

%((relationMostInstance(Pred,Type,Value),{G=..[Pred,Inst,Value],GI=..[Pred,Inst,_]})) => (({GI=..[Pred,Inst,_]},isa(Inst,Type), ~GI) => G ).
((relationAllInstance(Pred,Type,Value),{G=..[Pred,Inst,Value]})) =>  ((isa(Inst,Type), {G=..[Pred,Inst,Value]} => G )).
relationMostInstance(Pred,Type,Value),{G=..[Pred,Inst,Value]} => pfcDefault(isa(Inst,Type) => G).

:-decl_mpred_hybrid((zDefault/1,zPenguin/1,zFly/1,zBird/1,zCanary/1)).

'UnaryPredicate'(Pred)<=>(arity(Pred,1),tPred(Pred)).

'UnaryPredicate'(zDefault).
(zDefault(P)/pfcLiteral(P))  =>  (~neg(P) => P).
zDefault((P => Q))/pfcLiteral(Q) => ((P, ~neg(Q) => Q)).
%zDefault((P => Q))/pfcLiteral(Q) => (Q <=(P, ~neg(Q))).
%zDefault((Q <= P))/pfcLiteral(Q) => (Q <=(P, ~neg(Q))).

% birds fly by default.
=> zDefault((zBird(X) => zFly(X))).

% here's one way to do an isa hierarchy.
% genlPreds = subclass.

/*(genlPreds(C1,C2)) =>
  {P1 =.. [C1,X],
    P2 =.. [C2,X]},
  (P1 => P2).

(genlPreds(C1,C2),arity(C1,2)) =>
  {P1 =.. [C1,X,Y],
    P2 =.. [C2,X,Y]},
  (P1 => P2).

(genlPreds(C1,C2),arity(C1,3)) =>
  {P1 =.. [C1,X,Y,Z],
    P2 =.. [C2,X,Y,Z]},
  (P1 => P2).
*/

(relationMostInstance(Pred,_Arg1Type,Value),argIsa(Pred,2,Type)=>(isa(Value,Type),isa(Pred,tRolePredicate))).
(relationAllInstance(Pred,_Arg1Type,Value),argIsa(Pred,2,Type)=>(isa(Value,Type),isa(Pred,tRolePredicate))).

=> genlPreds(zCanary,zBird).
=> genlPreds(zPenguin,zBird).

% penguins do neg fly.
zPenguin(X) => neg(zFly(X)).

% chilly is a penguin.
=> zPenguin(iChilly).

% tweety is a canary.
=> zCanary(iTweety).


% birds fly by default.
=> (pfcDefault((tBird(X) => tFly(X)))).

% => genls(tBird,tFly).


% penguins do neg tFly.
tPenguin(X) => neg(tFly(X)).

% iChilly is a tPenguin.
((=> tPenguin(iChilly))).

% iTweety is a tCanary.
((=> tCanary(iTweety))).

:-must(in_file_expansion;in_file_directive).

:-listing(tBird).
:-listing(tFly).


:-dynamic((fly/1,bird/1,penguin/1,canary/1)).


% birds fly by default.
(pfcDefault((bird(X) => fly(X)))).

% heres one way to do an subclass hierarchy.

(((genls_test(C1,C2) =>
  {P1 =.. [C1,X],
    P2 =.. [C2,X]},
  (P1 => P2)))).

(genls_test(canary,bird)).
(genls_test(penguin,bird)).

% penguins do neg fly.
(penguin(X) => neg(fly(X))).

% chilly is a penguin.
(penguin(chilly)).

% tweety is a canary.
(canary(tweety)).

:-listing([fly/1,bird/1,penguin/1,canary/1]).

:-must(fly(tweety)).
 

/*
:-prolog.

(isa(COLTYPEINST,COLTYPE) , typeGenls(COLTYPE,COL)) => genls(COLTYPEINST,COL).
genls(_Sub, Super) => tCol(Super).
genls(Sub, _Super) => tCol(Sub).
% use backchain instead (isa(I,Sub), disjointWith(Sub, Super)) => neg(isa(I,Super)).



(genls(I,Sub),{dif:dif(I,Super),is_asserted(genls(I,Sub)),is_asserted(genls(Sub, Super)), nonvar(I),nonvar(Sub),nonvar(Super)})    => (genls(I,Super) , completeExtentAsserted(genls)).


% tCol(Col) <=> isa(Col,tCol).

*/


% :-prolog.


:- must_det(argIsa(genlPreds,2,_Type)).

