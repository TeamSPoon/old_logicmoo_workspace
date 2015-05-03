/** <module>
% ===================================================================
% File 'logicmoo_i_builtin.pl'
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

:-assert_until_end_of_file(infSupertypeName).
:-onEndOfFile(dmsg(infSupertypeName)).
:- pfc_begin.

:- op(500,fx,'~').
:- op(1050,xfx,('=>')).
:- op(1050,xfx,'<=>').
:- op(1050,xfx,('<=')).
:- op(1100,fx,('=>')).
:- op(1150,xfx,('::::')).
tCol(meta_argtypes).
tCol(functorDeclares).
tCol(prologMultiValued).
tCol(prologSingleValued).
tCol(tCol).
tCol(tFunction).
tCol(tInferInstanceFromArgType).
tCol(tPred).
tCol(tRelation).
tCol(meta_argtypes).
tCol(ttSpatialType).
tCol(ttTypeType).


=> neg(arity(pathConnects,1)).

%user:ruleRewrite(isa(isInstFn(Sub),Super),genls(Sub,Super)):-ground(Sub:Super),!.
user:ruleRewrite(mudLabelTypeProps(Lbl,T,[]),typeHasGlyph(T,Lbl)):-nonvar(T),!.
user:ruleRewrite(mudLabelTypeProps(Lbl,T,Props),typeProps(T,[typeHasGlyph(Lbl)|Props])):-nonvar(T),!.


%:-rtrace.
typeGenls(tAgentGeneric,ttAgentType).
%:-nortrace.
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
genls(tSpatialThing,tTemporalThing).
genls(ttSpatialType,ttTemporalType).

ttUnverifiableType(ftDice).
ttUnverifiableType(vtDirection).


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




isa(arity,tBinaryPredicate).

(arity(Pred,2),tPred(Pred)) <=> isa(Pred,tBinaryPredicate).
prologHybrid(relationMostInstance(tBinaryPredicate,tCol,ftValue)).
relationMostInstance(BP,_,_)=>tBinaryPredicate(BP).
prologHybrid(relationAllInstance(tBinaryPredicate,tCol,ftValue)).
relationAllInstance(BP,_,_)=>tBinaryPredicate(BP).

((isa(Inst,ttSpatialType), tCol(Inst)) => genls(Inst,tSpatialThing)).

% (isa(Inst,Type), tCol(Inst)) => isa(Type,ttTypeType).
% (isa(TypeType,ttTypeType) , isa(Inst,TypeType), genls(SubInst,Inst)) => isa(SubInst,TypeType).

(ttFormatType(FT),{compound(FT)})=>meta_argtypes(FT).

=> tCol(vtDirection).

disjointWith(Sub, Super) => disjointWith( Super, Sub).
disjointWith(tObj,tRegion).
disjointWith(ttSpatialType,ttAbstractType).


genls(tPartofObj,tItem).



% dividesBetween(tItem,tPathway).
dividesBetween(tObj,tItem,tAgentGeneric).
dividesBetween(tTemporalThing,tObj,tRegion).
dividesBetween(tAgentGeneric,tPlayer,tNpcPlayer).
%:-export(repl_to_string(tAgentGeneric,ftTerm)).
%:-export(repl_writer/2).
%:-export(repl_writer(tAgentGeneric,ftTerm)).
%prologHybrid(typeProps(tCol,ftVoprop)).

% defined more correctly below dividesBetween(S,C1,C2) => (disjointWith(C1,C2) , genls(C1,S) ,genls(C2,S)).
dividesBetween(tItem,tMassfull,tMassless).
dividesBetween(tObj,tItem,tAgentGeneric).
dividesBetween(tObj,tMassfull,tMassless).
dividesBetween(tSpatialThing,tObj,tRegion).
formatted_resultIsa(ftDice(ftInt,ftInt,ftInt),ftInt).
(dividesBetween(tAgentGeneric,tMale,tFemale)).

% dividesBetween(tItem,tPathways).
dividesBetween(tItem,tMassfull,tMassless).
dividesBetween(tObj,tItem,tAgentGeneric).
dividesBetween(tObj,tMassfull,tMassless).
dividesBetween(tSpatialThing,tObj,tRegion).
dividesBetween(tAgentGeneric,tPlayer,tNpcPlayer).

% dividesBetween(S,C1,C2) => (disjointWith(C1,C2) , genls(C1,S) ,genls(C2,S)).

% disjointWith(P1,P2) => ((neg(isa(C,P1))) <=> isa(C,P2)).

% isa(Col1, ttObjectType) => neg(isa(Col1, ttFormatType)).

=> tCol(tCol).
=> tCol(tPred).
=> tCol(tFunction).
=> tCol(tRelation).
=> tCol(ttSpatialType).
=> tCol(ttFormatType).
=> tCol(functorDeclares).
% tCol(ArgsIsa):-mpred_hooks:mpred_is_trigger(ArgsIsa).
% TODO decide if OK
%tCol(F):-t(functorDeclares,F).
=> tCol(ttFormatType).
=> tCol(vtActionTemplate).
=> tCol(tRegion).
=> tCol(tContainer).

%(mpred_prop(_,meta_argtypes(ArgTypes)),{is_declarations(ArgTypes)}) => meta_argtypes(ArgTypes).


% tCol(Type),(tBinaryPredicate(Pred)/(functor(G,Pred,2),G=..[Pred,isInstFn(Type),Value])), G => relationMostInstance(Pred,Type,Value).


isa(tRegion,ttSpatialType).
isa(tRelation,ttAbstractType).

genlPreds(genls,equals).
% genls(A, B):- tCol(A),{A=B}.

% rtrace(Goal):- Goal. % (hotrace((visible(+all),visible(+unify),visible(+exception),leash(-all),leash(+exception))),(trace,Goal),leash(+all)).

% :- gutracer.


tCol(tFly).


(tCol(Inst), {isa_from_morphology(Inst,Type)}) => isa(Inst,Type).

% HOW TO MAKE THIS FAST? isa(Inst,Type) <= {isa_from_morphology(Inst,Type)}.

%((disjointWith(P1,P2) , genls(C1,P1), {dif:dif(C1,P1)}) =>    disjointWith(C1,P2)).
% (disjointWith(C1,P2) <= (genls(C1,P1), {dif:dif(C1,P1)}, disjointWith(P1,P2))).

tCol(completelyAssertedCollection).
tCol(completeIsaAsserted).
% genls(completeIsaAsserted,tSpatialThing).
genls(completelyAssertedCollection,tCol).
completelyAssertedCollection(tItem).
completelyAssertedCollection(tRegion).
completelyAssertedCollection(tObj).
completelyAssertedCollection(tAgentGeneric).
completelyAssertedCollection(tCarryAble).
completelyAssertedCollection(vtVerb).

genls(ttTypeByAction,completelyAssertedCollection).

% dividesBetween(tItem,tPathway).
dividesBetween(tItem,tMassfull,tMassless).
dividesBetween(tObj,tItem,tAgentGeneric).
dividesBetween(tObj,tMassfull,tMassless).
dividesBetween(tSpatialThing,tObj,tRegion).
dividesBetween(tAgentGeneric,tPlayer,tNpcPlayer).


((dividesBetween(S,C1,C2),{ground(S:C1:C2)}) => ((disjointWith(C1,C2) , genls(C1,S) ,genls(C2,S)))).

isa(Col1, ttObjectType) => ~isa(Col1, ttFormatType).

(neg(isa(I,Super)) <= (disjointWith(Sub, Super),isa(I,Sub))).
% disjointWith(P1,P2) => {\+(isa(P1,ttNonGenled)),\+(isa(P2,ttNonGenled))},(neg(isa(C,P1)) <=> isa(C,P2)).


=> tCol(ttSpatialType).

% Representations
vtActionTemplate(ArgTypes)/is_declarations(ArgTypes) => meta_argtypes(ArgTypes).



argIsa(aDirectionsFn,2,ftListFn(vtDirection)).
argIsa(apathFn,1,tRegion).
argIsa(apathFn,2,vtDirection).
argIsa(localityOfObject,1,tObj).
argIsa(localityOfObject,2,tSpatialThing).
argIsa(mudColor,1,tObj).
argIsa(mudColor,2,vtColor).
argIsa(mudFacing,1,tObj).
argIsa(mudFacing,2,vtDirection).
argIsa(mudMemory,2,ftTerm).

/*
tms_reject_why(mudAtLoc(iArea1025, _),isa(iArea1025,tRegion)).
tms_reject_why(localityOfObject(iArea1025, iOfficeRoom7),isa(iArea1025,tRegion)).
tms_reject_why(localityOfObject(R,_),isa(R,tRegion)):- isa(R,tRegion).
tms_reject_why(mudFacing(R,_),isa(R,tRegion)):- isa(R,tRegion).
tms_reject_why(mudAtLoc(R,_),isa(R,tRegion)):- isa(R,tRegion).

%deduce_facts_forward(localityOfObject(_,Region),isa(Region,tSpatialThing)).
deduce_facts_forward(localityOfObject(Obj,_),isa(Obj,tObj)).
fix_argIsa(F,N,vtDirection(Val),vtDirection):-add(user:mpred_prop(F,argSingleValueDefault(N,Val))),!.

*/

tCol(tChannel).
tCol(tItem).
tCol(vtVerb).

% predIsFlag(tAgentGeneric(ftID),[predIsFlag]).
% prologOnly(createableSubclassType/2).
% alt_forms1(none_AR,localityOfObject(P,R),mudAtLoc(P,L)):-ground(localityOfObject(P,R)),is_asserted(mudAtLoc(P,L)),nonvar(L),once(locationToRegion(L,R)).
% alt_forms1(none_AR,mudAtLoc(P,L),localityOfObject(P,R)):-ground(mudAtLoc(P,L)),once(locationToRegion(L,R)),nonvar(R).
% argsIsa(mudFacing,ftTerm).
% we need a way to call this: maxCapacity
% we need a way to call this: typeMaxCapacity
%:- compile_predicates([isa/2]).
%prologHybrid(repl_to_string(tAgentGeneric,term),[prologSingleValued,argSingleValueDefault(2,default_repl_obj_to_string)]).
% prologHybrid(repl_writer(tAgentGeneric,term),[prologSingleValued,argSingleValueDefault(2,default_repl_writer)]).
%:- forall(ttPredType(F),dynamic(F/1)).
%:- foreach(retract(isa(I,C)),assert_hasInstance(C,I)).
%isa(AT,ttAgentType):- genls(AT,ttAgentGeneric).
%genls(AT,ttAgentGeneric):- isa(AT,ttAgentType).
%subFormat(ftTextType,ftText).
predIsFlag(tItem(ftID),[predIsFlag]).
%predIsFlag(tRegion(ftID),[predIsFlag]).
predIsFlag(tRegion(ftID),tCol).
predIsFlag(tThinking(tAgentGeneric),[predIsFlag]).
prologHybrid(isEach(mudLastCommand/2,mudNamed/2, mudSpd/2,mudStr/2,typeGrid/3)).
prologHybrid(isEach((mudContains/2))).
prologHybrid(isEach((mudLastCmdSuccess/2 ))).

:- do_gc.

:- dynamic(mudDescription/2).
:- dynamic((tItem/1, tRegion/1, instVerbOverride/3,mudNamed/2, determinerString/2, mudKeyword/2 ,descriptionHere/2, mudToHitArmorClass0/2, tThinking/1, tDeleted/1, mudWeight/2, mudPermanence/3, act_term/2, mudAgentTurnnum/2, mudAtLoc/2, mudEnergy/2, mudHealth/2, mudDescription/2, mudFacing/2, mudCmdFailure/2, mudSpd/2, typeGrid/3, mudHeight/2, mudMemory/2, isa/2, pathName/3, mudPossess/2, mudScore/2, mudStm/2, mudStr/2, wearsClothing/2)).
:- dynamic((mudArmorLevel/2, mudLevelOf/2, mudToHitArmorClass0/2, mudBareHandDamage/2, chargeCapacity/2, mudEnergy/2, tCol/1, tAgentGeneric/1, tItem/1, tRegion/1, instVerbOverride/3,mudNamed/2, determinerString/2, mudKeyword/2 ,descriptionHere/2, tThinking/1, mudWeight/2, mudPermanence/3, act_term/2, mudAgentTurnnum/2, mudAtLoc/2, mudEnergy/2, mudHealth/2, mudDescription/2, mudFacing/2, failure/2, gridValue/4, mudHeight/2, mudMemory/2, isa/2, pathName/3, mudPossess/2, mudScore/2, mudStm/2, mudStr/2, mudWearing/2)).
prologHybrid(isEach(tItem/1, tRegion/1, instVerbOverride/3,mudNamed/2, determinerString/2, mudKeyword/2 ,descriptionHere/2, mudToHitArmorClass0/2, tThinking/1, tDeleted/1, mudWeight/2, mudPermanence/3, act_term/2, mudAgentTurnnum/2, mudAtLoc/2, mudEnergy/2, mudHealth/2, mudDescription/2, mudFacing/2, mudCmdFailure/2, mudSpd/2, typeGrid/3, mudHeight/2, mudMemory/2, isa/2, pathName/3, mudPossess/2, mudScore/2, mudStm/2, mudStr/2, wearsClothing/2)).
prologHybrid(isEach( mudArmorLevel/2, mudLevelOf/2, mudToHitArmorClass0/2, mudBareHandDamage/2, chargeCapacity/2, mudEnergy/2, tCol/1, tAgentGeneric/1, tItem/1, tRegion/1, instVerbOverride/3,mudNamed/2, determinerString/2, mudKeyword/2 ,descriptionHere/2, tThinking/1, mudWeight/2, mudPermanence/3, act_term/2, mudAgentTurnnum/2, mudAtLoc/2, mudEnergy/2, mudHealth/2, mudDescription/2, mudFacing/2, failure/2, gridValue/4, mudHeight/2, mudMemory/2, isa/2, pathName/3, mudPossess/2, mudScore/2, mudStm/2, mudStr/2, mudWearing/2)).
prologHybrid(typeHasGlyph,2).
prologHybrid(typeHasGlyph(tCol,ftString)).
prologHybrid(mudActAffect/3).
prologHybrid(mudAtLoc,2).
prologHybrid(mudColor/2).
prologHybrid(mudHealth,2).
prologHybrid(mudMaterial/2).
prologHybrid(mudMaxHitPoints(tAgentGeneric,ftInt)).
prologHybrid(mudNeedsLook,2).
prologHybrid(mudNeedsLook/2,[completeExtentAsserted]).
prologHybrid(mudShape/2).
prologHybrid(mudSize/2).
prologHybrid(mudStowing(tAgentGeneric,tItem)).
prologHybrid(mudTexture/2).
prologHybrid(pathBetween/3).
:-dynamic((latitude/2, mudMoveDist/2, longitude/2)).
prologOnly(mudMoveDist/2).
:- dynamic(mudMoveDist/2).
meta_argtypes(mudMoveDist(tAgentGeneric,ftInt)).
prologSingleValued(mudMoveDist,[mpred_module(user),query(call),argSingleValueDefault(2,1)]).
prologOnly(stat_total/2).
prologOnly(user:verb_alias(ftString,vtVerb)).
tCol(tContainer).
tCol(tRegion).
tCol(vtBasicDir).
tCol(vtBasicDirPlusUpDown).
tCol(vtDirection).
tCol(vtVerb).
:- dynamic stat_total/2.
:- dynamic(spawn_rate/2).
tCol(tMonster).
%prologOnly(user:action_info(vtActionTemplate,ftText)).
prologOnly(agent_call_command(tAgentGeneric,ftAction)).
prologOnly(member(ftTerm,ftTerm)).
prologOnly(mud_test(ftTerm,ftCallable)).
prologOnly(use_action_templates(ftTerm)).
tPred(isEach(tAgentGeneric/1, mudEnergy/2,mudHealth/2, mudAtLoc/2, failure/2, typeGrid/3, gridValue/4, isa/2, tItem/1, mudMemory/2, pathName/3, mudPossess/2, tRegion/1, mudScore/2, mudStm/2, mudFacing/2, localityOfObject/2, tThinking/1, mudWearing/2, mudFacing/2, mudHeight/2, act_term/2, nameStrings/2, mudDescription/2, pathBetween/3, mudAgentTurnnum/2)).
prologHybrid(mudToHitArmorClass0 / 2).
prologHybrid(mudAtLoc/2).
prologOnly((agent_call_command/2)).
:-decl_mpred_hybrid(isEach(argIsa/3, formatted_resultIsa/2, typeHasGlyph/2, inRegion/2, mudContains/2, isa/2, mudLabelTypeProps/3, mudMemory/2, mudPossess/2, mudStowing/2, genls/2, mudToHitArmorClass0/2, 
 pddlSomethingIsa/2, resultIsa/2, subFormat/2, tCol/1, tRegion/1, completeExtentAsserted/1, ttFormatType/1, typeProps/2)).
prologHybrid(isEach(argIsa/3, formatted_resultIsa/2, typeHasGlyph/2, inRegion/2, mudContains/2, isa/2, mudLabelTypeProps/3, mudMemory/2, mudPossess/2, mudStowing/2, genls/2, mudToHitArmorClass0/2, 
 pddlSomethingIsa/2, resultIsa/2, subFormat/2, tCol/1, tRegion/1, completelyAssertedCollection/1, ttFormatType/1, typeProps/2)).



arity(typeHasGlyph,2).
arity(mudTermAnglify,2).
arity(mudMaxHitPoints,2).


prologHybrid(instVerbOverride(ftTerm,ftAction,ftAction)).
%isa(localityOfObject,prologHybrid). 
%isa(mudActAffect, prologMultiValued).
%isa(mudMaxHitPoints,prologHybrid).
isa(vtDirection,ttValueType).

prologMultiValued(agent_text_command(tAgentGeneric,ftText,tAgentGeneric,ftAction)).

formatted_resultIsa(apathFn(tRegion,vtDirection),tPathway).

prologOnly(is_vtActionTemplate/1).

is_vtActionTemplate(C):-nonvar(C),get_functor(C,F),!,atom_concat(act,_,F).
defnSufficient(ftAction,is_vtActionTemplate).

genls('FemaleAnimal',tPlayer).
genls('MaleAnimal',tPlayer).
genls(isEach('PortableObject','ProtectiveAttire','SomethingToWear'),tCarryAble).
genls(isEach('ProtectiveAttire','SomethingToWear'),tWearAble).
genls(isEach(tRegion,tAgentGeneric),tChannel).

tCol(meta_argtypes).
meta_argtypes(aDirectionsFn(ftTerm,ftListFn(ftTerm))).
meta_argtypes(apathFn(tRegion,vtDirection)).
meta_argtypes(xyzFn(tRegion,ftInt,ftInt,ftInt)).

:- do_gc.

tCol(ttTypeByAction).
genls(ttTypeByAction,tCol).
(isa(X,ttTypeByAction) => isa(X,tCol)).


% (isa(Inst,Type),isa(Type,ttTypeByAction)) => isa(Inst,tHasAction).

genls(tAgentGeneric,tObj).
genls(tAgentGeneric,tSpatialThing).
genls(tCarryAble,tItem).
genls(tChargeAble,tItem).
genls(tContolDevice,tChargeAble).
genls(tDoor,tFurniture).
genls(tDoor,tItem).
genls(tDrinkAble,tItem).
genls(tEatAble,tItem).
genls(tFunction,tRelation).
genls(tFurniture,tObj).
genls(tFurniture,tPartofObj).
genls(tHumanPlayer,tPlayer).
genls(tItem,tObj).
genls(tItem,tSpatialThing).
genls(tMonster,ttAgentGeneric).
genls(tNpcPlayer,tPlayer).
genls(tObj,tSpatialThing).
genls(tPathway,tDoor).
genls(tPlayer,tAgentGeneric).
genls(tPred,tRelation).
genls(tRegion,tSpatialThing).
genls(ttObjectType,tCol).
genls(ttSpatialType,tCol).
genls(tUseAble,tItem).
genls(tWearAble,tItem).
genls(vtBasicDir,vtBasicDirPlusUpDown).
genls(vtBasicDirPlusUpDown,vtDirection).
genls(vtDirection,tTypevalue).
genls(vtPosture,vtVerb).

prologHybrid(typeHasGlyph(tCol,ftString)).
prologHybrid(mudColor(tSpatialThing,vtColor)).

prologHybrid(mudKnowing(tAgentGeneric,ftTerm)).
prologHybrid(mudLabelTypeProps(ftString,tCol,ftVoprop)).
prologHybrid(mudListPrice(tItem,ftNumber)).
:-dynamic(mudOpaqueness/2).
prologHybrid(mudOpaqueness(ftTerm,ftPercent)).
prologHybrid(mudPossess(tAgentGeneric,tObj)).
prologHybrid(mudShape(tSpatialThing,vtShape)).
prologHybrid(mudSize(tSpatialThing,ftTerm)).
prologHybrid(mudTextSame(ftText,ftText)).
prologHybrid(mudTexture(tSpatialThing,vtTexture)).
prologHybrid(typeGrid(tCol,ftInt,ftListFn(ftString))).
prologListValued(aDirectionsFn(ftTerm,ftListFn(ftTerm))).
prologListValued(mudGetPrecepts(tAgentGeneric,ftListFn(tSpatialThing)),[mpred_module(user)]).
prologListValued(mudNearFeet(tAgentGeneric,ftListFn(tSpatialThing)),[]).
prologListValued(mudNearReach(tAgentGeneric,ftListFn(tSpatialThing)),[mpred_module(user)]).
prologMultiValued(action_rules(tAgentGeneric,vtVerb,ftListFn(ftVar),ftVoprop)).
prologMultiValued(mudLastCmdSuccess(tAgentGeneric,ftAction)).
prologMultiValued(descriptionHere(ftTerm,ftString)).
prologMultiValued(descriptionHere(ftTerm,ftString),prologOrdered).
prologMultiValued(determinerString(ftTerm,ftString)).
prologMultiValued(typeHasGlyph(ftTerm,ftString)).
prologMultiValued(gridValue(tRegion,ftInt,ftInt,tObj)).
prologMultiValued(instVerbOverride(ftTerm,ftAction,ftAction)).
prologMultiValued(isa(ftTerm,tCol)).
prologMultiValued(mudActAffect(ftTerm,ftTerm,ftTerm)).
prologMultiValued(mudActAffect(tItem,vtVerb,ftTerm(ftVoprop))).
prologMultiValued(mudCmdFailure(tAgentGeneric,ftAction)).

:- do_gc.


prologMultiValued(mudDescription(ftTerm,ftString),prologOrdered,prologHybrid).
prologMultiValued(mudDescription(ftTerm,ftText), [predProxyAssert(add_description),predProxyRetract(remove_description),predProxyQuery(query_description)],prologHybrid).
prologMultiValued(mudDescription(ftTerm,ftText),[predProxyAssert(add_description)],prologHybrid).
prologMultiValued(mudKeyword(ftTerm,ftString),prologHybrid).
prologMultiValued(mudMemory(tAgentGeneric,ftTerm),prologHybrid).
prologMultiValued(mudNamed(ftTerm,ftTerm),[],prologHybrid).
prologMultiValued(mudPossess(tObj,tObj),prologHybrid).
prologMultiValued(nameStrings(ftTerm,ftString),prologHybrid).
prologMultiValued(pathBetween(tRegion,vtDirection,tRegion),prologHybrid).
prologMultiValued(pathName(tRegion,vtDirection,ftString),prologHybrid).
prologMultiValued(genls(tCol,tCol),prologHybrid).
prologMultiValued(typeGrid(tCol,ftInt,ftListFn(ftString)),prologHybrid).
prologMultiValued(verbAsWell(ftTerm,ftAction,ftAction),prologHybrid).

prologNegByFailure(mudNeedsLook(tObj,ftBoolean),prologHybrid).
prologNegByFailure(tAgentGeneric(ftID),prologHybrid).
prologNegByFailure(tCol(ftID),prologHybrid).
prologNegByFailure(tItem(ftID),prologHybrid).
prologNegByFailure(tRegion(ftID),prologHybrid).
prologNegByFailure(tThinking(tAgentGeneric),prologHybrid).
pathName(Region,Dir,Text)=>mudDescription(apathFn(Region,Dir),Text).

prologSingleValued(chargeCapacity(tChargeAble,ftInt),prologHybrid).
prologSingleValued(location_center(tRegion,xyzFn(tRegion,ftInt,ftInt,ftInt)),prologHybrid).
prologSingleValued(mudAgentTurnnum(tAgentGeneric,ftInt),[argSingleValueDefault(2,0)],prologHybrid).
prologSingleValued(mudArmor(tObj,ftInt),prologHybrid).
prologSingleValued(mudArmorLevel(tWearAble,ftInt),prologHybrid).
prologSingleValued(mudAtLoc(tObj,xyzFn(tRegion,ftInt,ftInt,ftInt)),prologHybrid).
prologSingleValued(mudAttack(tObj,ftInt),prologHybrid).
prologSingleValued(mudBareHandDamage(tAgentGeneric,ftInt),prologHybrid).
% prologSingleValued(mudBareHandDamage(tAgentGeneric,ftDice),prologHybrid).
% prologSingleValued(mudEnergy(tChargeAble,ftInt(500)),prologHybrid).
prologSingleValued(mudEnergy(tChargeAble,ftInt),prologHybrid).
prologSingleValued(mudEnergy(tObj,ftInt),[argSingleValueDefault(2,500)],prologHybrid).
prologSingleValued(mudFacing(tObj,vtDirection(vNorth)),prologHybrid).
prologSingleValued(mudFacing(tObj,vtDirection),[argSingleValueDefault(2,vNorth)],prologHybrid).
prologSingleValued(mudHealth(tObj,ftInt),prologHybrid).
prologSingleValued(mudHeight(tObj,ftInt),prologHybrid).
prologSingleValued(mudHeight(tSpatialThing,ftInt),prologHybrid).
prologSingleValued(mudID(tObj,ftID),prologHybrid).
prologSingleValued(mudLastCommand(tAgentGeneric,ftAction),prologHybrid).
prologSingleValued(mudLevelOf(tCarryAble,ftInt),prologHybrid).
prologSingleValued(mudMaxHitPoints(tAgentGeneric,ftInt),[prologHybrid],prologHybrid).
prologSingleValued(mudMoveDist(tAgentGeneric,ftInt),[argSingleValueDefault(2,1)]).
prologSingleValued(mudNeedsLook(tAgentGeneric,ftBoolean),argSingleValueDefault(2,vFalse),prologHybrid).
prologSingleValued(mudPermanence(tItem,vtVerb,vtPerminance),prologHybrid).
prologSingleValued(mudScore(tObj,ftInt),prologHybrid).
prologSingleValued(mudSpd(tAgentGeneric,ftInt),prologHybrid).
prologSingleValued(mudStm(tAgentGeneric,ftInt),prologHybrid).
prologSingleValued(mudStr(tAgentGeneric,ftInt),prologHybrid).
prologSingleValued(mudToHitArmorClass0(tAgentGeneric,ftInt),prologHybrid).
prologSingleValued(mudWeight(tObj,ftInt),prologHybrid).
% prologSingleValued(spawn_rate(isPropFn(genls(tObj)),ftInt)).
prologSingleValued(spawn_rate(tCol,ftInt)).
prologSingleValued(stat_total(tAgentGeneric,ftInt)).
prologSingleValued(typeGrid(tCol,ftInt,ftListFn(ftString))).
resultIsa(apathFn,tPathway).
% '<=>'(isa(Whom,tNpcPlayer),whenAnd(isa(Whom,tPlayer),naf(isa(Whom,tHumanPlayer)))).
'<=>'(mudDescription(apathFn(Region,Dir),Text),pathName(Region,Dir,Text)).
'<=>'(nameStrings(apathFn(Region,Dir),Text),pathName(Region,Dir,Text)).
tInferInstanceFromArgType(vtSize).
tInferInstanceFromArgType(vtTexture).

vtBasicDir(vEast).
vtBasicDir(vNorth).
vtBasicDir(vSouth).
vtBasicDir(vWest).
vtBasicDirPlusUpDown(vDown).
vtBasicDirPlusUpDown(vUp).
%localityOfObject(Above,HasSurface):- mudLocOnSurface(Above,HasSurface).
%localityOfObject(Clothes,Agent):- mudSubPart(Agent,Clothes).
%localityOfObject(Inner,Container):- mudInsideOf(Inner,Container).
%localityOfObject(Inner,Outer):- user:only_if_pttp, localityOfObject(Inner,Container),localityOfObject(Container,Outer).
nameStrings(apathFn(Region,Dir),Text):- pathName(Region,Dir,Text).
meta_argtypes(mudMaterial(tSpatialThing,vtMaterial)).
meta_argtypes(mudSize(tSpatialThing,vtSize)).
meta_argtypes(mudTexture(tSpatialThing,vtTexture)).
meta_argtypes(mudWearing(tAgentGeneric,tWearAble)).
meta_argtypes(pathName(tRegion,vtDirection,ftString)).
meta_argtypes(resultIsa(tFunction,tCol)).
meta_argtypes(wasSuccess(tAgentGeneric,ftBoolean)).
meta_argtypes(type_action_info(tCol,vtActionTemplate,ftText)).
%NEXT TODO predTypeMax(mudEnergy,tObj,130).
%NEXT TODO predTypeMax(mudHealth,tObj,500).

tCol(ttAgentType).

prologHybrid(pathBetween(tRegion,vtDirection,tRegion)).
prologHybrid(pathConnects(tRegion,tRegion),tSymmetricRelation).

ttAgentType(tMonster).
% user:instTypeProps(apathFn(Region,_Dir),tPathway,[localityOfObject(Region)]).


=> tCol(vtActionTemplate).
=> tCol(tRegion).
=> tCol(tContainer).
disjointWith(tObj,tRegion).
disjointWith(tObj,tRegion).
disjointWith(tRegion,tObj).


ttTemporalType(tAgentGeneric).
ttTemporalType(tItem).
ttTemporalType(tObj).
ttTemporalType(tRegion).
tCol(tChannel).
tChannel(A):- tAgentGeneric(A).
tChannel(A):- tRegion(A).
tChannel(iGossupChannel).
ttTypeFacet(tChannel).
typeGenls(tAgentGeneric,ttAgentType).
typeGenls(tItem,ttItemType).
typeGenls(tObj,ttObjectType).
:-add(isa(tObj,ttTemporalType)).
:-add(isa(tRegion,ttTemporalType)).
typeGenls(tRegion,ttRegionType).
typeGenls(ttAgentType,tAgentGeneric).
typeGenls(ttItemType,tItem).
typeGenls(ttObjectType,tObj).
typeGenls(ttRegionType,tRegion).
cycAssert(A,B):- trace_or_throw(cycAssert(A,B)).




prologHybrid(dividesBetween(tCol,tCol,tCol)).

% defined more correctly below dividesBetween(S,C1,C2) => (disjointWith(C1,C2) , genls(C1,S) ,genls(C2,S)).
dividesBetween(tItem,tMassfull,tMassless).
dividesBetween(tObj,tItem,tAgentGeneric).
dividesBetween(tObj,tMassfull,tMassless).
dividesBetween(tTemporalThing,tObj,tRegion).
formatted_resultIsa(ftDice(ftInt,ftInt,ftInt),ftInt).
(dividesBetween(tAgentGeneric,tMale,tFemale)).

% dividesBetween(tItem,tPathways).
dividesBetween(tItem,tMassfull,tMassless).
dividesBetween(tObj,tItem,tAgentGeneric).
dividesBetween(tObj,tMassfull,tMassless).
dividesBetween(tTemporalThing,tObj,tRegion).
dividesBetween(tAgentGeneric,tPlayer,tNpcPlayer).


isa(tRegion,ttTemporalType).

completelyAssertedCollection(tCol).
completelyAssertedCollection(ttFormatType).
completelyAssertedCollection(tItem).
completelyAssertedCollection(tRegion).
completelyAssertedCollection(tObj).
completelyAssertedCollection(tAgentGeneric).
completelyAssertedCollection(tCarryAble).
completelyAssertedCollection(vtVerb).
genls(ttTypeByAction,completelyAssertedCollection).

arity(pathConnects,2).

pathConnects(R1,R2):-pathBetween(R1,Dir,R2),nop(Dir).
pathConnects(R1,R2):-pathBetween(R2,Dir,R1),nop(Dir).



% ==================================================
% Classes of things
% ==================================================
:- do_gc.

genls(tAgentGeneric,tObj).
genls(tItem,tObj).
genls(tClothing, tWashAble).
genls(tClothing, tWearAble).
genls(tFood,tEatAble).
genls(tFood, tItem).
genls(tClothing, tItem).

genls( tCarryAble, tItem).
genls( tCarryAble, tDropAble).

% throw(sane_transitivity (genls( tCarryAble, tThrowAble))).
% genls( tCarryAble, tCarryAble).

genls(tPortableDevice,tCarryAble).

predIsFlag(spatialInRegion/1).

:-do_gc.

genls(tClothing, tFoldAble).
genls(tClothing, tWearAble).

genls(tLiquidContainer, tDrinkAble).
genls(tLiquidContainer, tCarryAble).


genls(tFoldAble, tCarryAble).
% genls(tThrowAble, tCarryAble).
genls(tPortableDevice,tCarryAble).
genls(tPortableDevice,tPhysicalDevice).
genls(tPhysicalDevice,tUseAble).
genls(tWearAble, tCarryAble).
genls(tFood,tCarryAble).
genls(tCarryAble,tObj).
genls(tPartofObj,tNotTakAble).
genls(tBodyPart,tPartofObj).
genls(tSpatialThing,tLookAble).
genls(tFurnature,tOntoAble).
genls(tFurnature,tItem).

genls(tPartofFurnature,tPartofObj).

:- do_gc.




%(isa(I,Sub), disjointWith(Sub, Super)) => neg(isa(I,Super)).

neg(isa(I,Super)) <= (isa(I,Sub), disjointWith(Sub, Super)).

genls(tPartofObj,tItem).

% dividesBetween(tItem,tPathways).
dividesBetween(tItem,tMassfull,tMassless).
dividesBetween(tObj,tItem,tAgentGeneric).
dividesBetween(tObj,tMassfull,tMassless).
dividesBetween(tSpatialThing,tObj,tRegion).
dividesBetween(tAgentGeneric,tPlayer,tNpcPlayer).

% dividesBetween(S,C1,C2) => (disjointWith(C1,C2) , genls(C1,S) ,genls(C2,S)).

% disjointWith(P1,P2) => (not(isa(C,P1)) <=> isa(C,P2)).

% isa(Col1, ttObjectType) => ~isa(Col1, ttFormatType).

=> tCol(tCol).
=> tCol(tPred).
=> tCol(tFunction).
=> tCol(tRelation).
=> tCol(ttSpatialType).
=> tCol(ttFormatType).
% => tCol(functorDeclares).
% tCol(ArgsIsa):-ttPredType(ArgsIsa).
% TODO decide if OK
%tCol(F):-t(functorDeclares,F).
=> tCol(ttFormatType).
=> tCol(vtActionTemplate).
=> tCol(tRegion).
=> tCol(tContainer).

isa(tRegion,ttSpatialType).
isa(tRelation,ttAbstractType).
typeProps(tTorso,[mudColor(isLikeFn(mudColor,tSkin)),mudShape(vUnique)]).
typeProps(tSkin,[mudColor(vUnique),mudShape(vUnique)]).

%Empty Location
% You *have* to use 0 as the id of the empty location. (no way!)
mudLabelTypeProps(--,ftVar,[]).

%NEXT TODO predTypeMax(mudEnergy,tAgentGeneric,120).

typeProps(tAgentGeneric,[predInstMax(mudHealth,500)]).
genls('IndoorsIsolatedFromOutside',tRegion).
genls('SpaceInAHOC',tRegion).

typeProps(tAgentGeneric,[mudMoveDist(1)]).
typeProps(tAgentGeneric,[predInstMax(mudHealth,500), predInstMax(mudEnergy,200), mudHealth(500), mudEnergy(200),  mudFacing(isRandom(vtBasicDir)), mudAgentTurnnum(0), mudScore(1), 
    mudMemory(aDirectionsFn([vNorth,vSouth,vEast,vWest,vNE,vNW,vSE,vSW,vUp,vDown]))]).
% typeProps(tAgentGeneric,mudLastCommand(actStand)).
typeProps(tAgentGeneric,mudNeedsLook(vFalse)).
typeProps(tFood,[mudHeight(0)]).
typeProps(tItem,mudEnergy(140)).
typeProps(tItem,mudListPrice(0)).
typeProps(tObj,[mudOpaqueness(100)]).
typeProps(tRegion,[mudOpaqueness(1)]).
typeProps(tSpatialThing,mudHeight(0)).

% :-end_module_type(dynamic).

% instTypeProps(isSelf,Type,[mudNamed(isSelf),typeHasGlyph(Lbl)|SP]):-  mudLabelTypeProps(Lbl,Type,SomeProps),nonvar(Type),flatten(SomeProps,SP).

:- do_gc.
% Vacuum World example objects........
mudLabelTypeProps(wl,tWall,[mudHeight(3),mudWeight(4)]).

%TOO SLOW isa(I,SC)<=isa(I,C),genls(C,SC).

(wearsClothing(A,I)=>{add(tAgentGeneric(A)),add(tClothing(I))}).


genls(tBread, tFood).
typeProps(tCrackers,[mudColor(vTan),isa(tBread),mudShape(isEach(vCircular,vFlat)),mudSize(vSmall),mudTexture(isEach(vDry,vCoarse))]).


user:action_info(C,_)=>vtActionTemplate(C).

cachedPredicate(Goal)=>{forall(Goal,pfc_add(Goal))}.

tCol(cachedPredicate).
cachedPredicate(vtActionTemplate(_)).


tCol(random_path_dir).
random_path_dir(Dir):-nonvar(Dir),!,fail.
random_path_dir(Dir):-random_instance(vtBasicDir,Dir,true).
random_path_dir(Dir):-random_instance(vtBasicDirPlusUpDown,Dir,true).
random_path_dir(Dir):-random_instance(vtDirection,Dir,true).

prologOnly(ensure_some_pathBetween/2).

prologOnly(onEachLoad).
argsQuoted(onEachLoad).
argsQuoted(must).

/*

 the CycL language extends Prolog's first order logic capabilities with some higher order logics.  
 It also extrends prolog to show proofs.. one issue is the CycL language never signed up for cuts or other execution  orders.    
 PrologMUD extends the CycL language to allow preset program flow (unless a predicate is declared to not honor order of execution 
  (this is usually best!)).  PrologMUD  implements a new design of the cyc canonicalizer..   

 usually in Cyc the rules "(implies (and Axy Byz) (and Cwxyz Dwx))" are converted to DNF (Correct me if I am wrong.. 
 since i have heard it uses ConjectiveNormalForm as well) ... the DNF generates Clausal forms..  The forms choosen 



?-  kif_to_boxlog(((parent('$VAR'('G'),'$VAR'('P')) & parent('$VAR'('P'),'$VAR'('C'))) => grandparent('$VAR'('G'),'$VAR'('C'))),O). 

O = [ (-parent(G, P):- -grandparent(G, C), parent(P, C)), 
      (-parent(P, C):- -grandparent(G, C), parent(G, P)), 
      (grandparent(G, C):-parent(G, P), parent(P, C))].


?- kif_to_boxlog( (grandparent('$VAR'('G'),'$VAR'('C')) => exists('$VAR'('P'), (parent('$VAR'('G'),'$VAR'('P')) & parent('$VAR'('P'),'$VAR'('C'))))),O).

    (-grandparent(G, C):- mudEquals(P, skUnkArg2OfParentArg1OfFn(KB, C, G)), (-parent(G, P) ; -parent(P, C))),   % You have proven G is not the grandparent of C when you have proven tha G has no children or that C has no parents
    (-mudEquals(P, skUnkArg2OfParentArg1OfFn(KB, C, G)):- grandparent(G, C), (-parent(G, P) ; -parent(P, C))), 
    (parent(G, P):-grandparent(G, C), mudEquals(P, skUnkArg2OfParentArg1OfFn(KB, C, G))), % if you prove G is grandparent of P somehow, you will have proved that G is parent to  parentOf P
    (parent(P, C):-grandparent(G, C), mudEquals(P, skUnkArg2OfParentArg1OfFn(KB, C, G))),
    (mudEquals(P, skUnkArg2OfParentArg1OfFn(KB, C, G)):- grandparent(G, C),  \+ (parent(G, P) , parent(P, C)))]  % We failed to find a true P


O = [ 
      (-grandparent(G, P):- -parent(G, _P) ; -parent(_P, P)),    
      parent(G, P):- grandparent(G, C), parent(P,C),   % if you prove G is grandparent of P somehow, you will have proved that G is parent to  parentOf P
      parent(P, C):- grandparent(G, C), parent(G,P))].   % if you prove G is grandparent of P somehow, you will have proved that G is parent to  parentOf P

*/

/*

(((meta_argtypes(Types)/
 (functor(Types,F,A), A >1, functor(Matcher,F,A),arity(F,A)))
  => 
    ((Matcher => {between(1,A,N),arg(N,Matcher,I),arg(N,Types,T),ground(I:T)},\+ttFormatType(T),isa(I,T),{dmsg(isa(I,T))})))).

((argQuotedIsa(Pred, _, 'CycLSentence') => 'SententialOperator'(Pred))).

*/
