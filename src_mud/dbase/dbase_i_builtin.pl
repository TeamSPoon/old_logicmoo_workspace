/** <module> 
% ===================================================================
% File 'dbase_i_builtin.pl'
% Purpose: Emulation of OpenCyc for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface' 1.0.0
% Revision:  $Revision: 1.9 $
% Revised At:   $Date: 2002/06/27 14:13:20 $
% ===================================================================
% File used as storage place for all predicates which change as
% the world is run.
%
% props(Obj,height(ObjHt))  == holds(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt))  == padd(height,Obj,ObjHt,...) ==  moo(QueryForm)
% kretract[all](Obj,height(ObjHt))  == kretract[all](Obj,height,ObjHt) == pretract[all](height,Obj,ObjHt) == del[all](QueryForm)
% keraseall(AnyTerm).
%
%
% Dec 13, 2035
% Douglas Miles
*/

:- begin_transform_moo_preds.

% =================================================================================================
% BEGIN world English
% =================================================================================================

:- decl_mpred_prolog(naf/1).
:- decl_mpred_prolog(call_mpred/1).
:- decl_mpred_hybrid(macroDeclarer/1).

:- decl_mpred_hybrid(predTypeMax/3).
:- decl_mpred_hybrid(argIsa/3).

tCol(tSpatialThing).

tCol(tChannel).
tCol(ttCompleteExtentAsserted).

predArgTypes(mudFtInfo(ttFormatType,ftTerm)).
ttCompleteExtentAsserted(mudFtInfo).
ttCompleteExtentAsserted(ttCompleteExtentAsserted).

:-decl_mpred_hybrid((genlInverse/2,genlPreds/2)).

prologMultiValued(genlInverse(tPred,tPred)).
prologMultiValued(genlPreds(tPred,tPred)).

mpred_prop(mudSubclass, ttCompleteExtentAsserted).
mpred_prop(mudFtInfo, ttCompleteExtentAsserted).
mpred_prop(mudSubclass/2, prologHybrid).
mpred_prop(mudFtInfo/2,prologHybrid).
mudSubclass(isEach(tRegion,tAgentGeneric),tChannel).

:-decl_mpred_prolog(forwardRule/2).

:-decl_mpred_hybrid(mudTextSame(ftText,ftText)).

:- decl_mpred_prolog(stat_total/2).
% :- decl_mpred_prolog(createableSubclassType/2).
:- decl_mpred_prolog(mudMoveDist/2).
:- decl_mpred_prolog(was_imported_kb_content/2).
:- must(decl_mpred_prolog(agent_call_command/2)).

:-ensure_universal_stub(mudAtLoc,2).


:- decl_mpred_hybrid((
     tCol/1, tAgentGeneric/1, tItem/1, tRegion/1,
     instVerbOverride/3,mudNamed/2, determinerString/2, mudKeyword/2 ,descriptionHere/2, 
     mudToHitArmorClass0/2,

      tThinking/1,
      tDeleted/1,
 mudWeight/2,
 mudPermanence/3,
      act_term/2,
      mudAgentTurnnum/2,
      
      mudAtLoc/2,
      mudEnergy/2,
      mudHealth/2,
      mudDescription/2,
      mudFacing/2,
      mudCmdFailure/2,
      mudSpd/2,
      typeGrid/3,
      mudHeight/2,
      mudMemory/2,
      
      mudIsa/2,
      pathName/3, 
      mudPossess/2,
      mudScore/2,
      mudStm/2,      
      mudStr/2,
      wearsClothing/2)).

:- decl_mpred_hybrid((
      mudArmorLevel/2,
      mudLevelOf/2,
      mudToHitArmorClass0/2,
      mudBareHandDamage/2,
      chargeCapacity/2,
      mudEnergy/2,
     tCol/1, tAgentGeneric/1, tItem/1, tRegion/1,
     instVerbOverride/3,mudNamed/2, determinerString/2, mudKeyword/2 ,descriptionHere/2, 

      tThinking/1,
 mudWeight/2,
 mudPermanence/3,
      act_term/2,
      mudAgentTurnnum/2,
      
      mudAtLoc/2,
      mudEnergy/2,
      mudHealth/2,
      mudDescription/2,
      mudFacing/2,
      failure/2,
      gridValue/4,
      mudHeight/2,
      mudMemory/2,
      mudIsa/2,
      pathName/3, 
      mudPossess/2,
      mudScore/2,
      mudStm/2,      
      mudStr/2,
      mudWearing/2)).

/*
:-multifile localityOfObject/2.

:- context_module(M),
   asserta(dbase_mod(M)),
   dmsg(assert_if_new(dbase_mod(M))).

*/

:- decl_mpred_hybrid((ttNotCreatable/1, tCol/1, mudSubclass/2, predArgTypes/1 ,ttCreateable/1)).


:- decl_mpred_hybrid(((vFormatted/1,
                       mudContains/2))).


:- decl_mpred_hybrid mudLastCommand/2.
:- decl_mpred_hybrid mudNamed/2, mudSpd/2.
:- decl_mpred_hybrid mudStr/2. 
:- decl_mpred_hybrid mudSubclass/2.
:- decl_mpred_hybrid typeGrid/3.
:- decl_mpred_hybrid(mudSubclass/2).
:- decl_mpred_hybrid(prologSingleValued/1).
:- discontiguous(prologSingleValued/1).
:- decl_mpred_hybrid(argSingleValueDefault, 3).

:- decl_mpred_hybrid(predModule, 2).
:- decl_mpred_hybrid(predProxyAssert, 2).
:- decl_mpred_hybrid(predProxyRetract, 2).
:- decl_mpred_hybrid(predProxyQuery, 2).
:- decl_mpred_hybrid(argSingleValueDefault, 3).

:- decl_mpred(mudMoveDist/2,[predArgTypes(mudMoveDist(tAgentGeneric,ftInt)),prologSingleValued,predModule(user),query(call),argSingleValueDefault(2,1)]).

% mudMoveDist(X,Y):-callStub_moo(holds_t,mudMoveDist(X,Y)).


:- decl_mpred(objid,2).

% flags
:-decl_mpred(tAgentGeneric(ftID),[predIsFlag]).
:-decl_mpred(tItem(ftID),[predIsFlag]).
:-decl_mpred(tRegion(ftID),[predIsFlag]).
:-decl_mpred(tCol(ftID),[predIsFlag]).
:-decl_mpred(tThinking(tAgentGeneric),[predIsFlag]).
:-decl_mpred(tDeleted(ftID),[predIsFlag]).

:- decl_mpred(mudNeedsLook/2,[ttCompleteExtentAsserted]).
:- decl_mpred(mudMaxHitPoints(tAgentGeneric,ftInt)).


:-decl_mpred(tRegion(ftID),tCol).

tChannel(iGossupChannel).

% ttCreateable(tSpatialThing).


% prologMultiValued(gridValue(tRegion,ftInt,ftInt,tObj)).


prologMultiValued(mudSubclass(tCol,tCol)).
prologMultiValued(mudIsa(ftTerm,tCol)).


prologMultiValued(mudPossess(tObj,tObj)).

:- decl_mpred_hybrid(predProxyAssert,2).
:- begin_transform_moo_preds.

mudSubclass(ttCreateable,tCol).

% :- (do_term_expansions->true;throw(not_term_expansions)).

:-dynamic(tMonster/1).

tCol(tItem).
tCol(prologMultiValued).
tCol(prologSingleValued).
tCol(ttCreateable).
tCol(macroDeclarer).

:-add((mudSubclass(isEach(prologMultiValued,prologOrdered,prologNegByFailure,predArgTypes,prologHybrid,prologPTTP,prologOnly,prologMacroHead,prologListValued,prologSingleValued),tPred))).

:-add((argIsa(isEach(tPred,prologMultiValued,prologOrdered,prologNegByFailure,predArgTypes,prologHybrid,prologPTTP,prologOnly,prologMacroHead,prologListValued,prologSingleValued),1,tPred))).
:-add((argIsa(isEach(tPred,prologMultiValued,prologOrdered,prologNegByFailure,predArgTypes,prologHybrid,prologPTTP,prologOnly,prologMacroHead,prologListValued,prologSingleValued),2,ftListFn(ftVoprop)))).

:-add((mudIsa(isEach(prologMultiValued,prologOrdered,prologNegByFailure,predArgTypes,prologPTTP,prologHybrid,prologOnly,prologOnly,prologMacroHead,prologListValued,prologSingleValued),macroDeclarer))).

:-doall((is_pred_declarer(F),decl_type(F),add(mudIsa(F,macroDeclarer)),add(mudSubclass(F,tRelation)))).

:-decl_mpred_prolog(repl_writer(tAgentGeneric,ftTerm)).
%:-decl_mpred_hybrid(repl_writer(tAgentGeneric,term),[prologSingleValued,argSingleValueDefault(2,default_repl_writer)]).
:-decl_mpred_prolog(repl_to_string(tAgentGeneric,ftTerm)).
%:-decl_mpred_hybrid(repl_to_string(tAgentGeneric,term),[prologSingleValued,argSingleValueDefault(2,default_repl_obj_to_string)]).

%mpred(ArgTypes,PropTypes):-decl_mpred_prop(ArgTypes,PropTypes).
% pddlSomethingIsa('NpcCol1012-Ensign732',['NpcCol1012',actor,'MaleAnimal']).

:-dynamic_multifile_exported((mudNeedsLook/2)).



% :- style_check(-discontiguous).
:- debug.


:-decl_mpred_prolog(verb_alias(ftString,vtVerb)).

prologHybrid(mudLabelTypeProps(ftString,tCol,ftVoprop)).

prologHybrid(typeGrid(tCol,ftInt,ftListFn(ftString))).

prologMultiValued(action_rules(tAgentGeneric,vtVerb,ftTerm(tObj),ftVoprop)).


tCol(vtActionTemplate).

prologSingleValued(mudAtLoc(tObj,xyzFn(tRegion,ftInt,ftInt,ftInt))).

% =================================================================================================
% BEGIN world database
% =================================================================================================

:- begin_transform_moo_preds.


predArgTypes(resultIsa(tFunction,tCol)).
predArgTypes(formatted_resultIsa(ttFormatType,tCol)).

formatted_resultIsa(apathFn(tRegion,vtDirection),tPathway).
formatted_resultIsa(ftDice(ftInt,ftInt,ftInt),ftInt).
resultIsa(apathFn,tPathway).
resultIsa(txtFormatFn,ftString).



mudSubclass(tPathway,tDoor).



mudSubclass(tDoor,tFurniture).
mudSubclass(tFurniture,tPartofObj).

% mudSubclass(dir,value).

% database backing impls
% builtin = ftCallable native
% dynamic = ftCallable dynamic assert/call
% dbase_t = a dbase_t/N
% pttp = pttp _int compiled
% cyc = 
% col =

% multivalued
%mpred(G,[predArgMulti(AT)|LIST]):-prologMultiValued(G,AT,LIST).
/*
prologMacroHead(pddlSomethingIsa(ftTerm,ftListFn(tCol))).
prologMacroHead(macroSomethingDescription(ftTerm,ftListFn(ftString))).
prologMacroHead(pddlObjects(tCol,ftListFn(ftID))).
prologMacroHead(pddlSorts(tCol,ftListFn(tCol))).
prologMacroHead(pddlPredicates(ftListFn(ftVoprop))).
prologMacroHead(pddlTypes(ftListFn(tCol))).
*/

predArgTypes(pddlSomethingIsa(ftTerm,ftListFn(tCol))).
predArgTypes(macroSomethingDescription(ftTerm,ftListFn(ftString))).
predArgTypes(pddlObjects(tCol,ftListFn(ftID))).
predArgTypes(pddlSorts(tCol,ftListFn(tCol))).
predArgTypes(pddlPredicates(ftListFn(ftVoprop))).
predArgTypes(pddlTypes(ftListFn(tCol))).

%mpred(ArgTypes,[prologSingleValued]):-prologSingleValued(ArgTypes).
%mpred(CallSig,[external(M)]):-prologOnly(M:CallSig).


% prologMultiValued
%prologMultiValued(G,AT,[prologOrdered|LIST]):-prologMultiValued(G,LIST),functor_catch(G,_,AT).

prologMultiValued(pathBetween(tRegion,vtDirection,tRegion)).
prologMultiValued(mudNamed(ftTerm,ftTerm),[]).

:-must_det(argIsa_call(genlPreds,2,_Type)).
% genlPreds(tPred,tPred).

 
prologHybrid(mudKnowing(tAgentGeneric,ftTerm)).
prologMultiValued(cmdsuccess(tAgentGeneric,ftAction)).
prologMultiValued(mudCmdFailure(tAgentGeneric,ftAction)).
prologMultiValued(nameStrings(ftTerm,ftString)).
prologMultiValued(determinerString(ftTerm,ftString)).
prologMultiValued(descriptionHere(ftTerm,ftString),prologOrdered).
prologMultiValued(mudDescription(ftTerm,ftString),prologOrdered).
prologMultiValued(mudKeyword(ftTerm,ftString)).
prologMultiValued(mudActAffect(ftTerm,ftTerm,ftTerm)).
prologMultiValued(mudMemory(tAgentGeneric,ftTerm)).
predArgTypes(mudWearing(tAgentGeneric,tWearAble)).
predArgTypes(mudLastCmdSuccess(tAgentGeneric,ftBoolean)).

:-decl_mpred(mudActAffect/3).

% :-decl_mpred_prolog(member/2).


nameStrings(apathFn(Region,Dir),Text):- pathName(Region,Dir,Text).
mudDescription(apathFn(Region,Dir),Text):- pathName(Region,Dir,Text).

mudSubclass(tAgentGeneric,tObj).
mudSubclass(tItem,tObj).
mudSubclass(tFurniture,tObj).

% single valued
predArgTypes(pathName(tRegion,vtDirection,ftString)).
predArgTypes(instVerbOverride(ftTerm,ftAction,ftAction),[prologHybrid]).

:-dynamic(spawn_rate/2).

tChannel(A):-tRegion(A).
tChannel(A):-tAgentGeneric(A).

mudSubclass(tAgentGeneric,tSpatialThing).
mudSubclass(tRegion,tSpatialThing).
mudSubclass(tObj,tSpatialThing).
mudSubclass(tItem,tSpatialThing).

mudSubclass(tDrinkable,tItem).
mudSubclass(tPossessable,tItem).
mudSubclass(tUseable,tItem).
mudSubclass(tEatAble,tItem).
mudSubclass(tChargeable,tItem).
mudSubclass(tWearAble,tItem).


:-decl_mpred_hybrid(pathBetween,3).
:-decl_mpred_hybrid(mudAtLoc,2).
:-decl_mpred_hybrid(mudHealth,2).


% we need a way to call this: typeMaxCapacity
prologSingleValued(predTypeMax(prologSingleValued,tCol,ftInt)).
% we need a way to call this: maxCapacity
prologSingleValued(predInstMax(ftID,prologSingleValued,ftInt)).

:- assert_if_new(mpred_prop(localityOfObject,call_tabled)).

predArgTypes(forwardRule(ftTerm,ftTerm)).
% forwardRule(localityOfObject(O,Region),atloc(O,LOC)):-
prologMultiValued(forwardRule(ftTerm,ftTerm)).
prologMultiValued(ruleEquiv(ftTerm,ftTerm),prologOnly).
prologMultiValued(ruleHybridChain(ftTerm,ftTerm),prologOnly).

% ftCallable code
% ruleEquiv(prologMultiValued(CallSig,[predProxyAssert(add),predProxyRetract(del),predProxyQuery(req)]),prologHybrid(CallSig)).
ruleEquiv(prologMultiValued(CallSig,[predProxyAssert(hooked_asserta),predProxyRetract(hooked_retract),predProxyQuery(call)]),prologOnly(CallSig)).
ruleEquiv(prologMultiValued(CallSig,[predProxyAssert(pttp_tell),predProxyRetract(pttp_retract),predProxyQuery(pttp_ask)]),prologPTTP(CallSig)).


%  predModule(isa(obj,col),user).
% db_prop_prolog(world,isa(obj,col)).
% db_prop_prolog(world,same(ftID,ftID)).

mudSubclass(ftTextType,ftText).

predArgTypes(argSingleValueDefault(prologSingleValued,ftInt,ftTerm)).

prologOnly(member(ftTerm,ftTerm)).

% live another day to fight (meaning repl_to_string/1 for now is in ftCallable)
% prologSingleValued(repl_writer(tAgentGeneric,term),argSingleValueDefault(2,default_repl_writer)).
% prologSingleValued(repl_to_string(tAgentGeneric,term),[prologSingleValued,argSingleValueDefault(2,default_repl_obj_to_string)]).

%prologMultiValued(label_type(ftString,col),[prologSingleValued]).



prologListValued(mudNearFeet(tAgentGeneric,ftListFn(tSpatialThing)),[]).
prologListValued(mudNearReach(tAgentGeneric,ftListFn(tSpatialThing)),[predModule(user)]).
prologListValued(mudGetPrecepts(tAgentGeneric,ftListFn(tSpatialThing)),[predModule(user)]).

mudSubclass(tFunction,tRelation).
mudSubclass(tPred,tRelation).

prologOnly(mud_test(ftTerm,ftCallable)).

prologMultiValued(predProxyAssert(prologMultiValued,ftTerm)).
prologNegByFailure(predArgMulti(prologMultiValued,ftInt)).
prologMultiValued(predProxyQuery(prologMultiValued,ftTerm)).

mudIsa(ftText,ttFormatType).

predArgTypes(type_action_info(tCol,vtActionTemplate,ftText)).
predArgTypes(argIsa(tRelation,ftInt,tCol)).
predArgTypes(argFormat(tRelation,ftInt,ttFormatType)).

prologOnly(action_template(ftTerm)).
prologOnly(action_info(vtActionTemplate,ftText)).
prologOnly(agent_text_command(tAgentGeneric,ftText,tAgentGeneric,ftAction)).

prologMultiValued(mudDescription(ftTerm,ftText),
  [predProxyAssert(add_description),predProxyRetract(remove_description),predProxyQuery(query_description)]).


%mudSubclass(AT,ttAgentGeneric):-mudIsa(AT,ttAgentType).
%mudIsa(AT,ttAgentType):-mudSubclass(AT,ttAgentGeneric).

mudSubclass(tMonster,ttAgentGeneric).

mudIsa(Inst,tHasAction):-mudIsa(Inst,Type),mudIsa(Type,ttTypeByAction).
mudSubclass(ttObjectType,tCol).

tCol(tItem).
tCol(vtVerb).


ruleEquiv(nameStrings(apathFn(Region,Dir),Text),pathName(Region,Dir,Text)).
ruleEquiv(mudDescription(apathFn(Region,Dir),Text),pathName(Region,Dir,Text)).

% dbase_t(action_info,What,text("command is: ",What)):- holds_t(action_type,What).

:-decl_mpred_hybrid(mudStowing(tAgentGeneric,tItem)).


% formattype(FormatType):-mudSubclass(FormatType,formattype).

% single valued
mudSubclass(tAgentGeneric,tObj).
mudSubclass(tItem,tObj).


prologMultiValued(pathName(tRegion,vtDirection,ftString)).
prologMultiValued(instVerbOverride(ftTerm,ftAction,ftAction)).

prologSingleValued(mudAgentTurnnum(tAgentGeneric,ftInt(0))).
prologSingleValued(mudAgentTurnnum(tAgentGeneric,ftInt)).
prologSingleValued(mudArmorLevel(tWearAble,ftInt)).
prologSingleValued(mudAttack(tObj,ftInt)).
prologSingleValued(mudEnergy(tChargable,ftInt(500))).
prologSingleValued(mudEnergy(tObj,ftInt),[argSingleValueDefault(2,500)]).
prologSingleValued(chargeCapacity(tChargable,ftInt)).
prologSingleValued(mudEnergy(tChargable,ftInt)).
prologSingleValued(mudHealth(tObj,ftInt)).
prologSingleValued(mudArmor(tObj,ftInt)).
prologSingleValued(mudFacing(tObj,vtDirection(vNorth))).
prologSingleValued(mudFacing(tObj,vtDirection)).
prologSingleValued(mudHeight(tObj,ftInt)).
prologSingleValued(mudID(tObj,ftID)).
predArgTypes(localityOfObject(tObj,tSpatialThing)).
prologSingleValued(mudLastCommand(tAgentGeneric,tCommand)).
prologSingleValued(location_center(tRegion,xyzFn(tRegion,ftInt,ftInt,ftInt))).
prologSingleValued(mudMoveDist(tAgentGeneric,ftInt(1))).
prologSingleValued(mudMoveDist(tAgentGeneric,ftNumber)).
prologSingleValued(mudBareHandDamage(tAgentGeneric,ftDice)).
prologSingleValued(mudLevelOf(tPossessable,ftInt)).
prologSingleValued(mudMaxHitPoints(tAgentGeneric,ftInt)).
prologSingleValued(mudMaxHitPoints(tAgentGeneric,ftInt),[prologHybrid]).
prologSingleValued(mudToHitArmorClass0(tAgentGeneric,ftInt)).
prologSingleValued(mudPermanence(tItem,vtVerb,vtPerminance)).
prologSingleValued(mudScore(tObj,ftInt)).
prologSingleValued(spawn_rate(propFn(mudSubclass(tObj)),ftInt)).

prologSingleValued(mudSpd(tAgentGeneric,ftInt)).

prologSingleValued(stat_total(tAgentGeneric,ftInt)).
prologSingleValued(mudStm(tAgentGeneric,ftInt)).
prologSingleValued(mudStr(tAgentGeneric,ftInt)).
prologSingleValued(typeGrid(tCol,ftInt,ftListFn(ftString))).
prologSingleValued(mudWeight(tObj,ftInt)).

prologSingleValued(mudHeight(tSpatialThing,ftInt)).

prologMultiValued(comment(ftTerm,ftString)).
prologMultiValued(pathBetween(tRegion,vtDirection,tRegion)).

prologOnly(typeProps(tCol,ftVoprop)).
prologOnly(instTypeProps(ftID,tCol,ftVoprop)).


typeProps(tFood,[mudHeight(0)]).
typeProps(tAgentGeneric,[mudMoveDist(1)]).

typeProps(tSpatialThing,mudHeight(0)).


:-decl_type(vtBasicDir).

vtBasicDir(vNorth).
vtBasicDir(vEast).
vtBasicDir(vSouth).
vtBasicDir(vWest).

:-decl_type(vtBasicDirPlusUpDown).
mudSubclass(vtBasicDir,vtBasicDirPlusUpDown).
vtBasicDirPlusUpDown(vUp).
vtBasicDirPlusUpDown(vDown).

mudSubclass(vtBasicDirPlusUpDown,vtDirection).

% :- must((argIsa_call_0(comment,2,W), W\=term)).

:-decl_mpred_hybrid(mudNeedsLook,2).

prologNegByFailure(mudNeedsLook(tObj,ftBoolean)). 

prologSingleValued(mudNeedsLook(tAgentGeneric,ftBoolean),argSingleValueDefault(2,vFalse)). 


typeProps(tAgentGeneric,mudNeedsLook(vFalse)).

tCol(ftVar).
tCol(ftString).
ttFormatType(ftVar).
ttFormatType(ftString).
/*
:-decl_mpred_prolog(ftVar/1).
:-decl_mpred_prolog(ftString/1).
:-decl_mpred_prolog(ftNumber/1).
:-decl_mpred_prolog(ftInteger/1).
*/

:-decl_mpred_hybrid(mudColor/2).
:-decl_mpred_hybrid(mudMaterial/2).
:-decl_mpred_hybrid(mudTexture/2).
:-decl_mpred_hybrid(mudSize/2).
:-decl_mpred_hybrid(mudShape/2).

mudFtInfo(apathFn(tRegion,vtDirection),vFormatted).
mudFtInfo(ftDice(ftInt,ftInt,ftInt),vFormatted).
mudFtInfo(ftListFn(tCol),vFormatted).
mudFtInfo(xyzFn(tRegion,ftInt,ftInt,ftInt),vFormatted).

mudFtInfo(ftAction,prologCall(is_vtActionTemplate(isSelf))).
mudFtInfo(ftAtom,prologCall(atom(isSelf))).
mudFtInfo(ftBoolean,prologCall(member(isSelf,[vTrue,vFalse]))).
mudFtInfo(ftID,prologCall((atom(isSelf),compound(isSelf)))).
mudFtInfo(ftNumber,prologCall(number(isSelf))).
mudFtInfo(ftCallable,prologCall(predicate_property(isSelf,visible))).
mudFtInfo(ftProlog,prologCall(predicate_property(isSelf,visible))).
mudFtInfo(ftRest,prologCall(true)).
mudFtInfo(ftString,prologCall(string(isSelf))).
mudFtInfo(ftTerm,prologCall(nonvar(isSelf))).
mudFtInfo(ftVar,prologCall(var(isSelf))).

mudSubclass(tAgentGeneric,tSpatialThing).
mudSubclass(tPathway,tDoor).
mudSubclass(tChargeable,tItem).
mudSubclass(tDoor,tItem).
mudSubclass(tDrinkable,tItem).
mudSubclass(tEatAble,tItem).
mudSubclass(tItem,tSpatialThing).
mudSubclass(tObj,tSpatialThing).
mudSubclass(tPossessable,tItem).
mudSubclass(tRegion,tSpatialThing).
mudSubclass(tUseable,tItem).
mudSubclass(tWearAble,tItem).

disjointWith(tObj,tRegion).
mudIsa(vtDirection,ttValueType).

mudSubclass(vtDirection,tTypevalue).


ttFormatType(ftVoprop).
%  subft(ftNumber,term).
% subft(ftNumber,term).
mudSubclass(ftPercent,ftNumber).
mudSubclass(ftNumber,ftPercent).
mudSubclass(ftAtom,ftTerm).
mudSubclass(ftDice,ftInt).
mudSubclass(ftInt,ftNumber).  

mudSubclass(ftID,ftTerm).
mudSubclass(ftVoprop,ftTerm).
mudSubclass(ftInteger,ftNumber).
mudSubclass(ftString,ftTerm).
mudSubclass(ftString,ftText).
mudSubclass(ftText,ftTerm).
mudSubclass(ftTerm,ftProlog).
mudSubclass(ftCallable,ftProlog).
mudSubclass(ftVar,ftProlog).

% flags
prologNegByFailure(tAgentGeneric(ftID)).
prologNegByFailure(tItem(ftID)).
prologNegByFailure(tRegion(ftID)).
prologNegByFailure(tCol(ftID)).

prologNegByFailure(tThinking(tAgentGeneric)).
prologNegByFailure(tDeleted(ftID)).


prologMultiValued(mudDescription(ftTerm,ftText),[predProxyAssert(add_description)]).

prologMultiValued(verbAsWell(ftTerm,ftAction,ftAction)).
prologMultiValued(nameStrings(ftTerm,ftString)).
prologMultiValued(determinerString(ftTerm,ftString)).
prologMultiValued(descriptionHere(ftTerm,ftString)).

predArgTypes(mudSize(tSpatialThing,vtSize)).
predArgTypes(mudTexture(tSpatialThing,vtTexture)).

mudSubclass(discoverableType,tCol).

discoverableType(vtTexture).
discoverableType(vtSize).

:-decl_mpred_hybrid(mudKwLabel,2).
:-decl_mpred_hybrid(argIsa/3).

prologMultiValued(mudDescription(ftTerm,ftString)).
prologMultiValued(mudKeyword(ftTerm,ftString)).
prologMultiValued(mudKwLabel(ftTerm,ftString)).
prologMultiValued(mudActAffect(tItem,vtVerb,ftTerm(ftVoprop))).
prologMultiValued(mudMemory(tAgentGeneric,ftTerm)).
prologMultiValued(typeGrid(tCol,ftInt,ftListFn(ftString))).
prologMultiValued(gridValue(tRegion,ftInt,ftInt,tObj)).
prologMultiValued(mudSubclass(tCol,tCol)).
prologMultiValued(mudIsa(ftTerm,tCol)).

mudSubclass(tPlayer,tAgentGeneric).
mudSubclass(tHumanPlayer,tPlayer).
mudSubclass(tNpcPlayer,tPlayer).
mudSubclass('MaleAnimal',tPlayer).
mudSubclass('FemaleAnimal',tPlayer).
mudSubclass(isEach('PortableObject','ProtectiveAttire','SomethingToWear'),tPossessable).
mudSubclass(isEach('ProtectiveAttire','SomethingToWear'),tWearAble).
mudSubclass(tContolDevice,tChargable).
mudSubclass(vtPosture,vtVerb).


ruleEquiv(mudIsa(Whom,tNpcPlayer),and(mudIsa(Whom,tPlayer),naf(mudIsa(Whom,tHumanPlayer)))).

prologListValued(aDirectionsFn(ftTerm,ftListFn(ftTerm))).

prologMultiValued(predModule(tRelation,ftAtom)).

prologOnly(agent_call_command(tAgentGeneric,ftAction)).

predTypeMax(mudHealth,tObj,500).
predTypeMax(mudEnergy,tObj,130).

typeProps(tItem,mudEnergy(140)).


% =================================================================================================
% END world database
% =================================================================================================

:- end_transform_moo_preds.
