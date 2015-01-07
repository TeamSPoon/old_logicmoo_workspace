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
tCol(tTemporallyExistingThing).
tCol(activeAgent).

tCol(tChannel).
tCol(completeExtentAsserted).

predArgTypes(mudFtInfo(ttFormatType,ftTerm)).
predArgTypes(mudSubft(ttFormatType,ttFormatType)).

:-decl_mpred_hybrid((genlInverse/2,genlPreds/2)).

prologMultiValued(genlInverse(tPred,tPred)).
prologMultiValued(genlPreds(tPred,tPred)).

mpred_prop(mudSubft, completeExtentAsserted).
mpred_prop(mudFtInfo, completeExtentAsserted).
mpred_prop(mudSubft/2, prologHybrid).
mpred_prop(mudFtInfo/2,prologHybrid).
% mpred_prop(subft/2, prologOnly).
% mpred_prop(ft_info/2,prologOnly).
mudSubclass(isEach(tRegion,tAgentGeneric,actGossup),tChannel).

:-decl_mpred_hybrid(mudMudDictionary(ftText,ftText)).

:-decl_mpred(tRegion(ftID),tType).

tChannel(actGossup).

ttCreateable(tTemporallyExistingThing).

:-dynamic type_max_health/2. 

prologMultiValued(mudGrid(tRegion,ftInt,ftInt,tObj)).


prologMultiValued(mudSubclass(tCol,tCol)).
prologMultiValued(mudIsa(ftTerm,tCol)).
predArgTypes(somethingIsa(ftTerm,ftListFn(tCol))).
predArgTypes(somethingDescription(ftTerm,ftListFn(ftString))).

:- decl_mpred_hybrid(predProxyAssert,2).
:- begin_transform_moo_preds.

mudSubclass(ttCreateable,tCol).

% :- (do_term_expansions->true;throw(not_term_expansions)).

:-dynamic(tMonster/1).

tCol(tItem).
tCol(prologMultiValued).
tCol(prologSingleValued).
tCol(ttCreateable).
tCol(colDeclarer).

:-add((mudSubclass(isEach(prologMultiValued,prologOrdered,prologNegByFailure,predArgTypes,prologHybrid,prologBuiltin,prologOnly,prologMacroHead,prologListValued,prologSingleValued),tPred))).

:-add((argIsa(isEach(tPred,prologMultiValued,prologOrdered,prologNegByFailure,predArgTypes,prologHybrid,prologBuiltin,prologOnly,prologMacroHead,prologListValued,prologSingleValued),1,tPred))).
:-add((argIsa(isEach(tPred,prologMultiValued,prologOrdered,prologNegByFailure,predArgTypes,prologHybrid,prologBuiltin,prologOnly,prologMacroHead,prologListValued,prologSingleValued),2,ftListFn(ftVoprop)))).

:-add((mudIsa(isEach(prologMultiValued,prologOrdered,prologNegByFailure,predArgTypes,prologHybrid,prologBuiltin,prologOnly,prologMacroHead,prologListValued,prologSingleValued),colDeclarer))).

:-doall((argsIsaProps(F),decl_type(F),add(mudSubclass(F,tRelation)))).
:-doall((argsIsaProps(F),decl_type(F),add(mudIsa(F,colDeclarer)))).

:-decl_mpred_prolog(repl_writer(tAgentGeneric,ftTerm)).
%:-decl_mpred_hybrid(repl_writer(agent,term),[prologSingleValued,argSingleValueDefault(2,default_repl_writer)]).
:-decl_mpred_prolog(repl_to_string(tAgentGeneric,ftTerm)).
%:-decl_mpred_hybrid(repl_to_string(agent,term),[prologSingleValued,argSingleValueDefault(2,default_repl_obj_to_string)]).

%mpred(ArgTypes,PropTypes):-decl_mpred_prop(ArgTypes,PropTypes).
% somethingIsa('NpcCol1012-Ensign732',['NpcCol1012',actor,'MaleAnimal']).

:-dynamic_multifile_exported((mudNeedsLook/2)).



% :- style_check(-discontiguous).
:- debug.

:- begin_prolog_source.

:- decl_mpred_hybrid(mudTermAnglify/2).
:- decl_mpred_prolog(term_anglify_args/6).
:- decl_mpred_prolog(term_anglify_last/2).

term_anglify_last(Head,English):-compound(Head),
   functor(Head,F,A),A>1,
   not(ends_with_icase(F,"Fn")),not(starts_with_icase(F,"SKF-")),
   atom_codes(F,[C|_]),code_type(C,lower),
   Head=..[F|ARGS],
   term_anglify_args(Head,F,A,ARGS,prologSingleValued,English).

mudTermAnglify(Head,EnglishO):- compound(Head), 
   Head=..[F|ARGS],mpred_prop(F,Info),
   member(Info,[prologSingleValued,argMulti(_)]),   
   term_anglify_args(Head,F,1,ARGS,Info,English),fully_expand(English,EnglishO),!.


term_anglify_args(Head,F,A,ARGS,argMulti(Which),English):- !,replace_nth(ARGS,Which,_OldVar,NewVar,NEWARGS),!,
   NewHead=..[F|NEWARGS], findall(NewVar,req(NewHead),ListNewVar),list_to_set_safe(ListNewVar,SetNewVar),NewVar=ftListFn(SetNewVar),
   term_anglify_args(Head,F,A,NewHead,prologSingleValued,English).


/*

term_expansion((term_anglify_args(_Head,F,A,ARGS0,prologSingleValued,English):- add_arg_parts_of_speech(F,1,ARGS0,ARGS),verb_after_arg(F,A,After),
   insert_into(ARGS,After,verbFn(F),NEWARGS),
   fully_expand(NEWARGS,English),X),O).

*/
term_anglify_args(_Head,F,A,ARGS0,prologSingleValued,English):- add_arg_parts_of_speech(F,1,ARGS0,ARGS),verb_after_arg(F,A,After),
insert_into(ARGS,After,verbFn(F),NEWARGS),
fully_expand(NEWARGS,English),!.

unCamelCase(S,String):-any_to_string(S,Str),S\=Str,!,unCamelCase(Str,String),!.
unCamelCase("",""):-!.
unCamelCase(S,String):-sub_string(S,0,1,_,Char),sub_string(S,1,_,0,Rest),unCamelCase(Rest,RestString),string_lower(Char,NewChar),
(Char\=NewChar->atomics_to_string(['_',NewChar,RestString],String);atomics_to_string([Char,RestString],String)),!.

mudTermAnglify(verbFn(mudIsa),[is,a]):-!.
mudTermAnglify(verbFn(F),[is|UL]):-not(string_lower(F,F)),unCamelCase(F,U),atomics_to_string(UL,"_",U).
mudTermAnglify(verbFn(F),[is,F]):-atom_concat(_,'ing',F).
mudTermAnglify(verbFn(F),[F,is]).
% term_anglify(ftCallable(Term),String):-term_to_atom(Term,Atom),any_to_string(Atom,String).
mudTermAnglify(determinerString(Obj,Text),[np(Obj),is,uses,ftString(Text),as,a,determiner]).
mudTermAnglify(nameStrings(Obj,Text),[np(Obj),is,refered,to,as,ftString(Text)]).
mudTermAnglify(mudTermAnglify(Term,Text),[ftCallable(Term),is,converted,to,english,using,ftCallable(Text)]).

:- end_prolog_source.


prologMultiValued(verb_alias(ftString,ftString)).

prologHybrid(mudLabelTypeProps(ftString,tCol,ftListFn(ftTerm(property)))).

prologHybrid(mudTypeGrid(tCol,ftInt,ftTerm)).

prologMultiValued(action_rules(tAgentGeneric,vtVerb,ftTerm(tObj),ftTerm(ftListFn(ftTerm(ftVoprop))))).


tCol(vtActionTemplate).

prologSingleValued(mudAtLoc(tObj,xyzFn(tRegion,ftInt,ftInt,ftInt))).

% =================================================================================================
% BEGIN world database
% =================================================================================================

:- begin_transform_moo_preds.

predArgTypes(forwardRule(ftTerm,ftTerm)).
% forwardRule(localityOfObject(O,Region),atloc(O,LOC)):-

% 


predArgTypes(resultIsa(tFunction,tCol)).
predArgTypes(formatted_resultIsa(ttFormatType,tCol)).

formatted_resultIsa(apathFn(tRegion,vtDirection),areaPath).
formatted_resultIsa(ftDice(ftInt,ftInt,ftInt),ftInt).
resultIsa(apathFn,areaPath).


mudSubclass(areaPath,tDoor).
mudSubclass(tDoor,tFurniture).
mudSubclass(tFurniture,tPartof).

% subclass(dir,value).

% flags
:-decl_mpred_hybrid(tAgentGeneric(ftID),[flag]).
:-decl_mpred_hybrid(tItem(ftID),[flag]).
:-decl_mpred_hybrid(tRegion(ftID),[flag]).
:-decl_mpred_hybrid(tCol(ftID),[flag]).
:-decl_mpred_hybrid(tThinking(tAgentGeneric),[flag]).
:-decl_mpred_hybrid(tDeleted(ftID),[flag]).


% database backing impls
% builtin = ftCallable native
% dynamic = ftCallable dynamic assert/call
% dbase_t = a dbase_t/N
% pttp = pttp _int compiled
% cyc = 
% col =

% multivalued
%mpred(G,[argMulti(AT)|LIST]):-prologMultiValued(G,AT,LIST).

prologMacroHead(somethingIsa(ftTerm,ftListFn(tCol))).
prologMacroHead(somethingDescription(ftTerm,ftListFn(ftString))).
prologMacroHead(objects(tCol,ftListFn(ftID))).
prologMacroHead(sorts(tCol,ftListFn(tCol))).

%mpred(ArgTypes,[prologSingleValued]):-prologSingleValued(ArgTypes).
%mpred(CallSig,[external(M)]):-prologBuiltin(M:CallSig).

prologBuiltin(nearby(tObj,tObj)).
%prologBuiltin(isa(obj,col)).
%prologBuiltin(same(ftID,ftID)).


% prologMultiValued
%prologMultiValued(G,AT,[ordered|LIST]):-prologMultiValued(G,LIST),functor_catch(G,_,AT).

prologMultiValued(pathBetween(tRegion,vtDirection,tRegion)).
prologMultiValued(mudNamed(ftTerm,ftTerm),[]).

:-must_det(argIsa_call(genlPreds,2,_Type)).
% genlPreds(tPred,tPred).

prologMultiValued(cmdsuccess(tAgentGeneric,ftAction)).
prologMultiValued(mudCmdfailure(tAgentGeneric,ftAction)).
prologMultiValued(nameStrings(ftTerm,ftString)).
prologMultiValued(determinerString(ftTerm,ftString)).
prologMultiValued(descriptionHere(ftTerm,ftString),prologOrdered).
prologMultiValued(mudDescription(ftTerm,ftString)).
prologMultiValued(mudKeyword(ftTerm,ftString)).
prologMultiValued(act_affect(ftTerm,ftTerm,ftTerm)).
prologMultiValued(mudMemory(tAgentGeneric,ftTerm)).
predArgTypes(mudWearing(tAgentGeneric,tWearable)).
predArgTypes(success(tAgentGeneric,ftTerm)).

:-decl_mpred(act_affect/3).

% :-decl_mpred_prolog(member/2).


nameStrings(apathFn(Region,Dir),Text):- pathName(Region,Dir,Text).
mudDescription(apathFn(Region,Dir),Text):- pathName(Region,Dir,Text).

mudSubclass(tAgentGeneric,tObj).
mudSubclass(tItem,tObj).
mudSubclass(tItem,tObj).

% single valued
predArgTypes(pathName(tRegion,vtDirection,ftString)).
predArgTypes(verbOverride(ftTerm,ftAction,ftAction),[prologHybrid]).

:-dynamic(spawn_rate/2).

prologSingleValued(type_max_charge(tCol,ftInt)).
prologSingleValued(max_charge(ftTerm,ftInt)).
prologSingleValued(type_max_health(tCol,ftInt)).
prologSingleValued(max_health(ftTerm,ftInt)).

tChannel(A):-tRegion(A).
tChannel(A):-tAgentGeneric(A).

mudSubclass(tAgentGeneric,tSpatialThing).
mudSubclass(tRegion,tSpatialThing).
mudSubclass(tObj,tSpatialThing).
mudSubclass(tItem,tSpatialThing).

mudSubclass(tDrinkable,tItem).
mudSubclass(tPossessable,tItem).
mudSubclass(tUseable,tItem).
mudSubclass(tEatable,tItem).
mudSubclass(tChargeable,tItem).
mudSubclass(tWearable,tItem).


:-decl_mpred_hybrid(pathBetween,3).
:-decl_mpred_hybrid(mudAtLoc,2).
:-decl_mpred_hybrid(mudHealth,2).


:- assert_if_new(mpred_prop(localityOfObject,call_tabled)).


prologMultiValued(equivRule(ftTerm,ftTerm),prologOnly).

% ftCallable code
equivRule(prologMultiValued(CallSig,[predProxyAssert(hooked_asserta),predProxyRetract(hooked_retract),predProxyQuery(call)]),prologBuiltin(CallSig)).


predModule(nearby(tObj,tObj),user).
%  predModule(isa(obj,col),user).
% db_prop_prolog(world,isa(obj,col)).
% db_prop_prolog(world,same(ftID,ftID)).



predArgTypes(somethingIsa(ftTerm,ftListFn(tCol))).
predArgTypes(somethingDescription(ftTerm,ftListFn(ftString))).
predArgTypes(objects(tCol,ftListFn(ftID))).
predArgTypes(predicates(ftListFn(ftVoprop))).
predArgTypes(sorts(tCol,ftListFn(tCol))).

predArgTypes(argSingleValueDefault(prologSingleValued,ftInt,ftTerm)).

predArgTypes(member(ftTerm,ftTerm)).

% live another day to fight (meaning repl_to_string/1 for now is in ftCallable)
% prologSingleValued(repl_writer(agent,term),argSingleValueDefault(2,default_repl_writer)).
% prologSingleValued(repl_to_string(agent,term),[prologSingleValued,argSingleValueDefault(2,default_repl_obj_to_string)]).

%prologMultiValued(label_type(ftString,col),[prologSingleValued]).



prologListValued(mudNearFeet(tAgentGeneric,ftListFn(tSpatialThing)),[]).
prologListValued(mudNearReach(tAgentGeneric,ftListFn(tSpatialThing)),[predModule(user)]).
prologListValued(mudGetPrecepts(tAgentGeneric,ftListFn(tSpatialThing)),[predModule(user)]).

mudSubclass(tFunction,tRelation).
mudSubclass(tPred,tRelation).

predArgTypes(mud_test(ftTerm,ftCallable)).

prologMultiValued(predProxyAssert(prologMultiValued,ftTerm)).
prologNegByFailure(argMulti(prologMultiValued,ftInt)).
prologMultiValued(predProxyQuery(prologMultiValued,ftTerm)).

mudIsa(ftText,ttFormatType).

predArgTypes(type_action_info(tAgentcol,vtActionTemplate,ftText)).
predArgTypes(action_info(vtActionTemplate,ftText)).
predArgTypes(vtActionTemplate(ftTerm)).
predArgTypes(argIsa(tRelation,ftInt,tCol)).
predArgTypes(argFormat(tRelation,ftInt,ttFormatType)).

predArgTypes(agent_text_command(tAgentGeneric,ftText,tAgentGeneric,ftAction)).

prologMultiValued(mudDescription(ftTerm,ftText),[predProxyAssert(add_description),predProxyRetract(remove_description),predProxyQuery(query_description)]).



tCol(tItem).
tCol(vtVerb).


equivRule(nameStrings(apathFn(Region,Dir),Text),pathName(Region,Dir,Text)).
equivRule(mudDescription(apathFn(Region,Dir),Text),pathName(Region,Dir,Text)).

% dbase_t(action_info,What,text("command is: ",What)):- holds_t(action_type,What).

:-decl_mpred_hybrid(mudStowing(tAgentGeneric,tItem)).


% formattype(FormatType):-subclass(FormatType,formattype).

% single valued
mudSubclass(tAgentGeneric,tObj).
mudSubclass(tItem,tObj).


prologMultiValued(pathName(tRegion,vtDirection,ftString)).
prologMultiValued(verbOverride(ftTerm,ftAction,ftAction)).

prologSingleValued(mudAgentTurnnum(tAgentGeneric,ftInt(0))).
prologSingleValued(mudAgentTurnnum(tAgentGeneric,ftInt)).
prologSingleValued(armorLevel(tWearable,ftInt)).
prologSingleValued(mudAttack(tObj,ftInt)).
prologSingleValued(mudCharge(tObj,ftInt(500))).
prologSingleValued(mudCharge(tObj,ftInt),[argSingleValueDefault(2,500)]).
prologSingleValued(chargeCapacity(tChargable,ftInt)).
prologSingleValued(chargeRemaining(tChargable,ftInt)).
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
prologSingleValued(mudTypeGrid(regioncol,ftInt,ftListFn(ftTerm))).
prologSingleValued(mudWeight(tObj,ftInt)).

prologSingleValued(mudHeight(tSpatialThing,ftInt)).

prologMultiValued(comment(ftTerm,ftString)).
prologMultiValued(pathBetween(tRegion,vtDirection,tRegion)).

prologOnly(default_type_props(tCol,ftVoprop)).
prologOnly(default_inst_props(ftID,tCol,ftVoprop)).


default_type_props(tFood,[mudHeight(0)]).
default_type_props(tAgentGeneric,[mudMoveDist(1)]).

default_type_props(tSpatialThing,mudHeight(0)).



% :- must((argIsa_call_0(comment,2,W), W\=term)).

:-decl_mpred_hybrid(mudNeedsLook,2).

prologNegByFailure(mudNeedsLook(tObj,ftBoolean)). 

prologSingleValued(mudNeedsLook(tAgentGeneric,ftBoolean),argSingleValueDefault(2,false)). 


default_type_props(tAgentGeneric,mudNeedsLook(false)).

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

mudFtInfo(ftAction,prologCall(vtActionTemplate(isSelf))).
mudFtInfo(ftAtom,prologCall(atom(isSelf))).
mudFtInfo(ftBoolean,prologCall(member(isSelf,[vTrue,vFalse]))).
mudFtInfo(ftID,prologCall((atom(isSelf),compound(isSelf)))).
mudFtInfo(ftNumber,prologCall(number(isSelf))).
mudFtInfo(ftCallable,prologCall(callable(isSelf))).
mudFtInfo(ftRest,prologCall(true)).
mudFtInfo(ftString,prologCall(string(isSelf))).
mudFtInfo(ftTerm,prologCall(nonvar(isSelf))).
mudFtInfo(ftVar,prologCall(var(isSelf))).

mudSubclass(tAgentGeneric,tSpatialThing).
mudSubclass(areaPath,tDoor).
mudSubclass(tChargeable,tItem).
mudSubclass(tDoor,tItem).
mudSubclass(tDrinkable,tItem).
mudSubclass(tEatable,tItem).
mudSubclass(tItem,tSpatialThing).
mudSubclass(tObj,tSpatialThing).
mudSubclass(tPossessable,tItem).
mudSubclass(tRegion,tSpatialThing).
mudSubclass(tUseable,tItem).
mudSubclass(tWearable,tItem).

disjointWith(tObj,tRegion).
mudIsa(vtDirection,ttValueType).

mudSubclass(vtDirection,tTypevalue).


ttFormatType(ftVoprop).
%  subft(ftNumber,term).
% subft(ftNumber,term).
mudSubft(ftPercent,ftNumber).
mudSubft(ftNumber,ftPercent).
mudSubft(ftAtom,ftTerm).
mudSubft(ftDice,ftInt).
mudSubft(ftInt,ftDice).  

mudSubft(ftID,ftTerm).
mudSubft(ftVoprop,ftTerm).
mudSubft(ftInteger,ftNumber).
mudSubft(ftString,ftTerm).
mudSubft(ftString,ftText).
mudSubft(ftText,ftTerm).
mudSubft(ftTerm,ftProlog).
mudSubft(ftCallable,ftProlog).
mudSubft(ftVar,ftProlog).

:-debug.

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

predArgTypes(mudSize(tSpatialThing,size_value)).
predArgTypes(mudTexture(tSpatialThing,vTextureValue)).

mudSubclass(discoverableType,tCol).

discoverableType(vTextureValue).
discoverableType(size_value).

:-decl_mpred_hybrid(kwLabel,2).

prologMultiValued(mudDescription(ftTerm,ftString)).
prologMultiValued(mudKeyword(ftTerm,ftString)).
prologMultiValued(kwLabel(ftTerm,ftString)).
prologMultiValued(act_affect(tItem,vtVerb,ftTerm(ftVoprop))).
prologMultiValued(mudMemory(tAgentGeneric,ftTerm)).
prologMultiValued(mudGrid(tRegion,ftInt,ftInt,tObj)).
prologMultiValued(mudSubclass(tCol,tCol)).
prologMultiValued(mudIsa(ftTerm,tCol)).

mudSubclass(tPlayer,tAgentGeneric).
mudSubclass(tHumanPlayer,tPlayer).
mudSubclass(tNpcPlayer,tPlayer).
mudSubclass('MaleAnimal',tPlayer).
mudSubclass('FemaleAnimal',tPlayer).
mudSubclass(isEach('PortableObject','ProtectiveAttire','SomethingToWear'),tPossessable).
mudSubclass(isEach('ProtectiveAttire','SomethingToWear'),tWearable).
mudSubclass(tContolDevice,tChargable).
mudSubclass(vtPosture,vtVerb).


equivRule(mudIsa(Whom,tNpcPlayer),and(mudIsa(Whom,tPlayer),naf(mudIsa(Whom,tHumanPlayer)))).

prologListValued(directions(ftTerm,ftListFn(ftTerm))).

prologMultiValued(predModule(tRelation,ftAtom)).

predArgTypes(agent_call_command(tAgentGeneric,ftAction)).

type_max_health(tObj,500).
type_max_charge(tObj,130).

default_type_props(tItem,mudCharge(140)).


% =================================================================================================
% END world database
% =================================================================================================

:- end_transform_moo_preds.
