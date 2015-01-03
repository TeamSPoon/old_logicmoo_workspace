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
tCol('TemporallyExistingThing').
tCol(activeAgent).

tCol(tChannel).
tCol(completeExtentAsserted).

argsIsaInList(mudFtInfo(tFormattype,ftTerm)).
argsIsaInList(mudSubft(tFormattype,tFormattype)).

:-decl_mpred_hybrid((genlInverse/2,genlPreds/2)).

multiValued(genlInverse(tMpred,tMpred)).
multiValued(genlPreds(tMpred,tMpred)).

mpred_prop(mudSubft, completeExtentAsserted).
mpred_prop(mudFtInfo, completeExtentAsserted).
mpred_prop(mudSubft/2, prologHybrid).
mpred_prop(mudFtInfo/2,prologHybrid).
% mpred_prop(subft/2, prologOnly).
% mpred_prop(ft_info/2,prologOnly).
mudSubclass(eachOf(tRegion,tAgentGeneric,actGossup),tChannel).

:-decl_mpred_hybrid(mudMudDictionary(ftText,ftText)).

:-decl_mpred(tRegion(id),tType).

tChannel(actGossup).

createableType('TemporallyExistingThing').

:-dynamic type_max_health/2. 

multiValued(mudContains(tContainer,tObj)).
multiValued(mudGrid(tRegion,ftInt,ftInt,tObj)).
multiValued(mudPossess(tAgentGeneric,tSpatialthing)).
genlPreds(mudPossess,wearsClothing).
multiValued(mudSubclass(tCol,tCol)).
multiValued(mudIsa(ftTerm,tCol)).
argsIsaInList(somethingIsa(ftTerm,ftList(tCol))).
argsIsaInList(somethingDescription(ftTerm,ftList(string))).

:- decl_mpred_hybrid(mudAssertWithPred,2).
:- begin_transform_moo_preds.

mudSubclass(createableType,tCol).

% :- (do_term_expansions->true;throw(not_term_expansions)).

:-dynamic(tMonster/1).

%createableType(col).
tCol(tItem).
tCol(multiValued).
tCol(singleValued).
tCol(createableType).
tCol(colDeclarer).

:-add((mudSubclass(eachOf(multiValued,tOrdered,negationByFailure,argsIsaInList,prologHybrid,prologBuiltin,prologOnly,nonGroundOK,assertionMacroHead,listValued,singleValued),tMpred))).

:-add((argIsa(eachOf(tMpred,multiValued,tOrdered,negationByFailure,argsIsaInList,prologHybrid,prologBuiltin,prologOnly,nonGroundOK,assertionMacroHead,listValued,singleValued),1,tMpred))).
:-add((argIsa(eachOf(tMpred,multiValued,tOrdered,negationByFailure,argsIsaInList,prologHybrid,prologBuiltin,prologOnly,nonGroundOK,assertionMacroHead,listValued,singleValued),2,ftList(prop)))).

:-add((mudIsa(eachOf(multiValued,tOrdered,negationByFailure,argsIsaInList,prologHybrid,prologBuiltin,prologOnly,nonGroundOK,assertionMacroHead,listValued,singleValued),colDeclarer))).

:-doall((argsIsaProps(F),decl_type(F),add(mudSubclass(F,tRelation)))).
:-doall((argsIsaProps(F),decl_type(F),add(mudIsa(F,colDeclarer)))).

:-decl_mpred_prolog(repl_writer(tAgentGeneric,ftTerm)).
%:-decl_mpred_hybrid(repl_writer(agent,term),[singleValued,default_sv(2,default_repl_writer)]).
:-decl_mpred_prolog(repl_to_string(tAgentGeneric,ftTerm)).
%:-decl_mpred_hybrid(repl_to_string(agent,term),[singleValued,default_sv(2,default_repl_obj_to_string)]).

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
   term_anglify_args(Head,F,A,ARGS,singleValued,English).

mudTermAnglify(Head,EnglishO):- compound(Head), 
   Head=..[F|ARGS],mpred_prop(F,Info),
   member(Info,[singleValued,multi(_)]),   
   term_anglify_args(Head,F,1,ARGS,Info,English),fully_expand(English,EnglishO),!.


term_anglify_args(Head,F,A,ARGS,multi(Which),English):- !,replace_nth(ARGS,Which,_OldVar,NewVar,NEWARGS),!,
   NewHead=..[F|NEWARGS], findall(NewVar,req(NewHead),ListNewVar),list_to_set_safe(ListNewVar,SetNewVar),NewVar=ftList(SetNewVar),
   term_anglify_args(Head,F,A,NewHead,singleValued,English).


/*

term_expansion((term_anglify_args(_Head,F,A,ARGS0,singleValued,English):- add_arg_parts_of_speech(F,1,ARGS0,ARGS),verb_after_arg(F,A,After),
   insert_into(ARGS,After,verbFn(F),NEWARGS),
   fully_expand(NEWARGS,English),X),O).

*/
term_anglify_args(_Head,F,A,ARGS0,singleValued,English):- add_arg_parts_of_speech(F,1,ARGS0,ARGS),verb_after_arg(F,A,After),
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
% term_anglify(prolog(Term),String):-term_to_atom(Term,Atom),any_to_string(Atom,String).
mudTermAnglify(determinerString(Obj,Text),[np(Obj),is,uses,string(Text),as,a,determiner]).
mudTermAnglify(nameStrings(Obj,Text),[np(Obj),is,refered,to,as,string(Text)]).
mudTermAnglify(mudTermAnglify(Term,Text),[prolog(Term),is,converted,to,english,using,prolog(Text)]).

:- end_prolog_source.


multiValued(verb_alias(string,string)).

prologHybrid(mudLabelTypeProps(string,tCol,ftList(ftTerm(property)))).

prologHybrid(mudTypeGrid(tCol,ftInt,ftTerm)).

multiValued(action_rules(tAgentGeneric,tVerb,ftTerm(tObj),ftTerm(ftList(ftTerm(property))))).


tCol(tActionType).

singleValued(mudAtLoc(tObj,xyzFn(tRegion,ftInt,ftInt,ftInt))).

% =================================================================================================
% BEGIN world database
% =================================================================================================

:- begin_transform_moo_preds.

argsIsaInList(forwardRule(ftTerm,ftTerm)).
% forwardRule(localityOfObject(O,Region),atloc(O,LOC)):-

% 


argsIsaInList(resultIsa(tFpred,tCol)).


argsIsaInList(formatted_resultIsa(tFormattype,tCol)).

formatted_resultIsa(apathFn(tRegion,vtDirection),areaPath).
formatted_resultIsa(ftDice(ftInt,ftInt,ftInt),ftInt).
resultIsa(apathFn,areaPath).


mudSubclass(areaPath,tDoor).
mudSubclass(tDoor,tFurniture).
mudSubclass(tFurniture,tPartof).

% subclass(dir,value).

% flags
:-decl_mpred_hybrid(tAgentGeneric(id),[flag]).
:-decl_mpred_hybrid(tItem(id),[flag]).
:-decl_mpred_hybrid(tRegion(id),[flag]).
:-decl_mpred_hybrid(tCol(id),[flag]).
:-decl_mpred_hybrid(thinking(tAgentGeneric),[flag]).
:-decl_mpred_hybrid(deleted(id),[flag]).


% database backing impls
% builtin = prolog native
% dynamic = prolog dynamic assert/call
% dbase_t = a dbase_t/N
% pttp = pttp _int compiled
% cyc = 
% col =

% multivalued
%mpred(G,[multi(AT)|LIST]):-multiValued(G,AT,LIST).

assertionMacroHead(somethingIsa(ftTerm,ftList(tCol))).
assertionMacroHead(somethingDescription(ftTerm,ftList(string))).
assertionMacroHead(objects(tCol,ftList(id))).
assertionMacroHead(sorts(tCol,ftList(tCol))).

%mpred(ArgTypes,[singleValued]):-singleValued(ArgTypes).
%mpred(CallSig,[external(M)]):-prologBuiltin(M:CallSig).

prologBuiltin(nearby(tObj,tObj)).
%prologBuiltin(isa(obj,col)).
%prologBuiltin(same(id,id)).


% multiValued
%multiValued(G,AT,[ordered|LIST]):-multiValued(G,LIST),functor_catch(G,_,AT).

multiValued(pathBetween(tRegion,vtDirection,tRegion)).
multiValued(mudNamed(ftTerm,ftTerm),[]).

:-must_det(argIsa_call(genlPreds,2,_Type)).
genlPreds(tMpred,tMpred).

multiValued(cmdsuccess(tAgentGeneric,ftAction)).
multiValued(mudCmdfailure(tAgentGeneric,ftAction)).
multiValued(nameStrings(ftTerm,string)).
multiValued(determinerString(ftTerm,string)).
multiValued(descriptionHere(ftTerm,string),tOrdered).
multiValued(mudDescription(ftTerm,string)).
multiValued(mudKeyword(ftTerm,string)).
multiValued(act_affect(ftTerm,ftTerm,ftTerm)).
multiValued(mudMemory(tAgentGeneric,ftTerm)).
argsIsaInList(mudWearing(tAgentGeneric,tWearable)).
argsIsaInList(success(tAgentGeneric,ftTerm)).

:-decl_mpred(act_affect/3).

% :-decl_mpred_prolog(member/2).


nameStrings(apathFn(Region,Dir),Text):- pathName(Region,Dir,Text).
mudDescription(apathFn(Region,Dir),Text):- pathName(Region,Dir,Text).

mudSubclass(tAgentGeneric,tObj).
mudSubclass(tItem,tObj).
mudSubclass(tItem,tObj).

% single valued
argsIsaInList(pathName(tRegion,vtDirection,string)).
argsIsaInList(verbOverride(ftTerm,ftAction,ftAction),[prologHybrid]).

:-dynamic(spawn_rate/2).

singleValued(type_max_charge(tCol,ftInt)).
singleValued(max_charge(ftTerm,ftInt)).
singleValued(type_max_health(tCol,ftInt)).
singleValued(max_health(ftTerm,ftInt)).

tChannel(A):-tRegion(A).
tChannel(A):-tAgentGeneric(A).

mudSubclass(tAgentGeneric,tSpatialthing).
mudSubclass(tRegion,tSpatialthing).
mudSubclass(tObj,tSpatialthing).
mudSubclass(tItem,tSpatialthing).

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


multiValued(equivRule(ftTerm,ftTerm),nonGroundOK,prologOnly).

% prolog code
equivRule(multiValued(CallSig,[mudAssertWithPred(hooked_asserta),retract_with_pred(hooked_retract),query_with_pred(call)]),prologBuiltin(CallSig)).


ask_module(nearby(tObj,tObj),user).
%  ask_module(isa(obj,col),user).
% db_prop_prolog(world,isa(obj,col)).
% db_prop_prolog(world,same(id,id)).



argsIsaInList(somethingIsa(ftTerm,ftList(tCol))).
argsIsaInList(somethingDescription(ftTerm,ftList(string))).
argsIsaInList(objects(tCol,ftList(id))).
argsIsaInList(predicates(ftList(functor))).
argsIsaInList(sorts(tCol,ftList(tCol))).

argsIsaInList(default_sv(singleValued,ftInt,ftTerm)).

argsIsaInList(member(ftTerm,ftTerm)).

% live another day to fight (meaning repl_to_string/1 for now is in prolog)
% singleValued(repl_writer(agent,term),default_sv(2,default_repl_writer)).
% singleValued(repl_to_string(agent,term),[singleValued,default_sv(2,default_repl_obj_to_string)]).

%multiValued(label_type(string,col),[singleValued]).



listValued(get_feet(tAgentGeneric,ftList(tSpatialthing)),[]).
listValued(get_near(tAgentGeneric,ftList(tSpatialthing)),[ask_module(user)]).
listValued(get_percepts(tAgentGeneric,ftList(tSpatialthing)),[ask_module(user)]).

mudSubclass(tFpred,tPred).
mudSubclass(tMpred,tPred).
mudSubclass(tPred,tRelation).

argsIsaInList(mud_test(ftTerm,prolog)).

multiValued(mudAssertWithPred(multiValued,ftTerm)).
negationByFailure(multi(multiValued,ftInt)).
multiValued(ask_predicate(multiValued,ftTerm)).

mudIsa(ftText,tFormattype).

argsIsaInList(type_action_info(tAgentcol,tActionType,ftText)).
argsIsaInList(action_info(tActionType,ftText)).
argsIsaInList(tActionType(ftTerm)).
argsIsaInList(argIsa(tRelation,ftInt,tCol)).
argsIsaInList(argFormat(tRelation,ftInt,tFormattype)).

argsIsaInList(agent_text_command(tAgentGeneric,ftText,tAgentGeneric,goal)).

multiValued(mudDescription(ftTerm,ftText),[mudAssertWithPred(add_description),retract_with_pred(remove_description),query_with_pred(query_description)]).



tCol(tItem).
tCol(tVerb).


equivRule(nameStrings(apathFn(Region,Dir),Text),pathName(Region,Dir,Text)).
equivRule(mudDescription(apathFn(Region,Dir),Text),pathName(Region,Dir,Text)).

% dbase_t(action_info,What,text("command is: ",What)):- holds_t(action_type,What).

:-decl_mpred_hybrid(mudStowed(tAgentGeneric,tItem)).


% formattype(FormatType):-subclass(FormatType,formattype).

% single valued
mudSubclass(tAgentGeneric,tObj).
mudSubclass(tItem,tObj).


multiValued(pathName(tRegion,vtDirection,string)).
multiValued(verbOverride(ftTerm,ftAction,ftAction)).

singleValued(mudAgentTurnnum(tAgentGeneric,ftInt(0))).
singleValued(mudAgentTurnnum(tAgentGeneric,ftInt)).
singleValued(armorLevel(tWearable,ftInt)).
singleValued(actAttack(tObj,ftInt)).
singleValued(mudCharge(tObj,ftInt(500))).
singleValued(mudCharge(tObj,ftInt),[default_sv(2,500)]).
singleValued(chargeCapacity(tChargable,ftInt)).
singleValued(chargeRemaining(tChargable,ftInt)).
singleValued(mudHealth(tObj,ftInt)).
singleValued(defence(tObj,ftInt)).
singleValued(mudFacing(tObj,vtDirection(vNorth))).
singleValued(mudFacing(tObj,vtDirection)).
singleValued(mudHeight(tObj,ftInt)).
singleValued(objid(tObj,id)).
argsIsaInList(localityOfObject(tObj,tSpatialthing)).
singleValued(mudLastCommand(tAgentGeneric,tCommand)).
singleValued(location_center(tRegion,xyzFn(tRegion,ftInt,ftInt,ftInt))).
singleValued(movedist(tAgentGeneric,ftInt(1))).
singleValued(movedist(tAgentGeneric,number)).
singleValued(mudBareHandDamage(tAgentGeneric,ftDice)).
singleValued(mudLevelOf(tPossessable,ftInt)).
singleValued(mudMaxHitPoints(tAgentGeneric,ftInt)).
singleValued(mudMaxHitPoints(tAgentGeneric,ftInt),[prologHybrid]).
singleValued(mudToHitArmorClass0(tAgentGeneric,ftInt)).
singleValued(mudPermanence(tItem,tVerb,ftTerm)).
singleValued(mudScore(tObj,ftInt)).
singleValued(spawn_rate(propFn(mudSubclass(tObj)),ftInt)).

singleValued(mudSpd(tAgentGeneric,ftInt)).

singleValued(stat_total(tAgentGeneric,ftInt)).
singleValued(mudStm(tAgentGeneric,ftInt)).
singleValued(mudStr(tAgentGeneric,ftInt)).
singleValued(mudTypeGrid(regioncol,ftInt,ftList(ftTerm))).
singleValued(mudWeight(tObj,ftInt)).

singleValued(mudHeight(tSpatialthing,ftInt)).

multiValued(comment(ftTerm,string)).
multiValued(pathBetween(tRegion,vtDirection,tRegion)).

prologOnly(default_type_props(tCol,ftVoprop)).
prologOnly(default_inst_props(id,tCol,ftVoprop)).


default_type_props(tFood,[mudHeight(0)]).
default_type_props(tAgentGeneric,[movedist(1)]).

default_type_props(tSpatialthing,mudHeight(0)).



% :- must((argIsa_call_0(comment,2,W), W\=term)).

:-decl_mpred_hybrid(mudNeedsLook,2).

negationByFailure(mudNeedsLook(tObj,ftBoolean)). 

singleValued(mudNeedsLook(tAgentGeneric,ftBoolean),default_sv(2,false)). 


default_type_props(tAgentGeneric,mudNeedsLook(false)).

tCol(var).
tCol(string).
tFormattype(var).
tFormattype(string).
/*
:-decl_mpred_prolog(var/1).
:-decl_mpred_prolog(string/1).
:-decl_mpred_prolog(number/1).
:-decl_mpred_prolog(integer/1).
*/

:-decl_mpred_hybrid(mudColor/2).
:-decl_mpred_hybrid(mudMaterial/2).
:-decl_mpred_hybrid(mudTexture/2).
:-decl_mpred_hybrid(mudSize/2).
:-decl_mpred_hybrid(mudShape/2).

mudFtInfo(ftAction(prolog),tFormatted).
mudFtInfo(apathFn(tRegion,vtDirection),tFormatted).
mudFtInfo(atom,atom(self)).
mudFtInfo(ftBoolean,member(self,[true,false])).
mudFtInfo(ftDice(ftInt,ftInt,ftInt),tFormatted).
% ft_info(dir,is_any_dir(self)).
% ft_info(formattype,formatted).
mudFtInfo(id,nonvar(self)).
mudFtInfo(ftList(tCol),tFormatted).
mudFtInfo(number,number(self)).
mudFtInfo(prolog,true).
mudFtInfo(ftRest,true).
mudFtInfo(string,string(self)).
mudFtInfo(ftTerm,nonvar(self)).
% ft_info(col,col(self)).
mudFtInfo(var,var(self)).
mudFtInfo(xyzFn(tRegion,ftInt,ftInt,ftInt),tFormatted).

mudSubclass(tAgentGeneric,tSpatialthing).
mudSubclass(areaPath,tDoor).
mudSubclass(tChargeable,tItem).
mudSubclass(tDoor,tItem).
mudSubclass(tDrinkable,tItem).
mudSubclass(tEatable,tItem).
mudSubclass(tItem,tSpatialthing).
mudSubclass(tObj,tSpatialthing).
mudSubclass(tPossessable,tItem).
mudSubclass(tRegion,tSpatialthing).
mudSubclass(tUseable,tItem).
mudSubclass(tWearable,tItem).

disjointWith(tObj,tRegion).
mudIsa(vtDirection,tValuetype).

mudSubclass(vtDirection,tTypevalue).


tFormattype(ftVoprop).
%  subft(number,term).
% subft(number,term).
mudSubft(ftPercent,number).
mudSubft(number,ftPercent).
mudSubft(atom,ftTerm).
mudSubft(ftDice,ftInt).
mudSubft(ftInt,ftDice).  

mudSubft(id,ftTerm).
mudSubft(ftVoprop,ftTerm).
mudSubft(integer,number).
mudSubft(string,ftTerm).
mudSubft(string,ftText).
mudSubft(ftTerm,prolog).
mudSubft(ftText,ftTerm).
mudSubft(var,prolog).

:-debug.

% flags
negationByFailure(tAgentGeneric(id)).
negationByFailure(tItem(id)).
negationByFailure(tRegion(id)).
negationByFailure(tCol(id)).

negationByFailure(thinking(tAgentGeneric)).
negationByFailure(deleted(id)).


multiValued(mudDescription(ftTerm,ftText),[mudAssertWithPred(add_description)]).

multiValued(verbAsWell(ftTerm,ftAction,ftAction)).
multiValued(nameStrings(ftTerm,string)).
multiValued(determinerString(ftTerm,string)).
multiValued(descriptionHere(ftTerm,string)).

argsIsaInList(mudSize(tSpatialthing,size_value)).
argsIsaInList(mudTexture(tSpatialthing,vTextureValue)).

mudSubclass(discoverableType,tCol).

discoverableType(vTextureValue).
discoverableType(size_value).

:-decl_mpred_hybrid(kwLabel,2).

multiValued(mudDescription(ftTerm,string)).
multiValued(mudKeyword(ftTerm,string)).
multiValued(kwLabel(ftTerm,string)).
multiValued(act_affect(tItem,tVerb,ftTerm(effect))).
multiValued(mudMemory(tAgentGeneric,ftTerm)).
multiValued(wearsClothing(tAgentGeneric,tWearable)).
multiValued(mudGrid(tRegion,ftInt,ftInt,tObj)).
multiValued(mudPossess(tAgentGeneric,tItem)).
multiValued(mudSubclass(tCol,tCol)).
multiValued(mudIsa(ftTerm,tCol)).

mudSubclass(tPlayer,tAgentGeneric).
mudSubclass(tHumanPlayer,tPlayer).
mudSubclass(tNpcPlayer,tPlayer).
mudSubclass('MaleAnimal',tPlayer).
mudSubclass('FemaleAnimal',tPlayer).
mudSubclass(eachOf('PortableObject','ProtectiveAttire','SomethingToWear'),tPossessable).
mudSubclass(eachOf('ProtectiveAttire','SomethingToWear'),tWearable).
mudSubclass('ControlDevice',tChargable).
mudSubclass(tPosture,tCommand).


equivRule(mudIsa(Whom,tNpcPlayer),and(mudIsa(Whom,tPlayer),naf(mudIsa(Whom,tHumanPlayer)))).

listValued(directions(ftTerm,ftList(ftTerm))).

multiValued(ask_module(tRelation,atom)).

argsIsaInList(agent_call_command(tAgentGeneric,ftTerm(tVerb))).

type_max_health(tObj,500).
type_max_charge(tObj,130).

default_type_props(tItem,mudCharge(140)).


% =================================================================================================
% END world database
% =================================================================================================

:- end_transform_moo_preds.
