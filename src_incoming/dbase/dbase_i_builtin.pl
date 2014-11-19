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
type('TemporallyExistingThing').
type(activeAgent).

type(channel).
type(completeExtentAsserted).

argsIsaInList(ft_info(formattype,term)).
argsIsaInList(subft(formattype,formattype)).

:-decl_mpred_hybrid((genlInverse/2,genlPreds/2)).

multiValued(genlInverse(mpred,mpred)).
multiValued(genlPreds(mpred,mpred)).

mpred_prop(subft, completeExtentAsserted).
mpred_prop(ft_info, completeExtentAsserted).
mpred_prop(subft/2, prologHybrid).
mpred_prop(ft_info/2,prologHybrid).
% mpred_prop(subft/2, prologOnly).
% mpred_prop(ft_info/2,prologOnly).
subclass(eachOf(region,agent,gossup),channel).


channel(gossup).

createableType('TemporallyExistingThing').

:-dynamic moo:type_max_health/2. 

multiValued(contains(container,obj)).
multiValued(grid(region,int,int,obj)).
multiValued(possess(agent,spatialthing)).
genlPreds(possess,wearsClothing).
multiValued(subclass(type,type)).
multiValued(isa(term,type)).
argsIsaInList(somethingIsa(term,list(type))).
argsIsaInList(somethingDescription(term,list(string))).

:- decl_mpred_hybrid(assert_with_pred,2).
:- begin_transform_moo_preds.

subclass(createableType,type).

% :- (do_term_expansions->true;throw(not_term_expansions)).

:-dynamic(monster/1).

%createableType(type).
type(item).
type(multiValued).
type(singleValued).
type(createableType).
type(typeDeclarer).

:-add((subclass(eachOf(multiValued,ordered,negationByFailure,argsIsaInList,prologHybrid,prologBuiltin,prologOnly,nonGroundOK,assertionMacroHead,listValued,singleValued),mpred))).

:-add((argIsa(eachOf(mpred,multiValued,ordered,negationByFailure,argsIsaInList,prologHybrid,prologBuiltin,prologOnly,nonGroundOK,assertionMacroHead,listValued,singleValued),1,mpred))).
:-add((argIsa(eachOf(mpred,multiValued,ordered,negationByFailure,argsIsaInList,prologHybrid,prologBuiltin,prologOnly,nonGroundOK,assertionMacroHead,listValued,singleValued),2,list(prop)))).

:-add((isa(eachOf(multiValued,ordered,negationByFailure,argsIsaInList,prologHybrid,prologBuiltin,prologOnly,nonGroundOK,assertionMacroHead,listValued,singleValued),typeDeclarer))).

:-doall((argsIsaProps(F),decl_type(F),add(subclass(F,relation)))).
:-doall((argsIsaProps(F),decl_type(F),add(isa(F,typeDeclarer)))).

:-decl_mpred_prolog(repl_writer(agent,term)).
%:-decl_mpred_hybrid(repl_writer(agent,term),[singleValued,default_sv(2,look:default_repl_writer)]).
:-decl_mpred_prolog(repl_to_string(agent,term)).
%:-decl_mpred_hybrid(repl_to_string(agent,term),[singleValued,default_sv(2,look:default_repl_obj_to_string)]).

%mpred(ArgTypes,PropTypes):-decl_mpred_prop(ArgTypes,PropTypes).
% somethingIsa('NpcCol1012-Ensign732',['NpcCol1012',actor,'MaleAnimal']).

:-dynamic_multifile_exported((needs_look/2)).



% :- style_check(-discontiguous).
:- debug.

:- begin_prolog_source.

:- decl_mpred_hybrid(term_anglify/2).
:- decl_mpred_prolog(term_anglify_args/6).
:- decl_mpred_prolog(term_anglify_last/2).

term_anglify_last(Head,English):-compound(Head),
   functor(Head,F,A),A>1,
   not(ends_with_icase(F,"Fn")),not(starts_with_icase(F,"SKF-")),
   atom_codes(F,[C|_]),code_type(C,lower),
   Head=..[F|ARGS],
   term_anglify_args(Head,F,A,ARGS,singleValued,English).

term_anglify(Head,EnglishO):- compound(Head), 
   Head=..[F|ARGS],mpred_prop(F,Info),
   member(Info,[singleValued,multi(_)]),   
   term_anglify_args(Head,F,1,ARGS,Info,English),world:fully_expand(English,EnglishO),!.


term_anglify_args(Head,F,A,ARGS,multi(Which),English):- !,replace_nth(ARGS,Which,_OldVar,NewVar,NEWARGS),!,
   NewHead=..[F|NEWARGS], findall(NewVar,req(NewHead),ListNewVar),list_to_set_safe(ListNewVar,SetNewVar),NewVar=list(SetNewVar),
   term_anglify_args(Head,F,A,NewHead,singleValued,English).


/*

term_expansion((term_anglify_args(_Head,F,A,ARGS0,singleValued,English):- add_arg_parts_of_speech(F,1,ARGS0,ARGS),verb_after_arg(F,A,After),
insert_into(ARGS,After,verbFn(F),NEWARGS),
world:fully_expand(NEWARGS,English),X),O).

*/
term_anglify_args(_Head,F,A,ARGS0,singleValued,English):- add_arg_parts_of_speech(F,1,ARGS0,ARGS),verb_after_arg(F,A,After),
insert_into(ARGS,After,verbFn(F),NEWARGS),
world:fully_expand(NEWARGS,English),!.

unCamelCase(S,String):-any_to_string(S,Str),S\=Str,!,unCamelCase(Str,String),!.
unCamelCase("",""):-!.
unCamelCase(S,String):-sub_string(S,0,1,_,Char),sub_string(S,1,_,0,Rest),unCamelCase(Rest,RestString),string_lower(Char,NewChar),
(Char\=NewChar->atomics_to_string(['_',NewChar,RestString],String);atomics_to_string([Char,RestString],String)),!.

term_anglify(verbFn(isa),[is,a]):-!.
term_anglify(verbFn(F),[is|UL]):-not(string_lower(F,F)),unCamelCase(F,U),atomics_to_string(UL,"_",U).
term_anglify(verbFn(F),[is,F]):-atom_concat(_,'ing',F).
term_anglify(verbFn(F),[F,is]).
% term_anglify(prolog(Term),String):-term_to_atom(Term,Atom),any_to_string(Atom,String).
term_anglify(determinerString(Obj,Text),[np(Obj),is,uses,string(Text),as,a,determiner]).
term_anglify(nameStrings(Obj,Text),[np(Obj),is,refered,to,as,string(Text)]).
term_anglify(term_anglify(Term,Text),[prolog(Term),is,converted,to,english,using,prolog(Text)]).

:- end_prolog_source.


multiValued(verb_alias(string,string)).

prologHybrid(label_type_props(string,type,list(term(property)))).

prologHybrid(type_grid(type,int,term)).

multiValued(action_rules(agent,verb,term(obj),term(list(term(property))))).


type(actiontype).

singleValued(atloc(obj,xyz(region,int,int,int))).

% =================================================================================================
% BEGIN world database
% =================================================================================================

:- begin_transform_moo_preds.

argsIsaInList(forwardRule(term,term)).
% forwardRule(localityOfObject(O,Region),atloc(O,LOC)):-

% 


argsIsaInList(resultIsa(fpred,type)).


argsIsaInList(formatted_resultIsa(formattype,type)).

formatted_resultIsa(apath(region,dir),areaPath).
formatted_resultIsa(dice(int,int,int),int).
resultIsa(apath,areaPath).


subclass(areaPath,door).
subclass(door,furniture).
subclass(furniture,partof).

% subclass(dir,value).

% flags
:-decl_mpred_hybrid(agent(id),[flag]).
:-decl_mpred_hybrid(item(id),[flag]).
:-decl_mpred_hybrid(region(id),[flag]).
:-decl_mpred_hybrid(type(id),[flag]).
:-decl_mpred_hybrid(thinking(agent),[flag]).
:-decl_mpred_hybrid(deleted(id),[flag]).


% database backing impls
% builtin = prolog native
% dynamic = prolog dynamic assert/call
% dbase_t = a dbase_t/N
% pttp = pttp _int compiled
% cyc = 
% type =

% multivalued
%mpred(G,[multi(AT)|LIST]):-multiValued(G,AT,LIST).

assertionMacroHead(somethingIsa(term,list(type))).
assertionMacroHead(somethingDescription(term,list(string))).
assertionMacroHead(objects(type,list(id))).
assertionMacroHead(sorts(type,list(type))).

%mpred(ArgTypes,[singleValued]):-singleValued(ArgTypes).
%mpred(CallSig,[external(M)]):-prologBuiltin(M:CallSig).

prologBuiltin(world:nearby(obj,obj)).
%prologBuiltin(world:isa(obj,type)).
%prologBuiltin(world:same(id,id)).


% multiValued
%multiValued(G,AT,[ordered|LIST]):-multiValued(G,LIST),functor_catch(G,_,AT).

multiValued(pathBetween(region,dir,region)).
multiValued(named(term,term),[]).

:-must_det(argIsa_call(genlPreds,2,_Type)).
genlPreds(mpred,mpred).

multiValued(cmdsuccess(agent,action)).
multiValued(cmdfailure(agent,action)).
multiValued(nameStrings(term,string)).
multiValued(determinerString(term,string)).
multiValued(descriptionHere(term,string),ordered).
multiValued(description(term,string)).
multiValued(keyword(term,string)).
multiValued(act_affect(term,term,term)).
multiValued(memory(agent,term)).
argsIsaInList(wearing(agent,wearable)).
argsIsaInList(success(agent,term)).

:-decl_mpred(act_affect/3).

:-decl_mpred_prolog(member/2).


nameStrings(apath(Region,Dir),Text):- pathName(Region,Dir,Text).
description(apath(Region,Dir),Text):- pathName(Region,Dir,Text).

subclass(agent,obj).
subclass(item,obj).
subclass(item,obj).

% single valued
argsIsaInList(pathName(region,dir,string)).
argsIsaInList(verbOverride(term,action,action),[prologHybrid]).

:-dynamic(spawn_rate/2).

singleValued(type_max_charge(type,int)).
singleValued(max_charge(term,int)).
singleValued(type_max_health(type,int)).
singleValued(max_health(term,int)).

channel(A):-region(A).
channel(A):-agent(A).

subclass(agent,spatialthing).
subclass(region,spatialthing).
subclass(obj,spatialthing).
subclass(item,spatialthing).

subclass(drinkable,item).
subclass(possessable,item).
subclass(useable,item).
subclass(eatable,item).
subclass(chargeable,item).
subclass(wearable,item).


:-decl_mpred_hybrid(pathBetween,3).
:-decl_mpred_hybrid(atloc,2).
:-decl_mpred_hybrid(health,2).


:- assert_if_new(mpred_prop(localityOfObject,call_tabled)).


multiValued(equivRule(term,term),nonGroundOK,prologOnly).

% prolog code
equivRule(multiValued(CallSig,[assert_with_pred(hooked_asserta),retract_with_pred(hooked_retract),query_with_pred(call)]),prologBuiltin(CallSig)).


ask_module(nearby(obj,obj),world).
%  ask_module(isa(obj,type),world).
% db_prop_prolog(world,isa(obj,type)).
% db_prop_prolog(world,same(id,id)).



argsIsaInList(somethingIsa(term,list(type))).
argsIsaInList(somethingDescription(term,list(string))).
argsIsaInList(objects(type,list(id))).
argsIsaInList(predicates(list(functor))).
argsIsaInList(sorts(type,list(type))).

argsIsaInList(default_sv(singleValued,int,term)).

argsIsaInList(member(term,term)).

% live another day to fight (meaning repl_to_string/1 for now is in prolog)
% singleValued(repl_writer(agent,term),default_sv(2,look:default_repl_writer)).
% singleValued(repl_to_string(agent,term),[singleValued,default_sv(2,look:default_repl_obj_to_string)]).

%multiValued(label_type(string,type),[singleValued]).



listValued(look:get_feet(agent,list(spatialthing)),[]).
listValued(look:get_near(agent,list(spatialthing)),[ask_module(look)]).
listValued(get_percepts(agent,list(spatialthing)),[ask_module(look)]).

argsIsaInList(mud_test(term,prolog)).

multiValued(assert_with_pred(multiValued,term)).
negationByFailure(multi(multiValued,int)).
multiValued(ask_predicate(multiValued,term)).

isa(text,formattype).

argsIsaInList(type_action_info(agenttype,actiontype,text)).
argsIsaInList(action_info(actiontype,text)).
argsIsaInList(actiontype(term)).
argsIsaInList(argIsa(relation,int,type)).
argsIsaInList(argFormat(relation,int,formattype)).

argsIsaInList(agent_text_command(agent,text,agent,goal)).

multiValued(description(term,text),[assert_with_pred(add_description),retract_with_pred(remove_description),query_with_pred(query_description)]).



type(item).


equivRule(nameStrings(apath(Region,Dir),Text),pathName(Region,Dir,Text)).
equivRule(description(apath(Region,Dir),Text),pathName(Region,Dir,Text)).

% dbase_t(action_info,What,text("command is: ",What)):- holds_t(actiontype,What).




% formattype(FormatType):-subclass(FormatType,formattype).

term_specifier_text(Text,pred):- mpred_prop(Text,arity(_)).

% single valued
subclass(agent,obj).
subclass(item,obj).


multiValued(pathName(region,dir,string)).
multiValued(verbOverride(term,action,action)).

singleValued(agent_turnnum(agent,int(0))).
singleValued(agent_turnnum(agent,int)).
singleValued(armorLevel(wearable,int)).
singleValued(attack(obj,int)).
singleValued(charge(obj,int(500))).
singleValued(charge(obj,int),[default_sv(2,500)]).
singleValued(chargeCapacity(chargable,int)).
singleValued(chargeRemaining(chargable,int)).
singleValued(health(obj,int)).
singleValued(defence(obj,int)).
singleValued(facing(obj,dir(n))).
singleValued(facing(obj,dir)).
singleValued(height(obj,int)).
singleValued(objid(obj,id)).
singleValued(localityOfObject(obj,spatialthing)).
singleValued(last_command(agent,command)).
singleValued(location_center(region,xyz(region,int,int,int))).
singleValued(movedist(agent,int(1))).
singleValued(movedist(agent,number)).
singleValued(mudBareHandDamage(agent,dice)).
singleValued(mudLevelOf(possessable,int)).
singleValued(mudMaxHitPoints(agent,int)).
singleValued(mudMaxHitPoints(agent,int),[prologHybrid]).
singleValued(mudToHitArmorClass0(agent,int)).
singleValued(permanence(item,verb,term)).
singleValued(score(obj,int)).
singleValued(spawn_rate(propFn(subclass(obj)),int)).

singleValued(spd(agent,int)).

singleValued(stat_total(agent,int)).
singleValued(stm(agent,int)).
singleValued(str(agent,int)).
singleValued(type_grid(regiontype,int,list(term))).
singleValued(weight(obj,int)).

singleValued(height(spatialthing,int)).

multiValued(comment(term,string)).
multiValued(pathBetween(region,dir,region)).

prologOnly(default_type_props(type,voprop)).
prologOnly(default_inst_props(id,type,voprop)).


default_type_props(food,[height(0)]).

default_type_props(spatialthing,height(0)).



% :- must((argIsa_call_0(comment,2,W), W\=term)).

:-decl_mpred_hybrid(needs_look,2).

negationByFailure(needs_look(obj,boolean)). 

singleValued(needs_look(agent,boolean),default_sv(2,false)). 


default_type_props(agent,needs_look(false)).

type(var).
type(string).
formattype(var).
formattype(string).
:-decl_mpred_prolog(var/1).
:-decl_mpred_prolog(string/1).
:-decl_mpred_prolog(number/1).
:-decl_mpred_prolog(integer/1).

:-decl_mpred_hybrid(color/2).
:-decl_mpred_hybrid(material/2).
:-decl_mpred_hybrid(texture/2).
:-decl_mpred_hybrid(size/2).
:-decl_mpred_hybrid(shape/2).

ft_info(action(prolog),formatted).
ft_info(apath(region,dir),formatted).
ft_info(atom,atom(self)).
ft_info(boolean,member(self,[true,false])).
ft_info(dice(int,int,int),formatted).
% ft_info(dir,is_any_dir(self)).
% ft_info(formattype,formatted).
ft_info(id,nonvar(self)).
ft_info(list(type),formatted).
ft_info(number,number(self)).
ft_info(prolog,true).
ft_info(rest,true).
ft_info(string,string(self)).
ft_info(term,nonvar(self)).
% ft_info(type,type(self)).
ft_info(var,var(self)).
ft_info(xyz(region,int,int,int),formatted).

subclass(agent,spatialthing).
subclass(areaPath,door).
subclass(chargeable,item).
subclass(door,item).
subclass(drinkable,item).
subclass(eatable,item).
subclass(item,spatialthing).
subclass(obj,spatialthing).
subclass(possessable,item).
subclass(region,spatialthing).
subclass(useable,item).
subclass(wearable,item).

disjointWith(obj,region).

formattype(voprop).
%  subft(number,term).
% subft(number,term).
subft(percent,number).
subft(number,percent).
subft(atom,term).
subft(dice,int).
subft(int,dice).  
subft(dir,string).
subft(id,term).
subft(voprop,term).
subft(int,integer).
subft(integer,number).
subft(string,term).
subft(string,text).
subft(term,prolog).
subft(text,term).
subft(var,prolog).

:-debug.

% flags
negationByFailure(agent(id)).
negationByFailure(item(id)).
negationByFailure(region(id)).
negationByFailure(type(id)).

negationByFailure(thinking(agent)).
negationByFailure(deleted(id)).


multiValued(description(term,text),[assert_with_pred(add_description)]).

multiValued(verbAsWell(term,action,action)).
multiValued(nameStrings(term,string)).
multiValued(determinerString(term,string)).
multiValued(descriptionHere(term,string)).

argsIsaInList(size(spatialthing,size_value)).
argsIsaInList(texture(spatialthing,texture_value)).

subclass(discoverableType,type).

discoverableType(texture_value).
discoverableType(size_value).

:-decl_mpred_hybrid(kwLabel,2).

multiValued(description(term,string)).
multiValued(keyword(term,string)).
multiValued(kwLabel(term,string)).
multiValued(act_affect(item,verb,term(effect))).
multiValued(memory(agent,term)).
multiValued(wearsClothing(agent,wearable)).
multiValued(grid(region,int,int,obj)).
multiValued(possess(agent,item)).
multiValued(subclass(type,type)).
multiValued(isa(term,type)).

subclass(player,agent).
subclass(human_player,player).
subclass(npc_player,player).
subclass('MaleAnimal',player).
subclass('FemaleAnimal',player).
subclass(eachOf('PortableObject','ProtectiveAttire','SomethingToWear'),possessable).
subclass(eachOf('ProtectiveAttire','SomethingToWear'),wearable).
subclass('ControlDevice',chargable).
subclass(posture,command).


equivRule(isa(Whom,npc_player),and(isa(Whom,player),naf(isa(Whom,human_player)))).

listValued(directions(term,list(term))).

multiValued(ask_module(relation,atom)).

argsIsaInList(agent_call_command(agent,term(verb))).

type_max_health(obj,500).
type_max_charge(obj,130).

default_type_props(item,charge(140)).


% =================================================================================================
% END world database
% =================================================================================================

:- end_transform_moo_preds.


