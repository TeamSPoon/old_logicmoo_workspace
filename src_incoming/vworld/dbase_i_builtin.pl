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
% props(Obj,height(ObjHt))  == k(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt))  == padd(height,Obj,ObjHt,...) == add(QueryForm)
% kretract[all](Obj,height(ObjHt))  == kretract[all](Obj,height,ObjHt) == pretract[all](height,Obj,ObjHt) == del[all](QueryForm)
% keraseall(AnyTerm).
%
%
% Dec 13, 2035
% Douglas Miles
*/

% =================================================================================================
% BEGIN world English
% =================================================================================================


moo:term_anglify_last(Head,English):-compound(Head),
   functor(Head,F,A),A>1,
   not(ends_with_icase(F,"Fn")),not(starts_with_icase(F,"SKF-")),
   atom_codes(F,[C|_]),code_type(C,lower),
   Head=..[F|ARGS],
   term_anglify_args(Head,F,A,ARGS,singleValued,English).

moo:term_anglify(Head,English):-
      functor(Head,F,A),
      moodb:is_db_prop(F,A,Info),member(Info,[singleValued,multi(_)]),
      Head=..[F|ARGS],
      term_anglify_args(Head,F,A,ARGS,Info,English),fully_expand(English,EnglishO),!.


term_anglify_args(Head,F,A,ARGS,multi(Which),English):- !,replace_nth(ARGS,Which,OldVar,NewVar,NEWARGS),!,
      NewHead=..[F|NEWARGS], findall(NewVar,NewHead,ListNewVar),list_to_set_safe(ListNewVar,SetNewVar),NewVar=list(ListNewVar),
      term_anglify_args(Head,F,A,NewHead,singleValued,English).
term_anglify_args(Head,F,A,ARGS0,singleValued,English):- add_arg_parts_of_speech(F,1,ARGS0,ARGS),verb_after_arg(F,A,After),
   insert_into(ARGS,After,verbFn(F),NEWARGS),
   fully_expand(NEWARGS,English),!.

unCamelCase(S,String):-any_to_string(S,Str),S\=Str,!,unCamelCase(Str,String),!.
unCamelCase("",""):-!.
unCamelCase(S,String):-sub_string(S,0,1,_,Char),sub_string(S,1,_,0,Rest),unCamelCase(Rest,RestString),string_lower(Char,NewChar),
  (Char\=NewChar->atomics_to_string(['_',NewChar,RestString],String);atomics_to_string([Char,RestString],String)),!.

moo:term_anglify(verbFn(mud_isa),[is,a]):-!.
moo:term_anglify(verbFn(isa),[is,a]):-!.
moo:term_anglify(verbFn(F),[is|UL]):-not(string_lower(F,F)),unCamelCase(F,U),atomics_to_string(UL,"_",U).
moo:term_anglify(verbFn(F),[is,F]):-atom_concat(_,'ing',F).
moo:term_anglify(verbFn(F),[F,is]).
% moo:term_anglify(prolog(Term),String):-term_to_atom(Term,Atom),any_to_string(Atom,String).
moo:term_anglify(determinerString(Obj,Text),[np(Obj),is,uses,string(Text),as,a,determiner]).
moo:term_anglify(nameString(Obj,Text),[np(Obj),is,refered,to,as,string(Text)]).
moo:term_anglify(moo:term_anglify(Term,Text),[prolog(Term),is,converted,to,english,using,prolog(Text)]).


add_arg_parts_of_speech(_F,_N,[],[]).
add_arg_parts_of_speech(F,N,[A|ARGS0],[ARG|ARGS]):-argIsa_call_or_undressed(F,N,A,ARG),N1 is N+1, add_arg_parts_of_speech(F,N1,ARGS0,ARGS).

argIsa_call_or_undressed(F,N,Obj,fN(Obj,Type)):-argIsa_call_0(F,N,Type),!.
argIsa_call_or_undressed(F,N,Obj,Obj).

verb_after_arg(_,_,1).


:- style_check(+discontiguous).


db_prop_argsIsa(ArgTypes):-db_prop_findall(ArgTypes,_).


db_prop_findall(ArgTypes,PROPS):-moo:db_prop(ArgTypes,PROPS).
db_prop_findall(ArgTypes,PROPS):-db_prop_multi(ArgTypes,PROPS).
% db_prop_findall(ArgTypes,[]):-moo:db_prop(ArgTypes).
db_prop_findall(ArgTypes,[]):-db_prop_sv(ArgTypes).
db_prop_findall(ArgTypes,[]):-db_prop_multi(ArgTypes).


moo:db_prop(ArgTypes,[from_game_load]):-db_prop_from_game_load(ArgTypes).

% =================================================================================================
% BEGIN world database
% =================================================================================================

:- moodb:begin_transform_moo_preds.

dbase_t(inRegion,O,Region):-atloc(O,LOC),locationToRegion(LOC,Region).
dbase_t(inRegion,apath(Region,Dir),Region):- holds_t(pathBetween,Region,Dir,_To).

db_prop_format(apath(region,dir),areaPath).
db_prop_format(dice(int,int,int),int).
db_resultIsa(apath,areaPath).


% prolog code
moo:db_prop(CallSig,[module(M)]):-db_prop_prolog(M,CallSig).


% db_prop_prolog(world,nearby(object,object)).
db_prop_prolog(world,mud_isa(object,type)).
% db_prop_prolog(world,same(id,id)).

% game_assert
moo:db_prop(G,[assert(game_assert)]):- db_prop_game_assert(G).


db_prop_game_assert(somethingIsa(term,list(type))).
db_prop_game_assert(somethingDescription(term,list(string))).
db_prop_game_assert(objects(type,list(id))).
% db_prop_game_assert(predicates(list(functors))).
db_prop_game_assert(sorts(type,list(type))).

% multivalued
moo:db_prop(G,[multi(AT)|LIST]):-db_prop_multi(G,AT,LIST).

% singlevalued
moo:db_prop(ArgTypes,[singleValued]):-db_prop_sv(ArgTypes).

% flags
moo:db_prop(ArgTypes,[flag,singleValued]):-db_prop_flag(ArgTypes).
moo:db_prop(ArgTypes,[flag,singleValued]):-db_prop_flag(ArgTypes).

moo:db_prop(repl_writer(agent,term),[singleValued,default(look:default_repl_writer)]).
moo:db_prop(repl_to_string(agent,term),[singleValued,default(look:default_repl_obj_to_string)]).



moo:db_prop(look:get_feet(agent,list(spatial)),[]).
moo:db_prop(look:get_near(agent,list(spatial)),[module(look)]).
moo:db_prop(get_precepts(agent,list(spatial)),[module(look)]).



type(T):-moo:subclass(A,B),(T=B;T=A).
type(item).


dbase_t(nameString,apath(Region,Dir),Text):- holds_t(pathName,Region,Dir,Text).
description(apath(Region,Dir),Text):- holds_t(pathName, Region,Dir,Text).

dbase_t(type_action_help,agent,What,Help):- holds_t(action_help,What,Help).
dbase_t(action_help,What,text("command is: ",What)):- holds_t(action_info,What).

moo:action_info(list(term)).
moo:agent_call_command(_Gent,list(Obj)):- term_listing(Obj).

channel(A):-region(A).
channel(A):-agent(A).
channel(gossup).

moo:subclass(agent,spatialthing).
moo:subclass(region,spatialthing).
moo:subclass(object,spatialthing).
moo:subclass(item,spatialthing).


moo:subclass(drinkable,item).
moo:subclass(possessable,item).
moo:subclass(useable,item).
moo:subclass(eatable,item).
moo:subclass(chargeable,item).
moo:subclass(wearable,item).


moo:type_default_props(_,food,[height(0)]).

moo:specifier_text(Text,pred):- moodb:is_db_prop(Text,_,arity(_,_)).

% single valued
moo:subclass(agent,object).
moo:subclass(item,object).


moo:db_prop(pathName(region,dir,string),[]).
db_prop_sv(verbOverride(term,action,action)).

db_prop_sv(atloc(object,xyz(region,int,int,int))).
db_prop_sv(act_turn(agent,int)).
db_prop_sv(armorLevel(possessable,int)).
db_prop_sv(attack(agent,int)).
db_prop_sv(charge(agent,int)).
db_prop_sv(chargeCapacity(chargable,int)).
db_prop_sv(chargeRemaining(chargable,int)).
db_prop_sv(damage(agent,int)).
db_prop_sv(defence(agent,int)).
db_prop_sv(facing(agent,dir)).
db_prop_sv(height(agent,int)).
% db_prop_sv(id(object,term)).
db_prop_sv(inRegion(term,region)).
db_prop_sv(last_command(agent,command)).
db_prop_sv(location_center(region,xyz(region,int,int,int))).
db_prop_sv(movedist(agent,number)).
db_prop_sv(mudBareHandDamage(agent,dice)).
db_prop_sv(mudLevelOf(possessable,int)).
db_prop_sv(mudMaxHitPoints(agent,int)).
db_prop_sv(mudToHitArmorClass0(agent,int)).
db_prop_sv(pathBetween(region,dir,region)).
db_prop_sv(permanence(item,verb,int)).
db_prop_sv(score(object,int)).
db_prop_sv(spawn_rate(moo:subclass(object),int)).
db_prop_sv(spd(agent,int)).
db_prop_sv(stm(agent,int)).
db_prop_sv(str(agent,int)).
% db_prop_sv(type_grid(regiontype,int,list(term))).
db_prop_sv(weight(object,int)).

db_prop_sv(needs_look(agent,boolean)). 

moo:subclass(areaPath,door).
moo:subclass(door,item).

moo:subclass(dir,string).

% flags
db_prop_flag(agent(id)).
db_prop_flag(item(id)).
db_prop_flag(region(id)).
db_prop_flag(type(id)).

db_prop_flag(thinking(agent)).
db_prop_flag(deleted(id)).


% multivalued
db_prop_multi(G,AT,[ordered|LIST]):- db_prop_multi(G,LIST),functor_safe(G,_,AT).

% db_prop_multi(named(term,term),[genlpreds(id)]).
db_prop_multi(ofclass(term,type),[alias(mud_isa)]).
db_prop_multi(G,[]):-db_prop_multi(G).

db_prop_multi(verbAsWell(term,action,action)).
db_prop_multi(failure(agent,action)).
db_prop_multi(nameString(term,string)).
db_prop_multi(determinerString(term,string)).
db_prop_multi(descriptionHere(term,string)).
db_prop_multi(description(term,string)).
db_prop_multi(keyword(term,string)).
db_prop_multi(act(term,term,term)).
db_prop_multi(memory(agent,term)).
db_prop_multi(wearing(agent,wearable)).
db_prop_multi(grid(region,int,int,object)).
db_prop_multi(possess(agent,item)).
db_prop_multi(moo:subclass(type,type)).
db_prop_multi(mud_isa(term,type)).

:- moodb:end_transform_moo_preds.

% =================================================================================================
% END world database
% =================================================================================================

