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
% padd(Obj,height(ObjHt))  == padd(height,Obj,ObjHt,...) == dyn(QueryForm)
% kretract[all](Obj,height(ObjHt))  == kretract[all](Obj,height,ObjHt) == pretract[all](height,Obj,ObjHt) == del[all](QueryForm)
% keraseall(AnyTerm).
%
%
% Dec 13, 2035
% Douglas Miles
*/

:- decl_mpred(assert_with_pred,2).
:- begin_transform_moo_preds.


:- (do_term_expansions->true;throw(not_term_expansions)).

dyn:createableType(type).
dyn:type(multiValued).
dyn:type(singleValued).
dyn:expand_args(eachOf,subclass(eachOf(multiValued,negationByFailure,singleValued),mpred)).

% =================================================================================================
% BEGIN world English
% =================================================================================================
% :- style_check(-discontiguous).
:-debug.

moo:term_anglify_last(Head,English):-compound(Head),
   functor(Head,F,A),A>1,
   not(ends_with_icase(F,"Fn")),not(starts_with_icase(F,"SKF-")),
   atom_codes(F,[C|_]),code_type(C,lower),
   Head=..[F|ARGS],
   term_anglify_args(Head,F,A,ARGS,singleValued,English).

moo:term_anglify(Head,EnglishO):-
      get_mpred_prop(Head,Info),member(Info,[singleValued,multi(_)]),
      Head=..[F|ARGS],
      term_anglify_args(Head,F,1,ARGS,Info,English),world:fully_expand(English,EnglishO),!.


moo:term_anglify_args(Head,F,A,ARGS,multi(Which),English):- !,replace_nth(ARGS,Which,_OldVar,NewVar,NEWARGS),trace,!,
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

moo:term_anglify(verbFn(isa),[is,a]):-!.
moo:term_anglify(verbFn(F),[is|UL]):-not(string_lower(F,F)),unCamelCase(F,U),atomics_to_string(UL,"_",U).
moo:term_anglify(verbFn(F),[is,F]):-atom_concat(_,'ing',F).
moo:term_anglify(verbFn(F),[F,is]).
% moo:term_anglify(prolog(Term),String):-term_to_atom(Term,Atom),any_to_string(Atom,String).
moo:term_anglify(determinerString(Obj,Text),[np(Obj),is,uses,string(Text),as,a,determiner]).
moo:term_anglify(nameString(Obj,Text),[np(Obj),is,refered,to,as,string(Text)]).
moo:term_anglify(term_anglify(Term,Text),[prolog(Term),is,converted,to,english,using,prolog(Text)]).

dyn:singleValued(type_max_damage(type,int)).

dyn:multiValued(verb_alias(string,string)).

dyn:multiValued(label_type_props(string,type,list(term(property)))).

dyn:multiValued(type_grid(type,int,term)).

dyn:multiValued(action_rules(agent,verb,term(object),term(list(term(property))))).

dyn:type_max_damage(object,500).
dyn:type_max_charge(object,120).


add_arg_parts_of_speech(_F,_N,[],[]).
add_arg_parts_of_speech(F,N,[A|ARGS0],[ARG|ARGS]):-argIsa_call_or_undressed(F,N,A,ARG),N1 is N+1, add_arg_parts_of_speech(F,N1,ARGS0,ARGS).

argIsa_call_or_undressed(F,N,Obj,fN(Obj,Type)):-argIsa_call_0(F,N,Type),!.
argIsa_call_or_undressed(_F,_N,Obj,Obj).

verb_after_arg(_,_,1).


:- style_check(+discontiguous).

:- decl_mpred(default_sv, 2).
:- decl_mpred(ask_module, 2).

% =================================================================================================
% BEGIN world database
% =================================================================================================

:- begin_transform_moo_preds.

% forwardRule(inRegion(O,Region),atloc(O,LOC)):-

% :-trace.

dbase_t(inRegion,O,Region):-atloc(O,LOC),world:locationToRegion(LOC,Region).
dbase_t(inRegion,apath(Region,Dir),Region):- holds_t(pathBetween,Region,Dir,_To).

db_prop_format(apath(region,dir),areaPath).
db_prop_format(dice(int,int,int),int).
db_resultIsa(apath,areaPath).


% prolog code
equivRule(multiValued(CallSig,[assert_with_pred(hooked_asserta),query_with_pred(call)]),prologPredicate(CallSig)).


dyn:ask_module(nearby(object,object),world).
% dyn:ask_module(mud_isa(object,type),world).
% db_prop_prolog(world,mud_isa(object,type)).
% db_prop_prolog(world,same(id,id)).


dyn:argsIsa(somethingIsa(term,list(type))).
argsIsa(somethingDescription(term,list(string))).
argsIsa(objects(type,list(id))).
argsIsa(predicates(list(functor))).
argsIsa(sorts(type,list(type))).

argsIsa(default_sv(singleValued,term)).

% live another day to fight (meaning repl_to_string/1 for now is in prolog)
%dyn:singleValued(repl_writer(agent,term),default_sv(look:default_repl_writer)).
%dyn:singleValued(repl_to_string(agent,term),[singleValued,default_sv(look:default_repl_obj_to_string)]).

dyn:multiValued(label_type(string,type),[singleValued]).


/*
dyn:listValued(look:get_feet(agent,list(spatial)),[]).
dyn:listValued(look:get_near(agent,list(spatial)),[ask_module(look)]).
*/
dyn:listValued(get_precepts(agent,list(spatial)),[ask_module(look)]).
dyn:argsIsa(mud_test(term,prolog)).

dyn:multiValued(assert_with_pred(multiValued,term)).
dyn:negationByFailure(multi(multiValued,int)).
dyn:multiValued(ask_predicate(multiValued,term)).
dyn:multiValued(equivRule(term,term)).

dyn:subclass(text,formattype).

dyn:argsIsa(action_help(verb,text)).

argsIsa(agent_text_command(agent,text,agent,verb)).

dyn:multiValued(description(term,text),[assert_with_pred(assert_description),ask_predicate(query_description)]).



dyn:type(item).

dyn:equivRule(nameString(apath(Region,Dir),Text),pathName(Region,Dir,Text)).
dyn:equivRule(description(apath(Region,Dir),Text),pathName(Region,Dir,Text)).

% dbase_t(action_help,What,text("command is: ",What)):- holds_t(action_info,What).

dyn:expand_args(eachOf,subclass(eachOf(region,agent,gossup),channel)).

dyn:subclass(agent,spatialthing).
dyn:subclass(region,spatialthing).
dyn:subclass(object,spatialthing).
dyn:subclass(item,spatialthing).


dyn:subclass(drinkable,item).
dyn:subclass(possessable,item).
dyn:subclass(useable,item).
dyn:subclass(eatable,item).
dyn:subclass(chargeable,item).
dyn:subclass(wearable,item).


moo:ft_info(atom,atom(self)).
moo:ft_info(apath(region,dir),formatted).
moo:ft_info(string,string(self)).
moo:ft_info(number,number(self)).
moo:ft_info(type,isa(self,type)).
moo:ft_info(dir,any_to_dir(self,_)).
moo:ft_info(dice(int,int,int),formatted).
moo:ft_info(xyz(region,int,int,int),formatted).
moo:ft_info(list(type),formatted).
moo:ft_info(term,nonvar(self)).
moo:ft_info(id,nonvar(self)).
moo:ft_info(prolog,true).
moo:ft_info(rest,true).
moo:ft_info(var,var(self)).
moo:ft_info(action(prolog),formatted).
moo:ft_info(formattype,formatted).

dyn:subft(var,prolog).
dyn:subft(term,prolog).
dyn:subft(atom,term).
dyn:subft(string,term).
% dyn:subft(number,term).
dyn:subft(id,term).

dyn:subft(int,integer).
dyn:subft(integer,number).
dyn:subft(dice,int).

dyn:formattype(FormatType):-dyn:subclass(FormatType,formattype).

moo:term_specifier_text(Text,pred):- get_mpred_prop(_,arity(Text,_)).

% single valued
dyn:subclass(agent,object).
dyn:subclass(item,object).


dyn:multiValued(pathName(region,dir,string)).
dyn:multiValued(verbOverride(term,action,action)).

dyn:singleValued(atloc(object,xyz(region,int,int,int))).
dyn:singleValued(act_turn(agent,int)).
dyn:singleValued(armorLevel(possessable,int)).
dyn:singleValued(attack(agent,int)).
dyn:singleValued(charge(agent,int)).
dyn:singleValued(stat_total(agent,int)).
dyn:singleValued(chargeCapacity(chargable,int)).
dyn:singleValued(chargeRemaining(chargable,int)).
dyn:singleValued(damage(agent,int)).
dyn:singleValued(defence(agent,int)).
dyn:singleValued(facing(agent,dir)).
dyn:singleValued(inRegion(term,region)).
dyn:singleValued(last_command(agent,command)).
dyn:singleValued(location_center(region,xyz(region,int,int,int))).
dyn:singleValued(movedist(agent,number)).
dyn:singleValued(mudBareHandDamage(agent,dice)).
dyn:singleValued(mudLevelOf(possessable,int)).
dyn:singleValued(mudMaxHitPoints(agent,int)).
dyn:singleValued(mudToHitArmorClass0(agent,int)).
dyn:singleValued(pathBetween(region,dir,region)).
dyn:singleValued(permanence(item,verb,int)).
dyn:singleValued(score(object,int)).
dyn:singleValued(spawn_rate(propFn(subclass(object)),int)).
dyn:singleValued(spd(agent,int)).
dyn:singleValued(stm(agent,int)).
dyn:singleValued(str(agent,int)).
dyn:singleValued(type_grid(regiontype,int,list(term))).
dyn:singleValued(weight(object,int)).

dyn:multiValued(comment(term,string)).

dyn:singleValued(height(agent,int)).
moo:type_default_props(self,food,[height(0)]).

:- must((argIsa_call_0(comment,2,W), W\=term)).

:-decl_mpred(needs_look/2).

dyn:negationByFailure(needs_look(agent,boolean)). 

dyn:subclass(areaPath,door).
dyn:subclass(door,item).

dyn:subclass(dir,string).
dyn:subclass(string,text).
dyn:subclass(text,term).


:-debug.

% flags
dyn:negationByFailure(agent(id)).
dyn:negationByFailure(item(id)).
dyn:negationByFailure(region(id)).
dyn:negationByFailure(type(id)).

dyn:negationByFailure(thinking(agent)).
dyn:negationByFailure(deleted(id)).


dyn:multiValued(description(term,text),[assert_with_pred(assert_description)]).

multiValued(verbAsWell(term,action,action)).
multiValued(failure(agent,action)).
multiValued(nameString(term,string)).
multiValued(determinerString(term,string)).
multiValued(descriptionHere(term,string)).

:-decl_mpred(kwLabel,2).

multiValued(description(term,string)).
multiValued(keyword(term,string)).
multiValued(kwLabel(term,string)).
multiValued(act_affect(item,verb,term(effect))).
multiValued(memory(agent,term)).
multiValued(wearing(agent,wearable)).
multiValued(grid(region,int,int,object)).
multiValued(possess(agent,item)).
multiValued(subclass(type,type)).
multiValued(isa(term,type)).

dyn:listValued(directions(term,list(term))).

dyn:multiValued(ask_module(multiValued,atom)).

dyn:argsIsa(agent_call_command(agent,term(verb))).


% =================================================================================================
% END world database
% =================================================================================================

:- end_transform_moo_preds.


