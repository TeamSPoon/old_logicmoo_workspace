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

:- decl_mpred(assert_with_pred,2).
:- begin_transform_moo_preds.

moo:subclass(createableType,type).

:- (do_term_expansions->true;throw(not_term_expansions)).

moo:createableType(type).
moo:type(multiValued).
moo:type(singleValued).
moo:expand_args(eachOf,subclass(eachOf(multiValued,negationByFailure,argsIsa,singleValued),mpred)).

% =================================================================================================
% BEGIN world English
% =================================================================================================
% :- style_check(-discontiguous).
:-debug.

%:- decl_dynamic_prolog(moo:term_anglify_args/6).

moo:term_anglify_last(Head,English):-compound(Head),
   functor(Head,F,A),A>1,
   not(ends_with_icase(F,"Fn")),not(starts_with_icase(F,"SKF-")),
   atom_codes(F,[C|_]),code_type(C,lower),
   Head=..[F|ARGS],
   term_anglify_args(Head,F,A,ARGS,singleValued,English).

moo:term_anglify(Head,EnglishO):- compound(Head),
   Head=..[F|ARGS],mpred_prop(F,Info),
   member(Info,[singleValued,multi(_)]),   
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
moo:term_anglify(nameStrings(Obj,Text),[np(Obj),is,refered,to,as,string(Text)]).
moo:term_anglify(term_anglify(Term,Text),[prolog(Term),is,converted,to,english,using,prolog(Text)]).

moo:singleValued(type_max_damage(type,int)).

moo:multiValued(verb_alias(string,string)).

moo:multiValued(label_type_props(string,type,list(term(property)))).

moo:multiValued(type_grid(type,int,term)).

moo:multiValued(action_rules(agent,verb,term(object),term(list(term(property))))).

moo:type_max_damage(object,500).
moo:type_max_charge(object,120).


add_arg_parts_of_speech(_F,_N,[],[]).
add_arg_parts_of_speech(F,N,[A|ARGS0],[ARG|ARGS]):-argIsa_call_or_undressed(F,N,A,ARG),N1 is N+1, add_arg_parts_of_speech(F,N1,ARGS0,ARGS).

argIsa_call_or_undressed(F,N,Obj,fN(Obj,Type)):- argIsa_call_0(F,N,Type),!.
argIsa_call_or_undressed(_F,_N,Obj,Obj).

verb_after_arg(_,_,1).


:- style_check(+discontiguous).
:- style_check(-discontiguous).

:- decl_mpred(default_sv, 2).
:- decl_mpred(ask_module, 2).


% =================================================================================================
% BEGIN world database
% =================================================================================================

:- begin_transform_moo_preds.

% forwardRule(inRegion(O,Region),atloc(O,LOC)):-

% :-trace.

:-decl_mpred(pathBetween,3).
:-decl_mpred(atloc,2).
:-decl_mpred(damage,2).


inRegion(apath(Region,Dir),Region):- is_asserted(pathBetween(Region,Dir,_To)).
inRegion(O,Region):-req(atloc(O,LOC)),world:locationToRegion(LOC,Region).
inRegion(O,Region):-is_asserted(inRegion(O,Region)).

db_prop_format(apath(region,dir),areaPath).
db_prop_format(dice(int,int,int),int).
db_resultIsa(apath,areaPath).


% prolog code
equivRule(multiValued(CallSig,[assert_with_pred(hooked_asserta),query_with_pred(call)]),prologPredicate(CallSig)).


moo:ask_module(nearby(object,object),world).
%  moo:ask_module(isa(object,type),world).
% db_prop_prolog(world,isa(object,type)).
% db_prop_prolog(world,same(id,id)).


moo:argsIsa(somethingIsa(term,list(type))).
moo:argsIsa(somethingDescription(term,list(string))).
moo:argsIsa(objects(type,list(id))).
moo:argsIsa(predicates(list(functor))).
moo:argsIsa(sorts(type,list(type))).

moo:argsIsa(default_sv(singleValued,term)).

% live another day to fight (meaning repl_to_string/1 for now is in prolog)
% moo:singleValued(repl_writer(agent,term),default_sv(look:default_repl_writer)).
% moo:singleValued(repl_to_string(agent,term),[singleValued,default_sv(look:default_repl_obj_to_string)]).

moo:multiValued(moo:label_type(string,type),[singleValued]).


/*
moo:listValued(look:get_feet(agent,list(spatial)),[]).
moo:listValued(look:get_near(agent,list(spatial)),[ask_module(look)]).
*/
moo:listValued(get_precepts(agent,list(spatial)),[ask_module(look)]).
moo:argsIsa(mud_test(term,prolog)).

moo:multiValued(assert_with_pred(multiValued,term)).
moo:negationByFailure(multi(multiValued,int)).
moo:multiValued(ask_predicate(multiValued,term)).
moo:multiValued(equivRule(term,term)).

moo:subclass(text,formattype).

moo:argsIsa(action_info(verb,text)).

moo:argsIsa(agent_text_command(agent,text,agent,goal)).

moo:multiValued(description(term,text),[assert_with_pred(assert_description),ask_predicate(query_description)]).



moo:type(item).

moo:equivRule(nameStrings(apath(Region,Dir),Text),pathName(Region,Dir,Text)).
moo:equivRule(description(apath(Region,Dir),Text),pathName(Region,Dir,Text)).

% dbase_t(action_info,What,text("command is: ",What)):- holds_t(action_info,What).

moo:expand_args(eachOf,subclass(eachOf(region,agent,gossup),channel)).

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

moo:subft(var,prolog).
moo:subft(term,prolog).
moo:subft(atom,term).
moo:subft(string,term).
%  moo:subft(number,term).
moo:subft(id,term).

moo:subft(int,integer).
moo:subft(integer,number).
moo:subft(dice,int).

% moo:formattype(FormatType):-moo:subclass(FormatType,formattype).

moo:term_specifier_text(Text,pred):- mpred_prop(Text,arity(_)).

% single valued
moo:subclass(agent,object).
moo:subclass(item,object).


moo:multiValued(pathName(region,dir,string)).
moo:multiValued(verbOverride(term,action,action)).

moo:singleValued(atloc(object,xyz(region,int,int,int))).
moo:singleValued(agent_turnnum(agent,int)).
moo:singleValued(armorLevel(possessable,int)).
moo:singleValued(attack(agent,int)).
moo:singleValued(charge(agent,int)).
moo:singleValued(stat_total(agent,int)).
moo:singleValued(chargeCapacity(chargable,int)).
moo:singleValued(chargeRemaining(chargable,int)).
moo:singleValued(damage(agent,int)).
moo:singleValued(defence(agent,int)).
moo:singleValued(facing(agent,dir)).
moo:singleValued(inRegion(term,region)).
moo:singleValued(last_command(agent,command)).
moo:singleValued(location_center(region,xyz(region,int,int,int))).
moo:singleValued(movedist(agent,number)).
moo:singleValued(mudBareHandDamage(agent,dice)).
moo:singleValued(mudLevelOf(possessable,int)).
moo:singleValued(mudMaxHitPoints(agent,int)).
moo:singleValued(mudToHitArmorClass0(agent,int)).
moo:singleValued(permanence(item,verb,int)).
moo:singleValued(score(object,int)).
moo:singleValued(spawn_rate(propFn(subclass(object)),int)).
moo:singleValued(spd(agent,int)).
moo:singleValued(stm(agent,int)).
moo:singleValued(str(agent,int)).
moo:singleValued(type_grid(regiontype,int,list(term))).
moo:singleValued(weight(object,int)).

moo:multiValued(comment(term,string)).
moo:multiValued(pathBetween(region,dir,region)).

moo:singleValued(height(agent,int)).
moo:default_type_props(self,food,[height(0)]).

% :- must((argIsa_call_0(comment,2,W), W\=term)).

:-decl_mpred(needs_look,2).

moo:negationByFailure(needs_look(agent,boolean)). 

moo:subclass(areaPath,door).
moo:subclass(door,item).

moo:subft(dir,string).
moo:subft(string,text).
moo:subft(text,term).


:-debug.

% flags
moo:negationByFailure(agent(id)).
moo:negationByFailure(item(id)).
moo:negationByFailure(region(id)).
moo:negationByFailure(type(id)).

moo:negationByFailure(thinking(agent)).
moo:negationByFailure(deleted(id)).


moo:multiValued(description(term,text),[assert_with_pred(assert_description)]).

multiValued(verbAsWell(term,action,action)).
multiValued(failure(agent,action)).
multiValued(nameStrings(term,string)).
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

singleValued(type_max_charge(type,int)).
singleValued(max_charge(term,int)).
singleValued(type_max_damage(type,int)).
singleValued(max_damage(term,int)).

moo:listValued(directions(term,list(term))).

moo:multiValued(ask_module(multiValued,atom)).

moo:argsIsa(agent_call_command(agent,term(verb))).


% =================================================================================================
% END world database
% =================================================================================================

:- end_transform_moo_preds.


