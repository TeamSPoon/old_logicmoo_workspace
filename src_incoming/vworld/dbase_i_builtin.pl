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

:- decl_mpred(term_anglify/2).
:- decl_mpred(assert_with_pred, 2).
:- begin_transform_moo_preds.


:- (do_term_expansions->true;throw(not_term_expansions)).

dyn:createableType(type).
dyn:type(mpred).
dyn:type(singleValued).
dyn:expand_args(eachOf,subclass(eachOf(multiValued,negationByFailure,singleValued),mpred)).

% =================================================================================================
% BEGIN world English
% =================================================================================================
% :- style_check(-discontiguous).
:-debug.

dyn:term_anglify_last(Head,English):-compound(Head),
   functor(Head,F,A),A>1,
   not(ends_with_icase(F,"Fn")),not(starts_with_icase(F,"SKF-")),
   atom_codes(F,[C|_]),code_type(C,lower),
   Head=..[F|ARGS],
   term_anglify_args(Head,F,A,ARGS,singleValued,English).

dyn:term_anglify(Head,EnglishO):-
      get_mpred_prop(Head,Info),member(Info,[singleValued,multi(_)]),
      Head=..[F|ARGS],
      term_anglify_args(Head,F,1,ARGS,Info,English),world:fully_expand(English,EnglishO),!.


term_anglify_args(Head,F,A,ARGS,multi(Which),English):- !,replace_nth(ARGS,Which,_OldVar,NewVar,NEWARGS),trace,!,
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

dyn:term_anglify(verbFn(isa),[is,a]):-!.
dyn:term_anglify(verbFn(F),[is|UL]):-not(string_lower(F,F)),unCamelCase(F,U),atomics_to_string(UL,"_",U).
dyn:term_anglify(verbFn(F),[is,F]):-atom_concat(_,'ing',F).
dyn:term_anglify(verbFn(F),[F,is]).
% dyn:term_anglify(prolog(Term),String):-term_to_atom(Term,Atom),any_to_string(Atom,String).
dyn:term_anglify(determinerString(Obj,Text),[np(Obj),is,uses,string(Text),as,a,determiner]).
dyn:term_anglify(nameString(Obj,Text),[np(Obj),is,refered,to,as,string(Text)]).
dyn:term_anglify(dyn:term_anglify(Term,Text),[prolog(Term),is,converted,to,english,using,prolog(Text)]).

mpred(type_max_damage(type,int)).

mpred(verb_alias(string,string)).

mpred(label_type_props(string,type,list(props))).

mpred(type_grid(type,int,term)).

mpred(action_rules(term(agent),term(verb),term(object),term(list(props)))).

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

dbase_t(inRegion,O,Region):-atloc(O,LOC),world:locationToRegion(LOC,Region).
dbase_t(inRegion,apath(Region,Dir),Region):- holds_t(pathBetween,Region,Dir,_To).

db_prop_format(apath(region,dir),areaPath).
db_prop_format(dice(int,int,int),int).
db_resultIsa(apath,areaPath).


% prolog code
dyn:mpred(CallSig,[ask_module(M),assert_with_pred(add),query_with_pred(call)]):-db_prop_prolog(M,CallSig).


% db_prop_prolog(world,nearby(object,object)).
db_prop_prolog(world,mud_isa(object,type)).
% db_prop_prolog(world,same(id,id)).


dyn:argsIsa(somethingIsa(term,list(type))).
argsIsa(somethingDescription(term,list(string))).
argsIsa(objects(type,list(id))).
argsIsa(predicates(list(functor))).
argsIsa(sorts(type,list(type))).

% live another day to fight (meaning repl_to_string/1 for now is in prolog)
% dyn:singleValued(repl_writer(agent,term),default_sv(look:default_repl_writer)).
% dyn:mpred(repl_to_string(agent,term),[singleValued,default_sv(look:default_repl_obj_to_string)]).

dyn:mpred(label_type(string,type),[singleValued]).


/*
dyn:mpred(look:get_feet(agent,list(spatial)),[]).
dyn:mpred(look:get_near(agent,list(spatial)),[ask_module(look)]).
dyn:mpred(get_precepts(agent,list(spatial)),[ask_module(look)]).
*/
mpred(mud_test(term,prolog)).

mpred(assert_with_pred(mpred,term)).
mpred(multi(mpred,int)).
mpred(ask_predicate(mpred,term)).
mpred(equivRule(term,term)).

dyn:subclass(text,formattype).

mpred(action_help(verb,text)).

argsIsa(agent_text_command(agent,text,agent,verb)).

mpred(description(term,text),[assert_with_pred(assert_description),ask_predicate(query_description)]).



type(T):-dyn:subclass(A,B),(T=B;T=A).
dyn:type(item).



dyn:equivRule(nameString(apath(Region,Dir),Text),pathName(Region,Dir,Text)).
dyn:equivRule(description(apath(Region,Dir),Text),pathName(Region,Dir,Text)).

% dbase_t(action_help,What,text("command is: ",What)):- holds_t(action_info,What).

dyn:expand_args(subclass(eachOf(region,agent,gossup),channel)).

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


dyn:ft_info(atom,atom(self)).
dyn:ft_info(apath(region,dir),formatted).
dyn:ft_info(string,string(self)).
dyn:ft_info(number,number(self)).
dyn:ft_info(type,isa(self,type)).
dyn:ft_info(dir,any_to_dir(self,_)).
dyn:ft_info(dice(int,int,int),formatted).
dyn:ft_info(xyz(region,int,int,int),formatted).
dyn:ft_info(list(type),formatted).
dyn:ft_info(term,nonvar(self)).
dyn:ft_info(id,nonvar(self)).
dyn:ft_info(prolog,true).
dyn:ft_info(rest,true).
dyn:ft_info(var,var(self)).
dyn:ft_info(action(prolog),formatted).

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
dyn:formattype(FormatType):-dbase:holds_t(isa, FormatType, formattype).

dyn:type_default_props(_Inst,food,[height(0)]).

dyn:specifier_text(Text,pred):- get_mpred_prop(_,arity(Text,_)).

% single valued
dyn:subclass(agent,object).
dyn:subclass(item,object).


dyn:mpred(pathName(region,dir,string)).

dyn:mpred(erbOverride(term,action,action)).
dyn:singleValued(verbOverride(term,action,action)).
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
dyn:singleValued(height(agent,int)).
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

mpred(comment(term,string)).

:-decl_mpred(needs_look/2).

dyn:singleValued(needs_look(agent,boolean)). 

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


dyn:mpred(description(term,text),[assert_with_pred(assert_description)]).

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

mpred(directions(term,list(term))).


dyn:action_info(list(term)).

moo:agent_call_command(_Gent,list(Obj)):- term_listing(Obj).

dyn:mpred(ask_module(mpred,atom)).
dyn:argsIsa(agent_call_command(agent,term(verb))).

% =================================================================================================
% END world database
% =================================================================================================

:- end_transform_moo_preds.


