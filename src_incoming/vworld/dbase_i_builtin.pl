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

moo:createableType(type).
moo:type(mpred).
moo:type(singleValued).
moo:expand_args(eachOf,subclass(eachOf(multiValued,negationByFailure,singleValued),mpred)).

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

moo:term_anglify(verbFn(isa),[is,a]):-!.
moo:term_anglify(verbFn(F),[is|UL]):-not(string_lower(F,F)),unCamelCase(F,U),atomics_to_string(UL,"_",U).
moo:term_anglify(verbFn(F),[is,F]):-atom_concat(_,'ing',F).
moo:term_anglify(verbFn(F),[F,is]).
% moo:term_anglify(prolog(Term),String):-term_to_atom(Term,Atom),any_to_string(Atom,String).
moo:term_anglify(determinerString(Obj,Text),[np(Obj),is,uses,string(Text),as,a,determiner]).
moo:term_anglify(nameString(Obj,Text),[np(Obj),is,refered,to,as,string(Text)]).
moo:term_anglify(moo:term_anglify(Term,Text),[prolog(Term),is,converted,to,english,using,prolog(Text)]).

moo:type_max_damage(object,500).
moo:type_max_charge(object,120).


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
moo:mpred(CallSig,[ask_module(M),assert_with_pred(add),query_with_pred(call)]):-db_prop_prolog(M,CallSig).


% db_prop_prolog(world,nearby(object,object)).
db_prop_prolog(world,isa(object,type)).
% db_prop_prolog(world,same(id,id)).


moo:argsIsa(somethingIsa(term,list(type))).
argsIsa(somethingDescription(term,list(string))).
argsIsa(objects(type,list(id))).
argsIsa(predicates(list(functor))).
argsIsa(sorts(type,list(type))).


moo:singleValued(repl_writer(agent,term),default_sv(look:default_repl_writer)).

moo:mpred(repl_to_string(agent,term),[singleValued,default_sv(look:default_repl_obj_to_string)]).

moo:mpred(label_type(string,type),[singleValued]).



moo:mpred(look:get_feet(agent,list(spatial)),[]).
moo:mpred(look:get_near(agent,list(spatial)),[ask_module(look)]).
moo:mpred(get_precepts(agent,list(spatial)),[ask_module(look)]).


mpred(description(term,text),[assert_with_pred(assert_description),ask_predicate(query_description)]).



type(T):-moo:subclass(A,B),(T=B;T=A).
moo:type(item).



moo:equivRule(nameString(apath(Region,Dir),Text),pathName(Region,Dir,Text)).
moo:equivRule(description(apath(Region,Dir),Text),pathName(Region,Dir,Text)).

dbase_t(action_help,What,text("command is: ",What)):- holds_t(action_info,What).

moo:expand_args(subclass(eachOf(region,agent,gossup),channel)).

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


moo:type_default_props(_Inst,food,[height(0)]).

moo:specifier_text(Text,pred):- get_mpred_prop(_,arity(Text,_)).

% single valued
moo:subclass(agent,object).
moo:subclass(item,object).


moo:mpred(pathName(region,dir,string)).

moo:singleValued(verbOverride(term,action,action)).
moo:singleValued(atloc(object,xyz(region,int,int,int))).
moo:singleValued(act_turn(agent,int)).
moo:singleValued(armorLevel(possessable,int)).
moo:singleValued(attack(agent,int)).
moo:singleValued(charge(agent,int)).
moo:singleValued(stat_total(agent,int)).
moo:singleValued(chargeCapacity(chargable,int)).
moo:singleValued(chargeRemaining(chargable,int)).
moo:singleValued(damage(agent,int)).
moo:singleValued(defence(agent,int)).
moo:singleValued(facing(agent,dir)).
moo:singleValued(height(agent,int)).
moo:singleValued(inRegion(term,region)).
moo:singleValued(last_command(agent,command)).
moo:singleValued(location_center(region,xyz(region,int,int,int))).
moo:singleValued(movedist(agent,number)).
moo:singleValued(mudBareHandDamage(agent,dice)).
moo:singleValued(mudLevelOf(possessable,int)).
moo:singleValued(mudMaxHitPoints(agent,int)).
moo:singleValued(mudToHitArmorClass0(agent,int)).
moo:singleValued(pathBetween(region,dir,region)).
moo:singleValued(permanence(item,verb,int)).
moo:singleValued(score(object,int)).
moo:singleValued(spawn_rate(propFn(subclass(object)),int)).
moo:singleValued(spd(agent,int)).
moo:singleValued(stm(agent,int)).
moo:singleValued(str(agent,int)).
% moo:singleValued(type_grid(regiontype,int,list(term))).
moo:singleValued(weight(object,int)).

moo:singleValued(needs_look(agent,boolean)). 

moo:subclass(areaPath,door).
moo:subclass(door,item).

moo:subclass(dir,string).

:-debug.

% flags
moo:negationByFailure(agent(id)).
moo:negationByFailure(item(id)).
moo:negationByFailure(region(id)).
moo:negationByFailure(type(id)).

moo:negationByFailure(thinking(agent)).
moo:negationByFailure(deleted(id)).


moo:mpred(description(term,text),[assert_with_pred(assert_description)]).

multiValued(verbAsWell(term,action,action)).
multiValued(failure(agent,action)).
multiValued(nameString(term,string)).
multiValued(determinerString(term,string)).
multiValued(descriptionHere(term,string)).

:-decl_mpred(kwLabel,2).

multiValued(description(term,string)).
multiValued(keyword(term,string)).
multiValued(keyword(term,kwLabel)).
multiValued(act_result(item,verb,effect)).
multiValued(memory(agent,term)).
multiValued(wearing(agent,wearable)).
multiValued(grid(region,int,int,object)).
multiValued(possess(agent,item)).
multiValued(subclass(type,type)).
multiValued(isa(term,type)).


% =================================================================================================
% END world database
% =================================================================================================
moo:action_info(list(term)).
moo:agent_call_command(_Gent,list(Obj)):- term_listing(Obj).

:- end_transform_moo_preds.


