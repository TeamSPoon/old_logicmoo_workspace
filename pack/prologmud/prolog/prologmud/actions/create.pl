% :-swi_module(user). 
:-module(modCreate, []).
/** <module> A command to  ...
% charge(Agent,Chg) = charge (amount of charge agent has)
% health(Agent,Dam) = damage
% mudLastCmdSuccess(Agent,Action,Suc) = checks success of last action (actually checks the cmdfailure predicate)
% score(Agent,Scr) = score
% to do this.
% Douglas Miles 2014
*/
:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).



% ====================================================
% item rez (to mudStowing inventory)
% ====================================================

:-export(rez_to_inventory/3).
rez_to_inventory(Agent,NameOrType,NewObj):-
  gensym('_rez',SS),
  must_det_l([
  w_tl(lmconf:current_source_suffix(SS),show_call(createByNameMangle(NameOrType,NewObj,Clz))),
   padd(NewObj,authorWas(rez_to_inventory(Agent,NameOrType,NewObj,Clz))),
   ain(genls(Clz,tItem)),
   padd(Agent,mudStowing(NewObj)),
   add_missing_instance_defaults(NewObj),
   mudStowing(Agent,NewObj),
   ireq(t(mudStowing,Agent,NewObj)),
   mudPossess(Agent,NewObj),
   ireq(t(mudPossess,Agent,NewObj)),
   mudPossess(Agent,NewObj)]).


action_info(actRez(isOneOf([tCol,ftID,ftTerm])),"Rezes a new subclass of 'item' or clone of tObj of some NameOrType into mudStowing inventory").

agent_call_command(Agent,actRez(NameOrType)):- nonvar(NameOrType),
        must(rez_to_inventory(Agent,NameOrType,NewObj)),
        fmt([rezed,NameOrType,NewObj]).

% ====================================================
% object/col creation
% ====================================================
action_info(actCreate(ftListFn(ftTerm)), "Rezes a new 'tSpatialThing' or creates a new 'col' of some NameOrType and if it's an 'item' it will put in mudStowing inventory").

agent_call_command(Agent,actCreate(SWhat)):- with_all_dmsg(must_det(create_new_object(Agent,SWhat))).

:-decl_mpred_prolog(authorWas(ftTerm,ftTerm)).
:-decl_mpred_prolog(current_pronoun(tAgent,ftString,ftTerm)).

:-export(create_new_object/2).

create_new_object(Agent,[tCol,NameOfType|DefaultParams]):-!,create_new_type(Agent,[NameOfType|DefaultParams]).

create_new_object(Agent,[NameOrType|Params]):-
    create_meta(NameOrType,NewType,tSpatialThing,NewObj),
   assert_isa(NewObj,NewType),
   ain(genls(NewType,tItem)),
   padd(NewObj,authorWas(create_new_object(Agent,[NameOrType|Params]))),
   padd(Agent,current_pronoun("it",NewObj)),   
   getPropInfo(Agent,NewObj,Params,2,PropList),!,
   padd(NewObj,PropList),
   must((isa(NewObj,tItem),padd(Agent,mudStowing(NewObj)))),
   add_missing_instance_defaults(NewObj).

:-export(create_new_type/2).
create_new_type(Agent,[NewObj|DefaultParams]):-
   decl_type(NewObj),
   padd(NewObj,authorWas(create_new_type(Agent,[NewObj|DefaultParams]))),
   padd(Agent,current_pronoun("it",NewObj)),
   getPropInfo(Agent,NewObj,DefaultParams,2,PropList),!,
   ain(typeProps(NewObj,PropList)).


getPropInfo(_Agent,_NewName,PropsIn,N,[mudDescription(ftText(need,to,actParse,PropsIn,N))]).



:- include(prologmud(mud_footer)).
