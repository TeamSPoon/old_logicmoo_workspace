% :-swi_module(user). 
:-swi_module(modCreate, []).
/** <module> A command to  ...
% charge(Agent,Chg) = charge (amount of charge agent has)
% health(Agent,Dam) = damage
% cmdsuccess(Agent,Suc) = checks success of last action (actually checks the cmdfailure predicate)
% score(Agent,Scr) = score
% to do this.
% Douglas Miles 2014
*/
:- include(logicmoo(vworld/moo_header)).

% :- register_module_type (mtCommand).



% ====================================================
% item rez (to mudStowing inventory)
% ====================================================

:-export(rez_to_inventory/3).
rez_to_inventory(Agent,NameOrType,NewObj):-   
  must_det_l([
   create_meta(NameOrType,Clz,tItem,NewObj),
   add(isa(NewObj,Clz)),
   padd(Agent,mudStowing(NewObj)),
   padd(NewObj,authorWas(rez_to_inventory(Agent,NameOrType,NewObj))),
   add_missing_instance_defaults(NewObj),
   mudStowing(Agent,NewObj),
   ireq(dbase_t(mudStowing,Agent,NewObj)),
   mudPossess(Agent,NewObj),
   ireq(dbase_t(mudPossess,Agent,NewObj)),
   mudPossess(Agent,NewObj)]).


user:action_info(actRez(isOneOf([tCol,ftTerm])),"Rezes a new 'item' of some NameOrType into mudStowing inventory").
user:agent_call_command(Agent,actRez(NameOrType)):- nonvar(NameOrType),
        must(rez_to_inventory(Agent,NameOrType,NewObj)),
        fmt([rezed,NameOrType,NewObj]).

% ====================================================
% object/col creation
% ====================================================
user:action_info(actCreate(ftListFn(ftTerm)), "Rezes a new 'tSpatialThing' or creates a new 'col' of some NameOrType and if it's an 'item' it will put in mudStowing inventory").

user:agent_call_command(Agent,actCreate(SWhat)):- with_all_dmsg(must_det(create_new_object(Agent,SWhat))).

:-decl_mpred_prolog(authorWas(ftTerm,ftTerm)).
:-decl_mpred_prolog(current_pronoun(tAgentGeneric,ftString,ftTerm)).

:-export(create_new_object/2).

create_new_object(Agent,[tCol,NameOfType|DefaultParams]):-!,create_new_type(Agent,[NameOfType|DefaultParams]).

create_new_object(Agent,[NameOrType|Params]):-
   create_meta(NameOrType,NewType,tSpatialThing,NewObj),
   assert_isa(NewObj,NewType),
   add(subclass(NewType,tItem)),
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
   add(typeProps(NewObj,PropList)).


getPropInfo(_Agent,_NewName,PropsIn,N,[comment(ftText(need,to,actParse,PropsIn,N))]).



% :- include(logicmoo(vworld/moo_footer)).
