% :-swi_module(user). 
:-swi_module(actCreate, []).
/** <module> A command to  ...
% charge(Agent,Chg) = charge (amount of charge agent has)
% health(Agent,Dam) = damage
% cmdsuccess(Agent,Suc) = checks success of last action (actually checks the cmdfailure predicate)
% score(Agent,Scr) = score
% to do this.
% Douglas Miles 2014
*/
:- include(logicmoo(vworld/moo_header)).

:- register_module_type(mtCommand).



% ====================================================
% item rez (to mudStowed inventory)
% ====================================================

:-swi_export(rez_to_inventory/3).
rez_to_inventory(Agent,NameOrType,NewName):-   
   create_meta(NameOrType,Clz,tItem,NewName),
   padd(Agent,mudStowed(NewName)),
   add(mudIsa(NewName,Clz)),
   padd(NewName,authorWas(rez_to_inventory(Agent,NameOrType,NewName))),
   add_missing_instance_defaults(NewName).


action_info(actRez(isOneOf([tCol,ftTerm])),"Rezes a new 'item' of some NameOrType into mudStowed inventory").
agent_call_command(Agent,actRez(NameOrType)):- nonvar(NameOrType),rez_to_inventory(Agent,NameOrType,_NewName).

% ====================================================
% object/col creation
% ====================================================
action_info(actCreate(ftListFn(ftTerm)), "Rezes a new 'spatialthing' or creates a new 'col' of some NameOrType and if it's an 'item' it will put in mudStowed inventory").

agent_call_command(Agent,actCreate(SWhat)):- with_all_dmsg(must_det(create_new_object(Agent,SWhat))).

:-decl_mpred_prolog(authorWas(ftTerm,ftTerm)).
:-decl_mpred_prolog(current_pronoun(tAgentGeneric,ftString,ftTerm)).

:-swi_export(create_new_object/2).

create_new_object(Agent,[tCol,NameOfType|DefaultParams]):-!,create_new_type(Agent,[NameOfType|DefaultParams]).

create_new_object(Agent,[NameOrType|Params]):-
   create_meta(NameOrType,NewType,tSpatialthing,NewName),
   assert_isa(NewName,NewType),
   add(mudSubclass(NewType,tItem)),
   padd(NewName,authorWas(create_new_object(Agent,[NameOrType|Params]))),
   padd(Agent,current_pronoun("it",NewName)),   
   getPropInfo(Agent,NewName,Params,2,PropList),!,
   padd(NewName,PropList),
   must((mudIsa(NewName,tItem),padd(Agent,mudStowed(NewName)))),
   add_missing_instance_defaults(NewName).

:-swi_export(create_new_type/2).
create_new_type(Agent,[NewName|DefaultParams]):-
   decl_type(NewName),
   padd(NewName,authorWas(create_new_type(Agent,[NewName|DefaultParams]))),
   padd(Agent,current_pronoun("it",NewName)),
   getPropInfo(Agent,NewName,DefaultParams,2,PropList),!,
   add(default_type_props(NewName,PropList)).


getPropInfo(_Agent,_NewName,PropsIn,N,[comment(ftText(need,to,actParse,PropsIn,N))]).



:- include(logicmoo(vworld/moo_footer)).
