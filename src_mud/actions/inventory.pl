% :-swi_module(user). 
:-swi_module(actInventory, [actInventory/2,inventory1/2]).
/** <module> A command to  ...
% Douglas Miles 2014
% inventory(Agt,Inv) = inventory (anything the agent has taken
*/
:- include(logicmoo(vworld/moo_header)).

:- register_module_type(mtCommand).

:-debug.

% ====================================================
% the entire inventory system
% ====================================================
action_info(actInventory(isOptional(tAgentGeneric,isAgentSelf)), "Examine an inventory").
agent_call_command(Agent,actInventory(Who)):- show_kb_preds(Agent,actInventory(Who,value)).

prologListValued(actInventory(tAgentGeneric,ftListFn(tObj))).

% Get only the Inv (inventory)
actInventory(Agent,Percepts) :-  inventory0(Agent,Percepts0),!,flatten_set(Percepts0,Percepts).
inventory0(Agent, Inv) :-
	findall(Poss,inventory1(Agent,Poss),Inv).

inventory1(Agent,Poss):-mudInsideOf(Poss,Agent).

:-decl_mpred_hybrid(mudPossess(tAgentGeneric,tObj)).

mudPossess(Agent,Poss):-inventory1(Agent,Poss).

test_exists(O):- tItem(O).
test_exists(O):- tAgentGeneric(O).
test_exists(O):- tRegion(O).
test_anyInst(O):- tCol(O).
test_anyInst(O):- test_exists(O).

% helps for testings
% :- listing(inventory:_).

:- include(logicmoo(vworld/moo_footer)).
