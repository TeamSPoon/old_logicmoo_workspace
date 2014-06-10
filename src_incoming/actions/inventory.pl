:- module(inventory, [inventory/2]).
/** <module> A command to  ...
% Douglas Miles 2014
% inventory(Agt,Inv) = inventory (anything the agent has taken
*/
:- include(logicmoo(vworld/moo_header)).

:- moodb:register_module_type(command).

:-debug.

% ====================================================
% the entire inventory system
% ====================================================
moo:action_help(inventory(optional(agent,self)), "Examine an inventory").
moodb:agent_call_command(Agent,inventory(Who)):- show_kb_preds(Agent,inventory(Who,value)).

% Get only the Inv (inventory)
inventory(Agent,Percepts) :-  inventory0(Agent,Percepts0),!,flatten_dedupe(Percepts0,Percepts).
inventory0(Agent, Inv) :-
	findall(Poss,possess(Agent,Poss),Inv).


test_exists(O):-moo:item(O).
test_exists(O):-kb:agent(O).
test_exists(O):-moo:region(O).
test_anyInst(O):-moo:type(O).
test_anyInst(O):-test_exists(O).

:- listing(inventory:_).

:- include(logicmoo(vworld/moo_footer)).

