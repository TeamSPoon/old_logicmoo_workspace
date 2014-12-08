% :-swi_module(user). 
:-swi_module(inventory, [inventory/2,inventory1/2]).
/** <module> A command to  ...
% Douglas Miles 2014
% inventory(Agt,Inv) = inventory (anything the agent has taken
*/
:- include(logicmoo(vworld/moo_header)).

:- register_module_type(command).

:-debug.

% ====================================================
% the entire inventory system
% ====================================================
action_info(inventory(optional(agent,self)), "Examine an inventory").
agent_call_command(Agent,inventory(Who)):- show_kb_preds(Agent,inventory(Who,value)).

listValued(inventory(agent,list(obj))).

% Get only the Inv (inventory)
inventory(Agent,Percepts) :-  inventory0(Agent,Percepts0),!,flatten_set(Percepts0,Percepts).
inventory0(Agent, Inv) :-
	findall(Poss,inventory1(Agent,Poss),Inv).

inventory1(Agent,Poss):-inside_of(Poss,Agent).

:-decl_mpred_hybrid(possess(agent,obj)).

possess(Agent,Poss):-inventory1(Agent,Poss).

test_exists(O):- item(O).
test_exists(O):- agent(O).
test_exists(O):- region(O).
test_anyInst(O):- type(O).
test_anyInst(O):- test_exists(O).

% helps for testings
% :- listing(inventory:_).

:- include(logicmoo(vworld/moo_footer)).

