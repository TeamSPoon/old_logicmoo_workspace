:- module(inventory, [inventory/2]).
/** <module> A command to  ...
% Douglas Miles 2014
% inventory(Agt,Inv) = inventory (anything the agent has taken
*/
:- include(logicmoo('vworld/moo_header.pl')).

:- register_module_type(command).


% ====================================================
% the entire inventory system
% ====================================================
moo:decl_action(inventory(optional(agent,self)), "Examine an inventory").
moo:agent_call_command(Agent,inventory(Who)):- show_kb_preds(Agent,inventory(Who,value)).

% Get only the Inv (inventory)
inventory(Agent,Percepts) :-  inventory0(Agent,Percepts0),!,flatten_dedupe(Percepts0,Percepts).
inventory0(Agent, Inv) :-
	findall(Poss,possess(Agent,Poss),Inv).


:- include(logicmoo('vworld/moo_footer.pl')).

