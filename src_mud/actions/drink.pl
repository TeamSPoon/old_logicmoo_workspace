% This file defines the agents action of drinking. 
% Very simple... but kept separate to maintain modularity
%
% This uses the worth/2 predicate from take.pl
% Will (theoretically) only be used in conjuction with take action
%
% It will destroy something, even if it is not food... talk about a garbage disposal. 
% :-swi_module(user). 
:-swi_module(modDrink, []).

:- include(logicmoo(vworld/moo_header)).

:- register_module_type(mtCommand).

user:action_info(actDrink(tDrinkable),"Drink a Drinkable Item").

% Drink something held
% Check to make sure it's in the agents possession... 
% if it is, process it's worth, then destroy it
user:agent_call_command(Agent,actDrink(Obj)) :-
	mudPossess(Agent,Obj),
	do_act_affect(Agent,actDrink,Obj),
	clr(mudPossess(Agent,Obj)),
	call_update_charge(Agent,actDrink).

update_charge(Agent,actDrink) :- add(mudEnergy(Agent,-1)).

:- include(logicmoo(vworld/moo_footer)).
