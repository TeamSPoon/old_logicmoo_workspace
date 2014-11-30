% This file defines the agents action of drinking. 
% Very simple... but kept separate to maintain modularity
%
% This uses the worth/2 predicate from take.pl
% Will (theoretically) only be used in conjuction with take action
%
% It will destroy something, even if it is not food... talk about a garbage disposal. 
% :-swi_module(user). 
:-swi_module(drink, []).

:- include(logicmoo(vworld/moo_header)).

:- register_module_type(command).

action_info(drink(drinkable),"Drink a Drinkable Item").

% Eat something held
% Check to make sure it's in the agents possession... 
% if it is, process it's worth, then destroy it
agent_call_command(Agent,drink(SObj)) :-
	possess(Agent,Obj),
        match_object(SObj,Obj),
	do_act_affect(Agent,drink,Obj),
	del(possess(Agent,Obj)),
	call_update_charge(Agent,eat).

update_charge(Agent,drink) :- add(charge(Agent,-1)).

:- include(logicmoo(vworld/moo_footer)).
