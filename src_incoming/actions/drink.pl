% This file defines the agents action of drinking. 
% Very simple... but kept separate to maintain modularity
%
% This uses the worth/2 predicate from take.pl
% Will (theoretically) only be used in conjuction with take action
%
% It will destroy something, even if it is not food... talk about a garbage disposal. 
% :- module(user). 
:- module(drink, []).

:- include(logicmoo(vworld/moo_header)).

:- register_module_type(command).

moo:action_info(drink(drinkable),"Drink a Drinkable Item").

% Eat something held
% Check to make sure it's in the agents possession... 
% if it is, process it's worth, then destroy it
moo:agent_call_command(Agent,drink(SObj)) :-
	possess(Agent,Obj),
        object_match(SObj,Obj),
	worth(Agent,drink,Obj),
	del(possess(Agent,Obj)),
	call_update_charge(Agent,eat).

moo:update_charge(Agent,drink) :-
	del(charge(Agent,Old)),
	New is Old - 1,
	add(charge(Agent,New)).

:- include(logicmoo(vworld/moo_footer)).
