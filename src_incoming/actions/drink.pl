% This file defines the agents action of drinking. 
% Very simple... but kept separate to maintain modularity
%
% This uses the worth/2 predicate from take.pl
% Will (theoretically) only be used in conjuction with take action
%
% It will destroy something, even if it is not food... talk about a garbage disposal. 

:- module(drink, []).

:- include(logicmoo('vworld/vworld_header.pl')).

:- register_module_type(command).

moo:decl_action(drink(item)).


% Eat something held
% Check to make sure it's in the agents possession... 
% if it is, process it's worth, then destroy it
moo:agent_call_command(Agent,drink(Obj)) :-
	findall(Poss,possess(Agent,Poss),Inv),
	member(Obj,Inv),
	worth(Agent,eat,Obj),
	del(possess(Agent,Obj)),
	moo:update_charge(Agent,eat).

moo:decl_update_charge(Agent,drink) :-
	del(charge(Agent,Old)),
	New is Old - 1,
	add(charge(Agent,New)).

:- include(logicmoo('vworld/vworld_footer.pl')).
