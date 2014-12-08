% eat.pl
% July 1, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
/** <module> 
% This file defines the agents action of eating. 
% Very simple... but kept separate to maintain modularity
%
% This uses the worth/2 predicate from take.pl
% Will (theoretically) only be used in conjuction with take action
%
% It will destroy something, even if it is not food... talk about a garbage disposal. 
*/

% :-swi_module(user). 
:-swi_module(eat, []).

:- include(logicmoo(vworld/moo_header)).

:- register_module_type(command).

subclass(food,eatable).
action_info(eat(eatable),"nourish oneself").

% Eat something held
% Check to make sure it's in the agents possession... 
% if it is, process it's worth, then destroy it
agent_call_command(Agent,eat(SObj)) :-
	possess(Agent,Obj),
        match_object(SObj,Obj),
	do_act_affect(Agent,eat,Obj),
	del(possess(Agent,Obj)),
	call_update_charge(Agent,eat).

update_charge(Agent,eat) :-
	del(charge(Agent,Old)),
	New is Old - 1,
	add(charge(Agent,New)).

:- include(logicmoo(vworld/moo_footer)).


