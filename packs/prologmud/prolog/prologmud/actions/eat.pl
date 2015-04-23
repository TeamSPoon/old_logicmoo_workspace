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
:-swi_module(modEat, []).

:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).

genls(tFood,tEatAble).
action_info(actEat(tEatAble),"nourish oneself").

% Eat something held
% Check to make sure it's in the agents possession... 
% if it is, process it's worth, then destroy it
user:agent_call_command(Agent,actEat(Obj)) :-
	mudPossess(Agent,Obj),
	must((do_act_affect(Agent,actEat,Obj))),
        dmsg_show(_),
	must((clr(mudPossess(Agent,Obj)))),
        must(not(mudPossess(Agent,Obj))),
	must((call_update_charge(Agent,actEat))).

update_charge(Agent,actEat) :-
	del(mudEnergy(Agent,Old)),
	New is Old - 1,
	add(mudEnergy(Agent,New)).

% :- include(prologmud(mud_footer)).
