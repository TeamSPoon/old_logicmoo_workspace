% drop.pl
% May 18, 1996
% John Eikenberry
% Dec 13, 2035
% Douglas Miles
%
/** <module> 
% This file defines the basic drop predicate
% 
*/
% :-swi_module(user). 
:-swi_module(modDrop, []).

:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).

% orderedBy(tDropAble,tNearestReachableItem).

action_info(actDrop(isOneOf([tDropAble,tNearestReachableItem,tObj,ftID])),"Drop an item").

% Drop something
user:agent_call_command(Agent,actDrop(Obj)) :-
	mudPossess(Agent,Obj),
        mudAtLoc(Agent,LOC),
        clr(mudPossess(Agent,Obj)),
        must(not((mudPossess(Agent,Obj)))),
        add(mudAtLoc(Obj,LOC)),
	must(call_update_charge(Agent,actDrop)).

%Nothing to drop
user:agent_call_command(Agent,actDrop(_)) :-
	call_update_charge(Agent,actDrop),
	(add_cmdfailure(Agent,actDrop)).

% Record keeping
update_charge(Agent,actDrop) :- add(mudEnergy(Agent,-1)).

% user:agent_text_command(Agent,[actDrop,X],Agent,actDrop(X)).

%:-must_det(show_call(get_agent_text_command(agent1,[drop,item1],_R,_CMD))).

% :- include(prologmud(mud_footer)).
