% :- module(user). 
:- module(sit, []).
/** <module> Agent Postures there and does nothing
% Agent will loose a bit of charge, but heal a bit of damage
% May 18, 1996
% John Eikenberry
% Douglas Miles 2014

*/
:- include(logicmoo(vworld/moo_header)).

:- moo:register_module_type(command).

:-decl_mpred(moo:posture/1).

subclass(command,action).

moo:posture(sit).
moo:posture(stand).
moo:posture(lay).
moo:posture(kneel).

moo:type(posture).

moo_posture(P):-isa(P,posture).

:-decl_mpred(singleValued(stance(agent,posture))).

moo:agent_text_command(Agent,[Sit],Agent,Sit):- moo_posture(Sit).
moo:action_info(Posture,text("sets and agent's stance to ",Posture)):-moo_posture(Posture).

% Sit and do nothing.
moo:agent_call_command(Agent,Sit):- nonvar(Sit),moo_posture(Sit),
        fmt('agent ~w is now ~wing ',[Agent,Sit]),
        padd(Agent,stance(Sit)),
	call_update_charge(Agent,Sit).

moo:update_charge(Agent,Sit) :- moo_posture(Sit), padd(Agent,[charge(-1)]).

:- include(logicmoo(vworld/moo_footer)).

