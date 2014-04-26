:- module(sit, []).
/** <module> Agent Postures there and does nothing
% Agent will loose a bit of charge, but heal a bit of damage
% May 18, 1996
% John Eikenberry
% Douglas Miles 2014

*/
:- include(logicmoo('vworld/vworld_header.pl')).

:- register_module_type(command).

is_posture(sit).
is_posture(stand).
is_posture(lay).
is_posture(kneel).

moo:decl_action(Posture,text("sets and agent's posture to ",Posture)):-is_posture(Posture).

% Sit - do nothing.
moo:agent_call_command(Agent,Sit) :-is_posture(Sit),
        fmt('agent ~w is now ~wing ',[Agent,Sit]),
        padd(Agent,posture(Sit)),
	moo:update_charge(Agent,Sit).

moo:decl_update_charge(Agent,Sit) :- is_posture(Sit), padd(Agent,[charge(-1)]).

:- include(logicmoo('vworld/vworld_footer.pl')).

