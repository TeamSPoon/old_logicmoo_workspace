/** <module>
% logon.pl
%
% This file defines how the agents gain their presence and leave the sim
% Comments below document the basic idea.
%
% Dec 13, 2035
% Douglas Miles
*/
:- module(login, []).

:- include(logicmoo('vworld/moo_header.pl')).

:- register_module_type(command).

moo:decl_action(login(string)).
moo:decl_action(rename(string)).

% logon
moo:agent_text_command(Agent,[login,NewName],Agent,rename(NewName)).
moo:agent_call_command(Agent,rename(NewName)):- padd(Agent,named(NewName)).

% logout
moo:agent_text_command(Agent,[Quit],Agent,prologCall(assert(wants_logout(Agent)))):-lorq(Quit).

lorq(Quit):-member(Quit,[logout,quit]).
moo:decl_action(Quit,"logs out of game (quits)"):-lorq(Quit).

:- include(logicmoo('vworld/moo_footer.pl')).


