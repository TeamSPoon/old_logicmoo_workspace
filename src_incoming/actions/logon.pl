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

:- include(logicmoo('vworld/vworld_header.pl')).

:- register_module_type(command).

moo:decl_action(login(string)).
moo:decl_action(rename(string)).

% logon
moo:agent_text_command(Agent,[login,NewName],Agent,rename(NewName)).
moo:agent_call_command(Agent,rename(NewName)):- padd(Agent,named(NewName)).

% logout
moo:agent_text_command(Agent,[logout],Agent,prologCall(assert(wants_logout(Agent)))).


:- include(logicmoo('vworld/vworld_footer.pl')).


