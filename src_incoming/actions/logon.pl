/** <module>
% logon.pl
%
% This file defines how the agents gain their presence and leave the sim
% Comments below document the basic idea.
%
% Dec 13, 2035
% Douglas Miles
*/
% :- module(user). 
:- module(login, []).

:- include(logicmoo(vworld/moo_header)).

:- register_module_type(command).
:- multifile thlocal:wants_logout/1.

moo:action_info(login(string),"(Re)Login using the name string").
moo:action_info(rename(string),"Rename your player").

% logon
moo:agent_text_command(Agent,[login,NewName],Agent,rename(NewName)).
moo:agent_call_command(Agent,rename(NewName)):- padd(Agent,named(NewName)).

% become
moo:agent_call_command(Agent,become(NewName)):- become_player(Agent,NewName).


% logout
moo:agent_text_command(Agent,[Quit],Agent,prologCall(assert(thlocal:wants_logout(Agent)))):- lorq(Quit).

lorq(Quit):-member(Quit,[logout,quit]).

moo:action_info(Quit,"logs out of game (quits)"):-lorq(Quit).

:- include(logicmoo(vworld/moo_footer)).


