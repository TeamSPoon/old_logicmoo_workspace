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
moo:agent_text_command(Agent,[login,Other],Agent,rename(Other)).
moo:agent_call_command(Agent,rename(Other)):- padd(Agent,named(Other)).

% become
moo:action_info(become(optional(agent,random(agent))),"Assume the role of an agent").
moo:agent_call_command(Agent,become(Other)):- show_call(become_player(Agent,Other)).


% logout
moo:action_info(logout(optional(agent,self)),"logs out of game (quits)").
moo:agent_call_command(_Agent,logout(Other)):-get_agent_session(Other,O),assert(thlocal:wants_logout(O)).

moo:verb_alias(quit,logout).

:- include(logicmoo(vworld/moo_footer)).


