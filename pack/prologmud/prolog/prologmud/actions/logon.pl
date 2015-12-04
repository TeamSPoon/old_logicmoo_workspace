/** <module>
% logon.pl
%
% This file defines how the agents gain their presence and leave the sim
% Comments below document the basic idea.
%
% Dec 13, 2035
% Douglas Miles
*/
% :-swi_module(user). 
:-swi_module(modLogin, []).

:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).
:- multifile t_l:wants_logout/1.

% rename
user:action_info(actRename(ftString),"Rename your player").
user:agent_call_command(Agent,actRename(Other)):- padd(Agent,mudNamed(Other)).

% become
user:verb_alias(become,actLogin).

% logon
user:verb_alias(logon,actLogin).

% login
user:action_info(actLogin(isOptional(tAgent,isRandom(tAgent))),"(Re)Login and assume the role of an agent").
user:agent_call_command(Agent,actLogin(Other)):- show_call(become_player(Agent,Other)).

% logout
user:action_info(actLogout(isOptional(tAgent,isSelfAgent)),"logs out of game (quits)").
user:agent_call_command(_Agent,actLogout(Other)):-get_agent_session(Other,O),assert(t_l:wants_logout(O)).

% quit
user:verb_alias(quit,actLogout).

% logoff
user:verb_alias(logoff,actLogout).


:- include(prologmud(mud_footer)).
