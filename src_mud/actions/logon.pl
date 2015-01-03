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
:-swi_module(actLogin, []).

:- include(logicmoo(vworld/moo_header)).

:- register_module_type(tCommand).
:- multifile thlocal:wants_logout/1.

t_f(t,f).
t_f(true,false).
t_f(on,off).
t_f(1,0).
t_f(actShow,actHide).
t_f(yes,no).
t_f(y,n).

to_on_off(FLAG,ON,OFF,ON_OFF):- t_f(_,FLAG) -> ON_OFF=OFF; ON_OFF=ON.


% rename
action_info(actRename(string),"Rename your player").
agent_call_command(Agent,actRename(Other)):- padd(Agent,mudNamed(Other)).

% become
verb_alias(become,actLogin).

% logon
verb_alias(logon,actLogin).

% login
action_info(actLogin(optional(tAgentGeneric,random(tAgentGeneric))),"(Re)Login and assume the role of an agent").
agent_call_command(Agent,actLogin(Other)):- show_call(become_player(Agent,Other)).

% logout
action_info(actLogout(optional(tAgentGeneric,self)),"logs out of game (quits)").
agent_call_command(_Agent,actLogout(Other)):-get_agent_session(Other,O),assert(thlocal:wants_logout(O)).

% quit
verb_alias(quit,actLogout).

% logoff
verb_alias(logoff,actLogout).

% dmsg/show/hide
action_info(dmsg(optional(ftBoolean,actShow)),"set the dmsg flag to on/off").
agent_call_command(_Agent,dmsg(actShow)):- listing(tlbugger:dmsg_hidden(_)),current_prolog_flag(opt_debug,ON_OFF),fmt(current_prolog_flag(opt_debug,ON_OFF)).
agent_call_command(_Agent,dmsg(FLAG)):- to_on_off(FLAG,on,off,ON_OFF),!,set_bugger_flag(opt_debug,ON_OFF),fmt(current_prolog_flag(opt_debug,ON_OFF)).
action_info(actShow(optional(prolog,actShow)),"show messages of col").
agent_call_command(_Agent,actShow(A)):-bugger:dmsg_show(A).
action_info(actHide(optional(prolog,actShow)),"hide messages of col").
agent_call_command(_Agent,actHide(A)):-bugger:dmsg_hide(A).

% debug
verb_alias(debug,dmsg).

:- include(logicmoo(vworld/moo_footer)).
