% as_agent.pl
% Dec 13, 2035
% Douglas Miles
%
% This allows control of any cha5racter from any otgher character

:- module(as_agent, []).

:- include(logicmoo(vworld/moo_header)).

:- moo:register_module_type(command).

moo:action_info(as(agent,command)), "as <agent> <command>").
moo:agent_call_command(Agent,as(OtherAgent,Command)):- call_agent_command(OtherAgent,Command).

:- include(logicmoo(vworld/moo_footer)).


