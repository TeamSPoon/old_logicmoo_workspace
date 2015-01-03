% as_agent.pl
% Dec 13, 2035
% Douglas Miles
%
% This allows control of any cha5racter from any otgher character

:-swi_module(as_agent, []).

:- include(logicmoo(vworld/moo_header)).

:- register_module_type(tCommand).

action_info('as'(tAgentGeneric,tCommand), "as <agent> <command>").
agent_call_command(_Agent,'as'(OtherAgent,Command)):- call_agent_command(OtherAgent,Command).

:- include(logicmoo(vworld/moo_footer)).
