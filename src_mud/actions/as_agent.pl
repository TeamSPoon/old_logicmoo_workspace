% as_agent.pl
% Dec 13, 2035
% Douglas Miles
%
% This allows control of any cha5racter from any otgher character

:-swi_module(as_agent, []).

:- include(logicmoo(vworld/moo_header)).

:- register_module_type(mtCommand).

user:action_info('actAs'(tAgentGeneric,ftAction), "actAs <agent> <command>").
user:agent_call_command(_Agent,'actAs'(OtherAgent,Command)):- call_agent_command(OtherAgent,Command).

:- include(logicmoo(vworld/moo_footer)).
