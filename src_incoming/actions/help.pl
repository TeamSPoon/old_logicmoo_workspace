:- module(help, []).
/** <module> A command to tell an agent all the possible commands
% help.pl
% Douglas Miles 2014
*/
:- include(logicmoo(vworld/moo_header)).

:- moodb:register_module_type(command).

moo:type_action_help(agent, help, "shows this help").

% Help - A command to tell an agent all the possible commands
moo:agent_call_command(_Agent,help) :- doall((moo:type_action_help(A,B,C),fmt(moo:type_action_help(A,B,C)))).


moo:specifier_text(Text,verb):- moo:type_action_help(_,A,_),nonvar(A),functor(A,Text,_).


:- include(logicmoo(vworld/moo_footer)).

