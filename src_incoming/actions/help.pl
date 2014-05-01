:- module(help, []).
/** <module> A command to tell an agent all the possible commands
% help.pl
% Douglas Miles 2014
*/
:- include(logicmoo('vworld/vworld_header.pl')).

:- register_module_type(command).

moo:decl_action(agent, help, "shows this help").

% Help - A command to tell an agent all the possible commands
moo:agent_call_command(_Agent,help) :- doall((moo:decl_action(A,B,C),fmt(moo:decl_action(A,B,C)))).


moo:specifier_text(Text,verb):- moo:decl_action(A,_,_),functor(A,Text,_).



:- include(logicmoo('vworld/vworld_footer.pl')).

