:- module(help, []).
/** <module> A command to tell an agent all the possible commands
% help.pl
% Douglas Miles 2014
*/
:- include(logicmoo(vworld/moo_header)).

:- moodb:register_module_type(command).

moodb:type_action_help(agent, help, "shows this help").
moodb:type_action_help(_What,TEMPL,Help):-moo:action_help(TEMPL,Help).
moodb:type_action_help(_What,TEMPL,S):-moo:action_info(TEMPL),sformat(S,'Prolog looks like: ~q',[TEMPL]).

% Help - A command to tell an agent all the possible commands
moo:agent_call_command(_Agent,help) :- doall((moodb:type_action_help(A,B,C),fmt(moo:type_action_help(A,B,C)))).

moo:specifier_text(Text,verb):- moodb:type_action_help(_,A,_),nonvar(A),functor(A,Text,_).


:- include(logicmoo(vworld/moo_footer)).

