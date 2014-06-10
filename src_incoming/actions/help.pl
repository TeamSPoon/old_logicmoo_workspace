:- module(help, []).
/** <module> A command to tell an agent all the possible commands
% help.pl
% Douglas Miles 2014
*/
:- include(logicmoo(vworld/moo_header)).

:- moo:register_module_type(command).

moo:type_action_help(agent, help, "shows this help").
moo:type_action_help(_What,TEMPL,Help):-dyn:action_help(TEMPL,Help).
moo:type_action_help(_What,TEMPL,S):-dyn:action_info(TEMPL),sformat(S,'Prolog looks like: ~q',[TEMPL]).

% Help - A command to tell an agent all the possible commands
moo:agent_call_command(_Agent,help) :- doall((moo:type_action_help(A,B,C),fmt(dyn:type_action_help(A,B,C)))).

dyn:specifier_text(Text,verb):- moo:type_action_help(_,A,_),nonvar(A),functor(A,Text,_).


:- include(logicmoo(vworld/moo_footer)).

