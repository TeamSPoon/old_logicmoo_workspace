:- module(help, []).
/** <module> A command to tell an agent all the possible commands
% help.pl
% Douglas Miles 2014
*/
:- include(logicmoo(vworld/moo_header)).

:- moo:register_module_type(command).

moo:type_action_help(agent, help, "shows this help").
moo:type_action_help(_What,TEMPL,Help):- moo:action_help(TEMPL,Help).
moo:type_action_help(_What,TEMPL,S):-moo:action_info(TEMPL),sformat(S,'Prolog looks like: ~q',[TEMPL]).
moo:type_action_help(What,Syntax,text([makes,happen,List])):- moo:action_rules(Agent,Verb,[Obj|Objs],List),
      safe_univ(Syntax,[Verb,Obj|Objs]),
      once(member(isa(Obj,Type),List);Type=term),ignore(Agent=an(What)),ignore(What=agent).

% Help - A command to tell an agent all the possible commands
moo:agent_call_command(_Agent,help) :- doall((moo:type_action_help(A,B,C),fmt(type_action_help(A,B,C)))).

moo:term_specifier_text(Text,verb):- moo:type_action_help(_,A,_),nonvar(A),functor_safe(A,Text,_).


:- include(logicmoo(vworld/moo_footer)).

