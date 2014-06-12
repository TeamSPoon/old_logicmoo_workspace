:- module(help, []).
/** <module> A command to tell an agent all the possible commands
% help.pl
% Douglas Miles 2014
*/
:- include(logicmoo(vworld/moo_header)).

:- moo:register_module_type(command).

moo:type_action_help(agent, help, "shows this help").
moo:type_action_help(agent, help(optional(string,"")), "shows this help").

:-export(get_type_action_help/3).

get_type_action_help(A,B,C):-get_type_action_help_0(A,B,C).

get_type_action_help_0(isaFn(A),TEMPL,text(Text,'does: ',do(A2,TEMPL))):- get_agent_text_command(A,Text,A2,Goal),(nonvar(Goal)->TEMPL=Goal;TEMPL=Text).
get_type_action_help_0(A,B,C):- call_no_cuts(moo:type_action_help(A,B,C)).
get_type_action_help_0(_What,TEMPL,Help):- call_no_cuts(moo:action_help(TEMPL,Help)).
get_type_action_help_0(_What,TEMPL,S):- call_no_cuts(moo:action_info(TEMPL)),sformat(S,'Prolog looks like: ~q',[TEMPL]).
get_type_action_help_0(What,Syntax,text([makes,happen,List])):- call_no_cuts(moo:action_rules(Agent,Verb,[Obj|Objs],List)),
      safe_univ(Syntax,[Verb,Obj|Objs]), once(member(isa(Obj,Type),List);Type=term),ignore(Agent=an(What)),ignore(What=agent).


commands_list(ListS):-findall(action_help(B,C,A),(get_type_action_help(A,B,C),numbervars(get_type_action_help(A,B,C),0,_,[attvar(skip),singletons(true)])),List),
   predsort(alpha_shorter,List,ListS).

alpha_shorter(OrderO, P1,P2):-arg(1,P1,O1),arg(1,P2,O2),!,alpha_shorter_1(OrderO, O1,O2),!.
alpha_shorter(OrderO, P1,P2):-alpha_shorter_1(OrderO, P1,P2),!.

alpha_shorter_1(OrderO, P1,P2):-get_functor(P1,F1,A1),get_functor(P2,F2,A2),compare(OrderF,F1,F2), 
 (OrderF \== '=' -> OrderO=OrderF ;
  (compare(OrderA,A1,A2), (OrderA \== '=' -> OrderO=OrderA ; compare(OrderO,P1,P2)))).


% Help - A command to tell an agent all the possible commands
moo:agent_call_command(_Agent,help) :- commands_list(ListS),forall(member(E,ListS),fmt(E)).
moo:agent_call_command(_Agent,help(Str)) :-commands_list(ListS),forall(member(E,ListS),write_string_if_contains(Str,E)).

write_string_if_contains(Must,E):-ignore((with_output_to(string(Str),fmt(E)),str_contains_all([Must],Str),fmt(Str))).

moo:term_specifier_text(Text,verb):- get_type_action_help(_,A,_),nonvar(A),functor_safe(A,Text,_).

moo:agent_text_command(Agent,[Who],Agent,Cmd):- get_type_action_help(_,Syntax,_),Syntax=..[Who,optional(_,Default)],Cmd=..[Who,Default].
moo:agent_text_command(Agent,[Who,Type],Agent,Cmd):- get_type_action_help(_,Syntax,_),Syntax=..[Who,optional(Type,_)],Cmd=..[Who,Type].

:- include(logicmoo(vworld/moo_footer)).

