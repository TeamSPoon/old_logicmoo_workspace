% :-swi_module(user). 
:-swi_module(actHelp, [show_help/0]).
/** <module> A command to tell an agent all the possible commands
% help.pl
% Douglas Miles 2014
*/
:- include(logicmoo(vworld/moo_header)).

:- register_module_type(tCommand).

mudIsa(tHumanPlayer,tAgentcol).
mudSubclass(tAgentcol,tCol).

% type_action_info(human_player,help, "shows this help").
type_action_info(tHumanPlayer,actHelp(optional(string,"")), "shows this help").

action_info(What,ftText("command is: ",What)):- tActionType(What).


:-swi_export(get_type_action_help_commands_list/3).
get_type_action_help_commands_list(A,B,C):-no_repeats(get_type_action_help_0(A,B,C)).

:-swi_export(get_type_action_templates/1).
get_type_action_templates(Templ):- no_repeats((get_type_action_help_0(_,Templ,_),good_template(Templ))).

tActionType(Templ):-get_type_action_templates(Templ).

:-swi_export(good_template/1).
good_template(Templ):- \+ contains_singletons(Templ).

get_type_action_help_0(What,TEMPL,Help):- call_no_cuts(type_action_info(What,TEMPL,Help)).
get_type_action_help_0(_What,TEMPL,Help):- call_no_cuts(action_info(TEMPL,Help)).
get_type_action_help_0(_What,TEMPL,S):-    call_no_cuts(tActionType(TEMPL)),sformat(S,'Prolog looks like: ~q',[TEMPL]).
get_type_action_help_0(isaFn(A),TEMPL,ftText(Text,'does: ',do(A2,TEMPL))):- between(1,5,L),length(Text,L),get_agent_text_command(A,Text,A2,Goal),(nonvar(Goal)->TEMPL=Goal;TEMPL=Text).
get_type_action_help_0(What,Syntax,ftText([makes,happen,List])):- call_no_cuts(action_rules(Agent,Verb,[Obj|Objs],List)),atom(Verb),safe_univ(Syntax,[Verb,Obj|Objs]), 
                     % once(member(isa(Obj,_Type),List);_Type=term),
                      ignore(Agent=an(What)),ignore(What=tAgentGeneric).


action_info_db(TEMPL,S):- (PRED=agent_call_command(_,TEMPL);PRED=agent_text_command(_,_,_,TEMPL)) ,
   predicate_property(user:PRED,multifile),clause(PRED,_BODY,REF),nonvar(TEMPL),clause_property(REF,file(S)).

action_info(TEMPL,ftText(file,S,mudContains,TEMPL)):-action_info_db(TEMPL,S),not(clause_asserted(action_info(TEMPL,_Help),true)).

commands_list(ListS):-findall(action_info(B,C,A),(get_type_action_help_commands_list(A,B,C),
 numbervars(action_info(A,B,C),0,_,[attvar(bind),singletons(true)])),List),
   predsort(alpha_shorter,List,ListS).

alpha_shorter(OrderO, P1,P2):-arg(1,P1,O1),arg(1,P2,O2),!,alpha_shorter_1(OrderO, O1,O2),!.
alpha_shorter(OrderO, P1,P2):-alpha_shorter_1(OrderO, P1,P2),!.

alpha_shorter_1(OrderO, P1,P2):-functor_h(P1,F1,A1),functor_h(P2,F2,A2),compare(OrderF,F1,F2), 
 (OrderF \== '=' -> OrderO=OrderF ;
  (compare(OrderA,A1,A2), (OrderA \== '=' -> OrderO=OrderA ; compare(OrderO,P1,P2)))).


% Help - A command to tell an agent all the possible commands
show_help:- commands_list(ListS),forall(member(E,ListS),fmt(E)).
agent_call_command(_Agent,actHelp) :- show_help.
agent_call_command(_Agent,actHelp(Str)) :-commands_list(ListS),forall(member(E,ListS),write_string_if_contains(Str,E)).

write_string_if_contains("",E):-!,fmt(E).
write_string_if_contains(Must,E):-ignore((with_output_to(string(Str),fmt(E)),str_contains_all([Must],Str),fmt(Str))).

term_specifier_text(Text,tVerb,Inst):- get_type_action_templates(A),nonvar(A),functor_safe(A,Inst,_),name_text(Inst,Text).

%agent_text_command(Agent,[Who],Agent,Cmd):- nonvar(Who), get_type_action_templates(Syntax),Syntax=..[Who,optional(_,Default)],Cmd=..[Who,Default].
%agent_text_command(Agent,[Who,Type],Agent,Cmd):- get_type_action_templates(Syntax),nonvar(Who),Syntax=..[Who,optional(Type,_)],Cmd=..[Who,Type].

:- include(logicmoo(vworld/moo_footer)).
