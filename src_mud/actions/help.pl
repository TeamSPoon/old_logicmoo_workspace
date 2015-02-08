% :-swi_module(user). 
:-swi_module(modHelp, [show_help/0]).
/** <module> A command to tell an agent all the possible commands
% help.pl
% Douglas Miles 2014
*/
:- include(logicmoo(vworld/moo_header)).

:- register_module_type(mtCommand).

%mudIsa(tHumanPlayer,ttAgentType).
%mudSubclass(ttAgentType,tCol).

% user:type_action_info(human_player,help, "shows this help").
user:type_action_info(tHumanPlayer,actHelp(isOptional(ftString,"")), "shows this help").

% user:action_info(TEMPL,S):- vtActionTemplate(TEMPL),to_param_doc(TEMPL,S).



:-decl_mpred_prolog(get_type_action_help_commands_list/3).
get_type_action_help_commands_list(A,B,C):-no_repeats_old(get_type_action_help_0(A,B,C)).

:-decl_mpred_prolog(get_all_templates/1).
get_all_templates(Templ):- call_tabled_can(no_repeats_old(get_all_templates0(Templ))).
get_all_templates0(Templ):-get_good_templates(Templ).
get_all_templates0(Templ):-get_bad_templates(Templ),not(get_good_templates(Templ)).
get_good_templates(Templ):- no_repeats_old((get_type_action_help_1(_,Templ,_),good_template(Templ))).
get_bad_templates(Templ):- no_repeats_old((get_type_action_help_1(_,Templ,_),not(good_template(Templ)))).


vtActionTemplate(Templ):- loop_check(get_all_templates(Templ),fail).

:-decl_mpred_prolog(good_template/1).
good_template(Templ):- \+ contains_singletons(Templ).


to_param_doc(TEMPL,S):-sformat(S,'Prolog looks like: ~q',[TEMPL]).



get_type_action_help_0(What,TEMPL,Help):- call_no_cuts(user:type_action_info(What,TEMPL,Help)).
get_type_action_help_0(_What,TEMPL,Help):- call_no_cuts(user:action_info(TEMPL,Help)).
get_type_action_help_0(isaFn(A),TEMPL,txtConcatFn(Text,'does: ',do(A2,TEMPL))):- between(1,5,L),length(Text,L),get_agent_text_command(A,Text,A2,Goal),(nonvar(Goal)->TEMPL=Goal;TEMPL=Text).
get_type_action_help_0(What,Syntax,txtConcatFn(makes,happen,List)):- call_no_cuts(action_rules(Agent,Verb,[Obj|Objs],List)),atom(Verb),safe_univ(Syntax,[Verb,Obj|Objs]), 
                     % once(member(isa(Obj,_Type),List);_Type=term),
                      ignore(Agent=an(What)),ignore(What=tAgentGeneric).

get_type_action_help_1(What,TEMPL,S):- get_type_action_help_0(What,TEMPL,S).
get_type_action_help_1(_What,TEMPL,S):- call_no_cuts(mudIsa(TEMPL,vtActionTemplate)),to_param_doc(TEMPL,S).


first_pl((BODY,_),PL):-!,
 first_pl(BODY,PL).
first_pl(PL,PL).

action_info_db(TEMPL,INFO,WAS):- (PRED=user:agent_call_command(_,WAS);PRED=user:agent_text_command(_,_,_,WAS)) ,
   clause(PRED,BODY,REF),clause_property(REF,file(S)),
   (nonvar(WAS)->true;once(( ignore((nop(S=S),first_pl(BODY,PL),ignore(catch(notrace(PL),_,true)))),nonvar(WAS)))),
   
    (TEMPL=@=WAS -> ((clause_property(REF,line_count(LC)),INFO=line(LC:S))) ;  (not(not(TEMPL=WAS)) -> INFO=file(S) ; fail)).
 
user:action_info(TEMPL,txtConcatFn(S,contains,WAS)):-action_info_db(TEMPL,S,WAS),not(clause_asserted(user:action_info(TEMPL,_Help),true)).


commands_list(ListS):- findall(Templ,get_all_templates(Templ),List),predsort(alpha_shorter_1,List,ListS).
commands_list_old(ListS):-
 findall(user:action_info(B,C,A),(get_type_action_help_commands_list(A,B,C), numbervars(user:action_info(A,B,C),0,_,[attvar(bind),singletons(true)])),List),
   predsort(alpha_shorter,List,ListS).

alpha_shorter(OrderO, P1,P2):-arg(1,P1,O1),arg(1,P2,O2),!,alpha_shorter_1(OrderO, O1,O2),!.
alpha_shorter(OrderO, P1,P2):-alpha_shorter_1(OrderO, P1,P2),!.

alpha_shorter_1(OrderO, P1,P2):-functor_h(P1,F1,A1),functor_h(P2,F2,A2),compare(OrderF,F1,F2), 
 (OrderF \== '=' -> OrderO=OrderF ;
  (compare(OrderA,A1,A2), (OrderA \== '=' -> OrderO=OrderA ; compare(OrderO,P1,P2)))).


get_template_docu(TEMPL,DOC):-no_repeats_old((get_type_action_help_0(_,TEMPL,DOC)*->true;get_type_action_help_1(_What,TEMPL,DOC))).
get_template_docu_all(TEMPL,DOC):-no_repeats_old(get_type_action_help_1(_What,TEMPL,DOC)).

show_templ_doc(TEMPL):-findall(DOC,get_template_docu(TEMPL,DOC),DOCL),nvfmt(TEMPL=DOCL).
show_templ_doc_all(TEMPL):-findall(DOC,get_template_docu_all(TEMPL,DOC),DOCL),nvfmt(TEMPL=DOCL).

nvfmt([XX]):-!,nvfmt(XX).
nvfmt(XX=[YY]):-!,nvfmt(XX=YY).
nvfmt(XX):-copy_term(XX,X),numbervars(X,0,_,[attvar(bind),singletons(true)]),fmt(X).

% Help - A command to tell an agent all the possible commands
show_help:- commands_list(ListS),forall(member(E,ListS),show_templ_doc(E)).
user:agent_call_command(_Agent,actHelp) :- show_help.
user:agent_call_command(_Agent,actHelp(Str)) :-show_help(Str).

show_help(Str):-commands_list(ListS),forall(member(E,ListS),write_string_if_contains(Str,E)).

write_string_if_contains("",E):-!,show_templ_doc(E),!.
write_string_if_contains(Must,E):-ignore((with_output_to(string(Str),show_templ_doc_all(E)),str_contains_all([Must],Str),fmt(Str))).

user:hook_coerce(Text,vtVerb,Inst):- get_all_templates(A),nonvar(A),functor_safe(A,Inst,_),name_text(Inst,Text).

%user:agent_text_command(Agent,[Who],Agent,Cmd):- nonvar(Who), get_all_templates(Syntax),Syntax=..[Who,isOptional(_,Default)],Cmd=..[Who,Default].
%user:agent_text_command(Agent,[Who,Type],Agent,Cmd):- get_all_templates(Syntax),nonvar(Who),Syntax=..[Who,isOptional(Type,_)],Cmd=..[Who,Type].

:- include(logicmoo(vworld/moo_footer)).
