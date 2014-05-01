
/** <module> 
% It is the basic module system of the MUD
%
% Douglas Miles
% Dec 13, 2035
%
% This module defines the module types that we use:
% utility,planner,parser,actions
*/
:- module(moo,[coerce/3, current_context_module/1,

    term_expansion_local/2,
         register_module_type/1, 
         registered_module_type/2, 
         end_module_type/1,
         register_timer_thread/3,
         end_module_type/2         ]).

current_context_module(Ctx):-loading_module_h(Ctx),!.
current_context_module(Ctx):-context_module(Ctx).

create_queryPred(H,B):-functor(H,HF,HA),functor(B,BF,BA),
      moo:export(HF/HA), 
      dynamic(BF/BA),moo:export(BF/BA), multifile(BF/BA), 
      asserta((H:-B)).

:- dynamic thlocal:current_agent/2.
:- thread_local(thload:current_agent/2).

:-asserta(thload:current_agent(_,dead)).
:-ignore(retract(thload:current_agent(_,dead))).

:- dynamic moo:action_rules/4.
:- multifile moo:action_rules/4.
:- dynamic moo:use_usable/4.
:- multifile moo:use_usable/4. 
:- dynamic moo:subclass/2.
:- multifile moo:subclass/2.
:- dynamic moo:type_default_props/3.
:- multifile moo:type_default_props/3.
:- multifile moo:type_grid/3.
:- dynamic moo:type_grid/3.
:- dynamic moo:label_type_props/3.
:- multifile moo:label_type_props/3.

:- multifile moo:world_agent_plan/3.
:- multifile moo:agent_text_command/4.
:- multifile moo:agent_call_command/2.
:- multifile moo:decl_update_charge/2.
:- multifile moo:decl_update_stats/2.

:- dynamic moo:specifier_text/2.
:- multifile moo:specifier_text/2.

:-dynamic(moo:call_after_load/1).
:-multifile(moo:call_after_load/1).

:- create_queryPred(moo:createableType(A) , moo:decl_createableType(A)).
:- create_queryPred(moo:createableType(A,B) , moo:decl_createableType(A,B)).

:-dynamic(moo:decl_subclass/2).
:-multifile(moo:decl_subclass/2).
:- create_queryPred(moo:subclass(A,B) , moo:decl_subclass(A,B)).

:- dynamic moo:decl_action/1.
:- multifile moo:decl_action/1.
:- dynamic moo:decl_action/2.
:- multifile moo:decl_action/2.
:- dynamic moo:decl_action/3.
:- multifile moo:decl_action/3.

:- create_queryPred(moo:update_charge(A,B) , moo:decl_update_charge(A,B)).
:- create_queryPred(moo:update_stats(A,B) , moo:decl_update_stats(A,B)).


moo:decl_action(agent,What,Help):-moo:decl_action(What,Help).
moo:decl_action(What,text("command is: ",What)):- moo:decl_action(What).

:- create_queryPred(moo:coerce0(A,B,C) , moo:decl_coerce(A,B,C)).

moo:decl_coerce(_,_,_):-fail.
moo:coerce(What,Type,NewThing):-moo:decl_coerce(What,Type,NewThing),!.
moo:coerce(What,_Type,NewThing):-NewThing = What.

:- dynamic moo:decl_database_hook/2.
:- multifile moo:decl_database_hook/2.


:- dynamic moo:decl_mud_test/2.
:- multifile moo:decl_mud_test/2.



% hooks are declared as
% moo:decl_database_hook(assert(A_or_Z),Fact).
% moo:decl_database_hook(retract(One_or_All),Fact).

moo:run_database_hooks(Type,Hook):- must(doall((copy_term(Hook,HCopy),moo:decl_database_hook(Type,HCopy)))).



:-dynamic registered_module_type/2.


register_timer_thread(Name,_Seconds,_OnTick):-current_thread(Name,_Status).
register_timer_thread(Name,Seconds,OnTick):-
   thread_create(tick_every(Name,Seconds,OnTick),_ID,[alias(Name)]). 

tick_every(Name,Seconds,OnTick):-repeat,sleep(Seconds),catch(OnTick,E,dmsg(caused(Name,OnTick,E))),fail.
   
end_module_type(Type):-current_context_module(CM),end_module_type(CM,Type).
end_module_type(CM,Type):-retractall(registered_module_type(CM,Type)).

register_module_type(Type):-current_context_module(CM),register_module_type(CM,Type).

register_module_type(CM,Types):-is_list(Types),!,forall(member(T,Types),register_module_type(CM,T)).
register_module_type(CM,Type):-asserta_new(registered_module_type(CM,Type)).

registered_module_type(Type):-current_context_module(CM),registered_module_type(CM,Type).

:-meta_predicate term_expansion_local(?,?),term_expansion_local0(?,?).
% :-meta_predicate user:term_expansion(?,?).

term_expansion_local(X,_):-var(X),!,fail.
term_expansion_local( ((':-'(_))) , _ ):-!,fail.

term_expansion_local(_:B1,B2):-!,term_expansion_local(B1,B2),!.

term_expansion_local(((H1:-B1)),H2B2):- !,
   nonvar(B1),  current_context_module(CM),!,        
      functor(H1,F,A), atom_concat(P,'_hook',F),!,atomic_list_concat([_,_|_],'_',P),
      H1=..[F|ARGS], H2=..[P|ARGS],
      B2 = '@'((nop(CM), B1), CM ),
      module_transparent((P/A)),
      % copy_term(H2,META), meta_predicate(META),
      multifile(P/A),
      export(P/A),
      ignore(H2B2 = ((moo:H2 :- B2))),!.

term_expansion_local(X,Y):- loading_module_h(CM),functor(X,F,A),term_expand_local(CM,X,F,A,Y).



term_expand_local(CM,X,F,A,Y):-findall(Y,term_expand_local_each(CM,X,F,A,Y),Ys), Ys == [],!,fail.  

term_expand_local_each(_,_,F,A,_):- member(F/A,[never_expand]),!,fail.
term_expand_local_each(CM,X,F,A,X):-registered_module_type(CM,utility),export(F/A).
term_expand_local_each(CM,X,F,A,X):-registered_module_type(CM,dynamic),dynamic(F/A).


term_expansion_local0(A,B):-term_expansion_local(A,B),!.
term_expansion_local0(A,A).


% user:term_expansion(X,Y):- term_expansion_local0(X,Y).

:- include(logicmoo('vworld/vworld_header.pl')).

:- register_module_type(utility).

moo:agent_text_command(_Agent,_Text,_AgentTarget,_Cmd):-fail.

:- include(logicmoo('vworld/vworld_footer.pl')).

