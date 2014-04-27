
:- module(moo,[coerce/3, current_context_module/1]).

current_context_module(Ctx):-loading_module_h(Ctx),!.
current_context_module(Ctx):-context_module(Ctx).

create_queryPred(H,B):-functor(H,HF,HA),functor(B,BF,BA),
      moo:export(HF/HA), 
      dynamic(BF/BA),moo:export(BF/BA), multifile(BF/BA), 
      asserta((H:-B)).

:- dynamic thlocal:current_agent/1.
:- thread_local(thload:current_agent/1).

:-asserta(thload:current_agent(dead)).
:-ignore(retract(thload:current_agent(dead))).

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
