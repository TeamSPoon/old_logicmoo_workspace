/** <module> 
% This module defines the module types that we use:
% utility,planner,parser,action,database,effects,spawning_loading,connection
% It is the basic module system of the Logicmoo MUD
%
% Project Logicmoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:- module(moo,[coerce/3, current_context_module/1,
         dynamic_multifile_exported/1,
         dynamic_multifile_exported/2,
         op(1150,fx,dynamic_multifile_exported),
         term_expansion_local/2,
         moo:register_module_type/1, 
         registered_module_type/2,
         end_module_type/1,
         enter_term_anglify/2,
         % decl_dbase_pred/2,
         register_timer_thread/3,
         end_module_type/2        
          ]).

:- dynamic registered_module_type/2.

dynamic_multifile_exported(M:FA):- !, dynamic_multifile_exported(M,FA).
dynamic_multifile_exported( FA ):- !, current_module(M),dynamic_multifile_exported(M,FA).

dynamic_multifile_exported(_,M:F/A):-!,dynamic_multifile_exported(M,F,A).
dynamic_multifile_exported(_, M:F ):-!,dynamic_multifile_exported(M,F).
dynamic_multifile_exported(M, [A] ):-!,dynamic_multifile_exported(M,A).
dynamic_multifile_exported(M,[A|L]):-!,dynamic_multifile_exported(M,A),dynamic_multifile_exported(M,L).
dynamic_multifile_exported(M,(A,L)):-!,dynamic_multifile_exported(M,A),dynamic_multifile_exported(M,L).
dynamic_multifile_exported(M, F/A ):-!,dynamic_multifile_exported(M,F,A).

dynamic_multifile_exported(M,F,A):-!, '@'(( dynamic(F/A), multifile(F/A), M:export(F/A)), M).

enter_term_anglify(X,Y):-findall(X-Y-Body,clause(moo:term_anglify(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).
enter_term_anglify(X,Y):-findall(X-Y-Body,clause(moo:term_anglify_np(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).
enter_term_anglify(X,Y):-findall(X-Y-Body,clause(moo:term_anglify_last(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).
enter_term_anglify(X,Y):-findall(X-Y-Body,clause(moo:term_anglify_np_last(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).

:- dynamic_multifile_exported moo:db_prop/2, moo:db_prop/1, moo:is_db_prop/3.

:- dynamic_multifile_exported moo:term_anglify/2.
:- dynamic_multifile_exported moo:term_anglify_last/2.
:- dynamic_multifile_exported moo:term_anglify_np/3.
:- dynamic_multifile_exported moo:term_anglify_np_last/3.

current_context_module(Ctx):-loading_module_h(Ctx),!.
current_context_module(Ctx):-context_module(Ctx).

create_queryPred(H,B):-functor(H,HF,HA),functor(B,BF,BA),
      moo:export(HF/HA), 
      dynamic_multifile_exported(BF/BA),moo:export(BF/BA), dynamic_multifile_exported(BF/BA), 
      asserta((H:-B)).

:- dynamic_multifile_exported thlocal:current_agent/2.
:- thread_local(thload:current_agent/2).

:-asserta(thload:current_agent(_,dead)).
:-ignore(retract(thload:current_agent(_,dead))).

:- dynamic_multifile_exported moo:action_rules/4.
:- dynamic_multifile_exported moo:agent_call_command/2.
:- dynamic_multifile_exported moo:agent_text_command/4.
:- dynamic_multifile_exported moo:decl_db_prop/2.
:- dynamic_multifile_exported moo:decl_update_charge/2.
:- dynamic_multifile_exported moo:decl_update_stats/2.
:- dynamic_multifile_exported moo:label_type_props/3.
:- dynamic_multifile_exported moo:specifier_text/2.
:- dynamic_multifile_exported moo:subclass/2.
:- dynamic_multifile_exported moo:decl_subclass/2.
:- dynamic_multifile_exported moo:type_default_props/3.
:- dynamic_multifile_exported moo:type_grid/3.
:- dynamic_multifile_exported moo:use_usable/4.
:- dynamic_multifile_exported moo:world_agent_plan/3.

moo:db_prop(ArgTypes,PropTypes):-moo:decl_db_prop(ArgTypes,PropTypes).

:- dynamic_multifile_exported moo:call_after_load/1.

:- create_queryPred(moo:createableType(A) , moo:decl_createableType(A)).
:- create_queryPred(moo:createableType(A,B) , moo:decl_createableType(A,B)).

:-dynamic_multifile_exported(moo:decl_subclass/2).
:- create_queryPred(moo:subclass(A,B) , moo:decl_subclass(A,B)).

:- dynamic_multifile_exported moo:decl_action/1.
:- dynamic_multifile_exported moo:decl_action/2.
:- dynamic_multifile_exported moo:decl_action/3.

:- create_queryPred(moo:update_charge(A,B) , moo:decl_update_charge(A,B)).
:- create_queryPred(moo:update_stats(A,B) , moo:decl_update_stats(A,B)).


moo:decl_action(agent,What,Help):-moo:decl_action(What,Help).
moo:decl_action(What,text("command is: ",What)):- moo:decl_action(What).

:- create_queryPred(moo:coerce0(A,B,C) , moo:decl_coerce(A,B,C)).

moo:decl_coerce(_,_,_):-fail.
moo:coerce(What,Type,NewThing):-moo:decl_coerce(What,Type,NewThing),!.
moo:coerce(What,_Type,NewThing):-NewThing = What.


:- dynamic_multifile_exported moo:decl_database_hook/2.
:- dynamic_multifile_exported moo:decl_mud_test/2.
:- dynamic_multifile_exported moo:verb_alias/2.


% hooks are declared as
% moo:decl_database_hook(assert(A_or_Z),Fact).
% moo:decl_database_hook(retract(One_or_All),Fact).

moo:run_database_hooks(Type,Hook):- must(doall((copy_term(Hook,HCopy),moo:decl_database_hook(Type,HCopy)))).



:- dynamic_multifile_exported registered_module_type/2.

:- meta_predicate moo:tick_every(*,*,0).
:- meta_predicate moo:register_timer_thread(*,*,0).


register_timer_thread(Name,_Seconds,_OnTick):-current_thread(Name,_Status).
register_timer_thread(Name,Seconds,OnTick):-
   thread_create(tick_every(Name,Seconds,OnTick),_ID,[alias(Name)]). 

tick_every(Name,Seconds,OnTick):-repeat,sleep(Seconds),catch(OnTick,E,dmsg(caused(Name,OnTick,E))),fail.
   
end_module_type(Type):-current_context_module(CM),end_module_type(CM,Type).
end_module_type(CM,Type):-retractall(registered_module_type(CM,Type)).

moo:register_module_type(Type):- current_context_module(CM),moo:register_module_type(CM,Type),dbase:begin_transform_cyc_preds.

moo:register_module_type(CM,Types):-is_list(Types),!,forall(member(T,Types),moo:register_module_type(CM,T)).
moo:register_module_type(CM,Type):-asserta(registered_module_type(CM,Type)).

registered_module_type(Type):- current_context_module(CM),registered_module_type(CM,Type).

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
      dynamic_multifile_exported(P/A),
      export(P/A),
      ignore(H2B2 = ((moo:H2 :- B2))),!.

term_expansion_local(X,Y):- loading_module_h(CM),functor(X,F,A),term_expand_local(CM,X,F,A,Y).



term_expand_local(CM,X,F,A,Y):-findall(Y,term_expand_local_each(CM,X,F,A,Y),Ys), Ys == [],!,fail.  

term_expand_local_each(_,_,F,A,_):- member(F/A,[never_expand]),!,fail.
term_expand_local_each(CM,X,F,A,X):-registered_module_type(CM,utility),export(F/A).
term_expand_local_each(CM,X,F,A,X):-registered_module_type(CM,dynamic_multifile_exported),dynamic_multifile_exported(F/A).


term_expansion_local0(A,B):-term_expansion_local(A,B),!.
term_expansion_local0(A,A).

% moo:decl_dbase_pred(Pred,Flags):-debase:dbase_define_db_prop(Pred,Flags).

% user:term_expansion(X,Y):- term_expansion_local0(X,Y).

%:- include(logicmoo('vworld/moo_header.pl')).
%:- moo:register_module_type(utility).

moo:agent_text_command(_Agent,_Text,_AgentTarget,_Cmd):-fail.

%:- include(logicmoo('vworld/moo_footer.pl')).

