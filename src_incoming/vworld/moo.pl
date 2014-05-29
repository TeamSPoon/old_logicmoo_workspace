/** <module> 
% This module defines the module types that we use:
% utility,planner,parser,action,database,effects,spawning_loading,connection
% It is the basic module system of the Logicmoo MUD
%
% Project LogicMoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:- module(moodb,
        [coerce/3, 
           add_db_prop/3,
           rem_db_prop/3,
         current_context_module/1,
         op(1150,fx,registerCycPred),
         registerCycPred/1,         
         registerCycPred/2,
         registerCycPred/3,
         isRegisteredCycPred/3,
         registerCycPredPlus2/1,
         term_expansion_local/2,
         run_database_hooks/2,
         register_module_type/1,
         ended_transform_moo_preds/0,
         registered_module_type/2,
         end_module_type/1,
         enter_term_anglify/2,
         % decl_dbase_pred/2,
         register_timer_thread/3,
         scan_updates/0,
          begin_transform_moo_preds/0,
          end_transform_moo_preds/0,
         end_module_type/2        
        ]).

scan_updates:-thread_property(X,alias(loading_code)),thread_property(X,status(running)),!.
scan_updates:-ignore(catch(make,_,true)).

:-dynamic(isRegisteredCycPred/3).
:-dynamic(ended_transform_moo_preds/0).
ended_transform_moo_preds.
begin_transform_moo_preds:- retractall(ended_transform_moo_preds).
end_transform_moo_preds:- retractall(ended_transform_moo_preds),asserta(ended_transform_moo_preds).



:- use_module(logicmoo(logicmoo_util/logicmoo_util_library)).

:- dynamic registered_module_type/2.



enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).
enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify_np(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).
enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify_last(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).
enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify_np_last(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).

:- dynamic_multifile_exported db_prop/2, is_db_prop/3.

add_db_prop(_,_,Var):- var(Var),!.
add_db_prop(_,_,[]):- !.
add_db_prop(F,A,[C|L]):-!, add_db_prop(F,A,C),add_db_prop(F,A,L),!.
add_db_prop(F,A,CL):- assert_if_new(is_db_prop(F,A,CL)).

rem_db_prop(_,_,Var):- var(Var),!.
rem_db_prop(_,_,[]):- !.
rem_db_prop(F,A,[C|L]):-!, rem_db_prop(F,A,C),rem_db_prop(F,A,L),!.
rem_db_prop(F,A,CL):- retractall(is_db_prop(F,A,CL)).

% ============================================
% Prolog to Cyc Predicate Mapping
%
%  the following will all do the same things:
%
% :- registerCycPred('BaseKB':isa/2). 
% :- registerCycPred('BaseKB':isa(_,_)). 
% :- registerCycPred(isa(_,_),'BaseKB'). 
% :- registerCycPred('BaseKB',isa,2). 
%
%  Will make calls 
% :- isa(X,Y)
%  Query into #$BaseKB for (#$isa ?X ?Y) 
%
% ============================================
:-dynamic(isRegisteredCycPred/3).

% :- registerCycPred('BaseKB':isa/2). 
registerCycPred(Mt:Pred/Arity):- !,
   registerCycPred(Mt,Pred,Arity).

% :- registerCycPred('BaseKB':isa(_,_)). 
registerCycPred(Mt:Term):-
   functor(Term,Pred,Arity),
   registerCycPred(Mt,Pred,Arity).

registerCycPred(M:F):-!, '@'(registerCycPred(F), M).
registerCycPred(F/A):- reallyRegisterCycPred(F,A).
registerCycPred([A]):-!,registerCycPred(A).
registerCycPred([A|L]):-!,registerCycPred(A),registerCycPred(L).
registerCycPred((A,L)):-!,registerCycPred(A),registerCycPred(L).

registerCycPredPlus2(M:F):-!, '@'(registerCycPredPlus2(F), M).
registerCycPredPlus2(F/A):- A2 is A -2, registerCycPred(F/A2).
registerCycPredPlus2([A]):-!,registerCycPredPlus2(A).
registerCycPredPlus2([A|L]):-!,registerCycPredPlus2(A),registerCycPredPlus2(L).
registerCycPredPlus2((A,L)):-!,registerCycPredPlus2(A),registerCycPredPlus2(L).

reallyRegisterCycPred(F,A) :- registerCycPred(_,F,A).

get_term_fa(_:P,F,A):-!,get_term_fa(P,F,A).
get_term_fa(F/A,F,A):-!.
get_term_fa(P,F,A):-functor_safe(P,F,A).

% :- registerCycPred(isa,2). 
registerCycPred(Term,Arity):- integer(Arity),!,
   get_term_fa(Term,Pred,_),
   registerCycPred(_Mt,Pred,Arity).
% :- registerCycPred(isa(_,_),'BaseKB'). 
registerCycPred(Term,Mt):- !,
   get_term_fa(Term,Pred,Arity),
   registerCycPred(Mt,Pred,Arity).


   
% :- registerCycPred('BaseKB',isa,2). 
registerCycPred(Mt,_:Pred,Arity):-!,registerCycPred(Mt,Pred,Arity).
registerCycPred(Mt,Pred,0):-!,registerCycPred(Mt,Pred,2).
registerCycPred(Mt,Pred,Arity):-isRegisteredCycPred(Mt,Pred,Arity),!.
registerCycPred(Mt,Pred,Arity):-
      M = moodb,
      assertz(M:isRegisteredCycPred(Mt,Pred,Arity)),
      add_db_prop(Pred,Arity,isRegisteredCycPred(Mt,Pred,Arity)),!.


:- registerCycPred(db_prop/2).
:- registerCycPred(subclass/2).

:- thread_local(thload:current_agent/2).
:- asserta(thload:current_agent(_,dead)).
:- ignore(retract(thload:current_agent(_,dead))).

current_context_module(Ctx):-loading_module_h(Ctx),!.
current_context_module(Ctx):-context_module(Ctx).


%:- dynamic_multifile_exported agent_call_command/2.
:- dynamic_multifile_exported call_after_load/1.
:- dynamic_multifile_exported thlocal:current_agent/2.
:- dynamic_multifile_exported world_agent_plan/3.


:- registerCycPred(term_anglify_last/2).
:- registerCycPred(term_anglify/2).

:- registerCycPred action_info/1.
:- registerCycPred action_help/2.
:- registerCycPred type_action_help/3.
:- registerCycPred action_rules/4.
:- registerCycPred agent_text_command/4.
:- registerCycPred createableSubclassType/2.
:- registerCycPred createableType/1.
:- registerCycPred label_type_props/3.
:- registerCycPred specifier_text/2.
:- registerCycPred subclass/2.
:- registerCycPred term_anglify/2.
:- registerCycPred term_anglify_last/2.
:- registerCycPred term_anglify_np/3.
:- registerCycPred term_anglify_np_last/3.
:- registerCycPred type_default_props/3.
:- registerCycPred type_grid/3.
:- registerCycPred update_charge/2.
:- registerCycPred update_stats/2.
:- registerCycPred use_usable/4.

:- dynamic_multifile_exported(decl_coerce/3).

decl_coerce(_,_,_):-fail.
coerce(What,Type,NewThing):- decl_coerce(What,Type,NewThing),!.
coerce(What,_Type,NewThing):-NewThing = What.


:- dynamic_multifile_exported decl_database_hook/2.
:- dynamic_multifile_exported decl_mud_test/2.
:- dynamic_multifile_exported verb_alias/2.


% hooks are declared as
% decl_database_hook(assert(A_or_Z),Fact).
% decl_database_hook(retract(One_or_All),Fact).


run_database_hooks(Type,Hook):- must(doall((copy_term(Hook,HCopy), decl_database_hook(Type,HCopy)))).



:- dynamic_multifile_exported registered_module_type/2.

:- meta_predicate tick_every(*,*,0).
:- meta_predicate register_timer_thread(*,*,0).


register_timer_thread(Name,_Seconds,_OnTick):-current_thread(Name,_Status).
register_timer_thread(Name,Seconds,OnTick):-
   thread_create(tick_every(Name,Seconds,OnTick),_ID,[alias(Name)]). 

tick_every(Name,Seconds,OnTick):- repeat,sleep(Seconds),catch(OnTick,E,dmsg(caused(Name,OnTick,E))),fail.
   
end_module_type(Type):-current_context_module(CM),end_module_type(CM,Type).
end_module_type(CM,Type):- retractall(registered_module_type(CM,Type)).

register_module_type(Type):- current_context_module(CM), register_module_type(CM,Type),begin_transform_moo_preds.

register_module_type(CM,Types):-is_list(Types),!,forall(member(T,Types), register_module_type(CM,T)).
register_module_type(CM,Type):-asserta(registered_module_type(CM,Type)).

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
      dynamic_multifile_exported(CM:P/A),
      export(P/A),
      ignore(H2B2 = (( H2 :- B2))),!.

term_expansion_local(X,Y):- loading_module_h(CM),functor(X,F,A),term_expand_local(CM,X,F,A,Y).



term_expand_local(CM,X,F,A,Y):-findall(Y,term_expand_local_each(CM,X,F,A,Y),Ys), Ys == [],!,fail.  

term_expand_local_each(_,_,F,A,_):- member(F/A,[never_expand]),!,fail.
term_expand_local_each(CM,X,F,A,X):- registered_module_type(CM,utility),export(F/A).
term_expand_local_each(CM,X,F,A,X):- registered_module_type(CM,dynamic_multifile_exported), dynamic_multifile_exported(CM:F/A).


term_expansion_local0(A,B):-term_expansion_local(A,B),!.
term_expansion_local0(A,A).

% decl_dbase_pred(Pred,Flags):-debase:dbase_define_db_prop(Pred,Flags).

% user:term_expansion(X,Y):- term_expansion_local0(X,Y).

%:- include(logicmoo(vworld/moo_header)).
%:- register_module_type(utility).

% agent_text_command(_Agent,_Text,_AgentTarget,_Cmd):-fail.

:- registerCycPred(agent_call_command,2).

%:- include(logicmoo(vworld/moo_footer)).

% :-make.

