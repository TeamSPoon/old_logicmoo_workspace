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
% :- module(user). 
:- module(moo,[coerce/3, current_context_module/1,
    rescan_dbase_t_once/0,
    rescan_dbase_t/0,
    term_expansion_local/2,
         register_module_type/1, 
         registered_module_type/2, 
         end_module_type/1,
         agent_text_command/4,
         decl_dbase_pred/2,
         register_timer_thread/3,
         end_module_type/2         ]).


hdr_debug(_,_):-!.
hdr_debug(F,A):-'format'(F,A).

:- dynamic_multifile_exported action_info/1.
:- dynamic_multifile_exported action_info/2.
:- dynamic_multifile_exported action_info/3.
:- dynamic_multifile_exported mud_test/2.
:- dynamic_multifile_exported agent_call_command/2.
:- dynamic_multifile_exported moo:world_agent_plan/3.
:- dynamic_multifile_exported now_unused/1.
:- dynamic_multifile_exported decl_mud_test/2.
:- dynamic_multifile_exported verb_alias/2.

:- multifile hook:decl_database_hook/2.

:- dynamic_multifile_exported registered_module_type/2.
:- dynamic_multifile_exported (decl_coerce)/3.
:- dynamic_multifile_exported agent_text_command/4.
:- dynamic_multifile_exported check_permanence/4.

:- dynamic_multifile_exported((action_info/1,action_info/2,action_rules/4,action_info/3,term_specifier_text/2,action_verb_useable/4)).
:- dynamic_multifile_exported((term_anglify/2,term_anglify_last/2, term_anglify_np/3,term_anglify_np_last/3)).
:- dynamic_multifile_exported((update_charge/2,update_stats/2,default_type_props/3)).

:- module_transparent register_module_type/1.

:-thread_local thlocal:session_agent/2.
:-dynamic_multifile_exported(thlocal:session_agent/2).
:-thread_local thlocal:repl_writer/2.
:-thread_local thlocal:repl_to_string/2.
:-thread_local thlocal:dbase_change/2.
:-thread_local thlocal:dbase_opcall/2.

current_context_module(Ctx):-loading_module_h(Ctx),!.
current_context_module(Ctx):-context_module(Ctx).

create_queryPred(H,B):-functor(H,HF,HA),functor(B,BF,BA),
      moo:export(HF/HA), 
      dynamic(BF/BA),moo:export(BF/BA), multifile(BF/BA), 
      asserta((H :- call_no_cuts(B))).


:- dynamic moo:decl_db_prop/2.
:- multifile moo:decl_db_prop/2.

:- dynamic moo:action_rules/4.
:- multifile moo:action_rules/4.
:- dynamic moo:use_usable/4.
:- multifile moo:use_usable/4. 
:- dynamic moo:subclass/2.
:- multifile moo:subclass/2.
:- dynamic moo:default_type_props/3.
:- multifile moo:default_type_props/3.
:- multifile moo:type_grid/3.
:- dynamic moo:type_grid/3.
:- dynamic moo:label_type_props/3.
:- multifile moo:label_type_props/3.

:- multifile moo:mpred/2.
:- dynamic moo:mpred/2.

:- dynamic moo:loading_module_h/1.
:- multifile moo:loading_module_h/1.
:- multifile moo:world_agent_plan/3.
:- multifile moo:agent_text_command/4.
:- multifile moo:agent_call_command/2.
:- multifile moo:update_charge/2.
:- multifile moo:update_stats/2.

:- dynamic moo:term_specifier_text/2.
:- multifile moo:term_specifier_text/2.

:-dynamic(moo:call_after_load/1).
:-multifile(moo:call_after_load/1).

%:- create_queryPred(moo:createableType(A) , moo:createableType(A)).
%:- create_queryPred(moo:createableType(A,B) , moo:createableType(A,B)).

:-dynamic(moo:subclass/2).
:-multifile(moo:subclass/2).
%:- create_queryPred(moo:subclass(A,B) , moo:subclass(A,B)).

:- dynamic moo:action_info/1.
:- multifile moo:action_info/1.
:- dynamic moo:action_info/2.
:- multifile moo:action_info/2.
:- dynamic moo:action_info/3.
:- multifile moo:action_info/3.

:- create_queryPred(moo:qqcall_update_charge(A,B) , moo:update_charge(A,B)).
:- create_queryPred(moo:qqcall_update_charge(A,B) , moo:update_stats(A,B)).


moo:action_info(agent,What,Help):-moo:action_info(What,Help).
moo:action_info(What,text("command is: ",What)):- moo:action_info(What).

:- create_queryPred(moo:coerce0(A,B,C) , moo:decl_coerce(A,B,C)).

moo:decl_coerce(_,_,_):-fail.
moo:coerce(What,Type,NewThing):-moo:decl_coerce(What,Type,NewThing),!.
moo:coerce(What,_Type,NewThing):-NewThing = What.


:- dynamic moo:decl_mud_test/2.
:- multifile moo:decl_mud_test/2.



% hooks are declared as
% hook:decl_database_hook(assert(A_or_Z),Fact).
% hook:decl_database_hook(retract(One_or_All),Fact).

moo:run_database_hooks(Type,Hook):- copy_term(Hook,HCopy),doall(call_no_cuts(hook:decl_database_hook(Type,HCopy))).

rescan_dbase_t:-  loop_check(rescan_dbase_t_once,true).

reduce_clause((C:-B),C):-B==true,!.
reduce_clause(C,C).

rescan_dbase_t_once:- must(remove_duplicated_facts),must(rerun_database_hooks).

remove_duplicated_facts:-doall((gather_fact_heads(M,H),BB=true,once((findall(C,(clause(H,B),B=@=BB,reduce_clause((H:-B),C)),CL1),list_to_set(CL1,CL2),once(reduce_fact_heads(M,H,CL1,CL2)))))).
rerun_database_hooks:-doall((gather_fact_heads(_M,H),forall(clause(H,true),run_database_hooks(assert(z),H)))).


reduce_fact_heads(_M,_H,CL1,CL1):-!. % no change
reduce_fact_heads(M,H,CL1,CL2):- 
   length(CL1,L1),length(CL2,L2),
   dmsg(reduce_fact_heads(M,H,from(L1,L2))),
   forall(member(C,CL1),retractall(M:C)),
   forall(member(C,CL2),assertz(M:C)).

gather_fact_heads(M,H):- (nonvar(M)->true; member(M,[dbase,moo,world,user,hook])), current_predicate(M:F/A), 
  once((once((A>0,atom(F),F\=(:),var(H), debugOnError(functor(H,F,A)))),compound(H),predicate_property(M:H,number_of_clauses(_)),
  not((arg(_,vv(system,bugger,logicmoo_util_dcg,user),M);predicate_property(M:H,imported_from(_)))),predicate_property(M:H,dynamic))).

:-dynamic registered_module_type/2.

:- meta_predicate moo:tick_every(*,*,0).
:- meta_predicate moo:register_timer_thread(*,*,0).


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


:-thread_local ended_transform_moo_preds/0, always_expand_on_thread/1, prevent_transform_moo_preds/0, may_moo_term_expand/1, always_transform_heads/0.

:-module_transparent begin_transform_moo_preds/0, end_transform_moo_preds/0.
:-export((begin_transform_moo_preds/0,end_transform_moo_preds/0)).
begin_transform_moo_preds:- retractall(ended_transform_moo_preds),context_module(CM),asserta(may_moo_term_expand(CM)).

end_transform_moo_preds:- retractall(ended_transform_moo_preds),asserta(ended_transform_moo_preds).


:-thread_local is_compiling_clause/0.

is_compiling:-is_compiling_clause;compiling.

:-meta_predicate term_expansion_local(?,?),term_expansion_local0(?,?).
% :-meta_predicate user:term_expansion(?,?).

term_expansion_local(X,_):-not(compound(X)),!,fail.
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

term_expansion_local(X,Y):- compound(X),loading_module_h(CM),functor(X,F,A),term_expand_local(CM,X,F,A,Y).



term_expand_local(CM,X,F,A,Y):-findall(Y,term_expand_local_each(CM,X,F,A,Y),Ys), Ys == [],!,fail.  

term_expand_local_each(_,_,F,A,_):- member(F/A,[never_expand]),!,fail.
term_expand_local_each(CM,X,F,A,X):-registered_module_type(CM,utility),export(F/A).
term_expand_local_each(CM,X,F,A,X):-registered_module_type(CM,dynamic),dynamic(F/A).

term_expansion_local0(A,B):- compound(A),term_expansion_local(A,B),!.

moo:decl_dbase_pred(Pred,Flags):-dbase:define_db_prop(Pred,Flags).

% user:term_expansion(X,Y):- term_expansion_local0(X,Y).

%:- include(logicmoo('vworld/moo_header.pl')).

:- register_module_type(utility).

moo:agent_text_command(_Agent,_Text,_AgentTarget,_Cmd):-fail.

:- include(logicmoo('dbase/dbase.pl')).

%:- include(logicmoo('vworld/moo_footer.pl')).

