end_of_file.

/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
:- if(\+ current_module(mpred_pfc)).

:- module(mpred_pfc_d, []).

:- export((



pp_cur_DB_Current/0, 

          all_different_head_vals/1,all_different_head_vals_2/2,


mpred_run_resume/0,mpred_run_pause/0,

 to_addable_form_wte/3,
 with_mpred_trace_exec/1,
 to_predicate_isas_each/2,
 to_addable_form/2,
 attvar_op/2,
 no_side_effects/1,
 mpred_rule_hb_0/3,
 retract_eq_quitely_f/1,
 mpred_fwc0/1,
 mpred_remove1/2,
 mpred_non_neg_literal/1,
 to_predicate_isas0/2,

 (::::)/2, (<-)/2, (<==>)/2, (==>)/2,action_is_undoable/1,
  mpred_assumption/1,mpred_assumptions/2,mpred_axiom/1,bagof_or_nil/3,bases_union/2,brake/1,basePFC:bt/3,build_rhs/2,
  build_neg_test/3,build_rule/3,build_test/2,build_trigger/3, 

   
  
  defaultmpred_select/1,fc_eval_action/2,foob/1,
  foreachl_do/2,get_next_fact/1,if_missing/2,mpred_justification/2,justifications/2,mpred_BC/1,mpred_BC_CACHE/1,mpred_CALL/1,
  mpred_CALL/2,mpred_CALL/3,mpred_CALL_MI/3,mpred_halt/0,mpred_halt/1,mpred_halt/2,basePFC:hs/1,mpred_action/2,
  mpred_ain/1,mpred_ain/2,mpred_ain_DbToHead/2,mpred_ain_actiontrace/2,mpred_ain_special_support/2,mpred_add_support/2,mpred_ain_trigger_reprop/2,mpred_ain_by_type/2,
  mpred_ask/2,mpred_assert/2,mpred_asserta/2,mpred_assertz/2,mpred_basis_list/2,mpred_bt_pt_combine/3,mpred_child/2,mpred_children/2,
  mpred_classifyFacts/4,mpred_clause_u/1,mpred_collect_supports/1,mpred_unhandled_command/3,mpred_compile_rhs_term/2,mpred_conjoin/3,mpred_connective/1,mpred_current_db/1,
  mpred_database/1,mpred_database_item/1,mpred_database_term/1,mpred_db_type/2,mpred_debugging/0,mpred_default_setting/2,mpred_define_bc_rule/3,mpred_descendant/2,
  mpred_descendants/2,mpred_do_and_undo_method/2,mpred_enqueue/2,mpred_error/1,mpred_error/2,mpred_eval_lhs/2,mpred_eval_rhs/2,mpred_fact/1,
  mpred_fact/2,mpred_facts/1,mpred_facts/2,mpred_facts/3,mpred_fwc/1,mpred_get_support/2,mpred_get_trigger_quick/1,mpred_get_trigger_quick/2,get_mpred_is_tracing/1,
  mpred_is_tracing_exec/0,mpred_literal/1,mpred_load/1,mpred_make_supports/1,mpred_ain_object/1,mpred_aina/2,mpred_ainz/2,
  mpred_negated_literal/1,mpred_negation/2,mpred_nf/2,mpred_nf1_negation/2,mpred_nf_negation/2,mpred_nf_negations/2,mpred_noTrace/0,mpred_noWatch/0,
  mpred_nospy/0,mpred_nospy/1,mpred_nospy/3,mpred_positive_literal/1,mpred_post/2,basePFC:qu/0,basePFC:qu/1,mpred_rem_actionTrace/1,
  mpred_rem_support/2,mpred_remove_old_version/1,mpred_remove_supports/1,mpred_remove_supports_quietly/1,mpred_reset/0,mpred_retract/1,mpred_retract_i_or_warn/1,mpred_retract_supported_relations/1,
  mpred_retract_type_1/2,mpred_run/0,basePFC:sm/1,mpred_select_hook/1,mpred_select_justification_node/3,mpred_set_warnings/1,mpred_pp_cur_justifications/2,mpred_is_spying_pred/2,
  mpred_spy/1,mpred_spy/2,mpred_spy/3,mpred_step/0,mpred_support_relation/1,mpred_supported/1,mpred_supported/2,mpred_test/1,
  basePFC:tms/1,mpred_trace/0,mpred_trace/1,mpred_trace/2,mpred_trace_add_print/2,mpred_trace_break/2,mpred_trace_exec/0,mpred_trace_mpred_ain/1,
  mpred_trace_mpred_ain/2,mpred_trace_msg/1,mpred_trace_msg/2,mpred_trace_rem/1,mpred_trigger_key/2,mpred_trigger_key/2,mpred_undo/1,mpred_unfwc/1,
  mpred_unfwc_check_triggers/1,mpred_union/3,mpred_unique_u/1,mpred_untrace/0,mpred_untrace/1,mpred_warn/0,mpred_warn/1,
  mpred_warn/2,mpred_warnings/1,mpred_watch/0,well_founded_0/2,mpred_why/0,mpred_why/1,mpred_whyBrouse/2,mpred_handle_why_command/3,
  nmpred_warn/0,basePFC:nt/4,pfcl_do/1,
  % pp_cur_DB/0,
  pp_cur_facts/0,pp_cur_facts/1,pp_cur_facts/2,pp_cur_items/1,pp_cur_rules/0,pp_cur_supports/0,pp_cur_triggers/0,
  mpred_load/1,process_rule/3,basePFC:pt/3,
  remove_if_unsupported/1,remove_selection/1,
  select_next_fact/1,basePFC:spft/5,stop_trace/1,supporters_list/2,triggerSupports/2,trigger_trigger/3,well_founded/1,
  well_founded_list/2,why_buffer/2,

mpred_call_0/1,fix_negations/2,
is_side_effect_disabled/0,
call_with_bc_triggers/1,
% justification/2,

mpred_withdraw1/2,

ain_fast/1,
assert_u/1,
check_context_module/0,
check_never_assert/1,
clause_or_call/2,
clause_u/2,
mpred_aina/1,
compute_resolve/3,
cwc/0,
get_mpred_is_tracing/1,
has_body_atom/2,
has_cl/1,
is_action_body/1,
is_bc_body/1,
is_fc_body/1,
is_resolved/1,
justifications/2,
mpred_clause_is_asserted/2,
map_first_arg/2,
mpred_negation_w_neg/2,
mpred_no_chaining/1,
% lmconf:mpred_provide_storage_clauses/4,
mpred_remove_file_support/1,
mpred_rule_hb/3,
mreq/1,
neg_in_code/1,
call_u/1,
retract_u/1,
retractall_u/1,
ruleBackward/2,
show_if_debug/1,
%spft/5,
supporters_list/2,
call_u/1,
with_each_item/3,
with_each_item/2,
mpred_is_silient/0,
  do_assumpts/2,fcnt/2,fcpt/2,mpred_fwc1/1,mpred_ain_rule_if_rule/1,mpred_descendant1/3,mpred_eval_rhs1/2,mpred_nf1/2,
  mpred_post1/2,mpred_withdraw/1,mpred_withdraw/2,mpred_remove/1,mpred_remove/2,mpred_pp_cur_justification1/2,mpred_pp_cur_justifications2/3,mpred_spy1/3,
  mpred_unfwc1/1,mpred_why1/1,mpred_blast/1,trigger_trigger1/2 )).

 :- meta_predicate 
        bagof_or_nil(?, ^, -),
        brake(*),
        call_u(*),
        fc_eval_action(0, ?),
        foreachl_do(0, ?),
        mpred_ain(+, +),
        mpred_aina(+, +),
        mpred_ainz(+, +),
        mpred_CALL(1, +),
        mpred_fact(?, +),
        with_each_item(1,+),
        with_each_item(2,+,+),
        pfcl_do(0).


:- (multifile user:term_expansion/2).
:- module_transparent (( bagof_or_nil/3,brake/1,fc_eval_action/2,foreachl_do/2,mpred_CALL/2,
  mpred_fact/2,pfcl_do/1  )).
:- export(( do_assumpts/2,fcnt/2,fcpt/2,mpred_fwc1/1,mpred_ain_rule_if_rule/1,mpred_descendant1/3,mpred_eval_rhs1/2,mpred_nf1/2,
  mpred_post1/2,mpred_withdraw/1,mpred_withdraw/2,mpred_remove/1,mpred_remove/2,mpred_pp_cur_justification1/2,mpred_pp_cur_justifications2/3,mpred_spy1/3,
  mpred_unfwc1/1,mpred_why1/1,mpred_blast/1,trigger_trigger1/2  )).
:- dynamic ((  (::::)/2, (<-)/2, (<==>)/2, (==>)/2,basePFC:bt/3,foob/1,if_missing/2,basePFC:hs/0,
  mpred_action/2,mpred_database/1,mpred_debugging/0,mpred_do_and_undo_method/2,get_mpred_is_tracing/1,mpred_is_tracing_exec/0,basePFC:qu/1,basePFC:sm/1,
  mpred_select_hook/1,mpred_is_spying_pred/2,basePFC:tms/1,mpred_warnings/1,basePFC:nt/4,basePFC:pt/3,basePFC:spft/5,
  why_buffer/2  )).
:- multifile((  (::::)/2, (<-)/2, (<==>)/2, (==>)/2,basePFC:bt/3,foob/1,if_missing/2,basePFC:hs/0,
  mpred_action/2,mpred_database/1,mpred_debugging/0,mpred_do_and_undo_method/2,get_mpred_is_tracing/1,mpred_is_tracing_exec/0,basePFC:qu/1,basePFC:sm/1,
  mpred_select_hook/1,mpred_is_spying_pred/2,basePFC:tms/1,mpred_warnings/1,basePFC:nt/4,basePFC:pt/3,basePFC:spft/5,user:term_expansion/2,
  why_buffer/2  )).



:- include(mpred_pfc_utils).
:- source_location(S,_),prolog_load_context(module,M),forall(source_file(M:H,S),(functor(H,F,A),M:module_transparent(M:F/A),M:export(M:F/A))).

% =================================================
% ==============  UTILS BEGIN        ==============
% =================================================
isSlot(V):- is_ftVar(V).

%% with_each_item(+P2,+HT,+S) semidet.
%
% Call P(E,S). each Element in the list.
%
with_each_item(P,HV,S):- var(HV),!,call(P,HV,S).
with_each_item(P,M:HT,S) :- !,must_be(atom,M),M:with_each_item(P,HT,S).
with_each_item(P,[H|T],S) :- !, call(P,H,S), with_each_item(P,T,S).
with_each_item(P,(H,T),S) :- !,with_each_item(P,H,S), with_each_item(P,T,S).
with_each_item(P,H,S) :- call(P,H,S).

%% with_each_item(+P2,+HT) semidet.
%
% Call P(E). each Element in the list.
%
with_each_item(P,HV):- var(HV),!,call(P,HV).
with_each_item(P,M:HT) :- !,must_be(atom,M),M:with_each_item(P,HT).
with_each_item(P,[H|T]) :- !, call(P,H), with_each_item(P,T).
with_each_item(P,(H,T)) :- !,with_each_item(P,H), with_each_item(P,T).
with_each_item(P,H) :- call(P,H).

% =================================================
% ==============  UTILS END          ==============
% =================================================

:- style_check(+singleton).
%   File   : mpred_syntax.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Purpose: syntactic sugar for Pfc - operator definitions and term expansions.

:- op(500,fx,'-').
:- op(300,fx,'~').
:- op(1050,xfx,('==>')).
:- op(1050,xfx,'<==>').
:- op(1050,xfx,('<-')).
:- op(1100,fx,('==>')).
:- op(1150,xfx,('::::')).


:- use_module(library(lists)).

:- dynamic ('==>')/2.
:- dynamic ('::::')/2.
:- dynamic '<==>'/2.
:- dynamic '<-'/2.
:- dynamic basePFC:pt/3.
:- dynamic basePFC:nt/4.
:- dynamic basePFC:bt/3.
:- dynamic mpred_do_and_undo_method/2.
:- dynamic mpred_action/2.
:- dynamic basePFC:tms/1.
:- dynamic basePFC:qu/1.
:- dynamic mpred_database/1.
:- dynamic basePFC:hs/1.
:- dynamic mpred_debugging/0.
:- dynamic mpred_select_hook/1.
:- dynamic basePFC:sm/1.

% mpred_current_db(fooo).

:- meta_predicate brake(*).
:- meta_predicate fc_eval_action(*,*).
:- meta_predicate foreachl_do(0,*).
:- meta_predicate pfcl_do(0).
:- meta_predicate mpred_fact(*,*).
:- meta_predicate bagof_or_nil(?,^,-).
:- meta_predicate mpred_CALL(1,*).


:- dynamic('user:term_expansion'/2).
:- multifile('user:term_expansion'/2).
:- dynamic((basePFC:spft/5,why_buffer/2)).
:- asserta(basePFC:spft(umt,foooo,u,u,'src')).
:- asserta(foooo).

/*
user:term_expansion((P==>Q),(:- mpred_ain((P==>Q)))).
%user:term_expansion((P==>Q),(:- mpred_ain(('<-'(Q,P))))).  % speed-up attempt
user:term_expansion(('<-'(P,Q)),(:- mpred_ain(('<-'(P,Q))))).
user:term_expansion((P<==>Q),(:- mpred_ain((P<==>Q)))).
user:term_expansion((_ruleName :::: Rule),(:- mpred_ain((_ruleName :::: Rule)))).
user:term_expansion((==>P),(:- mpred_ain(P))).
*/

%  predicates to examine the state of mpred_

basePFC:qu:- listing(basePFC:qu/1).


%   File   : mpred_core.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated: 10/11/87, ...
%            4/2/91 by R. McEntire: added calls to valid_dbref as a
%                                   workaround for the Quintus 3.1
%                                   bug in the recorded database.
%   Purpose: core Pfc predicates.


% % initialization of global assertons 

%  mpred_default_setting/1 initialized a global assertion.
%   mpred_default_setting(P,Q) - if there is any fact unifying with P, then do 
%   nothing, else assert Q.

mpred_default_setting(GeneralTerm,Default):-
  clause_u(GeneralTerm,true) -> true ; assert_u(Default).


%% ruleBackward( +R, ?Condition) is semidet.
%
% Rule Backward.
%
ruleBackward(R,Condition):- call_u(( ruleBackward0(R,Condition),functor(Condition,F,_),\+ arg(_,v(call_prologsys,call_u),F))).
%ruleBackward0(F,Condition):-clause_u(F,Condition),\+ (is_true(Condition);mpred_is_info(Condition)).

%% ruleBackward0( +F, ?Condition) is semidet.
%
% Rule Backward Primary Helper.
%
ruleBackward0(F,Condition):- call_u((  '<-'(F,Condition),\+ (is_true(Condition);mpred_is_info(Condition)) )).


%% mpred_ainz( +G, ?S) is semidet.
%
% PFC Ainz.
%
mpred_ainz(G):-mpred_ain(G).
mpred_ainz(G,S):-mpred_ain(G,S).

%% mpred_aina( +G, ?S) is semidet.
%
% PFC Aina.
%
mpred_aina(G):-mpred_ain(G).
mpred_aina(G,S):-mpred_ain(G,S).


%%  mpred_ain(P,S) 
%
%  asserts P into the dataBase with support from S.
%
%  mpred_ain/2 and mpred_post/2 are the proper ways to add new clauses into the
%  database and have forward reasoning done.
%
mpred_ain(P):- get_source_ref(UU), mpred_ain(P,UU).


mpred_ain(P,S):- 
  must(to_addable_form_wte(assert,P,P0)),with_each_item(mpred_post1,P0,S),
  mpred_run.
%mpred_ain(_,_).
mpred_ain(P,S):- mpred_warn("mpred_ain(~p,~p) failed",[P,S]).



ain_fast(P):- get_source_ref(UU), ain_fast(P,UU).
ain_fast(P,S):- 
  with_each_item(mpred_post1,P,S),
  mpred_run.




%% mpred_post(+Ps,+S) 
%
% tries to assert a fact or set of fact to the database.  For
% each fact (or the singleton) mpred_post1 is called. It always succeeds.
%
mpred_post(P, S):- must(to_addable_form_wte(assert,P,P0)),with_each_item(mpred_post1,P0,S).

%% mpred_post1(+P,+S) is det.
%
% tries to add a fact to the database, and, if it succeeded,
% adds an entry to the Pfc queue for subsequent forward chaining.
% It always succeeds.
%
mpred_post1(    P,   S):- fixed_negations(P,P0),!, mpred_post1( P0,   S).
mpred_post1(( \+P ), S):- !, must_be(nonvar,P) ->  mpred_withdraw(P, S).
mpred_post1(   MP,   S):- 
  strip_module(MP,_M,P),
  %  db mpred_ain_db_to_head(P,P2),
  % mpred_remove_old_version(P),
  mpred_add_support(P,S),
  mpred_unique_u(P),
  assert_u(P),
  mpred_trace_op(add,P,S),
  !,
  mpred_enqueue(P,S),
  !.

mpred_post1(_,_).
% mpred_post1(P,S):-  mpred_warn("mpred_ain(~p,~p) failed",[P,S]).

 
%%  mpred_ain_db_to_head(+P,-NewP) is semidet.
% takes a fact P or a conditioned fact
%  (P:-C) and adds the Db context.
% 
mpred_ain_db_to_head(P,NewP):-
  lookup_u(mpred_current_db(Db)),
  (Db=true        -> NewP = P;
   P=(Head:-Body) -> NewP = (Head:- (Db,Body));
   otherwise      -> NewP = (P:- Db)).




%% mpred_unique_u( +P) is semidet.
%
% PFC Unique For Internal Interface.
%
% mpred_unique_u(X) is true if there is no assertion X in the prolog db.
mpred_unique_u((Head:-Tail)):- !, \+ clause_u(Head,Tail).
mpred_unique_u(P):- !, \+ clause_u(P,true).


get_search_mode(_P,_S,Mode):- lookup_u(sm(Mode)),!.
get_search_mode(_P,_S,Mode):- must(Mode=direct),!.

mpred_enqueue(P,S):-
  get_user_tbox(TBOX),
  (get_search_mode(P,S,Mode)
    -> (Mode=direct  -> mpred_fwc(P) ;
	Mode=depth   -> mpred_asserta_w_support(qu(TBOX,P,S),S) ;
	Mode=breadth -> mpred_assert_w_support(qu(TBOX,P,S),S) ;
	true         -> mpred_error("Unrecognized sm mode: ~p", Mode))
     ; mpred_error("No sm mode")).


%% mpred_remove_old_version( :TermIdentifier) is semidet.
%
% if there is a rule of the form Identifier ::: Rule then delete it.
%
mpred_remove_old_version((Identifier::::Body)):-
  % this should never happen.
  var(identifier),
  !,
  mpred_warn("variable used as an  rule name in ~p :::: ~p",
          [Identifier,Body]).

  
mpred_remove_old_version((Identifier::::Body)):-
  nonvar(Identifier),
  clause_u((Identifier::::OldBody),_),
  \+(Body=OldBody),
  mpred_withdraw((Identifier::::OldBody)),
  !.
mpred_remove_old_version(_).



% mpred_run compute the deductive closure of the current database. 
% How this is done depends on the searching mode:
%    direct -  mpred_fwc has already done the job.
%    depth or breadth - use the queue mechanism.

% mpred_run :- lookup_u(sm(direct)),!.
% mpred_run :- repeat, \+ mpred_step, !.
mpred_run:-
%  (\+ lookup_u(sm(direct))),
  mpred_step,
  mpred_run.
mpred_run.


% mpred_step removes one entry from the queue and reasons from it.


mpred_step:-  
  % if hs/1 is true, reset it and fail, thereby stopping inferencing.
  lookup_u(hs(Was)),
  mpred_retract(hs(Was)),
  mpred_trace_msg('Stopping on: ~p',[hs(Was)]),
  !, 
  fail.

mpred_step:-
  % draw immediate conclusions from the next fact to be considered.
  % fails iff the queue is empty.
  get_next_fact(P),
  pfcl_do(mpred_fwc(P)),
  !.

get_next_fact(P):-
  %identifies the nect fact to mpred_fwc from and removes it from the queue.
  select_next_fact(P),
  remove_selection(P).

remove_selection(P):- 
  clause(basePFC:qu(P),B,Ref),call(B),!,erase(Ref),
  mpred_remove_supports_quietly(basePFC:qu(P)),
  !.
remove_selection(P):-
  brake(format("~Nmpred_:get_next_fact - selected fact not on Queue: ~p",
               [P])).


% select_next_fact(P) identifies the next fact to reason from.  
% It tries the user defined predicate first and, failing that, 
%  the default mechanism.

select_next_fact(P):- 
  mpred_select_hook(P),
  !.  
select_next_fact(P):- 
  defaultmpred_select(P),
  !.  

% the default selection predicate takes the item at the froint of the queue.
defaultmpred_select(P):- basePFC:qu(P),!.

% mpred_halt stops the forward chaining.
mpred_halt:-  mpred_halt(anonymous(mpred_halt)).

mpred_halt(Format,Args):- format(string(Now),Format,Args), mpred_halt(Now).

mpred_halt(Now):-
  mpred_trace_msg("New halt signal ",[Now]),
  (basePFC:hs(Was) -> 
       mpred_warn("mpred_halt finds halt signal already set to: ~p ",[Was])
     ; assert_u(basePFC:hs(Now))).


stop_trace(Msg):- notrace((tracing,leash(+all),dtrace(dmsg(Msg)))),!,rtrace.
stop_trace(Msg):- dtrace(dmsg(Msg)).


% 
%  predicates for manipulating triggers
% 


mpred_ain_trigger_reprop(basePFC:pt(umt,Trigger,Body),Support):-
  !,
   mpred_trace_msg('~N~n\tAdding positive~n\t\ttrigger: ~p~n\t\tbody: ~p~n\t Support: ~p~n',
                 [Trigger,Body,Support]),
  mpred_assert(basePFC:pt(umt,Trigger,Body),Support),
  copy_term(basePFC:pt(umt,Trigger,Body),Tcopy),
  mpred_BC(Trigger),
  mpred_eval_lhs(Body,(Trigger,Tcopy)),
  fail.


mpred_ain_trigger_reprop(basePFC:nt(umt,Trigger,Test,Body),Support):-
  !,
  mpred_trace_msg('~N~n\tAdding negative~n\t\ttrigger: ~p~n\t\ttest: ~p~n\t\tbody: ~p~n\t Support: ~p~n',
		[Trigger,Test,Body,Support]),
  copy_term(Trigger,TriggerCopy),  
  mpred_assert(basePFC:nt(umt,TriggerCopy,Test,Body),Support),
 %  stop_trace(mpred_assert(basePFC:nt(umt,TriggerCopy,Test,Body),Support)),
  \+Test,
  mpred_eval_lhs(Body,((\+Trigger),basePFC:nt(umt,TriggerCopy,Test,Body))).

mpred_ain_trigger_reprop(basePFC:bt(umt,Trigger,Body),Support):-
  !,
   mpred_trace_msg('~N~n\tAdding backwards~n\t\ttrigger: ~p~n\t\tbody: ~p~n\t Support: ~p~n',
                 [Trigger,Body,Support]),
  mpred_assert(basePFC:bt(umt,Trigger,Body),Support),
  mpred_bt_pt_combine(Trigger,Body,Support).

mpred_ain_trigger_reprop(X,Support):-
  mpred_warn("Unrecognized trigger to mpred_ain_trigger_reprop: ~p\n~~p~n",[X,Support]).


mpred_bt_pt_combine(Head,Body,Support):- 
  %  a backward trigger (basePFC:bt/3) was just added with head and Body and support Support
  %  find any basePFC:pt/3''s with unifying heads and add the instantied basePFC:bt/3 body.
  mpred_get_trigger_quick(basePFC:pt(umt,Head,Body)),
  mpred_eval_lhs(Body,Support),
  fail.
mpred_bt_pt_combine(_,_,_):- !.

mpred_get_trigger_quick(Trigger):-  clause_u(Trigger,true).
mpred_get_trigger_quick(_ABOX,Trigger):-  clause_u(Trigger,true).


% 
%  predicates for manipulating action traces.
% 

mpred_ain_actiontrace(Action,Support):- 
  % adds an action trace and it''s support.
  mpred_add_support(mpred_action(Action),Support).

mpred_rem_actionTrace(mpred_action(A)):-
  mpred_do_and_undo_method(A,M),
  M,
  !.


%%  mpred_retract(X) is det.
%
%  predicates to remove Pfc facts, triggers, action traces, and queue items
%  from the database.
%
mpred_retract(X):- 
  %  retract an arbitrary thing.
  mpred_db_type(X,Type),!,
  mpred_retract_type_1(Type,X),
  !.

mpred_retract_type_1(fact,X):-   
  %  db mpred_ain_DbToHead(X,X2), retract_u(X2). 
  % stop_trace(mpred_retract_type_1(fact,X)),
  (retract_u(X) 
   *-> mpred_unfwc(X) ; mpred_unfwc(X)).

mpred_retract_type_1(rule,X):- 
  %  db  mpred_ain_DbToHead(X,X2),  retract_u(X2).
  retract_u(X).

mpred_retract_type_1(trigger,X):- 
  retract_u(X)
    -> mpred_unfwc(X)
     ; mpred_warn("Trigger not found to retract_u: ~p",[X]).

mpred_retract_type_1(action,X):- mpred_rem_actionTrace(X).
  

%%  mpred_ain_object(X) 
%
% adds item X to some database
%
mpred_ain_object(X):-
  % what type of X do we have?
  mpred_db_type(X,Type),
  % call the appropriate predicate.
  mpred_ain_by_type(Type,X).

mpred_ain_by_type(fact,X):- 
  mpred_unique_u(X), 
  assert_u(X),!.
mpred_ain_by_type(rule,X):- 
  mpred_unique_u(X), 
  assert_u(X),!.
mpred_ain_by_type(trigger,X):- 
  assert_u(X).
mpred_ain_by_type(action,_ZAction):- !.


  

%%  mpred_withdraw(P).
%  removes support S from P and checks to see if P is still supported.
%  If it is not, then the fact is retreactred from the database and any support
%  relationships it participated in removed.

mpred_withdraw(Ps):- get_source_ref(UU), mpred_withdraw(Ps,UU).

/*
mpred_withdraw(List):- 
  % iterate down the list of facts to be withdrawn.
  nonvar(List),
  List=[_|_],
  remlist(List).
  
mpred_withdraw(P):- 
  % mpred_withdraw/1 is the user''s interface - it withdraws user support for P.
  mpred_withdraw(P,(u,u)).

remlist([H|T]):-
  % mpred_withdraw each element in the list.
  mpred_withdraw(H,(u,u)),
  remlist(T).
*/

%%  mpred_withdraw(P,S) is det.
% removes support S from P and checks to see if P is still supported.
%  If it is not, then the fact is retreactred from the database and any support
%  relationships it participated in removed.
mpred_withdraw(Ps,S):- with_each_item(mpred_withdraw1,Ps,S).
mpred_withdraw1(P,S):-
  mpred_trace_msg('~N~n\tRemoving~n\t\tsupport: ~p~n\t\tfrom: ~p~n',[S,P]),
  mpred_rem_support(P,S)
     -> remove_if_unsupported(P)
      ; mpred_warn("mpred_withdraw/2 Could not find support ~p to remove from fact ~p",
                [S,P]).

%%  mpred_remove(+P) is det.
% 
%  mpred_remove is like mpred_withdraw, but if P is still in the DB after removing the
%  user''s support, it is retracted by more forceful means (e.g. remove).
% 
mpred_remove(P):- get_source_ref(UU), mpred_remove(P,UU).
mpred_remove(P,S):- with_each_item(mpred_remove1,P,S).
mpred_remove1(P,S):-
  mpred_withdraw(P,S),
  mpred_BC(P)
     -> mpred_blast(P) 
      ; true.

% 
%  mpred_blast(+F) retracts fact F from the DB and removes any dependent facts
% 

mpred_blast(F):- 
  mpred_remove_supports(F),
  mpred_undo(F).


% removes any remaining supports for fact F, complaining as it goes.

mpred_remove_supports(F):- 
  mpred_rem_support(F,S),
  mpred_warn("~p was still supported by ~p",[F,S]),
  fail.
mpred_remove_supports(_).

mpred_remove_supports_quietly(F):- 
  mpred_rem_support(F,_),
  fail.
mpred_remove_supports_quietly(_).

%% mpred_undo(X) undoes X.
%
% - a positive or negative trigger.
% - an action by finding a method and successfully executing it.
% - or a random fact, printing out the trace, if relevant.
%
mpred_undo(mpred_action(A)):-  
  % undo an action by finding a method and successfully executing it.
  !,
  mpred_rem_actionTrace(mpred_action(A)).

mpred_undo(basePFC:pt(umt,Key,Head,Body)):-  
  % undo a positive trigger.
  %
  !,
  (show_success(retract_u(basePFC:pt(umt,Key,Head,Body)))
    -> mpred_unfwc(basePFC:pt(umt,Head,Body))
     ; mpred_warn("Trigger not found to undo: ~p",[basePFC:pt(umt,Head,Body)])).

mpred_undo(basePFC:pt(umt,Head,Body)):- fail,
  % undo a positive trigger.
  %
  !,
  (show_success(retract_u(basePFC:pt(umt,Head,Body)))
    -> mpred_unfwc(basePFC:pt(umt,Head,Body))
     ; mpred_warn("Trigger not found to undo: ~p",[basePFC:pt(umt,Head,Body)])).

mpred_undo(basePFC:nt(umt,Head,Condition,Body)):-  
  % undo a negative trigger.
  !,
  (show_success(retract_u(basePFC:nt(umt,Head,Condition,Body)))
    -> mpred_unfwc(basePFC:nt(umt,Head,Condition,Body))
     ; mpred_warn("Trigger not found to undo: ~p",[basePFC:nt(umt,Head,Condition,Body)])).

mpred_undo(Fact):-
  % undo a random fact, printing out the trace, if relevant.
  retract_u(Fact),
  mpred_trace_rem(Fact),
  mpred_unfwc(Fact).
  


%%  mpred_unfwc(+P) 
%
% "un-forward-chains" from fact P.  That is, fact P has just
%  been removed from the database, so remove all support relations it
%  participates in and check the things that they support to see if they
%  should stayuser in the database or should also be removed.
%
mpred_unfwc(F):- 
  mpred_retract_supported_relations(F),
  mpred_unfwc1(F).

mpred_unfwc1(F):-
  mpred_unfwc_check_triggers(F),
  % is this really the right place for mpred_run<?
  mpred_run.


mpred_unfwc_check_triggers(F):-
  mpred_db_type(F,fact),
  copy_term(F,Fcopy),
  basePFC:nt(umt,Fcopy,Condition,Action),
  (\+ Condition),
  mpred_eval_lhs(Action,((\+F),basePFC:nt(umt,F,Condition,Action))),
  fail.
mpred_unfwc_check_triggers(_).

mpred_retract_supported_relations(Fact):-
  mpred_db_type(Fact,Type),
  (Type=trigger -> mpred_rem_support(P,(_,Fact))
                ; mpred_rem_support(P,(Fact,_))),
  remove_if_unsupported(P),
  fail.
mpred_retract_supported_relations(_).



%  remove_if_unsupported(+Ps) checks to see if all Ps are supported and removes
%  it from the DB if they are not.
remove_if_unsupported(P):- 
   mpred_supported(P) -> true ;  mpred_undo(P).






%%  mpred_fwc(+X) 
%
% forward chains from a fact or a list of facts X.
% 
mpred_fwc(Ps):- with_each_item(mpred_fwc0,Ps).
mpred_fwc0(Fact):- loop_check(mpred_fwc1(Fact),true).

% mpred_fwc1(+P) forward chains for a single fact.

mpred_fwc1(P):-
  strip_module(P,_M,Fact),
  mpred_ain_rule_if_rule(Fact),
  copy_term(Fact,F),
  % check positive triggers
  fcpt(Fact,F),
  % check negative triggers
  fcnt(Fact,F).


% 
%  mpred_ain_rule_if_rule(P) does some special, built in forward chaining if P is
%  a rule.
%  

mpred_ain_rule_if_rule((P==>Q)):-  
  !,  
  process_rule(P,Q,(P==>Q)).
mpred_ain_rule_if_rule((Name::::P==>Q)):- 
  !,  
  process_rule(P,Q,(Name::::P==>Q)).
mpred_ain_rule_if_rule((P<==>Q)):- 
  !, 
  process_rule(P,Q,(P<==>Q)), 
  process_rule(Q,P,(P<==>Q)).
mpred_ain_rule_if_rule((Name::::P<==>Q)):- 
  !, 
  process_rule(P,Q,((Name::::P<==>Q))), 
  process_rule(Q,P,((Name::::P<==>Q))).

mpred_ain_rule_if_rule(('<-'(P,Q))):-
  !,
  mpred_define_bc_rule(P,Q,('<-'(P,Q))).

mpred_ain_rule_if_rule(_).


fcpt(Fact,F):- 
  mpred_get_trigger_quick(basePFC:pt(umt,F,Body)),
  mpred_trace_msg('~N~n\tFound positive trigger: ~p~n\t\tbody: ~p~n',
		[F,Body]),
  mpred_eval_lhs(Body,(Fact,basePFC:pt(umt,F,Body))),
  fail.

%fcpt(Fact,F):- 
%  mpred_get_trigger_quick(basePFC:pt(umt,presently(F),Body)),
%  mpred_eval_lhs(Body,(presently(Fact),basePFC:pt(umt,presently(F),Body))),
%  fail.

fcpt(_,_).

fcnt(_ZFact,F):-
  basePFC:spft(umt,X,_,basePFC:nt(umt,F,Condition,Body),_ZSourceRef),
  Condition,
  mpred_withdraw(X,(_,basePFC:nt(umt,F,Condition,Body))),
  fail.
fcnt(_,_).


% 
%  mpred_define_bc_rule(+Head,+Body,+Parent_rule) - defines a backeard
%  chaining rule and adds the corresponding basePFC:bt/3 triggers to the database.
% 

mpred_define_bc_rule(Head,_ZBody,Parent_rule):-
  (\+ mpred_literal(Head)),
  mpred_warn("Malformed backward chaining rule.  ~p not atomic.",[Head]),
  mpred_warn("rule: ~p",[Parent_rule]),
  !,
  fail.

mpred_define_bc_rule(Head,Body,Parent_rule):-
  copy_term(Parent_rule,Parent_ruleCopy),
  build_rhs(Head,Rhs),
  foreachl_do(mpred_nf(Body,Lhs),
          (build_trigger(Lhs,rhs(Rhs),Trigger),
           mpred_ain(basePFC:bt(umt,Head,Trigger),(Parent_ruleCopy,u)))).
 



% 
%  eval something on the LHS of a rule.
% 

 
mpred_eval_lhs((Test->Body),Support):-  
  !, 
  (call(Test) -> mpred_eval_lhs(Body,Support)),
  !.

mpred_eval_lhs(rhs(X),Support):-
  !,
  mpred_eval_rhs(X,Support),
  !.

mpred_eval_lhs(X,Support):-
  mpred_db_type(X,trigger),
  !,
  mpred_ain_trigger_reprop(X,Support),
  !.

%mpred_eval_lhs(snip(X),Support):- 
%  snip(Support),
%  mpred_eval_lhs(X,Support).

mpred_eval_lhs(X,_):-
  mpred_warn("Unrecognized item found in trigger body, namely ~p.",[X]).


% 
%  eval something on the RHS of a rule.
% 

mpred_eval_rhs([],_):- !.
mpred_eval_rhs([Head|Tail],Support):- 
  mpred_eval_rhs1(Head,Support),
  mpred_eval_rhs(Tail,Support).


mpred_eval_rhs1({Action},Support):-
 % evaluable Prolog code.
 !,
 fc_eval_action(Action,Support).

mpred_eval_rhs1(P,_ZSupport):-
 % predicate to remove.
 mpred_negated_literal(P),
 !,
 mpred_withdraw(P).

mpred_eval_rhs1([X|Xrest],Support):-
 % embedded sublist.
 !,
 mpred_eval_rhs([X|Xrest],Support).

mpred_eval_rhs1(Assertion,Support):-
 % an assertion to be added.
 mpred_post1(Assertion,Support).


mpred_eval_rhs1(X,_):-
  mpred_warn("Malformed rhs of a rule: ~p",[X]).



%% fc_eval_action(+Action,+Support)
%
%  evaluate an action found on the rhs of a rule.
% 

fc_eval_action(Action,Support):-
  call(Action), 
  (action_is_undoable(Action) 
     -> mpred_ain_actiontrace(Action,Support) 
      ; true).


% 
%  
% 

trigger_trigger(Trigger,Body,_ZSupport):-
 trigger_trigger1(Trigger,Body).
trigger_trigger(_,_,_).


%trigger_trigger1(presently(Trigger),Body):-
%  !,
%  copy_term(Trigger,TriggerCopy),
%  mpred_BC(Trigger),
%  mpred_eval_lhs(Body,(presently(Trigger),basePFC:pt(umt,presently(TriggerCopy),Body))),
%  fail.

trigger_trigger1(Trigger,Body):-
  copy_term(Trigger,TriggerCopy),
  mpred_BC(Trigger),
  mpred_eval_lhs(Body,(Trigger,basePFC:pt(umt,TriggerCopy,Body))),
  fail.



%%  mpred_BC(F) is det.
% 
%  is true iff F is a fact available for forward chaining 
%  (or from the backchaining store)
%  Note that this has the side effect of catching unsupported facts and
%  assigning them support from God.
% 
mpred_BC(P):-mpred_BC_CACHE(P),mpred_CALL(mpred_BC, P).
mpred_BC_CACHE(P):-
 ignore((
  % trigger any bc rules.
  basePFC:bt(umt,P,Trigger),
  mpred_get_support(basePFC:bt(umt,P,Trigger),S),
  mpred_eval_lhs(Trigger,S),
  fail)).

mpred_CALL(F):- mpred_CALL(mpred_CALL, Cut, F), (var(Cut)->true;(Cut=cut(Cut)->(!,Cut);Cut)).

mpred_CALL(How,F):- mpred_CALL(How, Cut, F), (var(Cut)->true;(Cut=cut(Cut)->(!,Cut);Cut)).

mpred_CALL(How,SCut, F):- 
  %  this is probably not advisable due to extreme inefficiency.
  var(F) ->  mpred_fact(F) ;
  loop_check_term((predicate_property(F,number_of_clauses(_)) -> 
     (clause_u(F,Condition),mpred_CALL(How,Cut,Condition),(var(Cut)->true;(Cut=cut(Cut)->(!,Cut);Cut)));
  mpred_CALL_MI(How,SCut,F)),call(How,F),fail).

mpred_CALL_MI(_How, cut(true), !):- !.
mpred_CALL_MI(How, Cut, (P1,P2)):- !, mpred_CALL(How, Cut, P1), mpred_CALL(How, Cut, P2).
mpred_CALL_MI(How, Cut, (P1;P2)):- !, mpred_CALL(How, Cut, P1); mpred_CALL(How, Cut, P2).
mpred_CALL_MI(How, Cut, (P1->P2)):- !, mpred_CALL(How, Cut, P1)-> mpred_CALL(How, Cut, P2).
mpred_CALL_MI(How, Cut, (P1*->P2)):- !, mpred_CALL(How, Cut, P1)*-> mpred_CALL(How, Cut, P2).
mpred_CALL_MI(_How,_, F):- 
  %  we really need to check for system predicates as well.
  current_predicate(_,F),!, call(F).




%% action_is_undoable(?A) 
%
% an action is action_is_undoable if there exists a method for undoing it.
%
action_is_undoable(A):- mpred_do_and_undo_method(A,_).



%% mpred_nf(+In,-Out)
% 
% maps the LHR of a Pfc rule In to one normal form 
%  Out.  It also does certmpred_ain optimizations.  Backtracking into this
%  predicate will produce additional clauses.
%

mpred_nf(LHS,List):-
  mpred_nf1(LHS,List2),
  mpred_nf_negations(List2,List).


%%  mpred_nf1(+In,-Out) 
%
% maps the LHR of a Pfc rule In to one normal form
%  Out.  Backtracking into this predicate will produce additional clauses.

% handle a variable.

mpred_nf1(P,[P]):- var(P), !.

% these next two rules are here for upward compatibility and will go 
% away eventually when the P/Condition form is no longer used anywhere.

mpred_nf1(P/Cond,[(\+P)/Cond]):- mpred_negated_literal(P), !.

mpred_nf1(P/Cond,[P/Cond]):-  mpred_literal(P), !.

%  handle a negated form

mpred_nf1(NegTerm,NF):-
  mpred_negation(NegTerm,Term),
  !,
  mpred_nf1_negation(Term,NF).

%  disjunction.

mpred_nf1((P;Q),NF):- 
  !,
  (mpred_nf1(P,NF) ;   mpred_nf1(Q,NF)).


%  conjunction.

mpred_nf1((P,Q),NF):-
  !,
  mpred_nf1(P,NF1),
  mpred_nf1(Q,NF2),
  append(NF1,NF2,NF).

%  handle a random literal.

mpred_nf1(P,[P]):- 
  mpred_literal(P), 
  !.

%=% shouln't we have something to catch the rest as errors?
mpred_nf1(Term,[Term]):-
  mpred_warn("mpred_nf doesn't know how to normalize ~p",[Term]),!,fail.


%% mpred_nf1_negation( +P, ?P) is semidet.
%
%  mpred_nf1_negation(P,NF) is true if NF is the normal form of \+P.
%
mpred_nf1_negation((P/Cond),[(\+(P))/Cond]):- !.

mpred_nf1_negation((P;Q),NF):-
  !,
  mpred_nf1_negation(P,NFp),
  mpred_nf1_negation(Q,NFq),
  append(NFp,NFq,NF).

mpred_nf1_negation((P,Q),NF):- 
  % this code is not correct! twf.
  !,
  mpred_nf1_negation(P,NF) 
  ;
  (mpred_nf1(P,Pnf),
   mpred_nf1_negation(Q,Qnf),
   append(Pnf,Qnf,NF)).

mpred_nf1_negation(P,[\+P]).


%%  mpred_nf_negations(List2,List) is det.
%
% sweeps through List2 to produce List,
%  changing -{...} to {\+...}
% % ? is this still needed? twf 3/16/90

%% mpred_nf_negations( :TermX, :TermX) is semidet.
%
% PFC Normal Form Negations.
%
mpred_nf_negations(X,X) :- !.  % I think not! twell_founded_0 3/27/90

mpred_nf_negations([],[]).

mpred_nf_negations([H1|T1],[H2|T2]):-
  mpred_nf_negation(H1,H2),
  mpred_nf_negations(T1,T2).


%% mpred_nf_negation( +X, ?X) is semidet.
%
% PFC Normal Form Negation.
%
mpred_nf_negation(Form,{\+ X}):- 
  nonvar(Form),
  Form=(-({X})),
  !.
mpred_nf_negation(X,X).


 
%%  build_rhs(+Conjunction,-Rhs)
% 

build_rhs(X,[X]):- 
  var(X),
  !.

build_rhs((A,B),[A2|Rest]):- 
  !, 
  mpred_compile_rhs_term(A,A2),
  build_rhs(B,Rest).

build_rhs(X,[X2]):-
   mpred_compile_rhs_term(X,X2).


mpred_compile_rhs_term((P/C),((P:-C))):- !.
mpred_compile_rhs_term(P,P).



%% mpred_negation( +N, ?P) is semidet.
%
%  is true if N is a negated term and P is the term
%  with the negation operator stripped.
%
mpred_negation((-P),P).
mpred_negation((\+(P)),P).



%% mpred_negated_literal( +P) is semidet.
%
% PFC Negated Literal.
%
mpred_negated_literal(P):- 
  mpred_negation(P,Q),
  mpred_positive_literal(Q).

mpred_literal(X):- mpred_negated_literal(X).
mpred_literal(X):- mpred_positive_literal(X).

mpred_positive_literal(X):-   is_ftNonvar(X),
  functor(X,F,_), 
  \+ mpred_connective(F).


%% mpred_connective( +VALUE1) is semidet.
%
% PFC Connective.
%
mpred_connective(';').
mpred_connective(',').
mpred_connective('/').
mpred_connective('|').
mpred_connective(('==>')).
mpred_connective(('<-')).
mpred_connective('<==>').

mpred_connective('-').
% mpred_connective('-').
mpred_connective('\\+').


%% process_rule( +Lhs, ?Rhs, ?Parent_rule) is semidet.
%
% Process Rule.
%
process_rule(Lhs,Rhs,Parent_rule):-
  copy_term(Parent_rule,Parent_ruleCopy),
  build_rhs(Rhs,Rhs2),
  foreachl_do(mpred_nf(Lhs,Lhs2), 
          build_rule(Lhs2,rhs(Rhs2),(Parent_ruleCopy,u))).


%% build_rule( +Lhs, ?Rhs, ?Support) is semidet.
%
% Build Rule.
%
build_rule(Lhs,Rhs,Support):-
  build_trigger(Lhs,Rhs,Trigger),
  mpred_eval_lhs(Trigger,Support).

build_trigger([],Consequent,Consequent).

build_trigger([V|Triggers],Consequent,basePFC:pt(umt,V,X)):-
  var(V),
  !, 
  build_trigger(Triggers,Consequent,X).

build_trigger([(T1/Test)|Triggers],Consequent,basePFC:nt(umt,T2,Test2,X)):-
  mpred_negation(T1,T2),
  !, 
  build_neg_test(T2,Test,Test2),
  build_trigger(Triggers,Consequent,X).

build_trigger([(T1)|Triggers],Consequent,basePFC:nt(umt,T2,Test,X)):-
  mpred_negation(T1,T2),
  !,
  build_neg_test(T2,true,Test),
  build_trigger(Triggers,Consequent,X).

build_trigger([{Test}|Triggers],Consequent,(Test->X)):-
  !,
  build_trigger(Triggers,Consequent,X).

build_trigger([T/Test|Triggers],Consequent,basePFC:pt(umt,T,X)):-
  !, 
  build_test(Test,Test2),
  build_trigger([{Test2}|Triggers],Consequent,X).


%build_trigger([snip|Triggers],Consequent,snip(X)):-
%  !,
%  build_trigger(Triggers,Consequent,X).

build_trigger([T|Triggers],Consequent,basePFC:pt(umt,T,X)):-
  !, 
  build_trigger(Triggers,Consequent,X).

% 
%  build_neg_test(+,+,-).
% 
%  builds the test used in a negative trigger (basePFC:nt/4).  This test is a
%  conjunction of the check than no matching facts are in the db and any
%  additional test specified in the rule attached to this - term.
% 

build_neg_test(T,Testin,Testout):-
  build_test(Testin,Testmid),
  mpred_conjoin((mpred_BC(T)),Testmid,Testout).

  
% this just strips away any currly brackets.

build_test({Test},Test):- !.
build_test(Test,Test).




%  simple typeing for Pfc objects

mpred_db_type(Var,Type):- var(Var),!, Type=fact.
mpred_db_type(~_,Type):- !, Type=fact.
mpred_db_type(('==>'(_,_)),Type):- !, Type=rule.
mpred_db_type(('<==>'(_,_)),Type):- !, Type=rule.
mpred_db_type(('<-'(_,_)),Type):- !, Type=rule.
mpred_db_type(basePFC:pt(umt,_,_,_),Type):- !, Type=trigger.
mpred_db_type(basePFC:pt(umt,_,_),Type):- !, Type=trigger.
mpred_db_type(basePFC:nt(umt,_,_,_),Type):- !,  Type=trigger.
mpred_db_type(basePFC:bt(umt,_,_),Type):- !,  Type=trigger.
mpred_db_type(basePFC:spft(umt,_,_,_,_ZSourceRef),Type):- !, Type=support.
mpred_db_type(mpred_action(_),Type):- !, Type=action.
mpred_db_type((('::::'(_,X))),Type):- !, mpred_db_type(X,Type).
mpred_db_type(((':'(_,X))),Type):- !, mpred_db_type(X,Type).
mpred_db_type(_,fact):-
  %  if it''s not one of the above, it must be a fact!
  !.

mpred_assert(P,Support):- 
  (mpred_clause_u(P) ; assert_u(P)),
  !,
  mpred_add_support(P,Support).

mpred_asserta(P,Support):-
  (mpred_clause_u(P) ; asserta_u(P)),
  !,
  mpred_add_support(P,Support).

mpred_assertz(P,Support):-
  (mpred_clause_u(P) ; assertz_u(P)),
  !,
  mpred_add_support(P,Support).



%% mpred_clause_u( +Head) is semidet.
%
% PFC Clause For Internal Interface.
%
mpred_clause_u((Head:- Body)):-
  !,
  copy_term((Head:-Body),(Head_copy:-Body_copy)),
  clause_u(Head,Body),
  variant(Head,Head_copy),
  variant(Body,Body_copy).

mpred_clause_u(Head):-
  % find a unit clause identical to Head by finding one which unifies,
  % and then checking to see if it is identical
  copy_term(Head,Head_copy),
  clause_u(Head_copy,true),
  variant(Head,Head_copy).



%% foreachl_do( +Binder, ?Body) is semidet.
%
% Foreachl Do.
%
foreachl_do(Binder,Body):- Binder,pfcl_do(Body),fail.
foreachl_do(_,_).


%% pfcl_do( +X) is semidet.
%
% executes X once and always succeeds.
%
pfcl_do(X):- X,!.
pfcl_do(_).


%% mpred_union(L1,L2,L3) is semidet.
%
%  true if set L3 is the result of appending sets
%  L1 and L2 where sets are represented as simple lists.
%
mpred_union([],L,L).
mpred_union([Head|Tail],L,Tail2):-  
  memberchk(Head,L),
  !,
  mpred_union(Tail,L,Tail2).
mpred_union([Head|Tail],L,[Head|Tail2]):-  
  mpred_union(Tail,L,Tail2).


%  mpred_conjoin(+Conjunct1,+Conjunct2,?Conjunction).
%  arg3 is a simplified expression representing the conjunction of
%  args 1 and 2.

mpred_conjoin(true,X,X):- !.
mpred_conjoin(X,true,X):- !.
mpred_conjoin(C1,C2,(C1,C2)).



%   File   : pfcdb.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Author :  Dan Corpron
%   Updated: 10/11/87, ...
%   Purpose: predicates to manipulate a Pfc database (e.g. save,
% 	restore, reset, etc.0

% mpred_database_term(P/A) is true iff P/A is something that Pfc adds to
% the database and should not be present in an empty Pfc database

mpred_database_term(basePFC:spft/5).
mpred_database_term(basePFC:pt/3).
mpred_database_term(basePFC:bt/3).
mpred_database_term(basePFC:nt/4).
mpred_database_term(basePFC:qu/1).
mpred_database_term('==>'/2).
mpred_database_term('<==>'/2).
mpred_database_term('<-'/2).

mpred_database_term('==>'/1).
mpred_database_term('~'/1).

:- forall(mpred_database_term(T),shared_multifile(T)).

%% mpred_reset() is det.
%
% removes all forward chaining rules and justifications from db.
%
mpred_reset:-
 mpred_trace_msg("Reseting DB"),
  clause_u(basePFC:spft(umt,P,F,Trigger,SourceRef),true),
  mpred_retract_i_or_warn(P),
  mpred_retract_i_or_warn(basePFC:spft(umt,P,F,Trigger,SourceRef)),
  fail.
mpred_reset:-  
  mpred_database_item(T),!,
  mpred_warn("Couldn't full mpred_reset: ~p.~n",[T]), must(pp_cur_DB_Current),!,
  trace,mpred_error("Pfc database not empty after mpred_reset, e.g., ~p.~n",[T]),!,fail.
mpred_reset:- mpred_trace_msg("Reset DB complete").

% true if there is some Pfc crud still in the database.
mpred_database_item(TermO):-
  mpred_database_term(P/A),
  functor(Term,P,A),
  clause_u(Term,B), 
  B\= loop_check_nr(_),
  \+ ( B= Term),
  ((B== true)-> TermO=Term; TermO=(Term:B)).
   

mpred_retract_i_or_warn(X):- retract_u(X), !.
mpred_retract_i_or_warn(X):- X=basePFC:hs(_),!.
mpred_retract_i_or_warn(X):- X=basePFC:spft(umt,_,basePFC:hs(_),_,_),!.
mpred_retract_i_or_warn(X):- 
  mpred_warn("Couldn't retract_user ~p.~n",[X]).




%   File   : pfcdebug.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: provides predicates for examining the database and debugginh 
%   for Pfc.

:- dynamic get_mpred_is_tracing/1.
:- dynamic mpred_is_spying_pred/2.
:- dynamic mpred_is_tracing_exec/0.
:- dynamic mpred_warnings/1.

:- mpred_default_setting(mpred_warnings(_), mpred_warnings(true)).


%  mpred_fact(P) is true if fact P was asserted into the database via add.

mpred_fact(P):- mpred_fact(P,true).

%  mpred_fact(P,C) is true if fact P was asserted into the database via
%  add and contdition C is satisfied.  For example, we might do:
%  
%   mpred_fact(X,mpred_userFact(X))
% 

mpred_fact(P,C):- 
  mpred_get_support(P,_),
  mpred_db_type(P,fact),
  call(C).

%  mpred_facts(-ListofPmpred_facts) returns a list of facts added.

mpred_facts(L):- mpred_facts(_,true,L).

mpred_facts(P,L):- mpred_facts(P,true,L).

%  mpred_facts(Pattern,Condition,-ListofPmpred_facts) returns a list of facts added.

%% mpred_facts( +P, ?C, ?L) is semidet.
%
% PFC Facts.
%
mpred_facts(P,C,L):- setof(P,mpred_fact(P,C),L).


%% brake( +X) is semidet.
%
% Brake.
%
brake(X):-  X, break.


% 
%  predicates providing a simple tracing facility
% 

mpred_trace_mpred_ain(P):- 
  % this is here for upward compat. - should go away eventually.
  mpred_trace_mpred_ain(P,(o,o)).


mpred_ain_special_support(Fact,Support):- fail,
  Support = (How,basePFC:pt(umt,How2,rhs([Fact]))),  
  ignore(((mpred_ain_trigger_reprop(basePFC:nt(umt,How,(mpred_CALL(How2)),rhs([Fact])),(How==>Fact,How)),fail))),
  dmsg('~n   \\\\^  Extra ^//  ~n',[]),
  fail.
mpred_ain_special_support(P,S):-mpred_trace_mpred_ain(P,S),!. 
  
mpred_trace_mpred_ain(P,S):- 
 notrace((
   mpred_trace_add_print(P,S),
   mpred_trace_break(P,S))).
   

mpred_trace_add_print(P,S):-
  get_mpred_is_tracing(P), !,
  (
  \+ \+ 
   (S=(U,U)
       -> wdmsg("~NAdding (~p) ~p",[U,P])
        ; wdmsg("~NAdding (:) ~p~NSupported By: ~p",[P,S]))),!.

mpred_trace_add_print(_,_).


mpred_trace_break(P,_ZS):-
  mpred_is_spying_pred(P,add) -> 
   (wdmsg("~NBreaking on mpred_ain(~p)",[P]),
    break)
   ; true.



mpred_trace_rem(basePFC:pt(umt,_,_)):-
  % hack for now - never trace triggers.
  !.
mpred_trace_rem(basePFC:nt(umt,_,_)):-
  % hack for now - never trace triggers.
  !.


mpred_trace_rem(P):-
  (get_mpred_is_tracing(P) 
     -> wdmsg('~NRemoving ~p.',[P])
      ; true),
  (mpred_is_spying_pred(P,mpred_withdraw)
   -> (wdmsg("~NBreaking on mpred_withdraw(~p)",[P]),
       break)
   ; true).


mpred_trace:- mpred_trace(_).

mpred_trace(Form):-
  assert_u(get_mpred_is_tracing(Form)).



%% mpred_trace( +Form, ?Condition) is semidet.
%
% PFC Trace.
%
mpred_trace(Form,Condition):- 
  assert_u((get_mpred_is_tracing(Form):- Condition)).

mpred_spy(Form):- mpred_spy(Form,[add,mpred_withdraw],true).

mpred_spy(Form,Modes):- mpred_spy(Form,Modes,true).

mpred_spy(Form,[add,mpred_withdraw],Condition):-
  !,
  mpred_spy1(Form,add,Condition),
  mpred_spy1(Form,mpred_withdraw,Condition).

mpred_spy(Form,Mode,Condition):-
  mpred_spy1(Form,Mode,Condition).

mpred_spy1(Form,Mode,Condition):-
  assert_u((mpred_is_spying_pred(Form,Mode):- Condition)).

mpred_nospy:- mpred_nospy(_,_,_).

mpred_nospy(Form):- mpred_nospy(Form,_,_).

mpred_nospy(Form,Mode,Condition):- 
  clause_u(mpred_is_spying_pred(Form,Mode), Condition, Ref),
  erase(Ref),
  fail.
mpred_nospy(_,_,_).

mpred_notrace:- mpred_untrace.
mpred_untrace:- mpred_untrace(_).
mpred_untrace(Form):- retractall_i(mpred_is_tracing_pred(Form)).

% needed:  mpred_trace_rule(Name)  ...


% if the correct flag is set, trace exection of Pfc
mpred_trace_msg(Info):- notrace(((lookup_u(mpred_is_tracing_exec);tracing)->in_cmt(wdmsg(Info));true)).
mpred_trace_msg(Format,Args):- notrace((format_to_message(Format,Args,Info),mpred_trace_msg(Info))).

mpred_warn(Info):- notrace((lookup_u(mpred_warnings(true));tracing) -> wdmsg(warn(mpred,Info)) ; mpred_trace_msg('WARNING/PFC: ~p',[Info])),
                (debugging(mpred)->trace;true).
mpred_warn(Format,Args):- notrace((format_to_message(Format,Args,Info),mpred_warn(Info))).

mpred_error(Info):- notrace(tracing -> wdmsg(error(pfc,Info)) ; mpred_warn(error(Info))).
mpred_error(Format,Args):- notrace((format_to_message(Format,Args,Info),mpred_error(Info))).


mpred_watch:- assert_u(mpred_is_tracing_exec).
mpred_trace_exec:- assert_u(mpred_is_tracing_exec).

mpred_noWatch:-  retractall_u(mpred_is_tracing_exec).

mpred_error(Msg):-  mpred_error(Msg,[]).

mpred_error(Msg,Args):- 
  format("~NERROR/Pfc: ",[]),
  format(Msg,Args).

% mpred_test(G):- mpred_why(G).


mpred_load_term(:- module(_,L)):-!, maplist(export,L).
mpred_load_term(:- TermO):-call(TermO).
mpred_load_term(TermO):-mpred_ain_object(TermO).

mpred_load(PLNAME):- % unload_file(PLNAME),
   open(PLNAME, read, In, []),
   repeat,
   line_count(In,_Lineno),
   % double_quotes(_DQBool)
   Options = [variables(_Vars),variable_names(VarNames),singletons(_Singletons),comment(_Comment)],
   catchv((read_term(In,Term,[syntax_errors(error)|Options])),E,(dmsg(E),fail)),
   b_setval('$variable_names',VarNames),expand_term(Term,TermO),mpred_load_term(TermO),
   Term==end_of_file,
   close(In).

% 
%  These control whether or not warnings are printed at all.
%    mpred_warn.
%    nmpred_warn.
% 
%  These print a warning message if the flag mpred_warnings is set.
%    mpred_warn(+Message)
%    mpred_warn(+Message,+ListOfArguments)
% 

mpred_warn:- 
  retractall_u(mpred_warnings(_)),
  assert_u(mpred_warnings(true)).

nmpred_warn:-
  retractall_u(mpred_warnings(_)),
  assert_u(mpred_warnings(false)).
 
mpred_warn(Msg):-  mpred_warn(Msg,[]).

mpred_warn(Msg,Args):- 
  format(string(S),Msg,Args),  
  (mpred_warnings(true) -> wdmsg(warn(mpred_,S)) ; mpred_trace_msg('WARNING/PFC: ~s',[S])),!.


%%  mpred_set_warnings(+TF) is det.
%   true = sets flag to cause Pfc warning messages to print.
%   false = sets flag to cause Pfc warning messages not to print.
% 
mpred_set_warnings(True):- 
  retractall_u(mpred_warnings(_)),
  assert_u(mpred_warnings(True)).
mpred_set_warnings(false):- 
  retractall_u(mpred_warnings(_)).

%   File   : pfcjust.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: predicates for accessing Pfc justifications.
%   Status: more or less working.
%   Bugs:

%  *** predicates for exploring supports of a fact *****


:- use_module(library(lists)).

mpred_justification(F,J):- supporters_list(F,J).

justifications(F,Js):- bagof(J,mpred_justification(F,J),Js).



%%  mpred_basis_list(+P,-L)
%
%  is true iff L is a list of "mpred_basis_list" facts which, taken
%  together, allows us to deduce P.  A mpred "based on" list fact is an axiom (a fact 
%  added by the user or a raw Prolog fact (i.e. one w/o any support))
%  or an assumption.
%
mpred_basis_list(F,[F]):- (mpred_axiom(F) ; mpred_assumption(F)),!.

mpred_basis_list(F,L):-
  % i.e. (reduce 'append (map 'mpred_basis_list (mpred_justification f)))
  mpred_justification(F,Js),
  bases_union(Js,L).


%%  bases_union(+L1,+L2).
%
%  is true if list L2 represents the union of all of the 
%  facts on which some conclusion in list L1 is based.
%
bases_union([],[]).
bases_union([X|Rest],L):-
  mpred_basis_list(X,Bx),
  bases_union(Rest,Br),
  mpred_union(Bx,Br,L).
	
mpred_axiom(F):- 
  mpred_get_support(F,(U,U)).

%% mpred_assumption(P)
% 
%  an mpred_assumption is a failed goal, i.e. were assuming that our failure to 
%  prove P is a proof of not(P)
%
mpred_assumption(P):- mpred_negation(P,_).
   

%% mpred_assumptions( +X, +AsSet) is semidet.
%
% true if AsSet is a set of assumptions which underly X.
%
mpred_assumptions(X,[X]):- mpred_assumption(X).
mpred_assumptions(X,[]):- mpred_axiom(X).
mpred_assumptions(X,L):-
  mpred_justification(X,Js),
  do_assumpts(Js,L).


%% do_assumpts(+Set1,?Set2) is semidet.
%
% Assumptions Secondary Helper.
%
do_assumpts([],[]).
do_assumpts([X|Rest],L):-
  mpred_assumptions(X,Bx),
  do_assumpts(Rest,Br),
  mpred_union(Bx,Br,L).  


%  mpred_proofTree(P,T) the proof tree for P is T where a proof tree is
%  of the form
% 
%      [P , J1, J2, ;;; Jn]         each Ji is an independent P justifier.
%           ^                         and has the form of
%           [J11, J12,... J1n]      a list of proof trees.


%% mpred_child(+P,?Q) is semidet.
%
% is true iff P is an immediate justifier for Q.
%
mpred_child(P,Q):-
  mpred_get_support(Q,(P,_)).

mpred_child(P,Q):-
  mpred_get_support(Q,(_,Trig)),
  mpred_db_type(Trig,trigger),
  mpred_child(P,Trig).


%% mpred_children( +P, ?L) is semidet.
%
% PFC Children.
%
mpred_children(P,L):- bagof(C,mpred_child(P,C),L).



%% mpred_descendant( +P, ?Q) is semidet.
%
% mpred_descendant(P,Q) is true iff P is a justifier for Q.
%
mpred_descendant(P,Q):- 
   mpred_descendant1(P,Q,[]).


%% mpred_descendant1( +P, ?Q, ?Seen) is semidet.
%
% PFC Descendant Secondary Helper.
%
mpred_descendant1(P,Q,Seen):-
  mpred_child(X,Q),
  (\+ member(X,Seen)),
  (P=X ; mpred_descendant1(P,X,[X|Seen])).
  

%% mpred_descendants( +P, ?L) is semidet.
%
% PFC Descendants.
%
mpred_descendants(P,L):- 
  bagof(Q,mpred_descendant1(P,Q,[]),L).

bagof_or_nil(T,G,B):- (bagof(T,G,B) *-> true; B=[]).

% 
%  predicates for manipulating support relationships
% 

%  mpred_add_support(+Fact,+Support)
mpred_add_support(P,(Fact,Trigger)):-
  (Trigger= basePFC:nt(umt,F,Condition,Action) -> 
    (mpred_trace_msg('~N~n\tAdding fcnt via support~n\t\ttrigger: ~p~n\t\tcond: ~p~n\t\taction: ~p~n\t from: ~p~N',
      [F,Condition,Action,mpred_add_support(P,(Fact,Trigger))]));true),
  assert_u(basePFC:spft(umt,P,Fact,Trigger,_ZSourceRef)).

mpred_get_support(P,(Fact,Trigger)):-
      basePFC:spft(umt,P,Fact,Trigger,_ZSourceRef).


% There are three of these to try to efficiently handle the cases
% where some of the arguments are not bound but at least one is.

mpred_rem_support(P,(Fact,Trigger)):-
  nonvar(P),
  !,
  mpred_retract_i_or_warn(basePFC:spft(umt,P,Fact,Trigger,_ZSourceRef)).


mpred_rem_support(P,(Fact,Trigger)):-
  nonvar(Fact),
  !,
  mpred_retract_i_or_warn(basePFC:spft(umt,P,Fact,Trigger,_ZSourceRef)).

mpred_rem_support(P,(Fact,Trigger)):-
  mpred_retract_i_or_warn(basePFC:spft(umt,P,Fact,Trigger,_ZSourceRef)).


mpred_collect_supports(Tripples):-
  bagof(Tripple, mpred_support_relation(Tripple), Tripples),
  !.
mpred_collect_supports([]).

mpred_support_relation((P,F,T)):-
  basePFC:spft(umt,P,F,T,_ZSourceRef).

mpred_make_supports((P,S1,S2)):- 
  mpred_add_support(P,(S1,S2)),
  (mpred_ain_object(P); true),
  !.

%%  mpred_trigger_key(+Trigger,-Key) 
% 
%  Arg1 is a trigger.  Key is the best term to index it on.
% 
%  Get a key from the trigger that will be used as the first argument of
%  the trigger mpred_basis_list clause_i that stores the trigger.

mpred_trigger_key(X,X):- var(X), !.
mpred_trigger_key(basePFC:pt(umt,Key,_),Key).
mpred_trigger_key(basePFC:pt(umt,Key,_,_),Key).
mpred_trigger_key(basePFC:nt(umt,Key,_,_),Key).
mpred_trigger_key(Key,Key).

% For chart parser
mpred_trigger_key(chart(word(W),_ZL),W):- !.
mpred_trigger_key(chart(stem([Char1|_ZRest]),_ZL),Char1):- !.
mpred_trigger_key(chart(Concept,_ZL),Concept):- !.
mpred_trigger_key(X,X).



%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999
%%%%%%%%%%%%%%%%%% 99999999999999999999999999999999999999999999

pp_cur_DB_Current:-
 must_det_l((
  pp_cur_facts,
  pp_cur_rules,
  pp_cur_triggers,
  pp_cur_supports)).

%  pp_cur_facts ...

pp_cur_facts:- ignore(pp_cur_facts(_,true)).

pp_cur_facts(Pattern):- pp_cur_facts(Pattern,true).

pp_cur_facts(P,C):-
  mpred_facts(P,C,L),
  mpred_classifyFacts(L,User,Pfc,_ZRule),
  format("~N~nUser added facts:",[]),
  pp_cur_items(User),
  format("~N~nPPfc added facts:",[]),
  pp_cur_items(Pfc).

%  printitems clobbers it''s arguments - beware!

pp_cur_items([]).
pp_cur_items([H|T]):-
  numbervars(H,0,_),
  format("~N  ~p",[H]),
  pp_cur_items(T).

mpred_classifyFacts([],[],[],[]).

mpred_classifyFacts([H|T],User,Pfc,[H|Rule]):-
  mpred_db_type(H,rule),
  !,
  mpred_classifyFacts(T,User,Pfc,Rule).

mpred_classifyFacts([H|T],[H|User],Pfc,Rule):-
  get_source_ref(UU),
  mpred_get_support(H,UU),
  !,
  mpred_classifyFacts(T,User,Pfc,Rule).

mpred_classifyFacts([H|T],User,[H|Pfc],Rule):-
  mpred_classifyFacts(T,User,Pfc,Rule).

pp_cur_rules:-
 format("~NRules...~n",[]),
  bagof_or_nil((P==>Q),clause_u((P==>Q),true),R1),
  pp_cur_items(R1),
  bagof_or_nil((P<==>Q),clause_u((P<==>Q),true),R2),
  pp_cur_items(R2),
  bagof_or_nil((P<-Q),clause_u((P<-Q),true),R3),
  pp_cur_items(R3).

pp_cur_triggers:-
  format("~NPositive triggers...~n",[]),
  bagof_or_nil(basePFC:pt(umt,T,B),mpred_get_trigger_quick(basePFC:pt(umt,T,B)),Pts),
  pp_cur_items(Pts),
  format("~NNegative triggers...~n",[]),
  bagof_or_nil(basePFC:nt(umt,A,B,C),mpred_get_trigger_quick(basePFC:nt(umt,A,B,C)),Nts),
  pp_cur_items(Nts),
  format("~NGoal triggers...~n",[]),
  bagof_or_nil(basePFC:bt(umt,A,B),mpred_get_trigger_quick(basePFC:bt(umt,A,B)),Bts),
  pp_cur_items(Bts).

pp_cur_supports:- 
  % temporary hack.
  format("~NSupports...~n",[]),
  ignore((setof((P >= S), mpred_get_support(P,S),L),
    pp_cur_items(L))).


%   File   : mpred_why.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated:
%   Purpose: predicates for interactively exploring Pfc justifications.

% ***** predicates for brousing justifications *****

:- use_module(library(lists)).

mpred_why:- 
  why_buffer(P,_),
  mpred_why(P).

mpred_why(N):-
  number(N),
  !,
  why_buffer(P,Js),
  mpred_handle_why_command(N,P,Js).

mpred_why(P):-
  justifications(P,Js),
  retractall_u(why_buffer(_,_)),
  assert_u(why_buffer(P,Js)),
  in_cmt((mpred_whyBrouse(P,Js))).

mpred_why1(P):-
  justifications(P,Js),
  in_cmt((mpred_whyBrouse(P,Js))).

% non-interactive
mpred_whyBrouse(P,Js):- 
   mpred_pp_cur_justifications(P,Js), !.

% Interactive
mpred_whyBrouse(P,Js):-
  mpred_pp_cur_justifications(P,Js),
  mpred_ask(' >> ',Answer),
  mpred_handle_why_command(Answer,P,Js).

mpred_handle_why_command(q,_,_):- !.
mpred_handle_why_command(h,_,_):- 
  !,
  format("~N
Justification Brouser Commands:
 q   quit.
 N   focus on Nth mpred_justification.
 N.M brouse step M of the Nth mpred_justification
 user   up a level ~n",
  []).

mpred_handle_why_command(N,_ZP,Js):-
  float(N),
  !,
  mpred_select_justification_node(Js,N,Node),
  mpred_why1(Node).

mpred_handle_why_command(u,_,_):-
  % u=up
  !.

mpred_unhandled_command(N,_,_):-
  integer(N),
  !,
  format("~N~p is a yet unimplemented command.",[N]),
  fail.

mpred_unhandled_command(X,_,_):-
 format("~N~p is an unrecognized command, enter h. for help.",[X]),
 fail.
  
mpred_pp_cur_justifications(P,Js):-
  format("~NJustifications for ~p:",[P]),
  mpred_pp_cur_justification1(Js,1).

mpred_pp_cur_justification1([],_).

mpred_pp_cur_justification1([J|Js],N):-
  % show one mpred_justification and recurse.
  nl,
  mpred_pp_cur_justifications2(J,N,1),
  N2 is N+1,
  mpred_pp_cur_justification1(Js,N2).

mpred_pp_cur_justifications2([],_,_).

mpred_pp_cur_justifications2([C|Rest],JustNo,StepNo):- 
 (StepNo==1->fmt('~N~n',[]);true),
  copy_term(C,CCopy),
  numbervars(CCopy,0,_),
  format("~N    ~p.~p ~p",[JustNo,StepNo,CCopy]),
  StepNext is 1+StepNo,
  mpred_pp_cur_justifications2(Rest,JustNo,StepNext).

mpred_ask(Msg,Ans):-
  format("~N~p",[Msg]),
  read(Ans).

mpred_select_justification_node(Js,Index,Step):-
  JustNo is integer(Index),
  nth1(JustNo,Js,Justification),
  StepNo is 1+ integer(Index*10 - JustNo*10),
  nth1(StepNo,Justification,Step).


%%  mpred_supported(+P) is semidet.
%
%  succeeds if P is "supported". What this means
%  depends on the TMS mode selected.
%
mpred_supported(P):- 
  basePFC:tms(Mode),
  mpred_supported(Mode,P).

%%  mpred_supported(+TMS,+P) is semidet.
%
%  succeeds if P is "supported". What this means
%  depends on the TMS mode supplied.
%
mpred_supported(local,P):- !, mpred_get_support(P,_).
mpred_supported(cycles,P):-  !, well_founded(P).
mpred_supported(_,_):- true.


%% well_founded(+Fact) is semidet.
%
% a fact is well founded if it is supported by the user
%  or by a set of facts and a rules, all of which are well founded.
% 
well_founded(Fact):- with_each_item(well_founded_0,Fact,[]).

well_founded_0(F,_):-
  % supported by user (mpred_axiom) or an "absent" fact (mpred_assumption).
  (mpred_axiom(F) ; mpred_assumption(F)),
  !.

well_founded_0(F,Descendants):-
  % first make sure we aren't in a loop.
  (\+ memberchk(F,Descendants)),
  % find a mpred_justification.
  supporters_list(F,Supporters),
  % all of whose members are well founded.
  well_founded_list(Supporters,[F|Descendants]),
  !.

%%  well_founded_list(+List,-Decendants) is det.
%
% simply maps well_founded over the list.
%
well_founded_list([],_).
well_founded_list([X|Rest],L):-
  well_founded_0(X,L),
  well_founded_list(Rest,L).



%% supporters_list(+F,-ListofSupporters) is det.
%
% where ListOfSupports is a list of the
% supports for one mpred_justification for fact F -- i.e. a list of facts which,
% together allow one to deduce F.  One of the facts will typically be a rule.
% The supports for a user-defined fact are: [u].
%
supporters_list(F,[Fact|MoreFacts]):-
  mpred_get_support(F,(Fact,Trigger)),
  triggerSupports(Trigger,MoreFacts).

triggerSupports(u,[]):- !.
triggerSupports(Trigger,[Fact|MoreFacts]):-
  mpred_get_support(Trigger,(Fact,AnotherTrigger)),
  triggerSupports(AnotherTrigger,MoreFacts).

:-module_transparent(mpred_ain/1).
:-module_transparent(mpred_aina/1).
:-module_transparent(mpred_ainz/1).
:-module_transparent(logicmoo_util_database:ain/1).
:-module_transparent(logicmoo_util_database:aina/1).
:-module_transparent(logicmoo_util_database:ainz/1).
:-multifile(logicmoo_util_database:ain/1).
:-multifile(logicmoo_util_database:aina/1).
:-multifile(logicmoo_util_database:ainz/1).
:-asserta((logicmoo_util_database:ainz(G):- !, call_u(mpred_ainz(G)))).
:-asserta((logicmoo_util_database:ain(G):- !, call_u(mpred_ain(G)))).
:-asserta((logicmoo_util_database:aina(G):- !, call_u(mpred_aina(G)))).

:- endif.
