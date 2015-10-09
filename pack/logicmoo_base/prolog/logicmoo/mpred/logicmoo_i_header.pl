/** <module> 
% All modules are declared here so that this next lines dont have to be pasted into every file.
% Since this list will need at least 160 entries to cover the obj classes rooms and commands, 
% we add the modules here to not waste 160^2 lines of text and having to not 
% update 160+ files whenever a new module is used
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/


:- if( \+ current_predicate( t_l:current_pttp_db_oper/1 )).


:- op(500,fx,'~').
:- op(1050,xfx,'<==>').
:- op(1050,xfx,('<-')).
:- op(1200,fx,('=>')).
:- op(1200,fx,('==>')).
:- op(1100,fx,('nesc')).
:- op(1150,xfx,('::::')).
:- op(300,fx,'-').
:- op(600,yfx,'&').  
:- op(600,yfx,'v').
:- op(1075,xfx,'<-').
:- op(1075,xfx,'<-').
:- op(1070,xfx,'=>').
:- op(1070,xfx,'<=>').
:- op(1100,xfx,('<==>')).
:- op(1100,xfx,('==>')).
:- op(350,xfx,'xor').
:- op(300,fx,user:'~').
:- op(300,fx,user:'-').
:- op(400,yfx,user:'&').  
:- op(500,yfx,user:'v').
:- op(1075,xfx,user:'<-').
:- op(1075,xfx,user:'<==>').
:- op(350,xfx,user:'xor').

:- must(context_module(user)).

:- thread_local(t_l:disable_mpred_term_expansions_locally /0).
:- multifile(system:term_expansion/2).
:- multifile(user:term_expansion/2).
:- multifile(user:goal_expansion/2).

:-dynamic(registered_mpred_file/1).
:-dynamic(never_registered_mpred_file/1).




:- multifile(t_l:current_pttp_db_oper/1).
:- thread_local(t_l:current_pttp_db_oper/1).

:- dynamic(new_was_isa/0).
:- multifile(new_was_isa/0).

:- dynamic(mpred_univ/3).
:- multifile(mpred_univ/3).

load_time_sanity.

:-dynamic(lmconf:hook_one_second_timer_tick/0).
:-multifile(lmconf:hook_one_second_timer_tick/0).
:-dynamic(lmconf:hook_one_minute_timer_tick/0).
:-multifile(lmconf:hook_one_minute_timer_tick/0).

:- dynamic(lmconf:startup_option/2).

:- dynamic(completelyAssertedCollection/1).
:- multifile(completelyAssertedCollection/1).
:- dynamic(((tCol/1,tFunction/1,tSet/1,mpred_module/2))).
:- multifile(((tCol/1,tFunction/1,tSet/1,mpred_module/2))).
:- dynamic(ttPredType/1).
:- multifile(ttPredType/1).
:- dynamic(functorDeclares/1).
:- multifile(functorDeclares/1).
:- dynamic(ttFormatType/1).
:- multifile(ttFormatType/1).
:- dynamic(tPred/1).
:- multifile(tPred/1).
:- dynamic(tRelation/1).
:- multifile(tRelation/1).
:- dynamic(arity/2).
:- multifile(arity/2).
:- dynamic((neg)/1).
:- multifile((neg)/1).

:- dynamic(ttUnverifiableType/1).
:- multifile(ttUnverifiableType/1).

:- dynamic(type_prefix/2).
:- multifile(type_prefix/2).

:- dynamic(type_suffix/2).
:- multifile(type_suffix/2).

:- dynamic(natFunction/2).
:- multifile(natFunction/2).



:- dynamic(pfcControlled/1).
:- multifile(pfcControlled/1).

:- dynamic(agent_text_command/4).
:- multifile(agent_text_command/4).

:- dynamic(arity/2).
:- multifile(arity/2).



:- dynamic(('nesc')/1).
:- dynamic((('neg'))/1).
:- dynamic((('<-'))/2).
:- dynamic(('<-')/2).
:- dynamic(('::::')/2).
:- dynamic(('<==>')/2).
:- dynamic('pt'/2).
:- dynamic('pk'/3).
:- dynamic('nt'/3).
:- dynamic('bt'/2).

:- dynamic('spftY'/4).
:- dynamic(mpred_undo_method/2).
:- dynamic(fcTmsMode/1).
:- dynamic(mpred_queue/2).
:- dynamic(mpred_halt_signal/1).
:- dynamic(mpred_select/2).
:- dynamic(mpred_search/1).

:- multifile(('nesc')/1).
:- multifile(('neg')/1).
:- multifile(('<-')/2).
:- multifile(('<-')/2).
:- multifile(('::::')/2).
:- multifile(('<==>')/2).
:- multifile('pt'/2).
:- multifile('pk'/3).
:- multifile('nt'/3).
:- multifile('bt'/2).
:- multifile(mpred_undo_method/2).
:- multifile((mpred_action)/1).
:- multifile(fcTmsMode/1).
:- multifile(mpred_queue/2).
:- multifile(mpred_halt_signal/1).
:- multifile(mpred_select/2).
:- multifile(mpred_search/1).

:- dynamic((exactlyAssertedEL/4,exactlyAssertedEL/5,exactlyAssertedEL/6,exactlyAssertedEL/7)).
:- dynamic((exactlyAssertedEL_next/4,exactlyAssertedEL_next/5,exactlyAssertedEL_next/6,exactlyAssertedEL_next/7)).
:- dynamic((exactlyAssertedEL_first/4,exactlyAssertedEL_first/5,exactlyAssertedEL_first/6,exactlyAssertedEL_first/7)).
:- dynamic(assertedTinyKB_implies_first/4).
:- dynamic(assertedTinyKB_not_first/3).
:- dynamic((exactlyAssertedEL_first/5,exactlyAssertedEL_with_vars/5,exactlyAssertedEL_with_vars/6,assertedTinyKB_implies_Already/4)).
:- dynamic user:term_expansion/2.
:- multifile user:term_expansion/2.

:- dynamic(formatted_resultIsa/2).
:- multifile(formatted_resultIsa/2).

:- dynamic(  asserted_argIsa_known/3).
:- dynamic(  argQuotedIsa/3).
:- dynamic(  resultGenls/2).
:- multifile(  resultGenls/2).

:- dynamic(  argGenls/3).
:- multifile(  argGenls/3).

:- dynamic( prologSideEffects/1).
:- multifile(prologSideEffects/1).

:- dynamic(  argsQuoted/1).
:- multifile(  argsQuoted/1).

:- dynamic(  disjointWith/2).
:- multifile(  disjointWith/2).


:- dynamic(was_chain_rule/1).
:- multifile(was_chain_rule/1).


:- dynamic(is_edited_clause/3).
:- multifile(is_edited_clause/3).



:-thread_local(t_l:print_mode/1).

%:-dynamic((t/1,t/2)).
:- dynamic((
   meta_argtypes/1,
         % t/1,
         % t/2,
          t/3,
          t/4,
          t/5,
          t/6,
          t/7,
          t/8,
          t/9,
          t/10,
          t/11,
        %  asserted_mpred_t/1,
          asserted_mpred_t/2,
          asserted_mpred_t/3,
          asserted_mpred_t/4,
          asserted_mpred_t/5,
          asserted_mpred_t/6,
          asserted_mpred_t/7,
          assertion_f/1,
          assertion_t/1,
        %  asserted_mpred_f/1,
          asserted_mpred_f/2,
          asserted_mpred_f/3,
          asserted_mpred_f/4,
          asserted_mpred_f/5,
          asserted_mpred_f/6,
          asserted_mpred_f/7,
         % mpred_f/1,
          mpred_f/2,
          mpred_f/3,
          mpred_f/4,
          mpred_f/5,
          mpred_f/6,
          mpred_f/7)).

:-multifile((t/1,t/2)).
:- multifile((
  meta_argtypes/1,
         % t/1,
        %  t/2,
          t/3,
          t/4,
          t/5,
          t/6,
          t/7,
          t/8,
          t/9,
          t/10,
          t/11,
        %  asserted_mpred_t/1,
          asserted_mpred_t/2,
          asserted_mpred_t/3,
          asserted_mpred_t/4,
          asserted_mpred_t/5,
          asserted_mpred_t/6,
          asserted_mpred_t/7,
          assertion_f/1,
          assertion_t/1,
        %  asserted_mpred_f/1,
          asserted_mpred_f/2,
          asserted_mpred_f/3,
          asserted_mpred_f/4,
          asserted_mpred_f/5,
          asserted_mpred_f/6,
          asserted_mpred_f/7,
         % mpred_f/1,
          mpred_f/2,
          mpred_f/3,
          mpred_f/4,
          mpred_f/5,
          mpred_f/6,
          mpred_f/7)).


:-dynamic(user_db:grant_openid_server/2).
:-multifile(user_db:grant_openid_server/2).

:- dynamic user:'$was_imported_kb_content$'/2.
:- multifile user:'$was_imported_kb_content$'/2.
:- discontiguous(user:'$was_imported_kb_content$'/2).

:- user: ensure_loaded(library(logicmoo/logicmoo_utils)).

:-foreach(arg(_,isEach(prologMultiValued,prologOrdered,prologNegByFailure,meta_argtypes,prologPTTP,prologKIF,pfcControlled,tSet,tPredType,
 prologHybrid,predCanHaveSingletons,prologDynamic,prologBuiltin,tCol,prologMacroHead,prologListValued,prologSingleValued,functorDeclares),P),
   ((dynamic(P/1),multifile(P/1)))).

:- multifile listing_mpred_hook/1.
:- dynamic listing_mpred_hook/1.

:- dynamic genls/2.
:- dynamic isa/2.
:- dynamic mudKeyword/2.
:- multifile mudKeyword/2.
:- multifile genls/2.
:- multifile isa/2.

%:- dynamic t/2.
%:- multifile t/2.

:- style_check(-singleton).
:- set_prolog_flag(double_quotes, atom).
:- set_prolog_flag(double_quotes, string). 
:- set_prolog_flag(generate_debug_info, true).

% these do not get defined!?
% :-dynamic user_db:assert_user/2, user_db:grant_openid_server/2, user_db:retractall_grant_openid_server/2, user_db:retractall_user/2, user_db:assert_grant_openid_server/2.

:-dynamic((
                     argIsa/3,
                     isa/2,
                     resultIsa/2,
                      ttTemporalType/1)).
:-multifile((
                     argIsa/3,
                     isa/2,
                     resultIsa/2,
                      ttTemporalType/1)).

:- multifile(mpred_online:semweb_startup).

:- dynamic(t_l:infForward).
:- dynamic(mpred_module_ready).
:- dynamic lmconf:pfcManageHybrids/0.
:- thread_local t_l:into_form_code/0.
:- dynamic_multifile_exported user: defnSufficient/2.
:- thread_local user: repl_to_string/2.
:- thread_local user: repl_writer/2.
:- thread_local t_l:infSupertypeName/0.
:- dynamic_multifile_exported user: loaded_external_kbs/0.
:- dynamic_multifile_exported user: loading_module_h/1.
:- dynamic_multifile_exported user: registered_module_type/2.
:- dynamic_multifile_exported user: is_mpred_file/1.
:- dynamic_multifile_exported lmconf:decl_database_hook/2.

:- multifile user: local_term_anglify/2.
:- multifile user:term_anglify_last/2.
:- multifile user:term_anglify_np/3.
:- multifile user:term_anglify_np_last/3.

% HOOKS
:- multifile user: create_random_fact/1.
:- multifile lmconf:decl_coerce/3.
:- multifile lmconf:decl_database_hook/2.
:- multifile user: deduce_facts/2.
:- multifile user: default_type_props/3.
:- multifile user: fact_always_true/1.
:- multifile user: fact_maybe_deduced/1.
:- multifile user: tms_reject_why/2.
:- multifile user: fskel/7.
:- multifile lmconf:hook_coerce/3.
:- dynamic lmconf:hook_coerce/3.
:- multifile user: hooked_random_instance/3.

:- multifile user: now_unused/1.
:- multifile lmconf:mpred_provide_read_attributes/3.
:- multifile lmconf:mpred_provide_setup/4.
:- multifile lmconf:mpred_provide_storage_clauses/3.
:- multifile lmconf:mpred_provide_storage_op/2.
:- multifile lmconf:mpred_provide_write_attributes/2.

% DYN HOOKS
:- dynamic_multifile_exported user: is_never_type/1.

% DYN FOR CODE
:- dynamic_multifile_exported lmconf:after_mpred_load/0.
:- dynamic_multifile_exported lmconf:use_cyc_database/0.
:- dynamic_multifile_exported lmconf:agent_session/2.

:- dynamic_multifile_exported user: fact_is_false/2.
:- dynamic_multifile_exported user: kbp_t_list_prehook/2.


% DYN KB
:- dynamic_multifile_exported   only_if_pttp/0.
:- dynamic_multifile_exported   use_kif/2.
:- dynamic_multifile_exported   is_mpred_prop/2.
%:- dynamic_multifile_exported   hasInstance_dyn/2.
:- dynamic_multifile_exported   arity/2.
:- dynamic_multifile_exported   mpred_prop/2.
:- dynamic_multifile_exported   relationMostInstance/3.
:- dynamic_multifile_exported user:'<==>'/2.
% :- dynamic_multifile_exported   ruleForward/2.
:- dynamic_multifile_exported   ruleRewrite/2.
% :- dynamic_multifile_exported   ruleBackward/2.
:- dynamic_multifile_exported   tNearestReachableItem/1.
:- dynamic_multifile_exported   tFarthestReachableItem/1.
:- dynamic_multifile_exported deduceFromArgTypes/1.
:- dynamic_multifile_exported prologSideEffects/1.

:-dynamic agent_action_queue/3.
:-multifile agent_action_queue/3.

% ================================================
% Thread Locals
% ================================================

:- thread_local t_l:consulting_sources/0.
:- thread_local t_l:already_in_file_term_expansion/0.
:- thread_local t_l:agent_current_action/2.
:- thread_local t_l:caller_module/2.
:- thread_local t_l:mpred_opcall/2.
:- thread_local t_l:deduceArgTypes/1.


:- thread_local t_l:side_effect_ok/0.
:- thread_local(t_l:use_side_effect_buffer/0).

:- if( \+ current_predicate(t_l:verify_side_effect_buffer/0)).
:- thread_local(t_l:verify_side_effect_buffer/0).
:- endif.

:- thread_local(t_l:side_effect_buffer/3).


:- thread_local t_l:agenda_slow_op_do_prereqs/0.
:- thread_local t_l:enable_src_loop_checking/0.
:- thread_local t_l:in_dynamic_reader/1.
:- thread_local t_l:in_prolog_source_code/0.
:- thread_local t_l:is_calling/0.
:- thread_local t_l:infAssertedOnly/1.
:- thread_local t_l:infInstanceOnly/1.
:- thread_local t_l:infSkipArgIsa/0.
:- thread_local t_l:infSkipFullExpand/0.
:- thread_local t_l:into_form_code/0.
:- thread_local t_l:inVoProp/0.
:- thread_local t_l:no_arg_type_error_checking/0.
:- thread_local t_l:noDBaseHOOKS/1.
:- thread_local t_l:noDBaseMODs/1.
:- thread_local t_l:noRandomValues/1.
:- thread_local t_l:agenda_suspend_scans/0.
:- thread_local t_l:tracing80/0.
:- thread_local t_l:useAltPOS/0.
:- thread_local t_l:useOnlyExternalDBs/0.
:- thread_local t_l:usePlTalk/0.
:- thread_local t_l:with_callMPred/1.
:- thread_local t_l:assert_op_override/1.

:- dynamic   lmconf:session_io/4, lmconf:session_agent/2, lmconf:agent_session/2, user: telnet_fmt_shown/3, user: agent_action_queue/3.
:- multifile lmconf:session_io/4, lmconf:session_agent/2, lmconf:agent_session/2, user: telnet_fmt_shown/3, user: agent_action_queue/3.
:- thread_local(infSecondOrder/0).
:- thread_local(infThirdOrder/0).

:- dynamic_multifile_exported(lmconf:use_cyc_database/0).
:- thread_local(t_l:already_in_file_term_expansion/0).
:- thread_local(t_l:assert_op_override/1).
:- thread_local(t_l:caller_module/2).
:- thread_local(t_l:consulting_sources/0).
:- thread_local(t_l:deduceArgTypes/1).
:- thread_local(t_l:enable_src_loop_checking/0).
:- thread_local(t_l:in_prolog_source_code/0).
:- thread_local(t_l:infMustArgIsa/0).
:- thread_local(t_l:infSkipArgIsa/0).
:- thread_local(t_l:infSkipFullExpand/0).
:- thread_local(t_l:is_calling/0).
:- thread_local(t_l:mpred_opcall/2).
:- thread_local(t_l:mpred_mpred_add_loaded/0).
:- thread_local(t_l:noDBaseHOOKS/1).
:- thread_local(t_l:noDBaseMODs/1).
:- thread_local(t_l:mpred_loads_file/0).
:- thread_local(t_l:with_callMPred/1).
:- dynamic_multifile_exported(user: isa_pred_now_locked/0).
:- dynamic_multifile_exported(user: mpred_manages_unknowns/0).

:- dynamic_multifile_exported( props/2).
:- dynamic_multifile_exported( props/2).


:- thread_local repl_to_string/2.
:- thread_local repl_writer/2.

:- dynamic_multifile_exported(  grid_key/1).

:- endif.

