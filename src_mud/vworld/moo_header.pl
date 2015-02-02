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


% :-set_prolog_flag(unknown,fail).
:- set_prolog_flag(double_quotes, atom).
:- set_prolog_flag(double_quotes, string). 
:- set_prolog_flag(generate_debug_info, true).

:- thread_local(thlocal:current_pttp_db_oper/1).
%:- thread_local thlocal:user:use_pttp/0.
:- dynamic user:use_pttp/0.
% these do not get defined!?
% :-dynamic user_db:assert_user/2, user_db:grant_openid_server/2, user_db:retractall_grant_openid_server/2, user_db:retractall_user/2, user_db:assert_grant_openid_server/2.


% ================================================
% Thread Locals
% ================================================

:- thread_local thlocal:with_callMPred/1.
:- thread_local thlocal:usePlTalk/0.
:- thread_local thlocal:useOnlyExternalDBs/0.
:- thread_local thlocal:useAltPOS/0.
:- thread_local thlocal:tracing80/0.
:- thread_local thlocal:skip_db_op_hooks/0.
:- thread_local thlocal:session_agent/2.
:- thread_local thlocal:noRandomValues/1.
:- thread_local thlocal:noDBaseMODs/1.
:- thread_local thlocal:noDBaseHOOKS/1.
:- thread_local thlocal:no_arg_type_error_checking/0.
:- thread_local thlocal:into_form_code/0.
:- thread_local thlocal:infInstanceOnly/1.
:- thread_local thlocal:infAssertedOnly/1.
:- thread_local thlocal:in_prolog_source_code/0.
:- thread_local thlocal:in_dynamic_reader/1.
:- thread_local thlocal:enable_src_loop_checking/0.
:- thread_local thlocal:do_slow_kb_op_now/0.
:- thread_local thlocal:deduceArgTypes/1.
:- thread_local thlocal:dbase_opcall/2.
:- thread_local thlocal:dbase_change/2.
:- thread_local thlocal:dbase_capture/2.
:- thread_local thlocal:caller_module/2.
:- thread_local thlocal:agent_current_action/2.
:- thread_local thlocal:adding_from_srcfile/0.


:- dynamic_multifile_exported thglobal:use_cyc_database/0.
:- dynamic_multifile_exported thglobal:global_session_agent/2.

:- dynamic_multifile_exported user:decl_database_hook/2.
:- dynamic_multifile_exported user:decl_coerce/3.

:- dynamic_multifile_exported user:ttCompleteExtentAsserted/1.
:- dynamic_multifile_exported user:is_mpred_prop/3.
:- dynamic_multifile_exported user:repl_writer/2.
:- dynamic_multifile_exported user:repl_to_string/2.
:- dynamic_multifile_exported user:mudMemory/2.
:- dynamic_multifile_exported user:mudNeedsLook/2.
:- dynamic_multifile_exported user:mpred_arity/2.
:- dynamic_multifile_exported user:mpred_prop/2.
:- dynamic_multifile_exported user:world_agent_plan/3.
:- dynamic_multifile_exported user:verb_alias/2.
:- dynamic_multifile_exported user:use_usable/4.
:- dynamic_multifile_exported user:update_stats/2.
:- dynamic_multifile_exported user:update_charge/2.
:- dynamic_multifile_exported user:typeProps/2.
:- dynamic_multifile_exported user:type_action_info/3.
:- dynamic_multifile_exported user:tRegion/1.
:- dynamic_multifile_exported user:term_specifier_text/2.
:- dynamic_multifile_exported user:term_anglify_np_last/3.
:- dynamic_multifile_exported user:term_anglify_np/3.
:- dynamic_multifile_exported user:term_anglify_last/2.
:- dynamic_multifile_exported user:term_anglify/2.
:- dynamic_multifile_exported user:ruleHybridChain/2.
:- dynamic_multifile_exported user:ruleEquiv/2.
:- dynamic_multifile_exported user:registered_module_type/2.
:- dynamic_multifile_exported user:provide_mpred_write_attributes/2.
:- dynamic_multifile_exported user:provide_mpred_storage_op/4.
:- dynamic_multifile_exported user:provide_mpred_storage_clauses/3.
:- dynamic_multifile_exported user:provide_mpred_setup/4.
:- dynamic_multifile_exported user:provide_mpred_read_attributes/3.
:- dynamic_multifile_exported user:provide_mpred_currently/4.
:- dynamic_multifile_exported user:now_unused/1.
:- dynamic_multifile_exported user:mudTermAnglify/2.
:- dynamic_multifile_exported(user:is_known_trew/1).
:- dynamic_multifile_exported(user:is_known_true/1).
:- dynamic_multifile_exported user:mudSubclass/2.
:- dynamic_multifile_exported user:mudStrowing/2.
:- dynamic_multifile_exported user:hasInstance_dyn/2.
:- dynamic_multifile_exported user:loading_module_h/1.
:- dynamic_multifile_exported(mpred_prop/2).
:- dynamic_multifile_exported(mpred_arity/2).
:- dynamic_multifile_exported(never_type/1).
:- dynamic_multifile_exported user:mudToHitArmorClass0/2.
:- dynamic_multifile_exported fact_always_true/1.
:- dynamic_multifile_exported user:mudStowing/2.
:- dynamic_multifile_exported user:mudSubclass/2.
:- dynamic_multifile_exported user:mudIsa/2.
:- dynamic_multifile_exported user:mudMoveDist/2.
:- dynamic_multifile_exported user:mudLabelTypeProps/3.
:- dynamic_multifile_exported user:mudFtInfo/2.
:- dynamic_multifile_exported user:mudContains/2.
:- dynamic_multifile_exported user:mud_test_local/2.
:- dynamic_multifile_exported user:mud_test_local/1.
:- dynamic_multifile_exported user:mud_test_local/0.
:- dynamic_multifile_exported user:mud_test/2.
:- dynamic_multifile_exported user:mud_test/1.
:- dynamic_multifile_exported user:mud_test/0.
:- dynamic_multifile_exported user:longitude/2.
:- dynamic_multifile_exported user:loaded_external_kbs/0.
:- dynamic_multifile_exported user:latitude/2.
:- dynamic_multifile_exported user:label_type/2.
:- dynamic_multifile_exported user:hasInstance_dyn/2.
:- dynamic_multifile_exported user:hooked_random_instance/3.
:- dynamic_multifile_exported user:decl_coerce/3.
:- dynamic_multifile_exported user:ft_info/2.
:- dynamic_multifile_exported user:subFormat/2.
:- dynamic_multifile_exported user:mudIsa/2.
:- dynamic_multifile_exported user:decl_type/1.
:- dynamic_multifile_exported user:mudSubClass/2.
:- dynamic_multifile_exported user:ttFormatType/1.
:- dynamic_multifile_exported user:forwardRule/2.
:- dynamic_multifile_exported user:fact_maybe_deduced/1.
:- dynamic_multifile_exported user:fact_is_false/2.
:- dynamic_multifile_exported user:default_type_props/3.
:- dynamic_multifile_exported user:deduce_facts/2.
:- dynamic_multifile_exported user:create_random_fact/1.
:- dynamic_multifile_exported user:check_permanence/4.
:- dynamic_multifile_exported user:agent_text_command/4.
:- dynamic_multifile_exported user:agent_call_command/2.
:- dynamic_multifile_exported user:action_verb_useable/4.
:- dynamic_multifile_exported user:action_rules/4.
:- dynamic_multifile_exported user:action_info/3.
:- dynamic_multifile_exported user:action_info/2.
:- dynamic_multifile_exported user:action_info/1.

