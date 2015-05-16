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

:-dynamic(user:irc_event_hooks/3).
:-multifile(user:irc_event_hooks/3).

:-dynamic(user:deliver_event_hooks/2).
:-multifile(user:deliver_event_hooks/2).

:- dynamic_multifile_exported user:irc_user_plays/3.

:- thread_local thlocal:wants_logout/1.
:- multifile thlocal:wants_logout/1.

:- dynamic   thglobal:session_io/4, thglobal:session_agent/2, thglobal:agent_session/2, user:telnet_fmt_shown/3, user:agent_action_queue/3.
:- multifile thglobal:session_io/4, thglobal:session_agent/2, thglobal:agent_session/2, user:telnet_fmt_shown/3, user:agent_action_queue/3.

:- dynamic_multifile_exported user:term_specifier_text/2.
:- dynamic_multifile_exported user:type_action_info/3.
:- dynamic_multifile_exported user:update_charge/2.
:- dynamic_multifile_exported user:update_stats/2.
:- dynamic_multifile_exported user:use_usable/4.
:- dynamic_multifile_exported user:verb_alias/2.
:- dynamic_multifile_exported user:vtActionTemplate/1.
:- dynamic_multifile_exported user:mud_test/0.
:- dynamic_multifile_exported user:mud_test/1.
:- dynamic_multifile_exported user:mud_test/2.
:- dynamic_multifile_exported user:mud_test_local/0.
:- dynamic_multifile_exported user:mud_test_local/1.
:- dynamic_multifile_exported user:mud_test_local/2.
:- dynamic_multifile_exported user:world_agent_plan/3.
:- dynamic_multifile_exported user:action_info/2.
:- dynamic_multifile_exported user:action_rules/4.
:- dynamic_multifile_exported user:action_verb_useable/4.
:- dynamic_multifile_exported user:agent_call_command/2.
:- dynamic_multifile_exported user:agent_call_command_fallback/2.
:- dynamic_multifile_exported user:agent_text_command/4.
:- dynamic_multifile_exported user:check_permanence/4.



:-op(0,fx,user:('disabled')).
:-op(0,fx,user:('enabled')).
:-op(0,fy,user:('disabled')).
:-op(0,fy,user:('enabled')).
:- '@'(ensure_loaded(library(logicmoo/util/logicmoo_util_bugger)),user).

:-dynamic_multifile_exported(user_db:grant_openid_server/2).
:-multifile(user_db:grant_openid_server/2).
:- multifile '$was_imported_kb_content$'/2.
:- discontiguous('$was_imported_kb_content$'/2).
:- multifile(user:disabled/1).
:- discontiguous(user:disabled/1).
:- multifile(user:enabled/1).
:- discontiguous(user:enabled/1).
:- multifile user:was_enabled/1.
:- discontiguous(user:was_enabled/1).
:- multifile user:listing_mpred_hook/1.
:- dynamic_multifile_exported user:listing_mpred_hook/1.

:- multifile user:genls/2.
:- dynamic_multifile_exported user:genls/2.
:- multifile user:isa/2.
:- dynamic_multifile_exported user:isa/2.

:- style_check(-singleton).


:-op(1190,fx,user:(disabled)).
:-op(1190,fx,user:(enabled)).
:-op(1190,fy,user:(disabled)).
:-op(1190,fy,user:(enabled)).
:-op(1120,fx,user:(export)).
:-op(1120,fx,user:(dynamic_multifile_exported)).

:- set_prolog_flag(double_quotes, atom).
:- set_prolog_flag(double_quotes, string). 
:- set_prolog_flag(generate_debug_info, true).

% these do not get defined!?
% :-dynamic_multifile_exported user_db:assert_user/2, user_db:grant_openid_server/2, user_db:retractall_grant_openid_server/2, user_db:retractall_user/2, user_db:assert_grant_openid_server/2.

:- multifile(mpred_online:semweb_startup).

:-dynamic(user:tChannel/1).
:-multifile(user:tChannel/1).



:- dynamic_multifile_exported thglobal:pfcManageHybrids/0.
:- dynamic_multifile_exported thlocal:infMustArgIsa/0.
:- thread_local thlocal:into_form_code/0.
:- thread_local thlocal:current_why/2.
:- dynamic_multifile_exported user:defnSufficient/2.
:- thread_local user:repl_to_string/2.
:- thread_local user:repl_writer/2.
:- dynamic_multifile_exported user:loaded_external_kbs/0.
:- dynamic_multifile_exported user:loading_module_h/1.
:- dynamic_multifile_exported user:registered_module_type/2.
:- dynamic_multifile_exported user:already_added_this_round/1.
:- dynamic_multifile_exported user:must_compile_special_clause_file/1.

% HOOKS
:- multifile user:decl_coerce/3.
:- multifile user:listen_to_ops/2.
:- multifile user:deduce_facts/2.
:- multifile user:default_type_props/3.
:- multifile user:fact_always_true/1.
:- multifile user:fact_is_false/2.
:- multifile user:fact_maybe_deduced/1.
:- multifile user:tms_reject_why/2.
:- multifile user:hook_coerce/3.

:- multifile user:create_random_fact/1.
:- multifile user:local_term_anglify/2.
:- multifile user:term_anglify_last/2.
:- multifile user:term_anglify_np/3.
:- multifile user:term_anglify_np_last/3.

:- multifile user:hooked_random_instance/3.

:- multifile user:now_unused/1.
:- multifile user:provide_mpred_read_attributes/3.
:- multifile user:provide_mpred_setup/4.
:- multifile user:provide_mpred_clauses/3.
:- multifile user:provide_mpred_op/2.
:- multifile user:provide_mpred_write_attributes/2.

% DYN HOOKS
:- dynamic_multifile_exported user:is_never_type/1.

% DYN FOR CODE
:- dynamic_multifile_exported thglobal:after_mpred_load/0.
:- dynamic_multifile_exported thglobal:use_cyc_database/0.

:- dynamic_multifile_exported user:fact_is_false/2.
:- dynamic_multifile_exported user:kbp_t_list_prehook/2.


% DYN KB
:- dynamic_multifile_exported user:only_if_pttp/0.
:- dynamic_multifile_exported user:use_snark/2.
:- dynamic_multifile_exported user:is_mpred_prop/2.
%:- dynamic_multifile_exported user:hasInstance_dyn/2.
:- dynamic_multifile_exported user:arity/2.
:- dynamic_multifile_exported user:mpred_prop/2.
:- dynamic_multifile_exported user:'<=>'/2.
% :- dynamic_multifile_exported user:ruleForward/2.
:- dynamic_multifile_exported user:ruleRewrite/2.
% :- dynamic_multifile_exported user:ruleBackward/2.

% :-must(not(user:mpred_prop(t,prologHybrid))).

:- retractall(thlocal:disable_mpred_term_expansions_locally/0).




:- multifile user:term_specifier_text/2.
:- multifile user:type_action_info/3.
:- multifile user:update_charge/2.
:- multifile user:update_stats/2.
:- multifile user:use_usable/4.
:- multifile user:verb_alias/2.
:- multifile user:vtActionTemplate/1.
:- multifile user:mud_test/0.
:- multifile user:mud_test/1.
:- multifile user:mud_test/2.
:- multifile user:mud_test_local/0.
:- multifile user:mud_test_local/1.
:- multifile user:mud_test_local/2.
:- multifile user:world_agent_plan/3.
:- multifile user:action_info/2.
:- multifile user:action_rules/4.
:- multifile user:action_verb_useable/4.
:- multifile user:agent_call_command/2.
:- multifile user:agent_text_command/4.
:- multifile user:check_permanence/4.

