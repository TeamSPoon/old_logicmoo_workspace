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

% :- include(logicmoo(mpred/'mpred_header.pi')).

:- '$set_source_module'(baseKB).

:- file_begin(code).
:- op(1150,fx,kb_dynamic).

:- kb_dynamic(  irc_event_hooks/3).
:- kb_dynamic(  deliver_event_hooks/2).
:- kb_dynamic   irc_user_plays/3.

:- thread_local t_l:wants_logout/1.
:- kb_dynamic t_l:wants_logout/1.

:- kb_dynamic   lmcache:session_io/4, lmcache:session_agent/2, lmcache:agent_session/2,   telnet_fmt_shown/3,   agent_action_queue/3.
:- kb_dynamic lmcache:session_io/4, lmcache:session_agent/2, lmcache:agent_session/2,   telnet_fmt_shown/3,   agent_action_queue/3.

:- kb_dynamic   mudDescription/1.
:- kb_dynamic   term_specifier_text/2.
:- kb_dynamic   type_action_info/3.
:- kb_dynamic   update_charge/2.
:- kb_dynamic   update_stats/2.
:- kb_dynamic   use_usable/4.
:- kb_dynamic   verb_alias/2.
:- kb_dynamic   vtActionTemplate/1.
:- kb_dynamic   mud_test/0.
:- kb_dynamic   mud_test/1.
:- kb_dynamic   mud_test/2.
:- kb_dynamic   mud_test_local/0.
:- kb_dynamic   mud_test_local/1.
:- kb_dynamic   mud_test_local/2.
:- kb_dynamic   world_agent_plan/3.
:- kb_dynamic   action_info/2.
:- kb_dynamic   action_rules/4.
:- kb_dynamic   action_verb_useable/4.
:- kb_dynamic   agent_command/2.
:- kb_dynamic   agent_command_fallback/2.
:- kb_dynamic   agent_text_command/4.
:- kb_dynamic   check_permanence/4.



:-op(0,fx,  ('disabled')).
:-op(0,fx,  ('enabled')).
:-op(0,fy,  ('disabled')).
:-op(0,fy,  ('enabled')).
:- '@'(ensure_loaded(library(logicmoo/util/logicmoo_util_bugger)),user).

:- kb_dynamic(user_db:grant_openid_server/2).
:- kb_dynamic(user_db:grant_openid_server/2).
:- kb_dynamic '$was_imported_kb_content$'/2.
:- discontiguous('$was_imported_kb_content$'/2).
:- kb_dynamic(  disabled/1).
:- discontiguous(  disabled/1).
:- kb_dynamic(  enabled/1).
:- discontiguous(  enabled/1).
:- kb_dynamic   was_enabled/1.
:- discontiguous(  was_enabled/1).
:- kb_dynamic   listing_mpred_hook/1.


:- kb_dynamic   genls/2.
:- kb_dynamic(  isa/2).

:- style_check((-(singleton))).

/*
:-op(1190,fx,  (disabled)).
:-op(1190,fx,  (enabled)).
:-op(1190,fy,  (disabled)).
:-op(1190,fy,  (enabled)).
:-op(1120,fx,  (export)).
:-op(1120,fx,  (kb_dynamic)).
*/

:- set_prolog_flag(double_quotes, atom).
:- set_prolog_flag(double_quotes, string). 
:- set_prolog_flag(generate_debug_info, true).

% these do not get defined!?
% :- kb_dynamic user_db:assert_user/2, user_db:grant_openid_server/2, user_db:retractall_grant_openid_server/2, user_db:retractall_user/2, user_db:assert_grant_openid_server/2.

:- kb_dynamic(mpred_online:semweb_startup).

:- kb_dynamic(  tChannel/1).


:- kb_dynamic lmcache:pfcManageHybrids/0.
:- kb_dynamic t_l:infMustArgIsa/0.
:- thread_local t_l:into_form_code/0.
:- thread_local t_l:current_local_why/2.
:- kb_dynamic   defnSufficient/2.
:- thread_local   t_l:repl_to_string/2.
:- thread_local   t_l:repl_writer/2.
:- kb_dynamic   loaded_external_kbs/0.
:- kb_dynamic   loading_module_h/1.
:- kb_dynamic   registered_module_type/2.
:- kb_dynamic   must_compile_special_clause_file/1.

% HOOKS
:- kb_dynamic   decl_coerce/3.
:- kb_dynamic   listen_to_ops/2.
:- kb_dynamic   deduce_facts/2.
:- kb_dynamic   default_type_props/3.
:- kb_dynamic   fact_always_true/1.
:- kb_dynamic   fact_is_false/2.
:- kb_dynamic   fact_maybe_deduced/1.
:- kb_dynamic   tms_reject_why/2.
:- kb_dynamic   impl_coerce_hook/3.

:- kb_dynamic   create_random_fact/1.
:- kb_dynamic   local_term_anglify/2.
:- kb_dynamic   term_anglify_last/2.
:- kb_dynamic   term_anglify_np/3.
:- kb_dynamic   term_anglify_np_last/3.

:- kb_dynamic   hooked_random_instance/3.

:- kb_dynamic   now_unused/1.
:- kb_dynamic   provide_mpred_read_attributes/3.
:- kb_dynamic   provide_mpred_setup/4.
:- kb_dynamic   provide_mpred_clauses/3.
:- kb_dynamic   provide_mpred_op/2.
:- kb_dynamic   provide_mpred_write_attributes/2.

% DYN HOOKS
:- kb_dynamic   is_never_type/1.

% DYN FOR CODE
:- kb_dynamic lmcache:after_mpred_load/0.
:- kb_dynamic lmconf:use_cyc_database/0.

:- kb_dynamic   fact_is_false/2.
% :- kb_dynamic(kbp_t_list_prehook/2).


% DYN KB
:- kb_dynamic   only_if_pttp/0.
:- kb_dynamic   use_kif/2.
:- kb_dynamic   is_mpred_prop/2.
%:- kb_dynamic   hasInstance_dyn/2.
:- kb_dynamic   arity/2.
:- kb_dynamic   mpred_prop/2.
:- kb_dynamic   '<=>'/2.
% :- kb_dynamic   ruleForward/2.
:- kb_dynamic   ruleRewrite/2.
% :- kb_dynamic   ruleBackward/2.

% :-must(not(  mpred_prop(t,prologHybrid))).


:- kb_dynamic   term_specifier_text/2.
:- kb_dynamic   update_charge/2.
:- kb_dynamic   update_stats/2.
:- kb_dynamic   use_usable/4.
:- kb_dynamic   verb_alias/2.
:- kb_dynamic   vtActionTemplate/1.
:- kb_dynamic   mud_test/0.
:- kb_dynamic   mud_test/1.
:- kb_dynamic   mud_test/2.
:- kb_dynamic   mud_test_local/0.
:- kb_dynamic   mud_test_local/1.
:- kb_dynamic   mud_test_local/2.
:- kb_dynamic   world_agent_plan/3.
:- kb_dynamic   action_info/2.
:- kb_dynamic   action_rules/4.
:- kb_dynamic   action_verb_useable/4.
:- kb_dynamic   agent_command/2.
:- kb_dynamic   agent_text_command/4.
:- kb_dynamic   check_permanence/4.

% :- file_begin(pfc).
:- enable_mpred_expansion.


