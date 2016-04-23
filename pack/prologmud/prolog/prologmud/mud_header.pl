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

:-shared_multifile(  irc_event_hooks/3).
:-shared_multifile(  irc_event_hooks/3).

:-shared_multifile(  deliver_event_hooks/2).
:-shared_multifile(  deliver_event_hooks/2).

:- shared_multifile   irc_user_plays/3.

:- thread_local t_l:wants_logout/1.
:- shared_multifile t_l:wants_logout/1.

:- shared_multifile   thglobal:session_io/4, thglobal:session_agent/2, thglobal:agent_session/2,   telnet_fmt_shown/3,   agent_action_queue/3.
:- shared_multifile thglobal:session_io/4, thglobal:session_agent/2, thglobal:agent_session/2,   telnet_fmt_shown/3,   agent_action_queue/3.

:- shared_multifile   mudDescription/1.
:- shared_multifile   term_specifier_text/2.
:- shared_multifile   type_action_info/3.
:- shared_multifile   update_charge/2.
:- shared_multifile   update_stats/2.
:- shared_multifile   use_usable/4.
:- shared_multifile   verb_alias/2.
:- shared_multifile   vtActionTemplate/1.
:- shared_multifile   mud_test/0.
:- shared_multifile   mud_test/1.
:- shared_multifile   mud_test/2.
:- shared_multifile   mud_test_local/0.
:- shared_multifile   mud_test_local/1.
:- shared_multifile   mud_test_local/2.
:- shared_multifile   world_agent_plan/3.
:- shared_multifile   action_info/2.
:- shared_multifile   action_rules/4.
:- shared_multifile   action_verb_useable/4.
:- shared_multifile   a_command/2.
:- shared_multifile   a_command_fallback/2.
:- shared_multifile   agent_text_command/4.
:- shared_multifile   check_permanence/4.



:-op(0,fx,  ('disabled')).
:-op(0,fx,  ('enabled')).
:-op(0,fy,  ('disabled')).
:-op(0,fy,  ('enabled')).
:- '@'(ensure_loaded(library(logicmoo/util/logicmoo_util_bugger)),user).

:-shared_multifile(user_db:grant_openid_server/2).
:-shared_multifile(user_db:grant_openid_server/2).
:- shared_multifile '$was_imported_kb_content$'/2.
:- discontiguous('$was_imported_kb_content$'/2).
:- shared_multifile(  disabled/1).
:- discontiguous(  disabled/1).
:- shared_multifile(  enabled/1).
:- discontiguous(  enabled/1).
:- shared_multifile   was_enabled/1.
:- discontiguous(  was_enabled/1).
:- shared_multifile   listing_mpred_hook/1.
:- shared_multifile   listing_mpred_hook/1.


:- shared_multifile   genls/2.
:- shared_multifile(  isa/2).

:- style_check((-(singleton))).

/*
:-op(1190,fx,  (disabled)).
:-op(1190,fx,  (enabled)).
:-op(1190,fy,  (disabled)).
:-op(1190,fy,  (enabled)).
:-op(1120,fx,  (export)).
:-op(1120,fx,  (shared_multifile)).
*/

:- set_prolog_flag(double_quotes, atom).
:- set_prolog_flag(double_quotes, string). 
:- set_prolog_flag(generate_debug_info, true).

% these do not get defined!?
% :-shared_multifile user_db:assert_user/2, user_db:grant_openid_server/2, user_db:retractall_grant_openid_server/2, user_db:retractall_user/2, user_db:assert_grant_openid_server/2.

:- shared_multifile(mpred_online:semweb_startup).

:-shared_multifile(  tChannel/1).


:- shared_multifile thglobal:pfcManageHybrids/0.
:- shared_multifile t_l:infMustArgIsa/0.
:- thread_local t_l:into_form_code/0.
:- thread_local t_l:current_local_why/2.
:- shared_multifile   defnSufficient/2.
:- thread_local   t_l:repl_to_string/2.
:- thread_local   t_l:repl_writer/2.
:- shared_multifile   loaded_external_kbs/0.
:- shared_multifile   loading_module_h/1.
:- shared_multifile   registered_module_type/2.
:- shared_multifile   must_compile_special_clause_file/1.

% HOOKS
:- shared_multifile   decl_coerce/3.
:- shared_multifile   listen_to_ops/2.
:- shared_multifile   deduce_facts/2.
:- shared_multifile   default_type_props/3.
:- shared_multifile   fact_always_true/1.
:- shared_multifile   fact_is_false/2.
:- shared_multifile   fact_maybe_deduced/1.
:- shared_multifile   tms_reject_why/2.
:- shared_multifile   hook_coerce/3.

:- shared_multifile   create_random_fact/1.
:- shared_multifile   local_term_anglify/2.
:- shared_multifile   term_anglify_last/2.
:- shared_multifile   term_anglify_np/3.
:- shared_multifile   term_anglify_np_last/3.

:- shared_multifile   hooked_random_instance/3.

:- shared_multifile   now_unused/1.
:- shared_multifile   provide_mpred_read_attributes/3.
:- shared_multifile   provide_mpred_setup/4.
:- shared_multifile   provide_mpred_clauses/3.
:- shared_multifile   provide_mpred_op/2.
:- shared_multifile   provide_mpred_write_attributes/2.

% DYN HOOKS
:- shared_multifile   is_never_type/1.

% DYN FOR CODE
:- shared_multifile thglobal:after_mpred_load/0.
:- shared_multifile thglobal:use_cyc_database/0.

:- shared_multifile   fact_is_false/2.
:- shared_multifile   kbp_t_list_prehook/2.


% DYN KB
:- shared_multifile   only_if_pttp/0.
:- shared_multifile   use_kif/2.
:- shared_multifile   is_mpred_prop/2.
%:- shared_multifile   hasInstance_dyn/2.
:- shared_multifile   arity/2.
:- shared_multifile   mpred_prop/2.
:- shared_multifile   '<=>'/2.
% :- shared_multifile   ruleForward/2.
:- shared_multifile   ruleRewrite/2.
% :- shared_multifile   ruleBackward/2.

% :-must(not(  mpred_prop(t,prologHybrid))).


:- shared_multifile   term_specifier_text/2.
:- shared_multifile   update_charge/2.
:- shared_multifile   update_stats/2.
:- shared_multifile   use_usable/4.
:- shared_multifile   verb_alias/2.
:- shared_multifile   vtActionTemplate/1.
:- shared_multifile   mud_test/0.
:- shared_multifile   mud_test/1.
:- shared_multifile   mud_test/2.
:- shared_multifile   mud_test_local/0.
:- shared_multifile   mud_test_local/1.
:- shared_multifile   mud_test_local/2.
:- shared_multifile   world_agent_plan/3.
:- shared_multifile   action_info/2.
:- shared_multifile   action_rules/4.
:- shared_multifile   action_verb_useable/4.
:- shared_multifile   a_command/2.
:- shared_multifile   agent_text_command/4.
:- shared_multifile   check_permanence/4.

% :- file_begin(pfc).
:- enable_mpred_expansion.


