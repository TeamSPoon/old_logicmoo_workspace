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
:- op(1150,fx,decl_mpred_hybrid).

:- decl_mpred_hybrid(  irc_event_hooks/3).
:- decl_mpred_hybrid(  irc_event_hooks/3).

:- decl_mpred_hybrid(  deliver_event_hooks/2).
:- decl_mpred_hybrid(  deliver_event_hooks/2).

:- decl_mpred_hybrid   irc_user_plays/3.

:- thread_local t_l:wants_logout/1.
:- decl_mpred_hybrid t_l:wants_logout/1.

:- decl_mpred_hybrid   thglobal:session_io/4, thglobal:session_agent/2, thglobal:agent_session/2,   telnet_fmt_shown/3,   agent_action_queue/3.
:- decl_mpred_hybrid thglobal:session_io/4, thglobal:session_agent/2, thglobal:agent_session/2,   telnet_fmt_shown/3,   agent_action_queue/3.

:- decl_mpred_hybrid   mudDescription/1.
:- decl_mpred_hybrid   term_specifier_text/2.
:- decl_mpred_hybrid   type_action_info/3.
:- decl_mpred_hybrid   update_charge/2.
:- decl_mpred_hybrid   update_stats/2.
:- decl_mpred_hybrid   use_usable/4.
:- decl_mpred_hybrid   verb_alias/2.
:- decl_mpred_hybrid   vtActionTemplate/1.
:- decl_mpred_hybrid   mud_test/0.
:- decl_mpred_hybrid   mud_test/1.
:- decl_mpred_hybrid   mud_test/2.
:- decl_mpred_hybrid   mud_test_local/0.
:- decl_mpred_hybrid   mud_test_local/1.
:- decl_mpred_hybrid   mud_test_local/2.
:- decl_mpred_hybrid   world_agent_plan/3.
:- decl_mpred_hybrid   action_info/2.
:- decl_mpred_hybrid   action_rules/4.
:- decl_mpred_hybrid   action_verb_useable/4.
:- decl_mpred_hybrid   agent_command/2.
:- decl_mpred_hybrid   agent_command_fallback/2.
:- decl_mpred_hybrid   agent_text_command/4.
:- decl_mpred_hybrid   check_permanence/4.



:-op(0,fx,  ('disabled')).
:-op(0,fx,  ('enabled')).
:-op(0,fy,  ('disabled')).
:-op(0,fy,  ('enabled')).
:- '@'(ensure_loaded(library(logicmoo/util/logicmoo_util_bugger)),user).

:- decl_mpred_hybrid(user_db:grant_openid_server/2).
:- decl_mpred_hybrid(user_db:grant_openid_server/2).
:- decl_mpred_hybrid '$was_imported_kb_content$'/2.
:- discontiguous('$was_imported_kb_content$'/2).
:- decl_mpred_hybrid(  disabled/1).
:- discontiguous(  disabled/1).
:- decl_mpred_hybrid(  enabled/1).
:- discontiguous(  enabled/1).
:- decl_mpred_hybrid   was_enabled/1.
:- discontiguous(  was_enabled/1).
:- decl_mpred_hybrid   listing_mpred_hook/1.
:- decl_mpred_hybrid   listing_mpred_hook/1.


:- decl_mpred_hybrid   genls/2.
:- decl_mpred_hybrid(  isa/2).

:- style_check((-(singleton))).

/*
:-op(1190,fx,  (disabled)).
:-op(1190,fx,  (enabled)).
:-op(1190,fy,  (disabled)).
:-op(1190,fy,  (enabled)).
:-op(1120,fx,  (export)).
:-op(1120,fx,  (decl_mpred_hybrid)).
*/

:- set_prolog_flag(double_quotes, atom).
:- set_prolog_flag(double_quotes, string). 
:- set_prolog_flag(generate_debug_info, true).

% these do not get defined!?
% :- decl_mpred_hybrid user_db:assert_user/2, user_db:grant_openid_server/2, user_db:retractall_grant_openid_server/2, user_db:retractall_user/2, user_db:assert_grant_openid_server/2.

:- decl_mpred_hybrid(mpred_online:semweb_startup).

:- decl_mpred_hybrid(  tChannel/1).


:- decl_mpred_hybrid thglobal:pfcManageHybrids/0.
:- decl_mpred_hybrid t_l:infMustArgIsa/0.
:- thread_local t_l:into_form_code/0.
:- thread_local t_l:current_local_why/2.
:- decl_mpred_hybrid   defnSufficient/2.
:- thread_local   t_l:repl_to_string/2.
:- thread_local   t_l:repl_writer/2.
:- decl_mpred_hybrid   loaded_external_kbs/0.
:- decl_mpred_hybrid   loading_module_h/1.
:- decl_mpred_hybrid   registered_module_type/2.
:- decl_mpred_hybrid   must_compile_special_clause_file/1.

% HOOKS
:- decl_mpred_hybrid   decl_coerce/3.
:- decl_mpred_hybrid   listen_to_ops/2.
:- decl_mpred_hybrid   deduce_facts/2.
:- decl_mpred_hybrid   default_type_props/3.
:- decl_mpred_hybrid   fact_always_true/1.
:- decl_mpred_hybrid   fact_is_false/2.
:- decl_mpred_hybrid   fact_maybe_deduced/1.
:- decl_mpred_hybrid   tms_reject_why/2.
:- decl_mpred_hybrid   impl_coerce_hook/3.

:- decl_mpred_hybrid   create_random_fact/1.
:- decl_mpred_hybrid   local_term_anglify/2.
:- decl_mpred_hybrid   term_anglify_last/2.
:- decl_mpred_hybrid   term_anglify_np/3.
:- decl_mpred_hybrid   term_anglify_np_last/3.

:- decl_mpred_hybrid   hooked_random_instance/3.

:- decl_mpred_hybrid   now_unused/1.
:- decl_mpred_hybrid   provide_mpred_read_attributes/3.
:- decl_mpred_hybrid   provide_mpred_setup/4.
:- decl_mpred_hybrid   provide_mpred_clauses/3.
:- decl_mpred_hybrid   provide_mpred_op/2.
:- decl_mpred_hybrid   provide_mpred_write_attributes/2.

% DYN HOOKS
:- decl_mpred_hybrid   is_never_type/1.

% DYN FOR CODE
:- decl_mpred_hybrid thglobal:after_mpred_load/0.
:- decl_mpred_hybrid thglobal:use_cyc_database/0.

:- decl_mpred_hybrid   fact_is_false/2.
:- decl_mpred_hybrid(kbp_t_list_prehook/2).


% DYN KB
:- decl_mpred_hybrid   only_if_pttp/0.
:- decl_mpred_hybrid   use_kif/2.
:- decl_mpred_hybrid   is_mpred_prop/2.
%:- decl_mpred_hybrid   hasInstance_dyn/2.
:- decl_mpred_hybrid   arity/2.
:- decl_mpred_hybrid   mpred_prop/2.
:- decl_mpred_hybrid   '<=>'/2.
% :- decl_mpred_hybrid   ruleForward/2.
:- decl_mpred_hybrid   ruleRewrite/2.
% :- decl_mpred_hybrid   ruleBackward/2.

% :-must(not(  mpred_prop(t,prologHybrid))).


:- decl_mpred_hybrid   term_specifier_text/2.
:- decl_mpred_hybrid   update_charge/2.
:- decl_mpred_hybrid   update_stats/2.
:- decl_mpred_hybrid   use_usable/4.
:- decl_mpred_hybrid   verb_alias/2.
:- decl_mpred_hybrid   vtActionTemplate/1.
:- decl_mpred_hybrid   mud_test/0.
:- decl_mpred_hybrid   mud_test/1.
:- decl_mpred_hybrid   mud_test/2.
:- decl_mpred_hybrid   mud_test_local/0.
:- decl_mpred_hybrid   mud_test_local/1.
:- decl_mpred_hybrid   mud_test_local/2.
:- decl_mpred_hybrid   world_agent_plan/3.
:- decl_mpred_hybrid   action_info/2.
:- decl_mpred_hybrid   action_rules/4.
:- decl_mpred_hybrid   action_verb_useable/4.
:- decl_mpred_hybrid   agent_command/2.
:- decl_mpred_hybrid   agent_text_command/4.
:- decl_mpred_hybrid   check_permanence/4.

% :- file_begin(pfc).
:- enable_mpred_expansion.


