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
:-op(0,fx,('disabled')).
:-op(0,fx,('enabled')).
:-op(0,fy,('disabled')).
:-op(0,fy,('enabled')).
:- '@'(ensure_loaded('../../src_lib/logicmoo_util/logicmoo_util_all'),user).

:-dynamic(user_db:grant_openid_server/2).
:-multifile(user_db:grant_openid_server/2).
:- multifile user:was_imported_kb_content/2.
:- discontiguous(user:was_imported_kb_content/2).
:- multifile(user:disabled/1).
:- discontiguous(user:disabled/1).
:- multifile(user:enabled/1).
:- discontiguous(user:enabled/1).
:- multifile user:was_enabled/1.
:- discontiguous(user:was_enabled/1).
:- multifile user:listing_mpred_hook/1.
:- dynamic user:listing_mpred_hook/1.

:- multifile user:genls/2.
:- dynamic user:genls/2.

:-op(1190,fx,(disabled)).
:-op(1190,fx,(enabled)).
:-op(1190,fy,(disabled)).
:-op(1190,fy,(enabled)).
:-op(1120,fx,(export)).
:-op(1120,fx,(dynamic_multifile_exported)).

:- set_prolog_flag(double_quotes, atom).
:- set_prolog_flag(double_quotes, string). 
:- set_prolog_flag(generate_debug_info, true).

% these do not get defined!?
% :-dynamic user_db:assert_user/2, user_db:grant_openid_server/2, user_db:retractall_grant_openid_server/2, user_db:retractall_user/2, user_db:assert_grant_openid_server/2.

:- multifile(user:semweb_startup).

:- dynamic thglobal:pfcManageHybrids/0.
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

:- multifile user:local_term_anglify/2.
:- multifile user:term_anglify_last/2.
:- multifile user:term_anglify_np/3.
:- multifile user:term_anglify_np_last/3.

% HOOKS
:- multifile user:create_random_fact/1.
:- multifile user:decl_coerce/3.
:- multifile user:decl_database_hook/2.
:- multifile user:deduce_facts/2.
:- multifile user:default_type_props/3.
:- multifile user:fact_always_true/1.
:- multifile user:fact_is_false/2.
:- multifile user:fact_maybe_deduced/1.
:- multifile user:tms_reject_why/2.
:- multifile user:fskel/7.
:- multifile user:hook_coerce/3.
:- multifile user:hooked_random_instance/3.

:- multifile user:now_unused/1.
:- multifile user:provide_mpred_read_attributes/3.
:- multifile user:provide_mpred_setup/4.
:- multifile user:provide_mpred_storage_clauses/3.
:- multifile user:provide_mpred_storage_op/2.
:- multifile user:provide_mpred_write_attributes/2.

% DYN HOOKS
:- dynamic_multifile_exported user:is_never_type/1.

% DYN FOR CODE
:- dynamic_multifile_exported thglobal:after_dbase_load/0.
:- dynamic_multifile_exported thglobal:use_cyc_database/0.
:- dynamic_multifile_exported thglobal:global_session_agent/2.

:- dynamic_multifile_exported user:fact_is_false/2.
:- dynamic_multifile_exported user:kbp_t_list_prehook/2.


% DYN KB
:- dynamic_multifile_exported user:only_if_pttp/0.
:- dynamic_multifile_exported user:use_snark/2.
:- dynamic_multifile_exported user:is_mpred_prop/3.
:- dynamic_multifile_exported user:hasInstance_dyn/2.
:- dynamic_multifile_exported user:arity/2.
:- dynamic_multifile_exported user:mpred_prop/2.
:- dynamic_multifile_exported user:'<=>'/2.
% :- dynamic_multifile_exported user:ruleForward/2.
:- dynamic_multifile_exported user:ruleRewrite/2.
% :- dynamic_multifile_exported user:ruleBackward/2.

% :- '@'(ensure_loaded(dbase),'user').

