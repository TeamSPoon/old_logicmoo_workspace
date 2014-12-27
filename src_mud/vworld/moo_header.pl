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

% these do not get defined!?
% :-dynamic user_db:assert_user/2, user_db:grant_openid_server/2, user_db:retractall_grant_openid_server/2, user_db:retractall_user/2, user_db:assert_grant_openid_server/2.

:- dynamic_multifile_exported decl_database_hook/2.
:- dynamic_multifile_exported deduce_facts/2.
:- dynamic_multifile_exported create_random_fact/1.
:- dynamic_multifile_exported hooked_random_instance/3.
:- dynamic_multifile_exported fact_maybe_deduced/1.

:- dynamic_multifile_exported term_anglify/2.

:- dynamic_multifile_exported(thlocal:tracing80/0).
:- dynamic_multifile_exported(thlocal:usePlTalk/0).

:- dynamic_multifile_exported hybrid_rule/2.

:- dynamic_multifile_exported loaded_external_kbs/0.


% ========================================
% decl_mpred_hybrid database
% ========================================

:- dynamic_multifile_exported action_info/2.
:- dynamic_multifile_exported action_rules/4.
:- dynamic_multifile_exported agent_call_command/2.
:- dynamic_multifile_exported agent_text_command/4.
:- dynamic_multifile_exported check_permanence/4.
:- dynamic_multifile_exported decl_mud_test/2.
:- dynamic_multifile_exported default_type_props/2.
:- dynamic_multifile_exported default_inst_props/3.
:- dynamic_multifile_exported label_type/2.
:- dynamic_multifile_exported label_type_props/3.
:- dynamic_multifile_exported mud_test/2.
:- dynamic_multifile_exported mud_test/1.
:- dynamic_multifile_exported mud_test/0.
:- dynamic_multifile_exported mud_test_local/2.
:- dynamic_multifile_exported mud_test_local/1.
:- dynamic_multifile_exported mud_test_local/0.
:- dynamic_multifile_exported now_unused/1.
:- dynamic_multifile_exported term_specifier_text/2.
:- dynamic_multifile_exported type_action_info/3.
:- dynamic_multifile_exported update_charge/2.
:- dynamic_multifile_exported update_stats/2.
:- dynamic_multifile_exported use_usable/4.
:- dynamic_multifile_exported movedist/2.
:- dynamic_multifile_exported verb_alias/2.
:- dynamic_multifile_exported world_agent_plan/3.
:- dynamic_multifile_exported contains/2.
:- dynamic_multifile_exported longitude/2.
:- dynamic_multifile_exported latitude/2.
:- dynamic_multifile_exported region/1.
:- dynamic_multifile_exported((actiontype/1,action_info/2,action_rules/4,type_action_info/3,term_specifier_text/2,action_verb_useable/4)).
:- dynamic_multifile_exported((decl_coerce)/3).
:- dynamic_multifile_exported((term_anglify/2,term_anglify_last/2, term_anglify_np/3,term_anglify_np_last/3)).
:- dynamic_multifile_exported((update_charge/2,update_stats/2)).
:- dynamic_multifile_exported(ft_info/2).

:- dynamic_multifile_exported thglobal:use_cyc_database/0.

end_of_file.


:- dynamic_multifile_exported ft_info/2.
:- dynamic_multifile_exported subft/2.
:- dynamic_multifile_exported col/1.


%  very very first import
:- within_user(ensure_loaded(logicmoo('vworld/moo.pl'))).

% :-context_module(Ctx),writeq(context_module(Ctx)),nl.
:-context_module(Ctx),asserta(context_module_h(Ctx)).

% These three are for use with Quintus
%:- prolog_flag(unknown,error,fail). % Not sure if this is needed for Quintus
:- within_user(ensure_loaded(library(random))).
%:- within_user(ensure_loaded(library(date))).
% This one is for use with SWI
:- within_user(ensure_loaded(library(quintus))).


