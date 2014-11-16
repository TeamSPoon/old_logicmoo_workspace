/** <module> 
% All modules are declared here so that this next lines dont have to be pasted into every file.
% Since this list will need at least 160 entries to cover the obj classes rooms and commands, 
% we add the modules here to not waste 160^2 lines of text and having to not 
% update 160+ files whenever a new module is used
%
% Project Logicmoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/


% :-set_prolog_flag(unknown,fail).
:- set_prolog_flag(double_quotes, atom).
:- set_prolog_flag(double_quotes, string). 

% these do not get defined!?
% :-dynamic user_db:assert_user/2, user_db:grant_openid_server/2, user_db:retractall_grant_openid_server/2, user_db:retractall_user/2, user_db:assert_grant_openid_server/2.

:- dynamic_multifile_exported hook:decl_database_hook/2.
:- dynamic_multifile_exported hook:deduce_facts/2.
:- dynamic_multifile_exported hook:create_random_fact/1.
:- dynamic_multifile_exported hook:hooked_random_instance/3.
:- dynamic_multifile_exported hook:fact_maybe_deduced/1.

:- dynamic_multifile_exported moo:term_anglify/2.

end_of_file.


:- dynamic_multifile_exported moo:ft_info/2.
:- dynamic_multifile_exported moo:subft/2.
:- dynamic_multifile_exported moo:type/1.


%  very very first import
:- within_user(ensure_loaded(logicmoo('vworld/moo.pl'))).

% :-context_module(Ctx),writeq(context_module(Ctx)),nl.
:-context_module(Ctx),asserta(moo:context_module_h(Ctx)).

% These three are for use with Quintus
%:- prolog_flag(unknown,error,fail). % Not sure if this is needed for Quintus
:- within_user(ensure_loaded(library(random))).
%:- within_user(ensure_loaded(library(date))).
% This one is for use with SWI
:- within_user(ensure_loaded(library(quintus))).


