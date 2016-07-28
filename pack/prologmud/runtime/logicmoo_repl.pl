#!/usr/bin/env swipl
/*
 Logic Engine startup

*/

:- set_prolog_stack(global, limit(16*10**9)).
:- set_prolog_stack(local, limit(16*10**9)).
:- set_prolog_stack(trail, limit(16*10**9)).

:- user:ensure_loaded(setup_paths).

% ==============================
% Load logicmoo REPL Base
% ==============================
:- asserta(baseKB:load_mud_www).
:- system:ensure_loaded(library(logicmoo_user)).

% :- statistics.
user:file_search_path(prologmud, library(prologmud)).
setup_rl_read_history_0:-
  ((current_prolog_flag(readline, true))->expand_file_name("~/.pl-history", [File|_]),(exists_file(File) -> rl_read_history(File); true),at_halt(rl_write_history(File));true).
:- setup_rl_read_history_0.
:- initialization(setup_rl_read_history_0,restore).

% ==============================
% Default Daemons
% ==============================

% :- use_module(library(persistency)).

:- asserta(baseKB:load_mud_www).


% [Optionaly] Load an Eggdrop (Expects you have  Eggdrop runinng with PROLOG.TCL scripts @ https://github.com/TeamSPoon/MUD_ircbot/)
:- if((fail , exists_source(library(eggdrop)))).
:- ensure_loaded(library(eggdrop)).
:- eggdrop:egg_go.
:- initialization((current_predicate(egg_go/0)->egg_go;true),now).
:- endif.

% [Required] Load the Logicmoo Base System
%USER :- time(system:ensure_loaded(logicmoo_user)).
% :- meta_predicate testml(//).



% [Optionaly] Load the Logicmoo WWW System
:- if(if_defined(baseKB:load_mud_www)).
:- system:use_module(library(logicmoo/mpred_online/mpred_www)).
/*
:- (if_file_exists(ensure_loaded(logicmoo(mpred_online/logicmoo_i_www)))).
:- (if_file_exists(ensure_loaded(library(logicmoo/logicmoo_run_pldoc)))).
:- (if_file_exists(ensure_loaded(library(logicmoo/logicmoo_run_swish)))).
:- (if_file_exists(ensure_loaded(library(logicmoo/logicmoo_run_clio)))).

:- ensure_webserver.
*/
:- endif.

:- (if_file_exists(system:ensure_loaded(library(logicmoo_user)))).

% ==============================
% Load the infernce engine
% ==============================

% [Required] Load the Logicmoo Backchaining Inference System
% :- gripe_time(40,with_no_mpred_expansions(if_file_exists(ensure_loaded(logicmoo(logicmoo_engine))))).

% :-  m1,m4,m1,egg_go.

