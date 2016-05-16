/*  LogicMOO User Modules Setup
%
%
% Dec 13, 2035
% Douglas Miles
*/
:- if(( system:use_module(system:library('logicmoo/util/logicmoo_util_filesystem.pl')), push_modules)). 
:- endif.
:- module(logicmoo_webbot_file,[]).
% restore entry state
:- reset_modules.

:- prolog_ide(thread_monitor).
:- prolog_ide(debug_monitor).

:- system:use_module(library(debug)).
:- forall(debugging(X),nodebug(X)).
:- nodebug(logicmoo_webbot).

:- set_prolog_flag(lm_expanders,true).
:- set_prolog_flag(mpred_te,true).

:- system:use_module(library(logicmoo_utils)).
:- system:use_module(library(logicmoo_base)).
:- system:use_module(library(logicmoo/mpred_online/mpred_www)).
%:- system:use_module(library(logicmoo/mpred_online/mpred_pldoc_util)).
:- ensure_webserver(3020).

:- forall(debugging(X),nodebug(X)).

:- set_prolog_flag(lm_expanders,false).
:- set_prolog_flag(read_attvars,false).
:- set_prolog_flag(mpred_te,false).
:- use_module(library(socket)).
:- system:use_module(library(eggdrop)).
:- forall(debugging(X),nodebug(X)).
:- initialization(egg_go).
:- egg_go.

:- set_prolog_flag(lm_expanders,true).
:- set_prolog_flag(mpred_te,true).

:- forall(debugging(X),nodebug(X)).

:- system:use_module(library(logicmoo_user)).
%:- forall(debugging(X),nodebug(X)).

%:- system:use_module(library(logicmoo/mpred_online/mpred_rdf)).

% :- tdebug.

% :- qsave_program('lm_user.prc').
:- qsave_program('lm_webbot.prc').

:- threads.
%:- use_listing_vars.
:- statistics.
% :- repeat,sleep(3.0),fail.
