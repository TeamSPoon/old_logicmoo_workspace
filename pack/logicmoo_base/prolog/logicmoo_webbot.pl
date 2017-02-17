/*  LogicMOO User Modules Setup
%
%
% Dec 13, 2035
% Douglas Miles
*/
:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )). 
:- endif.
:- module(logicmoo_webbot_file,[]).
% restore entry state
:- lmce:reset_modules.

%:- prolog_ide(thread_monitor).
%:- prolog_ide(debug_monitor).

:- system:use_module(library(debug)).
:- nodebug_logicmoo(_).
:- nodebug_logicmoo(logicmoo_webbot).

:- set_prolog_flag(lm_expanders,true).
:- set_prolog_flag(mpred_te,true).

:- ensure_loaded(library(logicmoo_utils)).
:- ensure_loaded(library(logicmoo_base)).
%:- ensure_loaded(library(logicmoo/mpred_online/mpred_www)).
%:- ensure_loaded(library(logicmoo/mpred_online/mpred_pldoc_util)).

:- initialization(ensure_webserver(3020)).
:- initialization(ensure_webserver(3020),restore).




:- initialization(wdmsg("Running Initialization/1")).
:- initialization(wdmsg("Initialization of restore state"),restore).

:- set_prolog_flag(lm_expanders,false).
:- set_prolog_flag(read_attvars,false).
:- set_prolog_flag(mpred_te,false).
:- set_lang(pl).
:- system:use_module(library(socket)).
:- user:use_module(library(eggdrop)).

:- initialization(eggdrop:egg_go,restore).
:- initialization(eggdrop:egg_go).
wbg:- eggdrop:egg_go.

:- set_prolog_flag(lm_expanders,true).
:- set_prolog_flag(mpred_te,true).



size666_stacks:- Six = 6, set_prolog_stack(global, limit(Six*10**9)),set_prolog_stack(local, limit(Six*10**9)),set_prolog_stack(trail, limit(Six*10**9)).

:- abolish(system:nondet/1).
:- at_start(size666_stacks).


:- baseKB:ensure_loaded(library(logicmoo_user),except([op(_,_,_)])).
%
%:- initialization(set_prolog_flag(mpred_te,false)).
%:- initialization(set_prolog_flag(mpred_te,false),restore).

%:- ensure_loaded(library(logicmoo/mpred_online/mpred_rdf)).

% :- tdebug.

% :- qsave_program('lm_user.prc').
%:- qsave_program('lm_webbot.prc').

:- threads.
%:- use_listing_vars.
:- statistics.
% :- repeat,sleep(3.0),fail.


