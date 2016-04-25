/*  LogicMOO User Modules Setup
%
%
% Dec 13, 2035
% Douglas Miles
*/
:- if(('$current_source_module'(CM),'$current_typein_module'(M),
  asserta(logicmoo_webbot_user_module_uses(M,CM)))).
:- endif.
:- module(logicmoo_webbot,
 [qsp/0]).


:- autoload.
:- use_module(library(logicmoo_utils)).
:- autoload.
:- use_module(library(eggdrop)).
:- use_module(library(debug)).
:- debug(logicmoo_webbot),nodebug(logicmoo_webbot).
qsp:- qsave_program(irc_webbot).
:- autoload. % system_markers
:- initialization(egg_go).
:- egg_go.

:- use_module(library(logicmoo_user)).

:- ensure_webserver(3020).
:- load_snark.
:- threads.
:- statistics.

