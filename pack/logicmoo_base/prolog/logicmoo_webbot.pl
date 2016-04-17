/*  LogicMOO User Modules Setup
%
%
% Dec 13, 2035
% Douglas Miles
*/
:- if(('$current_source_module'(CM),'$current_typein_module'(M),
  asserta(logicmoo_webbot:user_module_uses(M,CM)))).
:- endif.
:- module(logicmoo_webbot,
 [qsp/0]).



:- use_module(library(eggdrop)).
:- use_module(library(debug)).
:- debug(_),nodebug(_).
qsp:- qsave_program(irc_webbot).
:- initialization(egg_go).
:- egg_go.

:- use_module(library(logicmoo_user)).

:- mpred_www:ensure_webserver(3020).
:- logicmoo_snark:load_snark.