/** <module>
% All modules are declared here so that this next lines dont have to be pasted into every file.
% Since this list will need at least 160 entries to cover the obj classes rooms and commands,
% we add the modules here to not waste 160^2 lines of text and having to not
% update 160+ files whenever a new module is used
%
% Project LogicMoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

/*
:- context_module(Ctx),
   asserta(moodb:loading_module_h(Ctx)),
   !. % 'format'('%        ~q.~n',[loading_module_h(Ctx)]).

*/

% :- set_prolog_flag(unknown,fail).
% :- set_prolog_flag(unknown,error).
% :- set_prolog_flag(double_quotes,atom).
:- set_prolog_flag(double_quotes,string).
:- set_prolog_flag(verbose_load,true).

%:- debug.
%:- ensure_loaded(logicmoo(logicmoo_util/logicmoo_util_library)).
%:- ensure_loaded(logicmoo(logicmoo_util/logicmoo_util_bugger)).

:- ensure_loaded(logicmoo(vworld/moo)).
:- ensure_loaded(logicmoo(vworld/dbase)).

% logicmoo vworld mud server
:- ensure_moo_loaded(logicmoo(vworld/world)).
:- ensure_moo_loaded(logicmoo(vworld/toploop_telnet)).
:- ensure_moo_loaded(logicmoo(vworld/toploop_npc)).
:- ensure_moo_loaded(logicmoo(vworld/parser_e2c)).
:- ensure_moo_loaded(logicmoo(vworld/parser_imperative)).

:- ensure_moo_loaded(logicmoo(vworld/moo_loader)).
:- ensure_moo_loaded(logicmoo(vworld/moo_testing)).

:-include(moo_loadall).


