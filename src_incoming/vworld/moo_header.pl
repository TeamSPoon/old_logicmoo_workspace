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

:- context_module(Ctx),asserta(loading_module_h(Ctx)),
   !. % 'format'('%        ~q.~n',[loading_module_h(Ctx)]).


% :- set_prolog_flag(unknown,fail).
% :- set_prolog_flag(unknown,error).
:- set_prolog_flag(double_quotes,atom).
:- set_prolog_flag(double_quotes,string).
:- set_prolog_flag(verbose_load,true).

:- debug.
:- use_module(logicmoo(logicmoo_util/logicmoo_util_library)).
:- use_module(logicmoo(logicmoo_util/logicmoo_util_bugger)).
:- use_module(logicmoo(vworld/moo)).
:- use_module(logicmoo(vworld/dbase)).
:- use_module(logicmoo(vworld/world)).

:- begin_transform_moo_preds.


