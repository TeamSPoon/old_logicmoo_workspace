
end_of_file.

% These three are for use with Quintus
%:- prolog_flag(unknown,error,fail). % Not sure if this is needed for Quintus
%:- use_module(library(random)).
%:- use_module(library(date)).

% This one is for use with SWI
:- use_module(library(quintus)).

% logicmoo utils shared with other systems

:- set_prolog_flag(double_quotes, atom).
:- set_prolog_flag(double_quotes, string).

:- use_module(logicmoo(logicmoo_util/logicmoo_util_bugger)).
:- use_module(logicmoo(logicmoo_util/logicmoo_util_library)).
:- use_module(logicmoo(logicmoo_util/logicmoo_util_ctx_frame)).
:- use_module(logicmoo(logicmoo_util/logicmoo_util_strings)).
:- use_module(logicmoo(logicmoo_util/logicmoo_util_terms)).
:- use_module(logicmoo(logicmoo_util/logicmoo_util_dcg)).
:- use_module(logicmoo(vworld/moo)).
:- use_module(logicmoo(vworld/dbase)).




