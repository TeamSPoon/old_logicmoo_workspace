
end_of_file.

% These three are for use with Quintus
%:- prolog_flag(unknown,error,fail). % Not sure if this is needed for Quintus
%:- use_module(library(random)).
%:- use_module(library(date)).

% This one is for use with SWI
:- use_module(library(quintus)).

% logicmoo utils shared with other systems

:- include(logicmoo(logicmoo_util/logicmoo_util_header)).
:- use_module(logicmoo(vworld/moo)).
:- use_module(logicmoo(vworld/dbase)).




