#!/usr/bin/env swipl
%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
% Dec 13, 2035
% Douglas Miles
%  cls ; kill -9 %1 ; fg ; swipl -g "ensure_loaded(pack(logicmoo_base/t/examples/base/'sanity_abc.pfc'))."

:- module(sanity,[]).

:- use_module(library(logicmoo_base)). 

:-debug(mpred).
:-mpred_trace_exec.

:- file_begin(pfc).

:- include('df_include.pfc').
 
most(t(test1_2_3,1,2,3)).


?- listing(test1_2_3).

:- mpred_test(test1_2_3(1,2,3)).


