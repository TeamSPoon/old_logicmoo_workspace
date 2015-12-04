#!/usr/bin/env swipl
%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
% Dec 13, 2035
% Douglas Miles

:- module(sanity_sk_02,[]).

:- use_module(library(logicmoo_base)).

:- dynamic(sk_out/1).
:- dynamic(sk_in/1).

:- debug(_).
:- nodebug(http(_)).
:- debug(mpred).
:- mpred_trace_exec.
:- begin_pfc.

% :- process_this_script.

:- read_attvars(true).

sk_in(Ex)==>sk_out(Ex).

sk_in(avar([vn='ExIn',sk='SKF-666'])).

sk_in(fl(X,Y,X,Y)).

:- must((sk_out(Ex),get_attr(Ex,sk,What),What='SKF-666')).

:- prolog_load_context(module,M),module(M).


