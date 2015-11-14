#!/usr/bin/env swipl
%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
% Dec 13, 2035
% Douglas Miles

:- module(sanity_sk_02,[]).

:- use_module(library(logicmoo_utils)).
:- use_module(library(logicmoo_base)).

:- mpred_pfc_file.

:- debug(_).
:- nodebug(http(_)).

:- dynamic(sk_out/1).
:- dynamic(sk_in/1).

:- begin_pfc.

% :- process_this_script.

:- read_attvars(true).

:- debug(mpred).
:- mpred_trace_exec.
:- autoload.

:- visible(+all).
:- leash(-all).
:- leash(+exception).
sk_in(Ex)==>sk_out(Ex).

:- visible(+all).
:- leash(-all).
:- leash(+exception).

sk_in(avar([vn='Ex',sk='SKF-666'])).

:- must((sk_out(Ex),get_attr(Ex,sk,What),What='SKF-666')).




