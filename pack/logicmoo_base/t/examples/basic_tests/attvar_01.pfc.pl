#!/usr/bin/env swipl

:- module(sanity_ks_two,[]).

% :- use_module(library(logicmoo_user)).

:- use_module(library(logicmoo_utils)).
:- use_module(library(logicmoo_base)).

:- mpred_pfc_file.

:- debug(_).
:- nodebug(http(_)).
:- mpred_trace_exec.
:- begin_pfc.

:- visible(+all).
:- leash(-all).
:- leash(+exception).
:- trace.

argsQuoted(my_sk).

:- read_attvars(true).

:- visible(+all).
:- leash(-all).
:- leash(+exception).
:- trace.

my_sk(avar([vn='Ex',sk='SKF-666'])).

:- visible(+all).
:- leash(-all).
:- leash(+exception).
:- trace.

:- must((my_sk(Ex),get_attr(Ex,sk,What),What='SKF-666')).


