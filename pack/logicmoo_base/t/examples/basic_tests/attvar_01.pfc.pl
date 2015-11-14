#!/usr/bin/env swipl

:- module(sanity_ks_two,[]).

:- use_module(library(logicmoo_base)).

:- debug(_).
:- nodebug(http(_)).
:- mpred_trace_exec.
:- begin_pfc.

argsQuoted(my_sk).

:- read_attvars(true).
my_sk(avar([vn='Ex',sk='SKF-666'])).
:- must((my_sk(Ex),get_attr(Ex,sk,What),What='SKF-666')).


