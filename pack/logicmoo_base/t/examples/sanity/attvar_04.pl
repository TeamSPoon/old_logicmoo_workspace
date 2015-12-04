#!/usr/bin/env swipl

:- module(sanity_ks_two,[]).

:- use_module(library(logicmoo_base)).

:- debug(_).
:- nodebug(http(_)).
:- debug(mpred).
:- mpred_trace_exec.

:- dynamic(sk_out/1).
:- dynamic(sk_in/1).

argsQuoted(my_sk).

:- read_attvars(true).

% :- file_begin(pl).
my_sk(avar([vn='Ex',sk='SKF-666'])).

:- must((my_sk(Ex),get_attr(Ex,sk,What),What=='SKF-666')).


