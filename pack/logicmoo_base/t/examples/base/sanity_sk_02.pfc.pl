#!/usr/bin/env swipl

:- module(sanity_sk_02,[]).

:- use_module(library(logicmoo_utils)).
:- use_module(library(logicmoo_base)).

:- mpred_pfc_file.

:- debug(_).
:- nodebug(http(_)).

:- dynamic(sk_out/1).

:- begin_pfc.

% :- process_this_script.

:- read_skolems(true).

:- mpred_trace_exec.

sk_in(avar([vn='Ex',sk='SKF-666'])).

sk_in(Ex)==>sk_out(Ex).

:- must((sk_out(Ex),get_attr(Ex,sk,What),What='SKF-666')).




