#!/usr/bin/env swipl

:- module(sanity_ks_two,[]).

% :- use_module(library(logicmoo_user)).

:- use_module(library(logicmoo_utils)).
:- use_module(library(logicmoo_base)).

:- mpred_pfc_file.

:- debug(_).
:- nodebug(http(_)).
:- begin_pfc.

% :- process_this_script.

:- read_skolems(true).

my_sk(avar([vn='Ex',sk='SKF-666'])).

:- must((my_sk(Ex),get_attr(Ex,sk,What),What='SKF-666')).


