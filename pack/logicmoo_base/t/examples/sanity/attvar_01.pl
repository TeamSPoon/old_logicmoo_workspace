#!/usr/bin/env swipl

:- module(sanity_ks_two,[]).

:- use_module(library(logicmoo_utils)).

:- debug(_).
% :- nodebug(http(_)).
:- debug(mpred).

% :- begin_file(pl).


:- dynamic(sk_out/1).
:- dynamic(sk_in/1).

:- read_attvars(true).

sk_in(avar([vn='Ex',sk='SKF-666'])).

:- listing(sk_in).

:- must((sk_in(Ex),get_attr(Ex,sk,What),What=='SKF-666')).





