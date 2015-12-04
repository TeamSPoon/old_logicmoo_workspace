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

:- begin_pfc.

% :- process_this_script.

:- read_attvars(true).


sk_in(avar([vn='Ex',sk='SKF-666'])).

sk_in(Ex)==>sk_out(Ex).


:- must((sk_out(Ex),get_attr(Ex,sk,What),What='SKF-666')).




