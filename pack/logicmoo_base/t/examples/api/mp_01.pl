#!/usr/bin/env swipl

:- module(sm1,[]).

:- meta_predicate(like_bag(^,-)).

:- user:ensure_loaded(library(logicmoo_base)).

like_bag(I,O):-copy_term(I,O).

'==>'(a,b).

pfcControlled(c).

c :- a.
