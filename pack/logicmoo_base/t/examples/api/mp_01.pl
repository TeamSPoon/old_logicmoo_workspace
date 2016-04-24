#!/usr/bin/env swipl

:- module(sm1,[]).

:- meta_predicate(like_bag(^,-)).

:- use_module(library(logicmoo_user)).

like_bag(I,O):-copy_term(I,O).

'==>'(a,b).

pfcControlled(c).

c :- a.
