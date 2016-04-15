#!/usr/bin/env swipl

:- module(sm1,[]).

:- meta_predicate(like_bag(^,-)).

like_bag(I,O):-copy_term(I,O).
