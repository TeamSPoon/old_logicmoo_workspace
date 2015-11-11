#!/usr/bin/env swipl

:- module(sanity_ks_two,[]).

% :- use_module(library(logicmoo_user)).

:- use_module(library(logicmoo_utils)).
:- use_module(library(logicmoo_base)).

:- mpred_pfc_file.

:- debug(_).
:- nodebug(http(_)).
:- begin_pfc.


forall(C, forall(G, exists(P,  grandparent(C,G) => (parent(C,P) & (parent(P,G)))))).

% forall(C, forall(G, grandparent(C,G) => exists(P, (parent(P,G) & parent(C,P))))).

grandparent(douglas,trudy).

% parent(douglas,eileen).

