#!/usr/bin/env swipl

:- module(sanity_ks_two,[]).

:- use_module(library(logicmoo_utils)).


:-export(demo_nb_linkval/1).
demo_nb_linkval(T) :-
        T = nice(N),
        (   N = world,
            nb_linkval(myvar, T),
            fail
        ;   nb_getval(myvar, V),
            writeln(V)
        ).

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




q :- q(X), writeln(X).
q(X) :- depth_of_var(X, D), format('Depth = ~w~n', [D]), D < 5, q(X),notail.

notail.

/*
Running this says:

1 ?- q.
Depth = 1
Depth = 2
Depth = 3
Depth = 4
Depth = 5

*/
