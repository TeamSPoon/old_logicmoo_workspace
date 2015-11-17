#!/usr/bin/env swipl
%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
% Dec 13, 2035
% Douglas Miles
%  cls ; kill -9 %1 ; fg ; swipl -g "ensure_loaded(pack(logicmoo_base/t/examples/base/'sanity_abc.pfc'))."

:- module(sanity,[]).


:- dynamic((foob/1,if_missing/2,good/1)).

:- use_module(library(logicmoo_base)).

:- begin_pfc.

% this should have been ok
(if_missing(Missing,Create) ==> ((\+ Missing/(Missing\==Create), \+ Create , \+ ~(Create)) ==> Create)).
if_missing(Missing,Create) ==> 
 ( ( \+ Missing/(Missing\=@=Create)) ==> Create).

:- ain((good(X) ==> if_missing(foob(_),foob(X)))).

:- ain(good(az)).

:- mpred_test(foob(az)).

==> foob(b).

:- mpred_test(\+foob(az)).

==> (\+ foob(b)).

:- mpred_test(foob(az)).

:- rtrace(rem( good(az) )).

:- listing([foob,good]).

:- break.

:- mpred_test( \+foob(az)).



:- mpred_test(foob(b)).

~ foob(b).

:- mpred_test(\+foob(b)).
:- mpred_test(foob(az)).


