%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
% Dec 13, 2035
% Douglas Miles

:- module(sanity_if_missong_01,[]).

:- use_module(library(logicmoo_user)).

:- begin_pfc.

% this should have been ok
% (if_missing(Missing,Create) ==> ((\+ Missing/(Missing\==Create), \+ Create , \+ ~(Create)) ==> Create)).
if_missing(Missing,Create) ==> 
 ( ( \+ (Missing/(Missing\=@=Create))) ==> Create).

if_missing(foob(_),foob(a)).

:- mpred_test(foob(a)).

foob(b).

:- mpred_test(\+foob(a)).
:- mpred_test(foob(b)).

~foob(b).

:- mpred_test(\+foob(b)).
:- mpred_test(foob(a)).






