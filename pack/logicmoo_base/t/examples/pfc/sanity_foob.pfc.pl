/** <module>
%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
% Dec 13, 2035
% Douglas Miles
*/

:- use_module(library(logicmoo/logicmoo_user)).

if_missing(foob(_),foob(a)).

:- mpred_test(foob(a)).

foob(b).

:- mpred_test(\+foob(a)).
:- mpred_test(foob(b)).

~foob(b).

:- mpred_test(\+foob(b)).
:- mpred_test(foob(a)).






