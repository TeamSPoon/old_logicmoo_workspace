%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
% Dec 13, 2035
% Douglas Miles

:- module(sanity_bc,[]).

:- baseKB:use_module(library(logicmoo/logicmoo_user)).

:- begin_pfc.

bc_q(N) <- bc_p(N).

bc_p(a).
bc_p(b).


?- must(bc_p(b)).

%= nothing cached
?- listing(bc_q/1).

?- must(bc_q(b)).

%= something cached
?- listing(bc_q/1).

