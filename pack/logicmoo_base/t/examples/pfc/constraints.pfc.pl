:- use_module(library(logicmoo/logicmoo_user)).

% is this how to define constraints?

either(P,Q) ==> (not(P) ==> Q), (not(Q) ==> P).

(P,Q ==> false) ==> (P ==> not(Q)), (Q ==> not(P)).
