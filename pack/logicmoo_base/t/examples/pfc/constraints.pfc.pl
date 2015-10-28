:- use_module(library(logicmoo/logicmoo_user)).

% is this how to define constraints?

either(P,Q) ==> (( ~P) ==> Q), (( ~Q) ==> P).

(P,Q ==> false) ==> (P ==> ( ~Q)), (Q ==> ( ~P)).
