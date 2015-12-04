#!/usr/bin/env swipl
%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
% Dec 13, 2035
% Douglas Miles
%  cls ; kill -9 %1 ; fg ; swipl -g "ensure_loaded(pack(logicmoo_base/t/examples/base/'sanity_abc.pfc'))."
 
:- dynamic(most/1).

meta_argtypes(most(ftAssertable)).

% BWD chaining
most((Q <- P))/mpred_literal(Q) ==> (Q <-(P, \+ ~(Q))).

% FWD chaining
most(P==>Q)/nonvar(Q) ==> (((P ==> most(Q)))).
% most(P==>Q)/(mpred_literal_nv(Q),if_missing_mask(Q,R,Test))  ==> ((P, (\+ R)/Test ) ==> Q).

% NEG chaining
most(~Q)/nonvar(Q)  ==>  (( \+ Q ) ==> ~ Q ).

% POS chaining
most(Q)/mpred_positive_literal(Q)  ==>  ( \+(~Q)  ==> Q ).
most(Q)/(mpred_literal_nv(Q),if_missing_mask(Q,R,Test)) ==> ( ( \+R /Test ) ==> Q ).


% most(Q) ==> if_missing(Q,Q).

%(most(P=>Q)/(mpred_literal_nv(Q),if_missing_mask(Q,R,Test)))  ==> ((P, \+ R/Test) => Q).
%(most(P=>Q)/nonvar(Q)) ==> (P => most(Q)).


