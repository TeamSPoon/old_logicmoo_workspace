% Compiler Utilities Module
%
% Part of the CHR-rp compiler.
%
% Author: Leslie De Koninck, K.U.Leuven, Belgium

:- module(util,
	[
	    list_to_conj/2,
	    make_term/3,
	    maplist/5,
	    memberc/2,
	    numlist2/3,
	    select_eq/3,
	    varlist_substract/3,
	    zip/3
	]).

list_to_conj([H|T],Conj) :- list_to_conj(T,H,Conj).
list_to_conj([],true).

list_to_conj([H|T],E,(E,Conj)) :- list_to_conj(T,H,Conj).
list_to_conj([],E,E).

numlist2(B,E,L) :- numlist(B,E,L), !.
numlist2(_,_,[]).

make_term(Functor,Argument,Term) :-
	Term =.. [Functor,Argument].
	
varlist_substract([X|T],S,Rem) :-
	(   memberc(X,S)
	->  varlist_substract(T,S,Rem)
	;   Rem = [X|RemT],
	    varlist_substract(T,S,RemT)
	).
varlist_substract([],_,[]).

memberc(X,Xs) :- member(M,Xs), X == M, !.

zip([H1|T1],[H2|T2],[H1-H2|T12]) :- zip(T1,T2,T12).
zip([],[],[]).


select_eq([H|T],E,R) :-
	(   H == E
	->  R = T
	;   R = [H|R2],
	    select_eq(T,E,R2)
	).