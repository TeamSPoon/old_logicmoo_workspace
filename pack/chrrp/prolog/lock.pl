% Variable Locking Module
%
% Part of the CHR-rp runtime system.
%
% Author: Leslie De Koninck, K.U.Leuven, Belgium

:- module(lock,
	[
	    lock/1,
	    unlock/1
	]).

attr_unify_hook(_,_) :- fail.	

lock(Term) :- 
	(   var(Term)
	->  put_attr(Term,lock,locked)
	;   term_variables(Term,Vars),
	    lock_vars(Vars)
	).

lock_vars([H|T]) :- put_attr(H,lock,locked), lock_vars(T).
lock_vars([]).

unlock(Term) :- term_variables(Term,Vars), unlock_vars(Vars).

unlock_vars([H|T]) :- del_attr(H,lock), unlock_vars(T).
unlock_vars([]).