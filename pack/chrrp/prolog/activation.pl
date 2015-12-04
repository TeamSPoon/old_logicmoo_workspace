% Activation Module
%
% Part of the CHR-rp runtime system.
%
% Author: Leslie De Koninck, K.U.Leuven, Belgium

:- module(activation,
	[
	    check_activation/0,
	    check_activation/1
	]).

:- use_module(priority_queue,
	[
	    delete_min/1,
	    find_min/2
	]).

check_activation :-
	(   find_min(Prio,Item)
	->  delete_min(Prio),
	    call(Item),
	    check_activation
	;   true
	).
check_activation(Current) :-
	(   find_min(Prio,Item)
	->  (   Current > Prio
	    ->  delete_min(Prio),
		call(Item),
		check_activation(Current)
	    ;   true
	    )
	;   true
	).
