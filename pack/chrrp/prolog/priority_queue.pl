% Priority queue interface
%
% Part of the CHR-rp runtime system.
%
% Author: Leslie De Koninck

:- module(priority_queue,
	[
	    insert/2,
	    find_min/2,
	    delete_min/1
	]).
:- use_module(priority_queue_1,[]).
:- use_module(fib_heap,[]).

lowest_static_priority(2).

:- lowest_static_priority(LSP), priority_queue_1:initialize(LSP).

insert(Prio,Item) :-
	lowest_static_priority(LSP),
	(   Prio =< LSP
	->  priority_queue_1:insert(Prio,Item)
	;   fib_heap:insert(Prio,Item)
	).

find_min(Prio,Item) :-
	(   priority_queue_1:find_min(Prio,Item)
	->  true
	;   fib_heap:find_min(Prio,Item)
	).

delete_min(Prio) :-
	lowest_static_priority(LSP),
	(   Prio =< LSP
	->  priority_queue_1:delete_min(Prio)
	;   fib_heap:extract_min
	).