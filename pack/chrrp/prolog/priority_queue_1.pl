% Priority Queue Module
%
% Part of the CHR-rp runtime system.
%
% Author: Leslie De Koninck, K.U.Leuven, Belgium

:- module(priority_queue_1,
	[
	    delete_min/1,
	    find_min/2,
	    initialize/1,
	    insert/2	    
	]).

initialize(Size) :-
	length(Buckets,Size),
	maplist(=([]),Buckets),
	Table =.. [t|Buckets],
	nb_setval(table,t(Table,Size)).

insert(Priority,Item) :-
	nb_getval(table,Table),
	Table = t(T,C),
	arg(Priority,T,Bucket),
	setarg(Priority,T,[Item|Bucket]),
	(   Priority =< C
	->  setarg(2,Table,Priority)
	;   true
	).

find_min(QPrio,QItem) :-
	nb_getval(table,Table),
	Table = t(T,C),
	functor(T,_,Max),
	find_first(C,Max,T,Table,QPrio,QItem).

find_first(Prio,Max,T,Table,QPrio,QItem) :-
	Prio =< Max,
	(   arg(Prio,T,[QItem|_])
	->  QPrio = Prio,
	    setarg(2,Table,Prio)
	;   NextPrio is Prio + 1,
	    find_first(NextPrio,Max,T,Table,QPrio,QItem)
	).

delete_min(Priority) :- 
	nb_getval(table,t(T,_)),
	arg(Priority,T,[_|Bucket]),
	setarg(Priority,T,Bucket).