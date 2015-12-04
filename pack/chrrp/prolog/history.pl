% History Module
%
% Part of the CHR-rp runtime system.
%
% Author: Leslie De Koninck, K.U.Leuven, Belgium


:- module(history,
	[
	    not_in_history/3,
	    add_to_history/3,
	    new_history/1
	]).

% new_history(History)

new_history(ht(1,0,t(_))).

% add_to_history(History,Hash,Key)

add_to_history(History,Hash,Key) :-
	History = ht(Capacity,Size,Table),
	Index is (Hash mod Capacity) + 1,
	arg(Index,Table,Bucket),
	(   var(Bucket)
	->  Bucket = [Key-Hash]
	;   setarg(Index,Table,[Key-Hash|Bucket])
	),
	NewSize is Size + 1,
	setarg(2,History,NewSize),
	(   NewSize < Capacity
	->  true
	;   double_size(History)
	).

% not_in_history(History,Hash,Key)

not_in_history(ht(Capacity,_,Table),Hash,Key) :-
	Index is (Hash mod Capacity) + 1,
	arg(Index,Table,Bucket),
	(   var(Bucket)
	->  true
	;   \+ memberchk(Key-Hash,Bucket)
	).


double_size(History) :-
	History = ht(Capacity,_,Table),
	NewCapacity is Capacity * 2,
	setarg(1,History,NewCapacity),
	Table =.. [_|Buckets],
	functor(NewTable,t,NewCapacity),
	setarg(3,History,NewTable),
	process_buckets(Buckets,NewTable,NewCapacity).

process_buckets([H|T],Table,Capacity) :-
	(   nonvar(H)
	->  process_bucket(H,Table,Capacity)
	;   true
	),
	process_buckets(T,Table,Capacity).
process_buckets([],_,_).

process_bucket([H|T],Table,Capacity) :-
	H = _-Hash,
	Index is (Hash mod Capacity) + 1,
	arg(Index,Table,Bucket),
	(   var(Bucket)
	->  Bucket = [H]
	;   setarg(Index,Table,[H|Bucket])
	),
	process_bucket(T,Table,Capacity).
process_bucket([],_,_).
 