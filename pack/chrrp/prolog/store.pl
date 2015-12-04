% Constraint Store Module
%
% Part of the CHR-rp runtime system.
%
% Author: Leslie De Koninck, K.U.Leuven, Belgium

:- module(store,
	[
	    delete_first/2,
	    delete_key/2,
	    insert_in_store/4,
	    make_ground/3,
	    new_store/1,
	    store_lookup/3,
	    select_by_id/4
	]).

make_ground(Term,Module,GroundTerm) :-
	(   var(Term)
	->  get_attr(Term,Module,Attr),
	    arg(1,Attr,GroundTerm)
	;   functor(Term,F,A),
	    functor(GroundTerm,F,A),
	    make_ground(Term,Module,1,A,GroundTerm)
	).

make_ground(Term,Module,I,A,GroundTerm) :-
	(   I =< A
	->  arg(I,Term,Arg),
	    make_ground(Arg,Module,GroundArg),
	    arg(I,GroundTerm,GroundArg),
	    NextI is I + 1,
	    make_ground(Term,Module,NextI,A,GroundTerm)
	;   true
	).
	
new_store(ht(4,0,t(_,_,_,_))).

insert_in_store(Suspension,Key,Store,Result) :-
	Store = ht(Capacity,Size,Table),
	term_hash(Key,Hash),
	Index is (Hash mod Capacity) + 1,
	arg(Index,Table,Bucket),
	(   var(Bucket)
	->  Result = [],
	    Bucket = [Key - [Suspension]],
	    NewSize is Size + 1,
	    setarg(2,Store,NewSize),
	    (   NewSize < Capacity
	    ->  true
	    ;   double_size(Store)
	    )
	;   pair_by_key(Bucket,Key,Pair)
	->  Pair = _ - Suspensions,
	    Result = [Suspension|Suspensions],
	    setarg(2,Pair,Result)
	;   Result = [],
	    setarg(Index,Table,[Key - [Suspension]|Bucket]),
	    NewSize is Size + 1,
	    setarg(2,Store,NewSize),
	    (   NewSize < Capacity
	    ->  true
	    ;   double_size(Store)
	    )
	).

store_lookup(Key,ht(Capacity,_,Table),Suspensions) :-
	term_hash(Key,Hash),
	Index is (Hash mod Capacity) + 1,
	arg(Index,Table,Bucket),
	nonvar(Bucket),
	pair_by_key(Bucket,Key,_-Suspensions).


delete_key(Key,ht(Capacity,_,Table)) :-
	term_hash(Key,Hash),
	Index is (Hash mod Capacity) + 1,
	arg(Index,Table,Bucket),
	delete_pair_by_key(Bucket,Key,NewBucket),
	setarg(Index,Table,NewBucket).

delete_first(Key,ht(Capacity,_,Table)) :-
	term_hash(Key,Hash),
	Index is (Hash mod Capacity) + 1,
	arg(Index,Table,Bucket),
	pair_by_key(Bucket,Key,Pair),
	Pair = _-[_|Rest],
	setarg(2,Pair,Rest).

delete_pair_by_key([H|T],Key,L) :-
	(   H = Key - _
	->  L = T
	;   L = [H|T2],
	    delete_pair_by_key(T,Key,T2)
	).


double_size(Store) :-
	Store = ht(Capacity,_,Table),
	NewCapacity is Capacity * 2,
	setarg(1,Store,NewCapacity),
	Table =.. [_|Buckets],
	functor(NewTable,ht,NewCapacity),
	setarg(3,Store,NewTable),
	process_buckets(Buckets,NewTable,NewCapacity).

process_buckets([H|T],Table,Capacity) :-
	(   var(H)
	->  true
	;   process_bucket(H,Table,Capacity)
	),
	process_buckets(T,Table,Capacity).
process_buckets([],_,_).


process_bucket([H|T],Table,Capacity) :-
	H = Key - _,
	term_hash(Key,Hash),
	Index is (Hash mod Capacity) + 1,
	arg(Index,Table,Bucket),
	(   var(Bucket)
	->  Bucket = [H]
	;   setarg(Index,Table,[H|Bucket])
	),
	process_bucket(T,Table,Capacity).
process_bucket([],_,_).
	
	
pair_by_key([H|T],Key,Pair) :-
	(   H = Key - _
	->  Pair = H
	;   pair_by_key(T,Key,Pair)
	).
	
select_by_id([H|T],Id,IdFieldNb,Susps) :-
	(   arg(IdFieldNb,H,Id)
	->  Susps = T
	;   Susps = [H|Remaining],
	    select_by_id(T,Id,IdFieldNb,Remaining)
	).
