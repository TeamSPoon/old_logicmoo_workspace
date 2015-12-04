:- module(fib_heap,[insert/2,find_min/2,extract_min/0]).
:- use_module(library(chr)).
:- chr_constraint 
	insert(+,?),
	extract_min,
	ch2rt(+int),
	min(+int,+number),
	item(+dense_int,?any,+number,+int,+dense_int),
	findmin,
	find_min(?,?).

:- chr_option(toplevel_show_store,off).

insert @ insert(Prio,Item) <=> next_key(Key), item(Key,Item,Prio,0,0), min(Key,Prio).

keep_min @ min(_,Prio1) \ min(_,Prio2) <=> Prio1 =< Prio2 | true.

extr      @ extract_min, min(Key,_), item(Key,_,_,_,_) <=> ch2rt(Key), findmin.
extr_none @ extract_min <=> fail. 

c2r      @ ch2rt(Parent) \ item(Key,Item,Prio,Rank,Parent)#X <=> item(Key,Item,Prio,Rank,0) pragma passive(X).
c2r_done @ ch2rt(_) <=> true.

findmin  @ findmin, item(Key,_,Prio,_,0) ==> min(Key,Prio).
foundmin @ findmin <=> true.

findmin2 @ min(Key,Prio), item(Key,Item,_Prio,_,_) \ find_min(QPrio,QItem) <=>
	QPrio = Prio, QItem = Item.
findmin2b @ find_min(_,_) <=> fail. 

same_rank @ item(Key1,Item1,Prio1,Rank,0), item(Key2,Item2,Prio2,Rank,0) <=>
	Rank1 is Rank + 1,
	(   Prio1 < Prio2 
	->  item(Key2,Item2,Prio2,Rank,Key1),
	    item(Key1,Item1,Prio1,Rank1,0)
	;   item(Key1,Item1,Prio1,Rank,Key2),
	    item(Key2,Item2,Prio2,Rank1,0)
	).

next_key(Key) :- 
	b_getval(ikey,Key),
	Key1 is Key + 1,
	b_setval(ikey,Key1).

:- nb_setval(ikey,1).