/*

 _________________________________________________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/

/* Print term as a tree */

print_tree(T) :-
   numbervars(T,1,_),
   pt(T,0), nl, fail.
print_tree(_).

pt(A,I) :-
   as_is(A), !,
   tab(I), write(A), nl.
pt([T|Ts],I) :- !,
   pt(T,I),
   pl(Ts,I).
pt(T,I) :- !,
   T=..[F|As],
   tab(I), write(F), nl,
   I0 is I+3,
   pl(As,I0).

pl([],_) :- !.
pl([A|As],I) :- !,
   pt(A,I),
   pl(As,I).

as_is(V):-var(V).
as_is('$VAR'(_)).
as_is(A) :- atomic(A), !.
as_is('_'(_)) :- !.
as_is(X) :-
   need_print_quote(X).



report_item(none,_).
report_item(expr,Item) :-
   write_tree(Item), nl.
report_item(tree,Item) :-
   print_tree(Item), nl.
report_item(_,Item) :-
   print_tree(Item), nl.

need_print_quote(A&R) :-
   atom(A), !,
   quote_amp(R).
need_print_quote(_-_).
need_print_quote(_--_).
need_print_quote(_+_).
need_print_quote(verb(_,_,_,_,_)).
need_print_quote(wh(_)).
need_print_quote(name(_)).
need_print_quote(prep(_)).
need_print_quote(det(_)).
need_print_quote(quant(_,_)).
need_print_quote(int_det(_)).

quote_amp('$VAR'(_)) :- !.
quote_amp(R) :-
   need_print_quote(R).


