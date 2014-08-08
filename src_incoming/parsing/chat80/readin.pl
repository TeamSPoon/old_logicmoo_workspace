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

/* Read a sentence */

/* Read sentence */

read_in(P):-current_input(I),read_line_to_codes(I,Codes),code_to_atomic_list(Codes,P).
%code_to_atomic_list(end_of_file,[0]).
code_to_atomic_list(end_of_file,end_of_file).
code_to_atomic_list(end_of_file,O):-!,[bye]=O.
code_to_atomic_list(end_of_file,O):-!,[]=O.
code_to_atomic_list(Codes,P):-string_codes(S,Codes),atomic_list_concat(P, " ",S),!.
%read_in(P):-initread(L),words(P,L,[]),!,to_nl.

initread([K1,K2|U]):-get(K1),get0(K2),readrest(K2,U).

readrest(46,[]):-!.
readrest(63,[]):-!.
readrest(33,[]):-!.
readrest(K,[K1|U]):-K=<32,!,get(K1),readrest(K1,U).
readrest(_K1,[K2|U]):-get0(K2),readrest(K2,U).

words([V|U]) --> word(V),!,blanks,words(U).
words([]) --> [].

word(U1) --> [K],{lc(K,K1)},!,alphanums(U2),{name(U1,[K1|U2])}.
word(nb(N)) --> [K],{digit(K)},!,digits(U),{name(N,[K|U])}.
word(V) --> [K],{name(V,[K])}.

alphanums([K1|U]) --> [K],{alphanum(K,K1)},!,alphanums(U).
alphanums([]) --> [].

alphanum(95,95) :- !.
alphanum(K,K1):-lc(K,K1).
alphanum(K,K):-digit(K).

digits([K|U]) --> [K],{digit(K)},!,digits(U).
digits([]) --> [].

blanks--> [K],{K=<32},!,blanks.
blanks --> [].

digit(K):-K>47,K<58.

lc(K,K1):-K>64,K<91,!,K1 is K\/8'40.
lc(K,K):-K>96,K<123.

to_nl :-
   repeat,
   get0(10), !.
