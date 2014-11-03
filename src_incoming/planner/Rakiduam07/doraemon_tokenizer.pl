:- module(doraemon_tokenizer,[nextToken/5,nextLine/4]).

:- use_module(library(file_utils)).
:- use_module(library(lists)).

main([File]):-
%	file_name_extension(File,FileBase,'.pddl'),
%       	file_to_string( File, S ),
	open(File,'read',Stream),
	stream_to_string(Stream,S),
	analize(S,_,pos(1,1),O),
	displaypos(O),
	nl,nl.


analize(S,Sn,I,O):-
	nextToken(I,I1,T1,S,S1),
	nl,
	atom_codes(XT1,T1),display(XT1),nl,
	nextToken(I1,I2,T2,S1,S2),
	atom_codes(XT2,T2),display(XT2),nl,
	nextToken(I2,O,L3,S2,Sn),
	atom_codes(XL3,L3),display(XL3),nl.
	

lexerror(pos(Lin,Pos)) :-
	display('Lexical error in line '), display(Lin),
	display(' position '), display(Pos),
	nl.

displaypos(pos(Lin,Pos)) :-
	display('line '), display(Lin),
	display(' position '), display(Pos),
	nl.
	

incremecol(pos(A,B),pos(A,B1)):-
	B1 is B+1.
incremelin(pos(A,_B),pos(A1,1)):-
	A1 is A+1.

nextToken(I,O,Token,S,ST) :-
	tokenize1(S,ST,I,O,Token).


tokenize1([10|T1],T,I,O,Token) :-
	incremelin(I,I1),
	tokenize1(T1,T,I1,O,Token).
	

tokenize1([32|T1],T,I,O,Token) :-
	incremecol(I,I1),
	tokenize1(T1,T,I1,O,Token).

tokenize1([C1|T1],T,I,O,Token) :-
	C1 \== 32,
	C1 \== 10,
	incremecol(I,I1),
	tokenize2(T1,T,I1,O,RestToken),
	append([C1],RestToken,Token).
	
tokenize2([],[],I,I,[]).
tokenize2([C1|T1],T,I,O,Token) :-
	C1 \== 32,
	C1 \== 10,
	incremecol(I,I1),
	tokenize2(T1,T,I1,O,RestToken),
	append([C1],RestToken,Token).

tokenize2([C1|T1],T1,I,O,[]) :-
	(C1 == 32;
	C1 == 10),
	incremecol(I,O).
	

nextLine(I,O,S,St) :-
	endOfLine(S,St,I,O).

endOfLine([10|St],St,I,O) :-
	incremelin(I,O).

endOfLine([_|T],T1,I,O) :-
	incremecol(I,I1),
	endOfLine(T,T1,I1,O).
