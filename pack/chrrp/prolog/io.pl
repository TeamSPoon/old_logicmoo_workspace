% IO Module
%
% Part of the CHR-rp compiler.
%
% Author: Leslie De Koninck, K.U.Leuven, Belgium

:- module(io,
	[
	    read_file/2,
	    write_file/2
	]).
	
:- op(1180, xfx, ==>).
:- op(1180, xfx, <=>).
:- op(1150, fx, chr_constraint).
:- op(1100, xfx, @).
:- op(1190, xfx, pragma).
:- op(500, yfx, #).
:- op(1100, xfx, \).

read_file(File,Clauses) :-
	see(File),
	read_clauses(Clauses),
	seen.

read_clauses(Clauses) :-
	read_term(Clause,[module(io)]),
	(   Clause \== end_of_file
	->  Clauses = [Clause|Rest],
	    read_clauses(Rest)
	;   Clauses = []
	).

write_file(File,Clauses) :-
	tell(File),
	maplist(portray_clause,Clauses),
	told.