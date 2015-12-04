% Scheduling Module
%
% Part of the CHR-rp compiler.
%
% Author: Leslie De Koninck, K.U.Leuven, Belgium

:- module(scheduling,
	[
	    schedule_code/3
	]).

:- use_module(database,
	[
	    get_highest_priority/2,
	    get_first_occurrence/3
	]).

schedule_code(FA,SuspRef,Code) :-
	get_highest_priority(FA,P),
	(   P =:= 0
	->  Code = true
	;   get_first_occurrence(FA,P,Occ),
	    term_to_atom(occurrence(FA,P,Occ),CallFunctor),
	    Call =.. [CallFunctor,SuspRef],
	    Code = insert(P,Call)
	).
