% Suspension Term Layout Module
%
% Part of the CHR-rp compiler.
%
% Author: Leslie De Koninck, K.U.Leuven, Belgium

:- module(suspension,
	[
	    new_suspension/2,
	    suspension_args/3,
	    suspension_term_field/4,
	    suspension_term_field_nb/3,
	    fix_suspension_layout/0,
	    check_uses_history/1
	]).
:- use_module(library(chr)).
:- use_module(database,
	[
	    get_nb_constraint_indexes/2,
	    get_constraint_index/3
	]).
:- use_module(util,
	[
	    numlist2/3,
	    make_term/3
	]).

:- chr_constraint
	% new_suspension(F/A,Suspension)
	new_suspension(+,?),
	% suspension_args(F/A,Suspension,Args)
	suspension_args(+,?,?),
	% suspension_term_field(F/A,Suspension,FieldName,Field)
	suspension_term_field(+,?,+,?),
	% suspension_term_field_nb(F/A,FieldName,FieldNb)
	suspension_term_field_nb(+,+,?),
	% suspension_term_fields(F/A,Fields)
	suspension_term_fields(+,?),
	% fix_suspension_layout
	fix_suspension_layout,
	% uses_history(F/A)
	uses_history(+),
	% check_uses_history(F/A)
	check_uses_history(+).

fix_suspension_layout \ new_suspension(FA,Suspension) <=>
	suspension_term_fields(FA,Fields),
	length(Fields,L),
	functor(Suspension,suspension,L).

fix_suspension_layout \ suspension_args(FA,Suspension,Args) <=>
	suspension_term_fields(FA,Fields),
	sublist(functor2(arg,1),Fields,ArgFields),
	maplist(suspension_term_field(FA,Suspension),ArgFields,Args).
suspension_args(_/A,_,Args) ==> length(Args,A).

functor2(F,A,FA) :- functor(FA,F,A).

uses_history(FA) \ uses_history(FA) <=> true.

uses_history(FA) \ check_uses_history(FA) <=> true.
check_uses_history(_) <=> fail.

suspension_term_field(FA,_,history,_) ==> uses_history(FA).
suspension_term_field_nb(FA,history,_) ==> uses_history(FA).

fix_suspension_layout \ 
    suspension_term_field(FA,Suspension,FieldName,Field) <=>
	suspension_term_field_nb(FA,FieldName,FieldNb),
	arg(FieldNb,Suspension,Field).

fix_suspension_layout \ suspension_term_field_nb(FA,FieldName,FieldNb) <=>
	suspension_term_fields(FA,Fields),
	nth1(FieldNb,Fields,FieldName), !.

fix_suspension_layout \ suspension_term_fields(F/A,Fields) <=>
	get_nb_constraint_indexes(F/A,Nb),
	numlist2(1,A,Nums1),
	maplist(make_term(arg),Nums1,Fields1),
	numlist2(1,Nb,Nums2),
	maplist(get_constraint_index(F/A),Indexes,Nums2),
	maplist(make_term(index),Indexes,Fields2),
	(   check_uses_history(F/A)
	->  History = history
	;   History = []
	),
	flatten([id,state,History,Fields1,Fields2],Fields).