/* Copyright (C) 1993 Swedish Institute of Computer Science */

%   File       : fastrw.pl
%   Author     : Mats Carlsson
%   Updated    : 3 September 1999
%   Purpose    : Fast term I/O

:- module(fastrw, [
	fast_read/1, fast_read/2,
	fast_write/1, fast_write/2,
	fast_buf_read/2,
	fast_buf_write/3
   ]).

fast_write(Term) :-
	current_output(Stream),
	stream_code(Stream, Code),
	do_fast_write(Term, Code, fast_write(Term)).

fast_write(Stream, Term) :-
	stream_code(Stream, Code),
	do_fast_write(Term, Code, fast_write(Stream,Term)).

fast_buf_write(Term, Size, Addr) :-
	open_buf_write(Code),
	do_fast_write(Term, Code, fast_buf_write(Term,Size,Addr)),
	c_buffer_data(Code, Size, Addr).

do_fast_write(Term, Code, Goal) :-
	numbervars(Term, 0, _),
	c_fast_write(Term, Code, RC),
	fastrw_check(RC, Goal),
	fail.
do_fast_write(_, _, _).

fast_read(Term) :-
	current_input(Stream),
	stream_code(Stream, Code),
	do_fast_read(Term0, Code, fast_read(Term)),
	Term = Term0.

fast_read(Stream, Term) :-
	stream_code(Stream, Code),
	do_fast_read(Term0, Code, fast_read(Stream,Term)),
	Term = Term0.

fast_buf_read(Term, Addr) :-
	open_buf_read(Code, Addr),
	do_fast_read(Term0, Code, fast_buf_read(Term,Addr)),
	Term = Term0.

do_fast_read(Term, Code, Goal) :-
	c_fast_read(Term, Code, RC),
	fastrw_check(RC, Goal).

fastrw_check(0, _).
fastrw_check(-1, Goal) :-
	prolog:illarg(existence(byte,0,past_end_of_stream), Goal, 0).
fastrw_check(-2, Goal) :-
	prolog:illarg(consistency('expected version','gotten version',0), Goal, 0).
fastrw_check(-3, Goal) :-
	prolog:illarg(consistency('wellformed string','actual input',0), Goal, 0).
fastrw_check(-4, Goal) :-
	prolog:illarg(system('term refs out of sync'), Goal, 0).

%----------------------------------------------------------------

:- dynamic foreign/2, foreign_resource/2, foreign_file/2.
foreign(plc_fast_write, c_fast_write(+term, +address, [-integer])).
foreign(plc_fast_read, c_fast_read(+term, +address, [-integer])).
foreign(plc_open_buf_write, open_buf_write([-address])).
foreign(plc_open_buf_read, open_buf_read([-address], +integer/*+address*/)).
foreign(plc_buffer_data, c_buffer_data(+address, -integer, -integer/*-address*/)).
%% [PD] Quintus 3.5 frw_init and frw_deinit must be callable in Quintus.
foreign(frw_init, c, init_fastrw(+integer)).
foreign(frw_deinit, c, deinit_fastrw(+integer)).


% qpc needs an explicit function list
foreign_resource(fastrw,
		 [init(frw_init),deinit(frw_deinit),
		  plc_fast_write,plc_fast_read,
		  plc_open_buf_write,plc_open_buf_read,
		  plc_buffer_data]).

%% [PD] Quintus 3.5
foreign_file(library(system(libpl)),
	     [frw_init,frw_deinit,
	      plc_fast_write,plc_fast_read,
	      plc_open_buf_write,plc_open_buf_read,
	      plc_buffer_data]).


%%% if SICSTUS
%/*
:- load_foreign_resource(library(system(fastrw))).
%*/
%%% endif SICSTUS
%%% if QUINTUS
/*
:- load_foreign_executable(library(system(libpl))),
   abolish(foreign_file, 2),
   abolish(foreign, 2).

:- initialization init_fastrw(42).
*/
%%% endif QUINTUS
