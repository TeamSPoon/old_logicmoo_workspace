/*
    Copyright (C) 2008,2009 Stephan Schiffel <stephan.schiffel@gmx.de>

    This file is part of the GGP starter code.

    The GGP starter code is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    The GGP starter code is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with the GGP starter code.  If not, see <http://www.gnu.org/licenses/>.
*/

:- module(main, [
	start_game_player/0,
	start_game_player/1
	], eclipse_language).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- comment(summary, "contains the main routine to setup the game player").
:- comment(start_game_player/1, [
	summary: "starts the game player on the given port",
	args: [
		"Port": "the port to listen on"
		],
	amode: start_game_player(++),
	resat: no,
	fail_if: "None. Always succeed.",
	see_also: [start_game_player/0]
	]).
:- comment(start_game_player/0, [
	summary: "short for start_game_player(8000)",
	see_also: [start_game_player/1]
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(http_server).
:- use_module(http_method).
:- use_module(logger).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- local variable(int_handler).

start_game_player :-
	start_game_player(8000).
	
:- mode start_game_player(++).
start_game_player(Port) :-
	(get_flag(hostarch, "i386_linux") ->
		save_interrupt_handler,
		set_interrupt_handler(int, control_c_handler/0)
	;
		true
	),
	printf("starting game player on port %w ...\n", [Port]),
	flush(stdout),
	block(
		http_server(Port, 3),
		int, (
		cleanup,
		writeln("game player stopped with CTRL-C.")
		)).

save_interrupt_handler :-
	(get_interrupt_handler(int, OldControlCHandler, Module) ->
		true
	;
		OldControlCHandler=default/0,
		Module=main
	),
	setval(int_handler, OldControlCHandler@Module).

control_c_handler :-
	% write logs and reset the interrupt handler
	cleanup,
	% stop the server and exit normally
	exit_block(int).
	
cleanup :-
	% write all logs
	closelogs,
	% reset the interrupt handler
	(get_flag(hostarch, "i386_linux") ->
		getval(int_handler, OldControlCHandler@Module),
		set_interrupt_handler(int, OldControlCHandler)@Module
	;
		true
	).
