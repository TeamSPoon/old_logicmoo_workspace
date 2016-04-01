/*
    Copyright (C) 2009 Stephan Schiffel <stephan.schiffel@gmx.de>

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

:- module(debug_player, [
	setup_game/1,
	play_game/4
	], eclipse_language).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- comment(summary, "contains routines to play a game with the game player without sending and receiving messages").
:- comment(play_game/4, [
	summary: "plays the given games against random players",
	args: [
		"GameFile": "the name of a file containing the game rules",
		"RoleIndex": "the index of the role of this player",
		"StartClock": "the startclock",
		"PlayClock": "the playclock"
		],
	amode: play_game(++, ++, ++, ++),
	resat: no,
	fail_if: "GameFile does not exist.",
	see_also: []
	]).

:- comment(setup_game/1, [
	summary: "Sets up the player as if a start message for the given game was received. Startclock and playclock are set to one day. compute_best_move/2 is not called.",
	args: [
		"GameFile": "the name of a file containing the game rules"
		],
	amode: setup_game(++),
	resat: no,
	fail_if: "GameFile does not exist.",
	see_also: []
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(gameplayer).
:- use_module(message_handler).
:- use_module(game_description).
:- use_module(gdl_parser).
:- use_module(time_sync).
:- use_module(logger).
:- use_module(match_info).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- mode setup_game(++).
setup_game(GameFile) :-
	(exists(GameFile) ->
		load_rules_from_file(GameFile),
		get_matchid_from_filename(GameFile, MatchID),
		
		set_match_id(MatchID),
		
		current_time(Time),
		Deadline is Time+86400,
		set_deadline(Deadline),
		set_startclock(86400),
		set_playclock(86400),
		
		once(role(Role)),
		set_our_role(Role),
		
		initial_state(InitialState),
		set_current_state(InitialState),
		
		printf("Game \"%w\" set up successfully.%n",[MatchID])
	;
		printf("File \"%w\" doesn't exists!%n",[GameFile]),
		fail
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- mode play_game(++, ++, ++, ++).
play_game(GameFile, RoleIndex, StartClock, PlayClock) :-
	(exists(GameFile) ->
		load_rules_from_file(GameFile),
		(role2index(Role, RoleIndex) ->
			true
		;
			printf("Invalid role index: %w%n",[RoleIndex]),
			fail
		),
		get_matchid_from_filename(GameFile, MatchID),
		get_rules(Rules),
		printf("Start playing game \"%w\" ...%n",[MatchID]),
		fake_send_message(start(MatchID, Role, Rules, StartClock, PlayClock), _StartReply),
		initial_state(Z0),
		play_steps(Z0, MatchID, Role, [])
	;
		printf("File \"%w\" doesn't exists!%n",[GameFile]),
		fail
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- mode play_steps(++, ++, ++, ++).
play_steps(Z, MatchID, _Role, LastMoves) :-
	terminal(Z), !,
	printf("=========================================%n",[]),
	printf("final state: %w%nlast moves:%w%n",[Z, LastMoves]),
	fake_send_message(stop(MatchID, LastMoves), _StopReply).

play_steps(Z, MatchID, Role, LastMoves) :-
	printf("=========================================%n",[]),
	printf("current state: %w%nlast moves:%w%n",[Z, LastMoves]),
	fake_send_message(play(MatchID, LastMoves), MoveString),
	printf("Received move: \"%w\"%n",[MoveString]),
	(parse_gdl_term_string(MoveString, Move),
	 legal(Role, Move, Z) ->
		true
	;
		printf("Illegal move \"%w\" in state %w!%n",[MoveString, Z]),
		fail
	),
	roles(Roles),
	(foreach(R, Roles),
	 foreach(M, Moves),
	 param(Role, Move, Z) do
		(Role=R ->
			M=Move
		;
			get_random_move(R, M, Z)
		)
	),
	state_update(Z, Moves, Z1),
	play_steps(Z1, MatchID, Role, Moves).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- mode get_random_move(++, -, ++).
get_random_move(Role, Move, Z) :-
	findall(M, legal(Role, M, Z), Ms),
	sort(Ms, Ms1),
	MoveVector=..[[]|Ms1],
	I is (random mod length(Ms1))+1,
	arg(I, MoveVector, Move).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- mode get_matchid_from_filename(++, -).
get_matchid_from_filename(GameFile, MatchID) :-
	pathname(GameFile, _, MatchID, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- mode fake_send_message(+, -).
fake_send_message(start(MatchID, Role, Rules, StartClock, PlayClock), Reply) :- !,
	set_log_prefix(MatchID),
	current_time(LocalReceiveTime),
	(game_start(MatchID, Role, Rules, StartClock, PlayClock, LocalReceiveTime) ->
		true
	;
		printf("game_start failed!%n", []),
		fail
	),
	Reply="READY".

fake_send_message(play(MatchID, Moves), Reply) :- !,
	current_time(LocalReceiveTime),
	(game_play(MatchID, Moves, LocalReceiveTime, Reply) ->
		true
	;
		printf("game_play failed!%n", []),
		fail
	).

fake_send_message(stop(MatchID, Moves), Reply) :- !,
	current_time(LocalReceiveTime),
	(game_stop(MatchID, Moves, LocalReceiveTime) ->
		true
	;
		printf("game_stop failed!%n", []),
		fail
	),
	closelogs,
	Reply="DONE".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
