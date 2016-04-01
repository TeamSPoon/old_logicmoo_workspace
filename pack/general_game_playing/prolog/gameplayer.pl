/*
    Copyright (C) 2008 Stephan Schiffel <stephan.schiffel@gmx.de>

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

:- module(gameplayer, [
	game_start/6,
	game_play/4,
	game_stop/3
	], eclipse_language).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- comment(summary, "contains the basic routines of a general game player to handle start, play, and stop messages").
:- comment(game_start/6, [
	summary: "is called after receiving the start message of a match",
	args: [
		"MatchID": "string containing the identifier of the current match",
		"Role": "the role name of this player",
		"Rules": "the game description as a list of prolog rules",
		"StartClock": "start clock in seconds",
		"PlayClock": "play clock in seconds",
		"MsgReceiveTime": "time when the message was received (in seconds since start of the process)"
		],
	amode: game_start(++, ++, +, ++, ++, ++),
	resat: no,
	fail_if: "None. Always succeed.",
	see_also: [game_start_timed_part/2, game_play/4, game_stop/3],
    desc: html("game_start/6 must return before the start clock times out, i.e., before MsgReceiveTime+StartClock-{Time for sending messages}")
	]).
:- comment(game_start_timed_part/2, [
	summary: "is called after receiving the start message of a match",
	args: [
		"InitialState": "the initial state of the match in form of a list of fluents",
		"Role": "the role name of this player"
		],
	amode: game_start_timed_part(++, ++),
	resat: no,
	fail_if: "None. Always succeed.",
	see_also: [game_start/6, timeout:timeout/3],
    desc: html("game_start_timed_part/2 should contain everything that is to be done within the start clock but that is not crucial for playing the game. game_start_timed_part/2 is called with timeout/3, that means it will be stopped automatically if the start clock is over.")
	]).
:- comment(game_play/4, [
	summary: "is called after receiving a play message of a match",
	args: [
		"MatchID": "string containing the identifier of the current match",
		"Moves": "the moves of the players in the last step",
		"MsgReceiveTime": "time when the message was received (in seconds since start of the process)",
		"MoveString": "the game description as a list of prolog rules"
		],
	amode: game_play(++,++,++,-),
	resat: no,
	fail_if: "None. Always succeed.",
	see_also: [game_start/6, game_stop/3, gdl_parser:convert_to_gdl_string/2],
    desc: html("game_play/4 must return before the play clock times out, i.e., before MsgReceiveTime+PlayClock-{Time for sending messages}. MoveString must be instantiated to a string containing a legal move in gdl format.")
	]).
:- comment(game_play_timed_part/2, [
	summary: "is called after receiving a play message of a match",
	args: [
		"CurrentState": "the current state of the match in form of a list of fluents",
		"Role": "the role name of this player"
		],
	amode: game_play_timed_part(++,++),
	resat: no,
	fail_if: "None. Always succeed.",
	see_also: [game_play/4, match_info:set_current_best_move/1, compute_best_move:compute_best_move/2],
    desc: html("game_play_timed_part/2 calls the search algorithm. game_play_timed_part/2 is called with timeout/3, that means it will be stopped automatically if the play clock is over. The best move found has to be stored using match_info:set_current_best_move/1.")
	]).
:- comment(game_stop/3, [
	summary: "is called after receiving the stop message of a match",
	args: [
		"MatchID": "string containing the identifier of the current match",
		"Moves": "the moves of the players in the last step",
		"MsgReceiveTime": "time when the message was received (in seconds since start of the process)"
		],
	amode: game_stop(++,++,++),
	resat: no,
	fail_if: "None. Always succeed.",
	see_also: [game_start/6, game_play/4],
    desc: html("game_stop/3 should contain all code necessary for cleaning up such that the player is ready for the next match.")
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(time_sync).
:- use_module(match_info).
:- use_module(logger).
:- use_module(gdl_parser).
:- use_module(game_description).
:- use_module(compute_best_move).

:- lib(timeout).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- mode game_start(++, ++, +, ++, ++, ++).
game_start(MatchID, Role, Rules, StartClock, PlayClock, MsgReceiveTime) :-
	% compute deadline
	Deadline is MsgReceiveTime+StartClock-1, % allow 1s message roundtrip time
	set_deadline(Deadline),
	
	% remember some information about the match
	set_match_id(MatchID),
	set_startclock(StartClock),
	set_playclock(PlayClock),
	set_our_role(Role),
	
	% load the rules
	load_rules(Rules),
	
	% compute and save the initial state
	initial_state(InitialState),
	set_current_state(InitialState),
	
	% do something until the deadline
	time_to_deadline(TimeToDeadline),
	(TimeToDeadline>0 ->
		timeout(
			game_start_timed_part(InitialState, Role),
			TimeToDeadline,
			logln("gameplayer.log", "timeout: game_start_timed_part stopped")
		)
	;
		logln("gameplayer.log","timeout: game_start stopped")
	),
	!.
game_start(MatchID, Role, Rules, StartClock, PlayClock, MsgReceiveTime) :-
	log_printf("gameplayer.log","error: %w failed!",[game_start(MatchID, Role, Rules, StartClock, PlayClock, MsgReceiveTime)]).

:- mode game_start_timed_part(++, ++).
game_start_timed_part(InitialState, Role) :-
	log_printf("gameplayer.log","our role: %w, initial state: %w",[Role, InitialState]),
	% Here you should do things like:
	% - analyzing the game
	% - generating an evaluation function
	% - trying to solve the game
	% This predicate here will be stopped automatically if the time runs out.
	% Be sure to save everything you need later in some non-logical storage (e.g., variable/1, store/1, array/1)
	logln("gameplayer.log","game_start_timed_part finished.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- local variable(submittedmove, []).

:- mode game_play(++,++,++,-).
game_play(_MatchID, Moves, MsgReceiveTime, MoveString) :-
	% compute deadline
	get_playclock(PlayClock),
	Deadline is MsgReceiveTime+PlayClock-1, % 1s message roundtrip time
	set_deadline(Deadline),
	
	update_current_state(Moves, CurrentState),
	get_our_role(Role),
	set_current_best_move([]),
	
	% compute best move
	time_to_deadline(TimeToDeadline),
	(TimeToDeadline>0 ->
		timeout(game_play_timed_part(CurrentState, Role), TimeToDeadline, logln(".log", "time is up ..."))
	;
		logln("gameplayer.log", "time is up ...")
	),
	
	% convert move to gdl string
	get_current_best_move(Move),
	setval(submittedmove, Move),
	convert_to_gdl_string(Move, MoveString),
	logln("gameplayer.log", best_move(Move)),
	!.
game_play(MatchID, Moves, MsgReceiveTime, MoveString) :-
	log_printf("gameplayer.log","error: %w failed!",[game_play(MatchID, Moves, MsgReceiveTime, MoveString)]),
	MoveString="fail".

:- mode game_play_timed_part(++, ++).
game_play_timed_part(CurrentState, Role) :-
	compute_best_move(CurrentState, Role),
	logln("gameplayer.log","compute_best_move finished.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- mode game_stop(++, ++, ++).
game_stop(_MatchID, Moves, _MsgReceiveTime) :-
	% compute state update
	update_current_state(Moves, CurrentState),
	
	% compute goal values
	roles(Roles),
	get_our_role(OurRole),
	(foreach(Role, Roles),
	 foreach(Value, Values), param(CurrentState, OurRole, OurValue) do (
		goal(Role, Value, CurrentState), !,
		(Role=OurRole -> Value=OurValue ; true)
	)),
	log_printf("gameplayer.log", "final value: %w, our value: %w", [Values, OurValue]),
	!.
game_stop(MatchID, Moves, MsgReceiveTime) :-
	log_printf("gameplayer.log","error: %w failed!",[game_stop(MatchID, Moves, MsgReceiveTime)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- mode update_current_state(++, -).
update_current_state(Moves, CurrentState) :-
	get_current_state(LastState),
	get_our_role(Role),
	getval(submittedmove, SubmittedMove),
	setval(submittedmove, []),
	(Moves\=[] ->
		does(Role, OurMove, Moves), !,
		(OurMove=SubmittedMove ->
			true
		;
			log_printf("gameplayer.log","possible error: we got a different move than we submitted (%w\\=%w)",[OurMove, SubmittedMove])
		),
		state_update(LastState, Moves, CurrentState),
		set_current_state(CurrentState)
	;
		% this is the case for the first play-message (no prior moves)
		CurrentState=LastState
	),
	logln("gameplayer.log", "=================================================="),
	logln("gameplayer.log", last_moves(Moves)-new_state(CurrentState)).
