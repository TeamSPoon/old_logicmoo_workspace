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

:- module(compute_best_move, [
	compute_best_move/2
	], eclipse_language).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- comment(summary, "should contain the strategy of the player").
:- comment(compute_best_move/2, [
	summary: "computes the best move of the player",
	args: [
		"CurrentState": "the current state as a list of fluents",
		"Role": "the role name of this player"
		],
	amode: compute_best_move(++, ++),
	resat: no,
	fail_if: "None. Always succeed.",
	see_also: [gameplayer:game_play_timed_part/2, match_info:set_current_best_move/1],
    desc: html("compute_best_move/2 should search the game tree and store the best move in the current state with set_current_best_move/1. Subsequent calls to set_current_best_move/1 will overwrite the previously stored move. compute_best_move/2 will be stopped when the time runs out. The last move stored with set_current_best_move/1 will then be send to the game master.")
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(game_description).
:- use_module(match_info).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compute_best_move(CurrentState, Role) :-
	% first compute a legal move, so in case time runs out we can at least play legally
	legal(Role, LegalMove, CurrentState), !,
	set_current_best_move(LegalMove),
	% TODO: now search for the best move (call set_current_best_move/1 whenever you found a better move)
	true.
