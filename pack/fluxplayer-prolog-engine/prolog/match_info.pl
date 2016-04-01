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

:- module(match_info, [
	set_match_id/1,
	get_match_id/1,

	set_our_role/1,
	get_our_role/1,
	
	set_startclock/1,
	get_startclock/1,

	set_playclock/1,
	get_playclock/1,

	set_current_state/1,
	get_current_state/1,
	
	set_current_best_move/1,
	get_current_best_move/1
	], eclipse_language).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- local variable(currentmatch, "none").
:- mode set_match_id(++).
set_match_id(V) :- setval(currentmatch, V).
get_match_id(V) :- getval(currentmatch, V).

:- local variable(our_role, none).
:- mode set_our_role(++).
set_our_role(V) :- setval(our_role, V).
get_our_role(V) :- getval(our_role, V).

:- local variable(startclock, 0).
:- mode set_startclock(++).
set_startclock(V) :- setval(startclock, V).
get_startclock(V) :- getval(startclock, V).

:- local variable(playclock, 0).
:- mode set_playclock(++).
set_playclock(V) :- setval(playclock, V).
get_playclock(V) :- getval(playclock, V).

:- local variable(current_state, []).
:- mode set_current_state(++).
set_current_state(V) :- setval(current_state, V).
get_current_state(V) :- getval(current_state, V).

:- local variable(current_best_move, none).
:- mode set_current_best_move(++).
set_current_best_move(V) :- setval(current_best_move, V).
get_current_best_move(V) :- getval(current_best_move, V).
