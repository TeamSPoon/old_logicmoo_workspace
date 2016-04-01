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

:- module(time_sync, [
	sync_time/1,
	external_time_to_local/2,
	local_time_to_external/2,
	time_to_deadline/1,
	current_time/1,
	get_deadline/1,
	set_deadline/1
	], eclipse_language).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- local variable(deadline, 0.0), variable(timeoffset, 0.0).
:- % initialize timeoffset to something that is about right
	get_flag(unix_time, TUnix), % seconds since 01 Jan 1970
	statistics(session_time, SessionTimeSeconds),
	TimeOffset is SessionTimeSeconds-TUnix,
	setval(timeoffset, TimeOffset).

:- mode sync_time(++).
sync_time(TimeInMillis) :-
	statistics(session_time, SessionTimeSeconds),
	TimeOffset is SessionTimeSeconds-TimeInMillis/1000.0,
	setval(timeoffset, TimeOffset).

:- mode external_time_to_local(++,-).
external_time_to_local(ExternalTimeInMillis, LocalTime) :-
	getval(timeoffset, TimeOffset),
	LocalTime is ExternalTimeInMillis/1000.0+TimeOffset.

:- mode local_time_to_external(++,-).
local_time_to_external(LocalTime, ExternalTimeInMillis) :-
	getval(timeoffset, TimeOffset),
	ExternalTimeInMillis is integer(truncate(((LocalTime-TimeOffset)*1000))).

:- mode time_to_deadline(-).
time_to_deadline(SecondsToDeadline) :-
	getval(deadline, DeadLine),
	current_time(CurrentTime),
	SecondsToDeadline is DeadLine-CurrentTime.

:- mode current_time(-).
current_time(CurrentTime) :-
	statistics(session_time, CurrentTime).

:- mode get_deadline(-).
get_deadline(Deadline) :-
	getval(deadline, Deadline).

:- mode set_deadline(++).
set_deadline(Deadline) :-
	setval(deadline, Deadline).
