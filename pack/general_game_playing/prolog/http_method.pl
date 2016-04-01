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

:- module(http_method).

:- use_module(message_handler).
:- use_module(time_sync).

http_method("POST", _Url, Message, Reply, 200, [contentLength(CL)]) :-
	current_time(LocalTime),
	local_time_to_external(LocalTime, ReceiveTime),
	% Actually, the current time is not the receive time,
	% since the message could have been in the queue for a while, if
	% messages arrive faster than they are handled. However, this is the best, we can do
	% without writing our own http_server library.
	handle_message(Message, ReceiveTime, Reply),
	!,
	string_length(Reply, CL).
http_method("POST", _, _, "", 400, []).
