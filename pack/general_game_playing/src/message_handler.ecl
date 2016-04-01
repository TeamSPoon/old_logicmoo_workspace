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

:- module(message_handler).

:- export handle_message/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(gdl_parser).
:- use_module(logger).
:- use_module(gameplayer).
:- use_module(time_sync).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- mode handle_message(++, ++, -).
handle_message(MessageString, ReceiveTime, Reply) :-
	(parse_gdl_message_string(MessageString, Message) -> true ; writeln("error parsing message!"), fail),
	handle_message_switch(Message, MessageString, ReceiveTime, Reply).

:- mode handle_message_switch(+, ++, ++, -).
handle_message_switch(start(MatchID, Role, Rules, StartClock, PlayClock), MessageString, ReceiveTime, Reply) :- !,
	set_log_prefix(MatchID),
	logln("messages.txt", MessageString),
	external_time_to_local(ReceiveTime, LocalReceiveTime),
	game_start(MatchID, Role, Rules, StartClock, PlayClock, LocalReceiveTime),
	Reply="READY".

handle_message_switch(play(MatchID, Moves), MessageString, ReceiveTime, Reply) :- !,
	logln("messages.txt", MessageString),
	external_time_to_local(ReceiveTime, LocalReceiveTime),
	game_play(MatchID, Moves, LocalReceiveTime, Reply).

handle_message_switch(stop(MatchID, Moves), MessageString, ReceiveTime, Reply) :- !,
	logln("messages.txt", MessageString),
	external_time_to_local(ReceiveTime, LocalReceiveTime),
	game_stop(MatchID, Moves, LocalReceiveTime),
	closelogs,
	Reply="DONE".

handle_message_switch(kill(immediately), _MessageString, _ReceiveTime, _Reply) :- !,
	exit_block(int).
