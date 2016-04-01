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

:- module(logger, [
	set_log_prefix/1,
	logln/2,
	log_printf/3,
	closelogs/0
	],  eclipse_language).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- comment(summary, "contains predicates for log output").
:- comment(logln/2, [
	summary: "writes a term to a logfile",
	args: [
		"LogID": "id of a log",
		"Term": "the term to be logged"
		],
	amode: logln(++, +),
	resat: no,
	fail_if: "None. Always succeed.",
	see_also: [log_printf/3, set_log_prefix/1],
    desc: html("logln/2 appends a line consisting of a timestamp and Term to the logfile. The name of the logfile is composed of a directory (default is \"logs\") the log prefix (typically the id of the current match) and LogID.")
	]).
:- comment(log_printf/3, [
	summary: "writes a term to a logfile",
	args: [
		"LogID": "id of a log",
		"FormatString": "a printf format string",
		"Parameters": "a list of positional parameters for the format string"
		],
	amode: log_printf(++, ++, +),
	resat: no,
	fail_if: "None. Always succeed.",
	see_also: [printf/2, logln/2, set_log_prefix/1],
    desc: html("log_printf/3 appends a line consisting of a timestamp and a string written by printf(FormatString, Parameters) to the logfile. The name of the logfile is composed of a directory (default is \"logs\") the log prefix (typically the id of the current match) and LogID.")
	]).
:- comment(set_log_prefix/1, [
	summary: "sets the prefix for newly opened log files",
	args: [
		"LogPrefix": "a string"
		],
	amode: set_log_prefix(++),
	resat: no,
	fail_if: "None. Always succeed.",
	see_also: [log_printf/3, logln/2],
    desc: html("The name of a logfile is composed of a directory (default is \"logs\") the log prefix (typically the id of the current match) and a LogID. set_log_prefix/1 sets the log prefix for all logs.")
	]).
:- comment(closelogs/0, [
	summary: "closes all log files",
	resat: no,
	fail_if: "None. Always succeed.",
    desc: html("Closes all log files and forces them to be written to disk.")
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(time_sync).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- local variable(log_dir, "logs").
:- local variable(log_prefix, "default").
:- local store(logs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- mode set_log_prefix(+).
set_log_prefix(Prefix) :-
	closelogs,
	setval(log_prefix, Prefix).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- mode logln(++, +).
logln(Log, Message) :-
	get_log_stream(Log, LogStream),
	current_time(LocalTime),
	local_time_to_external(LocalTime, ExternalTime),
	write_term(LogStream, ExternalTime:Message, [compact(true), attributes(full), depth(full), variables(full)]),
	nl(LogStream).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- mode log_printf(++, ++, +).
log_printf(Log, FormatString, Arguments) :-
	get_log_stream(Log, LogStream),
	current_time(LocalTime),
	local_time_to_external(LocalTime, ExternalTime),
	printf(LogStream, "%w : ", [ExternalTime]),
	printf(LogStream, FormatString, Arguments),
	nl(LogStream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- mode get_log_stream(++, -).
get_log_stream(Log, LogStream) :-
	store_get(logs, Log, LogStream), !.
get_log_stream(Log, LogStream) :-
	getval(log_prefix, Prefix),
	getval(log_dir, LogDir),
	(is_directory(LogDir) ->
		true
	;
		mkdir(LogDir)
	),
	concat_string([LogDir, "/", Prefix, "_", Log], LogFile),
	open(LogFile, append, LogStream, [flush(end_of_line)]),
	% replace line above with the next one for less expensive logging at the expense of losing part of the log in case of a crash
	% open(LogFile, append, LogStream),
	store_set(logs, Log, LogStream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

closelogs :-
	stored_keys_and_values(logs, LogsAndStreams),
	(foreach(_Log-Stream, LogsAndStreams) do close(Stream)),
	store_erase(logs),
	setval(log_prefix, "default").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
file_type(8'170000).
directory_flag(8'40000).

is_directory(File) :-
	get_file_info(File, mode, Mode),
	Mode /\ file_type =:= directory_flag.
