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

:- module(game_description, [
	% interface to gdl games

	% for calling complex goal in the context of the game_description_interface module
	call_goal/3,

	% game stuff
	goal/3,
	legal/3,
	terminal/1,
	next/3,
	init/1,
	role/1,
	does/3,
	true/2,

	state_update/3,
	initial_state/1,
	roles/1,
	role2index/2,
	number_of_roles/1,

	% meta-gameing stuff
	game_rule/4,
	get_rules/1,

	% loading the rules of the game, and/or overriding the rules with better ones
	load_rules_from_file/1,
	load_rules/1
	], eclipse_language).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(gdl_parser).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- local reference(state, []).
:- local reference(moves, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- mode load_rules_from_file(++).
load_rules_from_file(Filename) :-
	open(Filename, read, InputStream),
	read_string(InputStream, end_of_file, _, GameDescription),
	close(InputStream),
	(parse_gdl_description_string(GameDescription, Rules) -> true ; writeln("error: can't parse game description!"), fail),
	load_rules(Rules).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- mode load_rules(++).
load_rules(Rules) :-
	sort_clauses(Rules, SortedRules),
	(foreach(Rule, SortedRules),
	 fromto(Clauses, [game_rule(Name, Arity, Head, Tail)|ClausesIn], ClausesIn, SortedRules) do
		(Rule=(Head:-Tail) -> true ; Rule=Head, Tail=true),
		functor(Head, Name, Arity)
	),
	expand_goal(Clauses, Clauses1),
	% make sure we don't get interrupted during compiling (we could end up in some inconsistent state if we have only half of the clauses)
	(events_defer ->
		(compile_term(Clauses1) -> events_nodefer ; events_nodefer, fail)
	;
		compile_term(Clauses1)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% sort the clauses in a List alphabetically according to the name of the head
:- mode sort_clauses(+,-).
sort_clauses(List1, List2) :-
	(foreach(Clause,List1), foreach(key(Name,Arity)-Clause,KeyList1) do
		(Clause= (:- _) ->
			Name=0, Arity=0 % 0 is a number and comes before every atom according to @</2
		;
			(Clause=(Head:-_) -> true ; Clause=Head),
			functor(Head,Name,Arity)
		)
	),
	keysort(KeyList1,KeyList2),
	(foreach(key(_,_)-Clause,KeyList2), foreach(Clause,List2) do true),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pure gdl stuff

d_true(Fluent) :-
	getval(state, State),
	member(Fluent, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

d_does(Role, Move) :-
	getval(moves, Moves),
	roles(Roles),
	d_does1(Role, Move, Roles, Moves).

:- mode d_does1(?, ?, ++, ++).
d_does1(Role, Move, [Role|_], [Move|_]).
d_does1(Role, Move, [_|Roles], [_|Moves]) :-
	d_does1(Role, Move, Roles, Moves).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

d_distinct(X,Y) :- X\=Y.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% the following will be overwritten when loading a game description

d_role(_) :- fail.
d_init(_) :- fail.
d_next(_) :- fail.
d_legal(_,_) :- fail.
d_goal(_,_) :- fail.
d_terminal :- fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
game_rule(_Name,_Arity,_Head,_Tail) :- fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_rules(Rules) :-
	findall(Rule, (
		game_rule(_,_,Head,Tail),
		(Tail=true ->
			Rule=Head
		;
			Rule=(Head:-Tail)
		)
		), Rules).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- mode goal(?, ?, ++).
goal(Role, Value, State) :-
	setval(state, State),
	d_goal(Role, Value).

:- mode legal(?, ?, ++).
legal(Role, Move, State) :-
	setval(state, State),
	d_legal(Role, Move).

:- mode terminal(++).
terminal(State) :-
	setval(state, State),
	d_terminal, !.

:- mode next(?, ++, ++).
next(Fluent, State, Moves) :-
	setval(state, State),
	setval(moves, Moves),
	d_next(Fluent).

init(Fluent) :-
	d_init(Fluent).

role(Role) :-
	d_role(Role).

:- mode does(?, ?, ++).
does(Role, Move, Moves) :-
	roles(Roles),
	d_does1(Role, Move, Roles, Moves).

:- mode true(?, ++).
true(Fluent, State) :-
	member(Fluent, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% for calling complex goal in the context of the game_description_interface module
% Goal may contain any predicate defined in the game (in general everything starting with "d_") plus holds/2
% Note: call_goal(G) is basically just a short hand for call(G)@game_description_interface
:- mode call_goal(+, ++, ++).
call_goal(Goal, State, Moves) :-
	setval(state, State),
	setval(moves, Moves),
	call(Goal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
roles(Roles) :-
	findall(Role, d_role(Role), Roles).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
role2index(Role, Index) :-
	roles(Roles),
	role2index1(Role, Index, 1, Roles).

:- mode role2index1(?, ?, ++, ++).
role2index1(Role, Index, Index, [Role|_]).
role2index1(Role, Index, I, [_|Roles]) :-
	I1 is I+1,
	role2index1(Role, Index, I1, Roles).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
number_of_roles(NRoles) :-
	roles(Roles), length(Roles, NRoles).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
initial_state(State) :-
	findall(Fluent, d_init(Fluent), State1),
	sort(State1, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- mode state_update(++, ++, ?).
state_update(OldState, Moves, NewState) :-
	setval(state, OldState),
	setval(moves, Moves),
	findall(Fluent, d_next(Fluent), NewState1),
	sort(NewState1, NewState).
