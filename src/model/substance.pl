:- module(substance, [substance/1]).
/** <module> Interface module tying display engines to the underlying model

*/

%%	substance(-Term:term) is nondet
%
%	Term is a partially bound term, used to interrogate state
%	of the MUD.
%
%	map_origin(X, Y) - absolute coordinates Y down location of UL
%			corner cell
%       map_size(X, Y) - size to display, in cells across and down
%
%       cell(+Player, +X, +Y, -Semantics) -
%		       - called with Player, X, and Y bound, binds
%		       Semantics to a term understandable by
%		       mud_specific:style/1
%
substance(map_origin(0,0)).
substance(map_size(4,4)).  % small for debugging ez
substance(cell(_, 0, _, wall)).
substance(cell(_, _, 0, wall)).
substance(cell(_, 3, _, wall)).
substance(cell(_, _, 3, wall)).
substance(cell(_, X, Y, floor)) :-
	between(1, 2, X),
	between(1, 2, Y).

substance(need_new_player(_, G)) :- gensym(player, G).
substance(player_split(P)) :- debug(logicmoo, 'player ~w abandoned', [P]).
