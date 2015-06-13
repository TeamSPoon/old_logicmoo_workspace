
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Copyright 2007 Pablo Kogan, Guillermo Torres, Mario Moya		    %
    % 									    %
    % This file is part of Rakiduam.					    %
    % 									    %
    % Rakiduam is free software; you can redistribute it and/or modify	    %
    % it under the terms of the GNU General Public License as published by  %
    % the Free Software Foundation; either version 3 of the License, or	    %
    % (at your option) any later version.				    %
    % 									    %
    % Rakiduam is distributed in the hope that it will be useful,	    %
    % but WITHOUT ANY WARRANTY; without even the implied warranty of	    %
    % MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the	    %
    % GNU General Public License for more details.			    %
    % 									    %
    % You should have received a copy of the GNU General Public License	    %
    % along with this program.  If not, see <http://www.gnu.org/licenses/>. %
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%:- module(perceptions,[assert_perceptions/0,get_perceptions/1],[assertions]).
:- module(perceptions,_,[assertions]).

:- use_module(ambiente,[pelota_pred/3,jugadores_propios/1,jugador/1]).
:- use_module(configuration,[get_player/3,get_ball_name/1]).
:- use_module(field_grid,[subfield_center/4,point_to_cell_position/3]).
:- use_module(library(lists)).

:- data [perceptions/1].

:- comment(title, "Primitive perceptions module ").

:- comment(author, "Mario Moya").

:- comment(module, "Esté módulo resuelve e inserta las percepciones 
	primitivas que son necesarias para que el planificador las 
	entienda según su representacion en strips. En este caso las
	percepciones corresponden a las primitivas carrying y
	waiting_at.").



konstant(3).

% :- pred assert_perception 
% 	# "Calcula la lista de percepciones y lo agrega a la base de conocimientos"
assert_perceptions :-
	jugadores_propios(J),
	get_waiting_at_list(J,Wa),
	get_carrying_ball(Cb),
	append(Wa,[Cb],Plist),
	asserta_fact(perceptions(Plist)).

assert_perceptions :-
	jugadores_propios(J),
	get_waiting_at_list(J,Wa),
	asserta_fact(perceptions(Wa)).

% :- pred assert_perception : list
% 	# "Calcula la lista de percepciones y lo devuelve en el parámetro"
get_perceptions(PList):-
	jugadores_propios(J),
	get_carrying_ball(Cb),
	get_waiting_at_list(J,Wa),
	append(Wa,[Cb],PList).

get_perceptions(Wa) :-
	jugadores_propios(J),
	get_waiting_at_list(J,Wa).



get_carrying_ball(carrying(P,Ball)) :-
	pelota_pred(Xb,Yb,_),
	konstant(K),
	jugador(robot(propio,Number,pos(Xr,Yr,_Z,_R))),
	Xr>Xb-K,
	Xr<Xb+K,
	Yr>Yb-K,
	Yr<Yb+K,
	get_ball_name(Ball),
	get_player(P,Number,propio).

get_carrying_ball(waiting_at(ball,FieldCell)) :-
	pelota_pred(Xb,Yb,_),
	get_in_field_position(Xb,Yb,FieldCell).


get_in_field_position(X,Y,Cell) :-
	point_to_cell_position(X,Y,Cell).
	

get_waiting_at_list([],[]).
get_waiting_at_list([robot(propio,Number,pos(Xr,Yr,_Z,_R))|Tail],
	[waiting_at(P,FieldCell)|L]) :-
	get_player(P,Number,propio),
	get_in_field_position(Xr,Yr,FieldCell),
	get_waiting_at_list(Tail,L).



