
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


:- module(actions,[execute_action/3,insert_action_to_strategy/5],[assertions]).

:-use_module(ambiente,[pelota_pred/3,cancha/4,robot_entre/5,jugador/1]).
:-use_module(configuration,[get_player/3,get_ball_name/1]).
:-use_module(navegacion,[ir_a_posicion/5,gira/5]).
:-use_module(primitivas,[llevar_pelota_a_posicion/5]).
:-use_module(field_grid,[subfield_center/4]).
:-use_module(field,[sentido/1,centro_arco_contrario/2]).

:- comment(title, "Actions implementation module ").

:- comment(author, "Mario Moya").

:- comment(module, "Este módulo implementa la ejecucion de las
	acciones de la representacion en strips.  Utilizando los
	predicados de los modulos navegacion y primitivas, implementa
	el predicado execute_action para cada tipo de accion definida
	en la representación strips.").


execute_action(noop,0,0).
	%este predicado debe comunicarse con estrategia, get_team_behavior, etc.
	
execute_action(move(P,_,cell(Col2,Row2)),Vi,Vd) :-
	subfield_center(Col2,Row2,CoordX,CoordY),
	get_player(P,Number,propio),
	jugador(robot(propio,Number,Pos)),
	ir_a_posicion(robot(propio,Number,Pos),CoordX,CoordY,Vi,Vd).

execute_action(grabBall(P,Obj,_),Vi,Vd) :-
%	subfield_center(Pos, CoordX,CoordY),
	get_ball_name(Obj),
	get_player(P,Number,propio),
	%si está detraz de la pelota ir a la pelota
	pelota_pred(Xb,Yb,_),
	sentido(S),
	cancha(X1,Y1,X2,Y2),
	(S>0, %el caso de azul-
	 (Xb>90,X is Xb-S*2.5; X is Xb),
	  robot_entre(robot(propio,Number,_),X,Y1,X2,Y2)
	;
	 S<0, %amarillo
	 (Xb<9,X is Xb-S*2.5; X is Xb),
	  robot_entre(robot(propio,Number,_),X1,Y1,X,Y2)), 
	ir_a_posicion(robot(propio,Number,_),Xb,Yb,Vi,Vd).

execute_action(grabBall(P,Obj,_),Vi,Vd) :-
	%si el robot esta entre la pelota y el arco contrario
	%va hasta la pelota
	get_ball_name(Obj),
	get_player(P,Number,propio),
	centro_arco_contrario(Xarco,Yarco),
	pelota_pred(Xb,Yb,_),
	sentido(S),
	robot_entre(robot(propio,Number,_),Xb+S*3,Yb,Xarco,Yarco),
	llevar_pelota_a_posicion(robot(propio,Number,_),Xb,Yb,Vi,Vd).



execute_action(kick(P,Obj,cell(FromCol,FromRow),cell(ToCol,ToRow)), Vi,Vd) :-
	get_ball_name(Obj),
	subfield_center(FromCol,FromRow,_,Yf),
	subfield_center(ToCol,ToRow,_,Yt),
	get_player(P,_,propio),
	Yt =< Yf,
	gira(Vi,Vd,100,to_left,200).
	

execute_action(kick(P,Obj,cell(FromCol,FromRow),cell(ToCol,ToRow)), Vi,Vd) :-
	get_ball_name(Obj),
	subfield_center(FromCol,FromRow,_,Yf),
	subfield_center(ToCol,ToRow,_,Yt),
	get_player(P,_,propio),
	Yf < Yt,
	gira(Vi,Vd,100,to_right,200).


% Reemplaza la accion, es decir las velocidades de un jugador
% en una lista de velocidades por las que se envian por parametro:
%   insert_action_to_strategy(Vl,Vr,N,Olist,NList).
%   Vl y Vr son las nuevas velocidades
%   N es el número de jugador (o el indice de donde hay que reemplazar)
%   Olist es la lista de velocidades original
%   NList es la lista de velocidades modificada, reemplazando
%   los elementos que estaban el la posicion N*2 de la lista original
%   por Vl y Vr.  
insert_action_to_strategy(Vl,Vr,1,[_,_|R],[Vl,Vr|R]).

insert_action_to_strategy(Vl,Vr,N,[An,Bn|R],[An,Bn|NL]):-
	N1 is N-1,
	insert_action_to_strategy(Vl,Vr,N1,R,NL).

	
