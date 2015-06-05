%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%     Copyright 2007 Pablo Kogan, Guillermo Torres, Mario Moya
%
%     This file is part of Rakiduam.
%
%     Rakiduam is free software; you can redistribute it and/or modify
%     it under the terms of the GNU General Public License as published by
%     the Free Software Foundation; either version 3 of the License, or
%     (at your option) any later version.
%
%     Rakiduam is distributed in the hope that it will be useful,
%     but WITHOUT ANY WARRANTY; without even the implied warranty of
%     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%     GNU General Public License for more details.
%
%     You should have received a copy of the GNU General Public License
%     along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%:-module(field_grid,_,[assertions,fd]).
:-module(field_grid,[subfield_center/4,point_to_cell_position/3,neighbor/2],[assertions,fd]).
:-use_module(configuration,[get_field/4]).
:-use_module(ambiente,[largo_cancha/1,ancho_cancha/1,arco_contrario/4]).

:- comment(title, "Field grid module ").

:- comment(author, "Mario Moya").

:- comment(module, "Este módulo calcula los casilleros en que se divide
	la cancha, según los paramétros que se hayan especificado
	en la configuración. La idea de este módulo es tener tantas
	filas y columnas como se especifique. Se asume que los indices
	para filas y columnas son números enteros que comienzan en 1").

%no deberian ser hechos.
number_of_rows(23).
number_of_columns(28).
%largo_cancha(84.78819999999999).
%ancho_cancha(70.6).


:- pred cell_with(-Width) :: int 
 # "return in @var{Width} the width size of the cell".
cell_width(L):- 
	number_of_columns(0),
	largo_cancha(L).

cell_width(Cw):- 
	number_of_columns(N),
	largo_cancha(L),
	Cw is L//N.

cell_height(A):- 
	number_of_rows(0),
	ancho_cancha(A).

cell_height(Ch):- 
	number_of_rows(N),
	ancho_cancha(A),
	Ch is A//N.

cell_x_coordinate(Column, X) :-
	cell_width(Cw),
	X is ((Column-1)*Cw) + (Cw//2).

cell_y_coordinate(Row, Y) :-
	cell_height(Ch),
	number_of_rows(N),
	Y is ((N-Row)*Ch) + (Ch//2).

%dadas un columna y una fila, calcula el punto central de la celda.
subfield_center(Column,Row,X,Y):-
	cell_x_coordinate(Column,X),
	cell_x_coordinate(Row,Y).


%dadas dos coordenada X e Y, calcula en que subcampo se encuentra dicho punto.
% TODO hacer que oppGoal y ownGoal, sean relativos al equipo elegido.
%      Ahora asume que oppGoal es el arco bajo, y ownGoal es el arco alto.
point_to_cell_position(X,_,oppGoal) :-
	relative_value_of_X(X,0).

point_to_cell_position(X,_,ownGoal) :-
	relative_value_of_X(X,Xr),
	largo_cancha(Lc),
	Xr >= Lc.

point_to_cell_position(X,Y,cell(Column,Row)) :-
	cell_height(Ch),
	relative_value_of_Y(Y,Yr),
	Row is (Yr//Ch) + 1,
	cell_width(Cw),
	relative_value_of_X(X,Xr),
	Column is Xr//Cw + 1.


%devuelve el valor relativo de X, dentro de los limites de la cancha
relative_value_of_X(Xaxe,Xrel) :-
	get_field(X1f,_,X2f,_),
	Xaxe > X1f,
	Xaxe < X2f,
	Xrel is Xaxe-X1f.

relative_value_of_X(Xaxe,0) :-
	get_field(X1f,_,_X2f,_),
	Xaxe =< X1f.

relative_value_of_X(Xaxe,Lc) :-
	get_field(_X1f,_,X2f,_),
	Xaxe >= X2f,
	largo_cancha(Lc).

%devuelve el valor relativo de Y, dentro de los limites de la cancha
relative_value_of_Y(Yaxe,Yrel) :-
	get_field(_,Y1f,_,Y2f),
	Yaxe > Y1f,
	Yaxe < Y2f,
	Yrel is Yaxe-Y1f.

relative_value_of_Y(Yaxe,0) :-
	get_field(_,Y1f,_,_Y2f),
	Yaxe =< Y1f.

relative_value_of_Y(Yaxe,Ac) :-
	get_field(_,_Y1f,_,Y2f),
	Yaxe >= Y2f,
	ancho_cancha(Ac).


inrange(C,R) :-
	inrange_columns(C),
	inrange_rows(R).

inrange_columns(C):-
	number_of_columns(Cn),
	C in 1 .. Cn.

inrange_rows(R) :-
	number_of_rows(Rn),
	R in 1 .. Rn.

neighbor(cell(C,R1),cell(C,R2)) :- 
 	inrange(C,R1),
 	R2 .=. R1-1, 
	inrange_rows(R2).
 	%R2 .>=. 1.

neighbor(cell(C,R1),cell(C,R2)) :- 
	inrange(C,R1),
	R2 .=. R1+1, 
	inrange_rows(R2).

%Case 2: the same Row
neighbor(cell(C1,R),cell(C2,R)) :- 
	inrange(C1,R),
	C2 .=. C1-1, 
	inrange_columns(C2).
%	C2 >= 1.

neighbor(cell(C1,R),cell(C2,R)) :- 
	inrange(C1,R),
	C2 .=. C1+1, 
	inrange_columns(C2).

%Case 3: (+,+)
neighbor(cell(C1,R1),cell(C2,R2)) :- 
	inrange(C1,R1),
	inrange(C2,R2),
	R2 .=. R1+1, 
	C2 .=. C1+1. 
	

%Case 4: (+,-)
neighbor(cell(C1,R1),cell(C2,R2)) :- 
	inrange(C1,R1),
	inrange(C2,R2),
	R2 .=. R1+1, 
	C2 .=. C1-1.

%Case 5: (-,+)
neighbor(cell(C1,R1),cell(C2,R2)) :- 
	inrange(C1,R1),
	inrange(C2,R2),
	R2 .=. R1-1, 
	C2 .=. C1+1.

%Case 6: (-,-)
neighbor(cell(C1,R1),cell(C2,R2)) :- 
	inrange(C1,R1),
	inrange(C2,R2),
	R2 .=. R1-1, 
	C2 .=. C1-1.


distance_between(Cell1,Cell2,1) :-
	neighbor(Cell1,Cell2).

distance_between(Cell1,Cell2,Distance) :-
	neighbor(Cell1,Aux),
	distance_between(Aux,Cell2,DistanceAux),
	Distance is DistanceAux+1.

