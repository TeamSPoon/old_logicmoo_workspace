
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

:- module(field,[sentido/1,centro_arco_propio/2,centro_arco_contrario/2],[assertions]).

:-use_module(ambiente,[cancha/4,largo_cancha/1,arco_propio/4,arco_contrario/4,medio_cancha/2,alto_area/1,ancho_area/1,alto_area_chica/1,ancho_area_chica/1]).

sentido(Signo):- %Signo =1 si es azul =-1 si es amarillo.
	largo_cancha(L),
	arco_propio(Xp1,_Yp1,_Xp2,_Yp2),
	arco_contrario(Xc1,_Yc1,_Xc2,_Yc2),
	Signo is (Xp1 - Xc1)/L.


centro_arco_propio(X,Y):-
	arco_propio(X,Yp1,_Xp2,Yp2),
	Y is Yp1 + (Yp2-Yp1)/2.
campo_propio(X1,Y1,X2,Y2):-
	largo_cancha(L),
	arco_propio(Xp1,_Yp1,_Xp2,_Yp2),
	arco_contrario(Xc1,_Yc1,_Xc2,_Yc2),
	Signo is (Xp1 - Xc1)/L,
	(Signo >0, %azul
	    cancha(_Xc1,Y1,X2,Y2),
	    medio_cancha(X1,_Y1);
	    Signo<0,
	    cancha(X1,Y1,_X2,Y2),
	    medio_cancha(X2,_Y1)).
campo_contrario(X1,Y1,X2,Y2):-
	largo_cancha(L),
	arco_propio(Xp1,_Yp1,_Xp2,_Yp2),
	arco_contrario(Xc1,_Yc1,_Xc2,_Yc2),
	Signo is (Xp1 - Xc1)/L,
	(Signo <0, %amarillo
	    cancha(_Xc1,Y1,X2,Y2),
	    medio_cancha(X1,_Y1);
	    Signo>0,
	    cancha(X1,Y1,_X2,Y2),
	    medio_cancha(X2,_Y1)).
	 
area_propia(X1,Y1,X2,Y2):-
	centro_arco_propio(X,Y),
	alto_area(A),
	Y1 is Y - A/2, %alto del area/2
	Y2 is Y + A/2,
	largo_cancha(L),
	arco_propio(Xp1,_Yp1,_Xp2,_Yp2),
	arco_contrario(Xc1,_Yc1,_Xc2,_Yc2),
	Signo is (Xp1 - Xc1)/L,
	ancho_area(An),
	Xmedio is X-Signo*An/2, %ancho del area/2
	X1 is Xmedio - An/2,
	X2 is Xmedio + An/2.

area_chica_propia(X1,Y1,X2,Y2):-
	centro_arco_propio(X,Y),
	alto_area_chica(A),
	Y1 is Y - A/2, %alto del area/2
	Y2 is Y + A/2,
	largo_cancha(L),
	arco_propio(Xp1,_Yp1,_Xp2,_Yp2),
	arco_contrario(Xc1,_Yc1,_Xc2,_Yc2),
	Signo is (Xp1 - Xc1)/L,
	ancho_area_chica(An),
	Xmedio is X-Signo*An/2, %ancho del area/2
	X1 is Xmedio - An/2,
	X2 is Xmedio + An/2.
%esta mal porque centro arco contrario es otro
area_chica_contraria(X1,Y1,X2,Y2):-
	centro_arco_contrario(_X,Y),
	alto_area_chica(A),
	Y1 is Y - A/2, %alto del area/2
	Y2 is Y + A/2,
	largo_cancha(L),
	arco_propio(Xp1,_Yp1,_Xp2,_Yp2),
	arco_contrario(Xc1,_Yc1,_Xc2,_Yc2),
	Signo is (Xp1 - Xc1)/L,
	ancho_area_chica(An),
	Xmedio is Xc1+Signo*An/2, %ancho del area/2
	X1 is Xmedio - An/2,
	X2 is Xmedio + An/2.

%ver error
area_contraria(X1,Y1,X2,Y2):-
	centro_arco_contrario(_X,Y),
	alto_area(La),
	Y1 is Y - La/2, %alto del area/2
	Y2 is Y + La/2,
	largo_cancha(L),
	arco_propio(Xp1,_Yp1,_Xp2,_Yp2),
	arco_contrario(Xc1,_Yc1,_Xc2,_Yc2),
	Signo is (Xp1 - Xc1)/L,
	ancho_area(A),
	Xmedio is Xc1+(Signo*A/2), %ancho del area/2
	X1 is Xmedio - A/2,
	X2 is Xmedio + A/2.

centro_arco_contrario(X,Y):-
        largo_cancha(L),
	arco_propio(Xp1,_Yp1,_Xp2,_Yp2),
	arco_contrario(Xc1,Yc1,_Xc2,Yc2),
	Signo is (Xp1 - Xc1)/L,
	X is Xc1-Signo*2,
	Y is Yc1 + (Yc2-Yc1)/2.	
