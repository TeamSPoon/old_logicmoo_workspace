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
:-module(ambiente,[cancha/4,largo_cancha/1,ancho_cancha/1,medio_cancha/2,alto_area/1,ancho_area/1,alto_area_chica/1,ancho_area_chica/1,insertar_estado/1,pelota/3,pelota_pred/3,pelota_en_direccion_a_zona/6,pelota_entre/4,robot_entre/5,atascado/1,atascado2/1,mas_cercano/1,mas_cercanos/2,mas_cercanos2/1,mas_cercanos_lugar/4,jugadores/1,jugadores_propios/1,estado/2,jugador/1,jugador_prev/1,insertar_accion/1,accion_prev/3,accion_prom/3,arco_propio/4,arco_contrario/4,iniciar_ambiente/1,pelota_anterior/3,distancia/3],[assertions]).

:- data [pelota/3,pelota_anterior/3,jugadores/1,estado/2,jugadores_prev/1,estado_prev/2,acciones_prev/1,acciones_prom/1,atascado_robot/1,arco_propio/4,arco_contrario/4,cancha/4,ancho_area/1,alto_area/1,ancho_area_chica/1,alto_area_chica/1].

:-use_module(configuration,[get_arco_alto/4,get_arco_bajo/4,get_field/4,get_anchoArea/1,get_altoArea/1,get_anchoAreaChica/1,get_altoAreaChica/1,get_numplayers/1]).
%:-use_module(perceptions,[assert_perceptions/0]).

:- comment(title, "Modulo ambiente").

:- comment(author, "Pablo Kogan").

:- comment(module, "Este módulo representa el estado interno de los agentes.").

:- comment(doinclude,cancha/4).
:- comment(doinclude,largo_cancha/1).
:- comment(doinclude,ancho_cancha/1).
:- comment(doinclude,medio_cancha/2).
:- comment(doinclude,alto_area/1).
:- comment(doinclude,ancho_area/1).
:- comment(doinclude,alto_area_chica/1).
:- comment(doinclude,ancho_area_chica/1).
:- comment(doinclude,insertar_estado/1).
:- comment(doinclude,pelota/3).
:- comment(doinclude,pelota_pred/3).
:- comment(doinclude,pelota_en_direccion_a_zona/6).
:- comment(doinclude,pelota_entre/4).
:- comment(doinclude,robot_entre/5).
:- comment(doinclude,atascado/1).
:- comment(doinclude,mas_cercano/1).
:- comment(doinclude,mas_cercanos2/1).
:- comment(doinclude,mas_cercanos/1).
:- comment(doinclude,mas_cercanos_lugar/4).
:- comment(doinclude,jugadores/1).
:- comment(doinclude,jugadores_propios/1).
:- comment(doinclude,jugador/1).
:- comment(doinclude,jugador_prev/1).
:- comment(doinclude,insertar_accion/1).
:- comment(doinclude,accion_prev/3).
:- comment(doinclude,accion_prom/3).


iniciar_ambiente(azul):-
%arco azul  
	get_arco_alto(X1,Y1,X2,Y2),
%asserta_fact(arco_propio(93.4259,33.9320,93.4259,49.6801)),
	asserta_fact(arco_propio(X1,Y1,X2,Y2)),
	get_arco_bajo(X1b,Y1b,X2b,Y2b),
%asserta_fact(arco_contrario(6.8118,33.9320,6.8118,49.6801)).
	asserta_fact(arco_contrario(X1b,Y1b,X2b,Y2b)),
	get_field(FX1,FY1,FX2,FY2),
	asserta_fact(cancha(FX1,FY1,FX2,FY2)),
	get_anchoArea(ANA),
	asserta_fact(ancho_area(ANA)),
	get_altoArea(ALA),
	asserta_fact(alto_area(ALA)),
	get_anchoAreaChica(ANAC),
	asserta_fact(ancho_area_chica(ANAC)),
	get_altoAreaChica(ALAC),
	asserta_fact(alto_area_chica(ALAC)),
	get_numplayers(N),
	assert_actions_prev_prom(N).


iniciar_ambiente(amarillo):-
	get_arco_alto(X1,Y1,X2,Y2),
%asserta_fact(arco_contrario(93.4259,33.9320,93.4259,49.6801)),
	asserta_fact(arco_contrario(X1,Y1,X2,Y2)),
	get_arco_bajo(X1b,Y1b,X2b,Y2b),
%asserta_fact(arco_propio(6.8118,33.9320,6.8118,49.6801)).
	asserta_fact(arco_propio(X1b,Y1b,X2b,Y2b)),
	get_field(FX1,FY1,FX2,FY2),
	asserta_fact(cancha(FX1,FY1,FX2,FY2)),
	get_anchoArea(ANA),
	asserta_fact(ancho_area(ANA)),
	get_altoArea(ALA),
	asserta_fact(alto_area(ALA)),
	get_anchoAreaChica(ANAC),
	asserta_fact(ancho_area_chica(ANAC)),
	get_altoAreaChica(ALAC),
	asserta_fact(alto_area_chica(ALAC)),
	get_numplayers(N),
	assert_actions_prev_prom(N).


iniciar_ambiente(_):-
%arco azul  
	get_arco_alto(X1,Y1,X2,Y2),
%asserta_fact(arco_propio(93.4259,33.9320,93.4259,49.6801)),
	asserta_fact(arco_propio(X1,Y1,X2,Y2)),
	get_arco_bajo(X1b,Y1b,X2b,Y2b),
%asserta_fact(arco_contrario(6.8118,33.9320,6.8118,49.6801)).
	asserta_fact(arco_contrario(X1b,Y1b,X2b,Y2b)),
	get_field(FX1,FY1,FX2,FY2),
	asserta_fact(cancha(FX1,FY1,FX2,FY2)),
	get_anchoArea(ANA),
	asserta_fact(ancho_area(ANA)),
	get_altoArea(ALA),
	asserta_fact(alto_area(ALA)),
	get_anchoAreaChica(ANAC),
	asserta_fact(ancho_area_chica(ANAC)),
	get_altoAreaChica(ALAC),
	asserta_fact(alto_area_chica(ALAC)),
	get_numplayers(N),
	assert_actions_prev_prom(N).






%***************************datos de la cancha
% :- pred cancha(-Xinferior,-Yinferior,-Xsuperior,-Ysuperior) :: int * int * int * int 
%  # "Retorna en los dos puntos extremos de la chancha, el punto inferior (@var{Xiferior},@var{Yiferior}) y el punto superior (@var{Xsuperior},@var{Ysuperior}).".
% %cancha(Xinferior,Yinferior,Xsuperior,Ysuperior)
% cancha(6.8118,6.3730,93.4259,77.2392).

%arco_propio(93.4259,33.9320,93.4259,49.6801).
%arco_contrario(6.8118,33.9320,6.8118,49.6801).
:- pred largo_cancha(-Largo) :: int 
 # "Retorna en @var{Largo} el largo de la cancha en base al predicado cancha.".
largo_cancha(L):-
	cancha(Xi,_,Xs,_),
	L is abs(Xs-Xi).
:- pred ancho_cancha(-Ancho) :: int 
 # "Retorna en @var{Ancho} el ancho de la cancha en base al predicado cancha.".
ancho_cancha(A):-
	cancha(_,Yi,_,Ys),
	A is abs(Ys-Yi).
:- pred medio_cancha(-Xmedio,Ymedio) :: int * int
 # "Retorna el punto medio de la cancha (@var{Xmedio},@var{Ymedio}) calculado en base al predicado cancha.".
medio_cancha(X,Y):-
	cancha(X1,Y1,_X2,_Y2),
	largo_cancha(L),
	ancho_cancha(A),
	X is X1 + L/2,
	Y is Y1 + A/2.
%*************************************%Medidas del área del arco
% :- pred ancho_area(-Ancho) :: int 
%  # "Retorna en @var{Ancho} el ancho del area.".
% ancho_area(14).
% :- pred alto_area(-Alto) :: int 
%  # "Retorna en @var{Alto} el alto del area.".
% alto_area(31).

% :- pred ancho_area_chica(-Ancho) :: int 
%  # "Retorna en @var{Ancho} el ancho del area chica.".
% ancho_area_chica(6).
% :- pred alto_area_chica(-Alto) :: int 
%  # "Retorna en @var{Alto} el alto del area chica.".
% alto_area_chica(15.5).

:- pred pelota_pred(-X,-Y,-Z) :: int * int * int
 # "Retorna la posicion predecida de la pelota en el punto (@var{X},@var{Y},@var{Z}), calculada en base a las dos ultimas posiciones de esta. La tercera ordenada @var{Z} no es utilizada.".

pelota_pred(X,Y,_):-
	pelota(Xball,Yball,_),
	pelota_anterior(Xa,Ya,_),
	X is Xball + (Xball-Xa),
	Y is Yball + (Yball-Ya).

:- pred insertar_estado(+Estado) :: list 
 # "Modifica la base de conocimiento para que quede consistente con el nuevo estado @var{Estado} del ambiente.".
insertar_estado([pos(Xball,Yball,Zball),Jugadores,estado(E1,E2)]):-
	(pelota(X,Y,Z),
	 retractall_fact(pelota(X,Y,Z)),
	 retractall_fact(pelota_anterior(_X,_Y,_Z)),
	 asserta_fact(pelota_anterior(X,Y,Z));
	    asserta_fact(pelota_anterior(Xball,Yball,Zball))),
	asserta_fact(pelota(Xball,Yball,Zball)),
	(pelota_anterior(X1,Y1,_),pelota(X2,Y2,_),
	sqrt((X1-X2)**2+(Y1-Y2)**2)>5,
	retractall_fact(atascado_robot(_)),
 	acciones_prev(Accion_Prev),
	acciones_prom(Accion_Prom),
	retractall_fact(acciones_prom(Accion_Prom)),
	asserta_fact(acciones_prom([0,0,0,0,0,0,0,0,0,0])),
	retractall_fact(acciones_prev(Accion_Prev)),
	asserta_fact(acciones_prev([0,0,0,0,0,0,0,0,0,0])),
	display('Modifico la posicion de la pelota, estado: '),display(E1),display(','),display(E2),display('.'),nl;
	    true), %si no se ha movido la pelota
	(jugadores(J),
	 retractall_fact(jugadores(J)),
	 retractall_fact(jugadores_prev(_J)),
	 asserta_fact(jugadores_prev(J));
	    asserta_fact(jugadores_prev(Jugadores))),
	asserta_fact(jugadores(Jugadores)),
	(estado(EP1,EP2),
	  retractall_fact(estado(EP1,EP2)),
	 retractall_fact(estado_prev(_EP1,_EP2)),
	 asserta_fact(estado_prev(EP1,EP2));
			 asserta_fact(estado_prev(E1,E2))),
	asserta_fact(estado(E1,E2)).
%************************************************************************
:- pred pelota_entre(+X1,+Y1,+X2,+Y2) :: int *int *int *int 
 # "Devuelve verdadero si la pelota se encuentra en el rectangulo delimitado por los puntos (@var{X1},@var{Y1}) y  (@var{X2},@var{Y2}).".
%falta test
pelota_entre(Xi,Yi,Xs,Ys):- % si la pelota se encuentra en área de marca
	pelota_pred(X,Y,_),
	X>=Xi,X=<Xs,
	Y>=Yi,Y=<Ys.
pelota_entre(Xs,Ys,Xi,Yi):- % si la pelota se encuentra en área de marca
	pelota_pred(X,Y,_),
	X>=Xi,X=<Xs,
	Y>=Yi,Y=<Ys.
pelota_entre(Xi,Ys,Xs,Yi):- % si la pelota se encuentra en área de marca
	pelota_pred(X,Y,_),
	X>=Xi,X=<Xs,
	Y>=Yi,Y=<Ys.
pelota_entre(Xs,Yi,Xi,Ys):- % si la pelota se encuentra en área de marca
	pelota_pred(X,Y,_),
	X>=Xi,X=<Xs,
	Y>=Yi,Y=<Ys.
%************************************************************************
:- pred robot_entre(+Robot,+X1,+Y1,+X2,+Y2) :: atom * int *int *int *int 
 # "Devuelve verdadero si el robot @var{Robot} se encuentra en el rectangulo delimitado por los puntos (@var{X1},@var{Y1}) y  (@var{X2},@var{Y2}).".
%falta test
robot_entre(robot(_,_,pos(X,Y,_,_)),Xi,Yi,Xs,Ys):- % si el robot se encuentra en área de marca
%	pelota_pred(X,Y,_),
	X>=Xi,X=<Xs,
	Y>=Yi,Y=<Ys.
robot_entre(robot(_,_,pos(X,Y,_,_)),Xs,Ys,Xi,Yi):- % si el robot se encuentra en área de marca
	%pelota_pred(X,Y,_),
	X>=Xi,X=<Xs,
	Y>=Yi,Y=<Ys.
robot_entre(robot(_,_,pos(X,Y,_,_)),Xi,Ys,Xs,Yi):- % si el robot se encuentra en área de marca
%	pelota_pred(X,Y,_),
	X>=Xi,X=<Xs,
	Y>=Yi,Y=<Ys.
robot_entre(robot(_,_,pos(X,Y,_,_)),Xs,Yi,Xi,Ys):- % si el robot se encuentra en área de marca
%	pelota_pred(X,Y,_),
	X>=Xi,X=<Xs,
	Y>=Yi,Y=<Ys.
%************************************************************************

:- pred pelota_en_direccion_a_zona(+X1,+Y1,+X2,+Y2,-X,-Y) :: int *int *int *int *int *int
 # "Segun los dos ultimos estados de la pelota se calcula la trayectoria y analiza si se dirige en dirección a la zona delimitada por el punto  (@var{X1},@var{Y1}) y el punto (@var{X2},@var{Y2}).  Si la pelota se dirige en direccion a los limites del rectangulo devuelve el punto de impacto  (@var{X},@var{Y}).  Caso contrario el predicado falla.".

pelota_en_direccion_a_zona(X1,Y1,X2,Y2,X,Y):-
	pelota_anterior(Xball1,Yball1,_),
	pelota(Xball2,Yball2,_),
	%Diferencia en y
	Dy_Ball is Yball2-Yball1,
	%Diferencia en x
	Dx_Ball is Xball2-Xball1,
	puntos_cortantes(Dx_Ball,Dy_Ball,Xball2,Yball2,X1,Y1,X2,Y2,L),
	%display('Lista de puntos cortantes'),display(L),nl,
	punto_cercano((Xball2,Yball2),L,(X,Y)),
	%display('Punto mas cercano Y '),display(Y),display(' X '),display(X),
	%si se dirige en dirección al punto es el punto elegido
	punto_intermedio((Xball1,Yball1),(Xball2,Yball2),(X,Y)).

%Auxiliares pelota_en_direccion_a_zona

%entre(N1,N2,N3) devuelve verdadero si el número N2 esta entre N1 y N3
entre(N1,N2,N3):-
	N1=<N2, N2=<N3.
entre(N1,N2,N3):-
	N3=<N2, N2=<N1.

%punto_intermedio((X1,Y1),(X2,Y2),(X3,Y3)) devuelve verdadero si el punto (X2,Y2) se encuentre entre medio de los puntos (X1,Y1) y (X3,Y3).
punto_intermedio((X1,Y1),(X2,Y2),(X3,Y3)):-
	(X1=<X2,X2=<X3;X1>=X2,X2>=X3),
	(Y1=<Y2,Y2=<Y3;Y1>=Y2,Y2>=Y3).

%puntos_cortantes(Dx,Dy,X,Y,X1,Y1,X2,Y2,L)
%devuelve en L la lista de los puntos pertenecientes al cuadrado delimitado por los puntos (X1,Y1) y (X2,Y2) que son tabien pertenecientes a la función de pendiente Dy/Dx, que pasa por el punto (X,Y)
 
puntos_cortantes(0,Dy,X,_Y,X1,Y1,X2,Y2,[(X,Y1),(X,Y2)]):-
	Dy=\=0,
	entre(X1,X,X2).
%no difiere en x
puntos_cortantes(Dx,0,_X,Y,X1,Y1,X2,Y2,[(X1,Y),(X2,Y)]):-
	Dx=\=0,
	entre(Y1,Y,Y2).
%no difiere en y
puntos_cortantes(0,0,_X,_Y,_X1,_Y1,_X2,_Y2,[]).
%esta quieta

puntos_cortantes(Dx,Dy,X,Y,X1,Y1,X2,Y2,[(X1,Yp1)|L]):-
%p1
	Mb is Dy/Dx, %display(Mb),
	Cb is Y-Mb*X, 
	Yp1 is Mb*X1+Cb,
	entre(Y1,Yp1,Y2),
	punto_2(Mb,Cb,X1,Y1,X2,Y2,L).
puntos_cortantes(Dx,Dy,X,Y,X1,Y1,X2,Y2,L):-
	%display('p1,2'), 
	Mb is Dy/Dx,
	Cb is Y-Mb*X, 
	punto_2(Mb,Cb,X1,Y1,X2,Y2,L).

%auxiliares puntos_cortantes
punto_2(Mb,Cb,X1,Y1,X2,Y2,[(Xp2,Y2)|L]):-
	Xp2 is (Y2-Cb)/Mb,
	entre(X1,Xp2,X2),
	punto_3(Mb,Cb,X1,Y1,X2,Y2,L).
punto_2(Mb,Cb,X1,Y1,X2,Y2,L):-
	punto_3(Mb,Cb,X1,Y1,X2,Y2,L).
punto_3(Mb,Cb,X1,Y1,X2,Y2,[(X2,Yp3)|L]):-
	Yp3 is Mb*X2+Cb,
	entre(Y1,Yp3,Y2),
	punto_4(Mb,Cb,X1,Y1,X2,Y2,L).
punto_3(Mb,Cb,X1,Y1,X2,Y2,L):-
	punto_4(Mb,Cb,X1,Y1,X2,Y2,L).
punto_4(Mb,Cb,X1,Y1,X2,_Y2,[(Xp4,Y1)]):-
	Xp4 is (Y1-Cb)/Mb,
	entre(X1,Xp4,X2).
punto_4(_Mb,_Cb,_X1,_Y1,_X2,_Y2,[]).

%punto_cercano((Xball,Yball),L,(X,Y))
%devuelve en (X,Y) el punto de la lista L mas cercano al punto (Xball,Yball)

punto_cercano((_Xball2,_Yball2),[(X,Y)],(X,Y)).
punto_cercano((Xball2,Yball2),[(X,Y)|L],(Xs,Ys)):-
	punto_cercano((Xball2,Yball2),L,(Xc,Yc)),
	distancia((Xball2,Yball2),(Xc,Yc),Dc),
	distancia((Xball2,Yball2),(X,Y),Di),
	Dc=<Di,Xs is Xc,Ys is Yc;
	Xs is X, Ys is Y.

%distancia((X1,Y1),(X2,Y2),D)
%devuelve en D la distancia entre los dos puntos

distancia((X1,Y1),(X2,Y2),D):-
	D is sqrt((X1-X2)**2+(Y1-Y2)**2).

%********************************************************************************

:- pred atascado(+Robot) :: atom
# "El predicado es verdadero si el jugador es considrerado atascado, esto ocurre si: la potencia de acción anterior es diferente a 0 en las dos ruedas 	Viprev=\=0,Vdprev=\=0; y la potencia de acción promedio acumulada difiere en menos de 12.5 con la potencia de acción tamada en el evento anterior; y un factor de desplazamiento calculado en base a los dos últimos eventos es menor que un factor calculado en base a la potencia promedio en las dos ruedas del evento anterior.  En resumen si la accion anterior era moverse y no ocurrio esto esta atascado.".

%Luego la potencia en las dos redas es para girar a máxima velocidad en el sentido contrario al que se estaba intentando mover.

%atascado(robot(_,J,pos(_,_,_,_))):-
%	atascado_robot(J),retractall_fact(atascado_robot(J)),!.
atascado(robot(E,J,pos(Xr,Yr,_,Rr))):-
	%display('atascado'),nl,
	jugador_prev(robot(E,J,pos(Xprev,Yprev,_,Rprev))),
	%display(Xprev),nl,
	accion_prev(J,Viprev,Vdprev),
	accion_prom(J,Viprom,Vdprom),
	%display(Viprev),nl,display(Viprom),nl,
	!,
	Viprev=\=0,Vdprev=\=0,
	abs(Viprev - Viprom + Vdprev - Vdprom)<0.1, %velocidad inercial difiere de la velocidad anterior en un 10% de la velocidad posible.
	Desplazamiento is (abs(Xr-Xprev)+abs(Yr-Yprev)+abs(Rr-Rprev)/180),
	%display('diferencia angular'),display(abs(Rr-Rprev)/180),
	%display('diferencia '),display(Desplazamiento),nl,
	Factor is abs((Viprev+Vdprev)/2),
        %display('factor'),display(Factor),nl,
	Desplazamiento<(Factor/2048),
	(atascado_robot(J),retractall_fact(atascado_robot(J));
	 asserta_fact(atascado_robot(J))). 
	%display('poco mov direccional'),display(J).
						%	Signo is (Vdprev-Viprev)/abs(Vdprev-Viprev),
%	Vi is -Signo*125, %Vdpred*1.5,
%	Vd is Signo*125. %-Vipred*1.5.
%*****jugador mas cercano
%ver paso de mas cercano a lugar en ambiente
atascado2(robot(E,J,pos(Xr,Yr,_,Rr))):-
	%display('atascado'),nl,
	jugador_prev(robot(E,J,pos(Xprev,Yprev,_,Rprev))),
	%display(Xprev),nl,
	accion_prev(J,Viprev,Vdprev),
	accion_prom(J,Viprom,Vdprom),
	%display(Viprev),nl,display(Viprom),nl,
	!,
	Viprev=\=0,Vdprev=\=0,
	abs(Viprev - Viprom + Vdprev - Vdprom)<6.25, %velocidad inercial difiere de la velocidad anterior en un 10% de la velocidad posible.
	Desplazamiento is (abs(Xr-Xprev)+abs(Yr-Yprev)+abs(Rr-Rprev)/180),
	%display('diferencia angular'),display(abs(Rr-Rprev)/180),
	%display('diferencia '),display(Desplazamiento),nl,
	Factor is abs((Viprev+Vdprev)/2),
        %display('factor'),display(Factor),nl,
	Desplazamiento<(Factor/256).

:- pred mas_cercanos2(+Robot) :: atom
# "El predicado es verdadero el robot @var{Robot} es uno de los dos jugadores propios mas ceranos a la pelota.".
mas_cercanos2(Robot):-
	jugadores_propios(Jugadores),
	pelota_pred(X,Y,_),
	ordenar(Jugadores,Jordenado,X,Y),!,
	primeros2(Jordenado,Robot).
primeros2([R|_],R).
primeros2([_|[R|_]],R).

:- pred mas_cercanos(+Robot,+Num) :: atom * int
# "El predicado es verdadero el robot @var{Robot} es uno de los @var{Num} jugadores propios mas ceranos a la pelota.".
mas_cercanos(Robot,Num):-
	jugadores_propios(Jugadores),
	pelota_pred(X,Y,_),
	ordenar(Jugadores,Jordenado,X,Y),!,
	primeros(Jordenado,Robot,Num).


:- pred mas_cercanos_lugar(+Robot,+Num,X,Y) :: atom * int * int * int
# "El predicado es verdadero el robot @var{Robot} es uno de los @var{Num} jugadores propios mas ceranos al punto (@var{X},@var{Y}).".
mas_cercanos_lugar(Robot,Num,X,Y):-
	jugadores_propios(Jugadores),
	%pelota_pred(X,Y,_),
	ordenar(Jugadores,Jordenado,X,Y),!,
	primeros(Jordenado,Robot,Num).


primeros([R|_],R,_).
primeros([_|Resto],R,N):-
	N>1,
	N2 is N-1,primeros(Resto,R,N2).

:- pred mas_cercano(+Robot) :: atom
# "El predicado es verdadero el robot @var{Robot} es el jugador propio mas cerano a la pelota.".
mas_cercano(Robot):-
	jugadores_propios(Jugadores),
	pelota_pred(X,Y,_),
	min(Jugadores,Robot1,X,Y),!,
	Robot==Robot1.

ordenar([],[],_Xr,_Yr).
ordenar(L,[X|R],Xr,Yr):-
	min(L,X,Xr,Yr),
	del(L,X,L2),
	ordenar(L2,R,Xr,Yr).
min([Robot],Robot,_,_).
min([Robot|R],Menor,Xr,Yr):-
	min(R,M1,Xr,Yr),
	menor(Robot,M1,Xr,Yr,Menor).
menor(robot(_E1,_J1,pos(Xr1,Yr1,_Zr1,_Rr1)),robot(E2,J2,pos(Xr2,Yr2,Zr2,Rr2)),X,Y,robot(E2,J2,pos(Xr2,Yr2,Zr2,Rr2))):-
	sqrt((X-Xr1)**2 + (Y-Yr1)**2)>sqrt((X-Xr2)**2 + (Y-Yr2)**2).
menor(robot(E1,J1,pos(Xr1,Yr1,Zr1,Rr1)),robot(_E2,_J2,pos(Xr2,Yr2,_Zr2,_Rr2)),X,Y,robot(E1,J1,pos(Xr1,Yr1,Zr1,Rr1))):-
	sqrt((X-Xr1)**2 + (Y-Yr1)**2)=<sqrt((X-Xr2)**2 + (Y-Yr2)**2).
%menor(robot(E1,J1,pos(Xr1,Yr1,Zr1,Rr1)),robot(_E2,_J2,pos(_Xr2,_Yr2,_Zr2,_Rr2)),_X,_Y,robot(E1,J1,pos(Xr1,Yr1,Zr1,Rr1))).
del([],_,[]).
del([X|R],X,R).
del([Y|R],X,[Y|L]):-
	del(R,X,L).
%********************************************************************************
%devuelve los jugadores propios

:- pred jugadores_propios(-Jugadores) :: list
 # "Devuelve en @var{Jugadores}, una lista de los jugadores propios.".
jugadores_propios(P):-
	jugadores(J),
	jugadores_propios2(J,P).
jugadores_propios2([robot('propio',N,P)|R],[robot('propio',N,P)|RP]):-
	jugadores_propios2(R,RP).
jugadores_propios2(_,[]).

:- pred jugador(+Robot) :: atom
 # "Predicado para la seleccion del jugador X.".

%selección de jugador X
%09/07/2008 cambio jugador2 por member
jugador(Robot):-
	jugadores(Jugadores),
	member(Robot,Jugadores).
%	jugador2(Jugadores,Robot).

% jugador(Robot):-
% 	jugadores(Jugadores),
% 	jugador2(Jugadores,Robot).
jugador_prev(Robot):-
	jugadores_prev(Jugadores),
	member(Robot,Jugadores).
	%jugador2(Jugadores,Robot).

jugador2(_,robot(_,X,_)):- get_numplayers(N),X>N,display('error').
jugador2([],_).
jugador2([robot(E,X,P)|_R],robot(E,X,P)).
jugador2([_RR|R],robot(E,X,PosRobot)):-
	jugador2(R,robot(E,X,PosRobot)).

%estado interno inicial de las acciones previas y promedio realizadas
assert_actions_prev_prom(N) :-
	get_initial_velocity_list(N,Vs),
	asserta_fact(acciones_prev(Vs)),
	asserta_fact(acciones_prom(Vs)).

%assert_actions_prev_prom(_).

get_initial_velocity_list(0,[]).

get_initial_velocity_list(N,[0,0|Vs]):-
	N1 is N - 1,
	get_initial_velocity_list(N1,Vs),!.
	
%acciones_prev([0,0,0,0,0,0,0,0,0,0]).
%acciones_prom([0,0,0,0,0,0,0,0,0,0]).


:- pred insertar_accion(+Acciones) :: list
 # "Inserta las acciones realizadas actualizando las acciones previas y pormedio de la base de conocimiento.".

insertar_accion(Acciones):-
	acciones_prev(Accion_Prev),
	acciones_prom(Accion_Prom),
	%display('insertar_acción_prom'), display(Accion_Prom),nl,display(Accion_Prev),
	calcular_prom(Accion_Prom,Accion_Prev,Accion_Res),
	retractall_fact(acciones_prom(Accion_Prom)),
	asserta_fact(acciones_prom(Accion_Res)),
	retractall_fact(acciones_prev(Accion_Prev)),
	asserta_fact(acciones_prev(Acciones)).


calcular_prom([],[],[]).
calcular_prom([X|R1],[Y|R2],[P|R3]):-
	((X>0;X<0),P is (X+Y)/2;P is Y) , 
	%display('Prom'),display(P),
	calcular_prom(R1,R2,R3).

accion_prom(X,Vi,Vd):-
	acciones_prom(Acciones),
	seleccion(Acciones,X,Vi,Vd).

accion_prev(X,Vi,Vd):-
	acciones_prev(Acciones),
	seleccion(Acciones,X,Vi,Vd).

seleccion([Vi|[Vd|_R]],1,Vi,Vd).
%seleccion([],_,0,0).
seleccion([_Vi|[_Vd|R]],X,Vi,Vd):-
	X1 is X-1,
	seleccion(R,X1,Vi,Vd).
