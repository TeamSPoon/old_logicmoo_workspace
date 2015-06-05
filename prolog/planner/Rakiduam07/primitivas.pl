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

%:- module(primitivas, [patear_pelota_a_posicion/5,llevar_pelota_a_posicion/5,acompannar_a_lleva_pelota/5,acompannar_a_lleva_pelota2/5,acompannar_a_lleva_pelota_pres/5,acompannar_a_lleva_pelota_def/5,esperar_pelota_en_posicion/7,desbloquear/3,ir_a_posicion_segura/5,ir_a_posicion_segura_pres/5,ir_a_posicion_insegura/5,ir_a_posicion_precisa/5,ir_a_posicion/8,gira/5,goto_position/5,avanzar/3,distancia_entre_puntos/4], [assertions]).

:- module(primitivas, [patear_pelota_a_posicion/5,llevar_pelota_a_posicion/5,acompannar_a_lleva_pelota/5,acompannar_a_lleva_pelota2/5,acompannar_a_lleva_pelota_pres/5,acompannar_a_lleva_pelota_def/5,esperar_pelota_en_posicion/7,desbloquear/3,ir_a_posicion_segura/5,ir_a_posicion_segura_pres/5,ir_a_posicion_insegura/5,ir_a_posicion_precisa/5], [assertions]).
:- use_module(ambiente).
:-use_module(navegacion).


:- comment(title, "Modulo primitivas").

:- comment(author, "Guillermo Torres, Mario Moya, Pablo Kogan").

:- comment(module, "Este módulo representa las primitivas de movimiento de los agentes.").


:- comment(doinclude,patear_pelota_a_posicion/5).
:- comment(doinclude,llevar_pelota_a_posicion/5).
:- comment(doinclude,acompannar_a_lleva_pelota/5). 
:- comment(doinclude,acompannar_a_lleva_pelota2/5).
:- comment(doinclude,acompannar_a_lleva_pelota_def/5). 
:- comment(doinclude,esperar_pelota_en_posicion/7).
:- comment(doinclude,desbloquear/3).
:- comment(doinclude,ir_a_posicion_segura/5).
:- comment(doinclude,ir_a_posicion_insegura/5).
:- comment(doinclude,ir_a_posicion_precisa/5).


%Ver... Problemas
:- pred patear_pelota_a_posicion(+Robot,+Xd,+Yd,-Vi,-Vd) :: atom *int * int * int * int 
 # "Este predicado no esta siendo utilizado.".

patear_pelota_a_posicion(robot(E,J,pos(Xr,Yr,_,Rr)),Xd,Yd,Vi,Vd):-
%	display('entro'), 
	DVer is 3.7, %Ver DVer
	DDest is 2.8, % Distancia a la pelota, desde el punto que se dirige el agente para llevar la pelota hacia el destino
	AVer is 30, %Ver ángulo Diferencia angular 
	pelota_pred(Xball,Yball,_),
	%Xball is 4,Yball is 4,
	Dy_Ball_Dest is Yd-Yball,
	Dx_Ball_Dest is Xd-Xball,
	Dy_Robot_Ball is Yball-Yr,
	Dx_Robot_Ball is Xball-Xr,
	atan2(Dy_Ball_Dest,Dx_Ball_Dest,Ang_Ball_Dest),
%	display('Angulo Pelota Destino '),display(Ang_Ball_Dest),
	atan2(Dy_Robot_Ball,Dx_Robot_Ball,Ang_Robot_Ball),
%	display('Angulo Robot Pelota '),display(Ang_Robot_Ball),
%	display('paso antan'),
	%Calculo la función Pelota_Destino yd = Md.X + Cd
	Md is Dy_Ball_Dest/Dx_Ball_Dest,
	Cd is Yball-Md*Xball,
	Dist_Robot_Ball=sqrt(Dx_Robot_Ball**2+Dy_Robot_Ball**2),
	(Xball>Xd,Signo is 1;Xball<Xd,Signo is -1;Signo is 0),
	Ang_Dif is abs(Ang_Ball_Dest-Ang_Robot_Ball),
%	display(Ang_Dif),nl,

	( %Si el angulo de la pelota al destino y del robot a la pelota difieren en menso de Ave
	  %y si la distancia del robot a la pelota es menor a Dver sigo con dirección a la pelota
	  Ang_Dif<AVer, %Ver ángulo
	  Dist_Robot_Ball<DVer, %Ver DVer
	  %Xnuevo is Xball, Ynuevo is Yball,
%	  display('Va traz la pelota'),
	  %Xnuevo is Xball - Signo* sqrt(DDest**2/(1+Md**2)),
	  %Ynuevo is Md*Xdv +Cd;	    
	  %Xnuevo is Xd,
	  %Ynuevo is Yd;
	  (Ang_Ball_Dest>0,Vi is -125,Vd is 125;Vi is 125,Vd is -125);
	  %Calculo los puntos destino nuevos.

	  Xdv is Xball + Signo* sqrt(DDest**2/(1+Md**2)),
	  Ydv is Md*Xdv +Cd,
	  evacion_obstaculo(Xr,Yr,Xball,Yball,Xdv,Ydv,Xnuevo,Ynuevo,2.5),
	ir_a_posicion_insegura(robot(E,J,pos(Xr,Yr,_Zr,Rr)),Xnuevo,Ynuevo,Vi,Vd)
	).
	%display(Xnuevo),nl,display(Ynuevo),nl,

%	ir_a_posicion_segura(Xr,Yr,Rr,Xnuevo,Ynuevo,Vi,Vd).
%********************************************************************************



%Casos de test levar pelota  a posición

%main:-
%	Xr is 5,Yr is 3,Xd is 0,Yd is 8,
%	llevar_pelota_a_posicion(robot(E,J,pos(Xr,Yr,_,_Rr)),Xd,Yd,Vi,Vd).
%	llevar_pelota_a_posicion(robot(_,_,pos,(1,1,_,_)),1,10,Vi,Vd).
:- pred llevar_pelota_a_posicion(+Robot,+Xd,+Yd,-Vi,-Vd) :: atom * int * int * int * int 
 # "Este predicado devuelve la accion (@var{Vi}, @var{Vd}, potencia en las ruedas) necesaria para que el robot @var{Robot} lleve la pelota al punto (@var{Xd},@var{Yd}).".

llevar_pelota_a_posicion(robot(E,J,pos(Xr,Yr,_,Rr)),Xd,Yd,Vi,Vd):-
%	display('entro'), 
	DVer is 3.7, %Ver DVer
	DDest is 2.4, % Distancia a la pelota, desde el punto que se dirige el agente para llevar la pelota hacia el destino
	AVer is 30, %Ver ángulo Diferencia angular 
	pelota_pred(Xball,Yball,_),
	%Xball is 4,Yball is 4,
	Dy_Ball_Dest is Yd-Yball,
	Dx_Ball_Dest is Xd-Xball,
	Dy_Robot_Ball is Yball-Yr,
	Dx_Robot_Ball is Xball-Xr,
	atan2(Dy_Ball_Dest,Dx_Ball_Dest,Ang_Ball_Dest),
%	display('Angulo Pelota Destino '),display(Ang_Ball_Dest),
	atan2(Dy_Robot_Ball,Dx_Robot_Ball,Ang_Robot_Ball),
%	display('Angulo Robot Pelota '),display(Ang_Robot_Ball),
%	display('paso antan'),
	%Calculo la función Pelota_Destino yd = Md.X + Cd
	Md is Dy_Ball_Dest/Dx_Ball_Dest,
	Cd is Yball-Md*Xball,
	Dist_Robot_Ball=sqrt(Dx_Robot_Ball**2+Dy_Robot_Ball**2),
	(Xball>Xd,Signo is 1;Xball<Xd,Signo is -1;Signo is 0),
	Ang_Dif is abs(Ang_Ball_Dest-Ang_Robot_Ball),
%	display(Ang_Dif),nl,

	( %Si el angulo de la pelota al destino y del robot a la pelota difieren en menso de Ave
	  %y si la distancia del robot a la pelota es menor a Dver sigo con dirección a la pelota
	  Ang_Dif<AVer, %Ver ángulo
	  Dist_Robot_Ball<DVer, %Ver DVer
	  %Xnuevo is Xball, Ynuevo is Yball,
%	  display('Va traz la pelota'),
	  %Xnuevo is Xball - Signo* sqrt(DDest**2/(1+Md**2)),
	  %Ynuevo is Md*Xdv +Cd;	    
	  Xnuevo is Xd,
	  Ynuevo is Yd;

	  %Calculo los puntos destino nuevos.

	  Xdv is Xball + Signo* sqrt(DDest**2/(1+Md**2)),
	  Ydv is Md*Xdv +Cd,
	  evacion_obstaculo(Xr,Yr,Xball,Yball,Xdv,Ydv,Xnuevo,Ynuevo,2.5)
	),
	%display(Xnuevo),nl,display(Ynuevo),nl,
	ir_a_posicion_insegura(robot(E,J,pos(Xr,Yr,_Zr,Rr)),Xnuevo,Ynuevo,Vi,Vd).
%	ir_a_posicion_segura(Xr,Yr,Rr,Xnuevo,Ynuevo,Vi,Vd).
%********************************************************************************
:- pred acompannar_a_lleva_pelota(+Robot,+Xd,+Yd,-Vi,-Vd) :: atom * int * int * int * int 
 # "Este predicado devuelve la accion (@var{Vi}, @var{Vd}, potencia en las ruedas) necesaria para que el robot @var{Robot} acompañe al que lleva la pelota a una distancia razonable (@var{Xd},@var{Yd}).".

acompannar_a_lleva_pelota(robot(E,J,pos(Xr,Yr,_,Rr)),Xd,Yd,Vi,Vd):-
%	display('entro'), 
%	DVer is 3.7, %Ver DVer
	ancho_cancha(An),
	DDest is An/6, % Distancia a la pelota, desde el punto que se dirige el agente para llevar la pelota hacia el destino
%	AVer is 30, %Ver ángulo Diferencia angular 
	pelota_pred(Xball,Yball,_),
	%Xball is 4,Yball is 4,
	Dy_Ball_Dest is Yd-Yball,
	Dx_Ball_Dest is Xd-Xball,
%	Dy_Robot_Ball is Yball-Yr,
%	Dx_Robot_Ball is Xball-Xr,
%	atan2(Dy_Ball_Dest,Dx_Ball_Dest,Ang_Ball_Dest),
%	display('Angulo Pelota Destino '),display(Ang_Ball_Dest),
%	atan2(Dy_Robot_Ball,Dx_Robot_Ball,Ang_Robot_Ball),
%	display('Angulo Robot Pelota '),display(Ang_Robot_Ball),
%	display('paso antan'),
	%Calculo la función Pelota_Destino yd = Md.X + Cd
	Md is Dy_Ball_Dest/Dx_Ball_Dest,
	Cd is Yball-Md*Xball,
%	Dist_Robot_Ball=sqrt(Dx_Robot_Ball**2+Dy_Robot_Ball**2),
	(Xball>Xd,Signo is 1;Xball<Xd,Signo is -1;Signo is 0),
%	Ang_Dif is abs(Ang_Ball_Dest-Ang_Robot_Ball),
%	display(Ang_Dif),nl,

%	( %Si el angulo de la pelota al destino y del robot a la pelota difieren en menso de Ave
	  %y si la distancia del robot a la pelota es menor a Dver sigo con dirección a la pelota
%	  Ang_Dif<AVer, %Ver ángulo
%	  Dist_Robot_Ball<DVer, %Ver DVer
	  %Xnuevo is Xball, Ynuevo is Yball,
%	  display('Va traz la pelota'),
	  %Xnuevo is Xball - Signo* sqrt(DDest**2/(1+Md**2)),
	  %Ynuevo is Md*Xdv +Cd;	    
%	  Xnuevo is Xd,
%	  Ynuevo is Yd;

	  %Calculo los puntos destino nuevos.

	  Xdv is Xball + Signo* sqrt(DDest**2/(1+Md**2)),
	  Ydv is Md*Xdv +Cd,
	  evacion_obstaculo(Xr,Yr,Xball,Yball,Xdv,Ydv,Xnuevo,Ynuevo,7.5), 
%	),
	%display(Xnuevo),nl,display(Ynuevo),nl,
	ir_a_posicion_segura_pres(robot(E,J,pos(Xr,Yr,_Zr,Rr)),Xnuevo,Ynuevo,Vi,Vd).
%	ir_a_posicion_segura(Xr,Yr,Rr,Xnuevo,Ynuevo,Vi,Vd).
%********************************************************************************

%********************************************************************************
:- pred acompannar_a_lleva_pelota2(+Robot,+Xd,+Yd,-Vi,-Vd) :: atom * int * int * int * int 
 # "Este predicado devuelve la accion (@var{Vi}, @var{Vd}, potencia en las ruedas) necesaria para que el robot @var{Robot} acompañe al que lleva la pelota a una distancia razonable (@var{Xd},@var{Yd}).".

acompannar_a_lleva_pelota2(robot(E,J,pos(Xr,Yr,_,Rr)),Xd,_Yd,Vi,Vd):-
%	display('entro'), 
%	DVer is 3.7, %Ver DVer
	ancho_cancha(An),
	DDest is An/5, % Distancia a la pelota, desde el punto que se dirige el agente para llevar la pelota hacia el destino
%	AVer is 30, %Ver ángulo Diferencia angular 
	pelota_pred(Xball,Yball,_),
	(Xball>Xd,Signo is 1;Xball<Xd,Signo is -1;Signo is 0),
	Xdv is Xball + Signo* DDest,
	Ydv is Yball,
	evacion_obstaculo(Xr,Yr,Xball,Yball,Xdv,Ydv,Xnuevo,Ynuevo,10), 
	ir_a_posicion_segura(robot(E,J,pos(Xr,Yr,_Zr,Rr)),Xnuevo,Ynuevo,Vi,Vd).


acompannar_a_lleva_pelota_pres(robot(E,J,pos(Xr,Yr,_,Rr)),Xd,_Yd,Vi,Vd):-
%	display('entro'), 
%	DVer is 3.7, %Ver DVer
	ancho_cancha(An),
	DDest is An/5, % Distancia a la pelota, desde el punto que se dirige el agente para llevar la pelota hacia el destino
%	AVer is 30, %Ver ángulo Diferencia angular 
	pelota_pred(Xball,Yball,_),
	(Xball>Xd,Signo is 1;Xball<Xd,Signo is -1;Signo is 0),
	Xdv is Xball + Signo* DDest,
	Ydv is Yball,
	evacion_obstaculo(Xr,Yr,Xball,Yball,Xdv,Ydv,Xnuevo,Ynuevo,10), 
	ir_a_posicion_segura_pres(robot(E,J,pos(Xr,Yr,_Zr,Rr)),Xnuevo,Ynuevo,Vi,Vd).

%********************************************************************************


%********************************************************************************
:- pred acompannar_a_lleva_pelota_def(+Robot,+Xd,+Yd,-Vi,-Vd) :: atom * int * int * int * int 
 # "Este predicado devuelve la accion (@var{Vi}, @var{Vd}, potencia en las ruedas) necesaria para que el robot @var{Robot} acompañe al que lleva la pelota a una distancia razonable entre la pelota y el punto (@var{Xd},@var{Yd}).".

acompannar_a_lleva_pelota_def(robot(E,J,pos(Xr,Yr,_,Rr)),Xd,Yd,Vi,Vd):-
%	display('entro'), 
%	DVer is 3.7, %Ver DVer
	ancho_cancha(An),
	DDest is An/6, % Distancia a la pelota, desde el punto que se dirige el agente para llevar la pelota hacia el destino
%	AVer is 30, %Ver ángulo Diferencia angular 
	pelota_pred(Xball,Yball,_),
	%Xball is 4,Yball is 4,
	Dy_Ball_Dest is Yd-Yball,
	Dx_Ball_Dest is Xd-Xball,
%	Dy_Robot_Ball is Yball-Yr,
%	Dx_Robot_Ball is Xball-Xr,
%	atan2(Dy_Ball_Dest,Dx_Ball_Dest,Ang_Ball_Dest),
%	display('Angulo Pelota Destino '),display(Ang_Ball_Dest),
%	atan2(Dy_Robot_Ball,Dx_Robot_Ball,Ang_Robot_Ball),
%	display('Angulo Robot Pelota '),display(Ang_Robot_Ball),
%	display('paso antan'),
	%Calculo la función Pelota_Destino yd = Md.X + Cd
	Md is Dy_Ball_Dest/Dx_Ball_Dest,
	Cd is Yball-Md*Xball,
%	Dist_Robot_Ball=sqrt(Dx_Robot_Ball**2+Dy_Robot_Ball**2),
	(Xball>Xd,Signo is 1;Xball<Xd,Signo is -1;Signo is 0),
%	Ang_Dif is abs(Ang_Ball_Dest-Ang_Robot_Ball),
%	display(Ang_Dif),nl,

%	( %Si el angulo de la pelota al destino y del robot a la pelota difieren en menso de Ave
	  %y si la distancia del robot a la pelota es menor a Dver sigo con dirección a la pelota
%	  Ang_Dif<AVer, %Ver ángulo
%	  Dist_Robot_Ball<DVer, %Ver DVer
	  %Xnuevo is Xball, Ynuevo is Yball,
%	  display('Va traz la pelota'),
	  %Xnuevo is Xball - Signo* sqrt(DDest**2/(1+Md**2)),
	  %Ynuevo is Md*Xdv +Cd;	    
%	  Xnuevo is Xd,
%	  Ynuevo is Yd;

	  %Calculo los puntos destino nuevos.

	  Xdv is Xball - Signo* sqrt(DDest**2/(1+Md**2)),
	  Ydv is Md*Xdv +Cd,
	  evacion_obstaculo(Xr,Yr,Xball,Yball,Xdv,Ydv,Xnuevo,Ynuevo,10), 
%	),
	%display(Xnuevo),nl,display(Ynuevo),nl,
	ir_a_posicion_segura(robot(E,J,pos(Xr,Yr,_Zr,Rr)),Xnuevo,Ynuevo,Vi,Vd).
%	ir_a_posicion_segura(Xr,Yr,Rr,Xnuevo,Ynuevo,Vi,Vd).
%********************************************************************************






:-pred esperar_pelota_en_posicion(+Robot,+X,+Y,+Xd,+Yd,-Vi,-Vd) :: atom * int * int * int * int 
 # "Este predicado devuelve la accion (@var{Vi}, @var{Vd}, potencia en las ruedas) necesaria para que el robot @var{Robot} se ubique en la posicion (@var{X},@var{Y}), con una orientacion tal, para que luego de que la pelota impacte con el, salga en direccion al punto  (@var{Xd},@var{Yd}).".

esperar_pelota_en_posicion(robot(E,J,pos(Xr,Yr,_,Rr)),Xball,Yball,Xd,Yd,Vi,Vd):-
%	display('entro'), 
	DVer is 3.7, %Ver DVer
	DDest is 2.4,
	AVer is 30, %Ver ángulo
	%pelota_pred(Xball,Yball,_),
	%Xball is 4,Yball is 4,
	Dy_Ball_Dest is Yd-Yball,
	Dx_Ball_Dest is Xd-Xball,
	Dy_Robot_Ball is Yball-Yr,
	Dx_Robot_Ball is Xball-Xr,
	atan2(Dy_Ball_Dest,Dx_Ball_Dest,Ang_Ball_Dest),
%	display('Angulo Pelota Destino '),display(Ang_Ball_Dest),
	atan2(Dy_Robot_Ball,Dx_Robot_Ball,Ang_Robot_Ball),
%	display('Angulo Robot Pelota '),display(Ang_Robot_Ball),
%	display('paso antan'),
	%Calculo la función Pelota_Destino yd = Md.X + Cd
	Md is Dy_Ball_Dest/Dx_Ball_Dest,
	Cd is Yball-Md*Xball,
	Dist_Robot_Ball=sqrt(Dx_Robot_Ball**2+Dy_Robot_Ball**2),
	(Xball>Xd,Signo is 1;Xball<Xd,Signo is -1;Signo is 0),
	Ang_Dif is abs(Ang_Ball_Dest-Ang_Robot_Ball),
%	display(Ang_Dif),nl,

	( %Si el angulo de la pelota al destino y del robot a la pelota difieren en menso de Ave
	  %y si la distancia del robot a la pelota es menor a Dver sigo con dirección a la pelota
	  Ang_Dif<AVer, %Ver ángulo
	  Dist_Robot_Ball<DVer, %Ver DVer
	  %Xnuevo is Xball, Ynuevo is Yball,
%	  display('Va traz la pelota'),
	  %Xnuevo is Xball - Signo* sqrt(DDest**2/(1+Md**2)),
	  %Ynuevo is Md*Xdv +Cd;	    
	  Xnuevo is Xd,
	  Ynuevo is Yd;

	  %Calculo los puntos destino nuevos.

	  Xdv is Xball + Signo* sqrt(DDest**2/(1+Md**2)),
	  Ydv is Md*Xdv +Cd,
	  evacion_obstaculo(Xr,Yr,Xball,Yball,Xdv,Ydv,Xnuevo,Ynuevo,3)
	),
	%display(Xnuevo),nl,display(Ynuevo),nl,
	ir_a_posicion_precisa(robot(E,J,pos(Xr,Yr,_Zr,Rr)),Xnuevo,Ynuevo,Vi,Vd).
%	ir_a_posicion_segura(Xr,Yr,Rr,Xnuevo,Ynuevo,Vi,Vd).
%********************************************************************************
%no esta siendo usado
evacion_obstaculo_pelota(X1,Y1,Xo,Yo,Xb,Yb,Xb,Yb):-%no es obstaculo
	Xo<(X1-3),Xo<(Xb+3);
	Xo>(X1+3),Xo>(Xb-3);
	Yo<(Y1-3),Yo<(Yb+3);
	Yo>(Y1+3),Yo>(Yb-3).

%idem a evacion obstaculo con la diferencia que el obstaculo es mas chico.
evacion_obstaculo_pelota(X1,Y1,Xo,Yo,Xb,Yb,Xd,Yd):-
	%Cálculo Recta (Robot-Pelota) Yp = Mp*X + Cp 
	Dx is Xb-X1,
	Dy is Yb-Y1,
	%D_b is sqrt(Dx**2 + Dy**2), % pitagoras distancia al objetivo (Xb,Yb)
	Mp is Dy/Dx,
	Cp is Y1-Mp*X1,
	%Cálculo Recta perpendicular a Yp Yobs=Mobs*X + Cobs
	Mobs is -(1/Mp),
	Cobs is Yo-Mobs*Xo,
	%Cálculo del punto intersección de las rectas
	Xc is (Cobs-Cp)/(Mp-Mobs),
	Yc is Mp*Xc+Cp,
	%Cálculo de distancia del obstaculo a la intersección de las rectas
	D is sqrt((Xc-Xo)**2+(Yc-Yo)**2),
	(D<3.5, %Asumo  como la diagonal mayor del robot
		%Cálculo (Xs,Ys) (Xi,Yi) puntos alternativos para evadir el obstaculo
		% a Dt = 2 del obstaculo
		Raiz is sqrt(12.25 /(1+Mobs**2)), %3.5**2=12.25 Raiz es la diferencia en x
		Xs is Xo + Raiz,
		%display(Mp),		display(Mobs),
		Ys is Mobs*Xs + Cobs,
		Xi is Xo - Raiz,
		Yi is Mobs*Xi + Cobs,
		%display('Xs,Ys,Xi,Yi'),display(Xs),display(Ys),display(Xi),display(Yi),nl,
		%Se selecciona uno de los dos puntos dependiendo de la heuristica
		%Se toma el punto mas cercano
		Ds is sqrt((X1- Xs)**2 + (Y1- Ys)**2),
		Di is sqrt((X1- Xi)**2 + (Y1- Yi)**2),
		(Ds<Di,Xd is Xs,Yd is Ys;Xd is Xi,Yd is Yi);
		%else no es obstáculo
		Xd is Xb, Yd is Yb
	).

%********************************************************************************
%********************************************************************************
:- pred desbloquear(+Robot,-Vi,-Vd) :: atom *  int * int 
 # "Este predicado devuelve la accion (@var{Vi}, @var{Vd}, potencia en las ruedas) necesaria para que el robot @var{Robot} gire en el lugar a maxima velosidad en el sentido contrario al que se estaba intentando mover.".

%evitar Atascamientos
%un jugador es considrerado atascado si:
%la potencia de acción anterior es diferente a 0 en las dos ruedas 	Viprev=\=0,Vdprev=\=0,
%la potencia de acción promedio acumulada difiere en menos de 12.5 con la potencia de acción tamada en el evento anterior
%un factor de desplazamiento calculado en base a los dos últimos eventos es menor que un factor calculado en base a la potencia promedio en las dos ruedas del evento anterior.

%Luego la potencia en las dos redas es para girar a máxima velocidad en el sentido contrario al que se estaba intentando mover.


desbloquear(robot(_E,J,pos(_Xr,_Yr,_,_Rr)),Vi,Vd):-
	%jugador_prev(robot(E,J,pos(Xprev,Yprev,_,Rprev))),
	%display(Xprev),nl,
	accion_prev(J,Viprev,Vdprev),
	%accion_prom(J,Viprom,Vdprom),
	%display(Viprev),nl,display(Viprom),nl,
	Signo is (Vdprev-Viprev)/abs(Vdprev-Viprev),
	((Viprev+Vdprev)=\=0,
	Vi is -Signo*125, %Vdpred*1.5,
	Vd is Signo*125;
	Vi is Signo*125, %Vdpred*1.5,
	Vd is -Signo*125).
	%-Vipred*1.5.
	


%********************************************************************************
:- pred ir_a_posicion_segura(+Robot,+X,+Y,-Vi,-Vd) :: atom * int * int * int * int 
 # "Este predicado devuelve la accion (@var{Vi}, @var{Vd}, potencia en las ruedas) necesaria para que el robot @var{Robot} se dirija al punto (@var{Xd},@var{Yd}), evadiendo los obstaculos que existan en el camino.".

ir_a_posicion_segura(robot(E,J,pos(Xr,Yr,_Zr,Rr)),Xsel,Ysel,Iz,De):-
	jugadores(Jugadores),
	ordenar(Jugadores,Jordenado,Xr,Yr),
	%display(Jordenado),
	evitar_obstaculos(Jordenado,Xr,Yr,Xsel,Ysel,Xnuevo,Ynuevo),
	ir_a_posicion(robot(E,J,pos(Xr,Yr,_Zr,Rr)),Xnuevo,Ynuevo,Iz,De).
ir_a_posicion_segura_pres(robot(E,J,pos(Xr,Yr,_Zr,Rr)),Xsel,Ysel,Iz,De):-
	jugadores(Jugadores),
	ordenar(Jugadores,Jordenado,Xr,Yr),
	%display(Jordenado),
	evitar_obstaculos(Jordenado,Xr,Yr,Xsel,Ysel,Xnuevo,Ynuevo),
	ir_a_posicion(robot(E,J,pos(Xr,Yr,_Zr,Rr)),Xnuevo,Ynuevo,Iz,De).


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


evitar_obstaculos([],_Xr,_Yr,X,Y,X,Y).
evitar_obstaculos([robot(_,_N,pos(Xr,Yr,_Zr,_Rr))|R],Xr,Yr,X,Y,Xd,Yd):-
	evitar_obstaculos(R,Xr,Yr,X,Y,Xd,Yd).
evitar_obstaculos([robot(_,_N,pos(Xo,Yo,_Zr,_Rr))|R],X1,Y1,Xb,Yb,Xd,Yd):-
%	evacion_obstaculo(X1,Y1,Xo,Yo,Xb,Yb,Xd1,Yd1),
	evitar_obstaculos(R,X1,Y1,Xb,Yb,Xd2,Yd2),
	%(Xd1==Xd2,Yd1==Yd2,Xd is Xd1,Yd is Yd1;
	evacion_obstaculo(X1,Y1,Xo,Yo,Xd2,Yd2,Xd,Yd,5).
	%evitar_obstaculos([robot(_N,pos(Xo,Yo,_Zr,_Rr))|R],X1,Y1,Xd2,Yd2,Xd,Yd)).


%************************************************************	
%evación del obstaculo Xo,Yo
%robot en posición X1,Y1
%destino inicial posición Xb,Yb

evacion_obstaculo(X1,Y1,Xo,Yo,Xb,Yb,Xb,Yb,_Dist_Obst):-%no es obstaculo
	Xo<(X1-3),Xo<(Xb);
	Xo>(X1+3),Xo>(Xb);
	Yo<(Y1-3),Yo<(Yb);
	Yo>(Y1+3),Yo>(Yb).
evacion_obstaculo(X1,Y1,Xo,Yo,Xb,Yb,Xd,Yd,Dist_Obst):-
	%Cálculo Recta (Robot-Pelota) Yp = Mp*X + Cp 
	Dx is Xb-X1,
	Dy is Yb-Y1,
	%D_b is sqrt(Dx**2 + Dy**2), % pitagoras distancia al objetivo (Xb,Yb)
	Mp is Dy/Dx,
	Cp is Y1-Mp*X1,
	%Cálculo Recta perpendicular a Yp Yobs=Mobs*X + Cobs
	Mobs is -(1/Mp),
	Cobs is Yo-Mobs*Xo,
	%Cálculo del punto intersección de las rectas
	Xc is (Cobs-Cp)/(Mp-Mobs),
	Yc is Mp*Xc+Cp,
	%Cálculo de distancia del obstaculo a la intersección de las rectas
	D is sqrt((Xc-Xo)**2+(Yc-Yo)**2),
	(D<3.5, %Asumo 3.5 como la diagonal mayor del robot
		%Cálculo (Xs,Ys) (Xi,Yi) puntos alternativos para evadir el obstaculo
		% a Dt = 5 del obstaculo
		Raiz is sqrt(Dist_Obst**2 /(1+Mobs**2)), %5**2=25 Raiz es la diferencia en x
		Xs is Xo + Raiz,
		%display(Mp),		display(Mobs),
		Ys is Mobs*Xs + Cobs,
		Xi is Xo - Raiz,
		Yi is Mobs*Xi + Cobs,
		%display('Xs,Ys,Xi,Yi'),display(Xs),display(Ys),display(Xi),display(Yi),nl,
		%Se selecciona uno de los dos puntos dependiendo de la heuristica
		%Se toma el punto mas cercano
		Ds is sqrt((X1- Xs)**2 + (Y1- Ys)**2),
		Di is sqrt((X1- Xi)**2 + (Y1- Yi)**2),
		(Ds<Di,Xd is Xs,Yd is Ys;Xd is Xi,Yd is Yi);
		%else no es obstáculo
		Xd is Xb, Yd is Yb
	).

%************************************************************	
%********************************************************************************
:- pred ir_a_posicion_insegura(+Robot,+X,+Y,-Vi,-Vd) :: atom * int * int * int * int 
 # "Este predicado devuelve la accion (@var{Vi}, @var{Vd}, potencia en las ruedas) necesaria para que el robot @var{Robot} se dirija al punto (@var{Xd},@var{Yd}), sin evadir los obstaculos que existan en el camino.".

ir_a_posicion_insegura(robot(E,J,pos(Xr,Yr,_Zr,Rr)),X,Y,Iz,De):-
	ir_a_posicion(robot(E,J,pos(Xr,Yr,_Zr,Rr)),X,Y,Iz,De).



%********************************************capa mas baja

%********************************************************************************
:- pred ir_a_posicion_precisa(+Robot,+X,+Y,-Vi,-Vd) :: atom * int * int * int * int 
 # "Este predicado devuelve la accion (@var{Vi}, @var{Vd}, potencia en las ruedas) necesaria para que el robot @var{Robot} se dirija al punto (@var{Xd},@var{Yd}), sin evadir los obstaculos que existan en el camino.".

ir_a_posicion_precisa(robot(E,J,pos(Xr,Yr,_Zr,Rr)),X,Y,Iz,De):-
	ir_a_posicion(robot(E,J,pos(Xr,Yr,_Zr,Rr)),X,Y,Iz,De).



%********************************************capa mas baja



%función que devuelve el ángulo X = arctan(Dy/Dx) según el cuadrante en en rango [-180, 180]

%Ver Casos especiales que evitan divición por 0
%atan2(Dy,0,90):-Dy>0.
%atan2(Dy,0,-90).

atan2(Dy,Dx,X):- %segundo cuadrante
	Dx < 0, Dy >0, X is truncate(180/3.14 * atan(Dy/Dx)+180).
atan2(Dy,Dx,X):- %tercer cuadrante
	Dx < 0, Dy < 0, X is truncate(180/3.14 * atan(Dy/Dx)-180).	
atan2(Dy,Dx,X):- %primero y cuarto cuadrante se dejan como estan
	 X is truncate(180/3.14 * atan(Dy/Dx)).

%ajusta el ángulo T en T2 para que este dentro del rango [-180,180]

ajuste_angulo(T,T2):-
	T =< 180,
	ajuste_angulo2(T,T2).
ajuste_angulo(T,T2):-
	T3 is T - 360,
	ajuste_angulo(T3,T2).
ajuste_angulo2(T,T):- (T > -180).
ajuste_angulo2(T,T2):-
	T3 is T + 360,
	ajuste_angulo2(T3,T2).

%devuelve el ángulo que debería moverse hacia el objetivo sumandole el ángulo
%que tiene el robot Rr
angulo_deseado(0,0,Rr,Theta_e):-
	Theta_e is 90 - Rr.
angulo_deseado(Dy,Dx,Rr,Theta_e):-
		atan2(Dy,Dx,Desired_angle),
		Theta_e is Desired_angle - Rr .

