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

:- module(navegacion,[ir_a_posicion/5,gira/5,resolver/4,ir_a_posicion_y_apuntar/6,calcular_angulo/4,apuntar_a_posicion/5], [assertions]).
:- use_module(ambiente).
:- use_module(logger).
:- use_module(trigonometry,[atan2/3,ajuste_angulo/2]).
:- use_module(configuration,[get_environment/1]).

:- comment(title, "Modulo navegacion").

:- comment(author, "Guillermo Torres, Mario Moya, Pablo Kogan").

:- comment(module, "Este módulo representa las primitivas de movimiento de los agentes.").


:- comment(doinclude,ir_a_posicion/5).


calcular_angulo(robot('propio',_Number,pos(Xr,Yr,_Z,_Rr)),Xp,Yp, Angulo) :-
	Dx is Xp-Xr,
	Dy is Yp-Yr,
	atan2(Dy,Dx,Angulo).

apuntar_a_posicion(robot('propio',_Number,pos(Xr,Yr,_Z,_Rr)),Xp,Yp, Vi,Vd) :-
	Dx is Xp-Xr,
	Dy is Yp-Yr,
	atan2(Dy,Dx,Angulo),
	apuntar(robot('propio',_Number,pos(Xr,Yr,_Z,_Rr)),Angulo,Vi,Vd).
	
	

ir_a_posicion_y_apuntar(robot('propio',Number,pos(Xr,Yr,_Z,_Rr)), X,Y,Angulo,Vi,Vd) :-
	Dx is X-Xr,
	Dy is Y-Yr,
	Distancia is sqrt(Dx**2 + Dy**2), % pitáagoras distancia al objetivo (X,Y)
	distancia_cero(Distancia),
	accion_prev(Number,0,0),
	agregar_comentario('Apuntando '),agregar_comentario(Number),
	apuntar(robot('propio',_Number,pos(Xr,Yr,_Z,_Rr)),Angulo,Vi,Vd).

ir_a_posicion_y_apuntar(robot('propio',Number,pos(Xr,Yr,_Z,_Rr)), X,Y,_Angulo,Vi,Vd) :-
	Dx is X-Xr,
	Dy is Y-Yr,
	Distancia is sqrt(Dx**2 + Dy**2), % pitáagoras distancia al objetivo (X,Y)
	distancia_cero(Distancia),
	\+(accion_prev(Number,0,0)),
	detener(Vi,Vd),
		agregar_comentario('Deteniendo '),agregar_comentario(Number).

ir_a_posicion_y_apuntar(robot('propio',_Number,pos(Xr,Yr,_Z,_Rr)), X,Y,_Angulo,Vi,Vd) :-
	Dx is X-Xr,
	Dy is Y-Yr,
	Distancia is sqrt(Dx**2 + Dy**2), % pitáagoras distancia al objetivo (X,Y)
	\+(distancia_cero(Distancia)),
	ir_a_posicion(robot('propio',_Number,pos(Xr,Yr,_Z,_Rr)),X,Y,Vi,Vd).

			 
apuntar(robot('propio',_Number,pos(_Xr,_Yr,_Z,Rr)), Angulo,0,0) :-
	diferencia_angular(Angulo, Rr, Diferencia),	
	error_angular(0, Error),
	abs(Diferencia) =< Error.

apuntar(robot('propio',_Number,pos(_Xr,_Yr,_Z,Rr)), Angulo,Vi,Vd) :-
	diferencia_angular(Angulo,Rr, Diferencia),
	error_angular(0, Error),
	abs(Diferencia) > Error,	
	resolver_apuntar(Diferencia,0,Vi,Vd).

diferencia_angular(Alfa, Beta, Diferencia):-
	Aux is (Alfa - Beta),
	ajuste_angulo(Aux, Diferencia),!.


error_angular(_Distancia,5).

distancia_cero(Distancia):-
	get_environment(real),
	Distancia =< (250). %tuning

distancia_cero(Distancia):-
	get_environment(simulado),
	Distancia =< (5). 

calcular_potencia(_Distancia,100).


:- pred ir_a_posicion(+Xr,+Yr,+Rr,+X,+Y,+Vc,-Vi,-Vd) :: int * int * int * int * int * int * int * int 
 # "Este predicado devuelve la accion (@var{Vi}, @var{Vd}, potencia en las ruedas) necesaria para que el robot ubicado en el punto (@var{Xr},@var{Yr}) con una rotacion @var{Rr} y con una presición de @var{Vc}, se dirija al punto (@var{X},@var{Y})".

%Devuelve la velocidad en cada rueda Vi Vd para que el robot ubicado en Xr,Yr y rotación Rr
%se dirija a la posición X,Y destino

ir_a_posicion(robot(propio,_Number,pos(Xr,Yr,_Z,_Rr)), X,Y,0,0) :-
	Dx is X-Xr,
	Dy is Y-Yr,
	Distancia is sqrt(Dx**2 + Dy**2), % pitáagoras distancia al objetivo (X,Y)
	distancia_cero(Distancia).

ir_a_posicion(robot(propio,_Number,pos(Xr,Yr,_Z,Rr)), X,Y,Vi,Vd) :-
	Dx is X-Xr,
	Dy is Y-Yr,
	D_e is sqrt(Dx**2 + Dy**2), % pitáagoras distancia al objetivo (X,Y)
%	display('dx, dy, D_e'),nl,	display(Dx),nl,	display(Dy),nl,	display(D_e),nl,
	angulo_deseado(Dy,Dx,Rr,Theta_e),
%	display(' Theta_e '),	display(Theta_e),	nl,
	ajuste_angulo(Theta_e,Theta_e2),
%	display('Angulo ajustado '),	display(Theta_e2),	nl,
	resolver(Theta_e2,D_e,Vi,Vd).
	

detener(0,0).
avanzar(robot('propio',_Number,_Pos), Vi,Vd):-
	Vi is 125,
	Vd is 125.

%Diferncial es re groso.
%to right implica que se aplica el diferencial sobre la rueda derecha

gira(Vl,Vr, Fuerza_de_giro, 'to_right', Diferencial):-
	Potencia is (Fuerza_de_giro*125/100),
	PotenciaDiferencial is ((Potencia * Diferencial)/100),
	Vr is  truncate(Potencia - PotenciaDiferencial),
	Vl is truncate(Potencia).
%to left implica que se aplica el diferencial sobre la rueda izquieda
gira(Vl,Vr, Fuerza_de_giro, 'to_left', Diferencial):-

	Potencia is (Fuerza_de_giro*125/100),
	PotenciaDiferencial is ((Potencia * Diferencial)/100),
	Vl is  truncate(Potencia - PotenciaDiferencial),
	Vr is truncate(Potencia).


%error_angular(0, 3).


%voy para la izquierda
resolver_apuntar(Theta_e2,Distancia,Vi,Vd):-
	distancia_cero(Distancia),
	error_angular(Distancia,Error),
	Theta_e2>=Error , Theta_e2 < 90,
	PotenciaDeGiro is Theta_e2/4,
	Diferencial is 200,
	gira(Vi,Vd,PotenciaDeGiro,'to_left',Diferencial).

%voy para la derecha
resolver_apuntar(Theta_e2,Distancia,Vi,Vd):-
	distancia_cero(Distancia),
	error_angular(Distancia,Error),
	Theta_e2<(-Error) , Theta_e2 > (-90),
	PotenciaDeGiro is Theta_e2*(-1)/4,
	Diferencial is 200,
	gira(Vi,Vd,PotenciaDeGiro,'to_right',Diferencial).
	
resolver_apuntar(Theta_e2,Distancia,Vi,Vd):-
	distancia_cero(Distancia),
	Theta_e2=<(-90),
	PotenciaDeGiro is (Theta_e2+180)/4,
	Diferencial is 200,
	gira(Vi,Vd,PotenciaDeGiro,'to_right',Diferencial).

%voy para la derecha
resolver_apuntar(Theta_e2,Distancia,Vi,Vd):-
	distancia_cero(Distancia),
	Theta_e2>=90,
	PotenciaDeGiro is abs(Theta_e2-180)/4,
	Diferencial is 200,
	gira(Vi,Vd,PotenciaDeGiro,'to_left',Diferencial).
%-------------------------------------
	

%voy para adelante
resolver(Theta_e2,D_e,125,125):-
	error_angular(D_e,Error),
	Theta_e2>(-Error), Theta_e2<Error.
%voy para atras
resolver(Theta_e2,D_e,-125,-125):-
	error_angular(D_e,Error),
	(Theta_e2>(180-Error) ; Theta_e2<(-180+Error)).
%voy para la izquierda
resolver(Theta_e2,D_e,Vi,Vd):-
	error_angular(D_e,Error),
	Theta_e2>=Error , Theta_e2 < 90,
	calcular_potencia(D_e,PotenciaDeGiro),
	Diferencial is Theta_e2,
	gira(Vi,Vd,PotenciaDeGiro,'to_left',Diferencial).
%voy para la derecha
resolver(Theta_e2,D_e,Vi,Vd):-
	error_angular(D_e,Error),
	Theta_e2=<(-Error) , Theta_e2 > (-90),
	calcular_potencia(D_e,PotenciaDeGiro),
	Diferencial is Theta_e2*(-1),
	gira(Vi,Vd,PotenciaDeGiro,'to_right',Diferencial).
	
	
resolver(Theta_e2,D_e,Vi,Vd):-
	Theta_e2=<(-90),
	calcular_potencia(D_e,Potencia),
	PotenciaDeGiro is Potencia*(-1),
	Diferencial is Theta_e2+180,
	gira(Vi,Vd,PotenciaDeGiro,'to_right',Diferencial).

%voy para la derecha
resolver(Theta_e2,D_e,Vi,Vd):-
	Theta_e2>=90,
	calcular_potencia(D_e,Potencia),
	PotenciaDeGiro is Potencia*(-1),
	Diferencial is abs(Theta_e2-180),
	gira(Vi,Vd,PotenciaDeGiro,'to_left',Diferencial).
	



%devuelve el ángulo que debería moverse hacia el objetivo sumandole el ángulo
%que tiene el robot Rr
angulo_deseado(0,0,Rr,Theta_e):-
	Theta_e is 90 - Rr.
angulo_deseado(Dy,Dx,Rr,Theta_e):-
		atan2(Dy,Dx,Desired_angle),
		Theta_e is Desired_angle - Rr .

%devuelve en Ka la potencia para las ruedas según la distancia al objetivo D_e		
