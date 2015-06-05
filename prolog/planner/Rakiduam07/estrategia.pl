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

:- module(estrategia, [estrategia/2,iniciar/1,get_team_behavior/1], [assertions]).
%% modulo azul
%% módulo principal de la estrategia del equipo azul
%:- use_module(sim_video_server).
%% módulo de conexión con el servidor de video
%:- use_module(sim_command_server).
%% módulo de conexión con el servidor de comandos
:- use_module(primitivas).
:- use_module(navegacion).
%% módulo para gestión de logs
:- use_module(logger).
%% modulo para gestion de datos de ambiente
:- use_module(ambiente,[iniciar_ambiente/1,insertar_estado/1,insertar_accion/1,jugador/1,distancia/3,jugador_prev/1,pelota_entre/4,pelota_pred/3,pelota_pred/3,pelota_entre/4,mas_cercanos/2,medio_cancha/2,mas_cercanos_lugar/4,atascado2/1,atascado/1,robot_entre/5,pelota_pred/3,cancha/4,mas_cercano/1,pelota_en_direccion_a_zona/6,largo_cancha/1,arco_propio/4,arco_contrario/4,cancha/4,medio_cancha/2,alto_area/1,mas_cercanos2/1,ancho_area/1,alto_area_chica/1,ancho_area_chica/1,ancho_cancha/1]).
:- use_module(configuration,[players_names/1,get_player/3,get_role/2]).

:- comment(title, "Modulo estrategia").
:- comment(author, "Pablo Kogan").

:- comment(module, "Este módulo representa el el comportamiento de los agentes, la divicion de roles y todo lo que tenga que ver con la estrategia de juego. La interfaz esta establecida mediante el predicado estrategia.").

:- comment(doinclude,estrategia/2).


iniciar(Equipo):- iniciar_ambiente(Equipo).


:- pred estrategia(Estado,-ListaVelocidades) :: list * list 
 # "El predicado estrategia resuelve la acción a tomar (velocidad en cada rueda de cada robot @var{ListaVelocidades}) en base al @var{Estado} actual del ambiente.".
%% la función estrategia resuelve la acción a tomar (velocidad en cada rueda de cada robot) en base al Estado actual

estrategia(Estado,Vs):-
%	nl,display(Estado),nl,
	insertar_estado(Estado),
	get_team_behavior(Vs),
%	display(Vs),nl,
	insertar_accion(Vs),
	!.


get_team_behavior(Vs) :-
	players_names(Players),
	get_behavior(Players,Vs),!.

get_behavior([],[]).

get_behavior([Name|Tail],[Vi,Vd|Vs]) :-
	comportamiento(Name,Vi,Vd),
	get_behavior(Tail,Vs).



% estrategia(Estado,[Iz1,De1,Iz2,De2,Iz3,De3,Iz4,De4,Iz5,De5]):-
% 	nl,display(Estado),nl,
% 	insertar_estado(Estado),
%  	comportamiento('kechu',Iz5,De5),
%  	%display(Iz5),display(','),display(De5),nl,
%  	comportamiento('meli',Iz4,De4),
%  	comportamiento('küla',Iz3,De3),
%  	comportamiento('epu',Iz2,De2),
%  	comportamiento('kiñe',Iz1,De1),
% 	display([Iz1,De1,Iz2,De2,Iz3,De3,Iz4,De4,Iz5,De5]),nl,
% 	insertar_accion([Iz1,De1,Iz2,De2,Iz3,De3,Iz4,De4,Iz5,De5]),
% 	%display(' paso insertar_accion'), 
% 	!.


%***************************asignación roles
% 
% asignacion_rol('kiñe','arquero').
% asignacion_rol('epu','jugador').
% asignacion_rol('küla','jugador').
% asignacion_rol('meli','jugador').
% asignacion_rol('kechu','jugador').
%****************************asignación robots
asignacion_robot(Name, robot(propio,Num,Pos)) :-
	get_player(Name,Num,propio),
	jugador(robot(propio,Num,Pos)).

asignacion_robot(_,_).
% asignacion_robot('kiñe',robot('propio',1,Pos)):-
% 	jugador(robot('propio',1,Pos)).
% asignacion_robot('epu',robot('propio',2,Pos)):-
% 	jugador(robot('propio',2,Pos)).
% asignacion_robot('küla',robot('propio',3,Pos)):-
% 	jugador(robot('propio',3,Pos)).
% asignacion_robot('meli',robot('propio',4,Pos)):-
% 	jugador(robot('propio',4,Pos)).
% asignacion_robot('kechu',robot('propio',5,Pos)):-
% 	jugador(robot('propio',5,Pos)).

%*******************************************comportamientos dinamicos*****************************************

% comportamiento(Jugador,Iz,De):-
% 	asignacion_rol(Jugador,Rol),
% 	asignacion_robot(Jugador,Robot),
% 	accion(Rol,Robot,Iz,De).

comportamiento(Jugador,Iz,De):-
	get_role(Jugador,Rol),
	asignacion_robot(Jugador,Robot),
	accion(Rol,Robot,Iz,De).
	%set_fact(behavior(Jugador,Iz,De)).

punto(1060,1445).

accion(quieto,robot(propio,_,_),0,0).

accion(prueba1,robot(propio,_,pos(Xr,Yr,_Zr,_R)),0,0) :-
%	Xa1 is Xr-300,
%	Xa2 is Xr+300,
%	Ya1 is Yr-300,
%	Ya2 is Yr+300,
%        pelota_entre(Xa1,Ya1,Xa2,Ya2),
 	punto(X,Y),
	distancia((Xr,Yr),(X,Y),D),
	D<250.


accion(prueba1,Robot,Iz,De) :-
%	pelota_pred(X,Y,_Z),
	punto(X,Y),
	ir_a_posicion(Robot,X,Y,Iz,De).



% accion(prueba,R,125,-125) :-
% 	jugador_prev(R).


accion(prueba,robot(propio,_,pos(Xr,Yr,_Zr,_R)),125,-125) :-
	Xa1 is Xr-5,
	Xa2 is Xr+5,
	Ya1 is Yr-5,
	Ya2 is Yr+5,
        pelota_entre(Xa1,Ya1,Xa2,Ya2).


accion(prueba,Robot,Iz,De) :-
	pelota_pred(X,Y,_Z),
	ir_a_posicion(Robot,X,Y,Iz,De).


accion(puntor,Robot,Iz,De) :-
	pelota_pred(X,Y,_Z),
	apuntar_a_posicion(Robot,X,Y,Iz,De).

accion(arquero,Robot,Iz,De):-
	atajar(arquero,Robot,Iz,De).

% El rol de jugador
%si la pelota esta en el campo própio hace juego brusco
accion(jugador,Robot,Iz,De):-
	campo_propio(X1c,Y1c,X2c,Y2c),	pelota_entre(X1c,Y1c,X2c,Y2c),
	brusco(Robot,Iz,De).

%si la pelota esta en las bandas y el jugador esta entre los tres mas
%cercanos hace juego brusco
accion(jugador,Robot,Iz,De):-
	bandas(X1c,Y1c,X2c,Y2c),pelota_entre(X1c,Y1c,X2c,Y2c),
	mas_cercanos(Robot,3),
	%mas_cercanos(Robot,2),
	brusco(Robot,Iz,De).

%si la pelota esta en el área contraria y el jugador esta entre los tres
%mas cercanos realiza ataque_area
accion(jugador,Robot,Iz,De):-
	area_contraria(Xa1,Ya1,Xa2,Ya2),
	pelota_entre(Xa1,Ya1,Xa2,Ya2),
	mas_cercanos(Robot,3),
	ataque_area(Robot,Iz,De).

%si esta entre los tres más cercanos realiza ataque
accion(jugador,Robot,Iz,De):-
%	campo_contrario(X1c,Y1c,X2c,Y2c),
%	pelota_entre(X1c,Y1c,X2c,Y2c),
	mas_cercanos(Robot,3),
	ataque(Robot,Iz,De).

%si es el más cercano al centro de la cancha juega de pichero
accion(jugador,Robot,Iz,De):-
	medio_cancha(X,Y),
	mas_cercanos_lugar(Robot,1,X,Y),
	pichero(Robot,Iz,De).

%en caso que no se cumpla ninguna de las anteriores juega de líbero
accion(jugador,Robot,Iz,De):-
	libero(Robot,Iz,De).




% tratar el tema del not
no(A):-A,!,fail.
no(_).
%****************************************
%brusco es un comportamiento defensivo 
brusco(Robot,Iz,De):-
	%si no soy el mas cercano y esta atascado entonces desbloquear
	(no(mas_cercano(Robot)),
        atascado2(Robot);atascado(Robot)),!,
	desbloquear(Robot,Iz,De).

brusco(Robot,Iz,De):-	
	%si esta en la zona del arquero entonces ir a la línea del área grande
	%para no hacer penal
        area_chica_propia(Xa1,Ya1,Xa2,Ya2),
	robot_entre(Robot,Xa1-4,Ya1-4,Xa2+4,Ya2+4),
	pto_area(Robot,X,Y),
	ir_a_posicion_insegura(Robot,X,Y,Iz,De). 
	
brusco(Robot,Iz,De):-	
	%si la pelota entro en el área grande ir al eje area mas cercano 
	%para no hacer penal
        area_propia(Xa1,Ya1,Xa2,Ya2),
	pelota_entre(Xa1,Ya1,Xa2,Ya2),
	eje_area_mas_cercano(Robot,X,Y),
	ir_a_posicion_insegura(Robot,X,Y,Iz,De). 

brusco(Robot,Iz,De):-  
	%si está detraz de la pelota ir a la pelota
	pelota_pred(Xb,Yb,_),
	sentido(S),
	cancha(X1,Y1,X2,Y2),
	(S>0, %el caso de azul-
	(Xb>90,X is Xb-S*2.5; X is Xb),
	robot_entre(Robot,X,Y1,X2,Y2);
	 S<0, %amarillo
	(Xb<9,X is Xb-S*2.5; X is Xb),
	robot_entre(Robot,X1,Y1,X,Y2)), 
	ir_a_posicion_insegura(Robot,Xb,Yb,Iz,De).

brusco(Robot,Iz,De):-
	 % si no ir a posición defenciva
	centro_arco_propio(X,Y),
	acompannar_a_lleva_pelota_def(Robot,X,Y,Iz,De).


%********************en ataque
ataque_area(Robot,Iz,De):-
	%si no soy el mas cercano y esta atascado entonces desbloquear
	%(no(mas_cercano(Robot)),
        atascado(Robot),
	desbloquear(Robot,Iz,De).
ataque_area(Robot,Iz,De):-	
	%si esta en la zona del area chica contraria y no soy el mas cercano ir a la posición base
	area_chica_contraria(Xa1,Ya1,Xa2,Ya2),
	robot_entre(Robot,Xa1-2,Ya1-2,Xa2+2,Ya2+2),
	centro_arco_contrario(X,Y),
	(mas_cercano(Robot),
	 patear_pelota_a_posicion(Robot,X,Y,Iz,De);
	acompannar_a_lleva_pelota2(Robot,X,Y,Iz,De)).
ataque_area(Robot,Iz,De):-
	%si el robot esta entre la pelota y el arco contrario
	%salir del lugar porque complica el gol
	centro_arco_contrario(X,Y),
	pelota_pred(Xb,Yb,_),
	sentido(S),
	robot_entre(Robot,Xb+S*3,Yb,X,Y),
	acompannar_a_lleva_pelota(Robot,X,Y,Iz,De).
ataque_area(Robot,Iz,De):-
	%en caso contrario llevar pelota al arco contrario
	centro_arco_contrario(Xo,Yo),
	llevar_pelota_a_posicion(Robot,Xo,Yo,Iz,De).

%**************************************************************	
ataque(Robot,Iz,De):-
	%si no soy el mas cercano y esta atascado entonces desbloquear
	(no(mas_cercano(Robot)),
        atascado2(Robot);atascado(Robot)),!,
	%display('atascado '),display(Robot),nl,
	desbloquear(Robot,Iz,De).

ataque(Robot,Iz,De):-
	 % si esta entre la pelota y el arco contrario irse lejos
	%mas_cercano(Robot),
	centro_arco_contrario(X,Y),
	pelota_pred(Xb,Yb,_),
	sentido(S),
	robot_entre(Robot,Xb+S*3,Yb,X,Y),
	acompannar_a_lleva_pelota2(Robot,X,Y,Iz,De).

ataque(Robot,Iz,De):-
	% si esta entre la pelota y el arco contrario irse lejos
	mas_cercanos2(Robot),
	centro_arco_contrario(Xo,Yo),
	llevar_pelota_a_posicion(Robot,Xo,Yo,Iz,De).
	
ataque(Robot,Iz,De):-
	%en caso contrario ir hacia la pelota
        pelota_pred(Xb,Yb,_),
	ir_a_posicion_insegura(Robot,Xb,Yb,Iz,De).
	%patear_pelota_a_posicion(Robot,X,Y,Iz,De).

%**************************************************************
libero(Robot,Iz,De):-
	%si no soy el mas cercano y esta atascado entonces desbloquear
	(no(mas_cercano(Robot)),
        atascado2(Robot);atascado(Robot)),!,
	%display('atascado '),display(Robot),nl,
	desbloquear(Robot,Iz,De).

libero(Robot,Iz,De):-
	%si esta en la zona del rol entonces llevar la pelota al objetivo
	 centro_arco_contrario(X,Y),
	 acompannar_a_lleva_pelota2(Robot,X,Y,Iz,De).

%********************************************************************

pichero(Robot,Iz,De):-
	%si no soy el mas cercano y esta atascado entonces desbloquear
	%(no(mas_cercano(Robot)),
        atascado(Robot),
	desbloquear(Robot,Iz,De).
%pichero(Robot,Iz,De):-
	%si esta en la zona del rol entonces llevar la pelota al objetivo
	%area_contraria(Xa1,Ya1,Xa2,Ya2),
	%pelota_entre(Xa1-2,Ya1+4,Xa2+2,Ya2-4),
	%mas_cercano(Robot),
	%centro_arco_contrario(X,Y),patear_pelota_a_posicion(Robot,X,Y,Iz,De).
%pichero(Robot,Iz,De):-
	%si esta en la zona del rol entonces llevar la pelota al objetivo
	%area1('pichero',Xa1,Ya1,Xa2,Ya2),
	%pelota_entre(Xa1,Ya1,Xa2,Ya2),
	%centro_arco_contrario(X,Y),llevar_pelota_a_posicion(Robot,X,Y,Iz,De).
pichero(Robot,Iz,De):-
	%si esta en dirección al area chica propia entonces esperar la pelota en el area
	area1('pichero2',Xa1,Ya1,Xa2,Ya2),
	pelota_en_direccion_a_zona(Xa1,Ya1,Xa2,Ya2,X,Y),
	(robot_entre(Robot,X-2,Y-2,X+2,Y+2),Iz is 0,De is 0;
	sentido(S),
	ir_a_posicion_precisa(Robot,X,Y+S*2,Iz,De)).

pichero(Robot,Iz,De):-
	%caso contrario el comportamiento defecto es ir a la posición base del rol
	pelota_pred(Xb,_Yb,_),
	area1('pichero2',Xa1,Ya1,Xa2,Ya2),
	sentido(S),
	(Xb>Xa1,Xb<Xa2, X is Xb+S*2; Xb<Xa1, X is Xa1+S*2; X is Xa2+S*2),
	Y is (Ya2-Ya1)/2+Ya1,
	(robot_entre(Robot,X-2,Y-2,X+2,Y+2),Iz is 0,De is 0;
	ir_a_posicion_precisa(Robot,X,Y,Iz,De)). %posición baseposición base,

%********************************************************************

atajar(_Rol,Robot,Iz,De):-
	%si esta atascado entonces desbloquear
	atascado(Robot), %situación
	desbloquear(Robot,Iz,De). %comportamiento

atajar(Rol,Robot,Iz,De):-
	%si esta en la zona del rol entonces llevar la pelota al objetivo
	area_propia(Xa1,Ya1,Xa2,Ya2),
%	sentido(S),
	pelota_entre(Xa1,Ya1,Xa2,Ya2),
	mas_cercano(Robot),
	objetivo(Rol,X,Y),patear_pelota_a_posicion(Robot,X,Y,Iz,De).

atajar(_Rol,Robot,Iz,De):-
	%si esta en dirección al area chica propia entonces esperar la pelota en el area
	area_chica_propia(Xa1,Ya1,Xa2,Ya2),
	pelota_en_direccion_a_zona(Xa1,Ya1,Xa2,Ya2-3,X,Y),
	(robot_entre(Robot,X-2,Y-2,X+2,Y+2),Iz is 0,De is 0;
	ir_a_posicion_precisa(Robot,X,Y,Iz,De)).

atajar(Rol,Robot,Iz,De):-
	%caso contrario el comportamiento defecto es ir a la posición base del rol
	pos_base(Rol,X,Y),
	(robot_entre(Robot,X-2,Y-2,X+2,Y+2),Iz is 0,De is 0;
	ir_a_posicion_precisa(Robot,X,Y,Iz,De)). %po25sición baseposición base,


%***********************************************************datos para estrategia
area1('pichero2',12.7,39,87.5,44).
area1('pichero',17,33,82,51).
bandas(6,6,93.5,9).
bandas(6,74.5,93.5,78).
eje_area_mas_cercano(robot(_,_,pos(Xr,Yr,_,_)),X,Y):-
	sentido(S),
	S<0, %amarillo
	area_propia(_Xa1,Ya1,Xa2,Ya2),	
	(sqrt((Xa2-Xr)**2+(Ya1-Yr)**2)<sqrt((Xa2-Xr)**2+(Ya2-Yr)**2),
	Y is Ya1;Y is Ya2),
	X is Xa2.
eje_area_mas_cercano(robot(_,_,pos(Xr,Yr,_,_)),X,Y):-
	sentido(S),S>0,
	area_propia(Xa1,Ya1,_Xa2,Ya2),	
	(sqrt((Xa1-Xr)**2+(Ya1-Yr)**2)<sqrt((Xa1-Xr)**2+(Ya2-Yr)**2),
	Y is Ya1;Y is Ya2),
	X is Xa1.
%pto_area(robot(_,_,pos(_Xr,Y,_,_)),79,41).
pto_area(robot(_,_,pos(_Xr,Yr,_,_)),X,Y):-
	sentido(S),
	S<0, %amarillo
	area_propia(_Xa1,_Ya1,Xa2,_Ya2),	
	Y is Yr+1,
	X is Xa2.
%linea_area_mas_cercano(robot(_,_,pos(_Xr,Yr,_,_)),79.6,Yr).
pto_area(robot(_,_,pos(_Xr,Yr,_,_)),X,Y):-
	sentido(S),
	S>0,
	area_propia(Xa1,_Ya1,_Xa2,_Ya2),	
	Y is Yr+1,
	X is Xa1.


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
%posiciones de los roles de la estrategia

pos_base(arquero,X,Y):-
	pelota_pred(_Xb,Yb,_),
	area_chica_propia(X1,Y1,X2,Y2),
	largo_cancha(L),
	arco_propio(Xp1,_Yp1,_Xp2,_Yp2),
	arco_contrario(Xc1,_Yc1,_Xc2,_Yc2),
	Signo is (Xp1 - Xc1)/L,
 	(Signo<0,X is X1; X is X2),
	(Y1<Yb,Yb<Y2,Y is Yb;
	Y1>Yb,Y is Y1;
	Y is Y2).
pos_base(arquero,X,Y):-	centro_arco_propio(X,Y).
 
pos_base('delantero_derecho',X,Y):-
	largo_cancha(L),
	ancho_cancha(A),
	medio_cancha(_Xmc,Ymc),
	arco_propio(Xp1,_Yp1,_Xp2,_Yp2),
	arco_contrario(Xc1,_Yc1,_Xc2,_Yc2),
	Signo is (Xp1 - Xc1)/L,
	X is Xc1 + Signo*L/4,
	Y is Ymc + Signo*A/4.
pos_base('delantero_izquierdo',X,Y):-
	largo_cancha(L),
	ancho_cancha(A),
	medio_cancha(_Xmc,Ymc),
	arco_propio(Xp1,_Yp1,_Xp2,_Yp2),
	arco_contrario(Xc1,_Yc1,_Xc2,_Yc2),
	Signo is (Xp1 - Xc1)/L,
	X is Xc1 + Signo*L/4,
	Y is Ymc - Signo*A/4.
pos_base('defensor_derecho',X,Y):-
	largo_cancha(L),
	ancho_cancha(A),
	medio_cancha(_Xmc,Ymc),
	arco_propio(Xp1,_Yp1,_Xp2,_Yp2),
	arco_contrario(Xc1,_Yc1,_Xc2,_Yc2),
	Signo is (Xp1 - Xc1)/L,
	X is Xp1 - Signo*L/4,
	Y is Ymc + Signo*A/4.
pos_base('defensor_izquierdo',X,Y):-
	largo_cancha(L),
	ancho_cancha(A),
	medio_cancha(_Xmc,Ymc),
	arco_propio(Xp1,_Yp1,_Xp2,_Yp2),
	arco_contrario(Xc1,_Yc1,_Xc2,_Yc2),
	Signo is (Xp1 - Xc1)/L,
	X is Xp1 - Signo*L/4,
	Y is Ymc - Signo*A/4.
area(arquero,X1,Y1,X2,Y2):-
	area_propia(X1,Y1,X2,Y2).
area('delantero_derecho',X1,Y1,X2,Y2):-
	pos_base('delantero_derecho',X,Y),
	largo_cancha(L),
	ancho_cancha(A),
	X1 is X - L/4,
	Y1 is Y - A/4,
	X2 is X + L/4,
	Y2 is Y + A/4.
area('delantero_izquierdo',X1,Y1,X2,Y2):-
	pos_base('delantero_izquierdo',X,Y),
	largo_cancha(L),
	ancho_cancha(A),
	X1 is X - L/4,
	Y1 is Y - A/4,
	X2 is X + L/4,
	Y2 is Y + A/4.
area('defensor_derecho',X1,Y1,X2,Y2):-
	pos_base('defensor_derecho',X,Y),
	largo_cancha(L),
	ancho_cancha(A),
	X1 is X - L/4,
	Y1 is Y - A/4,
	X2 is X + L/4,
	Y2 is Y + A/4.
area('defensor_izquierdo',X1,Y1,X2,Y2):-
	pos_base('defensor_izquierdo',X,Y),
	largo_cancha(L),
	ancho_cancha(A),
	X1 is X - L/4,
	Y1 is Y - A/4,
	X2 is X + L/4,
	Y2 is Y + A/4.
 
objetivo(_Rol,X,Y):- 
	centro_arco_contrario(X,Y).

