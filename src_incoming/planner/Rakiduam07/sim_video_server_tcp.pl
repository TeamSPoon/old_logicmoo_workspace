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
:-module(sim_video_server_tcp,[iniciarVS/3,recibirVS/2,nuevo_juego/2,test/0],[assertions]).
:- use_module(library(sockets)).
:- use_module(library(strings)).
:- comment(title, "Modulo Interfaz con el servidor de video").

:- comment(author, "Pablo Kogan").

:- comment(module, "Este módulo define las primitivas para inicializar el servidor de video y recibir los paquetes de este.").

:- comment(doinclude,iniciarVS/3).
:- comment(doinclude,recibirVS/2).

test:-
	iniciarVS(_,6363,Stream),
	repeat,
	      recibirVS(Stream,Lista),
	      display('acepto el stream'),
	      display(Lista),nl,
%              socket_recv_code(Stream,Mensaje,512),
%	      display(Mensaje),
%              socket_recv_code(Stream,Mensaje2,512),
%	      display(Mensaje2),
	      fail.

:- pred iniciarCS(+HostVS, +PortVS, -StreamVS) ::
        atom * int *  stream
 # "Retorna en @var{StreamCS} un stream conectado a un socket udp en @var{HostCS}:@var{PortCS}.".


%iniciarVS(+HostVS,+PortVS,-StreamVS)
%inicializa un socket udp conectandolo contra un HostVS:PortVS
%manda el mensaje conexión y espera la respuesta para asegurarse de la conexión
iniciarVS(HostVS,PortVS,Socket):-
        %connect_to_socket_type(HostVS, PortVS,dgram, StreamVS), 
        %display(StreamVS,'coneccion_'),
        %socket_recv_code(StreamVS,Mensaje,10),
        %write_string(Mensaje),
	Queue is 5,
        bind_socket(PortVS, Queue, Socket),  %% Already in "listen" state
%        socket_accept(Socket, StreamVS),	
        display('Escuchando a Simulador en Puerto '),
        display(PortVS),
        display(', Host '),
        display(HostVS),
        display('.'),
        nl.
:- pred nuevo_juego(+SocketVS, -StreamVS) ::
        atom *  stream
 # "Retorna en @var{StreamCS} un stream conectado a un socket tcp en @var{SocketVS}.".


%iniciarVS(+HostVS,+PortVS,-StreamVS)
%inicializa un socket udp conectandolo contra un HostVS:PortVS
%manda el mensaje conexión y espera la respuesta para asegurarse de la conexión
nuevo_juego(Socket,StreamVS):-
        socket_accept(Socket, StreamVS),
        display('Acepto nuevo juego '),
        display(StreamVS),
        display('.'),
        nl.

:- pred recibirVS(+StreamVS, +EstadoAmbiente) ::
        stream * list
 # "Recibe por el socket udp conectado a @var{StreamVS} los datos del ambiente y los trasforma a estructuras de prolog en una lista @var{EstadoAmbiente}.".
%recibirVS(?StreamVS,-[Pelota,PelotaAnterior,Jugadores,Estado])
%Espera recibir los datos del ambiente y los transforma a estructuras de prolog
 
recibirVS(StreamVS,[Pelota,Jugadores,Estado]):-
        %socket_recv_code(StreamVS,Mensaje,512),
	%display('comienza a recibir el stream'),
        !,socket_recv_code(StreamVS,Mensaje,512),	
	%display(' Mensaje VS '),nl, 	display(Mensaje),nl,
	%display('********************* string ********************'),	write_string(Mensaje),nl,
	posxyz(Mensaje,Pelota,MensajeR),
	%display('Pelota '),write_pos(Pelota),nl,
	%posxyz(MensajeS,PelotaAnterior,MensajeR), no se utiliza pelota anterior en esta versión
	%display('Pelota Anterior '),write_pos(PelotaAnterior),nl,
	jugadores(MensajeR,Jugadores,MensajeP),
	%display('Jugadores'),nl,write_jugadores(Jugadores),
%	next_number(MensajeP,Estado,MensajeO),
%	next_number(MensajeO,TienePelota,MensajeN),
	estado(MensajeP,Estado,_MensajeN),
	%display('Estado '),nl,write_estado(Estado),nl,
	!.
	%display('Resto'),
	%write_string(MensajeN),nl
	



%funciones de parser in
next_line([],[],[]).
next_line([32|C],[],C).
next_line([10|C],[],C).
next_line([X|C],[X|CL],CR):-
	next_line(C,CL,CR).

%Resuelve el problema de números negativos
numero_string(X,[45|R]):-
	number_codes(X1,R),
	X is -X1.
numero_string(X,R):-
	number_codes(X,R).
next_number([],0,[]).
next_number(Mensaje,X,MensajeS):-
	next_line(Mensaje,StrX,MensajeS),
	%display(StrX),write_string(StrX),nl,%hasta StrXesta bien
	numero_string(X,StrX), %error en la función number_codes para números negativos
	%display(X),
	!.
posxyzr(Stream,pos(X,Y,Z,R),StreamQ):-
	next_number(Stream, X ,StreamY),
        next_number(StreamY, Y,StreamZ),
        next_number(StreamZ, Z,StreamR),
        next_number(StreamR, R,StreamQ).
equipos(Stream,'contrario',[robot('contrario',5,X)],5,StreamS):-
	posxyzr(Stream,X,StreamS).
equipos(Stream,'propio',[robot('propio',5,X)|L],5,StreamS):-
	posxyzr(Stream,X,StreamS1),
	equipos(StreamS1,'contrario',L,1,StreamS). 
equipos(Stream,Equipo,[robot(Equipo,N,X)|L],N,StreamS):-
	posxyzr(Stream,X,StreamS1),
	N2 is N+1,
	equipos(StreamS1,Equipo,L,N2,StreamS).
 jugadores(Stream,ListaJugadores,StreamS):-
	equipos(Stream,'propio',ListaJugadores,1,StreamS).

posxyz(Stream,pos(X,Y,Z),StreamR):-
	next_number(Stream, X,StreamY),
        next_number(StreamY, Y,StreamZ),
        next_number(StreamZ, Z,StreamR).
write_pos(pos(X,Y,Z)):-
	display('('),display(X),display(','),display(Y),display(','),display(Z),display(')').
write_jugadores([]):-nl.
write_jugadores([robot(E,N,pos(X,Y,Z,R))|C]):-
	display('Robot '),display(E),display(' '),display(N), display('('),display(X),display(','),display(Y),display(','),display(Z),display(','),display(R),display(')'),
	write_jugadores(C).
estado(Stream,estado(Estado,TienePelota),StreamQ):-
	next_number(Stream, Estado ,StreamY),
        next_number(StreamY, TienePelota,StreamQ).
write_estado(estado(1,Y)):-
	display('FREE_BALL '),
	write_tienepelota(Y).
write_estado(estado(2,Y)):-
	display('PLACE_KICK '),
	write_tienepelota(Y).
write_estado(estado(3,Y)):-
	display('PENALTY_KICK '),
	write_tienepelota(Y).
write_estado(estado(4,Y)):-
	display('FREE_KICK '),
	write_tienepelota(Y).
write_estado(estado(5,Y)):-
	display('GOAL_KICK '),
	write_tienepelota(Y).
write_estado(estado(_,Y)):-
	display('INDEFINIDO '),
	write_tienepelota(Y).
write_tienepelota(0):-
	display('ANYONES_BALL').
write_tienepelota(1):-
	display('BLUE_BALL').
write_tienepelota(2):-
	display('YELLOW_BALL').
write_tienepelota(_):-
	display('INDEFINIDO').

%// gameState
%const long FREE_BALL = 1;
%const long PLACE_KICK = 2;
%const long PENALTY_KICK = 3;
%const long FREE_KICK = 4;
%const long GOAL_KICK = 5;
%
%// whosBall
%const long ANYONES_BALL = 0;
%const long BLUE_BALL = 1;
%const long YELLOW_BALL = 2;


	
	