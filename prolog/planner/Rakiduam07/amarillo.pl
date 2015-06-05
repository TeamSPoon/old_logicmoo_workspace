%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%     Copyright 2007 Pablo Kogan, Guillermo Torres, Mario Moya, Claudio Vaucheret
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



:- module(amarillo, [main/0], [assertions]).
%% modulo azul
%% módulo principal de la estrategia del equipo azul
:- use_module(library(concurrency)).

:- use_module(sim_video_server_linux).
%% módulo de conexión con el servidor de video
:- use_module(command_server).
%% módulo de conexión con el servidor de comandos
:- use_module(estrategia).
%% módulo para gestión de logs
:- use_module(logger).
%% modulo para gestion de datos de ambiente
%:- use_module(ambiente).  
:- use_module(configuration,[get_video_port/1,get_video_host/1,get_command_port/1,get_command_host/1]).

:- comment(title, "Equipo de Futbol con Robots").

:- comment(author, "Pablo Kogan").

:- comment(module, "Este módulo representa es el que tiene el predicado main que mantiene la comuncación con el servidor de video y de comandos.  Otra de las funciones posibles es habilitar un logger para llevar los log del juego.").

%:- comment(doinclude,main/0).
%:- comment(main/0,"El predicado main inicializa el servidor de comandos y el servidor de video e inicializa el juego.").

%% la función main inicializa el servidor de comandos y el servidor de video
%% e inicializa el juego

:- pred  main # "El predicado main inicializa el servidor de comandos y el servidor de video e inicializa el juego.".
main:-
	get_video_port(PuertoV),
	get_video_host(HostV),
	iniciarVS(HostV,PuertoV,VideoServer), %'192.168.0.3',6363,VideoServer), %
	get_command_port(PuertoC),
	get_command_host(HostC),
	iniciarCS(HostC,PuertoC,CommandServer), %'192.168.0.3',6364,CommandServer), %
	iniciar('amarillo'),
        % create_threads(10),
	juego(VideoServer,CommandServer).   %udp claudio
%	wait_for_connections(VideoServer).
	



%% l función juego repite hasta que se apriete ^C
%% con la función recibirVS recibe en Estado el estado actual del ambiente
%% con la función estrategia resulve la acción a realizar en base al Estado actual
 %% con la función sendCS se envía la acción al servidor de comandos
juego(VideoServer,CommandServer):-
	iniciarLog('estrategiaAmarillo.log',Archivo), 
	repeat,
	   recibirVS(VideoServer,Estado),
	   display(Estado),nl,   
	   estrategia(Estado,ListaVelocidades),
	   display(ListaVelocidades),nl,
	   escribirLog(Archivo,Estado,ListaVelocidades),
	   sendCS(CommandServer,ListaVelocidades),
	   %display(ListaVelocidades),
	fail. 

%manejo de thread

:- concurrent connection/1.

% wait_for_connections(Socket):-
%         repeat,
%         nuevo_juego(Socket, Stream),
% %        socket_buffering(Stream, read, _Old, unbuf),
%         assertz_fact(connection(Stream)),
%         fail.

create_threads(0).
create_threads(N):-
        N > 0,
        eng_call(handle_connection, create, create),
        N1 is N - 1,
        create_threads(N1).

handle_connection:-
        retract_fact(connection(Stream)),
        juego(Stream,Stream),
        fail.
