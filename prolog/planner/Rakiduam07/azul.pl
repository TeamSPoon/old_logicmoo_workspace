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
:- module(azul, [main/0], [assertions]).

%% modulo azul
%% módulo principal de la estrategia del equipo azul
:- use_module(library(concurrency)).

:- use_module(sim_video_server_tcp,[iniciarVS/3,recibirVS/2,nuevo_juego/2]).
%% módulo de conexión con el servidor de video
:- use_module(sim_command_server, [sendCS/2]).
%% módulo de conexión con el servidor de comandos
:- use_module(estrategia,[iniciar/1,estrategia/2]).
%% módulo para gestión de logs
:- use_module(logger,[iniciarLog/2, escribirLog/3]).
%% modulo para gestion de datos de ambiente
%:- use_module(ambiente).  
:- use_module(configuration,[get_video_port/1,get_video_host/1]).

:- use_module(perceptions,[get_perceptions/1]).

:- use_module(actions,[execute_action/3,insert_action_to_strategy/5]).

:- use_module('continuos/continuous_pop',[initialplan/1,continuouspop/2,perception/1,action/1]).
:- use_module('continuos/goalgen',[find_depth_bound/1]).
:- comment(title, "Equipo de Futbol con Robots").

:- comment(author, "Pablo Kogan").

:- comment(module, "Este módulo representa es el que tiene el predicado main que mantiene la comuncación con el servidor de video y de comandos.  Otra de las funciones posibles es habilitar un logger para llevar los log del juego.").

%:- comment(doinclude,main/0).
%:- comment(main/0,"El predicado main inicializa el servidor de comandos y el servidor de video e inicializa el juego.").

%% la función main inicializa el servidor de comandos y el servidor de video
%% e inicializa el juego

%% archivo de configuración, usamos xml

:- pred  main # "El predicado main inicializa el servidor de comandos y el servidor de video e inicializa el juego.".
main:-
	get_video_port(Puerto),
	get_video_host(Host),
	iniciarVS(Host,Puerto,VideoServer), %'192.168.0.3',6363,VideoServer), %

%	iniciarCS('localhost',6364,CommandServer), %'192.168.0.3',6364,CommandServer), %
	iniciar(azul),
%         create_threads(10),
% 	wait_for_connections(VideoServer).

	%para probar sin threads.
	nuevo_juego(VideoServer, Stream),
	juego(Stream,Stream).
	

%% la función juego repite hasta que se apriete ^C
%% con la función recibirVS recibe en Estado el estado actual del ambiente
%% con la función estrategia resulve la acción a realizar en base al Estado actual
%% con la función sendCS se envía la acción al servidor de comandos
juego(VideoServer,CommandServer):-
	iniciarLog('estrategia.log',Archivo), 
%	ejecucion(VideoServer,CommandServer).
 	initialplan(Plan),
 	find_depth_bound(DB),
	eng_call(continuouspop(Plan,DB),create,create),
	%aqui comienza el controlador
	repeat,
	   recibirVS(VideoServer,Estado),
	   estrategia(Estado,ListaVelocidadesAux),
	   getPlannedAction(Action,Lv,Rv),
	   (Action == noop,
	    ListaVelocidadesAux = ListaVelocidades
	   ;
	    get_planned_player(N),
	    insert_action_to_strategy(Lv,Rv,N,ListaVelocidadesAux,ListaVelocidades)
	   ),
%	   estrategia(Estado,ListaVelocidades),
	   escribirLog(Archivo,Estado,ListaVelocidades),
	   sendCS(CommandServer,ListaVelocidades),
	   %display(ListaVelocidades),
	fail. 



% juego(VideoServer,CommandServer):-
% 	iniciarLog('estrategia.log',Archivo), 
% %	ejecucion(VideoServer,CommandServer).
%  	initialplan(Plan),
%  	find_depth_bound(DB),
% 	%eng_call(controlador,create,create),
% 	eng_call(continuouspop(Plan,DB),create,create),
% 	%aqui comienza el controlador
% 	repeat,
% 	   recibirVS(VideoServer,Estado),
% 	   estrategia(Estado,ListaVelocidadesAux),
% 	   getPlannedAction(Action,Lv,Rv),
% 	   (Action == noop,
% 	    ListaVelocidadesAux = ListaVelocidades
% 	   ;
% 	    get_planned_player(N),
% 	    insert_action_to_strategy(Lv,Rv,N,ListaVelocidadesAux,ListaVelocidades)
% 	   ),
% %	   estrategia(Estado,ListaVelocidades),
% 	   escribirLog(Archivo,Estado,ListaVelocidades),
% 	   sendCS(CommandServer,ListaVelocidades),
% 	   %display(ListaVelocidades),
% 	fail. 




get_planned_player(3).

getPlannedAction(Act,Lv,Rv):- 
	get_perceptions(Percepts),
	asserta_fact(perception(Percepts)),
	display('Controlador asserta : '),display(Percepts),nl,
	display('Controlador ESPERANDO ACCION : OPEN'),nl,
	retract_fact(action(Act)),
	display('Controlador ESPERANDO ACCION : CLOSE'),nl,
        %retract_fact_nb(action(Act)),
	execute_action(Act,Lv,Rv),!.
%	display(Act), nl,!.

%getPlannedAction(noop,0,0).

:- concurrent connection/1.

wait_for_connections(Socket):-
        repeat,
           nuevo_juego(Socket, Stream),
%          socket_buffering(Stream, read, _Old, unbuf),
           assertz_fact(connection(Stream)),
        fail.

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
