:- module(principal, [main/0], [assertions]).

%% modulo azul
%% módulo principal de la estrategia del equipo azul
:- use_module(library(concurrency)).

:- use_module(sim_video_server_tcp).
%% módulo de conexión con el servidor de video
:- use_module(sim_command_server).
%% módulo de conexión con el servidor de comandos
:- use_module(estrategia).
%% módulo para gestión de logs
:- use_module(logger).
%% modulo para gestion de datos de ambiente
%:- use_module(ambiente).  
:- comment(title, "Equipo de Futbol con Robots").

:- comment(author, "Pablo Kogan").

:- comment(module, "Este módulo representa es el que tiene el predicado main que mantiene la comuncación con el servidor de video y de comandos.  Otra de las funciones posibles es habilitar un logger para llevar los log del juego.").

%:- comment(doinclude,main/0).
%:- comment(main/0,"El predicado main inicializa el servidor de comandos y el servidor de video e inicializa el juego.").

%% la función main inicializa el servidor de comandos y el servidor de video
%% e inicializa el juego

:- pred  main # "El predicado main inicializa el servidor de comandos y el servidor de video e inicializa el juego.".
main:-
	iniciarVS('localhost',6363,VideoServer), %'192.168.0.3',6363,VideoServer), %

%	iniciarCS('localhost',6364,CommandServer), %'192.168.0.3',6364,CommandServer), %
	iniciar('azul'),
        create_threads(10),
	wait_for_connections(VideoServer).
	 
%% la función juego repite hasta que se apriete ^C
%% con la función recibirVS recibe en Estado el estado actual del ambiente
%% con la función estrategia resulve la acción a realizar en base al Estado actual
 %% con la función sendCS se envía la acción al servidor de comandos
juego(VideoServer,CommandServer):-
	%iniciarLog('estrategia.log',Archivo), 
	repeat,
	   recibirVS(VideoServer,Estado),
	   %escribirLog(Archivo,Estado),
	   estrategia(Estado,ListaVelocidades),
	   sendCS(CommandServer,ListaVelocidades),
	   %display(ListaVelocidades),
	fail. 

%manejo de thread

:- concurrent connection/1.

wait_for_connections(Socket):-
        repeat,
        nuevo_juego(Socket, Stream),
%        socket_buffering(Stream, read, _Old, unbuf),
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
