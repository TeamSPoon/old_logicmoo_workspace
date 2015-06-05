:-module(sim_video_server_linux,[iniciarVS/3,recibirVS/2],[assertions]).
%para windows:- use_module(library(sockets)).
:- use_module('socket_udp/socket_udp'). %para usar servidor udp
:- use_module(library(strings)).
:- use_module(library(lists)).
:- use_module(video_parser,[analyze/4]).

:- comment(title, "Modulo Interfaz con el servidor de video").

:- comment(author, "Pablo Kogan").

:- comment(module, "Este módulo define las primitivas para inicializar el servidor de video y recibir los paquetes de este.").

:- comment(doinclude,iniciarVS/3).
:- comment(doinclude,recibirVS/2).

:- pred iniciarCS(+HostVS, +PortVS, -StreamVS) ::
        atom * int *  stream
 # "Retorna en @var{StreamCS} un stream conectado a un socket udp en @var{HostCS}:@var{PortCS}.".

%iniciarVS(+HostVS,+PortVS,-StreamVS)
%inicializa un socket udp conectandolo contra un HostVS:PortVS
%manda el mensaje conexión y espera la respuesta para asegurarse de la conexión
iniciarVS(_HostVS,PortVS,SocketVS):-
%para windows        connect_to_socket_type(HostVS, PortVS,dgram, StreamVS), 
%para windows        display(StreamVS,'coneccion_'),
%para windows        socket_recv_code(StreamVS,Mensaje,10),
%para windows        write_string(Mensaje),
        socket_udp(PortVS, SocketVS), %para linux
        display('Conectado a Video Server en Puerto '),
        display(PortVS),
%para windows        display(', Host '),
%para windows        display(HostVS),
        display('.'),
        nl.



:- pred recibirVS(+StreamVS, +EstadoAmbiente) ::
        stream * list
 # "Recibe por el socket udp conectado a @var{StreamVS} los datos del ambiente y los trasforma a estructuras de prolog en una lista @var{EstadoAmbiente}.".
%recibirVS(?StreamVS,-[Pelota,PelotaAnterior,Jugadores,Estado])
%Espera recibir los datos del ambiente y los transforma a estructuras de prolog

recibirVS(StreamVS,[Pelota,Jugadores,Estado]):-
%para windows        socket_recv_code(StreamVS,Mensaje,512),
	recv_udp(StreamVS,512,Mensaje),
	analyze(Mensaje,Pelota,Jugadores,Estado).
	


