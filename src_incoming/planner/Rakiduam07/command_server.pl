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
%:-module(command_server,[iniciarCS/3,sendCS/2],[assertions]).
:-module(command_server,[transform/4,iniciarCS/3,sendCS/2],[assertions]).
:- use_module(library(sockets)).
:- use_module(configuration).
:- use_module(library(lists)).

:- comment(title, "Modulo Interfaz con el servidor de comandos").

:- comment(author, "Pablo Kogan").

:- comment(module, "Este módulo define las primitivas para inicializar el servidor de comandos y enviar los paquetes a este.").

:- comment(doinclude,iniciarCS/3).
:- comment(doinclude,sendCS/2).

:- pred iniciarCS(+HostCS, +PortCS, -StreamCS) ::
        atom * int *  stream
 # "Retorna en @var{StreamCS} un stream conectado a un socket udp en @var{HostCS}:@var{PortCS}.".

%iniciarCS(+HostCS,+PortCS,-StreamCS)
%inicializa un socket udp conectandolo contra un HostCS:PortCS

iniciarCS(HostCS,PortCS,StreamCS):-
        connect_to_socket_type(HostCS, PortCS,dgram, StreamCS), 
        display('Conectado a Command Server en Puerto '),
        display(PortCS),
        display(', Host '),
        display(HostCS),
        display('.'),
        nl.

:- pred sendCS(+StreamCS, +ListaVelocidades) ::
        stream * string
 # "Envia por el socket udp conectado a @var{StreamCS} la lista @var{ListaVelocidades} que representa los comandos para todos los robots.".
%recibirCS(?StreamVS,+ListadeVelocidades)
%Espera recibir los datos del ambiente y los transforma a estructuras de prolog

transform(N,[],L,L):-
	get_numplayers(M),N is M + 1.

transform(N,[Vi,Vd|Rest],Line,LTs) :-
	N1 is N+1,
	get_player(Player,N,propio),
	atom_codes(Player,P_str),
	number_codes(Vi,Vi_str),
	number_codes(Vd,Vd_str),
	list_concat([P_str,",",Vi_str,",",Vd_str,"\n"],Tail),
	dlist(Tail,Line,Ntail),
	transform(N1,Rest,Ntail,LTs).
	

sendCS(StreamCS,ListaVelocidades):-
	get_environment(real),
        transform(1,ListaVelocidades,Message,[]),
 	socket_send(StreamCS,Message),!.

sendCS(StreamCS,ListaVelocidades):-
	get_environment(simulado),
	listavel(ListaVelocidades,Lista_Str),
 	socket_send(StreamCS,Lista_Str),!.

%***************************funciones de parser out
listavel([],[10]).
listavel([Numero|Resto],Lista_Str):-
	listavel(Resto,Resto_Str),
	number_codes(Numero,Numero_Str),
	append2(Numero_Str,Resto_Str,Lista_Str).
append2([],L2,[10|L2]).
append2([X|C],L2,[X|L3]):-
	append2(C,L2,L3).

	
	
