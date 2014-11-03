:- module(socket_udp,[socket_udp/2,recv_udp/3,close_udp/1,test/0],[foreign_interface]). 
:-use_module(library(strings)).

:- true pred socket_c(in(X), go(Y)):: int * int + foreign # 
"Given a number @var{X}, it is unified with @var{Y} by using the most
specific internal representation (short integer, float, or long
integer).  @var{How} returns how the conversion was done.
It behaves unpredictably if @var{X} is not a number.".

:- true pred recv_c(in(X), in(Z), go(Y)):: int * int * string + (foreign,returns(Y),do_not_free(Y)).
:- true pred close_c(in(X)):: int + foreign.

:- use_foreign_source(socket_udp_c).

:- use_foreign_library('LINUXi86', ['c']).

socket_udp(A, B):- 
        socket_c(A, B).
 
recv_udp(A,B,C):-
	recv_c(A,B,C).

close_udp(A):-
	close_c(A).

test:-
	socket_udp(6363,Socket),
	display(Socket),
	recv_udp(Socket,10,Msg),
	write_string(Msg),
	% recv_udp(Socket,45,Msg1),
	% write_string(Msg1),
	% recv_udp(Socket,5,Msg3),
	% write_string(Msg3),
	close_udp(Socket).

%bugs no se pueden recibir paquetes de largo 4 y 5 para
%para salvar este bug se reciben paquetes de 6 como mínimo
