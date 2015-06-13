:- use_module(socket_udp).
:- use_module(library(strings)).
fa:- socket_udp(2005,Y),
	display(Y),
	recv_udp(Y,10,Msg),
	display('mensaje en prolog '),
	write_string(Msg),nl,
	recv_udp(Y,10,Msg1),
	display('mensaje en prolog '),
	write_string(Msg1),nl,
	close_udp(Y).


