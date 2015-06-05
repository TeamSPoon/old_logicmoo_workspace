:-use_module(library(sockets)).
main:-
	connect_to_socket_type('localhost',2005,dgram, Stream),
	display(Stream,'12345').
