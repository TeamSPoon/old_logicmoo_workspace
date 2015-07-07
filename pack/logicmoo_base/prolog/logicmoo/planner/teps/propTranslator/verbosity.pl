:- dynamic is_verbose.

%% verbosity

%% un comment the following line to make
%% the program verbose by default

%is_verbose.

verbose :- assert(is_verbose).
noverbose :- retractall(is_verbose).

dwritef(X,Y) :-
	(is_verbose -> writef(X,Y);true).
