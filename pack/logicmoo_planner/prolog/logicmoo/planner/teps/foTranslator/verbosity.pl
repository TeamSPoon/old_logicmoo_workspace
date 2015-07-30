:- dynamic is_verbose.

%% verbosity

%is_verbose.
verbose :- assert(is_verbose).
noverbose :- retractall(is_verbose).

dwritef(X,Y) :-
	(is_verbose -> writef(X,Y);true).
