:- module(read_write,[main/0],_).
:- use_module(library(strings)).
:- use_module(library(lists)).

% Read the PDDL input file and write the STRIPS output file.

main :- 
	open('MyPDDL.pddl',read,Str), % Open the file for reading. The Stream of all the file is unified in Str.
	open('MySTRIPS.pl',write,Str1), % Open the file for writing. The Stream to write is unified in Srt1
	read_pddl(Str,X),
	write_module(Str1),
	write_pddl(Str1,X), 
	close(Str1),
	close(Str).


read_pddl(Str,[Line|Tail]):- % This predicate generates a list of the lines included in the Stream.
        get_line(Str,Line),
	Line \== end_of_file,
	read_pddl(Str,Tail).

read_pddl(_,[]).


%:- module('blocks-strips',[achieves/2,preconditions/2,deletes/2,holds/2,dif/2],[]).

write_module(Str) :-
	write_string(Str,":- module(name_domain,[exported_predicates],[imported_predicates])."),
	nl(Str),
	nl(Str).


write_pddl(Str,[X|T]):- 
	            write_string("Linea: "),
                    write_string(X), % This line is prompted.
		    nl,
                %    write_string(Str,"Linea: "),
                    write_string(Str,X), % This line is wrote in MySTRIPS.pl.
		    nl(Str),
		    write_pddl(Str,T).
write_pddl(_,[]).
