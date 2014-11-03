:- package(pddltostrips). 
:- load_compilation_module('pddltostrips_tr'). 
:- add_sentence_trans(parser_ext/2).
 


%:- multifile init/2. %Multifile predicates can be defined by clauses defined in several modules and all the modules which define a predicate as multifile can use that predicate. It allows to load predicates of loaded files.

