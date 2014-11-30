:-swi_module(dbase_rdf_entailment,
	  []).

end_of_file.

/** <module> MUD Entailment

Installed to the ClioPatria SeRQL and SPARQL server

The function of an entailment module is  to provide an implementation of
rdf/3 that extends basic triple-lookup using the entailment rules of the
semantic web sub language of RDF.

This entailment module does MUD-DB entailment.

*/


:- use_module(library(semweb/rdf_db)).
% :- use_module(library(semweb/rdf_persistency)).
% :- use_module(rdfql(rdfql_runtime)).	% runtime tests
:- use_module(library(nb_set)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).
% :- rdf_attach_library((.)).

:- rdf_meta
	dbase_rdf_entailment:rdf(o,o,o),
	dbase_rdf_entailment:individual_of(r,r).

:- public rdf/3.


from_a(S0,P0,O0,S,P,O):-from_a(S0,S),from_a(P0,P),from_a(O0,O).

from_a(P:_,_):-atom(P),!.
from_a(_,_):-!.

to_a(S0,P0,O0,S,P,O):-to_a(S0,S),to_a(P0,P),to_a(O0,O).

to_a(P:A,O1):-atom(P),rdf_current_prefix(P,PP),!,atom_concat(PP,A,O2),!,O1=O2.
to_a(O,O1):-O=O1.

rdf(S0,P0,O0):-
   from_a(S0,P0,O0,S,P,O),
   dbase_rdf(S,P,O),
   to_a(S0,P0,O0,S,P,O).
   

:- rdf_register_prefix(logicmoo, 'http://onto.ui.sav.sk/agents.owl#').
:- rdf_register_prefix(skos,	  'http://www.w3.org/2004/02/skos/core#').
:- rdf_register_prefix(skosxl,  'http://www.w3.org/2008/05/skos-xl#').

% http://localhost:3020/cliopatria/browse/list_triples?graph=file:///t:/devel/cliopatria/rdf/base/rdfs.rdfs
% http://localhost:3020/cliopatria/browse/list_triples_with_object?r=http://www.w3.org/1999/02/22-rdf-syntax-ns%23Property
% http://localhost:3020/servlets/loadLibraryOntology?resultFormat=html&ontology=rdfs
% http://localhost:3020/servlets/loadLibraryOntology?resultFormat=html&ontology=owl

dbase_rdf(skosxl:prefLabel,   rdf:type, owl:'ObjectProperty').
dbase_rdf(skosxl:altLabel,    rdf:type, owl:'ObjectProperty').
dbase_rdf(skosxl:hiddenLabel, rdf:type, owl:'ObjectProperty').

dbase_rdf('http://www.w3.org/2000/01/rdf-schema#ContainerMembershipProperty','http://www.w3.org/2000/01/rdf-schema#subClassOf','http://www.w3.org/1999/02/22-rdf-syntax-ns#Property').
dbase_rdf('http://www.w3.org/2002/07/owl#someValuesFrom','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/1999/02/22-rdf-syntax-ns#Property').
dbase_rdf('http://www.w3.org/2002/07/owl#someValuesFrom','http://www.w3.org/2000/01/rdf-schema#label',literal(someValuesFrom)).
dbase_rdf('http://www.w3.org/2002/07/owl#someValuesFrom','http://www.w3.org/2000/01/rdf-schema#domain','http://www.w3.org/2002/07/owl#Restriction').
dbase_rdf('http://www.w3.org/2002/07/owl#FunctionalProperty','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2000/01/rdf-schema#Class').
dbase_rdf('http://www.w3.org/2002/07/owl#FunctionalProperty','http://www.w3.org/2000/01/rdf-schema#label',literal('FunctionalProperty')).
dbase_rdf('http://www.w3.org/2002/07/owl#FunctionalProperty','http://www.w3.org/2000/01/rdf-schema#subClassOf','http://www.w3.org/1999/02/22-rdf-syntax-ns#Property').
dbase_rdf('http://www.w3.org/1999/02/22-rdf-syntax-ns#object','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/1999/02/22-rdf-syntax-ns#Property').
dbase_rdf('http://www.w3.org/1999/02/22-rdf-syntax-ns#object','http://www.w3.org/2000/01/rdf-schema#comment',literal('The object of an RDF statement.')).
dbase_rdf('http://www.w3.org/1999/02/22-rdf-syntax-ns#object','http://www.w3.org/2000/01/rdf-schema#domain','http://www.w3.org/1999/02/22-rdf-syntax-ns#Statement').
dbase_rdf('http://www.w3.org/1999/02/22-rdf-syntax-ns#object','http://www.w3.org/2000/01/rdf-schema#isDefinedBy','http://www.w3.org/1999/02/22-rdf-syntax-ns#').
dbase_rdf('http://www.w3.org/1999/02/22-rdf-syntax-ns#object','http://www.w3.org/2000/01/rdf-schema#label',literal(lang(en,object))).

dbase_rdf(S, P, O):-rdf_db:rdf(S, P, O).

dbase_rdf(literal(L), _, _) :-		% should move to compiler
	nonvar(L), !, fail.
dbase_rdf(_, literal(L), _) :-		% idem
	nonvar(L), !, fail.
dbase_rdf(S, P, O) :-
	var(P), !,
	(   rdf_db:rdf(S,P,O)
	;   rdf(P, rdf:type, rdf:'Property'),
	    rdf(S, P, O),
	    \+ rdf_db:rdf(S,P,O)
	).
dbase_rdf(S, P, C) :-
	rdf_reachable(rdf:type, rdfs:subPropertyOf, P), !,
	individual_of(S, C).
dbase_rdf(S, P, O) :-					% transitive predicates
	rdf_reachable(rdfs:subClassOf, rdfs:subPropertyOf, P), !,
	(   (nonvar(S) ; nonvar(O))
	->  rdfs_subclass_of(S, O)		% generate from given start
	;   individual_of(S, rdfs:'Class'),	% generated unbounded (slow!)
	    rdfs_subclass_of(S, O)
	).
dbase_rdf(S, rdfs:subPropertyOf, O) :- !,
	(   nonvar(S)
	->  individual_of(S, rdf:'Property'),
	    rdfs_subproperty_of(S, O)
	;   nonvar(O)
	->  individual_of(O, rdf:'Property'),
	    rdfs_subproperty_of(S, O)
	;   individual_of(S, rdf:'Property'),
	    rdfs_subproperty_of(S, O)
	).
dbase_rdf(S, serql:directSubClassOf, O) :- !,
	rdf_has(S, rdfs:subClassOf, O).
dbase_rdf(S, serql:directType, O) :- !,
	rdf_has(S, rdf:type, O).
dbase_rdf(S, serql:directSubPropertyOf, O) :- !,
	rdf_has(S, rdfs:subPropertyOf, O).
dbase_rdf(S, P, O) :-
	rdf_has(S, P, O).


%%	individual_of(?Resource, ?Class)
individual_of(Resource, Class):-
   dbase_individual_of(Resource, Class).


dbase_individual_of(Resource, Class) :-
	nonvar(Resource), !,
	(   Resource = literal(_)
	->  rdfs_subclass_of(Class, rdfs:'Literal')
	;   dbase_rdf_has_type(Resource, MyClass),
	    rdfs_subclass_of(MyClass, Class)
	;   rdf_equal(Class, rdfs:'Resource')
	).
dbase_individual_of(Resource, Class) :-
	nonvar(Class), !,
	(   rdf_equal(Class, rdfs:'Resource')
	->  rdf_subject(Resource)
	;   rdfs_subclass_of(SubClass, Class),
	    dbase_rdf_has_type(Resource, SubClass)
	).
dbase_individual_of(Resource, Class) :-
	rdf_subject(Resource),
	individual_of(Resource, Class).


%%	dbase_rdf_has_type(+Resource, -Class) is nondet.
%%	dbase_rdf_has_type(-Resource, +Class) is nondet.
%
%	Perform RDFS entailment rules to enumerate the types of Resource
%	or generate all resources entailed  by   the  given  class.

dbase_rdf_has_type(Resource, Class) :-
	empty_nb_set(Set),
	(   atom(Resource)
	->  (   rdf_has(Resource, rdf:type, Class)
	    ;	rdf_db:rdf(Resource, P, _),
		rdf_has(P, rdfs:domain, Class)
	    ;	rdf_db:rdf(_, P, Resource),
		rdf_has(P, rdfs:range, Class)
	    ),
	    add_nb_set(Class, Set, New),
	    New == true
	;   atom(Class)
	->  (	rdf_has(Resource, rdf:type, Class)
	    ;	rdf_has(P, rdfs:domain, Class),
		rdf_has(Resource, P, _)
	    ;	rdf_has(P, rdfs:range, Class),
		rdf_has(_, P, Resource)
	    ),
	    add_nb_set(Resource, Set, New),
	    New == true
	;   throw(error(instantiation_error, _))
	).


		 /*******************************
		 *	       REGISTER		*
		 *******************************/

:- multifile
	cliopatria:entailment/2.

cliopatria:entailment(dbase_rdf, dbase_rdf_entailment).
