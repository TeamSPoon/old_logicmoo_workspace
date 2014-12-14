:-module(dbase_rdf_store, [dbase_rdf/3]).

/** <module> MUD STORE

Installed to the ClioPatria SeRQL and SPARQL server

The function of an entailment module is  to provide an implementation of
rdf/3 that extends basic triple-lookup using the entailment rules of the
semantic web sub language of RDF.

This entailment module does MUD-DB entailment.

@tbd	Check the completeness
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_persistency)).
% :- use_module(rdfql(rdfql_runtime)).	% runtime tests
:- use_module(library(nb_set)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/turtle)).
% :- rdf_attach_library((.)).

:-op(1150, fx, (rdf_meta)).   :- rdf_meta
	dbase_rdf_store:rdf(o,o,o),
        rdf_x(o,r,o,o),
        rdf_assert_x(o,r,o,o),
	dbase_rdf_store:individual_of(r,r).

:- public rdf/3.

:- rdf_register_prefix(skos,	  'http://www.w3.org/2004/02/skos/core#',[keep(true)]).
:- rdf_register_prefix(skosxl,  'http://www.w3.org/2008/05/skos-xl#',[keep(true)]).
:- rdf_register_prefix(knowrob_objects, "http://ias.cs.tum.edu/kb/knowrob_objects.owl#",[keep(true)]).
:- rdf_register_prefix(knowrob, "http://ias.cs.tum.edu/kb/knowrob.owl#",[keep(true)]).

rdf_graph_ns(G,G):-var(G),!.
rdf_graph_ns(G:_,O):-!,rdf_graph_ns(G,O).
rdf_graph_ns(G,G).

is_url(S):-not(atom(S)),!,fail.
is_url(S):-atom_chars(S,Chars),(memberchk(':',Chars);memberchk('/',Chars);memberchk('#',Chars)),!.
is_url(S):-atom_chars(S,['h'|Chars]),memberchk('/',Chars),!.


rdf_current_resource(O):-(rdf_resource(O);rdf_current_predicate(O)). % aalready included rdf_subject(O).

prolog_to_cname(A,_):-not(atom(A)),!,fail.
prolog_to_cname(a,rdf:type).
prolog_to_cname([],rdf:nil).
prolog_to_cname(Atom,NS:Atom):-rdf_current_prefix(NS,_),rdf_global_object(NS:Atom,URL),rdf_current_resource(URL).
prolog_to_cname(URL,_):-not(is_url(URL)),!,fail.
prolog_to_cname(URL,NS:Atom):-rdf_global_object(NS:Atom,URL).
prolog_to_cname('http://www.w3.org/1999/02/22-rdf-syntax-ns#first',rdf:first).
prolog_to_cname('http://www.w3.org/1999/02/22-rdf-syntax-ns#type',rdf:type).
prolog_to_cname('http://www.w3.org/1999/02/22-rdf-syntax-ns#',rdf:'<>').
prolog_to_cname(URL,Px:Rest):-concat_atom([NS,Rest],'#',URL),atom_concat(NS,'#',NSH),rdf_current_prefix(Px,NSH),!.

rdf_to_node(_,Var,V):-var(Var),!,must(copy_term(Var,V)).
rdf_to_node(G,[H|T],List):-must(nonvar(G)),!, maplist(rdf_to_node(G),[H|T],HT),!,rdfs_assert_list(HT, List, G).
rdf_to_node(_,NS:R,URL):-must(ground(NS:R)),must(rdf_global_object(NS:R,URL)),!.
rdf_to_node(_,U,U):-atom(U),is_url(U),!.
rdf_to_node(G,S,Sxx):-atom(S),prolog_to_cname(S,Sx),!,rdf_to_node(G,Sx,Sxx).
rdf_to_node(_,U,O):-rdf_to_lit(U,M),must(rdf_global_object(M,O)),!.
rdf_to_node(_,node(S),Sx):-atom_concat('__bnode',S,Sx),!.
rdf_to_node(G,S,URL):-atom(S),rdf_graph_ns(G,NS),rdf_global_object(NS:S,URL),!.
rdf_to_node(_,Ss,Sx):-rdf_global_object(Ss,Sx),!.
rdf_to_node(_,Sx,Sx).


rdf_to_lit(U,literal(type(xsd:integer, S))):-integer(U),!,atom_string(U,S).
rdf_to_lit(U,literal(type(xsd:double, S))):-number(U),!,atom_string(U,S).
rdf_to_lit(U,literal(type(xsd:string, U))):-string(U),!.


rdf_to_cname(_,Var,V):-var(Var),!,copy_term(Var,V).
rdf_to_cname(_,P:S,P:S):-!.
rdf_to_cname(_,Sx,S):-is_url(Sx),prolog_to_cname(Sx,S0),!,must(S=S0).
rdf_to_cname(_,Sx,S):-prolog_to_cname(Sx,S0),!,must(S=S0).
rdf_to_cname(G,Sx,S):-rdfs_list_to_prolog_list(Sx,LL),!,maplist(rdf_to_cname(G),LL,S).
rdf_to_cname(_,literal(type('http://www.w3.org/2001/XMLSchema#string', Atom)),String):-string_to_atom(String,Atom),!.
rdf_to_cname(_,literal(type(_, Atom)),String):-atom_to_term(Atom,String),!.
rdf_to_cname(_,Sx,S):-compound(Sx),!,must(rdf_literal_value_safe(Sx,S)).
rdf_to_cname(_,Sx,node(S)):-atom(Sx),atom_concat('__bnode',S,Sx),!.
rdf_to_cname(G,Sx,NS:SI):-atom(Sx),rdf_graph_ns(G,NS),!,must(SI=Sx).
rdf_to_cname(_,Ss,Sx):-copy_term(Ss,Sx).

rdf_literal_value_safe(Sx,S):-rdf_literal_value(Sx,S),!.

onLoadTTL([],_File):-!.
onLoadTTL(List,G):-is_list(List),!,forall(member(X,List),onLoadTTL(X,G)).
onLoadTTL(X,G):-format('~q.~n',[X-G]),fail.
onLoadTTL(rdf(S,P,O),G):-rdf_assert_x(S,P,O,G).

rdf_to_graph(G,Go):-var(G),!,rdf_to_graph(user,Go).
rdf_to_graph(Gx,G):-rdf_current_ns(G,Gx),!.
rdf_to_graph(Gx,Gx):-rdf_current_ns(Gx,_),!.
rdf_to_graph(G,Gx):-rdf_create_graph(G),rdf_graph_property(G,source(S)),(rdf_register_prefix(G, S,[keep(true)]),Gx=G),!.
rdf_to_graph(G,G):- atom(G),atomic_list_concat(['source://',G,'#'],S),
   rdf_register_prefix(G,S),
   ignore(rdf_set_graph(G,source(S))),!.


rdf_from_graph(G,G).

rdf_from_node_io(G,o,S,Sx):-!,rdf_to_cname(G,S,Sx),!.
rdf_from_node_io(_,i,S,Sx):-ground(S),!,ignore(S=Sx).
rdf_from_node_io(G,i,S,Sx):-rdf_to_cname(G,S,Sx),!.

rdf_to_node_io(_,S,Sx,o):-var(S),!,must(copy_term(S,Sx)).
rdf_to_node_io(G,S,Sx,i):-rdf_to_node(G,S,Sx).

bs:- rdf_process_turtle('bootstrap.ttl',onLoadTTL,[prefixes(X)]),forall(member(NS-Prefix,X),rdf_register_prefix(NS,Prefix)).

enforce_never_p(_,_,[]):-!.
enforce_never_p(_,P,List):-member(L,List),rdf_equal(P,L),!,fail.
enforce_never_p(_,_,_).


rdf(S,P,O):- show_call(dbase_rdf(S,P,O)).
   
current_g(knowrob).

dbase_rdf(S,P,O):-
  current_g(G),
  maplist(rdf_to_cname(G),[S,P,O],[Sc,Pc,Oc]),
  dbase_t_rdf(Sc,Pc,Oc),
  maplist(rdf_to_node_ignore(G),[Sc,Pc,Oc],[S,P,O]).

rdf_to_node_ignore(G,S,Sxx):-atom(S),prolog_to_cname(S,Sx),!,rdf_to_node(G,Sx,Sxx).
rdf_to_node_ignore(G,A,B):-rdf_to_node(G,A,BB),ignore(B=BB).

:- rdf_register_prefix(logicmoo, 'http://onto.ui.sav.sk/agents.owl#').


o_to_p(O,type):-O==owl:'Class',!.
o_to_p(O,O).

p_to_o(P,owl:'Class'):-P==type,!.
p_to_o(O,O).

dbase_t_rdf(Sc,rdf:type,CC):- /*o_to_p(CC,Oc),*/hasInstance(Oc,Sc),p_to_o(Oc,CC).
dbase_t_rdf(Sc,Pc,Oc):-dbase_t(Pc,Sc,Oc).

:-export(rdf_assert_x/4).
rdf_assert_x(S,P,O,G):-Q=rdf_x(S,P,O,G),not(ground(Q)),!,Q.
rdf_assert_x(S,P,O,G):-
  must((rdf_to_graph(G,Gx))),rdf_to_node(Gx,S,Sx),rdf_to_node(Gx,P,Px),rdf_to_node(Gx,O,Ox),rdf_assert(Sx,Px,Ox,Gx).

:-export(rdf_x/4).
rdf_x(S,P,O,G):-
  notrace((once((rdf_to_node_io(user,G,Gx,_Gio),rdf_to_node_io(Gx,S,Sx,Sio),rdf_to_node_io(Gx,P,Px,Pio),rdf_to_node_io(Gx,O,Ox,Oio))))),
                (nonvar(P)->NeverP=[];NeverP=[rdf:rest,rdf:first,rdf:type]),
                rdf(Sx,Px,Ox,Gx),
                rdf_from_node_io(G,Pio,Px,P),
                enforce_never_p(G,P,NeverP),
                once((rdf_from_graph(Gx,G),rdf_from_node_io(G,Sio,Sx,S),rdf_from_node_io(G,Oio,Ox,O))).



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


dbase_rdf(S, P, O):-!, rdf_db:rdf(S, P, O).

dbase_rdf(S, P, O):-dmsg(dbase_rdf(S, P, O)),!,fail.

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
dbase_rdf(S, serql:directSubClassOf, O) :- !, rdf_has(S, rdfs:subClassOf, O).
dbase_rdf(S, serql:directType, O) :- !, rdf_has(S, rdf:type, O).
dbase_rdf(S, serql:directSubPropertyOf, O) :- !, rdf_has(S, rdfs:subPropertyOf, O).
dbase_rdf(S, P, O) :- rdf_has(S, P, O).


%%	individual_of(?Resource, ?Class)
individual_of(Resource, Class):- dbase_individual_of(Resource, Class).


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

cliopatria:entailment(dbase_rdf, dbase_rdf_store).
