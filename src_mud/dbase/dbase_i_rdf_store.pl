end_of_file.

:-module(dbase_rdf_store, [dbase_rdf/3,po/2,rdf_object/1,rdf_assert_hook/1 ]).

/** <module> MUD STORE

Installed to the ClioPatria SeRQL and SPARQL server

The function of an entailment module is  to provide an implementation of
rdf/3 that extends basic triple-lookup using the entailment rules of the
semantic web sub language of RDF.

This entailment module does MUD-DB entailment.

@tbd	Check the completeness
*/
:- use_module(library(semweb/rdf_db),except([rdf/3])).
:- use_module(library(semweb/rdf_persistency)).
% :- use_module(rdfql(rdfql_runtime)).	% runtime tests
:- use_module(library(nb_set)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_edit)).
:- use_module(library(url)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).

ssl_verify(_SSL, _ProblemCert, _AllCerts, _FirstCert, _Error) :- !.
        


%% n3_parse(+URL)
%
% Parse an OWL file and load it into the local RDF database.
% 
% Resolves owl:imports and supports both the common URL formats
% (file paths, file:// or http://) and the package:// URLs used
% in ROS to reference files with respect to the surrounding ROS
% package.
%
% @param URL Local or global file path or URL of the forms file://, http:// or package://
%
:-export(n3_parse/1).

graph_src(URL,Source):-rdf_current_prefix(Source,URL),!.
graph_src(URL,Source):-atom_concat(URL,'#',URIH),rdf_current_prefix(Source,URIH),!.
graph_src(URL,URL).

n3_parse(URL) :- graph_src(URL,Source),n3_parse(URL,[graph(Source),base_uri(URL),register_namespaces(true)]).
n3_parse(URL,Options) :-
  n3_parse_1(URL,[URL],Options).
  

n3_parse_1(URL,Imported,Options) :- catchv(n3_parse_2(URL,Imported,Options),E,dmsg(E:n3_parse_1(URL,Imported,Options))).
:-export(owl_file_loaded/1).

n3_parse_2(URL,Imported,Options) :-

  ((sub_string(URL,0,4,_,'http'), !,
    http_open(URL,RDF_Stream,[ cert_verify_hook(ssl_verify)
				]),
    rdf_load(RDF_Stream,[blank_nodes(noshare)|Options]),
    close(RDF_Stream)),
    assert(owl_file_loaded(URL))
    ;
   (sub_string(URL,0,7,_,'package'), !,

    % retrieve part after package://
    sub_atom(URL, 10, _, 0, Path),
    atomic_list_concat(PathList, '/', Path),

    % determine package name and resolve path
    selectchk(Pkg, PathList, LocalPath),
    rospack_package_path(Pkg, PkgPath),

    % build global path and load OWL file
    atomic_list_concat([PkgPath|LocalPath], '/',  GlobalPath),

    rdf_load(GlobalPath,[blank_nodes(noshare)|Options]),
    assert(owl_file_loaded(URL))
    ) ; (
    rdf_load(URL,[blank_nodes(noshare)|Options])),
    assert(owl_file_loaded(URL))
  ),
  (   rdf_db:rdf(_,'http://www.w3.org/2002/07/owl#imports',Import_URL),
      not( owl_file_loaded(Import_URL)),!,
      n3_parse_1(Import_URL,[Import_URL|Imported],Options)
    ; true).

:- n3_parse('http://omeo.googlecode.com/svn/trunk/build/ro-subset.owl').
:- n3_parse('http://knowrob.org/kb/roboearth.owl').
:- n3_parse('http://ias.cs.tum.edu/kb/knowrob.owl').
:- n3_parse('http://raw.github.com/knowrob/knowrob/master/knowrob_omics/rdf/locations.rdf').
:- n3_parse('http://raw.github.com/knowrob/knowrob/master/knowrob_omics/rdf/roboearth.rdf').


% :- rdf_attach_library((.)).
:-  with_no_term_expansions(use_module(cliopatria(cliopatria))).



/*[rdf_register_prefix/2,rdf_register_prefix/3,rdf_current_ns/2,rdf/4,rdf_assert/4,
                                      rdf_current_prefix/2,rdf_global_object/2,rdf_resource/1,rdf_current_predicate/1,
                                      rdf_has/3,rdf_graph_property/2,rdf_equal/2,rdf_literal_value/2,rdf_reachable/3,rdf_set_graph/2,rdf_subject/1]).
                                      */
:- rdf_register_prefix(agents_owl, 'http://onto.ui.sav.sk/agents.owl#',[force(true)]).
:- rdf_register_prefix(skos, 'http://www.w3.org/2004/02/skos/core#',[force(true)]).
:- rdf_register_prefix(skosxl,  'http://www.w3.org/2008/05/skos-xl#',[force(true)]).
:- rdf_register_prefix(knowrob_objects, 'http://ias.cs.tum.edu/kb/knowrob_objects.owl#',[force(true)]).
:- rdf_register_prefix(knowrob, 'http://ias.cs.tum.edu/kb/knowrob.owl#',[force(true)]).
:- rdf_register_prefix(mud,'http://www.prologmoo.com/onto/mud.owl#',[force(true)]).

:- public(rdf/3).
rdf(S,P,O):- show_call(dbase_rdf(S,P,O)).

:-op(1150, fx, (rdf_meta)).   :- rdf_meta
	rdf(o,o,o),
        rdf_x(o,r,o,o),
        rdf_assert_x(o,r,o,o),
	dbase_rdf_store:individual_of(r,r).



rdf_graph_ns(G,G):-var(G),!.
rdf_graph_ns(G:_,O):-!,rdf_graph_ns(G,O).
rdf_graph_ns(G,G).

is_url(S):-not(atom(S)),!,fail.
is_url(S):-atom_chars(S,Chars),(memberchk(':',Chars);memberchk('/',Chars);memberchk('#',Chars)),!.
is_url(S):-atom_chars(S,['h'|Chars]),memberchk('/',Chars),!.

% allow rdf_current_resource(knowrob:'Food')
rdf_current_resource(NS:Atom):-rdf_global_object(NS:Atom,URL),!,rdf_current_resource(URL).
rdf_current_resource(O):-(rdf_resource(O);rdf_current_predicate(O)). % aalready included rdf_subject(O).

prolog_to_qname(A,_):-not(atom(A)),!,fail.
prolog_to_qname(a,rdf:tType).
prolog_to_qname(ftInt,xsd:integer).

prolog_to_qname(Atom,knowrob:Proper):-atom(Atom),current_predicate(mudIsa/2),mudIsa(Atom,tCol),toPropercase(Atom,Proper),rdf_current_resource(knowrob:Proper),!.
prolog_to_qname([],rdf:nil).
prolog_to_qname(P,O):-po(P,C),P\=C,!,(prolog_to_qname(O,C);O=C).
prolog_to_qname(Atom,NS:Atom):-rdf_current_prefix(NS,_),rdf_global_object(NS:Atom,URL),rdf_current_resource(URL).
prolog_to_qname(URL,_):-not(is_url(URL)),!,fail.
prolog_to_qname(URL,NS:Atom):-rdf_global_object(NS:Atom,URL).
prolog_to_qname('http://www.w3.org/1999/02/22-rdf-syntax-ns#first',rdf:first).
prolog_to_qname('http://www.w3.org/1999/02/22-rdf-syntax-ns#type',rdf:tType).
prolog_to_qname('http://www.w3.org/1999/02/22-rdf-syntax-ns#',rdf:'<>').
prolog_to_qname(URL,Px:Rest):-concat_atom([NS,Rest],'#',URL),atom_concat(NS,'#',NSH),rdf_current_prefix(Px,NSH),!.


:- dynamic(rdf_alias/2).
cache_rdf_alias(M:_R:From,To):-!,cache_rdf_alias(M:From,To).
cache_rdf_alias(From,To):-rdf_alias(From,To),!.
cache_rdf_alias(From,To):-fmt(url_alias(From,To)),asserta(rdf_alias(From,To)),!.
list_rdf_alias:-listing(rdf_alias).


rdf_to_url(From,To):-must(ground(From)),rdf_alias(mud:From,To),!.
rdf_to_url(From,To):-must(ground(From)),rdf_to_url(mud,From,To),must(ground(To)),cache_rdf_alias(mud:From,To),!.

rdf_to_url(_,Var,V):-var(Var),!,must(copy_term(Var,V)).
%rdf_to_url(G,G:From,To):-!,rdf_to_url(G,From,To).
%rdf_to_url(_,G:From,To):-!,rdf_to_url(G,From,To).
rdf_to_url(G,From,To):-rdf_alias(G:From,To),!.
rdf_to_url(G,From,To):-rdf_to_url0(G,From,To),!,cache_rdf_alias(G:From,To),!.
rdf_to_url(_,Sx,Sx).

rdf_to_url0(G,Var,V):-not(ground(Var)),trace_or_throw(nonground(rdf_to_url(G,Var,V))).
rdf_to_url0(G,Var,V):-not(ground(G)),trace_or_throw(nonground(rdf_to_url(G,Var,V))).
rdf_to_url0(G,_:U/_, UO):-atom(U),!,must(rdf_to_url(G,U,UO)).
rdf_to_url0(G,U/_, UO):-atom(U),!,must(rdf_to_url(G,U,UO)).
rdf_to_url0(G,[H|T],List):-must(nonvar(G)),!, maplist(rdf_to_url(G),[H|T],HT),!,rdfs_assert_list(HT, List, G).
rdf_to_url0(G,NS:S,Sxx):-atom(S),once(prolog_to_qname(S,Sx)),Sx\=NS:S,rdf_to_url(G,Sx,Sxx).
rdf_to_url0(_,NS:R,URL):-must(ground(NS:R)),must(rdf_global_object(NS:R,URL)),!.
rdf_to_url0(_,U,U):-atom(U),is_url(U),!.
rdf_to_url0(G,S,Sxx):-atom(S),prolog_to_qname(S,Sx),Sx\=S,!,rdf_to_url(G,Sx,Sxx).
rdf_to_url0(_,U,O):-rdf_to_lit(U,M),must(rdf_global_object(M,O)),!.
rdf_to_url0(_,node(S),Sx):-atom_concat('__bnode',S,Sx),!.
rdf_to_url0(G,S,URL):-atom(S),rdf_graph_ns(G,NS),rdf_global_object(NS:S,URL),!.
rdf_to_url0(_,S,Sx):-catchv((rdf_global_object(S,Sx),not(compound(Sx))),_,fail),!.
rdf_to_url0(G,C,List):-compound(C),C=..[H|T],must(nonvar(G)),!,must(( maplist(rdf_to_url(G),[H|T],HT),!,rdfs_assert_list(HT, List, G))).


rdf_to_lit(U,literal(tType(xsd:integer, S))):-integer(U),!,atom_string(U,S).
rdf_to_lit(U,literal(tType(xsd:double, S))):-number(U),!,atom_string(U,S).
rdf_to_lit(U,literal(tType(xsd:string, U))):-string(U),!.

rdf_to_qname(_,Var,V):-var(Var),!,copy_term(Var,V).
rdf_to_qname(_,O,P):-p_to_o(O,P),!.
rdf_to_qname(_,P:S,P:S):-!.
rdf_to_qname(_,Sx,S):-is_url(Sx),prolog_to_qname(Sx,S0),!,must(S=S0).
rdf_to_qname(_,Sx,S):-prolog_to_qname(Sx,S0),!,must(S=S0).
rdf_to_qname(G,Sx,S):-rdfs_list_to_prolog_list(Sx,LL),!,maplist(rdf_to_qname(G),LL,S).
rdf_to_qname(_,literal(tType('http://www.w3.org/2001/XMLSchema#string', Atom)),String):-string_to_atom(String,Atom),!.
rdf_to_qname(_,literal(tType(_, Atom)),Term):-catch(term_to_atom(Term,Atom),_,fail),!.
rdf_to_qname(_,literal(tType(_, String)),Term):-catch(term_string(Term,String),_,fail),!.
rdf_to_qname(_,literal(Sx),S):-!,must(rdf_literal_value_safe(literal(Sx),S)).
rdf_to_qname(_,Sx,S):-compound(Sx),!,must(rdf_literal_value_safe(Sx,S)).
rdf_to_qname(_,Sx,node(S)):-atom(Sx),atom_concat('__bnode',S,Sx),!.
rdf_to_qname(G,Sx,NS:SI):-atom(Sx),rdf_graph_ns(G,NS),!,must(SI=Sx).
rdf_to_qname(_,Ss,Sx):-copy_term(Ss,Sx).

rdf_literal_value_safe(Sx,S):-rdf_literal_value(Sx,S),!.

onLoadTTL([],_File):-!.
onLoadTTL(List,G):-is_list(List),!,forall(member(X,List),onLoadTTL(X,G)).
onLoadTTL(X,G):-format('~q.~n',[X-G]),fail.
onLoadTTL(rdf(S,P,O),G):-rdf_assert_x(S,P,O,G).

rdf_to_graph(G,Go):-var(G),!,rdf_to_graph(user,Go).
rdf_to_graph(Gx,G):-rdf_current_ns(G,Gx),!.
rdf_to_graph(Gx,Gx):-rdf_current_ns(Gx,_),!.
rdf_to_graph(G,Gx):-rdf_create_graph(G),rdf_graph_property(G,source(S)),(rdf_register_prefix(G, S,[force(true)]),Gx=G),!.
rdf_to_graph(G,G):- atom(G),atomic_list_concat(['source://',G,'#'],S),
   rdf_register_prefix(G,S),
   ignore(rdf_set_graph(G,source(S))),!.


rdf_from_graph(G,G).

rdf_to_prolog_io(G,o,S,Sx):-!,must(notrace(rdf_to_qname(G,S,Sx))),!.
rdf_to_prolog_io(_,i,S,Sx):-ground(S),!,ignore(S=Sx).
rdf_to_prolog_io(G,i,S,Sx):-must(notrace(rdf_to_qname(G,S,Sx))),!.

rdf_to_url_io(_,S,Sx,o):-var(S),!,must(copy_term(S,Sx)).
rdf_to_url_io(G,S,Sx,i):-notrace(must(rdf_to_url(G,S,Sx))).

bs:- rdf_process_turtle('bootstrap.ttl',onLoadTTL,[prefixes(X)]),forall(member(NS-Prefix,X),rdf_register_prefix(NS,Prefix)).

enforce_never_p(_,_,[]):-!.
enforce_never_p(_,P,List):-member(L,List),rdf_equal(P,L),!,fail.
enforce_never_p(_,_,_).


   
current_g(knowrob).

rdf_object(NS:C):-!,ground(rdf_object(NS:C)).
rdf_object(L):-is_list(L),!.
rdf_object(C):-atomic(C).
rdf_object(O):-ground(O).


% user:decl_database_hook(assert(_A_or_Z),G):-rdf_assert_hook(G),!.

rdf_assert_ignored(mpred_prop(_,arity(1))).
rdf_assert_ignored(mpred_prop(_,argsIsaInList(_))).
rdf_assert_ignored(G):-functor(G,F,_),member(F,[hybrid_rule,mudTermAnglify,equivRule]).
rdf_assert_ignored(G):-functor(G,_,1).
rdf_assert_ignored(G):-  not(ground(G)). 

cyc_to_rdf(mpred_prop(P,PST),svo(F,StubType,S)):- PST=..[StubType,S],rdf_object(S),rdf_to_pred(P,F).
cyc_to_rdf(argIsa(P,1,D),domain(P,D)).
cyc_to_rdf(mudIsa(apathFn(A,Dir),T),mudIsa([apathFn,A,Dir],T)).
cyc_to_rdf(pathName(A,Dir,String),mudNamed([apathFn,A,Dir],String)).
cyc_to_rdf(default_sv(PAB, 2, V),type_default(A,[P,self,V])):-PAB=[P,A,_].
cyc_to_rdf(argIsa(P,2,D),range(P,D)):-mpred_arity(P,2).

rdf_assert_hook(CYC):-once(cyc_to_rdf(CYC,RDF)),CYC\=RDF,must(call(rdf_assert_hook(RDF))).
rdf_assert_hook(PSO):-rdf_assert_ignored(PSO),!.
rdf_assert_hook(PSO):-rdf_assert_hook0(PSO),!.
rdf_assert_hook(PSO):-dmsg(once(skipped(rdf_assert_hook(PSO)))).


rdf_assert_hook0(mudLabelTypeProps(A,Food,Props)):-atom_string(A,S),!,must((rdf_assert_hook(default_type_props(Food,[label(S)|Props])))).
rdf_assert_hook0(default_type_props(Food,Props)):-is_list(Props),!,forall(member(P,Props),must(rdf_assert_hook(default_type_props(Food,P)))).
rdf_assert_hook0(default_type_props(Food,Prop)):-Prop=..[P|ARGS],must(rdf_assert_hook(type_default(Food,[P,self|ARGS]))).
rdf_assert_hook0(mudSubclass(C,P)):-!,rdf_object(C),rdf_object(P),rdf_assert_x(C,rdfs:subClassOf,P).
rdf_assert_hook0(mudDescription(C,P)):-!,rdf_object(C),rdf_object(P),rdf_assert_x(C,rdfs:comment,P).
rdf_assert_hook0(mudIsa(Prop,tMpred)):- rdf_to_pred(Prop,P),!,rdf_object(P),rdf_assert_x(P,rdf:tType,owl:'Property').
rdf_assert_hook0(mudIsa(Prop,singleValued)):- functor(Prop,P,_),!,rdf_object(P),rdf_assert_x(P,rdf:tType,owl:'FunctionalProperty').
rdf_assert_hook0(arity(W1,N)):-rdf_to_pred(W1,W),N>1,rdf_assert_x(W,rdf:tType,owl:'Property').
rdf_assert_hook0(mudIsa(W,tCol)):-!,rdf_object(W),rdf_assert_x(W,rdf:tType,owl:'Class').
rdf_assert_hook0(mudIsa(C,P)):-!,rdf_object(C),rdf_object(P),P\=tCol,rdf_assert_x(C,rdf:tType,P).
rdf_assert_hook0(svo(S,P,O)):-!,must(rdf_assert_x(S,P,O)).
rdf_assert_hook0(PSO):-PSO=..[P,S,O],!,rdf_assert_x(S,P,O).
rdf_assert_hook0(PSO):-PSO=..[P,S|O],!,rdf_assert_x(S,P,O).

rdf_to_pred(W,W):-var(W),!.
rdf_to_pred(W,F):-get_functor(W,F),!.

rdf_to_url_ignore(G,S,Sxx):-atom(S),prolog_to_qname(S,Sx),!,rdf_to_url(G,Sx,Sxx).
rdf_to_url_ignore(G,A,B):-rdf_to_url(G,A,BB),ignore(B=BB).

po(mud:O,OO):-nonvar(O),!,po(O,OO).
po(tFood,knowrob:'Food').
po(mpred_arity,arity).
po(tCol,owl:'Class').
po(tItem,knowrob:'HumanScaleObject').
po(tSpatialthing,knowrob:'SpatialThing').
po(mudSubclass,rdfs:subClassOf).
po(tRegion,knowrob:'FixedStructure').
po(tAgentGeneric,knowrob:'Agent-Generic').
po(ftInt,xsd:integer).
po(string,xsd:string).
po(F,G:A):-rdf_alias(mud:F,G:A).



o_to_p(O,T):-po(T,C),O==C,!.
o_to_p(O,O).

p_to_o(P,C):-po(T,C),P==T,!.
p_to_o(O,O).

dbase_t_rdf(Sc,rdf:tType,CC):- /*o_to_p(CC,Oc),*/clause(hasInstance(Oc,Sc),true),p_to_o(Oc,CC).
dbase_t_rdf(Sc,Pc,Oc):-dbase_t(Pc,Sc,Oc).

:-export(rdf_assert_x/3).
rdf_assert_x(S,P,O):- rdf_assert_x(S,P,O,mud).
:-export(rdf_assert_x/4).
% rdf_assert_x(S,P,O,G):-Q=rdf_x(S,P,O,G),not(ground(Q)),!,Q.
rdf_assert_x(S,P,O,G):-
  logOnFailure((
    notrace((must(notrace((rdf_to_graph(G,Gx)))),rdf_to_url(Gx,S,Sx),rdf_to_url(Gx,P,Px),rdf_to_url(Gx,O,Ox))),
      logOnFailure(((must(call(rdf_assert(Sx,Px,Ox,Gx)))))))).


:-export(rdf_x/3).
rdf_x(S,P,O):- rdf_x(S,P,O,mud).
:-export(rdf_x/4).
rdf_x(S,P,O,G):-
  notrace(once((rdf_to_url_io(user,G,Gx,_Gio),rdf_to_url_io(Gx,S,Sx,Sio),rdf_to_url_io(Gx,P,Px,Pio),rdf_to_url_io(Gx,O,Ox,Oio)))),
                notrace(((nonvar(P)->NeverP=[];NeverP=[rdf:ftRest,rdf:first,rdf:tType]))),
                rdf_db:rdf(Sx,Px,Ox,Gx),
                rdf_to_prolog_io(G,Pio,Px,P),
                enforce_never_p(G,P,NeverP),
                once((rdf_from_graph(Gx,G),rdf_to_prolog_io(G,Sio,Sx,S),rdf_to_prolog_io(G,Oio,Ox,O))).



dbase_rdf(S,P,O):-
  current_g(G),
  maplist(rdf_to_qname(G),[S,P,O],[Sc,Pc,Oc]),
  dbase_t_rdf(Sc,Pc,Oc),
  maplist(rdf_to_url_ignore(G),[Sc,Pc,Oc],[S,P,O]).

% http://localhost:3020/cliopatria/browse/list_triples?graph=file:///t:/devel/cliopatria/rdf/base/rdfs.rdfs
% http://localhost:3020/cliopatria/browse/list_triples_with_object?r=http://www.w3.org/1999/02/22-rdf-syntax-ns%23Property
% http://localhost:3020/servlets/loadLibraryOntology?resultFormat=html&ontology=rdfs
% http://localhost:3020/servlets/loadLibraryOntology?resultFormat=html&ontology=owl

dbase_rdf(skosxl:prefLabel,   rdf:tType, owl:'ObjectProperty').
dbase_rdf(skosxl:altLabel,    rdf:tType, owl:'ObjectProperty').
dbase_rdf(skosxl:hiddenLabel, rdf:tType, owl:'ObjectProperty').

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
	;   rdf_db:rdf(P, rdf:tType, rdf:'Property'),
	    rdf_db:rdf(S, P, O),
	    \+ rdf_db:rdf(S,P,O)
	).
dbase_rdf(S, P, C) :-
	rdf_reachable(rdf:tType, rdfs:subPropertyOf, P), !,
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
dbase_rdf(S, serql:directType, O) :- !, rdf_has(S, rdf:tType, O).
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
	->  (   rdf_has(Resource, rdf:tType, Class)
	    ;	rdf_db:rdf(Resource, P, _),
		rdf_has(P, rdfs:domain, Class)
	    ;	rdf_db:rdf(_, P, Resource),
		rdf_has(P, rdfs:range, Class)
	    ),
	    add_nb_set(Class, Set, New),
	    New == true
	;   atom(Class)
	->  (	rdf_has(Resource, rdf:tType, Class)
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


:-export(sync_rdf/0).

sync_rdf:-
   forall(disjointWith0(A,B),rdf_assert_hook(disjointWith(A,B))),
   forall(is_known_trew(B),rdf_assert_hook(B)),
   forall(dbase_t(P,S,O),rdf_assert_hook(svo(S,P,O))),
   forall(po(P,O),rdf_assert_hook(mudSubclass(P,O))),
   forall(hasInstance(C,I),rdf_assert_hook(mudIsa(I,C))).

:- sync_rdf.

:- multifile(user:call_OnEachLoad/1).
:- asserta_if_new(user:call_OnEachLoad(sync_rdf)).
