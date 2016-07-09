:- module(conf_prefixes, []).
:- use_module(library(semweb/rdf_db)).

/** <module> Configure prefixes (namespaces)

Register  additional  prefixes.  Registering  a    prefix  serves  three
purposes:

    * It can be used in code, e.g., rdf(X, rdf:type, rdfs:'Class')
    * It can be used in the same fashion from the toplevel
    * It is used by the web-services to present resources more compact.

@see	rdf_register_prefix/2 and rdf_register_prefix/3
*/

% :- rdf_register_prefix(cornetto, 'http://purl.org/vocabularies/cornetto/').
