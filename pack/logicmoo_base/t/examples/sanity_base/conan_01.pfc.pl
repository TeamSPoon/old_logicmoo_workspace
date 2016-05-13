#!/usr/bin/env swipl

:- module(sanity_ks_two,[]).

:- dynamic(prologHybrid/1).
:- use_module(library(logicmoo_base)).

/*

((prologHybrid(F),arity(F,A)/is_ftNameArity(F,A))<==>hybrid_support(F,A)/is_ftNameArity(F,A)).

prologMultiValued(mudDescription(ftTerm,ftText), [predProxyAssert(add_description),predProxyRetract(remove_description),predProxyQuery(query_description)],prologHybrid).

prologHybrid(isEach(mudLastCommand/2,mudNamed/2, mudSpd/2,mudStr/2,typeGrid/3)).

((prologHybrid(F),arity(F,A)/is_ftNameArity(F,A))<==>hybrid_support(F,A)/is_ftNameArity(F,A)).

*/

:- must((fully_expand( ptF(foo/2),O), O = (arity(foo, 2), ptF(foo), tPred(foo)))).



