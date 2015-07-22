/* -*- Mode: Prolog -*- */

:- module(owl2_reasoning_rules,[]).

:- use_module(owl2_model).

% yap will not allow us to table a predicate already declared dynamic..
:- abolish(owl2_model:subClassOf/2).
:- abolish(owl2_model:subPropertyOf/2).
:- abolish(owl2_model:classAssertion/2).
:- abolish(owl2_model:propertyAssertion/3).

:- multifile owl2_model:subClassOf/2.
:- multifile owl2_model:subPropertyOf/2.
:- multifile owl2_model:classAssertion/2.
:- multifile owl2_model:propetyAssertion/3.

:- table owl2_model:subClassOf/2.
:- table owl2_model:subPropertyOf/2.
:- table owl2_model:classAssertion/2.
:- table owl2_model:propertyAssertion/3.


%:- table member/2.

% ----------------------------------------
% TBox Reasoning
% ----------------------------------------

someValuesFrom_propchain(_,[]).
someValuesFrom_propchain(X,[P|PL]):-
        subClassOf(X,someValuesFrom(P,Y)),
        someValuesFrom_propchain(Y,PL).


:- table subClassOf_all/2.
owl2_model:subClassOf_all(_,[]).
owl2_model:subClassOf_all(X,[D|L]):-
        subClassOf(X,D),
        subClassOf_all(X,L).

:- table pairwise_equivalent_class/2.
pairwise_equivalent_class(X,Y) :- equivalentClasses(L),member(X,L),member(Y,L).
pairwise_equivalent_class(X,Y) :- subClassOf(X,Y),subClassOf(Y,X).

:- table pairwise_disjoint_class/2.
pairwise_disjoint_class(X,Y) :- disjointClasses(L),member(X,L),member(Y,L).
pairwise_disjoint_class(X,Y) :- subClassOf(X,Y),subClassOf(Y,X).

% transitivity of subprop
owl2_model:subPropertyOf(X,Y) :- subPropertyOf(X,Z),subPropertyOf(Z,Y).

% transitivity of subclass
owl2_model:subClassOf(X,Y) :- subClassOf(X,Z),subClassOf(Z,Y).

owl2_model:subClassOf(X,Y) :-
        pairwise_equivalent_class(X,Y).

owl2_model:subClassOf(X,Y) :-
        pairwise_equivalent_class(X,intersectionOf(L)),
        member(Y,L).

owl2_model:subClassOf(X,Y) :-
        pairwise_equivalent_class(Y,unionOf(L)),
        member(X,L).

owl2_model:subClassOf(X,Y) :-
        pairwise_equivalent_class(Y,intersectionOf(L)),
        subClassOf_all(X,L).

owl2_model:subClassOf(X,someValuesFrom(P,D)) :-
        subPropertyOf(P1,P),
        subClassOf(X,someValuesFrom(P1,D)).

% this appears to cause a loop, even with tabling...
%owl2_model:subClassOf(X,someValuesFrom(P,D)) :-
%        subClassOf(X,someValuesFrom(P,D1)),
%        subClassOf(D1,D).

%owl2_model:subClassOf(X,someValuesFrom(P,Y)) :-
%        transitiveProperty(P),
%        subClassOf(X,someValuesFrom(P,someValuesFrom(P,Y))).
owl2_model:subClassOf(X,someValuesFrom(P,Y)) :-
        subClassOf(X,someValuesFrom(P,D)),
	transitiveProperty(P),
        subClassOf(D,someValuesFrom(P,Y)).

%

/*
owl2_model:subClassOf(X,allValuesFrom(P,D)) :-
        subClassOf(X,allValuesFrom(P,D1)),
        subClassOf(D1,D).
  */
%subClassOf(X,someValuesFrom(P,Y)) :-
%        subClassOf(X,someValuesFrom(P1,D1)),
%        subPropertyOf(propertyChain([P1,P2]),P), % TODO
%        subClassOf(D1,someValuesFrom(P2,D2)).
%subClassOf(X,someValuesFrom(P,Y)) :-
%        subClassOf(X,someValuesFrom(P1,D1)),
%        subPropertyOf(propertyChain([P1|PL]),P), % TODO
%        someValuesFrom_propchain(D1,PL).


% TODO: make subClassOf nothing?
:- table unsatisfiable/1.
unsatisfiable(X) :-
        pairwise_disjoint_class(A,B),
        subClassOf(X,A),
        subClassOf(X,B).

% ----------------------------------------
% ABox Reasoning
% ----------------------------------------

% reified version of Grosof

owl2_model:classAssertion(C,I) :- classAssertion(C2,I),subClassOf(C2,C).

owl2_model:classAssertion(C,I) :- propertyDomain(P,C),propertyAssertion(P,I,_).
owl2_model:classAssertion(C,I) :- propertyRange(P,C),propertyAssertion(P,_,I).

owl2_model:propertyAssertion(P,A,B) :- transitiveProperty(P),propertyAssertion(P,A,C),propertyAssertion(P,C,B).
owl2_model:propertyAssertion(P,A,B) :- symmetricProperty(P),propertyAssertion(P,B,A).
owl2_model:propertyAssertion(P,A,B) :- inverseProperties(P,Q),propertyAssertion(Q,B,A).
owl2_model:propertyAssertion(Q,A,B) :- subPropertyOf(P,Q),propertyAssertion(P,A,B).

% role chains
%propertyAssertion(P,A,B) :- subPropertyOf(propertyChain([P1|PL]),P),propertyAssertion(P,A,C),propertyAssertion_chain(PL,C,B).

propertyAssertion_chain([],_,_).
propertyAssertion_chain([P|PL],A,B) :- propertyAssertion(P,A,C),propertyAssertion_chain(PL,C,B).

% todo: translate to sameIndividuals/1
sameAs(A,B) :- propertyAssertion(P,X,A),functionalProperty(P),propertyAssertion(P,X,B).
sameAs(A,B) :- propertyAssertion(P,A,X),inverseFunctionalProperty(P),propertyAssertion(P,B,X).

illegal(P) :- propertyAssertion(P,A,A),irreflexiveProperty(P).
illegal(P) :- propertyAssertion(P,A,B),asymmetricProperty(P),propertyAssertion(P,B,A).




/** <module> 

  ---+ Synopsis


---+ Details

See http://www.w3.org/TR/2008/WD-owl2-profiles-20081202/#Reasoning_in_OWL_2_RL_and_RDF_Graphs_using_Rules

---+ Additional Information



*/
