# Logicmoo_base 

A package for First Order Logic (FOL) declarations in Prolog source code.

# Introduction 

Prolog, like most logic programming languages, offers depth first backward chaining as the only reasoning scheme. It is well known that sound and complete reasoning systems can be built using either exclusive backward chaining or exclusive forward chaining. Thus, this is not a theoretical problem. It is also well understood how to ``implement'' forward reasoning using an exclusively backward chaining system and vice versa. Thus, this need not be a practical problem. In fact, many of the logic-based languages developed for AI applications allow one to build systems with both forward and backward chaining rules.

There are, however, some interesting and important issues which need to be addresses in order to provide the Prolog programmer with a practical, efficient, and well integrated facility for forward chaining.

This module uses such a facility, written by Tim Finin called PFC, which he has implemented in standard Prolog. The PFC  system is a package that provides a forward reasoning capability to be used together with conventional Prolog programs. The PFC  inference rules are Prolog terms which are asserted as facts into the regular Prolog database.

The PFC  system package provides a forward reasoning capability to be used together with conventional Prolog programs. The PFC  inference rules are Prolog terms which are asserted as clauses into the regular Prolog database. When new facts or forward reasoning rules are added to the Prolog database (via a special predicate pfc_add/1, forward reasoning is triggered and additional facts that can be deduced via the application of the forward chaining rules are also added to the database. A simple justification-based truth-maintenance system is provided as well as simple predicates to explore the resulting proof trees.   Additionally this module provides the user with various methods for trying out new techniques of backwards chaining without rewriting their code.

# FIRST ORDER LOGIC 
Despite Prolog's logic heritage and its use of model elimination in linear logic theorem-proving in projects such as LeanTAP , Prolog fails to qualify as a full general-purpose theorem-proving system. There are three main reasons: (1) Prolog is a programming language and not an inference engine so if we used  the unification algorithm of Prolog for FOL, it is unsound,  (2) Prolog's unbounded depth-first search strategy is inefficient when it is doing complete search, and (3) Prolog's inference system is not complete for non-Horn clauses. Nevertheless, Prolog is quite interesting from a theorem-proving standpoint because of its very high inference rate as compared to conventional theorem-proving programs. 

Logicmoo’s use of the Prolog Technology Theorem Prover (PTTP) was to overcome the deficiencies while retaining as fully as possible the high performance of well-engineered Prolog systems.

PTTP is an implementation of the model elimination theorem-proving procedure that extends Prolog to the full first-order predicate calculus. PTTP differs from Prolog in its use of (1) unification with the occurs check for soundness, (2) depth-first iterative deepening search instead of unbounded depth-first search to make the search strategy complete, and (3) the model elimination inference rule that is added to Prolog inferences to make the inference system complete. PTTP also extends Prolog by providing the capability of printing the proofs it finds. Because PTTP compiles the clauses of a problem, its inference rate is very high. Because PTTP uses depth-first search, its storage requirements are low and term size need not be limited to reduce memory usage at the expense of completeness. PTTP's simple architecture facilitates its adaptation and use in applications.

# PFC Backward-Chaining Rules
Pfc includes a special kind of backward chaining rule which is used to generate all possible solutions to a goal that is sought in the process of forward chaining.     


# Defining new Forward chaining/backchaining rules..

````
spouse(X,Y) <==> spouse(Y,X).
spouse(X,Y),gender(X,G1), {otherGender(G1,G2)}
     ==> gender(Y,G2).
gender(P,male) <==> male(P).
gender(P,female) <==> female(P).
parent(X,Y),female(X) <==> mother(X,Y).
parent(X,Y),parent(Y,Z) ==> grandparent(X,Z).
grandparent(X,Y),male(X) <==> grandfather(X,Y).
grandparent(X,Y),female(X) <==> grandmother(X,Y).
mother(Ma,Kid),parent(Kid,GrandKid)
      ==>grandmother(Ma,GrandKid).
grandparent(X,Y),female(X) <==> grandmother(X,Y).
parent(X,Y),male(X) <==> father(X,Y).
mother(Ma,X),mother(Ma,Y),{X\==Y}
     ==>sibling(X,Y).

````




# Synopsis

An inference engine is a computer program that tries to derive answers from a knowledge base.  The LogicMOO inference engine (like Cyc) performs general logical deduction (including modus ponens, modus tollens, universal quantification and existential quantification).
````

% this means that both P and Q can't be true.
disjoint(P,Q), {current_predciate(_,P),current_predciate(_,Q)}
  ==>
  (P ==> not(Q)),
  (Q ==> not(P)).

````

# Description

The =logicmoo_base= module allows one to use optimal First Order Logic declarations in Prolog code.
During *development*, these declarations log informative information when values don't match
expectations.  A typical development environment converts this into a helpful
stack trace which assists in locating the programing error.

FOL declarations can be give manually by calling pfc_assert/1.  `logicmoo_base` also inserts
predicate Mode declarations for you based on your PlDoc structured comments.  

## Why?

We love First order logic!  That's one reason we love Prolog. But
sometimes we are left disapointed when prolog acts more like a programming language than an inference engine.  

Inference Engines can:

  * offer documentation to those reading our code
  * help find errors during development
  * structure our thinking during development
  * provide a place to persist content tests 


````

% this means that both P and Q can't be true.
disjoint(P,Q), {current_predciate(_,P),current_predciate(_,Q)}
  ==>
  (P ==> not(Q)),
  (Q ==> not(P)).

````

You may have noticed that Logicmoo defines {}/1 as a escape construct for bypassing the FOL's salient body goals. 


As exemplified above, this is the same control construct that is used in grammar rules for bypassing the expansion of rule body goals when a rule is converted into a clause. 
Both control constructs can be combined in order to call a goal from a grammar rule body, while bypassing at the same time the Prolog compiler. Consider the following example:


# Changes in this Version

  * Fix packaging error

# Installation

Using SWI-Prolog 6.3.16 or later:

    $ swipl
    1 ?- pack_install(logicmoo_base).

Source code available and pull requests accepted on GitHub:
https://github.com/TeamSPoon/PrologMUD/tree/master/pack/logicmoo_base
