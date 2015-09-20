# Introduction 



# Synopsis

    :- use_module(library(logicmoo/logicmoo_base)).

    %% 
    even(X) :-
        0 is X mod 2.

# Description

The =logicmoo_base= module allows one to use optimal First Order Logic declarations in Prolog code.
During *development*, these declarations log informative information when values don't match
expectations.  A typical development environment converts this into a helpful
stack trace which assists in locating the programing error.

In *production*, the declarations are completely removed by macros
and do nothing.  Production time
is defined as any time when optimization is enabled:
`current_prolog_flag(optimise, true)`.

FOL declarations can be give manually by calling pfc_assert/1.  `logicmoo_base` also inserts
predicate Mode declarations for you based on your PlDoc structured comments.  For
example, during development, the definition of `even` above becomes

    even(A) :-
        must(the(integer, A),
        0 is A mod 2.

## Why?

We love First order logic!  That's one reason we love Prolog. But
sometimes we are left disapointed when prolog acts more like a programming language than an inference engine.  

Inference Engines can:

  * offer documentation to those reading our code
  * help find errors during development
  * structure our thinking during development
  * provide a place to persist content tests
  * 

# Defining new Forward chaining/backchaining rules..


You may have noticed that Logicmoo defines {}/1 as a escape construct for bypassing the FOL's salient body goals. 


As exemplified above, this is the same control construct that is used in grammar rules for bypassing the expansion of rule body goals when a rule is converted into a clause. Both control constructs can be combined in order to call a goal from a grammar rule body, while bypassing at the same time the Logtalk compiler. Consider the following example:


Mavis fols are defined using error:has_fol/2. We might define an
`even_integer` FOL with

    error:has_fol(even_integer, X) :-
        0 is X mod 2.

We can use the definition manually:

    frobnify(A, B) :-
        the(integer, A),
        the(even_integer, B),
        B is 2*A.

or simply add it to our PlDoc comments:

    %% frobnify(+A:integer, -B:even_integer)
    frobnify(A, B) :-
        B is 2*A.

We can declare fols for bound variables, like `A`, and
not-yet-bound variables, like `B`. The FOL constraints are implemented
with when/2 so they apply as soon as a variable is ground.

To disable FOL checking in production, start Prolog with the
`-O` command line argument. A macro eliminates calls to the/2 so they
have no runtime overhead.

# Changes in this Version

  * Fix packaging error

# Installation

Using SWI-Prolog 6.3.16 or later:

    $ swipl
    1 ?- pack_install(logicmoo_base).

Source code available and pull requests accepted on GitHub:
https://github.com/mndrix/logicmoo_base
