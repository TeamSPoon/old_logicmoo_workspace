/*
    Author:        Ulrich Neumerkel
    E-mail:        ulrich@complang.tuwien.ac.at
    Copyright (C): 2009 Ulrich Neumerkel. All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY Ulrich Neumerkel ``AS IS'' AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL Ulrich Neumerkel OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

The views and conclusions contained in the software and documentation
are those of the authors and should not be interpreted as representing
official policies, either expressed or implied, of Ulrich Neumerkel.



*/
:- if( ( \+ current_prolog_flag(xref,true))).
:- if( \+ current_module(rec_lambda)).
:- if( \+ current_predicate(memberchk_same/2)).

:- module(rec_lambda, [
		   (^)/3, (^)/4, (^)/5, (^)/6, (^)/7, (^)/8, (^)/9,
		 %  ( \)/1, ( \)/2, ( \)/3, ( \)/4, ( \)/5, ( \)/6, ( \)/7,
		   (+\)/2, (+\)/3, (+\)/4, (+\)/5, (+\)/6, (+\)/7,
                   (reenter_lambda)/0, (reenter_lambda)/1, (reenter_lambda)/2, (reenter_lambda)/3, (reenter_lambda)/4, (reenter_lambda)/5, (reenter_lambda)/6,
                   (lambda)/3,(lambda)/5,(lambda)/7,(lambda)/9,
                   ctn/2,
		   op(201,xfx, ( +\ )) ]).




/* Part of LogicMOO Base Lambda expressions

This library provides lambda expressions to simplify higher order
programming based on call/N.

Lambda expressions are represented by ordinary Prolog terms.
There are two kinds of lambda expressions:

    Free+\X1^X2^ ..^XN^Goal

         \X1^X2^ ..^XN^Goal

The second is a shorthand for t+\X1^X2^..^XN^Goal.

Xi are the parameters.

Goal is a goal or continuation. Syntax note: Operators within Goal
require parentheses due to the low precedence of the ^ operator.

Free contains variables that are valid outside the scope of the lambda
expression. They are thus free variables within.

All other variables of Goal are considered local variables. They must
not appear outside the lambda expression. This restriction is
currently not checked. Violations may lead to unexpected bindings.

In the following example the parentheses around X>3 are necessary.

==
?- use_module(library(lambda)).
?- use_module(library(apply)).

?- maplist( \X^(X>3),[4,5,9]).
true.
==

In the following X is a variable that is shared by both instances of
the lambda expression. The second query illustrates the cooperation of
continuations and lambdas. The lambda expression is in this case a
continuation expecting a further argument.

==
?- Xs = [A,B], maplist(X+\Y^dif(X,Y), Xs).
Xs = [A, B],
dif(X, A),
dif(X, B).

?- Xs = [A,B], maplist(X+\dif(X), Xs).
Xs = [A, B],
dif(X, A),
dif(X, B).
==

The following queries are all equivalent. To see this, use
the fact f(x,y).
==
?- call(f,A1,A2).
?- call( \X^f(X),A1,A2).
?- call( \X^Y^f(X,Y), A1,A2).
?- call( \X^(X+\Y^f(X,Y)), A1,A2).
?- call(call(f, A1),A2).
?- call(f(A1),A2).
?- f(A1,A2).
A1 = x,
A2 = y.
==

Further discussions
http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/ISO-Hiord

@tbd Static expansion similar to apply_macros.
@author Ulrich Neumerkel
*/


:- meta_predicate ctn(?,?).
%ctn(I,O):-copy_term_nat(I,O).
ctn(I,O):-copy_term(I,O).


:- meta_predicate
	reenter_lambda,
	reenter_lambda(?),
	reenter_lambda(?,?),
	reenter_lambda(?,?,?),
	reenter_lambda(?,?,?,?),
	reenter_lambda(?,?,?,?,?),
	reenter_lambda(?,?,?,?,?,?).



:- discontiguous(rec_lambda:term_expansion/2).

term_expansion((H :- (recval(A,FC),B)), (H:-nb_current(Key,FC),!,B)):- functor(H,(reenter_lambda),A),atom_concat('$rec_lambda_',A,Key),!.

reenter_lambda :-recval(0,FC),call(FC).
reenter_lambda(V1):-recval(1,FC),call(FC,V1).
reenter_lambda(V1,V2):-recval(2,FC),call(FC,V1,V2).
reenter_lambda(V1,V2,V3):-recval(3,FC),call(FC,V1,V2,V3).
reenter_lambda(V1,V2,V3,V4):-recval(4,FC),call(FC,V1,V2,V3,V4).
reenter_lambda(V1,V2,V3,V4,V5):-recval(5,FC),call(FC,V1,V2,V3,V4,V5).
reenter_lambda(V1,V2,V3,V4,V5,V6):-recval(6,FC),call(FC,V1,V2,V3,V4,V5,V6).



lambda(T1,         FC,A1):-           H=lambda(T1,FC), copy_term(H,TC), b_setval('$rec_lambda_1',TC),  copy_term(H,lambda(A1,Body)),call(Body).
lambda(T1,T2,      FC,A1,A2):-        H=lambda(T1,T2,FC), copy_term(H,TC), b_setval('$rec_lambda_2',TC),  copy_term(H,lambda(A1,A2,Body)),call(Body).
lambda(T1,T2,T3,   FC,A1,A2,A3):-     H=lambda(T1,T2,T3,FC), copy_term(H,TC), b_setval('$rec_lambda_3',TC),  copy_term(H,lambda(A1,A2,A3,Body)),call(Body).
lambda(T1,T2,T3,T4,FC,A1,A2,A3,A4):-  H=lambda(T1,T2,T3,T4,FC), copy_term(H,TC), b_setval('$rec_lambda_4',TC),  copy_term(H,lambda(A1,A2,A3,A4,Body)),call(Body).



:- meta_predicate no_hat_call(0).

:- meta_predicate
	^(?,0,?),
	^(?,1,?,?),
	^(?,2,?,?,?),
	^(?,3,?,?,?,?),
	^(?,4,?,?,?,?,?),
	^(?,5,?,?,?,?,?,?),
	^(?,6,?,?,?,?,?,?,?).




term_expansion((H:-B),(H :- (ctn(HC,HCC),b_setval(Key,HCC),B))):-functor(H,(^),A2),A is A2-2,H=..[F,V1,Goal|_],HC=..[F,V1,Goal],atom_concat('$rec_lambda_',A,Key),!.

% ^(V1,Goal,V1) :-  b_setval('$rec_lambda_1',^(V1,Goal)), no_hat_call(Goal).
^(V1,Goal,V1) :-  
   no_hat_call(Goal).
^(V1,Goal,V1,V2) :-
   call(Goal,V2).
^(V1,Goal,V1,V2,V3) :-
   call(Goal,V2,V3).
^(V1,Goal,V1,V2,V3,V4) :-
   call(Goal,V2,V3,V4).
^(V1,Goal,V1,V2,V3,V4,V5) :-
   call(Goal,V2,V3,V4,V5).
^(V1,Goal,V1,V2,V3,V4,V5,V6) :-
   call(Goal,V2,V3,V4,V5,V6).
^(V1,Goal,V1,V2,V3,V4,V5,V6,V7) :-
   call(Goal,V2,V3,V4,V5,V6,V7).


:- meta_predicate
	\(0),
	\(1,?),
	\(2,?,?),
	\(3,?,?,?),
	\(4,?,?,?,?),
	\(5,?,?,?,?,?),
	\(6,?,?,?,?,?,?).

term_expansion((H:-B),(H :- (ctn(HC,HCC),b_setval(Key,HCC),B))):-functor(H,( \),A1),A is A1-1, atom_concat('$rec_lambda_',A,Key),H=..[F,FC|_],HC=..[F,FC],!.

\(FC) :-
   ctn(FC,C),no_hat_call(C).
\(FC,V1) :-
   ctn(FC,C),call(C,V1).
\(FC,V1,V2) :-
   ctn(FC,C),call(C,V1,V2).
\(FC,V1,V2,V3) :-
   ctn(FC,C),call(C,V1,V2,V3).
\(FC,V1,V2,V3,V4) :-
   ctn(FC,C),call(C,V1,V2,V3,V4).
\(FC,V1,V2,V3,V4,V5) :-
   ctn(FC,C),call(C,V1,V2,V3,V4,V5).
\(FC,V1,V2,V3,V4,V5,V6) :-
   ctn(FC,C),call(C,V1,V2,V3,V4,V5,V6).

:- meta_predicate
	+\(?,0),
	+\(?,1,?),
	+\(?,2,?,?),
	+\(?,3,?,?,?),
	+\(?,4,?,?,?,?),
	+\(?,5,?,?,?,?,?),
	+\(?,6,?,?,?,?,?,?).

term_expansion((H:-B),(H :- (ctn(HC,HCC),b_setval(Key,HCC),B))):-functor(H,(+\),A2),A is A2-2, atom_concat('$rec_lambda_',A,Key),H=..[F,GV,FC|_],HC=..[F,GV,FC],!.

+\(GV,FC) :-
   ctn(GV+FC,GV+C),no_hat_call(C).
+\(GV,FC,V1) :-
   ctn(GV+FC,GV+C),call(C,V1).
+\(GV,FC,V1,V2) :-
   ctn(GV+FC,GV+C),call(C,V1,V2).
+\(GV,FC,V1,V2,V3) :-
   ctn(GV+FC,GV+C),call(C,V1,V2,V3).
+\(GV,FC,V1,V2,V3,V4) :-
   ctn(GV+FC,GV+C),call(C,V1,V2,V3,V4).
+\(GV,FC,V1,V2,V3,V4,V5) :-
   ctn(GV+FC,GV+C),call(C,V1,V2,V3,V4,V5).
+\(GV,FC,V1,V2,V3,V4,V5,V6) :-
   ctn(GV+FC,GV+C),call(C,V1,V2,V3,V4,V5,V6).


%% no_hat_call(:Goal)
%
% Like call, but issues an error for a goal (^)/2.  Such goals are
% likely the result of an insufficient number of arguments.

no_hat_call(MGoal) :-
   strip_module(MGoal, _, Goal),
   (  nonvar(Goal),
      Goal = (_^_)
   -> throw(error(existence_error(lambda_parameters,Goal),_))
   ;  call(MGoal)
   ).

% I would like to replace this by:
% V1^Goal :- throw(error(existence_error(lambda_parameters,V1^Goal),_)).

:- endif.
:- endif.
:- endif.

% :- abolish( cyc: (/)/2).

