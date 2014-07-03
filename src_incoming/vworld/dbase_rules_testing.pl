:- discontiguous(pttp_test/2).


do_pttp_test(TestName):- forall(pttp_test(TestName,Data),
                                (                                 
                                 dmsg(do_pttp_test(TestName)),
                                 eraseall(int_query,_),
                                 /*
                                 eraseall(int_not_firstOrder,_),
                                 eraseall(not_firstOrder,_),
                                 eraseall(int_firstOrder,_),
                                 eraseall(firstOrder,_),*/
                                 once(pttp_assert(Data)),once((ignore(call_print_tf(prove_timed(TestName,query))),sleep(1))))),
                              listing(took).

prove_timed(TestName,A):- 
   statistics(cputime, D),
        (   prove(A)
        ->  B=success
        ;   B=failure
        ),
        statistics(cputime, C),
        F is C-D,
        assert(took(TestName,B,F)),!,
        B=success.


call_print_tf(G):- G,dmsg(succceeded(G)).
call_print_tf(G):- dmsg(failed_finally(G)),sleep(5).


unimplemented:-throw(unimplemented).

pttp_assert_int([Y]):-pttp_assert_int(Y),!.
pttp_assert_int([Y|L]):-!,pttp_assert_int(Y),pttp_assert_int(L),!.

%firstOrder -> holds_t
%not_firstOrder -> holds_f
%int_firstOrder -> dbase_t
%int_not_firstOrder -> dbase_f

pttp_assert_int(Y):- assertz_if_new(Y). % , copy_term(Y,YY),numbervars(YY),dmsg(pttp_assert_int(YY)),!.

pttp_assert_int(A,_) :-				% 2-ary predicate for use as
	pttp_assert_int(A).			% apply_to_conjuncts argument

:- export int_query/6, int_query/7.
:- thread_local(is_query_functor/1).
:- thread_local int_query/6.
:- thread_local int_query/7.

%eraseall(M:F,A):-!,functor(C,F,A),forall(clause(M:C,_,X),erase(X)).
%eraseall(F,A):-functor(C,F,A),forall(clause(C,_,X),erase(X)).

is_query_functor(query).

is_query_lit(Q):- functor(Q,F,_),atom_concat('quer',_,F).


get_int_query(Int_query):- is_query_functor(X),atom_concat('int_',X,Int_query).

pttp_query(Y):- term_variables(Y,Vars),gensym(query_pttp,Q),Z=..[Q|Vars],
    atom_concat('int_',Q,Int_query).
    with_assertions(is_query_functor(Q), (pttp_assert_int(((Z:-Y))), prove(Int_query))).

:-export(pttp_query/1).
:-export(pttp_assert/1).
:-export(pttp_assert_int/1).


/*
pttp_assert(X) :-
	timed_call(pttp1(X,Y),'PTTP to Prolog translation'),
	!,
	timed_call(dpttp2(Y),'Prolog compilation'),
	!.
*/





pttp_test(logicmoo_example1,
	((
          motherOf(joe,sue),
          (motherOf(X,Y) => female(Y)),
          (sonOf(Y,X) => (motherOf(X,Y);fatherOf(X,Y))),          
          (query:-female(Y))
	))).


logicmoo_example1_holds:-xray_test(logicmoo_example1_holds).

pttp_test(logicmoo_example1_holds,
	((
          firstOrder(motherOf,joe,sue),
          (firstOrder(motherOf,X,Y) => firstOrder(female,Y)),
          (firstOrder(sonOf,Y,X) => (firstOrder(motherOf,X,Y);firstOrder(fatherOf,X,Y))),          
          (query:-firstOrder(female,Y))
	))).


logicmoo_example2:-xray_test(logicmoo_example2).

%  int_kbholds(sonOf,gun,phil,A,B,C,C,D,E,F):-D=[G,[1,F,A,B]|H],E=[G|H].
%  int_not_kbholds(female,gun,A,B,C,C,D,E,F):-D=[G,[-2,F,A,B]|H],E=[G|H].
%  int_query(A,B,C,D,E,F,G):- (E=[H,[3,query,A,B]|I],J=[H|I]),firstOrder(K,phil,gun,A,B,C,D,J,F).
%  firstOrder(A,B,C,D,E,F,G,H,I):-J=firstOrder(A,B,C), (identical_member(J,D)->fail; (identical_member(J,E),!;unifiable_member(J,E)),G=F,H=[K,[red,J,D,E]|L],I=[K|L];int_kbholds(A,B,C,D,E,F,G,H,I,J)).
%  not_kbholds(A,B,C,D,E,F,G,H):-I=firstOrder(A,B), (identical_member(I,D)->fail; (identical_member(I,C),!;unifiable_member(I,C)),F=E,G=[J,[redn,I,C,D]|K],H=[J|K];int_not_kbholds(A,B,C,D,E,F,G,H,I)).
pttp_test(logicmoo_example2,
	((
          firstOrder(sonOf,gun,phil),
          not(firstOrder(female,gun)),
          (query:-firstOrder(What,phil,gun))
          % What = fatherOf
	))).


logicmoo_example3:-xray_test(logicmoo_example3).

pttp_test(logicmoo_example3_will_fail,
	((
          (secondOrder(genls,SubClass,SuperClass) & firstOrder(SubClass,Instance) => firstOrder(SuperClass,Instance)),
          (secondOrder(genlPreds,P1,P2) & firstOrder(P1,A) => firstOrder(P2,A)),
          (secondOrder(genlPreds,P1,P2) & firstOrder(P1,A,B) => firstOrder(P2,A,B)),
          (secondOrder(genlPreds,P1,P2) & firstOrder(P1,A,B,C) => firstOrder(P2,A,B,C)),
          % (secondOrder(genlPreds,P1,P2) & firstOrder(P1,A,B,C,D) => firstOrder(P2,A,B,C,D)),
          (secondOrder(genlInverse,P1,P2) & firstOrder(P1,A,B) => firstOrder(P2,B,A)),

            (firstOrder(P2,A,B,C,D):-secondOrder(genlPreds,P1,P2) , firstOrder(P1,A,B,C,D) ),
           (~firstOrder(P,B,A) :- secondOrder(irreflexive,P) & firstOrder(P,A,B)),
           % (secondOrder(irreflexive,P) & firstOrder(P,A,B) => ~ firstOrder(P,B,A)),

           (~ secondOrder(P,A,B)<=> ~firstOrder(P,A,B)),
           secondOrder(genlInverse,parentOf,sonOf),
         %  secondOrder(arg1Isa,genlInverse
           secondOrder(genlPreds,motherOf,parentOf),
           
           secondOrder(irreflexive,sonOf),           

          (query:- not(firstOrder(sonOf,gun,phil)))
          % Expected true
	))).

          % (all X:( all Y : (motherOf(X,Y)) => (bellyButton(X) , older(X,Y) , female(Y)) )),


% pttp_test(_,_):-!,fail.

pttp_test(chang_lee_example1,
	((
		p(g(X,Y),X,Y),
		p(X,h(X,Y),Y),
		(p(U,Z,W) :- p(X,Y,U) , p(Y,Z,V) , p(X,V,W)),
		(p(X,V,W) :- p(X,Y,U) , p(Y,Z,V) , p(U,Z,W)),
		(query :- p(k(X),X,k(X)))
	))).

%%% ***
%%% ****f* PTTP_Examples/chang_lee_example2
%%% DESCRIPTION
%%%   In an associative system with an identity element,
%%%   if the square of every element is the identity,
%%%   the system is commutative.
%%% NOTES
%%%   this is problem GRP001-5 in TPTP
%%% SOURCE

pttp_test(chang_lee_example2,
	((
		p(e,X,X),
		p(X,e,X),
		p(X,X,e),
		p(a,b,c),
		(p(U,Z,W) :- p(X,Y,U) , p(Y,Z,V) , p(X,V,W)),
		(p(X,V,W) :- p(X,Y,U) , p(Y,Z,V) , p(U,Z,W)),
		(query :- p(b,a,c))
	))).

%%% ***
%%% ****f* PTTP_Examples/chang_lee_example3
%%% DESCRIPTION
%%%   In a group the left identity is also a right identity.
%%% NOTES
%%%   this is problem GRP003-1 in TPTP
%%% SOURCE

pttp_test(chang_lee_example3,
	((
          p(e,X,X),
          p(i(X),X,e),
          (p(U,Z,W) :- p(X,Y,U) , p(Y,Z,V) , p(X,V,W)),
          (p(X,V,W) :- p(X,Y,U) , p(Y,Z,V) , p(U,Z,W)),
          (query :- p(a,e,a))
	))).


%%% ***
%%% ****f* PTTP_Examples/chang_lee_example4
%%% DESCRIPTION
%%%   In a group with left inverse and left identity
%%%   every element has a right inverse.
%%% NOTES
%%%   this is problem GRP004-1 in TPTP
%%% SOURCE
pttp_test(chang_lee_example4,
	((
          p(e,X,X),
          p(i(X),X,e),
          (p(U,Z,W) :- p(X,Y,U) , p(Y,Z,V) , p(X,V,W)),
          (p(X,V,W) :- p(X,Y,U) , p(Y,Z,V) , p(U,Z,W)),
          (query :- p(a,X,e))
	))).


%%% ***
%%% ****f* PTTP_Examples/chang_lee_example5
%%% DESCRIPTION
%%%   If S is a nonempty subset of a group such that
%%%   if x,y belong to S, then x*inv(y) belongs to S,
%%%   then the identity e belongs to S.
%%% NOTES
%%%   this is problem GRP005-1 in TPTP
%%% SOURCE

pttp_test(chang_lee_example5,
	((
		p(e,X,X),
		p(X,e,X),
		p(X,i(X),e),
		p(i(X),X,e),
		s(a),
		(s(Z) :- s(X) , s(Y) , p(X,i(Y),Z)),
		(p(U,Z,W) :- p(X,Y,U) , p(Y,Z,V) , p(X,V,W)),
		(p(X,V,W) :- p(X,Y,U) , p(Y,Z,V) , p(U,Z,W)),
		(query :- s(e))
	))).

%%% ***
%%% ****f* PTTP_Examples/chang_lee_example6
%%% DESCRIPTION
%%%   If S is a nonempty subset of a group such that
%%%   if x,y belong to S, then x*inv(y) belongs to S,
%%%   then S contains inv(x) whenever it contains x.
%%% NOTES
%%%   this is problem GRP006-1 in TPTP
%%% SOURCE

pttp_test(chang_lee_example6,
	((
		p(e,X,X),
		p(X,e,X),
		p(X,i(X),e),
		p(i(X),X,e),
		s(b),
		(s(Z) :- s(X) , s(Y) , p(X,i(Y),Z)),
		(p(U,Z,W) :- p(X,Y,U) , p(Y,Z,V) , p(X,V,W)),
		(p(X,V,W) :- p(X,Y,U) , p(Y,Z,V) , p(U,Z,W)),
		(query :- s(i(b)))
	))).

%%% ***
%%% ****f* PTTP_Examples/chang_lee_example7
%%% DESCRIPTION
%%%   If a is a prime and a = b*b/c*c then a divides b.
%%% NOTES
%%%   this is problem NUM014-1 in TPTP
%%%
%%%   this problem is non-Horn
%%%   so clauses are written in disjunction form to
%%%   result in generation of all contrapositives
%%%
%%%   because the query is ground, it is unnecessary
%%%   for its negation to be included
%%% SEE ALSO
%%%   chang_lee_example1, chang_lee_example8
%%% SOURCE

pttp_test(chang_lee_example7,
	((
		p(a),
		m(a,s(c),s(b)),
		m(X,X,s(X)),
		(not_m(X,Y,Z) ; m(Y,X,Z)),
		(not_m(X,Y,Z) ; d(X,Z)),
		(not_p(X) ; not_m(Y,Z,U) ; not_d(X,U) ; d(X,Y) ; d(X,Z)),
		(query :- d(a,b))
	))).

%%% ***
%%% ****f* PTTP_Examples/chang_lee_example8
%%% DESCRIPTION
%%%    Any number greater than one has a prime divisor.
%%% NOTES
%%%   this is problem NUM015-1 in TPTP
%%%
%%%   this problem is non-Horn
%%%   so clauses are written in disjunction form to
%%%   result in generation of all contrapositives
%%%
%%%   the negation of the query is included
%%%   to allow multiple instances to be used in
%%%   the proof (and yield an indefinite answer)
%%% SEE ALSO
%%%   chang_lee_example1, chang_lee_example7
%%% SOURCE

pttp_test(chang_lee_example8,
	((
		l(1,a),
		d(X,X),
		(p(X) ; d(g(X),X)),
		(p(X) ; l(1,g(X))),
		(p(X) ; l(g(X),X)),
		(not_p(X) ; not_d(X,a)),		% negation of query
		(not_d(X,Y) ; not_d(Y,Z) ; d(X,Z)),
		(not_l(1,X) ; not_l(X,a) ; p(f(X))),
		(not_l(1,X) ; not_l(X,a) ; d(f(X),X)),
		(query :- (p(X) , d(X,a)))
	))).

%%% ***
%%% ****f* PTTP_Examples/chang_lee_example9
%%% DESCRIPTION
%%%   There exist infinitely many primes.
%%% NOTES
%%%   this is problem NUM016-2 in TPTP
%%% SOURCE

pttp_test(chang_lee_example9,
	((
		l(X,f(X)),
		not_l(X,X),
		(not_l(X,Y) ; not_l(Y,X)),
		(not_d(X,f(Y)) ; l(Y,X)),
		(p(X) ; d(h(X),X)),
		(p(X) ; p(h(X))),
		(p(X) ; l(h(X),X)),
		(not_p(X) ; not_l(a,X) ; l(f(a),X)),	% negation of query
		(query :- p(X) , l(a,X) , not_l(f(a),X))
	))).

%%% ***
%%% ****f* PTTP_Examples/overbeek_example4
%%% DESCRIPTION
%%%   Show that Kalman's shortest single axiom for the
%%%   equivalential calculus, XGK, can be derived from the
%%%   Meredith single axiom PYO.
%%% NOTES
%%%   a harder problem than the Chang and Lee examples
%%%   from Overbeek's competition problems
%%%
%%%   this is problem LCL024-1 in TPTP
%%% SOURCE

pttp_test(_,_):-!,fail.


pttp_test(overbeek_example4,	
	((
		p(e(X,e(e(Y,e(Z,X)),e(Z,Y)))),
		(p(Y) :- p(e(X,Y)), p(X)),
		(queryXXX :- p(e(e(e(a,e(b,c)),c),e(b,a)))),
                query:-prove(queryXXX,100,0,2)
	))).

% overbeek_example4 :- prove(query,100,0,2).	% cost 30 proof
%%% ***



chang_lee_example2x :-
        nl,
        write(chang_lee_example2),
        pttp_assert((
                p(e,X,X),
                p(X,e,X),
                p(X,X,e),
                p(a,b,c),
                (p(U,Z,W) :- p(X,Y,U) , p(Y,Z,V) , p(X,V,W)),
                (p(X,V,W) :- p(X,Y,U) , p(Y,Z,V) , p(U,Z,W)),
                (query :- prove(p(b,a,c)))
        )),
   time(prove(query)).

chang_lee_example8x :-
        nl,
        write(chang_lee_example8),
        pttp_assert((
                l(1,a),
                d(X,X),
                (p(X) ; d(g(X),X)),
                (p(X) ; l(1,g(X))),
                (p(X) ; l(g(X),X)),
                (not_p(X) ; not_d(X,a)),
                (not_d(X,Y) ; not_d(Y,Z) ; d(X,Z)),
                (not_l(1,X) ; not_l(X,a) ; p(f(X))),
                (not_l(1,X) ; not_l(X,a) ; d(f(X),X)),
                (query :- (p(X) , d(X,a)))
        )),
   time(prove(query)).




%%% ****f* PTTP_Examples/chang_lee_example1
%%% DESCRIPTION
%%%   Prove that in an associative system with left and right
%%%   solutions, there is a right identity element.
%%% NOTES
%%%   this is problem GRP028-4 in TPTP
%%%
%%%   this and the other chang_lee examples are taken from
%%%   C.L. Chang and R.C.T. Lee,
%%%   Symbolic Logic and Mechanical Theorem Proving,
%%%   Academic Press, New York, 1973, pp. 298-305.
%%%
%%%   the result of executing these examples
%%%   can be seen in the file pttp_assert-examples.typescript
%%%
%%%   this problem contains only Horn clauses
%%%   (clauses with at most one positive literal)
%%%   so neither contrapositives nor more than one
%%%   instance of an all negative query are required
%%%   for completeness
%%%
%%%   so clauses are written in implication form to
%%%   suppress generation of unnecessary contrapositives
%%%   and the negation of the query is not included
%%% SEE ALSO
%%%   chang_lee_example7, chang_lee_example8
%%% SOURCE

chang_lee_example1 :-
	pttp_assert((
		p(g(X,Y),X,Y),
		p(X,h(X,Y),Y),
		(p(U,Z,W) :- p(X,Y,U) , p(Y,Z,V) , p(X,V,W)),
		(p(X,V,W) :- p(X,Y,U) , p(Y,Z,V) , p(U,Z,W)),
		(query :- p(k(X),X,k(X)))
	)),
	fail.				% clear stack used in compilation
chang_lee_example1 :-
	time(prove(query)).			% run query with fresh stack
%%% ***
%%% ****f* PTTP_Examples/chang_lee_example2
%%% DESCRIPTION
%%%   In an associative system with an identity element,
%%%   if the square of every element is the identity,
%%%   the system is commutative.
%%% NOTES
%%%   this is problem GRP001-5 in TPTP
%%% SOURCE

chang_lee_example2 :-
	pttp_assert((
		p(e,X,X),
		p(X,e,X),
		p(X,X,e),
		p(a,b,c),
		(p(U,Z,W) :- p(X,Y,U) , p(Y,Z,V) , p(X,V,W)),
		(p(X,V,W) :- p(X,Y,U) , p(Y,Z,V) , p(U,Z,W)),
		(query :- p(b,a,c))
	)),
	fail.
chang_lee_example2 :-
	time(prove(query)).
%%% ***
%%% ****f* PTTP_Examples/chang_lee_example3
%%% DESCRIPTION
%%%   In a group the left identity is also a right identity.
%%% NOTES
%%%   this is problem GRP003-1 in TPTP
%%% SOURCE

chang_lee_example3 :-
	pttp_assert((
		p(e,X,X),
		p(i(X),X,e),
		(p(U,Z,W) :- p(X,Y,U) , p(Y,Z,V) , p(X,V,W)),
		(p(X,V,W) :- p(X,Y,U) , p(Y,Z,V) , p(U,Z,W)),
		(query :- p(a,e,a))
	)),
	fail.
chang_lee_example3 :-
	time(prove(query)).
%%% ***
%%% ****f* PTTP_Examples/chang_lee_example4
%%% DESCRIPTION
%%%   In a group with left inverse and left identity
%%%   every element has a right inverse.
%%% NOTES
%%%   this is problem GRP004-1 in TPTP
%%% SOURCE

chang_lee_example4 :-
	pttp_assert((
		p(e,X,X),
		p(i(X),X,e),
		(p(U,Z,W) :- p(X,Y,U) , p(Y,Z,V) , p(X,V,W)),
		(p(X,V,W) :- p(X,Y,U) , p(Y,Z,V) , p(U,Z,W)),
		(query :- p(a,X,e))
	)),
	fail.
chang_lee_example4 :-
	time(prove(query)).
%%% ***
%%% ****f* PTTP_Examples/chang_lee_example5
%%% DESCRIPTION
%%%   If S is a nonempty subset of a group such that
%%%   if x,y belong to S, then x*inv(y) belongs to S,
%%%   then the identity e belongs to S.
%%% NOTES
%%%   this is problem GRP005-1 in TPTP
%%% SOURCE

chang_lee_example5 :-
	pttp_assert((
		p(e,X,X),
		p(X,e,X),
		p(X,i(X),e),
		p(i(X),X,e),
		s(a),
		(s(Z) :- s(X) , s(Y) , p(X,i(Y),Z)),
		(p(U,Z,W) :- p(X,Y,U) , p(Y,Z,V) , p(X,V,W)),
		(p(X,V,W) :- p(X,Y,U) , p(Y,Z,V) , p(U,Z,W)),
		(query :- s(e))
	)),
	fail.
chang_lee_example5 :-
	time(prove(query)).
%%% ***
%%% ****f* PTTP_Examples/chang_lee_example6
%%% DESCRIPTION
%%%   If S is a nonempty subset of a group such that
%%%   if x,y belong to S, then x*inv(y) belongs to S,
%%%   then S contains inv(x) whenever it contains x.
%%% NOTES
%%%   this is problem GRP006-1 in TPTP
%%% SOURCE

chang_lee_example6 :-
	pttp_assert((
		p(e,X,X),
		p(X,e,X),
		p(X,i(X),e),
		p(i(X),X,e),
		s(b),
		(s(Z) :- s(X) , s(Y) , p(X,i(Y),Z)),
		(p(U,Z,W) :- p(X,Y,U) , p(Y,Z,V) , p(X,V,W)),
		(p(X,V,W) :- p(X,Y,U) , p(Y,Z,V) , p(U,Z,W)),
		(query :- s(i(b)))
	)),
	fail.
chang_lee_example6 :-
	time(prove(query)).
%%% ***
%%% ****f* PTTP_Examples/chang_lee_example7
%%% DESCRIPTION
%%%   If a is a prime and a = b*b/c*c then a divides b.
%%% NOTES
%%%   this is problem NUM014-1 in TPTP
%%%
%%%   this problem is non-Horn
%%%   so clauses are written in disjunction form to
%%%   result in generation of all contrapositives
%%%
%%%   because the query is ground, it is unnecessary
%%%   for its negation to be included
%%% SEE ALSO
%%%   chang_lee_example1, chang_lee_example8
%%% SOURCE

chang_lee_example7 :-
	pttp_assert((
		p(a),
		m(a,s(c),s(b)),
		m(X,X,s(X)),
		(not_m(X,Y,Z) ; m(Y,X,Z)),
		(not_m(X,Y,Z) ; d(X,Z)),
		(not_p(X) ; not_m(Y,Z,U) ; not_d(X,U) ; d(X,Y) ; d(X,Z)),
		(query :- d(a,b))
	)),
	fail.
chang_lee_example7 :-
	time(prove(query)).
%%% ***
%%% ****f* PTTP_Examples/chang_lee_example8
%%% DESCRIPTION
%%%    Any number greater than one has a prime divisor.
%%% NOTES
%%%   this is problem NUM015-1 in TPTP
%%%
%%%   this problem is non-Horn
%%%   so clauses are written in disjunction form to
%%%   result in generation of all contrapositives
%%%
%%%   the negation of the query is included
%%%   to allow multiple instances to be used in
%%%   the proof (and yield an indefinite answer)
%%% SEE ALSO
%%%   chang_lee_example1, chang_lee_example7
%%% SOURCE

chang_lee_example8 :-
	pttp_assert((
		l(1,a),
		d(X,X),
		(p(X) ; d(g(X),X)),
		(p(X) ; l(1,g(X))),
		(p(X) ; l(g(X),X)),
		(not_p(X) ; not_d(X,a)),		% negation of query
		(not_d(X,Y) ; not_d(Y,Z) ; d(X,Z)),
		(not_l(1,X) ; not_l(X,a) ; p(f(X))),
		(not_l(1,X) ; not_l(X,a) ; d(f(X),X)),
		(query :- (p(X) , d(X,a)))
	)),
	fail.
chang_lee_example8 :-
	time(prove(query)).
%%% ***
%%% ****f* PTTP_Examples/chang_lee_example9
%%% DESCRIPTION
%%%   There exist infinitely many primes.
%%% NOTES
%%%   this is problem NUM016-2 in TPTP
%%% SOURCE

chang_lee_example9 :-
	pttp_assert((
		l(X,f(X)),
		not_l(X,X),
		(not_l(X,Y) ; not_l(Y,X)),
		(not_d(X,f(Y)) ; l(Y,X)),
		(p(X) ; d(h(X),X)),
		(p(X) ; p(h(X))),
		(p(X) ; l(h(X),X)),
		(not_p(X) ; not_l(a,X) ; l(f(a),X)),	% negation of query
		(query :- p(X) , l(a,X) , not_l(f(a),X))
	)),
	fail.
chang_lee_example9 :-
   	time(prove(query)).
%%% ***
%%% ****f* PTTP_Examples/overbeek_example4
%%% DESCRIPTION
%%%   Show that Kalman's shortest single axiom for the
%%%   equivalential calculus, XGK, can be derived from the
%%%   Meredith single axiom PYO.
%%% NOTES
%%%   a harder problem than the Chang and Lee examples
%%%   from Overbeek's competition problems
%%%
%%%   this is problem LCL024-1 in TPTP
%%% SOURCE

overbeek_example4 :-
	pttp_assert((
		p(e(X,e(e(Y,e(Z,X)),e(Z,Y)))),
		(p(Y) :- p(e(X,Y)), p(X)),
		(query :- p(e(e(e(a,e(b,c)),c),e(b,a))))
	)),
	fail.
overbeek_example4 :-
	prove(query,100,0,2).	% cost 30 proof
%%% ***


