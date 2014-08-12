:-module(dbase_rules_pttp,[ nnf/2, pttp1/2,
      op(400,fy,-),    % negation
      op(500,xfy,&),   % conjunction
      op(600,xfy,v),   % disjunction
      op(650,xfy,=>),  % implication
      op(680,xfy,<=>), % equivalence
      op( 500, fy, ~),    % negation
      op( 500, fy, all),  % universal quantifier
      op( 500, fy, ex),   % existential quantifier
  %    op( 500,xfy, :),
       nnf/4,
       pttp_assert/1,
       pttp_test/2,
       do_pttp_test/1
        ]).

:- style_check(+discontiguous).
:- style_check(-singleton).

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
%%%   can be seen in the file pttp-examples.typescript
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


:-include(dbase_rules_nnf).
:-include(dbase_rules_testing).

pttp_assert(X) :-
	timed_call(pttp1(X,Y),'PTTP to Prolog translation'),
	!,
	timed_call(pttp2(Y),'Prolog compilation'),
	!.

   

%%% ****h* PTTP/PTTP
%%% COPYRIGHT
%%%   Copyright (c) 1988-2003 Mark E. Stickel, SRI International, Menlo Park, CA 94025  USA
%%% 
%%%   Permission is hereby granted, free of charge, to any person obtaining a
%%%   copy of this software and associated documentation files (the "Software"),
%%%   to deal in the Software without restriction, including without limitation
%%%   the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%%   and/or sell copies of the Software, and to permit persons to whom the
%%%   Software is furnished to do so, subject to the following conditions:
%%% 
%%%   The above copyright notice and this permission notice shall be included
%%%   in all copies or substantial portions of the Software.
%%% 
%%%   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%%   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%%   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%%%   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
%%%   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%%%   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%%%   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%% DESCRIPTION
%%%  
%%%      A Prolog Technology Theorem Prover
%%%  
%%%               Mark E. Stickel
%%%  
%%%   Prolog is not a full theorem prover
%%%   for three main reasons:
%%%  
%%%     It uses an unsound unification algorithm without
%%%     the occurs check.
%%%  
%%%     Its inference system is complete for Horn
%%%     clauses, but not for more general formulas.
%%%  
%%%     Its unbounded depth-first search strategy
%%%     is incomplete.
%%%  
%%%   Also, it cannot display the proofs it finds.
%%%  
%%%   The Prolog Technology Theorem Prover (PTTP)
%%%   overcomes these limitations by
%%%  
%%%     transforming clauses so that head literals have
%%%     no repeated variables and unification without the
%%%     occurs check is valid; remaining unification
%%%     is done using complete unification with the
%%%     occurs check in the body;
%%%  
%%%     adding contrapositives of clauses (so that
%%%     any literal, not just a distinguished head
%%%     literal, can be resolved on) and the model-
%%%     elimination procedure reduction rule that
%%%     matches goals with the negations of their
%%%     ancestor goals;
%%%  
%%%     using a sequence of bounded depth-first searches
%%%     to prove a theorem;
%%%  
%%%     retaining information on what formulas are
%%%     used for each inference so that the proof
%%%     can be printed.
%%%  
%%%   This version of PTTP translates first-order
%%%   predicate calculus formulas written in a Prolog-like
%%%   notation into Prolog code.  The resulting code
%%%   is then compiled and executed.
%%%  
%%%   PTTP commands:
%%%  
%%%     pttp_assert(formula) - translates the first-order formula
%%%       (normally a conjunction of formulas) into Prolog
%%%       and compiles it
%%%
%%%     prove(formula) - tries to prove formula
%%%
%%%   Look at the description of these functions
%%%   and the examples for more details on how
%%%   pttp_assert and prove should be used.
%%%   For more information on PTTP, consult
%%%     Stickel, M.E.  A Prolog technology theorem prover:
%%%     implementation by an extended Prolog compiler.
%%%     Journal of Automated Reasoning 4, 4 (1988), 353-380.
%%%     and
%%%     Stickel, M.E.  A Prolog technology theorem prover:
%%%     a new exposition and implementation in Prolog.
%%%     Technical Note 464, Artificial Intelligence Center,
%%%     SRI International, Menlo Park, California, June 1989.
%%%
%%%
%%%
%%%   Several arguments are added to each predicate:
%%%   PosAncestors is list of positive ancestor goals
%%%   NegAncestors is list of negative ancestor goals
%%%   DepthIn is depth bound before goal is solved
%%%   DepthOut will be set to remaining depth bound after goal is solved
%%%   ProofIn is dotted-pair difference list of proof so far
%%%   ProofOut will be set to list of steps of proof so far after goal is solved
%%%  
%%%
%%%
%%%   Depth-first iterative-deepening search.
%%%
%%%   PTTP adds arguments DepthIn and DepthOut
%%%   to each PTTP literal to control bounded depth-first
%%%   search.  When a literal is called,
%%%   DepthIn is the current depth bound.  When
%%%   the literal exits, DepthOut is the new number
%%%   of levels remaining after the solution of
%%%   the literal (DepthIn - DepthOut is the number
%%%   of levels used in the solution of the goal.)
%%%
%%%   For clauses with empty bodies or bodies
%%%   composed only of builtin functions,
%%%   DepthIn = DepthOut.
%%%
%%%   For other clauses, the depth bound is
%%%   compared to the cost of the body.  If the
%%%   depth bound is exceeded, the clause fails.
%%%   Otherwise the depth bound is reduced by
%%%   the cost of the body.
%%%
%%%   p :- q , r.
%%%   is transformed into
%%%   p(DepthIn,DepthOut) :-
%%%       DepthIn >= 2, Depth1 is DepthIn - 2,
%%%       q(Depth1,Depth2),
%%%       r(Depth2,DepthOut).
%%%
%%%   p :- q ; r.
%%%   is transformed into
%%%   p(DepthIn,DepthOut) :-
%%%       DepthIn >= 1, Depth1 is DepthIn - 1,
%%%       (q(Depth1,DepthOut) ; r(Depth1,DepthOut)).
%%%
%%%
%%%
%%%   Complete inference.
%%%
%%%   Model elimination reduction operation and
%%%   identical ancestor goal pruning.
%%%
%%%   Two arguments are added to each literal, one
%%%   for all the positive ancestors, one for all
%%%   the negative ancestors.
%%%
%%%   Unifiable membership is checked in the list 
%%%   of opposite polarity to the goal
%%%   for performing the reduction operation.
%%%
%%%   Identity membership is checked in the list
%%%   of same polarity as the goal
%%%   for performing the ancestor goal pruning operation.
%%%   This is not necessary for soundness or completeness,
%%%   but is often effective at substantially reducing the
%%%   number of inferences.
%%%
%%%   The current head goal is added to the front
%%%   of the appropriate ancestor list during the
%%%   call on subgoals in bodies of nonunit clauses.
%%%
%%%
%%%
%%%   Proof Printing.
%%%
%%%   Add extra arguments to each goal so that information
%%%   on what inferences were made in the proof can be printed
%%%   at the end.
%%% ***
%%% ****f* PTTP/pttp
%%% DESCRIPTION
%%%   pttp is the PTTP compiler top-level predicate.
%%%   Its argument is a conjunction of formulas to be compiled.
%%% SOURCE



%%% ***
%%% ****f* PTTP/prove
%%% DESCRIPTION
%%%   prove(Goal) can be used to prove Goal using code compiled by PTTP.
%%%   It uses depth-first iterative-deepening search and prints the proof.
%%%
%%%   Depth-first iterative-deepening search can be controlled
%%%   by extra paramaters of the prove predicate:
%%%      prove(Goal,Max,Min,Inc,ProofIn)
%%%   Max is the maximum depth to search (defaults to a big number),
%%%   Min is the minimum depth to search (defaults to 0),
%%%   Inc is the amount to increment the bound each time (defaults to 1).
%%%   ProofIn specifies initial steps of proof to retrace (defaults to []).
%%%
%%%   A query can be compiled along with the axioms by including the
%%%   clause 'query :- ...'.  The query can then be proved by 'prove(query)'.
%%% SOURCE

prove(Goal,Max,Min,Inc,ProofIn,ProofOut) :- prove_inc(Goal,Max,Min,Inc,ProofIn,ProofOut).

prove_inc(Goal,Max,Min,Inc,ProofIn,ProofOut) :-
	expand_input_proof(ProofIn,PrfEnd),
	PrevInc is Min + 1,
	add_args(Goal,_,_,[],_,_,[],[],DepthIn,DepthOut,[PrfEnd|PrfEnd],ProofOut1,Goal1,_),
	!,
	timed_call(search(Goal1,Max,Min,Inc,PrevInc,DepthIn,DepthOut),'Proof'),
	contract_output_proof(ProofOut1,ProofOut),
	write_proof(ProofOut1),
	nl.

prove(Goal,Max,Min,Inc,ProofIn) :-
	prove(Goal,Max,Min,Inc,ProofIn,_).

prove(Goal,Max,Min,Inc) :-
	prove(Goal,Max,Min,Inc,[],_).

prove(Goal,Max,Min) :-
	prove(Goal,Max,Min,1,[],_).

prove(Goal,Max) :-
	prove(Goal,Max,2,3).

prove(Goal) :- prove(Goal,15).
%prove(Goal) :-  prove(Goal,1000000,0,1,[],_).

%%% ***

%%% ****if* PTTP/linearize
%%% DESCRIPTION
%%%   Prolog's unification operation is unsound for first-order
%%%   reasoning because it lacks the occurs check that would
%%%   block binding a variable to a term that contains the
%%%   variable and creating a circular term.  However, Prolog's
%%%   unification algorithm is sound and the occurs check is
%%%   unnecessary provided the terms being unified have no
%%%   variables in common and at least one of the terms has
%%%   no repeated variables.  A Prolog fact or rule head will
%%%   not have variables in common with the goal.  The linearize
%%%   transformation rewrites a fact or rule so that the fact
%%%   or rule head has no repeated variables and Prolog unification
%%%   can be used safely.  The rest of the unification can then
%%%   be done in the body of the transformed clause, using
%%%   the sound unify predicate.
%%%
%%%   For example,
%%%      p(X,Y,f(X,Y)) :- true.
%%%   is transformed into
%%%      p(X,Y,f(X1,Y1)) :- unify(X,X1), unify(Y,Y1).
%%% SOURCE

linearize(TermIn,TermOut,VarsIn,VarsOut,MatchesIn,MatchesOut) :-
	nonvar(TermIn) ->
		functor(TermIn,F,N),
		pttp_functor(TermOut,F,N),
		linearize_args(TermIn,TermOut,VarsIn,VarsOut,
		               MatchesIn,MatchesOut,1,N);
	identical_member(TermIn,VarsIn) ->
		VarsOut = VarsIn,
		conjoin(MatchesIn,unify(TermIn,TermOut),MatchesOut);
	%true ->
		TermOut = TermIn,
		VarsOut = [TermIn|VarsIn],
		MatchesOut = MatchesIn.

linearize_args(TermIn,TermOut,VarsIn,VarsOut,MatchesIn,MatchesOut,I,N) :-
	I > N ->
		VarsOut = VarsIn,
		MatchesOut = MatchesIn;
	%true ->
		arg(I,TermIn,ArgI),
		linearize(ArgI,NewArgI,VarsIn,Vars1,MatchesIn,Matches1),
		arg(I,TermOut,NewArgI),
		I1 is I + 1,
		linearize_args(TermIn,TermOut,Vars1,VarsOut,Matches1,MatchesOut,I1,N).
%%% ***
%%% ****if* PTTP/unify
%%% DESCRIPTION
%%%   Prolog's unification operation is unsound for first-order
%%%   reasoning because it lacks the occurs check that would
%%%   block binding a variable to a term that contains the
%%%   variable and creating a circular term.  Thus, PTTP
%%%   must provide a sound unfication algorithm with the occurs
%%%   check.
%%%
%%%   unify(X,Y) is similar to Prolog's X=Y, except that operations
%%%   like unify(X,f(X)) fail rather than create circular terms.
%%% SOURCE

unify(X,Y) :- unify_with_occurs_check(X,Y).

%%% ***

%%% ****if* PTTP/test_and_decrement_search_cost_expr
%%% SOURCE

test_and_decrement_search_cost_expr(DepthIn,Cost,Depth1,Expr) :-
	Cost == 0 ->
		Depth1 = DepthIn,
		Expr = true;
	%true ->
		Expr = test_and_decrement_search_cost(DepthIn,Cost,Depth1).

test_and_decrement_search_cost(DepthIn,Cost,Depth1):- DepthIn >= Cost , Depth1 is DepthIn - Cost.

%%% ***
%%% ****if* PTTP/search_cost
%%% DESCRIPTION
%%%   Search cost is ordinarily computed by counting literals in the body.
%%%   It can be given explicitly instead by including a number, as in
%%%     p :- search_cost(3).     (ordinarily, cost would be 0)
%%%     p :- search_cost(1),q,r. (ordinarily, cost would be 2)
%%%     p :- search_cost(0),s.   (ordinarily, cost would be 1)
%%% SOURCE

search_cost(Body,HeadArgs,N) :-
	Body = search_cost(M) ->
		N = M;
	Body = (A , B) ->
		(A = search_cost(M) ->	% if first conjunct is search_cost(M),
			N = M;		% search cost of conjunction is M
		%true ->
			search_cost(A,HeadArgs,N1),
			search_cost(B,HeadArgs,N2),
			N is N1 + N2);
	Body = (A ; B) ->
		search_cost(A,HeadArgs,N1),
		search_cost(B,HeadArgs,N2),
		min(N1,N2,N);
	builtin(Body) ->
		N = 0;
	%true ->
		N = 1.
%%% ***
%%% ****if* PTTP/search
%%% DESCRIPTION
%%%   Search driver predicate.
%%%   Note that depth-bounded execution of Goal is enabled by
%%%   the fact that the DepthIn and DepthOut arguments of
%%%   search are also the DepthIn and DepthOut arguments of Goal.
%%% SOURCE
search(Goal,Max,Min,Inc,PrevInc,DepthIn,DepthOut):-search0(Goal,Max,Min,Inc,PrevInc,DepthIn,DepthOut).

search0(Goal,Max,Min,Inc,PrevInc,DepthIn,DepthOut):- % trace,
        search1(Goal,Max,Min,Inc,PrevInc,DepthIn,DepthOut).

search1(_Goal,Max,Min,_Inc,_PrevInc,_DepthIn,_DepthOut) :-
	Min > Max,
	!,
	fail.
search1(Goal,_Max,Min,_Inc,PrevInc,DepthIn,DepthOut) :-
        write_search_progress(Min),
	DepthIn = Min,
	ccatch(call(Goal),E,trace),
	DepthOut < PrevInc.	% fail if solution found previously
search1(Goal,Max,Min,Inc,_PrevInc,DepthIn,DepthOut) :-
	Min1 is Min + Inc,
	search(Goal,Max,Min1,Inc,Inc,DepthIn,DepthOut).
%%% ***

%%% ****if* PTTP/make_wrapper
%%% SOURCE

make_wrapper(_DefinedPreds,[query,0],true) :-
	!.
make_wrapper(DefinedPreds,[P,N],Result):- must_det(make_wrapper0(DefinedPreds,[P,N],Result)). % ,dmsg(pp(Result)).

make_wrapper0(DefinedPreds,[P,N],Result) :-
	functor(Goal,P,N),
	Goal =.. [P|Args],
	ExtraArgs = [PosAncestors,NegAncestors,DepthIn,DepthOut,ProofIn,ProofOut],
	list_append(Args,ExtraArgs,Args1),
	Head =.. [P|Args1],
	internal_functor(P,IntP),
	list_length(ExtraArgs,NExtraArgs),
	NN is N + NExtraArgs + 1,
	(identical_member([IntP,NN],DefinedPreds) ->
	        list_append(ExtraArgs,[GoalAtom],ExtraArgs2),
		list_append(Args,ExtraArgs2,Args2),
		IntHead =.. [IntP|Args2];
	%true ->
		IntHead = fail),
	(negative_functor(P) ->
		negated_literal(Goal,PosGoal),
		Red = redn,  % atom in proof is negation of actual literal
		C1Ancestors = NegAncestors,
		C2Ancestors = PosAncestors;
	%true ->
		PosGoal = Goal,
		Red = red,
		C1Ancestors = PosAncestors,
		C2Ancestors = NegAncestors),
	(N = 0 ->	% special case for propositional calculus
		V1 = (identical_member(GoalAtom,C2Ancestors) , !);
	%true ->
		V1 = ((identical_member(GoalAtom,C2Ancestors) , !);
		       unifiable_member(GoalAtom,C2Ancestors))),
	V2 = (DepthOut = DepthIn,
		 ProofIn = [Prf,[Red,GoalAtom,PosAncestors,NegAncestors]|PrfEnd],
		 ProofOut = [Prf|PrfEnd]),
	conjoin(V1,V2,Reduce),
	Result = (Head :- GoalAtom = PosGoal,
		  	  (identical_member(GoalAtom,C1Ancestors) ->
			   	fail;
			  %true ->
			   	(Reduce;
				 IntHead))).
%%% ***
%%% ****if* PTTP/query
%%% DESCRIPTION
%%%   query uses the following definition instead of the more complex one
%%%   that would be created by make_wrapper
%%% SOURCE

% 3 = suc(suc(1))
% 3/67 = div(suc(suc(1)),67)


query(PosAncestors,NegAncestors,DepthIn,DepthOut,ProofIn,ProofOut) :- 
  get_int_query(Int_query),
	call(Int_query,PosAncestors,NegAncestors,DepthIn,DepthOut,ProofIn,ProofOut,query).

%%% ***
%%% ****if* PTTP/unifiable_member
%%% DESCRIPTION
%%%   unifiable_member(X,L) succeeds each time X is unifiable with an
%%%   element of the list L
%%% SOURCE

unifiable_member(X,[Y|L]) :- unify(X,Y); unifiable_member(X,L).

%%% ***
%%% ****if* PTTP/identical_member
%%% DESCRIPTION
%%%   identical_member(X,L) succeeds iff X is an element of the list L
%%%   it does not use unification during element comparisons
%%% SOURCE

% from memberchk_eq(X,L).

identical_member(X,[Y|Ys]) :-
	(   X == Y
	->  true
	;   identical_member(X,Ys)
	).

%%% ***

%%% ****if* PTTP/write_proof
%%% DESCRIPTION
%%%   write_proof prints the proof that PTTP finds
%%% SOURCE

write_proof(Proof) :-
	write('Proof:'),
	nl,
	proof_length(Proof,Len),
	write('length = '),
	write(Len),
	write(', '),
	proof_depth(Proof,Depth),
	write('depth = '),
	write(Depth),
	nl,
	write('Goal#  Wff#  Wff Instance'),
	nl,
	write('-----  ----  ------------'),
	add_proof_query_line(Proof,Proof2),
	process_proof(Proof2,0,Proof1),
	write_proof1(Proof1),
	nl,
	write('Proof end.').

write_proof1([]).
write_proof1([[LineNum,X,Head,Depth,Subgoals]|Y]) :-
	nl,
	write_indent_for_number(LineNum),
	write('['),
	write(LineNum),
	write(']  '),
	write_indent_for_number(X),
	write(X),
	write('   '),
	write_proof_indent(Depth),
	write(Head),
	(Subgoals = [] ->
		true;
	%true ->
		write(' :- '),
		write_proof_subgoals(Subgoals)),
	write(.),
	write_proof1(Y).	

write_proof_subgoals([X,Y|Z]) :-
	write('['),
	write(X),
	write('] , '),
	write_proof_subgoals([Y|Z]).
write_proof_subgoals([X]) :-
	write('['),
	write(X),
	write(']').

write_proof_indent(N) :-
	N > 0,
	write('   '),
	N1 is N - 1,
	write_proof_indent(N1).
write_proof_indent(0).

process_proof([Prf|PrfEnd],_LineNum,Result) :-
	Prf == PrfEnd,
	!,
	Result = [].
process_proof([[[X,Head,PosAncestors,NegAncestors]|Y]|PrfEnd],LineNum,Result) :-
	LineNum1 is LineNum + 1,
	process_proof([Y|PrfEnd],LineNum1,P),
	(is_query_lit(Head) ->
		Depth is 0;
	%true ->
		list_length(PosAncestors,N1),	% compute indentation to show
		list_length(NegAncestors,N2),	% level of goal nesting from
		Depth is N1 + N2 + 1),		% lengths of ancestor lists
	Depth1 is Depth + 1,
	collect_proof_subgoals(Depth1,P,Subgoals),
	(X = redn ->
		X1 = red,
		negated_literal(Head,Head1);
	 (number(X) , X < 0) ->
		X1 is - X,
		negated_literal(Head,Head1);
	%true ->
		X1 = X,
		Head1 = Head),
	Result = [[LineNum,X1,Head1,Depth,Subgoals]|P].

collect_proof_subgoals(_Depth1,[],Result) :-
	Result = [].
collect_proof_subgoals(Depth1,[[LineNum,_,_,Depth,_]|P],Result) :-
	Depth = Depth1,
	collect_proof_subgoals(Depth1,P,R),
	Result = [LineNum|R].
collect_proof_subgoals(Depth1,[[_,_,_,Depth,_]|P],Result) :-
	Depth > Depth1,
	collect_proof_subgoals(Depth1,P,Result).
collect_proof_subgoals(Depth1,[[_,_,_,Depth,_]|_],Result) :-
	Depth < Depth1,
	Result = [].

add_proof_query_line(Proof,Proof2) :-
	Proof = [Prf|_PrfEnd],
	nonvar(Prf),
	Prf = [[_,query,_,_]|_],
	!,
	Proof2 = Proof.
add_proof_query_line(Proof,Proof2) :-
	Proof = [Prf|PrfEnd],
	Proof2 = [[[0,query,[],[]]|Prf]|PrfEnd].
%%% ***

%%% ****if* PTTP/clauses
%%% DESCRIPTION
%%%   Negation normal form to Prolog clause translation.
%%%   Include a literal in the body of each clause to
%%%   indicate the number of the formula the clause came from.
%%% SOURCE

clauses((A , B),L,WffNum1,WffNum2) :-
	!,
	clauses(A,L1,WffNum1,W),
	clauses(B,L2,W,WffNum2),
	conjoin(L1,L2,L).

clauses(PNF,L,WffNum1,WffNum2):- once(nnf(PNF,Out)),clauses1(Out,L,WffNum1,WffNum2).

clauses1(A,L,WffNum1,WffNum2) :-
	write_clause_with_number(A,WffNum1),
	head_literals(A,Lits),
	clauses2(A,Lits,L,WffNum1),
	WffNum2 is WffNum1 + 1.

clauses2(A,[Lit|Lits],L,WffNum) :-
	body_for_head_literal(Lit,A,Body1),
	(Body1 == false ->
		L = true;
	%true ->
		conjoin(infer_by(WffNum),Body1,Body),
		clauses2(A,Lits,L1,WffNum),
		conjoin((Lit :- Body),L1,L)).
clauses2(_,[],true,_).

head_literals(Wff,L) :-
	Wff = (A :- B) ->	% contrapositives not made for A :- ... inputs
		head_literals(A,L);
	Wff = (A , B) ->
		head_literals(A,L1),
		head_literals(B,L2),
		list_union(L1,L2,L);
	Wff = (A ; B) ->
		head_literals(A,L1),
		head_literals(B,L2),
		list_union(L1,L2,L);
	%true ->
		L = [Wff].

body_for_head_literal(Head,Wff,Body) :-
	Wff = (A :- B) ->
		body_for_head_literal(Head,A,A1),
		conjoin(A1,B,Body);
	Wff = (A , B) ->
		body_for_head_literal(Head,A,A1),
		body_for_head_literal(Head,B,B1),
		disjoin(A1,B1,Body);
	Wff = (A ; B) ->
		body_for_head_literal(Head,A,A1),
		body_for_head_literal(Head,B,B1),
		conjoin(A1,B1,Body);
	Wff == Head ->
		Body = true;
	negated_literal(Wff,Head) ->
		Body = false;
	%true ->
		negated_literal(Wff,Body).
%%% ***
%%% ****if* PTTP/predicates
%%% DESCRIPTION
%%%   predicates returns a list of the predicates appearing in a formula.
%%% SOURCE

is_functor_like_search(Search):-atom(Search),arg(_,vv(search,prove),Search).

is_functor_like_firstOrder(Search):-atom(Search),arg(_,vv(firstOrder,prove),Search).

predicates(Wff,[]):-not(compound(Wff)),!.
predicates([Lw],L):- predicates(Lw,L),!.
predicates([Lw|ISTw],L):- !,
   predicates(Lw,L1),
   predicates(ISTw,L2),
   union(L2,L1,L).

predicates(Wff,L):- functor(Wff,Search,_),is_functor_like_search(Search),arg(1,Wff,X),predicates(X,L),!.

predicates(Wff,L) :-
        Wff = (A :- B) ->
                predicates(A,L1),
                predicates(B,L2),
                union(L2,L1,L);
        Wff = (A , B) ->
                predicates(A,L1),
                predicates(B,L2),
                union(L2,L1,L);
        Wff = (A ; B) ->
                predicates(A,L1),
                predicates(B,L2),
                union(L2,L1,L);
        functor(Wff,search,_) ->        % list predicates in first argument of search
                arg(1,Wff,X),
                predicates(X,L);
        builtin(Wff) ->
                L = [];
        %true ->
                functor(Wff,F,N),
                L = [[F,N]].



predicates(Wff,L) :- functor(Wff,F,A), predicates(Wff,F,A,L).

skipped_functor(F):- fail,is_2nd_order_holds(F).

predicates(Wff,F,___,L):- logical_functor(F), Wff=..[_|ARGS], predicates(ARGS,L).
predicates(Wff,F,A,  L):- builtin(F,A), Wff=..[_|ARGS], predicates(ARGS,L).
% predicates(Wff,F,___,L):- skipped_functor(F), Wff=..[_|ARGS], predicates(ARGS,L).
predicates(Wff,F,A,[[F,A]|L]):- Wff=..[_|ARGS], predicates(ARGS,L).

%%% ***
%%% ****if* PTTP/procedure
%%% DESCRIPTION
%%%   procedure returns a conjunction of the clauses
%%%   with head predicate P/N.
%%% SOURCE


procedure(P,N,Clauses,Proc) :-
	Clauses = (A , B) ->
		procedure(P,N,A,ProcA),
		procedure(P,N,B,ProcB),
		conjoin(ProcA,ProcB,Proc);
	(Clauses = (A :- B) , functor(A,P,N)) ->
		Proc = Clauses;
	%true ->
		Proc = true.

procedures([[P,N]|Preds],Clauses,Procs) :-
	procedure(P,N,Clauses,Proc),
	procedures(Preds,Clauses,Procs2),
	conjoin(Proc,Procs2,Procs).
procedures([],_Clauses,true).
%%% ***

head_body_was(_,_).

%%% ****if* PTTP/add_features
%%% SOURCE
add_features((Head :- Body),NewHeadBody):- 
   must_det(add_features0((Head :- Body),(Head1 :- Body1))),
   must_det(new_head_body(Head, Body,Head1 ,Body1,NewHeadBody)).

%new_head_body(Head, Body,Head1 ,Body1,(Head1 :- Body1)):-!.
% new_head_body(Head, Body,Head1 ,Body1,(Head1 :- head_body_was(Head, Body), Body1)):- arg_checks()
new_head_body(Head, infer_by(ProofID),Head1 ,Body1,(Head1 :- Body1)):- ground(Head),!.
new_head_body(Head, Body,Head1 ,Body1,(Head1 :- Body1)):- is_query_lit(Head),!.
new_head_body(Head, Body,Head1 ,Body1,(Head1 :- Body1)):- 
   true. 
   %   dmsg(pp((head_features(Head1) :-head_body_was(Head, Body), Body1))).

call_proof(Call,_):-catch(call(Call),E,fail).

add_features0((Head :- Body),(Head :- Body1)):-builtin(Head),!, add_features_hb(Head , Body , _Head1 , Body1).
add_features0(B,A):- add_features1(B,A).

add_features1((Head :- Body),(Head1 :- Body1)) :- (ground(Head);is_query_lit(Head)),!, add_features_hb_normal(Head , Body , Head1 , Body1).

add_features1((Head :- Body),(Head1 :- Body1)) :- add_features_hb(Head , Body , Head1 , Body1).

add_features_hb(Head , Body ,Head1 , Body1) :-
  
       linearize(Head,Head2,[],_,true,Matches),
       (negative_literal(Head) ->
               PosGoal = no;
       %true ->
               PosGoal = yes),
       Head =.. [_|HeadArgs],
  
       add_args(Body,
          PosGoal,GoalAtom,HeadArgs,
          PosAncestors,NegAncestors,
          NewPosAncestors,NewNegAncestors,
          Depth1,DepthOut,
          ProofIn,ProofOut,
                Body2,New),

       (var(New) ->
               PushAnc = true;

       PosGoal = yes ->
               NewNegAncestors = NegAncestors,
               PushAnc = (NewPosAncestors = [GoalAtom|PosAncestors]);
       %true -> ( PosGoal == false)
               NewPosAncestors = PosAncestors,
               PushAnc = (NewNegAncestors = [GoalAtom|NegAncestors])),

       search_cost(Body,HeadArgs,Cost),
       test_and_decrement_search_cost_expr(DepthIn,Cost,Depth1,TestExp),
       conjoin(PushAnc,Body2,Body4),
       conjoin(Matches,Body4,Body5),
       conjoin(TestExp,Body5,Body1),
     
     add_head_args(Head2,
          PosGoal,GoalAtom,HeadArgs,
          PosAncestors,NegAncestors,
          NewPosAncestors,NewNegAncestors,
          DepthIn,DepthOut,
          ProofIn,ProofOut,Head1,New).

add_features_hb_normal(Head , Body ,Head1 , Body1) :-
  
       ( is_query_lit(Head) ->
		Head2 = Head,
		add_args(Body,yes,query,[],
		         PosAncestors,NegAncestors,
			 PosAncestors,NegAncestors,
		         DepthIn,DepthOut,
			 ProofIn,ProofOut,
			 Body1,_);
	%true ->
		linearize(Head,Head2,[],_,true,Matches),
		(negative_literal(Head) ->
			PosGoal = no;
		%true ->
			PosGoal = yes),
		Head =.. [_|HeadArgs],
            
		add_args(Body,PosGoal,GoalAtom,HeadArgs,
                         PosAncestors,NegAncestors,
			 NewPosAncestors,NewNegAncestors,
		         Depth1,DepthOut,
			 ProofIn,ProofOut,
			 Body2,New),    

		(var(New) ->
			PushAnc = true;

		PosGoal = yes ->
			NewNegAncestors = NegAncestors,
			PushAnc = (NewPosAncestors = [GoalAtom|PosAncestors]);
		%true -> ( PosGoal == false)
			NewPosAncestors = PosAncestors,
			PushAnc = (NewNegAncestors = [GoalAtom|NegAncestors])),

                test_and_decrement_search_cost_expr(DepthIn,Cost,Depth1,TestExp),

		search_cost(Body,HeadArgs,Cost),
		conjoin(PushAnc,Body2,Body4),
		conjoin(Matches,Body4,Body5),
		conjoin(TestExp,Body5,Body1)),
     
    	Head2 =.. [P|L],
	internal_functor(P,IntP),
	list_append(L,
                    [PosAncestors,NegAncestors,
		       DepthIn,DepthOut,
		       ProofIn,ProofOut,
		       GoalAtom],
		    L1),
	Head1 =.. [IntP|L1].

%%% ***
%%% ****if* PTTP/add_args
%%% SOURCE

add_args((A , B),PosGoal,GoalAtom,HeadArgs,
         PosAncestors,NegAncestors,
	 NewPosAncestors,NewNegAncestors,
	 DepthIn,DepthOut,
	 ProofIn,ProofOut,
	 Body1,New) :-
		add_args(A,PosGoal,GoalAtom,HeadArgs,
                         PosAncestors,NegAncestors,
			 NewPosAncestors,NewNegAncestors,
		         DepthIn,Depth1,
			 ProofIn,Proof1,
		         A1,New),
		add_args(B,PosGoal,GoalAtom,HeadArgs,
		         PosAncestors,NegAncestors,
			 NewPosAncestors,NewNegAncestors,
		         Depth1,DepthOut,
			 Proof1,ProofOut,
                         B1,New),
		conjoin(A1,B1,Body1),!.

add_args((A ; B),PosGoal,GoalAtom,HeadArgs,
         PosAncestors,NegAncestors,
	 NewPosAncestors,NewNegAncestors,
	 DepthIn,DepthOut,
	 ProofIn,ProofOut,
	 Body1,New) :-
               add_args(A,PosGoal,GoalAtom,HeadArgs,
		         PosAncestors,NegAncestors,
			 NewPosAncestors,NewNegAncestors,
		         DepthA,DepthOut,
			 ProofIn,ProofOut,
			 A2,New),
		add_args(B,PosGoal,GoalAtom,HeadArgs,
		         PosAncestors,NegAncestors,
			 NewPosAncestors,NewNegAncestors,
		         DepthB,DepthOut,
			 ProofIn,ProofOut,
			 B2,New),
   trace,
                search_cost(A,HeadArgs,CostA),
		search_cost(B,HeadArgs,CostB),
		(CostA < CostB ->
			DepthA = DepthIn,
			Cost is CostB - CostA,
			test_and_decrement_search_cost_expr(DepthIn,Cost,DepthB,TestExp),
			A1 = A2,
			conjoin(TestExp,B2,B1);
		CostA > CostB ->
			DepthB = DepthIn,
			Cost is CostA - CostB,
			test_and_decrement_search_cost_expr(DepthIn,Cost,DepthA,TestExp),
			B1 = B2,
			conjoin(TestExp,A2,A1);
		%true ->
		        DepthA = DepthIn,
			DepthB = DepthIn,
			A1 = A2,
			B1 = B2),
		disjoin(A1,B1,Body1).

add_args(Search_cost,_PosGoal,_GoalAtom,_HeadArgs,_PosAncestors,_NegAncestors,_NewPosAncestors,_NewNegAncestors,Depth,Depth,Proof,Proof,true,_New):- functor(Search_cost,search_cost,_),!.

add_args(infer_by(N),PosGoal,GoalAtom,_HeadArgs,
         PosAncestors,NegAncestors,
	 _NewPosAncestors,_NewNegAncestors,
	 Depth,Depth,
	 ProofIn,ProofOut,
	 Body1,_New) :-
    (PosGoal = yes -> 
			N1 = N;
		%true ->  % atom in proof is negation of actual literal
			N1 is - N),
    Body1 = (ProofIn = [Prf,[N1,GoalAtom,PosAncestors,NegAncestors]|PrfEnd],
			 ProofOut = [Prf|PrfEnd]).

add_args(Body,_PosGoal,_GoalAtom,_HeadArgs,_PosAncestors,_NegAncestors,_NewPosAncestors,_NewNegAncestors,Depth,Depth,Proof,Proof,Body,_New):- builtin(Body),!.

% normal lit
add_args(BodyIn,
         _PosGoal,_GoalAtom,_HeadArgs,
         _PosAncestors,_NegAncestors,
	 NewPosAncestors,NewNegAncestors,
	 DepthIn,DepthOut,
	 ProofIn,ProofOut,
	 Body1,New) :-
   correct_lit(BodyIn,Body), 
    Body =.. L,
    list_append(L, [NewPosAncestors,NewNegAncestors,DepthIn,DepthOut,ProofIn,ProofOut], L1),
    Body11 =.. L1,
    Body1 = call_proof(Body11,Body),
    New = yes.

correct_lit(B,A):-once(correct_lit0(B,A)),B==A,!.
correct_lit(B,A):-once(correct_lit0(B,A)),!. % dmsg(once(correct_lit0(B,A))),term_variables(B,BV),term_variables(A,AV),must(AV==BV).

correct_lit0(Body,Body):-not(compound(Body)),!.
correct_lit0(BodyIn,Body):- var(BodyIn),trace_or_throw(correct_lit(BodyIn,Body)).
correct_lit0(BodyIn,Body):- functor(BodyIn,F,A),BodyIn=..[F|List],correct_lit(BodyIn,F,A,List,Body).

correct_lit(BodyIn,F,_,_,Body):- atom_concat('not_',_,F),!,negated_literal(BodyIn,Neg),!,correct_lit(Neg,NegBody),negated_literal(NegBody,Body).
correct_lit(BodyIn,F,A,L,Body):- is_holds_false(F),trace_or_throw(correct_lit(BodyIn,F,A,L,Body)).
correct_lit(BodyIn,F,_,[L|IST],Body):- is_holds_true(F),correct_true(L,IST,Body).
correct_lit(BodyIn,F,_,[L|IST],Body):- correct_true(F,[L|IST],Body).

do_not_wrap(F):-not(atom(F)),!,fail.
do_not_wrap(F):-arg(_,vv(query),F).
do_not_wrap(F):-atom_concat('int_',_,F).

correct_true(F,[L|IST],Body):- do_not_wrap(F),Body=..[F,L|IST].
correct_true(F,[L|IST],Body):- atom(F),builtin(F,_),Body=..[F,L|IST].
% correct_true(F,L,Body):- var(F),!,trace_or_throw(correct_true(F,L,Body)).
correct_true(F,[L|IST],Body):- Body =..[firstOrder,F,L|IST].

add_head_args(HeadIn,
          _PosGoal,GoalAtom,_HeadArgs,
          PosAncestors,NegAncestors,
          _NewPosAncestors,_NewNegAncestors,
          DepthIn,DepthOut,
          ProofIn,ProofOut,
          Head1,_New):-
     correct_lit(HeadIn,Head),
        Head =.. [P|L],
	internal_functor(P,IntP),
	list_append(L, [PosAncestors,NegAncestors, DepthIn,DepthOut, ProofIn,ProofOut, GoalAtom], L1),
	Head1 =.. [IntP|L1].


%%% ***
%%% ****if* PTTP/pttp1
%%% SOURCE

pttp1(X,Y) :-
	nl,
	write('PTTP input formulas:'),
	clauses(X,X0,1,_),		% prints and transforms input clauses
	apply_to_conjuncts(X0,add_features,X8),
	must_det(predicates(X8,IntPreds0)),
	must_det((list_reverse(IntPreds0,IntPreds1),
	procedures(IntPreds1,X8,IntProcs),
	must_det(predicates(X0,Preds0)),
	list_reverse(Preds0,Preds),
	apply_to_elements(Preds,make_wrapper(IntPreds1),Procs),
	conjoin(IntProcs,Procs,Y))),
	!.
%%% ***
%%% ****if* PTTP/pttp2
%%% SOURCE



pttp2(Y) :- !,
	apply_to_conjuncts(Y,pttp_assert_int,_),!.

pttp2(Y) :-
%	nl,
%	write('PTTP output formulas:'),
%	apply_to_conjuncts(Y,write_clause,_),
%	nl,
	nl,
	tell('pttp_temp.pl'),
	apply_to_conjuncts(Y,write_clause,_),
	nl,
	told,
	compile('pttp_temp.pl'),
	nl,
	!.
%%% ***
%%% ****if* PTTP/expand_input_proof
%%% SOURCE

expand_input_proof([],_Proof).
expand_input_proof([N|L],[[N|_]|L1]) :-
	expand_input_proof(L,L1).
%%% ***
%%% ****if* PTTP/contract_output_proof
%%% SOURCE

contract_output_proof([Prf|PrfEnd],Proof) :-
	Prf == PrfEnd,
	!,
	Proof = [].
contract_output_proof([[[N,_,_,_]|L]|PrfEnd],[N|L1]) :-
	contract_output_proof([L|PrfEnd],L1).
%%% ***
%%% ****if* PTTP/proof_length
%%% SOURCE

proof_length([Prf|PrfEnd],N) :-
	Prf == PrfEnd,
	!,
	N = 0.
proof_length([[[_,X,_,_]|L]|PrfEnd],N) :-
	proof_length([L|PrfEnd],N1),
	(X == query -> N is N1; N is N1 + 1).
%%% ***
%%% ****if* PTTP/proof_depth
%%% SOURCE

proof_depth([Prf|PrfEnd],N) :-
	Prf == PrfEnd,
	!,
	N = 0.
proof_depth([[[_,_,PosAnc,NegAnc]|L]|PrfEnd],N) :-
	proof_depth([L|PrfEnd],N1),
	list_length(PosAnc,N2),
	list_length(NegAnc,N3),
	N4 is N2 + N3,
	max(N1,N4,N).
%%% ***

%%% ****if* PTTP/pttp_functor
%%% DESCRIPTION
%%%   Sometimes the `functor' predicate doesn't work as expected and
%%%   a more comprehensive predicate is needed.  The pttp_functor'
%%%   predicate overcomes the problem of functor(X,13,0) causing
%%%   an error in Symbolics Prolog.  You may need to use it if
%%%   `functor' in your Prolog system fails to construct or decompose
%%%   terms that are numbers or constants.
%%% SOURCE

pttp_functor(Term,F,N) :-
	nonvar(F),
	atomic(F),
	N == 0,
	!,
	Term = F.
pttp_functor(Term,F,N) :-
	nonvar(Term),
	atomic(Term),
	!,
	F = Term,
	N = 0.
pttp_functor(Term,F,N) :-
	functor(Term,F,N).
%%% ***
%%% ****if* PTTP/list_append
%%% SOURCE

list_append([X|L1],L2,[X|L3]) :-
	list_append(L1,L2,L3).
list_append([],L,L).
%%% ***
%%% ****if* PTTP/list_reverse
%%% SOURCE

list_reverse(L1,L2) :-
	revappend(L1,[],L2).

revappend([X|L1],L2,L3) :-
	revappend(L1,[X|L2],L3).
revappend([],L,L).
%%% ***
%%% ****if* PTTP/list_union
%%% SOURCE

list_union([X|L1],L2,L3) :-
	identical_member(X,L2),
	!,
	list_union(L1,L2,L3).
list_union([X|L1],L2,[X|L3]) :-
	list_union(L1,L2,L3).
list_union([],L,L).
%%% ***
%%% ****if* PTTP/list_length
%%% SOURCE

list_length([_X|L],N) :-
	list_length(L,N1),
	N is N1 + 1.
list_length([],0).
%%% ***
%%% ****if* PTTP/min
%%% SOURCE

min(X,Y,Min) :-
	X =< Y ->
		Min = X;
	%true ->
		Min = Y.
%%% ***
%%% ****if* PTTP/max
%%% SOURCE

max(X,Y,Max) :-
	X =< Y ->
		Max = Y;
	%true ->
		Max = X.
%%% ***
%%% ****if* PTTP/conjoin
%%% SOURCE

conjoin(A,B,C) :-
	A == true ->
		C = B;
	B == true ->
		C = A;
	A == false ->
		C = false;
	B == false ->
		C = false;
	%true ->
		C = (A , B).
%%% ***
%%% ****if* PTTP/disjoin
%%% SOURCE

disjoin(A,B,C) :-
	A == true ->
		C = true;
	B == true ->
		C = true;
	A == false ->
		C = B;
	B == false ->
		C = A;
	%true ->
		C = (A ; B).
%%% ***
%%% ****if* PTTP/negated_functor
%%% SOURCE
negated_functor(-,_):-!,trace,fail.
negated_functor(~,_):-!,trace,fail.
negated_functor(F,NotF) :-
	name(F,L),
	name(not_,L1),
	(list_append(L1,L2,L) ->
		true;
	%true ->
		list_append(L1,L,L2)),
	name(NotF,L2).
%%% ***
%%% ****if* PTTP/negated_literal
%%% SOURCE

negated_literal(-A,B):-negated_literal(A,AA),!,negated_literal(AA,B).
negated_literal(A,B):- var(B),!,negated_literal_0(A,B).
negated_literal(A,-B):-negated_literal(B,BB),!,negated_literal_0(A,B).
negated_literal(A,B):-negated_literal_0(A,B).
negated_literal_0(Lit,NotLit) :-
	Lit =.. [F1|L1],
	negated_functor(F1,F2),
	(var(NotLit) ->
		NotLit =.. [F2|L1];
	%true ->
	       
               ( NotLit =.. [F2|L2],
		L1 == L2) ).
%%% ***
%%% ****if* PTTP/negative_functor
%%% SOURCE

negative_functor(F) :-
	name(F,L),
	name(not_,L1),
	list_append(L1,_,L).
%%% ***
%%% ****if* PTTP/negative_literal
%%% SOURCE

negative_literal(Lit) :-
	functor(Lit,F,_),
	negative_functor(F).

%%% ***
%%% ****if* PTTP/internal_functor
%%% SOURCE

internal_functor(P) :-
	name(P,L),
	name(int_,L1),
	list_append(L1,_,L).

internal_functor(P,IntP) :-
	name(P,L),
	name(int_,L1),
	list_append(L1,L,L2),
	name(IntP,L2).
%%% ***
%%% ****if* PTTP/apply_to_conjuncts
%%% SOURCE

apply_to_conjuncts(Wff,P,Wff1) :-
	Wff = (A , B) ->
		apply_to_conjuncts(A,P,A1),
		apply_to_conjuncts(B,P,B1),
		conjoin(A1,B1,Wff1);
	%true ->
		P =.. G,
		list_append(G,[Wff,Wff1],G1),
		T1 =.. G1,
		call(T1).
%%% ***
%%% ****if* PTTP/apply_to_elements
%%% SOURCE

apply_to_elements([X|L],P,Result) :-
	P =.. G,
	list_append(G,[X,X1],G1),
	T1 =.. G1,
	call(T1),
	apply_to_elements(L,P,L1),
	conjoin(X1,L1,Result).
apply_to_elements([],_,true).

%%% ***
%%% ****if* PTTP/write_clause
%%% SOURCE

write_clause(A) :-
	nl,
	write(A),
	write(.).

write_clause(A,_) :-				% 2-ary predicate for use as
	write_clause(A).			% apply_to_conjuncts argument
%%% ***
%%% ****if* PTTP/write_clause_with_number
%%% SOURCE

write_clause_with_number(A,WffNum) :-
	nl,
	write_indent_for_number(WffNum),
	write(WffNum),
	write('  '),
        copy_term(A,AA),
        numbervars(AA,0,_,[attvar(skip),singletons(true)]),
	write(AA),
	write(.).

write_indent_for_number(N) :-
	((number(N) , N <  100) -> write(' ') ; true),
	((number(N) , N <   10) -> write(' ') ; true).
%%% ***

%%% ****if* PTTP/timed_call
%%% DESCRIPTION
%%%   A query can be timed by timed_call(query,'Proof').
%%% NOTES
%%%   assumes that statistics(cputime,T) binds T to run-time in seconds
%%%   different Prolog systems have different ways to get this information
%%% SOURCE

timed_call(X,Type) :-
	statistics(cputime,T1),			%SWI Prolog
	(call(X) -> V = success ; V = failure),	%SWI Prolog
	statistics(cputime,T2),			%SWI Prolog
	Secs is T2 - T1,			%SWI Prolog
%	statistics(runtime,[T1,_]),		%Quintus/SICStus Prolog
%	(call(X) -> V = success ; V = failure),	%Quintus/SICStus Prolog
%	statistics(runtime,[T2,_]),		%Quintus/SICStus Prolog
%	Secs is (T2 - T1) / 1000.0,		%Quintus/SICStus Prolog
	nl,
	write(Type),
	write(' time: '),
	write(Secs),
	write(' seconds'),
	nl,
	V = success.
%%% ***
%%% ****if* PTTP/write_search_progress
%%% SOURCE

write_search_progress(Level) :-
	% write('cost '),
	write(Level),write('.'),current_output(S),flush_output(S).

%%% ***

%%% ****if* PTTP/builtin
%%% DESCRIPTION
%%%   List of builtin predicates that can appear in clause bodies.
%%%   No extra arguments are added for ancestor goals or depth-first
%%%   iterative-deepening search.  Also, if a clause body is
%%%   composed entirely of builtin goals, the head is not saved
%%%   as an ancestor for use in reduction or pruning.
%%%   This list can be added to as required.
%%% SOURCE

builtin(T) :-
	functor(T,F,N),
	builtin(F,N).

builtin(V,A):-var(V),!,trace_or_throw(builtin(V,A)).
builtin(!,0).
builtin(call_proof,2).
builtin(true,0).
builtin(false,0).
builtin(fail,0).
builtin(succeed,0).
builtin(trace,0).
builtin(atom,1).
builtin(integer,1).
builtin(number,1).
builtin(atomic,1).
builtin(constant,1).
builtin(functor,3).
builtin(arg,3).
builtin(var,1).
builtin(nonvar,1).
builtin(call,1).
builtin(=,2).
builtin(\=,2).
builtin(==,2).
builtin(\==,2).
builtin(=\=,2).
builtin(>,2).
builtin(<,2).
builtin(>=,2).
builtin(=<,2).
builtin(is,2).
builtin(display,1).
builtin(write,1).
builtin(nl,0).
builtin(infer_by,_).
builtin(search_cost,_).
builtin(test_and_decrement_search_cost,_).
builtin(unify,_).
builtin(identical_member,_).
builtin(unifiable_member,_).
builtin(F,_):- atom_concat(_,'_int',F),!.
builtin(F,_):-moo:mpred_prop(F,as_is(_)),!. %,fail.
%builtin(F,A):-functor(P,F,A),predicate_property(P,_).
%%% ***

end_of_file.


%%% ****h* PTTP/PTTP-dalit
%%% COPYRIGHT
%%%   Copyright (c) 1988-2003 Mark E. Stickel, SRI International, Menlo Park, CA 94025  USA
%%% 
%%%   Permission is hereby granted, free of charge, to any person obtaining a
%%%   copy of this software and associated documentation files (the "Software"),
%%%   to deal in the Software without restriction, including without limitation
%%%   the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%%   and/or sell copies of the Software, and to permit persons to whom the
%%%   Software is furnished to do so, subject to the following conditions:
%%% 
%%%   The above copyright notice and this permission notice shall be included
%%%   in all copies or substantial portions of the Software.
%%% 
%%%   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%%   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%%   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%%%   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
%%%   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%%%   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%%%   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%% DESCRIPTION
%%%  
%%%      A Prolog Technology Theorem Prover
%%%  
%%%               Mark E. Stickel
%%%  
%%%   This file contains changes to PTTP to use
%%%   depth-first iterative deepening dalit_search with bound on
%%%   D_Alit (maximum number of A-literals on a branch)
%%%   instead of
%%%   D_Inf (total number of subgoals).
%%%
%%%   To use, load pttp and then load this file
%%%   to replace changed definitions.
%%% SOURCE

:-abolish(prove,6).
prove(Goal,Max,Min,Inc,ProofIn,ProofOut):-dalit_prove(Goal,Max,Min,Inc,ProofIn,ProofOut).

dalit_prove(Goal,Max,Min,Inc,ProofIn,ProofOut) :-
	expand_input_proof(ProofIn,PrfEnd),
	PrevInc is Min + 1,
	dalit_add_args(Goal,_,_,[],_,_,[],[],DepthIn,[PrfEnd|PrfEnd],ProofOut1,Goal1,_),
	!,
	timed_call(dalit_search(Goal1,Max,Min,Inc,PrevInc,DepthIn),'Proof'),
	contract_output_proof(ProofOut1,ProofOut),
	write_proof(ProofOut1),
	nl.

dalit_prove(Goal,Max,Min,Inc,ProofIn) :-
	dalit_prove(Goal,Max,Min,Inc,ProofIn,_).

dalit_prove(Goal,Max,Min,Inc) :-
	dalit_prove(Goal,Max,Min,Inc,[],_).

dalit_prove(Goal,Max,Min) :-
	dalit_prove(Goal,Max,Min,1,[],_).

dalit_prove(Goal,Max) :-
	dalit_prove(Goal,Max,0,1,[],_).

dalit_prove(Goal) :-
	dalit_prove(Goal,10000,0,1,[],_).

:-abolish(search_cost,3).
search_cost(Body,HeadArgs,N):-dalit_search_cost(Body,HeadArgs,N).
dalit_search_cost(Body,HeadArgs,N) :-
	Body = dalit_search_cost(M) ->
		N = M;
	Body = (A , B) ->
		(A = dalit_search_cost(M) ->	% if first conjunct is dalit_search_cost(M),
			N = M;		% dalit_search cost of conjunction is M
		%true ->
			dalit_search_cost(A,HeadArgs,N1),
			dalit_search_cost(B,HeadArgs,N2),
			max(N1,N2,N));
	Body = (A ; B) ->
		dalit_search_cost(A,HeadArgs,N1),
		dalit_search_cost(B,HeadArgs,N2),
		min(N1,N2,N);
	builtin(Body) ->
		N = 0;
	%true ->
		N = 1.

:-abolish(search,6).
search(Goal,Max,Min,Inc,PrevInc,DepthIn):-dalit_search(Goal,Max,Min,Inc,PrevInc,DepthIn).

dalit_search(_Goal,Max,Min,_Inc,_PrevInc,_DepthIn) :-
	Min > Max,
	!,
	fail.
dalit_search(Goal,_Max,Min,_Inc,PrevInc,DepthIn) :-
        write_search_progress(Min),
	DepthIn = Min,
	call(Goal),
	true.			% should fail if solution found previously
dalit_search(Goal,Max,Min,Inc,_PrevInc,DepthIn) :-
	Min1 is Min + Inc,
	dalit_search(Goal,Max,Min1,Inc,Inc,DepthIn).

:-abolish(make_wrapper,3).
make_wrapper(Body,HeadArgs,N):-dalit_make_wrapper(Body,HeadArgs,N).

dalit_make_wrapper(_DefinedPreds,[query,0],true) :-
	!.
dalit_make_wrapper(DefinedPreds,[P,N],Result) :-
	functor(Goal,P,N),
	Goal =.. [P|Args],
	ExtraArgs = [PosAncestors,NegAncestors,DepthIn,ProofIn,ProofOut],
	list_append(Args,ExtraArgs,Args1),
	Head =.. [P|Args1],
	internal_functor(P,IntP),
	list_length(ExtraArgs,NExtraArgs),
	NN is N + NExtraArgs + 1,
	(identical_member([IntP,NN],DefinedPreds) ->
	        list_append(ExtraArgs,[GoalAtom],ExtraArgs2),
		list_append(Args,ExtraArgs2,Args2),
		IntHead =.. [IntP|Args2];
	%true ->
		IntHead = fail),
	(negative_functor(P) ->
		negated_literal(Goal,PosGoal),
		Red = redn,  % atom in proof is negation of actual literal
		C1Ancestors = NegAncestors,
		C2Ancestors = PosAncestors;
	%true ->
		PosGoal = Goal,
		Red = red,
		C1Ancestors = PosAncestors,
		C2Ancestors = NegAncestors),
	(N = 0 ->	% special case for propositional calculus
		V1 = (identical_member(GoalAtom,C2Ancestors) , !);
	%true ->
		V1 = ((identical_member(GoalAtom,C2Ancestors) , !);
		       unifiable_member(GoalAtom,C2Ancestors))),
	V2 = (
		 ProofIn = [Prf,[Red,GoalAtom,PosAncestors,NegAncestors]|PrfEnd],
		 ProofOut = [Prf|PrfEnd]),
	conjoin(V1,V2,Reduce),
	Result = (Head :- GoalAtom = PosGoal,
		  	  (identical_member(GoalAtom,C1Ancestors) ->
			   	fail;
			  %true ->
			   	(Reduce;
				 IntHead))).

query(PosAncestors,NegAncestors,DepthIn,ProofIn,ProofOut) :-
	int_query(PosAncestors,NegAncestors,DepthIn,ProofIn,ProofOut,query).

:-abolish(add_features,2).
add_features(A,B):-dalit_add_features(A,B).

dalit_add_features((Head :- Body),(Head1 :- Body1)) :-
	(functor(Head,query,_) ->
		Head2 = Head,
		dalit_add_args(Body,yes,query,[],
		         PosAncestors,NegAncestors,
			 PosAncestors,NegAncestors,
		         DepthIn,
			 ProofIn,ProofOut,
			 Body1,_);
	%true ->
		linearize(Head,Head2,[],_,true,Matches),
		(negative_literal(Head) ->
			PosGoal = no;
		%true ->
			PosGoal = yes),
		Head =.. [_|HeadArgs],
		dalit_add_args(Body,PosGoal,GoalAtom,HeadArgs,
                         PosAncestors,NegAncestors,
			 NewPosAncestors,NewNegAncestors,
		         Depth1,
			 ProofIn,ProofOut,
			 Body2,New),
		(var(New) ->
			PushAnc = true;
		PosGoal = yes ->
			NewNegAncestors = NegAncestors,
			PushAnc = (NewPosAncestors = [GoalAtom|PosAncestors]);
		%true ->
			NewPosAncestors = PosAncestors,
			PushAnc = (NewNegAncestors = [GoalAtom|NegAncestors])),
		dalit_search_cost(Body,HeadArgs,Cost),
		test_and_decrement_search_cost_expr(DepthIn,Cost,Depth1,TestExp),
		conjoin(PushAnc,Body2,Body4),
		conjoin(Matches,Body4,Body5),
		conjoin(TestExp,Body5,Body1)),
    	Head2 =.. [P|L],
	internal_functor(P,IntP),
	list_append(L,[PosAncestors,NegAncestors,
		       DepthIn,
		       ProofIn,ProofOut,
		       GoalAtom],
		    L1),
	Head1 =.. [IntP|L1].

:-abolish(add_args,13).

add_args(Body,PosGoal,GoalAtom,HeadArgs,
         PosAncestors,NegAncestors,
	 NewPosAncestors,NewNegAncestors,
	 DepthIn,
	 ProofIn,ProofOut,
	 Body1,New):- 
  dalit_add_args(Body,PosGoal,GoalAtom,HeadArgs,
         PosAncestors,NegAncestors,
	 NewPosAncestors,NewNegAncestors,
	 DepthIn,
	 ProofIn,ProofOut,
	 Body1,New).

dalit_add_args(Body,PosGoal,GoalAtom,HeadArgs,
         PosAncestors,NegAncestors,
	 NewPosAncestors,NewNegAncestors,
	 DepthIn,
	 ProofIn,ProofOut,
	 Body1,New) :-
	Body = (A , B) ->
		dalit_add_args(A,PosGoal,GoalAtom,HeadArgs,
                         PosAncestors,NegAncestors,
			 NewPosAncestors,NewNegAncestors,
		         DepthIn,
			 ProofIn,Proof1,
		         A1,New),
		dalit_add_args(B,PosGoal,GoalAtom,HeadArgs,
		         PosAncestors,NegAncestors,
			 NewPosAncestors,NewNegAncestors,
			 DepthIn,
			 Proof1,ProofOut,
                         B1,New),
		conjoin(A1,B1,Body1);
	Body = (A ; B) ->
		unimplemented;
	functor(Body,dalit_search_cost,_) ->
		ProofOut = ProofIn,
		Body1 = true;
	Body = infer_by(N) ->
		(PosGoal = yes -> 
			N1 = N;
		%true ->  % atom in proof is negation of actual literal
			N1 is - N),
		Body1 = (ProofIn = [Prf,[N1,GoalAtom,PosAncestors,NegAncestors]|PrfEnd],
			 ProofOut = [Prf|PrfEnd]);
	Body = dalit_search_cost(N) ->
		ProofOut = ProofIn,
		Body1 = true;
	builtin(Body) ->
		ProofOut = ProofIn,
		Body1 = Body;
	%true ->
		Body =.. L,
		list_append(L,
                       [NewPosAncestors,NewNegAncestors,
			DepthIn,
			ProofIn,ProofOut],
		       L1),
		Body1 =.. L1,
		New = yes.
%%% ***

