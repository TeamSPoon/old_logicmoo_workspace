:- if( (false , \+ ((current_prolog_flag(logicmoo_include,Call),Call))) ).
:- module(xray_pttp,[]).
:- endif.


:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.
:- prolog_load_context(directory,Dir),
   DirFor = pttp_new,
   (( \+ user:file_search_path(DirFor,Dir)) ->asserta(user:file_search_path(DirFor,Dir));true),
   absolute_file_name('../../../../',Y,[relative_to(Dir),file_type(directory)]),
   (( \+ user:file_search_path(pack,Y)) ->asserta(user:file_search_path(pack,Y));true).
:- attach_packs.
:- initialization(attach_packs).
% [Required] Load the Logicmoo Library Utils
:- use_module(  logicmoo(logicmoo_utils)).
:- initialization(attach_packs).

%%% ***

%%% ****if* PTTP/myfunctor
%%% DESCRIPTION
%%%   Sometimes the `functor' predicate doesn't work as expected and
%%%   a more comprehensive predicate is needed.  The `myfunctor'
%%%   predicate overcomes the problem of functor(X,13,0) causing
%%%   an error in Symbolics Prolog.  You may need to use it if
%%%   `functor' in your Prolog system fails to construct or decompose
%%%   terms that are numbers or constants.
%%% SOURCE

myfunctor(Term,F,N) :-
	nonvar(F),
	atomic(F),
	N == 0,
	!,
	Term = F.
myfunctor(Term,F,N) :-
	nonvar(Term),
	atomic(Term),
	!,
	F = Term,
	N = 0.
myfunctor(Term,F,N) :-
	functor(Term,F,N).
%%% ***
%%% ****if* PTTP/append
%%% SOURCE
/*
append([X|L1],L2,[X|L3]) :-
	append(L1,L2,L3).
append([],L,L).
*/
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
/*
intersection([X|L1],L2,[X|L3]) :-
        identical_member(X,L2),
        !,
        intersection(L1,L2,L3).
intersection([_X|L1],L2,L3) :-
        intersection(L1,L2,L3).
intersection([],_L,[]).

%%% min(X,Y,Min) :-
%%%        X =< Y ->
%%%                Min = X;
%%%        %true ->
%%%                Min = Y.

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
*/
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

negated_functor(F,NotF) :-
	name(F,L),
	name(not_,L1),
	(append(L1,L2,L) ->
		true;
	%true ->
		append(L1,L,L2)),
	name(NotF,L2).
%%% ***
%%% ****if* PTTP/negated_literal
%%% SOURCE

negated_literal(Lit,NotLit) :-
	Lit =.. [F1|L1],
	negated_functor(F1,F2),
	(var(NotLit) ->
		NotLit =.. [F2|L1];
	%true ->
		NotLit =.. [F2|L2],
		L1 == L2).
%%% ***
%%% ****if* PTTP/negative_functor
%%% SOURCE

negative_functor(F) :-
	name(F,L),
	name(not_,L1),
	append(L1,_,L).
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
	append(L1,_,L).

internal_functor(P,IntP) :-
	name(P,L),
	name(int_,L1),
	append(L1,L,L2),
	name(IntP,L2).
%%% ***
%%% ****if* PTTP/apply_to_conjuncts
%%% SOURCE

apply_to_conjuncts(Wff,P,Wff1) :-
  must(nonvar(Wff)),
      (  Wff = (A , B) ->
		apply_to_conjuncts(A,P,A1),
		apply_to_conjuncts(B,P,B1),
		conjoin(A1,B1,Wff1);
	%true ->
		P =.. G,
		append(G,[Wff,Wff1],G1),
		T1 =.. G1,
		must(T1)),!.
%%% ***
%%% ****if* PTTP/apply_to_elements
%%% SOURCE

apply_to_elements([X|L],P,Result) :-
	P =.. G,
	append(G,[X,X1],G1),
	T1 =.. G1,
	call(T1),
	apply_to_elements(L,P,L1),
	conjoin(X1,L1,Result).
apply_to_elements([],_,true).

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
%%%     Its unbounded depth-first prove strategy
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
%%%     pttp(formula) - translates the first-order formula
%%%       (normally a conjunction of formulas) into Prolog
%%%       and compiles it
%%%
%%%     prove(formula) - tries to prove formula
%%%
%%%   Look at the description of these functions
%%%   and the examples for more details on how
%%%   pttp and prove should be used.
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


%%% ****if* PTTP/builtin
%%% DESCRIPTION
%%%   List of builtin predicates that can appear in clause bodies.
%%%   No extra arguments are added for ancestor goals or depth-first
%%%   iterative-deepening prove.  Also, if a clause body is
%%%   composed entirely of builtin goals, the head is not saved
%%%   as an ancestor for use in reduction or pruning.
%%%   This list can be added to as required.
%%% SOURCE

builtin(T) :-
	functor(T,F,N),
	builtin(F,N).

builtin(!,0).
builtin(true,0).
builtin(false,0).
builtin(fail,0).
builtin(succeed,0).
builtin(dtrace,0).
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
builtin(write_proved,_).
builtin(prove,_).
builtin(unify,_).
builtin(search_cost,_).
builtin(test_and_decrement_search_cost,_).
builtin(unify_with_occurs_check,_).
builtin(identical_member,_).
builtin(unifiable_member,_).
builtin(inc_ncalls,0).
%%% ***

not_xray:-fail.

% :- set_flag(print_depth,1000).
% :- set_flag(variable_names,off).

:- was_dynamic(count_inferences_pred/1).
:- was_dynamic(trace_search_progress_pred/1).
:- was_dynamic(compile_proof_printing/0).

%%% Sound unification algorithm with occurs check that is called
%%% by code resulting from the `add_sound_unification' transformation.
%%% This should be coded in a lower-level language for efficiency.

%%% Depth-first iterative-deepening prove.
%%%
%%% `add_complete_search' adds arguments DepthIn and DepthOut
%%% to each PTTP literal to control bounded depth-first
%%% prove.  When a literal is called,
%%% DepthIn is the current depth bound.  When
%%% the literal exits, DepthOut is the new number
%%% of levels remaining after the solution of
%%% the literal (DepthIn - DepthOut is the number
%%% of levels used in the solution of the goal.)
%%%
%%% For clauses with empty bodies or bodies
%%% composed only of builtin functions,
%%% DepthIn = DepthOut.
%%%
%%% For other clauses, the depth bound is
%%% compared to the cost of the body.  If the
%%% depth bound is exceeded, the clause fails.
%%% Otherwise the depth bound is reduced by
%%% the cost of the body.
%%%
%%% p :- q , r.
%%% is transformed into
%%% p(DepthIn,DepthOut) :-
%%%     DepthIn >= 2, Depth1 is DepthIn - 2,
%%%     q(Depth1,Depth2),
%%%     r(Depth2,DepthOut).
%%%
%%% p :- q ; r.
%%% is transformed into
%%% p(DepthIn,DepthOut) :-
%%%     DepthIn >= 1, Depth1 is DepthIn - 1,
%%%     (q(Depth1,DepthOut) ; r(Depth1,DepthOut)).
%%%
%%%
%%%
%%% Complete inference.
%%%
%%% Model elimination reduction operation and
%%% identical ancestor goal pruning.
%%%
%%% Two arguments are added to each literal, one
%%% for all the positive ancestors, one for all
%%% the negative ancestors.
%%%
%%% Unifiable membership is checked in the list 
%%% of opposite polarity to the goal
%%% for performing the reduction operation.
%%%
%%% Identity membership is checked in the list
%%% of same polarity as the goal
%%% for performing the ancestor goal pruning operation.
%%% This is not necessary for soundness or completeness,
%%% but is often effective at substantially reducing the
%%% number of inferences.
%%%
%%% The current head goal is added to the front
%%% of the appropriate ancestor list during the
%%% call on subgoals in bodies of nonunit clauses.
:- ensure_loaded('pttp-examples').

pttp_tests:-time((ignore(source_file(chang_lee_example7,F)),!,ignore((source_file(Pred,F),atom(Pred),once(timed_call(Pred,'Total',Time)),assertz(timed_result(Pred,'Total',Time)),fail)))),listing(timed_result).


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
%%%   the sound unify_with_occurs_check predicate.
%%%
%%% `make_arg_w' transforms a clause so that its
%%% head has no repeated variables.  Unifying a goal with
%%% the clause head can then be done soundly without the occurs
%%% check.  The rest of the unification can then be done in
%%% the body of the transformed clause, using the sound `unify_with_occurs_check'
%%% predicate.
%%% For example,
%%%    p(X,Y,f(X,Y)) :- true.
%%% is transformed into
%%%    p(X,Y,f(X1,Y1)) :- unify_with_occurs_check(X,X1), unify_with_occurs_check(Y,Y1).
%%% SOURCE

linearize(Pred,TermIn,TermOut,VarsIn,VarsOut,MatchesIn,MatchesOut) :-
        nonvar(TermIn) ->
                functor(TermIn,F,N),
                myfunctor(TermOut,F,N),
		linearize_args(Pred,TermIn,TermOut,VarsIn,VarsOut,
		               MatchesIn,MatchesOut,1,N);
        identical_member(TermIn,VarsIn) ->
                (VarsOut = VarsIn,
                 CALL=..[Pred,TermIn,TermOut],
                 conjoin(MatchesIn,CALL,MatchesOut));
        %true ->
                TermOut = TermIn,
                VarsOut = [TermIn|VarsIn],
                MatchesOut = MatchesIn.

linearize_args(Pred,TermIn,TermOut,VarsIn,VarsOut,MatchesIn,MatchesOut,I,N) :-
           I > N ->
                   VarsOut = VarsIn,
                   MatchesOut = MatchesIn;
        cyclic_term(TermIn) ->
                VarsOut = VarsIn,
                MatchesOut = MatchesIn;
        %true ->
                arg(I,TermIn,ArgI),
                linearize(Pred,ArgI,NewArgI,VarsIn,Vars1,MatchesIn,Matches1),
                arg(I,TermOut,NewArgI),
                I1 is I + 1,
                linearize_args(Pred,TermIn,TermOut,Vars1,VarsOut,Matches1,MatchesOut,I1,N).


%%% ***
%%% ****if* PTTP/unifiable_member
%%% DESCRIPTION
%%%   unifiable_member(X,L) succeeds each time X is unifiable with an
%%%   element of the list L
%%% SOURCE
nonzero_search_cost(Body,HeadArgs,Cost) :-
        search_cost(Body,HeadArgs,Cost),
        Cost > 0.

%%% ***

%%% ****if* PTTP/test_and_decrement_search_cost_expr
%%% SOURCE

test_and_decrement_search_cost_expr(DepthIn,Cost,Depth1,Expr) :-
	Cost == 0 ->
		Depth1 = DepthIn,
		Expr = true;
	%true ->
		Expr = (DepthIn >= Cost , Depth1 is DepthIn - Cost).
%%% ***
%%% ****if* PTTP/search_cost
%%% DESCRIPTION
%%%   Search cost is ordinarily computed by counting literals in the body.
%%% It can be given explicitly instead by including a number, as in
%%%   p :- search_cost(3).     (ordinarily, cost would be 0)
%%%   p :- search_cost(1),q,r. (ordinarily, cost would be 2)
%%%   p :- search_cost(0),s.   (ordinarily, cost would be 1)
%%%
%%% Propositional goals are not counted into the prove cost so
%%% that fully propositional problems can be solved without
%%% deepening when iterative-deepening prove is used.

search_cost(Body,HeadArgs,N) :-
        Body = search_cost(M) ->
                N = M;
        Body = (A , B) ->
                (A = search_cost(M) ->  % if first conjunct is search_cost(M),
			N = M;		% prove cost of conjunction is M
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
        functor(Body,_,2) ->  % zero-cost 2-ary (0-ary plus ancestor lists) predicates
                N = 0;        % heuristic for propositional problems
        %true ->
                N = 1.
%%% ***
%%% ****if* PTTP/prove
%%% DESCRIPTION
%%%   Search driver predicate.
%%% ***
%%% ****f* PTTP/prove
%%% DESCRIPTION
%%%   prove(Goal) can be used to prove Goal using code compiled by PTTP.
%%%   It uses depth-first iterative-deepening prove and prints the proof.
%%%
%%%   Depth-first iterative-deepening prove can be controlled
%%%   by extra paramaters of the prove predicate:
%%%      prove(Goal,Max,Min,Inc,ProofIn)
%%% Max is the maximum depth to prove (defaults to a big number),
%%% Min is the minimum depth to prove (defaults to 0),
%%% Inc is the amount to increment the bound each time (defaults to 1).
%%%   ProofIn specifies initial steps of proof to retrace (defaults to []).
%%%
%%% Depth-first iterative deepening prove can be
%%% specified inside the PTTP formula by compiling
%%%   query :- prove(p(b,a,c),Max,Min,Inc)
%%% and executing
%%%   query.
%%% or directly by the user by compiling
%%%   query :- p(b,a,c))
%%% and executing
%%%   prove(query,Max,Min,Inc).
%%%
%%% The prove(Goal,Max,Min,Inc) predicate adds
%%% DepthIn and DepthOut arguments to its goal argument.

prove(Goal,Max,Min,Inc,ProofIn) :-
	prove(Goal,Max,Min,Inc,ProofIn,_).


%%% Depth-first iterative-deepening prove can be
%%% specified for a goal by wrapping it in a call
%%% on the prove predicate:
%%%    prove(Goal,Max,Min,Inc)
prove(Goal,Max,Min,Inc) :-
        PrevInc is Min + 1,
        add_complete_search_args(Goal,Goal,DepthIn,DepthOut,Goal1),
   (compile_proof_printing ->
           add_proof_recording_args(Goal1,_Proof,_ProofEnd,Goal2);
        %true ->
                Goal2 = Goal1),
        !,
        prove(Goal2,Max,Min,Inc,PrevInc,DepthIn,DepthOut).

prove(Goal,Max,Min) :-
        prove(Goal,Max,Min,3).

prove(Goal,Max) :-
        prove(Goal,Max,0).

prove(query) :- !,   prove(query,1000,0,3).
prove(Goal) :-
        prove(Goal,1000000).

%%% Actual prove driver predicate.
%%% Note that depth-bounded execution of Goal is enabled by
%%% the fact that the DepthIn and DepthOut arguments of
%%% prove are also the DepthIn and DepthOut arguments of Goal.
%%% SOURCE

prove(_Goal,Max,Min,_Inc,_PrevInc,_DepthIn,_DepthOut) :-
        Min > Max,
        !,
        fail.
prove(Goal,_Max,Min,_Inc,PrevInc,DepthIn,DepthOut) :-
        write_search_progress(Min),
        DepthIn = Min,
        call(Goal),
	DepthOut < PrevInc.	% fail if solution found previously
prove(Goal,Max,Min,Inc,_PrevInc,DepthIn,DepthOut) :-
        Min1 is Min + Inc,
        prove(Goal,Max,Min1,Inc,Inc,DepthIn,DepthOut).
%%% ***

%%% ****if* PTTP/make_wrapper

%%% SOURCE
%%% ***
%%% ****if* PTTP/query
%%% DESCRIPTION
%%%   query uses the following definition instead of the more complex one
%%%   that would be created by make_wrapper
%%% SOURCE

% wrong pttp ! query(PosAncestors,NegAncestors,DepthIn,DepthOut,ProofIn,ProofOut) :-
%	int_query(PosAncestors,NegAncestors,DepthIn,DepthOut,ProofIn,ProofOut,query).
%%% ***
%%% ****if* PTTP/unifiable_member
%%% DESCRIPTION
%%%   unifiable_member(X,L) succeeds each time X is unifiable with an
%%%   element of the list L
%%% SOURCE

unifiable_member(X,[Y|_]) :-
	unify_with_occurs_check(X,Y).
unifiable_member(X,[_|L]) :-
	unifiable_member(X,L).
%%% ***
%%% ****if* PTTP/identical_member
%%% DESCRIPTION
%%%   identical_member(X,L) succeeds iff X is an element of the list L
%%%   it does not use unification during element comparisons
%%% SOURCE
/*
identical_member(X,[Y|_])  :-
	X == Y,
	!.
identical_member(X,[_|L]) :-
	identical_member(X,L).
*/
%%% ***

%%% ****if* PTTP/clauses
%%% DESCRIPTION
%%% Negation normal form to Prolog clause translation.
%%% Include a literal in the body of each clause to
%%% indicate the number of the formula the clause came from.
%%% SOURCE
clauses((A , B),L,WffNum) :-
        !,
        clauses(A,L1,WffNum),
        WffNum2 is WffNum + 1,
        clauses(B,L2,WffNum2),
        conjoin(L1,L2,L).
clauses(A,L,WffNum) :-
        head_literals(A,Lits),
        clauses(A,Lits,L,WffNum).

clauses(A,[Lit|Lits],L,WffNum) :-
        body_for_head_literal(Lit,A,Body1),
        (compile_proof_printing ->
               % conjoin(infer_by(WffNum),Body1,Body);
                conjoin(infer_by(proof(A)),Body1,Body);
        %true ->
                Body = Body1),
        clauses(A,Lits,L1,WffNum),
        conjoin((Lit :- Body),L1,L).
clauses(_,[],true,_).

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
%%% predicates returns a list of the predicates appearing in a formula.
%%% SOURCE

predicates(Wff,L) :-
        Wff = (A :- B) ->
                predicates(A,L1),
                predicates(B,L2),
                list_union(L2,L1,L);
        Wff = (A , B) ->
                predicates(A,L1),
                predicates(B,L2),
                list_union(L2,L1,L);
        Wff = (A ; B) ->
                predicates(A,L1),
                predicates(B,L2),
                list_union(L2,L1,L);
        functor(Wff,prove,_) ->        % list predicates in first argument of prove
                arg(1,Wff,X),
                predicates(X,L);
        builtin(Wff) ->
                L = [];
        %true ->
                functor(Wff,F,N),
                L = [[F,N]].
%%% ***
%%% ****if* PTTP/procedure
%%% DESCRIPTION
%%% procedure returns a conjunction of the clauses
%%% with head predicate P/N.
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


:- if(not_xray).
procedures_with_ancestor_tests([[P,N]|Preds],Clauses,Procs) :-
	procedure(P,N,Clauses,Proc),
	procedures_with_ancestor_tests(Preds,Clauses,Procs2),
	conjoin(Proc,Procs2,Procs).
procedures_with_ancestor_tests([],_Clauses,true).
:- else.
procedures_with_ancestor_tests([[P,N]|Preds],Clauses,Procs) :-
        procedure(P,N,Clauses,Proc1),
        ancestor_tests(P,N,Tests),conjoin(Tests,Proc1,Proc),
        procedures_with_ancestor_tests(Preds,Clauses,Procs2),
        conjoin(Proc,Procs2,Procs).
procedures_with_ancestor_tests([],_Clauses,true).
:- endif.


query_n(M) :-                             % call query with depth bound M
        compile_proof_printing -> 
                query(M,_N,_Proof,_ProofEnd);
        %true ->
                query(M,_N).

query :-                                % unbounded prove of query
        query_n(1000000)
.

 %%% ***
%%% ****if* PTTP/write_clause
%%% SOURCE

write_clause(A) :-
	nl,
	write(A),
	write(.).

write_clause(A,_) :-                    % 2-ary predicate can be used as
        write_clause(A).                % argument of apply_to_conjuncts

write_clause_to_file((A,B),OutFile) :-
	write_clause_to_file(A,OutFile),
	write_clause_to_file(B,OutFile),
        !.
write_clause_to_file(A,OutFile) :-
	nl(OutFile),
	write(OutFile,A),
	write(OutFile,.),
	!.



%%% ***
%%% ****if* PTTP/write_clause_with_number
%%% SOURCE

write_clause_with_number(A,WffNum) :-
	nl,
	write_indent_for_number(WffNum),
	write(WffNum),
	write('  '),
	write(A),
	write(.).

write_indent_for_number(N) :-
	((number(N) , N <  100) -> write(' ') ; true),
	((number(N) , N <   10) -> write(' ') ; true).



%%% A query can be timed by time(query).

time(X) :-
        time(X,'Execution').

time(X,Type) :-
        clear_ncalls,

        statistics(runtime,[T1,_]),     % Quintus Prolog on Sun
%        T1 is get-internal-run-time,  % Common Lisp time function

        call(X),

        statistics(runtime,[T2,_]),     % Quintus Prolog on Sun
        Secs is (T2 - T1) / 1000.0,     % Quintus measures runtime in milliseconds
%        T2 is get-internal-run-time,  % Common Lisp time function
%        Secs is (T2 - T1) / 977.0,      % internal-time-units-per-second on Darwin

        nl,
        write(Type),
        write(' time: '),
        get_ncalls(N),
        (N > 0 -> write(N) , write(' inferences in ') ; true),
        write(Secs),
        write(' seconds, including printing'),
        nl.
%%% ***

%%% ****if* PTTP/timed_call
%%% DESCRIPTION
%%%   A query can be timed by timed_call(query,'Proof').
%%% NOTES
%%%   assumes that statistics(cputime,T) binds T to run-time in seconds
%%%   different Prolog systems have different ways to get this information
%%% SOURCE

timed_call(X,Type):-timed_call(X,Type,_Secs).
timed_call(X,Type,Secs) :-
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
	nl,
	write('prove for cost '),
	write(Level),
	write(' proof...  '),
	current_output(S),
	flush_output(S).
%%% ***


%%% ***

%%% ****if* PTTP/write_proof
%%% DESCRIPTION
%%%   write_proof prints the proof that PTTP finds
%%% SOURCE

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
	(Head == query ->
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




%%% Proof printing is turned on by print_proof,
%%% off by dont_print_proof.
%%%
%%% print_proof or dont_print_proof should be
%%% executed before the problem is compiled.

print_proof :-                          % enable proof printing
        retract(compile_proof_printing),
        fail.
print_proof :-
        assert(compile_proof_printing).

dont_print_proof :-                     % disable proof printing
        retract(compile_proof_printing),
        fail.
dont_print_proof.


:- print_proof.                         % default is to print proof







%%% Inference counting is turned on by count_inferences,
%%% off by dont_count_inferences.
%%%
%%% Inferences are counted by retracting the current count
%%% and asserting the incremented count, so inference counting
%%% is very slow.

count_inferences :-                     % enables compilation of inference counting
        retract(count_inferences_pred(_)),% this slows down the code substantially
        fail.
count_inferences :-
        assert(count_inferences_pred(inc_ncalls)).

dont_count_inferences :-                % disables compilation of inference counting
        retract(count_inferences_pred(_)),% this is the default for acceptable performance
        fail.
dont_count_inferences :-
        assert(count_inferences_pred(true)).

:- dont_count_inferences.               % default is to not count inferences

%%% Transformation to add inference counting to a clause.

add_count_inferences((Head :- Body),(Head :- Body1)) :-
        functor(Head,query,_) ->
                Body1 = Body;
        %true ->
                count_inferences_pred(P),
                conjoin(P,Body,Body1).

clear_ncalls :- flag(ncalls,_,0).
inc_ncalls :- flag(ncalls,X,X+1).
get_ncalls(X):- flag(ncalls,X,X).

%%% Search tracing is turned on by trace_search,
%%% off by dont_trace_search.

trace_search :-                         % enables prove progress reports
        retract(trace_search_progress_pred(_)),
        fail.
trace_search :-
        assert(trace_search_progress_pred(write_search_progress)).

dont_trace_search :-                    % disables prove progress reports
        retract(trace_search_progress_pred(_)),
        fail.
dont_trace_search :-
        assert(trace_search_progress_pred(nop)).

:- trace_search.                        % default is to dtrace searching


:- was_dynamic(use_sound_unification/1).
use_sound_unification(unify_with_occurs_check).

add_sound_unification((Head :- Body),(Head1 :- Body1)) :- use_sound_unification(Pred),!,
        linearize(Pred,Head,Head1,[],_,true,Matches),
        conjoin(Matches,Body,Body1).
add_sound_unification((Head :- Body),(Head :- Body)).


add_proof_recording((Head :- Body),(Head1 :- Body1)) :-
        Head =.. L,
        append(L,[Proof,ProofEnd],L1),
        Head1 =.. L1,
        add_proof_recording_args(Body,Proof,ProofEnd,Body2),
        (functor(Head,query,_) ->
                conjoin(Body2,write_proved(Proof,ProofEnd),Body1);
        %true ->
                Body1 = Body2).

add_proof_recording_args(Body,Proof,ProofEnd,Body1) :-
        Body = (A , B) ->
                add_proof_recording_args(A,Proof,Proof1,A1),
                add_proof_recording_args(B,Proof1,ProofEnd,B1),
                conjoin(A1,B1,Body1);
        Body = (A ; B) ->
                add_proof_recording_args(A,Proof,ProofEnd,A1),
                add_proof_recording_args(B,Proof,ProofEnd,B1),
                disjoin(A1,B1,Body1);
        Body =.. [prove,Goal|L] ->
                add_proof_recording_args(Goal,Proof,ProofEnd,Goal1),
                Body1 =.. [prove,Goal1|L];
        Body = infer_by(X) ->
                Body1 = (Proof = [X|ProofEnd]);
        Body = fail ->
                Body1 = Body;
        builtin(Body) ->
                Proof = ProofEnd,
                Body1 = Body;
        %true ->
                Body =.. L,
                append(L,[Proof,ProofEnd],L1),
                Body1 =.. L1.


add_ancestor((Head :- Body),(Head1 :- Body1)) :-
        functor(Head,query,_) ->
                Head1 = Head,
                add_ancestor_args(Body,[[],[]],Body1);
        %true ->
                Head =.. L,
                append(L,[PosAncestors,NegAncestors],L1),
                Head1 =.. L1,
                add_ancestor_args(Body,[NewPosAncestors,NewNegAncestors],Body2),
                (Body == Body2 ->
                        Body1 = Body2;
                negative_literal(Head) ->
                        NewPosAncestors = PosAncestors,
                        conjoin((NewNegAncestors = [Head|NegAncestors]),Body2,Body1);
                %true ->
                        NewNegAncestors = NegAncestors,
                        conjoin((NewPosAncestors = [Head|PosAncestors]),Body2,Body1)).

add_ancestor_args(Body,AncestorLists,Body1) :-
        Body = (A , B) ->
                add_ancestor_args(A,AncestorLists,A1),
                add_ancestor_args(B,AncestorLists,B1),
                conjoin(A1,B1,Body1);
        Body = (A ; B) ->
                add_ancestor_args(A,AncestorLists,A1),
                add_ancestor_args(B,AncestorLists,B1),
                disjoin(A1,B1,Body1);
        Body =.. [prove,Goal|L] ->
                add_ancestor_args(Goal,AncestorLists,Goal1),
                Body1 =.. [prove,Goal1|L];
        builtin(Body) ->
                Body1 = Body;
        %true ->
                Body =.. L,
                append(L,AncestorLists,L1),
                Body1 =.. L1.

ancestor_tests(P,N,Result) :-
        P == query ->
                Result = true;
        %true ->
                negated_functor(P,NotP),
                N2 is N - 2,            % N - 2 due to two ancestor-list arguments
                functor(Head1,P,N2),
                Head1 =.. [P|Args1],
                Head2 =.. [NotP|Args1],
                append(Args1,[PosAncestors,NegAncestors],Args),
                Head =.. [P|Args],
                (negative_functor(P) ->
                        C1Ancestors = NegAncestors, C2Ancestors = PosAncestors;
                %true ->
                        C1Ancestors = PosAncestors, C2Ancestors = NegAncestors),
                C1 = (Head :- identical_member(Head1,C1Ancestors), !, fail),
                count_inferences_pred(IncNcalls),
                (N2 = 0 ->              % special case for propositional calculus
                        conjoin((identical_member(Head2,C2Ancestors) , !),IncNcalls,V);
                %true ->
                        conjoin(unifiable_member(Head2,C2Ancestors),IncNcalls,V)),
                (compile_proof_printing ->
                        conjoin(V,infer_by(red),V1);
                %true ->
                        V1 = V),
                C2 = (Head :- V1),
                conjoin(C1,C2,Result).


:- was_dynamic(compile_complete_search/0).
compile_complete_search.

:- ensure_loaded(io).

add_complete_search(HeadBody,HeadBody) :-  (\+ compile_complete_search ),!.
add_complete_search((Head :- Body),(Head1 :- Body1)) :-
  Head =.. [_|HeadArgs],((
        Head =.. L,
        append(L,[DepthIn,DepthOut],L1),
        Head1 =.. L1,
        (functor(Head,query,_) ->
                add_complete_search_args(Head,Body,DepthIn,DepthOut,Body1);
        nonzero_search_cost(Body,HeadArgs,Cost) ->
                add_complete_search_args(Head,Body,Depth1,DepthOut,Body2),
                conjoin((DepthIn >= Cost , Depth1 is DepthIn - Cost),Body2,Body1);
        %true ->
                add_complete_search_args(Head,Body,DepthIn,DepthOut,Body1)))).


add_complete_search_args(Head,Body,DepthInOut,DepthInOut,Body) :-  (\+ compile_complete_search ),!.   
add_complete_search_args(Head,Body,DepthIn,DepthOut,Body1) :-  
   Head =.. [_|HeadArgs],
       	
        ((Body = (A , B) ->
                add_complete_search_args(Head,A,DepthIn,Depth1,A1),
                add_complete_search_args(Head,B,Depth1,DepthOut,B1),
                conjoin(A1,B1,Body1);
        Body = (A ; B) ->
                search_cost(A,HeadArgs,CostA),
                search_cost(B,HeadArgs,CostB),
                (CostA < CostB ->
                        add_complete_search_args(Head,A,DepthIn,DepthOut,A1),
                        add_complete_search_args(Head,B,Depth1,DepthOut,B2),
                        Cost is CostB - CostA,
                        conjoin((DepthIn >= Cost , Depth1 is DepthIn - Cost),B2,B1);
                CostA > CostB ->
                        add_complete_search_args(Head,A,Depth1,DepthOut,A2),
                        add_complete_search_args(Head,B,DepthIn,DepthOut,B1),
                        Cost is CostA - CostB,
                        conjoin((DepthIn >= Cost , Depth1 is DepthIn - Cost),A2,A1);
                %true ->
                        add_complete_search_args(Head,A,DepthIn,DepthOut,A1),
                        add_complete_search_args(Head,B,DepthIn,DepthOut,B1)),
                disjoin(A1,B1,Body1);
        Body = prove(Goal,Max,Min,Inc) ->
                PrevInc is Min + 1,
                add_complete_search_args(Head,Goal,DepthIn1,DepthOut1,Goal1),
                DepthIn = DepthOut,
                Body1 = prove(Goal1,Max,Min,Inc,PrevInc,DepthIn1,DepthOut1);
        Body = prove(Goal,Max,Min) ->
                add_complete_search_args(Head,prove(Goal,Max,Min,1),DepthIn,DepthOut,Body1);
        Body = prove(Goal,Max) ->
                add_complete_search_args(Head,prove(Goal,Max,0),DepthIn,DepthOut,Body1);
        Body = prove(Goal) ->
                add_complete_search_args(Head,prove(Goal,1000000),DepthIn,DepthOut,Body1);
        functor(Body,search_cost,_) ->
                DepthIn = DepthOut,
                Body1 = true;
        builtin(Body) ->
                DepthIn = DepthOut,
                Body1 = Body;
        %true ->
                Body =.. L,
                append(L,[DepthIn,DepthOut],L1),
                Body1 =.. L1)).

%%% ***

%%% ****if* PTTP/pttp1
%%% SOURCE

pttp(X) :-
        time(pttp1(X),'Compilation').

pttp1(X) :- retract_last,
        nl,
        write('PTTP input formulas:'),
        apply_to_conjuncts(X,write_clause,_),
        nl,
        clauses(X,X0,1),
        apply_to_conjuncts(X0,add_count_inferences,X1),
        apply_to_conjuncts(X1,add_ancestor,X2),
        predicates(X2,Preds0),
        list_reverse(Preds0,Preds),
        procedures_with_ancestor_tests(Preds,X2,X3),
        apply_to_conjuncts(X3,add_sound_unification,X4),
        apply_to_conjuncts(X4,add_complete_search,X5),
        (compile_proof_printing ->
                apply_to_conjuncts(X5,add_proof_recording,Y);
        %true ->
                Y = X5),

        pttp2(Y),
        !.
%%% ***
%%% ****if* PTTP/pttp2
%%% SOURCE

:- was_dynamic(kill_me/1).
retract_last:- retractall(query(_,_,_,_)), forall(retract(kill_me(C)),retractall(C)).

assert_clause(C,_):-assert(C),assert(kill_me(C)).
pttp2(Y) :-
%	nl,
%	write('PTTP output formulas:'),
%	apply_to_conjuncts(Y,write_clause,_),
%	nl,
        nl,
        write('PTTP output formulas:'),
        apply_to_conjuncts(Y,write_clause,_),
%         apply_to_conjuncts(Y,assert_clause,_),!.

        nl,
        nl,

        File = 'temp.prolog',                     % Quintus Prolog on Sun
%       File = 'darwin:>stickel>pttp>temp.prolog',% change file name for other systems

        myopen(File,write,OutFile),
        write_clause_to_file(Y,OutFile),
        myclose(OutFile),
        compile_with_cyclic_term(File),
        nl,
        !.

verbose_proof_printing.

write_proved(Proof,ProofEnd) :-
        write('proved'),
	verbose_proof_printing ->
	        write(' by:'),
		write_proof(Proof,ProofEnd);
	%true->
		length(Proof,X),
		write(' qed ;-) ':X).

write_proof(Proof,ProofEnd) :-
        Proof == ProofEnd,
        !.
write_proof([X|Y],ProofEnd) :-
	nl,
        write(' '),
        write(X),
        write_proof(Y,ProofEnd).
