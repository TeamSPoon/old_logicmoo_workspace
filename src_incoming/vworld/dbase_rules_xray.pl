:-module(dbase_rules_pttp,[xray/1]).

:-style_check(-singleton).

not_yet_implemented:-throw(not_yet_implemented).
error_in_lemmatize:-throw(error_in_lemmatize).
:-dynamic(query/2).
:-dynamic(query/4).
:-dynamic(query/6).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date: 15/03/98   File: defaults.pl
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%% 18/03/95 Created                                                          %%
%% 14/07/96 added configuration utilities
%% 15/03/98 added pttp config and optional proof printing
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog defaults.pl                                               %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% :- ensure_loaded(base).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date: 14/07/96   File: base.pl
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%% 25/03/96 Created                                                          %%
%% 14/07/96 removed configuration utilities (-> defaults.pl)
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog base.pl                                                   %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% ----------------------------------------------------------------------
%%% 
%%%       additional predicates
%%%
%%%

apply_to_list([],_,[]).
apply_to_list([Elem|List],P,[Elem1|List1]) :-
	T =.. [P,Elem,Elem1],
	call(T),
	apply_to_list(List,P,List1).


apply_to_list_flat([],_,[]).
apply_to_list_flat([Elem|List],P,ResList) :-
	T =.. [P,Elem,Result1],
	call(T),
	apply_to_list(List,P,List1),
	append(Result1,List1,ResList).


%%% mymember(?Element, ?Set)
%%% is true when Set is a list, and Element occurs in it.  It may be used
%%% to test for an element or to enumerate all the elements by backtracking.
%%% Indeed, it may be used to generate the Set!

mymember(X, [X|_]    ).
mymember(X, [_,X|_]  ).
mymember(X, [_,_,X|_]).
mymember(X, [_,_,_|L]) :-
	mymember(X, L).

%%% myselect(?Element, ?Set, ?Residue)
%%% is true when Set is a list, Element occurs in Set, and Residue is
%%% everything in Set except Element (things stay in the same order).

myselect(X, [X|R],     R        ).
myselect(X, [A,X|R],   [A|R]    ).
myselect(X, [A,B,X|R], [A,B|R]  ).
myselect(X, [A,B,C|L], [A,B,C|R]) :-
	myselect(X, L, R).

%%% mysubset(+Set1, +Set2)
%%% is true when each member of Set1 occurs in Set2.
%%%

mysubset([],_).
mysubset([X|R],L) :-
	mymember(X,L),
	mysubset(R,L).


concatenate(String1,String2,String) :-
	name(String1,L1),
	name(String2,L2),
	append(L1,L2,L),
	name(String,L),
	!.

% :- ensure_loaded(pttp).

% :- ensure_loaded(old_pttp).
:- set_prolog_flag(print_depth,1000).
:- set_prolog_flag(variable_names,off).

:- dynamic(count_inferences_pred/1).
:- dynamic(trace_search_progress_pred/1).
:- dynamic(compile_proof_printing/0).
:- dynamic(ncalls/1).

%%% Sound unification.
%%%
%%% `add_sound_unification' transforms a clause so that its
%%% head has no repeated variables.  Unifying a goal with
%%% the clause head can then be done soundly without the occurs
%%% check.  The rest of the unification can then be done in
%%% the body of the transformed clause, using the sound `unify'
%%% predicate.
%%%
%%% For example,
%%%    p(X,Y,f(X,Y)) :- true.
%%% is transformed into
%%%    p(X,Y,f(X1,Y1)) :- unify(X,X1), unify(Y,Y1).

add_sound_unification((Head :- Body),(Head1 :- Body1)) :-
        linearize(Head,Head1,[],_,true,Matches),
        conjoin(Matches,Body,Body1).

linearize(TermIn,TermOut,VarsIn,VarsOut,MatchesIn,MatchesOut) :-
        nonvar(TermIn) ->
                functor(TermIn,F,N),
                myfunctor(TermOut,F,N),
                linearize_args(TermIn,TermOut,VarsIn,VarsOut,MatchesIn,MatchesOut,1,N);
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

%%% Sound unification algorithm with occurs check that is called
%%% by code resulting from the 'add_sound_unification' transformation.
%%% This should be coded in a lower-level language for efficiency.
unify(X,Y) :- !, unify_with_occurs_check(X,Y).

unify(X,Y) :-
        var(X) ->
                (var(Y) ->
                        X = Y;
                %true ->
                        functor(Y,_,N),
                        (N = 0 ->
                                true;
                        N = 1 ->
                                arg(1,Y,Y1), not_occurs_in(X,Y1);
                        %true ->
                                not_occurs_in_args(X,Y,N)),
                        X = Y);
        var(Y) ->
                functor(X,_,N),
                (N = 0 ->
                        true;
                N = 1 ->
                        arg(1,X,X1), not_occurs_in(Y,X1);
                %true ->
                        not_occurs_in_args(Y,X,N)),
                X = Y;
        %true ->
                functor(X,F,N),
                functor(Y,F,N),
                (N = 0 ->
                        true;
                N = 1 ->
                        arg(1,X,X1), arg(1,Y,Y1), unify(X1,Y1);
                %true ->
                        unify_args(X,Y,N)).

unify_args(X,Y,N) :-
        N = 2 ->
                arg(2,X,X2), arg(2,Y,Y2), unify(X2,Y2),
                arg(1,X,X1), arg(1,Y,Y1), unify(X1,Y1);
        %true ->
                arg(N,X,Xn), arg(N,Y,Yn), unify(Xn,Yn),
                N1 is N - 1, unify_args(X,Y,N1).

not_occurs_in(Var,Term) :-
        Var == Term ->
                fail;
        var(Term) ->
                true;
        %true ->
                functor(Term,_,N),
                (N = 0 ->
                        true;
                N = 1 ->
                        arg(1,Term,Arg1), not_occurs_in(Var,Arg1);
                %true ->
                        not_occurs_in_args(Var,Term,N)).

not_occurs_in_args(Var,Term,N) :-
        N = 2 ->
                arg(2,Term,Arg2), not_occurs_in(Var,Arg2),
                arg(1,Term,Arg1), not_occurs_in(Var,Arg1);
        %true ->
                arg(N,Term,ArgN), not_occurs_in(Var,ArgN),
                N1 is N - 1, not_occurs_in_args(Var,Term,N1).

%%% Depth-first iterative-deepening search.
%%%
%%% 'add_complete_search' adds arguments DepthIn and DepthOut
%%% to each PTTP literal to control bounded depth-first
%%% search.  When a literal is called,
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

add_complete_search((Head :- Body),(Head1 :- Body1)) :-
        Head =.. L,
        append(L,[DepthIn,DepthOut],L1),
        Head1 =.. L1,
        (functor(Head,query,_) ->
                add_complete_search_args(Body,DepthIn,DepthOut,Body1);
        nonzero_search_cost(Body,Cost) ->
                add_complete_search_args(Body,Depth1,DepthOut,Body2),
                conjoin((DepthIn >= Cost , Depth1 is DepthIn - Cost),Body2,Body1);
        %true ->
                add_complete_search_args(Body,DepthIn,DepthOut,Body1)).

add_complete_search_args(Body,DepthIn,DepthOut,Body1) :-
        Body = (A , B) ->
                add_complete_search_args(A,DepthIn,Depth1,A1),
                add_complete_search_args(B,Depth1,DepthOut,B1),
                conjoin(A1,B1,Body1);
        Body = (A ; B) ->
                search_cost(A,CostA),
                search_cost(B,CostB),
                (CostA < CostB ->
                        add_complete_search_args(A,DepthIn,DepthOut,A1),
                        add_complete_search_args(B,Depth1,DepthOut,B2),
                        Cost is CostB - CostA,
                        conjoin((DepthIn >= Cost , Depth1 is DepthIn - Cost),B2,B1);
                CostA > CostB ->
                        add_complete_search_args(A,Depth1,DepthOut,A2),
                        add_complete_search_args(B,DepthIn,DepthOut,B1),
                        Cost is CostA - CostB,
                        conjoin((DepthIn >= Cost , Depth1 is DepthIn - Cost),A2,A1);
                %true ->
                        add_complete_search_args(A,DepthIn,DepthOut,A1),
                        add_complete_search_args(B,DepthIn,DepthOut,B1)),
                disjoin(A1,B1,Body1);
        Body = search(Goal,Max,Min,Inc) ->
                PrevInc is Min + 1,
                add_complete_search_args(Goal,DepthIn1,DepthOut1,Goal1),
                DepthIn = DepthOut,
                Body1 = search(Goal1,Max,Min,Inc,PrevInc,DepthIn1,DepthOut1);
        Body = search(Goal,Max,Min) ->
                add_complete_search_args(search(Goal,Max,Min,1),DepthIn,DepthOut,Body1);
        Body = search(Goal,Max) ->
                add_complete_search_args(search(Goal,Max,0),DepthIn,DepthOut,Body1);
        Body = search(Goal) ->
                add_complete_search_args(search(Goal,1000000),DepthIn,DepthOut,Body1);
        functor(Body,search_cost,_) ->
                DepthIn = DepthOut,
                Body1 = true;
        builtin(Body) ->
                DepthIn = DepthOut,
                Body1 = Body;
        %true ->
                Body =.. L,
                append(L,[DepthIn,DepthOut],L1),
                Body1 =.. L1.

nonzero_search_cost(Body,Cost) :-
        search_cost(Body,Cost),
        Cost > 0.

%%% Search cost is computed by counting literals in the body.
%%% It can be given explicitly instead by including a number, as in
%%%   p :- search_cost(3).     (ordinarily, cost would be 0)
%%%   p :- search_cost(1),q,r. (ordinarily, cost would be 2)
%%%   p :- search_cost(0),s.   (ordinarily, cost would be 1)
%%%
%%% Propositional goals are not counted into the search cost so
%%% that fully propositional problems can be solved without
%%% deepening when iterative-deepening search is used.

search_cost(Body,N) :-
        Body = search_cost(M) ->
                N = M;
        Body = (A , B) ->
                (A = search_cost(M) ->  % if first conjunct is search_cost(M),
                        N = M;          % search cost of the entire conjunction is M
                %true ->
                        search_cost(A,N1),
                        search_cost(B,N2),
                        N is N1 + N2);
        Body = (A ; B) ->
                search_cost(A,N1),
                search_cost(B,N2),
                min(N1,N2,N);
        builtin(Body) ->
                N = 0;
        functor(Body,_,2) ->  % zero-cost 2-ary (0-ary plus ancestor lists) predicates
                N = 0;        % heuristic for propositional problems
        %true ->
                N = 1.

%%% Depth-first iterative-deepening search can be
%%% specified for a goal by wrapping it in a call
%%% on the search predicate:
%%%    search(Goal,Max,Min,Inc)
%%% Max is the maximum depth to search (defaults to a big number),
%%% Min is the minimum depth to search (defaults to 0),
%%% Inc is the amount to increment the bound each time (defaults to 1).
%%%
%%% Depth-first iterative deepening search can be
%%% specified inside the PTTP formula by compiling
%%%   query :- search(p(b,a,c),Max,Min,Inc)
%%% and executing
%%%   query.
%%% or directly by the user by compiling
%%%   query :- p(b,a,c))
%%% and executing
%%%   search(query,Max,Min,Inc).
%%%
%%% The search(Goal,Max,Min,Inc) predicate adds
%%% DepthIn and DepthOut arguments to its goal argument.

search(Goal,Max,Min,Inc) :-
        PrevInc is Min + 1,
        add_complete_search_args(Goal,DepthIn,DepthOut,Goal1),
        (compile_proof_printing ->
                add_proof_recording_args(Goal1,_Proof,_ProofEnd,Goal2);
        %true ->
                Goal2 = Goal1),
        !,
        search(Goal2,Max,Min,Inc,PrevInc,DepthIn,DepthOut).

search(Goal,Max,Min) :-
        search(Goal,Max,Min,1).

search(Goal,Max) :-
        call_with_inference_limit(search(Goal,Max,0),10000000,R),
        dmsg(call_with_inference_limit(R)),
        !,not(R==inference_limit_exceeded).

search(Goal) :-!, catch(call_with_time_limit(30, search(Goal,1000)),time_limit_exceeded, (dmsg(time_limit_exceeded(Goal)), !,fail)).

search(Goal) :-
        search(Goal,1000000).

%%% Actual search driver predicate.
%%% Note that depth-bounded execution of Goal is enabled by
%%% the fact that the DepthIn and DepthOut arguments of
%%% search are also the DepthIn and DepthOut arguments of Goal.

search(_Goal,Max,Min,_Inc,_PrevInc,_DepthIn,_DepthOut) :-
        Min > Max,
        !,
        fail.
search(Goal,_Max,Min,_Inc,PrevInc,DepthIn,DepthOut) :-
        trace_search_progress_pred(P1),
        L1 =.. [P1,Min],
        call(L1),
        DepthIn = Min,
        call(Goal),
        DepthOut < PrevInc.   % fail if this solution was found in previous search
search(Goal,Max,Min,Inc,_PrevInc,DepthIn,DepthOut) :-
        Min1 is Min + Inc,
        search(Goal,Max,Min1,Inc,Inc,DepthIn,DepthOut).

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

/*
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

*/
add_ancestor_args(Body,AncestorLists,Body1) :-
        Body = (A , B) ->
                add_ancestor_args(A,AncestorLists,A1),
                add_ancestor_args(B,AncestorLists,B1),
                conjoin(A1,B1,Body1);
        Body = (A ; B) ->
                add_ancestor_args(A,AncestorLists,A1),
                add_ancestor_args(B,AncestorLists,B1),
                disjoin(A1,B1,Body1);
        Body =.. [search,Goal|L] ->
                add_ancestor_args(Goal,AncestorLists,Goal1),
                Body1 =.. [search,Goal1|L];
        builtin(Body) ->
                Body1 = Body;
        %true ->
                Body =.. L,
                append(L,AncestorLists,L1),
                Body1 =.. L1.
/*
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
*/

procedures_with_ancestor_tests([[P,N]|Preds],Clauses,Procs) :-
        procedure(P,N,Clauses,Proc1),
        ancestor_tests(P,N,Tests),
        conjoin(Tests,Proc1,Proc),
        procedures_with_ancestor_tests(Preds,Clauses,Procs2),
        conjoin(Proc,Procs2,Procs).
procedures_with_ancestor_tests([],_Clauses,true).

identical_member(X,[Y|_])  :-           % run-time predicate for
        X == Y,                         % finding identical ancestor
        !.
identical_member(X,[_|L]) :-
        identical_member(X,L).

unifiable_member(X,[Y|_]) :-            % run-time predicate for
        unify(X,Y).                     % finding complementary ancestor
unifiable_member(X,[_|L]) :-
        unifiable_member(X,L).

%%% Proof Printing.
%%%
%%% Add extra arguments to each goal so that information
%%% on what inferences were made in the proof can be printed
%%% at the end.

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
        Body =.. [search,Goal|L] ->
                add_proof_recording_args(Goal,Proof,ProofEnd,Goal1),
                Body1 =.. [search,Goal1|L];
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

/*
write_proved(Proof,ProofEnd) :-
        write('proved by'),
        write_proof(Proof,ProofEnd).

write_proof(Proof,ProofEnd) :-
        Proof == ProofEnd,
        !.
write_proof([X|Y],ProofEnd) :-
        write(' '),
        write(X),
        write_proof(Y,ProofEnd).
*/
%%% Negation normal form to Prolog clause translation.
%%% Include a literal in the body of each clause to
%%% indicate the number of the formula the clause came from.
/*
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
                conjoin(infer_by(WffNum),Body1,Body);
        %true ->
                Body = Body1),
        clauses(A,Lits,L1,WffNum),
        conjoin((Lit :- Body),L1,L).
clauses(_,[],true,_).
*/
head_literals(Wff,L) :-
        Wff = (A :- B) ->               % contrapositives are not formed for A :- ... inputs
                head_literals(A,L);
        Wff = (A , B) ->
                head_literals(A,L1),
                head_literals(B,L2),
                union(L1,L2,L);
        Wff = (A ; B) ->
                head_literals(A,L1),
                head_literals(B,L2),
                union(L1,L2,L);
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

%%% predicates returns a list of the predicates appearing in a formula.

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

%%% procedure returns a conjunction of the clauses
%%% with head predicate P/N.

procedure(P,N,Clauses,Proc) :-
        Clauses = (A , B) ->
                procedure(P,N,A,ProcA),
                procedure(P,N,B,ProcB),
                conjoin(ProcA,ProcB,Proc);
        (Clauses = (A :- B) , functor(A,P,N)) ->
                Proc = Clauses;
        %true ->
                Proc = true.

%%% pttp is the PTTP compiler top-level predicate.

old_pttp(X) :-
        time(old_pttp1(X),'Compilation').

old_pttp1(X) :-
        nl,
        write('PTTP input formulas:'),
        apply_to_conjuncts(X,write_clause,_),
        nl,
        clauses(X,X0,1),
        apply_to_conjuncts(X0,add_count_inferences,X1),
        apply_to_conjuncts(X1,add_ancestor,X2),
        predicates(X2,Preds0),
        reverse(Preds0,Preds),
        procedures_with_ancestor_tests(Preds,X2,X3),
        apply_to_conjuncts(X3,add_sound_unification,X4),
        apply_to_conjuncts(X4,add_complete_search,X5),
        (compile_proof_printing ->
                apply_to_conjuncts(X5,add_proof_recording,Y);
        %true ->
                Y = X5),
        nl,
        write('PTTP output formulas:'),
        apply_to_conjuncts(Y,write_clause,_),
        nl,
        nl,

        File = 'temp.prolog',                     % Quintus Prolog on Sun
%       File = 'darwin:>stickel>pttp>temp.prolog',% change file name for other systems

        open(File,write,OutFile),
        write_clause_to_file(Y,OutFile),
        close(OutFile),
        dmsg(fake(compile(File))),
        nl,
        !.

write_clause_to_file((A,B),OutFile) :-
	write_clause_to_file(A,OutFile),
	write_clause_to_file(B,OutFile),
        !.
write_clause_to_file(A,OutFile) :-
	nl(OutFile),
	write(OutFile,A),
	write(OutFile,.),
	!.


/*
query(M) :-                             % call query with depth bound M
        compile_proof_printing -> 
                query(M,_N,_Proof,_ProofEnd);
        %true ->
                query(M,_N).

query :-                                % unbounded search of query
        query(1000000).
*/
%%% Utility functions.

%%% Sometimes the `functor' predicate doesn't work as expected and
%%% a more comprehensive predicate is needed.  The `myfunctor'
%%% predicate overcomes the problem of functor(X,13,0) causing
%%% an error in Symbolics Prolog.  You may need to use it if
%%% 'functor' in your Prolog system fails to construct or decompose
%%% terms that are numbers or constants.

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

nop(_).

append([],L,L).
append([X|L1],L2,[X|L3]) :-
        append(L1,L2,L3).

reverse([X|L0],L) :-
        reverse(L0,L1),
        append(L1,[X],L).
reverse([],[]).

union([X|L1],L2,L3) :-
        identical_member(X,L2),
        !,
        union(L1,L2,L3).
union([X|L1],L2,[X|L3]) :-
        union(L1,L2,L3).
union([],L,L).

intersection([X|L1],L2,[X|L3]) :-
        identical_member(X,L2),
        !,
        intersection(L1,L2,L3).
intersection([_X|L1],L2,L3) :-
        intersection(L1,L2,L3).
intersection([],_L,[]).

min(X,Y,Min) :-
        X =< Y ->
                Min = X;
        %true ->
                Min = Y.

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

/*
negated_functor(F,NotF) :-
        name(F,L),
        name(not_,L1),
        (append(L1,L2,L) ->
                true;
        %true ->
                append(L1,L,L2)),
        name(NotF,L2).

negated_literal(Lit,NotLit) :-
        Lit =.. [F1|L1],
        negated_functor(F1,F2),
        (var(NotLit) ->
                NotLit =.. [F2|L1];
        %true ->
                NotLit =.. [F2|L2],
                L1 == L2).

negative_functor(F) :-
        name(F,L),
        name(not_,L1),
        append(L1,_,L).

negative_literal(Lit) :-
        functor(Lit,F,_),
        negative_functor(F).
*/

list_append([X|L1],L2,[X|L3]) :-
	list_append(L1,L2,L3).
list_append([],L,L).

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

negated_literal(A,B):-isVar(A),!,trace,B=not(A),!.
negated_literal(-A,B):-negated_literal(A,AA),!,negated_literal(AA,B).
negated_literal(A,B):-var(B),!,negated_literal_0(A,B).
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

apply_to_conjuncts(Wff,P,Wff1) :-
        Wff = (A , B) ->
                apply_to_conjuncts(A,P,A1),
                apply_to_conjuncts(B,P,B1),
                conjoin(A1,B1,Wff1);
        %true ->
                T1 =.. [P,Wff,Wff1],
                call(T1).

write_clause(A) :-
        nl,
        write(A),
        write(.).

write_clause(A,_) :-                    % 2-ary predicate can be used as
        write_clause(A).                % argument of apply_to_conjuncts

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

clear_ncalls :-
        retract(ncalls(_)),
        fail.
clear_ncalls :-
        assert(ncalls(0)).

inc_ncalls :-
        retract(ncalls(N)),
        N1 is N + 1,
        assert(ncalls(N1)),
        !.

%%% Search tracing is turned on by trace_search,
%%% off by dont_trace_search.

trace_search :-                         % enables search progress reports
        retract(trace_search_progress_pred(_)),
        fail.
trace_search :-
        assert(trace_search_progress_pred(write_search_progress)).

dont_trace_search :-                    % disables search progress reports
        retract(trace_search_progress_pred(_)),
        fail.
dont_trace_search :-
        assert(trace_search_progress_pred(nop)).

:- trace_search.                        % default is to trace searching

write_search_progress(Level) :-
        ncalls(N),
        (N > 0 -> write(N) , write(' inferences so far.') ; true),
        % nl,
        % write('Begin cost '),
        write(Level),
        write('.'),
        %write(' search...  '),
        !.

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
        ncalls(N),
        (N > 0 -> write(N) , write(' inferences in ') ; true),
        write(Secs),
        write(' seconds, including printing'),
        nl.

%%% List of builtin predicates that can appear in clause bodies.
%%% No extra arguments are added for ancestor goals or depth-first
%%% iterative-deepening search.  Also, if a clause body is
%%% composed entirely of builtin goals, the head is not saved
%%% as an ancestor for use in reduction or pruning.
%%% This list can be added to as required.

old_pttp_builtin(T) :-
        functor(T,F,N),
        old_pttp_builtin(F,N).

old_pttp_builtin(!,0).
old_pttp_builtin(true,0).
old_pttp_builtin(fail,0).
old_pttp_builtin(succeed,0).
old_pttp_builtin(trace,0).
old_pttp_builtin(atom,1).
old_pttp_builtin(integer,1).
old_pttp_builtin(number,1).
old_pttp_builtin(atomic,1).
old_pttp_builtin(constant,1).
old_pttp_builtin(functor,3).
old_pttp_builtin(arg,3).
old_pttp_builtin(var,1).
old_pttp_builtin(nonvar,1).
old_pttp_builtin(call,1).
old_pttp_builtin(=,2).
old_pttp_builtin(\=,2).
old_pttp_builtin(==,2).
old_pttp_builtin(\==,2).
old_pttp_builtin(>,2).
old_pttp_builtin(<,2).
old_pttp_builtin(>=,2).
old_pttp_builtin(=<,2).
old_pttp_builtin(is,2).
old_pttp_builtin(display,1).
old_pttp_builtin(write,1).
old_pttp_builtin(nl,0).
old_pttp_builtin(infer_by,_).
old_pttp_builtin(write_proved,_).
old_pttp_builtin(search,_).
old_pttp_builtin(search_cost,_).
old_pttp_builtin(unify,_).
old_pttp_builtin(identical_member,_).
old_pttp_builtin(unifiable_member,_).
old_pttp_builtin(inc_ncalls,0).

%%% Theorem proving examples from
%%%   Chang, C.L. and R.C.T. Lee.
%%%   Symbolic Logic and Mechanical Theorem Proving.
%%%   Academic Press, New York, 1973, pp. 298-305.

%%% Note that the search driver predicate
%%% can be invoked by search(query) as in
%%% chang_lee_example8 or can be explicitly
%%% included in the query as in chang_lee_example2.


/*
pttp(X):-old_pttp(X),!.
pttp1(X):-old_pttp1(X),!.
*/

pttp(X):-dpttp(X),!.
pttp1(X):-dpttp1(X),!.


% :- ensure_loaded(builtin).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date:  7/11/95   File: builtin.pl                   %%
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%%  7/11/95 Created                                                          %%
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog builtin.pl                                                %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% List of builtin predicates that can appear in clause bodies.
%%% No extra arguments are added for ancestor goals or depth-first
%%% iterative-deepening search.  Also, if a clause body is
%%% composed entirely of builtin goals, the head is not saved
%%% as an ancestor for use in reduction or pruning.
%%% This list can be added to as required.

builtin(T) :-
	nonvar(T),
        functor(T,F,N),
        builtin(F,N).

builtin(!,0).
builtin(true,0).
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
builtin(search,_).
builtin(search_cost,_).
builtin(unify,_).
builtin(identical_member,_).
builtin(unifiable_member,_).
builtin(inc_ncalls,0).
% --- compatibility predicates
builtin(justification,_).
builtin(compatible,_).
builtin(model_initialization,_).
% --- variable handling predicates
builtin(herbrand,1).
% hooks handling predicates
builtin(hook1,1).
builtin(hook2,1).
% --- lemma handling predicates
builtin(lemmatize,_).
builtin(dynamic_lemma,_).
builtin(static_lemma,_).
% --- misc
builtin(\+,1).

% --- special purpose predicates
builtin(P,_) :-
	builtin_predicate(P).
builtin_predicate(P) :-
	name(P,L),
        (name('_pl',L1);
	 name('_db',L1)
        ),
        append(_,L1,L).


%:- ensure_loaded(model).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date: 28/06/96   File: model.pl
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%% 26/06/95 Created                                                          %%
%% 25/06/96 moved compatible from defaults.pl
%% 28/06/96 updated cnf,make_matrix, make_clause
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog model.pl                                                  %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% :- ensure_loaded(base).

%%% ----------------------------------------------------------------------
%%% model_initialization/2
%%%     generates an initial model

model_initialization(Matrix,cmm(Model,NewMatrix)) :-
	catch(call_with_time_limit( 5, model_generation(matrix([],Matrix),NewMatrix,Model)),time_limit_exceeded,true),
%	display_cmm('INITIALIZATION',cmm(Model,NewMatrix)),
	!.

%%% Runtime predicate for consistency checking
%%%

compatible(MatrixJ,cmm(Model,ModelMatrix),cmm(NewModel,NewModelMatrix)) :-
	satisfy_matrix(MatrixJ,Model,NewModel) ->
	        append_matrix(MatrixJ,ModelMatrix,NewModelMatrix),
%		display_cmm('CHECKING - satisfy':MatrixJ,cmm(NewModel,NewModelMatrix)),
		verbose('+ satisfiability check');
	adjoin_matrix(MatrixJ,ModelMatrix,ModelMatrix1) ->
%	        display_cmm('CHECKING - generation I':MatrixJ,cmm(Model,ModelMatrix)),
		model_generation(ModelMatrix1,NewModelMatrix,NewModel);
%		display_cmm('CHECKING - generation II':MatrixJ,cmm(NewModel,NewModelMatrix));
	%true ->
%		display_cmm('CHECKING - failure':MatrixJ,cmm(Model,ModelMatrix)),
		verbose('- compatible *failure*'),
		fail.

%%% ----------------------------------------------------------------------
%%% satisfy_matrix/3
%%%     checks whether a given model, ModelI, satisfies
%%%     a formula in matrix form, Matrix, or whether the model
%%%     can be extended to a model, ModelO
%%%

satisfy_matrix([],Model,Model).
satisfy_matrix([Clause],ModelI,ModelO) :-
	!,
	satisfy_clause(Clause,ModelI,ModelO).
satisfy_matrix([C1,C2],ModelI,ModelO) :-
	!,
	satisfy_clause(C1,ModelI,Model1),
	satisfy_clause(C2,Model1,ModelO).
satisfy_matrix([C1,C2,C3|M],ModelI,ModelO) :-
	satisfy_clause(C1,ModelI,Model1),
	satisfy_clause(C2,Model1,Model2),
	satisfy_clause(C3,Model2,Model3),
	satisfy_matrix(M,Model3,ModelO).

satisfy_clause([],_,_) :- 
	!,
	fail.
satisfy_clause([L],ModelI,ModelO) :-
	!,
	satisfy_literal(L,ModelI,ModelO).
satisfy_clause([L1,L2],ModelI,ModelO) :-
	satisfy_literal(L1,ModelI,ModelO);
	satisfy_literal(L2,ModelI,ModelO).
satisfy_clause([L1,L2,L3|C],ModelI,ModelO) :-
	satisfy_literal(L1,ModelI,ModelO);
	satisfy_literal(L2,ModelI,ModelO);
	satisfy_literal(L3,ModelI,ModelO);
	satisfy_clause(C,ModelI,ModelO).

satisfy_literal(L,ModelI,ModelO) :-
        identical_member(L,ModelI) ->
	       ModelO = ModelI;
	%true  ->
	       negated_literal(L,NegL),
	       \+ identical_member(NegL,ModelI),
	       ModelO = [L|ModelI].


%%% ----------------------------------------------------------------------
%%% model_generation/2

model_generation(matrix(Units,Matrix),matrix(Units2,M1),Model) :-
	unit_extraction(Matrix,M1,Units1),
	% writeln(unit_extraction),dmsg(display_matrix(Matrix)),dmsg(display_matrix(M1)),dmsg(display_units(Units1)),
	dp(M1,Model1),
	append(Units,Units1,Units2),
/*	union(Units,Units1,Units2), append allows for multiple occurrences */
	append(Units2,Model1,Model),
	verbose('* model generation').

dp([],[]) :- !.
dp(M,_) :-
	mymember([],M),
	!,
	fail.
dp(M,NewMod) :-
	M=[[L|_]|_],
	split(M,L,M1,M2),
	unit_extraction(M1,M3,U3),
	unit_extraction(M2,M4,U4),
	(dp(M3,Mod),
         KK = [L|U3];
	 dp(M4,Mod),
         negated_literal(L,K),
	 KK = [K|U4]),
        append(KK,Mod,NewMod).


split([],_,[],[]).
split([C|M],L,M3,M4) :-
	split(M,L,M1,M2),
	negated_literal(L,NegL),
	(myselect(L,C,RestC) ->
	      M3=M1,
	      M4=[RestC|M2];
	myselect(NegL,C,RestC) ->
	      M3=[RestC|M1],
	      M4=M2;
        %true ->
	      M3=[C|M1],
	      M4=[C|M2]).

%%% ----------------------------------------------------------------------
%%% cnf/2

cnf(NNF,CNF) :-
	NNF = (F1,F2) ->
	    cnf(F1,CNF1),
	    cnf(F2,CNF2),
	    conjoin(CNF1,CNF2,CNF);
        NNF = (F1;(F2,F3)) ->
	    cnf((F1;F2),CNF1),
	    cnf((F1;F3),CNF2),
	    conjoin(CNF1,CNF2,CNF);
        NNF = ((F1,F2);F3) ->
	    cnf((F1;F3),CNF1),
	    cnf((F2;F3),CNF2),
	    conjoin(CNF1,CNF2,CNF);
	NNF = (F1;F2) ->
	    cnf(F1,CNF1),
	    cnf(F2,CNF2),
            (cnf_p(CNF1,CNF2,F1,F2) ->
	           disjoin(CNF1,CNF2,CNF);
	    %true ->
		   disjoin(CNF1,CNF2,CNF12),
	           cnf(CNF12,CNF));
	%true ->
	    NNF=CNF.

cnf_p(CNF1,CNF2,F1,F2) :-
	(F1=(_,_);F2=(_,_)) ->
	         fail;
		 F1=CNF1,F2=CNF2.

make_matrix(Wff,Matrix) :-
	Wff = (A,B) ->
	      make_matrix(A,MA),
	      make_matrix(B,MB),
	      append(MA,MB,Matrix);
	Wff = true ->
	      Matrix = [];
	Wff = false ->
	      Matrix = [[]];
	%true ->
	      make_clause(Wff,Clause),
	      Matrix=[Clause].

make_clause(Wff,Clause) :-
	Wff = (A;B) ->
	      make_clause(A,CA),
	      make_clause(B,CB),
	      append(CA,CB,Clause);
	%true ->
	      Clause=[Wff].

%%% ----------------------------------------------------------------------
%%% combine_clauses\3
%%%       merges two CNFs into one while simplifying the resulting CNF
%%%       CARE: C1 is supposed to be smaller than C2

combine_clauses(C,[],C) :-
	!,
	verbose('  trivial combination').
combine_clauses([[L]],C1,[[L]|C2]) :-
	!,
	verbose('  1 unit combination'),
	simplify(L,C1,C2).	
combine_clauses([[L1],[L2]],C1,[[L1],[L2]|C3]) :-
	!,
	verbose('  2 unit combinations'),
	simplify(L1,C1,C2),
	simplify(L2,C2,C3).
combine_clauses(C1,C2,C) :-	
	verbose('  general combination'),
	append(C1,C2,C3),
	unit_reduction(C3,C4),
	subs_reduction(C4,C).

%%% ----------------------------------------------------------------------
%%% adjoin/3
%%%
/* adjoin guarantees         that the new information
          is satisfied by the current unit clauses
   adjoin does not guarantee that the new information 
                                  together with the current matrix
          is satisfied by the current unit clauses
   So you better watch for new unit clauses stemming from reducing the
   new matrix

   Nonetheless: whenever a unit is added to the unit list, this literal
   has been removed (by simplify) from the matrix.
*/
      
adjoin_matrix([[L]],matrix(Units,_),_) :-
	verbose('  1 unit adjunction *failure*'),
	negated_literal(L,NegL),
	identical_member(NegL,Units),
	!,fail.
adjoin_matrix([[L]],matrix(Units,Matrix1),matrix([L|Units],Matrix2)) :-
	!,
	verbose('  1 unit adjunction'),
	simplify(L,Matrix1,Matrix2).
adjoin_matrix([[L1],[L2]],matrix(Units,_),_) :-
						% supposition not(negated_literal(L1,L2))
	verbose('  2 unit adjunction *failure*'),
	(negated_literal(L1,NegL) ; negated_literal(L2,NegL)),
	identical_member(NegL,Units),
	!,fail.
adjoin_matrix([[L1],[L2]],matrix(Units,Matrix1),matrix([L1,L2|Units],Matrix3)) :-
						% supposition not(negated_literal(L1,L2))
	!,
	verbose('  2 unit adjunctions'),
	simplify(L1,Matrix1,Matrix2),
	simplify(L2,Matrix2,Matrix3).
adjoin_matrix(Matrix1,matrix(Units,Matrix2),matrix(Units,Matrix3)) :-
	satisfy_matrix(Matrix1,Units,_) ->
	       	verbose('  full adjunction'),
		append(Matrix1,Matrix2,Matrix3);
	% true ->
		verbose('  weak satisfaction *failure*'),
		fail.


append_matrix([[L]],matrix(Units,Matrix),matrix(Units,[[L]|Matrix])) :-
						% allows for multiple occurrences
	verbose('  1 unit appendage'),
	!.
append_matrix([[L1],[L2]],matrix(Units,Matrix),matrix(Units,[[L1],[L2]|Matrix])) :-
						% allows for multiple occurrences
	verbose('  2 unit appendage'),
	!.
append_matrix(Matrix1,matrix(Units,Matrix2),matrix(Units,Matrix3)) :-
	verbose('  full appendage'),
	append(Matrix1,Matrix2,Matrix3).

%%% ----------------------------------------------------------------------
%%% matrix_reduction/2
%%%      bunch of matrix reductions
%%%

matrix_reduction(C1,C) :-
	taut_reduction(C1,C2),
	mult_reduction(C2,C3),
	unit_reduction(C3,C4),
	subs_reduction(C4,C).

%%% ----------------------------------------------------------------------
%%% unit_reduction/2
%%%      unit reduction
%%%

unit_reduction(M,[[L]|M1]) :-
	mymember([L],M),
	!,
	simplify(L,M,M2),
	unit_reduction(M2,M1).
unit_reduction(M,M).

%%% unit_extraction/3 
%%%        is a special purpose reduction for model-finding
%%%          2nd arg gives matrix without unit clauses
%%%          3rd arg gives literals in unit clauses

unit_extraction(M,M1,[L|R]) :-
	mymember([L],M),
	!,
	simplify(L,M,M2),
	unit_extraction(M2,M1,R).
unit_extraction(M,M,[]).


simplify(_L, [], [] ) .
simplify( L, [C|Cs] , NewCs ) :-
	mymember(L,C),
	!,
	simplify(L,Cs,NewCs).
simplify( L, [C|Cs], [NewC|NewCs] ) :-
	negated_literal(L,NegL),
	myselect(NegL,C,NewC),
	!,
	simplify(L,Cs,NewCs).
simplify( L, [C|Cs], [C|NewCs]    ) :-
	simplify(L,Cs,NewCs).

%%% ----------------------------------------------------------------------
%%% subs_reduction/2
%%%      subs reduction
%%%

subs_reduction([],[]).
subs_reduction([C1|M1],M) :-
	subs_reduction(M1,M2),
	(subsumes(C1,M2,M3) ->
	    M = [C1|M3];
	%true ->
	    M = M2).

subsumes(_,[],[]).
subsumes(C1,[C2|_],_) :-
	mysubset(C2,C1),
	!,
	fail.
subsumes(C,[C1|M1],M) :-
	subsumes(C,M1,M2),
	!,
	(mysubset(C,C1) ->
	        M=M2;
	%true ->
		M=[C1|M2]).

%%% ----------------------------------------------------------------------
%%% taut_reduction/2
%%%      taut reduction
%%%

taut_reduction([],[]) :- !.
taut_reduction([C|M1],M2) :-
	taut_reduction(M1,M3),
	(taut_clause(C) ->
	    M2 = M3;
	%true ->
	    M2 = [C|M3]
	).

taut_clause(C) :-
	mymember(L,C),
	negated_literal(L,K),
	mymember(K,C).
	
%%% ----------------------------------------------------------------------
%%% mult_reduction/2
%%%     mult reduction
%%%

mult_reduction([],[]).
mult_reduction([C|M1],[NewC|M3]) :-
	mult_reduction(M1,M3),
	remove_dups(C,NewC).

remove_dups([],[]).
remove_dups([L|RestC],NewC) :-
	remove_dups(RestC,NewRestC),
	(mymember(L,NewRestC) ->
	    NewC = NewRestC;
	%true ->
	    NewC = [L|NewRestC]
	).


% :- ensure_loaded(io).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date:  4/04/95   File: parser.pl                    %%
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%%  4/04/95 Created                                                          %%
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog parser.pl                                                 %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_file_info(_,size,666).

% reads the knowledge base from the file 'Name.kb'

read_kb(Name,Wff) :-
	concatenate(Name,'.kb',KBFile),
	read_clauses(KBFile,Wff).	

read_ckb(Name,Wff) :-
	concatenate(Name,'.ckb',CKBFile),
	read_clauses(CKBFile,Wff).	

read_que(Name,Wff) :-
	concatenate(Name,'.que',QFile),
	read_clauses(QFile,Wff).	

read_clauses(File,Wff) :-
	open(File,read,Stream),
	read_wff_loop(Stream,Wff),
	close(Stream).

read_wff_loop(Stream,Wff) :-
	read(Stream,Wff1),
	(Wff1 == end_of_file ->
	           Wff = true;
	 %true               ->
		   read_wff_loop(Stream,Wff2),
		   conjoin(Wff1,Wff2,Wff)).

read_matrix(File,Wff) :-
	open(File,read,Stream),
	read_matrix_loop(Stream,Wff),
	close(Stream).

read_matrix_loop(Stream,Matrix) :-
	read(Stream,Elem),
	(Elem == end_of_file ->
	           Matrix = [];
	%true                ->
		   read_matrix_loop(Stream,L),
		   Matrix = [Elem|L]).

% writes a compiled knowledge base consisting of contrapositives only
% to the file 'Name.ckb'

write_ckb(File,KB) :-
	concatenate(File,'.ckb',KBFile),
	open(KBFile,write,KBStream),
	concatenate(File,'.que',QFile),
	open(QFile,write,QStream),
        write_contrapositives(streams(KBStream,QStream),KB),
        close(KBStream),
        close(QStream),
	get_file_info(KBFile,size,KBFileSize),
	get_file_info(QFile,size,QFileSize),
	nl,nl,
	write(KBFile),write(" written "),write(KBFileSize),writeln(" bytes"),
	write(QFile), write(" written "),write(QFileSize), writeln(" bytes"),
	!.

write_cmm(File,Matrix) :-
	concatenate(File,'.cmm',MFile),
	open(MFile,write,MStream),
        write_matrix(MStream,Matrix),
        close(MStream),
	!.

write_query(File,Query) :-
	concatenate(File,'.que',QFile),
	open(QFile,write,QStream),
        write_query_only(QStream,Query),
        close(QStream),
	!.

write_query_only(Stream,(A,B)) :-
	!,
        write_query_only(Stream,A),
        write_query_only(Stream,B).
write_query_only(Stream,(A:-B)) :-
	functor(A,query,_) ->
		write_clauses(Stream,(A:-B));
	%true ->
		true.


write_contrapositives(Streams,(A,B)) :-
	!,
        write_contrapositives(Streams,A),
        write_contrapositives(Streams,B).
write_contrapositives(streams(KBStream,QStream),(A:-B)) :-
   pttp_assert_int((A:-B)),
   (
	functor(A,query,_) ->
		write_clauses(QStream,(A:-B));
	%true ->
		write_clauses(KBStream,(A:-B))).	


write_clauses(Stream,(A,B)) :-
        write_clauses(Stream,A),
        write_clauses(Stream,B),
        !.
write_clauses(Stream,A) :-
      pttp_assert_int(A),
        write(Stream,A),
        write(Stream,.),
        nl(Stream),
        !.
write_clauses(A) :-
	File = 'temp.pl',
	open(File,write,Stream),
	write_clauses(Stream,A),
	close(Stream).


write_matrix(Stream,[]) :-
	nl(Stream),
        !.
write_matrix(Stream,[E|L]) :-
      pttp_assert_int(E),
        write(Stream,E),
        write(Stream,.),
        nl(Stream),
	write_matrix(Stream,L),
        !.
write_matrix(L) :-
	File = 'temp.pl',
	open(File,write,Stream),
	write_matrix(Stream,L),
	close(Stream).


compile_ckb(File) :-	
	concatenate(File,'.ckb',KBFile),
        dmsg(fake(compile(KBFile))).

compile_query(File) :-	
	concatenate(File,'.que',QFile),
	dmsg(fake(compile(QFile))).

ask(Name,Query) :-
	(variables(Query,[]) ->
	         classical_clauses(Query,Q0),
		 cnf(Q0,Q1),
		 make_matrix(Q1,Q2),
		 matrix_reduction(Q2,Q);
	%true ->
		 Q = []),

	concatenate(Name,'.cmm',MFile),
	read_matrix(MFile,Matrix),
	
	dpttp1((query:-Query),Q,Matrix,Query1:Matrix),

	nl,
        write('XRay writing query ... '),
        write_query(Name,Query1),
	write('done.'),

	nl,
        write('XRay compiling query ... '),
        compile_query(Name),
	write('done.'),
        nl,
        !.

tell(Name,Wff) :-	
	read_kb(Name,KB),
	conjoin(Wff,KB,NewKB),
	dpttp(Name,NewKB).

write_proved(Proof,ProofEnd) :-
        write('proved'),
	verbose_flag ->
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

% :- ensure_loaded(db).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date: 24/11/95   File: db.pl                        %%
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%% 24/11/95 Created                                                          %%
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog db.pl                                                     %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prolog_clause((Head :- Body),(Head :- Body)) :-
	functor(Head,Pred,_),
	builtin_predicate(Pred),
	!.
prolog_clause(Fact,(Fact:-true)) :-
	functor(Fact,Pred,_),
	Pred \= ':-',
	builtin_predicate(Pred),
	!.
prolog_clause(_,true).

:-thread_local herbrand/1.

% :- ensure_loaded(herbrand).

add_herbrand_preds((Head :- Body),(Head :- Body1)) :-
	herbrandize_variables(Body,[],BodyVars,false,_),
	herbrandize_variables(Head,BodyVars,_,true,Matches),
        conjoin(Matches,Body,Body1).
	

herbrandize_variables(Term,VarsIn,VarsOut,MatchesIn,MatchesOut) :-
        builtin(Term) ->
	        VarsOut = VarsIn,
                MatchesOut = MatchesIn;
        /*cyclic_term(Term) ->
	        VarsOut = VarsIn,
                MatchesOut = MatchesIn;*/
        %true ->
	        nonvar(Term) ->
                       myfunctor(Term,_,N),
		       herbrandize_args(Term,VarsIn,VarsOut,MatchesIn,MatchesOut,1,N);
	        identical_member(Term,VarsIn) ->
	               VarsOut = VarsIn,
		       MatchesOut = MatchesIn;
	        %true ->
		       VarsOut = [Term|VarsIn],
		       conjoin(MatchesIn,herbrand(Term),MatchesOut).

herbrandize_args(Term,VarsIn,VarsOut,MatchesIn,MatchesOut,I,N) :-
        I > N ->
                VarsOut = VarsIn,
                MatchesOut = MatchesIn;
        %true ->
                arg(I,Term,Arg),
                herbrandize_variables(Arg,VarsIn,Vars1,MatchesIn,Matches1),
                I1 is I + 1,
                herbrandize_args(Term,Vars1,VarsOut,Matches1,MatchesOut,I1,N).

herbrand_universe(U) :-
	setof(X,herbrand(X),U).

herbrand_preds([],true).
herbrand_preds([C|Cs],Wff) :-
	herbrand_preds(Cs,Wffs),
	conjoin((herbrand(C):-true),Wffs,Wff).

add_answer_preds((query :- Query),(query :- (Query,nl,nl,write(answer:Vars),nl))) :-
	!,
	variables(Query,Vars).
add_answer_preds(R,R).

%%% constants returns a list of the constants appearing in a formula.

constants(Wff,L) :-
        Wff = (A :- B) ->
                constants(A,L1),
                constants(B,L2),
                union(L2,L1,L);
        Wff = (A , B) ->
                constants(A,L1),
                constants(B,L2),
                union(L2,L1,L);
        Wff = (A ; B) ->
                constants(A,L1),
                constants(B,L2),
                union(L2,L1,L);
        Wff = (A : B) ->
                constants(A,L1),
                constants(B,L2),
                union(L2,L1,L);
        myfunctor(Wff,search,_) ->        % list constants in first argument of search
                arg(1,Wff,X),
                constants(X,L);
        builtin(Wff) ->
                L = [];
        %true ->
                myfunctor(Wff,_,N),
                (N > 0 ->
		   constantize_args(Wff,[],L,1,N);
		 %true ->
                   L = []).

is_arg(Term):-cyclic_term(Term),!,fail.
is_arg(X):-functor(X,h,_).
is_arg(X):-functor(X,f,_).
%is_arg(X):-functor(X,i,_).
%is_arg(X):-functor(X,p,_).

constantize_args(Term,FnsIn,FnsOut,I,N) :-
	var(Term) ->
		FnsOut = FnsIn;
	cyclic_term(Term) ->
		FnsOut = FnsIn;
	atom(Term) ->
	        FnsOut = [Term|FnsIn];
	is_arg(Term) ->
	        FnsOut = [Term|FnsIn];
        I > N ->
                FnsOut = FnsIn;
        %true ->
                arg(I,Term,ArgI),
		(var(ArgI) ->
		        Fns1 = [];
		%true ->
		        myfunctor(ArgI,_,NI),
                        constantize_args(ArgI,FnsIn,Fns1,1,NI)),
                I1 is I + 1,
                constantize_args(Term,Fns1,FnsOut,I1,N).

%%% variables returns a list of the variables appearing in a formula.

variables(Wff,L) :-
        Wff = (A :- B) ->
                variables(A,L1),
                variables(B,L2),
                union(L2,L1,L);
        Wff = (A , B) ->
                variables(A,L1),
                variables(B,L2),
                union(L2,L1,L);
        Wff = (A ; B) ->
                variables(A,L1),
                variables(B,L2),
                union(L2,L1,L);
        Wff = (A : B) ->
                variables(A,L1),
                variables(B,L2),
                union(L2,L1,L);
        myfunctor(Wff,search,_) ->        % list variables in first argument of search
                arg(1,Wff,X),
                variables(X,L);
        builtin(Wff) ->
                L = [];
        %true ->
                myfunctor(Wff,_,N),
                (N > 0 ->
		   variablize_args(Wff,[],L,1,N);
		 %true ->
                   L = []).

variablize_args(Term,FnsIn,FnsOut,I,N) :-
	atom(Term) ->
		FnsOut = FnsIn;
	var(Term) ->
	        FnsOut = [Term|FnsIn];
        I > N ->
                FnsOut = FnsIn;
        %true ->
                arg(I,Term,ArgI),
		(var(ArgI) ->
		        union([ArgI],FnsIn,Fns1);
		%true ->
		        myfunctor(ArgI,_,NI),
                        variablize_args(ArgI,FnsIn,Fns1,1,NI)),
                I1 is I + 1,
                variablize_args(Term,Fns1,FnsOut,I1,N).


variablize_clause(Clause,Vars) :-
	ClauseTerm =.. [clause|Clause],
	variables(ClauseTerm,Vars).

instance([],_,Clause).
instance(Vars,Cons,Clause) :-
	myselect(V,Vars,V1s),
	member(V,Cons),
	instance(V1s,Cons,Clause).

skolem([],Term,Term).
skolem([Var|Vars],Term,SkTerm) :-
	skolem(Vars,Term,Term1),
	SkTerm=(Var^Term1).

instances(Clause,Terms,Instances) :-
	variablize_clause(Clause,Vars),
	skolem(Vars,instance(Vars,Terms,Clause),DoIt),
	setof(Clause,DoIt,Instances).

instantiation([],_,[]) :-
	!.
instantiation(Matrix,[],Matrix) :-
	!.
instantiation([Clause|Matrix],Terms,MatrixInstances) :-
	instances(Clause,Terms,Instances),
	instantiation(Matrix,Terms,IMatrix),
	combine_clauses(Instances,IMatrix,MatrixInstances).


% :- ensure_loaded(hooks).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date: 20/06/96   File: hooks.pl                     %%
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%% 20/06/96 Created                                                          %%
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog hooks.pl                                                  %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic(body_hooks_flag/0).
:- dynamic(pred_hooks_flag/0).

%%% ----------------------------------------------------------------------
%%% HOOKS CONFIGURATION

hook_configuration :-
	nl,write('HOOK CONFIGURATION:'),nl,nl,
	
	write("body hook handling     "),
	write(' = '),
	(body_hooks_flag -> write(on) ; write(off)),
	nl,

	write("predicate hook handling"),
	write(' = '),
	(pred_hooks_flag -> write(on) ; write(off)),
	nl.

%%% Hook handling is turned off by no_hook_handling

no_hook_handling :- no_body_hooks, no_pred_hooks.

%%% Body hooks are turned on by body_hooks.
%%% off by no_body_hooks.

body_hooks :-                          % enable body hooks
        retract(body_hooks_flag),
        fail.
body_hooks :-
        assert(body_hooks_flag).

no_body_hooks :-                       % disable body hooks
        retract(body_hooks_flag),
        fail.
no_body_hooks.

%%% Predicate hooks are turned on by pred_hooks.
%%% off by no_pred_hooks.

pred_hooks :-                          % enable predicate hooks
        retract(pred_hooks_flag),
        fail.
pred_hooks :-
        assert(pred_hooks_flag).

no_pred_hooks :-                       % disable predicate hooks
        retract(pred_hooks_flag),
        fail.
no_pred_hooks.

%%% SETTINGS for HOOKS HANDLING
%%%

% :- compile(hooks_config).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date: 13/07/96   File: hooks_config.pl              %%
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%% 13/07/96 Created                                                          %%
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog hooks_config.pl                                           %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- no_body_hooks.                      % default is no body hooks

:- no_pred_hooks.                      % default is no predicate hooks


%%% ----------------------------------------------------------------------
%%% BODY HOOKS

add_body_hooks((Head :- Body),(Head :- Body2)) :-
	body_hooks_flag,
	!,
	(bhook1_p(Head :- Body) ->
	    conjoin((bhook1(Head :- Body)),Body,Body1);
        %true ->
	    Body1=Body),
	(bhook2_p(Head :- Body) ->
	    conjoin(Body1,(bhook2(Head :- Body)),Body2);
        %true ->
	    Body2=Body1).
add_body_hooks((Head :- Body),(Head :- Body)).

%%% COMPILE-TIME conditions for body hook insertion

bhook1_p(Head :- Body) :-
	true.
bhook2_p(Head :- Body) :-
	true.

%%% RUN-TIME predicates for body hooks

bhook1(Head :- Body) :-
	(Head = _) ->
	    Head =.. [P|_],
	    nl,write(b1:(P)),nl,nl;
        %true ->
	     true.
bhook2(Head :- Body) :-
	(Head = _) ->
	    Head =.. [P|_],
	    nl,write(b2:(P)),nl,nl;
        %true ->
	     true.

%%% ----------------------------------------------------------------------
%%% BODY HOOKS

%%% COMPILE-TIME conditions for PREDICATE hook insertion

phook_tests(P,N,TestsA,Proc,ProcP) :-
	pred_hooks_flag,
	!,
	
	phook1_tests(P,N,Tests1),
	conjoin(Tests1,TestsA,Tests1A),
	
	phook2_tests(P,N,Tests2),
	conjoin(Tests1A,Tests2,Tests1A2),

	phook3_tests(P,N,Tests3),
	conjoin(Proc,Tests3,Proc3),

	conjoin(Tests1A2,Proc3,ProcP).
phook_tests(_,_,TestsA,Proc,ProcP) :-
	conjoin(TestsA,Proc,ProcP).

phook1_p(P,N) :-
	true.
phook2_p(P,N) :-
	true.
phook3_p(P,N) :-
	true.

%%% COMPILE-TIME predicates PREDICATE hook insertion

phook1_tests(P,N,Result) :-
	phook1_p(P,N),
	!,
	head(P,N,Head),
	Body=(nl,write(p1:P),nl,fail),
	Result = (Head :- Body).
phook1_tests(_,_,true).

phook2_tests(P,N,Result) :-
	phook2_p(P,N),
	!,
	head(P,N,Head),
	Body=(nl,write(p2:P),nl,fail),
	Result = (Head :- Body).
phook2_tests(_,_,true).

phook3_tests(P,N,Result) :-
	phook3_p(P,N),
	!,
	head(P,N,Head),
	Body=(nl,write(p3:P),nl,fail),
	Result = (Head :- Body).
phook3_tests(_,_,true).

head(P,N,Head) :-
	P == query ->
                Head = query;
	%true ->
		functor(Head,P,N).

% :- ensure_loaded(lemma).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date: 24/04/95   File: lemma.pl                     %%
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%% 04/04/95 Created                                                          %%
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog lemma.pl                                                  %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic(dynamic_lemma/3).
:- dynamic(static_lemma/3).

:- dynamic(lemma_handling_flag/0).

:- dynamic(lemma_mode_parameter/1).
:- dynamic(lemma_format_parameter/1).
:- dynamic(lemma_type_parameter/1).

%%% ----------------------------------------------------------------------
%%% LEMMA CONFIGURATION

lemma_configuration :-
	nl,write('LEMMA CONFIGURATION:'),nl,nl,
	
	write("Lemma handling"),
	write(' = '),
	(lemma_handling_flag ->
	    write(on),
	    nl,nl,
	
	    write(lemma_format),
	    write(' = '),
	    (lemma_format_parameter(P), write(P),write(' '),fail ; write('.')),
	    nl,
	    
	    write(lemma_mode),
	    write(' = '),
	    (lemma_mode_parameter(M),   write(M),write(' '),fail ; write('.')),
	    nl,
	    
	    write(lemma_type),
	    write(' = '),
	    (lemma_type_parameter(T),   write(T),write(' '),fail ; write('.')),
	    nl;
	%true ->
	    write(off)),
	nl.

%%% Lemma handling is turned on by lemma_handling,
%%% off by no_lemma_handling.

lemma_handling :-                          % enable lemma handling
        retract(lemma_handling_flag),
        fail.
lemma_handling :-
        assert(lemma_handling_flag).

no_lemma_handling :-                       % disable lemma handling
        retract(lemma_handling_flag),
        fail.
no_lemma_handling.

%%% Lemma mode indicates the usage of dynamic, static
%%% or dystatic modes of lemmas
%%% depending on lemma_mode_parameter(dynamic),lemma_mode_parameter(static), 
%%% and lemma_mode_parameter(dystatic)

lemma_mode(dynamic) :-                          % enable DYNAMIC lemmas
        retract(lemma_mode_parameter(_)),
        fail.
lemma_mode(dynamic) :-
        assert(lemma_mode_parameter(dynamic)).

lemma_mode(dystatic) :-                         % enable DYSTATIC  lemmas
        retract(lemma_mode_parameter(_)),
        fail.
lemma_mode(dystatic) :-
        assert(lemma_mode_parameter(dystatic)).

lemma_mode(static) :-                           % enable STATIC  lemmas
        retract(lemma_mode_parameter(_)),
        fail.
lemma_mode(static) :-
        assert(lemma_mode_parameter(static)).

%%% Dynamic lemma handling is alternatively turned on by dynamic_lemmas,
%%% off by no_dynamic_lemmas.

dynamic_lemmas :- lemma_mode(dynamic).                       % enable  DYNAMIC lemma handling

add_dynamic_lemmas :-                                        % add     DYNAMIC lemma handling
        retract(lemma_mode_parameter(dynamic)),
        fail.
add_dynamic_lemmas :-
        assert(lemma_mode_parameter(dynamic)).

no_dynamic_lemmas :-                                         % disable DYNAMIC lemma handling
        retract(lemma_mode_parameter(dynamic)),
        fail.
no_dynamic_lemmas.

%%% Static lemma handling is alternatively turned on by static_lemmas,
%%% off by no_static_lemmas.

static_lemmas :- lemma_mode(static).                        % enable  STATIC lemma handling

add_static_lemmas :-                                        % add     STATIC lemma handling
        retract(lemma_mode_parameter(static)),
        fail.
add_static_lemmas :-
        assert(lemma_mode_parameter(static)).

no_static_lemmas :-                                         % disable STATIC lemma handling
        retract(lemma_mode_parameter(static)),
        fail.
no_static_lemmas.

%%% Dystatic lemma handling is alternatively turned on by dystatic_lemmas,
%%% off by no_dystatic_lemmas.
%%% dystatic lemmas are static lemmas stemming from dynamic ones

dystatic_lemmas :- lemma_mode(dystatic).                    % enable  DYSTATIC lemma handling

add_dystatic_lemmas :-                                      % add     DYSTATIC lemma handling
        retract(lemma_mode_parameter(dystatic)),
        fail.
add_dystatic_lemmas :-
        assert(lemma_mode_parameter(dystatic)).

no_dystatic_lemmas :-                                       % disable DYSTATIC lemma handling
        retract(lemma_mode_parameter(dystatic)),
        fail.
no_dystatic_lemmas.

%%% some macros for easier lemma configuration.

lemma_flag :- lemma_mode_parameter(X).
dynamic_lemma_flag  :- lemma_mode_parameter(dynamic).
static_lemma_flag   :- lemma_mode_parameter(static).
dystatic_lemma_flag :- lemma_mode_parameter(dystatic).

no_lemmas :- no_dynamic_lemmas,no_static_lemmas,no_dystatic_lemmas.

%%% Lemma type indicates the location of the lemmatization predicates
%%%   DELTA lemmas are attached to rules stemming from default rules
%%%                ( Head =.. [gamma|-] or infer_by(default(_)) )
%%%   OMEGA lemmas are attached to rules stemming from classical rules
%%%                ( infer_by(extension(_) )
%%% depends on lemma_type_parameter(delta),lemma_type_parameter(omega), 
%%% and lemma_type_parameter(all)

lemma_type(delta) :-                          % enable DELTA lemmas
        retract(lemma_type_parameter(_)),
        fail.
lemma_type(delta) :-
        assert(lemma_type_parameter(delta)).

lemma_type(omega) :-                          % enable OMEGA  lemmas
        retract(lemma_type_parameter(_)),
        fail.
lemma_type(omega) :-
        assert(lemma_type_parameter(omega)).

lemma_type(all) :-                            % enable ALL  lemmas
        retract(lemma_type_parameter(_)),
        fail.
lemma_type(all) :-
        assert(lemma_type_parameter(delta)),
        assert(lemma_type_parameter(omega)).

%%% Delta lemma handling is alternatively added  by delta_lemmas,
%%% disabled by no_delta_lemmas.

add_delta_lemmas :-                                        % add     DELTA lemma handling
        retract(lemma_type_parameter(delta)),
        fail.
add_delta_lemmas :-
        assert(lemma_type_parameter(delta)).

no_delta_lemmas :-                                         % disable DELTA lemma handling
        retract(lemma_type_parameter(delta)),
        fail.
no_delta_lemmas.

%%% Omega lemma handling is alternatively added  by omega_lemmas,
%%% disabled by no_omega_lemmas.

add_omega_lemmas :-                                        % add     OMEGA lemma handling
        retract(lemma_type_parameter(omega)),
        fail.
add_omega_lemmas :-
        assert(lemma_type_parameter(omega)).

no_omega_lemmas :-                                         % disable OMEGA lemma handling
        retract(lemma_type_parameter(omega)),
        fail.
no_omega_lemmas.

%%% Lemma format indicates the usage of unit or disjunctive lemmas
%%% depending on lemma_format_parameter(unit) and lemma_format_parameter(disj)

lemma_format(unit) :-                          % enable UNIT lemmas
        retract(lemma_format_parameter(_)),
        fail.
lemma_format(unit) :-
        assert(lemma_format_parameter(unit)).

lemma_format(disj) :-                          % enable DISJUNCTIVE  lemmas
        retract(lemma_format_parameter(_)),
        fail.
lemma_format(disj) :-
        assert(lemma_format_parameter(disj)).

%%% SETTINGS for LEMMA HANDLING
%%%

% :- compile(lemma_config).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date: 13/07/96   File: lemma:config.pl              %%
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%% 13/07/96 Created                                                          %%
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog lemma:config.pl                                           %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- lemma_handling.                         % default is LEMMA HANDLING

:- lemma_mode(dynamic).                    % default is to use DYNAMIC lemmas only

:- lemma_type(delta).                      % default is to use DELTA lemmas only

:- lemma_format(unit).                     % default is to use UNIT lemmas only

/*
add_lemmatization_p(Head :- Body) :-
	lemma_flag,
	!,
	(functor(Head,query,_) -> fail;
         functor(Head,alpha,_) -> fail;
	 functor(Head,gamma,_) -> lemma_type_parameter(delta);
	 true ->                  lemma_type_parameter(omega)).

dynamic_lemma_test_p(P,N) :-
	dynamic_lemma_flag,
	!,
	(P == query -> fail;
         P == alpha -> fail;
	 P == gamma -> lemma_type_parameter(delta);
	 true       -> lemma_type_parameter(omega)).

static_lemma_test_p(P,N) :-
	(static_lemma_flag;dystatic_lemma_flag),
	!,
	(P == query -> fail;
         P == alpha -> fail;
	 P == gamma -> lemma_type_parameter(delta);
	 true       -> lemma_type_parameter(omega)).
*/
%%% ----------------------------------------------------------------------
%%% LEMMA GENERATION during COMPILATION

%%% Add extra arguments to each goal so that information
%%% on what inferences were made in the proof can be printed
%%% at the end.

add_lemmatization((Head :- Body),(Head1 :- Body1)) :-
	lemma_handling_flag,
	!,
        Head =.. L,
        append(L,[ProofOut,ProofEnd],L1),
        Head1 =.. L1,
        add_lemmatization_args(Body,Proof,ProofEnd,Body2,Lemma),
        (Lemma = true ->
	    Body1 = Body2,
	    ProofOut = Proof;
	 add_lemmatization_p(Head :- Body) ->
	    Lemmatization =.. [lemmatize,Lemma,Proof,ProofOut,ProofEnd],
	    conjoin(Body2,Lemmatization,Body1),
	    verbose("Lemmatization ":Lemma);
        %true ->
             Body1 = Body2,
             ProofOut = Proof).
add_lemmatization((Head :- Body),(Head :- Body)).

add_lemmatization_p(Head :- Body) :-
	lemma_flag,
	!,
	(functor(Head,query,_) -> fail;
         functor(Head,alpha,_) -> fail;
	 functor(Head,gamma,_) -> lemma_type_parameter(delta);
	 true ->                  lemma_type_parameter(omega)).
         
add_lemmatization_args(Body,Proof,ProofEnd,Body1,Lemma) :-
        Body = (A , B) ->
                add_lemmatization_args(A,Proof,Proof1,A1,L1),
                add_lemmatization_args(B,Proof1,ProofEnd,B1,L2),
                conjoin(A1,B1,Body1),
                conjoin(L1,L2,Lemma);
        Body = (A ; B) ->
                add_lemmatization_args(A,Proof,ProofEnd,A1,L1),
                add_lemmatization_args(B,Proof,ProofEnd,B1,L2),
                disjoin(A1,B1,Body1),
                conjoin(L1,L2,Lemma);
        Body = infer_by(X) ->
	        add_lemmatization_inference(X,Proof,ProofEnd,Record,Lemma),
		conjoin(Body,Record,Body1);
        Body =.. [search,Goal|L] ->
                add_lemmatization_args(Goal,Proof,ProofEnd,Goal1,_Lemma),
                Body1 =.. [search,Goal1|L],
                Lemma = true;
        Body = fail ->
                Body1 = Body,
                Lemma = true;
        builtin(Body) ->
                Proof = ProofEnd,
                Body1 = Body,
                Lemma = true;
        %true ->
                Body =.. L,
                append(L,[Proof,ProofEnd],L1),
                Body1 =.. L1,
                Lemma = true.

add_lemmatization_inference(Inference,Proof,ProofEnd,Record,Lemma) :-
	Inference = reduction(_) ->
	    /* ancestor test */
	    Lemma = true,
            (lemma_type_parameter(omega) ->
		Record = (Proof = [Inference|ProofEnd]);
	    %true ->
		Proof = ProofEnd,
		Record = true);
        Inference = extension(_) ->
            /* omega rule */
	    Proof = ProofEnd,
	    Record = true,
	    (lemma_type_parameter(omega) ->
		Lemma = Inference;
	    %true ->
		Lemma = true);
	Inference = default(_) ->
	    /* delta rule */
	    Lemma = Inference,
	    ((static_lemma_flag;dystatic_lemma_flag) ->
		Record = (Proof = [Inference|ProofEnd]);
	    %true ->
		Proof = ProofEnd,
		Record =true);
	Inference = static_lemma(_) ->
	    /* static lemma test (implicit (dy)static_lemma_flag)*/
	    Record = (Proof = [Inference|ProofEnd]),
	    Lemma = true;
	Inference = dynamic_lemma(_) ->
	    /* dynamic lemma test */
	    Lemma = true,
	    ((static_lemma_flag;dystatic_lemma_flag) ->
		Record = (Proof = [Inference|ProofEnd]);
	    %true ->
		Proof = ProofEnd,
		Record =true);
	%true ->
            /* unit. etc */
	    Proof = ProofEnd,
	    Record = true,
	    Lemma = true.


lemma_tests_p(P,N) :-
	lemma_handling_flag, (dynamic_lemma_test_p(P,N) ; static_lemma_test_p(P,N)).

dynamic_lemma_test_p(P,N) :-
	(dynamic_lemma_flag;dystatic_lemma_flag),
	!,
	(P == query -> fail;
         P == alpha -> fail;
	 P == gamma -> lemma_type_parameter(delta);
	 true       -> lemma_type_parameter(omega)).

static_lemma_test_p(P,N) :-
	(static_lemma_flag;dystatic_lemma_flag),
	!,
	(P == query -> fail;
         P == alpha -> fail;
	 P == gamma -> lemma_type_parameter(delta);
	 true       -> lemma_type_parameter(omega)).

lemma_tests(P,N,Result) :-
	lemma_handling_flag,
	lemma_tests_p(P,N),     % for avoiding problems with query/0
	!,
	N3 is N - 3,            % N - 3 due to 3 ancestor-lists
	functor(Head3,P,N3),
	Head3 =.. [P|Args1],
	append(Args1,[_,_,_],Args),
	Head =.. [P|Args],
	
	(dynamic_lemma_test_p(P,N) ->
	    dynamic_lemma_test(Head,Head3,DynamicLemmaBody);
	%true ->
	    DynamicLemmaBody=true),

	(static_lemma_test_p(P,N) ->
	    static_lemma_test(Head,Head3,StaticLemmaBody);
	%true ->
	    StaticLemmaBody=true),

	conjoin(DynamicLemmaBody,StaticLemmaBody,Result).
lemma_tests(_,_,true).

dynamic_lemma_test(Head,Head3,Test) :-
	lemma_format_parameter(unit) ->
	     ((static_lemma_flag;dystatic_lemma_flag) ->
		 Body = (infer_by(dynamic_lemma(Head3:Assumptions)),
		         dynamic_lemma(Head3,false,Assumptions)) ;
	     %true ->
        	 Body = (infer_by(dynamic_lemma(Head3)),
		         dynamic_lemma(Head3,false,[]))),
	     Test = (Head :- Body,!); /* REQUIRES FULLY INSTANTIIATED SUBGOALS */
	lemma_format_parameter(disj) ->
	%true              ->
	        not_yet_implemented.
		          
static_lemma_test(Head,Head3,Test) :-
	lemma_format_parameter(unit) ->
        	Body = (infer_by(static_lemma(Head3:Assumptions)),
		        static_lemma(Head3,false,Assumptions),
			justification(Assumptions)),
		Test = (Head :- Body);
	lemma_format_parameter(disj) ->
	%true              ->
	        not_yet_implemented.

%%% ----------------------------------------------------------------------
%%% LEMMA UTILIZATION during RUN-TIME

lemmatize(default(N:(Gamma :- _)),Proof,ProofOut,ProofEnd) :-
	verbose("Using static lemma code: ":lemmatize),
        !,
        remove_reductions(Proof,ProofOut),
        default_assumptions(ProofOut,ProofEnd,Ass),
        lemmatize_dynamically(gamma(N,Gamma),false,Ass).
%        lemmatize_statically(gamma(N,Gamma),false,Ass).
lemmatize(extension(_N:Goal),Proof,ProofOut,ProofEnd) :-
	verbose("Using static lemma code: ":lemmatize),
        !,
        skim_reductions(Goal,Proof,ProofOut,ProofEnd,Ancs),
        default_assumptions(ProofOut,ProofEnd,Ass),
        lemmatize_dynamically(Goal,Ancs,Ass).
%        lemmatize_statically(Goal,Ancs,Ass).
lemmatize(_Lemmatization,_Proof,_ProofOut,_ProofEnd) :-
        error_in_lemmatize.

lemmatize_statically(Goal,Ancestors,Assumptions) :-
        static_lemma(Goal,Ancestors,Assumptions),            % UNIFY, ==, ... ???
        !.
lemmatize_statically(Goal,Ancestors,Assumptions) :-
        assert(static_lemma(Goal,Ancestors,Assumptions)),
        verbose(static_lemma(Goal,Ancestors,Assumptions)).

lemmatize_dynamically(Goal,Ancestors,_) :-
        dynamic_lemma(Goal,Ancestors,_),                        % UNIFY, ==, ... ???
        !.
%lemmatize_dynamically(Goal,Ancestors,[]) :-               <=== only if cases below
%        !,
%        assert(dynamic_lemma(Goal,Ancestors,[])),
%        lemmatize_statically(Goal,Ancestors,[]),
%        verbose(dynamic_lemma(Goal,Ancestors)).
lemmatize_dynamically(Goal,Ancestors,Assumptions) :-
        (assert(dynamic_lemma(Goal,Ancestors,Assumptions)),
         verbose(activated:(dynamic_lemma(Goal,Ancestors,[Assumptions])));
         retract(dynamic_lemma(Goal,Ancestors,Assumptions)),
         verbose(deactivated:(dynamic_lemma(Goal,Ancestors,[Assumptions]))),
%         lemmatize_statically(Goal,Ancestors,Assumptions), <=== only if cases below
         !,
         fail).

% No check for ProofEnd, since default(...) should be contained
% Because only invoked in case of default inference

remove_reductions([default(X)|Proof],[default(X)|Proof]) :- 
        !.
remove_reductions([static_lemma(X)|Proof],[static_lemma(X)|ProofOut]) :- 
        !,
        remove_reductions(Proof,ProofOut).
remove_reductions([dynamic_lemma(X)|Proof],[dynamic_lemma(X)|ProofOut]) :- 
	/* ==> when using static lemmas <== */
        !,
        remove_reductions(Proof,ProofOut).
remove_reductions(Proof,ProofOut) :-
        Proof = [_|RestProof],
        remove_reductions(RestProof,ProofOut).

skim_reductions(_Goal,Proof,Proof,ProofEnd,false) :-
        Proof == ProofEnd,
        !.
skim_reductions(_Goal,Proof,Proof,_ProofEnd,false) :-
        Proof = [default(_)|_],
        !.
skim_reductions(Goal,[reduction(Anc)|Proof],Proof1,ProofEnd,Ancs) :-
        Goal == Anc,
        skim_reductions(Goal,Proof,Proof1,ProofEnd,Ancs),
        !.
skim_reductions(Goal,[reduction(Anc)|Proof],[reduction(Anc)|Proof1],ProofEnd,Ancs1) :-
        !,
        skim_reductions(Goal,Proof,Proof1,ProofEnd,Ancs),
        disjoin1(Anc,Ancs,Ancs1).
skim_reductions(Goal,[static_lemma(X)|Proof],[static_lemma(X)|Proof1],ProofEnd,Ancs) :-
        skim_reductions(Goal,Proof,Proof1,ProofEnd,Ancs).
skim_reductions(Goal,[dynamic_lemma(X)|Proof],[dynamic_lemma(X)|Proof1],ProofEnd,Ancs) :-
	/* ==> when using static lemmas <== */
        skim_reductions(Goal,Proof,Proof1,ProofEnd,Ancs).

%%% ----------------------------------------------------------------------
%%% default_assumptions/3 
%%%         extracts all neccessary default assunptions from a proof
%%%

default_assumptions(Proof,ProofEnd,[]) :-
        Proof == ProofEnd,
        !.
default_assumptions([default(_N:(_ :- _ : Just))|Proof],ProofEnd,Assumptions) :-
        !,
        default_assumptions(Proof,ProofEnd,Justs),
        combine_clauses(Just,Justs,Assumptions).
default_assumptions([static_lemma(_  : Just)|Proof],ProofEnd,Assumptions) :-
        !,
        default_assumptions(Proof,ProofEnd,Justs),
        combine_clauses(Just,Justs,Assumptions).
default_assumptions([dynamic_lemma(_ : Just)|Proof],ProofEnd,Assumptions) :-
	/* ==> when using static lemmas <== */
        !,
        default_assumptions(Proof,ProofEnd,Justs),
        combine_clauses(Just,Justs,Assumptions).
default_assumptions([_|Proof],ProofEnd,Wff) :-
        default_assumptions(Proof,ProofEnd,Wff).

%%% A is supposed to be a literal
%%% disjoin2 removes A from B; results in C

disjoin1(A,B,C) :-
        disjoin2(A,B,C1),
        disjoin(A,C1,C).

disjoin2(A,(B ; C),D) :-
        !,
        disjoin2(A,B,B1),
        disjoin2(A,C,C1),
        disjoin(B1,C1,D).
disjoin2(A,B,false) :-
        A == B,
        !.
disjoin2(_,B,B).


%%% ----------------------------------------------------------------------
%%% SOME helpful IO-Routines

write_lemmas(File) :-   
        concatenate(File,'.lem',LFile),
        open(LFile,write,LStream),
        !,
        (static_lemma(X,Y,Z),
         write_clauses(LStream,static_lemma(X,Y,Z)),
         fail;
         close(LStream)).

show_lemmas :-  
        show_dynamic_lemmas,fail;
        nl,show_static_lemmas.

show_dynamic_lemmas :- 
        (dynamic_lemma(X,Y,Z),
         write(dynamic_lemma(X,Y,Z)),
         nl,
         fail;
         true).
show_static_lemmas :-
        (static_lemma(X,Y,Z),
         write(static_lemma(X,Y,Z)),
         nl,
         fail;
         true).

remove_lemmas :-
        remove_static_lemmas,
        remove_dynamic_lemmas.
remove_dynamic_lemmas :-
        retractall(dynamic_lemma(_,_,_)).
remove_static_lemmas :-
        retractall(\(_,_,_)).


% :- ensure_loaded(xray_config).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date: 15/03/98   File: xray_config.pl               %%
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%% 15/03/98 Created                                                          %%
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog xray_config.pl                                            %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% :- ensure_loaded(hooks).
% :- ensure_loaded(lemma).

:- no_hook_handling,hook_configuration.     % indicated by {pred|body}_hook_flag

:- no_lemma_handling,lemma_configuration.   % indicated by lemma_handling_flag

:- dynamic(delta_ordering/1).
:- dynamic(verbose_flag/0).
:- dynamic(compile_complete_search/0).

%%% ----------------------------------------------------------------------
%%% PTTP CONFIGURATION

%%% complete search facilities are turned (during compile-time)
%%%  on by do_compile_complete_search,
%%% off by dont_compile_complete_search.
%%%
%%% do_compile_complete_search or dont_compile_complete_search *must* be
%%% executed before the problem is compiled.

do_compile_complete_search :-
        retract(compile_complete_search),
        fail.
do_compile_complete_search :-
        assert(compile_complete_search).

dont_compile_complete_search :-
        retract(compile_complete_search),
        fail.
dont_compile_complete_search.

:- do_compile_complete_search.            % default is to compile complete search

%%% Proof printing is (better) turned (during compile-time)
%%%  on by   do_compile_proof_printing (print_proof),
%%% off by dont_compile_proof_printing (dont_print_proof).
%%%
%%% do_compile_proof_printing or dont_compile_proof_printing *must* be
%%% executed before the problem is compiled.

do_compile_proof_printing   :- print_proof.
dont_compile_proof_printing :- dont_print_proof.

:- do_compile_proof_printing.            % default is to compile proof printing

%%% Inference counting is turned
%%%  on by   do_compile_count_inferences (count_inferences),
%%% off by dont_compile_count_inferences (dont_count_inferences).
%%%
%%% Inferences are counted by retracting the current count
%%% and asserting the incremented count, so inference counting
%%% is very slow.

do_compile_count_inferences   :- count_inferences.
dont_compile_count_inferences :- dont_count_inferences.

:- dont_compile_count_inferences.

pttp_configuration :-
	nl,writeln('PTTP CONFIGURATION:'),nl,
	(count_inferences_pred(true) ->
	    writeln('PTTP counts no inferences.');
	    writeln('PTTP counts inferences!')),
        (trace_search_progress_pred(nop) ->
	    writeln('PTTP does not trace search progress.');
	    writeln('PTTP traces search progress!')),
	(compile_proof_printing ->
	    writeln('PTTP compiles proof printing!');
	    writeln('PTTP does not compile proof printing.')),
	(compile_complete_search ->
	    writeln('PTTP compiles complete search!');
	    writeln('PTTP does not compile complete search.')).

:- pttp_configuration.

%%% ----------------------------------------------------------------------
%%% XRay CONFIGURATION

xray_configuration :-
	nl,write('XRay CONFIGURATION:'),nl,nl,
	
	write(delta_ordering),
	write(' = '),
	(delta_ordering(O), write(O),write(' '),fail ; write('.')),
	nl,
	
	write(verbose_mode),
	write(' = '),
	(verbose_flag, write("on") ; write("off")),
	nl.

%%% delta_ordering stears the order of admissibility 
%%% and compatibility checking in delta rules
%%% at compile-time

switch_delta_ordering :-
	delta_ordering(compatibility>admissibility),
	admissibility_first.
switch_delta_ordering :-
	delta_ordering(admissibility>compatibility),
	compatibility_first.

compatibility_first :-                      % check compatibility first
        retract(delta_ordering(_)),
        fail.
compatibility_first :-
        assert(delta_ordering(compatibility>admissibility)).

admissibility_first :-                      % check admissibility first
        retract(delta_ordering(_)),
        fail.
admissibility_first :-
        assert(delta_ordering(admissibility>compatibility)).

:- admissibility_first.                     % default is to check admissibility first


%%% Verbose proof printing is turned on by verbose_mode,
%%% off by dont_verbose_mode.
%%% works during compile- and run-time

verbose_mode :-                          % enable proof printing
        retract(verbose_flag),
        fail.
verbose_mode :-
        assert(verbose_flag).

no_verbose_mode :-                     % disable proof printing
        retract(verbose_flag),
        fail.
no_verbose_mode.

:- verbose_mode.                         % default is to print proof

%%% verbose predicate, chatting if verbose_mode is turned on
verbose(X) :-
	verbose_flag ->
	        write(X),nl;
	%true->
		true.

%%% PRINT CONFIGURATION
%%%

:- xray_configuration,nl.

configuration :-
	hook_configuration,
	lemma_configuration,
	pttp_configuration,
	xray_configuration.

%%% ----------------------------------------------------------------------
%%% XRay COMPILATION (patches pttp)

%%% Negation normal form/Defaults to Prolog clause translation.
%%% Include a literal in the body of each clause to
%%% indicate the number of the formula the clause came from.

clauses((A , B),L,WffNum) :-
        !,
        clauses(A,L1,WffNum),
        WffNum2 is WffNum + 1,
        clauses(B,L2,WffNum2),
        conjoin(L1,L2,L).
clauses( (Gamma :- Alpha : Beta) , L , WffNum ) :-
	!,
	clauses((Gamma :- gamma(WffNum,Gamma)),L1,WffNum),
	clauses((alpha(WffNum,Alpha) :- Alpha),L2,WffNum),
	conjoin(L1,L2,L3),
	conjoin(Gamma,Beta,C0),                             % ConDL-specific
	cnf(C0,C1),
	make_matrix(C1,C2),
	matrix_reduction(C2,Justification),
	(delta_ordering(compatibility>admissibility) ->
	    conjoin(justification(Justification),
	            alpha(WffNum,Alpha),
		    Body);
	%true ->
	    conjoin(alpha(WffNum,Alpha),
		    justification(Justification),
	            Body)),
	(compile_proof_printing ->
	    Record = infer_by(default(WffNum:(Gamma :- Alpha : Justification)));
	%true ->
	    Record = true),
	conjoin(Record,Body,Body1),
	DRule= (gamma(WffNum,Gamma) :- Body1),
	conjoin(DRule,L3,L).

clauses(A,L,WffNum) :-
        head_literals(A,Lits),
        clauses(A,Lits,L,WffNum).

clauses(A,[Lit|Lits],L,WffNum) :-
        body_for_head_literal(Lit,A,Body1),
        ((compile_proof_printing,Body1 = true) ->
                Record = infer_by(unit(WffNum:Lit));
         compile_proof_printing ->
                Record = infer_by(extension(WffNum:Lit));
        %true ->
		Record = true),
	conjoin(Record,Body1,Body),
        clauses(A,Lits,L1,WffNum),
        conjoin((Lit :- Body),L1,L).
clauses(_,[],true,_).

%%% This patches the original predicate
%%%
%%%

add_ancestor((Head :- Body),(Head1 :- Body1)) :-
        functor(Head,query,_) ->
                Head1 = Head,
                add_ancestor_args(Body,[[],[],[]],Body1);
	functor(Head,gamma,_) ->                      
                Head =.. L,                             
                append(L,[_,_,Defaults],L1),                             
                Head1 =.. L1,                                   
                add_ancestor_args(Body,[[],[],NewDefaults],Body2),
		conjoin((NewDefaults = [Head|Defaults]),Body2,Body1); 
	functor(Head,alpha,_) ->                      
                Head =.. L,                             
                append(L,[_,_,Defaults],L1),                             
                Head1 =.. L1,                                   
                add_ancestor_args(Body,[[],[],Defaults],Body1); 
        %true ->
                Head =.. L,
                append(L,[PosAncestors,NegAncestors,Defaults],L1),
                Head1 =.. L1,
                add_ancestor_args(Body,[NewPosAncestors,NewNegAncestors,Defaults],Body2),
                (Body == Body2 ->
                        Body1 = Body2;
                negative_literal(Head) ->
                        NewPosAncestors = PosAncestors,
                        conjoin((NewNegAncestors = [Head|NegAncestors]),Body2,Body1);
                %true ->
                        NewNegAncestors = NegAncestors,
                        conjoin((NewPosAncestors = [Head|PosAncestors]),Body2,Body1)).


ancestor_tests(P,N,Result) :-
        P == query ->
                Result = true;
	P == gamma ->
		Head = gamma(DefaultID,DefaultConseq,_,_,Defaults),
		Default = gamma(DefaultID,DefaultConseq),
		Result = (Head :- identical_member(Default,Defaults), !, fail);
	P == alpha ->
		Result = true;          % ??? <== Please, VERIFY !
        %true ->
                negated_functor(P,NotP),
                N3 is N - 3,            % N - 3 due to 3 ancestor-lists
                functor(Head1,P,N3),
                Head1 =.. [P|Args1],
                Head2 =.. [NotP|Args1],
                append(Args1,[PosAncestors,NegAncestors,_],Args),
                Head =.. [P|Args],
                (negative_functor(P) ->
                        C1Ancestors = NegAncestors, 
			C2Ancestors = PosAncestors;
                %true ->
                        C1Ancestors = PosAncestors, 
			C2Ancestors = NegAncestors),
                C1 = (Head :- identical_member(Head1,C1Ancestors), !, fail),
                count_inferences_pred(IncNcalls),
                (N3 = 0 ->              % special case for propositional calculus
                        conjoin((identical_member(Head2,C2Ancestors) , !),IncNcalls,V);
                %true ->
                        conjoin(unifiable_member(Head2,C2Ancestors),IncNcalls,V)),
                (compile_proof_printing ->
                        conjoin(V,infer_by(reduction(Head2)),V1);
                %true ->
                        V1 = V),
                C2 = (Head :- V1),
                conjoin(C1,C2,Result).


procedures_with_tests([[P,N]|Preds],Clauses,Procs) :-
        procedure(P,N,Clauses,Proc0),

        ancestor_tests(P,N,TestsA),
	lemma_tests(P,N,TestsL),
	conjoin(TestsA,TestsL,Tests),

	phook_tests(P,N,Tests,Proc0,ProcP),

        procedures_with_tests(Preds,Clauses,ProcsPs),
        conjoin(ProcP,ProcsPs,Procs).
procedures_with_tests([],_Clauses,true).


%%% Consistency checking.
%%%
%%% Add extra arguments to each goal so that information
%%% on what assumptions were made in the proof can be checked
%%% at each step/the end.
%%%
%%% I suppose Wff has to be replaced by cmm(Model,ModelStructure) ...
%%% [all this is a quick copy of add_proof_recording ...]

add_consistency_checking((Head :- Body),(Head1 :- Body1)) :-
        functor(Head,query,_) ->
                Head1 = Head,
		conjoin(model_initialization(MM0),Body,Body2),
		add_consistency_checking_args(Body2,MM0,MMOut,Body1);
        %true ->
		Head =.. L,
		append(L,[MMIn,MMOut],L1),
		Head1 =.. L1,
		add_consistency_checking_args(Body,MMIn,MMOut,Body1).

add_consistency_checking_args(Body,MMIn,MMOut,Body1) :-
        Body = (A , B) ->
                add_consistency_checking_args(A,MMIn,MMIn1,A1),
                add_consistency_checking_args(B,MMIn1,MMOut,B1),
                conjoin(A1,B1,Body1);
        Body = (A ; B) ->
                add_consistency_checking_args(A,MMIn,MMOut,A1),
                add_consistency_checking_args(B,MMIn,MMOut,B1),
                disjoin(A1,B1,Body1);
        Body =.. [search,Goal|L] ->
                add_consistency_checking_args(Goal,MMIn,MMOut,Goal1), % ???
                Body1 =.. [search,Goal1|L];
        Body = justification(X) ->
		Body1 = compatible(X,MMIn,MMOut);
        Body = fail ->
                Body1 = Body;
        builtin(Body) ->
                MMIn = MMOut,
                Body1 = Body;
        %true ->
                Body =.. L,
                append(L,[MMIn,MMOut],L1),
                Body1 =.. L1.

add_model_structure(WffI,Q,C,WffO) :-
	WffI = (A , B) ->
                add_model_structure(A,Q,C,A1),
                add_model_structure(B,Q,C,B1),
		conjoin(A1,B1,WffO);
        WffI = (A ; B) ->
                add_model_structure(A,Q,C,A1),
                add_model_structure(B,Q,C,B1),
		disjoin(A1,B1,WffO);
        WffI = (A :- B) ->
                add_model_structure(B,Q,C,B1),
		WffO = (A :- B1);
	WffI = model_initialization(Var) ->
	        combine_clauses(Q,C,Matrix),
		WffO = model_initialization(Matrix,Var);
        %true ->
	        WffO = WffI.

classical_clauses(WffI,WffO):-
    nnf(WffI,WffM),
    classical_clauses0(WffM,WffO).

classical_clauses0(WffI,WffO) :-
        WffI = (A , B) ->
                classical_clauses(A,A1),
                classical_clauses(B,B1),
		conjoin(A1,B1,WffO);
        WffI = (A ; B) ->
                classical_clauses(A,A1),
                classical_clauses(B,B1),
		disjoin(A1,B1,WffO);
        WffI = (A :- B) ->                        % ??? (special case query elim. TS Apr04)
	        WffO = true;
        builtin(WffI) ->
                WffO = true;
        %true ->
	        WffI = WffO.

query_clause(WffI,WffO) :-
        WffI = (A , B) ->
	        (query_clause(A,WffO);
                 query_clause(B,WffO));
        WffI = (A ; B) ->
                (query_clause(A,WffO);
                 query_clause(B,WffO));
        WffI = (A :- B) ->
	        (A = query ->
                      classical_clauses(B,WffO);
		 %true ->
                      fail);
        %true ->
	        fail.

query(M) :-                             % call query with depth bound M
	(compile_complete_search, compile_proof_printing , lemma_handling_flag) ->
	        query(M,_N,_LemProof,_LemProofEnd,_Proof,_ProofEnd);
        (compile_complete_search, (compile_proof_printing ; lemma_handling_flag)) -> 
                query(M,_N,_Proof,_ProofEnd);
        compile_complete_search ->
                query(M,_N).

query :-                                % unbounded search of query
        (compile_complete_search ->
	    query(1000000);
	%true ->
	    ((compile_proof_printing , lemma_handling_flag) ->
	         query(_LemProof,_LemProofEnd,_Proof,_ProofEnd);
	     (compile_proof_printing ; lemma_handling_flag) -> 
		 query(Proof,_ProofEnd);
             %true ->
		 query)).
	    


%%% ----------------------------------------------------------------------
%%% XRay: THE ACTUAL COMPILATION PROCEDURES
%%%

xray(Name) :-
	read_kb(Name,KB),
	dpttp(Name,KB).
	

dpttp(Name,X) :-
        time(dpttp1(X,Y:Z),'Compilation Phase I'),
	time(dpttp2(Name,Y:Z),'Compilation Phase II'),
	time(dpttp3(Name),'Compilation Phase III').

dpttp(X) :-
	Name = 'temp',
	dpttp(Name,X).

dpttp1(X0,Y:C) :-
      nnf(X0,X),
        nl,
        write('XRay input formulas:'),
        apply_to_conjuncts(X,write_clause,_),
        nl,

	constants(X,H),
	(H = [] ->
	    nl,write('Empty Herbrand universe.'),nl;
	 %true ->
	    nl,write('Herbrand universe':H),nl),

	classical_clauses(X,C0),
	cnf(C0,C1),
	make_matrix(C1,C2),
	instantiation(C2,H,C3),
	matrix_reduction(C3,C),

        (verbose_flag ->
	     nl,
	     write('Classical output formulas:'),
	     apply_to_list(C,write_clause,_),
	     nl;
	%true ->
	     true),

	dpttp1(X,C,Y:C).

dpttp1(X,C,Y:C) :- 
	query_clause(X,Q0),
	variables(Q0,Vars),
	(Vars=[] ->
	         cnf(Q0,Q1),
		 make_matrix(Q1,Q2),
		 matrix_reduction(Q2,Q),

		 XQ=X;
	%true ->
		 Q = [],

		 apply_to_conjuncts(X,add_answer_preds,XQ)),

        (verbose_flag ->
	     nl,
	     write('Query formula:'),
	     apply_to_conjuncts(Q0,write_clause,_),
	     nl,
	     write('      compiled:'),
	     apply_to_list(Q,write_clause,_),
	     nl,nl;
	%true ->
	     true),

	dpttp1(XQ,Q,C,Y:C).

dpttp1(X,Q,C,Y:C) :- 
        clauses(X,XC,1),

	constants(X,H),
	(H = [] ->
	    XH=true,
	    X0=XC;
	 %true ->
	    herbrand_preds(H,XH),
	    apply_to_conjuncts(XC,add_herbrand_preds,X0)),

        apply_to_conjuncts(X0,add_count_inferences,X1),
        apply_to_conjuncts(X1,add_ancestor,X2),
        predicates(X2,Preds0),
        reverse(Preds0,Preds),
	procedures_with_tests(Preds,X2,X3),
	/* all contrapositives available */
        apply_to_conjuncts(X3,add_sound_unification,X4),
	apply_to_conjuncts(X4,add_consistency_checking,X5),
        (compile_complete_search ->
	    apply_to_conjuncts(X5,add_complete_search,X6);
	%true ->
	    X5=X6),
	apply_to_conjuncts(X6,add_lemmatization,XL), /* relies on 'infer_by */
	(compile_proof_printing ->
                apply_to_conjuncts(XL,add_proof_recording,X7);
	%true ->
                X7 = XL),
	add_model_structure(X7,Q,C,X8),

	apply_to_conjuncts(X8,add_body_hooks,XD),

	apply_to_conjuncts(X,prolog_clause,XP),
	conjoin(XP,XD,XR),

	conjoin(XH,XR,Y),

        (verbose_flag -> 
	     (nl,
	     write('XRay output formulas:'),
	     %apply_to_conjuncts(Y,write_clause,_),
	     nl);
	%true ->
	     true),
	!.

dpttp1(X) :-
	dpttp1(X,_).

dpttp2(Name,Y:Z) :-
        nl,
        write('XRay writing compiled clauses ... '),
        write_ckb(Name,Y),
	write_cmm(Name,Z),
	write('done.'),
	!.

dpttp2(Y:Z) :-
	Name = 'temp',
	dpttp2(Name,Y:Z).

dpttp3(Name) :-
	nl,
        write('XRay compiling clauses ... '),
        compile_ckb(Name),
	write('done.'),
        nl,
        write('XRay compiling query ... '),
        compile_query(Name),
	write('done.'),
        nl,
        !.
dpttp3 :-
	Name = 'temp',
	dpttp3(Name).




:- getenv('LOGICMOO_HOME',_) -> true ; setenv('LOGICMOO_HOME','t:/devel/logicmoo').

user:file_search_path(logicmoo, X) :- getenv('LOGICMOO_HOME',MOO),atom_concat(MOO,'/src', X).

:- use_module(logicmoo(logicmoo_util/logicmoo_util_bugger)).
:- use_module(logicmoo(logicmoo_util/logicmoo_util_library)).
:- use_module(logicmoo(logicmoo_util/logicmoo_util_ctx_frame)).
:- use_module(logicmoo(logicmoo_util/logicmoo_util_strings)).
:- use_module(logicmoo(logicmoo_util/logicmoo_util_terms)).
:- use_module(logicmoo(logicmoo_util/logicmoo_util_dcg)).
:- use_module(logicmoo(logicmoo_util/logicmoo_util_library)).

:- '@'((getenv('LOGICMOO_HOME',MOO),
      atom_concat(MOO,'/src/logicmoo_util/logicmoo_util_all.pl', X),
      ensure_loaded(X)),'user').

xray_test(Name):- pttp_test(Name,Data),pttp(Data), time(search(query)).

unused_module_decl:-module(dbase_rules_pttp,[ nnf/2, pttp1/2,
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
       pttp_test/2,
       do_pttp_test/1
        ]).

:-   op(400,fy,-),    % negation
      op(500,xfy,&),   % conjunction
      op(600,xfy,v),   % disjunction
      op(650,xfy,(=>)),  % implication
      op(680,xfy,(<=>)), % equivalence
      op( 500, fy, (~)),    % negation
      op( 500, fy, all),  % universal quantifier
      op( 500, fy, ex),   % existential quantifier
      !.

% :- include(old_pttp).

user:file_search_path(yadlr, X) :- getenv('LOGICMOO_HOME',MOO),atom_concat(MOO,'/src_modules/yadlr/pl', X).

% aleph alias must resolve to the directory where aleph.pl exists.
% you can download aleph from http://www.comlab.ox.ac.uk/oucl/research/areas/machlearn/Aleph/aleph.pl
user:file_search_path(aleph, X) :- getenv('LOGICMOO_HOME',MOO),atom_concat(MOO,'/src_modules/aleph', X).

:- discontiguous pttp_test/2.
:- thread_local int_query/6.
:- thread_local int_query/7.

unimplemented:-throw(unimplemented).

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


prove(Q):-search(Q).

:-include(dbase_rules_nnf).
:-include(dbase_rules_testing).

pttp_assert(X) :- pttp(X).


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


:- module_predicates_are_exported(dbase_rules_pttp).
% :- module_meta_predicates_are_transparent(dbase_rules_pttp). 


:-pttp_assert(test123).
:-pttp_query(test123).

:-do_pttp_test(_X).
:-prolog.

:-xray(jusC_10_5).

























































end_of_file.

   

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

prove(Goal,Max,Min,Inc,ProofIn,ProofOut) :-
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
	prove(Goal,Max,0,1,[],_).

prove(Goal) :-!,
	prove(Goal,100,0,1,[],_).
prove(Goal) :-
	prove(Goal,1000000,0,1,[],_).

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

unify(X,Y) :-!, unify_with_occurs_check(X,Y).

unify(X,Y) :-
	var(X) ->
		(var(Y) ->
			X = Y;
		%true ->
			functor(Y,_,N),
			(N = 0 ->
				true;
			N = 1 ->
				arg(1,Y,Y1), not_occurs_in(X,Y1);
			%true ->
				not_occurs_in_args(X,Y,N)),
			X = Y);
	var(Y) ->
		functor(X,_,N),
		(N = 0 ->
			true;
		N = 1 ->
			arg(1,X,X1), not_occurs_in(Y,X1);
		%true ->
			not_occurs_in_args(Y,X,N)),
		X = Y;
	%true ->
		functor(X,F,N),
		functor(Y,F,N),
		(N = 0 ->
			true;
		N = 1 ->
			arg(1,X,X1), arg(1,Y,Y1), unify(X1,Y1);
		%true ->
			unify_args(X,Y,N)).

unify_args(X,Y,N) :-
	N = 2 ->
		arg(2,X,X2), arg(2,Y,Y2), unify(X2,Y2),
		arg(1,X,X1), arg(1,Y,Y1), unify(X1,Y1);
	%true ->
		arg(N,X,Xn), arg(N,Y,Yn), unify(Xn,Yn),
		N1 is N - 1, unify_args(X,Y,N1).
%%% ***
%%% ****if* PTTP/not_occurs_in
%%% DESCRIPTION
%%%   not_occurs_in(Var,Term) fails if variable Var occurs inside
%%%   Term and succeeds otherwise.
%%% SOURCE

not_occurs_in(Var,Term) :-
	Var == Term ->
		fail;
	var(Term) ->
		true;
	%true ->
		functor(Term,_,N),
		(N = 0 ->
			true;
		N = 1 ->
			arg(1,Term,Arg1), not_occurs_in(Var,Arg1);
		%true ->
			not_occurs_in_args(Var,Term,N)).

not_occurs_in_args(Var,Term,N) :-
	N = 2 ->
		arg(2,Term,Arg2), not_occurs_in(Var,Arg2),
		arg(1,Term,Arg1), not_occurs_in(Var,Arg1);
	%true ->
		arg(N,Term,ArgN), not_occurs_in(Var,ArgN),
		N1 is N - 1, not_occurs_in_args(Var,Term,N1).
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

search(_Goal,Max,Min,_Inc,_PrevInc,_DepthIn,_DepthOut) :-
	Min > Max,
	!,
	fail.
search(Goal,_Max,Min,_Inc,PrevInc,DepthIn,DepthOut) :-
        write_search_progress(Min),
	DepthIn = Min,
	catch(call(Goal),E,trace),
	DepthOut < PrevInc.	% fail if solution found previously
search(Goal,Max,Min,Inc,_PrevInc,DepthIn,DepthOut) :-
	Min1 is Min + Inc,
	search(Goal,Max,Min1,Inc,Inc,DepthIn,DepthOut).
%%% ***

%%% ****if* PTTP/make_wrapper
%%% SOURCE

make_wrapper(_DefinedPreds,[query,0],true) :-
	!.
make_wrapper(DefinedPreds,[P,N],Result) :-
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
	builtin(Wff) ->
		L = [];
	%true ->
		functor(Wff,F,N),
		L = [[F,N]].
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

%%% ****if* PTTP/add_features
%%% SOURCE

add_features((Head :- Body),(Head1 :- Body1)) :-
  
	(is_query_lit(Head) ->
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
		%true ->
			NewPosAncestors = PosAncestors,
			PushAnc = (NewNegAncestors = [GoalAtom|NegAncestors])),
		search_cost(Body,HeadArgs,Cost),
		test_and_decrement_search_cost_expr(DepthIn,Cost,Depth1,TestExp),
		conjoin(PushAnc,Body2,Body4),
		conjoin(Matches,Body4,Body5),
		conjoin(TestExp,Body5,Body1)),
    	Head2 =.. [P|L],
	internal_functor(P,IntP),
	list_append(L,[PosAncestors,NegAncestors,
		       DepthIn,DepthOut,
		       ProofIn,ProofOut,
		       GoalAtom],
		    L1),
	Head1 =.. [IntP|L1].
%%% ***
%%% ****if* PTTP/add_args
%%% SOURCE

add_args(Body,PosGoal,GoalAtom,HeadArgs,
         PosAncestors,NegAncestors,
	 NewPosAncestors,NewNegAncestors,
	 DepthIn,DepthOut,
	 ProofIn,ProofOut,
	 Body1,New) :-
	Body = (A , B) ->
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
		conjoin(A1,B1,Body1);
	Body = (A ; B) ->
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
		disjoin(A1,B1,Body1);
	functor(Body,search_cost,_) ->
		DepthOut = DepthIn,
		ProofOut = ProofIn,
		Body1 = true;
	Body = infer_by(N) ->
		DepthOut = DepthIn,
		(PosGoal = yes -> 
			N1 = N;
		%true ->  % atom in proof is negation of actual literal
			N1 is - N),
		Body1 = (ProofIn = [Prf,[N1,GoalAtom,PosAncestors,NegAncestors]|PrfEnd],
			 ProofOut = [Prf|PrfEnd]);
	Body = search_cost(N) ->
		DepthOut = DepthIn,
		ProofOut = ProofIn,
		Body1 = true;
	builtin(Body) ->
		DepthOut = DepthIn,
		ProofOut = ProofIn,
		Body1 = Body;
	%true ->
		Body =.. L,
		list_append(L,
                       [NewPosAncestors,NewNegAncestors,
		        DepthIn,DepthOut,
			ProofIn,ProofOut],
		       L1),
		Body1 =.. L1,
		New = yes.
%%% ***

%%% ****if* PTTP/pttp1
%%% SOURCE

pttp1(X,Y) :-
	nl,
	write('PTTP input formulas:'),
	clauses(X,X0,1,_),		% prints and transforms input clauses
	nl,
	apply_to_conjuncts(X0,add_features,X8),
	predicates(X8,IntPreds0),
	list_reverse(IntPreds0,IntPreds1),
	procedures(IntPreds1,X8,IntProcs),
	predicates(X0,Preds0),
	list_reverse(Preds0,Preds),
	apply_to_elements(Preds,make_wrapper(IntPreds1),Procs),
	conjoin(IntProcs,Procs,Y),
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

negated_literal(A,B):-isVar(A),!,trace,B=not(A),!.
negated_literal(-A,B):-negated_literal(A,AA),!,negated_literal(AA,B).
negated_literal(A,B):-var(B),!,negated_literal_0(A,B).
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
	write(A),
	write(.).

write_indent_for_number(N) :-
	((number(N) , N <  100) -> write(' ') ; true),
	((number(N) , N <   10) -> write(' ') ; true).

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

builtin(!,0).
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
%builtin(F,A):-moo:isRegisteredCycPred(Mt,F,A),!,fail.
%builtin(F,A):-functor(P,F,A),predicate_property(P,_).
%%% ***



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

dalit_make_wrapper(_DefinedPreds,[dalit_query,0],true) :-
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

dalit_query(PosAncestors,NegAncestors,DepthIn,ProofIn,ProofOut) :-
	int_query(PosAncestors,NegAncestors,DepthIn,ProofIn,ProofOut,dalit_query).

dalit_add_features((Head :- Body),(Head1 :- Body1)) :-
	(functor(Head,dalit_query,_) ->
		Head2 = Head,
		dalit_add_args(Body,yes,dalit_query,[],
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


:-pttp_assert(test123).
:-pttp_query(test123).

end_of_file.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date: 13/07/96   File: lemmaflex.pl
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%% 11/07/96 Created                                                          %%
%% 13/07/96 added compilation
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog lemmaflex.pl                                              %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lemma_runtime_procedures(Result) :-
	lemmatize_procedure(Lemmatization0),

	lemmatize_dynamically_procedure(DynamicLemmatization),
	lemmatize_statically_procedure(StaticLemmatization),
	conjoin(DynamicLemmatization,StaticLemmatization,Lemmatization1),

	conjoin(Lemmatization0,Lemmatization1,Lemmatization),

	default_assumptions_procedure(DefaultHandler),

	conjoin(Lemmatization,DefaultHandler,Result).

lemmatize_procedure(Result) :-
	lemma_handling_flag,
	!,

	/* DEFAULTHANDLER */
        ((static_lemma_flag;dystatic_lemma_flag)        % compile-flag 
	       ->
	          DefaultHandler   = default_assumptions(ProofOut,ProofEnd,Ass);
	 %true ->
		  DefaultHandler   = true,
		  Ass              = []
	),

	/* DELTA LEMMATIZE */
	(lemma_type_parameter(delta) ->
	    HeadD = lemmatize(default(N:(Gamma :- _)),Proof,ProofOut,ProofEnd),
	    /* REDUCTIONHANDLER */
	    (lemma_type_parameter(omega) ->
		ReductionHandlerD = remove_reductions(Proof,ProofOut);
	    %true ->
		ReductionHandlerD = true,
		ProofOut=Proof),
	    conjoin(ReductionHandlerD,DefaultHandler,BodyD1),
	    /* LEMMAHANDLER   */
	    lemma_handler(gamma(N,Gamma),false,Ass,LemmaHandlerD),
	    conjoin(BodyD1,LemmaHandlerD,BodyD),
	    RuleD = (HeadD :- !,BodyD);
	%true ->
	    RuleD = true),

	/* OMEGA LEMMATIZE */
	(lemma_type_parameter(omega) ->
	    HeadO             = lemmatize(extension(_:Goal),Proof,ProofOut,ProofEnd),
	    /* REDUCTIONHANDLER */
	    ReductionHandlerO = skim_reductions(Goal,Proof,ProofOut,ProofEnd,Ancs),
	    conjoin(ReductionHandlerO,DefaultHandler,BodyO1),
	    /* LEMMAHANDLER   */
	    lemma_handler(Goal,Ancs,Ass,LemmaHandler1),
	    (lemma_format_parameter(unit) ->
		LemmaHandlerO = (Ancs=false -> LemmaHandler1 ; verbose(skipping:Goal:Ancs));
	    %true ->
		LemmaHandlerO = LemmaHandler1),
	    conjoin(BodyO1,LemmaHandlerO,BodyO),
	    RuleO = (HeadO :- !,BodyO);
	%true ->
	    RuleO = true),

	/* ignore LEMMATIZE */
        HeadI = lemmatize(Inference,Proof,Proof,ProofEnd),
	BodyI = verbose("'lemmatize/4: Ignoring inference'":Inference),
	RuleI = (HeadI :- BodyI),

	/* RESULTING CODE */
        conjoin(RuleD,RuleO,Result1),
        conjoin(Result1,RuleI,Result).
lemmatize_procedure(Result) :-
	/* no_lemma_handling */
	Result = (lemmatize(_,_,_,_)).

lemma_handler(Goal,Ancs,Ass,LemmaHandler) :-
	((dynamic_lemma_flag;dystatic_lemma_flag) ->
	    DynamicLemmaHandler = lemmatize_dynamically(Goal,Ancs,Ass);
        %true ->
	    DynamicLemmaHandler = true),
	(static_lemma_flag ->
	    StaticLemmaHandler = lemmatize_statically(Goal,Ancs,Ass);
        %true ->
	    StaticLemmaHandler = true),
	conjoin(DynamicLemmaHandler,StaticLemmaHandler,LemmaHandler).

lemmatize_dynamically_procedure(Result) :-
	lemma_handling_flag,
	lemma_mode_parameter(Mode),
	(Mode = dynamic ; Mode = dystatic),
	!,

	Head0 = lemmatize_dynamically(Goal,Ancestors,_),
	Body0 = dynamic_lemma(Goal,Ancestors,_),           % UNIFY ?!
	Rule0 = (Head0 :- Body0, !),
	
	(dystatic_lemma_flag ->

	    Head1  = lemmatize_dynamically(Goal,Ancestors,[]),
	    Body1A = assert(dynamic_lemma(Goal,Ancestors,[])),
	    Body1S = lemmatize_statically(Goal,Ancestors,[]),
	    Rule1  = (Head1 :- !, Body1A, Body1S),

	    Body2S = lemmatize_statically(Goal,Ancestors,Assumptions);

	%true ->
	    Rule1  = true,
	    Body2S = true),

	conjoin(Rule0,Rule1,Rule01),

	Head2  = lemmatize_dynamically(Goal,Ancestors,Assumptions),
	Body2A =  assert(dynamic_lemma(Goal,Ancestors,Assumptions)),
	Body2R = retract(dynamic_lemma(Goal,Ancestors,Assumptions)),
	conjoin(Body2R,Body2S,Body2RS),
	Rule2  = (Head2 :- Body2A ; (Body2RS, ! , fail)),

	conjoin(Rule01,Rule2,Result).
lemmatize_dynamically_procedure(Rule) :-
	Head = lemmatize_dynamically(_,_,_),
	Body = (write(lemmatize_dynamically*not_in_charge),nl,trace,fail),
	Rule = (Head :- Body).

lemmatize_statically_procedure(Result) :-
	lemma_handling_flag,
	lemma_mode_parameter(Mode),
	(Mode = static ; Mode = dystatic),
	!,

	Head0  = lemmatize_statically(Goal,Ancestors,Assumptions),
	Body0  = static_lemma(Goal,Ancestors,Assumptions),           % UNIFY ?!
	Rule0  = (Head0 :- Body0, !),
	
	Head1  = lemmatize_statically(Goal,Ancestors,Assumptions),
	Body1A = assert(static_lemma(Goal,Ancestors,Assumptions)),
	Rule1  = (Head1 :- Body1A),

	Result = (Rule0,Rule1).
lemmatize_statically_procedure(Rule) :-
	Head = lemmatize_statically(_,_,_),
	Body = (write(lemmatize_statically*not_in_charge),nl,trace,fail),
	Rule = (Head :- Body).

default_assumptions_procedure(Result) :-
	lemma_handling_flag,
	lemma_mode_parameter(Mode),
	(Mode = static ; Mode = dystatic),
	!,
	
	Head0 = default_assumptions(Proof,ProofEnd,[]),
	Body0 = (Proof == ProofEnd),
	Rule0 = (Head0 :- Body0, !),

	BodyR = default_assumptions(Proof,ProofEnd,Justs),

	Body123 = (BodyR,combine_clauses(Just,Justs,Assumptions)),

	Head1 = default_assumptions([default(_:(_ :- _: Just))|Proof],ProofEnd,Assumptions),
	Head2 = default_assumptions([static_lemma(_   : Just) |Proof],ProofEnd,Assumptions),
	Head3 = default_assumptions([dynamic_lemma(_  : Just) |Proof],ProofEnd,Assumptions),

	Rule1 = (Head1 :- !, Body123),
	conjoin(Rule0,Rule1,Rule01),

	Rule2  = (Head2 :- !, Body123),
	Rule3  = (Head3 :- !, Body123),
	Rule23 = (Rule2,Rule3),
	conjoin(Rule01,Rule23,Rule0123),

	Head4 = default_assumptions([_|Proof],ProofEnd,Justs),
	Rule4 = (Head4 :- BodyR),

	conjoin(Rule0123,Rule4,Result).
default_assumptions_procedure(Rule) :- 
	Head = default_assumptions(_,_,_),
	Body = (write(default_assumptions*not_in_charge),nl,trace,fail),
	Rule = (Head :- Body).

%%% Compilation of run-time procedures for lemma handling
%%%

compile_lemma_handling(Name) :-
	lemma_runtime_procedures(LemmaProcs),
	write_lem(Name,LemmaProcs),
	compile_lem(Name).

read_lem(Name,Wff) :-
	concatenate(Name,'.lem',LFile),
	read_clauses(LFile,Wff).	
write_lem(File,LemmaProcs) :-
	concatenate(File,'.lem',LemmaFile),
	open(LemmaFile,write,LemmaStream),
        write_clauses(LemmaStream,LemmaProcs),
        close(LemmaStream),
	!.
compile_lem(File) :-	
	concatenate(File,'.lem',KBFile),
	compile(KBFile).

