%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FILENAME:  common.pl 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file contain common predicates that are used in planners

:- dynamic(domain/3).
:- dynamic(pairfrom/4).


end_of_file.

:- expects_dialect(sicstus).
:- use_module(library(timeout)).
:- use_module(library(lists)).



% command_line/0
%
%   run planner
%   takes two params from command line arguments
%   first is a domain file 
%   second problem file
%

:-set_prolog_stack(global, limit(16*10**9)).
:-set_prolog_stack(local, limit(16*10**9)).
:-set_prolog_stack(trail, limit(16*10**9)).

command_line:-
    prolog_flag(argv, [D,P]),!,
    solve_files(D, P),
    halt.

command_line:- test_blocks, test_all.

test_all:- expand_file_name('/devel/LogicmooDeveloperFramework/PrologMUD/packs/logicmoo_engine/prolog/logicmoo/planner/test/*/domain*.pddl',RList),reverse(RList,List),
  forall(member(E,List),once(test_domain(E))).

test_domain(DP):-
   format('~q.~n',[test_domain(DP)]),
  directory_file_path(D,_,DP),directory_files(D,RList),reverse(RList,List),forall(member(T,List),ignore((directory_file_path(D,T,TP),exists_file(TP),not(same_file(DP,TP)),solve_files(DP,TP)))).

test_blocks:- solve_files('/devel/LogicmooDeveloperFramework/PrologMUD/packs/logicmoo_engine/prolog/logicmoo/planner/test/blocks/domain-blocks.pddl', '/devel/LogicmooDeveloperFramework/PrologMUD/packs/logicmoo_engine/prolog/logicmoo/planner/test/blocks/blocks-03-0.pddl').

% solve_files(+DomainFile, +ProblemFile)
%
%   Reads files and set timelimit for planner
%
solve_files(DomainFile, ProblemFile):- 
 time(solve_files_0(DomainFile, ProblemFile)).

solve_files_0(DomainFile, ProblemFile):-
  format('~q.~n',[solve_files(DomainFile, ProblemFile)]),
    parseDomain(DomainFile, DD, _),
    parseProblem(ProblemFile, PP, _),
    term_to_ord_term(DD, D),
    term_to_ord_term(PP, P),
    reset_statistic,
    !,
    time_out(solve(D, P, S), 500000, _Result), % time limit for a planner
    show_statistic(P, S),
    !.


% solve(+Domain, +Problem, -Solution).
%
%   Set domain and problem on blackboard
%
solve(D, P, Solution):-
    get_init(P, I),    bb_put(initState, I),
    get_goal(P, G),    bb_put(goalState, G),
    get_metric(P, M),    bb_put(metric, M),
    get_actions(D, A),	bb_put(actions, A),
    get_objects(P, O),	bb_put(objects, O),
    make_init_state(IS),
    search(IS, G, Solution).


% term_to_ord_term(+Term, -OrdTerm)
%
%   Go throught the term and look for sets, return the same term
%   with all sets become ordered.
%
term_to_ord_term([], []).
term_to_ord_term(A, A):-
    atomic(A),
    !.
term_to_ord_term([H|T], R):-
    term_to_ord_term(H, OH),
    term_to_ord_term(T, OT),
    ord_add_element(OT, OH, R),
    !.
%    write(OH), write(OT), write('   '), write(R), nl.
term_to_ord_term(T, OT):-
    T =.. [F,P],
    !,
    term_to_ord_term(P, OP),
    OT =..[F,OP].
term_to_ord_term(T, OT):-
    T =.. [F,P|Ps],
    NT=.. [F|Ps],
    term_to_ord_term(P, OP),
    term_to_ord_term(NT, ONT),
    ONT =.. [_|OPs],
    OT =.. [F,OP|OPs],
    !.


% mysubset(+Subset, +Set)
%
%   It is similar to subset/2. Subset can include free variables that are 
%   grounded with atoms of Set.
%
mysubset([], _).
mysubset([X|R], S):- 
    member(X, S),
    mysubset(R, S).



% Collection of shortcuts


% get(+Structure, -Parameter)
%
get_actions(     	domain(_, _, _, _, _, _, _, A), A).    
get_problem_name(	problem(N, _, _, _, _, _, _, _, _), N).
get_init(    problem(_, _, _, _, I, _, _, _, _), I).
get_goal(    problem(_, _, _, _, _, G, _, _, _), G).
get_metric(    problem(_, _, _, _, _, _, _, M, _), M).
get_objects(    problem(_, _, _, O, _, _, _, _, _), O).
get_precondition(	action(_, _, P, _, _, _), P).
get_positiv_effect(	action(_, _, _, PE, _, _), PE).
get_negativ_effect(	action(_, _, _, _, NE, _), NE).
get_assign_effect(	action(_, _, _, _, _, AE), AE).
get_parameters(    action(_, P, _, _, _, _), P).
get_action_def(    action(Name, Params, _, _, _, _), F):-
    untype(Params, UP),
    F =.. [Name|UP].


% get_action(-Action, -ActionDef)
%
get_action(A):-
    get_action(A, _). 
get_action(A, ActionDef):-
    bb_get(actions, As),
    member(Afree, As),
    copy_term_spec(Afree, A),
%    A =.. [_, Name, Params|_],
    get_action_def(A, ActionDef).


get_goal(G):-bb_get(goalState, G).
get_init(I):-bb_get(initState, I).


% untype(LitOfParams, UntyperList).
%
untype([], []).
untype([H|T], [U|Us]):- 
    compound(H),
    H =.. [_T, [U]],
    !,
    untype(T, Us).
untype([H|T], [H|Us]):-
    untype(T, Us).


% setInit(+Init, -State)
%
setInit([], []).
setInit([set(F, V)|Ls], S):-
    F =.. A,
    concat_atom_iio(A, '-', CA),
    bb_put(CA, V),
%    write(CA),write(' '), write(V),  nl,
    setInit(Ls, S),
    !.
setInit([A|Ls], [A|Ss]):-
    setInit(Ls, Ss).


% concat_atom_iio(+List, +Delimiter, -ConcatenateAtom)
%

concat_atom_iio([E1, E2], D, O):-
    atom_concat(E1, D, Temp),
    atom_concat(Temp, E2, O).
concat_atom_iio([H|T], D, O):-
    concat_atom_iio(T, D, Ts),
    atom_concat(H, D, Temp),
    atom_concat(Temp, Ts, O).


% copy_term_spec(+Term, -Term)
%
%   Special version of copy_term. variable x represented as ?(x)
%   All occurs of ?(x) are replaced with real prolog variables.
%   Modified version of code published by Bartak: http://kti.mff.cuni.cz/~bartak/prolog/data_struct.html
%
copy_term_spec(A,B):-
    cp(A,[],B,_).

cp(A,Vars,A,Vars):-
    atomic(A), A\= ?(_).
cp(?(V),Vars,NV,NVars):-
    atomic(V),
    register_var(V,Vars,NV,NVars).
cp(V,Vars,NV,NVars):-
    var(V),
    register_var(V,Vars,NV,NVars).
cp(Term,Vars,NTerm,NVars):-
    compound(Term),
    Term \= ?(_),
    Term=..[F|Args],    % decompose term
    cp_args(Args,Vars,NArgs,NVars),
    NTerm=..[F|NArgs].  % construct copy term

cp_args([H|T],Vars,[NH|NT],NVars):-
    cp(H,Vars,NH,SVars),
cp_args(T,SVars,NT,NVars).
cp_args([],Vars,[],Vars).


% register_var(?, ?, ?)
%
%   During copying one has to remeber copies of variables which can be used further during copying.
%   Therefore the register of variable copies is maintained.
%
register_var(V,[X/H|T],N,[X/H|NT]):-
    V\==X,         % different variables
    register_var(V,T,N,NT).
register_var(V,[X/H|T],H,[X/H|T]):-
    V==X.          % same variables
register_var(V,[],N,[V/N]).


% minOfList(+List, -MaxiamlItem)
%
%   Find minimum value of the list
%
minOfList([X|Xs], Min):-
    minOfList(Xs, X, Min).
minOfList([], Min, Min).
minOfList([X|Xs], Min0, Min):-
    ( X @< Min0 ->
        Min1 = X
        ;
        Min1 = Min0
    ),
    minOfList(Xs, Min1, Min).


reset_statistic:-
    bb_put(stat_nodes, 0),
    statistics(runtime, [T,_]),
    bb_put(startTime, T).

show_statistic:-
    bb_get(stat_nodes, N),
    bb_get(startTime, T0),
    statistics(runtime, [T1,_]),
    statistics(memory, [M, _]),
    T is T1-T0,
    format('~3d sec      ~d nodes        ~d bytes~n', [T, N, M]).


% show_statistic(+Problem, +Solution).
%
show_statistic(P, S):-
    ground(S),
    get_problem_name(P, Name),
    bb_get(stat_nodes, N),
    bb_get(startTime, T0),
    statistics(runtime, [T1,_]),
    statistics(memory, [M, _]),
    T is T1-T0,
    length(S, L),
    format('~a ~3d ~d ~d ~d', [Name,T, N, M, L]),
    solution_to_lisp(S),
    nl,
    !.
show_statistic(_, _).

solution_to_lisp([]).
solution_to_lisp([H|T]):-
    H =.. [F|P],
    write(' ('),
    write(F),
    write_list(P),
    write(')'),
    solution_to_lisp(T).

write_list([]).
write_list([H|T]):-
    write(' '), write(H),
    write_list(T).


stat_node:-
    bb_get(stat_nodes, N),
    NN is N+1,
    bb_update(stat_nodes, _, NN).



space(0):-!.
space(I):-
    write('  '),
    NI is I-1,
    space(NI).

writel([]):-nl.
writel([H|T]):-
    write(H),nl,
    writel(T).

w(X):-
    var(X),
    domain(X, D, F),
    !,
    write(X=D-F).
w(X):-
    var(X),
    !,
    write(X).

w(X):-
    atomic(X),
    !,
    write(X).
w([H|T]):-
    write('['), 
    !,
    w_list([H|T]),
    write(']').
w(X):-
    compound(X),
    !,
    X=..[F|L],
    write(F),write('('),
    w_params(L),
    write(')').
w_params([H]):-
    w(H).
w_params([H,H2|T]):-
    w(H),write(','),
    w_params([H2|T]).
w_list([H]):-
    w(H), 
    !.
w_list([H|T]):-
    w(H),
    write(','),
    w_list(T).


% state_record(State, PreviousState, Action, Deep, StateRecord)
%
state_record(S, PS, A, D, [S, PS, A, D]).


% solution(+StateRecord, +Visited, -ListOfActions)
%
solution(SR, V, L):-
    solution(SR, V, [], L).
solution(SR, _, L, L):-
    state_record(_, nil, nil, _, SR), 
    !.
solution(SR, V, R, L):-
    state_record(_, PS, AD, _, SR),
    state_record(PS, _, _, _, Previous),
    member(Previous, V),
    solution(Previous, V, [AD|R], L).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handling mutexes

 make_mutex(M):-
    bagof(R1, forbiden_pair(R1), MA),
    bagof(R2, forbiden_pair(MA, R2), MB),
%    writel(MA),nl,
%    writel(MB),nl,
    union(MA, MB, M0),
%    list_to_set(M0_, M0),
%    write('Cistim:'),nl,
    clear_mutex1(M0, M1),
    clear_mutex2(M1, M2),
    clear_duplicates(M2, M).
    %write('Ocistene:'),nl,writel(M),nl, length(M, L), write('Pocet: '), write(L),nl.

clear_duplicates([], []).
clear_duplicates([H|T], R):-
    member(M, T),
    identical_but_for_variables(H, M),
    !,
    clear_duplicates(T, R).
clear_duplicates([H|T], [H|R]):-
    clear_duplicates(T, R).

forbiden_pair(R):-
    get_action(A),
    get_positiv_effect(A, PE),
    get_negativ_effect(A, NE),
    member(P, PE),
    member(Q, NE),
    copy_term_spec(P-Q, R).
forbiden_pair(MA, NR):-
    member(P-Q, MA),
    get_action(A),
    get_precondition(A, Precond),
    get_positiv_effect(A, PE),
    member(R, Precond),
    member(P, PE),
    copy_term_spec(R-Q, NR).

clear_mutex1([], []):-!.
clear_mutex1([PP-QQ|T], M):-
    (P-Q = PP-QQ
     ;
     P-Q = QQ-PP
    ),
    get_init(I),
    select(P, I, R),
    member(Q, R),
%    write('Rule1: '), write(PP-QQ),nl,
    clear_mutex1(T, M),
    !.
clear_mutex1([P-Q|R], [P-Q|M]):-
    clear_mutex1(R, M).

clear_mutex2(M0, M):-
    (select(P-Q, M0, R) 
     ;
     select(Q-P, M0, R)
    ),
    get_action(A, _Def),
    get_precondition(A, Precond),
    get_positiv_effect(A, PE),
    get_negativ_effect(A, NE),
    select(P, PE, RPE),
    \+ member(Q, NE),
    (
      member(Q, RPE)%, write('prva cast')
      ;
      all_not_in(Precond, P, Q, M0)%, write('druha cast')
    ),
%    write('Rule2: '), write(P-Q-_Def),nl,
    clear_mutex2(R, M),
    !.
clear_mutex2(M0, M0).

all_not_in([], _, _, _).
all_not_in([P|T], P, Q, M):-
    all_not_in(T, P, Q, M).
all_not_in([R|T], P, Q, M):-
    \+ (member(R-Q, M)
        ;
        member(Q-R, M)
       ),
    %write(precon-R),nl,
    all_not_in(T, P, Q, M).


% check_mutex(+State).
%
check_mutex(S):-
    bb_get(mutex, M),
    pairfrom(S, P, Q, _),
    (member(P-Q, M)
     ;
     member(Q-P, M)
    ),
%    write('Mutex pair.'), write(P-Q), nl,
    !,
    fail.
check_mutex(_).


identical_but_for_variables(X, Y) :-
    \+ \+ (
           copy_term(X, Z),
           numbervars(Z, 0, N),
           numbervars(Y, 0, N),
           Z = Y
          ).

% FILENAME:  domain.pl 
% :- module(pdd_domain, [pdd_domain/2, pdd_domain/3, pddl_not_equal/2, pddl_bind/1]).

:- expects_dialect(sicstus).

:- use_module(library(when)).
% :- use_module(library(atts)).
:- use_module(library(ordsets), [
        ord_intersection/3,
        ord_intersect/2,
        ord_union/3,
        ord_union/2,
        %ord_member/2,
        %ord_nonmember/2,
        ord_add_element/3,
        ord_del_element/3,
        list_to_ord_set/2
   ]).

% :- use_module(library(sets), [ del_element/3 ]).

sicstus_get_atts(V,As):- get_attrs(V,As).
sicstus_put_atts(V,As):- put_attrs(V,As).
%:- attribute(pddl_dom/2).
%:- attribute forbidden/1.

domain3:verify_attributes(Var, Other, Goals) :-
    sicstus_get_atts(Var, pddl_dom(Da, Fa)),             % are we involved?
    !,
    (   var(Other) ->                       % must be attributed then
        (   sicstus_get_atts(Other, pddl_dom(Db, Fb)) -> %   has a pdd_domain?
            (my_del_element2(Var, Fb, _) -> 
                 !, 
                 fail
                 ;
                 true
            ),
            ord_intersection(Da, Db, Dc),
            ord_union(Fa, Fb, Fc),
            Dc = [El|Els],              % at least one element
            (   Els = [] ->             % exactly one element
                Goals = [Other=El]      % implied binding
                ;   
                Goals = [],
                sicstus_put_atts(Other, pddl_dom(Dc, Fc))% rescue intersection
            )
            ;
            Goals = [],
            sicstus_put_atts(Other, pddl_dom(Da, Fa))    % rescue the pdd_domain
        )
        ;
        Goals = [],
        ord_intersect([Other], Da),      % value in pdd_domain?
        delete_from(Fa, Var, Other),
        sicstus_put_atts(Var, pddl_dom([Other], Fa)),
%        my_del_element(Var, Fa, NewFa),
        bind_all(Fa)
    ).
domain3:verify_attributes(_, _, []).                % unification triggered
                                            % because of attributes
                                            % in other modules

attribute_goal(Var, pdd_domain(Var,Dom, F)) :-     % interpretation as goal
    sicstus_get_atts(Var, pddl_dom(Dom, F)).

pdd_domain(X, Dom) :-
    pdd_domain(X, Dom, _).
pdd_domain(X, List) :-
    list_to_ord_set(List, Set),
    Set = [El|Els],                     % at least one element
    (   Els = [] ->                     % exactly one element
        X = El                          % implied binding
        ;
        sicstus_put_atts(Fresh, pddl_dom(Set, [])),
        X = Fresh                       % may call
                                        % domain3:verify_attributes/3
    ).

pdd_domain(X, Dom, F) :-
    var(Dom),
    !,
    sicstus_get_atts(X, pddl_dom(Dom, F)).

delete_from([], _, _).
delete_from([A|T], V, Value):-
    (A==V ->
        true
        ;
        sicstus_get_atts(A, pddl_dom(Ad, Af)),
        my_del_element(Value, Ad, NewAd),
        my_del_element(V, Af, NewAf),
        sicstus_put_atts(A, pddl_dom(NewAd, NewAf))
    ),
    delete_from(T, V, Value).

my_del_element(_, [], []).
my_del_element(E, [H|T], R):-
    E==H,
    !,
    my_del_element(E, T, R).
my_del_element(E, [H|T], [H|R]):-
    my_del_element(E, T, R).

my_del_element2(E, [H|T], R):-
    E==H,
    !,
    my_del_element(E, T, R).
my_del_element2(E, [H|T], [H|R]):-
    my_del_element2(E, T, R).


pddl_not_equal([], []).
pddl_not_equal(A, B):-
    ground(A), ground(B),
    !,
    A\=B.

pddl_not_equal(A, B):-
    ground(A),
    !,
    pddl_not_equal(B, A).
pddl_not_equal(A, B):-
    var(A), ground(B),
    !,
    sicstus_get_atts(A, pddl_dom(Ad, Fa)),
    ord_del_element(Ad, B, NewAd),
    sicstus_put_atts(A, pddl_dom(NewAd, Fa)),
    pddl_bind(Fa).
pddl_not_equal(A, B):-
    A==B,
    !,
    fail.
pddl_not_equal(A, B):-
    var(A), var(B),
    sicstus_get_atts(A, pddl_dom(Da, Fa)),
    sicstus_get_atts(B, pddl_dom(Db, Fb)),
%    ord_union([Fa,Fb,[A,B]], F),
    ord_union([[B],Fa], Faa),
    ord_union([[A],Fb], Fbb),
    sicstus_put_atts(A, pddl_dom(Da, Faa)),
    sicstus_put_atts(B, pddl_dom(Db, Fbb)),
    ord_union([[A],Fa], Faaaa),
    ord_union([[B],Fb], Fbbbb),
    pddl_bind(Faaaa),
    pddl_bind(Fbbbb).
pddl_not_equal([Ha|Ta], [Hb|Tb]):-
    !,
    pddl_not_equal(Ha, Hb),
    pddl_not_equal(Ta, Tb).
pddl_not_equal(A, B):-
    compound(A), compound(B),
    !,
    A =.. [Fa|Pa], B=..[Fb|Pb],
    (Fa=Fb ->
        pddl_not_equal(Pa, Pb)
        ;
        true
    ).


set_forbidden([], _).
set_forbidden([H|T], F):-
    sicstus_get_atts(H, pddl_dom(D, _)),
    sicstus_put_atts(H, pddl_dom(D, F)),
%    write(H-D-F),nl,
    set_forbidden(T, F).

bind_all([]).
bind_all([H|T]):-
  (var(H) ->
       pddl_bind([H])
       ;
       true
  ),
  bind_all(T).

pddl_bind(F):-
    setof(B, solvable(F, [], B), Bs),
    rotate(Bs, RB),
    bind_value(F, RB).

bind_value([], _).
bind_value([H|T], [B|Bs]):-
    (var(H) ->
        list_to_ord_set(B, OB),
        (OB=[VB] ->
            H=VB
            ;
            sicstus_get_atts(H, pddl_dom(_, Hf)),
            sicstus_put_atts(H, pddl_dom(OB, Hf))
        )
        ;
        true
   ),
   bind_value(T, Bs).

rotate([[]|_], []).
rotate(S, [F|Fs]):-
    first(S, F, R),
    rotate(R, Fs).

first([], [], []).
first([[F|T]|T2], [F|Fs], [T|R]):-
    first(T2, Fs, R).

solvable([], _, []).
solvable([H|T], FV, [M|S]):-
    (var(H) ->
        sicstus_get_atts(H, pddl_dom(Hd, _)),
        member(M, Hd),
        ordsets:ord_nonmember(M, FV),
        ord_add_element(FV, M, NewFV)
        ;
        NewFV=FV
    ),
    solvable(T, NewFV, S).

% FILENAME:  forward.pl 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This file must implements following predicates:
%%
%% step(+State, -ActionDef, -NewState)
%%   Return descendant of State and ActionDefinition that was used.
%%
%% is_goal(State) - is true when State is a goal state.  
%%
%% repeating(Goal1, Goal2):-  Goal1 is the same as Goal2.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- expects_dialect(sicstus).
:-use_module(library(ordsets)).

make_init_state(I):-
    get_init(I),
    get_goal(G),
    bb_put(fictiveGoal, G).


make_solution(S, S).
    
% step(+State, -ActionDef, -NewState)
%
%   Return descendant of State and ActionDefinition that was used.
%
step(State, ActionDef, NewState):-
    get_action(A, ActionDef),
    get_precondition(A, P),    mysubset(P, State),  % choose suitable action
    get_negativ_effect(A, NE), ord_subtract(State, NE, State2),
    get_positiv_effect(A, PE), ord_union(State2, PE, NewState).

is_goal(S):-
    get_goal(G),
    ord_subset(G, S).

repeating(S1, S2):-
    S1 =  S2.

% FILENAME:  forward-wa-star-h_add.pl 
%:-[readFile, parseProblem, parseDomain, common].
%:-['wa-star', forward, h_add].



% FILENAME:  forward-wa-star-h_diff.pl 
%:-[readFile, parseProblem, parseDomain, common].
%:-['wa-star', forward, 'h_diff'].

% % :-command_line.

% FILENAME:  h_add.pl 

:- expects_dialect(sicstus).

% :-use_module(library(sets)).

% h(+State, -EstimatedValue)
h(S, H):-h_add(S, H).
% h(S, H):-h_diff(S, H).


%
%   Estimated distance to achieve Goal.
%
h_add(S, E):-
    bb_get(fictiveGoal, G),
    relax(S, G, E).
%    write(G-S-E),nl.

relax(_, [], 0):-!.
relax(S, G, E):-
    subtract(G, S, Delta),
    setof(P, relax_step(S, P), RS),
    ord_union([S|RS], NS),
    relax(NS, Delta, NE),
    length(Delta, LD),
    E is LD+NE.

relax_step(State, PE):-
    get_action(A),
    get_precondition(A, P),
    mysubset(P, State),
    get_positiv_effect(A, PE).

% FILENAME:  h_diff.pl 
% h(+State, -EstimatedValue)
%
%   Estimated distance to achive Goal.
%
:- expects_dialect(sicstus).

h_diff(S, E):-
    bb_get(fictiveGoal, G),
    ord_subtract(G, S, I),
    length(I, E).
 
init_heuristics(_).

% FILENAME:  parseDomain.pl 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parseDomain.pl
%%   Simple parser of PDDL domain file into prolog syntax.
%% Author: Robert Sasak, Charles University in Prague
%%
%% Example: 
%% ?-parseDomain('blocks_world.pddl', O).
%%   O = domain(blocks,
%%        [strips, typing, 'action-costs'],
%%        [block],
%%        _G4108,
%%        [ on(block(?x), block(?y)),
%%               ontable(block(?x)),
%%               clear(block(?x)),
%%               handempty,
%%               holding(block(?x)) ],
%%        [number(f('total-cost', []))],
%%        _G4108,
%%        [ action('pick-up', [block(?x)],       %parameters
%%                    [clear(?x), ontable(?x), handempty], %preconditions
%%                    [holding(?x)],                       %positiv effects
%%          [ontable(?x), clear(?x), handempty], %negativ effects
%%          [increase('total-cost', 2)]),        %numeric effects
%%         ...],
%%       ...)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- expects_dialect(sicstus).


% parseDomain(+File, -Output).
%
%   Parse PDDL domain File and return it rewritten prolog syntax.   
%
parseDomain(F, O):- parseDomain(F, O, _).


% parseDomain(+File, -Output, -RestOfFile)
%
%   The same as above and also return rest of file. Can be useful when domain and problem are in one file.
%
parseDomain(File, Output, R) :-
    read_file(File, List),
    domainBNF(Output, List, R).


% Support for reading file as a list.
% :- [readFile].


% Defining operator ?. It is a syntax sugar for marking variables: ?x
:- op(300, fy, ?).


% domainBNF(domain(N, R, T, C, P, F, C, S))
%
%   DCG rules describing structure of domain file in language PDDL.
%   BNF description was obtain from http://www.cs.yale.edu/homes/dvm/papers/pddl-bnf.pdf
%   This parser do not fully NOT support PDDL 3.0
%   However you will find comment out lines ready for futher development.
%
domainBNF(domain(N, R, T, C, P, F, C, S))
                        --> ['(','define', '(','domain'], name(N), [')'],
                             (require_def(R)    ; []),
                             (types_def(T)      ; []), %:typing
                             (constants_def(C)  ; []),
                             (predicates_def(P) ; []),
                             (functions_def(F)  ; []), %:fluents
%                             (constraints(C)   ; []),    %:constraints
                             zeroOrMore(structure_def, S),
                             [')'].

require_def(R)          --> ['(',':','requirements'], oneOrMore(require_key, R), [')'].
require_key(strips)                             --> [':strips'].
require_key(typing)                             --> [':typing'].
require_key('action-costs')                             --> [':action-costs'].
require_key('goal-utilities')                             --> [':goal-utilities'].
%require_key('negative-preconditions')          --> [':negative-preconditions'].
%require_key('disjunctive-preconditions')       --> [':disjunctive-preconditions'].
require_key(equality)                           --> [':equality'].
require_key('existential-preconditions')        --> [':existential-preconditions'].
require_key('universal-preconditions')          --> [':universal-preconditions'].
require_key('quantified-preconditions')         --> [':quantified-preconditions'].
require_key('conditional-effects')              --> [':conditional-effects'].
require_key(fluents)                            --> [':fluents'].
require_key(adl)                                --> [':adl'].
require_key('durative-actions')                 --> [':durative-actions'].
require_key('derived-predicates')               --> [':derived-predicates'].
require_key('timed-initial-literals')           --> [':timed-initial-literals'].
require_key(preferences)                        --> [':preferences'].
require_key(constraints)                        --> [':constraints'].
% Universal requirements
require_key(R)                  --> [':', R].

types_def(L)                    --> ['(',':',types],      typed_list(name, L), [')'].
constants_def(L)                --> ['(',':',constants],  typed_list(name, L), [')'].
predicates_def(P)               --> ['(',':',predicates], oneOrMore(atomic_formula_skeleton, P), [')'].

atomic_formula_skeleton(F)
                                --> ['('], predicate(P), typed_list(variable, L), [')'], {F =.. [P|L]}.
predicate(P)                    --> name(P).

variable(V)                     --> ['?'], name(N), {V =.. [?, N]}.
atomic_function_skeleton(f(S, L))
                                --> ['('], function_symbol(S), typed_list(variable, L), [')'].
function_symbol(S)              --> name(S).
functions_def(F)                --> ['(',':',functions], function_typed_list(atomic_function_skeleton, F), [')'].              %:fluents
%constraints(C)                 --> ['(',':',constraints], con_GD(C), [')'].                                                   %:constraints
structure_def(A)                --> action_def(A).
%structure_def(D)               --> durative_action_def(D).                                                                    %:durativeactions
%structure_def(D)               --> derived_def(D).                                                                            %:derivedpredicates
%typed_list(W, G)               --> oneOrMore(W, N), ['-'], type(T), {G =.. [T, N]}.
typed_list(W, [G|Ns])           --> oneOrMore(W, N), ['-'], type(T), !, typed_list(W, Ns), {G =.. [T,N]}.
typed_list(W, N)                --> zeroOrMore(W, N).

primitive_type(N)               --> name(N).
type(either(PT))                --> ['(',either], !, oneOrMore(primitive_type, PT), [')'].
type(PT)                        --> primitive_type(PT).
function_typed_list(W, [F|Ls])
                                --> oneOrMore(W, L), ['-'], !, function_type(T), function_typed_list(W, Ls), {F =.. [T|L]}.    %:typing
function_typed_list(W, L)       --> zeroOrMore(W, L).

function_type(number)           --> [number].
emptyOr(_)                      --> ['(',')'].
emptyOr(W)                      --> W.

% Actions definitons
action_def(action(S, L, Precon, Pos, Neg, Assign))
                                --> ['(',':',action], action_symbol(S),
                                    [':',parameters,'('], typed_list(variable, L), [')'],
                                    action_def_body(Precon, Pos, Neg, Assign),
                                    [')'].
action_symbol(N)                --> name(N).
action_def_body(P, Pos, Neg, Assign)
                                --> (([':',precondition], emptyOr(pre_GD(P)))                ; []),
                                    (([':',effect],       emptyOr(effect(Pos, Neg, Assign))) ; []).
pre_GD([F])                     --> atomic_formula(term, F), !.
pre_GD(P)                       --> pref_GD(P).
pre_GD(P)                       --> ['(',and], pre_GD(P), [')'].
%pre_GD(forall(L, P))           --> ['(',forall,'('], typed_list(variable, L), [')'], pre_GD(P), [')'].         %:universal-preconditions
%pref_GD(preference(N, P))      --> ['(',preference], (pref_name(N); []), gd(P), [')'].                         %:preferences
pref_GD(P)                      --> gd(P).
pref_name(N)                    --> name(N).
gd(F)                           --> atomic_formula(term, F).                                                    %:this option is covered by gd(L)
%gd(L)                          --> literal(term, L).                                                           %:negative-preconditions
gd(P)                           --> ['(',and],  zeroOrMore(gd, P), [')'].
%gd(or(P))                      --> ['(',or],   zeroOrMore(gd ,P), [')'].                                       %:disjuctive-preconditions
%gd(not(P))                     --> ['(',not],  gd(P), [')'].                                                   %:disjuctive-preconditions
%gd(imply(P1, P2))              --> ['(',imply], gd(P1), gd(P2), [')'].                                         %:disjuctive-preconditions
%gd(exists(L, P))               --> ['(',exists,'('], typed_list(variable, L), [')'], gd(P), [')'].             %:existential-preconditions
%gd(forall(L, P))               --> ['(',forall,'('], typed_list(variable, L), [')'], gd(P), [')'].             %:universal-preconditions
gd(F)                           --> f_comp(F).                                                                  %:fluents
f_comp(compare(C, E1, E2))      --> ['('], binary_comp(C), f_exp(E1), f_exp(E2), [')'].
literal(T, F)                   --> atomic_formula(T, F).
literal(T, not(F))              --> ['(',not], atomic_formula(T, F), [')'].
atomic_formula(_, F)            --> ['('], predicate(P), zeroOrMore(term, T), [')'], {F =.. [P|T]}.             % cheating, maybe wrong


term(N)                         --> name(N).
term(V)                         --> variable(V).
f_exp(N)                        --> number(N).
f_exp(op(O, E1, E2))            --> ['('],binary_op(O), f_exp(E1), f_exp(E2), [')'].
f_exp('-'(E))                   --> ['(','-'], f_exp(E), [')'].
f_exp(H)                        --> f_head(H).
f_head(F)                       --> ['('], function_symbol(S), zeroOrMore(term, T), [')'], { F =.. [S|T] }.
f_head(S)                       --> function_symbol(S).
binary_op(O)                    --> multi_op(O).
binary_op(45)                   --> [45]. % 45 = minus = '-' 
binary_op('/')                  --> ['/'].
multi_op('*')                   --> ['*'].
multi_op('+')                   --> ['+'].
binary_comp('>')                --> ['>'].
binary_comp('<')                --> ['<'].
binary_comp('=')                --> ['='].
binary_comp('>=')               --> ['>='].
binary_comp('<=')               --> ['<='].
number(N)                       --> [N], {integer(N)}.
number(N)                       --> [N], {float(N)}.
effect(P, N, A)                 --> ['(',and], c_effect(P, N, A), [')'].
effect(P, N, A)                 --> c_effect(P, N, A).
%c_effect(forall(E))            --> ['(',forall,'('], typed-list(variable)∗) effect(E), ')'.    %:conditional-effects
%c_effect(when(P, E))           --> ['(',when], gd(P), cond_effect(E), [')'].                   %:conditional-effects
c_effect(P, N, A)               --> p_effect(P, N, A).
p_effect([], [], [])            --> [].
p_effect(Ps, Ns, [F|As])        --> ['('], assign_op(O), f_head(H), f_exp(E), [')'], p_effect(Ps, Ns, As), {F =.. [O, H, E]}.
p_effect(Ps, [F|Ns], As)        --> ['(',not], atomic_formula(term,F), [')'], p_effect(Ps, Ns, As).
p_effect([F|Ps], Ns, As)        --> atomic_formula(term, F), p_effect(Ps, Ns, As).
%p_effect(op(O, H, E))          --> ['('], assign_op(O), f_head(H), f_exp(E), [')'].            %:fluents , What is difference between rule 3 lines above???
%cond_effect(E)                 --> ['(',and], zeroOrMore(p_effect, E), [')'].                  %:conditional-effects
%cond_effect(E)                 --> p_effect(E).                                                %:conditional-effects
assign_op(assign)               --> [assign].
assign_op(scale_up)             --> [scale_up].
assign_op(scale_down)           --> [scale_down].
assign_op(increase)             --> [increase].
assign_op(decrease)             --> [decrease].


% BNF description include operator <term>+ to mark zero or more replacements.
% This DCG extension to overcome this. 
oneOrMore(W, [R|Rs], A, C) :- F =.. [W, R, A, B], F, (
                                                         oneOrMore(W, Rs, B, C)
                                                         ;
                                                         (Rs = [] , C = B) 
                                                     ).
% BNF operator <term>*
zeroOrMore(W, R)                --> oneOrMore(W, R).
zeroOrMore(_, [])               --> [].

% Name is everything that is not number, bracket or question mark.
% Those rules are not necessary, but rapidly speed up parsing process.
name(N)                         --> [N], {integer(N), !, fail}.
name(N)                         --> [N], {float(N), !, fail}.
name(N)                         --> [N], {N=')', !, fail}.
name(N)                         --> [N], {N='(', !, fail}.
name(N)                         --> [N], {N='?', !, fail}.
name(N)                         --> [N], {N='-', !, fail}.
name(N)                         --> [N].

% FILENAME:  parseProblem.pl 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parseDomain.pl
%%   Simple parser of PDDL domain file into prolog syntax.
%% Author: Robert Sasak, Charles University in Prague
%%
%% Example: 
%% ?-parseProblem('problem.pddl', O).
%%   O = problem('blocks-4-0',                                                                  % name
%%              blocks,                                                                         % domain name
%%              _G1443,                                                                         % require definition
%%              [block(d, b, a, c)],                                                            % object declaration
%%              [ clear(c), clear(a), clear(b), clear(d), ontable(c),                           % initial state
%%                ontable(a), ontable(b), ontable(d), handempty,
%%                set('total-cost', 0) ],
%%              [on(d, c), on(c, b), on(b, a)],                                                 % goal
%%              _G1447,                                                                         % constraints-not implemented
%%              metric(minimize, 'total-cost'),                                                 % metric
%%              _G1449                                                                          % length_specification-not implemented
%%              )
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- expects_dialect(sicstus).

% parseProblem(+File, -Output).
%
%   Parse PDDL problem File and return rewritten prolog syntax. 
%
parseProblem(F, O):-parseProblem(F, O, _).


% parseProblem(+File, -Output, -RestOfFile).
%
%   The same as above and also return rest of file. Can be useful when domain and problem are in one file.
%
parseProblem(F, O, R) :-
    read_file(F, L),
    problem(O, L, R).

% Support for reading file as a list.
% :- [readFile].



% DCG rules describing structure of problem file in language PDDL.
%
%   BNF description was obtain from http://www.cs.yale.edu/homes/dvm/papers/pddl-bnf.pdf
%   This parser do not fully NOT support PDDL 3.0
%   However you will find comment out lines ready for futher development.
%   Some of the rules are already implemented in parseDomain.pl
%
% :-[parseDomain]. % make sure that it is loaded.

problem(problem(Name, Domain, R, OD, I, G, _, MS, LS))   
                                --> ['(',define,'(',problem,Name,')',
                                     '(',':',domain, Domain,')'],
                                     (require_def(R)         ; []),
                                     (object_declaration(OD) ; []),
                                     init(I),
                                     goal(G),
%                                    (constraints(C)   ; []), %:constraints
                                     (metric_spec(MS)  ; []),
                                     (length_spec(LS)  ; []),
                                     [')'].

object_declaration(L)           --> ['(',':',objects], typed_list_as_list(name, L),[')'].

typed_list_as_list(W, [G|Ns])   --> oneOrMore(W, N), ['-'], type(T), !, typed_list_as_list(W, Ns), {G =.. [T,N]}.
typed_list_as_list(W, N)        --> zeroOrMore(W, N).



init(I)                         --> ['(',':',init], zeroOrMore(init_el, I), [')'].

init_el(I)                      --> literal(name, I).
init_el(set(H,N))               --> ['(','='], f_head(H), number(N), [')'].                                     % fluents
init_el(at(N, L))               --> ['(',at], number(N), literal(name, L), [')'].                               % timed-initial literal
goal(G)                         --> ['(',':',goal], pre_GD(G),[')'].
%constraints(C)                 --> ['(',':',constraints], pref_con_GD(C), [')'].                               % constraints
pref_con_GD(and(P))             --> ['(',and], zeroOrMore(pref_con_GD, P), [')'].
%pref_con_GD(foral(L, P))       --> ['(',forall,'('], typed_list(variable, L), [')'], pref_con_GD(P), [')'].    % universal-preconditions
%pref_con_GD(prefernce(N, P))   --> ['(',preference], (pref_name(N) ; []), con_GD(P), [')'].                    % prefernces
pref_con_GD(P)                  --> con_GD(P).

con_GD(and(L))                  --> ['(',and], zeroOrMore(con_GD, L), [')'].
con_GD(forall(L, P))            --> ['(',forall,'('], typed_list(variable, L),[')'], con_GD(P), [')'].
con_GD(at_end(P))               --> ['(',at,end],   gd(P), [')'].
con_GD(always(P))               --> ['(',always],   gd(P), [')'].
con_GD(sometime(P))             --> ['(',sometime], gd(P), [')'].
con_GD(within(N, P))            --> ['(',within], number(N), gd(P), [')'].

con_GD(at_most_once(P))         --> ['(','at-most-once'], gd(P),[')'].
con_GD(some_time_after(P1, P2)) --> ['(','sometime-after'], gd(P1), gd(P2), [')'].
con_GD(some_time_before(P1, P2))--> ['(','sometime-before'], gd(P1), gd(P2), [')'].
con_GD(always_within(N, P1, P2))--> ['(','always-within'], number(N), gd(P1), gd(P2), [')'].
con_GD(hold_during(N1, N2, P))  --> ['(','hold-during'], number(N1), number(N2), gd(P), [')'].
con_GD(hold_after(N, P))        --> ['(','hold-after'], number(N), gd(P),[')'].

metric_spec(metric(O, E))       --> ['(',':',metric], optimization(O), metric_f_exp(E), [')'].

optimization(minimize)          --> [minimize].
optimization(maximize)          --> [maximize].

metric_f_exp(E)                 --> ['('], binary_op(O), metric_f_exp(E1), metric_f_exp(E2), [')'], {E =..[O, E1, E2]}.
metric_f_exp(multi_op(O,[E1|E]))--> ['('], multi_op(O), metric_f_exp(E1), oneOrMore(metric_f_exp, E), [')']. % I dont see meanful of this rule, in additional is missing in f-exp
metric_f_exp(E)                 --> ['(','-'], metric_f_exp(E1), [')'], {E=..[-, E1]}.
metric_f_exp(N)                 --> number(N).
metric_f_exp(F)                 --> ['('], function_symbol(S), zeroOrMore(name, Ns), [')'], { F=..[S|Ns]}.%concat_atom_iio([S|Ns], '-', F) }.
metric_f_exp(function(S))       --> function_symbol(S).
metric_f_exp(total_time)        --> ['total-time'].
metric_f_exp(is_violated(N))    --> ['(','is-violated'], pref_name(N), [')'].

% Work arround
metric_f_exp(is_violated(N,V))    --> ['(','*','(','is-violated'], pref_name(N), [')'],number(V),[')'].

% Work arround
length_spec([])                 --> [not_defined].      % there is no definition???


% FILENAME:  readFile.pl 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  read_file
%%%  This is a modified version for parsing pddl files.
%%%  Read the input file character by character and parse it
%%%  into a list. Brackets, comma, period and question marks
%%%  are treated as separate words. White spaces separed 
%%%  words. 
%%%
%%%  Similar to read_sent in Pereira and Shieber, Prolog and
%%%        Natural Language Analysis, CSLI, 1987.
%%%
%%%  Examples:
%%%           :- read_file('input.txt', L).
%%%           input.txt> The sky was blue, after the rain.
%%%           L = [the, sky, was, blue, (','), after, the, rain, '.']
%%%
%%%           :- read_file('domain.pddl', L).
%%%           domain.pddl>
%%%           (define (domain BLOCKS)
%%%             (:requirements :strips :typing :action-costs)
%%%             (:types block)
%%%             (:predicates (on ?x - block ?y - block)
%%%           ...
%%%           L = ['(', define, '(', domain, blocks, ')', '(', :, requirements|...].

:- expects_dialect(sicstus).

%
% read_file(+File, -List).
%
read_file(File, Words) :- seeing(Old), exists_file(File), see(File), get_code(C), read_rest(C, Words), seen, see(Old).

/* Ends the input. */
read_rest(-1,[]) :- !.

/* Spaces, tabs and newlines between words are ignored. */
read_rest(C,Words) :- ( C=32 ; C=10 ; C=9 ; C=13 ; C=92 ) , !,
                     get_code(C1),
                     read_rest(C1,Words).

/* Brackets, comma, period or question marks are treated as separed words */
read_rest(C, [Char|Words]) :- ( C=40 ; C=41 ; C=44 ; C=45 ; C=46 ; C=63 ; C=58 ) , name(Char, [C]), !,
			get_code(C1),
			read_rest(C1, Words).

/* Read comments to the end of line */
read_rest(59, Words) :- get_code(Next), !, 
			      read_comment(Next, Last),
			      read_rest(Last, Words).

/* Otherwise get all of the next word. */
read_rest(C,[Word|Words]) :- read_word(C,Chars,Next),
                             name(Word,Chars),
                             read_rest(Next,Words).

/* Space, comma, newline, period, end-of-file or question mark separate words. */
read_word(C,[],C) :- ( C=32 ; C=44 ; C=10 ; C=9 ; C=13 ;
                         C=46 ; C=63 ; C=40 ; C=41 ; C=58 ; C= -1 ) , !.

/* Otherwise, get characters and convert to lower case. */
read_word(C,[LC|Chars],Last) :- lower_case(C, LC),
				get_code(Next),
                                read_word(Next,Chars,Last).

/* Convert to lower case if necessary. */
lower_case(C,C) :- ( C <  65 ; C > 90 ) , !.
lower_case(C,LC) :- LC is C + 32.


/* Keep reading as long you dont find end-of-line or end-of-file */
read_comment(10, 10) :- !.
read_comment(-1, -1) :- !.
read_comment(_, Last) :- get_code(Next),
			 read_comment(Next, Last).

%get0(C):-get_code(C), !.

/* for reference ... 
newline(10).
comma(44).
space(32).
period(46).
question_mark(63).
*/



% FILENAME:  test_validate_input_2009.pl 
:- expects_dialect(sicstus).
% :-[parseProblem, parseDomain].

%nop(_).

%parse_file(+File).
parse_file(F,L):- parseDomain(F, L, _),!, nop(portray_clause((parseDomain:-L))),writeq(F:L),nl.
parse_file(F,L):- parseProblem(F, L, _),!,nop(portray_clause((parseProblem:-L))),writeq(F:L),nl.

parse_file(F):- parse_file(F,L),arg(2,L,List),not(member('(',List)),!.
parse_file(F):- absolute_file_name(F,A),
	write('Parsing file failed. '), write('('), write(F:A), write(')'), nl,!.


test_dir(DirIn):-
  working_directory(WAS,WAS),
  atom_concat('/devel/LogicmooDeveloperFramework/PrologMUD/packs/logicmoo_engine/prolog/logicmoo/planner/test/',DirIn,DIR),
     call_cleanup(( 
        cd(DIR),
	write('Testing ':DIR), nl,
	test_colection),
        cd(WAS)).

test:- test_dir('ipc2008-no-cybersec/seq-opt/elevators-strips/'),
      test_dir('ipc2008-no-cybersec/netben-opt/elevators-strips/'),!.

test_rest:-
	
	test_dir('ipc2008-no-cybersec/seq-opt/openstacks-strips/'),
	test_dir('ipc2008-no-cybersec/seq-opt/parcprinter-strips/'),
	test_dir('ipc2008-no-cybersec/seq-opt/pegsol-strips/'),
	test_dir('ipc2008-no-cybersec/seq-opt/scanalyzer-strips/'),
	test_dir('ipc2008-no-cybersec/seq-opt/sokoban-strips/'),
	test_dir('ipc2008-no-cybersec/seq-opt/transport-strips/'),
	test_dir('ipc2008-no-cybersec/seq-opt/woodworking-strips/'),
        
        test_dir('ipc2008-no-cybersec/seq-sat/elevators-strips/'),!,
        expand_file_name('ipc2008-no-cybersec/*/*/',O),
        forall(member(E,O),test_dir(E)).
	

test_colection:-
	parse_file('p01.pddl'),
	parse_file('p01-domain.pddl'),
	parse_file('p02.pddl'),
	parse_file('p02-domain.pddl'),
	parse_file('p03.pddl'),
	parse_file('p03-domain.pddl'),
	parse_file('p04.pddl'),
	parse_file('p04-domain.pddl'),
	parse_file('p05.pddl'),
	parse_file('p05-domain.pddl'),
	parse_file('p06.pddl'),
	parse_file('p06-domain.pddl'),
	parse_file('p07.pddl'),
	parse_file('p07-domain.pddl'),
	parse_file('p08.pddl'),
	parse_file('p08-domain.pddl'),
	parse_file('p09.pddl'),
	parse_file('p09-domain.pddl'),
	parse_file('p10.pddl'),
	parse_file('p10-domain.pddl'),
	parse_file('p11.pddl'),
	parse_file('p11-domain.pddl'),
	parse_file('p12.pddl'),
	parse_file('p12-domain.pddl'),
	parse_file('p13.pddl'),
	parse_file('p13-domain.pddl'),
	parse_file('p14.pddl'),
	parse_file('p14-domain.pddl'),
	parse_file('p15.pddl'),
	parse_file('p15-domain.pddl'),
	parse_file('p16.pddl'),
	parse_file('p16-domain.pddl'),
	parse_file('p17.pddl'),
	parse_file('p17-domain.pddl'),
	parse_file('p18.pddl'),
	parse_file('p18-domain.pddl'),
	parse_file('p19.pddl'),
	parse_file('p19-domain.pddl'),
	parse_file('p20.pddl'),
	parse_file('p20-domain.pddl'),
	parse_file('p21.pddl'),
	parse_file('p21-domain.pddl'),
	parse_file('p22.pddl'),
	parse_file('p22-domain.pddl'),
	parse_file('p23.pddl'),
	parse_file('p23-domain.pddl'),
	parse_file('p24.pddl'),
	parse_file('p24-domain.pddl'),
	parse_file('p25.pddl'),
	parse_file('p25-domain.pddl'),
	parse_file('p26.pddl'),
	parse_file('p26-domain.pddl'),
	parse_file('p27.pddl'),
	parse_file('p27-domain.pddl'),
	parse_file('p28.pddl'),
	parse_file('p28-domain.pddl'),
	parse_file('p29.pddl'),
	parse_file('p29-domain.pddl'),
	parse_file('p30.pddl'),
	parse_file('p30-domain.pddl').


% FILENAME:  wa-star.pl 
% Interface:
%
% step(+State, -NewState)
% is_goal(State)
% h(State, Value) 
% repeating(+State, +AnotherState)
:- expects_dialect(sicstus).

:-use_module(library(ordsets)).
:-use_module(library(heaps)).


% search(+InitState, +GoalState, -Solution)
%
search(I, _, Solution):-
    a_star(I, Solution, _).
    
    
% a_star(+InitState, -Actions, -Cost).
%
a_star(S, A, C):-
    state_record(S, nil, nil, 0, SR),
    list_to_heap([0-SR], PQ),
    a_star(PQ, [], A, C).


% a_star(+Queue, +Visited, -Solution, -Cost)
%
a_star(PQ, _, 'NO SOLUTION', _):-
    empty_heap(PQ),
    !.
a_star(PQ, V, Solution, C):-
    get_from_heap(PQ, C, SR, _),
    state_record(S, _, _, _, SR),
    is_goal(S),
%    write('FOUND SOLUTION'),nl,
%    state_record(S, _, _, D, SR), write(C-D), write('   '),write(S),nl,
%    writel(V),nl,halt,
    solution(SR, V, Solution).

a_star(PQ, V, Solution, C):-
    get_from_heap(PQ, _K, SR, RPQ),
    ord_add_element(V, SR, NV),
    (    bagof(K-NS, next_node(SR, PQ, NV, K, NS), NextNodes) 
         ;
         NextNodes=[]
    ),
%    state_record(S, _, _, D, SR), write(_K-D), write('   '),write(S),length(NextNodes, L), write(L),nl,
%    write(NextNodes),nl,
    add_list_to_heap(RPQ, NextNodes, NPQ),
    stat_node,
    a_star(NPQ, NV, Solution, C).


% next_node(+StateRecord, +Queue, +Visited, -EstimateDeep, -NewStateRecord)
%
next_node_wastar(SR, Q, V, E, NewSR):- trace,
    state_record(S, _, _, D, SR),
    step(S, A, NewS),
    state_record(NewS, _, _, _, Temp),
    \+ my_ord_member(NewS, V),
    heap_to_list(Q, PQL),
    \+ member(Temp, PQL),
    h(S, H),
    E is 5*H+D,
    ND is D+1,
    state_record(NewS, S, A, ND, NewSR).


% next_node(+StateRecord, +Queue, +Visited, -EstimateDeep, -NewStateRecord)
%
next_node(SR, Q, V, E, NewSR):-
    state_record(S, _, _, D, SR),
    step(S, A, NewS),
    state_record(NewS, _, _, _, Temp),
    \+ my_ord_member(NewS, V),
    heap_to_list(Q, PQL),
    \+ member(Temp, PQL),
    h(S, H),
    E is H+D,
    ND is D+1,
    state_record(NewS, S, A, ND, NewSR).


% add_list_to_heap(+OldHeap, List, NewHeap)
%
add_list_to_heap(OH, [], OH).
add_list_to_heap(OH, [K-D|T], NH):-
    add_to_heap(OH, K, D, H),
    add_list_to_heap(H, T, NH).

my_ord_member(S, [SR|_]):-
    state_record(S2, _, _, _,SR),
    repeating(S, S2),
    !.
my_ord_member(S, [_|T]):-
    my_ord_member(S, T).




/*
                    GNU GENERAL PUBLIC LICENSE
                       Version 3, 29 June 2007

 Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
 Everyone is permitted to copy and distribute verbatim copies
 of this license document, but changing it is not allowed.

                            Preamble

  The GNU General Public License is a free, copyleft license for
software and other kinds of works.

  The licenses for most software and other practical works are designed
to take away your freedom to share and change the works.  By contrast,
the GNU General Public License is intended to guarantee your freedom to
share and change all versions of a program--to make sure it remains free
software for all its users.  We, the Free Software Foundation, use the
GNU General Public License for most of our software; it applies also to
any other work released this way by its authors.  You can apply it to
your programs, too.

  When we speak of free software, we are referring to freedom, not
price.  Our General Public Licenses are designed to make sure that you
have the freedom to distribute copies of free software (and charge for
them if you wish), that you receive source code or can get it if you
want it, that you can change the software or use pieces of it in new
free programs, and that you know you can do these things.

  To protect your rights, we need to prevent others from denying you
these rights or asking you to surrender the rights.  Therefore, you have
certain responsibilities if you distribute copies of the software, or if
you modify it: responsibilities to respect the freedom of others.

  For example, if you distribute copies of such a program, whether
gratis or for a fee, you must pass on to the recipients the same
freedoms that you received.  You must make sure that they, too, receive
or can get the source code.  And you must show them these terms so they
know their rights.

  Developers that use the GNU GPL protect your rights with two steps:
(1) assert copyright on the software, and (2) offer you this License
giving you legal permission to copy, distribute and/or modify it.

  For the developers' and authors' protection, the GPL clearly explains
that there is no warranty for this free software.  For both users' and
authors' sake, the GPL requires that modified versions be marked as
changed, so that their problems will not be attributed erroneously to
authors of previous versions.

  Some devices are designed to deny users access to install or run
modified versions of the software inside them, although the manufacturer
can do so.  This is fundamentally incompatible with the aim of
protecting users' freedom to change the software.  The systematic
pattern of such abuse occurs in the area of products for individuals to
use, which is precisely where it is most unacceptable.  Therefore, we
have designed this version of the GPL to prohibit the practice for those
products.  If such problems arise substantially in other domains, we
stand ready to extend this provision to those domains in future versions
of the GPL, as needed to protect the freedom of users.

  Finally, every program is threatened constantly by software patents.
States should not allow patents to restrict development and use of
software on general-purpose computers, but in those that do, we wish to
avoid the special danger that patents applied to a free program could
make it effectively proprietary.  To prevent this, the GPL assures that
patents cannot be used to render the program non-free.

  The precise terms and conditions for copying, distribution and
modification follow.

                       TERMS AND CONDITIONS

  0. Definitions.

  "This License" refers to version 3 of the GNU General Public License.

  "Copyright" also means copyright-like laws that apply to other kinds of
works, such as semiconductor masks.

  "The Program" refers to any copyrightable work licensed under this
License.  Each licensee is addressed as "you".  "Licensees" and
"recipients" may be individuals or organizations.

  To "modify" a work means to copy from or adapt all or part of the work
in a fashion requiring copyright permission, other than the making of an
exact copy.  The resulting work is called a "modified version" of the
earlier work or a work "based on" the earlier work.

  A "covered work" means either the unmodified Program or a work based
on the Program.

  To "propagate" a work means to do anything with it that, without
permission, would make you directly or secondarily liable for
infringement under applicable copyright law, except executing it on a
computer or modifying a private copy.  Propagation includes copying,
distribution (with or without modification), making available to the
public, and in some countries other activities as well.

  To "convey" a work means any kind of propagation that enables other
parties to make or receive copies.  Mere interaction with a user through
a computer network, with no transfer of a copy, is not conveying.

  An interactive user interface displays "Appropriate Legal Notices"
to the extent that it includes a convenient and prominently visible
feature that (1) displays an appropriate copyright notice, and (2)
tells the user that there is no warranty for the work (except to the
extent that warranties are provided), that licensees may convey the
work under this License, and how to view a copy of this License.  If
the interface presents a list of user commands or options, such as a
menu, a prominent item in the list meets this criterion.

  1. Source Code.

  The "source code" for a work means the preferred form of the work
for making modifications to it.  "Object code" means any non-source
form of a work.

  A "Standard Interface" means an interface that either is an official
standard defined by a recognized standards body, or, in the case of
interfaces specified for a particular programming language, one that
is widely used among developers working in that language.

  The "System Libraries" of an executable work include anything, other
than the work as a whole, that (a) is included in the normal form of
packaging a Major Component, but which is not part of that Major
Component, and (b) serves only to enable use of the work with that
Major Component, or to implement a Standard Interface for which an
implementation is available to the public in source code form.  A
"Major Component", in this context, means a major essential component
(kernel, window system, and so on) of the specific operating system
(if any) on which the executable work runs, or a compiler used to
produce the work, or an object code interpreter used to run it.

  The "Corresponding Source" for a work in object code form means all
the source code needed to generate, install, and (for an executable
work) run the object code and to modify the work, including scripts to
control those activities.  However, it does not include the work's
System Libraries, or general-purpose tools or generally available free
programs which are used unmodified in performing those activities but
which are not part of the work.  For example, Corresponding Source
includes interface definition files associated with source files for
the work, and the source code for shared libraries and dynamically
linked subprograms that the work is specifically designed to require,
such as by intimate data communication or control flow between those
subprograms and other parts of the work.

  The Corresponding Source need not include anything that users
can regenerate automatically from other parts of the Corresponding
Source.

  The Corresponding Source for a work in source code form is that
same work.

  2. Basic Permissions.

  All rights granted under this License are granted for the term of
copyright on the Program, and are irrevocable provided the stated
conditions are met.  This License explicitly affirms your unlimited
permission to run the unmodified Program.  The output from running a
covered work is covered by this License only if the output, given its
content, constitutes a covered work.  This License acknowledges your
rights of fair use or other equivalent, as provided by copyright law.

  You may make, run and propagate covered works that you do not
convey, without conditions so long as your license otherwise remains
in force.  You may convey covered works to others for the sole purpose
of having them make modifications exclusively for you, or provide you
with facilities for running those works, provided that you comply with
the terms of this License in conveying all material for which you do
not control copyright.  Those thus making or running the covered works
for you must do so exclusively on your behalf, under your direction
and control, on terms that prohibit them from making any copies of
your copyrighted material outside their relationship with you.

  Conveying under any other circumstances is permitted solely under
the conditions stated below.  Sublicensing is not allowed; section 10
makes it unnecessary.

  3. Protecting Users' Legal Rights From Anti-Circumvention Law.

  No covered work shall be deemed part of an effective technological
measure under any applicable law fulfilling obligations under article
11 of the WIPO copyright treaty adopted on 20 December 1996, or
similar laws prohibiting or restricting circumvention of such
measures.

  When you convey a covered work, you waive any legal power to forbid
circumvention of technological measures to the extent such circumvention
is effected by exercising rights under this License with respect to
the covered work, and you disclaim any intention to limit operation or
modification of the work as a means of enforcing, against the work's
users, your or third parties' legal rights to forbid circumvention of
technological measures.

  4. Conveying Verbatim Copies.

  You may convey verbatim copies of the Program's source code as you
receive it, in any medium, provided that you conspicuously and
appropriately publish on each copy an appropriate copyright notice;
keep intact all notices stating that this License and any
non-permissive terms added in accord with section 7 apply to the code;
keep intact all notices of the absence of any warranty; and give all
recipients a copy of this License along with the Program.

  You may charge any price or no price for each copy that you convey,
and you may offer support or warranty protection for a fee.

  5. Conveying Modified Source Versions.

  You may convey a work based on the Program, or the modifications to
produce it from the Program, in the form of source code under the
terms of section 4, provided that you also meet all of these conditions:

    a) The work must carry prominent notices stating that you modified
    it, and giving a relevant date.

    b) The work must carry prominent notices stating that it is
    released under this License and any conditions added under section
    7.  This requirement modifies the requirement in section 4 to
    "keep intact all notices".

    c) You must license the entire work, as a whole, under this
    License to anyone who comes into possession of a copy.  This
    License will therefore apply, along with any applicable section 7
    additional terms, to the whole of the work, and all its parts,
    regardless of how they are packaged.  This License gives no
    permission to license the work in any other way, but it does not
    invalidate such permission if you have separately received it.

    d) If the work has interactive user interfaces, each must display
    Appropriate Legal Notices; however, if the Program has interactive
    interfaces that do not display Appropriate Legal Notices, your
    work need not make them do so.

  A compilation of a covered work with other separate and independent
works, which are not by their nature extensions of the covered work,
and which are not combined with it such as to form a larger program,
in or on a volume of a storage or distribution medium, is called an
"aggregate" if the compilation and its resulting copyright are not
used to limit the access or legal rights of the compilation's users
beyond what the individual works permit.  Inclusion of a covered work
in an aggregate does not cause this License to apply to the other
parts of the aggregate.

  6. Conveying Non-Source Forms.

  You may convey a covered work in object code form under the terms
of sections 4 and 5, provided that you also convey the
machine-readable Corresponding Source under the terms of this License,
in one of these ways:

    a) Convey the object code in, or embodied in, a physical product
    (including a physical distribution medium), accompanied by the
    Corresponding Source fixed on a durable physical medium
    customarily used for software interchange.

    b) Convey the object code in, or embodied in, a physical product
    (including a physical distribution medium), accompanied by a
    written offer, valid for at least three years and valid for as
    long as you offer spare parts or customer support for that product
    model, to give anyone who possesses the object code either (1) a
    copy of the Corresponding Source for all the software in the
    product that is covered by this License, on a durable physical
    medium customarily used for software interchange, for a price no
    more than your reasonable cost of physically performing this
    conveying of source, or (2) access to copy the
    Corresponding Source from a network server at no charge.

    c) Convey individual copies of the object code with a copy of the
    written offer to provide the Corresponding Source.  This
    alternative is allowed only occasionally and noncommercially, and
    only if you received the object code with such an offer, in accord
    with subsection 6b.

    d) Convey the object code by offering access from a designated
    place (gratis or for a charge), and offer equivalent access to the
    Corresponding Source in the same way through the same place at no
    further charge.  You need not require recipients to copy the
    Corresponding Source along with the object code.  If the place to
    copy the object code is a network server, the Corresponding Source
    may be on a different server (operated by you or a third party)
    that supports equivalent copying facilities, provided you maintain
    clear directions next to the object code saying where to find the
    Corresponding Source.  Regardless of what server hosts the
    Corresponding Source, you remain obligated to ensure that it is
    available for as long as needed to satisfy these requirements.

    e) Convey the object code using peer-to-peer transmission, provided
    you inform other peers where the object code and Corresponding
    Source of the work are being offered to the general public at no
    charge under subsection 6d.

  A separable portion of the object code, whose source code is excluded
from the Corresponding Source as a System Library, need not be
included in conveying the object code work.

  A "User Product" is either (1) a "consumer product", which means any
tangible personal property which is normally used for personal, family,
or household purposes, or (2) anything designed or sold for incorporation
into a dwelling.  In determining whether a product is a consumer product,
doubtful cases shall be resolved in favor of coverage.  For a particular
product received by a particular user, "normally used" refers to a
typical or common use of that class of product, regardless of the status
of the particular user or of the way in which the particular user
actually uses, or expects or is expected to use, the product.  A product
is a consumer product regardless of whether the product has substantial
commercial, industrial or non-consumer uses, unless such uses represent
the only significant mode of use of the product.

  "Installation Information" for a User Product means any methods,
procedures, authorization keys, or other information required to install
and execute modified versions of a covered work in that User Product from
a modified version of its Corresponding Source.  The information must
suffice to ensure that the continued functioning of the modified object
code is in no case prevented or interfered with solely because
modification has been made.

  If you convey an object code work under this section in, or with, or
specifically for use in, a User Product, and the conveying occurs as
part of a transaction in which the right of possession and use of the
User Product is transferred to the recipient in perpetuity or for a
fixed term (regardless of how the transaction is characterized), the
Corresponding Source conveyed under this section must be accompanied
by the Installation Information.  But this requirement does not apply
if neither you nor any third party retains the ability to install
modified object code on the User Product (for example, the work has
been installed in ROM).

  The requirement to provide Installation Information does not include a
requirement to continue to provide support service, warranty, or updates
for a work that has been modified or installed by the recipient, or for
the User Product in which it has been modified or installed.  Access to a
network may be denied when the modification itself materially and
adversely affects the operation of the network or violates the rules and
protocols for communication across the network.

  Corresponding Source conveyed, and Installation Information provided,
in accord with this section must be in a format that is publicly
documented (and with an implementation available to the public in
source code form), and must require no special password or key for
unpacking, reading or copying.

  7. Additional Terms.

  "Additional permissions" are terms that supplement the terms of this
License by making exceptions from one or more of its conditions.
Additional permissions that are applicable to the entire Program shall
be treated as though they were included in this License, to the extent
that they are valid under applicable law.  If additional permissions
apply only to part of the Program, that part may be used separately
under those permissions, but the entire Program remains governed by
this License without regard to the additional permissions.

  When you convey a copy of a covered work, you may at your option
remove any additional permissions from that copy, or from any part of
it.  (Additional permissions may be written to require their own
removal in certain cases when you modify the work.)  You may place
additional permissions on material, added by you to a covered work,
for which you have or can give appropriate copyright permission.

  Notwithstanding any other provision of this License, for material you
add to a covered work, you may (if authorized by the copyright holders of
that material) supplement the terms of this License with terms:

    a) Disclaiming warranty or limiting liability differently from the
    terms of sections 15 and 16 of this License; or

    b) Requiring preservation of specified reasonable legal notices or
    author attributions in that material or in the Appropriate Legal
    Notices displayed by works containing it; or

    c) Prohibiting misrepresentation of the origin of that material, or
    requiring that modified versions of such material be marked in
    reasonable ways as different from the original version; or

    d) Limiting the use for publicity purposes of names of licensors or
    authors of the material; or

    e) Declining to grant rights under trademark law for use of some
    trade names, trademarks, or service marks; or

    f) Requiring indemnification of licensors and authors of that
    material by anyone who conveys the material (or modified versions of
    it) with contractual assumptions of liability to the recipient, for
    any liability that these contractual assumptions directly impose on
    those licensors and authors.

  All other non-permissive additional terms are considered "further
restrictions" within the meaning of section 10.  If the Program as you
received it, or any part of it, contains a notice stating that it is
governed by this License along with a term that is a further
restriction, you may remove that term.  If a license document contains
a further restriction but permits relicensing or conveying under this
License, you may add to a covered work material governed by the terms
of that license document, provided that the further restriction does
not survive such relicensing or conveying.

  If you add terms to a covered work in accord with this section, you
must place, in the relevant source files, a statement of the
additional terms that apply to those files, or a notice indicating
where to find the applicable terms.

  Additional terms, permissive or non-permissive, may be stated in the
form of a separately written license, or stated as exceptions;
the above requirements apply either way.

  8. Termination.

  You may not propagate or modify a covered work except as expressly
provided under this License.  Any attempt otherwise to propagate or
modify it is void, and will automatically terminate your rights under
this License (including any patent licenses granted under the third
paragraph of section 11).

  However, if you cease all violation of this License, then your
license from a particular copyright holder is reinstated (a)
provisionally, unless and until the copyright holder explicitly and
finally terminates your license, and (b) permanently, if the copyright
holder fails to notify you of the violation by some reasonable means
prior to 60 days after the cessation.

  Moreover, your license from a particular copyright holder is
reinstated permanently if the copyright holder notifies you of the
violation by some reasonable means, this is the first time you have
received notice of violation of this License (for any work) from that
copyright holder, and you cure the violation prior to 30 days after
your receipt of the notice.

  Termination of your rights under this section does not terminate the
licenses of parties who have received copies or rights from you under
this License.  If your rights have been terminated and not permanently
reinstated, you do not qualify to receive new licenses for the same
material under section 10.

  9. Acceptance Not Required for Having Copies.

  You are not required to accept this License in order to receive or
run a copy of the Program.  Ancillary propagation of a covered work
occurring solely as a consequence of using peer-to-peer transmission
to receive a copy likewise does not require acceptance.  However,
nothing other than this License grants you permission to propagate or
modify any covered work.  These actions infringe copyright if you do
not accept this License.  Therefore, by modifying or propagating a
covered work, you indicate your acceptance of this License to do so.

  10. Automatic Licensing of Downstream Recipients.

  Each time you convey a covered work, the recipient automatically
receives a license from the original licensors, to run, modify and
propagate that work, subject to this License.  You are not responsible
for enforcing compliance by third parties with this License.

  An "entity transaction" is a transaction transferring control of an
organization, or substantially all assets of one, or subdividing an
organization, or merging organizations.  If propagation of a covered
work results from an entity transaction, each party to that
transaction who receives a copy of the work also receives whatever
licenses to the work the party's predecessor in interest had or could
give under the previous paragraph, plus a right to possession of the
Corresponding Source of the work from the predecessor in interest, if
the predecessor has it or can get it with reasonable efforts.

  You may not impose any further restrictions on the exercise of the
rights granted or affirmed under this License.  For example, you may
not impose a license fee, royalty, or other charge for exercise of
rights granted under this License, and you may not initiate litigation
(including a cross-claim or counterclaim in a lawsuit) alleging that
any patent claim is infringed by making, using, selling, offering for
sale, or importing the Program or any portion of it.

  11. Patents.

  A "contributor" is a copyright holder who authorizes use under this
License of the Program or a work on which the Program is based.  The
work thus licensed is called the contributor's "contributor version".

  A contributor's "essential patent claims" are all patent claims
owned or controlled by the contributor, whether already acquired or
hereafter acquired, that would be infringed by some manner, permitted
by this License, of making, using, or selling its contributor version,
but do not include claims that would be infringed only as a
consequence of further modification of the contributor version.  For
purposes of this definition, "control" includes the right to grant
patent sublicenses in a manner consistent with the requirements of
this License.

  Each contributor grants you a non-exclusive, worldwide, royalty-free
patent license under the contributor's essential patent claims, to
make, use, sell, offer for sale, import and otherwise run, modify and
propagate the contents of its contributor version.

  In the following three paragraphs, a "patent license" is any express
agreement or commitment, however denominated, not to enforce a patent
(such as an express permission to practice a patent or covenant not to
sue for patent infringement).  To "grant" such a patent license to a
party means to make such an agreement or commitment not to enforce a
patent against the party.

  If you convey a covered work, knowingly relying on a patent license,
and the Corresponding Source of the work is not available for anyone
to copy, free of charge and under the terms of this License, through a
publicly available network server or other readily accessible means,
then you must either (1) cause the Corresponding Source to be so
available, or (2) arrange to deprive yourself of the benefit of the
patent license for this particular work, or (3) arrange, in a manner
consistent with the requirements of this License, to extend the patent
license to downstream recipients.  "Knowingly relying" means you have
actual knowledge that, but for the patent license, your conveying the
covered work in a country, or your recipient's use of the covered work
in a country, would infringe one or more identifiable patents in that
country that you have reason to believe are valid.

  If, pursuant to or in connection with a single transaction or
arrangement, you convey, or propagate by procuring conveyance of, a
covered work, and grant a patent license to some of the parties
receiving the covered work authorizing them to use, propagate, modify
or convey a specific copy of the covered work, then the patent license
you grant is automatically extended to all recipients of the covered
work and works based on it.

  A patent license is "discriminatory" if it does not include within
the scope of its coverage, prohibits the exercise of, or is
conditioned on the non-exercise of one or more of the rights that are
specifically granted under this License.  You may not convey a covered
work if you are a party to an arrangement with a third party that is
in the business of distributing software, under which you make payment
to the third party based on the extent of your activity of conveying
the work, and under which the third party grants, to any of the
parties who would receive the covered work from you, a discriminatory
patent license (a) in connection with copies of the covered work
conveyed by you (or copies made from those copies), or (b) primarily
for and in connection with specific products or compilations that
contain the covered work, unless you entered into that arrangement,
or that patent license was granted, prior to 28 March 2007.

  Nothing in this License shall be construed as excluding or limiting
any implied license or other defenses to infringement that may
otherwise be available to you under applicable patent law.

  12. No Surrender of Others' Freedom.

  If conditions are imposed on you (whether by court order, agreement or
otherwise) that contradict the conditions of this License, they do not
excuse you from the conditions of this License.  If you cannot convey a
covered work so as to satisfy simultaneously your obligations under this
License and any other pertinent obligations, then as a consequence you may
not convey it at all.  For example, if you agree to terms that obligate you
to collect a royalty for further conveying from those to whom you convey
the Program, the only way you could satisfy both those terms and this
License would be to refrain entirely from conveying the Program.

  13. Use with the GNU Affero General Public License.

  Notwithstanding any other provision of this License, you have
permission to link or combine any covered work with a work licensed
under version 3 of the GNU Affero General Public License into a single
combined work, and to convey the resulting work.  The terms of this
License will continue to apply to the part which is the covered work,
but the special requirements of the GNU Affero General Public License,
section 13, concerning interaction through a network will apply to the
combination as such.

  14. Revised Versions of this License.

  The Free Software Foundation may publish revised and/or new versions of
the GNU General Public License from time to time.  Such new versions will
be similar in spirit to the present version, but may differ in detail to
address new problems or concerns.

  Each version is given a distinguishing version number.  If the
Program specifies that a certain numbered version of the GNU General
Public License "or any later version" applies to it, you have the
option of following the terms and conditions either of that numbered
version or of any later version published by the Free Software
Foundation.  If the Program does not specify a version number of the
GNU General Public License, you may choose any version ever published
by the Free Software Foundation.

  If the Program specifies that a proxy can decide which future
versions of the GNU General Public License can be used, that proxy's
public statement of acceptance of a version permanently authorizes you
to choose that version for the Program.

  Later license versions may give you additional or different
permissions.  However, no additional obligations are imposed on any
author or copyright holder as a result of your choosing to follow a
later version.

  15. Disclaimer of Warranty.

  THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY
APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT
HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY
OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,
THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM
IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF
ALL NECESSARY SERVICING, REPAIR OR CORRECTION.

  16. Limitation of Liability.

  IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR CONVEYS
THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY
GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE
USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF
DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD
PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS),
EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
SUCH DAMAGES.

  17. Interpretation of Sections 15 and 16.

  If the disclaimer of warranty and limitation of liability provided
above cannot be given local legal effect according to their terms,
reviewing courts shall apply local law that most closely approximates
an absolute waiver of all civil liability in connection with the
Program, unless a warranty or assumption of liability accompanies a
copy of the Program in return for a fee.

                     END OF TERMS AND CONDITIONS

            How to Apply These Terms to Your New Programs

  If you develop a new program, and you want it to be of the greatest
possible use to the public, the best way to achieve this is to make it
free software which everyone can redistribute and change under these terms.

  To do so, attach the following notices to the program.  It is safest
to attach them to the start of each source file to most effectively
state the exclusion of warranty; and each file should have at least
the "copyright" line and a pointer to where the full notice is found.

    <one line to give the program's name and a brief idea of what it does.>
    Copyright (C) <year>  <name of author>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

Also add information on how to contact you by electronic and paper mail.

  If the program does terminal interaction, make it output a short
notice like this when it starts in an interactive mode:

    <program>  Copyright (C) <year>  <name of author>
    This program comes with ABSOLUTELY NO WARRANTY; for details type `show w'.
    This is free software, and you are welcome to redistribute it
    under certain conditions; type `show c' for details.

The hypothetical commands `show w' and `show c' should show the appropriate
parts of the General Public License.  Of course, your program's commands
might be different; for a GUI interface, you would use an "about box".

  You should also get your employer (if you work as a programmer) or school,
if any, to sign a "copyright disclaimer" for the program, if necessary.
For more information on this, and how to apply and follow the GNU GPL, see
<http://www.gnu.org/licenses/>.

  The GNU General Public License does not permit incorporating your program
into proprietary programs.  If your program is a subroutine library, you
may consider it more useful to permit linking proprietary applications with
the library.  If this is what you want to do, use the GNU Lesser General
Public License instead of this License.  But first, please read
<http://www.gnu.org/philosophy/why-not-lgpl.html>.

*/

