:- module(pddl_robert_sasak,[test_blocks/0,test_domain/1,test_all/0,test_rest/0,test_sas/0,test_dir_files_sas/1,test_dir_files_sas/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FILENAME:  common.pl 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file contain common predicates that are used in planners

:- dynamic(domain/3).
:- dynamic(pairfrom/4).


% end_of_file.

:- expects_dialect(sicstus).
:- use_module(library(timeout)).
:- use_module(library(lists)).



% command_line_sas/0
%
%   run planner
%   takes two params from command line arguments
%   first is a domain file 
%   second problem file
%

:-set_prolog_stack(global, limit(16*10**9)).
:-set_prolog_stack(local, limit(16*10**9)).
:-set_prolog_stack(trail, limit(16*10**9)).

command_line_sas:-
    prolog_flag(argv, [D,P]),!,
    solve_files(D, P),
    halt.

command_line_sas:- test_blocks, test_all.

slow_on('blocks-07-0.pddl').
slow_on('blocks-08-0.pddl').
slow_on('blocks-09-0.pddl').

filematch_sas(A,B):-must((filematch(A,B))).


test_all:-test_all(7).
test_all(N):- 
  filematch_sas('./orig_pddl_parser/test/?*?/domain*.pddl',_),!,
  (forall(filematch_sas('./orig_pddl_parser/test/?*?/domain*.pddl',E),once(test_domain(E,N)))).

test_all(N):- expand_file_name('./orig_pddl_parser/test/?*?/domain*.pddl',RList),RList\=[],!,reverse(RList,List),
  forall(member(E,List),once(test_domain(E,N))).

test_primaryobjects:- 
  (forall(filematch_sas('./primaryobjects_strips/?*?/domain*.*',E),once(test_domain(E)))). 

min_sas(A,B,A):-A =< B,!.
min_sas(_,A,A).


first_n_elements(ListR,Num,List):-length(ListR,PosNum),min_sas(PosNum,Num,MinNum),length(List,MinNum),append(List,_,ListR),!.

test_domain(DP):- thlocal:loading_files,!,must(load_domain(DP)).
test_domain(DP):- test_domain(DP,12).

test_domain(DP,Num):- \+ exists_file(DP),!, forall(filematch(DP,MATCH),(exists_file(MATCH),test_domain(MATCH,Num))).
test_domain(DP,Num):-
   format('~q.~n',[test_domain(DP)]),
  directory_file_path(D,_,DP),directory_files(D,RList),reverse(RList,ListR),
   sort(ListR,ListS),length(ListR,PosNum),min_sas(PosNum,Num,MinNum),length(List,MinNum),append(List,_,ListS),!,
   forall(member(T,List),ignore((directory_file_path(D,T,TP),exists_file(TP),not(same_file(DP,TP)),
  solve_files(DP,TP)))).

load_domain(DP):- \+ exists_file(DP),!, forall(filematch(DP,MATCH),((exists_file(MATCH),load_domain(MATCH)))).
load_domain(DP):-
   format('~q.~n',[load_domain(DP)]),
  directory_file_path(D,_,DP),directory_files(D,RList),   
   forall(member(T,RList),ignore((directory_file_path(D,T,TP),exists_file(TP),load_file(TP)))).

load_file(F):- must(read_file(F, L)),load_file_rest(L).
load_file_rest([]):-!.
load_file_rest(L):- first_n_elements(L,10,ES),
   (
   (append(_,['define','(','domain',Named|_],ES),domainBNF(O, L, R1)) ->  save_domain(Named,O);
   (append(_,['(','problem',Named|_],ES),problem(O, L, R1)) ->  save_problem(Named,O);
   (append(_,['('|_],ES),sterm(O, L, R1))  ->  save_sterm(O);
   must( sterm(O,L,R1)) ->  save_sterm(O)),!,
   load_file_rest(R1).
    
save_domain(Named,O):-pfc_add(is_saved_type(domain,Named,O)).
save_problem(Named,O):-pfc_add(is_saved_type(problem,Named,O)).
save_sterm(O):-gensym(sterm,Named),pfc_add(is_saved_type(sterm,Named,O)).

test_frolog:- test_dir_files_sas('frolog','p02-domain.pddl','p02.pddl'),
    test_dir_files_sas('frolog','tPddlAgent01-domain.pddl','tPddlAgent01.pddl'),
    !. % test_dir_files_sas('frolog','tPddlAgent02-domain.pddl','tPddlAgent02.pddl').

test_blocks:- solve_files('./orig_pddl_parser/test/blocks/domain-blocks.pddl', 
  './orig_pddl_parser/test/blocks/blocks-03-0.pddl'), fail.
test_blocks:- fail, expand_file_name('./orig_pddl_parser/test/blocks/domain*.pddl',RList),reverse(RList,List),
  forall(member(E,List),once(test_domain(E))).
test_blocks.

user:sanity_test:- test_blocks.

% solve_files(+DomainFile, +ProblemFile)
%
%   Reads files and set timelimit for planner
%
solve_files(DomainFile, ProblemFile):- 
 forall(must(filematch_sas(DomainFile,DomainFile0)),
   forall(must(filematch_sas(ProblemFile,ProblemFile0)),
     (time(solve_files_0(DomainFile0, ProblemFile0))))).

solve_files_0(DomainFile, ProblemFile):-
  format('~q.~n',[solve_files(DomainFile, ProblemFile)]),
   must_det_l(( parseDomain(DomainFile, DD),
    parseProblem(ProblemFile, PP),
    term_to_ord_term(DD, D0),copy_term_spec_q(D0, D),
    term_to_ord_term(PP, P0),copy_term_spec_q(P0, P),
    reset_statistic)),
    !,
    try_solve(D,P,S),
    show_statistic(P, S),
    !.



% try_solve(D,P,S):- once(time_out(solve(D, P, S), 3000, Result)), Result == time_out, portray_clause(hard_working:-try_solve(D,P,S)),fail.
try_solve(D,P,S):- gripe_time(14,time_out((solve(D, P, S)), 500000, Result)),!, % time limit for a planner (was 500000)
   ((\+ is_list(S)
     -> portray_clause('failed'(Result):-try_solve(D,P,S)) ;
       ((Result=time_out)->portray_clause('failed'(Result):-try_solve(D,P,S));true))),!.

try_solve(D,P,S):-dmsg('utter_failed'(warn):-try_solve(D,P,S)),!,fail.


% my_bb_put(CA, vannot(_Ano,V)):-!, bb_put(CA, V).
my_bb_put(CA, V):- bb_put(CA, V).

% solve(+Domain, +Problem, -Solution).
%
%   Set domain and problem on blackboard
%
:-thread_local(thlocal:other_planner/1).
solve(D,P,S):- thlocal:other_planner(C),logOnError(call(C,D,P,S)),!.
solve(D, P, Solution):-
    get_init(P, I),    my_bb_put(initState, I),
    get_goal(P, G),    my_bb_put(goalState, G),
    get_metric(P, M),    my_bb_put(metric, M),
    get_actions(D, A),	my_bb_put(actions, A),
    get_objects(P, O),	my_bb_put(objects, O),
    make_init_state(IS),
    search(IS, G, Solution).


% term_to_ord_term(+Term, -OrdTerm)
%
%   Go throught the term and look for sets, return the same term
%   with all sets become ordered.
%

term_to_ord_term(Term, OrdTerm):-t2ot(Term, OrdTerm).

t2ot(A, A):- \+ compound(A), !.

t2ot(T, OT):-t2ot_0(T, OT),!.


t2ot_0([H|T], R):-
    t2ot(H, OH),
    t2ot(T, OT),
    ord_add_element(OT, OH, R),
    !.
%    write(OH), write(OT), write('   '), write(R), nl.
t2ot_0(T, OT):-
    T =.. [F,P],
    !,
    t2ot(P, OP),
    OT =..[F,OP].
t2ot_0(T, OT):-
    T =.. [F,P|Ps],
    NT=.. [F|Ps],
    t2ot(P, OP),
    t2ot(NT, ONT),
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
    my_bb_put(CA, V),
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
copy_term_spec(A,B):-copy_term(A,B).

copy_term_spec_q(A,B):-
    cp(A,[],B,_).

cp(A,Vars,A,Vars):-
    atomic(A), A\= ?(_).
cp(?(V),Vars,NV,NVars):-
    atomic(V),
    register_var(V,Vars,NV,NVars).
cp('$VAR'(V),Vars,NV,NVars):-
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
    my_bb_put(stat_nodes, 0),
    statistics(runtime, [T,_]),
    my_bb_put(startTime, T).

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
    (is_list(S)-> length(S, L) ; L = -1),
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
    write_list_sas(P),
    write(')'),
    solution_to_lisp(T).

write_list_sas([]).
write_list_sas([H|T]):-
    write(' '), write(H),
    write_list_sas(T).


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
    my_bb_put(fictiveGoal, G).


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

% % :-command_line_sas.

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
parseDomain(F, O):- parseDomain(F, O, R), load_file_rest(R),!.


% parseDomain(+File, -Output, -RestOfFile)
%
%   The same as above and also return rest of file. Can be useful when domain and problem are in one file.
%
parseDomain(File, Output, R) :-  
    read_file(File, List),!,
   %  (nonvar(Output)->true;empty_assoc(Output)),
    % set_nb_propval(Output,filename,File),
    domainBNF(Output, List, R),!.

set_nb_propval(_PROPS,_Name,_Value):-!.
    

:-thread_local(thlocal:allow_sterm).

domainBNF(Output, List, R):- with_assertions(tlbugger:skipMust, debugOnError0(pddl_robert_sasak:domainBNF_dcg(Output,Output, List, R))),!.
domainBNF(Output, List, R):- with_assertions(thlocal:allow_sterm,with_assertions(tlbugger:skipMust, debugOnError0(pddl_robert_sasak:domainBNF_dcg(Output,Output, List, R)))),!,
   portray_clause((domainBNF:-thlocal:allow_sterm,Output)).
domainBNF(P     , List, R):- must(sterm(O, List, R)),!,must(sterm2pterm(O,P)),!,portray_clause((ed:-P)).
domainBNF(Output, List, R):- trace,domainBNF_dcg(_PROPS, Output, List, R),!.

:-export(domainBNF_dcg//2).

svar(Var,Var):-var(Var),!.
svar('$VAR'(Var),(Var)).
svar('?'(Var),(Var)).
svar(A,(R)):-atom(A),atom_concat('??',R,A).
svar(A,(R)):-atom(A),atom_concat('?',R,A).

sterm2pterm(VAR,VAR):-var(VAR),!.
sterm2pterm(VAR,'?'(UP)):-svar(VAR,SVAR),!,upcase_atom(SVAR,UP).
sterm2pterm([S],S):-atom(S). % ,atom_concat(':',_,S),!.
sterm2pterm([S|SLIST],PTERM):-atom(S),atom_concat(':',_,S),
            maplist(sterm2pterm,SLIST,PLIST),           
            PTERM=..[S,PLIST].
sterm2pterm([S|SLIST],PTERM):-atom(S),\+ svar(S,_),!,
            maplist(sterm2pterm,SLIST,PLIST),           
            PTERM=..[S|PLIST].
sterm2pterm(SLIST,PLIST):- is_list(SLIST),!,maplist(sterm2pterm,SLIST,PLIST).
sterm2pterm(VAR,VAR):-!.

sterm(_) --> [')'],{!,fail}.
sterm([]) --> ['(',')'],!.
sterm(A) --> action_def(A),!.
sterm(require_def(R)) --> require_def(R),!.
sterm(types(L))                    --> ['(',':',types],      typed_list(name, L), [')'].
sterm(constants(L))                --> ['(',':',constants],  typed_list(name, L), [')'].
sterm(preds(P)) --> predicates_def(P).
%sterm([H,T]) --> skey(H),['('],!,zeroOrMore(sterm, T), [')'],!.
%sterm([H,T]) --> ['('],skey(H),!,oneOrMore(sterm, T), [')'],!.
sterm([H|T]) --> ['('],satom(H),!, zeroOrMore(sterm, T), [')'],!.
sterm([H|T]) --> ['('],sterm(H),!, zeroOrMore(sterm, T), [')'],!.


sterm(N)-->satom(N).

satom(_) --> [')'],{!,fail}.
satom(V)                    --> skey(V),!.
satom('?'(V))                    --> ['?'],!,name(V).
satom(V)                         --> [V],!.

skey(_) --> [')'],{!,fail}.
skey(N)                         --> [':'],!,name(S),{atom_concat(':',S,N)},!.


can_pddl_30.

pddl_3_0 --> {can_pddl_30}, [],!.
pddl_3_0(_Feature) --> {fail, can_pddl_30}, [],!.
pddl_3_0_e(_Feature) --> {fail, can_pddl_30}, [],!.

% Support for reading file as a list.
% :- [readFile].


% Defining operator ?. It is a syntax sugar for marking variables: ?x
:- op(300, fy, ?).


% domainBNF_dcg(domain(N, R, T, C, P, F, C, S))
%
%   DCG rules describing structure of domain file in language PDDL.
%   BNF description was obtain from http://www.cs.yale.edu/homes/dvm/papers/pddl-bnf.pdf
%   This parser do not fully NOT support PDDL 3.0
%   However you will find comment out lines ready for futher development.
%
domainBNF_dcg(Struct, Struct)
                        --> ['(','define'],([':'];[]),['(','domain'], name(N), [')'],
                        {must(Struct = domain(N, R, T, C, P, F, C, S)),(var(OptionsDict)-> rb_new(OptionsDict) ; true)},
                        dcgMust(domainBNF_rest( Struct, R, T, C, P, F, C, S)),
                        {set_nb_propval(Struct,domain_name,N)}.
                        
domainBNF_rest( Struct, R, T, C, P, F, C, S ) --> 
                            dcgMust(require_def(R)    ; []),
                             dcgMust(types_def(T)      ; []), %:typing
                             dcgMust(constants_def(C)  ; []),
                             dcgMust(predicates_def(P) ; []),
                             dcgMust(functions_def(F)  ; []), %:fluents
%                            dcgMust (constraints(C)   ; []),    %:constraints
                             dcgMust(zeroOrMore(structure_def, S)),
                             [')'],{
                            set_nb_propval(PROPS,requires,R)  ,  
                            set_nb_propval(PROPS,types,T)    ,   %:typing
                            set_nb_propval(PROPS,constants,C) ,
                            set_nb_propval(PROPS,predicates,P) ,
                            set_nb_propval(PROPS,functions_def,F), %:fluents
                            set_nb_propval(PROPS,dconstraints,C)   ,    %:constraints
                            set_nb_propval(PROPS,actions, S),
                             !}.

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

predicate(_) --> [P], {P==not,!,fail}.
predicate(P)                    --> name(P).

variable(V)                     --> ['?'], name(N), {V =.. [?, N]}.
atomic_function_skeleton(f(S, L))
                                --> ['('], function_symbol(S), typed_list(variable, L), [')'].
function_symbol(S)              --> name(S).
functions_def(F)                --> ['(',':',functions], function_typed_list(atomic_function_skeleton, F), [')'].              %:fluents
dconstraints_def(C)                 --> ['(',':',constraints], con_GD(C), [')'].                                                   %:constraints
structure_def(A)                --> action_def(A).
structure_def(D)               --> durative_action_def(D).                                                                    %:durativeactions
%structure_def(D)               --> derived_def(D).                                                                            %:derivedpredicates
structure_def(D)         --> allowed_sterm(structure_def,D).
%typed_list(W, G)               --> oneOrMore(W, N), ['-'], type(T), {G =.. [T, N]}.
typed_list(W, [G|Ns])           --> oneOrMore(W, N), ['-'], type(T), !, typed_list(W, Ns), {G =.. [T,N]}.
typed_list(W, N)                --> zeroOrMore(W, N).


allowed_sterm(Why,sterm(Why,D))--> {thlocal:allow_sterm},sterm(D).                                                                           

effected_typed_list(W, [G|Ns])           --> oneOrMore(W, N), ['-'], effect(T), !, effected_typed_list(W, Ns), {G =.. [T,N]}.
effected_typed_list(W, N)                --> zeroOrMore(W, N).

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

% Actions definitons
durative_action_def(action(S, L, Precon, Pos, Neg, Assign))
                                --> ['(',':',action], action_symbol(S),
                                    [':',parameters,'('], typed_list(variable, L), [')'], 
                                    da_def_body(Precon, Pos, Neg, Assign),
                                    [')'].


% % 2 ?- phrase(emptyOr(pre_GD(P)),['(',accessible,?,x,')','(','no-inventory-object',?,x,')','(','has-location',?,x,?,y,')'],X).
% % P = accessible(?x),
% % X = ['(', 'no-inventory-object', ?, x, ')', '(', 'has-location', ?, x|...] .

da_def_body([P1,P2], Pos, Neg, Assign)
                                -->  
                                    (([':',duration], emptyOr(con_GD(P1)))                ; []),
                                    (([':',condition], emptyOr(pre_GD(P2)))                ; []),
                                    (([':',effect],       emptyOr(effect(Pos, Neg, Assign))) ; []).

action_def_body(P, Pos, Neg, Assign)
                                -->  
                                    (([':',precondition], emptyOr(pre_GD(P)))                ; []),
                                    (([':',effect],       emptyOr(effect(Pos, Neg, Assign))) ; []).


% % [1] 2 ?- pre_GD(X,['(',accessible,?,x,')'],[]).
% % X = accessible(?x) .
pre_GD(_)			--> [:,effect],{!,fail}.
% pre_GD(and(P))                  --> ['(',and],  zeroOrMore(pre_GD ,P), [')'].       
pre_GD([F])                     --> atomic_formula(term, F).
pre_GD(and(P))                  --> ['('],  oneOrMore(pre_GD ,P), [')'].       
pre_GD(P)                       --> pref_GD(P).
% pre_GD(P)                       --> ['(',and], dcgMust((pre_GD(P), [')'])).
% pre_GD(forall(L, P))           --> pddl_3_0, ['(',forall,'('],  dcgMust(((typed_list(variable, L), [')'], pre_GD(P), [')']))).         %:universal-preconditions
pref_GD(preference(N, P))      --> pddl_3_0, ['(', preference], dcgOptionalGreedy(pref_name(N)),  dcgMust(gd(P)), dcgMust([')']).                         %:preferences
pref_GD(P)                      --> gd(P).
pref_name(N)                    --> name(N).

% gd(and(P))                  --> pddl_3_0_e(gd), ['(',and],   zeroOrMore(gd ,P), [')'].       
% % gd(F)                           --> atomic_formula(term, F).                                                    %:this option is covered by gd(L)
gd(L)                          --> literal(term, L).                                                           %:negative-preconditions
gd(':-------------------------------------------------effect'(zzzzzzzzzzzz))                          --> [':','effect'].
gd(P)                           --> ['(',and],  zeroOrMore(gd, P), [')'].
gd(or(P))                      --> pddl_3_0_e(gd), ['(',or],   zeroOrMore(gd ,P), [')'].                                       %:disjuctive-preconditions
gd(not(P))                     --> pddl_3_0_e(gd), ['(',not],  gd(P), [')'].                                                   %:disjuctive-preconditions
gd(imply(P1, P2))              --> pddl_3_0_e(gd), ['(',imply], gd(P1), gd(P2), [')'].                                         %:disjuctive-preconditions
gd(exists(L, P))               --> pddl_3_0_e(gd), ['(',exists,'('], typed_list(variable, L), [')'], gd(P), [')'].             %:existential-preconditions
gd(forall(L, P))               --> pddl_3_0_e(gd), ['(',forall,'('], typed_list(variable, L), [')'], gd(P), [')'].             %:universal-preconditions
gd(F)                           --> f_comp(F).                                                                  %:fluents
f_comp(compare(C, E1, E2))      --> ['('], binary_comp(C), f_exp(E1), f_exp(E2), [')'].

literal(T, not(F))              --> neg_atomic_formula(T,F).
literal(T, F)                   --> atomic_formula(T, F).

atomic_formula(_, F)            --> ['('], predicate(P), zeroOrMore(term, T), [')'], {F =.. [P|T]}.             % cheating, maybe wrong


neg_atomic_formula(T,F)       --> [not], atomic_formula(T,F).
neg_atomic_formula(T,F)       --> ['(',not], atomic_formula(T,F),[')'].


term(V)                         --> variable(V).
term(V)                         --> number_sas(V).
term(N)                         --> [N],{bad_name(N),!,fail}.
term(N)                         --> name(N).

f_exp(N)                        --> number_sas(N).
f_exp(op(O, E1, E2))            --> ['('],binary_op(O), f_exp(E1), f_exp(E2), [')'].
f_exp('-'(E))                   --> ['(','-'], f_exp(E), [')'].
f_exp(H)                        --> f_head(H).
f_head(F)                       --> ['('], function_symbol(S), zeroOrMore(term, T), [')'], { F =.. [S|T] }.
f_head(S)                       --> function_symbol(S).
binary_op(O)                    --> multi_op(O).
binary_op(45)                   --> [45]. % 45 = minus = '-'  (TODO - WHY IS THIS HERE?)
binary_op('-')                  --> ['-'].
binary_op('/')                  --> ['/'].
multi_op('*')                   --> ['*'].
multi_op('+')                   --> ['+'].
binary_comp('>')                --> ['>'].
binary_comp('<')                --> ['<'].
binary_comp('=')                --> ['='].
binary_comp('>=')               --> ['>='].
binary_comp('<=')               --> ['<='].
number_sas(N)                       --> [N], {number(N),!}.
effect(P, N, A)                 --> ['(',and], c_effect(P, N, A), [')'].
effect(P, [M|N], A)                 --> ['(',not], c_effect([M|P], N, A), [')'].
effect(P, N, A)                 --> c_effect(P, N, A).
c_effect(forall(Es))            --> pddl_3_0, ['(',forall,'('], effected_typed_list(variable,Es), [')', ')'].    %:conditional-effects
c_effect(when(P, E))           --> pddl_3_0, ['(',when], gd(P), cond_effect(E), [')'].                   %:conditional-effects
c_effect(P, N, A)               --> p_effect(P, N, A).
p_effect([], [], [])            --> [].
p_effect(Ps, Ns, [F|As])        --> ['('], assign_op(O), f_head(H), f_exp(E), [')'], p_effect(Ps, Ns, As), {F =.. [O, H, E]}.
p_effect(Ps, [F|Ns], As)        --> neg_atomic_formula(term,F), p_effect(Ps, Ns, As).
p_effect([F|Ps], Ns, As)        --> atomic_formula(term, F), p_effect(Ps, Ns, As).
%p_effect(op(O, H, E))          --> pddl_3_0(op/3), ['('], assign_op(O), dcgMust((f_head(H), f_exp(E), [')'])).            %:fluents , What is difference between rule 3 lines above???
cond_effect(E)                 --> ['(',and], zeroOrMore(p_effect, E), [')'].                  %:conditional-effects
cond_effect(E)                 --> p_effect(E).                                                %:conditional-effects
assign_op(assign)               --> [assign].
assign_op(scale_up)             --> [scale_up].
assign_op(scale_down)           --> [scale_down].
assign_op(increase)             --> [increase].
assign_op(decrease)             --> [decrease].


% BNF description include operator <term>+ to mark zero or more replacements.
% This DCG extension to overcome this. 
oneOrMore(W, [R|Rs], A, C) :- call(W, R, A, B),  (
                                                         oneOrMore(W, Rs, B, C)
                                                         ;
                                                         (Rs = [] , C = B) 
                                                     ).
% BNF operator <term>*
zeroOrMore(W, R)                --> oneOrMore(W, R).
zeroOrMore(_, [])               --> [].

% Name is everything that is not number, bracket or question mark.
% Those rules are not necessary, but rapidly speed up parsing process.
name(N)                         --> [N] , {((\+ bad_name(N)))}.

bad_name(N):- (var(N);number(N)),!.
bad_name(':').
bad_name('not').
bad_name(N):-arg(_,v('(',')',?,(-)),N).

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
parseProblem(F, O):-parseProblem(F, O, R), load_file_rest(R),!.


% parseProblem(+File, -Output, -RestOfFile).
%
%   The same as above and also return rest of file. Can be useful when domain and problem are in one file.
%
parseProblem(F, O, R) :-
    read_file(F, L),!,
    problem(O, L, R),!.    

% Support for reading file as a list.
% :- [readFile].

problem(Output, List, R):- with_assertions(tlbugger:skipMust, debugOnError0(pddl_robert_sasak:problem_dcg(Output, List, R))),!.
problem(Output, List, R):- with_assertions(thlocal:allow_sterm,with_assertions(tlbugger:skipMust, debugOnError0(pddl_robert_sasak:problem_dcg(Output, List, R)))),!,
   portray_clause((problem:-thlocal:allow_sterm,Output)).
problem(P     , List, R):- must(sterm(O, List, R)),!,must(sterm2pterm(O,P)),!,portray_clause((ed:-P)).
problem(Output, List, R):- trace,problem_dcg(Output, List, R),!.

% DCG rules describing structure of problem file in language PDDL.
%
%   BNF description was obtain from http://www.cs.yale.edu/homes/dvm/papers/pddl-bnf.pdf
%   This parser do not fully NOT support PDDL 3.0
%   However you will find comment out lines ready for futher development.
%   Some of the rules are already implemented in parseDomain.pl
%
% :-[parseDomain]. % make sure that it is loaded.

problem_dcg(Struct)   
                                --> ['(',define],([':'];[]),['(',problem,Name,')',
                                     '(',':',domain, Domain,')'],
   {must((Struct = problem(Name, Domain, R, OD, I, G, UNK, MS, LS))),
           (var(OptionsDict)-> rb_new(OptionsDict) ; true)},
                                     dcgMust(problem_rest(R, OD, I, G, UNK, MS, LS)).

problem_rest(R, OD, I, G, _, MS, LS) --> 
   (require_def(R)         ; []),
   (object_declaration(OD) ; []),
   (dcgMust(init(I)) ; []),
   (goal(G) ; []),
%                                    (constraints(C)   ; []), %:constraints
   (metric_spec(MS) ; []),
   (length_spec(LS)  ; []),
   dcgMust([')']),!.


object_declaration(L)           --> ['(',':',objects], typed_list_as_list(name, L),[')'].

typed_list_as_list(W, OUT)   --> oneOrMore(W, N), ['-'],!, dcgMust(( type(T), typed_list_as_list(W, Ns), {G =.. [T,N], OUT = [G|Ns]})).
typed_list_as_list(W, N)        --> zeroOrMore(W, N).


goal_list(_,G) --> pre_GD(G).
goal_list(_,G) --> zeroOrMore(init_el,G).
goal_list(H,G) --> allowed_sterm(H,G).

init(I)                         --> ['(',':',init], goal_list(init,I), [')'].

init_el(I)                      --> literal(term, I).
init_el(I)                      --> pre_GD(I).
init_el(set(H,N))               --> ['(','='], f_head(H), number_sas(N), [')'].                                     % fluents
init_el(at(N, L))               --> ['(',at], number_sas(N), literal(name, L), [')'].                               % timed-initial literal
goal(G)                         --> ['(',':',goal],goal_list(goal,G),[')'].
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
con_GD(within(N, P))            --> ['(',within], number_sas(N), gd(P), [')'].

con_GD(at_most_once(P))         --> ['(','at-most-once'], gd(P),[')'].
con_GD(some_time_after(P1, P2)) --> ['(','sometime-after'], gd(P1), gd(P2), [')'].
con_GD(some_time_before(P1, P2))--> ['(','sometime-before'], gd(P1), gd(P2), [')'].
con_GD(always_within(N, P1, P2))--> ['(','always-within'], number_sas(N), gd(P1), gd(P2), [')'].
con_GD(hold_during(N1, N2, P))  --> ['(','hold-during'], number_sas(N1), number_sas(N2), gd(P), [')'].
con_GD(hold_after(N, P))        --> ['(','hold-after'], number_sas(N), gd(P),[')'].

metric_spec(metric(O, E))       --> ['(',':',metric], optimization(O), dcgMust((metric_f_exp(E), [')'])).

optimization(minimize)          --> [minimize].
optimization(maximize)          --> [maximize].

metric_f_exp(E)                 --> ['('], binary_op(O), metric_f_exp(E1), metric_f_exp(E2), [')'], {E =..[O, E1, E2]}.
metric_f_exp(multi_op(O,[E1|E]))--> ['('], multi_op(O), metric_f_exp(E1), oneOrMore(metric_f_exp, E), [')']. % I dont see meanful of this rule, in additional is missing in f-exp
metric_f_exp(E)                 --> ['(','-'], metric_f_exp(E1), [')'], {E=..[-, E1]}.
metric_f_exp(N)                 --> number_sas(N).
metric_f_exp(F)                 --> ['('], function_symbol(S), zeroOrMore(name, Ns), [')'], { F=..[S|Ns]}.%concat_atom_iio([S|Ns], '-', F) }.
metric_f_exp(function(S))       --> function_symbol(S).
metric_f_exp(total_time)        --> ['total-time'].
metric_f_exp(is_violated(N))    --> ['(','is-violated'], pref_name(N), [')'].

% Work arround
metric_f_exp(is_violated(N,V))    --> ['(','*','(','is-violated'], pref_name(N), [')'],number_sas(V),[')'].

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
read_file( File, Words) :-  exists_file(File), !, seeing(Old),call_cleanup(( see(File), get_code(C), (read_rest(C, Words))),( seen, see(Old))),!.
read_file(File0, Words) :-  must((filematch_sas(File0,File),exists_file(File),read_file( File, Words))),!.

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
test_parse_file(F,O):- must(read_file(F, L)),!,((domainBNF(O, L, _R1); must(((problem(O, L, _R2)))))),!.

test_parse_file(F):- test_parse_file(F,L),arg(2,L,List),!,
 (not(member('(',List))->true;
    ((absolute_file_name(F,A),
	write('Parsing file failed. '), write('('), write(F:A), write(')'), nl))),!.


test_dir_sas(DirIn):-forall(filematch_sas(DirIn,DirInM),test_dir_m(DirInM)).
test_dir_m(DIR):-
  working_directory(WAS,WAS),
     call_cleanup(( 
        cd(DIR),
	write('Testing ':DIR), nl,
	test_dir_files_sas(DIR)),
        cd(WAS)).

test_sas:- 
      test_dir_sas('ipc2008-no-cybersec/seq-sat/elevators-strips/'),!, % NO FIRST ANSWER
      !.

test_sas_sanity:- 
      test_dir_sas('ipc2008-no-cybersec/seq-opt/openstacks-strips/'), %PASSES BUT RUNS SLOW
       test_dir_sas('ipc2008-no-cybersec/seq-opt/transport-strips/'), %PASSES BUT RUNS
      test_dir_sas('ipc2008-no-cybersec/netben-opt/elevators-strips/'), % FAIL ALL
      !.

test_rest:-	
	test_dir_sas('ipc2008-no-cybersec/seq-opt/parcprinter-strips/'),
	test_dir_sas('ipc2008-no-cybersec/seq-opt/pegsol-strips/'),
	test_dir_sas('ipc2008-no-cybersec/seq-opt/scanalyzer-strips/'),
	test_dir_sas('ipc2008-no-cybersec/seq-opt/sokoban-strips/'),  % NO FIRST ANSWER
       
	test_dir_sas('ipc2008-no-cybersec/seq-opt/woodworking-strips/'),
	
        
        expand_file_name('ipc2008-no-cybersec/?*?/*/',O),
        forall(member(E,O),test_dir_sas(E)).

test_dir_files_sas(Dir,D,P):- directory_file_path(Dir,D,DF), directory_file_path(Dir,P,PF),
        test_parse_file(DF),test_parse_file(PF),
        solve_files(DF,PF),!.

test_dir_files_sas(Dir):-   
	test_dir_files_sas(Dir,'p01-domain.pddl','p01.pddl'),
	test_dir_files_sas(Dir,'p02-domain.pddl','p02.pddl'),
	test_dir_files_sas(Dir,'p03-domain.pddl','p03.pddl'),
	test_dir_files_sas(Dir,'p04-domain.pddl','p04.pddl'),
	test_dir_files_sas(Dir,'p05-domain.pddl','p05.pddl'),
	test_dir_files_sas(Dir,'p06-domain.pddl','p06.pddl'),
	test_dir_files_sas(Dir,'p07-domain.pddl','p07.pddl'),
	test_dir_files_sas(Dir,'p08-domain.pddl','p08.pddl'),
	test_dir_files_sas(Dir,'p09-domain.pddl','p09.pddl'),
	test_dir_files_sas(Dir,'p10-domain.pddl','p10.pddl'),
	test_dir_files_sas(Dir,'p11-domain.pddl','p11.pddl'),
	test_dir_files_sas(Dir,'p12-domain.pddl','p12.pddl'),
	test_dir_files_sas(Dir,'p13-domain.pddl','p13.pddl'),
	test_dir_files_sas(Dir,'p14-domain.pddl','p14.pddl'),
	test_dir_files_sas(Dir,'p15-domain.pddl','p15.pddl'),
	test_dir_files_sas(Dir,'p16-domain.pddl','p16.pddl'),
	test_dir_files_sas(Dir,'p17-domain.pddl','p17.pddl'),
	test_dir_files_sas(Dir,'p18-domain.pddl','p18.pddl'),
	test_dir_files_sas(Dir,'p19-domain.pddl','p19.pddl'),
	test_dir_files_sas(Dir,'p20-domain.pddl','p20.pddl'),
	test_dir_files_sas(Dir,'p21-domain.pddl','p21.pddl'),
	test_dir_files_sas(Dir,'p22-domain.pddl','p22.pddl'),
	test_dir_files_sas(Dir,'p23-domain.pddl','p23.pddl'),
	test_dir_files_sas(Dir,'p24-domain.pddl','p24.pddl'),
	test_dir_files_sas(Dir,'p25-domain.pddl','p25.pddl'),
	test_dir_files_sas(Dir,'p26-domain.pddl','p26.pddl'),
	test_dir_files_sas(Dir,'p27-domain.pddl','p27.pddl'),
	test_dir_files_sas(Dir,'p28-domain.pddl','p28.pddl'),
	test_dir_files_sas(Dir,'p29-domain.pddl','p29.pddl'),
	test_dir_files_sas(Dir,'p30-domain.pddl','p30.pddl').


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
  %  write('NO SOLUTION'),nl,
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



:- if(gethostname(c3po)).

:- module(pddl_robert_sasak).


:-thread_local(thlocal:loading_files).

% thlocal:other_planner(hyhtn_solve).

:- debug,must(test_blocks).
:- solve_files('benchmarks/mystery/domain.pddl','benchmarks/mystery/prob01.pddl').
:- test_domain('benchmarks/driverlog/domain.pddl',4).
:- solve_files('hsp-planners-master/hsp2-1.0/examples/parcprinter-strips/p02-domain-woac.pddl','hsp-planners-master/hsp2-1.0/examples/parcprinter-strips/p01-woac.pddl').

:- test_all(6).



% BAD :- test_domain('./elearning/domain.pddl').
% :- test_all.

% 
% :- solve_files('benchmarks/nomystery-sat11-strips/domain.pddl','benchmarks/nomystery-sat11-strips/p01.pddl').
% :- test_domain('./benchmarks/nomystery-sat11-strips/domain.pddl').


/*
:- solve_files('regression-tests/issue58-domain.pddl','regression-tests/issue58-problem.pddl').
:- forall(filematch_sas('./hsp-planners-master/?*?/pddl/?*?/?*domain*.*',E),once(test_domain(E,4))).
:- forall(filematch_sas('./hsp-planners-master/?*?/examples/?*?/?*domain*.*',E),once(test_domain(E,5))).

:- test_domain('./benchmarks/nomystery-sat11-strips/domain.pddl').

test_blocks:- fail, test_domain('./benchmarks/nomystery-sat11-strips/domain.pddl',RList),reverse(RList,List),
  forall(member(E,List),once(test_domain(E))).

% :-asserta(thlocal:loading_files).

:- forall(filematch_sas('./rover/?*?/?*domain*.*',E),once(load_domain(E))).
:- forall(filematch_sas('./hsp-planners-master/?*?/pddl/?*?/?*domain*.*',E),once(load_domain(E))).
:- forall(filematch_sas('./hsp-planners-master/?*?/examples/?*?/?*domain*.*',E),once(load_domain(E))).
:- forall(filematch_sas('./hsp-planners-master/?*?/examples/?*?/?*domain*.*',E),once(load_domain(E))).
:- forall(filematch_sas('./primaryobjects_strips/?*?/?*domain*.*',E),once(test_domain(E))).
:- solve_files('hakank-pddl/monkey-domain.pddl','hakank-pddl/monkey-prob01.pddl').

*/

%:- debug,must(test_primaryobjects).
 
%:- time(test_frolog).


:- endif.

