:- module(logicmoo_old_and_pddl,[test_blocks/0,test_domain/1,test_all/0,test_rest/0,test_sas/0,test_dir_files_sas/1,test_dir_files_sas/3]).
%:- set_prolog_flag(gc,true).

:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.
:- prolog_load_context(directory,Dir),
   DirFor = planner,
   (( \+ user:file_search_path(DirFor,Dir)) ->asserta(user:file_search_path(DirFor,Dir));true),
   absolute_file_name('../../../..',Y,[relative_to(Dir),file_type(directory)]),
   (( \+ user:file_search_path(pack,Y)) ->asserta(user:file_search_path(pack,Y));true).
:- attach_packs.
:- initialization(attach_packs).
% [Required] Load the Logicmoo Library Utils
:- user:ensure_loaded(library(logicmoo/util/logicmoo_util_all)).

:- if_startup_script->true;throw(not_startup_script_PDDL).

:- use_module(library(clpfd)).
:- use_module(library(dif)).
:-export(user:my_pfc_add/1).
user:my_pfc_add(A):-if_defined(pfc_add(A),assert_if_new(A)).

:- include(logicmoo_hyhtn).

% :- qcompile_libraries.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FILENAME:  common.pl 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file contain common predicates that are used in planners

% :- dynamic(domain/3).
% :- dynamic(pairfrom/4).

:- dynamic(is_saved_type/3).


% :- set_prolog_flag(gc,true).

:- initialization( profiler(_,cputime) ).
:- initialization(user:use_module(library(swi/pce_profile))).


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

must_filematch(A,B):-must((filematch(A,B))).


test_all:-test_all(7).
test_all(N):- 
  must_filematch('./orig_pddl_parser/test/?*?/domain*.pddl',_),!,
  (forall(must_filematch('./orig_pddl_parser/test/?*?/domain*.pddl',E),once(test_domain(E,N)))).

test_all(N):- expand_file_name('./orig_pddl_parser/test/?*?/domain*.pddl',RList),RList\=[],!,reverse(RList,List),
  forall(member(E,List),once(test_domain(E,N))).

test_primaryobjects:- 
  (forall(must_filematch('./primaryobjects_strips/?*?/domain*.*',E),once(test_domain(E)))). 

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


save_domain(Named,O):-my_pfc_add(is_saved_type(domain,Named,O)).
save_problem(Named,O):-my_pfc_add(is_saved_type(problem,Named,O)).
save_sterm(O):-gensym(sterm,Named),my_pfc_add(is_saved_type(sterm,Named,O)).

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
 forall(must(must_filematch(DomainFile,DomainFile0)),
   forall(must(must_filematch(ProblemFile,ProblemFile0)),
     (time(solve_files_0(DomainFile0, ProblemFile0))))).

solve_files_0(DomainFile, ProblemFile):-
  format('~q.~n',[solve_files(DomainFile, ProblemFile)]),
   must_det_l(( parseDomain(DomainFile, DD),
    parseProblem(ProblemFile, PP),
    term_to_ord_term(DD, D),prop_get(domain_name,D,DName),save_problem(DName,D),
    term_to_ord_term(PP, P),get_problem_name(P,PName),save_problem(PName,P),    
    reset_statistic)),
    !,
    record_time(try_solve(D,P,S),SolverTime),
    flag(time_used,X,X + SolverTime),
    show_statistic(P, S),
    !.


record_time(G,TimeUsed):- record_time(G,runtime,TimeUsed).
record_time(G,Runtime,TimeUsed):- statistics(Runtime, [B,_]),G,statistics(Runtime, [E,_]),TimeUsed is E - B.


% try_solve(D,P,S):-portray_clause(solve:-try_solve(D,P,S)),fail.
% try_solve(D,P,S):- once(time_out(solve(D, P, S), 3000, Result)), Result == time_out, portray_clause(hard_working:-try_solve(D,P,S)),fail.
try_solve(D,P,S):- gripe_time(14,time_out((solve(D, P, S)), 30000, Result)),!, % time limit for a planner (was 500000)
   ((\+ is_list(S)
     -> portray_clause('failed'(Result):-try_solve(D,P,S)) ;
       ((Result=time_out)->portray_clause('failed'(Result):-try_solve(D,P,S));true))),!.

try_solve(D,P,S):-dmsg('utter_failed'(warn):-try_solve(D,P,S)),!.



% solve(+Domain, +Problem, -Solution).
%
%   Set domain and problem on blackboard
%
:-thread_local(thlocal:other_planner/1).
solve(D,P,S):- thlocal:other_planner(C),logOnError(call(C,D,P,S)),!.
solve(D, P, Solution):-
  must_det_l((
    bb_put(currentProblem, P),
    bb_put(currentDomain, D),
    get_problem_init(P, UCI),
    get_problem_goal(P, UCG),
    copy_term_for_solve((UCI,UCG),(I,G)),
    prop_get(domain_name,D,Mt),    
    must(prop_get(domain_name,P,Mt)),
    % abolish(actn,2),

    get_domain_actions(D, A),
    maplist(save_varnames_in_action,A,CA),
    bb_put(actions, CA), maplist(save_actions(Mt), CA),

    bb_put(goalState, G),        
    bb_put(fictiveGoal, G))),!,    
    search(Mt,I, G, Solution).

save_varnames_in_action(A,CA):-varnames_for_assert(A,C,Vars),append_term(C,Vars,CA).

save_actions(Mt,A):- asserta_if_new(actn(Mt,A)).

% term_to_ord_term(+Term, -OrdTerm)
%
%   Go throught the term and look for sets, return the same term
%   with all sets become ordered.
%

term_to_ord_term(Term, OrdTerm):-t2ot(Term, OrdTerm).

t2ot(A, A):- \+ compound(A), !.
t2ot(vv(T), T):-!.
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
get_domain_actions(     	domain(_, _, _, _, _, _, _, A), A).    
get_problem_name(	problem(N, _, _, _, _, _, _, _, _), N).
get_problem_init(   problem(_, _, _, _, I, _, _, _, _), I).
get_problem_goal(    problem(_, _, _, _, _, G, _, _, _), G).
% get_metric(    problem(_, _, _, _, _, _, _, M, _), M).
% get_problem_objects(    problem(_, _, _, O, _, _, _, _, _), O).


% get_precondition(	action(_, _, P, _, _, _, _UT), P).
% get_positiv_effect(	action(_, _, _, PE, _, _, _UT), PE).
% get_negativ_effect(	action(_, _, _, _, NE, _, _UT), NE).

% get_assign_effect(	action(_, _, _, _, _, AE, _UT), AE).
% unused get_parameters(    action(_, P, _, _, _, _, _UT), P).
/*
% unused get_action_def(    action(Name, Params, _, _, _, _,UT), F):-
    untype(Params, UP),
    F =.. [Name|UP].
*/


record_onto_var(AttribName,AV,Value):-ignore((atom(Value),var(AV), (get_attr(AV,logicmoo_old_and_pddl,Dict)->true;(rb_new(Dict),put_attr(AV,logicmoo_old_and_pddl,Dict))),
   prop_get(AttribName,Dict,_)->true;prop_put(AttribName,Dict,AV))).

attr_unify_hook(_,_).
attr_portray_hook(Value, _Var) :- nonvar(Value),!,write((Value)).

record_var_names(V):- \+ compound(V),!.
record_var_names(N=V):-!,record_var_names(V,N).
record_var_names(List):-is_list(List),!,maplist(record_var_names,List).
record_var_names(Comp):-functor(Comp,F,A),arg(A,Comp,E),!,record_var_names(E).

record_var_names(ATTVAR,Value):-record_onto_var(varname,ATTVAR,Value).


record_var_type(ATTVAR,Type):-record_onto_var(type,ATTVAR,Type).

get_action_info(Mt,A):- actn(Mt,A).

get_constrained_action(Mt,action(S, PTs, Precon, Pos, Neg, Assign, UT )):-get_action_copy(Mt,action(S, PTs, Precon, Pos, Neg, Assign, UT , C, Vars)),
  nop((call(C),record_var_names(Vars))).


% get_constrained_action_bb(Mt,action(S, PTs, Precon, Pos, Neg, Assign, UT )):-!,actn(Mt,action(S, PTs, Precon, Pos, Neg, Assign, UT , _C, _Vars)).
get_constrained_action_bb(Mt,action(S, PTs, Precon, Pos, Neg, Assign, UT )):-get_action_bb(Mt,action(S, PTs, Precon, Pos, Neg, Assign, UT , C, Vars)),
  nop((call(C),record_var_names(Vars))).


% get_action(-Action, -ActionDef)
%
get_action_copy(Mt,A):- actn(Mt,A).

% get_action_copy(_Mt,A):- bb_get(actions, As),copy_term(As, Ass),!, member(A, Ass).
%get_action(Mt,A):- actn(Mt,A).

get_action_bb(_Mt,A):- bb_get(actions, As),copy_term(As, Ass),!, member(A, Ass).



all_dif(_):-!.
all_dif([A,B]):-!,dif(A,B),!.
all_dif([A,B,C|_]):-!,dif(A,B),dif(A,C),dif(B,C),!.
all_dif(_).

tk(_,_):-!.
tk(top,_):-!.
tk(K,X):- var(X),!,when(nonvar(X),tk(K,X)).
tk(K,X):- clause(error:has_type(K, X),B),!,show_call(B).
% tk(K,X):- dmsg(tk(K,X)).
tk(_,_).



%% get_param_types0(+Df, +ListOfParams, -NameOrVarList, -TypeList).
%
get_param_types(Df,H,P,K):-must((get_param_types0(Df,H,P,K),length(P,L),length(K,L))).


use_default(s(var),'?'(H),H).
use_default(s(var),(H),H).
use_default(s(val),'?'(H),'?'(H)).
use_default(s(val),H,H).
use_default(Df,_,Df).


adjust_types(T,GsNs,L):- must((get_param_types0(T, GsNs,Ps, Ks),pairs_keys_values(L,Ps, Ks))).

get_param_types0(_,[], [] ,[]).

get_param_types0(Df,[H|T],['?'(Name)|Ps],[K|Ks]):- use_default(Df,'?'(Name),K),
    svar_fixvarname(H,Name),!,
    get_param_types0(Df,T, Ps, Ks).
get_param_types0(Df,[H|T],[P1|Ps],[K|Ks]):-
    compound(H), H =.. [K, P1],not(is_list(P1)),!,
    get_param_types0(Df,T, Ps, Ks).
get_param_types0(Df,[H|T],[P1|Ps],[K|Ks]):-
    compound(H), H =.. [K, [P1]],!,    
    get_param_types0(Df,T, Ps, Ks).
get_param_types0(Df,[H|T],[P1,P2|Ps],[K,K|Ks]):-
    compound(H), H =.. [K, [P1,P2]],!,
    get_param_types0(Df,T, Ps, Ks).
get_param_types0(Df,[H|T],[H|Ps],[K|Ks]):-  use_default(Df,H,K),
    get_param_types0(Df,T, Ps, Ks).


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
    concat_atom_iio(T, D, PTs),
    atom_concat(H, D, Temp),
    atom_concat(Temp, PTs, O).


% copy_term_spec(+Term, -Term)
%
%   Special version of copy_term. variable x represented as ?(x)
%   All occurs of ?(x) are replaced with real prolog variables.
%   Modified version of code published by Bartak: http://kti.mff.cuni.cz/~bartak/prolog/data_struct.html
%

varnames_for_assert(A,B,After):-
     cp(A,[],B,After).

copy_term_for_solve(A,B):-
    cp(A,[],B,After),
    nb_setval('$variable_names',After).

cp(  VAR,Vars,VAR,Vars):- var(VAR),!.
cp(  VAR,Vars,NV,NVars):- svar(VAR,_),!,must((svar_fixvarname(VAR,Name),atom(Name))),!, must(register_var(Name=NV,Vars,NVars)).
cp([H|T],Vars,[NH|NT],NVars):-!,cp_args([H|T],Vars,[NH|NT],NVars).
cp( Term,Vars,Term,Vars):- \+compound(Term),!.
cp( Term,Vars,NTerm,NVars):-    
    Term \= ?(_),
    Term=..[F|Args],    % decompose term
    cp_args(Args,Vars,NArgs,NVars),
    NTerm=..[F|NArgs].  % construct copy term

cp_args([H|T],Vars,[NH|NT],NVars):-
    cp(H,Vars,NH,SVars), cp_args(T,SVars,NT,NVars).
cp_args([],Vars,[],Vars).


% register_var(?, ?, ?)
%
%   During copying one has to remeber copies of variables which can be used further during copying.
%   Therefore the register of variable copies is maintained.
%
register_var(N=V,IN,OUT):-register_var(N,IN,V,OUT).

register_var(N,T,V,OUT):- must(nonvar(N)),
   ((name_to_var(N,T,VOther)-> (OUT=T,samify(V,VOther)) 
     ;
     (nb_getval('$variable_names',Before),       
      (name_to_var(N,Before,VOther)  -> (samify(V,VOther),OUT= [N=V|T]);
         (var_to_name(V,T,_Other)                  -> OUT= [N=V|T];
           (var_to_name(V,Before,_Other)                 -> OUT= [N=V|T];fail)))))).


register_var(N,T,V,OUT):- var(N),
   (var_to_name(V,T,N)                -> OUT=T;
     (nb_getval('$variable_names',Before),        
          (var_to_name(V,Before,N)   -> OUT= [N=V|T];
               OUT= [N=V|T]))),!.


register_var(N,T,V,[N=V|T]).

% different variables (now merged)
samify(V,V0):-must(V=@=V0),V=V0. 

var_to_name(V,[N=V0|T],N):-
    V==V0 -> true ;          % same variables
    var_to_name(V,T,N).

name_to_var(N,T,V):- var(N),!,var_to_name(N,T,V).
name_to_var(N,[N0=V0|T],V):- 
   N0==N -> samify(V,V0) ; name_to_var(N,T,V).





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
%% step(Mt,+State, -ActionDef, -NewState)
%%   Return descendant of State and ActionDefinition that was used.
%%
%% is_goal(State) - is true when State is a goal state.  
%%
%% repeating(Goal1, Goal2):-  Goal1 is the same as Goal2.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- expects_dialect(sicstus).
:-use_module(library(ordsets)).


make_solution(S, S).
    
% step(Mt,+State, -ActionDef, -NewState)
%
%   Return descendant of State and ActionDefinition that was used.
%
step(Mt,State, ActionDef, NewState):-
  %  get_a ction(A, ActionDef),
  %  get_precondition(A, P),    
  get_constrained_action_bb(Mt,action(_S, _L, P, PE, NE, _Assign, ActionDef)),
  
    mysubset(P, State),  % choose suitable action
 %   get_negativ_effect(A, NE),
    ord_subtract(State, NE, State2),
 %   get_positiv_effect(A, PE), 
    ord_union(State2, PE, NewState).


is_goal(_Mt,S):-
    bb_get(goalState, G),
    ord_subset(G, S).

repeating(S1, S2):-
    S1 =@=  S2.

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
h(Mt,S, H):- h_add(Mt,S, H).
% h(S, H):- h_diff(S, H).


%
%   Estimated distance to achieve Goal.
%
h_add(Mt,S, E):-
    bb_get(fictiveGoal, G),
    relax(Mt,S, G, E).
%    write(G-S-E),nl.

relax(_Mt,_, [], 0):-!.
relax(Mt,S, G, E):-
    subtract(G, S, Delta),
    setof(P, relax_step(Mt,S, P), RS),
    ord_union([S|RS], NS),
    relax(Mt,NS, Delta, NE),
    length(Delta, LD),
    E is LD+NE.

relax_step(Mt,State, PE):-
    % get_a ction(A),
    get_constrained_action(Mt,action(_S, _L, P, PE0, _Neg, _Assign, _UT)),
    %get_precondition(A, P),
    
    mysubset(P, State),
    PE0 = PE.
    %get_positiv_effect(A, PE).

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

domainBNF(Output, List, R):- with_assertions(tlbugger:skipMust, debugOnError0(logicmoo_old_and_pddl:domainBNF_dcg(Output,Output, List, R))),!.
domainBNF(Output, List, R):- with_assertions(thlocal:allow_sterm,with_assertions(tlbugger:skipMust, debugOnError0(logicmoo_old_and_pddl:domainBNF_dcg(Output,Output, List, R)))),!,
   portray_clause((domainBNF:-thlocal:allow_sterm,Output)).
domainBNF(P     , List, R):- must(sterm(O, List, R)),!,must(sterm2pterm(O,P)),!,portray_clause((ed:-P)).
domainBNF(Output, List, R):- trace,domainBNF_dcg(_PROPS, Output, List, R),!.

:-export(domainBNF_dcg//2).

svar(Var,Var):-var(Var),!.
svar('$VAR'(Var),Name):-number(Var),format(atom(Name),'~w',['$VAR'(Var)]),!.
svar('$VAR'(Name),Name):-!.
svar('?'(Name),Name):-!.
svar(VAR,Name):-atom(VAR),atom_concat('??',Name,VAR).
svar(VAR,Name):-atom(VAR),atom_concat('?',Name,VAR).


svar_fixvarname(VAR,UP):-svar(VAR,SVAR),!,must(atom(SVAR)),fix_varcase(SVAR,UP).

sterm2pterm(VAR,VAR):-var(VAR),!.
sterm2pterm(VAR,'?'(UP)):-svar_fixvarname(VAR,UP).
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
                        {must(Struct = domain(N, R, T, C, P, F, C, S)), nop((var(OptionsDict)-> rb_new(OptionsDict) ; true))},
                        dcgMust(domainBNF_rest( Struct, R, T, C, P, F, C, S)),
                        {set_nb_propval(Struct,domain_name,N)}.
                        
domainBNF_rest(Struct, R, T, C, P, F, Dconstraints, ActionsAndRest ) --> 
                            dcgMust(require_def(R)    ; []),
                             dcgMust(types_def(T)      ; []), %:typing
                             dcgMust(constants_def(C)  ; []),
                             dcgMust(predicates_def(P) ; []),
                             dcgMust(functions_def(F)  ; []), %:fluents
%                            dcgMust (dconstraints_def(Dconstraints)   ; []),    %:constraints
                             dcgMust(zeroOrMore(structure_def, ActionsAndRest)),
                             [')'],{
                            set_nb_propval(Struct,requires,R)  ,  
                            set_nb_propval(Struct,types,T)    ,   %:typing
                            set_nb_propval(Struct,constants,C) ,
                            set_nb_propval(Struct,predicates,P) ,
                            set_nb_propval(Struct,functions,F), %:fluents
                            set_nb_propval(Struct,dconstraints,Dconstraints)   ,    %:constraints
                            set_nb_propval(Struct,actions, ActionsAndRest),
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
                                --> ['('], predicate(P), typed_list(variable, L), [')'], {F=..[P|L],!}.

predicate(_) --> [P], {P==not,!,fail}.
predicate(P)                    --> name(P).

variable(V)                     --> ['?'], name(N), {fix_varcase(N,VC),  V =.. [?, VC]}.

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


typed_list(W, L) --> typed_list0(W, GsNs),{adjust_types(W,GsNs,L)}.

typed_list0(W, GsNs)           --> oneOrMore(W, N), ['-'], type(T), !, typed_list0(W, Ns), {findall(G,(member(E,N),G =.. [T,E]),Gs), append(Gs,Ns,GsNs)}.
typed_list0(W, N)                --> zeroOrMore(W, N).


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
action_def(action(S, vv(PTs), Precon, Pos, Neg, Assign, UT , []))
                                --> ['(',':',action], action_symbol(S),
                                    [':',parameters,'('], typed_list(variable, L), [')'],
                                    {get_param_types(s(var,type),L,UL,PTs),UT=..[S|UL],!},
                                    action_def_body(Precon, Pos, Neg, Assign),
                                    [')'].
action_symbol(N)                --> name(N).

% Actions definitons
durative_action_def(action(S, vv(PTs), Precon, Pos, Neg, Assign, UT, []))
                                --> ['(',':',daction], action_symbol(S),
                                    [':',parameters,'('], typed_list(variable, L), [')'], 
                                    da_def_body(Precon, Pos, Neg, Assign),
                                    [')'], {get_param_types(s(var,type),L,UL,PTs),UT=..[S|UL],!}.


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

problem(Output, List, R):- with_assertions(tlbugger:skipMust, debugOnError0(logicmoo_old_and_pddl:problem_dcg(Output, List, R))),!.
problem(Output, List, R):- with_assertions(thlocal:allow_sterm,with_assertions(tlbugger:skipMust, debugOnError0(logicmoo_old_and_pddl:problem_dcg(Output, List, R)))),!,
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
   {must((Struct = problem(Name, Domain, R, OD, I, G, UNK, MS, LS))), nop((var(OptionsDict)-> rb_new(OptionsDict) ; true))},
                                     dcgMust(problem_rest(R, OD, I, G, UNK, MS, LS)).

problem_rest(R, OD, I, G, _, MS, LS) --> 
   (require_def(R)         ; []),
   (objects_def(OD) ; []),
   (dcgMust(init(I)) ; []),
   (goal(G) ; []),
%                                    (constraints(C)   ; []), %:constraints
   (metric_spec(MS) ; []),
   (length_spec(LS)  ; []),
   dcgMust([')']),!.


objects_def(L)           --> ['(',':',objects], typed_list_as_list(name, L),[')'].

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
%%%           ...))
%%%           L = ['(', define, '(', domain, blocks, ')', '(', :, requirements|...].

:- expects_dialect(sicstus).

fix_wordcase(Word,WordC):-upcase_atom(Word,UC),UC=Word,!,downcase_atom(Word,WordC).
fix_wordcase(Word,Word).


fix_varcase(Word,WordC):-!,name(Word,[F|R]),to_upper(F,U),name(WordC,[U|R]).
% the cut above stops the rest 
fix_varcase(Word,Word):-upcase_atom(Word,UC),UC=Word,!.
fix_varcase(Word,WordC):-downcase_atom(Word,UC),UC=Word,!,name(Word,[F|R]),to_upper(F,U),name(WordC,[U|R]).
fix_varcase(Word,Word). % mixed case

%
% read_file(+File, -List).
%
read_file( File, Words) :-  exists_file(File), !, seeing(Old),call_cleanup(( see(File), get_code(C), (read_rest(C, Words))),( seen, see(Old))),!.
read_file(File0, Words) :-  must((must_filematch(File0,File),exists_file(File),read_file( File, Words))),!.

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
read_rest(C,[WordC|Words]) :- read_word(C,Chars,Next),
                             name(Word,Chars),fix_wordcase(Word,WordC),
                             read_rest(Next,Words).

/* Space, comma, newline, backspace, carriage-return, 46 , 63,  ( ) period, end-of-file or question mark separate words. */
read_word(C,[],C) :- ( C=32 ; C=44 ; C=10 ; C=9 ; C=13 ;
                         C=46 ; C=63 ; C=40 ; C=41 ; C=58 ; C= -1 ) , !.

/* Otherwise, get characters and convert to lower case. */
read_word(C,[LC|Chars],Last) :- C=LC, % lower_case(C, LC),
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


test_dir_sas(DirIn):-forall(must_filematch(DirIn,DirInM),test_dir_m(DirInM)).
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
% step(Mt,+State, -NewState)
% is_goal(State)
% h(State, Value) 
% repeating(+State, +AnotherState)
:- expects_dialect(sicstus).

:-use_module(library(ordsets)).
:-use_module(library(heaps)).


% search(+InitState, +GoalState, -Solution)
%
search(Mt,I, _, Solution):-
    a_star(Mt,I, Solution, _).
    
    
% a_star(Mt,+InitState, -Actions, -Cost).
%
a_star(Mt,S, A, C):-
    state_record(S, nil, nil, 0, SR),
    list_to_heap([0-SR], PQ),
    a_star(Mt,PQ, [], A, C).


% a_star(Mt,+Queue, +Visited, -Solution, -Cost)
%
a_star(_Mt,PQ, _, 'NO SOLUTION', _):-
  %  write('NO SOLUTION'),nl,
    empty_heap(PQ),
    !.
a_star(Mt,PQ, V, Solution, C):-
    get_from_heap(PQ, C, SR, _),
    state_record(S, _, _, _, SR),
    is_goal(Mt,S),
%    write('FOUND SOLUTION'),nl,
%    state_record(S, _, _, D, SR), write(C-D), write('   '),write(S),nl,
%    writel(V),nl,halt,
    solution(SR, V, Solution).

a_star(Mt,PQ, V, Solution, C):-
    get_from_heap(PQ, _K, SR, RPQ),
    ord_add_element(V, SR, NV),
    (    bagof(K-NS, next_node(Mt,SR, PQ, NV, K, NS), NextNodes) 
         ;
         NextNodes=[]
    ),
%    state_record(S, _, _, D, SR), write(_K-D), write('   '),write(S),length(NextNodes, L), write(L),nl,
%    write(NextNodes),nl,
    add_list_to_heap(RPQ, NextNodes, NPQ),
    stat_node,
    a_star(Mt,NPQ, NV, Solution, C).



% next_node(Mt,+StateRecord, +Queue, +Visited, -EstimateDeep, -NewStateRecord)
%
next_node(Mt,SR, Q, V, E, NewSR):-
    state_record(S, _, _, D, SR),
    step(Mt,S, A, NewS),
    state_record(NewS, _, _, _, Temp),
    \+ my_ord_member(NewS, V),
    heap_to_list(Q, PQL),
    \+ member(Temp, PQL),
    h(Mt,S, H),
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


prop_merge_svo(Struct,Name,Value):-prop_merge(Name,Struct,Value).

prop_merge([Name],Struct,Value):-!,prop_merge(Name,Struct,Value).
prop_merge([Name|More],Struct,Value):-!, prop_get(Name,Struct,Ref),prop_merge(More,Ref,Value).
prop_merge(Name,Struct,ValueIn):- term_to_ord_term(ValueIn,Value),
   (prop_get(Name,Struct,Old) -> merge_values(Old,Value,New) ;  Value=New),
   prop_set(Name,Struct,New),!.


prop_put([Name],Struct,Value):-!,prop_set(Name,Struct,Value).
prop_put([Name|More],Struct,Value):-!, prop_get(Name,Struct,Ref),prop_put(More,Ref,Value).
prop_put(Name,Struct,ValueIn):-  term_to_ord_term(ValueIn,Value),
   prop_set(Name,Struct,Value).


prop_set(_,    RBTree,   _):- (\+ \+ RBTree=[] ),!, fail.
prop_set(Name, bb,   Value):- !, bb_put(Name,Value).
prop_set(_,    Struct,   _):- ( \+ compound(Struct)),!,fail.
prop_set(Name,RBTree,Value):- prop_put_dict(Name,RBTree,Value, NewDict),!,must(RBTree=NewDict).
prop_set(Name,Struct,Value):- integer(Name),!, nb_setarg(Name,Struct,Value).
prop_set(Name,Struct,Value):- user:named_arguments_template(Names,_,_,Struct),
     arg(N,Names,Name), !,  nb_setarg(N,Struct,Value).

prop_set(Name,Struct,Value):- functor(Struct,_,A),arg(A,Struct,RBTree),
   must(prop_put_dict(Name,RBTree,Value,NewDict)),!,
   (RBTree == NewDict -> true ; nb_setarg(A,Struct,NewDict)).

prop_put_dict(Name,RBTree,Value,NewDict):-var(RBTree),!,trace_or_throw(prop_put_dict(Name,RBTree,Value,NewDict)).

prop_put_dict(Name,RBTree,Value,NewDict):- nonvar(RBTree),
  ( is_rbtree(RBTree) -> (nb_rb_insert(RBTree,Name,Value),RBTree=NewDict) ;
    ( is_dict(RBTree) -> 
        (get_dict(Name,RBTree,Old) ->
          (Value==Old -> (RBTree = NewDict); ((nb_set_dict(Name,RBTree,Value), RBTree = NewDict)))
            ;
          put_dict(Name,RBTree,Value,NewDict)) ;   fail  )),!.


merge_values(Var,Value,Value):-var(Var),!.
merge_values([], Value,Value).
merge_values(Old,Value,Value):-Old==Value,!.
merge_values(Old,Value,New):-is_list(Old),!,(is_list(Value)->ord_union(Old,Value,New);ord_add_element(Old,Value,New)).
merge_values(Old,Value,[Value,Old]).


% Collection of shortcuts

:-export(user:named_arguments/1).
:-multifile(user:named_arguments/1).
:-dynamic(user:named_arguments/1).

:-export(user:named_arguments_template/4).
:-multifile(user:named_arguments_template/4).
:-dynamic(user:named_arguments_template/4).

:-user:my_pfc_add(prologHybrid(user:named_arguments_template(ftCompound, ftAtom, ftInt, ftCompound))).

user:named_arguments(domain(domain_name, requires, types, constants, predicates, functions, constraints, actions)).
user:named_arguments(problem(problem_name, domain_name, requires, objects, init, goal, constraints, metric, length)).
user:named_arguments(action(domain_name,action_name,parameters,preconditions,positiv_effect,negativ_effect,assign_effect,constraints,varnames)).

    
user:argtype_to_arg(dict, NewArg):-dict_create(NewArg,extended,[]),!.
user:argtype_to_arg(rb,   NewArg):-rb_new(NewArg),!.
user:argtype_to_arg(assoc,NewArg):-empty_assoc(NewArg),!.
user:argtype_to_arg(_,_).

cnames_to_sargs(Names,F,A,Struct) :- compound(Names),
    functor(Names,F,A),functor(Struct,F,A),arg(A,Names,SubArg),arg(A,Struct,NewArg),user:argtype_to_arg(SubArg,NewArg),!.

:-forall((user:named_arguments(Names),cnames_to_sargs(Names,F,A,Struct)),asserta_if_new(user:named_arguments_template(Names,F,A,Struct))).



prop_get(Name, Struct,  Value):-prop_get_0(Name, Struct,  Value),!.
prop_get(Name, _Struct, Value):- bb_get(Name,Value),!.

prop_get_0(_,       RBTree, _  ):- (\+ \+ RBTree=[] ),!, fail.
prop_get_0(Name, bb,      Value):- !, must(bb_get(Name,Value)).
prop_get_0(_   , Atomic,  _    ):- atomic(Atomic),!,fail.
prop_get_0(Name, Dict,    Value):- is_dict(Dict),!,get_dict(Name,Dict,Value).
prop_get_0(Name, RBTree,  Value):- is_rbtree(RBTree),!,nb_rb_get_node(RBTree,Name,Value).
prop_get_0(Name, Struct,  Value):- user:named_arguments_template(Names,_,_,Struct), arg(N,Names,Name),!,arg(N,Struct,Value).
prop_get_0(Indx, Struct,  Value):- integer(Indx),!, arg(Indx,Struct,Value).
prop_get_0(Name, Struct,  Value):- functor(Struct,_,A),arg(A,Struct,Ref),nonvar(Ref),!,prop_get_0(Name,Ref,Value).

/*

prop_get(Name,mutable(RBTree),Value):-!,nonvar(RBTree),prop_get(Name,RBTree,Value).


?- 
  prop_get(uses_domain, problem('blocks-3-0',blocks,[],[block([a,b,c])],[handempty,clear(a),clear(b),clear(c),ontable(a),ontable(b),ontable(c)],
   [on(b,a),on(c,b)],[],[],[],extended{constraints:[],goal:[on(b,a),on(c,b)],init:[handempty,clear(a),clear(b),clear(c),ontable(a),ontable(b),ontable(c)],
     length:[],metric:[],object:[block([a,b,c])],
      problem_filename:'/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/planner/orig_pddl_parser/test/blocks/blocks-03-0.pddl',
      problem_name:'blocks-3-0',requires:[],uses_domain:blocks}),X).



  ?-

   Y = problem('blocks-3-0',blocks,[],[block([a,b,c])],[handempty,clear(a),clear(b),clear(c),ontable(a),ontable(b),ontable(c)],[on(b,a),
     on(c,b)],[],[],[],extended{constraints:[],goal:[on(b,a),on(c,b)],init:[handempty,clear(a),clear(b),clear(c),ontable(a),
     ontable(b),ontable(c)],length:[],metric:[],object:[block([a,b,c])],problem_filename:
     '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/planner/orig_pddl_parser/test/blocks/blocks-03-0.pddl',
     problem_name:'blocks-3-0',requires:[],uses_domain:blocks}).

   ?- prop_get(init, $Y , O).

   ?- prop_merge(init, $Y , suey(y)).

      ?- prop_get(init, $Y , O).


   ?- prop_set(init, $Y , suey(y)).


*/


new_struct(Name,Struct):- user:named_arguments_template(_,Name,_,StructOrig),!,copy_term(StructOrig,Struct),!.

new_struct(Name,Struct):- var(Name),!,trace_or_throw(var_new_struct(Name,Struct)).

new_struct(Name,Struct):- rb_new(O),rb_insert_new(O,class,Name,Struct),!.
new_struct(Name,Struct):- rb_insert_new(_O,class,Name,Struct),!.



:- module(logicmoo_old_and_pddl).

domain_name(Name):- bb_get(currentProblem, P),!,must(arg(2,P,Name)).
domain_name(Name):- bb_get(currentDomain, D),!,must(arg(1,D,Name)).


% Sorts
sorts(chameleonWorld,primitive_sorts,[door,flexarium,chameleon,box,substrate]).
sorts(DName,primitive_sorts,TypesList):-bb_get(currentDomain, D),arg(1,D,DName),!,prop_get(types,D,TypesList).
sorts(DName,primitive_sorts,List):-nonvar(DName),findall(S,is_a_type(DName,S),List).


is_a_type(D,S):-loop_check(is_a_type0(D,S)).
is_a_type0(D,S):-sorts(D,_,L),member(S,L).
is_a_type0(Name,S):-bb_get(currentProblem,P),prop_get(domain_name,P,Name),objects(P,S,_).


pname_to_dname(t7,chameleonWorld).
pname_to_dname(PName,DName):-name_to_problem_struct(PName,P),!,prop_get(domain_name,P,DName).
pname_to_dname(_,DName):-domain_name(DName).

% Objects

objects(Name,Type,List):- name_to_problem_struct(Name,P),
   prop_get(objects,P,ObjsDef),
   member(Objs,ObjsDef),Objs=..[Type,List].

objects(t7,door,[door1]).
objects(t7,flexarium,[flexarium1]).
objects(t7,chameleon,[veiledChameleon]).
objects(t7,box,[box1,box2]).
objects(t7,substrate,[newsPaper1,newsPaper2]).

predicates(Name,Type,List):- name_to_domain_struct(Name,D),   
   prop_get(predicates,D,List).


% Predicates
predicates(chameleonWorld,[
    doorOpen(door),
    doorClosed(door),
    dirty(flexarium),
    clean(flexarium),
    inBox(chameleon,box),
    inHands(chameleon),
    inFlexarium(chameleon),
    boxOpen(box),
    boxClosed(box),
    insideFlexarium(substrate),
    outsideFlexarium(substrate)]).

% Object Class Definitions
substate_classes(chameleonWorld,door,Door,[
    [doorOpen(Door)],
    [doorClosed(Door)]]).
substate_classes(chameleonWorld,flexarium,Flexarium,[
    [dirty(Flexarium)],
    [clean(Flexarium)]]).
substate_classes(chameleonWorld,chameleon,Chameleon,[
    [inBox(Chameleon,Box)],
    [inHands(Chameleon)],
    [inFlexarium(Chameleon)]]).
substate_classes(chameleonWorld,box,Box,[
    [boxOpen(Box)],
    [boxClosed(Box)]]).
substate_classes(chameleonWorld,substrate,Substrate,[
    [insideFlexarium(Substrate)],
    [outsideFlexarium(Substrate)]]).

% Atomic Invariants

% Implied Invariants

% Inconsistent Constraints

% Operators
operator(chameleonWorld,takeOutFlex(Door,Chameleon),
    % prevail
    [     se(door,Door,[doorOpen(Door)])],
    % necessary
    [     sc(chameleon,Chameleon,[inFlexarium(Chameleon)]=>[inHands(Chameleon)])],
    % conditional
    []).
operator(chameleonWorld,putInBox(Box,Chameleon),
    % prevail
    [     se(box,Box,[boxOpen(Box)])],
    % necessary
    [     sc(chameleon,Chameleon,[inHands(Chameleon)]=>[inBox(Chameleon,Box)])],
    % conditional
    []).
operator(chameleonWorld,takeOutBox(Box,Chameleon),
    % prevail
    [     se(box,Box,[boxOpen(Box)])],
    % necessary
    [     sc(chameleon,Chameleon,[inBox(Chameleon,Box)]=>[inHands(Chameleon)])],
    % conditional
    []).
operator(chameleonWorld,putInFlex(Door,Substrate,Flexarium,Chameleon),
    % prevail
    [     se(door,Door,[doorOpen(Door)]),
     se(substrate,Substrate,[insideFlexarium(Substrate)]),
     se(flexarium,Flexarium,[clean(Flexarium)])],
    % necessary
    [     sc(chameleon,Chameleon,[inHands(Chameleon)]=>[inFlexarium(Chameleon)])],
    % conditional
    []).
operator(chameleonWorld,openDoor(Door),
    % prevail
    [],
    % necessary
    [     sc(door,Door,[doorClosed(Door)]=>[doorOpen(Door)])],
    % conditional
    []).
operator(chameleonWorld,closeDoor(Door),
    % prevail
    [],
    % necessary
    [     sc(door,Door,[doorOpen(Door)]=>[doorClosed(Door)])],
    % conditional
    []).
operator(chameleonWorld,time(Flexarium),
    % prevail
    [],
    % necessary
    [     sc(flexarium,Flexarium,[clean(Flexarium)]=>[dirty(Flexarium)])],
    % conditional
    []).
operator(chameleonWorld,wash(Chameleon,Box,Door,Substrate,Flexarium),
    % prevail
    [     se(chameleon,Chameleon,[inBox(Chameleon,Box)]),
     se(door,Door,[doorOpen(Door)]),
     se(substrate,Substrate,[outsideFlexarium(Substrate)])],
    % necessary
    [     sc(flexarium,Flexarium,[dirty(Flexarium)]=>[clean(Flexarium)])],
    % conditional
    []).
operator(chameleonWorld,addCleanNewspaper(Flexarium,Door,Chameleon,Box,Substrate),
    % prevail
    [     se(flexarium,Flexarium,[clean(Flexarium)]),
     se(door,Door,[doorOpen(Door)]),
     se(chameleon,Chameleon,[inBox(Chameleon,Box)])],
    % necessary
    [     sc(substrate,Substrate,[outsideFlexarium(Substrate)]=>[insideFlexarium(Substrate)])],
    % conditional
    []).
operator(chameleonWorld,removeDirtyNewspaper(Flexarium,Door,Chameleon,Box,Substrate),
    % prevail
    [     se(flexarium,Flexarium,[dirty(Flexarium)]),
     se(door,Door,[doorOpen(Door)]),
     se(chameleon,Chameleon,[inBox(Chameleon,Box)])],
    % necessary
    [     sc(substrate,Substrate,[insideFlexarium(Substrate)]=>[outsideFlexarium(Substrate)])],
    % conditional
    []).
operator(chameleonWorld,openBox(Box),
    % prevail
    [],
    % necessary
    [     sc(box,Box,[boxClosed(Box)]=>[boxOpen(Box)])],
    % conditional
    []).
operator(chameleonWorld,closeBox(Box),
    % prevail
    [],
    % necessary
    [     sc(box,Box,[boxOpen(Box)]=>[boxClosed(Box)])],
    % conditional
    []).


operator(chameleonWorld,removeDirtyNewspaper(Flexarium,Door,Chameleon,Box,Substrate),
    % prevail
    [     se(flexarium,Flexarium,[dirty(Flexarium)]),
     se(door,Door,[doorOpen(Door)]),
     se(chameleon,Chameleon,[inBox(Chameleon,Box)])],
    % necessary
    [     sc(substrate,Substrate,[insideFlexarium(Substrate)]=>[outsideFlexarium(Substrate)])],
    % conditional
    []).


:- style_check(+singleton).

op_action(Mt, S, PTs, NPrecon, Pos, Neg, Af, UT , True):- 
   loop_check(operator(Mt,UT,SE,SC,SS)),
   must_det_l((
      True = true,
      UT=..[S|ARGS],
      maplist(lock_var,ARGS),
      HintsIn=[],
      unss_ify(=,HintsIn,ss,SS,Af,Hints1),
      unss_ify(=,Hints1,se,SE,Precon,Hints2),
      unss_ify(=,Hints2,sc,SC,NEGPOS,Hints),
      divide_neg_pos(NEGPOS,[],[],Neg,Pos),   
      append(Precon,Neg,NPrecon),
      maplist(get_type_of(Mt,top,Hints),ARGS,PTs))).

lock_var(X):-when((?=(X,Y);nonvar(X)),X==Y).
unlock_var(V):-del_attrs(V).


add_wrapper(W,In , Out):-Out=..[W,In].

actn_operator(Mt,UT,SE,SC,SS):- 
 get_action_info(Mt,action(_S, PTs, Precon, Pos, Neg, Af, UT , Call, Vars)),
 must_det_l(( 
   UT=..[_|ARGS],
   show_call(Call),   
   maplist(record_var_names,Vars),
   maplist(create_hint,PTs,ARGS,ARGHints),
   conjuncts_to_list(Call,MORE),
   append(ARGHints,MORE,MOREARGHints),
   unss_ify(=,[],MOREARGHints,Precon,se,SEPs,HintsSE),
   unss_ify(=,[],HintsSE,Af,ss,ASEPs,HintsSS),
   unss_ify(add_wrapper(del),[],HintsSS,Neg,ss,NOTS,HintsNEG),
   unss_ify(add_wrapper(add),NOTS,HintsNEG,Pos,ss,NOTSPOSC,HintsPOS),
   mylist_to_set(HintsPOS,Hints),
   maplist(ress_ify(Mt,Def,Hints,se),SEPs,SE),
   maplist(ress_ify(Mt,Def,Hints,ss),ASEPs,SS),   
   maplist(make_rem_adds(Mt,Hints),NOTSPOSC,SC))).

make_rem_adds(Mt,Hints,A1-LIST,sc(Type,A1,(NEG=>POS))):-findall(N,member(del(N),LIST),NEG),findall(N,member(add(N),LIST),POS),
  must(get_type_of(Mt,top,Hints,A1,Type)).

create_hint(K,V,kt(K,V)).

mylist_to_set(HintsIn,HintsM):-must(list_to_set(HintsIn,HintsM)).

ghints(HintsIn,[],HintsIn).
ghints(HintsIn,[G|More],HintsOut):-
   ghints(HintsIn,G,HintsM),
   ghints(HintsM,More,HintsOut).
ghints(HintsIn,G,[kt(F,A),p(F)|HintsIn]):-G=..[F,A],!.
ghints(HintsIn,G,[kta(F,1,A),p(F)|Hints]):-G=..[F,A|ARGS],garg_hints(HintsIn,F,2,ARGS,Hints).

ss_other(ss).
ss_other(sc).
ss_other(se).

garg_hints(HintsIn,_,_,[],HintsIn).
garg_hints(HintsIn,F,N,[A|ARGS],[kta(F,N,A)|Hints]):- 
  N2 is N + 1, garg_hints(HintsIn,F,N2,ARGS,Hints).

unss_ify(_ ,WAS,Hints,G,_,WAS,Hints):-G==['Uninitialized'],!.
unss_ify(GT,WAS,HintsIn,G,SS,OUTS,Hints):-dess_ify(GT,WAS,HintsIn,G,SS,OUT,HintsM),mygroup_pairs_by_key(OUT,OUTS),mylist_to_set(HintsM,Hints).

dess_ify(GT,WAS,HintsIn,G,SS,OUT,Hints):-mylist_to_set(HintsIn,HintsM),HintsIn\=@=HintsM,!,
  dess_ify(GT,WAS,HintsM,G,SS,OUT,Hints).

dess_ify( _,WAS,HintsIn,[],_SS,WAS,HintsIn).

dess_ify(GT,WAS,HintsIn,NC,_SS,OUT,HintsIn):- ( \+ compound(NC)),!,must((call(GT,NC,NCC),append(WAS,[NCC],OUT))).

dess_ify(GT,WAS,HintsIn,[G|GG],SS,OUT,HintsOut):- !,
  dess_ify(GT,WAS,HintsIn,G,SS,MID,Hints1),dess_ify(GT,MID,Hints1,GG,SS,OUT,HintsOut).

dess_ify(GT,WAS,HintsIn,A1-[GLs|GG],SS,OUT,HintsOut):- 
  dess_ify(GT,WAS,HintsIn,A1-GLs,SS,SEs,Hints1),dess_ify(GT,SEs,Hints1,A1-GG,SS,OUT,HintsOut).

dess_ify(GT,WAS,HintsIn,SSG,SS,OUT,[kt(Type,A1)|HintsOut]):- SSG=..[SS,Type,A1,GLs],
  dess_ify(GT,WAS,HintsIn,A1-GLs,SS,OUT,HintsOut).

dess_ify(GT,WAS,HintsIn,SSG,SS,OUT,[kt(Type,A1)|HintsOut]):- SSG=..[SOTHER,Type,A1,GLs],ss_other(SOTHER),
  dess_ify(GT,WAS,HintsIn,A1-GLs,SS,OUT,HintsOut).

dess_ify(GT,WAS,HintsIn,A1-G,_ ,[A1-GO|WAS],HintsOut):- ghints(HintsIn,G,HintsOut),call(GT,G,GO).

dess_ify(GT,WAS,HintsIn,G,SS,OUT,Hints):- arg(1,G,A1), dess_ify(GT,WAS,HintsIn,A1-G,SS,OUT,Hints).

ress_ify(Mt, Def, Hints,SS,A1-Gs,GO):-GO=..[SS,Type,A1,Gs],must(get_type_of(Mt,Def,Hints,A1,Type)),!.
ress_ify(_Mt,Def,_Hints,SS,A1-Gs,GO):-GO=..[SS,Def,A1,Gs],!.




get_type_of(_ , Def, Hints,A1,Type):-member(kt(K,V),Hints),A1==V,K\==Def,!,Type=K,record_var_type(A1,Type).
get_type_of(Mt, Def, Hints,A1,Type):-atom(A1),get_type_of_atom(Mt,Def,Hints,A1,Type),!.
get_type_of(Mt,_Def,_Hints,A1,Type):-nonvar(A1),loop_check(objects(Mt,Type,List)),member(A1,List).

:- style_check(-singleton).
get_type_of_atom(Mt,Def,Hints,veiledChameleon,chameleon).
get_type_of_atom(Mt,Def,Hints,veiledchameleon,chameleon).
get_type_of_atom(Mt,Def,Hints,A1,Type):-pname_to_dname(P,D),is_a_type(D,Type),atom_concat(Type,Num,A1),Num=_.
get_type_of_atom(Mt,Def,Hints,A1,Type):-pname_to_dname(P,D),is_a_type(D,Type),atom_concat(_,Type,A1).
:- style_check(+singleton).



divide_neg_pos([],Neg,Pos,Neg,Pos).
divide_neg_pos([A|MORE],NL2,PL2,Neg,Pos):-divide_neg_pos(A,NL2,PL2,NegM,PosM),divide_neg_pos(MORE,NegM,PosM,Neg,Pos).
divide_neg_pos(NL1=>PL1,NL2,PL2,Neg,Pos):-append(NL1,NL2,Neg),append(PL1,PL2,Pos).

% Methods

% Domain Tasks
planner_task(chameleonWorld,t7,
    % Goals
    [
     se(door,door1,[doorClosed(door1)]),
     se(flexarium,flexarium1,[clean(flexarium1)]),
     se(chameleon,veiledChameleon,[inHands(veiledChameleon)]),
     se(box,box1,[boxClosed(box1)]),
     se(box,box2,[boxClosed(box2)]),
     se(substrate,newsPaper1,[outsideFlexarium(newsPaper1)]),
     se(substrate,newsPaper2,[insideFlexarium(newsPaper2)])],
    % INIT States
    [
     ss(door,door1,[doorClosed(door1)]),
     ss(flexarium,flexarium1,[dirty(flexarium1)]),
     ss(chameleon,veiledChameleon,[inFlexarium(veiledChameleon)]),
     ss(box,box1,[boxOpen(box1)]),
     ss(box,box2,[boxOpen(box2)]),
     ss(substrate,newsPaper1,[insideFlexarium(newsPaper1)]),
     ss(substrate,newsPaper2,[outsideFlexarium(newsPaper2)])]).

planner_task(Domain,Name,
  % Goals
    SEs,SSs):-
     name_to_problem_struct(Name,P),prop_get(domain_name,P,Domain),
     must_det_l((
        % copy_term_for_solve(P,PC),
        get_problem_init(P, UCI),
        get_problem_goal(P, UCG),    
        copy_term_for_solve((UCI,UCG),(I,G)),       
        unss_ify(=,[],[],I,ss,SSK,HintsSS),
        unss_ify(=,[],HintsSS,G,se,SEK,Hints),
        maplist(ress_ify(Mt,Def,Hints,ss),SSK,SSs),
        maplist(ress_ify(Mt,Def,Hints,se),SEK,SEs))).

name_to_problem_struct(Name,P):-Name==current,!,bb_get(currentProblem,P).
name_to_problem_struct(Name,P):-is_saved_type(problem,Name,P).
name_to_problem_struct(Name,P):-bb_get(currentProblem,P),prop_get(problem_name,P,NameO),Name=NameO.
name_to_problem_struct(Name,P):-loop_check(ocl_problem_struct(Name,P)).

kv_to_pddl([], []).
kv_to_pddl([_-N|T0], OUT) :- 
 	kv_to_pddl(T0,VALUES),
        append(N,VALUES,OUT).

ocl_problem_struct(Name,P):- no_repeats(Name,planner_task(DomainName,Name,SE,SS)),
   must_det_l((
      unss_ify(=,[],[],ss,SS,OUTI,HintsM),mygroup_pairs_by_key(OUTI,IOUT),kv_to_pddl(IOUT,I),
      unss_ify(=,[],HintsM,se,SE,OUTG,Hints),mygroup_pairs_by_key(OUTG,GOUT),kv_to_pddl(GOUT,G),
      P = problem(Name, DomainName, [ocl], [], I, G, Hints, /*MS*/ [], /*LS*/ []),
      ignore(loop_check(domain_name(DomainName))))).


name_to_domain_struct(Name,P):-Name==current,!,bb_get(currentDomain,P).
name_to_domain_struct(Name,P):-bb_get(currentDomain,P),prop_get(domain_name,P,NameO),Name=NameO.
name_to_domain_struct(Name,P):-is_saved_type(domain,Name,P).
name_to_domain_struct(Name,P):-loop_check(ocl_domain_struct(Name,P)).

% sorts % consts % preds
ocl_domain_struct(Name,D):- no_repeats(Name,loop_check(predicates(Name,PredsList))),
   % once((not((bb_get(currentDomain,D),once(Name==current;prop_get(domain_name,D,Name)))))),   
   must_det_l((
      findall(S,is_a_type(Name,S),Types),
      D = domain(Name, [ocl], Types, /*Consts*/ [] , PredsList, /*Fuents*/[]  ,/*Constrs*/ [], /*Dconstraints*/[], Actions),
      findall(action(Mt, S, PTs, Precon, Pos, Neg, Af, UT , True),op_action(Mt, S, PTs, Precon, Pos, Neg, Af, UT , True),Actions))).

mygroup_pairs_by_key([], []).
mygroup_pairs_by_key([M-N|T0], [M-ORDERED|T]) :-
 	mysame_key(M, T0, TN, T1),
 	mygroup_pairs_by_key(T1, T),
        mylist_to_set([N|TN],ORDERED).


mysame_key(M0, [M-N|T0], [N|TN], T) :-
 	M0 == M, !,
 	mysame_key(M, T0, TN, T).
mysame_key(_, L, [], L).

:-thread_local(thlocal:loading_files).
:-thread_local(thlocal:hyhtn_solve/1).
% thlocal:other_planner(hyhtn_solve).

new_struct(Name,mutable(O)):- dict_create(O,Name,[]),!.

:- if(gethostname(c3po);gethostname(titan)).

:- set_prolog_flag(gc,false).


:- flag(time_used,_,0).

:- debug,must(test_blocks).

:- solve_files('benchmarks/mystery/domain.pddl','benchmarks/mystery/prob01.pddl').
:- test_domain('benchmarks/driverlog/domain.pddl',4).
:- solve_files('hsp-planners-master/hsp2-1.0/examples/parcprinter-strips/p01-domain-woac.pddl','hsp-planners-master/hsp2-1.0/examples/parcprinter-strips/p01-woac.pddl').

:- test_domain('domains_ocl/chameleonWorld/*domain*').


:- test_all(7).


:- show_call(flag(time_used,W,W)).

:- pce_show_profile.



twhy
  :- show_call(record_time(forall(between(1,1000000,_),forall(get_action_bb(_),true)),_Time1)),
   show_call(record_time(forall(between(1,1000000,_),forall(actn(_,_),true)),_Time2)).

% :- twhy.

% BAD :- test_domain('./elearning/domain.pddl').
% :- test_all.

% 
% :- solve_files('benchmarks/nomystery-sat11-strips/domain.pddl','benchmarks/nomystery-sat11-strips/p01.pddl').
% :- test_domain('./benchmarks/nomystery-sat11-strips/domain.pddl').


/*

:- test_domain('domains_ocl/toasterWorldv2/domain*').

:- solve_files('regression-tests/issue58-domain.pddl','regression-tests/issue58-problem.pddl').
:- forall(must_filematch('./hsp-planners-master/?*?/pddl/?*?/?*domain*.*',E),once(test_domain(E,4))).
:- forall(must_filematch('./hsp-planners-master/?*?/examples/?*?/?*domain*.*',E),once(test_domain(E,5))).

:- test_domain('./benchmarks/nomystery-sat11-strips/domain.pddl').

test_blocks:- fail, test_domain('./benchmarks/nomystery-sat11-strips/domain.pddl',RList),reverse(RList,List),
  forall(member(E,List),once(test_domain(E))).

:-asserta(thlocal:loading_files).

:- forall(must_filematch('./rover/?*?/?*domain*.*',E),once(load_domain(E))).
:- forall(must_filematch('./hsp-planners-master/?*?/pddl/?*?/?*domain*.*',E),once(load_domain(E))).
:- forall(must_filematch('./hsp-planners-master/?*?/examples/?*?/?*domain*.*',E),once(load_domain(E))).
:- forall(must_filematch('./hsp-planners-master/?*?/examples/?*?/?*domain*.*',E),once(load_domain(E))).
:- forall(must_filematch('./primaryobjects_strips/?*?/?*domain*.*',E),once(test_domain(E))).
:- solve_files('hakank-pddl/monkey-domain.pddl','hakank-pddl/monkey-prob01.pddl').

*/

:- endif.

