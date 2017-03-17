%:- module(lp,[test_blocks/0,test_domain/1,test_all/0,test_rest/0,test_sas/0,test_dir_files_sas/1,test_dir_files_sas/3]).
%:- set_prolog_flag(gc,true).
:- op(100,xfy,'=>').
:- debug.
:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.
:- prolog_load_context(directory,Dir),
   DirFor = planner,
   (( \+ user:file_search_path(DirFor,Dir)) ->asserta(user:file_search_path(DirFor,Dir));true),
   absolute_file_name('../../../../',Y,[relative_to(Dir),file_type(directory)]),
   (( \+ user:file_search_path(pack,Y)) ->asserta(user:file_search_path(pack,Y));true).
:- attach_packs.
:- initialization(attach_packs).
% [Required] Load the Logicmoo Library Utils
:- user:ensure_loaded(library(logicmoo/util/logicmoo_util_all)).

:- dynamic(use_local_pddl/0).

use_local_pddl:-throw(uses_local_pddl).


:- if((false,(gethostname(c3po);gethostname(titan)))).

:- initialization(user:use_module(library(swi/pce_profile))).
:- initialization( profiler(_,walltime) ).

:- endif.

% :- use_module(library(clpfd)).
:- use_module(library(dif)).
:-export(user:my_pfc_add/1).
user:my_pfc_add(A):-if_defined(pfc_add(A),assert_if_new(A)).


% :- qcompile_libraries.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FILENAME:  common.pl 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file contain common predicates that are used in planners

% :- dynamic(domain/3).
% :- dynamic(pairfrom/4).

:- dynamic(is_saved_type/3).



:- if(\+current_predicate(init_locl_planner_interface0/4)).
% :- show_call_entry(with_no_mpred_expansions(user:ensure_loaded(planner((logicmoo_hyhtn))))).
:- endif.
% :- set_prolog_flag(gc,true).

:- initialization( profiler(_,cputime) ).
:- initialization(user:use_module(library(swi/pce_profile))).



:- expects_dialect(sicstus).
:- use_module(library(timeout)).
:- use_module(library(lists)).
:- user:ensure_loaded(library(logicmoo/util/logicmoo_util_structs)).
%:- user:ensure_loaded(library(logicmoo/common_logic/common_logic_sexpr)).

:- decl_struct(domain(domain_name, requires, types, constants, predicates, functions, constraints, actions, dict(extraprops))).
:- decl_struct(problem(problem_name, domain_name, requires, objects, init, goal, constraints, metric, length, dict(extraprops))).


:- decl_struct(action5(parameters=unk,sorted(preconditions),sorted(positiv_effect),sorted(negativ_effect),dict(extraprops))).

:- decl_argtypes(action(parameters=unk,sorted(preconditions),sorted(positiv_effect),sorted(negativ_effect),
    assign_effect,list(parameter_types),string(domain_name),list(varnames),
     dict(extraprops))).
:- decl_struct(
   action(string(action_name),list(parameter_types),sorted(preconditions),sorted(positiv_effect),sorted(negativ_effect),sorted(assign_effect),
        callable(parameters),callable(constraints),dict(extraprops))).



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

must_filematch(string(A),string(B)):-!.
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

test_domain(DP):- t_l:loading_files,!,must(load_domain(DP)).
test_domain(DP):- test_domain(DP,12).

test_domain(DP,Num):- \+ atom(DP),forall((filematch(DP,FOUND),exists_file(FOUND)),test_domain(FOUND,Num)),!.
test_domain(DP,Num):- \+ exists_file(DP),!, forall(filematch(DP,MATCH),(exists_file(MATCH),test_domain(MATCH,Num))).
test_domain(DP,Num):-
   format('~q.~n',[test_domain(DP)]),
  directory_file_path(D,_,DP),directory_files(D,RList),reverse(RList,ListR),
   sort(ListR,ListS),length(ListR,PosNum),min_sas(PosNum,Num,MinNum),length(List,MinNum),append(List,_,ListS),!,
   forall(member(T,List),ignore((directory_file_path(D,T,TP),exists_file(TP),not(same_file(DP,TP)),
  solve_files(DP,TP)))).

load_domain(string(DP)):-!,load_file(string(DP)).
test_domain(DP):- \+ atom(DP),forall((filematch(DP,FOUND),exists_file(FOUND)),test_domain(FOUND)),!.
load_domain(DP):- \+ exists_file(DP),!, forall(filematch(DP,MATCH),((exists_file(MATCH),load_domain(MATCH)))).
load_domain(DP):-
   format('~q.~n',[load_domain(DP)]),
  directory_file_path(D,_,DP),directory_files(D,RList),   
   forall(member(T,RList),ignore((directory_file_path(D,T,TP),exists_file(TP),load_file(TP)))).

load_file(F):- must(read_file(F, L, Filename)),load_file_rest(Filename,L).
load_file_rest(_,[]):-!.
load_file_rest(F,L):- first_n_elements(L,10,ES),
   (
   (append(_,['define','(','domain',Named|_],ES),must_det_l((domainBNF(O, L, R1),prop_set(filename,O,F)))) ->  save_type_named(domain,Named,O);
   (append(_,['(','problem',Named|_],ES),must_det_l((problem(O, L, R1),prop_set(filename,O,F)))) ->  save_type_named(problem,Named,O);
    must((ensure_struct(sexpr_file,O),prop_set(filename,O,F),sterm(SO, L, R1),prop_set(sterm_value,O,SO)))),
   load_file_rest(F,R1).

:-export(z2p/2).
z2p(A,A).

save_type_named(Type,Named,O):- doall(retract((is_saved_type(Type,Named,_):-_))),nop(ain((is_saved_type(Type,Named,A):-z2p(O,A)))).
save_sterm(O):-nop((gensym(sterm,Named),save_type_named(sterm,Named,O))).

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
     (time(show_call(solve_files_0(DomainFile0, ProblemFile0))),
      time(show_call(solve_files_w_ocl(DomainFile0, ProblemFile0)))))).

solve_files_0(DomainFile, ProblemFile):-
   must_det_l(( 
      format('~q.~n',[solve_files(DomainFile, ProblemFile)]))),
   parseDomain(DomainFile, DD),
    parseProblem(ProblemFile, PP),
    solve_files_ddpp(DD, PP).


solve_files_0(DomainFile, ProblemFile):-
   must_det_l(( 
  format('~q.~n',[solve_files(DomainFile, ProblemFile)]),
      parseDomain(DomainFile, D),
      prop_get(domain_name,D,DName),
      save_type_named(domain,DName,D),
      parseProblem(ProblemFile, P),
      prop_get(problem_name,P,PName),
      save_type_named(problem,PName,P),
      compile_domain(D,Dc),
      compile_problem(P,Pc),
      reset_statistic)),
    !,
    record_time(try_solve(PName,Dc,Pc,S),SolverTime),
    flag(time_used,X,X + SolverTime),
    show_statistic(P, S),
    !.


solve_files_ddpp(DD, PP):-
   must_det_l(( 
    term_to_ord_term(DD, D),prop_get(domain_name,D,DName),save_type_named(domain,DName,D),
    term_to_ord_term(PP, P),prop_get(problem_name,P,PName),save_type_named(problem,PName,P),    
    reset_statistic)),
    !,
    record_time(try_solve(PName, D,P,S),SolverTime),
    flag(time_used,X,X + SolverTime),
    show_statistic(P, S),
    !.


solve_files_w_ocl(DomainFile, ProblemFile):-
   must_det_l(( 
      format('~q.~n',[solve_files(DomainFile, ProblemFile)]))),
   parseDomain(DomainFile, DD),
    parseProblem(ProblemFile, PP),
    solve_files_w_ocl_pt2(DD, PP).

solve_files_w_ocl_pt2(DD, PP):-
   must_det_l(( 
    term_to_ord_term(DD, D),prop_get(domain_name,D,DName),save_type_named(domain,DName,D),
    term_to_ord_term(PP, P),prop_get(problem_name,P,PName),save_type_named(problem,PName,P),    
    reset_statistic)),
    !,
   with_assertions(t_l:other_planner(hyhtn_solve), record_time(try_solve(PName, D,P,S),SolverTime)),
    flag(time_used_other,X,X + SolverTime),
    show_statistic(P, S),
    !.



compile_problem(Pu,P):-
 must_det_l((
 replc_structure_vars(Pu,Pu0),
  term_to_ord_term(Pu0,P),
  prop_get(init,P, UCI),
  prop_get(goal,P, UCG),
  term_to_ord_term((UCI,UCG),OT),
  copy_term_for_assert(OT,(I,G)),
  prop_set(init,P, I),
  prop_set(goal,P, G),
  prop_get(problem_name,P,PName),
  save_type_named(problem,PName,P))).

compile_domain(D,Dc):-
 must_det_l((
    prop_get(domain_name,D, DName),
    prop_get(actions,D,A),
    must_maplist(copy_term_spec,A,AC),
    prop_set(actions,D,AC),
    must_maplist(to_action5,AC,A5),
    prop_set(actions5,D,A5),
    must_maplist(save_action(DName), A5),    
    D = Dc,  % replc_structure_vars2(D,Dc),
    save_type_named(domain,DName,Dc))).

save_varnames_in_action(A,CA):-varnames_for_assert(A,CA,Vars),prop_set(varnames,CA,Vars).

% DMILES
to_action5(action(_S, _L, P, PE, NE, Assign, ActionDef,_,_),action5(ActionDef, P, PE, NE,Assign)):-!.
to_action5(Struct,Term):-
  prop_get_nvlist(Struct,[parameters=UT,preconditions=Precon,positiv_effect=Pos,negativ_effect=Neg,assign_effect=Af]),
  Term=action5(UT, Precon, Pos, Neg, Af).
     
save_action(Mt,A):- ain(actn(Mt,A)).



record_time(G,TimeUsed):- record_time(G,runtime,TimeUsed).
record_time(G,Runtime,TimeUsed):- statistics(Runtime, [B,_]),G,statistics(Runtime, [E,_]),TimeUsed is E - B.


try_solve(PN,D,P,S):- t_l:loading_files,!,pmsg((loading_files(PN):-try_solve(D,P,S))),!.
% try_solve(PN,D,P,S):- once(time_out(solve(PN,D, P, S), 3000, Result)), Result == time_out, portray_clause(hard_working:-try_solve(PN,D,P,S)),fail.
try_solve(PN,D,P,S):- gripe_time(14,time_out((solve(PN,D, P, S)), 30000, Result)),!, % time limit for a planner (was 500000)
   ((\+ is_list(S)
     -> portray_clause('failed'(Result):-try_solve(PN,D,P,S)) ;
       ((Result=time_out)->portray_clause('failed'(Result):-try_solve(PN,D,P,S));true))),!.

try_solve(PN,D,P,S):-dmsg('utter_failed'(warn):-try_solve(PN,D,P,S)),!.

sdmsg(B,B):- \+ compound(B),!.
sdmsg((D:-B),SS):-B==true,!,sdmsg(D,SS).
sdmsg(B,SS):-B=..[F|A],must_maplist(sdmsg,A,AA),SS=..[F|AA].

pmsg(D):- sdmsg(D,SS),D \=@= SS, !,pmsg(SS).
pmsg(D):- compound(D),functor(D,(:-),_),!,subst(D,=,'k_===_v',SS),wdmsg(SS).
pmsg(D):- subst(D,=,'k_===_v',SS),wdmsg(SS),wdmsg((:-SS)).


% solve(PN,+Domain, +Problem, -Solution).
%
%   Set domain and problem on blackboard
%
:-thread_local(t_l:other_planner/1).
solve(PN,D,P,S):- t_l:other_planner(C),!,logOnError(call(C,PN,D,P,S)),!.

solve(_,D, P, Solution):-
  must_det_l((
    bb_put(currentProblem, P),
    bb_put(currentDomain, D),
    prop_get(init,P, UCI),
    prop_get(goal,P, UCG),
    copy_term_for_assert((UCI,UCG),(I,G)),
    prop_get(domain_name,D,Mt),    
    must(prop_get(domain_name,P,Mt)),
    % abolish(actn,2),

    prop_get(actions,D, A),
    must_maplist(save_varnames_in_action,A,CA),
    bb_put(actions, CA), must_maplist(save_action(Mt), CA),

    bb_put(goalState, G),        
    bb_put(fictiveGoal, G))),!,    
    search(da(Mt,A,A5),I, G, Solution).

solve(_PN,D,P,Solution):-
  must_det_l((
    bb_put(currentProblem, P), 
    bb_put(currentDomain, D),    
    prop_get(init,P, I),
    prop_get(goal,P, G),
    copy_term_for_assert((I,G),(Ic,Gc)),
    prop_get(domain_name,D,Mt),
    must(prop_get(domain_name,P,Mt)),
    prop_get(actions,D, A),
    prop_get(actions5,D, A5),
    bb_put(goalState, G), 
    bb_put(fictiveGoal, G))),    
    search(da(Mt,A,A5),Ic, Gc, Solution).
    


hyhtn_solve(_,D, P, Solution):-
  must_det_l((
    logicmoo_hyhtn:env_clear_doms_and_tasks,
    logicmoo_hyhtn:clean_problem,
    bb_put(currentProblem, P),
    bb_put(currentDomain, D),
    prop_get(init,P, UCI),
    prop_get(goal,P, UCG),


    copy_term_for_assert((UCI,UCG),(I,G)),
    prop_get(domain_name,D,Mt),    
    must(prop_get(domain_name,P,Mt)),
    must(prop_get(types,D,Types)),
    must(prop_get(predicates,D,Preds)),
    must(prop_get(objects,P,Objects)),
    must_maplist(save_ocl_objects,Objects),
    must_maplist(save_ocl_predicates,Preds),
    must_maplist(save_ocl_types,Types),
    wdmsg(dtpo(Mt,Types,Preds,Objects)),
    prop_get(actions,D, A),
    must_maplist(save_ocl_operators,A),
    bb_put(goalState, G),        
    bb_put(fictiveGoal, G))),
    ignore(logicmoo_hyhtn:init_locol_planner_interface0(G,I,Solution)).

save_ocl_operators(A):-dmsg(save_ocl_operators(A)), % varnames_for_assert(A,CA,Vars),
   must(( 
      prop_get_nvlist(A,
         [(preconditions)=Precon,positiv_effect=Pos,negativ_effect=Neg, assign_effect=Af, (parameters)= UT, 
                 parameter_types=SPT,parameters_decl=PDs]),
     UT=..[_|ARGS],     
     SPT=..[_|PTs], 
     nop(must_maplist(record_var_names,Vars)),
     must_maplist(create_hint,PTs,ARGS,ARGHints),
     %conjuncts_to_list(Call,MORE),
     append(ARGHints,Precon,M0),
     append(M0,Af,PrecondHints),
     append(Pos,Neg,POSNEG),
     append(PrecondHints,POSNEG,ALLHINTS),
     append(POSNEG,PrecondHints,REVALLHINTS),
     logicmoo_hyhtn:to_ssify(ALLHINTS,se,PrecondHints,SE),
     logicmoo_hyhtn:get_one_isa(S,X,REVALLHINTS),
     SC = sc(S,X,Neg=>Pos),
     OP = operator(UT,SE,SC,[]),
     varnames_for_assert(OP,COP,_Vars),
     env_aif(COP))).

env_aif(G):-functor(G,F,_),wdmsg(F:-G), assertz_new(ocl:G).

save_ocl_predicates(Decl):-dmsg(save_ocl_predicates(Decl)),prop_get(parameter_types,Decl,PTDecl),   
   env_aif(predicates([PTDecl])),PTDecl=..[F|PTypes],must_maplist(save_ocl_types,PTypes).

save_ocl_types(Atom):- atom( Atom ),!, save_ocl_types([Atom]-type).
save_ocl_types(Obj):- Obj=..[Type,List],!,save_ocl_types(List-Type).
save_ocl_types(Type-Type):-!.
save_ocl_types([Type]-Type):-!.
save_ocl_types(List-Type):- atom(List),!,save_ocl_types([List]-Type).
save_ocl_types(List-Type):- Type==type-> save_ocl_objects(type(List))->save_ocl_types(List-top)->save_ocl_types(List-primitive_sorts).
save_ocl_types(List-Type):-  env_aif(sorts(Type,List)),  
    (Type==non_primitive_sorts;true;save_ocl_types(Type-non_primitive_sorts)).

save_ocl_objects(Atom):- atom( Atom ),!, save_ocl_objects([Atom]-top).
save_ocl_objects(Type-Type):-!.
save_ocl_objects(_-top):-!.
save_ocl_objects(_-type):-!.
save_ocl_objects([Type]-Type):-!.
save_ocl_objects(List-Type):- atom(List),!,save_ocl_objects([List]-Type).
% save_ocl_objects(List-Type):- Type==top-> !.
save_ocl_objects(Obj):- Obj=..[Type,List],!,save_ocl_objects(List-Type).
save_ocl_objects(List-Type):- env_aif(objects(Type,List)).


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

/*
,
  nop( prop_get(constraints(A,C))),
  nop((  prop_get(varnames(A,Vars)))),
  nop((checkConstraints(C),record_var_names(Vars))).


get_action_copy(Mt,A):- actn(Mt,A).

get_constrained_action(Mt,action(S, PTs, Precon, Pos, Neg, Assign, UT )):-
  get_action_copy(Mt,action(S, PTs, Precon, Pos, Neg, Assign, UT , C, Vars)),
  nop((call(C),record_var_names(Vars))).


get_action_bb(_Mt,A):- bb_get(actions, As),copy_term(As, Ass),!, member(A, Ass).

get_constrained_action_bb(Mt,action(S, PTs, Precon, Pos, Neg, Assign, UT )):-
  get_action_bb(Mt,action(S, PTs, Precon, Pos, Neg, Assign, UT , C, Vars)),
  nop((call(C),record_var_names(Vars))).

*/
% DMILES

get_constrained_action(da(Mt,AS,AS5),action(S, PTs, Precon, Pos, Neg, Assign, UT )):- !, actn(Mt,action(S, PTs, Precon, Pos, Neg, Assign, UT, _,_)).
get_constrained_action(da(Mt,AS,AS5),action(S, PTs, Precon, Pos, Neg, Assign, UT )):-
  copy_term(AS,ASC),
  member(action(S, PTs, Precon, Pos, Neg, Assign, UT , C, Vars),ASC),
  nop((call(C),record_var_names(Vars))).


get_constrained_action_bb(_Mt,action(S, PTs, Precon, Pos, Neg, Assign, UT )):- bb_get(actions, As),As \= [],!, copy_term(As, Ass),!, member(action(S, PTs, Precon, Pos, Neg, Assign, UT, _,_), Ass).
get_constrained_action_bb(da(Mt,AS,AS5),action(S, PTs, Precon, Pos, Neg, Assign, UT )):-
  copy_term(AS,ASC),
  member(action(S, PTs, Precon, Pos, Neg, Assign, UT , C, Vars),ASC),
  nop((call(C),record_var_names(Vars))).




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

adjust_types(T,GsNs,Ps):- must((get_param_types0(T, GsNs,Ps, _))).
adjust_types(T,GsNs,L):- must((get_param_types0(T, GsNs,Ps, Ks),pairs_keys_values(L,Ps, Ks))).

get_param_types0(_,[], [] ,[]).

get_param_types0(Df,[H|T],[P1|Ps],[K|Ks]):- 
    svar_fixvarname(H,Name),!,
    P1 = '?'(Name),
    use_default(Df,P1,K),
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

get_param_types0(Df,[H|T],[P1,P2,P3|Ps],[K,K,K|Ks]):-
    compound(H), H =.. [K, [P1,P2,P3]],!,
    get_param_types0(Df,T, Ps, Ks).


get_param_types0(Df,[H|T],[H|Ps],[K|Ks]):-  must(atom(H)),use_default(Df,H,K),
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

/*
replc_vars(Term,NVars):-
 ignore(var(NVars) ->
   (NVarsM=mutable([]),replc_vars2(Term,NVarsM),arg(1,NVarsM,NVars)); replc_vars2(Term,NVars)).

replc_vars2(Term,NVars):-      
     compound(Term),
     arg(N,Term,V),
     once((var(V)->true;
     once((svar_fixvarname(V,Name)->  
        must_det_l((            
            arg(1,NVars,Vars),
            register_var(Name=NV,Vars,MVars),
            nb_setarg(N,Term,NV),
            nb_setarg(1,NVars,MVars)));
        replc_vars2(V,NVars))))),
     fail.
*/


replc_structure_vars1(B,AO):-replc_structure_vars2(B,A),if_changed(B,A,AO).

replc_structure_vars2(B,AO):-replc_structure_vars3(B,AO,_).

replc_structure_vars3(B,AO,Vars):- 
       (prop_get(varnames,B,Before)->true;Before=[]),
       replc_structure_vars4(B,Before,AO,Vars).
replc_structure_vars4(B,Before,AO,Vars):-
       cp(B,Before,A,After),!,       
       (Before\==After -> (numbervars(A:After),prop_set(varnames,A,After),unnumbervars(A:After,AO:Vars)) ;
         (A=AO)).

replc_structure_vars(A,AA):- copy_term(A,AC),
  must_det_l((     
      replc_structure_vars1(A,AA),
      (AA\=@=AC-> wdmsg(changed_replc_structure_var1s(A,AA));true))).

% ?- replc_structure_vars(v(a(?b,?c),mutable([]))). 


% copy_term_spec(+Term, -Term)
%
%   Special version of copy_term. variable x represented as ?(x)
%   All occurs of ?(x) are replaced with real prolog variables.
%   Modified version of code published by Bartak: http://kti.mff.cuni.cz/~bartak/prolog/data_struct.html
%

varnames_for_assert(A,B,After):-
     b_getval('$variable_names',Before),
     must(cp(A,Before,B,After)).

copy_term_for_assert(A,B):-
    must(cp(A,[],B,After)),
    b_setval('$variable_names',After).

cp( VAR,Vars,VAR,Vars):- var(VAR),!.
cp( VAR,Vars,NV,NVars):- logicmoo_i_sexp_reader:svar(VAR,_),!,must((svar_fixvarname(VAR,Name),atom(Name))),!, must(register_var(Name=NV,Vars,NVars)).
cp([],Vars,[],Vars).
cp( Term,Vars,Term,Vars):- \+compound(Term),!.
cp([H|T],Vars,[NH|NT],NVars):- !, cp(H,Vars,NH,SVars), cp(T,SVars,NT,NVars).
cp( Term,Vars,NTerm,NVars):-    
    Term=..[F|Args],    % decompose term
    (logicmoo_i_sexp_reader:svar(F,_)-> cp( [F|Args],Vars,NTerm,NVars);
    % construct copy term
    (cp(Args,Vars,NArgs,NVars), NTerm=..[F|NArgs])).  

/*

% register_var(?, ?, ?)
%
%   During copying one has to remeber copies of variables which can be used further during copying.
%   Therefore the register of variable copies is maintained.
%
register_var(N=V,IN,OUT):-register_var(N,IN,V,OUT).

register_var(N,T,V,OUT):- must(nonvar(N)),
   ((name_to_var(N,T,VOther)-> must((OUT=T,samify(V,VOther)));
     (once(nb_getval('$variable_names',Before);Before=[]),
      (name_to_var(N,Before,VOther)  -> must((samify(V,VOther),OUT= [N=V|T]));
         (var_to_name(V,T,_OtherName)                  -> OUT= [N=V|T];
           (var_to_name(V,Before,_OtherName)              -> OUT= [N=V|T];fail)))))).


register_var(N,T,V,OUT):- var(N),
   (var_to_name(V,T,N)                -> OUT=T;
     (once(nb_getval('$variable_names',Before);Before=[]),
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


*/ 


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
    prop_get(problem_name, P, Name),
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

% :- expects_dialect(sicstus).

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
% :- expects_dialect(sicstus).
:-use_module(library(ordsets)).


make_solution(S, S).
    
% step(Mt,+State, -ActionDef, -NewState)
%
%   Return descendant of State and ActionDefinition that was used.
%
step(Mt,State, ActionDef, NewState):-  
  %  get_a ction(A, ActionDef),
  %  get_precondition(A, P),    
  must(get_constrained_action_bb(Mt,action(_S, _L, P, PE, NE, _Assign, ActionDef))),
% DMILES 
%  get_constrained_action_bb(Mt,action5(ActionDef, P, PE, NE,_)),
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

% :- expects_dialect(sicstus).

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
    must(get_constrained_action(Mt,action(_S, _L, P, PE0, _Neg, _Assign, _UT))),
    %get_precondition(A, P),
    % DMILES
    %get_constrained_action_bb(Mt,action5(_,P,PE,_,_)),
  
    mysubset(P, State),
    PE0 = PE.
    %get_positiv_effect(A, PE).

% FILENAME:  h_diff.pl 
% h(+State, -EstimatedValue)
%
%   Estimated distance to achive Goal.
%
% :- expects_dialect(sicstus).

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
% :- expects_dialect(sicstus).


% parseDomain(+File, -Output).
%
%   Parse PDDL domain File and return it rewritten prolog syntax.   
%
parseDomain(F, O):- parseDomain(F, O, R), load_file_rest(F,R),!.


% parseDomain(+File, -Output, -RestOfFile)
%
%   The same as above and also return rest of file. Can be useful when domain and problem are in one file.
%
parseDomain(File, Output, R) :-  
    read_file(File, List, Filename),!,
    ensure_struct(domain,Output),
     prop_set(filename,Output,Filename),
     bb_put(filename,Filename),
    domainBNF(Output, List, R),!.


:-thread_local(t_l:allow_sterm).

domainBNF(Output, List, R):- with_assertions(tlbugger:skipMust, debugOnError0(domainBNF_dcg(Output, List, R))),!.
domainBNF(Output, List, R):- with_assertions(t_l:allow_sterm,with_assertions(tlbugger:skipMust, debugOnError0(domainBNF_dcg(Output, List, R)))),!,
   portray_clause((domainBNF:-t_l:allow_sterm,Output)).
domainBNF(Output, List, R):-  sterm(O, List, R), must_det_l((sterm2pterm(O,P),prop_put_extra_extra(Output,P),portray_clause((ed(Output):-P)))).
domainBNF(Output, List, R):- trace,with_no_assertions(tlbugger:skipMust, debugOnError0(domainBNF_dcg(Output, List, R))),!.

:-export(domainBNF_dcg//1).



sterm2pterm(VAR,VAR):-var(VAR),!.
sterm2pterm(In,Out):-nonvar(Out),!,sterm2pterm(In,OutM),must(Out=OutM).
sterm2pterm(VAR,'?'(UP)):-svar_fixvarname(VAR,UP),!.
sterm2pterm([S],S):-atom(S),!. % ,atom_concat(':',_,S),!.
sterm2pterm([S|SLIST],PTERM):-atom(S),atom_concat(':',_,S),
            must_maplist(sterm2pterm,SLIST,PLIST),           
            PTERM=..[S,PLIST].
sterm2pterm([S|SLIST],PTERM):-atom(S),\+ logicmoo_i_sexp_reader:svar(S,_),!,
            must_maplist(sterm2pterm,SLIST,PLIST),           
            PTERM=..[S|PLIST].
sterm2pterm(SLIST,PLIST):- is_list(SLIST),!,must_maplist(sterm2pterm,SLIST,PLIST).
sterm2pterm(VAR,VAR):-!.

sterm(_) --> [')'],{!,fail}.
sterm([]) --> ['(',')'],!.
sterm(A) --> action_def(A),!.
sterm(require_def(R)) --> require_def(R),!.
sterm(types(L))                    --> ['(',':',types],      typed_list_keys(type, L), [')'].
sterm(constants(L))                --> ['(',':',constants],  typed_list_keys(constant, L), [')'].
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


dcgStructSetOpt(Struct,Name,DCG,H,T) :- call(DCG,Value,H,T)-> prop_set(Name,Struct,Value); H=T.

dcgStructSetOptTraced(Struct,Name,DCG,H,T) :-(( call(DCG,Value,H,T)-> prop_set(Name,Struct,Value); H=T)).

% domainBNF_dcg(domain(N, R, T, C, P, F, C, S))
%
%   DCG rules describing structure of domain file in language PDDL.
%   BNF description was obtain from http://www.cs.yale.edu/homes/dvm/papers/pddl-bnf.pdf
%   This parser do not fully NOT support PDDL 3.0
%   However you will find comment out lines ready for futher development.
%
domainBNF_dcg(Struct)
                        --> ['(','define'],([':'];[]),['(','domain'], name(N), [')'],                   
                          {ensure_struct(domain,Struct) ,prop_set(domain_name, Struct,N),!},
                            dcgStructSetOpt(Struct,requires,require_def)  ,  
                            dcgStructSetOpt(Struct,types,types_def)    ,   %:typing
                            dcgStructSetOpt(Struct,constants,constants_def) ,
                            dcgStructSetOpt(Struct,predicates,predicates_def) ,
                            dcgStructSetOpt(Struct,functions,functions_def), %:fluents
                            dcgStructSetOptTraced(Struct,dconstraints,dconstraints_def)   ,    %:constraints
                            dcgStructSetOptTraced(Struct,actions,zeroOrMore(structure_def)), [')'].
                             

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

types_def(L)                    --> ['(',':',types],      typed_list_keys(type, L), [')'].
constants_def(L)                --> ['(',':',constants],  typed_list_keys(constant, L), [')'].
predicates_def(P)               --> ['(',':',predicates], oneOrMore(atomic_formula_skeleton, P), [')'].

% atomic_formula_skeleton(F)  --> ['('], predicate(P), typed_list(variable, L), [')'], {F=..[P|L],!}.
atomic_formula_skeleton(Struct) -->
   ['('],  predicate(S), typed_list_exact(variable, L), [')'],
     { must_det_l((get_param_types(top,L,PIs,PTs), 
     SPI=..[S|PIs],SPT=..[S|PTs],SPDL=..[predicate,S|L],
     ensure_struct(predicate,[parameters=SPI, parameter_types=SPT, parameters_decl=SPDL],Struct)))}.


predicate(_) --> [P], {P==not,!,fail}.
predicate(P)                    --> name(P).

variable(V)                     --> ['?'], name(N), { logicmoo_i_sexp_reader:fix_varcase(N,N0), V =.. [?, N0]}.

% atomic_function_skeleton(f(S, L)) --> ['('], function_symbol(S), typed_list(variable, L), [')'].
atomic_function_skeleton(f(S,Struct)) -->
   ['('],  function_symbol(S), typed_list_exact(variable, L), [')'],
     { must_det_l((get_param_types(top,L,PIs,PTs), 
     SPI=..[S|PIs],SPT=..[S|PTs],SPDL=..[function,S|L],
     ensure_struct(predicate,[parameters=SPI, parameter_types=SPT, parameters_decl=SPDL],Struct)))}.


typed_list_keys(Type, OUT) -->  typed_list(name, L), 
 {must_det_l((get_param_types(Type, L,PIs,PTs), pairs_keys_values(OUT,PIs,PTs)))}.


function_symbol(S)              --> name(S).
functions_def(F)                --> ['(',':',functions], function_typed_list(atomic_function_skeleton, F), [')'].              %:fluents
dconstraints_def(C)                 --> ['(',':',constraints], con_GD(C), [')'].                                                   %:constraints
structure_def(A)                --> action_def(A).
structure_def(D)               --> durative_action_def(D).                                                                    %:durativeactions
%structure_def(D)               --> derived_def(D).                                                                            %:derivedpredicates
structure_def(D)         --> allowed_sterm(structure_def,D).
%typed_list(W, G)               --> oneOrMore(W, N), ['-'], type(T), {G =.. [T, N]}.


typed_list_exact(W, L) --> typed_list0(W, L).
typed_list(W, L) --> typed_list0(W, GsNs),{adjust_types(W,GsNs,L)}.

typed_list0(W, GsNs)           --> oneOrMore(W, N), ['-'], type(T), !, typed_list0(W, Ns), {findall(G,(member(E,N),G =.. [T,E]),Gs), append(Gs,Ns,GsNs)}.
typed_list0(W, N)                --> zeroOrMore(W, N).


allowed_sterm(Why,sterm(Why,D))--> {t_l:allow_sterm},sterm(D).                                                                           

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
action_def(Struct)
                                --> 
           % action(S, PTs,  Precon, Pos, Neg, Assign, UT , C, Vars)
   {must((Struct = action(_, [], [],      [], [],  [],     [] , [],   mutable([sclass=dict]))))},
                                    ['(',':',action], action_symbol(S),
                                   dcgMust(( [':',parameters],

                                    ['('], typed_list_exact(variable, L), [')'],
                                    {must_det_l((get_param_types(top,L,PIs,PTs),
                                    SPI=..[S|PIs],
                                    SPT=..[S|PTs],
                                    prop_set_nvlist(Struct,[action_name=S,parameters=SPI, parameter_types=SPT, parameters_decl=L])))},
                                    dcgMust((action_def_body(Struct))),
                                    [')'])),!.
action_symbol(N)                --> name(N).

% Actions definitons
durative_action_def(action(S, vv(PTs), Precon, Pos, Neg, Assign, UT, []))
                                --> ['(',':',daction], action_symbol(S),
                                    [':',parameters,'('], typed_list(variable, L), [')'], 
                                    {get_param_types(s(val),L,PIs,PTs),UT=..[S|PIs],!},
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

action_def_body(Struct)
                                -->  
                                    (([':',precondition], emptyOr(pre_GD(P)))                ; []),
                                    (([':',effect],       emptyOr(effect(Pos, Neg, Assign))) ; []),
                                    {
                                      ignore(Pos=[]),
                                      must(prop_set_nvlist(Struct,[preconditions=P,positiv_effect=Pos,negativ_effect=Neg,assign_effect=Assign]))}.



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

% :- expects_dialect(sicstus).

% parseProblem(+File, -Output).
%
%   Parse PDDL problem File and return rewritten prolog syntax. 
%
parseProblem(F, O):-parseProblem(F, O, R), load_file_rest(F,R),!.


% parseProblem(+File, -Output, -RestOfFile).
%
%   The same as above and also return rest of file. Can be useful when domain and problem are in one file.
%
parseProblem(F, O, R) :-
   read_file(F, L , Filename),!,
   ensure_struct(problem,O),
   prop_set(filename,O,Filename),  
   problem(O, L, R),!.    

% Support for reading file as a list.
% :- [readFile].

problem(Output, List, R):- with_assertions(tlbugger:skipMust, debugOnError0(problem_dcg(Output, List, R))),!.
problem(Output, List, R):- with_assertions(t_l:allow_sterm,with_assertions(tlbugger:skipMust, debugOnError0(problem_dcg(Output, List, R)))),!,
   portray_clause((problem:-t_l:allow_sterm,Output)).
% problem(P     , List, R):- dtrace,trace,must(sterm(O, List, R)),!,must(sterm2pterm(O,P)),!,portray_clause((ed:-P)).
problem(Output, List, R):- must(problem_dcg(Output, List, R)),!.

% DCG rules describing structure of problem file in language PDDL.
%
%   BNF description was obtain from http://www.cs.yale.edu/homes/dvm/papers/pddl-bnf.pdf
%   This parser do not fully NOT support PDDL 3.0
%   However you will find comment out lines ready for futher development.
%   Some of the rules are already implemented in parseDomain.pl
%
% :-[parseDomain]. % make sure that it is loaded.

problem_dcg(Struct)   
        --> ['(',define],([':'];[]),['(',problem,Name,')','(',':',domain, Domain,')'],
                 {ensure_struct(problem,[problem_name=Name,domain_name=Domain],Struct)},
                    dcgStructSetOpt(Struct,requires,require_def),  
                    dcgStructSetOpt(Struct,objects,objects_def),
                    dcgStructSetOpt(Struct,init,init_def),  
                    dcgStructSetOpt(Struct,goal,goal_def),  
                    dcgStructSetOpt(Struct,pconstraints,pconstraints_def),   %:constraints  
                    dcgStructSetOpt(Struct,metric,metric_spec),  
                    dcgStructSetOpt(Struct,length,length_spec),
                    [')'].


objects_def(L)           --> ['(',':',objects], typed_list_as_list(name, L),[')'].

typed_list_as_list(W, OUT)   --> oneOrMore(W, N), ['-'],!, dcgMust(( type(T), typed_list_as_list(W, Ns), {G =.. [T,N], OUT = [G|Ns]})).
typed_list_as_list(W, N)        --> zeroOrMore(W, N).


goal_list(_,G) --> pre_GD(G).
goal_list(_,G) --> zeroOrMore(init_el,G).
goal_list(H,G) --> allowed_sterm(H,G).

init_def(I)                         --> ['(',':',init], goal_list(init,I), [')'].

init_el(I)                      --> literal(term, I).
init_el(I)                      --> pre_GD(I).
init_el(set(H,N))               --> ['(','='], f_head(H), number_sas(N), [')'].                                     % fluents
init_el(at(N, L))               --> ['(',at], number_sas(N), literal(name, L), [')'].                               % timed-initial literal
goal_def(G)                         --> ['(',':',goal],goal_list(goal,G),[')'].
pconstraints_def(C)                 --> ['(',':',constraints], pref_con_GD(C), [')'].                               % constraints
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

% :- expects_dialect(sicstus).

fix_wordcase(Word,WordC):-upcase_atom(Word,UC),UC=Word,!,downcase_atom(Word,WordC).
fix_wordcase(Word,Word).


%
% read_file(+File, -List).
%
read_file(string(String), Words, textDoc) :- must(open_string(String,In)),!, current_input(Old),call_cleanup(( set_input(In), get_code(C), (read_rest(C, Words))),( set_input(Old))),!.
read_file( File, Words , File) :-  exists_file(File), !, seeing(Old),call_cleanup(( see(File), get_code(C), (read_rest(C, Words))),( seen, see(Old))),!.
read_file(File0, Words,FinalName) :-  must((must_filematch(File0,File),exists_file(File),read_file( File, Words, FinalName))),!.

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
% :- expects_dialect(sicstus).
% :-[parseProblem, parseDomain].

%nop(_).

%parse_file(+File).
test_parse_file(F,O):- must(read_file(F, L, _Filename)),!,((domainBNF(O, L, _R1); must(((problem(O, L, _R2)))))),!.

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
% :- expects_dialect(sicstus).

:-use_module(library(ordsets)).
:-use_module(library(heaps)).


% search(+InitState, +GoalState, -Solution)
%
search(Mt,I, _, Solution):-
   term_to_ord_term(I,II),!,
    a_star(Mt, II, Solution, _).
    
    
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


domain_name_for_ocl(Name):- use_local_pddl, no_repeats(domain_name0(Name)).
domain_name0(Name):- bb_get(currentDomain, D),prop_get(domain_name,D,Name).
domain_name0(Name):- bb_get(currentProblem, P),prop_get(domain_name,P,Name).
domain_name0(Name):- user:is_saved_type(domain,Name,_).
domain_name0(Name):- user:is_saved_type(problem,_,P),prop_get(domain_name,P,Name).

problem_name(Name):- use_local_pddl, no_repeats(problem_name0(Name)).
problem_name0(Name):- bb_get(currentProblem, P),prop_get(problem_name,P,Name).
problem_name0(Name):- user:is_saved_type(problem,Name,_).


/* EXAMPLE OCLh

% Sorts
sorts(chameleonWorld,primitive_sorts,[door,flexarium,chameleon,box,substrate]).
*/ 

sorts_for_ocl(DName,primitive_sorts,TypesList):-  use_local_pddl, pddl_sorts(DName,primitive_sorts,TypesList).
pddl_sorts(DName,primitive_sorts,TypesList):- bb_get(currentDomain, D),prop_get(domain_name,D,DName),!,prop_get(types,D,TypesList).
pddl_sorts(DName,primitive_sorts,List):-nonvar(DName),findall(S,is_a_type(DName,S),List).


is_a_type(D,S):- use_local_pddl, loop_check(is_a_type0(D,S)).
is_a_type0(D,S):-sorts(D,_,L),member(S,L).
is_a_type0(Name,S):-bb_get(currentProblem,P),prop_get(domain_name,P,Name),objects_3(P,S,_).


pname_to_dname(t7,chameleonWorld).
pname_to_dname(PName,DName):-name_to_problem_struct(PName,P),!,prop_get(domain_name,P,DName).
pname_to_dname(_,DName):-domain_name(DName).

% Objects
objects_for_ocl(Name,Type,List):- use_local_pddl, name_to_problem_struct(Name,P),
   prop_get(objects,P,ObjsDef),
   member(Objs,ObjsDef),Objs=..[Type,List].

/* EXAMPLE OCLh
objects(t7,door,[door1]).
objects(t7,flexarium,[flexarium1]).
objects(t7,chameleon,[veiledChameleon]).
objects(t7,box,[box1,box2]).
objects(t7,substrate,[newsPaper1,newsPaper2]).
*/

% Predicates
predicates_for_ocl(Name,List):- use_local_pddl, name_to_domain_struct(Name,D),   
   prop_get(predicates,D,List).


/* EXAMPLE OCLh
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
    [inBox(Chameleon,_Box)],
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
*/

:- style_check(+singleton).

op_action(Mt, S, PTs, NPrecon, Pos, Neg, Af, UT , True):- use_local_pddl,
   loop_check(env_call(operator(Mt,UT,SE,SC,SS))),
   must_det_l((
      True = true,
      UT=..[S|ARGS],
      must_maplist(lock_var,ARGS),
      HintsIn=[],
      unss_ify(=,HintsIn,ss,SS,Af,Hints1),
      unss_ify(=,Hints1,se,SE,Precon,Hints2),
      unss_ify(=,Hints2,sc,SC,NEGPOS,Hints),
      divide_neg_pos(NEGPOS,[],[],Neg,Pos),   
      append(Precon,Neg,NPrecon),
      must_maplist(get_type_of(Mt,top,Hints),ARGS,PTs))).

lock_var(X):-when((?=(X,Y);nonvar(X)),X==Y).
unlock_var(V):-del_attrs(V).


add_wrapper(W,In , Out):-Out=..[W,In].

actn_operator(Mt,UT,SE,SC,SS):- use_local_pddl, actn(Mt,A),
 must_det_l(( 
    prop_get_nvlist(A,
       [(preconditions)=Precon,positiv_effect=Pos,negativ_effect=Neg, assign_effect=Af, (parameters)= UT, 
               parameter_types=PTs,
               (constraints)=Call,
               (varnames)=Vars]),
   UT=..[_|ARGS],
   show_call(Call),   
   must_maplist(record_var_names,Vars),
   must_maplist(create_hint,PTs,ARGS,ARGHints),
   conjuncts_to_list(Call,MORE),
   append(ARGHints,MORE,PrecondHints),
   unss_ify(=,[],PrecondHints,Precon,se,SEPs,HintsSE),
   unss_ify(=,[],HintsSE,Af,ss,ASEPs,HintsSS),
   unss_ify(add_wrapper(del),[],HintsSS,Neg,ss,NOTS,HintsNEG),
   unss_ify(add_wrapper(add),NOTS,HintsNEG,Pos,ss,NOTSPOSC,HintsPOS),
   mylist_to_set(HintsPOS,Hints),
   must_maplist(ress_ify(Mt,Def,Hints,se),SEPs,SE),
   must_maplist(ress_ify(Mt,Def,Hints,ss),ASEPs,SS),   
   must_maplist(make_rem_adds(Mt,Hints),NOTSPOSC,SC))).
  

make_rem_adds(Mt,Hints,A1-LIST,sc(Type,A1,(NEG=>POS))):-findall(N,member(del(N),LIST),NEG),findall(N,member(add(N),LIST),POS),
  must(get_type_of(Mt,top,Hints,A1,Type)).

create_hint(S,X,is_of_sort(X,S)).

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

unss_ify(_ ,WAS,Hints,G,_,WAS,Hints):- (nonvar(G);G==['Uninitialized']),!.
unss_ify(GT,WAS,HintsIn,G,SS,OUTS,Hints):-
  must((dess_ify(GT,WAS,HintsIn,G,SS,OUT,HintsM),!,mygroup_pairs_by_key(OUT,OUTS),!,mylist_to_set(HintsM,Hints))).

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
get_type_of(Mt,_Def,_Hints,A1,Type):-nonvar(A1),loop_check(objects_3(Mt,Type,List)),member(A1,List).

:- style_check(-singleton).
get_type_of_atom(Mt,Def,Hints,veiledChameleon,chameleon).
get_type_of_atom(Mt,Def,Hints,veiledchameleon,chameleon).
get_type_of_atom(Mt,Def,Hints,A1,Type):-pname_to_dname(P,D),is_a_type(D,Type),atom_concat(Type,Num,A1),Num=_.
get_type_of_atom(Mt,Def,Hints,A1,Type):-pname_to_dname(P,D),is_a_type(D,Type),atom_concat(_,Type,A1).
:- style_check(+singleton).



divide_neg_pos([],Neg,Pos,Neg,Pos).
divide_neg_pos([A|MORE],NL2,PL2,Neg,Pos):-divide_neg_pos(A,NL2,PL2,NegM,PosM),divide_neg_pos(MORE,NegM,PosM,Neg,Pos).
divide_neg_pos(NL1=>PL1,NL2,PL2,Neg,Pos):-append(NL1,NL2,Neg),append(PL1,PL2,Pos).

/* EXAMPLE OCLh

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
*/

planner_task_for_ocl(Domain,PName,
  % Goals
    SEs,SSs):-  use_local_pddl,
     name_to_problem_struct(PName,P),prop_get(domain_name,P,Domain),
     must_det_l((
        prop_get(init,P, UCI),
        prop_get(goal,P, UCG),    
        copy_term_for_assert((UCI,UCG),(I,G)),       
        unss_ify(=,[],[],I,ss,SSK,HintsSS),
        unss_ify(=,[],HintsSS,G,se,SEK,Hints),
        must_maplist(ress_ify(Mt,Def,Hints,ss),SSK,SSs),
        must_maplist(ress_ify(Mt,Def,Hints,se),SEK,SEs))).

name_to_problem_struct(Name,P):- use_local_pddl, problem_name(Name),name_to_problem_struct0(Name,P).
name_to_problem_struct0(Name,P):-Name==current,!,bb_get(currentProblem,P).
name_to_problem_struct0(Name,P):-is_saved_type(problem,Name,P).
name_to_problem_struct0(Name,P):-bb_get(currentProblem,P),prop_get(problem_name,P,NameO),Name=NameO.
name_to_problem_struct0(Name,P):-loop_check(ocl_problem_struct(Name,P)).

kv_to_pddl([], []).
kv_to_pddl([_-N|T0], OUT) :- 
 	kv_to_pddl(T0,VALUES),
        append(N,VALUES,OUT).

%TODO work on later        
ocl_problem_struct(Name,P):- use_local_pddl, no_repeats(Name,planner_task(DomainName,Name,SE,SS)),
   must_det_l((
      unss_ify(=,[],[],ss,SS,OUTI,HintsM),mygroup_pairs_by_key(OUTI,IOUT),kv_to_pddl(IOUT,I),
      unss_ify(=,[],HintsM,se,SE,OUTG,Hints),mygroup_pairs_by_key(OUTG,GOUT),kv_to_pddl(GOUT,G),
      P = problem(Name, DomainName, [ocl], [], I, G, Hints, /*MS*/ [], /*LS*/ []),
      ignore(loop_check(domain_name(DomainName))))).


name_to_domain_struct(Name,P):-Name==current,!,bb_get(currentDomain,P).
name_to_domain_struct(Name,P):-bb_get(currentDomain,P),prop_get(domain_name,P,NameO),Name=NameO.
name_to_domain_struct(Name,P):-is_saved_type(domain,Name,P).
name_to_domain_struct(Name,P):-loop_check(ocl_domain_struct(Name,P)).

%TODO work on later
% sorts % consts % preds
ocl_domain_struct(Name,D):- use_local_pddl, no_repeats(Name,loop_check(predicates(Name,PredsList))),
   % once((not((bb_get(currentDomain,D),once(Name==current;prop_get(domain_name,D,Name)))))),   
   must_det_l((
      findall(S,is_a_type(Name,S),Types),
      D = domain(Name, [ocl], Types, /*Consts*/ [] , PredsList, /*Fuents*/[]  ,/*Constrs*/ [], /*Dconstraints*/[], Actions),
      Mt=Name,
      findall(A,actn(Mt,A),Actions))).

mygroup_pairs_by_key([], []).
mygroup_pairs_by_key([M-N|T0], [M-ORDERED|T]) :-
 	mysame_key(M, T0, TN, T1),
 	mygroup_pairs_by_key(T1, T),
        mylist_to_set([N|TN],ORDERED).


mysame_key(M0, [M-N|T0], [N|TN], T) :-
 	M0 == M, !,
 	mysame_key(M, T0, TN, T).
mysame_key(_, L, [], L).

:-thread_local(t_l:loading_files).
:-thread_local(t_l:hyhtn_solve/1).
% t_l:other_planner(hyhtn_solve).



:- flag(time_used,_,0).
:- flag(time_used_other,_,0).

:- debug,(must(test_blocks)).

:- prolog_load_context(directory,Dir),
   must((absolute_file_name('../../../pddl/',Y,[relative_to(Dir),file_type(directory)]),
   (( \+ user:file_search_path(pddl,Y)) ->asserta(user:file_search_path(pddl,Y));true))).




:- solve_files(pddl('benchmarks/mystery/domain.pddl'),pddl('benchmarks/mystery/prob01.pddl')).
:- test_domain(pddl('benchmarks/driverlog/domain.pddl'),4).
:- solve_files(pddl('hsp2_1_0_pddl/parcprinter-strips/p01-domain-woac.pddl'),
               pddl('hsp2_1_0_pddl/parcprinter-strips/p01-woac.pddl')).


% :-doall(on_call_decl_hyhtn).

:- if(gethostname(c3po);gethostname(ubuntu);gethostname(titan)).

:- test_domain(pddl('domains_ocl/chameleonWorld/domain*')).
:- test_all(5). % should be 7

/*
:- if(current_predicate(pce_show_profile/0)).
:- pce_show_profile.
:- endif.
*/

:- retractall(t_l:loading_files).
:- endif.



% BAD :- test_domain('./elearning/domain.pddl').
% :- test_all.

% 
% :- solve_files('benchmarks/nomystery-sat11-strips/domain.pddl','benchmarks/nomystery-sat11-strips/p01.pddl').
% :- test_domain('./benchmarks/nomystery-sat11-strips/domain.pddl').



:- if(gethostname(c3po);gethostname(ubuntu);gethostname(titan)).


/*

:- test_domain('domains_ocl/toasterWorldv2/domain*').

:- solve_files('regression-tests/issue58-domain.pddl','regression-tests/issue58-problem.pddl').
:- forall(must_filematch('./hsp-planners-master/?*?/pddl/?*?/?*domain*.*',E),once(test_domain(E,4))).
:- forall(must_filematch('./hsp-planners-master/?*?/examples/?*?/?*domain*.*',E),once(test_domain(E,5))).

:- test_domain('./benchmarks/nomystery-sat11-strips/domain.pddl').

test_blocks:- fail, test_domain('./benchmarks/nomystery-sat11-strips/domain.pddl',RList),reverse(RList,List),
  forall(member(E,List),once(test_domain(E))).

% :-asserta(t_l:loading_files).

:- forall(must_filematch('./rover/?*?/?*domain*.*',E),once(load_domain(E))).
:- forall(must_filematch('./hsp-planners-master/?*?/pddl/?*?/?*domain*.*',E),once(load_domain(E))).
:- forall(must_filematch('./hsp-planners-master/?*?/examples/?*?/?*domain*.*',E),once(load_domain(E))).
:- forall(must_filematch('./hsp-planners-master/?*?/examples/?*?/?*domain*.*',E),once(load_domain(E))).
:- forall(must_filematch('./primaryobjects_strips/?*?/?*domain*.*',E),once(test_domain(E))).
:- solve_files('hakank-pddl/monkey-domain.pddl','hakank-pddl/monkey-prob01.pddl').

*/ 

:- endif.

% :- test_all(7).

:- show_call(flag(time_used_other,W,W)).
:- show_call(flag(time_used,W,W)).


