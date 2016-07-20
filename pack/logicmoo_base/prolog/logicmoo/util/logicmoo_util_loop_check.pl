% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_loop_check.pl
:- module(logicmoo_util_loop_check,
          [ any_term_overlap/2,
            any_term_overlap_atoms_of/2,
            atoms_of_0/2,
            call_setof_tabled/4,
            call_t/1,
            call_tabled/1,
            call_tabled/2,
            call_tabled0/4,
            call_tabled1/4,
            call_vars_tabled/3,
            can_fail/1,
            cannot_table_call/1,
            cannot_use_tables/1,
            cc_key/2,
            ex/0,
            expire_dont_add/0,
            expire_tabled_list/1,
            findall_nodupes/3,
            get_where/1,
            get_where0/1,
          memoize_on/3,
          memoize_on/4,
            is_loop_checked/1,
            lco_goal_expansion/2,
            lex/0,
            cyclic_break/1,
            loop_check/1,
            loop_check/2,
            loop_check_early/2,
            loop_check_term/3,
            loop_check_term_key/3,
            make_key/2,
            make_tabled_perm/1,
            mpred_expire_caches/1,
            no_loop_check/1,
            no_loop_check/2,
            no_loop_check_term_key/3,
            outside_of_loop_check/0,
            really_can_table/0,
            reduce_make_key/2,
            retract_can_table/0,
            skipped_table_call/1,
            transitive/3,
            transitive_except/4,
            transitive_lc/3
          ]).
:- multifile
        system:goal_expansion/2.
:- meta_predicate
        call_setof_tabled(?, ?, 0, -),
        call_t(0),
        call_tabled(0),
        call_tabled(?, 0),
        call_tabled0(?, ?, ?, ?),
        call_tabled1(?, ?, ?, ?),
        call_vars_tabled(?, ?, 0),
        cannot_table_call(0),
        cannot_use_tables(0),
        findall_nodupes(?, 0, -),
        loop_check(0),
        loop_check(0, 0),
        loop_check_early(0, 0),
        loop_check_term(0, ?, 0),
        loop_check_term_key(0, ?, 0),        
        make_tabled_perm(0),
        memoize_on(+,+,0),
        memoize_on(+,+,+,0),
        no_loop_check(0),
        no_loop_check(0, 0),
        no_loop_check_term_key(0, ?, 0),
        reduce_make_key(+, -),
        skipped_table_call(0),
        transitive(2, +, -),
        transitive_except(+, 2, +, -),
        transitive_lc(2, +, -).
:- meta_predicate(go_as_last(0,0)).
:- meta_predicate no_loop_check(0).
:- meta_predicate no_loop_check(0,0).
:- meta_predicate no_loop_check_term_key(0,?,0).
:- meta_predicate call_tabled(?,0).
:- meta_predicate loop_check(0).
:- meta_predicate loop_check(0,0).

:- meta_predicate call_tabled1(*,?,0,-).
:- meta_predicate call_tabled0(*,?,0,-).


:- export(go_as_last/2).
:- thread_local lmcache:going_last/1.
% go_as_last(Call1,Call2):- \+ lmcache:going_last(Call1),w_tl(lmcache:going_last(Call1), (Call1->true;(must(catch(Call2,_,fail))))),!.

%= 	 	 

%% go_as_last( :GoalCall1, :GoalCall2) is semidet.
%
% Go Converted To Last.
%
go_as_last(Call1,Call2):- \+ lmcache:going_last(Call1),w_tl(lmcache:going_last(Call1), (Call1->true;Call2)),!.



:- module_transparent
        any_term_overlap/2,
        any_term_overlap_atoms_of/2,
        atoms_of_0/2,
        can_fail/1,
        ex/0,
        expire_dont_add/0,
        expire_tabled_list/1,
        get_where/1,
        get_where0/1,
        system:goal_expansion/2,
        is_loop_checked/1,
        lco_goal_expansion/2,
        lex/0,
        mpred_expire_caches/1,
        outside_of_loop_check/0,
        really_can_table/0,
        retract_can_table/0.
:- dynamic
        system:goal_expansion/2.


:- include('logicmoo_util_header.pi').
:- system:use_module(logicmoo_util_catch).

%= 	 	 

%% transitive( :PRED2X, +A, -B) is semidet.
%
% Transitive.
%
transitive(X,A,B):- once(on_x_debug(call(X,A,R)) -> ( R\=@=A -> transitive_lc(X,R,B) ; B=R); B=A),!.


%= 	 	 

%% transitive_lc( :PRED2X, +A, -B) is semidet.
%
% Transitive Not Loop Checked.
%
transitive_lc(X,A,B):-transitive_except([],X,A,B).


%= 	 	 

%% transitive_except( +NotIn, :PRED2X, +A, -B) is semidet.
%
% Transitive Except.
%
transitive_except(NotIn,X,A,B):- memberchk_same_two(A,NotIn)-> (B=A,!) ;((once(on_x_debug(call(X,A,R)) -> ( R\=@=A -> transitive_except([A|NotIn],X,R,B) ; B=R); B=A))),!.


%= 	 	 

%% memberchk_same_two( ?X, :TermY0) is semidet.
%
% Memberchk Same Two.
%
memberchk_same_two(X, [Y0|Ys]) :- is_list(Ys),!,C=..[v,Y0|Ys],!, arg(_,C,Y), ( X =@= Y ->  (var(X) -> X==Y ; true)),!.
memberchk_same_two(X, [Y|Ys]) :- (   X =@= Y ->  (var(X) -> X==Y ; true) ;   (nonvar(Ys),memberchk_same_two(X, Ys) )).


%% cyclic_break( ?Cyclic) is semidet.
%
% Cyclic Break.
%
cyclic_break(Cyclic):-cyclic_term(Cyclic)->(writeq(cyclic_break(Cyclic)),nl,prolog);true.


% ===================================================================
% Loop checking
% ===================================================================
:- thread_local lmcache:ilc/1.

% = :- meta_predicate(call_t(0)).
% call_t(C0):-reduce_make_key(C0,C),!,table(C),!,query(C).
% call_t(C0):-query(C).

%= 	 	 

%% call_t( :GoalC) is semidet.
%
% Call True Stucture.
%
call_t(C):- call(C).

:- meta_predicate reduce_make_key(+,-).

%= 	 	 

%% reduce_make_key( +O, -O) is semidet.
%
% Reduce Make Key.
%
reduce_make_key(call(C),O):-!,reduce_make_key(C,O).
reduce_make_key(call_u(C),O):-!,reduce_make_key(C,O).
reduce_make_key(must(C),O):-!,reduce_make_key(C,O).
reduce_make_key(no_repeats(C),O):-!,reduce_make_key(C,O).
reduce_make_key(no_repeats(_,C),O):-!,reduce_make_key(C,O).
reduce_make_key(no_repeats_old(C),O):-!,reduce_make_key(C,O).
reduce_make_key(no_repeats_old(_,C),O):-!,reduce_make_key(C,O).
reduce_make_key(call_tabled(C),O):-!,reduce_make_key(C,O).
reduce_make_key(no_loop_check(C),O):-!,reduce_make_key(C,O).
reduce_make_key(loop_check(C),O):-!,reduce_make_key(C,O).
reduce_make_key(loop_check(C,_),O):-!,reduce_make_key(C,O).
reduce_make_key(loop_check_term_key(C,_,_),O):-!,reduce_make_key(C,O).
reduce_make_key(loop_check_term(C,_,_),O):-!,reduce_make_key(C,O).
reduce_make_key(fact_loop_checked(_,C),O):-!,reduce_make_key(C,O).
reduce_make_key(V+C,V+O):-!,reduce_make_key(C,O).
reduce_make_key(M:C,O):-atom(M),!,reduce_make_key(C,O).
reduce_make_key(O,O).



%= 	 	 

%% cc_key( ?CC, ?Key) is semidet.
%
% Cc Key.
%
cc_key(CC,Key):- cyclic_term(CC),!,dtrace,copy_term_nat(CC,CKey),numbervars(CKey,0,_),format(atom(Key),'~w',[CKey]),!.
cc_key(CC,O):- copy_term_nat(CC,Key),numbervars(Key,0,_),!,Key=O.


%= 	 	 

%% make_key( ?CC, ?KeyO) is semidet.
%
% Make Key.
%
make_key(M:CC,KeyO):- atom(M),!,((ground(CC)->Key=CC ; cc_key(CC,Key))),!,KeyO=Key.
make_key(CC,KeyO):- ((ground(CC)->Key=CC ; cc_key(CC,Key))),!,KeyO=Key.
:- '$set_predicate_attribute'(make_key(_,_), hide_childs, 1).
:- '$set_predicate_attribute'(make_key(_,_), trace, 1).


%= 	 	 

%% is_loop_checked( ?Call) is semidet.
%
% If Is A Loop Checked.
%
is_loop_checked(Call):-  make_key(Call,Key),!,(lmcache:ilc(Key);lmcache:ilc(Key+_)).

:- meta_predicate lmcode:loop_check_early(0,0).
:- export(loop_check_early/2).

%= 	 	 

%% loop_check_early( :GoalCall, :GoalTODO) is semidet.
%
% Loop Check Early.
%
loop_check_early(Call, TODO):- loop_check_term_key(Call,Call, TODO).

:- export(loop_check/1).

%= 	 	 

%% loop_check( :GoalCall) is semidet.
%
% Loop Check.
%
% loop_check(Call):- current_predicate(dra_call_tabled/1),!, dra_call_tabled(Call).
loop_check(Call):- loop_check(Call, fail).

:- export(loop_check/2).

%= 	 	 

%% loop_check( :GoalCall, :GoalTODO) is semidet.
%
% Loop Check.
%
loop_check(Call, TODO):- !,loop_check_early(Call, TODO).
loop_check(Call, TODO):- parent_goal(ParentCall,1)->(loop_check_term_key(Call,Call+ParentCall, TODO));loop_check_early(Call, TODO).

%= 	 	 

%% loop_check_term_key( :GoalCall, ?KeyIn, :GoalTODO) is semidet.
%
% Loop Check Term Key.
%
loop_check_term_key(Call,KeyIn,TODO):- notrace(make_key(KeyIn,Key)) -> loop_check_term(Call,Key,TODO).



%= 	 	 

%% no_loop_check( :GoalCall) is semidet.
%
% No Loop Check.
%
no_loop_check(Call):- no_loop_check(Call, fail).
:- export(no_loop_check/2).

%= 	 	 

%% no_loop_check( :GoalCall, :GoalTODO) is semidet.
%
% No Loop Check.
%
no_loop_check(Call, TODO):- !, no_loop_check_term_key(Call,Call,TODO).
%no_loop_check(Call, TODO):- parent_goal(Term,2)->no_loop_check_term_key(Call,Term+Call, TODO).

%= 	 	 

%% no_loop_check_term_key( :GoalCall, ?KeyIn, :GoalTODO) is semidet.
%
% No Loop Check Term Key.
%
no_loop_check_term_key(Call,KeyIn,TODO):- make_key(KeyIn,Key) -> wno_tl(lmcache:ilc(_),loop_check_term(Call,Key,TODO)).



%= 	 	 

%% loop_check_term( :GoalCall, ?Key, :GoalTODO) is semidet.
%
% Loop Check Term 50% of the time
%
% loop_check_term(Call,_Key,_TODO):- 1 is random(2) ,!,call(Call).
loop_check_term(Call,Key,TODO):- notrace(TT = lmcache:ilc(Key)),
 ( notrace( \+(TT)) -> (setup_call_cleanup(notrace(asserta(TT,REF)), Call, 
   erase(REF))) ; 
   ((can_fail(TODO)->retract_can_table;true),call(TODO)) ).


%= 	 	 

%% can_fail( ?G) is semidet.
%
% Can Fail.
%
can_fail(G):-not(G=true),not(G=must(_)).

% get_where(When)

%= 	 	 

%% get_where( :TermB) is semidet.
%
% Get Where.
%
get_where(B:L):-get_where0(F:L),file_base_name(F,B).

%= 	 	 

%% get_where0( :GoalF) is semidet.
%
% Get Where Primary Helper.
%
get_where0(F:L):-source_location(file,F),current_input(S),line_position(S,L),!.
get_where0(F:L):-source_location(F,L),!.
get_where0(A:0):-current_input(S),stream_property(S,alias(A)),!.
get_where0(M:0):-source_context_module(M),!.
get_where0(baseKB:0):-!.


%= 	 	 

%% lco_goal_expansion( :TermB, :TermA) is semidet.
%
% Lco Goal Expansion.
%

% lco_goal_expansion(_,_):-!,fail.
lco_goal_expansion(B,A):-nonvar(A),!,lco_goal_expansion(B,M),!,M=A.
lco_goal_expansion(V,V):- \+ compound(V),!.
lco_goal_expansion(loop_check(G),O):-!,lco_goal_expansion(loop_check(G,fail),O).
lco_goal_expansion(loop_check(G,TODO),loop_check_term_key(G,G:W,TODO)):-must(get_where(W)).
lco_goal_expansion(no_loop_check(G),O):-!,lco_goal_expansion(no_loop_check(G,fail),O).
lco_goal_expansion(no_loop_check(G,TODO),no_loop_check_term_key(G,G:W,TODO)):-must(get_where(W)).
lco_goal_expansion(B,A):- compound_name_arguments(B,F,ARGS),maplist(lco_goal_expansion,ARGS,AARGS),compound_name_arguments(A,F,AARGS).


/*
:- meta_predicate call_no_cuts_loop_checked(0).
:- module_transparent call_no_cuts_loop_checked/1.
:- meta_predicate call_no_cuts_loop_checked(0,0).
:- module_transparent call_no_cuts_loop_checked/2.
call_no_cuts_loop_checked(Call):-call_no_cuts_loop_checked(Call, fail).
call_no_cuts_loop_checked(Call, TODO):-!, loop_check(Call,TODO).
call_no_cuts_loop_checked(Call, TODO):- clause(Call,Body),make_key(Body,Key),loop_check_term(Body,Key,TODO).
*/

% =====================================================================================================================
:- export((call_tabled/2)).
:- export((cannot_table_call/1)).
:- export((cannot_use_tables/1)).
:- export((skipped_table_call/1)).
% =====================================================================================================================
:- meta_predicate call_tabled(0).
:- module_transparent call_tabled/1.

:- meta_predicate call_vars_tabled(?,?,0).
:- module_transparent call_vars_tabled/3.

% = :- meta_predicate((cannot_table_call(0))).
% = :- meta_predicate((cannot_use_tables(0))).
% = :- meta_predicate((skipped_table_call(0))).



:- meta_predicate call_setof_tabled(?,?,0,-).
:- meta_predicate findall_nodupes(?,0,-).
:- module_transparent call_setof_tabled/4.

:- dynamic(lmcache:call_tabled_cached_results/2).
:- dynamic(lmcache:call_tabled_perm/2).
:- thread_local lmcache:maybe_table_key/1.


%= 	 	 

%% retract_can_table is semidet.
%
% Retract Can Table.
%
retract_can_table :- retractall(maybe_table_key(_)).

% = :- meta_predicate(make_key(?,-)).

:- multifile(baseKB:mpred_on_expire_caches/1).
:- dynamic(baseKB:mpred_on_expire_caches/1).
:- module_transparent((ex)/0).

:- dynamic(baseKB:already_added_this_round/1).
:- export(baseKB:already_added_this_round/1).

%= 	 	 

%% expire_dont_add is semidet.
%
% Expire Dont Add.
%
expire_dont_add:-retractall(baseKB:already_added_this_round(_)),mpred_expire_caches(all),nop(dmsg(expire_dont_add)).


%= 	 	 

%% lex is semidet.
%
% Lex.
%
lex:-listing(lmcache:ilc(_)),forall(current_predicate(lmcache:F/A),listing(lmcache:F/A)),catchv(listing(baseKB:already_added_this_round),_,true).

%= 	 	 

%% ex is semidet.
%
% Ex.
%
(ex):-mpred_expire_caches(_),retractall(lmcache:ilc(_)),dmsg_showall(_),forall(current_predicate(lmcache:F/A),(functor(RA,F,A),retractall(RA))),catchv(expire_dont_add,_,true).


%= 	 	 

%% mpred_expire_caches( ?A) is semidet.
%
% Managed Predicate Expire Caches.
%
mpred_expire_caches(A):-doall(call_no_cuts(must(baseKB:mpred_on_expire_caches(A)))).

:-multifile(baseKB:mpred_on_expire_caches/1).
:-asserta((baseKB:mpred_on_expire_caches(A):-expire_tabled_list(A))).


%= 	 	 

%% expire_tabled_list( ?V) is semidet.
%
% Expire Tabled List.
%
expire_tabled_list(V):-var(V),!,retractall(lmcache:call_tabled_cached_results(_,_)).
expire_tabled_list(K):-compound(K),functor(K,_,1),retractall(lmcache:call_tabled_cached_results(isa(_,_),_)),retractall(lmcache:call_tabled_cached_results(isa(_,_)+_,_)),fail.
expire_tabled_list(K):-!,retractall(lmcache:call_tabled_cached_results(K,_)),retractall(lmcache:call_tabled_cached_results(K+_,_)).
expire_tabled_list(T):- atoms_of_0(T,A1), CT= lmcache:call_tabled_cached_results(Key,List),ignore(((CT,once(any_term_overlap_atoms_of(A1,List);(not(member(Key,List)),
  any_term_overlap_atoms_of(A1,Key))),retractall(CT)),fail)).


%= 	 	 

%% any_term_overlap_atoms_of( ?A1, ?T2) is semidet.
%
% Any Term Overlap Atoms Of.
%
any_term_overlap_atoms_of(A1,T2):-atoms_of_0(T2,A2),!,member(A,A1),member(A,A2),!.


%= 	 	 

%% any_term_overlap( ?T1, ?T2) is semidet.
%
% Any Term Overlap.
%
any_term_overlap(T1,T2):- atoms_of_0(T1,A1),atoms_of_0(T2,A2),!,member(A,A1),member(A,A2),!.

% = :- meta_predicate(make_tabled_perm(0)).


%= 	 	 

%% make_tabled_perm( :GoalCall) is semidet.
%
% Make Tabled Perm.
%
make_tabled_perm(Call):- must(really_can_table),must(outside_of_loop_check),
  term_variables(Call,Vars),!,make_key(Vars+Call,LKey),reduce_make_key(LKey,Key),
  findall(Vars,wno_tl(lmcache:cannot_save_table,no_loop_check(no_repeats_old(Call))),KList),
  must(KList=[_|_]),!,
  asserta(lmcache:call_tabled_perm(Key,KList)),!,
  member(Vars,KList).

memoize_on(M,(In->_),G):- \+ ground(In),nop((retractall(lmcache:memoized_on(M,In,_)))),!,G,!.
% memoize_on(M,(In->Out),G):-make_key(In,Key),memoize_on(M,Key,Out,G).
memoize_on(M,(In->Out),G):- memoize_on(M,In,Out,G),!.

:- dynamic(lmcache:memoized_on/3).
memoize_on(M,In,Out,_):- lmcache:memoized_on(M,In,Out),!.
memoize_on(M,In,Out,G):- G,!,call(assert_if_new,lmcache:memoized_on(M,In,Out)),!.




%= 	 	 

%% atoms_of_0( :TermC, ?L) is semidet.
%
% atoms of  Primary Helper.
%
atoms_of_0(Var,[]):- (var(Var);Var==[]),!.
atoms_of_0(':',[]).
atoms_of_0('moo',[]).
atoms_of_0('t',[]).
atoms_of_0(',',[]).
atoms_of_0(':-',[]).
atoms_of_0('$VAR',[]):-!.
atoms_of_0(Atom,[]):-number(Atom),!.
atoms_of_0(Atom,[Atom]):-atomic(Atom),!.
atoms_of_0([H|T],L):-!,atoms_of_0(H,HL),atoms_of_0(T,TL),append(HL,TL,L),!.
atoms_of_0(C,L):-C=..CL,atoms_of_0(CL,L),!.


:- thread_local lmcache:cannot_save_table/0.
:- thread_local lmcache:cannot_use_any_tables/0.


%= 	 	 

%% skipped_table_call( :GoalCall) is semidet.
%
% Skipped Table Call.
%
skipped_table_call(Call):- cannot_use_tables(cannot_table_call(Call)).

%= 	 	 

%% cannot_table_call( :GoalCall) is semidet.
%
% Cannot Table Call.
%
cannot_table_call(Call):- w_tl( lmcache:cannot_save_table,Call).

%= 	 	 

%% cannot_use_tables( :GoalCall) is semidet.
%
% Cannot Use Tables.
%
cannot_use_tables(Call):- w_tl( lmcache:cannot_use_any_tables,Call).


%= 	 	 

%% call_tabled( :GoalA) is semidet.
%
% Call Tabled.
%
call_tabled(A):-call_tabled(A,A).

%= 	 	 

%% call_tabled( ?Key, :GoalC) is semidet.
%
% Call Tabled.
%
call_tabled(Key,setof(Vars,C,List)):- !,call_setof_tabled(Key,Vars,C,List).
call_tabled(Key,findall(Vars,C,List)):- !,call_setof_tabled(Key,Vars,C,List).
call_tabled(Key,C):- sanity(nonvar(C)), term_variables(C,Vars),!,call_vars_tabled(Key,Vars,C).


%= 	 	 

%% call_vars_tabled( ?Key, ?Vars, :GoalC) is semidet.
%
% Call Variables Tabled.
%
call_vars_tabled(Key,Vars,C):- call_setof_tabled(Key,Vars,C,Set),!,member(Vars,Set).


%= 	 	 

%% call_setof_tabled( ?KeyIn, ?Vars, :GoalC, -List) is semidet.
%
% Call Setof Tabled.
%
call_setof_tabled(KeyIn,Vars,C,List):- make_key(KeyIn+Vars,Key),call_tabled0(Key,Vars,C,List).


%= 	 	 

%% findall_nodupes( ?Vs, :GoalC, -List) is semidet.
%
% Findall Nodupes.
%
findall_nodupes(Vs,C,List):- ground(Vs),!,(C->List=[Vs];List=[]),!.
findall_nodupes(Vs,C,L):- findall(Vs,no_repeats_old(Vs,call_t(C)),L).
%findall_nodupes(Vs,C,L):- setof(Vs,no_repeats_old(Vs,C),L).


% = :- meta_predicate(call_tabled0(?,?,?,?)).
% = :- meta_predicate(call_tabled1(?,?,?,?)).
%call_tabled0(Key,Vars,C,List):- lmcache:maybe_table_key(Key),dmsg(looped_findall_nodupes(Vars,C,List)),fail.

%= 	 	 

%% call_tabled0( ?Key, ?UPARAM2, ?UPARAM3, ?List) is semidet.
%
% Call Tabled Primary Helper.
%
call_tabled0( Key,_,_,List):- reduce_make_key(Key,RKey),lmcache:call_tabled_perm(RKey,List),!.
call_tabled0( Key,_,_,List):- \+ (lmcache:cannot_use_any_tables), lmcache:call_tabled_cached_results(Key,List),!.
call_tabled0(_Key,Vars,C,List):- lmcache:cannot_save_table,!,findall_nodupes(Vars,C,List).

call_tabled0(Key,Vars,C,List):- outside_of_loop_check,!, findall_nodupes(Vars,C,List),
  ignore((really_can_table,!,asserta_if_ground(lmcache:call_tabled_cached_results(Key,List)))),!.

%call_tabled0(Key,Vars,C,List):- lmcache:maybe_table_key(Key),!,findall_nodupes(Vars,C,List).

call_tabled0(Key,Vars,C,List):-call_tabled1(Key,Vars,C,List).


%= 	 	 

%% call_tabled1( ?Key, ?Vars, ?C, ?List) is semidet.
%
% Call Tabled Secondary Helper.
%
call_tabled1(Key,Vars,C,List):- asserta(lmcache:maybe_table_key(Key)), findall_nodupes(Vars,C,List),
  ignore((really_can_table,!,
  % if lmcache:maybe_table_key(Key) is now missing that meant a loop_checker had limited some results
  show_failure(why,retract(lmcache:maybe_table_key(Key))),!,
  asserta_if_ground(lmcache:call_tabled_cached_results(Key,List)))),!.


%= 	 	 

%% really_can_table is semidet.
%
% Really Can Table.
%
really_can_table:- not(test_tl(lmcache:cannot_save_table)),!.


%= 	 	 

%% outside_of_loop_check is semidet.
%
% Outside Of Loop Check.
%
outside_of_loop_check:- (clause(lmcache:ilc(_),B)->B=(!,fail);true).



%system:goal_expansion(LC,LCOO):-nonvar(LC),transitive(lco_goal_expansion,LC,LCO),LC\=@=LCO,must(LCO=LCOO),!.
%system:term_expansion(LC,LCOO):-nonvar(LC),transitive(lco_goal_expansion,LC,LCO),LC\=@=LCO,must(LCO=LCOO),!.
% user:term_expansion(LC,LCOO):-nonvar(LC),(LC=(H:-B)),lco_goal_expansion(B,BE),B\=@=BE,((H:-BE)=LCOO).

%= 	 	 

%% goal_expansion( ?LC, ?LCOO) is semidet.
%
% Hook To [system:goal_expansion/2] For Module Logicmoo_util_loop_check.
% Goal Expansion.
%

system:body_expansion(LC,PIn,LCOO,PIn):- 
   notrace((source_location(_,_),
      compound(LC),
      must(var(LCOO)),
      lco_goal_expansion(LC,LCOO),
      LC\=@=LCOO)).



