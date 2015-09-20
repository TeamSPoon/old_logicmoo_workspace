
:-swi_export(transitive/3).
:-meta_predicate(transitive(2,+,-)).
:-meta_predicate(transitive_lc(2,+,-)).
:-meta_predicate(transitive_except(+,2,+,-)).

transitive(X,A,B):- once(debugOnError(call(X,A,R)) -> ( R\=@=A -> transitive_lc(X,R,B) ; B=R); B=A),!.

transitive_lc(X,A,B):-transitive_except([],X,A,B).

transitive_except(NotIn,X,A,B):- memberchk_same(A,NotIn)-> (B=A,!) ;((once(debugOnError(call(X,A,R)) -> ( R\=@=A -> transitive_except([A|NotIn],X,R,B) ; B=R); B=A))),!.

:- meta_predicate no_loop_check(0).
:- meta_predicate no_loop_check(0,0).
:- meta_predicate no_loop_check_term_key(0,?,0).
:- meta_predicate call_tabled(?,0).
:- meta_predicate loop_check(0).
:- meta_predicate loop_check(0,0).
:- meta_predicate loop_check_nr(0).
:- meta_predicate loop_check_true(0).

%:- meta_predicate((loop_check(0,0))).
%:- meta_predicate((no_loop_check(0,0))).
%:- meta_predicate((no_loop_check(0))).
%:- meta_predicate((no_loop_check(0))).
:- meta_predicate((loop_check_term(0,?,0))).
:- meta_predicate((loop_check_term_key(0,?,0))).
%:- meta_predicate((loop_check(0,0))).
%:- meta_predicate((loop_check(0))).


% ===================================================================
% Loop checking
% ===================================================================
:- thread_local tlbugger:ilc/1.

:-meta_predicate(call_t(0)).
% call_t(C0):-reduce_make_key(C0,C),!,table(C),!,query(C).
% call_t(C0):-query(C).
call_t(C):- call(C).

reduce_make_key(call(C),O):-!,reduce_make_key(C,O).
reduce_make_key(call_u(C),O):-!,reduce_make_key(C,O).
reduce_make_key(req(C),O):-!,reduce_make_key(C,O).
reduce_make_key(pfc_call(C),O):-!,reduce_make_key(C,O).
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


cc_key(CC,Key):- cyclic_term(CC),copy_term(CC,CKey,_),numbervars(CKey,0,_),format(atom(Key),'~w',[CKey]),!.
cc_key(CC,Key):- copy_term(CC,Key,_),numbervars(Key,0,_),!.

make_key(M:CC,Key):- atom(M),!,hotrace((ground(CC)->Key=CC ; cc_key(CC,Key))).
make_key(CC,Key):- hotrace((ground(CC)->Key=CC ; cc_key(CC,Key))).


is_loop_checked(Call):-  make_key(Call,Key),!,tlbugger:ilc(Key).



loop_check(Call):- parent_goal(Term,2)->loop_check_term_key(Call,Term+Call,fail).
loop_check(Call, TODO):- parent_goal(Term,2)->loop_check_term_key(Call,Term+Call, TODO).
loop_check_term_key(Call,KeyIn,TODO):- make_key(KeyIn,Key) -> loop_check_term(Call,Key,TODO).


no_loop_check(Call):- parent_goal(Term,2)->no_loop_check_term_key(Call,Term+Call,fail).
no_loop_check(Call, TODO):- parent_goal(Term,2)->no_loop_check_term_key(Call,Term+Call, TODO).
no_loop_check_term_key(Call,KeyIn,TODO):- make_key(KeyIn,Key) -> with_no_assertions(tlbugger:ilc(_),loop_check_term(Call,Key,TODO)).


loop_check_term(Call,Key,TODO):- TT = tlbugger:ilc(Key),
 ( \+(TT) -> (setup_call_cleanup(asserta(TT,REF), Call, 
   erase_safe(TT,REF))) ; 
   ((can_fail(TODO)->retract_can_table;true),call(TODO)) ).

can_fail(G):-not(G=true),not(G=must(_)).

% get_where(When)
get_where(B:L):-get_where0(F:L),file_base_name(F,B).
get_where0(F:L):-source_location(file,F),current_input(S),line_position(S,L),!.
get_where0(F:L):-source_location(F,L),!.
get_where0(A:0):-current_input(S),stream_property(S,alias(A)),!.
get_where0(M:0):-context_module(M),!.
get_where0(user:0):-!.

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
:- swi_export((call_tabled/2)).
:- swi_export((cannot_table_call/1)).
:- swi_export((cannot_use_tables/1)).
:- swi_export((skipped_table_call/1)).
% =====================================================================================================================
:- meta_predicate call_tabled(0).
:- module_transparent call_tabled/1.

:- meta_predicate call_vars_tabled(?,?,0).
:- module_transparent call_vars_tabled/3.

:- meta_predicate((cannot_table_call(0))).
:- meta_predicate((cannot_use_tables(0))).
:- meta_predicate((skipped_table_call(0))).



:- meta_predicate call_setof_tabled(?,?,0,-).
:- meta_predicate findall_nodupes(?,0,-).
:- module_transparent call_setof_tabled/4.

:- dynamic(table_bugger:call_tabled_cached_results/2).
:- dynamic(table_bugger:call_tabled_perm/2).
:- thread_local table_bugger:maybe_table_key/1.

retract_can_table :- retractall(maybe_table_key(_)).

:- meta_predicate(make_key(?,-)).

:- module_transparent((ex)/0).

lex:-listing(tlbugger:ilc(_)),forall(current_predicate(table_bugger:F/A),listing(table_bugger:F/A)),catchvv(listing(user:already_added_this_round),_,true).
(ex):-expire_tabled_list(_),retractall(tlbugger:ilc(_)),dmsg_showall(_),forall(current_predicate(table_bugger:F/A),(functor(RA,F,A),retractall(RA))),catchvv(expire_dont_add,_,true).

expire_tabled_list(V):-var(V),!,retractall(table_bugger:call_tabled_cached_results(_,_)).
expire_tabled_list(K):-compound(K),functor(K,_,1),retractall(table_bugger:call_tabled_cached_results(isa(_,_),_)),retractall(table_bugger:call_tabled_cached_results(isa(_,_)+_,_)),fail.
expire_tabled_list(K):-!,retractall(table_bugger:call_tabled_cached_results(K,_)),retractall(table_bugger:call_tabled_cached_results(K+_,_)).
expire_tabled_list(T):- atoms_of(T,A1), CT= table_bugger:call_tabled_cached_results(Key,List),ignore(((CT,once(any_term_overlap_atoms_of(A1,List);(not(member(Key,List)),
  any_term_overlap_atoms_of(A1,Key))),retractall(CT)),fail)).

any_term_overlap_atoms_of(A1,T2):-atoms_of(T2,A2),!,member(A,A1),member(A,A2),!.

any_term_overlap(T1,T2):- atoms_of(T1,A1),atoms_of(T2,A2),!,member(A,A1),member(A,A2),!.

:-meta_predicate(make_tabled_perm(0)).

make_tabled_perm(Call):- must(really_can_table),must(outside_of_loop_check),
  term_variables(Call,Vars),!,make_key(Vars+Call,LKey),reduce_make_key(LKey,Key),
  findall(Vars,with_no_assertions(tlbugger:cannot_save_table,no_loop_check(no_repeats_old(Call))),KList),
  must(KList=[_|_]),!,
  asserta(table_bugger:call_tabled_perm(Key,KList)),!,
  member(Vars,KList).




:-thread_local tlbugger:cannot_save_table/0.
:-thread_local tlbugger:cannot_use_any_tables/0.

skipped_table_call(Call):- cannot_use_tables(cannot_table_call(Call)).
cannot_table_call(Call):- with_assertions( tlbugger:cannot_save_table,Call).
cannot_use_tables(Call):- with_assertions( tlbugger:cannot_use_any_tables,Call).

call_tabled(A):-call_tabled(A,A).
call_tabled(Key,setof(Vars,C,List)):- !,call_setof_tabled(Key,Vars,C,List).
call_tabled(Key,findall(Vars,C,List)):- !,call_setof_tabled(Key,Vars,C,List).
call_tabled(Key,C):- sanity(nonvar(C)), term_variables(C,Vars),!,call_vars_tabled(Key,Vars,C).

call_vars_tabled(Key,Vars,C):- call_setof_tabled(Key,Vars,C,Set),!,member(Vars,Set).

call_setof_tabled(KeyIn,Vars,C,List):- make_key(KeyIn+Vars,Key),call_tabled0(Key,Vars,C,List).

findall_nodupes(Vs,C,List):- ground(Vs),!,(C->List=[Vs];List=[]),!.
findall_nodupes(Vs,C,L):- findall(Vs,no_repeats_old(Vs,call_t(C)),L).
%findall_nodupes(Vs,C,L):- setof(Vs,no_repeats_old(Vs,C),L).


:-meta_predicate(call_tabled0(?,?,?,?)).
:-meta_predicate(call_tabled1(?,?,?,?)).
%call_tabled0(Key,Vars,C,List):- table_bugger:maybe_table_key(Key),dmsg(looped_findall_nodupes(Vars,C,List)),fail.
call_tabled0( Key,_,_,List):- reduce_make_key(Key,RKey),table_bugger:call_tabled_perm(RKey,List),!.
call_tabled0( Key,_,_,List):- \+ (tlbugger:cannot_use_any_tables), table_bugger:call_tabled_cached_results(Key,List),!.
call_tabled0(_Key,Vars,C,List):- tlbugger:cannot_save_table,!,findall_nodupes(Vars,C,List).

call_tabled0(Key,Vars,C,List):- outside_of_loop_check,!, findall_nodupes(Vars,C,List),
  ignore((really_can_table,!,asserta_if_ground(table_bugger:call_tabled_cached_results(Key,List)))),!.

%call_tabled0(Key,Vars,C,List):- table_bugger:maybe_table_key(Key),!,findall_nodupes(Vars,C,List).

call_tabled0(Key,Vars,C,List):-call_tabled1(Key,Vars,C,List).

call_tabled1(Key,Vars,C,List):- asserta(table_bugger:maybe_table_key(Key)), findall_nodupes(Vars,C,List),
  ignore((really_can_table,!,
  % if table_bugger:maybe_table_key(Key) is now missing that meant a loop_checker had limited some results
  show_call_failure(retract(table_bugger:maybe_table_key(Key))),!,
  asserta_if_ground(table_bugger:call_tabled_cached_results(Key,List)))),!.

really_can_table:- not(test_tl(tlbugger:cannot_save_table)),!.

outside_of_loop_check:- (clause(tlbugger:ilc(_),B)->B=(!,fail);true).



%system:goal_expansion(LC,LCOO):-nonvar(LC),transitive(lco_goal_expansion,LC,LCO),LC\=@=LCO,must(LCO=LCOO),!.
%system:term_expansion(LC,LCOO):-nonvar(LC),transitive(lco_goal_expansion,LC,LCO),LC\=@=LCO,must(LCO=LCOO),!.
% user:term_expansion(LC,LCOO):-nonvar(LC),(LC=(H:-B)),lco_goal_expansion(B,BE),B\=@=BE,((H:-BE)=LCOO).
user:goal_expansion(LC,LCOO):- current_predicate(logicmoo_bugger_loaded/0),once(lco_goal_expansion(LC,LCOO)),LC\=@=LCOO.



