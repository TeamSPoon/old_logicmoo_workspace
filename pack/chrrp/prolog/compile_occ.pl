% Compile Occurrences Module
%
% Part of the CHR-rp compiler.
%
% Author: Leslie De Koninck, K.U.Leuven, Belgium

:- module(compile_occ,
	[
	    compile_occurrences/1
	]).

:- use_module(suspension).
:- use_module(store_management,
	[
	    generate_kill_code/5
	]).
:- use_module(database,
	[
	    all_ground_modes/2,
	    constraint_index_occ/4,
	    get_all_active_occurrences/1,
	    get_head/3,
	    get_highest_priority/2,
	    get_highest_priority_body_constraint/2,
	    get_join_order/3,
	    get_next_occurrence/4,
	    get_next_priority/3,
	    get_occurrence/5,
	    get_rule_body/2,
	    get_rule_guard/2,
	    get_rule_priority/2,
	    get_schedule_depth/3,
	    get_target_module/1,
	    is_propagation_rule/1,
	    needs_check_activation_call/0,
	    needs_check_activation_call/1	    
	]).
:- use_module(util,
	[
	    list_to_conj/2,
	    varlist_substract/3,
	    memberc/2,
	    make_term/3,
	    select_eq/3,
	    zip/3,
	    maplist/5
	]).
:- use_module(constraint_code,
	[
	    generate_inline_constraint_code/2,
	    generate_late_indexing_insert_code/5
	]).
:- use_module(library(assoc)).

compile_occurrences(Clauses) :-
	get_all_active_occurrences(FAPCOcc),
	zip(FAPC,Occ,FAPCOcc),
	zip(FA,PC,FAPC),
	maplist(compile_occ,FA,PC,Occ,Cl),
	flatten(Cl,Clauses),
	fix_suspension_layout.

compile_occ(F/A,PC,Occ,Clauses) :-
	Clauses = [Clause1,Clause2|RemainingClauses],
	Clause1 = (ClauseHead :- Clause1Body),
	Clause2 = (ClauseHead :- ReturnCall),
	get_occurrence(F/A,PC,Occ,RuleId,HeadId),
	get_join_order(RuleId,HeadId,JoinOrder),
	term_to_atom(occurrence(F/A,PC,Occ),ClauseHeadFunctor),
	make_return_call(F/A,PC,Occ,1,[]-[State,Id,Susp],false,ReturnCall),
	ClauseHead =.. [ClauseHeadFunctor,Susp],
	get_head(RuleId,HeadId,Head),
	Head =.. [_|HeadArgs],
	new_suspension(F/A,Suspension),
	suspension_term_field(F/A,Suspension,id,Id),
	suspension_term_field(F/A,Suspension,state,State),
	suspension_args(F/A,Suspension,SuspArgs),
	head_match(HeadArgs,SuspArgs,[],Match),
	split_guard(Match,_,UnsafeMatch),
	list_to_conj_2(Match,MatchConj),
	term_variables(UnsafeMatch,MatchLockVars),
	lock_call(MatchLockVars,MatchLock),
	Context = []-[State,Id,Susp],
	EmptyContext = []-[], 
	(   JoinOrder = []
	->  guard_code(F/A,PC,Occ,MatchLockVars,UnlockVars,GuardCode),
	    unlock_call(UnlockVars,UnlockCall),
	    get_rule_body(RuleId,Body),
	    inline_activation(F/A,PC,Occ,Body,InlinedBody),
	    list_to_conj_2(InlinedBody,BodyConj),
	    kill_code(F/A,PC,Occ,Context,KillCode),
	    make_history_code(F/A,PC,Occ,EmptyContext,F/A,Suspension,
		    HistoryCheck,AddToHistoryCall),
	    PreCommit = [Susp = Suspension, arg(1,State,alive), MatchLock, 
			 MatchConj, GuardCode, HistoryCheck],
	    list_to_conj_2(PreCommit,PreCommitConj),
	    (   get_schedule_depth(RuleId,HeadId,ScheduleDepth)
	    ->  ScheduleDepth = 0,
		dynamic_body_head(F/A,PC,Occ,0,1,DynBodyHead,Susp,EmptyContext),
		get_rule_priority(RuleId,Priority),
		(   var(Priority)
		->  ScheduleCall = insert(Priority,DynBodyHead)
		;   ScheduleCall = 
			( Prio is Priority, insert(Prio,DynBodyHead) )
		),
		Clause1Body = 
		( 
		    PreCommitConj, !,
		    UnlockCall,
		    ScheduleCall,
		    ReturnCall
		),
		make_history_code_dyn_body(F/A,PC,Occ,EmptyContext,F/A,Susp,
		    AddToHistoryCall2),
		suspension_term_field_nb(F/A,state,StateField),
		make_still_alive_call(Context,StillAliveCall),
		PostCommit2 = [AddToHistoryCall2, KillCode, BodyConj],
		list_to_conj_2(PostCommit2,PostCommitConj2),
		RemainingClauses = 
		[(
		    DynBodyHead :- 
			    (   arg(StateField,Susp,State),
				StillAliveCall
			    ->  PostCommitConj2
			    ;   true
			    )
		)]
	    ;	make_check_activation_call(F/A,PC,Occ,CheckActivationCall),
		(   HeadId = k(_)
		->  MaybeReturnCall = ReturnCall
		;   MaybeReturnCall = true
		),
		PostCommit = [UnlockCall, AddToHistoryCall, KillCode, BodyConj,
			  CheckActivationCall,MaybeReturnCall],
		list_to_conj_2(PostCommit,PostCommitConj),
		Clause1Body = ( PreCommitConj, !, PostCommitConj ),
		RemainingClauses = []
	    )
	;   JoinOrder = [Next|_],
	    occurrence_head(F/A,PC,Occ,1,1,Rec1HeadA,Susps,Context),
	    (   get_schedule_depth(RuleId,HeadId,0)
	    ->  get_rule_priority(RuleId,Priority),
		Rec1Head = 
		(
		    Prio is Priority,
		    insert(Prio,Rec1HeadA),
		    ReturnCall
		)	    
	    ;   Rec1Head = Rec1HeadA
	    ),
	    get_head(RuleId,Next,NextHead),
	    get_occurrence(_,_,NextOcc,RuleId,Next),
	    get_known_vars(F/A,PC,Occ,1,KnownVars),
	    select_lookup_args(NextHead,KnownVars,LA),
	    lookup(NextHead,LA,PC,NextOcc,Key,Susps,Lookup),
	    lookup_key(NextHead,LA,Key),
	    PreCommit = [Susp = Suspension, arg(1,State,alive), MatchLock, 
			 MatchConj, Lookup],
	    delete(PreCommit,true,PreCommitFiltered),
	    list_to_conj(PreCommitFiltered,PreCommitConj),
	    Clause1Body = ( PreCommitConj, !, Rec1Head ),
	    compile_occ_loop(RuleId,F/A,PC,Occ,1,1,JoinOrder,MatchLockVars,
		RemainingClauses)
	).

guard_code(FA,PC,Occ,InVars,OutVars,Code) :-
	get_occurrence(FA,PC,Occ,RuleId,_),
	get_rule_guard(RuleId,Guard),
	list_to_conj(Guard,GuardConj),
	split_guard(Guard,_,GeneralGuard),
	term_variables(GeneralGuard,GuardVars),
	varlist_substract(GuardVars,InVars,GuardLockVars),
	lock_call(GuardLockVars,GuardLock),
	term_variables(InVars+GuardLockVars,OutVars),
	list_to_conj_2([GuardConj,GuardLock],Code).


compile_occ_loop(RuleId,FA,PC,Occ,Depth,Actives,[H|T],LockVars,Clauses) :-
	Clauses = [Clause1,Clause2|RemainingClauses],
	Clause1 = (Clause1Head :- Clause1Body),
	Clause2 = (Clause2Head :- ReturnCall),
	occurrence_head(FA,PC,Occ,Depth,Actives,Clause1Head,[Susp|Susps],
	    Context),
	occurrence_head(FA,PC,Occ,Depth,Actives,Clause2Head,[],Context),
	occurrence_head(FA,PC,Occ,Depth,Actives,Rec2Head,Susps,Context),
	make_return_call(FA,PC,Occ,Depth,Context,true,ReturnCall),
	get_head(RuleId,H,Head),
	Head =.. [_|HeadArgs],
	functor(Head,F,A),
	new_suspension(F/A,Suspension),
	suspension_term_field(F/A,Suspension,id,Id),
	suspension_term_field(F/A,Suspension,state,State),
	suspension_args(F/A,Suspension,SuspArgs),
	get_known_vars(FA,PC,Occ,Depth,KnownVars),
	head_match(HeadArgs,SuspArgs,KnownVars,Match),
	split_guard(Match,_,UnsafeMatch),
	list_to_conj(Match,MatchConj),
	term_variables(UnsafeMatch,MatchVars),
	varlist_substract(MatchVars,LockVars,MatchLockVars),
	lock_call(MatchLockVars,MatchLock),
	term_variables(LockVars+MatchVars,NewLockVars),
	mutual_exclusive_testing(FA,PC,Occ,Depth,Actives,Context,Id,
	    MutualExclusive),	
	(   T = []
	->  guard_code(FA,PC,Occ,NewLockVars,UnlockVars,GuardCode),
	    unlock_call(UnlockVars,UnlockCall),
	    get_rule_body(RuleId,Body),
	    inline_activation(FA,PC,Occ,Body,InlinedBody),
	    list_to_conj(InlinedBody,BodyConj),
	    make_check_activation_call(FA,PC,Occ,CheckActivationCall),
	    add_to_context([State,Id,Susp,Susps],Context,KillContext),
	    kill_code(FA,PC,Occ,KillContext,KillCode),
	    make_continuation_call(FA,PC,Occ,Susps,Context,ContinuationCall),
	    make_history_code(FA,PC,Occ,Context,F/A,Suspension,HistoryCheck,
		AddToHistoryCall),
	    get_occurrence(FA,PC,Occ,RuleId,HeadId),
	    (   get_schedule_depth(RuleId,HeadId,ScheduleDepth)
	    ->	(   ScheduleDepth =:= Depth + Actives - 2 % 1st dyn. scheduled
		->  make_still_alive_call(Context,StillAliveCall),
		    Clause1Body = 
		    (
			(   StillAliveCall
			->  InnerClauseBody
			;   true
			)
		    ),
		    RemainingClauses = []
		;   ScheduleDepth =:= Depth + Actives - 1 % dyn. scheduled body
		->  dynamic_body_head(FA,PC,Occ,Depth,Actives,DynBodyHead,Susp,
			Context),
		    get_rule_priority(RuleId,Priority),
		    (   var(Priority)
		    ->  ScheduleCall = insert(Priority,DynBodyHead)
		    ;   ScheduleCall = 
			    ( Prio is Priority, insert(Prio,DynBodyHead) )
		    ),
		    Clause1Body = 
		    (   PreCommitConj
		    ->  UnlockCall,
			ScheduleCall,
			Rec2Head
		    ;   Rec2Head
		    ),
		    make_history_code_dyn_body(FA,PC,Occ,Context,F/A,
			Susp,AddToHistoryCall2),
		    suspension_term_field_nb(F/A,state,LastStateField),
		    % was: add_to_context([LastState,_,_,_],Context,AliveCheckContext),
		    add_to_context([State,_,_,_],Context,AliveCheckContext),
		    make_still_alive_call(AliveCheckContext,StillAliveCall),
		    PostCommit2 = [AddToHistoryCall2, KillCode, BodyConj],
		    list_to_conj_2(PostCommit2,PostCommitConj2),
		    RemainingClauses = 
		    [(
			DynBodyHead :- 
				(   % was: arg(LastStateField,Susp,LastState),
				    arg(LastStateField,Susp,State),
				    StillAliveCall
				->  PostCommitConj2
				;   true
				)
		    )]
		;   ScheduleDepth < Depth + Actives - 1
		->  Clause1Body = InnerClauseBody,
		    RemainingClauses = []
		;   writeln(compile_occ_loop:failure), fail
		) 
	    ;	Clause1Body = InnerClauseBody,
		RemainingClauses = []
	    ),
	    PreCommit = [Susp = Suspension, arg(1,State,alive), 
			 MutualExclusive, MatchLock, MatchConj, GuardCode, 
			 HistoryCheck],
	    list_to_conj_2(PreCommit,PreCommitConj),
	    PostCommit = [UnlockCall, AddToHistoryCall, KillCode, BodyConj,
			  CheckActivationCall, ContinuationCall],
	    list_to_conj_2(PostCommit,PostCommitConj),
	    InnerClauseBody =
	    ( 
		(   PreCommitConj
		->  PostCommitConj
		;   Rec2Head
		)
	    )
	;   T = [Next|_],
	    add_to_context([State,Id,Susp,Susps],Context,NewContext),
	    get_occurrence(FA,PC,Occ,RuleId,HeadId),
	    (   Actives = 1,
		get_schedule_depth(RuleId,HeadId,Depth)
	    ->  context_1_to_2(NewContext,NextContext),
		NextActives is Actives + Depth,
		NextDepth = 1,
		get_rule_priority(RuleId,Priority),
		Rec1Head =
		(
		    Prio is Priority,
		    insert(Prio,Rec1HeadA),
		    Rec2Head
		)
	    ;   NextContext = NewContext,
		NextDepth is Depth + 1,
		NextActives = Actives,
		Rec1HeadA = Rec1Head
	    ),
	    occurrence_head(FA,PC,Occ,NextDepth,NextActives,Rec1HeadA,
		NextSusps,NextContext),
	    get_head(RuleId,Next,NextHead),
	    get_known_vars(FA,PC,Occ,NextDepth,NextKnownVars),
	    select_lookup_args(NextHead,NextKnownVars,LA),
	    get_occurrence(_,_,NextOcc,RuleId,Next),
	    lookup(NextHead,LA,PC,NextOcc,Key,NextSusps,Lookup),
	    lookup_key(NextHead,LA,Key),
	    (   get_schedule_depth(RuleId,HeadId,ScheduleDepth)
	    ->	(   ScheduleDepth =:= Depth + Actives - 2 % 1st dyn. scheduled
		->  make_still_alive_call(Context,StillAliveCall),
		    Clause1Body = 
		    (
			(   StillAliveCall
			->  InnerClauseBody
			;   true
			)
		    )
		;   Clause1Body = InnerClauseBody
		) 
	    ;	Clause1Body = InnerClauseBody
	    ),
	    PreCommit = [Susp = Suspension, arg(1,State,alive), 
			 MutualExclusive, MatchLock, MatchConj, Lookup],
	    list_to_conj_2(PreCommit,PreCommitConj),
	    InnerClauseBody =
	    (
		(   PreCommitConj
		->  Rec1Head
		;   Rec2Head
		)
	    ),
	    compile_occ_loop(RuleId,FA,PC,Occ,NextDepth,NextActives,T,
		NewLockVars,RemainingClauses)	    
	).


add_to_context([State,Id,Susp,Susps],Context,NewContext) :-
	Context = Context1-Context2,
	NewContext = [State,Id,Susp,Susps|Context1]-Context2.


context_1_to_2([State,Id,Susp,_|Context1]-Context2,
    []-[State,Id,Susp|RemainingContext2]) :- 
	context_1_to_2(Context1-Context2,[]-RemainingContext2).
context_1_to_2([]-Context2,[]-Context2).

% Replaces the highest priority body constraint by an inlined activation
inline_activation(FA,PC,Occ,Body,InlinedBody) :-
	get_occurrence(FA,PC,Occ,RuleId,HeadId),
	(   get_highest_priority_body_constraint(RuleId,BodyConstraint),
	    functor(BodyConstraint,BCF,BCA),
	    get_highest_priority(BCF/BCA,BCPrio),
	    ( HeadId = r(_) ; BCPrio < PC )
	->  select_eq(Body,BodyConstraint,RemainingBody),
	    generate_inline_constraint_code(BodyConstraint,InlineCode),
	    append(RemainingBody,[InlineCode],InlinedBody)
	;   InlinedBody = Body
	).


make_return_call(FA,PC,Occ,Depth,Context,LevelDownFlag,ReturnCall) :-
	get_occurrence(FA,PC,Occ,RuleId,HeadId),
	Context = Context1-Context2,
	(   Depth > 1
	->  PrevDepth is Depth - 1,
	    Context1 = [_,_,_,PrevSusps|PrevContext1],
	    length(Context2,C2Length),
	    Actives is C2Length/3,
	    occurrence_head(FA,PC,Occ,PrevDepth,Actives,ReturnCall,PrevSusps,
		PrevContext1-Context2)
	;   Context2 = [_,_,ActiveSuspRef],
	    \+ (LevelDownFlag = true, get_schedule_depth(RuleId,HeadId,0) )
	->  (   get_next_occurrence(FA,PC,Occ,NextOcc)
	    ->  term_to_atom(occurrence(FA,PC,NextOcc),NextOccCallFunctor),
		ReturnCall =.. [NextOccCallFunctor,ActiveSuspRef]
	    ;   generate_late_indexing_insert_code(FA,PC,Occ,ActiveSuspRef,
		    LateInsert),
		(   PC = (dynamic)
		->  ReturnCall = true
		;   get_next_priority(FA,PC,NextPC)
		->  term_to_atom(occurrence(FA,NextPC,1),ScheduleCallFunctor),
		    ScheduleCall =.. [ScheduleCallFunctor,ActiveSuspRef],
		    ReturnCall = (LateInsert, insert(NextPC,ScheduleCall))
		;   ReturnCall = LateInsert
		)
	    )
	;   ReturnCall = true % activated at dynamic priority
	).

%lock_call(_,true).
%/*
lock_call(Vars,Call) :- 
	maplist(make_term(lock),Vars,LockList),
	list_to_conj(LockList,Call).
%*/
%unlock_call(_,true).
%/*
unlock_call(Vars,Call) :-
	maplist(make_term(unlock),Vars,UnlockList),
	list_to_conj(UnlockList,Call).
%*/
make_check_activation_call(FA,PC,Occ,CheckActivationCall) :-
	get_occurrence(FA,PC,Occ,RuleId,HeadId),
	(   HeadId = r(_), % active constraint is removed
	    \+ needs_check_activation_call % optimization = on
	->  CheckActivationCall = true
	;   get_rule_priority(RuleId,PriorityExpression),
	    needs_check_activation_call(RuleId) % by body analysis
	->  (   integer(PriorityExpression)
	    ->  CheckActivationCall = check_activation(PriorityExpression)
	    ;   var(PriorityExpression)
	    ->  CheckActivationCall = check_activation(PriorityExpression)
	    ;   CheckActivationCall =
		(
		    Priority is PriorityExpression,
		    check_activation(Priority)
		)
	    )
	;   CheckActivationCall = true
	).


make_history_code_dyn_body(FA,PC,Occ,Context,LastFA,LastSuspRef,
    AddToHistoryCall) :-
	get_occurrence(FA,PC,Occ,RuleId,_),
	(   is_propagation_rule(RuleId)
	->  suspension_term_field_nb(LastFA,id,LastIdField),
	    history_tuple_dyn_body(FA,PC,Occ,Context,LastId,
		HistoryTuple),
	    suspension_term_field_nb(LastFA,history,HistoryField),
	    AddToHistoryCall = 
	    (
		arg(LastIdField,LastSuspRef,LastId),
		TupleRef = HistoryTuple,
		term_hash(TupleRef,Hash),
		arg(HistoryField,LastSuspRef,History),
		add_to_history(History,Hash,TupleRef)
	    )
	;   AddToHistoryCall = true
	).

make_history_code(FA,PC,Occ,Context,LastFA,LastSuspension,HistoryCheck,
    AddToHistoryCall) :-
	get_occurrence(FA,PC,Occ,RuleId,_),
	(   is_propagation_rule(RuleId)
	->  history_tuple(FA,PC,Occ,Context,LastFA,LastSuspension,
		HistoryTuple),
	    history_checks(FA,PC,Occ,Context,LastFA,LastSuspension,
		Hash,TupleRef,Checks),
	    HistoryCheck =
	    (
		TupleRef = HistoryTuple,
		term_hash(TupleRef,Hash),
		Checks
	    ),
	    suspension_term_field(LastFA,LastSuspension,history,LastHistory),
	    AddToHistoryCall = add_to_history(LastHistory,Hash,TupleRef)
	;   HistoryCheck = true,
	    AddToHistoryCall = true
	).	

history_tuple(FA,PC,Occ,Context,LastFA,LastSuspension,HistoryTuple) :-
	get_occurrence(FA,PC,Occ,RuleId,HeadId),
	get_ids_from_context(Context,Ids),
	suspension_term_field(LastFA,LastSuspension,id,LastId),
	append(Ids,[LastId],AllIds),
	get_join_order(RuleId,HeadId,JoinOrder),
	zip([HeadId|JoinOrder],AllIds,Zipped),
	list_to_assoc(Zipped,Assoc),
	assoc_to_list(Assoc,SortedZipped),
	zip(_,TupleIds,SortedZipped),
	HistoryTuple =.. [t,RuleId|TupleIds].
	
history_tuple_dyn_body(FA,PC,Occ,Context,LastId,HistoryTuple) :-
	get_occurrence(FA,PC,Occ,RuleId,HeadId),
	get_ids_from_context(Context,Ids),
	append(Ids,[LastId],AllIds),
	get_join_order(RuleId,HeadId,JoinOrder),
	zip([HeadId|JoinOrder],AllIds,Zipped),
	list_to_assoc(Zipped,Assoc),
	assoc_to_list(Assoc,SortedZipped),
	zip(_,TupleIds,SortedZipped),
	HistoryTuple =.. [t,RuleId|TupleIds].
	
history_checks(FA,PC,Occ,Context,LastFA,LastSuspension,Hash,TupleRef,
    ChecksConj) :-
	get_occurrence(FA,PC,Occ,RuleId,HeadId),
	get_join_order(RuleId,HeadId,JoinOrder),
	maplist(get_head(RuleId),[HeadId|JoinOrder],Heads),
	get_refs_from_context(Context,Refs),
	context_history_checks(Refs,Heads,Hash,TupleRef,ContextChecks),
	suspension_term_field(LastFA,LastSuspension,history,LastHistory),
	append(ContextChecks,[not_in_history(LastHistory,Hash,TupleRef)],
	    Checks),
	list_to_conj(Checks,ChecksConj).
	
context_history_checks([R|Rs],[H|Hs],Hash,TupleRef,[C|Cs]) :-
	functor(H,F,A),
	suspension_term_field_nb(F/A,history,HistoryFieldNb),
	C = 
	( 
	    arg(HistoryFieldNb,R,History),
	    not_in_history(History,Hash,TupleRef)
	),
	context_history_checks(Rs,Hs,Hash,TupleRef,Cs).
context_history_checks([],_,_,_,[]).	
	

% Kill Code

kill_code(FA,PC,Occ,Context,Code) :-
	get_refs_from_context(Context,Refs),
	get_states_from_context(Context,States),
	get_occurrence(FA,PC,Occ,RuleId,HeadId),
	get_join_order(RuleId,HeadId,JoinOrder),
	maplist(get_head(RuleId),[HeadId|JoinOrder],Heads),
	kill_code_2([HeadId|JoinOrder],Heads,Refs,States,Codes),
	list_to_conj(Codes,Code).

kill_code_2([I|Is],[H|Hs],[R|Rs],[S|Ss],[C|Cs]) :-
	I = r(_), !,
	H =.. [F|Args],
	length(Args,A),
	generate_kill_code(F/A,R,S,Args,C),
	kill_code_2(Is,Hs,Rs,Ss,Cs).
kill_code_2([_|Is],[_|Hs],[_|Rs],[_|Ss],Cs) :- kill_code_2(Is,Hs,Rs,Ss,Cs).
kill_code_2([],[],[],[],[]).

% Continuation Call


make_still_alive_call(Context,Call) :-
	get_states_from_context(Context,States),
	make_still_alive_call_1(States,Calls),
	list_to_conj(Calls,Call).

make_still_alive_call_1([H|T],[arg(1,H,alive)|Cs]) :-
	make_still_alive_call_1(T,Cs).
make_still_alive_call_1([],[]).


make_continuation_call(FA,PC,Occ,Susps,Context,Call) :-
	get_states_from_context(Context,States),
	get_occurrence(FA,PC,Occ,RuleId,HeadId),
	get_join_order(RuleId,HeadId,JoinOrder),
	(   nth1(N,[HeadId|JoinOrder],r(_)) % first removed head
	->  N1 is N - 1,
	    length(CheckStates,N1),
	    append(CheckStates,_,States)
	;   CheckStates = States
	),
	Context = Context1-Context2,
	length(Context2,C2L),
	Actives is C2L/3,
	check_states_call(CheckStates,0,Actives,FA,PC,Occ,
	    [n,n,n,Susps|Context1]-Context2,Call).

check_states_call(Hs,Depth,Actives,FA,PC,Occ,Context,CheckStatesCall) :-
	Hs = [_|_], !,
	(   Depth = 0
	->  length(ActiveHeads,Actives),
	    (   append(ActiveHeads,T,Hs)
	    ->  alive_checks(ActiveHeads,AliveChecks)
	    ;   alive_checks(Hs,AliveChecks), % just a guess
		T = []
	    ),
	    %alive_checks(ActiveHeads,AliveChecks),
	    list_to_conj(AliveChecks,AliveChecksConj),
	    CheckStatesCall =
	    (
	        (   AliveChecksConj
		->  RecursiveCall
		;   true
		)
	    )
	;   Hs = [H|T],
	    Context = Context1-Context2,
	    SubContext1Length is (Depth-1)*4,
	    length(SubContext1,SubContext1Length),
	    append(_,[Susps|SubContext1],Context1),
	    occurrence_head(FA,PC,Occ,Depth,Actives,JumpBack,Susps,
		SubContext1-Context2),
	    CheckStatesCall = 
	    (
		(   arg(1,H,alive)
		->  RecursiveCall
		;   JumpBack
		)
	    )
	),
	NextDepth is Depth + 1,
	check_states_call(T,NextDepth,Actives,FA,PC,Occ,Context,RecursiveCall).
check_states_call([],Depth,Actives,FA,PC,Occ,Context,CheckStatesCall) :-
	(   Depth = 0
	->  CheckStatesCall = true
	;   Context = Context1-Context2,
	    SubContext1Length is (Depth-1)*4,
	    length(SubContext1,SubContext1Length),
	    append(_,[Susps|SubContext1],Context1),
	    occurrence_head(FA,PC,Occ,Depth,Actives,CheckStatesCall,Susps,
		SubContext1-Context2)
	).

alive_checks([H|T],[arg(1,H,alive)|Cs]) :- alive_checks(T,Cs).
alive_checks([],[]).

% Context Utils

get_refs_from_context(Context,Refs) :- 
	Context = Context1-Context2,
	get_refs_from_context_1(Context1,Context1Refs),
	get_refs_from_context_2(Context2,Context2Refs),
	append(Context1Refs,Context2Refs,RevRefs),
	reverse(RevRefs,Refs).

get_refs_from_context_1([_,_,Ref,_|Context],[Ref|Refs]) :- !,
	get_refs_from_context_1(Context,Refs).
get_refs_from_context_1([],[]).

get_refs_from_context_2([_,_,Ref|Context],[Ref|Refs]) :- !,
	get_refs_from_context_2(Context,Refs).
get_refs_from_context_2([],[]).

get_states_from_context(Context,States) :- 
	Context = Context1-Context2,
	get_states_from_context_1(Context1,Context1States),
	get_states_from_context_2(Context2,Context2States),
	append(Context1States,Context2States,RevStates),
	reverse(RevStates,States).

get_states_from_context_1([State,_,_,_|Context],[State|States]) :- !,
	get_states_from_context_1(Context,States).
get_states_from_context_1([],[]).

get_states_from_context_2([State,_,_|Context],[State|States]) :- !,
	get_states_from_context_2(Context,States).
get_states_from_context_2([],[]).

get_ids_from_context(Context,Ids) :- 
	Context = Context1-Context2,
	get_ids_from_context_1(Context1,Context1Ids),
	get_ids_from_context_2(Context2,Context2Ids),
	append(Context1Ids,Context2Ids,RevIds),
	reverse(RevIds,Ids).

get_ids_from_context_1([_,Id,_,_|Context],[Id|Ids]) :- !,
	get_ids_from_context_1(Context,Ids).
get_ids_from_context_1([],[]).

get_ids_from_context_2([_,Id,_|Context],[Id|Ids]) :- !,
	get_ids_from_context_2(Context,Ids).
get_ids_from_context_2([],[]).


%%%

occurrence_head(FA,PC,Occ,Depth,Actives,Head,Susps,Context1-Context2) :-
	Context1Length is (Depth - 1) * 4,
	Context2Length is Actives * 3,
	length(Context1,Context1Length),
	length(Context2,Context2Length),
	RealDepth is Depth + Actives - 1,
	term_to_atom(occurrence(FA,PC,Occ,RealDepth),Functor),
	get_known_vars(FA,PC,Occ,RealDepth,KnownVars),
	append([Susps|Context1],Context2,Temp),
	append(Temp,KnownVars,Args),
	Head =.. [Functor|Args].

dynamic_body_head(FA,PC,Occ,0,1,DynBodyHead,Susp,Context) :- !,
	Context = []-[],
	term_to_atom(occurrence_body(FA,PC,Occ),Functor),
	get_known_vars(FA,PC,Occ,1,KnownVars),
	DynBodyHead =.. [Functor,Susp|KnownVars].
dynamic_body_head(FA,PC,Occ,Depth,Actives,DynBodyHead,Susp,Context) :- 
	Context = Context1-Context2,
	Context1Length is (Depth - 1) * 4,
	Context2Length is Actives * 3,
	length(Context1,Context1Length),
	length(Context2,Context2Length),
	RealDepth is Depth + Actives,
	term_to_atom(occurrence_body(FA,PC,Occ),Functor),
	get_known_vars(FA,PC,Occ,RealDepth,KnownVars),
	append([Susp|Context1],Context2,Temp),
	append(Temp,KnownVars,Args),
	DynBodyHead =.. [Functor|Args].

get_known_vars(FA,PC,Occ,Depth,KnownVars) :-
	get_occurrence(FA,PC,Occ,RuleId,HeadId),
	get_join_order(RuleId,HeadId,JoinOrder),
	length([_|AlreadyJoined],Depth),
	append(AlreadyJoined,_,JoinOrder),
	maplist(get_head(RuleId),[HeadId|AlreadyJoined],KnownHeads),
	term_variables(KnownHeads,KnownVars).
	
mutual_exclusive_testing(FA,PC,Occ,Depth,Alives,Context,CurrentId,
    MutualExclusive) :-
	get_occurrence(FA,PC,Occ,RuleId,HeadId),
	get_join_order(RuleId,HeadId,JoinOrder),
	Size is Depth + Alives - 2,
	length(AlreadyJoined,Size),
	append(AlreadyJoined,[Current|_],JoinOrder),
	maplist(get_head(RuleId),[HeadId|AlreadyJoined],KnownHeads),
	get_head(RuleId,Current,CurrentHead),
	reverse(KnownHeads,ReverseKnownHeads),
	mutual_exclusive_testing(ReverseKnownHeads,Context,CurrentHead,
	    CurrentId,MutualExclusiveList),
	list_to_conj(MutualExclusiveList,MutualExclusive).

mutual_exclusive_testing([H|T],[_,Id,_,_|Context1]-Context2,Current,CurrentId,
    List) :- !,
	(   unifiable(H,Current,_)
	->  List = [CurrentId \== Id|RemainingList],
	    mutual_exclusive_testing(T,Context1-Context2,Current,CurrentId,
		RemainingList)
	;   mutual_exclusive_testing(T,Context1-Context2,Current,CurrentId,
		List)
	).
mutual_exclusive_testing([H|T],[]-[_,Id,_|Context2],Current,CurrentId,List) :-
	!,	
	(   unifiable(H,Current,_)
	->  List = [CurrentId \== Id|RemainingList],
	    mutual_exclusive_testing(T,[]-Context2,Current,CurrentId,
		RemainingList)
	;   mutual_exclusive_testing(T,[]-Context2,Current,CurrentId,List)
	).
mutual_exclusive_testing([],[]-[],_,_,[]).

lookup_key(Head,Args,Key) :-
	lookup_key_args(Head,Args,KeyArgs),
	Key =.. [a|KeyArgs].

lookup_key_args(Head,[H|T],[H2|T2]) :-
	arg(H,Head,H2),
	lookup_key_args(Head,T,T2).
lookup_key_args(_,[],[]).

head_match([H1|T1],[H2|T2],PrevVars,Match) :-
	(   var(H1)
	->  (   \+ memberc(H1,PrevVars)
	    ->  H1 = H2,
		NewPrevVars = [H1|PrevVars],
		head_match(T1,T2,NewPrevVars,Match)
	    ;   Match = [H1 == H2|RemainingMatch],
		term_variables(H1+PrevVars,NewPrevVars),
		head_match(T1,T2,NewPrevVars,RemainingMatch)
	    )
	;   H1 =.. [F|Args1],
	    length(Args1,Len),
	    length(Args2,Len),
	    H2Templ =.. [F|Args2],
	    Match = [(nonvar(H2), H2 = H2Templ)|RemainingMatch],
	    %term_variables(H1+PrevVars,NewPrevVars),
	    append(Args1,T1,NewL1),
	    append(Args2,T2,NewL2),
	    head_match(NewL1,NewL2,PrevVars,RemainingMatch)
	).
head_match([],[],_,[]).
	
	

select_lookup_args(Head,KnownVars,LA) :- 
	Head =.. [_|Args],
	findall(I,(nth1(I,Args,Arg),is_lookup_arg(Arg,KnownVars)),LA).	

is_lookup_arg(Arg,KnownVars) :-
	term_variables(Arg,ArgVars),
	forall(member(Var,ArgVars),memberc(Var,KnownVars)).

	
lookup(Head,Args,PC,Occ,Key,Vals,Code) :-
	functor(Head,F,A),
	term_to_atom(index(F/A,Args),Atom),
	constraint_index_occ(F/A,Args,PC,Occ),
	(   all_ground_modes(F/A,Args)
	->  MakeGroundCall = true,
	    GroundKey = Key
	;   get_target_module(Mod),
	    MakeGroundCall = make_ground(Key,Mod,GroundKey)
	),
	Code =
	(
	    nb_getval(Atom,HT),
	    MakeGroundCall,
	    store_lookup(GroundKey,HT,Vals)
	).


% Safe guards cannot instantiate variables
split_guard([H|T],SafeGuard,GeneralGuard) :-
	(   safe_guard(H)
	->  SafeGuard = [H|SafeGuards],
	    split_guard(T,SafeGuards,GeneralGuard)
	;   GeneralGuard = [H|GeneralGuards],
	    split_guard(T,SafeGuard,GeneralGuards)
	).
split_guard([],[],[]).

safe_guard(_<_).
safe_guard(_>_).
safe_guard(_=<_).
safe_guard(_>=_).
safe_guard(_=:=_).
safe_guard(_=\=_).
safe_guard(_==_).
safe_guard(_\==_).
safe_guard(select(_,_,_)). % FALSE!!!
safe_guard((nonvar(X),X=Templ)) :-
	(   var(Templ)
	->  true
	;   Templ =.. [_|Args],
	    forall(member(X,Args),var(X))
	).

list_to_conj_2(List,Conj) :-
	(   List = []
	->  Conj = true
	;   maplist(make_term(nonvar),List,NonvarCondition),
	    list_to_conj(NonvarCondition,When),
	    when(When,
		(
		    delete(List,true,Filtered),
		    list_to_conj(Filtered,Conj)
		)
	    )
	).
