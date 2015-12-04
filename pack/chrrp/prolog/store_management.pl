% Store Management Module
%
% Part of the CHR-rp compiler.
%
% Author: Leslie De Koninck, K.U.Leuven, Belgium

:- module(store_management,
	[
	    generate_insert_into_index_code/5,
	    generate_attach_loop_clauses/1,
	    generate_detach_loop_clauses/1,
	    generate_attach_code/4,
	    generate_kill_code/5,
	    generate_show_store_clause/1,
	    generate_store_initializations/1,
	    generate_attr_unify_hook/1,
	    generate_schedule_clauses/1,
	    generate_update_index_clauses/1
	]).

:- use_module(database,
	[
	    all_ground_modes/2,
	    get_all_constraints/1,
	    get_constraint_index/3,
	    get_constraint_indexes/2,
	    get_target_module/1,
	    get_nb_constraint_indexes/2,
	    get_nb_nonground_indexed_constraints/1,
	    get_nonground_indexed_constraint/2,
	    get_nonground_indexed_constraint_index/2,
	    get_indexed_arguments/2,
	    get_sorted_constraint_indexes/2
	]).
:- use_module(suspension,
	[
	    new_suspension/2,
	    suspension_args/3,
	    suspension_term_field/4,
	    suspension_term_field_nb/3
	]).
:- use_module(util,
	[
	    list_to_conj/2
	]).
:- use_module(store,
	[
	    new_store/1
	]).
:- use_module(constraint_code,
	[
	    schedule_code/3
	]).

generate_attr_unify_hook(Clause) :-
	get_target_module(Mod),
	get_nb_nonground_indexed_constraints(NbVIC),
	AttrArity is NbVIC + 1,
	functor(Attr,attr,AttrArity),
	functor(OtherAttr,attr,AttrArity),
	Attr =.. [attr,_|Susps1],
	OtherAttr =.. [attr,OtherId|Susps2],
	sort_merge_code(Susps1,Susps2,Susps3,SortMergeCode),
	MergedAttr =.. [attr,OtherId|Susps3],
	ClauseHead = (Mod:attr_unify_hook(Attr,Value)),
	loop_1(1,Attr,NbVIC,Loop1Code),
	% safe version, check if necessary!!!
	%generate_schedule_call(1,MergedAttr,NbVIC,Schedule1Code), 
	generate_schedule_call(1,Attr,NbVIC,Schedule1Code),
	generate_schedule_call(1,Attr,NbVIC,Schedule2Code),
	ClauseBody =
	(
	    (   var(Value)
	    ->  (   get_attr(Value,Mod,OtherAttr)
		->  SortMergeCode,
		    put_attr(Value,Mod,MergedAttr),
		    Loop1Code,
		    Schedule1Code
		;   put_attr(Value,Mod,Attr),
		    Schedule2Code
		)
	    ;   %AttachIncrement
		throw(unsupported)
	    )
	),
	Clause = (ClauseHead :- ClauseBody).

generate_schedule_call(Index,Attr,Max,Code) :-
	(   Index =< Max
	->  get_nonground_indexed_constraint(Index,FA),
	    Next is Index + 1,
	    arg(Next,Attr,Suspensions),
	    term_to_atom(schedule(FA),HeadAtom),
	    ScheduleCall =.. [HeadAtom,Suspensions],
	    Code = (ScheduleCall,RemCode),
	    loop_2(Next,Attr,Max,RemCode)
	;   Code = true
	).

generate_schedule_clauses(Clauses) :-
	get_nb_nonground_indexed_constraints(NbVIC),
	generate_schedule_clauses(1,NbVIC,Clauses).

generate_schedule_clauses(Index,Max,Clauses) :-
	(   Index =< Max
	->  get_nonground_indexed_constraint(Index,FA),
	    term_to_atom(schedule(FA),HeadAtom),
	    Head1 =.. [HeadAtom,[Susp|Susps]],
	    Head2 =.. [HeadAtom,Susps],
	    Head3 =.. [HeadAtom,[]],
	    Clauses = [Clause1,Head3|RemClauses],
	    schedule_code(FA,Susp,ScheduleCode),
	    Clause1 = ( Head1 :- ScheduleCode, Head2 ),
	    NextIndex is Index + 1,
	    generate_schedule_clauses(NextIndex,Max,RemClauses)
	;   Clauses = []
	).

sort_merge_code([H1|T1],[H2|T2],[H3|T3],Code) :-
	SortMerge =
	(
	    sort(H1,S1),
	    sort(H2,S2),
	    merge_set(S1,S2,H3)
	),
	(   T1 = []
	->  Code = SortMerge,
	    T3 = []
	;   Code = (SortMerge, Remaining),
	    sort_merge_code(T1,T2,T3,Remaining)
	).

loop_1(Index,Attr,Max,Code) :-
	(   Index =< Max
	->  get_nonground_indexed_constraint(Index,FA),
	    term_to_atom(update_index(FA),HeadName),
	    UpdateCall =.. [HeadName,Suspensions],
	    Next is Index + 1,
	    arg(Next,Attr,Suspensions),
	    Code = (UpdateCall,RemCode),
	    loop_1(Next,Attr,Max,RemCode)
	;   Code = true
	).
	
loop_2(Index,Attr,Max,Code) :-
	(   Index =< Max
	->  get_nonground_indexed_constraint(Index,FA),
	    term_to_atom(schedule(FA),HeadName),
	    UpdateCall =.. [HeadName,Suspensions],
	    Next is Index + 1,
	    arg(Next,Attr,Suspensions),
	    Code = (UpdateCall,RemCode),
	    loop_2(Next,Attr,Max,RemCode)
	;   Code = true
	).
	
generate_update_index_clauses(Clauses) :-
	get_nb_nonground_indexed_constraints(NbVIC),
	generate_update_index_clauses(1,NbVIC,Clauses).

generate_update_index_clauses(Index,Max,Clauses) :-
	(   Index =< Max
	->  get_nonground_indexed_constraint(Index,FA),
	    term_to_atom(update_index(FA),HeadName),
	    Head1 =.. [HeadName,[SuspRef|SuspRefs]],
	    Head2 =.. [HeadName,SuspRefs],
	    Head3 =.. [HeadName,[]],
	    new_suspension(FA,Suspension),
	    suspension_args(FA,Suspension,Args),
	    new_suspension(FA,GroundSusp),
	    suspension_args(FA,GroundSusp,GroundArgs),
	    get_indexed_arguments(FA,IndexedArgs),
	    get_constraint_indexes(FA,Indexes),
	    flatten(Indexes,StoreIndexedArgs),
	    sort(StoreIndexedArgs,SortedStoreIndexedArgs),
	    intersection(IndexedArgs,SortedStoreIndexedArgs,MakeGroundArgs),
	    get_key_args(MakeGroundArgs,Args,NonGroundKeyArgs,RemArgs),
	    get_key_args(MakeGroundArgs,GroundArgs,GroundKeyArgs,RemArgs),
	    get_target_module(Mod),
	    make_ground_call_list(NonGroundKeyArgs,Mod,GroundKeyArgs,GCL),
	    list_to_conj(GCL,MakeGroundCall),
	    Body =
	    (
		SuspRef = Suspension,
		MakeGroundCall,
		IndexBody,
		Head2
	    ),
	    get_sorted_constraint_indexes(FA,SortedIndexes),
	    update_index_blocks(SortedIndexes,FA,SuspRef,Suspension,
		GroundSusp,IndexBody),
	    Clauses = [(Head1 :- Body), Head3|RemClauses],
	    NextIndex is Index + 1,
	    generate_update_index_clauses(NextIndex,Max,RemClauses)
	;   Clauses = []
	).

update_index_blocks([_-Indexes|T],FA,SuspRef,Susp,GroundSusp,Code) :-
	Indexes = [FirstIndex|_],
	suspension_term_field(FA,Susp,index(FirstIndex),Field),
	maplist(generate_remove_from_index_code(FA,SuspRef),Indexes,RemoveL),
	list_to_conj(RemoveL,RemoveConj),
	maplist(generate_insert_into_index_code(FA,GroundSusp,SuspRef),Indexes,
		    InsertL),
	list_to_conj(InsertL,InsertConj),
	Code =
	(   nonvar(Field)
	->  RemoveConj,
	    InsertConj,
	    RecursiveCode
	;   true
	),
	update_index_blocks(T,FA,SuspRef,Susp,GroundSusp,RecursiveCode).
update_index_blocks([],_,_,_,_,true).

make_ground_call_list([H1|T1],Mod,[H2|T2],[H3|T3]) :-
	H3 = make_ground(H1,Mod,H2),
	make_ground_call_list(T1,Mod,T2,T3).
make_ground_call_list([],_,[],[]).

generate_kill_code(FA,SuspRef,State,Args,Code) :-
	suspension_term_field_nb(FA,id,Id),
	freeze(Id,
	(
	    (   get_sorted_constraint_indexes(FA,Indexes),
		generate_remove_blocks(Indexes,FA,SuspRef,RemoveConj),
		generate_detach_code(FA,SuspRef,Args,DetachCode),
		Code =
		(
		    setarg(1,State,dead),
		    RemoveConj,
		    DetachCode
		)
	    ->  true
	    ;   writeln(generate_kill_code:failure), fail
	    )
	)).

generate_remove_blocks([_-Indexes|T],FA,SuspRef,Code) :-
	Indexes = [FirstIndex|_],
	suspension_term_field_nb(FA,index(FirstIndex),FieldNb),
	maplist(generate_remove_from_index_code(FA,SuspRef),Indexes,RemoveL),
	list_to_conj(RemoveL,RemoveConj),
	Code =
	(   arg(FieldNb,SuspRef,Field),
	    nonvar(Field)
	->  RemoveConj,
	    RecursiveCode
	;   true
	),
	generate_remove_blocks(T,FA,SuspRef,RecursiveCode).
generate_remove_blocks([],_,_,true).


    
generate_attach_code(FA,Suspension,SuspensionRef,Code) :-
	get_indexed_arguments(FA,IndexedArgs), !,
	suspension_args(FA,Suspension,Args),
	get_key_args(IndexedArgs,Args,KeyArgs),
	term_to_atom(attach(FA),F),
	AttachLoopCall =.. [F,AttachVars,SuspensionRef],
	Code =
	(
	    term_variables(KeyArgs,AttachVars),
	    AttachLoopCall
	).
generate_attach_code(_,_,_,true).

generate_detach_code(FA,SuspRef,Args,Code) :-
	get_indexed_arguments(FA,IndexedArgs), !,
	get_key_args(IndexedArgs,Args,KeyArgs),
	term_to_atom(detach(FA),F),
	DetachLoopCall =.. [F,DetachVars,SuspRef],
	Code =
	(
	    term_variables(KeyArgs,DetachVars),
	    DetachLoopCall
	).
generate_detach_code(_,_,_,true).

make_new_attr(VarId,Attr) :-
	get_nb_nonground_indexed_constraints(Arity),
	length(Args,Arity),
	Attr =.. [attr,VarId|Args],
	maplist(=([]),Args).


generate_attach_loop_clauses(Clauses) :-
	get_nb_nonground_indexed_constraints(NbVIC),
	generate_attach_loop_clauses(1,NbVIC,Clauses).
    
generate_attach_loop_clauses(Index,Max,Clauses) :-
	(   Index =< Max
	->  get_nonground_indexed_constraint(Index,FA),
	    term_to_atom(attach(FA),F),
	    Head1 =.. [F,[H|T],SuspRef],
	    Head2 =.. [F,T,SuspRef],
	    Head3 =.. [F,[],_],
	    get_target_module(Mod),
	    AttrIndex is Index + 1,
	    make_new_attr(VarId,NewAttr),
	    setarg(AttrIndex,NewAttr,[SuspRef]),
	    Clause1 =
	    (
		Head1 :-
		    (   get_attr(H,Mod,Attr)
		    ->  arg(AttrIndex,Attr,Suspensions),
			setarg(AttrIndex,Attr,[SuspRef|Suspensions])
		    ;   gen_id(VarId),
			put_attr(H,Mod,NewAttr)
		    ),
		    Head2
	    ),
	    Clauses = [Clause1,Head3|RemClauses],
	    generate_attach_loop_clauses(AttrIndex,Max,RemClauses)
	;   Clauses = []
	).

generate_detach_loop_clauses(Clauses) :-
	get_nb_nonground_indexed_constraints(NbVIC),
	generate_detach_loop_clauses(1,NbVIC,Clauses).

generate_detach_loop_clauses(Index,Max,Clauses) :-
	(   Index =< Max
	->  get_nonground_indexed_constraint(Index,FA),
	    term_to_atom(detach(FA),F),
	    Head1 =.. [F,[H|T],SuspRef],
	    Head2 =.. [F,T,SuspRef],
	    Head3 =.. [F,[],_],
	    get_target_module(Mod),
	    AttrIndex is Index + 1,
	    suspension_term_field_nb(FA,id,IdFieldNb),
	    Clause1 =
	    (
		Head1 :-
		    get_attr(H,Mod,Attr),
		    arg(AttrIndex,Attr,Suspensions),
		    arg(IdFieldNb,SuspRef,Id),
		    select_by_id(Suspensions,Id,IdFieldNb,
			RemainingSuspensions),
		    setarg(AttrIndex,Attr,RemainingSuspensions),
		    Head2
	    ),
	    Clauses = [Clause1,Head3|RemClauses],
	    generate_detach_loop_clauses(AttrIndex,Max,RemClauses)
	;   Clauses = []
	).


generate_store_initializations(:- initialization Init) :-
	get_all_constraints(FAs),
	maplist(generate_store_inits,FAs,Calls),
	flatten(Calls,Inits),
	list_to_conj(Inits,Init).

generate_store_inits(FA,Calls) :-
	get_constraint_indexes(FA,Indexes),
	init_calls(Indexes,FA,Calls).

init_calls([I|Is],FA,[C|Cs]) :-
	index_name(FA,I,IN),
	new_store(Store),
	C = nb_setval(IN,Store),
	init_calls(Is,FA,Cs).
init_calls([],_,[]).

generate_show_store_clause(Clause) :-
	get_all_constraints(FAs),
	maplist(generate_show_store_literal,FAs,Literals),
	flatten(Literals,Flat),
	list_to_conj(Flat,Body),
	Clause = (show_store :- Body).

generate_show_store_literal(F/A,Literal) :-
	index_name(F/A,[],IN),
	new_suspension(F/A,Suspension),
	suspension_args(F/A,Suspension,Args),
	C =.. [F|Args],
	Literal = 
	(
	    nb_getval(IN,Store),
	    (   store_lookup(a,Store,Suspensions)
	    ->  forall(member(Suspension,Suspensions),writeln(C))
	    ;   true
	    )
	).



% assumes that Suspension is a ground version of the term refered to by
% SuspensionRef
generate_insert_into_index_code(FA,Suspension,SuspensionRef,IndexedArgs,Code) :-
	suspension_args(FA,Suspension,Args),
	get_key_args(IndexedArgs,Args,KeyArgs),
	Key =.. [a|KeyArgs],
	index_name(FA,IndexedArgs,IndexName),
	suspension_term_field_nb(FA,index(IndexedArgs),FieldNb),
	Code =
	(
	    nb_getval(IndexName,Index),
	    insert_in_store(SuspensionRef,Key,Index,Result),
	    setarg(FieldNb,SuspensionRef,key(Key)),
	    (   Result = [_,Next|_]
	    ->  setarg(FieldNb,Next,Result)
	    ;   true
	    )
	).

generate_remove_from_index_code(FA,SuspRef,IndexedArgs,Code) :-
	suspension_term_field_nb(FA,index(IndexedArgs),FieldNb),
	index_name(FA,IndexedArgs,IndexName),
	Code =
	(
	    arg(FieldNb,SuspRef,Field),
	    (   Field = key(Key)
	    ->  nb_getval(IndexName,Index),
		store_lookup(Key,Index,[_|Suspensions]),
		(   Suspensions = []
		->  delete_key(Key,Index)
		;   Suspensions = [Next|_],
		    setarg(FieldNb,Next,key(Key)),
		    delete_first(Key,Index)
		)
	    ;   Field = [_,_|Tail],
		setarg(2,Field,Tail),
		(   Tail = [Next|_]
		->  setarg(FieldNb,Next,Field)
		;   true
		)
	    )
	).
	
	
get_key_args([I|Is],Args,[A|As]) :-
	nth1(I,Args,A),
	get_key_args(Is,Args,As).
get_key_args([],_,[]).

get_key_args(Indexes,Args,KeyArgs,RemArgs) :-
	get_key_args(Indexes,1,Args,KeyArgs,RemArgs).
get_key_args([I|Is],CI,[A|As],KAs,[A|RAs]) :-
	I > CI, !, NI is CI + 1, get_key_args([I|Is],NI,As,KAs,RAs).
get_key_args([I|Is],CI,[A|As],[A|KAs],RAs) :-
	I = CI, !, NI is CI + 1, get_key_args(Is,NI,As,KAs,RAs).
get_key_args([],_,As,[],As).


index_name(F/A,IndexArgs,Name) :- term_to_atom(index(F/A,IndexArgs),Name).
