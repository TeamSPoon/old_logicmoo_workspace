% Constraint Code Module
%
% Part of the CHR-rp compiler.
%
% Author: Leslie De Koninck, K.U.Leuven, Belgium

:- module(constraint_code,
	[
	    generate_constraint_code/1,
	    generate_inline_constraint_code/2,
	    generate_late_indexing_insert_code/5,
	    schedule_code/3
	]).
:- use_module(database,
	[
	    get_all_constraints/1,
	    get_constraint_priorities/2,
	    get_highest_priority/2,
	    get_indexed_arguments/2,
	    get_nonground_indexed_constraint_index/2,
	    get_occurrence/5,
	    get_sorted_constraint_indexes/2,
	    get_target_module/1
	]).
:- use_module(suspension,
	[
	    check_uses_history/1,
	    new_suspension/2,
	    suspension_args/3,
	    suspension_term_field/4
	]).
:- use_module(store_management,
	[
	    generate_attach_code/4,
	    generate_insert_into_index_code/5
	]).
:- use_module(history,
	[
	    new_history/1
	]).
:- use_module(scheduling,
	[
	    schedule_code/3
	]).
:- use_module(util,
	[
	    list_to_conj/2
	]).

generate_constraint_code(Code) :-
	get_all_constraints(FAs),
	maplist(generate_constraint_code,FAs,Clauses1),
	flatten(Clauses1,Code).

generate_constraint_code(F/A,Code) :-
	new_suspension(F/A,Susp),
	suspension_args(F/A,Susp,Args),
	suspension_term_field(F/A,Susp,id,Id),
	suspension_term_field(F/A,Susp,state,state(alive)),
	(   check_uses_history(F/A)
	->  suspension_term_field(F/A,Susp,history,History),
	    new_history(History)
	;   true
	),
	(   get_nonground_indexed_constraint_index(F/A,_)
	->  GenId = gen_id(Id)
	;   check_uses_history(F/A)
	->  GenId = gen_id(Id)
	;   GenId = true
	),
	generate_attach_code(F/A,Susp,SuspRef,Attach),
	generate_late_indexing_insert_code(F/A,0,0,SuspRef,Insert),
	schedule_code(F/A,SuspRef,Schedule),
	Head =.. [F|Args],
	BodyList = [GenId, SuspRef = Susp, Attach, Insert, Schedule],
	delete(BodyList,true,NewBodyList),
	list_to_conj(NewBodyList,Body),
	Code = (Head :- Body).

generate_inline_constraint_code(Head,Code) :-
	functor(Head,F,A),
	get_highest_priority(F/A,P),
	new_suspension(F/A,Susp),
	freeze(Susp,
	(
	    (   suspension_args(F/A,Susp,Args),
		Head =.. [F|Args],
		suspension_term_field(F/A,Susp,id,Id),
		suspension_term_field(F/A,Susp,state,state(alive)),
		(   check_uses_history(F/A)
		->  suspension_term_field(F/A,Susp,history,History),
		    new_history(History)
		;   true
    		),
		(   get_nonground_indexed_constraint_index(F/A,_)
		->  GenId = gen_id(Id)
		;   check_uses_history(F/A)
		->  GenId = gen_id(Id)
		;   GenId = true
		),
		generate_attach_code(F/A,Susp,SuspRef,Attach),
		generate_late_indexing_insert_code(F/A,0,0,SuspRef,Insert),
		(   P = 0
		->  Activate = true
		;   term_to_atom(occurrence(F/A,P,1),OccFunctor),
		    Activate =.. [OccFunctor,SuspRef]
		),
		CodeList = [GenId, SuspRef = Susp, Attach, Insert, Activate],
		delete(CodeList,true,Filtered),
		list_to_conj(Filtered,Code)
	    ->  true
	    ;	writeln(generate_inline_constraint_code:failure), fail
	    )
	)).


generate_late_indexing_insert_code(FA,Priority,Occ,SuspensionRef,Code) :-
	NextOcc is Occ + 1,
	(   get_occurrence(FA,Priority,NextOcc,_,_)
	->  Code = true
	;   new_suspension(FA,Suspension),
	    freeze(Suspension,
	    (
		(   suspension_args(FA,Suspension,Args),
		    get_sorted_constraint_indexes(FA,AllIndexes),
		    (   member(Priority-Indexes,AllIndexes)
		    ->  true
		    ;   Indexes = []
		    ), 
		    (   get_indexed_arguments(FA,IndexedArgs)
		    ->  flatten(Indexes,FlatIndexes),
			sort(FlatIndexes,SetFlatIndexes),
			intersection(IndexedArgs,SetFlatIndexes,
			    MakeGroundArgs),
			get_target_module(Mod),
			new_suspension(FA,GroundSuspension),
			suspension_args(FA,GroundSuspension,GroundArgs),
			get_key_args(MakeGroundArgs,Args,NonGroundKeyArgs,
			    RemArgs),
			get_key_args(MakeGroundArgs,GroundArgs,GroundKeyArgs,
			    RemArgs),
		        make_ground_call_list(NonGroundKeyArgs,Mod,
			    GroundKeyArgs,GCL),
			list_to_conj(GCL,MakeGroundCall)
		    ;   MakeGroundCall = true,
			GroundSuspension = Suspension
		    ),
		    (   Indexes = [FirstIndex|_]
		    ->  suspension_term_field(FA,Suspension,index(FirstIndex),
			    FirstIndexField),
			maplist(generate_insert_into_index_code(FA,
			    GroundSuspension,SuspensionRef),Indexes,
			    Insertions),
			list_to_conj(Insertions,InsertConj),
			suspension_term_field(FA,Suspension,state,State),
			(   get_indexed_arguments(FA,[]) % no triggering
			    %MakeGroundCall = true % incorrect
			->  Code =
			    (
				SuspensionRef = Suspension,
				(   arg(1,State,alive)
				->  InsertConj
			        ;   true
			        )
			    )
		        ;   Code = 
			    (
				SuspensionRef = Suspension,
			        (   arg(1,State,alive),
			    	    var(FirstIndexField)
				->  MakeGroundCall,
				    InsertConj
				;   true
				)
			    )
			)
		    ;   Code = true
	    	    )
		->  true
		;   writeln(generate_late_indexing_insert_code:failure), fail
		)
	    ))
	).

make_ground_call_list([H1|T1],Mod,[H2|T2],[H3|T3]) :-
	H3 = make_ground(H1,Mod,H2),
	make_ground_call_list(T1,Mod,T2,T3).
make_ground_call_list([],_,[],[]).

get_key_args(Indexes,Args,KeyArgs,RemArgs) :-
	get_key_args(Indexes,1,Args,KeyArgs,RemArgs).
get_key_args([I|Is],CI,[A|As],KAs,[A|RAs]) :-
	I > CI, !, NI is CI + 1, get_key_args([I|Is],NI,As,KAs,RAs).
get_key_args([I|Is],CI,[A|As],[A|KAs],RAs) :-
	I = CI, !, NI is CI + 1, get_key_args(Is,NI,As,KAs,RAs).
get_key_args([],_,As,[],As).