% The CHR-rp Compiler
%
% Author: Leslie De Koninck, K.U.Leuven, Belgium

% Started: 12/06/2007
% First working examples (dijkstra static priority rules, leq): 19/06/2007

:- use_module(io,[read_file/2,write_file/2]).
:- use_module(parse,[parse/2]).
:- use_module(triggering_analysis,[triggering_analysis/0]).
:- use_module(join_ordering,[join_ordering/0]).
:- use_module(body_analysis,[body_analysis/0]).
:- use_module(compile_occ,[compile_occurrences/1]).
:- use_module(constraint_code,[generate_constraint_code/1]).
:- use_module(passive_analysis,[passive_analysis/0]).
:- use_module(store_management,
	[
	    generate_show_store_clause/1,
	    generate_store_initializations/1,
	    generate_attr_unify_hook/1,
	    generate_schedule_clauses/1,
	    generate_update_index_clauses/1,
	    generate_attach_loop_clauses/1,
	    generate_detach_loop_clauses/1
	]).
:- use_module(database,
	[
	    no_inline_activation/0,
	    no_late_indexing/0,
	    no_reduced_check_activation_calls/0,
	    passive_occurrence/3,
	    schedule_depth/3
	]).

% Add compiler options before the main/0 or main/1 call:
% - no_inline_activation
% - no_late_indexing
% - no_reduced_check_activation_calls
% - passive_occurrence(F/A,PC,Occ)
% - schedule_depth(RuleId,Head,Depth); Head = r(_) or k(_)
main :- main(leq).

main(Program) :-
	atom_concat(Program,'.chr',InFile),
	atom_concat(Program,'.pl',OutFile),
	read_file(InFile,Lines), 
	parse(Lines,Rem),
	triggering_analysis,
	join_ordering,
	body_analysis,
	passive_analysis,
	compile_occurrences(OccurrenceCode),
	generate_store_initializations(StoreInits),
	generate_constraint_code(ConstraintCode),
	import_runtime_calls(ImportRuntime),
	generate_show_store_clause(ShowStore),
	(   generate_attr_unify_hook(UHook)
	->  generate_attach_loop_clauses(AttachClauses),
	    generate_detach_loop_clauses(DetachClauses),
	    generate_update_index_clauses(UpdateIndexClauses),
	    generate_schedule_clauses(ScheduleClauses)
	;   UHook = [],
	    AttachClauses = [],
	    DetachClauses = [],
	    UpdateIndexClauses = [],
	    ScheduleClauses = []
	),	
	PQInit = [],
	flatten(
		[
		    ImportRuntime,
		    Rem,
		    StoreInits,
		    PQInit,
		    ConstraintCode,
		    UHook,
		    ScheduleClauses,
		    UpdateIndexClauses,
		    OccurrenceCode,
		    AttachClauses,
		    DetachClauses,
		    ShowStore
		],Code),
	copy_term_nat(Code,Copy), % for writing attvars	
	write_file(OutFile,Copy).

import_runtime_calls(Calls) :-
	C1 = (:- use_module(activation,[check_activation/0,
		check_activation/1])),
	C2 = (:- use_module(history,[not_in_history/3,add_to_history/3])),
	C3 = (:- use_module(lock,[lock/1,unlock/1])),
	C4 = (:- use_module(priority_queue,[insert/2])),
	C5 = (:- use_module(store,[make_ground/3,new_store/1,insert_in_store/4,
		store_lookup/3,delete_key/2,delete_first/2,select_by_id/4])),
	C6 = (:- use_module(id,[gen_id/1])),
	C7 = (:- use_module(library(lists))),
	C8 = (:- style_check(-singleton)),
	Calls = [C1,C2,C3,C4,C5,C6,C7,C8].

