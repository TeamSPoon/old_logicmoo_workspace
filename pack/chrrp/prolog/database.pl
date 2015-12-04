% Database Module
%
% Part of the CHR-rp compiler.
%
% Author: Leslie De Koninck, K.U.Leuven, Belgium

:- module(database,
	[
	    all_ground_modes/2,
	    constraint/3,
	    constraint_index/3,
	    constraint_index_occ/4,
	    get_all_constraints/1,
	    get_all_active_occurrences/1,
	    get_constraint_index/3,
	    get_constraint_indexes/2,
	    get_constraint_priorities/2,
	    get_first_occurrence/3,
	    get_head/3,
	    get_head_indices/2,
	    get_highest_priority/2,
	    get_highest_priority_body_constraint/2,
	    get_indexed_arguments/2,
	    get_join_order/3,
	    get_nb_constraint_indexes/2,
	    get_nb_nonground_indexed_constraints/1,
	    get_nb_rules/1,
	    get_next_occurrence/4,
	    get_next_priority/3,
	    get_nonground_indexed_constraint/2,
	    get_nonground_indexed_constraint_index/2,
	    get_occurrence/5,
	    get_rule_body/2,
	    get_rule_guard/2,
	    get_rule_heads/2,
	    get_rule_priority/2,
	    get_schedule_depth/3,
	    get_sorted_constraint_indexes/2,
	    get_target_module/1,
	    highest_priority_body_constraint/2,
	    indexed_argument/2,
	    is_propagation_rule/1,
	    join_order/3,
	    needs_check_activation_call/0,
	    needs_check_activation_call/1,
	    no_check_activation_call/1,
	    no_inline_activation/0,
	    no_late_indexing/0,
	    no_reduced_check_activation_calls/0,
	    passive_occurrence/3,
	    rule/7,
	    schedule_depth/3   
	]).
:- use_module(library(chr)).
:- use_module(util,
	[
	    numlist2/3,
	    make_term/3,
	    zip/3
	]).

get_target_module(user).

:- chr_constraint 
	% all_ground_modes(F/A,Index)
	all_ground_modes(+,?),
	% constraint(F/A,Modes,Types)
	constraint(+,+,+),
	% constraint_index(F/A,Index,Priority)
	constraint_index(+,+,?),
	% constraint_index(F/A,Index,Priority,Id)
	constraint_index(+,?,?,?), % wrong modes because of bug
	% constraint_index_occ(F/A,Index,Priority,Occ)
	constraint_index_occ(+,+,?,+),
	% get_all_constraints(FAs)
	get_all_constraints(?),
	% get_all_active_occurrences(FAPCOccs)
	get_all_active_occurrences(?),
	% get_constraint_index(F/A,Index,Id)
	get_constraint_index(+,?,?),
	% get_constraint_indexes(F/A,Id,Indexes,Priorities)
	get_constraint_indexes(+,+,?,?),
	% get_constraint_priorities(F/A,Priorities)
	get_constraint_priorities(+,?),
	% get_head(RuleId,HeadId,Head)
	get_head(+,+,?),
	% get_head_indices(RuleId,Indices)
	get_head_indices(+,?),
	% get_highest_priority_body_constraint(RuleId,Constraint)
	get_highest_priority_body_constraint(+,?),
	% get_indexed_arguments(F/A,Index)
	get_indexed_arguments(+,?),
	% get_join_order(RuleId,HeadId,Indices)
	get_join_order(+,+,?),
	% get_nb_constraint_indexes(F/A,Nb)
	get_nb_constraint_indexes(+,?),
	% get_nb_nonground_indexed_constraints(Nb)
	get_nb_nonground_indexed_constraints(?),
	% get_nb_rules(Nb)
	get_nb_rules(?),
	% get_next_occurrence(FA,PC,Occ,NextOcc)
	get_next_occurrence(+,+,+,?),
	% get_nonground_indexed_constraint(Id,F/A)
	get_nonground_indexed_constraint(+,?),
	% get_nonground_indexed_constraint_index(F/A,Id)
	get_nonground_indexed_constraint_index(+,?),
	% get_occurrence(F/A,PC,OccId,RuleId,HeadId)
	get_occurrence(?,?,?,?,?),
	% get_rule_body(RuleId,Body)
	get_rule_body(+,?),
	% get_rule_guard(RuleId,Guard)
	get_rule_guard(+,?),
	% get_rule_heads(RuleId,Heads)
	get_rule_heads(+,?),
	% get_rule_priority(RuleId,Priority)
	get_rule_priority(+,?),
	% get_schedule_depth(RuleId,HeadId,Depth)
	get_schedule_depth(+,+,?),
	% highest_priority_body_constraint(RuleId,Constraint)
	highest_priority_body_constraint(+,?),
	% indexed_argument(F/A,Index)
	indexed_argument(+,+),
	% indexed_arguments(F/A,Id,Index)
	indexed_arguments(+,+,+),
	% is_propagation_rule(RuleId)
	is_propagation_rule(+),
	% join_order(RuleId,HeadId,Indices)
	join_order(+,+,+),
	% max_occurrence(F/A,PriorityClass,MaxOccId)
	max_occurrence(+,+,+), 
	% nb_constraint_indexes(F/A,Nb)
	nb_constraint_indexes(+,+),
	% nb_nonground_indexed_constraints(Nb)
	nb_nonground_indexed_constraints(+),
	% nb_rules(Nb)
	nb_rules(+),
	% needs_check_activation_call
	needs_check_activation_call,
	% needs_check_activation_call(RuleId)
	needs_check_activation_call(+),
	% no_check_activation_call(RuleId)
	no_check_activation_call(+),
	% no_inline_activation
	no_inline_activation,
	% no_late_indexing
	no_late_indexing,
	% no_reduced_check_activation_calls
	no_reduced_check_activation_calls,
	% not_passive(F/A,PC,OccId)
	not_passive(+,+,+),
	% occurrence(F/A,RuleId,HeadId,PriorityClass)
	occurrence(+,+,+,+), 
	% occurrence(F/A,RuleId,HeadId,PriorityClass,OccId)
	occurrence(?,?,?,?,?), % wrong modes because of bug
	% passive_occurrence(F/A,PriorityClass,OccId)
	passive_occurrence(+,+,+),
	% rule(RuleId,Name,Priority,KeptHeads,RemovedHeads,Guard,Body)
	rule(+,+,?,?,?,?,?),
	% schedule_depth(RuleId,HeadId,Depth)
	schedule_depth(+,+,+).

passive_occurrence(FA,PC,Occ) \ not_passive(FA,PC,Occ) <=> fail.
not_passive(_,_,_) <=> true.

get_next_priority(FA,CurrentPrio,NextPrio) :-
	get_constraint_priorities(FA,Priorities),
	sort(Priorities,SortedPriorities),
	append(_,[CurrentPrio,NextPrio|_],SortedPriorities),
	integer(NextPrio).

get_first_occurrence(FA,PC,QFirstOcc) :-
	get_next_occurrence(FA,PC,0,QFirstOcc).	

get_next_occurrence_1 @ occurrence(FA,_,_,PC,NextOcc),
    passive_occurrence(FA,PC,NextOcc) \
    get_next_occurrence(FA,PC,Occ,QNextOcc) <=> NextOcc =:= Occ + 1 |
	get_next_occurrence(FA,PC,NextOcc,QNextOcc).
get_next_occurrence_2 @ occurrence(FA,_,_,PC,NextOcc) \
    get_next_occurrence(FA,PC,Occ,QNextOcc) <=> NextOcc =:= Occ + 1 |
	QNextOcc = NextOcc.
get_next_occurrence_3 @ get_next_occurrence(_,_,_,_) <=> fail.

r1 @ rule(Id,_,Priority,KH,RH,_,_), constraint(FA,_,_) ==>
	(   integer(Priority)
	->  PC = Priority
	;   PC = 2
	),
	removed_occurrences(RH,Id,1,PC,FA),
	kept_occurrences(KH,Id,1,PC,FA).

nb_rules_1 @ nb_rules(Nb1) \ nb_rules(Nb2) <=> Nb1 >= Nb2 | true.
nb_rules_2 @ rule(Id,_,_,_,_,_,_) ==> nb_rules(Id).

get_nb_rules_1 @ nb_rules(Nb) \ get_nb_rules(QNb) <=> QNb = Nb.
get_nb_rules_2 @ get_nb_rules(_) <=> fail.

% in fact should be equal to the lowest known priority 
default_index @ constraint(FA,_,_) ==> constraint_index(FA,[],_).

is_propagation_rule_1 @ rule(Id,_,_,_,[],_,_) \ is_propagation_rule(Id) <=>
	true.
is_propagation_rule_2 @ is_propagation_rule(_) <=> fail.

get_join_order_1 @ join_order(RuleId,HeadId,Indices) \
    get_join_order(RuleId,HeadId,QIndices) <=> QIndices = Indices.
get_join_order_2 @ get_join_order(_,_,_) <=> fail.

get_rule_heads_1 @ rule(Id,_,_,KH,RH,_,_) \ get_rule_heads(Id,Heads) <=>
	append(KH,RH,Heads).
get_rule_heads_2 @ get_rule_heads(_,_) <=> fail.

get_rule_guard_1 @ rule(Id,_,_,_,_,G,_) \ get_rule_guard(Id,Guard) <=> 
	Guard = G.
get_rule_guard_2 @ get_rule_guard(_,_) <=> fail.

get_rule_priority_1 @ rule(Id,_,Prio,_,_,_,_) \ get_rule_priority(Id,QPrio) <=>
	QPrio = Prio.
get_rule_priority_2 @ get_rule_priority(_,_) <=> fail.

get_head_1 @ rule(Id,_,_,KH,_,_,_) \ get_head(Id,k(I),Head) <=>
	nth1(I,KH,Head).
get_head_2 @ rule(Id,_,_,_,RH,_,_) \ get_head(Id,r(I),Head) <=>
	nth1(I,RH,Head).
get_head_3 @ get_head(_,_,_) <=> fail.

get_rule_body_1 @ rule(Id,_,_,_,_,_,B) \ get_rule_body(Id,QB) <=> QB = B.
get_rule_body_2 @ get_rule_body(_,_) <=> fail.

get_head_indices_1 @ rule(Id,_,_,KH,RH,_,_) \ get_head_indices(Id,Indices) <=>
	length(KH,KL), length(RH,RL),
	numlist2(1,KL,Nums1), numlist2(1,RL,Nums2),
	maplist(make_term(k),Nums1,KI),
	maplist(make_term(r),Nums2,RI),
	append(RI,KI,Indices).
get_head_indices_2 @ get_head_indices(_,_) <=> fail.

occurrence_1 @ occurrence(FA,RuleId,HeadId,PC), max_occurrence(FA,PC,Max) <=>
	NewMax is Max + 1,
	occurrence(FA,RuleId,HeadId,PC,NewMax),
	max_occurrence(FA,PC,NewMax).
occurrence_2 @ occurrence(FA,RuleId,HeadId,PC) <=>
	occurrence(FA,RuleId,HeadId,PC,1),
	max_occurrence(FA,PC,1).

get_occurrence_1 @ occurrence(FA,RuleId,HeadId,PC,OccId) \
    get_occurrence(FA,PC,OccId,QRuleId,QHeadId) <=>
	QRuleId = RuleId, QHeadId = HeadId.
get_occurrence_2 @ occurrence(FA,RuleId,HeadId,PC,OccId) \
    get_occurrence(QFA,QPC,QOccId,RuleId,HeadId) <=>
	QFA = FA, QPC = PC, QOccId = OccId.
get_occurrence_3 @ get_occurrence(_,_,_,_,_) <=> fail.

/*get_all_occurrences_1 @ get_all_occurrences(FAPCOccs), 
    occurrence(FA,_,_,PC,Occ) ==> member(FA-PC-Occ,FAPCOccs), !.
get_all_occurrences_2 @ get_all_occurrences(FAPCOccs) <=>
	length(FAPCOccs,_), !.
*/
get_all_active_occurrences_1 @ get_all_active_occurrences(FAPCOccs), 
    occurrence(FA,_,_,PC,Occ) ==> not_passive(FA,PC,Occ) | 
	member(FA-PC-Occ,FAPCOccs), !.
get_all_active_occurrences_2 @ get_all_active_occurrences(FAPCOccs) <=>
	length(FAPCOccs,_), !.

get_all_constraints_1 @ get_all_constraints(FAs), constraint(FA,_,_) ==>
	member(FA,FAs), !.
get_all_constraints_2 @ get_all_constraints(FAs) <=> length(FAs,_), !.

kept_occurrences([KH|KHs],RuleId,Index,PC,F/A) :-
	(   functor(KH,F,A)
	->  occurrence(F/A,RuleId,k(Index),PC)
	;   true
	),
	NextIndex is Index + 1,
	kept_occurrences(KHs,RuleId,NextIndex,PC,F/A).
kept_occurrences([],_,_,_,_).

removed_occurrences([RH|RHs],RuleId,Index,PC,F/A) :-
	(   functor(RH,F,A)
	->  occurrence(F/A,RuleId,r(Index),PC)
	;   true
	),
	NextIndex is Index + 1,
	removed_occurrences(RHs,RuleId,NextIndex,PC,F/A).
removed_occurrences([],_,_,_,_).

get_constraint_priorities_1 @ get_constraint_priorities(FA,Priorities), 
    occurrence(FA,_,_,PC,Occ) ==> not_passive(FA,PC,Occ) | 
	member(PC,Priorities), !.
get_constraint_priorities_2 @ get_constraint_priorities(_,Priorities) <=>
	nonvar(Priorities) | length(Priorities,_), !.
get_constraint_priorities_3 @ get_constraint_priorities(_,Priorities) <=> 
	Priorities = [].

all_ground_modes_1 @ constraint(FA,Modes,_) \
    all_ground_modes(FA,ArgIndices) <=> 
	forall(member(I,ArgIndices),nth1(I,Modes,+)).
all_ground_modes_2 @ all_ground_modes(_,_) <=> fail.

no_late_indexing_1 @ no_late_indexing \ constraint_index(FA,I,P) <=>
	P \== 0 | constraint_index(FA,I,0).

passive_occurrence(FA,P,Occ) \ constraint_index_occ(FA,I,P,Occ) <=>
	get_constraint_priorities(FA,Ps), 
	(   member(P1,Ps),
	    P1 < P,
	    \+ (member(P2,Ps), P2 < P, P1 < P2)
	->  constraint_index(FA,I,P1)
	;   constraint_index(FA,I,0)
	).
constraint_index_occ(FA,I,P,_) <=> 
	%get_previous_priority(FA,P,Prev), constraint_index(FA,I,Prev).
	constraint_index(FA,I,P).
/*
constraint_index(FA,I,P) <=> nonvar(P), P \== 0, 
	get_constraint_priorities(FA,Ps), \+ member(P,Ps) |
	(   member(P1,Ps),
	    P1 < P,
	    \+ (member(P2,Ps), P2 < P, P1 < P2)
	->  constraint_index(FA,I,P1)
	;   constraint_index(FA,I,0)
	).
*/

constraint_index_1 @ constraint_index(FA,[],P1,_) \ 
    constraint_index(FA,[],P2) <=> var(P1) | P1 = P2.
constraint_index_1 @ constraint_index(FA,IA,P1,_) \ 
    constraint_index(FA,IA,P2) <=> P1 =< P2 | true.
constraint_index_2 @ constraint_index(FA,IA,_,Id), 
    constraint_index(FA,IA,P2) <=> constraint_index(FA,IA,P2,Id).
constraint_index_3 @ nb_constraint_indexes(FA,Id), 
    constraint_index(FA,IA,P) <=>
	NextId is Id + 1,
	constraint_index(FA,IA,P,NextId),
	nb_constraint_indexes(FA,NextId).
constraint_index_4 @ constraint_index(FA,IA,P) <=>
	constraint_index(FA,IA,P,1),
	nb_constraint_indexes(FA,1).
	
get_constraint_index_1 @ constraint_index(FA,Index,_,Id) \ 
    get_constraint_index(FA,QIndex,Id) <=> QIndex = Index.
get_constraint_index_2 @ constraint_index(FA,Index,_,Id) \
    get_constraint_index(FA,Index,QId) <=> QId = Id.
get_constraint_index_3 @ get_constraint_index(_,_,_) <=> fail.

get_nb_constraint_indexes_1 @ nb_constraint_indexes(FA,Nb) \ 
    get_nb_constraint_indexes(FA,QNb) <=> QNb = Nb.
get_nb_constraint_indexes_2 @ get_nb_constraint_indexes(_,QNb) <=> QNb = 0.

get_constraint_indexes(FA,Indexes) :- get_constraint_indexes(FA,1,Indexes,_).

get_constraint_indexes_1 @ constraint_index(FA,I,P,Id) \ 
    get_constraint_indexes(FA,Id,Indexes,Priorities) <=>
	Indexes = [I|Is],
	Priorities = [P|Ps],
	NextId is Id + 1,
	get_constraint_indexes(FA,NextId,Is,Ps).
get_constraint_indexes_2 @ get_constraint_indexes(_,_,Indexes,Priorities) <=>
	Indexes = [], Priorities = [].
	
get_sorted_constraint_indexes(FA,SortedIndexes) :-
	get_constraint_indexes(FA,1,Indexes,Priorities),
	(   member(Var,Priorities),
	    var(Var)
	->  get_lowest_priority(FA,Var)
	;   true
	),
	zip(Priorities,Indexes,Zipped),
	sort(Priorities,SortedPriorities),
	findall(X-Ys,(member(X,SortedPriorities),
	    findall(Y,member(X-Y,Zipped),Ys)),SortedIndexes).
	

indexed_argument_1 @ indexed_arguments(FA,Id,Is), indexed_argument(FA,I) <=> 
	sort([I|Is],S), indexed_arguments(FA,Id,S).
indexed_argument_2 @ indexed_argument(FA,I), 
    nb_nonground_indexed_constraints(MaxId) <=> 
	NewMaxId is MaxId + 1, 
	indexed_arguments(FA,NewMaxId,[I]),
	nb_nonground_indexed_constraints(NewMaxId).
indexed_argument_3 @ indexed_argument(FA,I) <=>
	indexed_arguments(FA,1,[I]),
	nb_nonground_indexed_constraints(1).

get_indexed_arguments_1 @ indexed_arguments(FA,_,IA) \ 
    get_indexed_arguments(FA,QIA) <=> QIA = IA.
get_indexed_arguments_2 @ get_indexed_arguments(_,_) <=> fail.

get_nb_nonground_indexed_constraints_1 @ nb_nonground_indexed_constraints(Nb) \ 
    get_nb_nonground_indexed_constraints(QNb) <=> QNb = Nb.
get_nb_nonground_indexed_constraints_2 @ 
    get_nb_nonground_indexed_constraints(QNb) <=> QNb = 0.

get_nonground_indexed_constraint_index_1 @ indexed_arguments(FA,Id,_) \ 
    get_nonground_indexed_constraint_index(FA,QId) <=> QId = Id.
get_nonground_indexed_constraint_index_2 @ 
    get_nonground_indexed_constraint_index(_,_) <=> fail.

get_nonground_indexed_constraint_1 @ indexed_arguments(FA,Id,_) \
    get_nonground_indexed_constraint(Id,QFA) <=> QFA = FA.
get_nonground_indexed_constraint_2 @ get_nonground_indexed_constraint(_,_) <=>
	fail.

no_reduced_check_activation_calls_1 @ no_reduced_check_activation_calls \
    no_check_activation_call(_) <=> true.
    
needs_check_activation_call_1 @ no_reduced_check_activation_calls \
    needs_check_activation_call <=> true.
needs_check_activation_call_2 @ needs_check_activation_call <=> fail.

needs_check_activation_call_3 @ no_check_activation_call(RuleId) \
    needs_check_activation_call(RuleId) <=> fail.
needs_check_activation_call_4 @ needs_check_activation_call(_) <=> true.

no_inline_activation_1 @ no_inline_activation \ 
    highest_priority_body_constraint(_,_) <=> true.

get_highest_priority_body_constraint_1 @ 
    highest_priority_body_constraint(RuleId,Constraint) \
    get_highest_priority_body_constraint(RuleId,QConstraint) <=>
	QConstraint = Constraint.
get_highest_priority_body_constraint_2 @
    get_highest_priority_body_constraint(_,_) <=> fail.

get_highest_priority(FA,Priority) :-
	(   get_constraint_priorities(FA,Priorities),
	    Priorities = [_|_]
	->  sort(Priorities,[Priority|_])
	;   Priority = 0
	).

get_lowest_priority(FA,Priority) :-
	(   get_constraint_priorities(FA,Priorities),
	    Priorities = [_|_]
	->  sort(Priorities,Sorted),
	    reverse(Sorted,[Priority|_])
	;   Priority = 0
	).
get_previous_priority(FA,Priority,PrevPriority) :-
	(   get_constraint_priorities(FA,Priorities),
	    sort(Priorities,Sorted),
	    nextto(PrevPriority,Priority,Sorted)
	->  true
	;   PrevPriority = 0
	).

% allows manually setting the schedule depth
schedule_depth(RuleId,HeadId,_) \ schedule_depth(RuleId,HeadId,_) <=> true.

get_schedule_depth_1 @ schedule_depth(RuleId,HeadId,Depth) \
    get_schedule_depth(RuleId,HeadId,QDepth) <=> QDepth = Depth.
get_schedule_depth_2 @ get_schedule_depth(_,_,_) <=> fail.