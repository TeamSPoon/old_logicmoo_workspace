% Join Ordering Analysis Module
%
% Part of the CHR-rp compiler.
%
% Author: Leslie De Koninck, K.U.Leuven, Belgium

:- module(join_ordering,
	[
	    join_ordering/0
	]).

:- use_module(database,
	[
	    get_head/3,
	    get_head_indices/2,
	    get_rule_priority/2,
	    join_order/3,
	    schedule_depth/3
	]).
:- use_module(util,
	[
	    memberc/2
	]).

join_ordering :- jo(1).

jo(RuleNb) :-
	get_head_indices(RuleNb,Indices), !, % rule exists
	jo(Indices,Indices,RuleNb),
	NextRuleNb is RuleNb + 1,
	jo(NextRuleNb).
jo(_).


jo([H|T],Indices,RuleNb) :-
	select(H,Indices,Remaining),
	join_order(RuleNb,H,Remaining),
	get_rule_priority(RuleNb,Priority),
	(   integer(Priority)
	->  true
	;   term_variables(Priority,PriorityVars),
	    generate_schedule_depth([H|Remaining],H,RuleNb,[],0,PriorityVars)
	),
	jo(T,Indices,RuleNb).
jo([],_,_).

generate_schedule_depth([H|Hs],ActHead,RuleNb,KnownVars,Depth,PriorityVars) :-
	get_head(RuleNb,H,Head),
	term_variables(Head+KnownVars,NewKnownVars),
	(   forall(member(Var,PriorityVars),memberc(Var,NewKnownVars))
	->  schedule_depth(RuleNb,ActHead,Depth)
	;   NextDepth is Depth + 1,
	    generate_schedule_depth(Hs,ActHead,RuleNb,NewKnownVars,NextDepth,
		PriorityVars)
	).
generate_schedule_depth([],_,RuleNb,_,_,_) :-
	write('CHR-rp compiler error: undefined rule priority in rule '),
	writeln(RuleNb), fail.

