% Passive Analysis Module
%
% Part of the CHR-rp compiler.
%
% Author: Leslie De Koninck, K.U.Leuven, Belgium

:- module(passive_analysis,
	[
	    passive_analysis/0
	]).
:- use_module(database,
	[
	    get_all_constraints/1,
	    get_head/3,
	    get_head_indices/2,
	    get_nb_rules/1,
	    get_rule_guard/2
	]).
:- use_module(util,
	[
	    memberc/2,
	    numlist2/3
	]).
	
passive_analysis :-
	get_all_constraints(_FAs),
	get_nb_rules(NbRules),
	numlist2(1,NbRules,RuleIds),
	forall((member(RuleId,RuleIds),unconditional_remove_rule(RuleId)),
	    writeln(uncond:RuleId)).


unconditional_remove_rule(Id) :-
	get_head_indices(Id,[r(1)]), % single headed simplification rule
	get_head(Id,r(1),Head),
	Head =.. [_|Args],
	is_all_different_varlist(Args,[]),
	get_rule_guard(Id,[]). % no guard

is_all_different_varlist([H|T],Already) :-
	var(H),
	\+ memberc(H,Already),
	is_all_different_varlist(T,[H|Already]).
is_all_different_varlist([],_).
	
