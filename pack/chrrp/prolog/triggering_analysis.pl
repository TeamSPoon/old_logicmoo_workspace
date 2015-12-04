% Triggering Analysis Module
%
% Part of the CHR-rp compiler.
%
% Author: Leslie De Koninck, K.U.Leuven, Belgium

:- module(triggering_analysis,
	[
	    triggering_analysis/0
	]).

:- use_module(database,
	[
	    all_ground_modes/2,
	    get_rule_guard/2,
	    get_rule_heads/2,
	    indexed_argument/2
	]).
	
triggering_analysis :-
	ta(1).

ta(RuleNb) :- 
	get_rule_heads(RuleNb,Heads),!, % rule exists
	get_rule_guard(RuleNb,Guard),
	term_variables(Guard,Vars),
	ta(Heads,Vars),
	NextRuleNb is RuleNb + 1,
	ta(NextRuleNb).
ta(_).

ta([H|T],Vars) :-
	functor(H,F,A),
	H =.. [_|Args],
	term_variables(T+Vars,AllVars),
	ta_args(Args,F/A,1,AllVars),
	term_variables(H+Vars,NextVars),
	ta(T,NextVars).
ta([],_).

ta_args([H|T],FA,Index,Vars) :-
	(   all_ground_modes(FA,[Index])
	->  true
	;   (   nonvar(H)
	    ->  indexed_argument(FA,Index)
	    ;   term_variables(T,TVars),
		(   ( memberc(H,TVars) ; memberc(H,Vars) )
		->  indexed_argument(FA,Index)
		;   true
		)
	    )
	),
	term_variables(H+Vars,NewVars),
	NextIndex is Index + 1,
	ta_args(T,FA,NextIndex,NewVars).
ta_args([],_,_,_).
	    
memberc(X,Xs) :- member(M,Xs), X == M, !.