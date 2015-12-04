% Body Analysis Module
%
% Part of the CHR-rp compiler.
%
% Author: Leslie De Koninck, K.U.Leuven, Belgium

:- module(body_analysis,
	[
	    body_analysis/0
	]).

:- use_module(database,
	[
	    get_constraint_priorities/2,
	    get_highest_priority/2,
	    get_nb_nonground_indexed_constraints/1,
	    get_rule_body/2,
	    get_rule_priority/2,
	    highest_priority_body_constraint/2,
	    no_check_activation_call/1
	]).

body_analysis :- body_analysis(1).


body_analysis(RuleId) :-
	(   get_rule_body(RuleId,Body) % rule exists
	->  get_rule_priority(RuleId,RP),
	    (   integer(RP)
	    ->  analyse_body(Body,RuleId,RP,no)
	    ;   %true
		analyse_body(Body,RuleId,2,no)
	    ),
	    NextId is RuleId + 1,
	    body_analysis(NextId)
	;   true
	).


analyse_body([H|T],RuleId,RP,Highest) :-
	(   functor(H,F,A),
	    get_highest_priority(F/A,HMax),
	    HMax =\= 0
	->  (   Highest = no,
		HMax =< RP
	    ->  NewHighest = HMax - H
	    ;   Highest = Max - _,
		HMax < Max
	    ->  NewHighest = HMax - H
	    ;   NewHighest = Highest
	    ),
	    analyse_body(T,RuleId,RP,NewHighest)
	;   safe_builtin_constraint(H)
	->  analyse_body(T,RuleId,RP,Highest)
	;   get_nb_nonground_indexed_constraints(0)
	->  analyse_body(T,RuleId,RP,Highest)
	;   true % unsafe built-in constraint
	).
analyse_body([],RuleId,_,no) :- !, no_check_activation_call(RuleId).
analyse_body([],RuleId,RP,Max - Head) :-
	(   RP =:= Max
	->  no_check_activation_call(RuleId)
	;   true
	),
	highest_priority_body_constraint(RuleId,Head).

safe_builtin_constraint(true).
safe_builtin_constraint(_ is _). % FALSE!!!
safe_builtin_constraint(_ > _).
safe_builtin_constraint(_ < _).
safe_builtin_constraint(_ >= _).
safe_builtin_constraint(_ =< _).
safe_builtin_constraint(_ =:= _).
safe_builtin_constraint(_ =\= _).
safe_builtin_constraint(_ == _).
safe_builtin_constraint(_ \== _).