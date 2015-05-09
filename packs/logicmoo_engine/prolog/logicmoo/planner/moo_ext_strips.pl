:- swi_module(domain_chr,[dom_chr/2]).
:- use_module(library(chr)).


% :- disable_mpreds_in_current_file.


:- chr_constraint dom_chr(?int,+list(int)).
:- chr_type list(T) ---> [] ; [T|list(T)].

dom_chr(_X,[]) <=> fail.
dom_chr(X,[Y]) <=> X = Y.
dom_chr(X,L) <=> nonvar(X) | memberchk(X,L).
dom_chr(X,L1), dom_chr(X,L2) <=> intersection(L1,L2,L3), dom_chr(X,L3).



:-dynamic(init_state/1).


/*-------------------------------------------------- */
/*  main_rstrips(+Goal, +InitState, -Plan)           */
/*-------------------------------------------------- */
main_rstrips(Goal, InitState, Plan):-
	% Assert initilize state to database.
	assert_init(InitState), 
	append(Goal, [and(Goal)], Stack),
	rstrips(Stack,[],Plan).

/*-------------------------------------------------------- */
/*  rstrips(+Stack, +Rstack, -Plan   )                     */
/*-------------------------------------------------------- */
% No more goals in the stack => plan is done 
rstrips([and(_)], Rstack,Plan) :- extract_plan(Rstack, [], Plan). 
% Top of the goals stak is an trio of [compound goal, action, subgoal].
rstrips([and(List),op(Act),Fact|Stack], Rstack, Plan):-  !,
	perform(List, Act, Fact, Rstack, NRStack),
	rstrips(Stack, NRStack, Plan).
% Top of the goals stack is a satisfied condition.
rstrips([Cond|Stack], RStack, Plan):- 
	hold(Cond, RStack), !,
	rstrips(Stack, [pro(Cond)|RStack], Plan).
% Top of the goals stack is an unsatisfied condition. 
rstrips([Cond|Stack], Rstack, Plan):- 
	adds(Action, Cond),
	% No repeat future action
	not(member(op(Action), Stack)),
	achieve(Cond, Action, Stack, Rstack, NStack, NRstack), 
	rstrips(NStack, NRstack, Plan).

/*------------------------------------------------------------- */
/* Achieve the subgoal by action or by regression.               */
/*    achieve(+Cond, +Action, +Stack, +Rstack, -NStack, -Rstack)*/
/*------------------------------------------------------------- */
achieve(Cond, Action, Stack, Rstack, NStack, Rstack) :-
	preserving(Action, Rstack, Stack),!,
	prec(Action, Prec),
	append(Prec, [and(Prec),op(Action),Cond|Stack], NStack).
achieve(_, Action, Stack, Rstack, NStack, NRstack) :-
	regress(Action, Stack, Rstack, NStack, NRstack).
	

/*-------------------------------------------------------- */
/* Regress the protection violating goal through the action*/
/* that achieved the protected subgoal                     */
/*   regress(+Action, +Stack, +Rstack, -NStack, -NRstack)  */
/*-------------------------------------------------------- */
regress(Action, Stack, Rstack, NStack, NRstack) :-
	setof(Cond, dels(Action,Cond), DelList),
	% Pop the Rstack up to the violated protectedf subgoal
	pop_rstack(DelList, Rstack, [Violated |Rstack1]),
	% Pop the stack up to the protection violating subgoal
	pop_stack(Stack, Violated, [], Stack1),
	go_through(Stack1, Violated, Rstack1, NStack, NRstack),!.

/*------------------------------------------------------------- */
/* Regress the violating subgoal through the violated goal      */
/*    go_through(+Stack, +Violated, +Rstack, -NStack, -NRstack) */ 
/*------------------------------------------------------------- */
go_through([X | Stack], V, [op(Act),and(List)| Rst], NSt, NRst) :-
	NSt = [X, and(List), op(Act), V | Stack],
	protect(List, Rst, NRst), !.

/*-------------------------------------------------------- */
/* Protect the conditions of the list in Rstack            */
/*    protect(+List, +Rstack, -NRstack)                    */
/*-------------------------------------------------------- */
protect([], Rstack, Rstack).
protect(_,[], _) :- 
	write(' **** Must be an error in stack'),nl,!,fail.
protect(CondList,[Cond | More], [pro(Cond) | NRstack] ) :-
	member(Cond, CondList), !,
	subtract(CondList, [Cond], CondList1),
	protect(CondList1, More, NRstack).
protect(CondList, [X | More], [X | NRstack]) :-
	protect(CondList, More, NRstack).

/*------------------------------------------------------------- */
/* Pop the Rstack until you find the subgoal that is been       */
/* violated by the one of the action deleted condition. If not  */
/* found then there is error in the stack.                      */
/*   pop_rstack(+DelList, +RStack, -Violated, -Rstack1)         */
/*------------------------------------------------------------- */
pop_rstack(_, [], _) :- 	
	write(' **** Must be an error in stack'),nl,!,fail.
pop_rstack(DelList, [pro(Fact) | More], [Fact | More]) :-
	member(Fact, DelList), !.
pop_rstack(DelList, [_| More], Rstack) :-
	pop_rstack(DelList, More, Rstack).	

/*------------------------------------------------------------- */
/* Pop the Stack until you reach the protection violating       */
/* subgoal.                                                     */
/*   pop_stack(+Stack, +Violated, +TempStack, -NStack1)         */
/*------------------------------------------------------------- */
pop_stack([], _, _, _) :-
	write(' **** Must be an error in stack'),nl,!,fail.
pop_stack([and(List) | More], Violated, TempStack, NStack) :-
	member(Violated, List), !,
	find_violating_subgoal([and(List) |More], TempStack, NStack), !.
pop_stack([X| More], Violated, TempStack, NStack) :-
	pop_stack(More, Violated, [X|TempStack], NStack) .

/*------------------------------------------------------------- */
/* Return the stack below and include the protection violating */
/* subgoal                                                      */
/*   find_violating_subgoal(+Stack, +TempStack, -NStack)        */
/*------------------------------------------------------------- */
find_violating_subgoal(NStack, [op(_) | _], NStack).
find_violating_subgoal(Stack, [X| More], NStack) :-
	find_violating_subgoal([X | Stack], More, NStack).

/*-------------------------------------------------------- */
/* Collect all the actions from the stack in reverse order  */
/*   extract_plan(+Rstack, +RevPlan, -Plan)                */
/*-------------------------------------------------------- */
extract_plan([],Plan, Plan).
extract_plan([op(X) | More], RevPlan, Plan) :- !,
	extract_plan(More, [X | RevPlan], Plan).
extract_plan([_ | More], RevPlan, Plan) :- 
	extract_plan(More, RevPlan, Plan).

/*----------------------------------------------------------------------- */
/* Does Fact holds in the current state?                                  */
/* A condition hold if there is some action A1 that added the condition to*/
/* the global state and there is no action A2 which executed after A1 that*/
/* deleted  the condition from the global state. We can think of the      */
/* initial state as an action that added all the initial conditions to the*/
/* global state.                                                          */
/*  hold(+Fact, +RStack)                                                  */
/*----------------------------------------------------------------------- */
% The fact is part of the intilize state.
hold(Cond, [])     :- init_state(Cond).
% The fact is part of applying an action
hold(Cond, [op(Action)|_]):- adds(Action, Cond), !.
% The fact is not deleted by the action and it holds.
hold(Cond, [op(Action)|More]):- !, 
	preserved(Action, Cond),
	hold(Cond, More).
hold(Cond, [_ | More]) :- hold(Cond, More).

/*----------------------------------------------------------------------- */
/* Verify that the list of facts still hold under the current plan.       */
/*  holds(+FactList, +RStack)                                             */
/*----------------------------------------------------------------------- */
holds([],_).
holds([Cond|More], RStack):-
	hold(Cond, RStack),
	holds(More, RStack).

/*----------------------------------------------------------------------- */
/* Perform the Action or regress if some protection are violated.         */
/*  perform(+CompundGoal, +Action,+SubGoal,+Rstack,-NewRstack)            */
/*----------------------------------------------------------------------- */
perform(List, Act, Fact, Rstack, NRstack):- 
	remove_protection(Rstack,List, Rstack1),
	NRstack = [pro(Fact), op(Act), and(List) | Rstack1].

/*----------------------------------------------------------------------- */
/* Remove the protection of every condition in the list.                  */
/*   remove_protection(+Rstack,+List, -NRstack)                           */
/*----------------------------------------------------------------------- */
remove_protection(Rstack,[], Rstack).
remove_protection(Rstack,[Cond | More], NRstack) :-
	remove_protection1(Rstack, Cond, Rstack1),
	remove_protection(Rstack1, More, NRstack).

remove_protection1([],_, []).
remove_protection1([pro(Cond) | More], Cond, [Cond | NRstack]) :- !,
	remove_protection1(More,Cond, NRstack).
remove_protection1([X|More],Cond, [X|NRstack]) :- 
	remove_protection1(More,Cond, NRstack).

/*----------------------------------------------------------------------- */
/* Prove that the action does not delete the protected facts.             */
/* For every fact in the delete list of the action you need to check that */
/* one of the following is true: the fact is not protected or the fact is */
/* protected but it is only temporary deletion meaning, there is an action*/
/* in the same execution context that will add the fact back.             */
/*   preserving(+Action, +Rstack, +GoalStack)                             */
/*----------------------------------------------------------------------- */
preserving(_,[],_) :- !.
preserving(Action, [pro(Fact)| More], Stack):- !,
	(preserved(Action, Fact), !; temporary_viol(Stack, Fact), !),
	preserving(Action, More, Stack).
preserving(Action, [_| More], Stack):- 
	preserving(Action, More, Stack).

/*----------------------------------------------------------------------- */
/* Check if a fact is not a member of the deletion set of the action.     */
/*   preserved(+Action, +Fact)                                            */
/*----------------------------------------------------------------------- */
preserved(Action, Fact):-
	(dels(Action, Fact),!,fail; !).


/*----------------------------------------------------------------------- */
/* Check if it only temporary violation.                                  */
/* A protected fact is temporary violated if there is an action in the    */
/* execution context after the action that delete the protected fact that */
/* will add the fact back.                                                */
/*   temporary_viol(+GoalStack, +Fact)                                    */
/*----------------------------------------------------------------------- */
temporary_viol([and(List)|_], Fact):-         % End of the protection scope
  member(Fact, List),!,fail.
temporary_viol([op(Act),_|_], Fact):-  adds(Act, Fact).
 temporary_viol([_|S], Fact):-  temporary_viol(S, Fact).
temporary_viol([],_):- write(' **** Must be an error in stack'),nl,!,fail.






/*********************************************************************/
/*********************************************************************/
/*********************************************************************/
% Utility
assert_init(InitState):- 
  retractall(init_state(X)),
  doall((member(X,InitState),assert(init_state(X)))), !.


% Libraries
%:- [library(basics),library(sets)].
%:- leash([call,exit,fail,redo]).


strips3(Goal, InitState, Plan):-
	strips1(Goal, InitState, [], [], _, RevPlan),
        reverse( RevPlan, Plan).

% strips(+GoalList, +State, +Plan, +ForbiddenActions, -NewState, -NewPlan )
strips1(GoalList, State, Plan, _, State, Plan) :-
	subset(GoalList,State).
strips1(GoalList, State, Plan, ForbiddenActions, NewState, NewPlan):-
	member(Goal, GoalList),
        not(member(Goal, State)),
	adds(Ac, Goal),
	% No repeat action during the planing
	not(member(Ac, ForbiddenActions)),
	preclist(Ac, PrecList), 
	strips1(PrecList, State, Plan, [Ac| ForbiddenActions], TmpState1, TmpPlan1), 
	apply_rule(Ac, TmpState1, TmpState2),	
%	append([Ac|TmpPlan1], Plan, TmpPlan2),
	strips1(GoalList, TmpState2, [Ac|TmpPlan1], ForbiddenActions, NewState, NewPlan).

apply_rule(Ac, State, NewState):-
        nl,write('doing '),write(Ac),ttyflush,
  	dellist(Ac, DelList),
  	subtract(State, DelList, TmpState),
	addlist(Ac, AddList),
	union(AddList, TmpState, NewState).

%unsatisfied(Goal,State,UnsatisfiedGoal) :-
%	member(UnsatisfiedGoal, Goal),
%	not(member(UnsatisfiedGoal, State)).

	
/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
% Utility predicates
%not(X) :- X, !, fail.
%not(_).

writeplan([]).
writeplan([A|B]):-
  write('       '),write(A),nl,
  writeplan(B).
/*
reverse(X,Y):- reverse(X,[],Y).
reverse([A|S], B, Sol):-
  reverse(S,[A|B],Sol).
reverse([],P,P).
*/





:-include(moo_ext_strips_test).




% Libraries
%:- [library(basics)], [library(sets)].
% rstrips  is the main RSTRIPS program.
% Arguments: initial state (+), goal list (+),  plan (-)  
rstrips( InitState,  Goal, Plan):-
	add_scope(1, Goal, Bstack),
	rstrips1( InitState, Bstack, [ ], 1, Plan),
        display_plan(Plan).  

%  Arguments to {\progfont rstrips1 / 5}: initial state (+), Bstack (+), Tstack (+),
%   paranthesis counter (+), plan (--)      

% DEbuging only.
%rstrips1( _, Bstack, Tstack, _,_) :-  nl,display_stack(Bstack, Tstack),fail.

% case: Bstack is empty 
rstrips1( _, [ ], Tstack, _, Plan) :- extract_plan( Tstack, Plan ).
% case: top of Bstack is an operator followed by a subgoal
rstrips1( InitState, [op(Op,S), scope(Cond, Scope) | Bstack], Tstack, Ctr, Plan):-  !,
	display_prec_accomplished( Cond, Op ), 
	remove_protection( S, Tstack, Tstack1),
	rstrips1( InitState, Bstack, [pro(scope(Cond, Scope)), op(Op,S)| Tstack1], Ctr, Plan).
% case: top of Bstack is a satisfied condition
rstrips1( InitState, [scope(Cond, Scope) | Bstack], Tstack, Ctr, Plan):- 
	holds( Cond, Tstack, InitState), !,
	display_hold_condition(Cond),
	NewCtr is Ctr+1,
	rstrips1( InitState, Bstack, [pro(scope(Cond, Scope)), op(no-op,NewCtr)| Tstack], NewCtr, Plan).
% case: top of Bstack is an unsatisfied condition  
rstrips1( InitState, [scope(Cond, Scope) |Bstack], Tstack, Ctr, Plan):-
	% find an operator adding the condition 
	adds(Op, CondList), member(Cond, CondList),
	% as in STRIPS, the following may be omitted, risking infinite loops 
	not(member(op(Op,_), Bstack)), % do not repeat actions  
	preserves_protection( Op, Bstack, Tstack), 
	prec( Op, Prec ),
       	display_op_selection( Cond , Op, Prec ), 
	NewCtr is Ctr+1,
	add_scope(NewCtr, Prec, PPrec),
	append(	PPrec, [op(Op,NewCtr), scope(Cond, Scope) | Bstack],NewBstack ),
	rstrips1( InitState, NewBstack, Tstack, NewCtr, Plan ).
% case: top of Bstack is an unsatisfied condition. Regress.
rstrips1( InitState, [scope(Cond, Scope) |Bstack], Tstack, Ctr, Plan):-
	% find an operator adding the condition 
	adds(Op, CondList), member(Cond, CondList),
	% as in STRIPS, the following may be omitted, risking infinite loops 
	not(member(op(Op,_), Bstack)), % do not repeat actions	
	% Find the violated goal.
	find_violated_subgoal(Op, Bstack, Tstack, Vgoal, Vscope, Vop),
	find_regressed_subgoal(Vop, [op(Op,_), scope(Cond, Scope)|Bstack], Rgoal, Rscope),
	display_regression(Op,Vgoal,Rgoal,Vop),
	regress(Vgoal, Vscope, Rgoal, Rscope, [scope(Cond, Scope)|Bstack], Tstack, NewBstack, NewTstack ),
	rstrips1( InitState, NewBstack, NewTstack, Ctr, Plan ).


% regress regresses the goal violating protection through the operator that
% achieves the protected subgoal                     
% Arguments: Bstack (+), Tstack (+), new Bstack (-), new Tstack (-)  
regress(Vgoal, Vscope, Rgoal, Rscope, Bstack, Tstack, NewBstack, NewTstack) :- 
	clean_bstack(Bstack, Rgoal, Rscope, Bstack1),
	clean_tstack(Tstack, Vgoal, Vscope, Tstack1),
	new_stacks(Bstack1, Tstack1, Vgoal, Vscope, NBstack, NewTstack),
	NewBstack = [scope(Rgoal, Rscope)| NBstack], !. 


% End of cleaning.
clean_bstack([scope(Rgoal, Rscope)|Bstack1], Rgoal, Rscope, Bstack1) :- !.
%clean_bstack([scope(Cond, Scope)|Bstack], Rgoal, Rscope, [scope(Cond, Scope)|Bstack1]) :-
%	Scope =< Rscope,
%	clean_bstack(Bstack, Rgoal, Rscope, Rscope, Bstack1).
clean_bstack([_|Bstack], Rgoal, Rscope, Bstack1) :-
	clean_bstack(Bstack, Rgoal, Rscope, Bstack1).


% End of cleaning.
clean_tstack([pro(scope(Vgoal, Vscope))|Tstack1], Vgoal, Vscope, [scope(Vgoal, Vscope)|Tstack1]) :- !.
% The condition is not part of the plan to achive the regress goal.
clean_tstack([pro(scope(Cond, Scope)), _|Tstack], Vgoal, Vscope, [scope(Cond, Scope)|Tstack1]) :-
	Scope =< Vscope, !,
	clean_tstack(Tstack, Vgoal, Vscope,  Tstack1).
% Anything else is discarded.
clean_tstack([_|Tstack], Vgoal, Vscope, Tstack1) :-clean_tstack(Tstack, Vgoal, Vscope, Tstack1).


% Build new stacks.
% The vilated goal was achived by a no-op.
new_stacks(Bstack, [scope(Vgoal,Vscope),op(no-op,_) |Tstack], Vgoal, Vscope,NBstack,Tstack) :-!,
	NBstack = [scope(Vgoal,Vscope)|Bstack].
% The vilated goal was achived by an operator.
new_stacks(Bstack,[scope(Vgoal,Vscope),op(Op,S)|Tstack],Vgoal,Vscope,NBstack,NTstack) :-!,
	NBstack = [op(Op,S),scope(Vgoal,Vscope)|Bstack],
	protect(S, Tstack, NTstack).
% Any thing else is transfered. 
new_stacks(Bstack, [X|Tstack], Vgoal, Vscope, NewBstack, NewTstack) :-
	new_stacks([X|Bstack], Tstack, Vgoal, Vscope, NewBstack, NewTstack).

% Find the violated operator.
find_violated_subgoal(Op, Bstack, [pro(scope(Cond, Scope)), op(Op1,_) | _], Cond, Scope, Op1):-
	dels(Op, CondList),member(Cond, CondList), 
	not(temporary_viol( Bstack, Cond, Scope)),!.
find_violated_subgoal(Op, Bstack, [_| More], Cond, Scope, Op1) :- 
       find_violated_subgoal(Op, Bstack, More, Cond, Scope, Op1).


% Find the regressed subgoal. 	
find_regressed_subgoal(_, [], _, _) :- !, fail. % Can not regress.
find_regressed_subgoal(Vop, [op(_,_), scope(Cond, Scope)|_], Cond, Scope) :-
	can_regress(Vop, Cond). 
find_regressed_subgoal(Vop, [op(_,_), scope(_, S1)|Bstack], Cond, Scope) :-
	skip_stack(S1, Bstack, Bstack1),
	find_regressed_subgoal(Vop, Bstack1, Cond, Scope).

skip_stack(Scope, [op(Op, Scope)|Bstack], [op(Op, Scope)|Bstack]) :- !.
skip_stack(Scope, [_|Bstack], Bstack1) :- skip_stack(Scope, Bstack, Bstack1).

can_regress(no_op, -) :- !.
can_regress(Vop, Cond) :- 
	dels(Vop, Cond), !, fail.
can_regress(Vop,Cond) :- 
	prec( Vop, CondList ),
        not(( member( Cond1, CondList ), inconsistent( Cond, Cond1) )).


% below is Avrami's
%can_regress(no-op,_) :- !.
%can_regress(Vop, Cond) :- 
%	dels(Vop, DelList),
%	member(Cond, DelList), !,fail.
%can_regress(Vop,Cond) :- 
%	prec(Vop, CondList),
%	no_constrains(CondList, Cond).

%no_constrains([], _).
%no_constrains([C|More], Cond) :-
%	not(inconsistent(Cond, C)) ,not(inconsistent(C, Cond)) ,
%	no_constrains(More, Cond).


% protect  adds protection to specified conditions in Tstack            
% Arguments: Scope number (+), Tstack (+), new Tstack (-)              

protect( _, [], []):-!.
protect( Scope, [scope(Cond, Scope) | More], [pro(scope(Cond, Scope)) | NTstack] ) :-!,
	protect( Scope, More, NTstack ).
protect( Scope, [X | More], [X | NTstack] ) :- 
	protect( Scope, More, NTstack ).

% {\progfont remove_protection / 3 removes the protection in Tstack of specified context.      
% Arguments: Context number (+), Tstack (+),  new Tstack (-)  

remove_protection( _, [], []) :- !.
remove_protection(Scope, [pro(scope(Cond, Scope)) | More], [scope(Cond, Scope) | NTstack] ) :- !,
	remove_protection( Scope, More, NTstack ).
remove_protection(Scope, [ X | More], [ X | NTstack] ) :-
	remove_protection( Scope, More, NTstack ). 

 
% {\progfont extract_plan / 2 collects all the operators from the stack in reverse order  
% Arguments: Tstack (+), plan (-)  

extract_plan( Tstack, Plan ) :- extract_plan1( Tstack, [ ], Plan ), !. 
extract_plan1([ ], Plan, Plan).
extract_plan1( [op(X,_) | More], RevPlan, Plan ) :-  not(X=no-op), extract_plan1( More, [X|RevPlan], Plan ).
extract_plan1( [_ | More], RevPlan, Plan ) :- extract_plan1( More, RevPlan, Plan ).


% {\progfont holds / 3 verifies that a condition holds in the ``current'' state; this is so iff one                        
%  of the following is true: (1) the current state is the initial state (i.e., empty 
% Tstack), and the condition is true in it; (2) the immediately preceding operator 
% added the condition; (3)  the immediately preceding operator did not delete 
% the condition, and (recursively) the condition held right before it 
%  Arguments: condition (+), Tstack (+), initial state (+)  

holds( Cond, [ ], InitState )     :- member( Cond, InitState ), !. % case (1) 
holds( Cond, [op(Op,_) | _], _ ):- adds( Op, CondList ), member(Cond, CondList),!. % case (2)
% case (3) 
holds( Cond, [op(Op,_) | More], InitState ) :- !, % case (3)
        not((dels(Op, CondList), member(Cond, CondList))), 
	holds( Cond, More, InitState ).
% otherwise, ignore top of Tstack 
holds( Cond, [_  |  More], InitState ) :- holds( Cond, More, InitState ). 


% {\progfont preserves_protection / 3 verifies that an operator does not delete any condition  
% which is protected (in Tstack). For every condition in the delete list of the      
% operator, it checks that one of the following is true: (1) the condition is not   
% protected, or (2) it is protected, but the deletion is only temporary, meaning     
% that there is an operator later in the same execution scope that reinstates       
% the condition.
% Arguments: operator (+), Bstack (+), Tstack (+) 

preserves_protection( _, _, [ ]) :- !.
preserves_protection( Op, Bstack, [pro(scope(Cond, Scope)) | More]) :- !,
	(not((dels(Op, CondList), member(Cond, CondList))) ; temporary_viol( Bstack, Cond, Scope)),!, 
	preserves_protection( Op, Bstack, More).
preserves_protection( Op, Bstack, [_| More]) :- 
       preserves_protection( Op, Bstack, More).


% {\progfont temporary_viol / 3 verifies that a goal protection violation is only temporary; this 
% is so whenever there is another operator later in the execution context with the  
% goal in its add list 
% reached end of scope.
temporary_viol( [ op(Op, Scope)| _], Cond, Scope) :- !, adds( Op, Cond).
% found 'white knight'
temporary_viol( [ op(Op,_) | _ ], Cond, _ ) :- adds( Op, Cond), !.
temporary_viol( [ _ | S], Cond, Scope) :-  temporary_viol( S, Cond, Scope).

            
% Arguments to {\progfont scope / 3}: Unique number (+), Sub-goals list (+), 
% Subgoal-numbered list (--)
add_scope(_, [], []):-!.
add_scope(Scope, [X|More], [scope(X,Scope)| Ret]) :- add_scope(Scope, More, Ret).


% display_plan /1 added by Yoav
display_plan(P) :-
   nl, write('--------------------------------------------------'),
   nl, write('PLANNING FOUND...'),
   nl, write('execute the following sequence:'), nl, tab(2), writeplan1(P).

display_plan(I, G, P) :-
   nl, write('--------------------------------------------------'),
   nl, write('PLANNING IS NOW COMPLETE;'),
   nl, write('to get from'), nl, tab(2), write(I),
   nl, write('to the goal'), nl, tab(2), write(G),
   nl, write('execute the following sequence:'), nl, tab(2), writeplan1(P).

writeplan1([]).
writeplan1([A|B]) :- write(A),write(' ; '),writeplan1(B).

% {\progfont display_prec_accomplished / 2 notifies when an operator's preconditions have been
% accomplished 

display_prec_accomplished( Cond, Op ) :-
	writel( [ 'Preconditions of operator ', Op, ' have been accomplished;' ]),
	tab(2), writel( [ Op, ' and ', Cond, ' are moved to Tstack;' ] ), 
	tab(2), writel( [ 'protection is lifted from the preconditions of ', Op ] ), 
	tab(2), writel( [ 'protection is placed on ', Cond ] ).
display_prec_accomplished( Cond, Op ) :-  % Execute uppon backtracking
	write('** Backtracking from: '),nl,
	tab(3), writel( [ 'Preconditions of operator ', Op, ' have been accomplished;' ]),
	tab(5), writel( [ Op, ' and ', Cond, ' are moved to Tstack;' ] ),  
	tab(5), writel( [ 'protection is lifted from the preconditions of ', Op ] ),  
	tab(5), writel( [ 'protection is placed on ', Cond ] ), !,fail.

% {\progfont display_op_selection / 3 notifies of operator selection 

display_op_selection( Cond, Op, Prec ) :-  
	writel( [ 'Operator ', Op, ' added to stack, in order to achieve ', Cond, ';' ] ), 
	tab(2), writel( [ 'it is preceded by its preconditions, ', Prec ] ).
display_op_selection( Cond, Op, Prec ) :- 
	write('** Backtracking:'), nl,
        tab(2), writel(['operator ', Op, ' de-selected for goal ', Cond ]),
        fail.  
%avrami's
%display_op_selection( Cond, Op, Prec ) :- % Execute uppon backtracking
%	write('** Backtracking from: '),nl, 
%	tab(3),writel( [ 'Operator ', Op, ' added to stack, in order to achieve ', Cond, ';' ] ), 
%	tab(5), writel( [ 'it is preceded by its preconditions, ', Prec ] ).

display_hold_condition(Cond) :-
	writel( [ '(Sub)goal ', Cond, ' holds in the current state.']),
	tab(2), writel( [ 'protection is placed on ', Cond ] ).
display_hold_condition(Cond) :- % Execute uppon backtracking
	writel(['** Backtracking past the (sub)goal ', Cond, ' ;' ]), nl,
        tab(2), write('can no longer assume it holds after no-op'),
        fail.
%avrami's
%display_hold_condition(Cond) :- % Execute uppon backtracking
%	write('** Backtracking pastfrom: '),nl, 
%	tab(3),writel( [ 'Subgoal ', Cond, ' holds in the current state.']),
%	tab(5), writel( [ 'protection is placed on ', Cond ] ).

% {\progfont display_regression / 2 prints regression steps 

display_regression( Op, X , V, Op1) :- 
	writel( [ 'Applying  operator ', Op, ' violates ', V, ';' ] ),
	tab(2), writel( [ 'regressing ', X, ' through ' , Op1] ).
display\_regression( \un, X , \un, Op1) :- % Execute upon backtracking
	write('** Backtracking: '), nl, tab(2), 
	writel( [ 'regressing ', X, ' through ', V, 
                  ' did not lead to a solution' ] ), 
        fail.
%Avrami's
%display_regression( Op, X , V, Op1) :- % Execute uppon backtracking
%%n	write('** Backtracking from: '),nl, 
%	tab(3),writel( [ 'Applying  operator ', Op, ' violates ', V, ';' ] ),
%	tab(5), writel( [ 'regressing ', X, ' through ' , Op1] ).













end_of_file.
































/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utilities
writel([]) :- nl.
writel([X|More] ):- write(X), tab(1), writel1(More).


doall(X) :- X, fail.
doall(_).

not(X) :- X, !, fail.
not(_).

remove(_,[],[]) :- !, fail.
remove(X, [X|More], More)  :- !.
remove(X, [Y|More], [Y|Ret]) :- remove(X, More, Ret).

writeplan([]).
writeplan([A|B]):-
  write('       '),write(A),nl,
  writeplan(B).

*/

% Display  rstrips stack.
display_stack(Bstack, Tstack) :-
	revers(Bstack, [], Temp),
	append(Temp, ['-----------'|Tstack], Stack),
	do1(Stack, [shift(1,1)], 1), !.
revers([], Temp, Temp).
revers([X|More], Temp, NTemp) :- revers(More, [X|Temp], NTemp).

do1([],_, _).
do1([op(O,S) | More], List, Cur):-
	N1 is Cur+1,
	do1(More, [shift(S,N1) | List], N1),
	tab1(N1),
	write(op(O,S)), nl.
do1([pro(scope(Cond, S))| More], List, _):-!, 
	member(shift(S,Cur), List),
	do1(More, List, Cur),
	tab1(Cur),
	write(pro(scope(Cond, S))), nl.
do1([scope(Cond, S)| More], List, _):- !, 
	member(shift(S,Cur), List),
	do1(More, List, Cur),
	tab1(Cur),
	write(scope(Cond, S)), nl.
do1([X| More], List, Cur):-!, 
	do1(More, List, Cur),
	tab1(Cur),
	write(X), nl.

tab1(N) :- N1 is 4*N, tab(N1).


/*-------------------------------------------------- */
/*  main_rstrips(+Goal, +InitState, -Plan)           */
/*-------------------------------------------------- */
main_rstrips(Goal, InitState, Plan):-
	% Assert initilize state to database.
	assert_init(InitState), 
	append(Goal, [and(Goal)], Stack),
	rstrips(Stack,[],Plan).

/*-------------------------------------------------------- */
/*  rstrips(+Stack, +Rstack, -Plan   )                     */
/*-------------------------------------------------------- */
% No more goals in the stack => plan is done 
rstrips([and(_)], Rstack,Plan) :- extract_plan(Rstack, [], Plan). 
% Top of the goals stak is an trio of [compound goal, action, subgoal].
rstrips([and(List),op(Act),Fact|Stack], Rstack, Plan):-  !,
	perform(List, Act, Fact, Rstack, NRStack),
	rstrips(Stack, NRStack, Plan).
% Top of the goals stack is a satisfied condition.
rstrips([Cond|Stack], RStack, Plan):- 
	hold(Cond, RStack), !,
	rstrips(Stack, [pro(Cond)|RStack], Plan).
% Top of the goals stack is an unsatisfied condition. 
rstrips([Cond|Stack], Rstack, Plan):- 
	adds(Action, Cond),
	% No repeat future action
	not(member(op(Action), Stack)),
	achieve(Cond, Action, Stack, Rstack, NStack, NRstack), 
	rstrips(NStack, NRstack, Plan).

/*------------------------------------------------------------- */
/* Achieve the subgoal by action or by regression.               */
/*    achieve(+Cond, +Action, +Stack, +Rstack, -NStack, -Rstack)*/
/*------------------------------------------------------------- */
achieve(Cond, Action, Stack, Rstack, NStack, Rstack) :-
	preserving(Action, Rstack, Stack),!,
	prec(Action, Prec),
	append(Prec, [and(Prec),op(Action),Cond|Stack], NStack).
achieve(_, Action, Stack, Rstack, NStack, NRstack) :-
	regress(Action, Stack, Rstack, NStack, NRstack).
	

/*-------------------------------------------------------- */
/* Regress the protection violating goal through the action*/
/* that achieved the protected subgoal                     */
/*   regress(+Action, +Stack, +Rstack, -NStack, -NRstack)  */
/*-------------------------------------------------------- */
regress(Action, Stack, Rstack, NStack, NRstack) :-
	setof(Cond, dels(Action,Cond), DelList),
	% Pop the Rstack up to the violated protectedf subgoal
	pop_rstack(DelList, Rstack, [Violated |Rstack1]),
	% Pop the stack up to the protection violating subgoal
	pop_stack(Stack, Violated, [], Stack1),
	go_through(Stack1, Violated, Rstack1, NStack, NRstack),!.

/*------------------------------------------------------------- */
/* Regress the violating subgoal through the violated goal      */
/*    go_through(+Stack, +Violated, +Rstack, -NStack, -NRstack) */ 
/*------------------------------------------------------------- */
go_through([X | Stack], V, [op(Act),and(List)| Rst], NSt, NRst) :-
	NSt = [X, and(List), op(Act), V | Stack],
	protect(List, Rst, NRst), !.

/*-------------------------------------------------------- */
/* Protect the conditions of the list in Rstack            */
/*    protect(+List, +Rstack, -NRstack)                    */
/*-------------------------------------------------------- */
protect([], Rstack, Rstack).
protect(_,[], _) :- 
	write(' **** Must be an error in stack'),nl,!,fail.
protect(CondList,[Cond | More], [pro(Cond) | NRstack] ) :-
	member(Cond, CondList), !,
	subtract(CondList, [Cond], CondList1),
	protect(CondList1, More, NRstack).
protect(CondList, [X | More], [X | NRstack]) :-
	protect(CondList, More, NRstack).

/*------------------------------------------------------------- */
/* Pop the Rstack until you find the subgoal that is been       */
/* violated by the one of the action deleted condition. If not  */
/* found then there is error in the stack.                      */
/*   pop_rstack(+DelList, +RStack, -Violated, -Rstack1)         */
/*------------------------------------------------------------- */
pop_rstack(_, [], _) :- 	
	write(' **** Must be an error in stack'),nl,!,fail.
pop_rstack(DelList, [pro(Fact) | More], [Fact | More]) :-
	member(Fact, DelList), !.
pop_rstack(DelList, [_| More], Rstack) :-
	pop_rstack(DelList, More, Rstack).	

/*------------------------------------------------------------- */
/* Pop the Stack until you reach the protection violating       */
/* subgoal.                                                     */
/*   pop_stack(+Stack, +Violated, +TempStack, -NStack1)         */
/*------------------------------------------------------------- */
pop_stack([], _, _, _) :-
	write(' **** Must be an error in stack'),nl,!,fail.
pop_stack([and(List) | More], Violated, TempStack, NStack) :-
	member(Violated, List), !,
	find_violating_subgoal([and(List) |More], TempStack, NStack), !.
pop_stack([X| More], Violated, TempStack, NStack) :-
	pop_stack(More, Violated, [X|TempStack], NStack) .

/*------------------------------------------------------------- */
/* Return the stack below and include the protection violating */
/* subgoal                                                      */
/*   find_violating_subgoal(+Stack, +TempStack, -NStack)        */
/*------------------------------------------------------------- */
find_violating_subgoal(NStack, [op(_) | _], NStack).
find_violating_subgoal(Stack, [X| More], NStack) :-
	find_violating_subgoal([X | Stack], More, NStack).

/*-------------------------------------------------------- */
/* Collect all the actions from the stack in reverse order  */
/*   extract_plan(+Rstack, +RevPlan, -Plan)                */
/*-------------------------------------------------------- */
extract_plan([],Plan, Plan).
extract_plan([op(X) | More], RevPlan, Plan) :- !,
	extract_plan(More, [X | RevPlan], Plan).
extract_plan([_ | More], RevPlan, Plan) :- 
	extract_plan(More, RevPlan, Plan).

/*----------------------------------------------------------------------- */
/* Does Fact holds in the current state?                                  */
/* A condition hold if there is some action A1 that added the condition to*/
/* the global state and there is no action A2 which executed after A1 that*/
/* deleted  the condition from the global state. We can think of the      */
/* initial state as an action that added all the initial conditions to the*/
/* global state.                                                          */
/*  hold(+Fact, +RStack)                                                  */
/*----------------------------------------------------------------------- */
% The fact is part of the intilize state.
hold(Cond, [])     :- init_state(Cond).
% The fact is part of applying an action
hold(Cond, [op(Action)|_]):- adds(Action, Cond), !.
% The fact is not deleted by the action and it holds.
hold(Cond, [op(Action)|More]):- !, 
	preserved(Action, Cond),
	hold(Cond, More).
hold(Cond, [_ | More]) :- hold(Cond, More).

/*----------------------------------------------------------------------- */
/* Verify that the list of facts still hold under the current plan.       */
/*  holds(+FactList, +RStack)                                             */
/*----------------------------------------------------------------------- */
holds([],_).
holds([Cond|More], RStack):-
	hold(Cond, RStack),
	holds(More, RStack).

/*----------------------------------------------------------------------- */
/* Perform the Action or regress if some protection are violated.         */
/*  perform(+CompundGoal, +Action,+SubGoal,+Rstack,-NewRstack)            */
/*----------------------------------------------------------------------- */
perform(List, Act, Fact, Rstack, NRstack):- 
	remove_protection(Rstack,List, Rstack1),
	NRstack = [pro(Fact), op(Act), and(List) | Rstack1].

/*----------------------------------------------------------------------- */
/* Remove the protection of every condition in the list.                  */
/*   remove_protection(+Rstack,+List, -NRstack)                           */
/*----------------------------------------------------------------------- */
remove_protection(Rstack,[], Rstack).
remove_protection(Rstack,[Cond | More], NRstack) :-
	remove_protection1(Rstack, Cond, Rstack1),
	remove_protection(Rstack1, More, NRstack).

remove_protection1([],_, []).
remove_protection1([pro(Cond) | More], Cond, [Cond | NRstack]) :- !,
	remove_protection1(More,Cond, NRstack).
remove_protection1([X|More],Cond, [X|NRstack]) :- 
	remove_protection1(More,Cond, NRstack).

/*----------------------------------------------------------------------- */
/* Prove that the action does not delete the protected facts.             */
/* For every fact in the delete list of the action you need to check that */
/* one of the following is true: the fact is not protected or the fact is */
/* protected but it is only temporary deletion meaning, there is an action*/
/* in the same execution context that will add the fact back.             */
/*   preserving(+Action, +Rstack, +GoalStack)                             */
/*----------------------------------------------------------------------- */
preserving(_,[],_) :- !.
preserving(Action, [pro(Fact)| More], Stack):- !,
	(preserved(Action, Fact), !; temporary_viol(Stack, Fact), !),
	preserving(Action, More, Stack).
preserving(Action, [_| More], Stack):- 
	preserving(Action, More, Stack).

/*----------------------------------------------------------------------- */
/* Check if a fact is not a member of the deletion set of the action.     */
/*   preserved(+Action, +Fact)                                            */
/*----------------------------------------------------------------------- */
preserved(Action, Fact):-
	(dels(Action, Fact),!,fail; !).


/*----------------------------------------------------------------------- */
/* Check if it only temporary violation.                                  */
/* A protected fact is temporary violated if there is an action in the    */
/* execution context after the action that delete the protected fact that */
/* will add the fact back.                                                */
/*   temporary_viol(+GoalStack, +Fact)                                    */
/*----------------------------------------------------------------------- */
temporary_viol([and(List)|_], Fact):-         % End of the protection scope
  member(Fact, List),!,fail.
temporary_viol([op(Act),_|_], Fact):-  adds(Act, Fact).
 temporary_viol([_|S], Fact):-  temporary_viol(S, Fact).
temporary_viol([],_):- write(' **** Must be an error in stack'),nl,!,fail.



/*********************************************************************/
/*********************************************************************/
/*********************************************************************/
% Database blocks

/* stack(X,Y) */
prec(stack(X,Y), [holding(X),clear(Y)]).
dels(stack(X,_), holding(X)).
dels(stack(_,Y), clear(Y)).
adds(stack(_,_), handempty).
adds(stack(X,Y), on(X,Y)). 
adds(stack(X,_), clear(X)).

/* unstack(X,Y) */
prec(unstack(X,Y), [on(X,Y), clear(X), handempty]).
dels(unstack(_,_), handempty).
dels(unstack(Y,_), clear(Y)).
dels(unstack(X,Y), on(X,Y)).
adds(unstack(X,_), holding(X)).
adds(unstack(_,Y), clear(Y)).

/* pickup(X) */
prec(pickup(X),  [ontable(X), clear(X), handempty]).
dels(pickup(X),  ontable(X)).
dels(pickup(X),  clear(X)).
dels(pickup(_),  handempty).
adds(pickup(X),  holding(X)).

/* putdown(X) */
prec(putdown(X), [holding(X)]).
dels(putdown(X), holding(X)).
adds(putdown(X), ontable(X)).
adds(putdown(_), handempty).
adds(putdown(X), clear(X)).



/*--------------------------------------
                        |
                        X
                       / \

    +-----+
    |  c  |
    +-----+      +-----+
    |  a  |      |  b  |
----+-----+------+-----+----------------
*/


/* -------------------------------------
                        |
                        X
                       / \



    +-----+      +-----+     +-----+
    |  a  |      |  b  |     |  c  |
----+-----+------+-----+-----+-----+------
*/
% Initilize state
init_state1([clear(a),clear(b),clear(c),ontable(a),ontable(b),ontable(c),handempty]).
goal1([on(a,b),on(b,c) ]).

init_state2([clear(a),clear(b),clear(c),ontable(a),ontable(b),ontable(c),handempty]).
goal2([on(b,c),on(a,b) ]).

init_state3([clear(b),clear(c),ontable(a),ontable(b),on(c,a),handempty]).
goal3([on(a,b),on(b,c)]).

init_state4([clear(a),on(a,b),on(b,c),ontable(c),handempty]).
goal4([on(a,b),on(b,a)]).

init_state5([clear(c),ontable(a),on(b,a),on(c,b),handempty]).
goal5([on(a,b),on(b,c)]).


test1 :-
	init_state1(InitState),
	goal1(Goal),
	do_it(Goal, InitState).
test2 :-
	init_state2(InitState),
	goal2(Goal),
	do_it(Goal, InitState).
test3 :-
	init_state3(InitState),
	goal3(Goal),
	do_it(Goal, InitState).
test4:-
	init_state4(InitState),
	goal4(Goal),
	do_it(Goal, InitState).
test5 :-
	init_state5(InitState),
	goal5(Goal),
	do_it(Goal, InitState).

do_it(Goal, InitState) :-
	main_rstrips(Goal,InitState,Plan),
	write('   from: '),write(InitState),nl,
	write('   goal: '),write(Goal),nl,
	write('     -->   '),nl,
        	writeplan(Plan),nl.

writeplan([]).
writeplan([A|B]):-
  write('       '),write(A),nl,
  writeplan(B).






main_strips(Goal, InitState, Plan):-
  iterative(Goal,InitState, _,Plan, 0,100).


iterative(_,_,_,_,N, Max) :-
	N > Max, !,fail.
iterative(Goal, InitState,  FinalState,Plan, N, _) :-
  	strips(Goal,InitState, FinalState, [],Plan,N), !.
iterative(Goal,InitState,  FinalState,Plan, N, Max) :-
  	N1 is N+1,
  	iterative(Goal,InitState, FinalState, Plan, N1, Max).


% strips(+GoalList, +State, +NState, +TempPlan, -Plan, +N)
strips(Goal, State, State, P, P, _) :-
	satisfied(Goal,State).
strips(_,_,_,_,_,0):- !, fail.
strips(Goal, State, NewState, Plan, PlanOut, N):-
	unsatisfied(Goal,State,UnsatisfiedGoal),
	adds(Action, UnsatisfiedGoal),
	prec(Action, Prec), 
	N1 is N-1,
	iterative(Prec, State, NewState1, PlanOut1, 0,N1),
	apply_action(Action, NewState1, NewState2),	
	length(PlanOut1, N2),
	NN is N1-N2,
	append([Action|PlanOut1], Plan, PlanOut2),
	strips(Goal, NewState2, NewState, PlanOut2, PlanOut, NN).

apply_action(Action, State, NewState):-
  	setof(Del,dels(Action, Del), DelList),
  	diff(State, DelList, S1),
	setof(X, adds(Action, X), Add),
	union(Add, S1, NewState),!.

unsatisfied(Goal,State,UnsatisfiedGoal) :-
	member(UnsatisfiedGoal, Goal),
	not(member(UnsatisfiedGoal, State)).

satisfied([],_).
satisfied([Goal | More],State) :-
	member(Goal, State),
	satisfied(More,State).
	


/****************************************************************************/
/****************************************************************************/
/****************************************************************************/
% Database blocks

/* stack(X,Y) */
prec(stack(X,Y), [holding(X),clear(Y)]).
dels(stack(X,_), holding(X)).
dels(stack(_,Y), clear(Y)).
adds(stack(_,_), handempty).
adds(stack(X,Y), on(X,Y)). 
adds(stack(X,_), clear(X)).

/* unstack(X,Y) */
prec(unstack(X,Y), [on(X,Y), clear(X), handempty]).
dels(unstack(_,_), handempty).
dels(unstack(X,_), clear(X)).
dels(unstack(X,Y), on(X,Y)).
adds(unstack(X,_), holding(X)).
adds(unstack(_,Y), clear(Y)).

/* pickup(X) */
prec(pickup(X),  [ontable(X), clear(X), handempty]).
dels(pickup(X),  ontable(X)).
dels(pickup(X),  clear(X)).
dels(pickup(_),  handempty).
adds(pickup(X),  holding(X)).

/* putdown(X) */
prec(putdown(X), [holding(X)]).
dels(putdown(X), holding(X)).
adds(putdown(X), ontable(X)).
adds(putdown(_), handempty).
adds(putdown(X), clear(X)).


% Initilize state
init_state1([clear(a),clear(b),clear(c),ontable(a),ontable(b),ontable(c),handempty]).
goal1([on(a,b),on(b,c)]).

init_state2([clear(a),clear(b),clear(c),ontable(a),ontable(b),ontable(c),handempty]).
goal2([on(b,c),on(a,b) ]).

init_state3([clear(b),clear(c),ontable(a),ontable(b),on(c,a),handempty]).
goal3([on(a,b),on(b,c)]).

init_state4([clear(a),on(a,b),on(b,c),ontable(c),handempty]).
goal4([on(a,b),on(b,c)]).

init_state5([clear(c),ontable(a),on(b,a),on(c,b),handempty]).
goal5([on(a,b),on(b,c)]).


test1 :-
	init_state1(InitState),
	goal1(Goal),
	do_it(Goal, InitState).
test2 :-
	init_state2(InitState),
	goal2(Goal),
	do_it(Goal, InitState).
test3 :-
	init_state3(InitState),
	goal3(Goal),
	do_it(Goal, InitState).
test4:-
	init_state4(InitState),
	goal4(Goal),
	do_it(Goal, InitState).
test5 :-
	init_state5(InitState),
	goal5(Goal),
	do_it(Goal, InitState).

do_it(Goal, InitState) :-
	main_strips(Goal,InitState,Plan),
	write('   from: '),write(InitState),nl,
	write('   goal: '),write(Goal),nl,
	write('     -->   '),nl,
      	reverse(Plan,RPlan),
        	writeplan(RPlan),nl.



/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
% Utility predicates
not(X) :- X, !, fail.
not(_).

writeplan([]).
writeplan([A|B]):-
  write('       '),write(A),nl,
  writeplan(B).

reverse(X,Y):- reverse(X,[],Y).
reverse([A|S], B, Sol):-
  reverse(S,[A|B],Sol).
reverse([],P,P).

% diff(A,B,R): R = A-B
diff(Xs,[], Xs).
diff(Xs, [Y|Ys], Ds) :- remove(Y, Xs, Ms), diff(Ms, Ys, Ds).

remove(_,[],[]).
remove(X, [X|Xs],Xs).
remove(Y, [X|Xs],[X|Ms]) :- remove(Y, Xs,Ms).


union(A,B,U) :- setof(X,sel2(X,A,B),U).

sel2(X,A,_) :- member(X,A).
sel2(X,_,B) :- member(X,B).









