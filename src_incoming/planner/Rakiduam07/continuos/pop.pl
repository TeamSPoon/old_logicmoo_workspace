:- module(pop,[solve/3,seq/2,solve_open_preconditions/3,add_these_preconds/4],[]).
%:- use_module(delrob_strips,['<-'/2,achieves/2,preconditions/2,deletes/2,holds/2,primitive/1]).
%:- use_module(blocks,['<-'/2,achieves/2,preconditions/2,deletes/2,primitive/1]).
:- use_module('../field_grid',[neighbor/2]).
:- use_module(soccer_strips,['<-'/2,'<<'/2,achieves/2,preconditions/2,deletes/2,primitive/1]).
:- use_module(library(write),[numbervars/3,write/1]).
:- use_module(library(when)).
% Computational Intelligence: a logical approach. 
% Prolog Code.
% PARTIAL ORDER PLANNER using STRIPS REPRESENTATION (Figures 8.5 & 8.6)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% Definitions:
% An agenda is a list of goals.
% A goal is of the form goal(P,NA) where P is a proposition 
%   that is a precondition of action instance index NA.
% A plan is of the form plan(As,Os,Ls) where 
%   As is a list of actions instances,
%      An action instance is of the form act(N,A) where N is an integer 
%      and A is an action (this is needed in case there are multiple 
%      instances of the same action). N is the action instance index.
%   Os is a list of ordering constraints (A1<A2) where A1 & A2 
%      are action instance indexes.
%   Ls is a list of causal links.
% A causal link is of the form cl(A0,P,A1) where A0 and A1 are action 
%   instance indexes, and P is a proposition that is a precondition of 
%   action A1 means that A0 is making P true for A1.

:- op(1200,xfx,[<-,<<]).
% N.B. we assume that conjunctions are represented as lists.
% `\=' is the object level not equal.
:- op(700,xfx, \=).

% pop(CPlan, Agenda,DIC, FPlan,DB) is true if
%  CPlan is the current plan
%  Agenda is the current agenda 
%  DIC is a list of delayed inequality constraints
%  FPlan is the final plan (with all subgoals supported)
%  and there are DB or fewer actions in the plan (i.e., DB is the depth-bound).
:- data index/1.

index(1).

getindex(I):-
	current_fact(index(I)),
	I1 is I + 1,
	set_fact(index(I1)).


solve_open_preconditions(Plan,Plan,_):-
   agenda(Plan,[]),
   inequality_constraints(Plan,DIC),
   all_constraints_satisfied(DIC).

solve_open_preconditions(CPlan,FPlan,DB) :-
   agenda(CPlan,Agenda),
   %inequality_constraints(CPlan,DIC0),
   selectgoal(Goal,Agenda,Agenda1),
   ( 
	Goal == nogoal ,
	inequality_constraints(CPlan,DIC),
	all_constraints_satisfied(DIC),
	set_new_agenda(CPlan,Agenda1,FPlan)
   ;
	set_new_agenda(CPlan,Agenda1,PlanAux),
	solve_goal(Goal,PlanAux,NPlan,DB,NDB),
	solve_open_preconditions(NPlan,FPlan,NDB)
   ).


pop(Plan,Plan,_):-
   agenda(Plan,[]),
   inequality_constraints(Plan,DIC),
   all_constraints_satisfied(DIC).

pop(CPlan,FPlan,DB) :-
   agenda(CPlan,Agenda),
   %inequality_constraints(CPlan,DIC0),

   selectgoal(Goal,Agenda,Agenda1),
   ( 
	Goal == nogoal ,
	inequality_constraints(CPlan,DIC),
	all_constraints_satisfied(DIC),
	set_new_agenda(CPlan,Agenda1,FPlan)
   ;
	set_new_agenda(CPlan,Agenda1,PlanAux),
	solve_goal(Goal,PlanAux,NPlan,DB,NDB),
	pop(NPlan,FPlan,NDB)
   ).

agenda(plan(_,_,_,Agenda,_),Agenda).
inequality_constraints(plan(_,_,_,_,DIC),DIC).

% set_new_agenda agrega NewAgenda  al plan descartando la anterior
set_new_agenda(plan(As,Os,Ls,_,DIC),NewAgenda,plan(As,Os,Ls,NewAgenda,DIC)).

% set_inequality_constraints agrega NDIC al plan descartando el anterior
set_inequality_constraints(plan(As,Os,Ls,Ag,_),NDIC,plan(As,Os,Ls,Ag,NDIC)).

% select(Goal,Agenda,NewAgenda) is true if Goal is selected from Agenda with
%    NewAgenda the remaining elements.

% select(G,[G|A],A).

selectgoal(nogoal,[],[]).
selectgoal(goal(G,GG),[goal(G,GG)|A],A) :-
   primitive(G),!.
selectgoal(goal((X \= Y),GG),[goal((X\=Y),GG)|A],A) :- !.

selectgoal(P,[goal(G,_GG)|R],A) :-
   (G << B),
   verifystatic(B),
   selectgoal(P,R,A).

selectgoal(P,[goal(G,GG)|R],A) :-
   (G <- B),
   makeintogoal(B,GG,R,NR),
   selectgoal(P,NR,A).

verifystatic([]). 
verifystatic([H|T]) :-
	H,
	verifystatic(T).
	


makeintogoal([],_,R,R).
makeintogoal([H|T],GG,R,[goal(H,GG)|NR]) :-
   makeintogoal(T,GG,R,NR).

% solve_goal(Goal,CPl,NPl,CAg,NAg,DB,DB1,DIC0,DIC1) 
%    chooses an action to
%    solve Goal, updating plan CPl to NPl and agenda CAg to NAg,
%    and updating depth bound DB to depth bound DB1, and
%    updating the delayed inequality constraints from DIC0 to DIC1

% CASE 0: inequality goal
solve_goal(goal((X \= Y),_),plan(As,Os,Ls,Ag,DIC0),plan(As,Os,Ls,Ag,DIC1),DB,DB) :-
    !,
%    inequality_constraints(Plan,DIC0),
    add_inequality_constraint((X\= Y),DIC0,DIC1).
%    set_inequality_constraints(Plan,DIC1,NPlan).

% CASE 1: use existing action
solve_goal(goal(P,A1),plan(As,Os,Ls,Ag,DIC0),plan(As,NOs,[cl(N0,P,A1)|Ls],Ag,DIC1),DB,DB) :-
   member(act(N0,Act0),As),
   achieves(Act0,P),
   add_constraint(N0<A1,Os,Os1) ,
   incorporate_causal_link(cl(N0,P,A1),As,Os1,NOs),
   check_inequality_constraints(DIC0,DIC1).

% CASE 2: add new action. 
%   Note that DB acts as the unique index of the new action instance.
solve_goal(goal(P,A1),
           plan(As,Os,Ls,Ag,DIC0),
           plan([act(Ni,Act0)|As],NOs,[cl(Ni,P,A1)|Ls],NAg,DIC1),DB,NDB) :-
   DB>0,
   achieves(Act0,P),
   getindex(Ni),
%   writeln(['*** new action ',act(DB,Act0), ' to achieve ',P,' for ',A1]),
   add_constraint(Ni<A1,Os,Os1),
   add_constraint(start<Ni,Os1,Os2),
   incorporate_action(act(Ni,Act0),Ls,Os2,Os3),
   incorporate_causal_link(cl(Ni,P,A1),As,Os3,NOs),
   add_preconds(act(Ni,Act0),Ag,NAg),
   NDB is DB-1,
   check_inequality_constraints(DIC0,DIC1).



% add_constraint(A0<A1,Os,Os1) adds ordering constraint A0<A1 to 
%    partial ordering Os producing partial ordering Os1.
% Fails if A0<A1 is inconsistent with Os.
% We represent parial orderings as their transitive closure.
add_constraint(C,L,L) :- member(C,L),!.      % green cut for efficiency only
add_constraint(A0<A1,L1,L2) :-
   different(A0,A1),
%   \+ member(A0<A1,L1),    % ommitted because of the cut for efficiency.
   \+ member(A1<A0,L1),
%   writeln([' ... adding constraint ',A0<A1]),
   add_constraint1(A0<A1,L1,L1,L2).

% add_constraint1(A0<A1,Os,AOs,Os1) adds ordering constraint A0<A1 
%    partial ordering Os is the remaining orderings to be checked
%    AOs is the list of all orderings
%    Os1 is the final set of orederings
add_constraint1(A0<A1,[],AOs,NOs) :-
    insert(A0<A1,AOs,NOs).

add_constraint1(A0<A1,[A1<A2|R],AOs,NR) :-
   different(A0,A2),
   insert(A0<A2,AOs,AOs1),
   add_constraint1(A0<A1,R,AOs1,NR).
add_constraint1(A0<A1,[A2<A0|R],AOs,NR) :-
   different(A1,A2),
   insert(A2<A1,AOs,AOs1),
   add_constraint1(A0<A1,R,AOs1,NR).
add_constraint1(A0<A1,[A2<A3|R],AOs,NR) :-
   different(A0,A3),
   different(A1,A2),
   add_constraint1(A0<A1,R,AOs,NR).

% incorporate_causal_link(CL, As, Os, NOs)
%  incororates causal link CL to links Ls producing new links NLs, and updating
%  parial ordering Os to NOs. RAs is the list of remaining actions.
%  As is the list of all actions.
incorporate_causal_link(CL, As, Os, NOs) :-
%   writeln(['Adding causal link: ',CL,nl,'  As=',As,nl,'  Os=',Os]),
   incorporate_causal_link1(CL, As,Os,NOs)
%,   writeln([' Added causal link: ',CL,nl,'  As=',As,nl,'  Os=',NOs])
.
incorporate_causal_link1(_,[],Os,Os).
incorporate_causal_link1(CL,[A|RAs],Os,NOs) :-
   protect(CL,A,Os,Os1),
   incorporate_causal_link1(CL,RAs,Os1,NOs).

incorporate_action(_,[],Os,Os).
incorporate_action(A,[CL|Ls],Os,NOs) :-
   protect(CL,A,Os,Os1),
   incorporate_action(A,Ls,Os1,NOs).

% protect(Cl,Action,Os0,Os1) protects causal link CL from Action if necessary
protect(cl(A0,_,_),act(A0,_),Os,Os) :- !.
protect(cl(A0,_,_),act(NA,_),Os,Os) :-
   member(NA<A0,Os),!.
protect(cl(_,_,A1),act(A1,_),Os,Os) :- !.
protect(cl(_,_,A1),act(NA,_),Os,Os) :-
   member(A1<NA,Os), !.
protect(cl(_,P,_),act(_,A),Os,Os) :-
%  different(NA,A0), \+ member(NA<A0,Os), different(NA,A1), \+ member(A1<NA,Os), 
     % deleted because of cuts
%   when(ground((A,P)), \+ deletes(A,P)).
    \+ deletes(A,P).
protect(cl(A0,P,A1),act(NA,A),Os,Os1) :-
%  different(NA,A0), \+ member(NA<A0,Os), different(NA,A1), \+ member(A1<NA,Os), 
     % deleted because of cuts
   deletes(A,P),
   enforce_order(A0,NA,A1,Os,Os1).

enforce_order(_,A,A1,Os,Os1) :-
   add_constraint(A1<A,Os,Os1)
%,   writeln(['Putting ',A,' after ',A1])
.
enforce_order(A0,A,_,Os,Os1) :-
   add_constraint(A<A0,Os,Os1)
%,   writeln(['Putting ',A,' before ',A0])
.

consistent(A0<A1,Os) :-
   different(A0,A1),
   \+ member(A1<A0,Os).

% =====================================
% interface to STRIPS notation
% /* we assume now that these actions are defined via new-strips
% achieves(A,P) :-
%    addlist(A,AL),
%    member(P,AL).
% achieves(init,P) :-
%    holds(P,init).
% deletes(A,P) :-
%    deletelist(A,AL),
%    member(P,AL).
% */

add_preconds(act(N,A),Ag0,Ag1) :-
   preconditions(A,Ps),
%   add_these_preconds(Ps,N,Ag0,Ag1), % makes the agenda a stack
   add_these_preconds(Ps,N,[],NPs),
   append(Ag0,NPs,Ag1).              % makes the agenda a queue
add_these_preconds([],_,Ag,Ag).
%add_these_preconds([X \= Y|R],A,Ag0,Ag1) :- !,
%   different(X,Y),
%   add_these_preconds(R,A,Ag0,Ag1).
add_these_preconds([P|R],A,Ag0,[goal(P,A)|Ag1]) :-
%   primitive(P),
   add_these_preconds(R,A,Ag0,Ag1).
%add_these_preconds([P|R],A,Ag0,Ag2) :-
%   \+ primitive(P),
%   (P <- B),
%   add_these_preconds(B,A,Ag0,Ag1),
%   add_these_preconds(R,A,Ag1,Ag2).

solve(Goals,Plan,DB) :-
   %agrega las metas como precondiciones de finish
   add_these_preconds(Goals,finish,[],Ag),
   %ejecuta pop
   pop(plan([act(finish,end),act(start,init)],[start<finish],[],Ag,[]),Plan,DB).

% seq(Plan,Seq) extracts a legal sequence Seq from Plan
seq(plan([],_,_,_,_),[]).
seq(plan(As,Os,_,_,_),[A|P]) :-
   remove(act(N,A),As,As1),
   \+ (member(act(N1,_),As1), member(N1<N,Os)),
   seq(plan(As1,Os,_,_,_),P).

% =============================================================================
%   INEQUALITY CONSTRAINTS

% check_inequality_constraints(DIC0,DIC1).
check_inequality_constraints([],[]).
check_inequality_constraints([(X \= Y)|DIC0],DIC1) :-
   add_inequality_constraint((X\= Y),DIC0,DIC1).

% add_inequality_constraint((X\= Y),DIC0,DIC1).
add_inequality_constraint((X\= Y),_,_) :-
   (X==Y),!,fail.
add_inequality_constraint((X \= Y),DIC0,DIC1) :-
   \+(X=Y),!,
   check_inequality_constraints(DIC0,DIC1).
add_inequality_constraint((X \= Y),DIC0,[(X\= Y)|DIC1]) :-
   check_inequality_constraints(DIC0,DIC1).

% all_constraints_satisfied(DIC)
all_constraints_satisfied([]).
all_constraints_satisfied([(X \= Y)|DIC]):-
    X \== Y,
    all_constraints_satisfied(DIC).
% =============================================================================

different(X,Y) :-
   X \== Y.

append([],L,L).
append([H|X],Y,[H|Z]) :-
   append(X,Y,Z).

% % member(X,L) is true if X is a member of list L
% member(X,[X|_]).
% member(X,[_|L]) :-
%    member(X,L).

% remove(X,L,R) is true if X is a member of list L, with remaining elements R
remove(X,[X|R],R).
remove(X,[A|L],[A|R]) :-
   remove(X,L,R).

% insert(E,L,L1) inserts E into L producing L1 
% E is not added it is already there.
insert(X,[],[X]).
insert(A,[A|R],[A|R]).
insert(A,[B|R],[B|R1]) :-
   different(A,B),
   insert(A,R,R1).

% writeln(L) is true if L is a list of items to be written on a line, followed by a newline.
writeln(L) :- \+ \+ (numbervars(L,0,_), writelnw(L) ).
writelnw([]) :- nl.
writelnw([nl|T]) :- !,nl, writeln(T).
writelnw([H|T]) :- write(H), writeln(T).

% =============================================================================



% TRY THE FOLLOWING QUERIES with delrob_strips.pl:
% solve([carrying(rob,k1)],P,3), seq(P,S).
% solve([sitting_at(k1,lab2)],P,7), seq(P,S).
% solve([carrying(rob,parcel),sitting_at(rob,lab2)],P,9), seq(P,S).
% solve([sitting_at(rob,lab2),carrying(rob,parcel)],P,9), seq(P,S).

% BLOCKS WORLD FOR PARTIAL ORDER PLANNER
% solve([on(a,f)],P,2), seq(P,S).
% solve([on(b,f)],P,4), seq(P,S).
% solve([on(c,d)],P,8), seq(P,S).
% solve([on(b,f),on(f,a)],P,6), seq(P,S). % "sussman's anomoly"
% solve([on(a,d),on(f,b)],P,6), seq(P,S). % "block swapping"
% solve([on(b,table),on(f,e)],P,6), seq(P,S). % independent problems

% For soccer
% solve([waiting_at(ball,oppGoal)],P,7), seq(P,S).
% solve([waiting_at(kula,cell(3,1))],P,2), seq(P,S).

% La siguiente consulta no esta andando:
% solve([waiting_at(kula,cell(2,3))],P,2), seq(P,S).


