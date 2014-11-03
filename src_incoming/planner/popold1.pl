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

:- op(1200,xfx,[<-]).
% N.B. we assume that conjunctions are represented as lists.
% `\=' is the object level not equal.
:- op(700,xfx, \=).

% nlp(CPlan, Agenda, FPlan,DB) is true if
%  CPlan is the current plan
%  Agenda is the current agenda and
%  FPlan is the final plan (with all subgoals supported)
%  and there are DB or fewer actions in the plan (i.e., DB is the depth-bound).

nlp(Plan,[],Plan,_).
nlp(CPlan,Agenda,FPlan,DB) :-
   select(Goal,Agenda,Agenda1),
   solve_goal(Goal,CPlan,NPlan,Agenda1,NAgenda,DB,NDB),
   nlp(NPlan,NAgenda,FPlan,NDB).

% select(Goal,Agenda,NewAgenda) is true if Goal is selected from Agenda with
%    NewAgenda the remaining elements.
select(G,[G|A],A). % this is only one of many selection algorithms.

% solve_goal(Goal,CPlan,NPlan,CAgenda,NAgenda,DB,DB1) chooses an action to
%    solve Goal, updating plan CPlan to NPlan and agenda CAgenda to NAgenda,
%    and updating depth bound DB to depth bound DB1

% CASE 1: use existing action
solve_goal(goal(P,A1),plan(As,Os,Ls),plan(As,NOs,[cl(N0,P,A1)|Ls]),Ag,Ag,DB,DB) :-
   member(act(N0,Act0),As),
   achieves(Act0,P),
   add_constraint(N0<A1,Os,Os1) ,
   incorporate_causal_link(cl(N0,P,A1),As,Os1,NOs).

% CASE 2: add new action. 
%   Note that DB acts as the unique index of the new action instance.
solve_goal(goal(P,A1),
           plan(As,Os,Ls),
           plan([act(DB,Act0)|As],NOs,[cl(DB,P,A1)|Ls]),Ag,NAg,DB,NDB) :-
   DB>0,
   achieves(Act0,P),
   writeln(['*** new action ',act(DB,Act0), ' to achieve ',P,' for ',A1]),
   add_constraint(DB<A1,Os,Os1),
   add_constraint(start<DB,Os1,Os2),
   add_constraint(DB<finish,Os2,Os3),
   incorporate_action(act(DB,Act0),Ls,Os3,Os4),
   incorporate_causal_link(cl(DB,P,A1),As,Os4,NOs),
   add_preconds(act(DB,Act0),Ag,NAg),
   NDB is DB-1.


% add_constraint(A0<A1,Os,Os1) adds ordering constraint A0<A1 to 
%    partial ordering Os producing partial ordering Os1.
% Fails if A0<A1 is inconsistent with Os.
% We represent parial orderings as their transitive closure.
add_constraint(C,L,L) :- member(C,L).
add_constraint(A0<A1,L1,L2) :-
   dif(A0,A1),
   \+ member(A0<A1,L1),
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
   dif(A0,A2),
   insert(A0<A2,AOs,AOs1),
   add_constraint1(A0<A1,R,AOs1,NR).
add_constraint1(A0<A1,[A2<A0|R],AOs,NR) :-
   dif(A1,A2),
   insert(A2<A1,AOs,AOs1),
   add_constraint1(A0<A1,R,AOs1,NR).
add_constraint1(A0<A1,[A2<A3|R],AOs,NR) :-
   dif(A0,A3),
   dif(A1,A2),
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
protect(cl(A0,_,_),act(NA,_),Os,Os) :-
   member(NA<A0,Os).
protect(cl(_,_,A1),act(NA,_),Os,Os) :-
   member(A1<NA,Os).
protect(cl(A0,P,A1),act(NA,A),Os,Os1) :-
   \+ member(NA<A0,Os), \+ member(A1<NA,Os),
   threatens(act(NA,A),cl(A0,P,A1)),
   enforce_order(A0,NA,A1,Os,Os1).
protect(cl(A0,P,A1),act(NA,A),Os,Os) :-
   \+ member(A<A0,Os), \+ member(A1<A,Os),
   when((ground(A),ground(P)),
        \+ threatens(act(NA,A),cl(A0,P,A1))).
enforce_order(_,A,A1,Os,Os1) :-
   add_constraint(A1<A,Os,Os1)
%,   writeln(['Putting ',A,' after ',A1])
.
enforce_order(A0,A,_,Os,Os1) :-
   add_constraint(A<A0,Os,Os1)
%,   writeln(['Putting ',A,' before ',A0])
.

threatens(act(NA,A),cl(A0,P,A1)) :-
   dif(NA,A0),
   dif(NA,A1),
   \+ \+ deletes(A,P).    
threatens(act(NA,A),cl(A0,P,A1)) :-
   dif(NA,A0),
   dif(NA,A1),
   \+ \+ achieves(A,P).

% =====================================
% interface to STRIPS notation
/* we assume now that these actions are defined via new-strips
achieves(A,P) :-
   addlist(A,AL),
   member(P,AL).
achieves(init,P) :-
   holds(P,init).
deletes(A,P) :-
   deletelist(A,AL),
   member(P,AL).
*/
add_preconds(act(N,A),Ag0,Ag1) :-
   preconditions(A,Ps),
   add_these_preconds(Ps,N,Ag0,Ag1).
add_these_preconds([],_,Ag,Ag).
add_these_preconds([P|R],A,Ag0,[goal(P,A)|Ag1]) :-
   primitive(P),
   add_these_preconds(R,A,Ag0,Ag1).
add_these_preconds([X \= Y|R],A,Ag0,Ag1) :-
   dif(X,Y),
   add_these_preconds(R,A,Ag0,Ag1).
add_these_preconds([P|R],A,Ag0,Ag2) :-
   \+ primitive(P),
   (P <- B),
   add_these_preconds(B,A,Ag0,Ag1),
   add_these_preconds(R,A,Ag1,Ag2).

solve(Goals,Plan,DB) :-
   add_these_preconds(Goals,finish,[],Ag),
   nlp(plan([act(finish,end),act(start,init)],[start<finish],[]),Ag,Plan,DB).

% seq(Plan,Seq) extracts a legal sequence Seq from Plan
seq(plan([],_,_),[]).
seq(plan(As,Os,_),[A|P]) :-
   remove(act(N,A),As,As1),
   \+ (member(act(N1,_),As1), member(N1<N,Os)),
   seq(plan(As1,Os,_),P).

% =============================================================================

% member(X,L) is true if X is a member of list L
member(X,[X|_]).
member(X,[_|L]) :-
   member(X,L).

% remove(X,L,R) is true if X is a member of list L, with remaining elements R
remove(X,[X|R],R).
remove(X,[A|L],[A|R]) :-
   remove(X,L,R).

% insert(E,L,L1) inserts E into L producing L1 
% E is not added it is already there.
insert(X,[],[X]).
insert(A,[A|R],[A|R]).
insert(A,[B|R],[B|R1]) :-
   dif(A,B),
   insert(A,R,R1).

% writeln(L) is true if L is a list of items to be written on a line, followed by a newline.
writeln(L) :- \+ \+ (numbervars(L,0,_), writelnw(L) ).
writelnw([]) :- nl.
writelnw([nl|T]) :- !,nl, writeln(T).
writelnw([H|T]) :- write(H), writeln(T).

% =============================================================================

% TRY THE FOLLOWING QUERIES with delrob_strips.pl:
% solve([carrying(rob,k1)],P,5), seq(P,S).
% solve([sitting_at(k1,lab2)],P,6), seq(P,S).
% solve([carrying(rob,parcel),sitting_at(rob,lab2)],P,8), seq(P,S).
% solve([sitting_at(rob,lab2),carrying(rob,parcel)],P,8), seq(P,S).
