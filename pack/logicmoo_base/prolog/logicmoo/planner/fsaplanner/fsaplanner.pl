%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: fsaplanner.pl
%
% The file ktrue.pl is loaded to do formula evaluation
%
% Prolog code for an iterative planner.
% See "Planning with Loops" by Hector Levesque for background
%
:- write('*** Loading fsaplanner by Hector Levesque and Yuxiao Hu (c) 2008'),
   nl.
:- style_check(-discontiguous).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                     September 15, 2008
%
%  This software was developed by Hector Levesque and Yuxiao Hu in 2008,
%      based on KPLANNER by Hector Levesque in 2004-2008.
%
%        Do not distribute without permission.
%        Include this notice in any copy made.
% 
%        Copyright (c) 2004 by The University of Toronto,
%                              Toronto, Ontario, Canada.
% 
%                     All Rights Reserved
% 
% Permission to use, copy, and modify, this software and its
% documentation for non-commercial research purpose is hereby granted
% without fee, provided that the above copyright notice appears in all
% copies and that both the copyright notice and this permission notice
% appear in supporting documentation, and that the name of The University
% of Toronto not be used in advertising or publicity pertaining to
% distribution of the software without specific, written prior
% permission.  The University of Toronto makes no representations about
% the suitability of this software for any purpose.  It is provided "as
% is" without express or implied warranty.
% 
% THE UNIVERSITY OF TORONTO DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
% SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
% FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TORONTO BE LIABLE FOR ANY
% SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
% RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
% CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  The following predicates need to be defined in a separate file:
%
%  For formula evaluation, we need a basic action theory
%    -prim_fluent(F):     declares F as a primitive fluent
%    -prim_action(A,RL):  declares A as a primitive action with RL as 
%                         a list of possible sensing results
%    -poss(A,C):          declares C is the precondition for action A
%    -causes(A,R,F,V,C):  when A occurs with sensing result R,
%                         then F is caused to have the possible values
%                         of each V for which C is possibly true
%    -settles(A,R,F,V,C): when A occurs with sensing result R,
%                         and C is known, then F is known to have value V
%    -rejects(A,R,F,V,C): when A occurs with sensing result R and C is known,
%                         then F is known not to have value V
%    -init(F,V):          V is a possible initial value for F.
%
%  For problems requiring a planning parameter, we also need:
%    -parm_fluent(F):     F is the planning parameter
%    -init_parm(W,F,V):   where W is 'generate' or 'test', V is a
%                         possible initial value for the fluent F
%
%  There are some directives that can be used to prune the search:
%    -good_action(A,C):   A is acceptable when C is possibly true
%    -good_state(N,C):    current state is acceptable, 
%                         if N actions remain and C is possibly true
%    -filter_useless:     do not consider actions that are no-ops in plans
%    -filter_beyond_goal: no actions after goal has been achieved
%    -gen_max(N):         in generating plans only search
%                         for plans to depth N
%    -test_max(N):        with parameters, only test plans to depth N
%    -max_state(N):       the maximum number of states in the solution
%
%  The top level predicate exported are
%    -kplan(G):           plan for goal G
%    -print:              print the plan as a table of transitions
%    -save(F):            save the graphical representation of the plan
%                         to dot file F.dot and postscript file F.ps
%    -draw:               draw the plan
%    -run(N):             run and print the trace of the plan
%                         with planning parameter set to N
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- include(ktrue).

:- dynamic parm_fluent/1, init_parm/3, searchBeyondGoal/0,
     goodAct/2, goodState/2, testMax/1, genMax/1, trivAct/2, maxState/1,
     thePlan/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  The top level predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kplan(Goal) :- 
   write('The goal:  '), write(Goal), nl, nl, Go is cputime, kp(Goal,Plan),  
   Diff is cputime-Go, nl, nl, write('A plan is found after '),
   write(Diff), write(' seconds.'), nl,
   assert(thePlan(Plan)).

kp(Goal,Plan) :- parm_fluent(_) -> gtplan(Goal,Plan) ; gplan(Goal,Plan).

print :-
  thePlan(Plan),!,pp(Plan),nl;
  write('No plan exists!'),nl,fail.

save(File) :-
  thePlan(Plan),!,
    swritef(S1,'%w.dot',[File]), pphdot(S1,Plan),
    swritef(S2,'dot -Tps %w.dot -o %w.ps',[File,File]),
    shell(S2) ;
  write('No plan exists!'),nl,fail.

draw :- save(tmp),shell('gv tmp.ps').

run :-
  parm_fluent(_),!,write('Please initialize plan parameter by calling "run(N)."'),fail;
  thePlan(Plan),!,runplan(Plan);
  write('No plan exists!'),nl,fail.

run(N) :- % Run the plan with parameter N
  \+parm_fluent(_),!,write('Please run without plan parameter by calling "run."'),fail;
  thePlan(Plan),!,iniReset,parm_fluent(F),assert(init(F,N)),runplan(Plan);
  write('No plan exists!'),nl,fail.

gtplan(Goal,Plan) :-
  genSmall(Goal,Plan), % nl, pp(Plan),
  tstLarge(Goal,Plan), iniReset.
gtplan(_,_) :- iniReset, fail.

genSmall(Goal,Plan) :- iniSet(generate), gplan(Goal,Plan).
genSmall(_,_) :- nl, write('No more plans to generate'), nl, fail.

tstLarge(Goal,Plan) :- iniSet(test), tplan(Goal,Plan). 
tstLarge(_,_) :- iniSet(generate), ttyecho('x'), fail.

iniSet(W) :- iniReset, parm_fluent(F), 
   ((init_parm(W,F,V), assert(init(F,V)), fail) ; true).

iniReset :- parm_fluent(F), retractall(init(F,_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  The plan tester
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tplan(Goal,Plan) :-
    \+ member(trans(0,_,_,_),Plan), % 0 is the final state. No out edge.
    testMax(Max), tp(Goal,Max,[],Plan,1), !.

% tp(G,N,H,P,S) execute P starting in H with S as the current state
% and terminate in a state of depth N at most and where G is known to hold

tp(G,_,H,_,0) :- kTrue(G,H).        % Goal must be satisfied in final state.
tp(G,N,H,P,S) :-
    N>0, N1 is N-1, member(trans(S,A,_,_),P),
    legalAct(A,RL,H), !, tpAll(G,N1,H,A,RL,P,S).

tpAll(_,_,_,_,[],_,_).
tpAll(G,N,H,A,[R|RL],P,S) :-
  impSense(A,R,H), !, tpAll(G,N,H,A,RL,P,S).
tpAll(G,N,H,A,[R|RL],P,S) :-
  member(trans(S,A,R,S1),P), !,
  tp(G,N,[o(A,R)|H],P,S1), !, tpAll(G,N,H,A,RL,P,S).

% A is primitive action whose precondition is known to be true
legalAct(A,RL,H) :- prim_action(A,RL), poss(A,C), kTrue(C,H).

% the sensing value R for action A is impossible in history H
impSense(A,R,H) :- settles(A,R,F,V,C), kTrue(C,H), \+ mval(F,V,H). 
impSense(A,R,H) :- rejects(A,R,F,_,_), \+ mval(F,_,[o(A,R)|H]). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  The plan generator
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gplan(Goal,Plan) :- genMax(Max), idplan(Goal,0,Max,Plan,1,_).

% idplan(G,N,M,P,C,NS)
%    find a plan P that has NS states by iterative deeping from N to M,
%    such that starting from State C, P reaches goal G.
%    State 1 is always the start state, and 0 is the (unique) final state

idplan(G,N,_,P,C,NS) :- ttyecho(' '), ttyecho(N), dfplan(G,N,[],[],P,C,2,NS).
idplan(G,N,M,P,C,NS) :- N < M, N1 is N+1, idplan(G,N1,M,P,C,NS).

% find a plan no deeper than N for achieving the goal starting in history H
% dfplan(G,N,H,P0,P1,C,NS0,NS1)
%    Starting from the current plan P0 that has NS0 states,
%    the current history H, and the current state C,
%    find, within N steps, a plan P1 that has NS1 states and achieves goal G

dfplan(Goal,_,H,P,P,0,NS,NS) :- 
    !, kTrue(Goal,H), (searchBeyondGoal -> true ; !).
dfplan(Goal,N,H,P0,P1,C,NS0,NS1) :-
    N > 0, N1 is N-1, okState(N,H), (member(trans(C,A,_,_),P0) ; true), !,
    okAct(A,RL,H), tryActs(A,RL,Goal,N1,H,P0,P1,C,NS0,NS1).

tryActs(_,[],_,_,_,P,P,_,NS,NS).
tryActs(A,[R|RL],Goal,N,H,P0,P1,C,NS0,NS1) :-
    tryAct(A,R,Goal,N,H,P0,P,C,NS0,NS), tryActs(A,RL,Goal,N,H,P,P1,C,NS,NS1).

tryAct(A,R,Goal,N,H,P0,P1,C,NS0,NS1) :-
    (impSense(A,R,H), !, P0=P1, NS0=NS1) ;   % no trans needed
    (member(trans(C,A,R,C1),P0), !,	     % follow existing trans to C1
       dfplan(Goal,N,[o(A,R)|H],P0,P1,C1,NS0,NS1)) ;
    (NS is NS0-1, between(0,NS,S),           % new trans to old state S
       dfplan(Goal,N,[o(A,R)|H],[trans(C,A,R,S)|P0],P1,S,NS0,NS1)) ;
    (maxState(MS), NS0<MS, NS is NS0+1,      % new trans to new state NS0
       dfplan(Goal,N,[o(A,R)|H],[trans(C,A,R,NS0)|P0],P1,NS0,NS,NS1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Filtering of trivial states, actions, plans
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% H is an acceptable state given that N actions are left
okState(N,H) :- goodState(N,C) -> mTrue(C,H) ; true.

% A is a acceptable action in H: legal, \+ filtered, \+ trivial
okAct(A,RL,H) :- legalAct(A,RL,H),
  (goodAct(A,C1) -> mTrue(C1,H) ; true), \+ trivAct(A,H).

% A is trivial if it is a sensing action that was just performed.
trivAct(A,[o(A,R)|_]) :- prim_action(A,[_,_|_]), \+ causes(A,R,_,_,_).

% A is useless if it has no effect and anything it senses is known
uselessAct(A,H) :- \+ causes(A,_,_,_,_), 
  \+ (settles(A,_,F,_,_), \+ kTrue(F=_,H)), 
  \+ (rejects(A,_,F,_,_), \+ kTrue(F=_,H)).

testMax(400).  /* default max depth of program when testing large */
genMax(20).    /* default max depth of program when generating small */
searchBeyondGoal.  /* consider non-empty plans when goal is achieved */
maxState(10).  /* default max number of states */

% user directives for setting up the above filters

good_action(A,C) :- assert(goodAct(A,C)).
good_state(N,C) :- assert(goodState(N,C)).
filter_useless :- assert(trivAct(A,H) :- uselessAct(A,H)).
filter_beyond_goal :- retractall(searchBeyondGoal).
test_max(N) :- retractall(testMax(_)), assert(testMax(N)).
gen_max(N) :- retractall(genMax(_)), assert(genMax(N)).
max_state(N) :- retractall(maxState(_)), assert(maxState(N)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  The plan pretty printer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ttyecho(C) :- write(C), ttyflush.

ptab(0).
ptab(N) :- N>0, N1 is N-1, write(' '), ptab(N1).

ntab(N) :- N>99;N=<0.
ntab(N) :- write('0'),N1 is 10*N,ntab(N1).

pp(P) :- format('~`-t~78|~n'),sort(P,Q),pp1(Q),!.

pp1([]).
pp1([trans(S,A,R,S1)|TL]) :-
    'format'('  State~t~d~10|  ~`-t ~w ~40|: ~w ~`-t~65|>  State~t~d~76|~n',[S,A,R,S1]),
    pp1(TL).

ppdot(FN,P) :-
    open(FN,write,S),
    write(S,'digraph{'),nl(S),
    write(S,'  0 [peripheries=2]'),nl(S),
    ppdotl(S,P),
    write(S,'}'),nl(S),
    close(S).

ppdotl(_,[]).
ppdotl(Stream,[trans(S,A,R,S1)|PL]) :-
   write(Stream,' '), write(Stream,S), write(Stream,' -> '), write(Stream,S1),
   write(Stream,' [label="'), write(Stream,A), write(Stream,':'),
   write(Stream,R), write(Stream,'"]'), nl(Stream),
   ppdotl(Stream,PL).

pphdot(FN,P) :-
   open(FN,write,S), write(S,'digraph{'),nl(S),
     write(S,'  0 [label="STOP",peripheries=2]'),nl(S),
     write(S,'  GO [label="",height=0,width=0,peripheries=0]'), nl(S),
     write(S,'  GO -> 1'), nl(S),
     nodelist(P,NL), ppnodelist(S,NL), ppedges(S,P), 
   write(S,'}'),nl(S), close(S).

ppnodelist(_,[]).
ppnodelist(Stream,[[State,Action]|NL]) :-
  write(Stream,'  '), write(Stream,State),   write(Stream,' [label="'),
  write(Stream,Action), write(Stream,'"]'), nl(Stream),
  ppnodelist(Stream,NL).

nodelist([],[]).
nodelist([trans(S,A,_,_)|P],NL) :-
   member(trans(S,A,_,_),P), !, nodelist(P,NL).
nodelist([trans(S,A,_,_)|P],[[S,A]|NL]) :- nodelist(P,NL).

ppedges(_,[]).
ppedges(Stream,[trans(S,A,R,S1)|P]) :-
  write(Stream,'  '), write(Stream,S), write(Stream,' -> '), write(Stream,S1),
  ( prim_action(A,[R]) ;
    (write(Stream,' [label="'), write(Stream,R), write(Stream,'"]')) ),
  nl(Stream), ppedges(Stream,P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  The plan tester as above but with printing (for debugging only)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trplan(Goal,Plan) :- testMax(Max), trp(0,Goal,Max,[],Plan,1), !.

trp(_,G,_,H,_,0) :- kTrue(G,H), write(' DONE').
trp(M,G,N,H,P,S) :- 
   member(trans(S,A,_,_),P), !,
   nl, ptab(M), write('State: '),write(S),write(', Action: '),write(A),
   write(' ::'),
   M1 is M+2, N > 0, 
   N1 is N-1, okState(N,H), okAct(A,RL,H), !, write(' -- OK'), 
   trpAll(M1,G,N1,H,A,RL,P,S).

trpAll(_,_,_,_,_,[],_,_).
trpAll(M,G,N,H,A,[R|RL],P,S) :- 
  impSense(A,R,H), !,  nl, ptab(M), write(R), write(': CANT HAPPEN'),
  trpAll(M,G,N,H,A,RL,P,S).
trpAll(M,G,N,H,A,[R|RL],P,S) :-
  member(trans(S,A,R,S1),P), !,
  nl, ptab(M), write(R), write(' --> State '),write(S1),write(':'),
  M1 is M+1, trp(M1,G,N,[o(A,R)|H],P,S1), !, trpAll(M,G,N,H,A,RL,P,S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  The plan executer for printing a trace of the plan execution
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

runplan(P) :- rp(0,1,[],[],P,1),!.

rp(M,N,B,_,_,0) :- pbtab(M,B),write(' '),ntab(N),write(N),write('. '),
  write('DONE'),nl.
rp(M,N,B,H,P,S) :-
  member(trans(S,A,_,_),P),!,
  prim_action(A,RL),filter(A,RL,H,L),!,
  (
   L=[R]->rpOne(M,N,B,H,A,R,P,S,' ');
   M1 is M+3,rpAll(M1,N,[M1|B],H,A,L,P,S)
  ).

rpOne(M,N,B,H,A,R,P,S,T) :-
  member(trans(S,A,R,S1),P),!,
  (
   pbtab(M,B),write(T),ntab(N),write(N),write('. '),write(A),write(':'),
   write(R),nl,N1 is N+1
  ),
  rp(M,N1,B,[o(A,R)|H],P,S1).

rpAll(_,_,_,_,_,[],_,_).
rpAll(M,N,B,H,A,[R|RL],P,S) :-
  (RL=[]->B=[_|B1];B=B1),
  rpOne(M,N,B1,H,A,R,P,S,'\\'),!,rpAll(M,N,B1,H,A,RL,P,S).

filter(_,[],_,[]).
filter(A,[R|RL],H,L) :-
  impSense(A,R,H),!,filter(A,RL,H,L);
  filter(A,RL,H,L1),L=[R|L1].

pbtab(M,B) :- pbtab(M,B,0).

pbtab(M,_,M).
pbtab(M,[],N) :- N1 is N+1,write(' '),pbtab(M,[],N1).
pbtab(M,BL,N) :- N1 is N+1,reverse(BL,[B|BR]),!,
  (
   (B=N1)->(write('|'),reverse(BR,BR1),pbtab(M,BR1,N1));
   (write(' '),pbtab(M,BL,N1))
  ).
  
