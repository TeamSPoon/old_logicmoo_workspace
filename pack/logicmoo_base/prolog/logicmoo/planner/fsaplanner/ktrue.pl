%
% FILE: ktrue.pl
%
% Prolog code for formulas in the situation calculus with possible values
% See "A feasible approach to disjunctive knowledge in the situation calculus"
%   by Stavros Vassos, Sebastian Sardina, and Hector Levesque for background
%
:- write('*** Loading ktrue by Hector Levesque (c) 2004, version 1.0'), nl.

:- getenv('OOA2_INCLUDEFILE',X),absolute_file_name(X,Y,[relative_to('/')]),include(Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                     December 15, 2004
%
% This software was developed by Hector Levesque from Sept-Dec 04.
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
%    -prim_fluent(F), declares F as a primitive fluent
%    -prim_action(A,RL), declares A as a primitive action with RL as 
%        a list of possible sensing results
%    -poss(A,C) declares C is the precondition for action A
%    -causes(A,R,F,V,C), when A occurs with sensing result R,
%        then F is caused to have the possible values of each V 
%        for which C is possibly true
%    -settles(A,R,F,V,C), when A occurs with sensing result R,
%        and C is known, then F is known to have value V
%    -rejects(A,R,F,V,C), when A occurs with sensing result R,
%        and C is known, then F is known not to have value V
%    -init(F,V), V is a possible initial value for F.
%
%  The top level predicates exported are kTrue(C,H) and mTrue(C,H)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic prim_fluent/1, prim_action/2, poss/2,
     init/2, causes/4, causes/5, settles/5, rejects/5.

% As a convenience, causes/4 expands to causes/5
causes(A,_,F,V,C) :- causes(A,F,V,C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  The top level predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ktrue(C,H) condition C is known true in H
kTrue(C,H) :- mTrue(C,H), !, \+ mTrue(neg(C),H).

% mtrue(C,H) condition C is possibly true in H according to what is known
mTrue(true,_) :- !.
mTrue(and(P1,P2),H) :- !, mTrue(P1,H), mTrue(P2,H).
mTrue(or(P1,P2),H) :- !, (mTrue(P1,H) ; mTrue(P2,H)).
mTrue(some(V,P,Q),H) :- subv(V,N,P,P1), subv(V,N,Q,Q1), !, 
    call(P1), mTrue(Q1,H).
mTrue(all(V,P,Q),H) :- subv(V,N,P,P1), subv(V,N,Q,Q1), !, 
    \+ (call(P1), mTrue(neg(Q1),H)).

mTrue(neg(true),_) :- !, fail.
mTrue(neg(neg(P)),H) :- !, mTrue(P,H).
mTrue(neg(and(P1,P2)),H) :- !, mTrue(or(neg(P1),neg(P2)),H).
mTrue(neg(or(P1,P2)),H) :- !,  mTrue(and(neg(P1),neg(P2)),H).
mTrue(neg(some(V,P,Q)),H) :- !, mTrue(all(V,P,neg(Q)),H).
mTrue(neg(all(V,P,Q)),H) :- !, mTrue(some(V,P,neg(Q)),H).

mTrue(neg(P),H) :- !, subf(P,P1,H), \+ call(P1).  /* fall to Prolog */
mTrue(P,H) :- subf(P,P1,H), call(P1).   /* last case: fall to Prolog */

% T2 is T1 with X1 replaced by X2 
subv(_,_,T1,T2) :- var(T1), !, T2 = T1.
subv(X1,X2,T1,T2) :- T1 = X1, !, T2 = X2.
subv(X1,X2,T1,T2) :- T1 =..[F|L1], subvl(X1,X2,L1,L2), !, T2 =..[F|L2].
subvl(_,_,[],[]).
subvl(X1,X2,[T1|L1],[T2|L2]) :- subv(X1,X2,T1,T2), !, subvl(X1,X2,L1,L2).

% P2 is P1 with all fluents replaced by a possible value at H 
subf(P1,P2,_) :- var(P1), !, P2 = P1.
subf(now,H,H) :- !.   /* now is pseudo fluent meaning current situation */
subf(m(F),L,H) :- !, setof(V1,mval(F,V1,H),L).     /* all possible vals */
subf(i(F),V,_) :- !, init(F,V).        /* the initial value of fluent F */
subf(P1,P2,H) :- prim_fluent(P1) -> mval(P1,P2,H) ;
    (P1=..[F|L1], subfl(L1,L2,H), P2=..[F|L2]).
subfl([],[],_).
subfl([T1|L1],[T2|L2],H) :- subf(T1,T2,H), subfl(L1,L2,H).

% apply the causal and sensing facts to get the value of a fluent
mval(F,V,[]) :- init(F,V).
mval(F,V,[o(A,R)|H]) :- 
    causes(A,R,F,_,_) -> (causes(A,R,F,V,C), mTrue(C,H)) ;
     (settles(A,R,F,V1,C), kTrue(C,H)) ->  V=V1 ;
       (mval(F,V,H), \+ (rejects(A,R,F,V,C), kTrue(C,H))).

% failure
false :- fail.
