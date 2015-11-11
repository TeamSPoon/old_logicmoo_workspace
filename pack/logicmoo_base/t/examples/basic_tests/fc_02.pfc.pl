/* <module>
% =============================================
% File 'mpred_builtin.pfc'
% Purpose: Agent Reactivity for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface' 1.0.0
% Revision: $Revision: 1.9 $
% Revised At: $Date: 2002/06/27 14:13:20 $
% =============================================
%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
%
% props(Obj,[height(ObjHt)]) == t(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == t(height(Obj,ObjHt)).
% padd(Obj,[height(ObjHt)]) == prop_set(height,Obj,ObjHt,...) == ain(height(Obj,ObjHt))
% [pdel/pclr](Obj,[height(ObjHt)]) == [del/clr](height,Obj,ObjHt) == [del/clr]svo(Obj,height,ObjHt) == [del/clr](height(Obj,ObjHt))
% keraseall(AnyTerm).
%
%                      ANTECEEDANT                                   CONSEQUENT
%
%         P =         test nesc true                         assert(P),retract(~P) , enable(P).
%       ~ P =         test nesc false                        assert(~P),retract(P), disable(P)
%
%   ~ ~(P) =         test possible (via not impossible)      retract( ~(P)), enable(P).
%  \+ ~(P) =         test impossiblity is unknown            retract( ~(P))
%   ~ \+(P) =        same as P                               same as P
%     \+(P) =        test naf(P)                             retract(P)
%
% Dec 13, 2035
% Douglas Miles
*/


:- op(500,fx,'~').
:- op(1050,xfx,('==>')).
:- op(1050,xfx,'<==>').
:- op(1100,fx,('==>')).
:- op(1150,xfx,('::::')).

:- use_module(library(logicmoo_user)).

:- dmsg(begin_abc123).
              
:- file_begin(pfc).

:- dynamic(tCol/1).
:- dynamic(singleValuedInArg/2).
:- dynamic(baseKB:ptReformulatorDirectivePredicate/1).

:- mpred_trace_exec.
:- mpred_watch.


:- abolish(c,0).
:- abolish(a,1).
:- abolish(b,1).
:- dynamic((a/1,b/1,c/0)).

:- mpred_test(ain(a(z))).

:- mpred_test(ain(==> a(z))).
:- mpred_test(a(z)).

:- mpred_test(ain(a(z) ==> z(a))).
:- mpred_test(z(a)).

:- mpred_test(a(_)).


~ a(z).

:- mpred_test(  ~(a(_))).
:- mpred_test(\+ a(_)).

~(~(a(z))).

'Ž'(a).

:- op(666,fx,('¯\\_(?)_/¯')).

:- xlisting(a).

% :-mpred_test(\+  ~(a(_))).
% :-mpred_test(\+ a(_)).

% U=nt(A,B,C),basePFC:spft('$ABOX',X,Y,Z),\+ \+

(a(B),d(B),f(B)) ==> b(B).
(a(B),d(B),e(B)) ==> b(B).
(a(B),e(B),d(B)) ==> b(B).

d(q).
% ?- nl,ZU=nt(_,_,_),ZU,basePFC:spft(UMT,X,Y,Z),\+ \+ ZU=Z,nl.

(b(_),e(q)) ==> c.
(~a(B),~e(B)) ==> q.

a(B)==>d(B).

:- mpred_test(\+c).

==> e(q).
==> b(q).
==> a(q).

:- mpred_test(c).

% :- endif.


