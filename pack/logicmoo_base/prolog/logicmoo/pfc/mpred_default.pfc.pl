/* 
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


:- file_begin(pfc).

:- dynamic(mpred_default/1).

meta_argtypes(mpred_default(ftAssertable)).

% BWD chaining
mpred_default((Q <- P))/mpred_literal(Q) ==> (Q <-(P, \+ ~(Q))).

% FWD chaining
mpred_default(P==>Q)/nonvar(Q) ==> (((P ==> mpred_default(Q)))).
% mpred_default(P==>Q)/(mpred_literal_nv(Q),if_missing_mask(Q,R,Test))  ==> ((P, (\+ R)/Test ) ==> Q).

% NEG chaining
mpred_default(~Q)/nonvar(Q)  ==>  (( \+ Q ) ==> ~ Q ).

% POS chaining
mpred_default(Q)/mpred_positive_literal(Q)  ==>  ( \+(~Q)  ==> Q ).
mpred_default(Q)/(mpred_literal_nv(Q),if_missing_mask(Q,R,Test)) ==> ( (\ +R/Test ) ==> Q ).



% mpred_default(Q) ==> if_missing(Q,Q).

%(mpred_default(P=>Q)/(mpred_literal_nv(Q),if_missing_mask(Q,R,Test)))  ==> ((P, \+ R/Test) => Q).
%(mpred_default(P=>Q)/nonvar(Q)) ==> (P => mpred_default(Q)).


:- if(lmconf:startup_option(datalog,sanity);lmconf:startup_option(clif,sanity)).

% :- ensure_loaded(pack(logicmoo_base/t/examples/pfc/'sanity_birdt.pfc')).

:- endif.

