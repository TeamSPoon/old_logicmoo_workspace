/* <module>
% =============================================
% File 'autoexec.pfc'
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


:-       op(990,xfx,(':=')),
         op(250,yfx,('?')),
         op(1,fx,('$')),
         op(200,fy,('@')),
         op(100,yfx,('.')),
         op(400,yfx,('rdiv')),
         op(1150,fx,('meta_predicate')),
         op(400,yfx,('//')),
         op(500,yfx,('/\\')),
         op(1200,fx,('?-')),
         op(1150,fx,('module_transparent')),
         op(1150,fx,('multifile')),
         op(1150,fx,('public')),
         op(1150,fx,('thread_initialization')),
         op(200,fy,('-')),
         op(500,yfx,('-')),
         op(700,xfx,('=:=')),
         op(1150,fx,('thread_local')),
         op(700,xfx,('as')),
         op(700,xfx,('=\\=')),
         op(400,yfx,('mod')),
         op(700,xfx,('=@=')),
         op(700,xfx,('@>')),
         op(200,xfy,('^')),
         op(1200,xfx,('-->')),
         op(700,xfx,('=..')),
         op(1100,xfy,(';')),
         op(700,xfx,('>:<')),
         op(700,xfx,(':<')),
         op(700,xfx,('@<')),
         op(700,xfx,('@=<')),
         op(700,xfx,('@>=')),
         op(400,yfx,('div')),
         op(400,yfx,('/')),
         op(700,xfx,('\\=@=')),
         op(1150,fx,('discontiguous')),
         op(400,yfx,('rem')),
         op(700,xfx,('\\=')),
         op(1050,xfy,('->')),
         op(400,yfx,('>>')),
         op(200,fy,('\\')),
         op(900,fy,('\\+')),
         op(1105,xfy,('|')),
         op(700,xfx,('\\==')),
         op(200,xfx,('**')),
         op(1150,fx,('volatile')),
         op(500,yfx,('\\/')),
         op(1150,fx,('initialization')),
         op(400,yfx,('*')),
         op(1150,fx,('dynamic')),
         op(700,xfx,('>=')),
         op(700,xfx,('>')),
         op(200,fy,('+')),
         op(500,yfx,('+')),
         op(1050,xfy,('*->')),
         op(700,xfx,('=<')),
         op(700,xfx,('<')),
         op(700,xfx,('=')),
         op(700,xfx,('is')),
         op(600,xfy,(':')),
         op(400,yfx,('<<')),
         op(1200,fx,(':-')),
         op(1200,xfx,(':-')),
         op(400,yfx,('xor')),
      %   op(1000,xfy,(',')),
         op(700,xfx,('==')).

:- ensure_abox(baseKB).
:- set_defaultAssertMt(baseKB).
:- set_fileAssertMt(baseKB).


:- with_ukb(baseKB,baseKB:ensure_mpred_file_loaded('system_common.pfc')).

:- with_ukb(baseKB,baseKB:ensure_mpred_file_loaded('system_if_missing.pfc')).

:- with_ukb(baseKB,baseKB:ensure_mpred_file_loaded('system_mdefault.pfc')).

:- with_ukb(baseKB,baseKB:ensure_mpred_file_loaded('system_singleValued.pfc')).

:- with_ukb(baseKB,baseKB:ensure_mpred_file_loaded('logicmoo/snark/common_logic_clif.pfc')).

:- with_ukb(baseKB,baseKB:ensure_mpred_file_loaded('user_relationAllExists.pfc')).


:-multifile(lmconf:locked_baseKB/0).
:-dynamic(lmconf:locked_baseKB/0).
:- asserta((lmconf:locked_baseKB)).


% :- mpred_test(ensure_loaded('pttpFWC.pfc')).


