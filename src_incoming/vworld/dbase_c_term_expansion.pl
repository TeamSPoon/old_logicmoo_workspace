/** <module> 
% ===================================================================
% File 'dbase_c_term_expansion'
% Purpose: Emulation of OpenCyc for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface' 1.0.0
% Revision:  $Revision: 1.9 $
% Revised At:   $Date: 2002/06/27 14:13:20 $
% ===================================================================
% File used as storage place for all predicates which change as
% the world is run.
%
% props(Obj,height(ObjHt))  == k(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt))  == padd(height,Obj,ObjHt,...) == add(QueryForm)
% kretract[all](Obj,height(ObjHt))  == kretract[all](Obj,height,ObjHt) == pretract[all](height,Obj,ObjHt) == del[all](QueryForm)
% keraseall(AnyTerm).
%
%
% Dec 13, 2035
% Douglas Miles
*/

     
expand_head(G,GH):-force_clause_expansion(G,GH),!.

:-multifile(user:goal_expansion/2).
   
set_list_len(List,A,NewList):-length(List,LL),A=LL,!,NewList=List.
set_list_len(List,A,NewList):-length(List,LL),A>LL,length(NewList,A),append(List,_,NewList),!.
set_list_len(List,A,NewList):-length(NewList,A),append(NewList,_,List),!.

if_mud_asserted(F,A2,_):-is_mpred_prolog(F,A2),!,fail.
if_mud_asserted(F,A2,A):-use_holds_db(F,A2,A).

if_use_holds_db(F,A2,_):- is_mpred_prolog(F,A2),!,fail.
if_use_holds_db(F,A,_):-  never_use_holds_db(F,A,_Why),!,fail.
if_use_holds_db(F,A2,A):- use_holds_db(F,A2,A),!.
if_use_holds_db(F,A,_):- integer(A),findall(n(File,Line),source_location(File,Line),SL),ignore(inside_clause_expansion(CE)),asserta(never_use_holds_db(F,A,discoveredInCode(F,A,SL,CE))),!,fail.

use_holds_db(F,A,_):- never_use_holds_db(F,A,_),!,fail.
use_holds_db(F,A2,A):- integer(A2), A is A2-2, isCycPredArity_Check(F,A),!.
use_holds_db(F,A,A):- integer(A),isa_type(F),!.
use_holds_db(F,A,A):- isCycPredArity_Check(F,A).

ensure_moo_pred(F,A,A):- is_mpred_prolog(F,A),!.
ensure_moo_pred(F,A,_):- never_use_holds_db(F,A,Why),!,throw(never_use_holds_db(F,A,Why)).
ensure_moo_pred(F,A,NewA):- use_holds_db(F,A,NewA),!.
ensure_moo_pred(F,A,A):- dmsg(once(decl_mpred(F,A))),moo:decl_mpred(F,A).

is_kb_module(Moo):-atom(Moo),member(Moo,[dyn,kb,opencyc]).
is_kb_mt_module(Moo):-atom(Moo),member(Moo,[moomt,kbmt,mt]).

prepend_module(_:C,M,M:C):-!.
prepend_module(C,M,M:C).

try_mud_body_expansion(G0,G2):- ((mud_goal_expansion_0(G0,G1),!,goals_different(G0, G1),!,moo:dbase_mod(DBASE))),prepend_module(G1,DBASE,G2).
mud_goal_expansion_0(G1,G2):- ((mud_pred_expansion(if_use_holds_db, holds_t - holds_f,G1,G2))).

try_mud_head_expansion(G0,G2):- ((mud_head_expansion_0(G0,G1),!,goals_different(G0, G1),!,moo:dbase_mod(DBASE))),prepend_module(G1,DBASE,G2).
mud_head_expansion_0(G1,G2):- ((mud_pred_expansion(if_mud_asserted, dbase_t - dbase_f,G1,G2))),!.

try_mud_asserted_expansion(G0,G2):- mud_asserted_expansion_0(G0,G1),!,goals_different(G0, G1),add_from_file(G1,G2),!.
mud_asserted_expansion_0(G1,G2):- ((mud_pred_expansion(if_mud_asserted, asserted_dbase_t - asserted_dbase_f,G1,G2))),!.


dmsg_p(_):-!.
dmsg_p(P):-once(dmsg(P)),!.
dmsg_p(_):-!.

:-'$hide'(goals_different/2).

goals_different(G0,G1):-G0==G1,!,fail.
goals_different(G0,G1):-goals_different_1(G0,G1),!.
goals_different(G0,G1):- G0\==G1.

goals_different_1(NV:G0,G1):-nonvar(NV),!,goals_different_1(G0,G1).
goals_different_1(G0,NV:G1):-nonvar(NV),!,goals_different_1(G0,G1).
goals_different_1(G0,G1):- (var(G0);var(G1)),!,throw(goals_different(G0,G1)).
goals_different_1(G0,G1):- G0 \= G1,!.


attempt_clause_expansion(B,BR):-  compound(B), copy_term(B,BC),numbervars(BC,0,_),!, attempt_clause_expansion(B,BC,BR).
attempt_clause_expansion(_,BC,_):-inside_clause_expansion(BC),!,fail.
attempt_clause_expansion(B,BC,BR):- 
    setup_call_cleanup(asserta(inside_clause_expansion(BC)),
    force_clause_expansion(B,BR),
    ignore(retract(inside_clause_expansion(BC)))).

force_clause_expansion(M:((H:-B)),R):- !, mud_rule_expansion(M:H,M:B,R),!.
force_clause_expansion(((M:H:-B)),R):- !, mud_rule_expansion(M:H,B,R),!.
force_clause_expansion(((H:-B)),R):-mud_rule_expansion(H,B,R),!.
force_clause_expansion(H,HR):- try_mud_asserted_expansion(H,HR),!.
force_clause_expansion(H,HR):- try_mud_head_expansion(H,HR),!.
force_clause_expansion(B,BR):-force_head_expansion(B,BR).

force_head_expansion(H,HR):- try_mud_head_expansion(H,HR),!.
force_head_expansion(H,HR):- user:expand_term(H,HR).

mud_rule_expansion(H,True,HR):-True==true,!,force_head_expansion(H,HR).
mud_rule_expansion(H,B,((HR:-BR))):-force_head_expansion(H,HR),user:expand_goal(B,BR),!.

is_term_head(H):- (( \+ \+ inside_clause_expansion(H))),!.
%is_term_head(_):- inside_clause_expansion(_),!,fail.
%is_term_head(H):-H=_, is_our_sources(H).


is_our_dir(LM):- user:file_search_path(logicmoo,LM0),absolute_file_name(LM0,LM).
current_loading_file_path(Path):- prolog_load_context(module,M),!,module_property(M,file(Path)).
current_loading_file_path(Dir):- prolog_load_context(directory,Dir0),!,absolute_file_name(Dir0,Dir).

is_our_sources(_):- current_loading_file_path(Dir),is_our_dir(LM),atom_concat(LM,_,Dir),!.
is_our_sources(_):- prolog_load_context(module,user),!,not(prolog_load_context(directory,_)).




univ_left(Comp,[M:P|List]):- nonvar(M),univ_left0(M, Comp, [P|List]),!.
univ_left(Comp,[H,M:P|List]):- nonvar(M),univ_left0(M,Comp,[H,P|List]),!.
univ_left(Comp,[P|List]):-moo:dbase_mod(DBASE), univ_left0(DBASE,Comp,[P|List]),!.

% univ_left0(dfgdfuser,Comp,List):- trace,Comp=..List,!.
univ_left0(M,M:Comp,List):- Comp=..List,!.

holds_form(G1,HOLDS,G2):-
      functor_safe(G1,F,A),
      ensure_moo_pred(F,A,NewA),!,            
      G1=..[F|List], set_list_len(List,NewA,NewList), 
      univ_left(G2,[HOLDS,F|NewList]),!.

xcall_form(G1,G2):- nonvar(G1),
      functor_safe(G1,F,A),
      ensure_moo_pred(F,A,NewA),
      G1=..[F|List], set_list_len(List,NewA,NewList), 
      univ_left(G2,[F|NewList]),!.
xcall_form(G1,G1).

:- meta_predicate mud_pred_expansion(+,+,+,-).

mud_pred_expansion(_Prd,_HNH,G1,_):-not(compound(G1)),!,fail.
mud_pred_expansion(_Prd,_HNH,_:G1,_):-var(G1),!,fail.
mud_pred_expansion(_Prd,_HNH,G1,G2):- functor_safe(G1,F,_),xcall_t==F,!,G2 = (G1),!.
mud_pred_expansion(Pred,NHOLDS - HOLDS, not(G1) ,G2):-!,mud_pred_expansion(Pred,HOLDS - NHOLDS,G1,G2).
mud_pred_expansion(Pred,NHOLDS - HOLDS, \+(G1) ,G2):-!,mud_pred_expansion(Pred,HOLDS - NHOLDS,G1,G2).

mud_pred_expansion(Pred, HNH, G0 ,G2):-
 functor_safe(G0,F,1),G1=..[F,MP],
 predicate_property(G0, meta_predicate(G1)),
 member(MP,[:,0,1,2,3,4,5,6,7,8,9]),!,
 G0=..[F,Term],
 mud_pred_expansion(Pred, HNH, Term ,Term2),
  G2=..[F,Term2],!.


mud_pred_expansion(Pred,HNH, Moo:G0,G3):- nonvar(Moo),is_kb_module(Moo),
   xcall_form(G0,G1),
   functor_safe(G1,F,A),
   ensure_moo_pred(F,A,_),
   mud_pred_expansion_0(Pred,HNH,G1,G2),!,G2=G3.

mud_pred_expansion(Pred,HNH, Moo:G1,G3):-  nonvar(Moo),!, mud_pred_expansion_0(Pred,HNH,Moo:G1,G2),!,G2=G3.
mud_pred_expansion(Pred,HNH,G1,G3):- mud_pred_expansion_0(Pred,HNH,G1,G2),!,G2=G3.

mud_pred_expansion_0(Pred,HNH,_:G1,G2):-!,compound(G1),
   mud_pred_expansion_1(Pred,HNH,G1,G2),!.
mud_pred_expansion_0(Pred,HNH,G1,G2):-!,compound(G1),
   mud_pred_expansion_1(Pred,HNH,G1,G2),!.

mud_pred_expansion_1(Pred,HNH,G1,G2):-G1=..[F|ArgList],functor_safe(G1,F,A),mud_pred_expansion_2(Pred,F,A,HNH,ArgList,G2).

mud_pred_expansion_2(_,Holds,_,HoldsT-HoldsF,_,_):-member(Holds,[HoldsT,HoldsF]),!,fail.
mud_pred_expansion_2(_,Holds,_,_,_,_):-member(Holds,[',',';']),!,fail.

mud_pred_expansion_2(Pred,F,A,HNH,ArgList,G2):-member(F,[':','.']),!,throw(mud_pred_expansion_2(Pred,F,A,HNH,ArgList,G2)).
mud_pred_expansion_2(Pred,F,_,HNH,ArgList,G2):- is_holds_true(F),holds_form_l(Pred,ArgList,HNH,G2).
mud_pred_expansion_2(Pred,F,_,HOLDS - NHOLDS,ArgList,G2):- is_holds_false(F),holds_form_l(Pred,ArgList,NHOLDS - HOLDS,G2).
% mud_pred_expansion_2(Pred,F,A,HNH,ArgList,G2):-is_2nd_order_holds(F),!,throw(mud_pred_expansion_2(Pred,F,A,HNH,ArgList,G2)).
mud_pred_expansion_2(Pred,F,A,HNH,ArgList,G2):- call(Pred,F,A,_),holds_form_l(Pred,[F|ArgList],HNH,G2).

holds_form_l(Pred,[G1],HNH,G2):-
   compound(G1),not(is_list(G1)),!,
   mud_pred_expansion(Pred,HNH,G1,G2).

holds_form_l(_,G1,HNH,G2):-do_holds_form(G1,HNH,G2).

do_holds_form([F|List],HOLDS - _NHOLDS,G2):-
   atom(F),
   G1=..[F|List],
   holds_form(G1,HOLDS,G2).

do_holds_form([F|List],HOLDS - _NHOLDS,G2):- G2=..[HOLDS,F|List].
 
