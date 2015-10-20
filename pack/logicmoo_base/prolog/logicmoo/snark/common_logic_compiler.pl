/** <module> mpred_clausify
% Provides a prolog database replacement that uses an interpretation of KIF
%
%  t/N
%  hybridRule/2
%  
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
%= Compute normal forms for SHOIQ formulae.
%= Skolemize SHOI<Q> formula.
%=
%= Copyright (C) 1999 Anthony A. Aaby <aabyan@wwc.edu>
%= Copyright (C) 2006-2007 Stasinos Konstantopoulos <stasinos@users.sourceforge.net>
%= Douglas R. Miles <logicmoo@gmail.com>
%=
%= This program is free software; you can redistribute it and/or modify
%= it under the terms of the GNU General Public License as published by
%= the Free Software Foundation; either version 2 of the License, or
%= (at your option) any later version.
%=
%= This program is distributed in the hope that it will be useful,
%= but WITHOUT ANY WARRANTY; without even the implied warranty of
%= MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%= GNU General Public License for more details.
%=
%= You should have received a copy of the GNU General Public License along
%= with this program; if not, write to the Free Software Foundation, Inc.,
%= 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%=
%= FORMULA SYNTAX
%=
%= not(A)
%= &(F, F)
%= v(F, F)
%= '=>'(F, F)
%= '<=>'(F, F)
%=    all(X,A)
%=    exists(X,A)
%=    atleast(X,N,A)
%=    atmost(X,N,A)

% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_compiler.pl
:- module(common_logic_compiler,         
          [ 
           nnf/3, 
           pnf/3, cf/4,
          op(300,fx,'-'),
          op(600,xfx,'=>'),
          op(600,xfx,'<=>'),
          op(350,xfx,'xor'),
          op(400,yfx,'&'),  
          op(500,yfx,'v'),
            atom_compat/3,
            axiom_lhs_to_rhs/2,
            axiom_lhs_to_rhs/3,
            b_d_p/2,
            boxRule/3,
            cf/4,
            cf_to_flattened_clauses/4,
            cf_to_flattened_clauses_0/4,
            cirRule/3,
            clausify/4,
            clean_repeats_d/2,
            cnf/3,
            cnf1/3,
            correct_boxlog/4,
            correct_boxlog_0/4,
            correct_cls/3,
            correct_cls0/3,
            corrected_modal/3,
            corrected_modal0/3,
            corrected_modal_recurse/3,
            corrected_modal_recurse0/3,
            ct_op/1,
            delete_sublits/3,
            demodal/3,
            demodal_sents/3,
            diaRule/3,
            display_form/2,
            dnf/3,
            dnf1/3,
            expand_cl/3,
            flattenConjs/3,
            flatten_clauses/2,
            get_quantifier_isa/3,
            inclause/6,
            incorrect_cl/2,
            invert_modal/2,
            is_lit_atom/1,
            is_sent_op_modality/1,
            logical_neg/3,
            logical_pos/3,
            logically_matches/3,
            make_1_cl/4,
            make_clause_from_set/3,
            make_clause_set/3,
            make_clauses/3,
            make_each/3,
            mk_skolem/5,
            mk_skolem_name/5,
            modal2sent/2,
            mpred_quf/2,
            mpred_quf_0/2,
            neg_op/1,
            negate/3,
            negate0/3,
            negate_one/3,
            negate_one_maybe/3,
            nnf/3,
            nnf/5,
            nnf_label/5,
            nnf_shared/5,
            nonegate/3,
            nonvar_unify/2,
            notin/2,
            nowrap_one/3,
            pnf/3,
            pnf/4,
            putin/3,
            removeQ/3,
            removeQ/4,
            removeQ_LC/4,
            removes_literal/2,
            same_var/2,
            share_scopes/2,
            simplify_atom/2,
            simplify_cheap/2,
            simplify_cheap_must/2,
            skolem_bad/4,
            skolem_f/5,
            skolem_fn/6,
            skolem_fn_shared/6,
            skolem_setting/1,
            third_order/1,
            to_poss/2,
            to_regular_cl/4,
            unbuiltin_negate/3,
            unbuiltin_negate/4,
            until_op/1,
            variants_are_equal/3
          ]).

% :- use_module(logicmoo(util/logicmoo_util_preddefs)).
:-
            op(1150,fx,(was_dynamic)),
            op(1150,fx,(was_multifile)),
            op(1150,fy,(was_module_transparent)),
            op(1150,fx,(was_export)),
            op(1150,fx,(was_shared_multifile)).

:- was_shared_multifile 
        
        agent_call_command/2,
        feature_test/0,
        mud_test/2,
        regression_test/0,
        sanity_test/0,
        type_action_info/3.
:- was_dynamic 
        
        wid/3.

:- nodebug(_).

:- was_dynamic(wid/3).

% :- use_module(logicmoo(snark/common_logic_sexpr)).

%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%
%=% 
%=%   mpred_clausify.P
%=%      SWI-Prolog version
%=%   Convert wffs to list of normal logic clauses
%=%
%=%   and       &  
%=%   or        v
%=%   not       ~
%=%   xor       xor
%=%   implies   =>   
%=%   iff       <=>  
%=%   all       all(X,0)
%=%   some      exists(Y,0)
%=%
%=%    all(X,p(X) => exists(Y, r(Y) & q(X,Y))) 
%=%  ===============
%=%    p(X) => r(sk1(X)) & q(X,sk1(X))
%=%  ===============
%=%    r(sk1(X)):- p(X).
%=%    q(X,sk1(X)):- p(X).


:- op(300,fx,'~').
:- op(300,fx,'-').
:- op(400,yfx,'&').  
:- op(500,yfx,'v').
:- op(1075,xfx,'=>').
:- op(1075,xfx,'<=>').
:- op(350,xfx,'xor').

:- op(300,fx,user:'~').
:- op(300,fx,user:'-').
:- op(400,yfx,user:'&').  
:- op(500,yfx,user:'v').
:- op(1075,xfx,user:'=>').
:- op(1075,xfx,user:'<=>').
:- op(350,xfx,user:'xor').

% SWI Prolog modules do not export operators by default
% so they must be explicitly placed in the user namespace


:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.
:- prolog_load_context(directory,Dir),asserta(user:file_search_path(logicmoo,Dir)).
:- was_dynamic(lmconf:isa_pred_now_locked/0).


:- include(logicmoo(mpred/'mpred_header.pi')).
% :- use_module(logicmoo(pttp/dbase_i_mpred_pttp_testing)). 
% :- use_module(logicmoo(pttp/dbase_i_mpred_pttp)). 

%  all(R, room(R) => exists(D, (door(D) & has(R,D))))
% for any arbitrary R, if R is a room then there exists some object D that is a door, and R has a D.
% door(sk6(_G180)):- room(_G180)
% has(_G180,sk6(_G180)):- room(_G180)
%  R is not a room if D is a door and R doesn't have D
% if there are no doors anywhere then there must not be rooms
% - room(R):- - has(R,_).

:- %(current_prolog_flag(argv,[pl|_]) -> )
     %op(400, fy, lmconf:(nesc) ),	% Necessity, Always
     %op(400, fy, lmconf:(poss) ),	% Possibly, Eventually
     op(400, fy, lmconf:(cir) ),	% Next time
     op(1075,xfx,user:'<-'),
  
  
     %op(400,fy,nesc),		% Necessity, Always
     %op(400,fy,poss),		% Possibly, Eventually
     op(400,fy,cir),		% Next time

     op(300,fx,'-'),
     op(300,fx,'~'),
     op(1075,xfx,'=>'),
     op(1075,xfx,'<-'),
     op(1075,xfx,'<=>'),
     op(350,xfx,'xor'),
     op(400,yfx,'&'),  
     op(500,yfx,'v')
     ,!.



to_poss(poss(BDT,X),poss(BDT,X)):-nonvar(X),!.
to_poss(X,poss(X)):-!.

:- style_check(+singleton).
%=% Negation Normal Form

% Usage: nnf(+KB,+Fml, ?NNF)
nnf(KB,Fml,NNF):-   
   nnf(KB,Fml,[],NNF,_),!.

skolem_setting(shared).
%skolem_setting(label).
%skolem_setting(nnf).
%skolem_setting(removeQ).
%skolem_setting(eliminate).
%skolem_setting(ignore).
%skolem_setting(leavein).

% get_quantifier_isa(TypedX,X,Col).
get_quantifier_isa(_,_,_):-fail.

logically_matches(KB,A,B):-nonvar(KB),!,logically_matches(_KB,A,B).
logically_matches(_,A,B):- (var(A);var(B)),!,A=B.
logically_matches(KB,all(_,A),B):-!,logically_matches(KB,A,B).
logically_matches(KB,B,all(_,A)):-!,logically_matches(KB,A,B).
logically_matches(KB,exists(V,A),exists(V,B)):-!,logically_matches(KB,A,B).
logically_matches(KB,[A],B):-!,logically_matches(KB,B,A).
logically_matches(KB,A,B):- once(corrected_modal_recurse(KB,A,AM)),A\=@=AM,!,logically_matches(KB,B,AM).
logically_matches(_,A,A).

axiom_lhs_to_rhs(_,poss(beliefs(A,~F1)),~nesc(knows(A,F1))).

:- discontiguous(nnf/5).
:- discontiguous(axiom_lhs_to_rhs/3).
%====== drive negation inward ===
%  nnf(KB,+Fml,+FreeV,-NNF,-Paths)
%
% Fml,NNF:    See above.
% FreeV:      List of free variables in Fml.
% Paths:      Number of disjunctive paths in Fml.

nnf(_KB,Lit,FreeV,Lit,1):- is_ftVar(Lit),!,ignore(FreeV=[Lit]).
nnf(KB,Lit,FreeV,Pos,Paths):- is_ftVar(Lit),!,nnf(KB,true_t(Lit),FreeV,Pos,Paths).

nnf(_,Var, _ ,Var,1):- leave_as_is(Var),!.

nnf(KB,Lit,FreeV,Pos,1):- is_ftVar(Lit),!,wdmsg(warn(nnf(KB,Lit,FreeV,Pos,1))),Pos=true_t(Lit).

nnf(KB,Fin,FreeV,NNF,Paths):- corrected_modal(KB,Fin,F), Fin \=@= F,!,nnf(KB,F,FreeV,NNF,Paths).

/*
nnf(KB,'CollectionSubsetFn'(Col,'TheSetOf'(Var,Formulas)),FreeV,Var,2):- is_ftVar(Var), \+ is_ftVar(Col),
   nnf(KB,all(Var,isa(Var,Col)&Formulas),FreeV,SubForms,_),   
   asserta(added_constraints(KB,Var,SubForms)).
*/
    

nnf(KB,Fin,FreeV,BOX,Paths):- corrected_modal(KB,Fin,nesc(BDT,F)),
	nnf(KB,F,FreeV,NNF,Paths), cnf(KB,NNF,CNF), boxRule(KB,nesc(BDT,CNF), BOX).

%   poss(A & B) ->  all(Vs,poss(A & B)) ->  ~exists(Vs,nesc(A & B))
axiom_lhs_to_rhs(all(Vs,poss(A & B)) ,  ~exists(Vs,nesc(A & B))).

%   poss(beliefs(A,~F1)) ->  poss(~knows(A,F1)) ->  ~nesc(knows(A,F1))
nnf(KB,Fin,FreeV,DIA,Paths):-  copy_term(Fin,Fml),axiom_lhs_to_rhs(KB,F1,F2) , 
 \+ \+ (numbervars(Fin,0,_,[attvar(skip)]),logically_matches(KB,Fin,F1)),
  dcall_success(nnf,(nop(Fml),logically_matches(KB,Fin,F1))),dcall(why,nnf(KB,F2,FreeV,DIA,Paths)).

nnf(KB,Fin,FreeV,CIR,Paths):- corrected_modal(KB,Fin,cir(CT,F)),
	nnf(KB,F,FreeV,NNF,Paths), cirRule(KB,cir(CT,NNF), CIR).

% A until B means it B starts after the ending of A
axiom_lhs_to_rhs(KB,startsAfterEndingOf(B,A),until(CT,A,B)):- share_scopes(KB,CT),!,set_is_lit(A),set_is_lit(B),!.



% axiom_lhs_to_rhs(KB,poss(- (- LIT)),poss(LIT)):-set_is_lit(LIT).
axiom_lhs_to_rhs(_KB,holdsIn(TIMESPAN,TRUTH),temporallySubsumes(TIMESPAN,TRUTH)).
axiom_lhs_to_rhs(_KB,temporallySubsumes(TIMESPAN,TRUTH),(until(TRUTH,~TIMESPAN)&until(~TRUTH,TIMESPAN))).
:- style_check(+singleton).


nnf(KB,until(CT,A,B),FreeV,NNF,Paths):-  set_is_lit(A),set_is_lit(B),  share_scopes(KB,CT),!,
	nnf(KB,A,FreeV,NNF1,Paths1),
	nnf(KB,B,FreeV,NNF2,Paths2),
	Paths is Paths1 + Paths2,
        set_is_lit(NNF1),
        set_is_lit(NNF2),
	NNF = until(CT,NNF1, NNF2).


% ==== typed quantifiers ========
nnf(KB,all(XL,NNF),FreeV,FmlO,Paths):- is_list(XL),XL=[X],!,
     nnf(KB,all(X,NNF),FreeV,FmlO,Paths).
nnf(KB,all(XL,NNF),FreeV,FmlO,Paths):- is_list(XL),XL=[X|MORE],!,
     nnf(KB,all(X,all(MORE,NNF)),FreeV,FmlO,Paths).
nnf(KB,all(TypedX,NNF),FreeV,FmlO,Paths):- get_quantifier_isa(TypedX,X,Col),
     nnf(KB,all(X,isa(X,Col)=>NNF),FreeV,FmlO,Paths).

nnf(KB,exists(XL,NNF),FreeV,FmlO,Paths):- is_list(XL),XL=[X],!,
     nnf(KB,exists(X,NNF),FreeV,FmlO,Paths).
nnf(KB,exists(XL,NNF),FreeV,FmlO,Paths):- is_list(XL),XL=[X|MORE],!,
     nnf(KB,exists(X,exists(MORE,NNF)),FreeV,FmlO,Paths).
nnf(KB,exists(TypedX,NNF),FreeV,FmlO,Paths):- get_quantifier_isa(TypedX,X,Col),
     nnf(KB,exists(X,isa(X,Col)=>NNF),FreeV,FmlO,Paths).


% ==== quantifiers ========
nnf(KB,all(X,NNF),FreeV,all(X,NNF2),Paths):-  
     list_to_set([X|FreeV],NewVars),
      nnf(KB,NNF,NewVars,NNF2,Paths).

nnf(KB,exists(X,Fml),FreeV,NNF,Paths):-  \+ contains_var(X,Fml),!,nnf(KB,Fml,FreeV,NNF,Paths).

nnf(KB,exists(X,Fml),FreeV,NNF,Paths):- skolem_setting(nnf),!, wdmsg(nnf(skolemizing(exists(X,Fml)))),
   must(mk_skolem(KB,Fml,X,FreeV,FmlSk)),
   must(nnf(KB,FmlSk,FreeV,NNF,Paths)).

% exists(X,nesc(f(X)))  ->  exists(X,not(poss(not(f(X))))) ->  not(poss(not(f(X))))
% nnf(KB,exists(X,Fml),FreeV,NNF,Paths):- nnf(KB,not(poss(b_d(KB,nesc,poss),not(Fml))),FreeV,NNF,Paths).

nnf(KB,exists(X,Fml),FreeV,NNF,Paths):- skolem_setting(label),
   nnf_label(KB,exists(X,Fml),FreeV,NNF,Paths),!.

nnf(KB,exists(X,Fml),FreeV,NNF,Paths):- skolem_setting(shared),
   nnf_shared(KB,exists(X,Fml),FreeV,NNF,Paths),!.


nnf(KB,exists(X,Fml),FreeV,NNF,Paths):- skolem_setting(ignore),
   list_to_set([X|FreeV],NewVars),
    nnf(KB,Fml,NewVars,NNF,Paths).

nnf(KB,exists(X,Fml),FreeV,exists(X,NNF),Paths):- (skolem_setting(removeQ);skolem_setting(leave)),
   list_to_set([X|FreeV],NewVars),
    nnf(KB,Fml,NewVars,NNF,Paths).


nnf(KB,atleast(1,X,Fml),FreeV,NNF,Paths):- !,
	nnf(KB,exists(X,Fml),FreeV,NNF,Paths).

nnf(KB,atleast(N,X,Fml),FreeV,NNF,Paths):- 
	!,
	NewN is N - 1,
        subst_except(Fml,X,Y,FmlY),
	nnf(KB,&(exists(X,Fml),atleast(NewN,Y,FmlY)),FreeV,NNF,Paths).
nnf(KB,atmost(1,X,Fml),FreeV,NNF,Paths):- 
	!,
        subst_except(Fml,X,Y,FmlY),
        subst_except(Fml,X,Z,FmlZ),
	nnf(KB,not(&(exists(Y,FmlY),exists(Z,FmlZ))),FreeV,NNF,Paths).
nnf(KB,atmost(N,X,Fml),FreeV,NNF,Paths):- 
	!,
        subst_except(Fml,X,Y,FmlY),
	NewN is N - 1,
	nnf(KB,&(exists(Y,FmlY),atmost(NewN,X,Fml)),FreeV,NNF,Paths).


nnf(KB,not(xor(X , Y)),FreeV,NNF,Paths):-
   !,
   nnf(KB,v(&(X , Y) , &(not(X) , not(Y))),FreeV,NNF,Paths).
   
nnf(KB,xor(X , Y),FreeV,NNF,Paths):-
   !,
   nnf(KB,&(v(X , Y) , v(not(X) , not(Y))),FreeV,NNF,Paths).
   
nnf(KB,(C => &(A,B)),FreeV,NNFO,PathsO):- 
	nnf(KB,A,FreeV,NNF1,Paths1),contains_no_negs(NNF1),
	nnf(KB,B,FreeV,NNF2,Paths2),contains_no_negs(NNF2),
        can_use_hack(two_implications),!,
        to_poss(NNF1,NNF1WFFChk),to_poss(NNF2,NNF2WFFChk),
        FullNNF2 = ((NNF1WFFChk => (C => NNF2))),
        FullNNF1 = ((NNF2WFFChk => (C => NNF1))),
	% Paths is Paths1 * Paths2,
	(Paths1 > Paths2 -> NNF = &(FullNNF2,FullNNF1);
		            NNF = &(FullNNF1,FullNNF2)),
        did_use_hack(two_implications),
        nnf(KB,NNF,FreeV,NNFO,PathsO).

nnf(KB,&(A,B),FreeV,NNF,Paths):- !,
	nnf(KB,A,FreeV,NNF1,Paths1),
	nnf(KB,B,FreeV,NNF2,Paths2),
	Paths is Paths1 * Paths2,
	(Paths1 > Paths2 -> NNF = &(NNF2,NNF1);
		            NNF = &(NNF1,NNF2)).

nnf(KB,<=>(A,B),FreeV,NNFO,Paths):- !,
	nnf(KB,A=>B,FreeV,NNF1,Paths1),
	nnf(KB,B=>A,FreeV,NNF2,Paths2),
	Paths is Paths1 * Paths2,
	(Paths1 > Paths2 -> NNF = &(NNF2,NNF1);
		            NNF = &(NNF1,NNF2)),
        nnf(KB,NNF,FreeV,NNFO,Paths).

nnf(KB,v(A,B),FreeV,NNF,Paths):- !,
        nnf(KB,A,FreeV,NNF1,Paths1),
	nnf(KB,B,FreeV,NNF2,Paths2),
	Paths is Paths1 + Paths2,
	(Paths1 > Paths2 -> NNF = v(NNF2,NNF1);
		            NNF = v(NNF1,NNF2)).

/*
% Release: \phi releases \psi if \psi is true until the first position in which \phi is true (or forever if such a position does not exist).
nnf(KB,Fml,FreeV,NNF,Paths):- 
   logically_matches(KB,Fml,release(CurrentPsi,ReleaserPhi)),
   Fml1 = (ReleaserPhi => ~CurrentPsi),
   nnf(KB,Fml1,FreeV,NNF,Paths).

% Until: \psi holds at the current or a future position, and \phi has to hold until that position. At that position \phi does not have to hold any more
nnf(KB,Fml,FreeV,NNF,Paths):- 
   logically_matches(KB,Fml,until(CurrentPsi,DisablerPhi)),
   Fml1 = (CurrentPsi v (DisablerPhi => ~CurrentPsi)),
   nnf(KB,Fml1,FreeV,NNF,Paths).

% ~until(Future,Current) -> ( always(~Current) v until(~Current,(~Future & ~Current)))
nnf(KB,Fml,FreeV,NNF,Paths):- !,
   logically_matches(KB,Fml,~until(Future,Current)),
   nnf(KB,not(Future),FreeV,NNFuture,_),
   nnf(KB,not(Current),FreeV,NNCurrent,_),
   Fml1 = v(always(NNCurrent), until(CT,NNCurrent,&(NNFuture,NNCurrent))),
   nnf(KB,Fml1,FreeV,NNF,Paths).
   
% ~next(Future) -> next(~Future)
nnf(KB,Fml,FreeV,NNF,Paths):- !,
   logically_matches(KB,Fml,~next(Future)),
   nnf(KB,next(~Future),FreeV,NNF,Paths).
*/   

nnf(KB,not(Fml),FreeV,NNF,Paths):- nonvar(Fml),   
   must(KB\=info_by(_)),
	(Fml = (not(A)) -> invert_modal(A,Fml1);
         Fml = (nesc(BDT,F)) -> Fml1 = poss(BDT,not(F));
	 Fml = (poss(BDT,F)) -> Fml1 = nesc(BDT,not(F));
	 Fml = (cir(CT,F)) -> Fml1 = cir(CT,not(F));
	 Fml = (until(CT,A,B)) -> 
            (nnf(KB,not(A),FreeV,NNA,_), nnf(KB,not(B),FreeV,NNB,_),Fml1 = v(always(CT,NNB), until(CT,NNB,&(NNA,NNB))));
             
	 Fml = (all(X,F)) -> Fml1 = exists(X,not(F));
	 Fml = (exists(X,F)) -> Fml1 = all(X,not(F));

	 Fml = (atleast(N,X,F)) -> Fml1 = atmost(N,X,F);
	 Fml = (atmost(N,X,F)) -> Fml1 = atleast(N,X,F);

	 Fml = (v(A,B)) -> Fml1 = &(not(A), not(B) );
	 Fml = (&(A,B)) -> Fml1 = v(not(A), not(B) );
	 Fml = ('=>'(A,B)) -> Fml1 = &(A, not(B) );
	 Fml = ('<=>'(A,B)) -> Fml1 = v(&(A, not(B)) , &(not(A), B) )
	),!,
        share_scopes(KB,BDT),share_scopes(KB,CT),!,
	nnf(KB,Fml1,FreeV,NNF,Paths).

nnf(KB,Fml,FreeV,NNF,Paths):-  
	(Fml = '=>'(A,B) -> Fml1 = v(not(A), B );         
	 Fml = '<=>'(A,B) -> Fml1 = v(&(A, B), &(not(A), not(B)) );
         Fml = '<=>'(A,B) -> Fml1 = v('=>'(A, B), '=>'(B, A) )
	), nnf(KB,Fml1,FreeV,NNF,Paths).

nnf(_,not(Fml),_FreeV,not(Fml),1):- is_ftVar(Fml),!.
% nnf(KB,Fml,_,Fml,1):- Fml=..[F,KB,_],third_order(F),!.
nnf(KB,[F|ARGS],FreeV,[F2|ARGS2],N):- !,
   nnf(KB,F,FreeV,F2,N1),
   nnf(KB,ARGS,FreeV,ARGS2,N2),N is N1 + N2.

nnf(KB,Fml,FreeV,FmlO,N):- 
   Fml=..[F|ARGS],
   nnf(KB,ARGS,FreeV,FARGS,N1),
   ARGS\=@=FARGS,!,Fml2=..[F|FARGS],
   nnf(KB,Fml2,FreeV,FmlO,N2),N is N1 + N2.

nnf(KB,Fml,FreeV,FmlO,N):- 
   arg(_,Fml,Arg),is_function(Arg),
   function_to_predicate(Arg,NewVar,PredifiedFunction),
   subst_except(Fml,Arg,NewVar,FmlMid),!,
   nnf(KB,all(NewVar,(PredifiedFunction & FmlMid)),FreeV,FmlO,N).

/*
nnf(KB,Fml,FreeV,Out,Path):- Fml=..[F,A],third_order(F),  
  nnf(KB,A,FreeV,NNF1,Path1),!,
  Fml2=..[F,KB,NNF1],nnf(KB,Fml2,FreeV,Out,Path2),Path is Path1+Path2.
*/

% nnf(KB, IN,FreeV,OUT,Paths):- simplify_cheap(IN,MID),IN\=MID,nnf(KB, MID,FreeV,OUT,Paths).
nnf(KB,[F|Fml],FreeV,Out,Paths):- arg(_,v((v),(&),(=>),(<=>)),F),nnf(KB,Fml,FreeV,NNF,Paths),Out =..[F| NNF],!.
% nnf(_KB , IN,[],OUT,1):- mnf(IN,OUT),IN\=OUT,!.
nnf(KB,Fml,_,FmlO,1):- nonegate(KB,Fml,FmlO),!.
nnf(_KB,Fml,_,Fml,1):-!.


is_lit_atom(IN):- leave_as_is(IN),!.
is_lit_atom(IN):- subst_except(IN,'&','*',M),subst_except(M,'v','*',O),!,O==IN.

/*
mnf(Var,Var):-leave_as_is(Var),!.
mnf(Fml,Out):-boxRule(_,Fml,M),Fml\=M,mnf(M,Out).
mnf(Fml,Out):-diaRule(_,Fml,M),Fml\=M,mnf(M,Out).
mnf(poss(DBT,A=>B),Out):- diaRule(_,poss(DBT,v( neg(-,B),A)),M),mnf(M,Out).
mnf(nesc(DBT,A=>B),Out):- mnf(v( neg(-,nesc(DBT, B)), nesc(DBT,A)),M),mnf(M,Out).
mnf([F|Fml],Out):- arg(_,v((v),(&),(=>),(<=>)),F),mnf(Fml,NNF),Out =..[F| NNF].
mnf(Var,Var):-!.
*/

% poss(P=>Q) ==>  poss( - Q v P )  ==>  - nesc( - ( - Q v P ) ) ==>  - nesc( Q & -P  )    .. how can i get the  nesc/poss very close to the P and Q ?

% poss(P=>Q)  ==>   ( -nesc(-P) =>  -nesc(-Q) )   ?

% poss(P=>Q)  ===>   poss( - Q v P ) ===>   poss(- Q) v poss(P)  ===>   - nesc(Q) v poss(P)   ===>      poss(P)=>nesc(Q)  

% poss(DBT,v( neg(-,B),A)) => -nesc(q & -p)

third_order(asserted_t).


% boxRule(KB,A,B):- convertAndCall(as_dlog,boxRule(KB,A,B)).
boxRule(_KB,BOX, BOX):-leave_as_is(BOX),!.
boxRule(KB,nesc(BDT,&(A,B)), &(BA,BB)):- nonvar(A),!, boxRule(KB,nesc(BDT,A),BA), boxRule(KB,nesc(BDT,B),BB).
boxRule(KB,nesc(BDT, IN), BOX):- \+ is_lit_atom(IN), share_scopes(KB,BDT), nnf(KB,not(nesc(BDT, not(IN))),BOX).
boxRule(_KB,BOX, BOX).
 
diaRule(KB,A,B):- convertAndCall(as_dlog,diaRule(KB,A,B)).
diaRule(_KB,BOX, BOX):-leave_as_is(BOX),!.
diaRule(KB,poss(BDT,v(A,B)), v(DA,DB)):- !, diaRule(KB,poss(BDT,A),DA), diaRule(KB,poss(BDT,B),DB).
diaRule(_KB,DIA, DIA).

cirRule(KB,A,B):- convertAndCall(as_dlog,cirRule(KB,A,B)).
cirRule(_KB,BOX, BOX):-leave_as_is(BOX),!.
cirRule(KB,cir(CT,v(A,B)), v(DA,DB)):- !, cirRule(KB,cir(CT,A),DA), cirRule(KB,cir(CT,B),DB).
cirRule(KB,cir(CT,&(A,B)), &(DA,DB)):- !, cirRule(KB,cir(CT,A),DA), cirRule(KB,cir(CT,B),DB).
cirRule(_KB,CIR, CIR).


corrected_modal_recurse(_,Var,OUT):-leave_as_is(Var),!,OUT=Var.
corrected_modal_recurse(KB, IN, OUT):- corrected_modal(KB,IN,OUTM),!,OUT=OUTM.
corrected_modal_recurse(KB, IN, OUTM):- corrected_modal_recurse0(KB, IN, M),!,
  (IN=@=M->OUT=M;corrected_modal_recurse(KB, M, OUT)),!,OUT=OUTM.

corrected_modal_recurse0(_,Var,OUT):-leave_as_is(Var),!,OUT=Var.
corrected_modal_recurse0(KB, IN,FOO):-  is_list(IN),!, must_maplist(corrected_modal_recurse(KB), IN,FOO ),!.
corrected_modal_recurse0(KB, H,FOO):-  compound(H),!,H=..[F|ARGS], must_maplist(corrected_modal_recurse(KB), ARGS,FOOL ),!,FOO=..[F|FOOL].
corrected_modal_recurse0(_, INOUT,  INOUT):- !.



corrected_modal(KB,IN,OUTM):-
  corrected_modal0(KB,IN,M),!,must(corrected_modal_recurse0(KB,M,OUT)),!,OUT=OUTM.


corrected_modal0(_,Var,_):-leave_as_is(Var),!,fail.
corrected_modal0(_,nesc(BDT,F),nesc(BDT,F)):-!.
corrected_modal0(_,poss(BDT,F),poss(BDT,F)):-!.
corrected_modal0(_,until(CT,A,B),until(CT,A,B)):-!.
corrected_modal0(_,cir(CT,F),cir(CT,F)):-!.
corrected_modal0(KB,BF,nesc(b_d(KB,B,D),F)):- BF=..[B,F],b_d_p(B,D).
corrected_modal0(KB,BF,poss(b_d(KB,B,D),F)):- BF=..[D,F],b_d_p(B,D).
corrected_modal0(KB,CF,cir(ct(KB,CT),F)):- CF=..[CT,F],ct_op(CT).
corrected_modal0(KB,CF,until(ct(KB,CT),A,B)):- CF=..[CT,A,B],until_op(CT).
corrected_modal0(_,BF,nesc(b_d(KB,B,D),F)):- BF=..[B,KB,F],b_d_p(B,D).
corrected_modal0(_,BF,poss(b_d(KB,B,D),F)):- BF=..[D,KB,F],b_d_p(B,D).
corrected_modal0(_,CF,cir(ct(KB,CT),F)):- CF=..[CT,KB,F],ct_op(CT).
corrected_modal0(KB,CF,until(ct(KB,CT),A,B)):- CF=..[CT,KB,A,B],until_op(CT).

share_scopes(KB,BDT):-compound(BDT),ignore(arg(1,BDT,KB)),!.
share_scopes(KB,ENV):-ignore(KB=ENV),!.

/*
share_scopes(KB,Neg,CT,BDT):- ignore(Neg=ct(KB,SymNeg)),ignore(BDT=bt(KB,SymNesc,SymPoss)),ignore(CT=ct(KB,SymAllways)),
  ignore(KB=KB),ignore(KB=ct(KB,SymAllways)),ignore(KB=ct(KB,SymUntil)),
  ignore(SymNeg=(-)),
  ignore(SymUntil=(until)),
  ignore(SymNesc=(nesc)),
  ignore(SymPoss=(poss)),
  ignore(SymAllways=(allways)).
*/
until_op(until).

ct_op(cir).
ct_op(nextly).


%ct_op(ist).
%ct_op(asserted_t).

neg_op(not).
neg_op(neg).
neg_op(~).
neg_op(-).
neg_op(\+).

b_d_p(nesc,poss).
b_d_p(box,poss).
b_d_p(box,dia).
b_d_p(knows,beliefs).
b_d_p(always,eventually).
b_d_p(always,sometimes).

% b_d(KB,A,I):- genlPreds(I,A).

%=%
%=%  Conjunctive Normal Form (CNF) : assumes Fml in NNF
%=%

% Usage: cnf(KB, +NNF, ?CNF )
cnf(KB,A,B):- convertAndCall(as_dlog,cnf(KB,A,B)).
cnf(_KB,AS_IS,       AS_IS):-leave_as_is(AS_IS),!.
cnf(KB,&(P,Q), &(P1,Q1)):- !, cnf(KB,P, P1), cnf(KB,Q, Q1).
cnf(KB,v(P,Q),     CNF):- !, cnf(KB,P, P1), cnf(KB,Q, Q1), cnf1(KB, v(P1,Q1), CNF ).
cnf(_KB,CNF,       CNF).

cnf1(_KB,AS_IS,       AS_IS):-leave_as_is(AS_IS),!.
cnf1(KB, v(LEFT, R), &(P1,Q1) ):- nonvar_unify(LEFT , &(P,Q)), !, cnf1(KB, v(P,R), P1), cnf1(KB, v(Q,R), Q1).
cnf1(KB, v(P, RIGHT), &(P1,Q1) ):- nonvar_unify(RIGHT , &(Q,R)), !, cnf1(KB, v(P,Q), P1), cnf1(KB, v(P,R), Q1).
cnf1(_KB, CNF,                 CNF).

nonvar_unify(NONVAR,UNIFY):- \+ leave_as_is(NONVAR),  NONVAR=UNIFY.
%=%
%=% Disjunctive Normal Form (DNF) : assumes Fml in NNF
%=%
% Usage: dnf(KB, +NNF, ?DNF )
dnf(KB,A,B):- convertAndCall(as_dlog,dnf(KB,A,B)).
dnf(_KB,AS_IS,       AS_IS):-leave_as_is(AS_IS),!.
dnf(KB, v(P,Q),  v(P1,Q1) ):- !, dnf(KB,P, P1), dnf(KB,Q, Q1).
dnf(KB, &(P,Q), DNF):- !, dnf(KB,P, P1), dnf(KB,Q, Q1), dnf1(KB,&(P1,Q1), DNF).
dnf(_KB,DNF,       DNF).

dnf1(KB,&(P, v(Q,R)),  v(P1,Q1) ):- !, dnf1(KB,&(P,Q), P1), dnf1(KB,&(P,R), Q1).
dnf1(KB,&(v(P,Q), R), v(P1,Q1) ):- !, dnf1(KB,&(P,R), P1), dnf1(KB,&(Q,R), Q1).
dnf1(_KB,DNF,                  DNF ).


simplify_cheap(IN,IN):- leave_as_is(IN),!.
simplify_cheap(IN,OUT):- IN = nesc(BDT,OUT),is_modal(OUT,BDT),!.
simplify_cheap(poss(BDT,nesc(BDT,IN)),OUT):- simplify_cheap_must(poss(BDT,IN),OUT).
simplify_cheap(poss(BDT,poss(BDT,IN)),OUT):- simplify_cheap_must(poss(BDT,IN),OUT).
simplify_cheap(nesc(BDT,poss(BDT,IN)),OUT):- simplify_cheap_must(poss(BDT,IN),OUT).
% simplify_cheap(not(not(IN)),OUT):- simplify_cheap_must(IN,OUT).
simplify_cheap(not( poss(BDT, poss(BDT, F))), not(F)):-nonvar(F),!.
simplify_cheap(poss(BDT, poss(BDT, F)),  poss(BDT, F)):-nonvar(F),!.
simplify_cheap(not(poss(_, not( F))), F):-nonvar(F),!.
%simplify_cheap(IN,-OUT):- IN = not(poss(BDT,OUT)), is_modal(OUT,BDT),!.
%simplify_cheap(IN,-OUT):- IN = not(nesc(BDT,OUT)), \+is_modal(OUT,BDT),!.

simplify_cheap_must(IN,IN):- leave_as_is(IN),!.
simplify_cheap_must(IN,OUT):- simplify_cheap(IN,OUT).
simplify_cheap_must(IN,IN).


%=
%=  Prenex Normal Form (PNF)
%=

% Usage: pnf(+KB, +Fml, ?PNF ) : assumes Fml in NNF


pnf(KB, F,PNF):- pnf(KB,F,[],PNF),!.

% pnf(+KB, +Fml, +Vars, ?PNF)

pnf(A,B,C,D):- convertAndCall(as_dlog,pnf(A,B,C,D)),!.

pnf(_,Var,_ ,Var):- leave_as_is(Var),!.

pnf(_, [],  _,           []):- !.

pnf(KB, IN,  _,              OUT):- is_list(IN),!, must_maplist(pnf(KB),IN,OUT).

%pnf(KB, IN, FreeV,              OUT):- once(mnf(IN,MID)),IN\=@=MID, pnf(KB,MID,FreeV,OUT).
%pnf(KB, IN, FreeV,              OUT):- simplify_cheap(IN,MID), pnf(KB,MID,FreeV,OUT).

pnf(KB,   all(X,F),Vs,   all(X,PNF)):- list_to_set([X|Vs],VVs), !, pnf(KB,F, VVs, PNF),!.

pnf(KB,   nesc(X,F),Vs,   nesc(X,PNF)):- !, pnf(KB,F,Vs, PNF),!.

pnf(KB,   poss(X,F),Vs,   poss(X,PNF)):- !, pnf(KB,F,Vs, PNF),!.

pnf(KB,  exists(X,F),Vs,exists(X,PNF)):- list_to_set([X|Vs],VVs), !, pnf(KB,F, VVs, PNF),!.

pnf(KB,  &(exists(X,A) , B),Vs,  exists(Y,PNF)):- !, copy_term((X,A,Vs),(Y,Ay,Vs)), pnf(KB,&(Ay,B),[Y|Vs], PNF),!.

pnf(KB,    v(exists(X,A), B),Vs,  exists(Y,PNF)):- !, copy_term((X,A,Vs),(Y,Ay,Vs)), pnf(KB,v(Ay,B),[Y|Vs], PNF).!.

pnf(KB, &(all(X,A), B),Vs, all(Y,PNF)):- !, copy_term((X,A,Vs),(Y,Ay,Vs)), pnf(KB,&(Ay , B),[Y|Vs], PNF),!.

pnf(KB, v(all(X,A), B),Vs, all(Y,PNF)):- !, copy_term((X,A,Vs),(Y,Ay,Vs)), pnf(KB,v(Ay,B),[Y|Vs], PNF),!.

pnf(KB, &(A,exists(X,B)),Vs,  exists(Y,PNF)):- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnf(KB,&(A, By),[Y|Vs], PNF),!.
pnf(KB, v(A,exists(X,B)),Vs,  exists(Y,PNF)):- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnf(KB,v(A,By),[Y|Vs], PNF),!.
pnf(KB, &(A,all(X,B)),Vs, all(Y,PNF)):- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnf(KB,&(A,By),[Y|Vs], PNF),!.
pnf(KB, v(A,all(X,B)),Vs, all(Y,PNF)):- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnf(KB,v(A,By),[Y|Vs], PNF),!.

pnf(KB, &(A, B),Vs,       PNF ):- pnf(KB,A,Vs,Ap), pnf(KB,B,Vs,Bp), 
                                     (A\=Ap; B\=Bp), pnf(KB,&(Ap,Bp),Vs,PNF),!.

pnf(KB, v(A, B),Vs,       PNF ):- pnf(KB,A,Vs,Ap), pnf(KB,B,Vs,Bp), 
                                     (A\=Ap; B\=Bp), pnf(KB,v(Ap,Bp),Vs,PNF),!.


pnf(KB, [A|B], Vs,       PNF ):- !, pnf(KB,A,Vs,Ap), pnf(KB,B,Vs,Bp), 
                                     (A\=Ap; B\=Bp), pnf(KB,[Ap|Bp],Vs,PNF),!.


pnf(KB, H,Vars,FOO ):- fail,  compound(H),H=..[F|ARGS], is_sentence_functor(F), !, pnf(KB, [F|ARGS],Vars,FOOL ),FOO=..FOOL.

pnf(_KB,          PNF, _,       PNF ).

%=%  Clausal Form (CF) : assumes Fml in PNF and
%                                 each quantified variable is unique

% cf(+Why,+KB,+Fml, -Cs)
% Cs is a list of the form: [cl(Head,Body), ...]
% Head and Body are lists.

% cf(Why,KB,A,B,C):- convertAndCall(as_dlog,cf(Why,KB,A,B,C)).
cf(Why,KB,PNF, LIST):- fail, !,
 must_det_l((
  removeQ(KB,PNF,[], UnQ),  
  cnf(KB,UnQ,CNF),
  boxlog_to_prolog(CNF,PTTP0),
  clean_repeats_d(PTTP0,PTTP),
  wdmsg(cnf:-PTTP),
  call((pttp1a_wid(Why,PTTP,O),O\=true)),
  conjuncts_to_list(O,LISTM),LISTM\=[],
  sort(LISTM,LIST),
  wdmsg(pttp:-LIST))),!.


cf(Why,KB,PNF, SET):- 
 must_det_l((
  removeQ(KB,PNF,[], UnQ),
  cnf(KB,UnQ,CNF0),!,
  nnf(KB,CNF0,[],CNF,_),
  wdmsg(cnf:-CNF),
 call(( conjuncts_to_list(CNF,Conj),
  make_clause_set([infer_by(Why)],Conj,EachClause),
  must_maplist(correct_cls(KB),EachClause,SOO),
  expand_cl(KB,SOO,SOOO))),
  sort(SOOO,SET))).


clean_repeats_d((PTT,P0),PTTP):-!, conjuncts_to_list((PTT,P0),DLIST),list_to_set(DLIST,DSET),must_maplist(clean_repeats_d,DSET,CSET),list_to_conjuncts((,),CSET,PTTP),!.
clean_repeats_d((PTT;P0),PTTP):-!, disjuncts_to_list((PTT;P0),DLIST),list_to_set(DLIST,DSET),must_maplist(clean_repeats_d,DSET,CSET),list_to_conjuncts((;),CSET,PTTP),!.
clean_repeats_d(PTTP,PTTP).


invert_modal(nesc(BD,A),poss(BD,A)):-set_is_lit(A),!.
invert_modal(poss(BD,A),nesc(BD,A)):-set_is_lit(A),!.
% invert_modal(A,poss(b_d(KB,nesc,poss),A)):-!.
invert_modal(A,A):-!.


removeQ(KB, F,  HH):- removeQ(KB, F, _, RQ0),!,RQ0=HH.

% removes quantifiers (also pushes modal operators inside the negations) 

removeQ_LC(KB, MID,FreeV,OUT):-loop_check(removeQ(KB, MID,FreeV,OUT)).

removeQ(_,Var,_ ,Var):- leave_as_is(Var),!.

removeQ(KB, IN,FreeV,OUT):-var(KB),!,check_is_kb(KB),removeQ(KB, IN,FreeV,OUT).
removeQ(KB, IN,FreeV,OUT):-  once(simplify_cheap(IN,MID)), IN\=@=MID, removeQ_LC(KB, MID,FreeV,OUT),!.

removeQ(KB, not(NN),Vars, XF):- nonvar(NN),NN=not(F), invert_modal(F,FI),!, removeQ(KB,  FI,Vars, XF) .
removeQ(KB, all(X,F),Vars, HH):- !,  removeQ(KB,F,[X|Vars], RQ0),RQ0=HH.

/*
removeQ(KB, not(nesc(BDT, not(F))),Vars, XF):- !,removeQ_LC(KB, poss(BDT, F),Vars, XF).
removeQ(KB, not(poss(BDT, not(F))),Vars, XF):- !,removeQ_LC(KB, nesc(BDT, F),Vars, XF).

removeQ(KB, not(nesc(BDT, (F))),Vars, XF):- !,removeQ(KB, poss(BDT, not(F)),Vars, XF).
removeQ(KB, not(poss(BDT, (F))),Vars, XF):- !,removeQ(KB, nesc(BDT, not(F)),Vars, XF).
*/

removeQ(KB, nesc(BDT, not(F)),Vars, XF):- !,removeQ(KB, not(poss(BDT, F)),Vars, XF).
removeQ(KB, poss(BDT, not(F)),Vars, XF):- !,removeQ(KB, not(nesc(BDT, F)),Vars, XF).

removeQ(KB,  exists(X,F),Vars, HH):- skolem_setting(removeQ),!,wdmsg(removeQ(skolemizing(exists(X,F)))),
	mk_skolem(KB,F,X,Vars,Fsk),
	removeQ(KB,Fsk,Vars, HH).

removeQ(KB, exists(X,F),Vars, HH):-   must(removeQ(KB,F,[X|Vars], RQ0)),RQ0=HH.

removeQ(KB, ':-'(H,B), Vars, ':-'(HH,BB ) ):- !, removeQ(KB,H, Vars, HH ),removeQ(KB,B, Vars, BB).
removeQ(KB, cl(H,B), _, O ):- !,correct_cls(KB,cl(H,B),O).
removeQ(KB,     [ H|B ],Vars, [ HH|BB ] ):- !,removeQ(KB,H, Vars, HH ),removeQ(KB,B, Vars, BB).

%removeQ(KB, H, Vars, HH ):- functor(H,F,1),adjust_kif(KB,H,MM),H\=@=MM,!, removeQ(KB, MM, Vars, HH ).

%removeQ(KB, H, Vars,HH ):- functor(H,F,1),kb_nlit(KB,F),once(nnf(KB,H,MM)),H\=@=MM,  removeQ_LC(KB, MM, Vars, HH ).
removeQ(KB, H,  Vars,HH ):- H = not( _), once(nnf(KB,H,MM)),H\=@=MM,  removeQ_LC(KB, MM, Vars, HH ).

removeQ(KB, H, Vars, HH ):- convertAndCall(as_dlog,removeQ(KB,H, Vars, HH )).

removeQ(KB, H,Vars,HH ):- compound(H),H=..[F|ARGS],!,removeQ(KB, ARGS,Vars,ARGSO ),HH=..[F|ARGSO].

removeQ(KB, F,Vars,OUT ):- nnf(KB,F,Vars,F0,_),(F0 =@=F -> F0=OUT; removeQ(KB, F0,Vars,OUT )),!.




nowrap_one(_,[One],One).
nowrap_one(Wrap,MORE,OUT):- OUT=..[Wrap,MORE].

display_form(KB,Form):- demodal_sents(KB,Form,OutM),local_pterm_to_sterm(OutM,Out),portray_clause(current_output,Out,[max_depth(0),portrayed(true)]).

demodal_sents(KB,I,O):- must_det_l((demodal(KB,I,M),modal2sent(M,O))).

demodal(KB,In,Prolog):- call_last_is_var(demodal(KB,In,Prolog)),!.
demodal(_KB,Var, Var):- cnotrace(leave_as_is(Var)),!.
demodal(KB,[H|T],[HH|TT]):- !, demodal(KB,H,HH),demodal(KB,T,TT).
demodal(KB, not(H), not(HH)):-!, demodal(KB,H, HH),!.

demodal(KB, nesc(b_d(KB2,X,_),F), HH):-KB\==KB2,XF =..[X,KB2,F],!,demodal(KB2,XF, HH).
demodal(KB, poss(b_d(KB2,_,X),F), HH):-KB\==KB2,XF =..[X,KB2,F],!,demodal(KB2,XF, HH).

demodal(KB, nesc(b_d(KB,X,_),F),   HH):- XF =..[X,F], !,demodal(KB,XF, HH).
demodal(KB, poss(b_d(KB,_,X),F),   HH):- XF =..[X,F], !,demodal(KB,XF, HH).

demodal(KB,neg(H),not(HH)):- nonvar(H),demodal(KB,H,HH).
demodal(KB,nesc(F), HH):- !,demodal(KB,F, HH).
demodal(KB,not(H),not(HH)):- nonvar(H),demodal(KB,H,HH).

demodal(KB,H,HH ):- H=..[F|ARGS],!,must_maplist(demodal(KB),ARGS,ARGSO),!,HH=..[F|ARGSO].

is_sent_op_modality(not).
is_sent_op_modality(poss).
is_sent_op_modality(nesc).
atom_compat(F,HF,HHF):- fail,F\=HF, is_sent_op_modality(F),is_sent_op_modality(HF), format(atom(HHF),'~w_~w',[F,HF]).

modal2sent(Var, Var):- cnotrace(leave_as_is(Var)),!.
modal2sent(G,O):- G=..[F,H], \+ leave_as_is(H), H=..[HF,HH], atom_compat(F,HF,HHF),!, GG=..[HHF,HH], modal2sent(GG,O).
modal2sent([H|T],[HH|TT]):- !, must(( modal2sent(H,HH),modal2sent(T,TT))),!.
modal2sent(H,HH ):- H=..[F|ARGS],!,must_maplist(modal2sent,ARGS,ARGSO),!,HH=..[F|ARGSO].


clausify(KB, &(P,Q), C1, C2 ):- 
	!,
	clausify(KB, P, C1, C3 ),
	clausify(KB, Q, C3, C2 ).
clausify(KB, P, [cl(A,B)|Cs], Cs ):- 
	inclause(KB, P, A, [], B, [] ),
	!.
clausify(_KB, _, C, C ).

inclause(KB, v(P,Q), A, A1, B, B1 ):- 
	!,
	inclause(KB, P, A2, A1, B2, B1 ),
	inclause(KB, Q, A,  A2, B,  B2 ).
inclause(KB, not( PP) , A,  A, B1, B ):- 
        negate(KB, not( PP),P),
	!,
	notin(P, A ),
	putin(P, B, B1 ).
inclause(_KB, P,  A1, A, B,  B ):- 
	!,
	notin(P, B ),
	putin(P, A, A1 ).

notin(X,[Y|_]):- X==Y, !, fail.
notin(X,[_|Y]):- !,notin(X,Y).
notin(_,[]).

putin(X,[],   [X]   ):- !.
putin(X,[Y|L],[Y|L] ):- X == Y,!.
putin(X,[Y|L],[Y|L1]):- putin(X,L,L1).

simplify_atom(H,SH):-simplify_cheap(H,SH),!.
simplify_atom(H,H).

to_regular_cl(KB,[(H1 & H2)],[Has],[cl([H1],H1P),cl([H2],H2P)]):- cnf(KB,Has,HasC),  append([HasC],[poss,H2],H1P), append([HasC],[poss,H1],H2P),!.
to_regular_cl(_KB,[(H1 & H2)],Has,[cl([H1],H1P),cl([H2],H2P)]):-  append(Has,[poss,H2],H1P), append(Has,[poss,H1],H2P),!.
to_regular_cl(_KB,[H],[],[cl([SH],[])]):-is_lit_atom(H),simplify_atom(H,SH).
to_regular_cl(_KB,HL,BL,[cl(HL,BL)]).


expand_cl(_KB,[],[]):-!.
expand_cl(KB,[cl(H,B)|O],OOut):- 
      to_regular_cl(KB,H,B,More),!,
      expand_cl(KB,O,OO),
      append(More,OO,OOut).

make_clause_set(_Extras ,[],[]).
make_clause_set(Extras,[CJ|Conj],CLAUSES):-
   make_clauses(Extras,CJ,CLS),
   make_clause_set(Extras,Conj,CLAUS),
   append(CLS,CLAUS,CLAUSES).

% make_clauses(Extras,_,[CJ],cl([CJ],[])):-is_lit_atom(CJ),!.
make_clauses(Extras,CJ,OOut):- disjuncts_to_list(CJ,Conj),make_clause_from_set(Extras,Conj,OOut).

negate_one_maybe(Extras,One,Neg):-negate_one(Extras,One,Neg).
   
make_clause_from_set(Extras,Conj,Out):- findall(E,make_each(Extras,Conj,E),Out).

make_each(Extras,Conj,E):- member(One,Conj), make_1_cl(Extras,One,Conj,E).

make_1_cl(Extras,One,Conj,cl([One],NewBodyListO)):- 
  negate_one_maybe(Extras,One,NHead),!,
  One\={_}, NHead\={_},
  delete_eq(Conj,One,Rest0),delete_eq(Rest0,NHead,Rest),
  must_maplist(negate_one_maybe(Extras),Rest,NewBodyList),!,
  flattenConjs(Extras,NewBodyList,NewBodyListM),
  must_maplist(lmconf:as_prolog,NewBodyListM,NewBodyListO).

flattenConjs(_Extras,I,O):- conjuncts_to_list(I,M),must_maplist(conjuncts_to_list,M,L),flatten(L,O).



:- was_export(logical_pos/3).
:- was_export(logical_neg/3).
logical_neg(KB,Wff,WffO):- 
  must(nonegate(KB,Wff,Wff1)),nnf(KB,not(Wff1),Wff2),must(nonegate(KB,Wff2,WffO)),!.
logical_pos(KB,Wff,WffO):- 
  must(nonegate(KB,Wff,Wff1)),nnf(KB,Wff1,Wff2),must(nonegate(KB,Wff2,WffO)),!.


negate_one(KB,Wff,WffO):- logical_neg(KB,Wff,WffO).


negate(KB,X,Z):- must(defunctionalize(X,Y)), must_det(negate0(KB,Y,Z)).
negate0(_,not(X),X).
negate0(_,X,not(X)).



mpred_quf(In,Out):- transitive(mpred_quf_0,In,Out).

mpred_quf_0(InOut,InOut):- non_compound(InOut),!.
% mpred_quf_0(In,Out):- current_predicate(db_quf/4),db_quf(change(assert,_Must),In,U,C),conjoin(U,C,Out).
mpred_quf_0(In,In).

:- was_export(nonegate/3).
nonegate(_KB,IO,IO):-!.
nonegate(KB,List,OutZ):- is_list(List),must_maplist(nonegate(KB),List,OutZ),!.
nonegate(KB,Fml,OutZ):- simplify_cheap(Fml,Fml2), Fml \=@= Fml2,nonegate(KB,Fml2,OutZ),!.
nonegate(KB,Fml,OutZ):- must((unbuiltin_negate(KB,Fml,Out),!,defunctionalize(Out,OutY),!,must(mpred_quf(OutY,OutZ)))),!.

unbuiltin_negate(_Neg,_, Fml,Fml):- is_ftVar(Fml),!.
unbuiltin_negate(_Neg,_, Fml,Out):- get_functor(Fml,F,A),pttp_builtin(F,A),!,must(Out=Fml).
unbuiltin_negate(_KB,Fml,Out):- once(negate(KB,Fml,Neg)),negate(KB,Neg,Out),!.





% skolem_fn

nnf_label(KB,exists(X,Fml),FreeV,NNF,Paths):-
   must_det_l((
         list_to_set([X|FreeV],NewVars),
         nnf(KB,Fml,NewVars,NNFMid,_Paths),
         skolem_fn(KB, NNFMid, X, FreeV, Fun, SkVars),
         SKF =.. [Fun|SkVars],
        % subst_except(NNFMid,X,SKF,FmlSk),
         % MAYBE CLOSE nnf(KB,((mudEquals(X,SKF) => ~FmlSk)v Fml),NewVars,NNF,Paths).
         %nnf(KB,  (((skolem(X,SKF))=>NNFMid) & FmlSk) ,NewVars,NNF,Paths))).
        % GOOD nnf(KB, isa(X,SKF) => (skolem(X,SKF)=>(NNFMid)) ,NewVars,NNF,Paths))).
         nnf(KB, skolem(X,SKF) => NNFMid ,NewVars,NNF,Paths))).

nnf_shared(KB,exists(X,Fml),FreeV,NNF,Paths):-
   must_det_l((
         list_to_set([X|FreeV],NewVars),
         nnf(KB,Fml,NewVars,NNFMid,_Paths),
         skolem_fn_shared(KB, NNFMid, X, FreeV, Fun, SkVars),
         SKF =.. [Fun|SkVars],
         % subst_except(NNFMid,X,SKF,FmlSk),
         nnf(KB, skolem(X,SKF) => NNFMid ,NewVars,NNF,Paths))).


%=%  Skolemizing : method 1

% Usage: mk_skolem(+Fml,+X,+FreeV,?FmlSk)
% Replaces existentially quantified variable with the formula
% VARIABLES MUST BE PROLOG VARIABLES
% exists(X,p(X)) ==> p(p(exists))

skolem_bad(Fml,X,FreeV,FmlSk):- 
	copy_term((X,Fml,FreeV),(Fml,Fml1,FreeV)),
	copy_term((X,Fml1,FreeV),(exists,FmlSk,FreeV)).

%=%  Skolemizing : method 2

% Usage: mk_skolem(KB, +Fml, +X, +FreeV, ?FmlSk )
% Replaces existentially quantified variable with a unique function
% fN(Vars) N=1,...
% VARIABLES MAYBE EITHER PROLOG VARIABLES OR TERMS


mk_skolem(KB, F, X, FreeV, Out):-  fail,
   must(skolem_f(KB, F, X, FreeV, Sk)),
   subst_except(F,X,Sk,Out).

mk_skolem(KB, F, X, FreeV, Out):-  
   must(skolem_f(KB, F, X, FreeV, Sk)),
   %writeq(freev(Sk,FreeV)),
   must(Out= '=>'({skolem(X,Sk)},F)),
   !,dcall(why, asserta((constraintRules(X,Sk,F)))).

mk_skolem(KB, F, X, FreeV, FmlSk):- 
    must(skolem_f(KB, F, X, FreeV, Sk)), 
    must(subst_except(F, X, Sk, FmlSk)),!.


skolem_f(KB, F, X, FreeVIn, Sk):- 
       must_det_l((
         delete_eq(FreeVIn,KB,FreeV0),
         delete_eq(FreeV0,X,FreeV),
         list_to_set(FreeV,FreeVSet),
	contains_var_lits(F,X,LitsList),
        mk_skolem_name(KB,X,LitsList,'',SK),
        concat_atom(['sk',SK,'Fn'],Fun),
	Sk =..[Fun|FreeVSet])).

skolem_fn(KB, F, X, FreeVIn,Fun, FreeVSet):- 
       must_det_l((
         delete_eq(FreeVIn,KB,FreeV0),
         delete_eq(FreeV0,X,FreeV),
         list_to_set(FreeV,FreeVSet),
	contains_var_lits(F,X,LitsList),
        mk_skolem_name(KB,X,LitsList,'',SK),
        concat_atom(['sk',SK,'Fn'],Fun))).

skolem_fn_shared(KB, F, X, _FreeVIn,Fun, Slots):- 
       must_det_l((
	contains_var_lits(F,X,LitsList),
        term_slots(LitsList,FreeV0),
        delete_eq(FreeV0,X,Slots),
        mk_skolem_name(KB,X,LitsList,'',SK),
        concat_atom(['sk',SK,'Fn'],Fun))).
/*


%=% Substitution

% Usage: subst_except(+Fml,+X,+Sk,?FmlSk)
subst_except(Fml,X,Sk,FmlSkO):- pred_subst(==,Fml,X,Sk,FmlSk),!,must(FmlSkO=FmlSk).


% Usage: pred_subst(+Pred,+Fml,+X,+Sk,?FmlSk)
pred_subst(Pred,   all(Y,P), X,Sk,   all(Y,P1) ):- !, pred_subst(Pred, P,X,Sk,P1 ).
pred_subst(Pred,exists(Y,P), X,Sk,exists(Y,P1) ):- !, pred_subst(Pred, P,X,Sk,P1 ).
pred_subst(Pred, &(P,Q), X,Sk,&(P1,Q1) ):- !, pred_subst(Pred, P,X,Sk,P1 ), pred_subst(Pred, Q,X,Sk,Q1 ).
pred_subst(Pred,  v(P,Q), X,Sk, v(P1,Q1) ):- !, pred_subst(Pred, P,X,Sk,P1 ), pred_subst(Pred, Q,X,Sk,Q1 ).

pred_subst(Pred,       P,    X,Sk,       P1    ):- call(Pred,P,X), Sk=P1,!.
pred_subst(_Pred,       P,    _,_,       P1    ):- is_ftVar(P), P1=P,!.
pred_subst(Pred,       P,    X,Sk,       P1    ):- compound(P),
                             P =..Args, 
                               pred_subst2(Pred, X, Sk, Args, ArgS ),!,
                             P1 =..ArgS.
pred_subst(_  ,        P,    _, _,       P     ).

pred_subst2(_   , _,  _, [], [] ).
pred_subst2(Pred, X, Sk, [A|As], [Sk|AS] ):- call(Pred, X, A), !, pred_subst2(Pred, X, Sk, As, AS).
pred_subst2(Pred, X, Sk, [A|As], [A|AS]  ):- is_ftVar(A), !, pred_subst2(Pred, X, Sk, As, AS).
pred_subst2(Pred, X, Sk, [A|As], [Ap|AS] ):- pred_subst(Pred, A,X,Sk,Ap ), pred_subst2(Pred, X, Sk, As, AS).
*/




%=%=%=%=%=%=%=%=%=%=%=
%=% generate a mk_skolem 

mk_skolem_name(_O,Var,Fml,SIn,SOut):- is_ftVar(Fml), same_var(Var,Fml),!,atom_concat('Is',SIn,SOut).
mk_skolem_name(_O,_V,Fml,SIn,SIn):- is_ftVar(Fml),!.
mk_skolem_name(_O ,_V,[],SIn,SIn):- !.
mk_skolem_name(_O,_V, OP,SIn,SIn):- is_log_op(OP),!.
mk_skolem_name(_O,_V,Fml,SIn,SOut):- atomic(Fml),!,i_name(Fml,N),toPropercase(N,CU),!,(atom_contains(SIn,CU)->SOut=SIn;atom_concat(SIn,CU,SOut)).
mk_skolem_name(KB,Var,[H|T],SIn,SOut):- !,mk_skolem_name(KB,Var,H,SIn,M),mk_skolem_name(KB,Var,T,M,SOut).
mk_skolem_name(KB,Var,isa(VX,Lit),SIn,SOut):- same_var(Var,VX),is_ftNonvar(Lit),!,mk_skolem_name(KB,Var,['Is',Lit,'In'],'',F),atom_concat(F,SIn,SOut).
mk_skolem_name(KB,Var,Fml,SIn,SOut):- Fml=..[F,VX],same_var(Var,VX),!,mk_skolem_name(KB,Var,['Is',F,'In'],SIn,SOut).
mk_skolem_name(KB,Var,Fml,SIn,SOut):- Fml=..[F,Other,VX|_],same_var(Var,VX),!,type_of_var(KB,Other,OtherType),
   mk_skolem_name(KB,Var,[OtherType,'Arg2Of',F],SIn,SOut).
mk_skolem_name(KB,Var,Fml,SIn,SOut):- Fml=..[F,VX|_],same_var(Var,VX),!,mk_skolem_name(KB,Var,['Arg1Of',F],SIn,SOut).
mk_skolem_name(KB,Var,Fml,SIn,SOut):- Fml=..[F|_],!,mk_skolem_name(KB,Var,['ArgNOf',F],SIn,SOut).

% same_var(Var,Fml):- not(not(Var=Fml)),!.
same_var(Var,Fml):- Var==Fml,!.


removes_literal(true_t(X),possible_t(X)).
removes_literal(true_t(X,Y),possible_t(X,Y)).
removes_literal(true_t(X,Y,Z),possible_t(X,Y,Z)).
removes_literal(true_t(X,Y,Z,A),possible_t(X,Y,Z,A)).

removes_literal(not_true_t(X),possible_t(X)).
removes_literal(not_true_t(X,Y),possible_t(X,Y)).
removes_literal(not_true_t(X,Y,Z),possible_t(X,Y,Z)).
removes_literal(not_true_t(X,Y,Z,A),possible_t(X,Y,Z,A)).


delete_sublits(H0,B,HH):- delete_eq(H0,B,H1),delete_eq(H1,B,H2),delete_eq(H2,B,HH),!.

% cl([-nesc(p)], [-poss(p), nesc(q), -poss(q)]).

flatten_clauses([H|T],HHTT):-!,flatten_clauses(H,HH),flatten_clauses(T,TT),append(HH,TT,HHTT).
flatten_clauses(poss(~(~(H))),poss(HH)):- !,flatten_clauses(H,HH),!.
flatten_clauses(nesc(~(~(H))),HH):- !,flatten_clauses(H,HH),!.
flatten_clauses((H,T),HHTT):-!,flatten_clauses(H,HH),flatten_clauses(T,TT),append(HH,TT,HHTT).
flatten_clauses([H],[H]):-!.

correct_cls(KB,H,HH):-loop_check(correct_cls0(KB,H,HH),H=HH),!.

correct_cls0(KB,CL0,CL1):- is_list(CL0),!,must_maplist(correct_cls(KB),CL0,CL1).
correct_cls0(KB,(H,T),HHTT):-!,correct_cls(KB,H,HH),correct_cls(KB,T,TT),append(HH,TT,HHTT).
correct_cls0(KB,(H:-B),O):-!,conjuncts_to_list(H,HH),conjuncts_to_list(B,BB),correct_cls0(KB,cl(HH,BB),O).

correct_cls0(KB,CL,O):- demodal_sents(KB,CL,CLM),CL\=@=CLM,!,correct_cls(KB,CLM,O).
correct_cls0(KB,cl(H,B),O):-flatten_clauses(B,BB),B\=@=BB,correct_cls0(KB,cl(H,BB),O).
correct_cls0(KB,cl(H,B),O):-removeQ(KB,H,HH),removeQ(KB,B,BB),(H\=@=HH ; B\=@=BB),!, correct_cls(KB,cl(HH,BB),O).

correct_cls0(KB,cl(H,B),O):- member(E,B),removes_literal(E,R),delete_sublits(B,R,BB),BB\=@=B,!,correct_cls(KB,cl(H,BB),O).



correct_cls0(KB,cl(H,B),O):- list_to_set(H,HH),HH\=@=H,!,correct_cls(KB,cl(HH,B),O).
correct_cls0(KB,cl(H,B),O):- list_to_set(B,BB),BB\=@=B,!,correct_cls(KB,cl(H,BB),O).

/*
correct_cls0(_,cl([not(poss(H))],B),cl([z_unused(~pos(H:-B))],[])):-member(not(H),B),!.
correct_cls0(KB,cl([not(poss(H))],B),O):- correct_cls0(KB,cl([not((H))],B),O).
correct_cls0(KB,cl([not(H)],B),O):- delete_sublits(B,poss(H),BB),BB\=@=B,!,correct_cls(KB,cl([not(H)],BB),O).
correct_cls0(KB,cl([not(H)],B),O):- delete_sublits(B,(H),BB),BB\=@=B,!,correct_cls(KB,cl([not(H)],BB),O).
correct_cls0(KB,cl([H],B),O):- delete_sublits(B,H,BB),BB\=@=B,!,correct_cls(KB,cl([H],BB),O).
correct_cls0(KB,cl([H],B),O):- delete_sublits(B,not(H),BB),BB\=@=B,!,correct_cls(KB,cl([H],BB),O).

correct_cls0(KB,cl(H,B),O):- member(E,B),E=poss(not(_)),delete_sublits(B,E,BB),BB\=@=B,!,correct_cls(KB,cl(H,BB),O).
correct_cls0(KB,cl(H,B),O):- member(E,B),E=nesc(not(P)),delete_sublits(B,E,BB),BB\=@=B,!,correct_cls(KB,cl(H,[not(P)|BB]),O).
correct_cls0(KB,cl(H,B),O):- member(E,B),delete_sublits(B,poss(E),BB),BB\=@=B,!,correct_cls(KB,cl(H,BB),O).
correct_cls0(KB,cl(H,B),O):- member(not(E),B),delete_sublits(B,poss(E),BB),BB\=@=B,!,correct_cls(KB,cl(H,BB),O).
correct_cls0(KB,cl(H,B),O):- member(not(E),B),delete_sublits(B,E,BB),BB\=@=B,!,correct_cls(KB,cl(H,BB),O).
correct_cls0(KB,cl(H,B),O):- member(nesc(not(E)),B),delete_sublits(B,poss(E),BB),BB\=@=B,!,correct_cls(KB,cl(H,BB),O).

% correct_cls0(KB,cl([(poss(H))],B),O):- correct_cls0(KB,cl([((H))],B),O).

correct_cls0(_,cl(H,B),O):- member(E,B),member(not(E),B),!,incorrect_cl(cl(H,B),O).

correct_cls0(_,cl([nesc((H))],B),cl([z_unused(nesc(H:-B))],[])):-member((H),B),!.
correct_cls0(KB,cl([nesc((H))],B),O):- delete_sublits(B,not(H),BB),BB\=@=B,!,correct_cls(KB,cl([(H)],BB),O).
correct_cls0(KB,cl([not((H))],B),O):- correct_cls(KB,cl([not(poss(H))],B),O).
*/

correct_cls0(_KB,cl(H,B),O):- !,O=cl(H,B).
correct_cls0(KB,H,O):-correct_cls0(KB,(H:-true),O).

incorrect_cl(cl(H,B),cl([z_unused(H:-B)],[])).



:- was_export(correct_boxlog/4).
correct_boxlog((CLAU,SES),KB,Why,FlattenedO):- nonvar(SES),conjuncts_to_list((CLAU,SES),CLAUSES),!,correct_boxlog_0(CLAUSES,KB,Why,FlattenedO),!.
correct_boxlog(CLAUSES,KB,Why,FlattenedO):- (\+ is_list(CLAUSES)),!,correct_boxlog_0([CLAUSES],KB,Why,FlattenedO),!.
correct_boxlog(BOXLOG,KB,Why,FlattenedS):-correct_boxlog_0(BOXLOG,KB,Why,FlattenedS),!.

correct_boxlog_0(BOXLOG,KB,Why,FlattenedS):-
  must_det_l((  
   must_maplist(adjust_kif(KB),BOXLOG,MODAL),
   %wdmsgl(modal(MODAL)),   
   must_maplist(demodal(KB),MODAL,CLAUSES),
   must_maplist(correct_cls(KB),CLAUSES,NCFs),
   must_maplist(clauses_to_boxlog(KB,Why),NCFs,ListOfLists),
   flatten([ListOfLists],Flattened),
   must_maplist(removeQ(KB),Flattened,FlattenedM),
   must_maplist(demodal(KB),FlattenedM,FlattenedO),
   predsort(variants_are_equal,FlattenedO,FlattenedS),
   wdmsgl(horn(FlattenedS)))),!.

variants_are_equal( =, A,B):- unnumbervars(A+B,AA+BB),AA=@=BB,!.
variants_are_equal( Order, A,B):- compare(Order,A,B).

cf_to_flattened_clauses(KB,Why,NCFsI,FlattenedO):-
  loop_check(cf_to_flattened_clauses_0(KB,Why,NCFsI,FlattenedO),NCFsI=FlattenedO),!.

cf_to_flattened_clauses_0(KB,Why,NCFsI,FlattenedO):- 
 must_det_l((
   must_maplist(correct_cls(KB),NCFsI,NCFs),
   % wdmsgl(cf(NCFs)),
   must_maplist(clauses_to_boxlog(KB,Why),NCFs,ListOfLists),
   flatten([ListOfLists],Flattened),
   as_prolog(Flattened,FlattenedL),
   list_to_set(FlattenedL,FlattenedS),
   must_maplist(demodal_sents(KB),FlattenedS,FlattenedO))),!.
  

