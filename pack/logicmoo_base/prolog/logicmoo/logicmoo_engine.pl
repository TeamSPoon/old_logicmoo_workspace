/** <module> logicmoo_i_mpred_pttp
% Provides a prolog database replacement that uses an interpretation of SNARK
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
%= n(Neg,A)
%= &(F, F)
%= v(F, F)
%= '=>'(F, F)
%= '<=>'(F, F)
%=    all(X,A)
%=    exists(X,A)
%=    atleast(X,N,A)
%=    atmost(X,N,A)
/*
:- module(logicmoo_i_snark, 
          [ 
           nnf/4, 
           pnf/3, pnf/2, cf/4,
          tsn/0,
          op(300,fx,'-'),
          op(600,xfx,'=>'),
          op(600,xfx,'<=>'),
          op(350,xfx,'xor'),
          op(400,yfx,'&'),  
          op(500,yfx,'v')
        ]). 
*/
:- nodebug(_).

:-dynamic(wid/3).

%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%
%=% 
%=%   snark_in_prolog.P
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

:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.
:- prolog_load_context(directory,Dir),asserta(user:file_search_path(logicmoo,Dir)).
:- dynamic(user:isa_pred_now_locked/0).
:- multifile(user:isa_pred_now_locked/0).

:- ensure_loaded(logicmoo(mpred/logicmoo_i_header)).
:- ensure_loaded(logicmoo(pttp/dbase_i_mpred_pttp)).

%  all(R, room(R) => exists(D, (door(D) & has(R,D))))
% for any arbitrary R, if R is a room then there exists some object D that is a door, and R has a D.
% door(sk6(_G180)):- room(_G180)
% has(_G180,sk6(_G180)):- room(_G180)
%  R is not a room if D is a door and R doesn't have D
% if there are no doors anywhere then there must not be rooms
% - room(R):- - has(R,_).

:- %(current_prolog_flag(argv,[pl|_]) -> )
     %op(400, fy, user:(nesc) ),	% Necessity, Always
     %op(400, fy, user:(poss) ),	% Possibly, Eventually
     op(400, fy, user:(cir) ),	% Next time
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

leave_as_is(V):- \+ compound(V),!.
leave_as_is('$VAR'(_)).
leave_as_is(ignore(_)).
leave_as_is(constraintExists(_,_)).
leave_as_is({}).
leave_as_is(kbMark(_)).

kb_nlit(_KB,Neg):-member(Neg,[-,~,neg,not]).

non_compound(InOut):- once(not(compound(InOut));is_ftVar(InOut)).

is_gaf(Gaf):- not(is_snark_rule(Gaf)).

:- export(is_snark_rule/1).
is_snark_rule(Var):- is_ftVar(Var),!,fail.
% is_snark_rule(_:- _):- !.
is_snark_rule(R):- get_functor(R,F,A),functor(P,F,A),snark_hook(P),!.

snark_hook(0=>0).
snark_hook(0<=>0).
snark_hook((0 & 0)).
snark_hook((0 v 0)).
snark_hook(0 <- 0).
snark_hook(~(0)).
snark_hook(-(0)).
snark_hook(n(?,0)).
snark_hook(all(+,0)).
snark_hook(exists(+,0)).
snark_hook(C):- non_compound(C),!,fail.
snark_hook(H:- _):- !,nonvar(H),!,snark_hook(H).


:- style_check(+singleton).


:- export(term_singletons/2).
term_singletons(A,Vs):- term_singletons(A,[],_,[],Vs). 
:- export(term_singletons/5).
term_singletons(Fml, NS,NS, S,S):- atomic(Fml),!.
term_singletons(Fml, NS,NS, S,S):- identical_member(Fml,NS),!.
term_singletons(Fml, NS, [Fml|NS], S, NSV):- is_ftVar(Fml),identical_member(Fml,S),!,delete_eq(S,Fml,NSV),!.
term_singletons(Fml, NS, NS, S, [Fml|S]):- is_ftVar(Fml),!.
term_singletons([H|T],NS,NSO,S,NSV):- !, term_singletons(H,NS,NSM,S,M),term_singletons(T,NSM,NSO,M,NSV).
term_singletons(Fml, NS,NSO, S,NSV):- compound(Fml),Fml=..[_,H|T],!, term_singletons(H,NS,NSM,S,M),term_singletons(T,NSM,NSO, M,NSV).

subst_eq(A,B,C,D):- subst(A,B,C,D).

get_kv(X=Y,X,Y):- !.
get_kv(KV,X,Y):- functor(KV,_,1),KV=..[X,Y],!.
get_kv(KV,X,Y):- arg(1,KV,X),arg(2,KV,Y),!.

:- export(subsT_each/4).
subsT_each(_,In,[],In):- !.
subsT_each(each,In,[KV|TODO],Out):- !,get_kv(KV,X,Y),subst_eq(In,X,Y,Mid),subsT_each(each,Mid,TODO,Out),!.
subsT_each(REV,In,[KV|TODO],Out):- !,get_kv(KV,X,Y),subst_eq(In,Y,X,Mid),subsT_each(REV,Mid,TODO,Out),!.

contains_var_lits(Fml,Var,Lits):- findall(Lit,contains_t_var(Fml,Var,Lit),Lits).

get_isa(Lit,I,TT):- compound(Lit),get_isa0(Lit,I,TT).
get_isa0(isa(I,T),I,TT):- to_iname(T,TT),!.
get_isa0(IT,I,TT):- IT=..[T,I],is_colection_name(IT,T,TT),!.

is_colection_name(_,-,_):- !,fail.
is_colection_name(IT,T,TT):- atom_length(T,TL),TL>2,not(atom_contains(T,'_')),not(predicate_property(IT,_)),to_iname(T,TT).

:- dynamic(mudEquals/2).
:- export(mudEquals/2).
mudEquals(X,Y):- X=Y.

:- export(not_mudEquals/2).
:- dynamic(not_mudEquals/2).
not_mudEquals(X,Y):- X \= Y.

contains_type_lits(Fml,Var,Lits):- findall(T,(contains_t_var(Fml,Var,Lit),get_isa(Lit,O,T),same_var(O,Var)),Lits).
contains_t_var(Fml,Var,Term):- each_subterm(Fml,Term),compound(Term),arg(_,Term,O),same_var(O,Var).

:- export(type_of_var/3).
type_of_var(Fml,Var,Type):- contains_type_lits(Fml,Var,Lits),!,(member(Type,Lits)*->true;Type='Unk').

to_dlog_ops([
       'theExists'='exists',
       'ex'='exists',
       'forAll'='all',
       'forall'='all',
       ';'='v',
       ','='&',
       '~'='-',
     'not'='-',      
     'neg'='-',
     'naf'='-',
     'and'='&',
      'or'='v',
      ':-'=':-',
      '<='=':-',
 'implies'='=>',
 'implies_fc'='=>',
 'implies_bc'=':-',
   'equiv'='<=>',
      '=>'='=>',
     '<=>'='<=>']).

to_symlog_ops([
   ';'='v',
   ','='&',
   '=>'='=>',
   '<=>'='<=>',
   '~'='-',
   ':-'=':-']).

to_prolog_ops([
   'v'=';',
   '&'=',',   
   '=>'='=>',
   '<=>'='<=>',
   '-'='not',
   ':-'=':-']).


to_nonvars(_Type,IN,IN):- is_ftVar(IN),!.
to_nonvars(Type,IN,OUT):- is_list(IN),!,maplist(to_nonvars(Type),IN,OUT),!.
to_nonvars(Type,IN,OUT):- call(Type,IN,OUT),!.

convertAndCall(Type,Call):- fail, Call=..[F|IN],maplist(to_nonvars(Type),IN,OUT), IN \=@= OUT, !, must(apply(F,OUT)).

as_dlog(Fml,Fml):- leave_as_is(Fml),!.
as_dlog(or(X,Y,Z),FmlO):- !,as_dlog(v(X,v(Y,Z)),FmlO),!.
as_dlog(and(X,Y,Z),FmlO):- !,as_dlog(&(X,&(Y,Z)),FmlO),!.
as_dlog(Fml,FmlO):- to_dlog_ops(OPS),subsT_each(each,Fml,OPS,FmlO),!.

as_symlog(Fml,Fml):- is_ftVar(Fml),!.
as_symlog(Fml,FmlO):- as_dlog(Fml,FmlM),to_symlog_ops(OPS),subsT_each(each,FmlM,OPS,FmlO).

:- dynamic(thglobal:as_prolog/2).
thglobal:as_prolog(Fml,Fml):- is_ftVar(Fml),!.
thglobal:as_prolog(Fml,FmlO):- as_symlog(Fml,FmlM),
  to_prolog_ops(OPS),subsT_each(each,FmlM,OPS,FmlO).


is_modal(n(_Neg,MODAL),BDT):- !, compound(MODAL),is_modal(MODAL,BDT),nonvar(BDT).
is_modal(MODAL,BDT):- compound(MODAL), (MODAL = nesc(BDT,_) ; MODAL = poss(BDT,_)),!,nonvar(BDT).

%=% Negation Normal Form

% Usage: nnf(Neg,+KB,+Orig,+Fml, ?NNF)

nnf(Neg,Fml,NNF):- copy_term(Fml,Orig), 
  nnf(Neg,_KB,Orig,Fml,NNF).

nnf(Neg,KB,Orig,Fml,NNF):-   
   nnf(Neg,KB,Orig,Fml,[],NNF,_),!.

skolem_setting(nnf).
%skolem_setting(removeQ).
%skolem_setting(eliminate).
%skolem_setting(ignore).
% skolem_setting(label).
%skolem_setting(leavein).

% Converts to syntax that NNF/DNF/CNF/removeQ like
adjust_kif(V,V):- is_ftVar(V),!.
adjust_kif(A,A):- \+ compound(A),!.
adjust_kif(n(N,Kif),n(N,KifO)):- !,adjust_kif(Kif,KifO).
adjust_kif(nesc(N,Kif),nesc(N,KifO)):- !,adjust_kif(Kif,KifO).
adjust_kif(poss(N,Kif),poss(N,KifO)):- !,adjust_kif(Kif,KifO).
adjust_kif(-(Kif),n((-),KifO)):- !,adjust_kif(Kif,KifO).
adjust_kif(not(Kif),n((-),KifO)):- !,adjust_kif(Kif,KifO).
adjust_kif(poss_not(Kif),poss(b_d(nesc,poss),n((-),KifO))):- !,adjust_kif(Kif,KifO).
adjust_kif(nesc_not(Kif),nesc(b_d(nesc,poss),n((-),KifO))):- !,adjust_kif(Kif,KifO).
adjust_kif(not_poss(Kif),n(-,poss(KifO))):- !,adjust_kif(Kif,KifO).
adjust_kif(not_nesc(Kif),n(-,nesc(KifO))):- !,adjust_kif(Kif,KifO).
adjust_kif(poss(Kif),poss(b_d(nesc,poss),KifO)):- !,adjust_kif(Kif,KifO).
adjust_kif(nesc(Kif),nesc(b_d(nesc,poss),KifO)):- !,adjust_kif(Kif,KifO).
adjust_kif(exists(L,Expr),               ExprO):-L==[],!,adjust_kif(Expr,ExprO).
adjust_kif(exists([L|List],Expr),exists(L,ExprO)):-is_list(List),!,adjust_kif(exists(List,Expr),ExprO).
adjust_kif(exists(L,Expr),               ExprO):- \+ contains_var(L,Expr),!,adjust_kif(Expr,ExprO).
adjust_kif(exists(L,Expr),exists(L,ExprO)):-!,adjust_kif(Expr,ExprO).
adjust_kif(all(L,Expr),               ExprO):-L==[],!,adjust_kif(Expr,ExprO).
adjust_kif(all([L|List],Expr),all(L,ExprO)):-is_list(List),!,adjust_kif(exists(List,Expr),ExprO).
adjust_kif(all(L,Expr),               ExprO):- \+ contains_var(L,Expr),!,adjust_kif(Expr,ExprO).
adjust_kif(all(L,Expr),all(L,ExprO)):-!,adjust_kif(Expr,ExprO).
adjust_kif('&'([L|Ist]),ConjO):- is_list([L|Ist]),list_to_conjuncts('&',[L|Ist],Conj),adjust_kif(Conj,ConjO).
adjust_kif('v'([L|Ist]),ConjO):- is_list([L|Ist]),list_to_conjuncts('v',[L|Ist],Conj),adjust_kif(Conj,ConjO).
adjust_kif(([L|Ist]),ConjO):- is_list([L|Ist]),list_to_conjuncts('&',[L|Ist],Conj),adjust_kif(Conj,ConjO).
adjust_kif(PAB,PABO):- PAB=..[P|AB],maplist(adjust_kif,AB,ABO),PABO=..[P|ABO].



%====== drive negation inward ===
%  nnf(Neg,KB, Orig,+Fml,+FreeV,-NNF,-Paths)
%
% Fml,NNF:    See above.
% FreeV:      List of free variables in Fml.
% Paths:      Number of disjunctive paths in Fml.

nnf(_Neg,_KB,_Orig,Lit,FreeV,Lit,1):- is_ftVar(Lit),!,ignore(FreeV=[Lit]).
nnf(Neg,KB, Orig,Lit,FreeV,Pos,Paths):- is_ftVar(Lit),!,nnf(Neg,KB,Orig,proven_t(Lit),FreeV,Pos,Paths).

nnf(_,_, _,Var, _ ,Var,1):- leave_as_is(Var),!.

nnf(Neg,KB, Orig,Lit,FreeV,Pos,1):- is_ftVar(Lit),!,wdmsg(warn(nnf(Neg,KB, Orig,Lit,FreeV,Pos,1))),Pos=proven_t(Lit).

nnf(Neg,KB, Orig,Fin,FreeV,NNF,Paths):- Fin\=nesc(_,_),is_b(nesc(BDT),Fin,F),!,nnf(Neg,KB, Orig,nesc(BDT,F),FreeV,NNF,Paths).
nnf(Neg,KB, Orig,Fin,FreeV,NNF,Paths):- Fin\=poss(_,_),is_b(poss(BDT),Fin,F),!,nnf(Neg,KB, Orig,poss(BDT,F),FreeV,NNF,Paths).
nnf(Neg,KB, Orig,-(Fin),FreeV,NNF,Paths):- nnf(Neg,KB, Orig,n(Neg,Fin),FreeV,NNF,Paths).

nnf(Neg,KB, Orig,n(NegM,Fin),FreeV,NNF,Paths):- NegM\==Neg,!, nnf(NegM,KB, Orig,n(NegM,Fin),FreeV,NNF,Paths).

nnf(Neg,KB, Orig,Fin,FreeV,BOX,Paths):- is_b(nesc(BDT),Fin,F), !,
	nnf(Neg,KB, Orig,F,FreeV,NNF,Paths), cnf(Orig,NNF,CNF), boxRule(Orig,nesc(BDT,CNF), BOX).

nnf(Neg,KB, Orig,Fin,FreeV,DIA,Paths):- is_b(poss(BDT),Fin,F), !,
	nnf(Neg,KB, Orig,F,FreeV,NNF,Paths), dnf(Orig,NNF,DNF), diaRule(Orig,poss(BDT,DNF), DIA).

nnf(Neg,KB, Orig,Fin,FreeV,CIR,Paths):- is_b(cir(CT),Fin,F), !,
	nnf(Neg,KB, Orig,F,FreeV,NNF,Paths), cirRule(Orig,cir(CT,NNF), CIR).

nnf(Neg,KB, Orig,nesc(BDT,F),FreeV,BOX,Paths):- !,trace, show_call(nnf(Neg,KB, Orig,F,FreeV,NNF,Paths), cnf(Orig,NNF,CNF), boxRule(Orig,nesc(BDT,CNF), BOX)).
nnf(Neg,KB, Orig,poss(BDT,F),FreeV,DIA,Paths):- !,trace, show_call(nnf(Neg,KB, Orig,F,FreeV,NNF,Paths), dnf(Orig,NNF,DNF), diaRule(Orig,poss(BDT,DNF), DIA)).
nnf(Neg,KB, Orig,  cir(CT,F),FreeV,CIR,Paths):- !,trace, show_call(nnf(Neg,KB, Orig,F,FreeV,NNF,Paths), cirRule(Orig,cir(CT,NNF), CIR)).


nnf(Neg,KB, Orig,until(A,B),FreeV,NNF,Paths):- !,
	nnf(Neg,KB, Orig,A,FreeV,NNF1,Paths1),
	nnf(Neg,KB, Orig,B,FreeV,NNF2,Paths2),
	Paths is Paths1 + Paths2,
	NNF = until(NNF1, NNF2).


% ==== quantifiers ========
nnf(Neg,KB, Orig,all(X,NNF),FreeV,all(X,NNF2),Paths):-  
     list_to_set([X|FreeV],NewVars),
      nnf(Neg,KB, Orig,NNF,NewVars,NNF2,Paths).

nnf(Neg,KB, Orig,exists(X,Fml),FreeV,NNF,Paths):-  \+ contains_var(X,Fml),!,nnf(Neg,KB, Orig,Fml,FreeV,NNF,Paths).

nnf(Neg,KB, Orig,exists(X,Fml),FreeV,NNF,Paths):- skolem_setting(nnf),!, wdmsg(nnf(Neg,skolemizing(exists(X,Fml)))),
   must(skolem(KB,Orig,Fml,X,FreeV,FmlSk)),
   must(nnf(Neg,KB, Orig,FmlSk,FreeV,NNF,Paths)).

nnf(Neg,KB, Orig,exists(X,Fml),FreeV,NNF,Paths):- skolem_setting(label),
   list_to_set([X|FreeV],NewVars),
     delete(NewVars,X,NewNewVars),
     subst(Fml,X,theThis,FmlNoSk),
	must(nnf(Neg,KB, Orig,(ignore(X =skF(NewNewVars,FmlNoSk)) => Fml),NewVars,NNF,Paths)).

nnf(Neg,KB, Orig,exists(X,Fml),FreeV,NNF,Paths):- skolem_setting(ignore),
   list_to_set([X|FreeV],NewVars),
    nnf(Neg,KB, Orig,Fml,NewVars,NNF,Paths).

nnf(Neg,KB, Orig,exists(X,Fml),FreeV,exists(X,NNF),Paths):- (skolem_setting(removeQ);skolem_setting(leave)),
   list_to_set([X|FreeV],NewVars),
    nnf(Neg,KB, Orig,Fml,NewVars,NNF,Paths).

nnf(Neg,KB, Orig,atleast(1,X,Fml),FreeV,NNF,Paths):- !,
	nnf(Neg,KB, Orig,exists(X,Fml),FreeV,NNF,Paths).

nnf(Neg,KB, Orig,atleast(N,X,Fml),FreeV,NNF,Paths):- 
	!,
	NewN is N - 1,
        subst_eq(Fml,X,Y,FmlY),
	nnf(Neg,KB, Orig,&(exists(X,Fml),atleast(NewN,Y,FmlY)),FreeV,NNF,Paths).
nnf(Neg,KB, Orig,atmost(1,X,Fml),FreeV,NNF,Paths):- 
	!,
        subst_eq(Fml,X,Y,FmlY),
        subst_eq(Fml,X,Z,FmlZ),
	nnf(Neg,KB, Orig,n(Neg,&(exists(Y,FmlY),exists(Z,FmlZ))),FreeV,NNF,Paths).
nnf(Neg,KB, Orig,atmost(N,X,Fml),FreeV,NNF,Paths):- 
	!,
        subst_eq(Fml,X,Y,FmlY),
	NewN is N - 1,
	nnf(Neg,KB, Orig,&(exists(Y,FmlY),atmost(NewN,X,Fml)),FreeV,NNF,Paths).

nnf(NegIn,KB, Orig,n(Neg,xor(X , Y)),FreeV,NNF,Paths):- 
   !,
   nnf(NegIn,KB, Orig,v(&(X , Y) , &(n(Neg,X) , n(Neg,Y))),FreeV,NNF,Paths).
   
nnf(Neg,KB, Orig,xor(X , Y),FreeV,NNF,Paths):- 
   !,
   nnf(Neg,KB, Orig,&(v(X , Y) , v(n(Neg,X) , n(Neg,Y))),FreeV,NNF,Paths).
   

nnf(Neg,KB, Orig,&(A,B),FreeV,NNF,Paths):- !,
	nnf(Neg,KB, Orig,A,FreeV,NNF1,Paths1),
	nnf(Neg,KB, Orig,B,FreeV,NNF2,Paths2),
	Paths is Paths1 * Paths2,
	(Paths1 > Paths2 -> NNF = &(NNF2,NNF1);
		            NNF = &(NNF1,NNF2)).

nnf(Neg,KB, Orig,v(A,B),FreeV,NNF,Paths):- !,
        nnf(Neg,KB, Orig,A,FreeV,NNF1,Paths1),
	nnf(Neg,KB, Orig,B,FreeV,NNF2,Paths2),
	Paths is Paths1 + Paths2,
	(Paths1 > Paths2 -> NNF = v(NNF2,NNF1);
		            NNF = v(NNF1,NNF2)).

nnf(Neg,KB, Orig,Fml,FreeV,NNF,Paths):- 
	(Fml = n(Neg,n(Neg,A)) -> Fml1 = A;
	 Fml = n(Neg,nesc(BDT,F)) -> Fml1 = poss(BDT,n(Neg,F));
	 Fml = n(Neg,poss(BDT,F)) -> Fml1 = nesc(BDT,n(Neg,F));
	 Fml = n(Neg,cir(CT,F)) -> Fml1 = cir(CT,n(Neg,F));
	 Fml = n(Neg,until(A,B)) -> (nnf(Neg,KB, Orig,n(Neg,A),FreeV,NNA,_), nnf(Neg,KB, Orig,n(Neg,B),FreeV,NNB,_),
                                     Fml1 = v(all(NNB), until(NNB,&(NNA,NNB))));
	 Fml = n(Neg,all(X,F)) -> Fml1 = exists(X,n(Neg,F));
	 Fml = n(Neg,exists(X,F)) -> Fml1 = all(X,n(Neg,F));

	 Fml = n(Neg,atleast(N,X,F)) -> Fml1 = atmost(N,X,F);
	 Fml = n(Neg,atmost(N,X,F)) -> Fml1 = atleast(N,X,F);

	 Fml = n(Neg,v(A,B)) -> Fml1 = &(n(Neg,A), n(Neg,B) );
	 Fml = n(Neg,&(A,B)) -> Fml1 = v(n(Neg,A), n(Neg,B) );
	 Fml = '=>'(A,B) -> Fml1 = v(n(Neg,A), B );
	 Fml = n(Neg,'=>'(A,B)) -> Fml1 = &(A, n(Neg,B) );
         Fml = '<=>'(A,B) -> Fml1 = v('=>'(A, B), '=>'(B, A) );
	 Fml = '<=>'(A,B) -> Fml1 = v(&(A, B), &(n(Neg,A), n(Neg,B)) );
	 Fml = n(Neg,'<=>'(A,B)) -> Fml1 = v(&(A, n(Neg,B)) , &(n(Neg,A), B) )
	),!,
	nnf(Neg,KB, Orig,Fml1,FreeV,NNF,Paths).


/*
nnf(Neg,KB, _Orig,Fml,_,Fml,1):- Fml=..[F,KB,_],third_order(F),!.
nnf(Neg,KB,  Orig,Fml,FreeV,Out,Path):- Fml=..[F,A],third_order(F),  
  nnf(Neg,KB, Orig,A,FreeV,NNF1,Path1),!,
  Fml2=..[F,KB,NNF1],nnf(Neg,KB, Orig,Fml2,FreeV,Out,Path2),Path is Path1+Path2.
*/

nnf(Neg,KB, Orig, IN,FreeV,OUT,Paths):- simplify_cheap(IN,MID),IN\=MID,nnf(Neg,KB, Orig, MID,FreeV,OUT,Paths).
nnf(Neg,KB, Orig,[F|Fml],FreeV,Out,Paths):- arg(_,v((v),(&),(=>),(<=>)),F),nnf(Neg,KB, Orig,Fml,FreeV,NNF,Paths),Out =..[F| NNF].
nnf(_ , _, _    , IN,[],OUT,1):- mnf(IN,OUT),IN\=OUT,!.
nnf(Neg,KB,_Orig,Fml,_,FmlO,1):- nonegate(Neg,KB,Fml,FmlO),!.


is_lit_atom(IN):- leave_as_is(IN),!.
is_lit_atom(IN):- subst(IN,'&','*',M),subst(M,'v','*',O),!,O==IN.

mnf(Var,Var):-leave_as_is(Var),!.
mnf(Fml,Out):-boxRule(_,Fml,M),Fml\=M,mnf(M,Out).
mnf(Fml,Out):-diaRule(_,Fml,M),Fml\=M,mnf(M,Out).
mnf(poss(DBT,A=>B),Out):- diaRule(_,poss(DBT,v( neg(-,B),A)),M),mnf(M,Out).
mnf(nesc(DBT,A=>B),Out):- mnf(v( neg(-,nesc(DBT, B)), nesc(DBT,A)),M),mnf(M,Out).
mnf([F|Fml],Out):- arg(_,v((v),(&),(=>),(<=>)),F),mnf(Fml,NNF),Out =..[F| NNF].
mnf(Var,Var):-!.


% poss(P=>Q) ==>  poss( - Q v P )  ==>  - nesc( - ( - Q v P ) ) ==>  - nesc( Q & -P  )    .. how can i get the  nesc/poss very close to the P and Q ?

% poss(P=>Q)  ==>   ( -nesc(-P) =>  -nesc(-Q) )   ?

% poss(P=>Q)  ===>   poss( - Q v P ) ===>   poss(- Q) v poss(P)  ===>   - nesc(Q) v poss(P)   ===>      poss(P)=>nesc(Q)  

% poss(DBT,v( neg(-,B),A)) => -nesc(q & -p)

third_order(asserted_t).


boxRule(Orig,A,B):- convertAndCall(as_dlog,boxRule(Orig,A,B)).
boxRule(Orig,nesc(BDT,&(A,B)), &(BA,BB)):- !, boxRule(Orig,nesc(BDT,A),BA), boxRule(Orig,nesc(BDT,B),BB).
boxRule(_Orig,nesc(BDT, IN), BOX):- \+ is_lit_atom(IN),  nnf(Neg,n(Neg,nesc(BDT, n(Neg,IN))),BOX).
boxRule(_Orig,BOX, BOX).
 
diaRule(Orig,A,B):- convertAndCall(as_dlog,diaRule(Orig,A,B)).
diaRule(Orig,poss(BDT,v(A,B)), v(DA,DB)):- !, diaRule(Orig,poss(BDT,A),DA), diaRule(Orig,poss(BDT,B),DB).
diaRule(_Orig,DIA, DIA).

cirRule(Orig,A,B):- convertAndCall(as_dlog,cirRule(Orig,A,B)).
cirRule(Orig,cir(CT,v(A,B)), v(DA,DB)):- !, cirRule(Orig,cir(CT,A),DA), cirRule(Orig,cir(CT,B),DB).
cirRule(Orig,cir(CT,&(A,B)), &(DA,DB)):- !, cirRule(Orig,cir(CT,A),DA), cirRule(Orig,cir B,DB).
cirRule(_Orig,CIR, CIR).


is_b(nesc(b_d(B,D)),BF,F):- BF=..[B,F],b_d(B,D).
is_b(poss(b_d(B,D)),DF,F):- DF=..[D,F],b_d(B,D).
is_b(nesc(b_d(B,D)),nesc(b_d(B,D),F),F):- b_d(B,D).
is_b(poss(b_d(B,D)),poss(b_d(B,D),F),F):- b_d(B,D).
is_b(nesc(b_d(B,D)),nesc(B,F),F):- b_d(B,D).
is_b(poss(b_d(B,D)),poss(D,F),F):- b_d(B,D).
is_b(cir(ct(CT)),CF,F):- CF=..[CT,F],ct(CT).
is_b(cir(ct(CT)),cir(CT,F),F):- ct(CT).

ct(cir).
ct(asserted_t).

b_d(box,poss).
b_d(nesc,poss).
b_d(knows,beliefs).
b_d(always,sometimes).
% b_d(A,I):- genlPreds(I,A).

%=%
%=%  Conjunctive Normal Form (CNF) : assumes Fml in NNF
%=%

% Usage: cnf(Orig, +NNF, ?CNF )
cnf(A,B):- copy_term(A,Orig),cnf(Orig,A,B).
cnf(Orig,A,B):- convertAndCall(as_dlog,cnf(Orig,A,B)).
cnf(Orig,&(P,Q), &(P1,Q1)):- !, cnf(Orig,P, P1), cnf(Orig,Q, Q1).
cnf(Orig,v(P,Q),     CNF):- !, cnf(Orig,P, P1), cnf(Orig,Q, Q1), cnf1(Orig, v(P1,Q1), CNF ).
cnf(_Orig,CNF,       CNF).

cnf1(Orig, v(&(P,Q), R), &(P1,Q1) ):- !, cnf1(Orig, v(P,R), P1), cnf1(Orig, v(Q,R), Q1).
cnf1(Orig, v(P, &(Q,R)), &(P1,Q1) ):- !, cnf1(Orig, v(P,Q), P1), cnf1(Orig, v(P,R), Q1).
cnf1(_Orig, CNF,                 CNF).


%=%
%=% Disjunctive Normal Form (DNF) : assumes Fml in NNF
%=%
% Usage: dnf(Orig, +NNF, ?DNF )
dnf(A,B):- copy_term(A,Orig),dnf(Orig,A,B).
dnf(Orig,A,B):- convertAndCall(as_dlog,dnf(Orig,A,B)).
dnf(Orig, v(P,Q),  v(P1,Q1) ):- !, dnf(Orig,P, P1), dnf(Orig,Q, Q1).
dnf(Orig, &(P,Q), DNF):- !, dnf(Orig,P, P1), dnf(Orig,Q, Q1), dnf1(Orig,&(P1,Q1), DNF).
dnf(_Orig,DNF,       DNF).

dnf1(Orig,&(P, v(Q,R)),  v(P1,Q1) ):- !, dnf1(Orig,&(P,Q), P1), dnf1(Orig,&(P,R), Q1).
dnf1(Orig,&(v(P,Q), R), v(P1,Q1) ):- !, dnf1(Orig,&(P,R), P1), dnf1(Orig,&(Q,R), Q1).
dnf1(_Orig,DNF,                  DNF ).


simplify_cheap(IN,OUT):- IN = nesc(BDT,OUT),is_modal(OUT,BDT),!.
simplify_cheap(poss(BDT,nesc(BDT,IN)),OUT):- simplify_cheap_must(poss(BDT,IN),OUT).
simplify_cheap(poss(BDT,poss(BDT,IN)),OUT):- simplify_cheap_must(poss(BDT,IN),OUT).
simplify_cheap(nesc(BDT,poss(BDT,IN)),OUT):- simplify_cheap_must(poss(BDT,IN),OUT).
simplify_cheap(n(BDT,n(BDT,IN)),OUT):- simplify_cheap_must(IN,OUT).
simplify_cheap(n(Neg, poss(BDT, poss(BDT, F))), n(Neg,F)):-nonvar(F),!.
simplify_cheap(n(Neg,poss(_, n(Neg, F))), F):-nonvar(F),!.
simplify_cheap((poss(BDT, poss(BDT, F))),  poss(BDT, F)):-nonvar(F),!.
%simplify_cheap(IN,-OUT):- IN = n(Neg,poss(BDT,OUT)), is_modal(OUT,BDT),!.
%simplify_cheap(IN,-OUT):- IN = n(Neg,nesc(BDT,OUT)), \+is_modal(OUT,BDT),!.


simplify_cheap_must(IN,OUT):- simplify_cheap(IN,OUT).
simplify_cheap_must(IN,IN).

is_sentence_functor(_).

%=
%=  Prenex Normal Form (PNF)
%=

% Usage: pnf(+Orig, +Fml, ?PNF ) : assumes Fml in NNF

pnf(A,B):- copy_term(A,Orig),pnf(Orig,A,B).

pnf(Orig, F,PNF):- pnf(Orig,F,[],PNF),!.

% pnf(+Orig, +Fml, +Vars, ?PNF)

pnf(A,B,C,D):- convertAndCall(as_dlog,pnf(A,B,C,D)),!.

pnf(_,Var,_ ,Var):- leave_as_is(Var),!.

pnf(_, [],  _,           []):- !.

pnf(_, IN,  _,              OUT):- is_list(IN),!, maplist(pnf,IN,OUT).

pnf(Orig, IN, FreeV,              OUT):- once(mnf(IN,MID)),IN\=@=MID, pnf(Orig,MID,FreeV,OUT).
pnf(Orig, IN, FreeV,              OUT):- simplify_cheap(IN,MID), pnf(Orig,MID,FreeV,OUT).

pnf(Orig,   all(X,F),Vs,   all(X,PNF)):- list_to_set([X|Vs],VVs), !, pnf(Orig,F, VVs, PNF).

pnf(Orig,   nesc(X,F),Vs,   nesc(X,PNF)):- !, pnf(Orig,F,Vs, PNF).

pnf(Orig,   poss(X,F),Vs,   poss(X,PNF)):- !, pnf(Orig,F,Vs, PNF).

pnf(Orig,  exists(X,F),Vs,exists(X,PNF)):- list_to_set([X|Vs],VVs), !, pnf(Orig,F, VVs, PNF).

pnf(Orig,  &(exists(X,A) , B),Vs,  exists(Y,PNF)):- !, copy_term((X,A,Vs),(Y,Ay,Vs)), pnf(Orig,&(Ay,B),[Y|Vs], PNF).

pnf(Orig,    v(exists(X,A), B),Vs,  exists(Y,PNF)):- !, copy_term((X,A,Vs),(Y,Ay,Vs)), pnf(Orig,v(Ay,B),[Y|Vs], PNF).

pnf(Orig, &(all(X,A), B),Vs, all(Y,PNF)):- !, copy_term((X,A,Vs),(Y,Ay,Vs)), pnf(Orig,&(Ay , B),[Y|Vs], PNF).

pnf(Orig, v(all(X,A), B),Vs, all(Y,PNF)):- !, copy_term((X,A,Vs),(Y,Ay,Vs)), pnf(Orig,v(Ay,B),[Y|Vs], PNF).

pnf(Orig, &(A,exists(X,B)),Vs,  exists(Y,PNF)):- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnf(Orig,&(A, By),[Y|Vs], PNF).
pnf(Orig, v(A,exists(X,B)),Vs,  exists(Y,PNF)):- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnf(Orig,v(A,By),[Y|Vs], PNF).
pnf(Orig, &(A,all(X,B)),Vs, all(Y,PNF)):- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnf(Orig,&(A,By),[Y|Vs], PNF).
pnf(Orig, v(A,all(X,B)),Vs, all(Y,PNF)):- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnf(Orig,v(A,By),[Y|Vs], PNF).

pnf(Orig, &(A, B),Vs,       PNF ):- pnf(Orig,A,Vs,Ap), pnf(Orig,B,Vs,Bp), 
                                     (A\=Ap; B\=Bp), pnf(Orig,&(Ap,Bp),Vs,PNF).

pnf(Orig, v(A, B),Vs,       PNF ):- pnf(Orig,A,Vs,Ap), pnf(Orig,B,Vs,Bp), 
                                     (A\=Ap; B\=Bp), pnf(Orig,v(Ap,Bp),Vs,PNF).


pnf(Orig, [A|B], Vs,       PNF ):- !, pnf(Orig,A,Vs,Ap), pnf(Orig,B,Vs,Bp), 
                                     (A\=Ap; B\=Bp), pnf(Orig,[Ap|Bp],Vs,PNF).


pnf(Orig, H,Vars,FOO ):-  compound(H),H=..[F|ARGS], is_sentence_functor(F), !, pnf(Orig, [F|ARGS],Vars,FOOL ),FOO=..FOOL.

pnf(_Orig,          PNF, _,       PNF ).

%=%  Clausal Form (CF) : assumes Fml in PNF and
%                                 each quantified variable is unique

% cf(KB, Orig,+Fml, ?Cs)
% Cs is a list of the form: [cl(Head,Body), ...]
% Head and Body are lists.

% cf(KB,A,B,C):- convertAndCall(as_dlog,cf(KB,A,B,C)).
cf(KB, Orig,PNF, SET):- removeQ(KB, Orig,PNF,[], UnQ), 
  cnf(Orig,UnQ,CNF),!,  
  conjuncts_to_list(CNF,Conj),!,% display_form(conj:-Conj),
  %nnf(-,-CNF,DisjN),dnf(DisjN,DNF),!, %display_form(dnf:-DNF),
  %disjuncts_to_list(DNF,Disj),!,% display_form(disj:-Disj),
  make_clause_set(KB,Conj,EachClause),!,
  maplist(correct_cls,EachClause,SOO),
  expand_cl(SOO,SOOO),
  list_to_set(SOOO,SET),!.

removeQ(F,  HH):- removeQ(_KB, _, F, _, RQ0),!,RQ0=HH.

% removes quantifiers (also pushes modal operators inside the negations) 

removeQ(_,_,Var,_ ,Var):- leave_as_is(Var),!.
removeQ(_KB, _Orig, F,_, RQ0):- is_list(F),!,maplist(removeQ,F,HH),!,HH=RQ0.


removeQ(KB, Orig, IN,FreeV,OUT):-  simplify_cheap(IN,MID), IN\=@=MID, removeQ(KB, Orig, MID,FreeV,OUT).

removeQ(KB, Orig, H, Vars, HH ):- convertAndCall(as_dlog,removeQ(KB, Orig,H, Vars, HH )).
removeQ(KB, Orig, n(Neg,n(Neg,F)),Vars, XF):- !, removeQ(KB, Orig,  F,Vars, XF) .
removeQ(KB, Orig, all(X,F),Vars, HH):- !,  removeQ(KB, Orig,F,[X|Vars], RQ0),RQ0=HH.

/*
removeQ(KB, Orig, n(Neg,nesc(b_d(B,X), n(Neg,F))),Vars, XF):- !,removeQ(KB, Orig, poss(b_d(B,X), F),Vars, XF).
removeQ(KB, Orig, n(Neg,poss(b_d(B,X), n(Neg,F))),Vars, XF):- !,removeQ(KB, Orig, nesc(b_d(B,X), F),Vars, XF).

removeQ(KB, Orig, n(Neg,nesc(b_d(B,X), (F))),Vars, XF):- !,removeQ(KB, Orig, poss(b_d(B,X), n(Neg,F)),Vars, XF).
removeQ(KB, Orig, n(Neg,poss(b_d(B,X), (F))),Vars, XF):- !,removeQ(KB, Orig, nesc(b_d(B,X), n(Neg,F)),Vars, XF).
*/

removeQ(KB, Orig, nesc(b_d(B,X), n(Neg,F)),Vars, XF):- !,removeQ(KB, Orig, n(Neg,poss(b_d(B,X), F)),Vars, XF).
removeQ(KB, Orig, poss(b_d(B,X), n(Neg,F)),Vars, XF):- !,removeQ(KB, Orig, n(Neg,nesc(b_d(B,X), F)),Vars, XF).

removeQ(KB, Orig,  exists(X,F),Vars, HH):- skolem_setting(removeQ),!,wdmsg(removeQ(skolemizing(exists(X,F)))),
	skolem(KB, Orig,F,X,Vars,Fsk),
	removeQ(KB, Orig,Fsk,Vars, HH).

removeQ(KB, Orig, exists(X,F),Vars, HH):- !,  removeQ(KB, Orig,F,[X|Vars], RQ0),RQ0=HH.

removeQ(KB, Orig, ':-'(H,B), Vars, ':-'(HH,BB ) ):- !, removeQ(KB, Orig,H, Vars, HH ),removeQ(KB, Orig,B, Vars, BB).
removeQ(_, _, cl(H,B), _, O ):- !,correct_cls(cl(H,B),O).
removeQ(KB, Orig,     [ H|B ],Vars, [ HH|BB ] ):- !,removeQ(KB, Orig,H, Vars, HH ),removeQ(KB, Orig,B, Vars, BB).


removeQ(KB,_Orig, F,_,FOO ):- kb_nlit(KB,Neg),once(nnf(Neg,F,FO)),F\=@=FO, loop_check(removeQ(KB,_,FO,_,FOO)).

removeQ(KB,Orig, H,Vars,HH ):- compound(H),H=..[F|ARGS],!,removeQ(KB,Orig, ARGS,Vars,ARGSO ),HH=..[F|ARGSO].

removeQ(_,_Orig, F,_,F0 ):- F=F0.

display_form(Form):- demodal_sents(Form,Out),portray_clause(Out).

demodal_sents(I,O):-transitive_lc(demodal_sents0,I,O).

demodal_sents0(I,O):-demodal(I,M),modal2sent(M,O),!.

demodal(Var, Var):- leave_as_is(Var),!.
demodal(n(Neg,H), HHH):- must(atom(Neg)),demodal(H, HH),!,HHH=..[Neg,HH].
demodal(nesc(b_d(X,_),F), XF):- !,demodal(F, HH), XF =..[X,HH].
demodal(poss(b_d(_,X),F), XF):- !,demodal(F, HH), XF =..[X,HH].
demodal(neg(H),not(HH)):- nonvar(H),demodal(H,HH).
demodal(-(H),not(HH)):- nonvar(H),demodal(H,HH).
demodal([H|T],[HH|TT]):- !, must(( demodal(H,HH),demodal(T,TT))),!.
demodal(H,HH ):- H=..[F|ARGS],!,maplist(demodal,ARGS,ARGSO),!,HH=..[F|ARGSO].

is_sent_op_modality(not).
is_sent_op_modality(poss).
is_sent_op_modality(nesc).
atom_compat(F,HF,HHF):- F\=HF, is_sent_op_modality(F),is_sent_op_modality(HF), format(atom(HHF),'~w_~w',[F,HF]).

modal2sent(Var, Var):- leave_as_is(Var),!.
modal2sent(G,O):- G=..[F,H], \+ leave_as_is(H), H=..[HF,HH], atom_compat(F,HF,HHF),!, GG=..[HHF,HH], modal2sent(GG,O).
modal2sent([H|T],[HH|TT]):- !, must(( modal2sent(H,HH),modal2sent(T,TT))),!.
modal2sent(H,HH ):- H=..[F|ARGS],!,maplist(modal2sent,ARGS,ARGSO),!,HH=..[F|ARGSO].


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
inclause(KB, n(Neg, PP) , A,  A, B1, B ):- 
        negate(KB, n(Neg, PP),P),
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

to_regular_cl([(H1 & H2)],[Has],[cl([H1],H1P),cl([H2],H2P)]):- cnf(Has,HasC),  append([HasC],[poss,H2],H1P), append([HasC],[poss,H1],H2P),!.
to_regular_cl([(H1 & H2)],Has,[cl([H1],H1P),cl([H2],H2P)]):-  append(Has,[poss,H2],H1P), append(Has,[poss,H1],H2P),!.
to_regular_cl([H],[],[cl([SH],[])]):-is_lit_atom(H),simplify_atom(H,SH).
to_regular_cl(HL,BL,[cl(HL,BL)]).


expand_cl([],[]):-!.
expand_cl([cl(H,B)|O],OOut):- 
      to_regular_cl(H,B,More),!,
      expand_cl(O,OO),
      append(More,OO,OOut).

make_clause_set(_ ,[],[]).
make_clause_set(KB,[CJ|Conj],CLAUSES):-
   make_clauses(CJ,CLS),
   make_clause_set(KB,Conj,CLAUS),
   append(CLS,CLAUS,CLAUSES).

make_clauses(_,[CJ],cl([CJ],[])):-is_lit_atom(CJ).
make_clauses(CJ,OOut):- disjuncts_to_list(CJ,Conj),make_clause_from_set(_KB,Conj,OOut).

negate_one_maybe(KB,One,Neg):-negate_one(KB,One,Neg).
   
make_clause_from_set(KB,Conj,Out):- findall(E,make_each(KB,Conj,E),Out).

make_each(KB,Conj,E):- member(One,Conj), make_1_cl(KB,One,Conj,E).

make_1_cl(KB,One,Conj,cl([One],NewBodyListO)):- 
  negate_one_maybe(KB,One,NHead),!,
  One\={_}, NHead\={_},
  delete_eq(Conj,One,Rest0),delete_eq(Rest0,NHead,Rest),
  maplist(negate_one_maybe(KB),Rest,NewBodyList),!,
  flattenConjs(KB,NewBodyList,NewBodyListM),
  maplist(thglobal:as_prolog,NewBodyListM,NewBodyListO).

flattenConjs(_KB,I,O):- conjuncts_to_list(I,M),maplist(conjuncts_to_list,M,L),flatten(L,O).

is_neg(n(_Neg,_)).
is_pos(One):- get_functor(One,F),!,not(is_log_op(F)).

:- export(is_log_sent/1).
is_log_sent(S):- get_functor(S,F,_),is_log_op(F).

not_log_op(OP):- not(is_log_op(OP)).
:- export(is_log_op/1).
is_log_op(OP):- atomic(OP),to_dlog_ops(OPS),!,(member(OP=_,OPS);member(_=OP,OPS)).


:- export(logical_pos/3).
:- export(logical_neg/3).
logical_neg(KB,Wff,WffO):- 
  must(nonegate(Neg,KB,Wff,Wff1)),nnf(Neg,KB,n(Neg,Wff1),n(Neg,Wff1),Wff2),must(nonegate(Neg,KB,Wff2,WffO)),!.
logical_pos(KB,Wff,WffO):- 
  must(nonegate(Neg,KB,Wff,Wff1)),nnf(Neg,KB,Wff1,Wff1,Wff2),must(nonegate(Neg,KB,Wff2,WffO)),!.


negate_one(KB,Wff,WffO):- logical_neg(KB,Wff,WffO).


negate(KB,X,Z):- must(defunctionalize(X,Y)), must(negate0(KB,Y,Z)).
negate0(KB,n(Neg,X),X):- kb_nlit(KB,Neg).
negate0(KB,X,n(Neg,X)):- kb_nlit(KB,Neg).


mpred_quf(In,Out):- transitive(mpred_quf_0,In,Out).

mpred_quf_0(InOut,InOut):- non_compound(InOut),!.
% mpred_quf_0(In,Out):- current_predicate(db_quf/4),db_quf(change(assert,_Must),In,U,C),conjoin(U,C,Out).
mpred_quf_0(In,In).

:- export(nonegate/4).
nonegate(Neg,KB,List,OutZ):- is_list(List),maplist(nonegate(Neg,KB),List,OutZ),!.
nonegate(Neg,KB,Fml,OutZ):- simplify_cheap(Fml,Fml2), Fml \=@= Fml2,nonegate(Neg,KB,Fml2,OutZ),!.
nonegate(Neg,KB,Fml,OutZ):- must((unbuiltin_negate(Neg,KB,Fml,Out),!,defunctionalize(Out,OutY),!,must(mpred_quf(OutY,OutZ)))),!.

unbuiltin_negate(_Neg,_, Fml,Fml):- is_ftVar(Fml),!.
unbuiltin_negate(_Neg,_, Fml,Out):- get_functor(Fml,F,A),pttp_builtin(F,A),!,must(Out=Fml).
unbuiltin_negate(_Neg,KB,Fml,Out):- once(negate(KB,Fml,Neg)),negate(KB,Neg,Out),!.

%=%  Skolemizing : method 1

% Usage: skolem(+Fml,+X,+FreeV,?FmlSk)
% Replaces existentially quantified variable with the formula
% VARIABLES MUST BE PROLOG VARIABLES
% exists(X,p(X)) ==> p(p(exists))

skolem_bad(Fml,X,FreeV,FmlSk):- 
	copy_term((X,Fml,FreeV),(Fml,Fml1,FreeV)),
	copy_term((X,Fml1,FreeV),(exists,FmlSk,FreeV)).

%=%  Skolemizing : method 2

% Usage: skolem(KB, Orig, +Fml, +X, +FreeV, ?FmlSk )
% Replaces existentially quantified variable with a unique function
% fN(Vars) N=1,...
% VARIABLES MAYBE EITHER PROLOG VARIABLES OR TERMS


skolem(KB, Orig, F, X, FreeV, Out):-  fail,
   must(skolem_f(KB, Orig, F, X, FreeV, Sk)),
   subst(F,X,Sk,Out).

skolem(KB , Orig, F, X, FreeV, Out):-  
   must(skolem_f(KB, Orig, F, X, FreeV, Sk)),
   %writeq(freev(Sk,FreeV)),
   must(Out= '=>'({constraintExists(X,Sk)},F)),
   !,show_call( asserta((constraintRules(X,Sk,F)))).

skolem(KB, Orig, F, X, FreeV, FmlSk):- 
    must(skolem_f(KB, Orig, F, X, FreeV, Sk)), 
    must(subst_eq(F, X, Sk, FmlSk)),!.


skolem_f(KB, Orig, F, X, FreeVIn, Sk):- 
       must_det_l((
         delete_eq(FreeVIn,KB,FreeV0),
         delete_eq(FreeV0,X,FreeV),
         list_to_set(FreeV,FreeVSet),
	contains_var_lits(F,X,LitsList),
        mk_skolem_name(Orig,X,LitsList,'',SK),
        concat_atom(['sk',SK,'Fn'],Fun),
	Sk =..[Fun|FreeVSet])).
/*


%=% Substitution

% Usage: subst_eq(+Fml,+X,+Sk,?FmlSk)
subst_eq(Fml,X,Sk,FmlSkO):- pred_subst(==,Fml,X,Sk,FmlSk),!,must(FmlSkO=FmlSk).


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
%=% generate a skolem 

mk_skolem_name(_O,Var,Fml,SIn,SOut):- is_ftVar(Fml), same_var(Var,Fml),!,atom_concat('Is',SIn,SOut).
mk_skolem_name(_O,_V,Fml,SIn,SIn):- is_ftVar(Fml),!.
mk_skolem_name(_O ,_V,[],SIn,SIn):- !.
mk_skolem_name(_O,_V, OP,SIn,SIn):- is_log_op(OP),!.
mk_skolem_name(_O,_V,Fml,SIn,SOut):- atomic(Fml),!,i_name(Fml,N),toPropercase(N,CU),!,(atom_contains(SIn,CU)->SOut=SIn;atom_concat(SIn,CU,SOut)).
mk_skolem_name(Orig,Var,[H|T],SIn,SOut):- !,mk_skolem_name(Orig,Var,H,SIn,M),mk_skolem_name(Orig,Var,T,M,SOut).
mk_skolem_name(Orig,Var,isa(VX,Lit),SIn,SOut):- same_var(Var,VX),not_ftVar(Lit),!,mk_skolem_name(Orig,Var,['Is',Lit,'In'],'',F),atom_concat(F,SIn,SOut).
mk_skolem_name(Orig,Var,Fml,SIn,SOut):- Fml=..[F,VX],same_var(Var,VX),!,mk_skolem_name(Orig,Var,['Is',F,'In'],SIn,SOut).
mk_skolem_name(Orig,Var,Fml,SIn,SOut):- Fml=..[F,Other,VX|_],same_var(Var,VX),!,type_of_var(Orig,Other,OtherType),
   mk_skolem_name(Orig,Var,[OtherType,'Arg2Of',F],SIn,SOut).
mk_skolem_name(Orig,Var,Fml,SIn,SOut):- Fml=..[F,VX|_],same_var(Var,VX),!,mk_skolem_name(Orig,Var,['Arg1Of',F],SIn,SOut).
mk_skolem_name(Orig,Var,Fml,SIn,SOut):- Fml=..[F|_],!,mk_skolem_name(Orig,Var,['ArgNOf',F],SIn,SOut).

% same_var(Var,Fml):- not(not(Var=Fml)),!.
same_var(Var,Fml):- Var==Fml,!.


%======  make a sequence out of a disjunction =====
flatten_or_list(A,B,C):- convertAndCall(as_symlog,flatten_or_list(A,B,C)).
flatten_or_list(Orig,v(X , Y), F):- !,
   flatten_or_list(Orig,X,A),
   flatten_or_list(Orig,Y,B),
   flatten([A,B],F).
flatten_or_list(_Orig,X,[X]).



fmtl(X):- thglobal:as_prolog(X,XX), fmt(XX).

write_list([F|R]):- write(F), write('.'), nl, write_list(R).
write_list([]).

numbervars_with_names(Term):- 
   term_variables(Term,Vars),name_variables(Vars),!,
   numbervars(Term,91,_,[attvar(skip),singletons(false)]),!.

name_variables([]).
name_variables([Var|Vars]):- 
   (var_property(Var, name(Name)) -> Var = '$VAR'(Name) ; true),
   name_variables(Vars).

wdmsgl(CNF):- compound(CNF),CNF=..[NAME,NF],!,must(wdmsgl_2(NAME,NF)).
wdmsgl(NF):- must((get_functor(NF,NAME),!,must(wdmsgl_2(NAME,NF)))).


wdmsgl_2(NAME,NF):- functor(NF,_,_),wdmsgl_3(NAME,&,NF).

wdmsgl_3(NAME,F,NF):- copy_term(vv(NAME,F,NF),vv(NAME2,F2,NF2)),
   numbervars_with_names(vv(NAME2,F2,NF2)),!,
   wdmsgl_4(NAME2,F2,NF2).

wdmsgl_4(NAME,F,NF):- is_list(NF),!,list_to_set(NF,NS),maplist(wdmsgl_4(NAME,F),NS).
wdmsgl_4(NAME,F,NF):- compound(NF),NF=..[FF,A,B],FF=F,not_ftVar(A),not_ftVar(B),!,
  maplist(wdmsgl_4(NAME,F),[A,B]).
wdmsgl_4(NAME,_,NF):- as_symlog(NF,NF2), with_all_dmsg(display_form(NAME:-NF2)).
wdmsgl_4(NAME,_,NF):- as_symlog(NF,NF2), cyc:pterm_to_sterm(NF2,NF3),with_all_dmsg(display_form(NAME:-NF3)).



put_singles(Wff,_,[],Wff).
put_singles(Wff,Exists,[S|Singles],NewWff):-   
   (((each_subterm(Wff,SubTerm),compound(SubTerm),
    SubTerm=..[OtherExists,SO,_],same_var(SO,S),
     member(OtherExists,[all,exists])))
 -> WffM = Wff ; WffM =..[Exists,S,Wff]),
   put_singles(WffM,Exists,Singles,NewWff),!.


ensure_quantifiers(Wff:- B,WffO):- B== true,!, ensure_quantifiers(Wff,WffO).
ensure_quantifiers(Wff:- B,Wff:- B):- !.
ensure_quantifiers(Wff,Wff):-!.
ensure_quantifiers(Wff,WffO):-
 must_det_l((show_call(term_singletons(Wff,[],NS,[],Singles)),
  put_singles(Wff,'exists',Singles,WffM),put_singles(WffM,'all',NS,WffO))).

:- multifile(function_corisponding_predicate/2).
:- dynamic(function_corisponding_predicate/2).

get_pred(Pred,F):- get_functor(Pred,F).
is_function(F):- is_ftVar(F),!,fail.

 
is_function(Function):- compound(Function),get_functor(Function,F,A),is_function(Function,F,A).

is_function(_,'SubLQuoteFn',_):- !,fail.
is_function(_,F,_):- atom_concat('sk',_Was,F),!,fail.
is_function(_,F,_):- atom_concat(_Was,'Fn',F).
is_function(_,F,_):- tFunction(F).
is_function(_,F,A):- A2 is A+1,current_predicate(F/A2), not(current_predicate(F/A)).

%:- pfc_add(isa(I,C)<=(ttPredType(C),user:isa(I,C))).

is_ftEquality(Term):- is_ftVar(Term),!,fail.
%is_ftEquality(Term):- get_pred(Term,Pred),is),!,(Pred==mudEquals;genlPreds(Pred,equals);clause_asserted(prologEquality(Pred))),!.
is_ftEquality(mudEquals(_,_)).
is_ftEquality(equals(_,_)).

function_to_predicate(Function,NewVar,PredifiedFunction):- 
  Function=..[F|ARGS],
  function_corisponding_predicate(F,P),
  PredifiedFunction=..[P,NewVar|ARGS].
function_to_predicate(Function,NewVar,mudEquals(NewVar,Function)).

:- export(defunctionalize/2).
defunctionalize(Wff,WffO):- defunctionalize(',',Wff,WffO).
defunctionalize(_ ,Wff,Wff):- non_compound(Wff),!.
defunctionalize(OP,Wff,WffO):- compound(Wff),
  each_subterm(Wff,SubTerm),
  compound(SubTerm),
  not(is_ftEquality(SubTerm)),
  arg(_,SubTerm,Function),is_function(Function),
  subst_eq(SubTerm,Function,NewVar,NewSubTerm),
  must(function_to_predicate(Function,NewVar,PredifiedFunction)),
  NEW =..[OP,PredifiedFunction,NewSubTerm],
  subst_eq(Wff,SubTerm,NEW,NextWff),!,
  defunctionalize(OP,NextWff,WffO).
defunctionalize(_,Wff,Wff).


delete_sublits(H0,B,HH):- delete_eq(H0,B,H1),delete_eq(H1,B,H2),delete_eq(H2,B,HH),!.

% cl([-nesc(p)], [-poss(p), nesc(q), -poss(q)]).

correct_cls(H,HH):-loop_check(correct_cls0(H,HH),H=HH),!.
correct_cls0(CL0,CL1):- is_list(CL0),!,maplist(correct_cls,CL0,CL1).
correct_cls0(cl([H],B),O):- delete_sublits(B,H,BB),BB\=@=B,!,correct_cls(cl([H],BB),O).
correct_cls0(cl(H,B),O):- list_to_set(B,BB),BB\=@=B,!,correct_cls(cl(H,BB),O).
correct_cls0(CL,O):- demodal_sents(CL,CLM),CL\=@=CLM,!,correct_cls(CLM,O).
correct_cls0(cl(H,B),O):-removeQ(H,HH),removeQ(B,BB),!,((H\=@=HH ; B\=@=BB) -> correct_cls(cl(HH,BB),O); O=cl(H,B)),!.

% kif_to_boxlog('=>'(WffIn,enables(Rule)),'$VAR'('MT2'),complete,Out1), % kif_to_boxlog('=>'(enabled(Rule),WffIn),'$VAR'('KB'),complete,Out).  

kif_to_prolog(X,E):- kif_to_boxlog(X,Y),!,list_to_set(Y,S),!,member(E,S).

%====== kif_to_boxlog(+Wff,-NormalClauses):-
:- export(kif_to_boxlog/2).
% kif_to_boxlog(Wff,Out):- loop_check(kif_to_boxlog(Wff,Out),Out=looped_kb(Wff)).
kif_to_boxlog(Wff,Out):- why_to_id(rule,Wff,Why), kif_to_boxlog(Wff,Why,Out),!.
kif_to_boxlog(WffIn,Out):-  why_to_id(rule,WffIn,Why), kif_to_boxlog(all('$VAR'('KB'),'=>'(asserted_t('$VAR'('KB'),WffIn),WffIn)),'$VAR'('KB'),Why,Out).
kif_to_boxlog(WffIn,NormalClauses):- why_to_id(rule,WffIn,Why), kif_to_boxlog(WffIn,'$VAR'('KB'),Why,NormalClauses).

alt_kif_to_boxlog(n(Neg, Wff),KB,Why,Out):- !, kif_to_boxlog(n(Neg, Wff),KB,Why,Out).
alt_kif_to_boxlog(Wff,KB,Why,Out):- loop_check(kif_to_boxlog((n(Neg,nesc(n(Neg,Wff)))),KB,Why,Out),Out=looped_kb(Wff)).

:- export(kif_to_boxlog/3).
kif_to_boxlog(WffIn,Why,Out):-  kif_to_boxlog(WffIn,'$VAR'('KB'),Why,Out),!.

kif_to_boxlog(Fml,KB,Why,Flattened):- var(KB),!,kif_to_boxlog(Fml,'$VAR'('KB'),Why,Flattened).

kif_to_boxlog(Wff,KB,Why,Out):- once(adjust_kif(Wff,M)),Wff \=@= M ,!,kif_to_boxlog(M,KB,Why,Out).
kif_to_boxlog((Wff:- B),KB,Why,Flattened):- is_true(B),!, kif_to_boxlog(Wff,KB,Why,Flattened),!.
kif_to_boxlog(WffInIn,KB,Why,FlattenedO):-  as_dlog(WffInIn,WffIn),WffInIn\=@=WffIn,!,kif_to_boxlog(WffIn,KB,Why,FlattenedO),!.

% kif_to_boxlog(Wff,KB,Why,Out):- loop_check(kif_to_boxlog(Wff,KB,Why,Out),alt_kif_to_boxlog(Wff,KB,Why,Out)),!.

kif_to_boxlog((HEAD:- BODY),KB,Why,FlattenedO):-  
  must_det_l((
   conjuncts_to_list(HEAD,HEADL),conjuncts_to_list(BODY,BODYL),
   maplist(correct_cls,[cl(HEADL,BODYL)],NCFs),
   maplist(clauses_to_boxlog(KB,Why),NCFs,ListOfLists),
   flatten([ListOfLists],Flattened),
   maplist(removeQ,Flattened,FlattenedO),
   wdmsgl(horn(FlattenedO)))),!.

kif_to_boxlog(WffIn,KB,Why,FlattenedO):-    
  must_det_l((
   must(numbervars_with_names(WffIn:KB:Why)),   
   ensure_quantifiers(WffIn,WffQ),
   Orig = WffQ,
   defunctionalize('=>',WffQ,Wff),   
   (WffQ\==Wff-> dmsg(defunctionalize('=>',WffQ,Wff));wdmsgl(kif(Wff))),
   as_dlog(Wff,Wff666),
   kb_nlit(KB,Neg),
   add_nesc(Wff666,Wff6667),
   nnf(Neg,KB,Orig,Wff6667,NNF),
   %wdmsgl(nnf(Neg,NNF)),
   pnf(NNF,PNF),
   %wdmsgl(pnf(PNF)),
   cf(KB,Orig,PNF,NCFsI),!,
   cf_to_flattened_clauses(KB,Why,NCFsI,FlattenedO))),!.

add_nesc(Wff666,Wff666):-!.
add_nesc(Wff666,Wff666):-leave_as_is(Wff666),!.
add_nesc(Wff666,Wff666):-is_modal(Wff666,_),!.
add_nesc(Wff666,nesc(Wff666)).

cf_to_flattened_clauses(KB,Why,NCFsI,FlattenedO):- 
 must_det_l((
   maplist(correct_cls,NCFsI,NCFs),
   % wdmsgl(cf(NCFs)),
   maplist(clauses_to_boxlog(KB,Why),NCFs,ListOfLists),
   flatten([ListOfLists],Flattened),
   thglobal:as_prolog(Flattened,FlattenedL),
   list_to_set(FlattenedL,FlattenedS),
   maplist(demodal_sents,FlattenedS,FlattenedO))),!.




clauses_to_boxlog(KB,Why,In,Prolog):- correct_cls(In,Mid), Mid \=@= In, !,clauses_to_boxlog(KB,Why,Mid,Prolog).
clauses_to_boxlog(_,_Why,cl([HeadIn],[]),Prolog):- !,is_lit_atom(HeadIn) -> Prolog=HeadIn ; kif_to_boxlog(HeadIn,Prolog).
clauses_to_boxlog(KB,Why,cl([],BodyIn),Prolog):-  !,
   is_lit_atom(BodyIn) -> clauses_to_boxlog(KB,Why,cl([inconsistentKB(KB)],BodyIn),Prolog);  kif_to_boxlog(-(BodyIn),Prolog).
clauses_to_boxlog(_,_Why,cl([HeadIn],BodyIn),(HeadIn:- BodyOut)):- maplist(logical_pos(_KB),BodyIn,Body), list_to_conjuncts(Body,BodyOut),!.
clauses_to_boxlog(KB,Why,cl([H,Head|List],BodyIn),Prolog):- 
  findall(Answer,((member(E,[H,Head|List]),delete_eq([H,Head|List],E,RestHead),
    maplist(logical_neg(KB),RestHead,RestHeadS),append(RestHeadS,BodyIn,Body),
    clauses_to_boxlog(KB,Why,cl([E],Body),Answer))),Prolog),!.


mpred_t_tell_snark(OP2,RULE):- 
 with_assertions(thlocal:current_pttp_db_oper(mud_call_store_op(OP2)),
   (show_call(call((must(snark_tell(RULE))))))).


fix_input_vars(AIn,A):- copy_term(AIn,A),numbervars(A,672,_).

%:- export(show_boxlog/1).
%assert_boxlog(AIn):- fix_input_vars(AIn,A), as_dlog(A,AA),kif_to_boxlog(AA,B),!,maplist(snark_tell_boxes(Why),B),!,nl,nl.
%:- export(show_boxlog2/2).
%assert_boxlog2(AIn):- fix_input_vars(AIn,A), with_all_dmsg((kif_to_boxlog(A,B),!,maplist(snark_tell_boxes(Why),B),!,nl,nl)).

snark_test_string(
"
% )
tell.

all(R,room(R) => exists(D, (door(D) & has(R,D)))).
room(room1).

ask.

room(What).

door(What).

:- snark_tell(a(XX) & b(XX) => c(XX)).
:- snark_tell(all(R,room(R) => exists(D, (door(D) & has(R,D))))).
:- snark_tell(loves(Child,motherFn(Child))).
:- snark_tell((p => q)).
:- snark_tell(~p <=> ~q).
:- snark_tell(p <=> q).
:- snark_tell(all(P, person(P) => -exists(D, dollar(D) & has(P,D)))).

:- snark_tell(go(sam) & (go(bill) v go(sally) ) & go(nancy)).

:- snark_tell(rains_tuesday => wear_rain_gear xor carry_umbrella).
:- snark_tell(exists(P, (person(P) & all(C, car(C) => ~has(P,C))))).

:- snark_tell(room(R) => exists(D, (door(D) & has(R,D)))).
:- snark_tell((goes(jane) xor goes(sandra) => goes(bill))).
:- snark_tell(exists(P, exists(C, (person(P) & car(C) & has(P,C))))).
:- snark_tell(~all(P,person(P) => exists(C, car(C) & has(P,C)))).
:- snark_tell((go(sam) & go(bill)) v (go(sally) & go(nancy))).
:- snark_tell(go(sam) & (go(bill) v go(sally) ) & go(nancy)).
:- snark_tell(exists(C, course(C) & exists(MT1, midterm(C,MT1) & exists(MT2, midterm(C,MT2) & different(MT1,MT2))))).
:- snark_tell(exists(C, course(C) & ~exists(MT3, midterm(C,MT3)))).

"
).


%:- export(tsn/0).
tsn:- with_all_dmsg(forall(clause(snark,C),must(C))).

% snark:- make.
tsnark:- snark_test_string(TODO),snark(string(TODO),current_output).

:- multifile(user:mud_regression_test/0).
user:mud_regression_test:- tsn.

:- thread_local(snark_action_mode/1).
:- asserta_if_new(snark_action_mode(tell)).

:- thread_local(snark_reader_mode/1).
:- asserta_if_new(snark_reader_mode(lisp)).


snark_read(In,Wff,Vs):- 
  (snark_reader_mode(lisp) -> 
    catch((lisp_read(In,WffIn),with_output_to(atom(A),write_term(WffIn,
      [module(logicmoo_i_snark),numbervars(true),quoted(true)])),
     read_term_from_atom(A,Wff,[module(logicmoo_i_snark),double_quotes(string),variable_names(Vs)])),E,(fmt(E),fail));
      catch(read_term(In,Wff,[module(logicmoo_i_snark),double_quotes(string),variable_names(Vs)]),E,(fmt(E),fail))).

%= ===== to test program =====-
:- ensure_loaded(library(logicmoo/plarkc/dbase_i_sexpr_reader)).

:- export(snark/0).
snark:- current_input(In),current_output(Out),!,snark(In,Out).

open_input(InS,InS):- is_stream(InS),!.
open_input(string(InS),In):- text_to_string(InS,Str),string_codes(Str,Codes),open_chars_stream(Codes,In),!.

:- export(snark/2).
snark(InS,Out):- 
  l_open_input(InS,In),
   repeat,             
      debugOnError((once((snark_action_mode(Mode),write(Out,Mode),write(Out,'> '))),
        snark_read(In,Wff,Vs),
         b_setval('$variable_names', Vs),
           portray_clause(Out,Wff,[variable_names(Vs),quoted(true)]),
           once(snark_process(Wff)),
           Wff == end_of_file)),!.

:- export(id_to_why/3).
why_to_id(Term,Wff,IDWhy):- not(atom(Term)),term_to_atom(Term,Atom),!,why_to_id(Atom,Wff,IDWhy).
why_to_id(Atom,Wff,IDWhy):- wid(IDWhy,Atom,Wff),!.
why_to_id(Atom,Wff,IDWhy):- must(atomic(Atom)),gensym(Atom,IDWhyI),kb_incr(IDWhyI,IDWhy),assertz_if_new(user:wid(IDWhy,Atom,Wff)).

:- export(snark_process/1).
snark_process(end_of_file):- !.
snark_process(prolog):- prolog_repl,!.
snark_process(Assert):- atom(Assert),retractall(snark_action_mode(_)),asserta(snark_action_mode(Assert)),fmtl(snark_action_mode(Assert)),!.
snark_process(Wff):- snark_action_mode(Mode),snark_process(Mode,Wff),!.

snark_process(_,':-'(Wff)):- !, snark_process(call,Wff).
snark_process(_,'?-'(Wff)):- !, snark_ask(Wff).
snark_process(_,'ask'(Wff)):- !, snark_ask(Wff).
snark_process(_,'tell'(Wff)):- !, snark_tell(Wff).
snark_process(call,Call):- !,call(Call).
snark_process(tell,Wff):- !, snark_tell(Wff).
snark_process(ask,Wff):- !, snark_ask(Wff).
snark_process(Other,Wff):- !, wdmsg(error(missing_snark_process(Other,Wff))),!,fail.

:- export(snark_ask_sent/1).
snark_ask_sent(Wff):- 
   why_to_id(ask,Wff,Why),
   term_variables(Wff,Vars),
   gensym(z_q,ZQ),
   Query=..[ZQ,666|Vars],
   kif_to_boxlog('=>'(Wff,Query),Why,Asserts),!,
   snark_tell_boxes(Why,Asserts),!,
   call_cleanup(
     snark_ask(Query),
     pttp_retractall_wid(Why)).


:- export(snark_ask/1).
snark_ask(P <=> Q):- snark_ask_sent(P <=> Q).
snark_ask(P => Q):- snark_ask_sent(P => Q).
snark_ask((P v Q)):- snark_ask_sent(((P v Q))).
snark_ask((P & Q)):- snark_ask_sent((P & Q)).
snark_ask(Goal0):-  logical_pos(_KB,Goal0,Goal),
    no_repeats(user:(
	add_args(Goal0,Goal,_,_,[],_,_,[],[],DepthIn,DepthOut,[PrfEnd|PrfEnd],_ProofOut1,Goal1,_),!,
        search(Goal1,60,0,1,3,DepthIn,DepthOut))).

:- export(snark_ask/2).
snark_ask(Goal0,ProofOut):- logical_pos(_KB,Goal0,Goal),
    no_repeats(user:(
	add_args(Goal0,Goal,_,_,[],_,_,[],[],DepthIn,DepthOut,[PrfEnd|PrfEnd],ProofOut1,Goal1,_),!,
        search(Goal1,60,0,1,3,DepthIn,DepthOut),
        contract_output_proof(ProofOut1,ProofOut))).

snark_tell(Wff):- why_to_id(tell,Wff,Why),snark_tell(Why,Wff).

:- export(snark_tell/2).

snark_tell(_,[]).
snark_tell(Why,[H|T]):- !,snark_tell(Why,H),kb_incr(Why,Why2),snark_tell(Why2,T).
snark_tell(Why,Wff):-  must(kif_to_boxlog(Wff,Why,Asserts)),must(snark_tell_boxes(Why,Wff,Asserts)),!.

snark_tell_boxes(Why,Wff0,Asserts0):- must_det_l((unnumbervars(Asserts0+Wff0,Asserts+Wff),
  %fully_expand(Get1,Get),
  get_constraints(Wff,Isas), snark_add_constraints(Why,Isas,Asserts))).

snark_add_constraints(Why,Isas,Get1Get2):- var(Get1Get2),!,trace_or_throw(var_snark_tell_isa_boxes(Why,Isas,Get1Get2)).
snark_add_constraints(Why,Isas,(Get1,Get2)):- !,snark_add_constraints(Why,Isas,Get1),kb_incr(Why,Why2),snark_add_constraints(Why2,Isas,Get2).
snark_add_constraints(Why,Isas,[Get1|Get2]):- !,snark_add_constraints(Why,Isas,Get1),kb_incr(Why,Why2),snark_add_constraints(Why2,Isas,Get2).
snark_add_constraints(_,_,[]).
snark_add_constraints(Why,Isas,((H:- B))):- conjoin(Isas,B,BB), snark_tell_boxes1(Why,(H:- BB)).
snark_add_constraints(Why,Isas,((H))):- snark_tell_boxes1(Why,(H:- Isas)).

snark_tell_boxes1(_,[]).
snark_tell_boxes1(Why,[H|T]):- !,must_det_l((snark_tell_boxes1(Why,H),kb_incr(Why,Why2),snark_tell_boxes1(Why2,T))).
snark_tell_boxes1(Why,AssertI):- must_det_l((simplify_bodies(AssertI,AssertO),snark_tell_boxes2(Why,AssertO))).

snark_tell_boxes2(Why,Assert):- 
  must_det_l((
  boxlog_to_prolog(Assert,Prolog),  
  unnumbervars(Prolog,PTTP), 
  assert_wfs(Why,PTTP))).

simplify_bodies((H:- B),(H:- BC)):- must_det_l((conjuncts_to_list(B,RB),simplify_list(RB,BB),list_to_conjuncts(BB,BC))).
simplify_bodies((B),(BC)):- must_det_l((conjuncts_to_list(B,RB),simplify_list(RB,BB),list_to_conjuncts(BB,BC))).


simplify_list(RB,BBO):- list_to_set(RB,BB),maplist(removeQ,BB,BBO).

assert_wfs(Why,PrologI):- must_det_l((thglobal:as_prolog(PrologI,Prolog), with_assertions(thlocal:current_why(Why,Prolog),pfc_add_h(Prolog)))).

pfc_add_h((H:- B)):- subst(H,not,neg,HH),subst(B,not,~,BB),dmsg((pfc_add=((HH:- BB)))),!.
pfc_add_h(H):- subst(H,not,neg,PrologM),dmsg(pfc_add = (PrologM)),!.

use_was_isa_h(_,ftTerm,true):- !.
use_was_isa_h(_,argi(mudEquals,_),true):- !.
use_was_isa_h(I,T,ISA):- to_isa_out(I,T,ISA),!.

generate_ante([],[],InOut,InOut).
generate_ante([I|VarsA],[T|VarsB],In,Isas):- use_was_isa_h(I,T,ISA), conjoin(In,ISA,Mid),generate_ante(VarsA,VarsB,Mid,Isas).

get_constraints(_,true):- !.
get_constraints(ListA,Isas):- 
     must_det_l((copy_term(ListA,ListB),
      term_variables(ListA,VarsA),
      term_variables(ListB,VarsB),
      attempt_attribute_args(isAnd,ftAskable,ListB),
      attribs_to_atoms(VarsB,VarsB),
      generate_ante(VarsA,VarsB,true,Isas))).

boxlog_to_prolog(IN,OUT):-leave_as_is(IN),!,IN=OUT.
boxlog_to_prolog(H, HH):-is_list(H),!,maplist(boxlog_to_prolog,H,HH).

boxlog_to_prolog((V:- TRUE),VE):- is_true(TRUE),boxlog_to_prolog(V,VE),!.
boxlog_to_prolog((H:- B),(HH:- BB)):- !,boxlog_to_prolog(H,HH),boxlog_to_prolog(B,BB).
boxlog_to_prolog((H & B),(HH , BB)):- !,boxlog_to_prolog(H,HH),boxlog_to_prolog(B,BB).
boxlog_to_prolog((H v B),(HH ; BB)):- !,boxlog_to_prolog(H,HH),boxlog_to_prolog(B,BB).
boxlog_to_prolog((H , B),(HH , BB)):- !,boxlog_to_prolog(H,HH),boxlog_to_prolog(B,BB).
boxlog_to_prolog((H ; B),(HH ; BB)):- !,boxlog_to_prolog(H,HH),boxlog_to_prolog(B,BB).
boxlog_to_prolog(H,O):- H=..[N,nesc(F)],kb_nlit(_,N),nonvar(F),!,HH=..[N,F],boxlog_to_prolog(HH,O).
boxlog_to_prolog(nesc(not(F)),O):- nonvar(F),!,boxlog_to_prolog(neg(F),O).
boxlog_to_prolog(nesc(F),O):- nonvar(F),!,boxlog_to_prolog(F,O).
boxlog_to_prolog(not(nesc(F)),O):- nonvar(F),!,boxlog_to_prolog(naf(F),O).
boxlog_to_prolog(~poss(F),O):-nonvar(F),!,boxlog_to_prolog(not_poss(F),O).
boxlog_to_prolog(n(Neg,H),n(Neg,HH)):- !,boxlog_to_prolog(H,HH).
boxlog_to_prolog(not(F),neg(O)):- nonvar(F),!,boxlog_to_prolog(F,O).
boxlog_to_prolog(IN,OUT):-demodal_sents(IN,M),IN\=@=M,!,boxlog_to_prolog(M,OUT).


boxlog_to_prolog( H, HH):- H=..[F|ARGS],!,boxlog_to_prolog(ARGS,ARGSO),!,HH=..[F|ARGSO].
boxlog_to_prolog(BL,PTTP):- thglobal:as_prolog(BL,PTTP).


/*
:- told.
:- dmsg_show(_).
:- dmsg('i see this').
:- snark_tell(exists(C, course(C) & ~exists(MT3, midterm(C,MT3)))).
:- snark_test_string(TODO),snark(string(TODO),current_output).
:- set_no_debug.
*/
/*
:- notrace.
:- nodebug.

:- wdmsg('we see this').

:- lsting(snark_tell/1).
:- snark_tell((p => q)).
:- snark_tell(~p <=> ~q).
:- snark_tell(tRoom(R) => exists(D, (tDoor(D) & has(R,D)))).
:- snark_tell(isa(R,tPred) => exists(D, (isa(D,ftInt) & arity(R,D)))).
:- snark_tell(all(P, person(P) => ~(exists(D, dollar(D) & has(P,D))))).
:- snark_tell(p <=> q).
:- snark_tell(all(P, person(P) => exists(D, dollar(D) & has(P,D)))).
*/
% :- prolog.

:- snark_tell(all(R,room(R) => exists(D, (door(D) & has(R,D))))).
% :- snark_tell(loves(Child,motherFn(Child))).

:- dynamic(snark_pred_head/1).
:- style_check(-singleton).

snark_pred_head(P):- var(P),!,isa(F,prologSNARK),arity(F,A),functor(P,F,A).
snark_pred_head(P):- get_functor(P,F,_),isa(F,prologPTTP).

:- dynamic(pttp_pred_head/1).

pttp_pred_head(P):- var(P),isa(F,prologPTTP),arity(F,A),functor(P,F,A).
pttp_pred_head(P):- get_functor(P,F,_),isa(F,prologPTTP).

:- multifile(snarky_comment/1).


pttp_listens_to_head(_OP,P):- pttp_pred_head(P).

pttp_listens_to_stub(prologPTTP).
pttp_listens_to_stub(prologSNARK).


user:provide_mpred_setup(Op,H):- provide_snark_op(Op,H).

% OPHOOK ASSERT
provide_snark_op(change(assert,How),(HeadBody)):- 
   pttp_listens_to_head(change(assert,How),HeadBody),
   why_to_id(provide_snark_op,(HeadBody),ID),
   snark_tell(ID,(HeadBody)).

% OPHOOK CALL
provide_snark_op(call(How),Head):- 
  pttp_listens_to_head(call(How),Head),
  pttp_call(Head).

% OPHOOK CLAUSES
provide_snark_op(clauses(How),(Head:- Body)):- 
   pttp_listens_to_head(clauses(How),Head),
   provide_mpred_storage_clauses(Head,Body,_Why).

% OPHOOK 
provide_snark_op(OP,(HeadBody)):- 
   pttp_listens_to_head(OP,HeadBody),
   snark_process(OP,HeadBody).


% CLAUSES HOOK 
user:provide_mpred_storage_clauses(H,B,wid3(IDWhy)):- wid(IDWhy,_,(H:- B)).
user:provide_mpred_storage_clauses(H,true,wid3(IDWhy)):- wid(IDWhy,_,(H)),compound(H),not(functor(H,':-',2)).


% REGISTER HOOK
user:provide_mpred_setup(OP,HeadIn,StubType,RESULT):-  pttp_listens_to_stub(StubType),!,
   get_pifunctor(HeadIn,Head,F),
      assert_if_new(isa(F,prologPTTP)),
         ensure_universal_stub(Head),
         RESULT = declared(pttp_listens_to_head(OP,Head)).

:- uses_logic(logicmoo_kb_refution).

:- if_startup_script(tsnark).
:- if_startup_script(ensure_loaded(logicmoo_i_mpred_snark_testing)).
:- logicmoo_example3.

end_of_file.

:- dynamic(user:int_proven_t/10).

int_proven_t(P, X, Y, E, F, A, B, C, G, D):- t(P,X,Y),
        test_and_decrement_search_cost(A, 0, B),
        C=[H, [proven_t(P, X, Y), D, E, F]|I],
        G=[H|I].


:- dynamic(user:int_assumed_t/10).
int_assumed_t(P, X, Y, E, F, A, B, C, G, D):- t(P,X,Y),
        test_and_decrement_search_cost(A, 0, B),
        C=[H, [assumed_t(P, X, Y), D, E, F]|I],
        G=[H|I].




