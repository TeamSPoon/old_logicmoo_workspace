/** <module> logicmoo_i_mpred_pttp
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
/*
:- module(logicmoo_i_kif, 
          [ 
           nnf/3, 
           pnf/3, cf/4,
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

:- dynamic(wid/3).

:- user:ensure_loaded(library(logicmoo/plarkc/dbase_i_sexpr_reader)).

%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%
%=% 
%=%   kif_in_prolog.P
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
:- multifile(user:type_action_info/3).
:- multifile(user:agent_call_command/2).
:- multifile(user:mud_test/2).
:- multifile(user:sanity_test/0).
:- multifile(user:regression_test/0).
:- multifile(user:feature_test/0).

:- ensure_loaded(logicmoo(mpred/logicmoo_i_header)).
:- user:ensure_loaded(logicmoo(pttp/dbase_i_mpred_pttp_testing)). 
% :- user:ensure_loaded(logicmoo(pttp/dbase_i_mpred_pttp)). 

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


:-dynamic(leave_as_is0/1).
leave_as_is(V):- \+ compound(V),!.
leave_as_is(V):-leave_as_is0(V),!.
leave_as_is0('$VAR'(_)).
leave_as_is0(infer_by(_)).
leave_as_is0(b_d(_,_,_)).
leave_as_is0(ct(_,_)).
% leave_as_is0('CollectionSubsetFn'(_,_)).
leave_as_is0(ignore(_)).
leave_as_is0(z_unused(_)).
leave_as_is0(isa(_,_)).
leave_as_is0({}).
leave_as_is0(kbMark(_)).

leave_as_is0(P):-prequent(P).

prequent(original(_)).
prequent(mudEquals(_,_)).
prequent(skolem(_,_)).
prequent(different(_,_)).
prequent(argInst(_,_,_)).
prequent(G):-functor(G,call_builtin,_).
prequent(G):-functor(G,not_call_builtin,_).

kb_nlit(_KB,Neg):-member(Neg,[(not),(~),(-),(neg)]).

set_is_lit(A):-when(nonvar(A),\+ is_ftVar(A)),!.

non_compound(InOut):- once(not(compound(InOut));is_ftVar(InOut)).

is_gaf(Gaf):-when(nonvar(Gaf),not(is_kif_rule(Gaf))).

:- export(is_kif_rule/1).
is_kif_rule(Var):- is_ftVar(Var),!,fail.
% is_kif_rule(_:- _):- !.
is_kif_rule(R):- get_functor(R,F,A),functor(P,F,A),kif_hook(P),!.

kif_hook(0=>0).
kif_hook(0<=>0).
kif_hook((0 & 0)).
kif_hook((0 v 0)).
kif_hook(0 <- 0).
kif_hook(~(0)).
kif_hook(not(0)).
kif_hook(all(+,0)).
kif_hook(exists(+,0)).
kif_hook(if(0,0)).
kif_hook(iff(0,0)).
kif_hook(C):- non_compound(C),!,fail.
kif_hook(H:- _):- !,nonvar(H),!,kif_hook(H).


:- style_check(+singleton).


subst_except(  Var, VarS,SUB,SUB ) :- Var==VarS,!.
subst_except(  Var, _,_,Var ) :- \+compound(Var),!.
subst_except(  Var, _,_,Var ) :- leave_as_is(Var),!.
subst_except([H|T],B,A,[HH|TT]):- !,
   subst_except(H,B,A,HH),
   subst_except(T,B,A,TT).
subst_except(HT,B,A,HHTT):- HT=..FARGS,subst_except(FARGS,B,A,[FM|MARGS]),
   (atom(FM)->HHTT=..[FM|MARGS];append_termlist(FM,MARGS,HHTT)).

append_termlist(Call,EList,CallE):-must((compound(Call),is_list(EList))), Call=..LeftSide, append(LeftSide,EList,ListE), CallE=..ListE.


correct_arities(_,FmlO,FmlO):-leave_as_is(FmlO),!.
correct_arities([],Fml,Fml):-!.
correct_arities([H|B],Fml,FmlO):-!,correct_arities(H,Fml,FmlM),correct_arities(B,FmlM,FmlO).
correct_arities(_,Fml,Fml):- \+ compound(Fml),!.
correct_arities(H,Fml,FmlO):- Fml=..[H,A|ARGS], ARGS\=[_],
  (ARGS==[]-> correct_arities(H,A,FmlO);
       (correct_arities(H,A,CA),FmlM=..[H|ARGS],correct_arities(H,FmlM,FmlMC),FmlO=..[H,CA,FmlMC])),!.
correct_arities(H,Fml,FmlM):- Fml=..[F|ARGS],must_maplist(correct_arities(H),ARGS,ARGSM),FmlM =.. [F|ARGSM].

:- export(term_slots/2).
term_slots(Term,Slots):-term_singletons(Term, [],NS, [],S),append(NS,S,Slots).

:- export(term_singletons/2).
term_singletons(A,Vs):- term_singletons(A,[],_,[],Vs). 
:- export(term_singletons/5).
term_singletons(Fml, NS,NS, S,S):- atomic(Fml),!.
term_singletons(Fml, NS,NS, S,S):- identical_member(Fml,NS),!.
term_singletons(Fml, NS, [Fml|NS], S, NSV):- is_ftVar(Fml),identical_member(Fml,S),!,delete_eq(S,Fml,NSV),!.
term_singletons(Fml, NS, NS, S, [Fml|S]):- is_ftVar(Fml),!.
term_singletons([H|T],NS,NSO,S,NSV):- !, term_singletons(H,NS,NSM,S,M),term_singletons(T,NSM,NSO,M,NSV).
term_singletons(Fml, NS,NSO, S,NSV):- compound(Fml),Fml=..[_,H|T],!, term_singletons(H,NS,NSM,S,M),term_singletons(T,NSM,NSO, M,NSV).

get_kv(X=Y,X,Y):- !.
get_kv(X-Y,X,Y):- !.
get_kv(KV,X,Y):- functor(KV,_,1),KV=..[X,Y],!.
get_kv(KV,X,Y):- arg(1,KV,X),arg(2,KV,Y),!.

:- export(subsT_each/3).
subsT_each(In,[],In):- !.
subsT_each(In,[KV|TODO],Out):- !,get_kv(KV,X,Y),subst_except(In,X,Y,Mid),!,subsT_each(Mid,TODO,Out),!.

contains_var_lits(Fml,Var,Lits):- findall(Lit,contains_t_var(Fml,Var,Lit),Lits).

get_isa(Lit,I,TT):- compound(Lit),get_isa0(Lit,I,TT).
get_isa0(isa(I,T),I,TT):- to_iname(T,TT),!.
get_isa0(IT,I,TT):- IT=..[T,I],is_colection_name(IT,T,TT),!.

is_colection_name(_,-,_):- !,fail.
is_colection_name(IT,T,TT):- atom_length(T,TL),TL>2,not(atom_contains(T,'_')),not(predicate_property(IT,_)),to_iname(T,TT).

:- dynamic(mudEquals/2).
:- export(mudEquals/2).
mudEquals(X,Y):- X=Y.
:- dynamic(skolem/3).
:- export(skolem/3).
skolem(X,Y,_):- X=Y.

:- export(not_mudEquals/2).
:- dynamic(not_mudEquals/2).
not_mudEquals(X,Y):- X \= Y.

contains_type_lits(Fml,Var,Lits):- findall(T,(contains_t_var(Fml,Var,Lit),get_isa(Lit,O,T),same_var(O,Var)),Lits).
contains_t_var(Fml,Var,Term):- each_subterm(Fml,Term),compound(Term),arg(_,Term,O),same_var(O,Var).

:- export(type_of_var/3).
type_of_var(Fml,Var,Type):- contains_type_lits(Fml,Var,Lits),!,(member(Type,Lits)*->true;Type='Unk').
:- style_check(+singleton).

to_dlog_ops([
       'theExists'='exists',
       'thereExists'='exists',
       'ex'='exists',
       'forAll'='all',
       'forall'='all',
       ';'='v',
       ','='&',
       '~'='not',
     '-'='not',      
     'neg'='not',
     'naf'='not',
     'and'='&',
      'or'='v',
      ':-'=':-',
      '<='=':-',
 'implies'='=>',
   'if'='=>',
   'iff'='<=>',
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
to_nonvars(_,Fml,Fml):- leave_as_is(Fml),!.
to_nonvars(Type,IN,OUT):- is_list(IN),!,must_maplist(to_nonvars(Type),IN,OUT),!.
to_nonvars(Type,IN,OUT):- call(Type,IN,OUT),!.


convertAndCall(Type,Call):- fail,Call=..[F|IN],must_maplist(to_nonvars(Type),IN,OUT), IN \=@= OUT, !, must(apply(F,OUT)).
convertAndCall(_Type,Call):-call_last_is_var(Call).

as_dlog(Fml,Fml):- leave_as_is(Fml),!.
as_dlog(Fml,FmlO):- to_dlog_ops(OPS),subsT_each(Fml,OPS,FmlM),!,correct_arities(['v','&'],FmlM,FmlO).




as_symlog(Fml,Fml):- leave_as_is(Fml),!.
as_symlog(Fml,FmlO):- as_dlog(Fml,FmlM),to_symlog_ops(OPS),subsT_each(FmlM,OPS,FmlM),correct_arities(['v','&'],FmlM,FmlO).

:- dynamic(thglobal:as_prolog/2).
thglobal:as_prolog(Fml,Fml):- is_ftVar(Fml),!.
thglobal:as_prolog(Fml,FmlO):- as_symlog(Fml,FmlM),
  to_prolog_ops(OPS),subsT_each(FmlM,OPS,FmlO).

is_modal(MODAL,_):- \+ compound(MODAL),!,fail.
is_modal(MODAL,BDT):- (MODAL = nesc(BDT,_) ; MODAL = poss(BDT,_)),!,nonvar(BDT).
is_modal(MODAL,BDT):- arg(_,MODAL,ARG),is_modal(ARG,BDT).

:- style_check(+singleton).
%=% Negation Normal Form

% Usage: nnf(+KB,+Fml, ?NNF)
nnf(KB,Fml,NNF):-   
   nnf(KB,Fml,[],NNF,_),!.

skolem_setting(label).
%skolem_setting(nnf).
%skolem_setting(removeQ).
%skolem_setting(eliminate).
%skolem_setting(ignore).
%skolem_setting(leavein).

adjust_kif(KB,Kif,KifO):-must(adjust_kif0(KB,Kif,KifO)),!.

% Converts to syntax that NNF/DNF/CNF/removeQ like
adjust_kif0(_,V,V):- is_ftVar(V),!.
adjust_kif0(_,A,A):- \+ compound(A),!.
adjust_kif0(KB,nesc(N,Kif),nesc(N,KifO)):- !,adjust_kif0(KB,Kif,KifO).
adjust_kif0(KB,poss(N,Kif),poss(N,KifO)):- !,adjust_kif0(KB,Kif,KifO).
adjust_kif0(KB,not(Kif),not(KifO)):- !,adjust_kif0(KB,Kif,KifO).
adjust_kif0(KB,not(KB,Kif),not(KifO)):- !,adjust_kif0(KB,Kif,KifO).
adjust_kif0(KB,neg(Kif),not(KifO)):- !,adjust_kif0(KB,Kif,KifO).
adjust_kif0(KB,not(Kif),not(KifO)):- !,adjust_kif0(KB,Kif,KifO).
adjust_kif0(KB,~(Kif),not(KifO)):- !,adjust_kif0(KB,Kif,KifO).
adjust_kif0(KB,t(Kif),t(KifO)):- !,adjust_kif0(KB,Kif,KifO).
adjust_kif0(KB,poss(Kif),poss(b_d(KB,nesc,poss),KifO)):- !,adjust_kif0(KB,Kif,KifO).
adjust_kif0(KB,nesc(Kif),nesc(b_d(KB,nesc,poss),KifO)):- !,adjust_kif0(KB,Kif,KifO).
adjust_kif0(KB,exists(L,Expr),               ExprO):-L==[],!,adjust_kif0(KB,Expr,ExprO).
adjust_kif0(KB,exists(L,Expr),               ExprO):-atom(L),subst(Expr,L,'$VAR'(L),ExprM),!,adjust_kif0(KB,exists('$VAR'(L),ExprM),ExprO).
adjust_kif0(KB,exists([L|List],Expr),exists(L,ExprO)):-is_list(List),!,adjust_kif0(KB,exists(List,Expr),ExprO).
adjust_kif0(KB,exists(L,Expr),               ExprO):- \+ contains_var(L,Expr),!,adjust_kif0(KB,Expr,ExprO).
adjust_kif0(KB,exists(L,Expr),exists(L,ExprO)):-!,adjust_kif0(KB,Expr,ExprO).
adjust_kif0(KB,all(L,Expr),               ExprO):-L==[],!,adjust_kif0(KB,Expr,ExprO).
adjust_kif0(KB,all(L,Expr),               ExprO):-atom(L),subst(Expr,L,'$VAR'(L),ExprM),!,adjust_kif0(KB,all('$VAR'(L),ExprM),ExprO).
adjust_kif0(KB,all([L|List],Expr),all(L,ExprO)):-is_list(List),!,adjust_kif0(KB,exists(List,Expr),ExprO).
adjust_kif0(KB,all(L,Expr),               ExprO):- \+ contains_var(L,Expr),!,adjust_kif0(KB,Expr,ExprO).
adjust_kif0(KB,all(L,Expr),all(L,ExprO)):-!,adjust_kif0(KB,Expr,ExprO).
adjust_kif0(KB,[L|Ist],ConjO):- is_list([L|Ist]),must_maplist(adjust_kif0(KB),[L|Ist],ConjO),!.
adjust_kif0(KB,'&'([L|Ist]),ConjO):- is_list([L|Ist]),list_to_conjuncts('&',[L|Ist],Conj),adjust_kif0(KB,Conj,ConjO).
adjust_kif0(KB,'v'([L|Ist]),ConjO):- is_list([L|Ist]),list_to_conjuncts('v',[L|Ist],Conj),adjust_kif0(KB,Conj,ConjO).
adjust_kif0(KB,(H:-[L|Ist]),(HH:-ConjO)):- adjust_kif(KB,H,HH),is_list([L|Ist]),adjust_kif0(KB,'&'([L|Ist]),ConjO).
adjust_kif0(KB,(H:-B),(HH:-ConjO)):- adjust_kif(KB,H,HH),adjust_kif(KB,B,ConjO),!.
adjust_kif0(_,A,A):-leave_as_is(A),!.
adjust_kif0(KB,Kif,KifO):- Kif=..[F|ARGS],adjust_kif0(KB,F,ARGS,KifO),!.
adjust_kif0(KB,PAB,PABO):- PAB=..[P|AB],must_maplist(adjust_kif0(KB),AB,ABO),PABO=..[P|ABO].

adjust_kif0(KB,true_t,[F|LIST],O3):-atom(F),!,PARGS=..[F|LIST],adjust_kif0(KB,(PARGS),O3),!.
adjust_kif0(KB,not_true_t,[F|LIST],O3):-atom(F),!,PARGS=..[F|LIST],adjust_kif0(KB,not(PARGS),O3),!.
adjust_kif0(KB,not,[A],not(O)):-!,adjust_kif0(KB,A,O),!.
adjust_kif0(KB,not,[A],not(O)):-!,adjust_kif0(KB,A,O),!.
adjust_kif0(KB,possible_t,[A],O):-!,adjust_kif0(KB,poss(A),O),!.
adjust_kif0(KB,possible_t,ARGS,O):-!,PARGS=..ARGS,adjust_kif0(KB,poss(PARGS),O).
adjust_kif0(KB,asserted_t,[A],O):-!,adjust_kif0(KB,t(A),O),!.
adjust_kif0(KB,asserted_t,ARGS,O):-!,PARGS=..ARGS,adjust_kif0(KB,t(PARGS),O).
adjust_kif0(KB,call_builtin,ARGS,O):-!,PARGS=..ARGS,adjust_kif0(KB,PARGS,O),!.
adjust_kif0(KB,true_t,ARGS,O):-PARGS=..ARGS,adjust_kif0(KB,PARGS,O),!.
adjust_kif0(KB,Not_P,ARGS,O):-atom_concat('not_',P,Not_P),!,PARGS=..[P|ARGS],adjust_kif0(KB,not(PARGS),O).
adjust_kif0(KB,Int_P,ARGS,O):-atom_concat('int_',P,Int_P),!,append(LARGS,[_, _, _, _, _, _, _ ],ARGS),
   PLARGS=..[P|LARGS],adjust_kif0(KB,PLARGS,O).
adjust_kif0(KB,P,ARGS,O):-atom_concat(_,'_t',P),!,append(LARGS,[_, _, _, _, _, _],ARGS),
   PARGS=..[P|LARGS],adjust_kif0(KB,PARGS,O).

adjust_kif0(KB,W,[P,A,R|GS],O):-is_wrapper_pred(W),PARGS=..[P,A,R|GS],adjust_kif0(KB,t(PARGS),O).
adjust_kif0(KB,F,ARGS,O):-KIF=..[F|ARGS],length(ARGS,L),L>2,adjust_kif0(KB,KIF,F,ARGS,Conj),KIF\=@=Conj,!,adjust_kif0(KB,Conj,O).
% adjust_kif0(KB,W,[A],O):-is_wrapper_pred(W),adjust_kif(KB,A,O),!.

adjust_kif0(KB,KIF,OP,ARGS,Conj):-must_maplist(adjust_kif(KB),ARGS,ABO),adjust_kif5(KB,KIF,OP,ABO,Conj).

adjust_kif5(_KB,_KIF,',',ARGS,Conj):- list_to_conjuncts('&',ARGS,Conj).
adjust_kif5(_,_,';',ARGS,Conj):-list_to_conjuncts('v',ARGS,Conj).
adjust_kif5(_,_,'&',ARGS,Conj):-list_to_conjuncts('&',ARGS,Conj).
adjust_kif5(_,_,'v',ARGS,Conj):-list_to_conjuncts('v',ARGS,Conj).

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

:-discontiguous(nnf/5).
:-discontiguous(axiom_lhs_to_rhs/3).
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
  show_call_success((nop(Fml),logically_matches(KB,Fin,F1))),show_call(nnf(KB,F2,FreeV,DIA,Paths)).

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
   must(skolem(KB,Fml,X,FreeV,FmlSk)),
   must(nnf(KB,FmlSk,FreeV,NNF,Paths)).

% exists(X,nesc(f(X)))  ->  exists(X,not(poss(not(f(X))))) ->  not(poss(not(f(X))))
% nnf(KB,exists(X,Fml),FreeV,NNF,Paths):- nnf(KB,not(poss(b_d(KB,nesc,poss),not(Fml))),FreeV,NNF,Paths).

nnf(KB,exists(X,Fml),FreeV,NNF,Paths):- skolem_setting(label),
   nnf_label(KB,exists(X,Fml),FreeV,NNF,Paths),!.


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
   

nnf(KB,&(A,B),FreeV,NNF,Paths):- !,
	nnf(KB,A,FreeV,NNF1,Paths1),
	nnf(KB,B,FreeV,NNF2,Paths2),
	Paths is Paths1 * Paths2,
	(Paths1 > Paths2 -> NNF = &(NNF2,NNF1);
		            NNF = &(NNF1,NNF2)).

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

is_sentence_functor(&).
is_sentence_functor(v).
is_sentence_functor(exists).
is_sentence_functor(all).

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
cf(Why,KB,PNF, LIST):- !,
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

/*
cf(Why,KB,PNF, SET):- 
 must_det_l((
  removeQ(KB,PNF,[], UnQ),
  cnf(KB,UnQ,CNF),!,
   
  conjuncts_to_list(CNF,Conj),
  make_clause_set([infer_by(Why)],Conj,EachClause),
  must_maplist(correct_cls(KB),EachClause,SOO),
  expand_cl(KB,SOO,SOOO),
  sort(SOOO,SET))).
*/


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
	skolem(KB,F,X,Vars,Fsk),
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


local_pterm_to_sterm(P,['$VAR'(S)]):- if_defined(logicmoo_i_sexp_reader:svar(P,S)),!.
local_pterm_to_sterm(P,['$VAR'(S)]):- if_defined(logicmoo_i_sexp_reader:lvar(P,S)),!.
local_pterm_to_sterm(P,[P]):- leave_as_is(P),!.
local_pterm_to_sterm((H:-P),(H:-S)):-!,local_pterm_to_sterm(P,S),!.
local_pterm_to_sterm((P=>Q),[implies,PP,=>,QQ]):-local_pterm_to_sterm(P,PP),local_pterm_to_sterm(Q,QQ).
local_pterm_to_sterm((P<=>Q),[equiv,PP,QQ]):-local_pterm_to_sterm(P,PP),local_pterm_to_sterm(Q,QQ).
local_pterm_to_sterm(all(P,Q),[all(PP),QQ]):-local_pterm_to_sterm(P,PP),local_pterm_to_sterm(Q,QQ).
local_pterm_to_sterm(exists(P,Q),[ex(PP),QQ]):-local_pterm_to_sterm(P,PP),local_pterm_to_sterm(Q,QQ).
local_pterm_to_sterm(not(Q),[not,QQ]):-local_pterm_to_sterm(Q,QQ).
local_pterm_to_sterm(poss(Q),[pos(QQ)]):-local_pterm_to_sterm(Q,QQ).
local_pterm_to_sterm('&'(P,Q),PPQQ):-local_pterm_to_sterm(P,PP),local_pterm_to_sterm(Q,QQ),flatten([PP,QQ],PPQQ0),list_to_set(PPQQ0,PPQQ).
local_pterm_to_sterm(','(P,Q),PPQQ):-local_pterm_to_sterm(P,PP),local_pterm_to_sterm(Q,QQ),flatten([PP,QQ],PPQQ0),list_to_set(PPQQ0,PPQQ).
local_pterm_to_sterm('v'(P,Q),[or,[PP],[QQ]]):-local_pterm_to_sterm(P,PP),local_pterm_to_sterm(Q,QQ),!.
local_pterm_to_sterm('beliefs'(P,Q),[beliefs(PP),QQ]):-local_pterm_to_sterm2(P,PP),local_pterm_to_sterm(Q,QQ),!.
local_pterm_to_sterm(P,S):-subst_except(P,'&',',',Q),P\=@=Q,!,local_pterm_to_sterm(Q,S),!.
local_pterm_to_sterm(P,S):-subst_except(P,'v',';',Q),P\=@=Q,!,local_pterm_to_sterm(Q,S),!.
local_pterm_to_sterm(P,[Q]):-P=..[F|ARGS],maplist(local_pterm_to_sterm2,ARGS,QARGS),Q=..[F|QARGS].
local_pterm_to_sterm(P,[P]).

local_pterm_to_sterm2(P,Q):-local_pterm_to_sterm(P,PP),([Q]=PP;Q=PP),!.

nowrap_one(_,[One],One).
nowrap_one(Wrap,MORE,OUT):- OUT=..[Wrap,MORE].

display_form(KB,Form):- demodal_sents(KB,Form,OutM),local_pterm_to_sterm(OutM,Out),portray_clause(current_output,Out,[max_depth(0),portrayed(true)]).

demodal_sents(KB,I,O):- must_det_l((demodal(KB,I,M),modal2sent(M,O))).

demodal(KB,In,Prolog):- call_last_is_var(demodal(KB,In,Prolog)),!.
demodal(_KB,Var, Var):- notrace(leave_as_is(Var)),!.
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

modal2sent(Var, Var):- notrace(leave_as_is(Var)),!.
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
  must_maplist(thglobal:as_prolog,NewBodyListM,NewBodyListO).

flattenConjs(_Extras,I,O):- conjuncts_to_list(I,M),must_maplist(conjuncts_to_list,M,L),flatten(L,O).

is_neg(not(_)).
is_pos(One):- get_functor(One,F),!,not(is_log_op(F)).

:- export(is_log_sent/1).
is_log_sent(S):- get_functor(S,F,_),is_log_op(F).

not_log_op(OP):- not(is_log_op(OP)).
:- export(is_log_op/1).
is_log_op(OP):- atomic(OP),to_dlog_ops(OPS),!,(member(OP=_,OPS);member(_=OP,OPS)).


:- export(logical_pos/3).
:- export(logical_neg/3).
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

:- export(nonegate/4).
nonegate(_KB,IO,IO):-!.
nonegate(KB,List,OutZ):- is_list(List),must_maplist(nonegate(KB),List,OutZ),!.
nonegate(KB,Fml,OutZ):- simplify_cheap(Fml,Fml2), Fml \=@= Fml2,nonegate(KB,Fml2,OutZ),!.
nonegate(KB,Fml,OutZ):- must((unbuiltin_negate(KB,Fml,Out),!,defunctionalize(Out,OutY),!,must(mpred_quf(OutY,OutZ)))),!.

unbuiltin_negate(_Neg,_, Fml,Fml):- is_ftVar(Fml),!.
unbuiltin_negate(_Neg,_, Fml,Out):- get_functor(Fml,F,A),pttp_builtin(F,A),!,must(Out=Fml).
unbuiltin_negate(_KB,Fml,Out):- once(negate(KB,Fml,Neg)),negate(KB,Neg,Out),!.

%=%  Skolemizing : method 1

% Usage: skolem(+Fml,+X,+FreeV,?FmlSk)
% Replaces existentially quantified variable with the formula
% VARIABLES MUST BE PROLOG VARIABLES
% exists(X,p(X)) ==> p(p(exists))

skolem_bad(Fml,X,FreeV,FmlSk):- 
	copy_term((X,Fml,FreeV),(Fml,Fml1,FreeV)),
	copy_term((X,Fml1,FreeV),(exists,FmlSk,FreeV)).

%=%  Skolemizing : method 2

% Usage: skolem(KB, +Fml, +X, +FreeV, ?FmlSk )
% Replaces existentially quantified variable with a unique function
% fN(Vars) N=1,...
% VARIABLES MAYBE EITHER PROLOG VARIABLES OR TERMS


skolem(KB, F, X, FreeV, Out):-  fail,
   must(skolem_f(KB, F, X, FreeV, Sk)),
   subst_except(F,X,Sk,Out).

skolem(KB, F, X, FreeV, Out):-  
   must(skolem_f(KB, F, X, FreeV, Sk)),
   %writeq(freev(Sk,FreeV)),
   must(Out= '=>'({skolem(X,Sk)},F)),
   !,show_call( asserta((constraintRules(X,Sk,F)))).

skolem(KB, F, X, FreeV, FmlSk):- 
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
%=% generate a skolem 

mk_skolem_name(_O,Var,Fml,SIn,SOut):- is_ftVar(Fml), same_var(Var,Fml),!,atom_concat('Is',SIn,SOut).
mk_skolem_name(_O,_V,Fml,SIn,SIn):- is_ftVar(Fml),!.
mk_skolem_name(_O ,_V,[],SIn,SIn):- !.
mk_skolem_name(_O,_V, OP,SIn,SIn):- is_log_op(OP),!.
mk_skolem_name(_O,_V,Fml,SIn,SOut):- atomic(Fml),!,i_name(Fml,N),toPropercase(N,CU),!,(atom_contains(SIn,CU)->SOut=SIn;atom_concat(SIn,CU,SOut)).
mk_skolem_name(KB,Var,[H|T],SIn,SOut):- !,mk_skolem_name(KB,Var,H,SIn,M),mk_skolem_name(KB,Var,T,M,SOut).
mk_skolem_name(KB,Var,isa(VX,Lit),SIn,SOut):- same_var(Var,VX),not_ftVar(Lit),!,mk_skolem_name(KB,Var,['Is',Lit,'In'],'',F),atom_concat(F,SIn,SOut).
mk_skolem_name(KB,Var,Fml,SIn,SOut):- Fml=..[F,VX],same_var(Var,VX),!,mk_skolem_name(KB,Var,['Is',F,'In'],SIn,SOut).
mk_skolem_name(KB,Var,Fml,SIn,SOut):- Fml=..[F,Other,VX|_],same_var(Var,VX),!,type_of_var(KB,Other,OtherType),
   mk_skolem_name(KB,Var,[OtherType,'Arg2Of',F],SIn,SOut).
mk_skolem_name(KB,Var,Fml,SIn,SOut):- Fml=..[F,VX|_],same_var(Var,VX),!,mk_skolem_name(KB,Var,['Arg1Of',F],SIn,SOut).
mk_skolem_name(KB,Var,Fml,SIn,SOut):- Fml=..[F|_],!,mk_skolem_name(KB,Var,['ArgNOf',F],SIn,SOut).

% same_var(Var,Fml):- not(not(Var=Fml)),!.
same_var(Var,Fml):- Var==Fml,!.


%======  make a sequence out of a disjunction =====
flatten_or_list(A,B,C):- convertAndCall(as_symlog,flatten_or_list(A,B,C)).
flatten_or_list(KB,v(X , Y), F):- !,
   flatten_or_list(KB,X,A),
   flatten_or_list(KB,Y,B),
   flatten([A,B],F).
flatten_or_list(_KB,X,[X]).



fmtl(X):- thglobal:as_prolog(X,XX), fmt(XX).

write_list([F|R]):- write(F), write('.'), nl, write_list(R).
write_list([]).


numbervars_with_names(Term,CTerm):- 
 must_det_l((
   source_variables_l(NamedVars),!,
   copy_term(Term:NamedVars,CTerm:CNamedVars),
   term_variables(CTerm,Vars),   
   get_var_names(Vars,CNamedVars,Names),
   b_implode_varnames0(Names),
   numbervars(CTerm,91,_,[attvar(skip),singletons(false)]),
   append(CNamedVars,NamedVars,NewCNamedVars),
   list_to_set(NewCNamedVars,NewCNamedVarsS),
   remove_grounds(NewCNamedVarsS,NewCNamedVarsSG),
   b_setval('$variable_names',NewCNamedVarsSG))).
get_var_names([],_,[]).
get_var_names([V|Vars],NamedVars,[S|SNames]):-
    get_1_var_name(V,NamedVars,S),
    get_var_names(Vars,NamedVars,SNames).

get_1_var_name(_V,[],_S).

get_1_var_name(Var,NamedVars,Name):- compound(Var),arg(1,Var,NV),!,get_1_var_name(NV,NamedVars,Name).
get_1_var_name(Var,NamedVars,Var=NV):-atom(Var),NamedVars=[_|T],nb_setarg(2,NamedVars,[Var=NV|T]),!.
get_1_var_name(Var,[N=V|_NamedVars],Name=V):-
     (Var == V -> Name = N ; (Var==Name -> Name=Var ; fail )),!.     
get_1_var_name(Var,[_|NamedVars],Name):- get_1_var_name(Var,NamedVars,Name).


wdmsgl(CNF):- compound(CNF),CNF=..[NAME,NF],!,must(wdmsgl(NAME:-NF)).
wdmsgl(CNF):- wdmsg(CNF),!.
wdmsgl(NF):- must((get_functor(NF,NAME),!,must(wdmsgl_2(NAME,NF)))).


wdmsgl_2(NAME,NF):- functor(NF,_,_),wdmsgl_3(NAME,&,NF).

wdmsgl_3(NAME,F,NF):- 
   numbervars_with_names(vv(NAME,F,NF),vv(NAME2,F2,NF2)),
   wdmsgl_4(NAME2,F2,NF2).

%wdmsgl_4(NAME,F,NF):- is_list(NF),!,list_to_set(NF,NS),must_maplist(wdmsgl_4(NAME,F),NS).
%wdmsgl_4(NAME,F,NF):- compound(NF),NF=..[FF,A,B],FF=F,not_ftVar(A),not_ftVar(B),!,must_maplist(wdmsgl_4(NAME,F),[A,B]).
% wdmsgl_4(NAME,_,NF):- as_symlog(NF,NF2), with_all_dmsg(display_form(KB,NAME:-NF2)).
wdmsgl_4(NAME,_,NF):- as_symlog(NF,NF2), with_all_dmsg(display_form(_KB,(NAME:-NF2))).



put_singles(Wff,_,[],Wff).
put_singles(Wff,Exists,[S|Singles],NewWff):-   
   (((each_subterm(Wff,SubTerm),compound(SubTerm),
    SubTerm=..[OtherExists,SO,_],same_var(SO,S),
     member(OtherExists,[all,exists])))
 -> WffM = Wff ; WffM =..[Exists,S,Wff]),
   put_singles(WffM,Exists,Singles,NewWff),!.


ensure_quantifiers(Wff:- B,WffO):- B== true,!, ensure_quantifiers(Wff,WffO).
ensure_quantifiers(Wff:- B,Wff:- B):- !.
% ensure_quantifiers(Wff,Wff):-!.
ensure_quantifiers(Wff,WffO):-
 must_det_l((show_call_failure(term_singletons(Wff,[],NS,[],Singles)),
  put_singles(Wff,'all',Singles,WffM),put_singles(WffM,'all',NS,WffO))).

:- multifile(function_corisponding_predicate/2).
:- dynamic(function_corisponding_predicate/2).

get_pred(Pred,F):- get_functor(Pred,F).
is_function(F):- is_ftVar(F),!,fail.

 
is_function(Function):- compound(Function),get_functor(Function,F,A),is_function(Function,F,A).



is_function(_,'SubLQuoteFn',_):- !,fail.
is_function(_,'CollectionSubsetFn',_).
is_function(_,F,_):- atom_concat('sk',_Was,F),!,fail.
is_function(P,_,_):- leave_as_is(P),!,fail.
is_function(_,F,_):- is_log_op(F),!,fail.
is_function(_,F,_):- atom_concat(_Was,'Fn',F).
is_function(_,F,_):- tFunction(F).
is_function(_,F,A):- A2 is A+1,current_predicate(F/A2), not(current_predicate(F/A)).

%:- pfc_add(isa(I,C)<=(ttPredType(C),user:isa(I,C))).

is_ftEquality(Term):- is_ftVar(Term),!,fail.
%is_ftEquality(Term):- get_pred(Term,Pred),is),!,(Pred==mudEquals;genlPreds(Pred,equals);clause_asserted(prologEquality(Pred))),!.
is_ftEquality(mudEquals(_,_)).
is_ftEquality(skolem(_,_)).
is_ftEquality(equals(_,_)).

:-thread_local(thlocal:dont_use_mudEquals/0).

function_to_predicate(Function,NewVar,PredifiedFunction):- 
 Function = 'CollectionSubsetFn'(Col,'TheSetOf'(NewVar,Formulas)), 
 must(is_ftVar(NewVar)), % \+ is_ftVar(Col),!,
 PredifiedFunction = (isa(NewVar,Col) & Formulas).

function_to_predicate(Function,NewVar,PredifiedFunction):- 
  Function=..[F|ARGS],
  function_corisponding_predicate(F,P),
  fresh_varname(Function,NewVar),
  PredifiedFunction=..[P,NewVar|ARGS],!.

function_to_predicate(Function,NewVar,mudEquals(NewVar,Function)):- \+ thlocal:dont_use_mudEquals, fresh_varname(Function,NewVar),!.

:-meta_predicate(call_last_is_var(1)).
call_last_is_var(MCall):- strip_module(MCall,M,Call),
   must((compound(Call),functor(Call,_,A))),
   arg(A,Call,Last),nonvar(Last),Call=..FArgs,
   append(Left,[Last],FArgs),append(Left,[IsVar],NFArgs),NewCall=..NFArgs,!,M:NewCall*->IsVar=Last;fail.



   

fresh_varname(F,NewVar):-is_ftVar(F),NewVar=F.
fresh_varname(F,NewVar):-var(F),fresh_varname('mudEquals',NewVar).
fresh_varname([F0|_],NewVar):-!,fresh_varname(F0,NewVar).
fresh_varname(F,NewVar):- compound(F),arg(_,F,F1),atom(F1),!,functor(F,F0,_),atom_concat(F0,F1,FN),upcase_atom(FN,FUP),gensym(FUP,VARNAME),NewVar = '$VAR'(VARNAME),!.
fresh_varname(F,NewVar):- functor(F,FN,_),!, upcase_atom(FN,FUP),gensym(FUP,VARNAME),NewVar = '$VAR'(VARNAME),!.

:- export(defunctionalize/2).
defunctionalize(Wff,WffO):- with_assertions(thlocal:dont_use_mudEquals,defunctionalize(',',Wff,WffO)).
defunctionalize(OP,Wff,WffO):- call_last_is_var(defunctionalize(OP,Wff,WffO)).
defunctionalize(_ ,Wff,Wff):- \+ compound(Wff),!.
defunctionalize(_ ,Wff,Wff):- non_compound(Wff),!.
defunctionalize(_ ,Wff,Wff):- leave_as_is(Wff),!.

defunctionalize(OP,(H:-B),WffO):- thlocal:dont_use_mudEquals,!,
 with_no_assertions(thlocal:dont_use_mudEquals,defunctionalize(OP,(H:-B),WffO)).
defunctionalize(OP,(H:-B),WffO):- !,
  defunctionalize(',',(B=>H),HH),
  (HH=(PreC,(NewBody=>NEWH))-> 
     defunctionalize(OP,(NEWH:not(PreC,NewBody)),WffO);
  (defunctionalize(OP,B,NewBody),WffO=(H:-NewBody))).
  
defunctionalize(OP,Wff,WffO):- 
  each_subterm(Wff,SubTerm),
  compound(SubTerm),
  \+ (is_ftEquality(SubTerm)),
  \+ (leave_as_is(SubTerm)),
  arg(_,SubTerm,Function),is_function(Function),
  subst_except(SubTerm,Function,NewVar,NewSubTerm),
  function_to_predicate(Function,NewVar,PredifiedFunction),
  subst_except(Wff,SubTerm,NewSubTerm,NextWff),!,
  defunctionalize(OP,NextWff,WffM),!,
  WffO=..[OP,PredifiedFunction,WffM].

defunctionalize(OP,Wff,WffO):-
  each_subterm(Wff,SubTerm),
  compound(SubTerm),
  not(is_ftEquality(SubTerm)),
  not(leave_as_is(SubTerm)),
  arg(_,SubTerm,Function),is_function(Function),
  subst_except(SubTerm,Function,NewVar,NewSubTerm),
  function_to_predicate(Function,NewVar,PredifiedFunction),
  NEW =..[OP,PredifiedFunction,NewSubTerm],
  subst_except(Wff,SubTerm,NEW,NextWff),!,
  defunctionalize(OP,NextWff,WffO),!.
defunctionalize(_,Wff,Wff).



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


correct_cls0(KB,cl([not(poss(H))],B),cl([z_unused(~pos(H:-B))],[])):-member(not(H),B),!.
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

correct_cls0(KB,cl(H,B),O):- member(E,B),member(not(E),B),!,incorrect_cl(cl(H,B),O).

correct_cls0(KB,cl([nesc((H))],B),cl([z_unused(nesc(H:-B))],[])):-member((H),B),!.
correct_cls0(KB,cl([nesc((H))],B),O):- delete_sublits(B,not(H),BB),BB\=@=B,!,correct_cls(KB,cl([(H)],BB),O).
correct_cls0(KB,cl([not((H))],B),O):- correct_cls(KB,cl([not(poss(H))],B),O).

correct_cls0(_KB,cl(H,B),O):- O=cl(H,B).

incorrect_cl(cl(H,B),cl([z_unused(H:-B)],[])).



:- export(correct_boxlog/4).
correct_boxlog(CLAUSES,KB,Why,FlattenedO):- (\+ is_list(CLAUSES)),!,correct_boxlog([CLAUSES],KB,Why,FlattenedO).
correct_boxlog(BOXLOG,KB,Why,FlattenedO):-
  must_det_l((  
   must_maplist(adjust_kif(KB),BOXLOG,MODAL),
   wdmsgl(modal(MODAL)),   
   must_maplist(demodal(KB),MODAL,CLAUSES),
   must_maplist(correct_cls(KB),CLAUSES,NCFs),
   must_maplist(clauses_to_boxlog(KB,Why),NCFs,ListOfLists),
   flatten([ListOfLists],Flattened),
   must_maplist(removeQ(KB),Flattened,FlattenedM),
   must_maplist(demodal(KB),FlattenedM,FlattenedO),
   wdmsgl(horn(FlattenedO)))),!.


% kif_to_boxlog('=>'(WffIn,enables(Rule)),'$VAR'('MT2'),complete,Out1), % kif_to_boxlog('=>'(enabled(Rule),WffIn),'$VAR'('KB'),complete,Out).  

kif_to_prolog(X,E):- kif_to_boxlog(X,Y),!,list_to_set(Y,S),!,member(E,S).

%====== kif_to_boxlog(+Wff,-NormalClauses):-
:- export(kif_to_boxlog/2).
% kif_to_boxlog(Wff,Out):- loop_check(kif_to_boxlog(Wff,Out),Out=looped_kb(Wff)).
kif_to_boxlog(Wff,Out):- why_to_id(rule,Wff,Why), kif_to_boxlog(Wff,Why,Out),!.
kif_to_boxlog(WffIn,Out):-  why_to_id(rule,WffIn,Why), kif_to_boxlog(all('$VAR'('KB'),'=>'(asserted_t('$VAR'('KB'),WffIn),WffIn)),'$VAR'('KB'),Why,Out).
kif_to_boxlog(WffIn,NormalClauses):- why_to_id(rule,WffIn,Why), kif_to_boxlog(WffIn,'$VAR'('KB'),Why,NormalClauses).

alt_kif_to_boxlog(not( Wff),KB,Why,Out):- !, kif_to_boxlog(not( Wff),KB,Why,Out).
alt_kif_to_boxlog(Wff,KB,Why,Out):- loop_check(kif_to_boxlog((not(nesc(not(Wff)))),KB,Why,Out),Out=looped_kb(Wff)).

:- export(kif_to_boxlog/3).
kif_to_boxlog(WffIn,Why,Out):-  kif_to_boxlog(WffIn,'$VAR'('KB'),Why,Out),!.


:- export(kif_to_boxlog/4).
kif_to_boxlog(Fml,KB,Why,Flattened):- var(KB),!,kif_to_boxlog(Fml,'$VAR'('KB'),Why,Flattened).
kif_to_boxlog(Wff,KB,Why,Out):- transitive_lc(adjust_kif(KB),Wff,M),Wff \=@= M ,!,kif_to_boxlog(M,KB,Why,Out).
kif_to_boxlog((Wff:- B),KB,Why,Flattened):- is_true(B),!, kif_to_boxlog(Wff,KB,Why,Flattened),!.
kif_to_boxlog(WffInIn,KB,Why,FlattenedO):-  as_dlog(WffInIn,WffIn),WffInIn\=@=WffIn,!,kif_to_boxlog(WffIn,KB,Why,FlattenedO),!.

% kif_to_boxlog(Wff,KB,Why,Out):- loop_check(kif_to_boxlog(Wff,KB,Why,Out),alt_kif_to_boxlog(Wff,KB,Why,Out)),!.

kif_to_boxlog((HEAD:- BODY),KB,Why,FlattenedO):-  
  must_det_l((
   check_is_kb(KB),
   conjuncts_to_list(HEAD,HEADL),conjuncts_to_list(BODY,BODYL),
   correct_boxlog(cl(HEADL,BODYL),KB,Why,FlattenedO))).

kif_to_boxlog(WffIn0,KB0,Why0,FlattenedO):-  
  nl,nl,nl,draw_line,draw_line,draw_line,draw_line,
  must_det_l((
    must(numbervars_with_names(WffIn0:KB0:Why0,WffIn:KB:Why)),      
   ensure_quantifiers(WffIn,Wff),
   wdmsgl(kif(Wff)),
   % KB = WffQ,
    check_is_kb(KB),
    must(dif(KB,Why)),
   %with_assertions(thlocal:dont_use_mudEquals,defunctionalize('=>',WffQ,Wff)),
   %(WffQ\==Wff-> dmsg(defunctionalize('=>',WffQ,Wff));wdmsgl(kif(Wff))),
   as_dlog(Wff,Wff666),
   % kb_nlit(KB,Neg),
   % original(Why)=>Wff666
   add_nesc(Wff666,Wff6667),
   add_preconds(Wff6667,Wff6668),
   adjust_kif(KB,Wff6668,Wff6669),
   wdmsgl(pkif(Wff6669)),
   nnf(KB,Wff6669,NNF),
   %wdmsgl(nnf(NNF)),
   pnf(KB,NNF,PNF),
   %wdmsgl(pnf(PNF)),
   save_wid(Why,kif,Wff),
   save_wid(Why,pkif,Wff6669),
   cf(Why,KB,PNF,NCFsI),!,
   cf_to_flattened_clauses(KB,Why,NCFsI,Flattened),
   list_to_set(Flattened,FlattenedM),!,
   correct_boxlog(FlattenedM,KB,Why,FlattenedO))).
   


check_is_kb(KB):-ignore('$VAR'('KB')=KB).

add_preconds(X,Z):-
 with_assertions(leave_as_is0('CollectionS666666666666666ubsetFn'(_,_)),
   with_assertions(thlocal:dont_use_mudEquals,defunctionalize('=>',X,Y))),add_preconds2(Y,Z).

add_preconds2(Wff6667,PreCondPOS):-
   must_det_l((get_lits(Wff6667,PreCond),list_to_set(PreCond,PreCondS),
     add_poss_to(PreCondS,Wff6667, PreCondPOS))).

add_poss_to([],Wff6667, Wff6667).
add_poss_to([PreCond|S],Wff6667, PreCondPOS):-!,
 add_poss_to(PreCond,Wff6667, PreCondM),
 add_poss_to(S,PreCondM, PreCondPOS).
 
add_poss_to(PreCond,Wff6667, PreCond=>Wff6667):-prequent(PreCond).
add_poss_to(PreCond,Wff6667, Wff6667):-leave_as_is(PreCond).
add_poss_to(not(_PreCond),Wff6667, Wff6667).
add_poss_to(PreCond,Wff6667, (poss(PreCond)=>Wff6667)).


add_nesc(X,X):-!.

add_nesc(IN,OUT):-is_list(IN),must_maplist(add_nesc,IN,OUT),!.
add_nesc(Wff666,Wff666):-leave_as_is(Wff666),!.
add_nesc(P<=>Q,O):-!,add_nesc(((P=>Q) & (Q=>P)),O).
add_nesc(PQ,PQO):- PQ=..[F,V|Q],pttp_quantifier(F),add_nesc(Q,QQ),PQO=..[F,V|QQ],!.
add_nesc(IN,poss(IN)):-IN=..[F|_],should_be_poss(F),!.
add_nesc(Wff666,Wff666):-is_modal(Wff666,_),!.
add_nesc(P=>Q,((PP & P & QP) =>Q)):-  add_poss(P,PP),add_poss(Q,QP).
add_nesc(IN,OUT):-IN=..[F|INL],logical_functor_pttp(F),!,must_maplist(add_nesc,INL,OUTL),OUT=..[F|OUTL].
add_nesc(Wff666,Wff666):-!.

add_nesc(Q,(PQ & Q)):-  add_poss(Q,PQ),!.
add_nesc((P & Q),(PQ & (P & Q))):-  add_poss(P & Q,PQ),!.
add_nesc(Wff666,Wff666):-!.
add_nesc(IN,OUT):-IN=..[F|INL],logical_functor_pttp(F),!,must_maplist(add_nesc,INL,OUTL),OUT=..[F|OUTL].
add_nesc(not(IN),not(IN)).
add_nesc(IN,(IN)).
add_nesc(IN,nesc(IN)).


% add_poss(Wff666,Wff666):-!.
% add_poss(X,X):-!.
add_poss(PQ,PQ):- var(PQ),!.
add_poss(PQ,PQO):- PQ=..[F,V,Q],pttp_quantifier(F),add_poss(Q,QQ),PQO=..[F,V,QQ],!.
add_poss(Wff666,true):-leave_as_is(Wff666),!.
add_poss(not(IN),not(IN)).
add_poss(INL,OUTC):-is_list(INL),must_maplist(add_poss,INL,OUTL),F='&',OUT=..[F|OUTL],correct_arities(F,OUT,OUTC).
add_poss(IN,OUT):-IN=..[F|INL],logical_functor_pttp(F),!,must_maplist(add_poss,INL,OUTL),OUT=..[F|OUTL].
add_poss(IN,poss(IN)).


% shall X => can X
% shall ~ X => ~ can X
% ~ shall X => can ~ X
get_lits(PQ,[]):- var(PQ),!.
get_lits(PQ,QQ):- PQ=..[F,_Vs,Q],pttp_quantifier(F),get_lits(Q,QQ).
get_lits(Wff666,[Wff666]):-leave_as_is(Wff666),!.
get_lits(not(IN),NOUT):-get_lits(IN,OUT),must_maplist(simple_negate_literal(not),OUT,NOUT).
get_lits(knows(WHO,IN),NOUT):-get_lits(IN,OUT),must_maplist(simple_negate_literal(knows(WHO)),OUT,NOUT).
get_lits(beliefs(WHO,IN),NOUT):-get_lits(IN,OUT),must_maplist(simple_negate_literal(beliefs(WHO)),OUT,NOUT).
get_lits(IN,OUTLF):-IN=..[F|INL],logical_functor_pttp(F),!,must_maplist(get_lits,INL,OUTL),flatten(OUTL,OUTLF).
get_lits(IN,[IN]).

simple_negate_literal(F,FX,X):-FX=..FXL,F=..FL,append(FL,[X],FXL),!.
simple_negate_literal(F,X,FX):-append_term(F,X,FX).

cf_to_flattened_clauses(KB,Why,NCFsI,FlattenedO):- 
 must_det_l((
   must_maplist(correct_cls(KB),NCFsI,NCFs),
   % wdmsgl(cf(NCFs)),
   must_maplist(clauses_to_boxlog(KB,Why),NCFs,ListOfLists),
   flatten([ListOfLists],Flattened),
   thglobal:as_prolog(Flattened,FlattenedL),
   list_to_set(FlattenedL,FlattenedS),
   must_maplist(demodal_sents(KB),FlattenedS,FlattenedO))),!.
  
pttp_quantifier(F):- pttp_nnf_pre_clean_functor(F,(all),[]);pttp_nnf_pre_clean_functor(F,(ex),[]).

should_be_poss(argInst).

clauses_to_boxlog(KB,Why,In,Prolog):- call_last_is_var(clauses_to_boxlog(KB,Why,In,Prolog)).
clauses_to_boxlog(KB,Why,In,Prolog):- is_list(In),must_maplist(clauses_to_boxlog(KB,Why),In,Prolog).
clauses_to_boxlog(_KB,_Why,(H:-B),(H:-B)):-!.
clauses_to_boxlog(KB,Why,In,Prolog):- correct_cls(KB,In,Mid), Mid \=@= In, !,clauses_to_boxlog(KB,Why,Mid,Prolog).

clauses_to_boxlog(_,_Why,cl([HeadIn],[]),Prolog):- !,is_lit_atom(HeadIn) -> Prolog=HeadIn ; kif_to_boxlog(HeadIn,Prolog).
clauses_to_boxlog(KB,Why,cl([],BodyIn),Prolog):-  !,
   is_lit_atom(BodyIn) -> clauses_to_boxlog(KB,Why,cl([inconsistentKB(KB)],BodyIn),Prolog);  kif_to_boxlog(not(BodyIn),Prolog).

clauses_to_boxlog(KB,_Why,cl([HeadIn],BodyIn),RET):-!, must_maplist(logical_pos(KB),BodyIn,Body), list_to_conjuncts(Body,BodyOut),!,RET=(HeadIn:- BodyOut).

clauses_to_boxlog(KB,Why,cl([H,Head|List],BodyIn),Prolog):- 
  findall(Answer,((member(E,[H,Head|List]),delete_eq([H,Head|List],E,RestHead),
    must_maplist(logical_neg(KB),RestHead,RestHeadS),append(RestHeadS,BodyIn,Body),
    clauses_to_boxlog(KB,Why,cl([E],Body),Answer))),Prolog),!.


mpred_t_tell_kif(OP2,RULE):- 
 with_assertions(thlocal:current_pttp_db_oper(mud_call_store_op(OP2)),
   (show_call(call((must(kif_tell(RULE))))))).


fix_input_vars(AIn,A):- copy_term(AIn,A),numbervars(A,672,_).

%:- export(show_boxlog/1).
%assert_boxlog(AIn):- fix_input_vars(AIn,A), as_dlog(A,AA),kif_to_boxlog(AA,B),!,must_maplist(kif_tell_boxes_undef(How,Why),B),!,nl,nl.
%:- export(show_boxlog2/2).
%assert_boxlog2(AIn):- fix_input_vars(AIn,A), with_all_dmsg((kif_to_boxlog(A,B),!,must_maplist(kif_tell_boxes_undef(How,Why),B),!,nl,nl)).



%:- export(tsn/0).
tsn:- with_all_dmsg(forall(clause(kif,C),must(C))).

% kif:- make.
tkif:- kif_test_string(TODO),kif_io(string(TODO),current_output).

:- multifile(user:sanity_test/0).
user:regression_test:- tsn.

:- thread_local(kif_action_mode/1).
:- asserta_if_new(kif_action_mode(tell)).

:- thread_local(kif_reader_mode/1).
:- asserta_if_new(kif_reader_mode(lisp)).

kif_read(InS,Wff,Vs):- must(l_open_input(InS,In)),
  must(((kif_reader_mode(lisp) ,without_must( catch(input_to_forms(In,Wff,Vs),E,(dmsg(E:kif_read_input_to_forms(In,Wff,Vs)),fail)))) *-> true ;
      catch(read_term(In,Wff,[module(user),double_quotes(string),variable_names(Vs)]),E,(dmsg(E:kif_read_term_to_forms(In,Wff,Vs)),fail)))).

%= ===== to test program =====-
:- ensure_loaded(library(logicmoo/plarkc/dbase_i_sexpr_reader)).

:- export(kif/0).
kif:- current_input(In),current_output(Out),!,kif_io(In,Out).

%open_input(InS,InS):- is_stream(InS),!.
%open_input(string(InS),In):- text_to_string(InS,Str),string_codes(Str,Codes),open_chars_stream(Codes,In),!.


:- export(kif_io/2).
kif_io(InS,Out):- 
  l_open_input(InS,In),
   repeat,             
      debugOnError((once((kif_action_mode(Mode),write(Out,Mode),write(Out,'> '))),
        kif_read(In,Wff,Vs),
         b_setval('$variable_names', Vs),
           portray_clause(Out,Wff,[variable_names(Vs),quoted(true)]),
           once(kif_process(Wff)),
           Wff == end_of_file)),!.

:- export(id_to_why/3).
why_to_id(Term,Wff,IDWhy):- not(atom(Term)),term_to_atom(Term,Atom),!,why_to_id(Atom,Wff,IDWhy).
why_to_id(Atom,Wff,IDWhy):- wid(IDWhy,Atom,Wff),!.
why_to_id(Atom,Wff,IDWhy):- must(atomic(Atom)),gensym(Atom,IDWhyI),kb_incr(IDWhyI,IDWhy),assertz_if_new(user:wid(IDWhy,Atom,Wff)).

:- export(kif_process/1).
kif_process(end_of_file):- !.
kif_process(prolog):- prolog_repl,!.
kif_process(Assert):- atom(Assert),retractall(kif_action_mode(_)),asserta(kif_action_mode(Assert)),fmtl(kif_action_mode(Assert)),!.
kif_process(Wff):- kif_action_mode(Mode),kif_process(Mode,Wff),!.

kif_process(_,':-'(Wff)):- !, kif_process(call,Wff).
kif_process(_,'?-'(Wff)):- !, kif_ask(Wff).
kif_process(_,'ask'(Wff)):- !, kif_ask(Wff).
kif_process(_,'tell'(Wff)):- !, kif_tell(Wff).
kif_process(call,Call):- !,call(Call).
kif_process(tell,Wff):- !, kif_tell(Wff).
kif_process(ask,Wff):- !, kif_ask(Wff).
kif_process(Other,Wff):- !, wdmsg(error(missing_kif_process(Other,Wff))),!,fail.

:- export(kif_ask_sent/1).
kif_ask_sent(Wff):- 
   why_to_id(ask,Wff,Why),
   term_variables(Wff,Vars),
   gensym(z_q,ZQ),
   Query=..[ZQ,666|Vars],
   kif_to_boxlog('=>'(Wff,Query),Why,QueryAsserts),!,
   kif_tell_boxes(pttp_assert_wid,Why,pttp_in,QueryAsserts),!,
   call_cleanup(
     kif_ask(Query),
     pttp_retractall_wid(Why)).


:- export(kif_ask/1).
kif_ask(P <=> Q):- kif_ask_sent(P <=> Q).
kif_ask(P => Q):- kif_ask_sent(P => Q).
kif_ask((P v Q)):- kif_ask_sent(((P v Q))).
kif_ask((P & Q)):- kif_ask_sent((P & Q)).
kif_ask(Goal0):-  logical_pos(_KB,Goal0,Goal),
    no_repeats(user:(
	add_args(Goal0,Goal,_,_,[],_,_,[],[],DepthIn,DepthOut,[PrfEnd|PrfEnd],_ProofOut1,Goal1,_),!,
        search(Goal1,60,0,1,3,DepthIn,DepthOut))).

:- export(kif_ask/2).
kif_ask(Goal0,ProofOut):- logical_pos(_KB,Goal0,Goal),
    no_repeats(user:(
	add_args(Goal0,Goal,_,_,[],_,_,[],[],DepthIn,DepthOut,[PrfEnd|PrfEnd],ProofOut1,Goal1,_),!,
        search(Goal1,60,0,1,3,DepthIn,DepthOut),
        contract_output_proof(ProofOut1,ProofOut))).

kif_tell(InS):- atom(InS),must_det_l((kif_read(string(InS),Wff,Vs),b_implode_varnames0(Vs),local_sterm_to_pterm(Wff,Wff0),kif_tell(Wff0))),!.
kif_tell(WffIn):- must_det_l((numbervars_with_names(WffIn,Wff),why_to_id(tell,Wff,Why),kif_tell(Why,Wff))),!.


local_sterm_to_pterm(Wff,WffO):- sexpr_sterm_to_pterm(Wff,WffO),!.



:-op(1000,fy,(kif_tell)).

:- export((kif_tell)/2).

kif_tell(_,[]).
kif_tell(Why,[H|T]):- !,must_det_l((kif_tell(Why,H),kb_incr(Why,Why2),kif_tell(Why2,T))).
kif_tell(Why,Wff):-  
   must_det_l((kif_to_boxlog(Wff,Why,Asserts),kif_tell_boxes(assert_wfs_def,Why,Wff,Asserts))),!.


:-thread_local(thlocal:assert_wfs/2).
assert_wfs_def(HBINFO,HB):-if_defined(thlocal:assert_wfs(HBINFO,HB)),!.
assert_wfs_def(Why,H):-assert_wfs_fallback(Why,H).

assert_wfs_fallback(Why, HB):- subst(HB,(~),(-),HB2),subst(HB2,(not_proven_t),(not_true_t),HB1),subst(HB1,(poss),(possible_t),HBO),assert_wfs_fallback0(Why, HBO).
assert_wfs_fallback0(Why,(H:-B)):- adjust_kif('$VAR'(KB),B,HBK),demodal('$VAR'(KB),HBK,HBKD),
   wdmsg((H:-w_infer_by(Why),HBKD)),pttp_assert_wid(Why,pttp_in,(H:-B)),!.
assert_wfs_fallback0(Why, HB):- adjust_kif('$VAR'(KB),HB,HBK),demodal('$VAR'(KB),HBK,HBKD),
   wdmsg((HBKD:-w_infer_by(Why))),pttp_assert_wid(Why,pttp_in,(HB)),!.




kif_tell_boxes(How,Why,Wff0,Asserts0):-
 must_det_l((
  show_call_failure(kif_unnumbervars(Asserts0+Wff0,Asserts+Wff)),  
  %fully_expand(Get1,Get),
  get_constraints(Wff,Isas), 
  kif_tell_adding_constraints(Why,Isas,Asserts))),
   findall(HB-WhyHB,retract(thlocal:in_code_Buffer(HB,WhyHB,_)),List),
   list_to_set(List,Set),
   forall(member(HB-WhyHB,Set),
      call(How,WhyHB,HB)).


kif_tell_adding_constraints(Why,Isas,Get1Get2):- var(Get1Get2),!,trace_or_throw(var_kif_tell_isa_boxes(Why,Isas,Get1Get2)).
kif_tell_adding_constraints(Why,Isas,(Get1,Get2)):- !,kif_tell_adding_constraints(Why,Isas,Get1),kb_incr(Why,Why2),kif_tell_adding_constraints(Why2,Isas,Get2).
kif_tell_adding_constraints(Why,Isas,[Get1|Get2]):- !,kif_tell_adding_constraints(Why,Isas,Get1),kb_incr(Why,Why2),kif_tell_adding_constraints(Why2,Isas,Get2).
kif_tell_adding_constraints(_,_,[]).
kif_tell_adding_constraints(_,_,z_unused(_)):-!.
kif_tell_adding_constraints(Why,Isas,((H:- B))):- conjoin(Isas,B,BB), kif_tell_boxes1(Why,(H:- BB)).
kif_tell_adding_constraints(Why,Isas,((H))):- kif_tell_boxes1(Why,(H:- Isas)).

kif_tell_boxes1(_,[]).
kif_tell_boxes1(Why,List):- is_list(List),!,list_to_set(List,[H|T]),must_det_l((kif_tell_boxes1(Why,H),kb_incr(Why,Why2),kif_tell_boxes1(Why2,T))).
kif_tell_boxes1(_,z_unused(_)):-!.
kif_tell_boxes1(Why,AssertI):- must_det_l((simplify_bodies(AssertI,AssertO),kif_tell_boxes3(save_wfs,Why,AssertO))).

:-thread_local(thlocal:in_code_Buffer/3).


kif_tell_boxes3(How,Why,Assert):- 
  must_det_l((
  boxlog_to_prolog(Assert,Prolog1),
  defunctionalize(Prolog1,Prolog2),
  kif_unnumbervars(Prolog2,PTTP), 
  call(How,Why,PTTP))).

kif_unnumbervars(X,YY):-
 must_det_l((
   with_output_to(string(A),write_term(X,[character_escapes(true),ignore_ops(true),quoted(true)])),
   atom_to_term(A,Y,NamedVars),
   YY=Y,
   add_newvars(NamedVars))).


simplify_bodies((H:- B),(H:- BC)):- must_det_l((conjuncts_to_list(B,RB),simplify_list(_KB,RB,BB),list_to_conjuncts(BB,BC))).
simplify_bodies((B),(BC)):- must_det_l((conjuncts_to_list(B,RB),simplify_list(_KB,RB,BB),list_to_conjuncts(BB,BC))).


simplify_list(KB,RB,BBS):- list_to_set(RB,BB),must_maplist(removeQ(KB),BB,BBO),list_to_set(BBO,BBS).

save_wfs(Why,PrologI):- must_det_l((thglobal:as_prolog(PrologI,Prolog), 
   with_assertions(thlocal:current_why(Why,Prolog),
   pfc_add_h(save_in_code_buffer,Why,Prolog)))).

nots_to(H,To,HH):-subst_except(H,neg,To,HH),subst_except(H,-,To,HH),subst_except(H,~,To,HH),subst_except(H,neg,To,HH),!.
neg_h_if_neg(H,HH):-nots_to(H,'~',HH).
neg_b_if_neg(HBINFO,B,BBB):-nots_to(B,'~',BB),sort_body(HBINFO,BB,BBB),!.


sort_body(HBINFO,BB,BBB):-sort_body_0(HBINFO,BB,BBB),(BBB=@=BB->true; (expand_to_hb(HBINFO,H,_),nop(dmsg([(H:-BB),'=>',(H:-BBB)])))).

sort_body_0(_,SORTED,SORTED):-leave_as_is(SORTED).
sort_body_0(HBINFO,(A,B),SORTED):-!,conjuncts_to_list((A,B),List),
   must_maplist(sort_body_0(HBINFO),List,ListIn),
   predsort(litcost_compare(HBINFO),ListIn,SortedL),
   list_to_conjuncts(SortedL,SORTED).
sort_body_0(HBINFO,(A;B),SORTED):-!,disjuncts_to_list((A;B),List),
   must_maplist(sort_body_0(HBINFO),List,ListIn),
   predsort(litcost_compare(HBINFO),ListIn,SortedL),
   list_to_conjuncts((;),SortedL,SORTED).
sort_body_0(_,SORTED,SORTED).

litcost_compare(_,=,A,B):- A=@=B,!.
litcost_compare(HBINFO,Comp,A,B):-lit_cost(HBINFO,A,AC),lit_cost(HBINFO,B,BC),compare(CompC,AC,BC),
  (CompC\== (=) -> CompC = Comp ; Comp = (<)).

lit_cost(_,A,9):-isSlot(A).
lit_cost(_,A,0):- \+ compound(A),!.
lit_cost(HBINFO,A,AC):- A=..[F,ARG], is_log_op(F),!,lit_cost(HBINFO,ARG,AC0),!,
 % this removes the headvar bonus
  term_slots(A,Slots),length(Slots,SC),
  AC is AC0+SC.
lit_cost(HBINFO,A,AC):- expand_to_hb(HBINFO,H,B),
  var_count_num(A,H,SH,UH),
  var_count_num(A,B,VC,Singles),
  AC is Singles*3 + VC + UH - SH.

simp_code(HB,(H:-BS)):-expand_to_hb(HB,H,B),conjuncts_to_list(B,BL),sort(BL,BS),!.
simp_code(A,A).


var_count_num(Term,SharedTest,SharedCount,UnsharedCount):- term_slots(Term,Slots),term_slots(SharedTest,TestSlots),
  subtract(Slots,TestSlots,UnsharedSlots),
  subtract(Slots,UnsharedSlots,SharedSlots),
  length(SharedSlots,SharedCount),
  length(UnsharedSlots,UnsharedCount).

pfc_add_h(How,Why,(H:- B)):- neg_h_if_neg(H,HH), neg_b_if_neg((HH:- B),B,BB),!,call(How,Why,(HH:-BB)).
pfc_add_h(How,Why,(H)):- neg_h_if_neg(H,HH), call(How,Why,(HH)).

save_in_code_buffer(_ ,HB):- simp_code(HB,SIMP),thlocal:in_code_Buffer(HB,_,SIMP),!.
save_in_code_buffer(Why,HB):- simp_code(HB,SIMP),assert(thlocal:in_code_Buffer(HB,Why,SIMP)).

use_was_isa_h(_,ftTerm,true):- !.
use_was_isa_h(_,argi(mudEquals,_),true):- !.
use_was_isa_h(_,argi(skolem,_),true):- !.
use_was_isa_h(I,T,ISA):- to_isa_out(I,T,ISA),!.

generate_ante([],[],InOut,InOut).
generate_ante([I|VarsA],[T|VarsB],In,Isas):- use_was_isa_h(I,T,ISA), conjoin(In,ISA,Mid),generate_ante(VarsA,VarsB,Mid,Isas).

get_constraints(T,true):- T==true.
get_constraints(_,true):- !.
get_constraints(ListA,Isas):- 
     must_det_l((copy_term(ListA,ListB),
      term_variables(ListA,VarsA),
      term_variables(ListB,VarsB),
      attempt_attribute_args(isAnd,ftAskable,ListB),
      attribs_to_atoms(VarsB,VarsB),
      generate_ante(VarsA,VarsB,true,Isas))).


boxlog_to_prolog(IN,OUT):-notrace(leave_as_is(IN)),!,IN=OUT.
boxlog_to_prolog(IN,OUT):-once(demodal_sents('$VAR'('KB'),IN,MID)),IN\=@=MID,!,boxlog_to_prolog(MID,OUT).
boxlog_to_prolog(IN,OUT):-once(subst_except(IN,neg,~,MID)),IN\=@=MID,!,boxlog_to_prolog(MID,OUT).
boxlog_to_prolog(IN,OUT):-once(subst_except(IN,poss,possible_t,MID)),IN\=@=MID,!,boxlog_to_prolog(MID,OUT).
boxlog_to_prolog(H, HH):-is_list(H),!,must_maplist(boxlog_to_prolog,H,HH).

boxlog_to_prolog((V:- TRUE),VE):- is_true(TRUE),boxlog_to_prolog(V,VE),!.
boxlog_to_prolog((H:- B),(HH:- BB)):- !,boxlog_to_prolog(H,HH),boxlog_to_prolog(B,BB).
boxlog_to_prolog((H & B),(HH , BB)):- !,boxlog_to_prolog(H,HH),boxlog_to_prolog(B,BB).
boxlog_to_prolog((H v B),(HH ; BB)):- !,boxlog_to_prolog(H,HH),boxlog_to_prolog(B,BB).
boxlog_to_prolog((H , B),(HH , BB)):- !,boxlog_to_prolog(H,HH),boxlog_to_prolog(B,BB).
boxlog_to_prolog((H ; B),(HH ; BB)):- !,boxlog_to_prolog(H,HH),boxlog_to_prolog(B,BB).
boxlog_to_prolog(H,O):- H=..[N,nesc(F)],kb_nlit(_,N),nonvar(F),!,HH=..[N,F],boxlog_to_prolog(HH,O).

/*
boxlog_to_prolog(nesc(not(F)),O):- nonvar(F),!,boxlog_to_prolog(neg(F),O).
boxlog_to_prolog(nesc(F),O):- nonvar(F),!,boxlog_to_prolog(F,O).
boxlog_to_prolog(not(nesc(F)),O):- nonvar(F),!,boxlog_to_prolog(naf(F),O).
boxlog_to_prolog(~poss(F),O):-nonvar(F),!,boxlog_to_prolog(not_poss(F),O).
boxlog_to_prolog(not(H),not(HH)):- !,boxlog_to_prolog(H,HH).
boxlog_to_prolog(not(F),neg(O)):- nonvar(F),!,boxlog_to_prolog(F,O).
*/

boxlog_to_prolog(IN,OUT):-demodal_sents(_KB,IN,M),IN\=@=M,!,boxlog_to_prolog(M,OUT).


boxlog_to_prolog( H, HH):- H=..[F|ARGS],!,boxlog_to_prolog(ARGS,ARGSO),!,HH=..[F|ARGSO].
boxlog_to_prolog(BL,PTTP):- thglobal:as_prolog(BL,PTTP).


:- dynamic(kif_pred_head/1).
:- style_check(-singleton).

kif_pred_head(P):- var(P),!,isa(F,prologKIF),arity(F,A),functor(P,F,A).
kif_pred_head(P):- get_functor(P,F,_),isa(F,prologKIF).
kif_pred_head(P):- get_functor(P,F,_),isa(F,prologPTTP).


:- dynamic(pttp_pred_head/1).

pttp_pred_head(P):- var(P),isa(F,prologPTTP),arity(F,A),functor(P,F,A).
pttp_pred_head(P):- get_functor(P,F,_),isa(F,prologPTTP).

:- multifile(kify_comment/1).


pttp_listens_to_head(_OP,P):- pttp_pred_head(P).

pttp_listens_to_stub(prologPTTP).
pttp_listens_to_stub(prologKIF).


user:provide_mpred_setup(Op,H):- provide_kif_op(Op,H).

% OPHOOK ASSERT
provide_kif_op(change(assert,How),(HeadBody)):- 
   pttp_listens_to_head(change(assert,How),HeadBody),
   why_to_id(provide_kif_op,(HeadBody),ID),
   kif_tell(ID,(HeadBody)).

% OPHOOK CALL
provide_kif_op(call(How),Head):- 
  pttp_listens_to_head(call(How),Head),
  pttp_call(Head).

% OPHOOK CLAUSES
provide_kif_op(clauses(How),(Head:- Body)):- 
   pttp_listens_to_head(clauses(How),Head),
   provide_mpred_storage_clauses(Head,Body,_Why).

% OPHOOK 
provide_kif_op(OP,(HeadBody)):- 
   pttp_listens_to_head(OP,HeadBody),
   kif_process(OP,HeadBody).


% CLAUSES HOOK 
user:provide_mpred_storage_clauses(H,B,wid3(IDWhy)):- wid(IDWhy,_,(H:- B)).
user:provide_mpred_storage_clauses(H,true,wid3(IDWhy)):- wid(IDWhy,_,(H)),compound(H),not(functor(H,':-',2)).


% REGISTER HOOK
user:provide_mpred_setup(OP,HeadIn,StubType,RESULT):-  pttp_listens_to_stub(StubType),!,
   get_pifunctor(HeadIn,Head,F),
      assert_if_new(isa(F,prologPTTP)),
         ensure_universal_stub(Head),
         RESULT = declared(pttp_listens_to_head(OP,Head)).


/*

:- dynamic(user:int_proven_t/10).

int_proven_t(P, X, Y, E, F, A, B, C, G, D):- t(P,X,Y),
        test_and_decrement_search_cost(A, 0, B),
        C=[H, [true_t(P, X, Y), D, E, F]|I],
        G=[H|I].


:- dynamic(user:int_assumed_t/10).
int_assumed_t(P, X, Y, E, F, A, B, C, G, D):- t(P,X,Y),
        test_and_decrement_search_cost(A, 0, B),
        C=[H, [assumed_t(P, X, Y), D, E, F]|I],
        G=[H|I].


*/
kif_test_string(
"
% )
tell.

all(R,room(R) => exists(D, (door(D) & has(R,D)))).
room(room1).

ask.

room(What).

door(What).

:- kif_tell(a(XX) & b(XX) => c(XX)).
:- kif_tell(all(R,room(R) => exists(D, (door(D) & has(R,D))))).
:- kif_tell(loves(Child,fatherFn(Child))).
:- kif_tell((p => q)).
:- kif_tell(~p <=> ~q).
:- kif_tell(p <=> q).
:- kif_tell(all(P, person(P) => -exists(D, dollar(D) & has(P,D)))).

:- kif_tell(go(sam) & (go(bill) v go(sally) ) & go(nancy)).

:- kif_tell(rains_tuesday => wear_rain_gear xor carry_umbrella).
:- kif_tell(exists(P, (person(P) & all(C, car(C) => ~has(P,C))))).

:- kif_tell(room(R) => exists(D, (door(D) & has(R,D)))).
:- kif_tell((goes(jane) xor goes(sandra) => goes(bill))).
:- kif_tell(exists(P, exists(C, (person(P) & car(C) & has(P,C))))).
:- kif_tell(~all(P,person(P) => exists(C, car(C) & has(P,C)))).
:- kif_tell((go(sam) & go(bill)) v (go(sally) & go(nancy))).
:- kif_tell(go(sam) & (go(bill) v go(sally) ) & go(nancy)).
:- kif_tell(exists(C, course(C) & exists(MT1, midterm(C,MT1) & exists(MT2, midterm(C,MT2) & different(MT1,MT2))))).
:- kif_tell(exists(C, course(C) & ~exists(MT3, midterm(C,MT3)))).

"
).



% skolem_fn

nnf_label(KB,exists(X,Fml),FreeV,NNF,Paths):-
   must_det_l((
         list_to_set([X|FreeV],NewVars),
         nnf(KB,Fml,NewVars,NNFMid,_Paths),
         skolem_fn(KB, NNFMid, X, FreeV, Fun, SkVars),
         SKF =.. [Fun|SkVars],
         subst_except(NNFMid,X,SKF,FmlSk),
         % MAYBE CLOSE nnf(KB,((mudEquals(X,SKF) => ~FmlSk)v Fml),NewVars,NNF,Paths).
         %nnf(KB,  (((skolem(X,SKF))=>NNFMid) & FmlSk) ,NewVars,NNF,Paths))).
        % GOOD nnf(KB, isa(X,SKF) => (skolem(X,SKF)=>(NNFMid)) ,NewVars,NNF,Paths))).
         nnf(KB, skolem(X,SKF) => NNFMid ,NewVars,NNF,Paths))).


/*
:- told.
:- dmsg_show(_).
:- dmsg('i see this').
:- kif_tell(exists(C, course(C) & ~exists(MT3, midterm(C,MT3)))).
:- kif_test_string(TODO),kif_io(string(TODO),current_output).
:- set_no_debug.
:- notrace.
:- nodebug.

:- wdmsg('we see this').

:- kif_tell((p => q)).
:- kif_tell(~p <=> ~q).
:- kif_tell(tRoom(R) => exists(D, (tDoor(D) & has(R,D)))).
:- kif_tell(all(P, person(P) => ~(exists(D, dollar(D) & has(P,D))))).
:- kif_tell(p <=> q).
:- kif_tell(all(P, person(P) => exists(D, dollar(D) & has(P,D)))).
*/
kif_result(_).
:- export((kif_test)/1).
kif_test(X):-kif_tell(X).
:-op(1000,fy,(kif_test)).
:- assert_until_eof(thlocal:canonicalize_types).

kif_sanity_test_0:-kif_test(all(R, exists(D, room(R) => (door(D) & has(R,D))))).

kif_sanity_test_0:-kif_test(p(A,R) & q(A,R)).


:- kif_result(
(=> pfc_default((
   room(R) => 
      {D = skIsDoorInRoomArg2ofHasFn(R)},has(R,D) & door(D))))).






kif_sanity_test_0:- kif_test(loves(fatherFn(Child),Child)).


% :- prolog.
%:- must(((kif_test(isa(F,tPred) => exists(A, (isa(A,ftInt) & arity(F,A))))))).

:-nop(( kif_result(
(=> pfc_default((
   tPred(F) => 
      {A = skIsIntInPredArg2ofArityFn(F)},arity(F,A) & ftInt(A))
 ))))).


kif_sanity_test_0:-kif_test'(relationAllExists causes-EventEvent Exhibitionism VisualEvent)'.

kif_sanity_test_0:-kif_test '(relationAllExists properSubEvents Exhibitionism (DisplayingFn SexOrgan))'.


kif_sanity_test_0:-kif_test '(knows UnitedStatesOfAmerica (thereExists ?THING  (and  (assets ChevronCorporation ?THING)  (objectFoundInLocation ?THING Kazakhstan))))'.
kif_sanity_test_0:-kif_test '
(not (beliefs UnitedStatesOfAmerica (not (thereExists ?THING  (and  (assets ChevronCorporation ?THING)  (objectFoundInLocation ?THING Kazakhstan))))))
'.
kif_sanity_test_0:-kif_test '
(not (beliefs UnitedStatesOfAmerica (not (forAll ?THING  (not (and  (assets ChevronCorporation ?THING)  (objectFoundInLocation ?THING Kazakhstan)))))))
'.
kif_sanity_test_0:-kif_test '
(knows UnitedStatesOfAmerica (not (forAll ?THING  (not (and  (assets ChevronCorporation ?THING)  (objectFoundInLocation ?THING Kazakhstan))))))
'.

kif_sanity_test_0:-kif_test '
(knows UnitedStatesOfAmerica (and  KA KB KC KD))
'.

kif_sanity_test_0:-kif_test '
(beliefs UnitedStatesOfAmerica (and  BA BB BC BD))
'.

kif_sanity_test_0:-kif_test '
(knows UnitedStatesOfAmerica (or  KOA KOB KOC KOD))
'.

kif_sanity_test_0:-kif_test '
(beliefs UnitedStatesOfAmerica (or  BOA BOB BOC BOD))
'.

kif_sanity_test_0:-kif_test '
(implies 
       (and 
           (different ?THEMAN ?WOMAN) 
           (intendedMaleficiary ?CRIME ?THEMAN) 
           (deliberateActors ?CRIME ?WOMAN) 
           (behaviorCapable ?THEMAN 
               (CollectionSubsetFn Punishing 
                   (TheSetOf ?RESPONSE 
                       (maleficiary ?RESPONSE ?WOMAN))) deliberateActors)) 

       (optionAvailableToAgent-SitType ?THEMAN 
           (CollectionSubsetFn 
               (AttemptingFn Punishing) 
               (TheSetOf ?RETALIATION 
                   (and 
                       (intendedMaleficiary ?RETALIATION ?WOMAN) 
                       (purposeInEvent ?THEMAN ?RETALIATION 
                           (not 
                               (thereExists ?ANOTRACT 
                                   (and 
                                       (isa ?ANOTRACT PurposefulAction) 
                                       (startsAfterEndingOf ?ANOTRACT ?CRIME) 
                                       (maleficiary ?ANOTRACT ?THEMAN) 
                                       (deliberateActors ?ANOTRACT ?WOMAN)))))))) deliberateActors))'.

%:-prolog.

kif_sanity_test_0:-kif_test '
(implies
       (and 
           (isa ?AGREEMENT Agreement) 
           (intangibleParts ?AGREEMENT ?OBLIGATION) 
           (isa ?OBLIGATION Obligation) 
           (agreeingAgents ?AGREEMENT ?WOMAN) 
           (agentViolatesObligation ?WOMAN ?OBLIGATION)) 
       (agentViolatesAgreement ?WOMAN ?AGREEMENT))'.

% :-prolog.

kif_sanity_test_0:-kif_test '
(implies 
       (and 
           (isa ?SEEING VisualEvent) 
           (objectActedOn ?SEEING ?WOMAN) 
           (isa ?WOMAN ExhibitionistOffender) 
           (actorPartsInvolved ?SEEING ?PART-TYPE) 
           (physicalPartTypes Eyes ?PART-TYPE) 
           (performedBy ?SEEING ?THEMAN)) 
       (increases-Generic ?SIT 
           (relationExistsInstance bodilyDoer 
               Shaming ?THEMAN) probability-Generic))'.


kif_sanity_test_0:-kif_test '
(implies 
       (and 
           (isa ?ACT CriminalAct) 
           (isa ?ACT Exhibitionism) 
           (perpetrator ?ACT ?PERP)) 
       (isa ?PERP ExhibitionistOffender))'.


kif_sanity_test_0:-kif_test '
(implies 
       (and 
           (isa ?PUNISH Punishing) 
           (performedBy ?PUNISH ?THEMAN) 
           (maleficiary ?PUNISH ?WOMAN)) 
       (beliefs ?THEMAN 
           (thereExists ?OBLIGATION 
               (agentViolatesObligation ?WOMAN ?OBLIGATION))))'.

kif_sanity_test_0:-kif_test '
(implies (and (isa ?MORAL-SHAMING Shaming)  (performedBy ?MORAL-SHAMING ?THEMAN)  (obligatedAgents TheGoldenRule ?THEMAN)) (agentViolatesObligation ?THEMAN TheGoldenRule))
'.

kif_sanity_test_0:-kif_test '
(thereExists ?THEMAN (implies 
   (thereExists ?MORAL-SHAMING (and (isa ?MORAL-SHAMING Shaming) (performedBy ?MORAL-SHAMING ?THEMAN)  (obligatedAgents TheGoldenRule ?THEMAN)))
   (agentViolatesObligation ?THEMAN TheGoldenRule)))'.


kif_sanity_test_0:-kif_test '
(implies 
       (and 
           (isa ?INST1 Exhibitionism) 
           ((PresentTenseVersionFn doneBy) ?INST1 ?INST2)) 
       (isa ?INST2 ExhibitionistOffender))'.


kif_sanity_test_0:-kif_test '
(implies 
       (and 
           (isa ?MS VisualEvent) 
           (actorPartsInvolved ?MS ?MP) 
           (isa ?MP Eyes)) 
       (holdsIn ?MS 
           (portalState ?MP OpenPortal)))'.

kif_sanity_test_0:-kif_test '
(implies 
       (and 
           (performedBy ?ACT ?WOMAN)
           (isa ?ACT (DisplayingFn SexOrgan))           
           (lawProscribesActType ?LAW Exhibitionism) 
           (subjectToCOC ?WOMAN ?LAW)) 
       (and 
           (isa ?ACT Exhibitionism) 
           (agentViolatesObligation ?WOMAN ?LAW)))'.

kif_sanity_test_0:-kif_test '
(not 
       (and 
           (subjectToCOC ?SUNBATHER KeepAreolaCoveredInPublic) 
           (objectFoundInLocation ?SUNBATHER ?BEACH) 
           (isa ?BEACH ToplessBeach)))'.


kif_sanity_test_0:-kif_test '
(implies 
       (and 
           (isa ?COC LegalCode-ModernWestern) 
           (isa ?ACT Exhibitionism) 
           (subjectToCOC ?WOMAN ?COC)
           (agentViolatesObligation ?WOMAN KeepAreolaCoveredInPublic) 
           (performedBy ?ACT ?WOMAN)) 
       (ist ?COC 
           (isa ?ACT CriminalAct)))'.

kif_sanity_test_0:-kif_test '
(implies 
       (and 
           (isa ?AREOLA 
               (BodyPartCollectionFn ?WOMAN Areola)) 
           (subjectToCOC ?WOMAN KeepAreolaCoveredInPublic) 
           (locationState ?WOMAN InPublic)) 
       (thereExists ?CLOTH 
           (and 
               (or 
                   (agentViolatesObligation ?WOMAN KeepAreolaCoveredInPublic) 
                   (covers-Generic ?CLOTH ?AREOLA)) 
               (or 
                   (agentViolatesObligation ?WOMAN KeepAreolaCoveredInPublic) 
                   (wearsClothing ?WOMAN ?CLOTH)))))'.


kif_sanity_test_0:-kif_test '
(implies 
       (and 
           (different ?THEMAN ?WOMAN) 
           (intendedMaleficiary ?CRIME ?THEMAN) 
           (deliberateActors ?CRIME ?WOMAN) 
           (behaviorCapable ?THEMAN 
               (CollectionSubsetFn Punishing 
                   (TheSetOf ?RESPONSE 
                       (maleficiary ?RESPONSE ?WOMAN))) deliberateActors)) 

       (optionAvailableToAgent-SitType ?THEMAN 
           (CollectionSubsetFn 
               (AttemptingFn Punishing) 
               (TheSetOf ?RETALIATION 
                   (and 
                       (intendedMaleficiary ?RETALIATION ?WOMAN) 
                       (purposeInEvent ?THEMAN ?RETALIATION 
                           (not 
                               (thereExists ?ANOTRACT 
                                   (and 
                                       (isa ?ANOTRACT PurposefulAction) 
                                       (startsAfterEndingOf ?ANOTRACT ?CRIME) 
                                       (maleficiary ?ANOTRACT ?THEMAN) 
                                       (deliberateActors ?ANOTRACT ?WOMAN)))))))) deliberateActors))'.

kif_sanity_test_0:-kif_test '
(beliefs InternationalCommunity 
       (thereExists ?WEAP 
           (and 
               (isa ?WEAP ChemicalWeapon) 
               (possesses Israel ?WEAP))))'.




:-if((fail)).

kif_sanity_test_0:-kif_test '
(implies 
       (hasBeliefSystems ?WOMAN Karma) 
       (beliefs ?WOMAN 
           (implies 
               (and 
                   (isa ?MORAL-SHAMING Shaming)
                   (isa ?ANY Punishing)
                   (sinner ?MORAL-SHAMING ?THEMAN) 
                   (isa ?THEMAN 
                       (IncarnationPhysicalFn ?SOUL Organism-Whole)) 
                   (not 
                       (punishmentFor ?THEMAN ?ANY 
                           (sinner ?MORAL-SHAMING ?THEMAN)))) 
               (thereExists ?NEXTLIFE 
                   (thereExists ?PUN 
                       (and 
                           (isa ?PUN Punishing) 
                           (startsAfterEndingOf ?NEXTLIFE ?THEMAN) 
                           (isa ?NEXTLIFE 
                               (IncarnationPhysicalFn ?SOUL Organism-Whole)) 
                           (punishmentFor ?NEXTLIFE ?PUN 
                               (sinner ?MORAL-SHAMING ?THEMAN))))))))'.


kif_sanity_test_0:-kif_test '
(implies 
       (and 
           (isa ?ACTION PurposefulAction) 
           (eventOccursAt ?ACTION ?LOCATION) 
           (geographicalSubRegions ?LAND ?LOCATION) 
           (territoryOf ?COUNTRY ?LAND) 
           (isa ?COUNTRY IndependentCountry) 
           (beliefs ?COUNTRY 
               (directingAgent ?ACTION ?AGENT))) 
       (causes-SitProp ?ACTION 
           (beliefs ?COUNTRY 
               (behaviorCapable ?AGENT 
                   (CollectionSubsetFn PurposefulAction 
                       (TheSetOf ?OBJ 
                           (eventOccursAt ?OBJ ?LAND))) directingAgent))))'.

:-endif.
:-if(if_defined(show_argtype_tests)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% this rule ...

kif_sanity_test_0:-kif_test((   wearing(A,B)  => has(A,B)  )).

% has to qualify argument types before canicalization

kif_sanity_test_0:-  kif_test((argInst(has,1,A) & argInst(has,2,B) => (wearing(A,B) => has(A,B)))).

% Which produced this code:
%
%       has(A, B):-wearing(A, B), argInst(has, 1, A), argInst(has, 2, B).
%
%       not_wearing(A, B):- not_has(A, B), argInst(has, 1, A), argInst(has, 2, B).  % why are the last two litterals so important? 
%
%       not_argInst(has, 1, A):- not_has(A, B), wearing(A, B), argInst(has, 2, B).   % notice we can disprove types
%
%       not_argInst(has, 2, A):- not_has(B, A), wearing(B, A), argInst(has, 1, B).


%


kif_sanity_test_0:-kif_test(has(A,B) => (argInst(has, 1, A) & argInst(has, 2, B))).

%         not_has(A, _):- not_argInst(has, 1, A).
%
%         argInst(has, 1, A):-has(A, _).
%
%         not_has(_, B)):- not_argInst(has, 2, B).
%
%         argInst(has, 2, A):-has(_, A).



kif_sanity_test_0:-kif_test(has(A,B) =>  (kb_argInst(KB, has, 1, A) & kb_argInst(KB, has, 2, B))).

% BAD!
%         (( not_has(A, _)):- not_kb_argInst( _BAD, has, 1, A)).
%
%          (kb_argInst( _BAD, has, 1, A):-has(A, _)).
%
%           (( not_has(_, A)):- not_kb_argInst( _BAD ,  has, 2, A)).
%
%            (kb_argInst( _BAD, has, 2, A):-has(_, A)).



% :- prolog.
% GOOD! (the software does this for us but wanted to show the singlton in the consequent on the conjuction)

kif_sanity_test_0:-kif_test(   argInst(kb_argInst, 1 ,KB) =>  (        has(A,B) =>  (kb_argInst(KB, has, 1, A) & kb_argInst(KB, has, 2, B)))).

%     (( not_argInst(kb_argInst, 1,KB)):-has(A, _),  not_kb_argInst(KB, has, 1, A)).
%
%     (( not_has(A, _)):-argInst(kb_argInst, 1,KB),  not_kb_argInst(KB, has, 1, A)).
%
%     (kb_argInst(KB, has, 1, A):- argInst(kb_argInst, 1,KB), has(A, _)).
%
%    (( not_argInst(kb_argInst, 1,KB)):-has(_, B),  not_kb_argInst(KB, has, 2, B)).
%
%     (( not_has(_, B)):-argInst(kb_argInst, 1,KB),  not_kb_argInst(KB, has, 2, B)).
%
%    (kb_argInst(KB, has, 2, B):-argInst(kb_argInst, 1,KB), has(_, B)).

% EVEN BETTER?
kif_sanity_test_0:-kif_test(   argInst(kb_argInst, 1 ,KB) & argInst(has, 1 , A) & argInst(has, 2 , B) =>  (  has(A,B) =>  (kb_argInst(KB, has, 1, A) & kb_argInst(KB, has, 2, B)))).


%   pfc_add= (not_has(A, B)):- not_kb_argInst(C, has, 1, A), argInst(has, 2, B), argInst(kb_argInst, 1, C), argInst(has, 1, A)).
%
%   pfc_add= (kb_argInst(C, has, 1, A):-has(A, B), argInst(has, 2, B), argInst(kb_argInst, 1, C), argInst(has, 1, A)).
%
%   pfc_add= (not_argInst(has, 2, A)):-has(B, A), not_kb_argInst(C, has, 1, B), argInst(kb_argInst, 1, C), argInst(has, 1, B)).
%
%   pfc_add= (not_argInst(kb_argInst, 1, A)):-has(B, C), not_kb_argInst(A, has, 1, B), argInst(has, 2, C), argInst(has, 1, B)).
%
%   pfc_add= (not_argInst(has, 1, A)):-has(A, B), not_kb_argInst(C, has, 1, A), argInst(has, 2, B), argInst(kb_argInst, 1, C)).
%
%   (not_has(C, A)):- not_kb_argInst(B, has, 2, A), argInst(has, 2, A), argInst(kb_argInst, 1, B), argInst(has, 1, C)).
%
%   (kb_argInst(B, has, 2, A):-has(C, A), argInst(has, 2, A), argInst(kb_argInst, 1, B), argInst(has, 1, C)).
%
%   (not_argInst(has, 2, A)):-has(C, A), not_kb_argInst(B, has, 2, A), argInst(kb_argInst, 1, B), argInst(has, 1, C)).
%
%   (not_argInst(kb_argInst, 1, A)):-has(C, B), not_kb_argInst(A, has, 2, B), argInst(has, 2, B), argInst(has, 1, C)).
%
%   (not_argInst(has, 1, A)):-has(A, B), not_kb_argInst(C, has, 2, B), argInst(has, 2, B), argInst(kb_argInst, 1, C)).


:-endif. %if_defined(show_argtype_tests)


kif_sanity_test_0:-kif_test(all(R,isa(R,tAgent) => exists(D, (isa(D,tNose) & mudContains(R,D))))).


user:sanity_test:- kif_test(all(R,'=>'(room(R) , exists(D, '&'(door(D) , has(R,D)))))).

user:sanity_test:- kif_to_boxlog(not((a , b ,  c , d)),S),!,disjuncts_to_list(S,L),
  list_to_set(L,SET),forall(member(P,SET),writeln(P)),!.

user:sanity_test:- logicmoo_example3.

user:regression_test:- logicmoo_example3.

:- user:ensure_loaded(logicmoo(plarkc/logicmoo_i_call_kb)).

kif_sanity_tests:- forall(clause(kif_sanity_test_0,B),must(B)).

:- if(gethostname(ubuntu)).
% :- logicmoo_example3.

%:- prolog.
:- endif.

:- ensure_loaded(logicmoo('pfc/foImplies.pfc')).

:- kif_test(all(X, (~tNotFly(X) => ~tPengin(X)))).
:- kif_test(not(and(omitArgIsa(RELN, N), argIsa(RELN, N, _THING)))).


:- kif_result((tNotFly(X):-tPengin(X))).
   % we prove we dont yet know if something not a pengiun when we call notFly and it fails
:- kif_result((  neg(tPengin(A)) :-  ~tNotFly(A)  )).


:- initialization(uses_logic(logicmoo_kb_refution)).


:- if_startup_script(tkif).
:- if_startup_script(ensure_loaded(logicmoo_i_mpred_kif_testing)).



