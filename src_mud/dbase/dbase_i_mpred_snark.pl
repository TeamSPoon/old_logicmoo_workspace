/** <module> dbase_i_mpred_pttp
% Provides a prolog database replacement that uses an interpretation of SNARK
%
%  dbase_t/N
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

%= FORMULA SYNTAX
%=
%= -(A)
%= '&'(F, F)
%= v(F, F)
%= '=>'(F, F)
%= '<=>'(F, F)
%=    all(X,A)
%=    exists(X,A)
%=    atleast(X,N,A)
%=    atmost(X,N,A)
/*
:- module(dbase_i_snark, 
          [ 
           nnf/4, 
           pnf/3, pnf/2, cf/4,
          tsn/0,
          op(300,fx,'-'),
          op(600,xfx,'=>'),
          op(650,xfx,'<=>'),
          op(350,xfx,'xor'),
          op(400,yfx,'&'),  
          op(500,yfx,'v')
        ]). 
*/
% SWI Prolog modules do not export operators by default
% so they must be explicitly placed in the user namespace

:- include(dbase_i_header).
:- ensure_loaded(dbase_i_mpred_pttp).

%  all(R, room(R) => exists(D, (door(D) & has(R,D))))
% for any arbitrary R, if R is a room then there exists some object D that is a door, and R has a D.
% door(sk6(_G180)):-room(_G180)
% has(_G180,sk6(_G180)):-room(_G180)
%  R is not a room if D is a door and R doesn't have D
% if there are no doors anywhere then there must not be rooms
% -room(R) :- -has(R,_).

:- %( current_prolog_flag(argv,[pl|_]) ->
     op( 400, fy, user:(box) ),	% Necessity, Always
     op( 400, fy, user:(dia) ),	% Possibly, Eventually
     op( 400, fy, user:(cir) ),	% Next time
     op(600,xfx,user:'<-'),
  
  
     op(400,fy,box),		% Necessity, Always
     op(400,fy,dia),		% Possibly, Eventually
     op(400,fy,cir),		% Next time

     op(300,fx,'-'),
     op(300,fx,'~'),
     op(600,xfx,'=>'),
     op(600,xfx,'<-'),
     op(650,xfx,'<=>'),
     op(350,xfx,'xor'),
     op(400,yfx,'&'),  
     op(500,yfx,'v')
     ,!.


non_compound(InOut):-once(not(compound(InOut));is_ftVar(InOut)).

is_gaf(Gaf):-not(is_snark_rule(Gaf)).

:-export(is_snark_rule/1).
is_snark_rule(Var):-is_ftVar(Var),!,fail.
% is_snark_rule(_:-_):-!.
is_snark_rule(R):-get_functor(R,F,A),functor(P,F,A),snark_hook(P),!.

snark_hook(0=>0).
snark_hook(0<=>0).
snark_hook(0 & 0).
snark_hook(0 v 0).
snark_hook(0 <- 0).
snark_hook(~(0)).
snark_hook(-(0)).
snark_hook(all(+,0)).
snark_hook(exists(+,0)).
snark_hook(C):-non_compound(C),!,fail.
snark_hook(H:-_):-!,nonvar(H),!,snark_hook(H).


:-style_check(+singleton).


:-export(term_singletons/2).
term_singletons(A,Vs):- term_singletons(A,[],_,[],Vs). 
:-export(term_singletons/5).
term_singletons(Fml, NS,NS, S,S):-atomic(Fml),!.
term_singletons(Fml, NS,NS, S,S):-identical_member(Fml,NS),!.
term_singletons(Fml, NS, [Fml|NS], S, NSV):- is_ftVar(Fml),identical_member(Fml,S),!,delete_eq(S,Fml,NSV),!.
term_singletons(Fml, NS, NS, S, [Fml|S]):- is_ftVar(Fml),!.
term_singletons([H|T],NS,NSO,S,NSV):- !, term_singletons(H,NS,NSM,S,M),term_singletons(T,NSM,NSO,M,NSV).
term_singletons(Fml, NS,NSO, S,NSV):- compound(Fml),Fml=..[_,H|T],!, term_singletons(H,NS,NSM,S,M),term_singletons(T,NSM,NSO, M,NSV).

subst_eq(A,B,C,D):-subst(A,B,C,D).

:-export(subsT_each/4).
subsT_each(_,In,[],In):-!.
subsT_each(each,In,[X=Y|TODO],Out):-!,subst_eq(In,X,Y,Mid),subsT_each(each,Mid,TODO,Out),!.
subsT_each(REV,In,[X=Y|TODO],Out):-subst_eq(In,Y,X,Mid),subsT_each(REV,Mid,TODO,Out),!.

contains_var_lits(Fml,Var,Lits):- findall(Lit,contains_t_var(Fml,Var,Lit),Lits).

get_isa(Lit,I,TT):-compound(Lit),get_isa0(Lit,I,TT).
get_isa0(isa(I,T),I,TT):-to_iname(T,TT),!.
get_isa0(IT,I,TT):-IT=..[T,I],is_colection_name(IT,T,TT),!.

is_colection_name(_,-,_):-!,fail.
is_colection_name(IT,T,TT):- atom_length(T,TL),TL>2,not(atom_contains(T,'_')),not(predicate_property(IT,_)),to_iname(T,TT).

:-export(mudEquals/2).
mudEquals(X,Y):-X=Y.

:-export(not_mudEquals/2).
not_mudEquals(X,Y):- X \= Y.


to_iname(T,TT):-not(current_predicate(i_name/3)),!,T=TT.
to_iname(T,TT):-is_ftVar(T)->TT=T;(not_log_op(T),i_name(t,T,TT)).

contains_type_lits(Fml,Var,Lits):- findall(T,(contains_t_var(Fml,Var,Lit),get_isa(Lit,O,T),same_var(O,Var)),Lits).
contains_t_var(Fml,Var,Term):-each_subterm(Fml,Term),compound(Term),arg(_,Term,O),same_var(O,Var).

:-export(type_of_var/3).
type_of_var(Fml,Var,Type):-contains_type_lits(Fml,Var,Lits),!,(member(Type,Lits)*->true;Type='Unk').

to_dlog_ops([
       % ';'='v',
       % ','='&',
       '~'='-',
     'not'='-',      
     'naf'='-',      
      ':-' = ':-',
     'and'='&',
      'or'='v',
       ';'='v',
      ':-'=':-',
      '<='=':-',
 'implies'='=>',
   'equiv'='<=>',
      '=>'='=>',
     '<=>'='<=>']).

to_symlog_ops(['v'='v',
   '&'='&',
   '=>'='=>',
   '<=>'='<=>',
   '~'='-',
   ':-'=':-']).

to_prolog_ops(['v'=';',
   '&'=',',
   '=>'='=>',
   '<=>'='<=>',
   '-'='not',
   ':-'=':-']).


to_nonvars(_Type,IN,IN):-is_ftVar(IN),!.
to_nonvars( Type,IN,OUT):-is_list(IN),!,maplist(to_nonvars(Type),IN,OUT),!.
to_nonvars( Type,IN,OUT):-call(Type,IN,OUT),!.

convertAndCall(Type,Call):- Call=..[F|IN],maplist(to_nonvars(Type),IN,OUT), IN \=@= OUT, !, must(apply(F,OUT)).

as_dlog(Fml,Fml):-is_ftVar(Fml),!.
as_dlog(Fml,FmlO):- to_dlog_ops(OPS),subsT_each(each,Fml,OPS,FmlO),!.
as_symlog(Fml,Fml):-is_ftVar(Fml),!.
as_symlog(Fml,FmlO):- as_dlog(Fml,FmlM),to_symlog_ops(OPS),subsT_each(each,FmlM,OPS,FmlO).
as_prolog(Fml,Fml):-is_ftVar(Fml),!.
as_prolog(Fml,FmlO):- as_dlog(Fml,FmlM),to_prolog_ops(OPS),subsT_each(each,FmlM,OPS,FmlO).


%=% Negation Normal Form

% Usage: nnf(+KB,+Orig,+Fml, ?NNF)


nnf(KB,Orig,Fml,NNF) :-
   as_dlog(Fml,FmlO),   
   nnf(KB,Orig,FmlO,[],NNF,_),!.

%=----- drive negation inward --------------
%  nnf(KB, Orig,+Fml,+FreeV,-NNF,-Paths)
%
% Fml,NNF:    See above.
% FreeV:      List of free variables in Fml.
% Paths:      Number of disjunctive paths in Fml.

is_b(box(b_d(B,D)),BF,F):-BF=..[B,F],b_d(B,D).
is_b(box(b_d(B,D)),box(b_d(B,D),F),F):-b_d(B,D).
is_b(box(b_d(B,D)),box(B,F),F):-b_d(B,D).
is_b(dia(b_d(B,D)),DF,F):-DF=..[D,F],b_d(B,D).
is_b(dia(b_d(B,D)),dia(b_d(B,D),F),F):-b_d(B,D).
is_b(dia(b_d(B,D)),dia(D,F),F):-b_d(B,D).
is_b(cir(ct(CT)),CF,F):-CF=..[CT,F],ct(CT).
is_b(cir(ct(CT)),cir(CT,F),F):-ct(CT).

ct(cir).
ct(asserted_t).
b_d(nesc,poss).
b_d(knows,beliefs).
b_d(always,sometimes).
b_d(box,dia).

nnf(KB, Orig,Lit,FreeV,Pos,1):-is_ftVar(Lit),!,wdmsg(warn(nnf(KB, Orig,Lit,FreeV,Pos,1))),Pos=proven_t(Lit).

nnf(KB, Orig,Fin,FreeV,BOX,Paths) :- is_b(box(BDT),Fin,F), !,
	nnf(KB, Orig,F,FreeV,NNF,Paths), cnf( Orig,NNF,CNF), boxRule( Orig,box(BDT,CNF), BOX).

nnf(KB, Orig,Fin,FreeV,DIA,Paths) :- is_b(dia(BDT),Fin,F), !,
	nnf(KB, Orig,F,FreeV,NNF,Paths), dnf( Orig,NNF,DNF), diaRule( Orig,dia(BDT,DNF), DIA).

nnf(KB, Orig,Fin,FreeV,CIR,Paths) :- is_b(cir(CT),Fin,F), !,
	nnf(KB, Orig,F,FreeV,NNF,Paths), cirRule( Orig,cir(CT,NNF), CIR).

nnf(KB, Orig,until(A,B),FreeV,NNF,Paths) :- !,
	nnf(KB, Orig,A,FreeV,NNF1,Paths1),
	nnf(KB, Orig,B,FreeV,NNF2,Paths2),
	Paths is Paths1 + Paths2,
	NNF = until(NNF1, NNF2).

% ----- quantifiers -----------------------
nnf(KB, Orig,all(X,NNF),FreeV,NNF2,Paths) :- \+ member(X,FreeV),!,  %= MUST?! use separate variables
      nnf(KB, Orig,all(X,NNF),[X|FreeV],NNF2,Paths).

%nnf(KB, Orig,exists (X,NNF),FreeV,NNF2,Paths) :- \+ member(X,FreeV),!,  %= MUST?! use separate variables
%        nnf(KB, Orig,exists(X,NNF),[X|FreeV],NNF2,Paths).

nnf(KB, Orig,all(X,F),FreeV,all(X,NNF),Paths) :- !,
	must((nnf(KB, Orig,F,[X|FreeV],NNF,Paths))).

nnf(KB, Orig,exists(X,Fml),FreeV,NNF,Paths) :- !,
	must(skolem(KB,Orig,Fml,X,FreeV,FmlSk)),
	must(nnf(KB, Orig,FmlSk,FreeV,NNF,Paths)).

nnf(KB, Orig,atleast(1,X,Fml),FreeV,NNF,Paths) :- !,
	nnf(KB, Orig,exists(X,Fml),FreeV,NNF,Paths).
nnf(KB, Orig,atleast(N,X,Fml),FreeV,NNF,Paths) :-
	!,
	NewN is N - 1,
        subst_eq(Fml,X,Y,FmlY),
	nnf(KB, Orig,'&'(exists(X,Fml),atleast(NewN,Y,FmlY)),FreeV,NNF,Paths).
nnf(KB, Orig,atmost(1,X,Fml),FreeV,NNF,Paths) :- 
	!,
        subst_eq(Fml,X,Y,FmlY),
        subst_eq(Fml,X,Z,FmlZ),
	nnf(KB, Orig,-('&'(exists(Y,FmlY),exists(Z,FmlZ))),FreeV,NNF,Paths).
nnf(KB, Orig,atmost(N,X,Fml),FreeV,NNF,Paths) :-
	!,
        subst_eq(Fml,X,Y,FmlY),
	NewN is N - 1,
	nnf(KB, Orig,'&'(exists(Y,FmlY),atmost(NewN,X,Fml)),FreeV,NNF,Paths).

nnf(KB, Orig,-(xor(X , Y)),FreeV,NNF,Paths) :-
   !,
   nnf(KB, Orig,v('&'(X , Y) , '&'(-(X) , -(Y))),FreeV,NNF,Paths).
   
nnf(KB, Orig,xor(X , Y),FreeV,NNF,Paths) :-
   !,
   nnf(KB, Orig,'&'(v(X , Y) , v(-(X) , -(Y))),FreeV,NNF,Paths).
   

nnf(KB, Orig,'&'(A,B),FreeV,NNF,Paths) :- !,
	nnf(KB, Orig,A,FreeV,NNF1,Paths1),
	nnf(KB, Orig,B,FreeV,NNF2,Paths2),
	Paths is Paths1 * Paths2,
	( Paths1 > Paths2 -> NNF = '&'(NNF2,NNF1);
		            NNF = '&'(NNF1,NNF2)).

nnf(KB, Orig,v(A,B),FreeV,NNF,Paths) :- !,
        nnf(KB, Orig,A,FreeV,NNF1,Paths1),
	nnf(KB, Orig,B,FreeV,NNF2,Paths2),
	Paths is Paths1 + Paths2,
	( Paths1 > Paths2 -> NNF = v(NNF2,NNF1);
		            NNF = v(NNF1,NNF2)).

nnf(KB, Orig,Fml,FreeV,NNF,Paths) :- 
	(Fml = -(-(A))   -> Fml1 = A;
	 Fml = -(box(BDT,F))      -> Fml1 = dia(BDT,-(F));
	 Fml = -(dia(BDT,F))      -> Fml1 = box(BDT,-(F));
	 Fml = -(cir(CT,F))      -> Fml1 = cir(CT,-(F));
	 Fml = -(until(A,B)) -> (nnf(KB, Orig,-(A),FreeV,NNA,_), nnf(KB, Orig,-(B),FreeV,NNB,_),
                                     Fml1 = v( all(NNB), until(NNB,'&'(NNA,NNB))));
	 Fml = -(all(X,F))   -> Fml1 = exists(X,-(F));
	 Fml = -(exists(X,F))    -> Fml1 = all(X,-(F));

	 Fml = -(atleast(N,X,F)) -> Fml1 = atmost(N,X,F);
	 Fml = -(atmost(N,X,F)) -> Fml1 = atleast(N,X,F);

	 Fml = -(v(A,B))  -> Fml1 = '&'( -(A), -(B) );
	 Fml = -('&'(A,B)) -> Fml1 = v( -(A), -(B) );
	 Fml = '=>'(A,B)        -> Fml1 = v( -(A), B );
	 Fml = -('=>'(A,B)) -> Fml1 = '&'( A, -(B) );
         Fml = '<=>'(A,B)        -> Fml1 = v( '=>'(A, B), '=>'(B, A) );
	 Fml = '<=>'(A,B)        -> Fml1 = v( '&'(A, B), '&'(-(A), -(B)) );
	 Fml = -('<=>'(A,B)) -> Fml1 = v( '&'(A, -(B)) , '&'(-(A), B) )
	),!,
	nnf(KB, Orig,Fml1,FreeV,NNF,Paths).

/*
nnf(KB, _Orig,Fml,_,Fml,1):- Fml=..[F,KB,_],third_order(F),!.
nnf(KB,  Orig,Fml,FreeV,Out,Path):- Fml=..[F,A],third_order(F),  
  nnf(KB, Orig,A,FreeV,NNF1,Path1),!,
  Fml2=..[F,KB,NNF1],nnf(KB, Orig,Fml2,FreeV,Out,Path2),Path is Path1+Path2.
*/
nnf( KB,_Orig,Fml,_,FmlO,1):- nonegate(KB,Fml,FmlO),!.



third_order(asserted_t).


boxRule( Orig,A,B):-convertAndCall(as_dlog,boxRule( Orig,A,B)).
boxRule( Orig,box(BDT,'&'(A,B)), '&'(BA,BB)) :- !, boxRule( Orig,box(BDT,A),BA), boxRule( Orig,box(BDT,B),BB).
boxRule(_Orig,BOX, BOX).
 
diaRule( Orig,A,B):-convertAndCall(as_dlog,diaRule( Orig,A,B)).
diaRule( Orig,dia(BDT,v(A,B)), v(DA,DB)) :- !, diaRule( Orig,dia(BDT,A),DA), diaRule( Orig,dia(BDT,B),DB).
diaRule(_Orig,DIA, DIA).

cirRule( Orig,A,B):-convertAndCall(as_dlog,cirRule( Orig,A,B)).
cirRule( Orig,cir(CT,v(A,B)), v(DA,DB)) :- !, cirRule( Orig,cir(CT,A),DA), cirRule( Orig,cir(CT,B),DB).
cirRule( Orig,cir(CT,'&'(A,B)), '&'(DA,DB)) :- !, cirRule( Orig,cir(CT,A),DA), cirRule( Orig,cir B,DB).
cirRule(_Orig,CIR, CIR).


%=%
%=%  Conjunctive Normal Form (CNF) -- assumes Fml in NNF
%=%

% Usage: cnf( Orig, +NNF, ?CNF )
cnf( A,B):-copy_term(A,Orig),cnf( Orig,A,B).
cnf( Orig,A,B):-convertAndCall(as_dlog,cnf( Orig,A,B)).
cnf( Orig,'&'( P,Q), '&'( P1,Q1)):- !, cnf( Orig,P, P1), cnf( Orig,Q, Q1).
cnf( Orig,v( P,Q),     CNF):- !, cnf( Orig,P, P1), cnf( Orig,Q, Q1), cnf1( Orig, v( P1,Q1), CNF ).
cnf(_Orig,CNF,       CNF).

cnf1( Orig, v('&'( P,Q), R), '&'( P1,Q1) ):- !, cnf1( Orig, v( P,R), P1), cnf1( Orig, v(Q,R), Q1).
cnf1( Orig, v( P, '&'(Q,R)), '&'( P1,Q1) ):- !, cnf1( Orig, v( P,Q), P1), cnf1( Orig, v( P,R), Q1).
cnf1(_Orig, CNF,                 CNF).


%=%
%=% Disjunctive Normal Form (DNF) -- assumes Fml in NNF
%=%
% Usage: dnf( Orig, +NNF, ?DNF )
dnf( A,B):-copy_term(A,Orig),dnf( Orig,A,B).
dnf( Orig,A,B):-convertAndCall(as_dlog,dnf( Orig,A,B)).
dnf( Orig, v( P,Q),  v( P1,Q1) ) :- !, dnf( Orig,P, P1), dnf( Orig,Q, Q1).
dnf( Orig, '&'( P,Q), DNF) :- !, dnf( Orig,P, P1), dnf( Orig,Q, Q1), dnf1( Orig,'&'( P1,Q1), DNF).
dnf(_Orig,DNF,       DNF).

dnf1( Orig,'&'( P, v(Q,R)),  v( P1,Q1) ):- !, dnf1( Orig,'&'( P,Q), P1), dnf1( Orig,'&'( P,R), Q1).
dnf1( Orig,'&'( v( P,Q), R), v( P1,Q1) ):- !, dnf1( Orig,'&'( P,R), P1), dnf1( Orig,'&'(Q,R), Q1).
dnf1(_Orig,DNF,                  DNF ).



%=
%=  Prenex Normal Form ( PNF)
%=

% Usage: pnf(+Orig, +Fml, ?PNF ) -- assumes Fml in NNF

pnf( A,B):-copy_term(A,Orig),pnf( Orig,A,B).

pnf(Orig, F,PNF) :- pnf(Orig,F,[],PNF).

% pnf(+Orig, +Fml, +Vars, ?PNF)

pnf(A,B,C,D):-convertAndCall(as_dlog,pnf(A,B,C,D)),!.

pnf( Orig,   all(X,F),Vs,   all(X,PNF)) :- !, pnf( Orig,F,[X|Vs], PNF).
pnf( Orig,  exists(X,F),Vs,exists(X,PNF)) :- !, pnf( Orig,F,[X|Vs], PNF).

pnf( Orig,  '&'(exists(X,A) , B),Vs,  exists(Y,PNF)) :- !, copy_term((X,A,Vs),(Y,Ay,Vs)),
                                        pnf( Orig,'&'(Ay,B),[Y|Vs], PNF).
pnf( Orig,  v(exists(X,A), B),Vs,  exists(Y,PNF)) :- !, copy_term((X,A,Vs),(Y,Ay,Vs)),
                                        pnf( Orig,v(Ay,B),[Y|Vs], PNF).
pnf( Orig, '&'(all(X,A), B),Vs, all(Y,PNF)) :- !, copy_term((X,A,Vs),(Y,Ay,Vs)),
                                        pnf( Orig,'&'(Ay , B),[Y|Vs], PNF).
pnf( Orig, v(all(X,A), B),Vs, all(Y,PNF)) :- !, copy_term((X,A,Vs),(Y,Ay,Vs)),
                                        pnf( Orig,v(Ay,B),[Y|Vs], PNF).

pnf( Orig, '&'(A,exists(X,B)),Vs,  exists(Y,PNF)) :- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnf( Orig,'&'(A, By),[Y|Vs], PNF).
pnf( Orig, v(A,exists(X,B)),Vs,  exists(Y,PNF)) :- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnf( Orig,v(A,By),[Y|Vs], PNF).
pnf( Orig, '&'(A,all(X,B)),Vs, all(Y,PNF)) :- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnf( Orig,'&'(A,By),[Y|Vs], PNF).
pnf( Orig, v(A,all(X,B)),Vs, all(Y,PNF)) :- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnf( Orig,v(A,By),[Y|Vs], PNF).

pnf( Orig, '&'(A, B),Vs,       PNF ) :- pnf( Orig,A,Vs,Ap), pnf( Orig,B,Vs,Bp), 
                                     (A\=Ap; B\=Bp), pnf( Orig,'&'(Ap,Bp),Vs,PNF).
pnf( Orig, v(A, B),Vs,       PNF ) :- pnf( Orig,A,Vs,Ap), pnf( Orig,B,Vs,Bp), 
                                     (A\=Ap; B\=Bp), pnf( Orig,v(Ap,Bp),Vs,PNF).

pnf(_Orig,          PNF, _,       PNF ).

%=%  Clausal Form (CF) -- assumes Fml in PNF and
%                                 each quantified variable is unique

% cf(KB, Orig,+Fml, ?Cs)
% Cs is a list of the form: [cl(Head,Body), ...]
% Head and Body are lists.

cf(KB,A,B,C):-convertAndCall(as_dlog,cf(KB,A,B,C)).
cf(KB, Orig,PNF, SO):- removeQ(KB, Orig,PNF,[], UnQ), cnf( Orig,UnQ,CNF), 
  once((dnf( Orig,UnQ,DNF), wdmsgl(dnf2(DNF)))),
  flatten_or_list(Orig,DNF,Flat),
  wdmsgl(flat(xor(Flat))),
  make_clause_Vs_each(KB,Flat,EachClause),
  clausify(KB,CNF,Cla,[]),!,append(Cla,EachClause,O),sort(O,SO).

% removes quantifiers
removeQ(KB,A,B,C,D):-convertAndCall(as_dlog,removeQ(KB,A,B,C,D)).
removeQ(KB, Orig, all(X,F),Vars, RQ) :- removeQ(KB, Orig,F,[X|Vars], RQ).
removeQ(KB, Orig,  exists(X,F),Vars, RQ) :-
	skolem(KB, Orig,F,X,Vars,Fsk),
	removeQ(KB, Orig,Fsk,Vars, RQ).
removeQ(_KB,_Orig, F,_,F ).

clausify(KB, '&'( P,Q), C1, C2 ) :-
	!,
	clausify(KB, P, C1, C3 ),
	clausify(KB, Q, C3, C2 ).
clausify(KB, P, [cl(A,B)|Cs], Cs ) :-
	inclause(KB, P, A, [], B, [] ),
	!.
clausify(_KB, _, C, C ).

inclause(KB, v( P,Q), A, A1, B, B1 ) :-
	!,
	inclause(KB, P, A2, A1, B2, B1 ),
	inclause(KB, Q, A,  A2, B,  B2 ).
inclause(KB, - PP , A,  A, B1, B ) :- 
        negate(KB, - PP,P),
	!,
	notin( P, A ),
	putin( P, B, B1 ).
inclause(_KB, P,  A1, A, B,  B ) :-
	!,
	notin( P, B ),
	putin( P, A, A1 ).

notin(X,[Y|_]) :- X==Y, !, fail.
notin(X,[_|Y]) :- !,notin(X,Y).
notin(_,[]).

putin(X,[],   [X]   ) :- !.
putin(X,[Y|L],[Y|L] ) :- X == Y,!.
putin(X,[Y|L],[Y|L1]) :- putin(X,L,L1).

make_clause_Vs_each(KB,List,Out):-must((findall(E,make_each(KB,List,E),Out),Out=[_|_])).
make_each(KB,List,E):- member(One,List),  make_1_cl(KB,One,List,E).

make_1_cl(KB,One,List,cl([One],NewBodyList)):-is_neg(One),!,delete_eq(List,One,Rest),maplist(negate(KB),Rest,NewBodyList).
make_1_cl(KB,One,List,cl([One],NewBodyList)):-is_pos(One),!,delete_eq(List,One,Rest),maplist(negate(KB),Rest,NewBodyList).

is_neg(-(_)).
is_pos(One):-get_functor(One,F),!,not(is_log_op(F)).

not_log_op(OP):-not(is_log_op(OP)).
is_log_op(OP):-atomic(OP),to_dlog_ops(OPS),!,(member(OP=_,OPS);member(_=OP,OPS)).


:-export(logical_pos/3).
:-export(logical_neg/3).
logical_neg(KB,Wff,WffO):-
  must(nonegate(KB,Wff,Wff1)),nnf(KB,-Wff1,-Wff1,Wff2),must(nonegate(KB,Wff2,WffO)),!.
logical_pos(KB,Wff,WffO):-
  must(nonegate(KB,Wff,Wff1)),nnf(KB,Wff1,Wff1,Wff2),must(nonegate(KB,Wff2,WffO)),!.



negate(A,B,C):-defunctionalize(A,AA),must(negate0(AA,B,C)).
negate0(_KB,-(ISA),isa(I,C)):- get_isa(ISA,I,C),!.
negate0(_KB,ISA,-(isa(I,C))):- get_isa(ISA,I,C),!.
negate0(_KB,-(X),(X)):-!.
negate0(_KB,(X),-(X)):-!.


dbase_quf(In,Out):-transitive(dbase_quf_0,In,Out).

dbase_quf_0(InOut,InOut):-non_compound(InOut),!.
dbase_quf_0(In,Out):- current_predicate(db_quf/4),db_quf(change(assert,_Must),In,U,C),conjoin(U,C,Out).

:-export(nonegate/3).
nonegate( KB,Fml,OutZ):- unbuiltin_negate(KB,Fml,Out),defunctionalize(Out,OutY),dbase_quf(OutY,OutZ).

unbuiltin_negate(_, Fml,Out):-get_functor(Fml,F,A),builtin(F,A),!,must(Out=Fml).
unbuiltin_negate(KB,Fml,Out):-once(negate(KB,Fml,Neg)),negate(KB,Neg,Out),!.

%=%  Skolemizing -- method 1

% Usage: skolemize(+Fml,+X,+FreeV,?FmlSk)
% Replaces existentially quantified variable with the formula
% VARIABLES MUST BE PROLOG VARIABLES
% exists(X,p(X)) --> p(p(exists))

skolem_bad(Fml,X,FreeV,FmlSk):-
	copy_term((X,Fml,FreeV),(Fml,Fml1,FreeV)),
	copy_term((X,Fml1,FreeV),(exists,FmlSk,FreeV)).

%=%  Skolemizing -- method 2

% Usage: skolem(KB, Orig, +Fml, +X, +FreeV, ?FmlSk )
% Replaces existentially quantified variable with a unique function
% fN(Vars) N=1,...
% VARIABLES MAYBE EITHER PROLOG VARIABLES OR TERMS

skolem(KB, Orig, F, X, FreeV, Out) :-  
   must(skolem_f(KB, Orig, F, X, FreeV, Sk)),
   must(Out=('=>'(mudEquals(X,Sk),F))),!.

skolem(KB, Orig, F, X, FreeV, FmlSk) :-
    must( skolem_f(KB, Orig, F, X, FreeV, Sk)), 
    must(subst_eq( F, X, Sk, FmlSk)),!.


skolem_f(KB, Orig, F, X, FreeVIn, Sk) :- 
       must_det_l([ 
        delete_eq(FreeVIn,KB,FreeV),
        list_to_set(FreeV,FreeVSet),
	contains_var_lits(F,X,LitsList),
        mk_skolem_name( Orig,X,LitsList,'',SK),
        concat_atom(['sk',SK,'Fn'],Fun),
	Sk =..[Fun,KB|FreeVSet]]).
/*


%=% Substitution

% Usage: subst_eq(+Fml,+X,+Sk,?FmlSk)
subst_eq(Fml,X,Sk,FmlSkO):-pred_subst(==,Fml,X,Sk,FmlSk),!,must(FmlSkO=FmlSk).


% Usage: pred_subst(+Pred,+Fml,+X,+Sk,?FmlSk)
pred_subst( Pred,   all(Y,P), X,Sk,   all(Y,P1) ) :- !, pred_subst( Pred, P,X,Sk,P1 ).
pred_subst( Pred,exists(Y,P), X,Sk,exists(Y,P1) ) :- !, pred_subst( Pred, P,X,Sk,P1 ).
pred_subst( Pred, '&'( P,Q), X,Sk,'&'( P1,Q1) ) :- !, pred_subst( Pred, P,X,Sk,P1 ), pred_subst( Pred, Q,X,Sk,Q1 ).
pred_subst( Pred,  v( P,Q), X,Sk, v( P1,Q1) ) :- !, pred_subst( Pred, P,X,Sk,P1 ), pred_subst( Pred, Q,X,Sk,Q1 ).

pred_subst( Pred,       P,    X,Sk,       P1    ) :- call(Pred,P,X), Sk=P1,!.
pred_subst(_Pred,       P,    _,_,       P1    ) :- is_ftVar(P), P1=P,!.
pred_subst( Pred,       P,    X,Sk,       P1    ) :- compound( P),
                             P =..Args, 
                               pred_subst2( Pred, X, Sk, Args, ArgS ),!,
                             P1 =..ArgS.
pred_subst(_  ,        P,    _, _,       P     ).

pred_subst2(_   , _,  _, [], [] ).
pred_subst2( Pred, X, Sk, [A|As], [Sk|AS] ) :- call( Pred, X, A), !, pred_subst2( Pred, X, Sk, As, AS).
pred_subst2( Pred, X, Sk, [A|As], [A|AS]  ) :- is_ftVar(A), !, pred_subst2( Pred, X, Sk, As, AS).
pred_subst2( Pred, X, Sk, [A|As], [Ap|AS] ) :- pred_subst( Pred, A,X,Sk,Ap ), pred_subst2( Pred, X, Sk, As, AS).
*/


%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%
%=% 
%=%   snark_in_prolog.P
%=%      SWI-Prolog version
%=%   Convert wffs to list of normal logic clauses
%=%
%=%   and       &  
%=%   or        v
%=%   not       -
%=%   xor       xor
%=%   implies   =>   
%=%   iff       <=>  
%=%   all       all(X,-)
%=%   some      exists(Y,-)
%=%
%=%    all(X,p(X) => exists(Y, r(Y) & q(X,Y))) 
%=%  --------------------------------------------
%=%    p(X) => r(sk1(X)) & q(X,sk1(X))
%=%  --------------------------------------------
%=%    r(sk1(X)) :- p(X).
%=%    q(X,sk1(X)) :- p(X).


:- op(300,fx,'~').
:- op(300,fx,'-').
:- op(400,yfx,'&').  
:- op(500,yfx,'v').
:- op(600,xfx,'=>').
:- op(650,xfx,'<=>').
:- op(350,xfx,'xor').

:- op(300,fx,user:'~').
:- op(300,fx,user:'-').
:- op(400,yfx,user:'&').  
:- op(500,yfx,user:'v').
:- op(600,xfx,user:'=>').
:- op(650,xfx,user:'<=>').
:- op(350,xfx,user:'xor').



%=%=%=%=%=%=%=%=%=%=%=
%=% generate a skolem 

mk_skolem_name(_O,Var,Fml,SIn,SOut):-is_ftVar(Fml), same_var(Var,Fml),!,atom_concat('Is',SIn,SOut).
mk_skolem_name(_O,_V,Fml,SIn,SIn):-is_ftVar(Fml),!.
mk_skolem_name(_O ,_V,[],SIn,SIn):-!.
mk_skolem_name(_O,_V, OP,SIn,SIn):- is_log_op(OP),!.
mk_skolem_name(_O,_V,Fml,SIn,SOut):- atomic(Fml),!,i_name(Fml,N),toPropercase(N,CU),!,(atom_contains(SIn,CU)->SOut=SIn;atom_concat(SIn,CU,SOut)).
mk_skolem_name( Orig,Var,[H|T],SIn,SOut):- !,mk_skolem_name( Orig,Var,H,SIn,M),mk_skolem_name( Orig,Var,T,M,SOut).
mk_skolem_name( Orig,Var,isa(VX,Lit),SIn,SOut):- same_var(same_var(Var,VX)),not_ftVar(Lit),!,mk_skolem_name( Orig,Var,['Is',Lit,'In'],'',F),atom_concat(F,SIn,SOut).
mk_skolem_name( Orig,Var,Fml,SIn,SOut):- Fml=..[F,VX],same_var(Var,VX),!,mk_skolem_name( Orig,Var,['Is',F,'In'],SIn,SOut).
mk_skolem_name( Orig,Var,Fml,SIn,SOut):- Fml=..[F,Other,VX|_],same_var(Var,VX),!,type_of_var( Orig,Other,OtherType),
   mk_skolem_name( Orig,Var,[OtherType,'Arg2Of',F],SIn,SOut).
mk_skolem_name( Orig,Var,Fml,SIn,SOut):- Fml=..[F,VX|_],same_var(Var,VX),!,mk_skolem_name( Orig,Var,['Arg1Of',F],SIn,SOut).
mk_skolem_name( Orig,Var,Fml,SIn,SOut):- Fml=..[F|_],!,mk_skolem_name( Orig,Var,['ArgNOf',F],SIn,SOut).

% same_var(Var,Fml):-not(not(Var=Fml)),!.
same_var(Var,Fml):-Var==Fml,!.


%=-----  make a sequence out of a disjunction -----
flatten_or_list(A,B,C):- convertAndCall(as_symlog,flatten_or_list(A,B,C)).
flatten_or_list(Orig,v(X , Y), F) :- !,
   flatten_or_list( Orig,X,A),
   flatten_or_list( Orig,Y,B),
   flatten([A,B],F).
flatten_or_list(_Orig,X,[X]).



fmtl(X):- as_prolog(X,XX), fmt(XX).

write_list([F|R]) :- write(F), write('.'), nl, write_list(R).
write_list([]).

numbervars_with_names(Term):-
   term_variables(Term,Vars),name_variables(Vars),!,numbervars(Vars,91,_,[attvar(skip),singletons(true)]),!.

name_variables([]).
name_variables([Var|Vars]):-
   (var_property(Var, name(Name)) -> Var = '$VAR'(Name) ; true),
   name_variables(Vars).

wdmsgl(CNF):- compound(CNF),CNF=..[NAME,NF],!,must(wdmsgl_2(NAME,NF)).
wdmsgl(NF):- must((get_functor(NF,NAME),!,must(wdmsgl_2(NAME,NF)))).


wdmsgl_2(NAME,NF):-functor(NF,_,_),wdmsgl_3(NAME,'&',NF).

wdmsgl_3(NAME,F,NF):-copy_term(vv(NAME,F,NF),vv(NAME2,F2,NF2)),
   numbervars_with_names(vv(NAME2,F2,NF2)),!,
   wdmsgl_4(NAME2,F2,NF2).

wdmsgl_4(NAME,F,NF):- is_list(NF),!,maplist(wdmsgl_4(NAME,F),NF).
wdmsgl_4(NAME,F,NF):- compound(NF),NF=..[FF,A,B],FF=F,not_ftVar(A),not_ftVar(B),!,
  maplist(wdmsgl_4(NAME,F),[A,B]).
wdmsgl_4(NAME,_,NF):- as_symlog(NF,NF2), with_all_dmsg(wdmsg(NAME=NF2)).



put_singles(Wff,_,[],Wff).
put_singles(Wff,Exists,[S|Singles],NewWff):-   
   (((each_subterm(Wff,SubTerm),compound(SubTerm),
    SubTerm=..[OtherExists,SO,_],same_var(SO,S),
     member(OtherExists,[all,exists])))
     -> WffM = Wff ; WffM =..[Exists,S,Wff]),
   put_singles(WffM,Exists,Singles,NewWff),!.


ensure_quantifiers(Wff:-B,WffO):- B== true,!, ensure_quantifiers(Wff,WffO).
ensure_quantifiers(Wff:-B,Wff:-B):- !.
ensure_quantifiers(Wff,WffO):- 
 must_det_l((show_call(term_singletons(Wff,[],NS,[],Singles)),
  put_singles(Wff,'exists',Singles,WffM),put_singles(WffM,'all',NS,WffO))).

:-multifile(function_corisponding_predicate/2).
:-dynamic(function_corisponding_predicate/2).

get_pred( Pred,F):-get_functor( Pred,F).
is_function(Function):-compound(Function),get_functor(Function,F,A),is_function(Function,F,A).
is_function(_,F,_):- atom_concat('sk',_Was,F),!,fail.
is_function(_,F,_):- atom_concat(_Was,'Fn',F).
is_function(_,F,A):- A2 is A+1,current_predicate(F/A2), not(current_predicate(F/A)).

is_ftEquality(Term):-get_pred(Term,Pred),(mpred_prop( Pred,prologEquality);Pred==mudEquals).

function_to_predicate(Function,NewVar,Pred):-
  Function=..[F|ARGS],
  function_corisponding_predicate(F,P),
  Pred=..[P,NewVar|ARGS].
function_to_predicate(Function,NewVar,mudEquals(NewVar,Function)).

:-export(defunctionalize/2).
defunctionalize(Wff,WffO):- defunctionalize(',',Wff,WffO).
defunctionalize(_ ,Wff,Wff):- non_compound(Wff),!.
defunctionalize(OP,Wff,WffO):- compound(Wff),
  each_subterm(Wff,SubTerm),
  compound(SubTerm),
  not(is_ftEquality(SubTerm)),
  arg(_,SubTerm,Function),is_function(Function),
  subst_eq(SubTerm,Function,NewVar,NewSubTerm),
  must(function_to_predicate(Function,NewVar,Pred)),
  NEW =..[OP,Pred,NewSubTerm],
  subst_eq(Wff,SubTerm,NEW,NextWff),!,
  defunctionalize(OP,NextWff,WffO).
defunctionalize(_,Wff,Wff).

conjuncts_to_list(Var,[Var]):-is_ftVar(Var),!.
conjuncts_to_list(true,[]).
conjuncts_to_list((A,B),ABL):-!,
  conjuncts_to_list(A,AL),
  conjuncts_to_list(B,BL),
  append(AL,BL,ABL).
conjuncts_to_list(Lit,[Lit]).

% kif_to_boxlog('=>'(WffIn,enables(Rule)),'$VAR'('MT2'),complete,Out1), % kif_to_boxlog('=>'(enabled(Rule),WffIn),'$VAR'('KB'),complete,Out).  
%=----- kif_to_boxlog(+Wff,-NormalClauses) ------

%:-export(kif_to_boxlog/2).
% kif_to_boxlog(Wff,Out) :- why_to_id(rule,Wff,Why), kif_to_boxlog(Wff,Why,Out),!.
% kif_to_boxlog(WffIn,Out) :- kif_to_boxlog(all('$VAR'('KB'),'=>'(asserted_t('$VAR'('KB'),WffIn),WffIn)),'$VAR'('KB'),complete,Out).
% kif_to_boxlog(WffIn,NormalClauses):- kif_to_boxlog(WffIn,'$VAR'('KB'),WffIn,NormalClauses).

:-export(kif_to_boxlog/3).
kif_to_boxlog(WffIn,Why,Out) :-  kif_to_boxlog(WffIn,'$VAR'('KB'),Why,Out),!.

kif_to_boxlog(Wff:-B,KB,Why,Flattened) :- is_true(B),!, kif_to_boxlog(Wff,KB,Why,Flattened).
kif_to_boxlog((HEADIn:-BODYIn),KB,Why,Flattened) :-  
  ignore('$VAR'('KB')=KB),
  must_det_l([
    as_dlog((HEADIn:-BODYIn),(HEAD:-BODY)),
   conjuncts_to_list(HEAD,HEADL),conjuncts_to_list(BODY,BODYL),
   NCFs = [cl(HEADL,BODYL)],
   maplist(clauses_to_boxlog(KB,Why),NCFs,ListOfLists),
   flatten([ListOfLists],Flattened),
   wdmsgl(prolog(Flattened))]),!.

kif_to_boxlog(WffInIn,KB,Why,Flattened) :-    
  ignore('$VAR'('KB')=KB),
  must_det_l([   
  as_dlog(WffInIn,WffIn),
   must(numbervars_with_names(WffIn:KB:Why)),   
   ensure_quantifiers(WffIn,WffQ),
   Orig = WffQ,
   defunctionalize('=>',WffQ,Wff),
   wdmsgl(kif(Wff)),
   nnf(KB,Orig,Wff,NNF),
   wdmsgl(nnf(NNF)),
   pnf(NNF,PNF),
   wdmsgl(pnf(PNF)),
   dnf(PNF,DNF),
   wdmsgl(dnf(DNF)),
   cf(KB,Orig,PNF,NCFs),
   wdmsgl(cf(NCFs)),   
   maplist(clauses_to_boxlog(KB,Why),NCFs,ListOfLists),
   flatten([ListOfLists],Flattened),
   wdmsgl(prolog(Flattened))]),!.

list_to_conjuncts([],true).
list_to_conjuncts([H],HH):-list_to_conjuncts(H,HH).
list_to_conjuncts([H|T],Body):-!,
    list_to_conjuncts(H,HH),
    list_to_conjuncts(T,TT),
    conjoin(HH,TT,Body).
list_to_conjuncts(H,H).


evidence(_KB,_,A,A):-!.
evidence(KB,nesc(_,_),mudEquals(X,Y),HeadOut):- !,subevidence(KB,possible_t,mudEquals(X,Y),HeadOut).
evidence(KB,nesc(_,_),asserted_t(X),HeadOut):- !,subevidence(KB,asserted_t,X,HeadOut).
evidence(KB,How,HeadIn,HeadOut):- subevidence(KB,How,HeadIn,HeadOut).

% oper_term(proven_t,T,T):-!.
oper_term(How,HeadIn,HeadOut):-append_term(How,HeadIn,HeadOut),!.

subevidence(_KB,nesc(How,Else),HeadIn,HeadOut):- (HeadIn= -(Was) -> oper_term(Else,Was,HeadOut);oper_term(How,HeadIn,HeadOut)),!.
subevidence(_KB,asserted_t,HeadIn,HeadOut):- HeadIn= -(Was)-> HeadOut=asserted_false([Was]);HeadOut=asserted_t([HeadIn]).
subevidence(_KB,possible_t,HeadIn,HeadOut):- HeadIn= -(Was)-> HeadOut=missing_t(Was);HeadOut=possible_t(HeadIn).
subevidence(_KB,believe,HeadIn,HeadOut):- HeadIn= -(Was)-> HeadOut=missing_t(Was);HeadOut=believe(HeadIn).
subevidence(_NT,E2,Was,HeadOut):-append_term(E2,Was,HeadOut),!. 

prepend_term(NewHead,Arg1,NewHeadM):-NewHead=..[F|ARGS],NewHeadM=..[F,Arg1|ARGS].



clauses_to_boxlog(_,_Why,cl([HeadIn],BodyIn),(HeadIn:-BodyOut)):- maplist(logical_pos(_KB),BodyIn,Body), list_to_conjuncts(Body,BodyOut),!.
clauses_to_boxlog(_,_Why,cl([HeadIn],[]),HeadIn):-!.
clauses_to_boxlog(KB,_Why,cl([HeadIn],BodyIn),OUT):-
  must_det_l([ 
   evidence(KB,nesc(proven_in(KB),impossible_in(KB)),HeadIn,Head),
   maplist(evidence(KB,nesc(proven_t,impossible_t)),BodyIn,Body),
   subst_eq(Head,-,impossible_t,NewHead),
   subst_eq(Body,-,impossible_t,NewBody),
   OfBody =.. ['when_in',KB|NewBody]]),
   must(OUT=[(NewHead:-OfBody)]).

clauses_to_boxlog(KB,Why,cl([],BodyIn),ListOf):-!,clauses_to_boxlog(KB,Why,cl([inconsistentKB(KB)],BodyIn),ListOf).
clauses_to_boxlog(KB,Why,cl([H,Head|List],BodyIn),ListOf):- 
  findall(Answer,((member(E,[H,Head|List]),delete_eq([H,Head|List],E,RestHead),
    maplist(logical_neg(KB),RestHead,RestHeadS),append(RestHeadS,BodyIn,Body),
    clauses_to_boxlog(KB,Why,cl([E],Body),Answer))),ListOf),!.


dbase_t_tell_snark(OP2,RULE):- 
 with_assertions(thlocal:current_pttp_db_oper(mud_call_store_op(OP2)),
   (show_call(call((must(snark_tell(RULE))))))).


%:-export(kif_to_boxlog/1).
%kif_to_boxlog(A):- as_dlog(A,AA),kif_to_boxlog(AA,B),!,maplist(snark_tell_boxes,B),!,nl,nl.
%:-export(kif_to_boxlog/1).
%kif_to_boxlog(A):- with_all_dmsg(( kif_to_boxlog(A,B),!,maplist(snark_tell_boxes,B),!,nl,nl)).

snark_test_string(
"
% )
tell.

all(R,room(R) => exists(D, (door(D) & has(R,D)))).
room(room1).

ask.

room(What).

door(What).

:-snark_tell(a(XX) & b(XX) => c(XX)).
:-snark_tell(all(R,room(R) => exists(D, (door(D) & has(R,D))))).
:-snark_tell(loves(Child,motherFn(Child))).
:-snark_tell((p => q)).
:-snark_tell(-p <=> -q).
:-snark_tell(p <=> q).
:-snark_tell(all( P, person( P) => -exists(D, dollar(D) & has( P,D)))).

:-snark_tell(go(sam) & ( go(bill) v go(sally) ) & go(nancy)).

:-snark_tell(rains_tuesday => wear_rain_gear xor carry_umbrella).
:-snark_tell(exists( P, (person( P) & all(C, car(C) => -has( P,C))))).

:-snark_tell(room(R) => exists(D, (door(D) & has(R,D)))).
:-snark_tell((goes(jane) xor goes(sandra) => goes(bill))).
:-snark_tell(exists( P, exists(C, (person( P) & car(C) & has( P,C))))).
:-snark_tell(-all( P,person( P) => exists(C, car(C) & has( P,C)))).
:-snark_tell((go(sam) & go(bill)) v (go(sally) & go(nancy))).
:-snark_tell(go(sam)  & ( go(bill) v go(sally) ) & go(nancy)).
:-snark_tell(exists(C, course(C) & exists(MT1, midterm(C,MT1) & exists(MT2, midterm(C,MT2) & different(MT1,MT2))))).
:-snark_tell(exists(C, course(C) & -exists(MT3, midterm(C,MT3)))).

"
).



% :-export(tsn/0).
tsn:-with_all_dmsg(forall(clause(tsnark,C),must(C))).

% tsnark:- make.
tsnark:- snark_test_string(TODO),snarky(string(TODO),current_output).

:- multifile(user:mud_regression_test/0).
user:mud_regression_test :- tsn.

:-thread_local(snark_action_mode/1).
:-asserta_if_new(snark_action_mode(tell)).

:-thread_local(snark_reader_mode/1).
:-asserta_if_new(snark_reader_mode(prolog)).


snark_read(In,Wff,Vs):-
  snark_reader_mode(lisp)->
    lisp_read(In,Wff);
      catch(read_term(In,Wff,[module(dbase_i_snark),double_quotes(string),variable_names(Vs)]),E,(fmt(E),fail)).

%= --------- to test program -------------
:-export(snarky/0).
snarky:-current_input(In),current_output(Out),!,snarky(In,Out).
:-export(snarky/2).
:-ensure_loaded(dbase_i_sexpr_reader).

open_input(InS,InS):-is_stream(InS),!.
open_input(string(InS),In):-text_to_string(InS,Str),string_codes(Str,Codes),open_chars_stream(Codes,In),!.


snarky(InS,Out) :-
  open_input(InS,In),
   repeat,             
        once((snark_action_mode(Mode),write(Out,Mode),write(Out,'> '))),
        snark_read(In,Wff,Vs),
         b_setval('$variable_names', Vs),
           portray_clause(Out,Wff,[variable_names(Vs),quoted(true)]),
           once(snark_process(Wff)),
           Wff == end_of_file,!.

:-export(id_to_why/3).
why_to_id(Term,Wff,IDWhy):-not(atom(Term)),term_to_atom(Term,Atom),!,why_to_id(Atom,Wff,IDWhy).
why_to_id(Atom,Wff,IDWhy):-wid(IDWhy,Atom,Wff),!.
why_to_id(Atom,Wff,IDWhy):-must(atomic(Atom)),gensym(Atom,IDWhyI),kb_incr(IDWhyI,IDWhy),assertz_if_new(user:wid(IDWhy,Atom,Wff)).

:-export(snark_process/1).
snark_process(end_of_file):-!.
snark_process(prolog):-prolog_repl,!.
snark_process(Assert):- atom(Assert),retractall(snark_action_mode(_)),asserta(snark_action_mode(Assert)),fmtl(snark_action_mode(Assert)),!.
snark_process(Wff):-snark_action_mode(Mode),snark_process(Mode,Wff),!.

snark_process(_,':-'(Wff)):-!, snark_process(call,Wff).
snark_process(_,'?-'(Wff)):- !, snark_ask(Wff).
snark_process(_,'ask'(Wff)):- !, snark_ask(Wff).
snark_process(_,'tell'(Wff)):- !, snark_tell(Wff).
snark_process(call,Call):-!,call(Call).
snark_process(tell,Wff):- !, snark_tell(Wff).
snark_process(ask,Wff):- !, snark_ask(Wff).
snark_process(Other,Wff):- !, wdmsg(error(missing_snark_process(Other,Wff))),!,fail.

:-export(snark_ask_sent/1).
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


:-export(snark_ask/1).
snark_ask(P <=> Q) :-snark_ask_sent(P <=> Q).
snark_ask(P => Q) :-snark_ask_sent(P => Q).
snark_ask(P v Q) :-snark_ask_sent(P v Q).
snark_ask(P & Q) :-snark_ask_sent(P & Q).
snark_ask(Goal0) :-  logical_pos(_KB,Goal0,Goal),
    no_repeats(user:(
	add_args(Goal0,Goal,_,_,[],_,_,[],[],DepthIn,DepthOut,[PrfEnd|PrfEnd],_ProofOut1,Goal1,_),!,
        search(Goal1,60,0,1,3,DepthIn,DepthOut))).

:-export(snark_ask/2).
snark_ask(Goal0,ProofOut) :- logical_pos(_KB,Goal0,Goal),
    no_repeats(user:( 
	add_args(Goal0,Goal,_,_,[],_,_,[],[],DepthIn,DepthOut,[PrfEnd|PrfEnd],ProofOut1,Goal1,_),!,
        search(Goal1,60,0,1,3,DepthIn,DepthOut),
        contract_output_proof(ProofOut1,ProofOut))).

snark_tell(Wff):- why_to_id(tell,Wff,Why),snark_tell(Why,Wff).

:-export(snark_tell/2).
snark_tell(_,[]).
snark_tell(Why,[H|T]):-!,snark_tell(Why,H),kb_incr(Why,Why2),snark_tell(Why2,T).
snark_tell(Why,Wff):-must(kif_to_boxlog(Wff,Why,Asserts)),must(snark_tell_boxes(Why,Asserts)),!.

snark_tell_boxes(_,[]).
snark_tell_boxes(Why,[H|T]):-!,snark_tell_boxes(Why,H),kb_incr(Why,Why2),snark_tell_boxes(Why2,T).
snark_tell_boxes(Why,Assert):- boxlog_to_pttp(Assert,Prolog),
  wdmsgl(snark_tell_boxes(pttp_assert_wid(Why,Prolog))),unnumbervars(Prolog,PTTP), must(pttp_assert_wid(Why,PTTP)).


boxlog_to_pttp(V,V):-is_ftVar(V),!.
boxlog_to_pttp(impossible_in(_, H), - HH):-!,boxlog_to_pttp(H,HH).
boxlog_to_pttp(proven_in(_, H),  HH):-!,boxlog_to_pttp(H,HH).

boxlog_to_pttp(FSkip1, Conj):-FSkip1=..[when_in,_|ARGS],maplist(boxlog_to_pttp,ARGS,LIST),list_to_conjuncts(LIST,Conj),!.
boxlog_to_pttp(proven_t(H),  HH):-!,boxlog_to_pttp(H,HH).
boxlog_to_pttp(impossible_t(H), - HH):-!,boxlog_to_pttp(H,HH).
boxlog_to_pttp((H:-B),(HH:-BB)):-!,boxlog_to_pttp(H,HH),boxlog_to_pttp(B,BB).
boxlog_to_pttp((H & B),(HH , BB)):-!,boxlog_to_pttp(H,HH),boxlog_to_pttp(B,BB).
boxlog_to_pttp((H v B),(HH ; BB)):-!,boxlog_to_pttp(H,HH),boxlog_to_pttp(B,BB).
boxlog_to_pttp((H , B),(HH , BB)):-!,boxlog_to_pttp(H,HH),boxlog_to_pttp(B,BB).
boxlog_to_pttp((H ; B),(HH ; BB)):-!,boxlog_to_pttp(H,HH),boxlog_to_pttp(B,BB).
boxlog_to_pttp( - (H), - (HH)):-!,boxlog_to_pttp(H,HH).
boxlog_to_pttp(BL,PTTP):-as_prolog(BL,PTTP).


/*
:-told.
:-dmsg_show(_).
:-dmsg('i see this').
:- kif_to_boxlog(exists(C, course(C) & -exists(MT3, midterm(C,MT3)))).
:- snark_test_string(TODO),snarky(string(TODO),current_output).
:-set_no_debug.
*/
:-notrace.
:-nodebug.

:-wdmsg('we see this').


:-dynamic(snark_pred_head/1).

snark_pred_head(P):-var(P),user:mpred_prop(F,prologSNARK),mpred_arity(F,A),functor(P,F,A).
snark_pred_head(P):-get_functor(P,F,_),mpred_prop(F,prologPTTP).

:-dynamic(pttp_pred_head/1).

pttp_pred_head(P):-var(P),user:mpred_prop(F,prologPTTP),mpred_arity(F,A),functor(P,F,A).
pttp_pred_head(P):-get_functor(P,F,_),mpred_prop(F,prologPTTP).

:-multifile(snarky_comment/1).


pttp_listens_to_stub(prologPTTP).
pttp_listens_to_stub(prologSNARK).


user:provide_mpred_setup(Op,H):-provide_snark_op(Op,H).

% OPHOOK ASSERT
provide_snark_op(change(assert,_How),(HeadBody)):- 
   pttp_listens_to_head(HeadBody),
   why_to_id(provide_snark_op,(HeadBody),ID),
   snark_tell(ID,(HeadBody)).

% OPHOOK CALL
provide_snark_op(call(_How),Head):- 
  pttp_listens_to_head(Head),
  pttp_call(Head).

% OPHOOK CLAUSES
provide_snark_op(clauses(_How),(Head:-Body)):- 
   pttp_listens_to_head(Head),
   provide_mpred_storage_clauses(wid,Head,Body).

% OPHOOK 
provide_snark_op(OP,(HeadBody)):- 
   pttp_listens_to_head(HeadBody),
   snark_process(OP,HeadBody).


% CLAUSES HOOK 
user:provide_mpred_storage_clauses(wid,H,B,ftProofFn(IDWhy)):-wid(IDWhy,_,(H:-B)).
user:provide_mpred_storage_clauses(wid,H,true,ftProofFn(IDWhy)):-wid(IDWhy,_,(H)),compound(H),not(functor(H,':-',2)).


% REGISTER HOOK
user:provide_mpred_setup(OP,HeadIn,StubType,RESULT):-  pttp_listens_to_stub(StubType),!,
   get_pifunctor(HeadIn,Head,F),
      assert_if_new(user:mpred_prop(F,prologPTTP)),
         ensure_universal_stub(Head),
         RESULT = declared(pttp_listens_to_head(OP,Head)).

:- uses_logic(logicmoo_kb_refution).

:- if_startup_script(tsnark).
:- if_startup_script(ensure_loaded(dbase_i_mpred_snark_testing)).
:- logicmoo_example3.

end_of_file.

:-dynamic(user:int_proven_t/10).

int_proven_t(P, X, Y, E, F, A, B, C, G, D) :- dbase_t(P,X,Y),
        test_and_decrement_search_cost(A, 0, B),
        C=[H, [proven_t(P, X, Y), D, E, F]|I],
        G=[H|I].


:-dynamic(user:int_assumed_t/10).
int_assumed_t(P, X, Y, E, F, A, B, C, G, D) :- dbase_t(P,X,Y),
        test_and_decrement_search_cost(A, 0, B),
        C=[H, [assumed_t(P, X, Y), D, E, F]|I],
        G=[H|I].




