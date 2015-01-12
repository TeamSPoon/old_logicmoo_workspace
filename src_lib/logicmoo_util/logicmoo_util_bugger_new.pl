:- module(zxz, 
          [ 
          ]). 


:-multifile(system:goal_expansion/2).
:-export(system:goal_expansion/2).
:-multifile(tlbugger:bugger_prolog_flag/2).
:-multifile(mpred_arity/2).
:-meta_predicate(lmdmsg_call(0)).
:-dynamic(mpred_arity/2).
:-thread_local(inxp/0).
:- meta_predicate wno(:,:).
:- meta_predicate wyes(:,:).
:-visible(+all).
:-leash(+all).


menvext(_Goal,F,top(A,B,C,D),top([F|A],B,C,D)).
menv(top([],[],[],[])).


lmdmsg(D):-format(user_error,'dmsg: ~q~n',[D]).
lmdmsg_call(D):- ( (lmdmsg(lmdmsg_call(D)),call(D),lmdmsg(lmdmsg_exit(D))) *-> true ; lmdmsg(lmdmsg_failed(D))).

:-use_module(library(apply)).
:-meta_predicate pcall(:).
:-export(pcall/1).
:-meta_predicate(out_mcall(:)).
:-export(out_mcall/1).
:-meta_predicate(in_mcall(:)).
:-export(in_mcall/1).
:-meta_predicate(mcall(:)).
:-export(mcall/1).
:-meta_predicate(mcall(?,?)).
:-export(mcall/2).
:-meta_predicate(mcall_expansion(:,?)).

mcall(Goal):- menv(Env),mcall(Goal, Env).

:-export(mcall/2).
% mcall(M:Goal,Env):-atomic(M),M=zxz,!,mcall(Goal,Env).
mcall(V, _):- var(V), !, throw(error(instantiation_error, _)).
mcall(true, _Env):-!, true.
mcall(fail, _Env):-!, fail.
mcall(A is B, _Env):-!, A is B.
mcall(\+ A, Env):-!, \+ mcall(A, Env).
mcall(!, _Env):-!, ( true ; throw(cut(_)) ).
mcall(tt, _Env):-!,  throw(tt).

mcall((Goal1, Goal2), Env):- !,mcall(Goal1, Env),mcall(Goal2, Env).
mcall((Goal1; Goal2), Env):- !,mcall(Goal1, Env);mcall(Goal2, Env).
mcall((Goal1 -> Goal2 ; Goal3), Env):- !, (mcall(Goal1, Env) -> mcall(Goal2, Env) ; mcall(Goal3, Env)).
% mcall(M:Goal,Env):-atomic(M),!,mcall(Goal,Env).
mcall(Goal,Env):-predicate_property(Goal,meta_predicate(MA)),MA=..[F|MARGS],Goal=..[F|GARGS],maplist(do_meta_arg(Env),MARGS,GARGS,NARGS),!,NG=..[F|NARGS],mcall2(NG,Env).
mcall(Goal,Env):-mcall2(Goal,Env).

do_meta_arg(_Env,(+),Goal,Goal).
do_meta_arg(_Env,(-),Goal,Goal).
do_meta_arg(_Env,(?),Goal,Goal).
do_meta_arg(Env, _,Goal,mcall(Goal,Env)).

:-export(mcall2/2).
mcall2(M:Goal,Env):-!,mcall3(M,Goal,Env).
mcall2(Goal,Env):-mcall3(user,Goal,Env).

mcall3(_M,Goal,Env):- 
    functor(Goal,F, _),
    nod('Call: ', Goal, Env),    
    catch( ( redo_clause(Env, Goal, F, BodyOut),call(BodyOut) ), cut(F), fail),
    nod('Exit: ', Goal, Env).
mcall3(_M,Goal, Env):-
    nod('Fail: ', Goal, Env),
    fail.

redo_clause(Env, Goal, F, mcall(BBody, Env1)) :- 
  predicate_property(Goal,number_of_clauses(NC)),!,NC>0,NC<100000,
  menvext(Goal,F,Env,Env1),
    (findall(Goal-Body, clause(Goal, Body), [First|Rest]),
    ( Goal-Body = First
    ; length(Rest, RL), length(RestC, RL),
      member(Goal-Body,RestC),
      nod('Redo: ', Goal, Env),
      Rest = RestC
    )),
    better_body(Body,BBody).

redo_clause(_Env, Goal, _F, pcall(Goal)).

better_body(Body,Body).

nod(Message, Goal, Env):- nop(nod(Message, Goal, Env)),!.
nod(Message, Goal, Env):- arg(1,Env,List),length(List,Tab),tab(Tab),tab(Tab),write(Env), write(': '), write(Message),write(Goal), nl.

:- meta_predicate trace_query(:,*,0).

trace_query(In, Out, Query):-
    consult(In),
    tell(Out),
    call_with_depth_limit(findall(Query, mcall(Query), Solutions), 40, XMessage),
    writeln(XMessage),
    writeln(Solutions),
    told,
    unload_file(In),
    true.

mpred_arity(A,L):-atom_length(A,L).


new_bugger_expansion(Goal,GO) :- \+ inxp, wyes(inxp,mcall_expansion(Goal,GO)),!.
mcall_expansion(M:Goal,GO):-atomic(M),nonvar(Goal),!, functor(Goal,F,A),mcall_expansion(Goal,M,F,A,GO),!.
mcall_expansion(Goal,GO):-nonvar(Goal),functor(Goal,F,A),mcall_expansion(Goal,user,F,A,GO),!.

mcall_expansion(_,_M,mcall,_,_):-!,fail.
% mcall_expansion(Goal,_M,_,_,Goal):-predicate_property(zxz:Goal,line_count(_)),!,fail.
mcall_expansion(_,_M,'[|]',_,_):-!,fail.
mcall_expansion(_,_M,'[]',_,_):-!,fail.
mcall_expansion(_,_M,module,_,_):-!,fail.
mcall_expansion(_,_M,(:-),1,_):-!,fail.
mcall_expansion(_,_M,pcall,_,_):-!,fail.
mcall_expansion(_,_M,call,_,_):-!,fail.
mcall_expansion(Goal,_M,_Call,_,Goal):-!.
mcall_expansion(Goal,M,F,_,zxz:mcall(M:Goal)):- atom_length(F,L),L<4.
mcall_expansion(Goal,M,F,A,zxz:mcall(M:Goal)):- \+ current_predicate(F/A).
mcall_expansion(Goal,M,F,A,zxz:mcall(M:Goal)):- \+ \+ mpred_arity(F,A).

wno(A,Goal):-setup_call_cleanup(asserta((A:-!,fail),REF),Goal,erase(REF)).
wyes(A,Goal):-setup_call_cleanup(asserta(A,REF),Goal,erase(REF)).

pcall(P):-out_mcall(call(P)).
out_mcall(Goal):-lmdmsg(out_mcall(Goal)), wno(inxp,Goal).
in_mcall(Goal):- % lmdmsg(in_mcall(Goal)), 
            wyes(inxp,Goal).


%user:goal_expansion(Goal,GO):- new_bugger_expansion(Goal,GO), Goal \=@= GO,!.
%system:goal_expansion(Goal,GO):-  new_bugger_expansion(Goal,GO), Goal \=@= GO,!.
%user:term_expansion(Goal,GO):- new_bugger_expansion(Goal,GO).
%system:term_expansion(Goal,GO):- new_bugger_expansion(Goal,GO).
%:-listing(system:goal_expansion/2).

end_of_file.


%= Compute normal forms for SHOIQ formulae.
%= Skolemize SHOIQ formula.
%=
%= Copyright (C) 1999 Anthony A. Aaby <aabyan@wwc.edu>
%= Copyright (C) 2006-2007 Stasinos Konstantopoulos <stasinos@users.sourceforge.net>
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
%= dlnot(A)
%= dland(F, F)
%= dlor(F, F)
%= dlimplies(F, F)
%= dlequiv(F, F)
%=    all(X,A)
%=    exists(X,A)
%=    atleast(X,N,A)
%=    atmost(X,N,A)


:- module(dbase_i_snark, 
          [ % nnf/2, pnf/3,pnf/2, cf/3,
          op(300,fx,'~'),
          op(600,xfx,'=>'),
          op(650,xfx,'<=>'),
          op(350,xfx,'xor'),
          op(400,yfx,'&'),  
          op(500,yfx,'v')
        ]). 

% SWI Prolog modules do not export operators by default
% so they must be explicitly placed in the user namespace

%  all(R, room(R) => exists(D, (door(D) & has(R,D))))
% for any arbitrary R, if R is a room then there exists some object D that is a door, and R has a D.
% door(sk6(_G180)):-room(_G180)
% has(_G180,sk6(_G180)):-room(_G180)
%  R is not a room if D is a door and R doesn't have D
% if there are no doors anywhere then there must not be rooms
% ~room(R) :- ~has(R,_).

:- ( current_prolog_flag(argv,[pl|_]) ->
     op( 400, fy, user:(box) ),	% Necessity, Always
     op( 400, fy, user:(dia) ),	% Possibly, Eventually
     op( 400, fy, user:(cir) )	% Next time
   ;
     op(400,fy,box),		% Necessity, Always
     op(400,fy,dia),		% Possibly, Eventually
     op(400,fy,cir)		% Next time
   ).

:-style_check(+singleton).

each_subterm(B, A):- (compound(B), arg(_, B, C), each_subterm(C, A));A=B.

each_subterm(A,Pred,B):- call( Pred,A,B).
each_subterm(A,Pred,O):- 
   compound(A),
   once(  A=[H|T] ;  A=..[H|T] ),
   (each_subterm(H,Pred,O);
     each_subterm(T,Pred,O)).

:-export(term_singletons/2).
term_singletons(A,Vs):- term_singletons(A,[],_,[],Vs). 
:-export(term_singletons/5).
term_singletons(Fml, NS,NS, S,S):-atomic(Fml),!.
term_singletons(Fml, NS,NS, S,S):-identical_member(Fml,NS),!.
term_singletons(Fml, NS, [Fml|NS], S, NSV):- is_ftVar(Fml),identical_member(Fml,S),!,delete_eq(S,Fml,NSV),!.
term_singletons(Fml, NS, NS, S, [Fml|S]):- is_ftVar(Fml),!.
term_singletons([H|T],NS,NSO,S,NSV):- !, term_singletons(H,NS,NSM,S,M),term_singletons(T,NSM,NSO,M,NSV).
term_singletons(Fml, NS,NSO, S,NSV):- compound(Fml),Fml=..[_,H|T],!, term_singletons(H,NS,NSM,S,M),term_singletons(T,NSM,NSO, M,NSV).


:-export(subsT_each/4).
subsT_each(_,In,[],In):-!.
subsT_each(each,In,[X=Y|TODO],Out):-!,subst_eq(In,X,Y,Mid),subsT_each(each,Mid,TODO,Out),!.
subsT_each(REV,In,[X=Y|TODO],Out):-subst_eq(In,Y,X,Mid),subsT_each(REV,Mid,TODO,Out),!.

contains_var_lits(Fml,Var,Lits):- findall(Lit,contains_t_var(Fml,Var,Lit),Lits).

get_isa(Lit,_,_):-not(compound(Lit)),!,fail.
get_isa(mudIsa(I,T),I,T).
get_isa(IT,I,T):-IT=..[T,I].

not_ftVar(V):-not(is_ftVar(V)).
is_ftVar(V):-var(V),!.
is_ftVar('$VAR'(_)).

contains_type_lits(Fml,Var,Lits):- findall(T,(contains_t_var(Fml,Var,Lit),get_isa(Lit,O,T),same_var(O,Var)),Lits).
contains_t_var(Fml,Var,Term):-each_subterm(Fml,Term),compound(Term),arg(_,Term,O),same_var(O,Var).

:-export(type_of_var/3).
type_of_var(Fml,Var,Type):-contains_type_lits(Fml,Var,Lits),!,(member(Type,Lits)*->true;Type='Unk').

to_dlog_ops([
       % ';'='dlor',
       % ','='dland',
     'not'='dlnot',      
      ':-'='entails',
       '&'='dland',
       '~'='dlnot',
     'and'='dland',
      'or'='dlor',
       'v'='dlor',
      ':-'='entails',
      '<='='entails',
 'implies'='dlimplies',
   'equiv'='dlequiv',
      '=>'='dlimplies',
     '<=>'='dlequiv']).

to_symlog_ops(['dlor'='v',
   'dland'='&',
   'dlimplies'='=>',
   'dlequiv'='<=>',
   'dlnot'='~',
   'entails'='<=']).

to_prolog_ops(['dlor'=';',
   'dland'=',',
   'dlimplies'='=>',
   'dlequiv'='<=>',
   'dlnot'='not',
   'entails'=':-']).


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

% Usage: nnf(MT, Orig,+Fml, ?NNF)

nnf(MT,Fml,NNF) :-    
   as_dlog(Fml,FmlO),
   copy_term(FmlO,FmlO2),
   nnf(MT,FmlO2,FmlO,[],NNF,_),!.

%=----- drive negation inward --------------
%  nnf(MT, Orig,+Fml,+FreeV,-NNF,-Paths)
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
ct(asserted).
b_d(nesc,poss).
b_d(knows,beliefs).
b_d(always,sometimes).
b_d(box,dia).

nnf(MT, _Orig,Lit,_FreeV,Pos,1):-is_ftVar(Lit),!,trace,Pos=proven(MT,Lit).

nnf(MT, Orig,Fin,FreeV,BOX,Paths) :- is_b(box(BDT),Fin,F), !,
	nnf(MT, Orig,F,FreeV,NNF,Paths), cnf( Orig,NNF,CNF), boxRule( Orig,box(BDT,CNF), BOX).

nnf(MT, Orig,Fin,FreeV,DIA,Paths) :- is_b(dia(BDT),Fin,F), !,
	nnf(MT, Orig,F,FreeV,NNF,Paths), dnf( Orig,NNF,DNF), diaRule( Orig,dia(BDT,DNF), DIA).

nnf(MT, Orig,Fin,FreeV,CIR,Paths) :- is_b(cir(CT),Fin,F), !,
	nnf(MT, Orig,F,FreeV,NNF,Paths), cirRule( Orig,cir(CT,NNF), CIR).

nnf(MT, Orig,until(A,B),FreeV,NNF,Paths) :- !,
	nnf(MT, Orig,A,FreeV,NNF1,Paths1),
	nnf(MT, Orig,B,FreeV,NNF2,Paths2),
	Paths is Paths1 + Paths2,
	NNF = until(NNF1, NNF2).

/*
% ----- quantifiers -----------------------
nnf(MT, Orig,all(X,NNF),FreeV,all(X,NNF),Paths) :- \+ occurs(X,FreeV),!,  %= MUST?! use separate variables
      nnf(MT, Orig,F,[X|FreeV],NNF,Paths).

nnf(MT, Orig,exists(X,P),FreeV,Q) :- \+ occurs(X,FreeV),   !,  %= MUST?! use separate variables
	skolem( Orig,Fml,X,FreeV,FmlSk),
	nnf(MT, Orig,FmlSk,FreeV,NNF,Paths).
*/

nnf(MT, Orig,all(X,F),FreeV,all(X,NNF),Paths) :- !,
	nnf(MT, Orig,F,[X|FreeV],NNF,Paths).

nnf(MT, Orig,exists(X,Fml),FreeV,NNF,Paths) :- !,
	skolem( Orig,Fml,X,FreeV,FmlSk),
	nnf(MT, Orig,FmlSk,FreeV,NNF,Paths).

nnf(MT, Orig,atleast(1,X,Fml),FreeV,NNF,Paths) :- !,
	nnf(MT, Orig,exists(X,Fml),FreeV,NNF,Paths).
nnf(MT, Orig,atleast(N,X,Fml),FreeV,NNF,Paths) :-
	!,
	NewN is N - 1,
        subst_eq(Fml,X,Y,FmlY),
	nnf(MT, Orig,dland(exists(X,Fml),atleast(NewN,Y,FmlY)),FreeV,NNF,Paths).
nnf(MT, Orig,atmost(1,X,Fml),FreeV,NNF,Paths) :- 
	!,
        subst_eq(Fml,X,Y,FmlY),
        subst_eq(Fml,X,Z,FmlZ),
	nnf(MT, Orig,dlnot(dland(exists(Y,FmlY),exists(Z,FmlZ))),FreeV,NNF,Paths).
nnf(MT, Orig,atmost(N,X,Fml),FreeV,NNF,Paths) :-
	!,
        subst_eq(Fml,X,Y,FmlY),
	NewN is N - 1,
	nnf(MT, Orig,dland(exists(Y,FmlY),atmost(NewN,X,Fml)),FreeV,NNF,Paths).

nnf(MT, Orig,dlnot(xor(X , Y)),FreeV,NNF,Paths) :-
   !,
   nnf(MT, Orig,dlor(dland(X , Y) , dland(dlnot(X) , dlnot(Y))),FreeV,NNF,Paths).
   
nnf(MT, Orig,xor(X , Y),FreeV,NNF,Paths) :-
   !,
   nnf(MT, Orig,dland(dlor(X , Y) , dlor(dlnot(X) , dlnot(Y))),FreeV,NNF,Paths).
   

nnf(MT, Orig,dland(A,B),FreeV,NNF,Paths) :- !,
	nnf(MT, Orig,A,FreeV,NNF1,Paths1),
	nnf(MT, Orig,B,FreeV,NNF2,Paths2),
	Paths is Paths1 * Paths2,
	( Paths1 > Paths2 -> NNF = dland(NNF2,NNF1);
		            NNF = dland(NNF1,NNF2)).

nnf(MT, Orig,dlor(A,B),FreeV,NNF,Paths) :- !,
        nnf(MT, Orig,A,FreeV,NNF1,Paths1),
	nnf(MT, Orig,B,FreeV,NNF2,Paths2),
	Paths is Paths1 + Paths2,
	( Paths1 > Paths2 -> NNF = dlor(NNF2,NNF1);
		            NNF = dlor(NNF1,NNF2)).

nnf(MT, Orig,Fml,FreeV,NNF,Paths) :- 
	(Fml = dlnot(dlnot(A))   -> Fml1 = A;
	 Fml = dlnot(box(BDT,F))      -> Fml1 = dia(BDT,dlnot(F));
	 Fml = dlnot(dia(BDT,F))      -> Fml1 = box(BDT,dlnot(F));
	 Fml = dlnot(cir(CT,F))      -> Fml1 = cir(CT,dlnot(F));
	 Fml = dlnot(until(A,B)) -> (nnf(MT, Orig,dlnot(A),FreeV,NNA,_), nnf(MT, Orig,dlnot(B),FreeV,NNB,_),
                                     Fml1 = dlor( all(NNB), until(NNB,dland(NNA,NNB))));
	 Fml = dlnot(all(X,F))   -> Fml1 = exists(X,dlnot(F));
	 Fml = dlnot(exists(X,F))    -> Fml1 = all(X,dlnot(F));

	 Fml = dlnot(atleast(N,X,F)) -> Fml1 = atmost(N,X,F);
	 Fml = dlnot(atmost(N,X,F)) -> Fml1 = atleast(N,X,F);

	 Fml = dlnot(dlor(A,B))  -> Fml1 = dland( dlnot(A), dlnot(B) );
	 Fml = dlnot(dland(A,B)) -> Fml1 = dlor( dlnot(A), dlnot(B) );
	 Fml = dlimplies(A,B)        -> Fml1 = dlor( dlnot(A), B );
	 Fml = dlnot(dlimplies(A,B)) -> Fml1 = dland( A, dlnot(B) );
	 Fml = dlequiv(A,B)        -> Fml1 = dlor( dland(A, B), dland(dlnot(A), dlnot(B)) );
	 Fml = dlnot(dlequiv(A,B)) -> Fml1 = dlor( dland(A, dlnot(B)) , dland(dlnot(A), B) )
	),!,
	nnf(MT, Orig,Fml1,FreeV,NNF,Paths).

/*
nnf(MT, _Orig,Fml,_,Fml,1):- Fml=..[F,MT,_],third_order(F),!.
nnf(MT,  Orig,Fml,FreeV,Out,Path):- Fml=..[F,A],third_order(F),  
  nnf(MT, Orig,A,FreeV,NNF1,Path1),!,
  Fml2=..[F,MT,NNF1],nnf(MT, Orig,Fml2,FreeV,Out,Path2),Path is Path1+Path2.
*/
nnf(MT,_Orig,Fml,_,Out,1):- negate(MT,Fml,Neg),negate(MT,Neg,Out),!.

third_order(asserted).

boxRule( Orig,A,B):-convertAndCall(as_dlog,boxRule( Orig,A,B)).
boxRule( Orig,box(BDT,dland(A,B)), dland(BA,BB)) :- !, boxRule( Orig,box(BDT,A),BA), boxRule( Orig,box(BDT,B),BB).
boxRule(_Orig,BOX, BOX).
 
diaRule( Orig,A,B):-convertAndCall(as_dlog,diaRule( Orig,A,B)).
diaRule( Orig,dia(BDT,dlor(A,B)), dlor(DA,DB)) :- !, diaRule( Orig,dia(BDT,A),DA), diaRule( Orig,dia(BDT,B),DB).
diaRule(_Orig,DIA, DIA).

cirRule( Orig,A,B):-convertAndCall(as_dlog,cirRule( Orig,A,B)).
cirRule( Orig,cir(CT,dlor(A,B)), dlor(DA,DB)) :- !, cirRule( Orig,cir(CT,A),DA), cirRule( Orig,cir(CT,B),DB).
cirRule( Orig,cir(CT,dland(A,B)), dland(DA,DB)) :- !, cirRule( Orig,cir(CT,A),DA), cirRule( Orig,cir B,DB).
cirRule(_Orig,CIR, CIR).


%=%
%=%  Conjunctive Normal Form (CNF) -- assumes Fml in NNF
%=%

% Usage: cnf( Orig, +NNF, ?CNF )
cnf( A,B):-copy_term(A,Orig),cnf( Orig,A,B).
cnf( Orig,A,B):-convertAndCall(as_dlog,cnf( Orig,A,B)).
cnf( Orig,dland( P,Q), dland( P1,Q1)):- !, cnf( Orig,P, P1), cnf( Orig,Q, Q1).
cnf( Orig,dlor( P,Q),     CNF):- !, cnf( Orig,P, P1), cnf( Orig,Q, Q1), cnf1( Orig, dlor( P1,Q1), CNF ).
cnf(_Orig,CNF,       CNF).

cnf1( Orig, dlor(dland( P,Q), R), dland( P1,Q1) ):- !, cnf1( Orig, dlor( P,R), P1), cnf1( Orig, dlor(Q,R), Q1).
cnf1( Orig, dlor( P, dland(Q,R)), dland( P1,Q1) ):- !, cnf1( Orig, dlor( P,Q), P1), cnf1( Orig, dlor( P,R), Q1).
cnf1(_Orig, CNF,                 CNF).


%=%
%=% Disjunctive Normal Form (DNF) -- assumes Fml in NNF
%=%
% Usage: dnf( Orig, +NNF, ?DNF )
dnf( A,B):-copy_term(A,Orig),dnf( Orig,A,B).
dnf( Orig,A,B):-convertAndCall(as_dlog,dnf( Orig,A,B)).
dnf( Orig, dlor( P,Q),  dlor( P1,Q1) ) :- !, dnf( Orig,P, P1), dnf( Orig,Q, Q1).
dnf( Orig, dland( P,Q), DNF) :- !, dnf( Orig,P, P1), dnf( Orig,Q, Q1), dnf1( Orig,dland( P1,Q1), DNF).
dnf(_Orig,DNF,       DNF).

dnf1( Orig,dland( P, dlor(Q,R)),  dlor( P1,Q1) ):- !, dnf1( Orig,dland( P,Q), P1), dnf1( Orig,dland( P,R), Q1).
dnf1( Orig,dland( dlor( P,Q), R), dlor( P1,Q1) ):- !, dnf1( Orig,dland( P,R), P1), dnf1( Orig,dland(Q,R), Q1).
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

pnf( Orig,  dland(exists(X,A) , B),Vs,  exists(Y,PNF)) :- !, copy_term((X,A,Vs),(Y,Ay,Vs)),
                                        pnf( Orig,dland(Ay,B),[Y|Vs], PNF).
pnf( Orig,  dlor(exists(X,A), B),Vs,  exists(Y,PNF)) :- !, copy_term((X,A,Vs),(Y,Ay,Vs)),
                                        pnf( Orig,dlor(Ay,B),[Y|Vs], PNF).
pnf( Orig, dland(all(X,A), B),Vs, all(Y,PNF)) :- !, copy_term((X,A,Vs),(Y,Ay,Vs)),
                                        pnf( Orig,dland(Ay , B),[Y|Vs], PNF).
pnf( Orig, dlor(all(X,A), B),Vs, all(Y,PNF)) :- !, copy_term((X,A,Vs),(Y,Ay,Vs)),
                                        pnf( Orig,dlor(Ay,B),[Y|Vs], PNF).

pnf( Orig, dland(A,exists(X,B)),Vs,  exists(Y,PNF)) :- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnf( Orig,dland(A, By),[Y|Vs], PNF).
pnf( Orig, dlor(A,exists(X,B)),Vs,  exists(Y,PNF)) :- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnf( Orig,dlor(A,By),[Y|Vs], PNF).
pnf( Orig, dland(A,all(X,B)),Vs, all(Y,PNF)) :- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnf( Orig,dland(A,By),[Y|Vs], PNF).
pnf( Orig, dlor(A,all(X,B)),Vs, all(Y,PNF)) :- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnf( Orig,dlor(A,By),[Y|Vs], PNF).

pnf( Orig, dland(A, B),Vs,       PNF ) :- pnf( Orig,A,Vs,Ap), pnf( Orig,B,Vs,Bp), 
                                     (A\=Ap; B\=Bp), pnf( Orig,dland(Ap,Bp),Vs,PNF).
pnf( Orig, dlor(A, B),Vs,       PNF ) :- pnf( Orig,A,Vs,Ap), pnf( Orig,B,Vs,Bp), 
                                     (A\=Ap; B\=Bp), pnf( Orig,dlor(Ap,Bp),Vs,PNF).

pnf(_Orig,          PNF, _,       PNF ).

%=%  Clausal Form (CF) -- assumes Fml in PNF and
%                                 each quantified variable is unique

% cf(MT, Orig,+Fml, ?Cs)
% Cs is a list of the form: [cl(Head,Body), ...]
% Head and Body are lists.

cf(MT,A,B,C):-convertAndCall(as_dlog,cf(MT,A,B,C)).
cf(MT, Orig,PNF, SO):- removeQ( Orig,PNF,[], UnQ), cnf( Orig,UnQ,CNF), 
  once((dnf( Orig,UnQ,DNF), wdmsgl(dnf2(DNF)))),
  flatten_or_list(Orig,DNF,Flat),
  wdmsgl(flat(xor(Flat))),
  make_clauses_each(MT,Flat,EachClause),
  clausify(MT, CNF,Cla,[]),!,append(Cla,EachClause,O),sort(O,SO).

% removes quantifiers
removeQ(A,B,C,D):-convertAndCall(as_dlog,removeQ(A,B,C,D)).
removeQ( Orig, all(X,F),Vars, RQ) :- removeQ( Orig,F,[X|Vars], RQ).
removeQ( Orig,  exists(X,F),Vars, RQ) :-
	skolem( Orig,F,X,Vars,Fsk),
	removeQ( Orig,Fsk,Vars, RQ).
removeQ(_Orig, F,_,F ).

clausify(MT,  dland( P,Q), C1, C2 ) :-
	!,
	clausify(MT,  P, C1, C3 ),
	clausify(MT,  Q, C3, C2 ).
clausify(MT,  P, [cl(A,B)|Cs], Cs ) :-
	inclause(MT, P, A, [], B, [] ),
	!.
clausify(_MT,  _, C, C ).

inclause(MT, dlor( P,Q), A, A1, B, B1 ) :-
	!,
	inclause(MT, P, A2, A1, B2, B1 ),
	inclause(MT, Q, A,  A2, B,  B2 ).
inclause(MT, dlnot( PP), A,  A, B1, B ) :- 
        negate(MT,dlnot( PP),P),
	!,
	notin( P, A ),
	putin(MT, P, B, B1 ).
inclause(MT, P,  A1, A, B,  B ) :-
	!,
	notin( P, B ),
	putin(MT, P, A, A1 ).

notin(X,[Y|_]) :- X==Y, !, fail.
notin(X,[_|Y]) :- !,notin(X,Y).
notin(_,[]).

putin(_MT,X,[],   [X]   ) :- !.
putin(_MT,X,[Y|L],[Y|L] ) :- X == Y,!.
putin( MT,X,[Y|L],[Y|L1]) :- putin(MT,X,L,L1).

make_clauses_each(MT,List,Out):-findall(E,make_each(MT,List,E),Out).
make_each(MT,List,E):- member(One,List), make_1_cl(MT,One,List,E).

make_1_cl(MT,One,List,cl([One],NewBodyList)):-is_neg(One),!,delete_eq(List,One,Rest),maplist(negate(MT),Rest,NewBodyList).
make_1_cl(MT,One,List,cl([One],NewBodyList)):-is_pos(One),!,delete_eq(List,One,Rest),maplist(negate(MT),Rest,NewBodyList).

is_neg(dlnot(_)).
is_pos(proven(_)).
is_pos(One):-get_functor(One,F),!,not(is_log_op(F)).

is_log_op(OP):-atomic(OP),to_dlog_ops(OPS),!,(member(OP=_,OPS);member(_=OP,OPS)).

negate(_MT,possible(X),impossible(X)):-!.
negate(_MT,missing(X),asserted(X)):-!.
negate(_MT,asserted(X),missing(X)):-!.
negate(_MT,dlnot(mudEquals(X,Y)),possible(mudEquals(X,Y))):-!.
negate(_MT,dlnot(X),(X)):-!.
negate(_MT,proven(X),dlnot(X)):-!.
negate(_MT,X,dlnot(X)):-!.

%=%  Skolemizing -- method 1

% Usage: skolemize(+Fml,+X,+FreeV,?FmlSk)
% Replaces existentially quantified variable with the formula
% VARIABLES MUST BE PROLOG VARIABLES
% exists(X,p(X)) --> p(p(exists))

skolem_bad(Fml,X,FreeV,FmlSk):-
	copy_term((X,Fml,FreeV),(Fml,Fml1,FreeV)),
	copy_term((X,Fml1,FreeV),(exists,FmlSk,FreeV)).



%=%  Skolemizing -- method 2

% Usage: skolem( Orig, +Fml, +X, +FreeV, ?FmlSk )
% Replaces existentially quantified variable with a unique function
% fN(Vars) N=1,...
% VARIABLES MAYBE EITHER PROLOG VARIABLES OR TERMS

skolem( Orig, F, X, FreeV, dlimplies(mudEquals(X,Sk),F)) :- skolem_f( Orig, F, X, FreeV, Sk),!.

skolem( Orig, F, X, FreeV, FmlSk) :-
     skolem_f( Orig, F, X, FreeV, Sk),
     pred_subst(same_var, F, X, Sk, FmlSk),!.




skolem_f( Orig, F, X, FreeV, Sk) :-
	contains_var_lits(F,X,LitsList),
        mk_skolem_name( Orig,X,LitsList,'',SK),
        concat_atom(['sk',SK,'Fn'],Fun),
	Sk =..[Fun|FreeV].




%=% Substitution

% Usage: subst_eq(+Fml,+X,+Sk,?FmlSk)
subst_eq(Fml,X,Sk,FmlSkO):-pred_subst(==,Fml,X,Sk,FmlSk),!,FmlSkO=FmlSk.


% Usage: pred_subst(+Pred,+Fml,+X,+Sk,?FmlSk)
/*
pred_subst( Pred,   all(Y,P), X,Sk,   all(Y,P1) ) :- !, pred_subst( Pred, P,X,Sk,P1 ).
pred_subst( Pred,exists(Y,P), X,Sk,exists(Y,P1) ) :- !, pred_subst( Pred, P,X,Sk,P1 ).
pred_subst( Pred, dland( P,Q), X,Sk,dland( P1,Q1) ) :- !, pred_subst( Pred, P,X,Sk,P1 ), pred_subst( Pred, Q,X,Sk,Q1 ).
pred_subst( Pred,  dlor( P,Q), X,Sk, dlor( P1,Q1) ) :- !, pred_subst( Pred, P,X,Sk,P1 ), pred_subst( Pred, Q,X,Sk,Q1 ).
*/
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


%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%
%=% 
%=%   normalize.P
%=%      SWI-Prolog version
%=%   Convert wffs to list of normal logic clauses
%=%
%=%   and       &  
%=%   or        v
%=%   not       ~
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
:- op(400,yfx,'&').  
:- op(500,yfx,'v').
:- op(600,xfx,'=>').
:- op(650,xfx,'<=>').
:- op(350,xfx,'xor').



%=%=%=%=%=%=%=%=%=%=%=
%=% generate a skolem 

mk_skolem_name(_O,Var,Fml,SIn,SOut):-is_ftVar(Fml), same_var(Var,Fml),!,atom_concat('Is',SIn,SOut).
mk_skolem_name(_O,_V,Fml,SIn,SIn):-is_ftVar(Fml),!.
mk_skolem_name(_O ,_V,[],SIn,SIn):-!.
mk_skolem_name(_O,_V, OP,SIn,SIn):- is_log_op(OP),!.
mk_skolem_name(_O,_V,Fml,SIn,SOut):- atomic(Fml),!,i_name(Fml,N),toPropercase(N,CU),!,(atom_contains(SIn,CU)->SOut=SIn;atom_concat(SIn,CU,SOut)).
mk_skolem_name( Orig,Var,[H|T],SIn,SOut):- !,mk_skolem_name( Orig,Var,H,SIn,M),mk_skolem_name( Orig,Var,T,M,SOut).
mk_skolem_name( Orig,Var,mudIsa(VX,Lit),SIn,SOut):- same_var(same_var(Var,VX)),not_ftVar(Lit),!,mk_skolem_name( Orig,Var,['Is',Lit,'In'],'',F),atom_concat(F,SIn,SOut).
mk_skolem_name( Orig,Var,Fml,SIn,SOut):- Fml=..[F,VX],same_var(Var,VX),!,mk_skolem_name( Orig,Var,['Is',F,'In'],SIn,SOut).
mk_skolem_name( Orig,Var,Fml,SIn,SOut):- Fml=..[F,Other,VX|_],same_var(Var,VX),!,type_of_var( Orig,Other,OtherType),
   mk_skolem_name( Orig,Var,[OtherType,'Arg2Of',F],SIn,SOut).
mk_skolem_name( Orig,Var,Fml,SIn,SOut):- Fml=..[F,VX|_],same_var(Var,VX),!,mk_skolem_name( Orig,Var,['Arg1Of',F],SIn,SOut).
mk_skolem_name( Orig,Var,Fml,SIn,SOut):- Fml=..[F|_],!,mk_skolem_name( Orig,Var,['ArgNOf',F],SIn,SOut).

same_var(Var,Fml):-not(not(Var=Fml)),!.
same_var(Var,Fml):-Var==Fml,!.


%=----- distribute --------------------------
distriBute(A,B,C):- convertAndCall(as_symlog,distriBute(A,B,C)).

distriBute((X & Y) v Z, 
            (X v Z) & (Y v Z),true) :- !.
distriBute(X v (Y & Z),
             (X v Y) & (X v Z),true) :- !.
distriBute(X,X,fail).


%=----- conjunctive normal form --------------

cnF( Orig,In,Out):-as_symlog(In,Mid),In\=@=Mid,!,cnF( Orig,Mid,Out).

cnF( Orig,(X & Y),(A & B)) :-
   !,
   cnF( Orig,X,A),
   cnF( Orig,Y,B).
cnF( Orig,(X v Y),G) :-
   !,
   cnF( Orig,X,A),
   cnF( Orig,Y,B),
   distriBute((A v B),F,Flag),
   (Flag -> cnF( Orig,F,G)  %= More work may be needed
             ;
            G = F ).
cnF(_Orig,X,X).


%=-----  make a sequence out of a conjunction -----
flatten_and(A,B,C):- convertAndCall(as_symlog,flatten_and(A,B,C)).
flatten_and( Orig,X & Y, F) :-
   !,
   flatten_and( Orig,X,A),
   flatten_and( Orig,Y, B),
   sequence_append(A,B,F).
flatten_and(_Orig,X,X).


%=-----  make a sequence out of a disjunction -----
flatten_or(A,B,C):- convertAndCall(as_symlog,flatten_or(A,B,C)).
flatten_or( Orig,X v Y, F) :-
   !,
   flatten_or( Orig,X,A),
   flatten_or( Orig,Y,B),
   sequence_append(A,B,F).
flatten_or(_Orig,X,X).

% flatten_or_list(A,B,C):- convertAndCall(as_symlog,flatten_or_list(A,B,C)).
flatten_or_list(Orig,dlor(X , Y), F) :- !,
   flatten_or_list( Orig,X,A),
   flatten_or_list( Orig,Y,B),
   flatten([A,B],F).
flatten_or_list(Orig,X v Y, F) :- !,
   flatten_or_list( Orig,X,A),
   flatten_or_list( Orig,Y,B),
   flatten([A,B],F).
flatten_or_list(_Orig,X,[X]).



%=----- append two sequences -------------------------------
sequence_append((X,R),S,(X,T)) :- !, sequence_append(R,S,T).
sequence_append((X),S,(X,S)).

%=----- separate into positive and negative literals -----------
separate((A,B),P,N) :- 
   !,
   (A = ~X -> N=[X|N1],
               separate(B,P,N1)
             ;
               P=[A|P1],
               separate(B,P1,N) ).
separate(A,P,N) :-
   (A = ~X -> N=[X],
               P = []
            ;
               P=[A],
               N = [] ).

%=----- tautology ----------------------------
tautology( P,N) :- some_occurs(N,P).

some_occurs([F|R],B) :-
   occurs(F,B) | some_occurs(R,B).

occurs(A,[F|_]) :-
   A == F,
   !.
occurs(A,[_|R]) :-
   occurs(A,R).


make_clauses( Orig,(A,B),C) :-
   !,
   flatten_or( Orig,A,F),
   separate(F,P,N),
   (tautology( P,N) -> 
      make_clauses( Orig,B,C)
          ;
      make_clause( P,N,D),
      C = [D|R],
      make_clauses( Orig,B,R) ).
make_clauses( Orig,A,C) :-
   flatten_or( Orig,A,F),
   separate(F,P,N),
   (tautology( P,N) ->
       C = []
        ;
       make_clause( P,N,D),
       C = [D] ).

make_clause([],N, (false :- B)) :-
   !,
   make_sequence(N,B,',').
make_clause( P,[],H) :-
   !,
   make_sequence( P,H,'|').
make_clause( P,N, (H :- T)) :-
   make_sequence( P,H,'|'),
   make_sequence(N,T,',').

make_sequence([A],A,_) :- !.
make_sequence([F|R],(F|S),'|') :-
   make_sequence(R,S,'|').
make_sequence([F|R],(F,S),',') :-
   make_sequence(R,S,',').


:-thread_local(snark_mode(assert)).
snark_mode(tell).

%= --------- to test program -------------
:-export(snarky/0).
snarky :- 
   repeat,             
        once((snark_mode(Mode),write(Mode),write(': '))),
        read(Wff),
        mmake,
        once(snark_process(Wff)),
        Wff == end_of_file,!.

wff_id(Cmd,ID):-functor(Cmd,I,_),gensym(I,ID).

:-export(snark_process/1).
snark_process(prolog):-prolog_repl,!.
snark_process(end_of_file):-!.
snark_process(Assert):- atom(Assert),retractall(snark_mode(_)),asserta(snark_mode(Assert)),fmtl(snark_mode(Assert)),!.
snark_process(Wff):-snark_mode(tell),wff_id(tell(Wff),ID),normalize(Wff,Asserts),maplist(snark_tell(ID),Asserts).
snark_process(Wff):-snark_mode(ask),wff_id(ask(Wff),ID),
   term_variables(Wff,Vars),
   Query=..[queryID,ID|Vars],
   normalize(dlimplies(Wff,Query),Asserts),
   maplist(snark_tell(ID),Asserts),!,
   forall(snark_call(Vars,Query),fmtl(Query)),
   snark_retract(ID),!.

snark_tell(ID,Assert):-fmtl(snark_tell(ID,Assert)),assert(snark_cl(ID,Assert)).

fmtl(X):- as_symlog(X,XX), fmt(XX).

write_list([F|R]) :- write(F), write('.'), nl, write_list(R).
write_list([]).

numbervars_with_names(Term):-
   term_variables(Term,Vars),
   name_variables(Vars),!,
   numbervars(Vars),!.

name_variables([]).
name_variables([Var|Vars]):-
   (var_property(Var, name(Name)) -> Var = '$VAR'(Name) ; true),
   name_variables(Vars).

wdmsgl(CNF):-  CNF=..[NAME,NF],wdmsgl_2(NAME,NF).
wdmsgl_2(NAME,NF):-functor(NF,_,_),wdmsgl_3(NAME,'&',NF).

wdmsgl_3(NAME,F,NF):-copy_term(vv(NAME,F,NF),vv(NAME2,F2,NF2)),
   numbervars_with_names(vv(NAME2,F2,NF2)),!,
   wdmsgl_4(NAME2,F2,NF2).

wdmsgl_4(NAME,F,NF):- is_list(NF),!,maplist(wdmsgl_4(NAME,F),NF).
wdmsgl_4(NAME,F,NF):- compound(NF),NF=..[FF,A,B],FF=F,not_ftVar(A),not_ftVar(B),!,
  maplist(wdmsgl_4(NAME,F),[A,B]).
wdmsgl_4(NAME,_,NF):- as_symlog(NF,NF2), wdmsg(NAME=NF2).



put_singles(Wff,_Exists,[],Wff).
put_singles(Wff,Exists,[S|Singles],NewWff):-
   Test=..[Exists,SO,_],   
   ((each_subterm(Wff,SubTerm),compound(SubTerm),SubTerm=Test,same_var(SO,S))
     -> WffM = Wff ; WffM =..[Exists,S,Wff]),
   put_singles(WffM,Exists,Singles,NewWff),!.
 
prenormalize(Wff,Wff):- !.
prenormalize(Wff,WffO):- 
 must_det_l((show_call(term_singletons(Wff,[],NS,[],Singles)),
  put_singles(Wff,'exists',Singles,WffM),put_singles(WffM,'all',NS,WffO))).

:-dynamic(function_corisponding_predicate/2).

get_pred( Pred,F):-get_functor( Pred,F).
is_function(Function):-compound(Function),get_functor(Function,F,A),is_function(Function,F,A).
is_function(_,F,_):- atom_concat(_Was,'Fn',F).
is_function(_,F,A):- A2 is A+1,current_predicate(F/A2), not(current_predicate(F/A)).

is_ftEquality(Term):-get_pred(Term,Pred),(mpred_prop( Pred,prologEquality);Pred==mudEquals).

function_to_predicate(Function,NewVar,Pred):-
  Function=..[F|ARGS],
  function_corisponding_predicate(F,P),
  Pred=..[P,NewVar|ARGS].
function_to_predicate(Function,NewVar,mudEquals(NewVar,Function)).

% defunctionalize(_,Wff,Wff):-!.   
defunctionalize(OP,Wff,WffO):- 
  each_subterm(Wff,SubTerm),
  compound(SubTerm),
  not(is_ftEquality(SubTerm)),
  arg(_,SubTerm,Function),is_function(Function),
  subst_eq(SubTerm,Function,NewVar,NewSubTerm),
  function_to_predicate(Function,NewVar,Pred),
  NEW =..[OP,Pred,NewSubTerm],
  subst_eq(Wff,SubTerm,NEW,NextWff),!,
  defunctionalize(OP,NextWff,WffO).
defunctionalize(_,Wff,Wff).   

  

%=----- normalize(+Wff,-NormalClauses) ------
% normalize_tn(WffIn,Out) :- normalize(dlimplies(asserted(WffIn),WffIn),'$VAR'('MT'),complete,Out).

normalize_tn(WffIn,Out) :- numbervars_with_names(WffIn),!,normalize(WffIn,'$VAR'('MT'),_,Out).
   normalize(WffIn,Out) :- numbervars_with_names(WffIn),!,normalize(WffIn,'$VAR'('MT'),_,Out).

normalize(WffIn,MT,Why,NormalClauses) :-
 ignore(Why=atom), 
  must_det_l((  
   prenormalize(WffIn,Wff),   
   %defunctionalize('dlimplies',Wff2,Wff),   
   wdmsgl(normalize(Wff)),
   nnf(MT,Wff,NNF),
   wdmsgl(nnf(NNF)),
   pnf(NNF,PNF),
   wdmsgl(pnf(PNF)),
   dnf(PNF,DNF),
   wdmsgl(dnf(DNF)),
   cf(MT,WffIn,PNF,NCFs),!,
   wdmsgl(cf(NCFs)),
   maplist(make_normal_asserts(MT,Why),NCFs,NormalClauses),
   wdmsgl(prolog(NormalClauses)))),!.

evidence(MT,nesc(_,_),mudEquals(X,Y),HeadOut):- !,subevidence(MT,possible,mudEquals(X,Y),HeadOut).
evidence(MT,nesc(_,_),asserted(X),HeadOut):- !,subevidence(MT,asserted,X,HeadOut).
evidence(MT,How,HeadIn,HeadOut):- subevidence(MT,How,HeadIn,HeadOut).

oper_term(proven,proven(B),proven(B)):-!.
oper_term(proven,T,T):-!.
oper_term(How,HeadIn,HeadOut):-append_term(How,HeadIn,HeadOut),!.

subevidence(_MT,nesc(How,Else),HeadIn,HeadOut):- (HeadIn=dlnot(Was)-> oper_term(Else,Was,HeadOut);oper_term(How,HeadIn,HeadOut)),!.
subevidence( MT,asserted,HeadIn,HeadOut):- HeadIn=dlnot(Was)-> HeadOut=asserted_false(MT,Was);HeadOut=asserted(MT,HeadIn).
subevidence( MT,possible,HeadIn,HeadOut):- HeadIn=dlnot(Was)-> HeadOut=missing(MT,Was);HeadOut=possible(MT,HeadIn).
subevidence( MT,believe,HeadIn,HeadOut):- HeadIn=dlnot(Was)-> HeadOut=missing(MT,Was);HeadOut=believe(MT,HeadIn).
subevidence(_MT,E2,Was,HeadOut):-append_term(E2,Was,HeadOut),!.

prepend_term(NewHead,Arg1,NewHeadM):-NewHead=..[F|ARGS],NewHeadM=..[F,Arg1|ARGS].

make_normal_asserts(MT,Why,cl([HeadIn],BodyIn),(OfHead:-OfBody)):-
  must_det_l(( 
   evidence(MT,nesc(proven_in(MT),impossible_in(MT)),HeadIn,Head),
   maplist(evidence(MT,nesc(proven,not_possible)),BodyIn,Body),
   subst_eq(Head,dlnot,impossible,NewHead),
   subst_eq(Body,dlnot,impossible,NewBody),
   sort(NewBody,NewBodyS),
   ((not_ftVar(Why),not(atom(Why)))->append_term(NewHead,Why,OfHead);NewHead=OfHead),
   OfBody =.. ['when',MT|NewBodyS])),!.


:- export(normalize/1).
normalize(A):- normalize(A,B),!,maplist(fmtl,B),!.
:- export(normalize_tn/1).
normalize_tn(A):- normalize_tn(A,B),!,maplist(fmtl,B),!.

:-normalize_tn(a(XX) & b(XX) => c(XX)).
:-normalize_tn(all(R,room(R) => exists(D, (door(D) & has(R,D))))).
/*
:-normalize(~p <=> ~q).
:-normalize(loves(R,motherFn(R))).
:-normalize(rains_tuesday => wear_rain_gear xor carry_umbrella).
:-normalize(exists( P, (person( P) & all(C, car(C) => ~has( P,C))))).

:-normalize(room(R) => exists(D, (door(D) & has(R,D)))).
:-normalize(all( P, person( P) => ~exists(D, dollar(D) & has( P,D)))).
:-normalize((goes(jane) xor goes(sandra) => goes(bill))).
:-normalize(exists( P, exists(C, (person( P) & car(C) & has( P,C))))).
:-normalize(~all( P,person( P) => exists(C, car(C) & has( P,C)))).
:-normalize((go(sam) & go(bill)) v (go(sally) & go(nancy))).
:-normalize(go(sam) & ( go(bill) v go(sally) ) & go(nancy)).
:-normalize(exists(C, course(C) & exists(MT1, midterm(C,MT1) & exists(MT2, midterm(C,MT2) & different(MT1,MT2))))).
:-normalize(exists(C, course(C) & ~exists(MT, midterm(C,MT)))).
:-prolog.
*/



