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
          tsn/0,
          op(300,fx,'~'),
          op(600,xfx,'=>'),
          op(650,xfx,'<=>'),
          op(350,xfx,'xor'),
          op(400,yfx,'&'),  
          op(500,yfx,'v')
        ]). 

:-multifile(use_snark_expansion/0).
:-thread_local(use_snark_expansion/0).
%user:term_expansion(H ,CE):-snark_term_expansion(H,CE),((H)\=@=CE,dmsg(snark_term_expansion(H,CE))).
%user:goal_expansion(H ,CE):-snark_goal_expansion(H,CE),((H)\=@=CE,dmsg(snark_goal_expansion(H,CE))).


use_snark_term(B):- use_snark_expansion;snark_hook(B);snark_pred_head(B).

snark_head_expansion(B,CE):- use_snark_term(B),snark_term_expansion(B,CE).

:-export(snark_clause_expansion/3).
snark_clause_expansion(H,B,OUT):- snark_head_expansion(H,HH),
  must(snark_goal_expansion(B,BB)),!,OUT=(HH:-BB).

snark_term_expansion(H,H):-not(compound(H)),!.
snark_term_expansion(M:H,M:HH):-!,snark_goal_expansion(H,HH).
snark_term_expansion((H:-B),CE):- (use_snark_expansion;snark_hook(H)), !,snark_clause_expansion(H,B,CE),((H:-B)\=@=CE,dmsg(snark_clause_expansion(H,B,CE))).
snark_term_expansion((:-B),(:-CE)):- !,snark_goal_expansion(B,CE).
snark_term_expansion([CA|RGS],CEARGS):-!,maplist(snark_arg_expansion,[CA|RGS],CEARGS).
snark_term_expansion(B,CE):-snark_goal_expansion(B,CE).

snark_arg_expansion(H,H):-not(compound(H)),!.
snark_arg_expansion(H,H):-!.

get_meta_args(C,MP):- predicate_property(C,meta_predicate(MP)),!.
get_meta_args(C,MP):- C=..[F|ARGS],functor(C,F,A),get_meta_args(C,F,A,ARGS,MPARGS),MP=..[F|MPARGS].

get_meta_args(_,module,_,_ARGS,_MPARGS):-!,fail.
get_meta_args(_,_,A,_ARGS,MPARGS):- A>2,!,length(MPARGS,A).

meta_term_expansion(_,B,B):-not(compound(B)),!.
meta_term_expansion(M,B,C):-member(M,[+,-,?]),!,snark_arg_expansion(B,C),!.
% [^,:] 
meta_term_expansion(_,B,C):-arg(1,B,IsVar),is_ftVar(IsVar),snark_arg_expansion(B,C),!.
meta_term_expansion(M,B,C):-number(M),!,snark_goal_expansion(B,C),!.
meta_term_expansion(_,B,C):-snark_term_expansion(B,C),!.

snark_goal_expansion(H,H):-not(compound(H)),!.
snark_goal_expansion(not(B),not(CE)):- !,snark_goal_expansion(B,CE).
snark_goal_expansion(M:(B),M:(CE)):- !,snark_goal_expansion(B,CE).
snark_goal_expansion('@'(B , M),'@'(CE , M)):- !,snark_goal_expansion(B,CE).
snark_goal_expansion((H , B),(HH , BB)):-!,snark_goal_expansion(H,HH),snark_goal_expansion(B,BB).
snark_goal_expansion((H ; B),(HH ; BB)):-!,snark_goal_expansion(H,HH),snark_goal_expansion(B,BB).
snark_goal_expansion((H -> B ; C),(HH -> BB ; CC)):-!,snark_goal_expansion(H,HH),snark_goal_expansion(B,BB),snark_goal_expansion(C,CC).
snark_goal_expansion(C,CEO):- C =..[F|ARGS],
   snark_goal_args_expansion(C,F,ARGS,CEO),!.
   

snark_goal_args_expansion(C,F,ARGS,CEO):- get_meta_args(C,MP),!,MP=..[F|MARGS],maplist(meta_term_expansion,MARGS,ARGS,CEARGS),!,CEO=..[F|CEARGS].
snark_goal_args_expansion(_,F,ARGS,CEO):- maplist(snark_arg_expansion,ARGS,CEARGS),
                         CE=..[F|CEARGS],
                          logical_pos_maybe(F,CE,CEO).


each_subterm00(B, A):- A=B;(compound(B), (functor(B,A,_);((arg(_, B, C), each_subterm00(C, A))))).


:-meta_predicate(gshow_call(0)).
:-export(gshow_call/1).


add_mi(_Caller,G,O):- prolog_load_context(source,File),
   source_location(File,_),trace,source_location(File,_),
   not(notrace((each_subterm00(G,ST),atom(ST),member(ST,[show_call,show_gcall])))), get_functor(G,F,A),!,use_gcall(F,A),!,
    not(dont_use_gcall(F,A)),!, O=gshow_call(G),!. % dmsg(expanded(G)).

logical_pos_maybe(_,A,A):-!.

dont_use_gcall(dont_use_gcall,2).
dont_use_gcall(gshow_call,1).
dont_use_gcall(show_call,1).
dont_use_gcall(_,1).
dont_use_gcall(F,A):-functor(G,F,A),predicate_property(G,meta_predicate(_)).
dont_use_gcall(goal_expansion,2).
use_gcall(_,2).
gshow_call(C):-show_call_failure(C).

user:goal_expansion(true,writeq(true)):-!.
%user:goal_expansion(V,V):-not(compound(V)),!.
%user:goal_expansion(G,O):-source_file(G,File),add_mi(file(File),G,O).



