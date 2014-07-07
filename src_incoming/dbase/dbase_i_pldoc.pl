
/*
term_listing(Obj):- catch(listing(Obj),_,fail),fail.
term_listing(Obj):-
   doall((
   predicate_property(H,number_of_clauses(_)),
   clause(H,B),
   use_term_listing(Obj,((H:-B))),
   show_term_listing(H,B),
   fail)).


use_term_listing(Obj,HB):- once((subst(HB,Obj,fov,H1B1), H1B1 \= HB)),!.
use_term_listing(Obj,HB):- term_to_atom(Obj,HO), term_to_atom(HB,HBO), sub_atom_icasechk(HBO,_,HO),!.


show_term_listing(H,true):- !, show_term_listing(H).
show_term_listing(H,B):- show_term_listing((H:-B)).

show_term_listing(H):- writeq(H),write('.'),nl,!.
*/

:-export((term_listing/1)).
term_listing([]):-!.
term_listing([]):-!,dtrace.
term_listing(P):-term_listing(P,[]).
term_listing(M:P,List):-!,term_listing(P,[module(M)|List]).
term_listing(F/A,List):-atom(F),integer(A),!,functor(P,F,A),term_listing(P,[functor(F),arity(A)|List]).
term_listing(Atom,List):-atomic(Atom), any_to_atom(Atom,C),term_listing_0(C,[contains(C)|List]).
term_listing(P,List):-not(is_list(P)),!,compound(P),term_listing_0(P,List).
term_listing(P,List):-is_list(P),!,append(P,List,Append),term_listing_0(_,Append).
term_listing(P,List):-term_listing_0(P,List).

term_listing_0(Atom,[]):-!,term_listing_0(Atom,[contains(Atom)]).
term_listing_0(Atom,UList):-
   ignore((catch(listing(Atom),_,fail))),
   doall(((
      synth_clause_for(Atom,H,B),
      once(use_term_listing(UList,H,B)),
      show_term_listing(H,B)))).

synth_clause_for(C,C,B):-compound(C),!,synth_clause_db(C,B).
synth_clause_for(_Atom,H,B):- cur_predicates(List),!,member(H,List),synth_clause_db(H,B).

synth_clause_db(H,info(Props)):- pred_info(H,Props).
synth_clause_db(H,B):-once((predicate_property_h(H,number_of_clauses(_)),ok_pred(H))),clause(H,B).
% synth_clause_db(H,database_req):-req(H).

:-export((use_term_listing/3)).
use_term_listing([],_,_):-!,fail.
use_term_listing([F1],H,B):-!,use_term_listing_1(F1,H,B),!.
use_term_listing([F1|FS],H,B):-!,use_term_listing_1(F1,H,B),!,use_term_listing(FS,H,B),!.
use_term_listing(F1,H,B):-use_term_listing_1(F1,H,B),!.

use_term_listing_1(noinfo,_,info(_)):-!,fail. 
use_term_listing_1(arity(A),H,_):-!,functor(H,_,A).
use_term_listing_1(functor(F),H,_):-!,functor(H,F,_).
use_term_listing_1(not(C),H,B):-nonvar(C),!,not(use_term_listing_1(C,H,B)).
use_term_listing_1(module(_M),_H,_B):-!.
use_term_listing_1(contains(HO),H,B):- !, use_term_listing_2((HO),H,B).
use_term_listing_1((HO),H,B):- atom(HO),!, use_term_listing_2((HO),H,B).
use_term_listing_1(_,_,_):-dtrace.

use_term_listing_2((HO),H,B):- not(compound(HO)),!, with_output_to(string(H1B1),write_canonical((H:-B))), (sub_atom_icasechk(HO,_,H1B1);sub_atom_icasechk(H1B1,_,HO)),!.
use_term_listing_2((HO),H,B):- not(not((( subst((H:-B),HO,fov,H1B1), H1B1 \= (H:-B))))),!.

use_term_listing(HO,HO):- synth_clause_db(H,B), use_term_listing(HO,H,B).

:-dynamic cur_predicates/1.
cur_predicates(List):-setof(P,(cur_predicate(P),ok_pred(P)),List),asserta((cur_predicates(List):-!)),!.
%cur_predicate(P):-current_predicate(_:F/A),functor(P,F,A).
cur_predicate(P):-current_predicate(F/A),nonvar(F),F\=':',functor(P,F,A).
%cur_predicate(P):-predicate_property_h(P,_).

predicate_property_h(M:H,P):-atom(M),!,nonvar(H),predicate_property_h(H,P).
predicate_property_h(H,P):- predicate_property(H,P).

ok_pred(P):-not(bad_pred(P)).
bad_pred(var):-!.
bad_pred(_:_):-!.
bad_pred(P):-functor(P,F,A),arg(_,v(cur_predicates/_,db_op/_,db_op00/_,db_op0/_,db_op_loop/_,do_expand_args_l/3),F/A).
bad_pred(P):-predicate_property(P,autoloaded(_)).
bad_pred(P):-not(predicate_property(P,number_of_clauses(_))).
bad_pred(P):-predicate_property(P,imported_from(_)),predicate_property(P,static).
bad_pred(P):-predicate_property(P,foreign).

pred_info(H,Props):- findall(PP,predicate_property_h(H,PP),Props1),/*findall(used_by(M),predicate_property(M:H,_),Props2),*/ ignore(Props2=[]),append(Props1,Props2,Props3),sort(Props3,Props).


show_term_listing(H,true):- !, show_term_listing(H).
show_term_listing(H,B):- show_term_listing((H:-B)).

show_term_listing(H):- not(not((snumbervars(H),writeq(H),write('.'),nl))),!.


