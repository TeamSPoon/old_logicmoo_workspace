
term_listing(P):-term_listing(P,[info]).
term_listing(M:P,List):-!,term_listing(P,[module(M)|List]).
term_listing(F/A,List):-atom(F),integer(A),!,functor(P,F,A),term_listing(P,[functor(F),arity(A)|List]).
term_listing(Atom,List):-atomic(Atom), any_to_atom(Atom,C),term_listing_0(C,[contains(C)|List]).
term_listing(P,List):-not(is_list(P)),!,compound(P),term_listing_0(P,List).
term_listing(P,List):-is_list(P),!,append(P,List,Append),term_listing_0(_,Append).
term_listing(P,List):-term_listing_0(P,List).

term_listing_0(Atom,UList):-
   ignore((catch(listing(Atom),_,fail))),
   doall(((
   synth_clause(Atom,H,B),
   use_term_listing(UList,H,B),
   show_term_listing(H,B),
   fail))).

synth_clause_comp(H,H,true):-req(H).
synth_clause_comp(H,H,info(Props)):-pred_info(H,Props).

synth_clause(C,H,B):-compound(C),!,synth_clause_comp(C,H,B).
synth_clause(_Atom,H,B):- cur_predicates(List),member(H,List),synth_clause(H,B).

use_term_listing([],_,_):-!.
use_term_listing([F1],H,B):-use_term_listing_1(F1,H,B),!.
use_term_listing([F1|FS],H,B):-use_term_listing_1(F1,H,B),!,use_term_listing(FS,H,B).
use_term_listing(F1,H,B):-use_term_listing_1(F1,H,B),!.

use_term_listing_1(noinfo,_,info(_)):-!,fail. 
use_term_listing_1(arity(A),H,_):-!,functor(H,_,A).
use_term_listing_1(functor(F),H,_):-!,functor(H,F,_).
use_term_listing_1(not(C),H,B):-nonvar(C),!,not(use_term_listing_1(C,H,B)).
use_term_listing_1(module(_M),_H,_B):-!.
use_term_listing_1(contains(HO),H,B):- !, use_term_listing_2((HO),H,B).
use_term_listing_1((HO),H,B):- atom(HO),!, use_term_listing_2((HO),H,B).
use_term_listing_1([F1|FS],H,B):-!,use_term_listing_1(F1,H,B),!,use_term_listing(FS,H,B).
use_term_listing_1(_,_,_).

use_term_listing_2((HO),H,B):- not(compound(HO)),!, with_output_to(string(H1B1),write_canonical((H:-B))), (sub_atom_icasechk(HO,_,H1B1);sub_atom_icasechk(H1B1,_,HO)),!.
use_term_listing_2((HO),H,B):-!, not(not(((subst((H:-B),HO,fov,H1B1), H1B1 \= (H:-B))))),!.

use_term_listing(HO,HO):- synth_clause(H,B), use_term_listing(HO,H,B).

cur_predicates(List):-setof(P,cur_predicate(P),List).
cur_predicate(P):-current_predicate(_:F/A),functor(P,F,A).
cur_predicate(P):-current_predicate(F/A),functor(P,F,A).
cur_predicate(P):-predicate_property(P,_).
cur_predicate(P):-predicate_property(_:P,_).

predicate_property_h(H,P):- setof(P,(predicate_property(H,P),predicate_property(_:H,P)),PP),member(P,PP).

pred_info(H,Props):- findall(PP,predicate_property_h(H,PP),Props1),findall(used_by(M),predicate_property(M:H,_),Props2),append(Props1,Props2,Props3),sort(Props3,Props).
synth_clause(H,info(Props)):- pred_info(H,Props).
synth_clause(H,B):-predicate_property_h(H,number_of_clauses(_)),clause(H,B).


show_term_listing(H,true):- !, show_term_listing(H).
show_term_listing(H,B):- show_term_listing((H:-B)).

show_term_listing(H):- writeq(H),write('.'),nl,!.


