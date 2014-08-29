

:-export((term_listing/1)).
term_listing([]):-!.
term_listing(Match):-
   ignore((catch(listing(Match),_,fail))),
   doall((
      synth_clause_for(H,B),
      once(ok_pred(H)),
      once(use_term_listing(Match,H,B)),
      show_term_listing(H,B),
      fail)).

synth_clause_for(H,B):- cur_predicate(H,_),synth_clause_db(H,B).

synth_clause_db(H,info(Props)):- once(pred_info(H,Props)).
synth_clause_db(H,B):- predicate_property(M:H,number_of_clauses(_)),!,clause(M:H,B).

:-export((use_term_listing/3)).
use_term_listing(noinfo,_,info(_)):-!,fail. 
use_term_listing(HO,H,B):- atom(HO),!, use_term_listing_2(exact,HO,H,B).
use_term_listing([],_,_):-!.
use_term_listing([F1],H,B):-!,use_term_listing(F1,H,B),!.
use_term_listing([F1|FS],H,B):-!,use_term_listing(F1,H,B),!,use_term_listing(FS,H,B),!.
use_term_listing((F1,FS),H,B):-!,use_term_listing(F1,H,B),!,use_term_listing(FS,H,B),!.
use_term_listing((F1;FS),H,B):-!,use_term_listing(F1,H,B);use_term_listing(FS,H,B).
use_term_listing(arity(A),H,_):-!,functor(H,_,A).
use_term_listing(functor(F),H,_):-!,functor(H,F,_).
use_term_listing(not(C),H,B):-nonvar(C),!,not(use_term_listing(C,H,B)).
use_term_listing(-(C),H,B):-nonvar(C),!,not(use_term_listing(C,H,B)).
use_term_listing(+(C),H,B):-nonvar(C),!,(use_term_listing(C,H,B)).
use_term_listing(module(M),H,_):-!,predicate_property(H,imported_from(M)).
use_term_listing(M:HO,H,B):-!,use_term_listing(module(M),H,B),!,use_term_listing(h(HO),H,B).
use_term_listing(F/A,H,_):-atom(F),functor(H,F,A),!.
use_term_listing(h(P),H,_):-!,use_term_listing(P,H,666666).
use_term_listing(b(P),_,B):-!,use_term_listing(P,666666,B).
use_term_listing(HO,H,B):- string(HO),!, use_term_listing_2(contains,HO,H,B).
use_term_listing(contains(HO),H,B):-!, use_term_listing_2(contains,HO,H,B).
use_term_listing(HO,H,B):- !,use_term_listing_2(exact,HO,H,B).

use_term_listing_2(contains,HO,H,B):- any_to_string(HO,HS),!, with_output_to(string(H1B1),write_canonical((H:-B))), (sub_atom_icasechk(HS,_,H1B1);sub_atom_icasechk(H1B1,_,HS)),!.
use_term_listing_2(exact,HO,H,B):- contains_var(HO,(H:-B)).

use_term_listing(HO,(H:-B)):-!, synth_clause_db(H,B), use_term_listing(HO,H,B).

:-dynamic cur_predicates/1.
cur_predicate(M:P,M:F/A):-
   current_predicate(M:F/A),functor(P,F,A),not(predicate_property(user:P,imported_from(_))).


ok_pred(F/A):-!,functor(P,F,A),ok_pred(P),!.
ok_pred(P):-not(bad_pred(P)).

bad_pred(M:P):-!,atom(M),bad_pred(P). 
bad_pred(P):-functor(P,F,A),arg(_,v(cur_predicates/_,db_op/_,db_op00/_,db_op0/_,db_op_loop/_,do_expand_args_l/3),F/A).
bad_pred(P):-predicate_property(P,autoloaded(_)).
bad_pred(P):-not(predicate_property(P,number_of_clauses(_))).
bad_pred(P):-predicate_property(P,imported_from(_)),predicate_property(P,static).
bad_pred(P):-predicate_property(P,foreign).

pred_info(H,Props):- findall(PP,predicate_property_h(H,PP),Props1),/*findall(used_by(M),predicate_property(M:H,_),Props2),*/ ignore(Props2=[]),append(Props1,Props2,Props3),sort(Props3,Props).


show_term_listing(H,true):- !, show_term_listing(H).
show_term_listing(H,B):- show_term_listing((H:-B)).

show_term_listing(H):- not(not((snumbervars(H),writeq(H),write('.'),nl))),!.


