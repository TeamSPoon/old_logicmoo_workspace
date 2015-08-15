
/*

modes of compile for (H:-B) are 
 fwc,  = pfc(forward)chain
 bwc,  = pfc(backwards)memoization
 cwc,  = prolog (static or dynamic depending on built_in status)
 pttp, = pttp iterative deepening
 dra,  = dynamic reording of alternatives (Solid tabling)

additional features that may be added/manipulated in the body
 ctx
 lin = argument linearization  via  linearize(lin,(H:-(G,B)),(HH:-BB),[],_,true,G).
 no_repeats (on head vars)
 no_repeats (on body vars)
 body reording
 unbound argument type containing
 argument typing on exit

*/
:- use_module(library(dialect/hprolog),[]).

% might trace down when it is not
% vg(G):-var(G),!,fail.
vg(G):- (ground(G); ( compound(G), \+ (arg(_,G,E),var(E)))),!.
vg(_,B,C):-vg(B),vg(C).
make_must_ground(H,BB,VG):-
   term_slots(H,HVs),
   term_slots(BB,BBVs),
   hprolog:intersect_eq(HVs,BBVs,Shared),
   hprolog:list_difference_eq(HVs,BBVs,UHVs),
   hprolog:list_difference_eq(BBVs,HVs,UBBVs),
   make_vg(UBBVs,Shared,UHVs,VG),!.

make_vg([],Shared,[],{vg(S)}):-  S=..[s|Shared],!.
make_vg(_,Shared,_,{vg(S)}):-  S=..[s|Shared],!.
make_vg(B,S,H,{vg(CB,CS,CH)}):- CB=..[b|B],CS=..[s|S],CH=..[h|H].

set_clause_compile(TYPE):-op_alias((:-),TYPE).

boxlog_to_compile(H,OUTPUT):- get_op_alias((:-),TYPE),!,boxlog_to_compile(TYPE,H,OUTPUTM),!,OUTPUTM=OUTPUT.

boxlog_to_compile(_,(H:-(Cwc,B)),(H:-(Cwc,B))):-Cwc == cwc,!.
boxlog_to_compile(cwc,H,OUTPUT):-!, boxlog_to_compile((:-),H,OUTPUT).
boxlog_to_compile(rev(=>),H,OUTPUT):-!, boxlog_to_compile(fwc,H,OUTPUT).
boxlog_to_compile(neg(<=),not(H),OUTPUT):-!, boxlog_to_compile(bwc,not(H),OUTPUT).

boxlog_to_compile((:-),(not(H):-_),true):- nonvar(H),prologBuiltin(H),!.
boxlog_to_compile((:-),(not(H):-B),HH:-(cwc,BBB)):-body_for_pfc(neg(H),HH,B,BB),make_must_ground(HH,BB,MMG),conjoin_body(BB,MMG,BBB).
boxlog_to_compile((:-),(H:-B),HH:-(cwc,BBB)):- body_for_pfc(H,HH,B,BB),make_must_ground(HH,BB,MMG),conjoin_body(BB,MMG,BBB).
boxlog_to_compile((:-),not(H),neg(H)):-  !.
boxlog_to_compile((:-),H,H):-  !.


boxlog_to_compile(fwc,(not(H):-_),true):- nonvar(H),H = skolem(_,_),!.
boxlog_to_compile(fwc,(not(H):-B),OUT):- term_slots(H,HV),term_slots(B,BV), HV\==BV,!,boxlog_to_compile(bwc,(not(H):-B),OUT).
boxlog_to_compile(fwc,(not(H):-B),BB=>(MMG,HH)):- body_for_pfc(neg(H),HH,B,BB),make_must_ground(HH,BB,MMG).
boxlog_to_compile(fwc,(H:-B),BB=>(MMG,HH)):- body_for_pfc(H,HH,B,BB),make_must_ground(HH,BB,MMG).
boxlog_to_compile(fwc,not(H),neg(H)):-  !.
boxlog_to_compile(fwc,H,H):-  !.

boxlog_to_compile(bwc,(not(H):-_),true):- nonvar(H),H = skolem(_,_),!.
boxlog_to_compile(bwc,(not(H):-B),(HH<=BBB)):-body_for_pfc(neg(H),HH,B,BB),make_must_ground(HH,BB,MMG),conjoin_body(BB,MMG,BBB).
boxlog_to_compile(bwc,(H:-B),(HH<=BBB)):- body_for_pfc(H,HH,B,BB),make_must_ground(HH,BB,MMG),conjoin_body(BB,MMG,BBB).
boxlog_to_compile(bwc,not(H),neg(H)):-  !.
boxlog_to_compile(bwc,H,H):-  !.

boxlog_to_compile(TYPE,(H:-BB),OUTPUT):- !,boxlog_to_compile2(TYPE,H,BB,OUTPUT).
boxlog_to_compile(TYPE,not(H),OUTPUT):-  !,boxlog_to_compile2(TYPE,not(H),true,OUTPUT).
boxlog_to_compile(TYPE,H,OUTPUT):-     !,boxlog_to_compile2(TYPE,H,true,OUTPUT).

boxlog_to_compile2(TYPE,not(H),BB,(neg(H):-OUTPUT)):-!,conjoin_maybe(TYPE,BB,OUTPUT).
boxlog_to_compile2(TYPE,H,BB,(H:-OUTPUT)):- conjoin_maybe(TYPE,BB,OUTPUT).
boxlog_to_compile2(TYPE,H,BB,(H:-OUTPUT)):- conjoin_maybe(TYPE,BB,OUTPUT).

conjoin_body({H},{BB},{C}):-conjoin_body(H,BB,C).
conjoin_body({H},({BB},D),O):-conjoin_body(H,BB,C),conjoin_body({C},D,O).
conjoin_body(H,(BB,D),O):-conjoin_body(H,BB,C),conjoin_body(C,D,O).
conjoin_body(H,BB,C):-conjoin(H,BB,C).
conjoin_maybe(_X,true,true):-!.
conjoin_maybe(TYPE,BB,OUTPUT):-conjoin(TYPE,BB,OUTPUT).


body_for_pfc(Head,NewHead,B,BB):- body_for_pfc_0(Head,NewHead,B,BB),!.

body_for_pfc_0(Head,Head,A,A):-is_ftVar(A).
body_for_pfc_0(Head,HeadO,(A,B), C):-!,body_for_pfc_0(Head,HeadM,A,AA),body_for_pfc_0(HeadM,HeadO,B,BB),conjoin_body(AA,BB,C).
body_for_pfc_0(Head,HeadO,(A;B),(AA;BB)):-!,body_for_pfc_0(Head,HeadM,A,AA),body_for_pfc_0(HeadM,HeadO,B,BB).
body_for_pfc_0(Head,HeadO,(A/B),(AA/BB)):-!,body_for_pfc_0(Head,HeadM,A,AA),body_for_pfc_0(HeadM,HeadO,B,BB).
body_for_pfc_0(H,(if_missing(H,HH)),skolem(In,Out),true):-contains_var(In,H),subst(H,In,Out,HH),!.
body_for_pfc_0(H,H,skolem(_,_),true).
body_for_pfc_0(neg(Head),neg(Head),skolem(_,_),true).
body_for_pfc_0(Head,Head,skolem(In,Out),{ignore(In=Out)}).
body_for_pfc_0(Head,Head,poss(X),{loop_check(\+ neg(X),true)}).
%body_for_pfc_0(Head,Head,skolem(In,Out),{(In=Out;when('nonvar'(In),ignore((In=Out))))}).
% body_for_pfc_0(Head,Head,skolem(In,Out),{when((?=(In,_);nonvar(In)),ignore(Out=In))}).
body_for_pfc_0(Head,Head,different(A,B),{dif:dif(A,B)}).
body_for_pfc_0(Head,Head,not(A),neg(A)):-!.
body_for_pfc_0(Head,Head,A,{A}):-prologBuiltin(A),!.
body_for_pfc_0(Head,Head,A,A).



can_use_hack(two_implications):-!,fail.
can_use_hack(_).
did_use_hack(X):-wdmsg(did_use_hack(X)).
