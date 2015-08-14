
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

set_clause_compile(TYPE):-op_alias((:-),TYPE).

boxlog_to_compile((AA),OUTPUT):- get_op_alias((:-),TYPE),!,boxlog_to_compile(TYPE,(AA),OUTPUT),!.

boxlog_to_compile(fwc,(not(AA):-_),true):- nonvar(AA),AA = skolem(_,_),!.
boxlog_to_compile(fwc,(not(AA):-B),BB=>neg(AA)):-body_for_pfc(AA,B,BB).
boxlog_to_compile(fwc,(AA:-B),BB=>AA):- body_for_pfc(AA,B,BB).
boxlog_to_compile(fwc,not(AA),neg(AA)):-  !.
boxlog_to_compile(fwc,(AA),(AA)):-  !.
boxlog_to_compile(TYPE,(AA:-BB),OUTPUT):- !,boxlog_to_compile2(TYPE,AA,BB,OUTPUT).
boxlog_to_compile(TYPE,not(AA),OUTPUT):-  !,boxlog_to_compile2(TYPE,not(AA),true,OUTPUT).
boxlog_to_compile(TYPE,(AA),OUTPUT):-     !,boxlog_to_compile2(TYPE,AA,true,OUTPUT).

boxlog_to_compile2(TYPE,not(AA),BB,(neg(AA):-OUTPUT)):-!,conjoin_maybe(TYPE,BB,OUTPUT).
boxlog_to_compile2(TYPE,AA,BB,((AA):-OUTPUT)):- conjoin_maybe(TYPE,BB,OUTPUT).
boxlog_to_compile2(TYPE,AA,BB,(AA:-OUTPUT)):- conjoin_maybe(TYPE,BB,OUTPUT).

body_conjoin({AA},{BB},{C}):-body_conjoin(AA,BB,C).
body_conjoin({AA},({BB},D),O):-body_conjoin(AA,BB,C),body_conjoin({C},D,O).
body_conjoin(AA,(BB,D),O):-body_conjoin(AA,BB,C),body_conjoin(C,D,O).
body_conjoin(AA,BB,C):-conjoin(AA,BB,C).
conjoin_maybe(_X,true,true):-!.
conjoin_maybe(TYPE,BB,OUTPUT):-conjoin(TYPE,BB,OUTPUT).


body_for_pfc(AA,B,(BB,{ground(AA)})):- body_for_pfc_0(B,BB).

body_for_pfc_0(A,A):-is_ftVar(A).
body_for_pfc_0((A,B), C):-!,body_for_pfc_0(A,AA),body_for_pfc_0(B,BB),body_conjoin(AA,BB,C).
body_for_pfc_0((A;B),(AA;BB)):-!,body_for_pfc_0(A,AA),body_for_pfc_0(B,BB).
body_for_pfc_0((A/B),(AA/BB)):-!,body_for_pfc_0(A,AA),body_for_pfc_0(B,BB).
body_for_pfc_0(skolem(In,Out),{(In=Out;when('nonvar'(In),ignore((In=Out))))}).
% body_for_pfc_0(skolem(In,Out),{when((?=(In,_);nonvar(In)),ignore(Out=In))}).
body_for_pfc_0(different(A,B),{dif:dif(A,B)}).
body_for_pfc_0(A,A).
