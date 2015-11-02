
/*

modes of compile for (H:-B) are 
 cwc,  = prolog's default (static or dynamic depending on built_in status)
 fwc,  = pfc(forward)chain  B==>-
 bwc,  = pfc(backwards)memoization 
 pttp, = pttp iterative deepening
 dra,  = dynamic reording of alternatives (Solid tabling)
 other(_) Extendable

File mode interpretation (LHS=>RHS) are 
 pl    = Prolog (thus no re-interpretation)
 pfc   = Prolog Forward Chaining
 clif  = Common Logic Interchange Format
 cycl  = CycL 
 kif   = Knowledge Interchange Format
 chr   = CHR mode
 
additional features that may be added/manipulated in the body
 ctx 
 lin = argument linearization  via  linearize(lin,(H:-(G,B)),(HH:-BB),[],_,true,G).
 no_repeats/2 (on head vars)
 no_repeats/2 (on body vars)
 body reording
 proof recording
 variable lifespans are "depth minimized"
 unbound argument type constaining typing on
     entry
     exit

Per-Litteral features
 t(Pred,Arg1,Arg2,Arg3,...)
 t(Pred,Arg1,Arg2,Arg3,...,+Ctx)
 i_c(isa,inst,col)
 c_c(genls,col1,col2)
 p_p(genlPreds,inst,col)
 p_c_c(relationalAllExists...)
 p_c_u(relationalAllInstance...)
 p_c_c(relationalAllAll...)  

 u = any
 i = instances like MTs and people
 s = strings
 g = formulas
 l = lists
 n = numbers
 z = specials like 
 p = preds
 c = collections

 asserted_
 impossible_
 beliefs_

*/

% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_boxlog.pl
:- module(common_logic_boxlog,
          [ avoidHeadLoop/2,
            body_for_mpred_1/5,
            body_for_mpred_2/5,
            body_for_pfc/5,
            boxlog_to_compile/2,
            boxlog_to_compile/3,
            boxlog_to_compile2/4,
            can_use_hack/1,
            conjoin_body/3,
            conjoin_maybe/3,
            correct_mode/3,
            did_use_hack/1,
            get_op_alias_compile/2,
            get_reln/2,
            is_unit/2,
            is_unit/0,
            is_unit/3,
            is_unit/4,
            is_units_b/1,
            is_units_h/1,
            make_must_ground/3,
            make_vg/4,
            overlaping/3,
            overlapingFunctors/2,
            reduce_literal/2,
            set_clause_compile/1,
            vg/1,
            vg/3]).

:- use_module(logicmoo(mpred/mpred_pfc)).
:- include('../mpred/mpred_header.pi').
:- common_logic_boxlog:use_module(library(dialect/hprolog),[]).

is_units_h(A):-maplist(is_unit,A).
is_units_b(A):-maplist(is_unit,A).
% is_unit(A):-is_unit(A).
is_unit.
is_unit(A,B):-is_unit(A),is_unit(B).
is_unit(A,B,C):-is_unit(A),is_unit(B),is_unit(C).
is_unit(A,B,C,D):-is_unit(A),is_unit(B),is_unit(C),is_unit(D).

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

make_vg([],Shared,[],{(S)}):-  S=..[is_unit|Shared],!.
make_vg(_,Shared,_,{(S)}):-  S=..[is_unit|Shared],!.
make_vg(B,S,H,{(CB,CS,CH)}):- CB=..[is_units_b,B],CS=..[is_unit|S],CH=..[is_units_h,H].

set_clause_compile(TYPE):-op_alias((:-),TYPE).

get_op_alias_compile(I,O):-get_op_alias(I,O),( I== (:-)),( O\== (:-)),!.
get_op_alias_compile(_,fwc).

boxlog_to_compile(H,OUTPUT):- get_op_alias_compile((:-),TYPE),!,must((boxlog_to_compile(TYPE,H,OUTPUTM))),!,OUTPUTM=OUTPUT.

boxlog_to_compile(_,(H:-(Cwc,B)),(H:-(Cwc,B))):- Cwc == cwc,!.
boxlog_to_compile(Mode,(H:-(Cwc,B)),(H:-(Cwc,B))):- Mode == Cwc,!.
boxlog_to_compile(cwc,H,OUTPUT):-!, boxlog_to_compile((:-),H,OUTPUT).
boxlog_to_compile(==>,H,OUTPUT):-!, boxlog_to_compile(fwc,H,OUTPUT).
boxlog_to_compile(=>,H,OUTPUT):-!, boxlog_to_compile(fwc,H,OUTPUT).
boxlog_to_compile(<=,H,OUTPUT):-!, boxlog_to_compile(fwc,H,OUTPUT).
boxlog_to_compile(<-,H,OUTPUT):-!, boxlog_to_compile(bwc,H,OUTPUT).
boxlog_to_compile(rev(==>),H,OUTPUT):-!, boxlog_to_compile(fwc,H,OUTPUT).
boxlog_to_compile(rev(=>),H,OUTPUT):-!, boxlog_to_compile(fwc,H,OUTPUT).
boxlog_to_compile(~(WHAT),(not(H):-B),OUTPUT):-!, boxlog_to_compile(WHAT,(not(H):-B),OUTPUT).
boxlog_to_compile(~(WHAT),not(H),OUTPUT):-!, boxlog_to_compile(WHAT,not(H),OUTPUT).

boxlog_to_compile((:-),(not(H):-B),unused_true((not(H):-B))):- nonvar(H),prologBuiltin(H),!.
boxlog_to_compile((:-),(not(H):-B),(HH:-(cwc,BBB))):-body_for_pfc((:-),~(H),HH,B,BB),make_must_ground(HH,BB,MMG),conjoin_body(BB,MMG,BBB).
boxlog_to_compile((:-),(H:-B),OUT):-pfcControlled(H),boxlog_to_compile((bwc),(H:-B),OUT),!.
boxlog_to_compile((:-),(H:-B),(HH:-(cwc,BBB))):- body_for_pfc((:-),H,HH,B,BB),make_must_ground(HH,BB,MMG),conjoin_body(BB,MMG,BBB).
boxlog_to_compile((:-),not(H),~(H)):-  !.
boxlog_to_compile((:-),H,H):-  !.

:-
 op(1199,fx,('==>')), 
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-').

:- op(1100,fx,(shared_multifile)).


boxlog_to_compile(fwc,(not(H):-B),unused_true((not(H):-B))):- nonvar(H),H = skolem(_,_),!.
boxlog_to_compile(fwc,(not(H):-B),OUT):- term_slots(H,HV),term_slots(B,BV), HV\==BV,!,boxlog_to_compile(bwc,(not(H):-B),OUT).
boxlog_to_compile(fwc,(not(H):-B),(BBB==>HH)):- body_for_pfc(fwc,~(H),HH,B,BB),make_must_ground(HH,BB,MMG),conjoin_body(BB,MMG,BBB).
boxlog_to_compile(fwc,(H:-B),(BBB==>HH)):- body_for_pfc(fwc,H,HH,B,BB),make_must_ground(HH,BB,MMG),conjoin_body(BB,MMG,BBB).
boxlog_to_compile(fwc,not(H),~(H)):-  !.
boxlog_to_compile(fwc,H,H):-  !.

boxlog_to_compile(bwc,(not(H):-B),unused_true((not(H):-B))):- nonvar(H),H = skolem(_,_),!.
boxlog_to_compile(bwc,(not(H):-B),(HH<-BBB)):-body_for_pfc(<-,~(H),HH,B,BB),make_must_ground(HH,BB,MMG),conjoin_body(BB,MMG,BBB).
boxlog_to_compile(bwc,(H:-B),OUT):-pfcRHS(H),term_slots(H,HV),term_slots(B,BV), HV==BV,boxlog_to_compile((fwc),(H:-B),OUT),!.
boxlog_to_compile(bwc,(H:-B),(HH<-BBB)):- body_for_pfc(<-,H,HH,B,BB),make_must_ground(HH,BB,MMG),conjoin_body(BB,MMG,BBB).
boxlog_to_compile(bwc,not(H),~(H)):-  !.
boxlog_to_compile(bwc,H,H):-  !.

boxlog_to_compile(TYPE,(H:-BB),OUTPUT):- !,boxlog_to_compile2(TYPE,H,BB,OUTPUT).
boxlog_to_compile(TYPE,not(H),OUTPUT):-  !,boxlog_to_compile2(TYPE,not(H),true,OUTPUT).
boxlog_to_compile(TYPE,H,OUTPUT):-     !,boxlog_to_compile2(TYPE,H,true,OUTPUT).

boxlog_to_compile2(TYPE,not(H),BB,(~(H):-OUTPUT)):-!,conjoin_maybe(TYPE,BB,OUTPUT).
boxlog_to_compile2(TYPE,H,BB,(H:-OUTPUT)):- conjoin_maybe(TYPE,BB,OUTPUT).
boxlog_to_compile2(TYPE,H,BB,(H:-OUTPUT)):- conjoin_maybe(TYPE,BB,OUTPUT).

conjoin_body({H},{BB},{C}):-conjoin_body(H,BB,C).
conjoin_body({H},({BB},D),O):-conjoin_body(H,BB,C),conjoin_body({C},D,O).
conjoin_body(H,(BB,D),O):-conjoin_body(H,BB,C),conjoin_body(C,D,O).
conjoin_body(H,BB,C):-conjoin(H,BB,C).
conjoin_maybe(_X,true,true):-!.
conjoin_maybe(TYPE,BB,OUTPUT):-conjoin(TYPE,BB,OUTPUT).

correct_mode(_,O,O):-var(O),!.
correct_mode((:-),{M},O):-!,correct_mode((:-),M,O).
correct_mode(Mode,(A,B),O):-!,correct_mode(Mode,A,AA),correct_mode(Mode,B,BB),conjoin_body(AA,BB,O).
correct_mode(_,O,O).

body_for_pfc(Mode,Head,NewNewHead,I,O):-reduce_literal(Head,NewHead),!,body_for_mpred_1(Mode,NewHead,NewNewHead,I,O),!.
body_for_pfc(Mode,Head,NewHead,B,BB):- body_for_mpred_1(Mode,Head,NewHead,B,BB),!.

body_for_mpred_1(Mode,Head,HeadO,C,CO):- (Mode ==(:-);Mode==(cwc);Mode==(<-)),
    overlaping(C,Head,Avoid),
    body_for_mpred_1(Mode,Head,HeadM,zzAvoidHeadLoop,AA),
    body_for_mpred_2(Mode,HeadM,HeadO,C,BB),!,
    subst(AA,zzAvoidHeadLoop,{Avoid},AAA),
       conjoin_body(AAA,BB,CM),correct_mode(Mode,CM,CO).
body_for_mpred_1(Mode,Head,NewNewHead,I,O):-body_for_mpred_2(Mode,Head,NewNewHead,I,M),correct_mode(Mode,M,O).


overlaping(C,Head,Avoid):- (is_ftVar(C);is_ftVar(Head)),!,Avoid=avoidHeadLoop(C,Head).
overlaping(~(C),Head,Avoid):-is_ftNonvar(C),!,overlaping(C,Head,Avoid).
overlaping(C,~(Head),Avoid):-is_ftNonvar(Head),!,overlaping(C,Head,Avoid).
overlaping(C,Head,Avoid):-is_ftNonvar(Head),is_ftNonvar(C), compound(C),compound(Head),once((get_reln(C,FC),get_reln(Head,HC))),!,overlapingFunctors(FC,HC),!,Avoid=avoidHeadLoop(C,Head).

overlapingFunctors(FC,HC):- (\+ \+ FC=HC),!.
overlapingFunctors(t,_):-!.
overlapingFunctors(_,t):-!.

get_reln(C,F):-var(C),!,F=_.
get_reln(C,F):-is_ftVar(C),!,F=_.
get_reln(~(C),RO):-nonvar(C),!,get_reln(C,RO).
get_reln(not(C),RO):-nonvar(C),!,get_reln(C,RO).
get_reln(\+(C),RO):-nonvar(C),!,get_reln(C,RO).
get_reln('{}'(C),RO):-nonvar(C),!,get_reln(C,RO).
get_reln(C,RO):-get_functor(C,F),
  (F==t->
     (arg(1,C,R),(is_ftVar(R)->RO=t;RO=R));
     RO=F),!.

avoidHeadLoop(C,Head):- stack_check, ground(C),(C\=Head),\+ is_loop_checked(C).

body_for_mpred_2(_Mode,Head,Head,A,A):-is_ftVar(A).
body_for_mpred_2(Mode,Head,HeadO,(A,B), C):-!,body_for_mpred_1(Mode,Head,HeadM,A,AA),body_for_pfc(Mode,HeadM,HeadO,B,BB),conjoin_body(AA,BB,C).
body_for_mpred_2(Mode,Head,HeadO,(A;B),(AA;BB)):-!,body_for_mpred_1(Mode,Head,HeadM,A,AA),body_for_pfc(Mode,HeadM,HeadO,B,BB).
body_for_mpred_2((:-),Head,HeadO,(A/B),(AA,BB)):-!,body_for_mpred_1(Mode,Head,HeadM,A,AA),body_for_pfc(Mode,HeadM,HeadO,B,BB).
body_for_mpred_2(Mode,Head,HeadO,(A/B),(AA/BB)):-!,body_for_mpred_1(Mode,Head,HeadM,A,AA),body_for_pfc(Mode,HeadM,HeadO,B,BB).


body_for_mpred_2((fwc),H,(if_missing(H,HH)),{skolem(In,Out)},true):-contains_var(In,H),subst(H,In,Out,HH),!.
body_for_mpred_2((fwc),H,(if_missing(H,HH)),skolem(In,Out),true):-contains_var(In,H),subst(H,In,Out,HH),!.
body_for_mpred_2(_Mode,~(Head),~(Head),skolem(_,_),true).
%body_for_mpred_2(Mode,H,H,skolem(_,_),true).
body_for_mpred_2(_Mode,Head,Head,skolem(In,Out),{ignore(In=Out)}).
body_for_mpred_2(_Mode,Head,Head,poss(X),{loop_check(\+ ~(X),true)}).
% body_for_mpred_2(Mode,Head,Head,skolem(In,Out),{(In=Out;when('nonvar'(In),ignore((In=Out))))}).
% body_for_mpred_2(Mode,Head,Head,skolem(In,Out),{when((?=(In,_);nonvar(In)),ignore(Out=In))}).
body_for_mpred_2(Mode,Head,NewHead,B,BBB):- once(reduce_literal(B,BB)),B\=@=BB,!,body_for_mpred_1(Mode,Head,NewHead,BB,BBB).
body_for_mpred_2(_Mode,Head,Head,not(A),~(A)):-!.
body_for_mpred_2(_Mode,Head,Head,different(A,B),{dif:dif(A,B)}).
body_for_mpred_2(_Mode,Head,Head,A,{A}):-prologBuiltin(A),!.
body_for_mpred_2(_Mode,Head,Head,A,A).

reduce_literal(A,A):-is_ftVar(A).
reduce_literal(~A,~A):-is_ftVar(A).
reduce_literal(~(different(P3, R3)),not_different(P3, R3)).
reduce_literal(~(mudEquals(P3, R3)),different(P3, R3)).
reduce_literal(~(skolem(P3, R3)),different(P3, R3)).
reduce_literal(~(termOfUnit(P3, R3)),different(P3, R3)).
reduce_literal(~(equals(P3, R3)),different(P3, R3)).
reduce_literal(termOfUnit(P3, R3),skolem(P3, R3)).
reduce_literal(~({A}),AA):- reduce_literal(~A,AA), AA \=@= ~A,!.
reduce_literal(~(A),~ A):-!.
reduce_literal(A,A).

can_use_hack(two_implications):-!,fail.
can_use_hack(_).
did_use_hack(X):-wdmsg(did_use_hack(X)).

