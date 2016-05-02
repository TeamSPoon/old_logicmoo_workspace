
:- file_begin(pfc).

:- set_fileAssertMt(baseKB).




prologSingleValued(C):-cwc,is_ftCompound(C),functor(C,F,_),!,prologSingleValued(F).

:-dynamic(baseKB:mpred_sv/2).
arity(mpred_sv,2).


% asserting mpred_sv(p,2) causes p/2 to be treated as a mpred_sv, i.e.
% if p(foo,1)) is a fact and we assert_db p(foo,2), then the forrmer assertion
% is retracted.
mpred_sv(Pred,Arity)==> prologSingleValued(Pred),arity(Pred,Arity),singleValuedInArg(Pred,Arity).

% prologSingleValued(Pred),arity(Pred,Arity) ==> hybrid_support(Pred,Arity).

% prologSingleValued(Pred),arity(Pred,Arity), \+ singleValuedInArg(Pred,_) ==> singleValuedInArg(Pred,Arity).
mdefault(((prologSingleValued(Pred),arity(Pred,Arity))==> singleValuedInArg(Pred,Arity))).



((singleValuedInArg(Pred,_))==>(prologSingleValued(Pred))).


singleValuedInArg(singleValuedInArg,2).



prologHybrid(singleValuedInArgDefault, 3).
prologHybrid(singleValuedInArgDefault(prologSingleValued,ftInt,ftTerm)).
((singleValuedInArgDefault(P, 2, V), arity(P,2), argIsa(P,1,Most)) ==> relationMostInstance(P,Most,V)).
(singleValuedInArgDefault(SingleValued,ArgN,S1)/ground(S1) ==> singleValuedInArg(SingleValued,ArgN)).
{FtInt=2},singleValuedInArgDefault(PrologSingleValued,FtInt,FtTerm),arity(PrologSingleValued,FtInt),
  argIsa(PrologSingleValued,1,Col)==>relationMostInstance(PrologSingleValued,Col,FtTerm).
((singleValuedInArgDefault(F, N, DEF)/is_ftNonvar(DEF), arity(F,A),
   {functor(P,F,A),replace_arg(P,N,DEF,Q),replace_arg(Q,N,WAS,R)})
       ==> mdefault( Q <- ({ground(P)},~R/nonvar(WAS)))).


((singleValuedInArgDefault(P, 2, V), arity(P,2), argIsa(P,1,Most)) <==> relationMostInstance(P,Most,V)).


:- if(lmconf:startup_option(datalog,sanity);lmconf:startup_option(clif,sanity)).

:- ensure_loaded(pack(logicmoo_base/t/examples/pfc/'sanity_sv.pfc')).

:- endif.



