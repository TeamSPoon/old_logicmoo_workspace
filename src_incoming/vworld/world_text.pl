

look_via_pred(_,_,[]).
look_via_pred(Pred,Trans,[L|List]):-!,
   catch((ignore(look_via_pred_0(Pred,Trans,L);dmsg(failed(look_via_pred_0(Pred,L))))),E,dmsg(error_failed(E,look_via_pred_0(Pred,L)))),
   look_via_pred(Pred,Trans,List).

look_via_pred_0(Pred,Trans,F=Call):- !,look_via_pred_1(Pred,Trans,F,Call).
look_via_pred_0(Pred,Trans,once(Call)):- !,functor(Call,F,_), look_via_pred_1(Pred,Trans,F,once(Call)).
look_via_pred_0(Pred,Trans,all(Call)):- !,functor(Call,F,_), look_via_pred_1(Pred,Trans,F,all(Call)).
look_via_pred_0(Pred,Trans,Call):- functor(Call,F,_), look_via_pred_1(Pred,Trans,F,Call).

look_via_pred_1(Pred,Trans,F,all(Call)):-!,look_via_pred_2(Pred,Trans,F,Call).
look_via_pred_1(Pred,Trans,F,once(Call)):-!,look_via_pred_2(Pred,Trans,F,once(Call)).
look_via_pred_1(Pred,Trans,F,Call):-look_via_pred_2(Pred,Trans,F,Call).

look_via_pred_2(Pred,Trans,F,Call0):-
      wsubst(Call0,value(Transform),value,Call),
      ignore( Transform = (Trans) ),
      wsubst(Call,value,NewValue,GCall),
      look_via_pred_3(Pred,Transform,F,GCall,NewValue).

look_via_pred_3(Pred,Transform,F,GCall,NewValue):-
  % dmsg(look_via_pred_3(Pred,Transform,F,GCall,NewValue)),
      doall((catch(call(GCall),Error, NewValue=Error), 
             fmt_call(Pred,Transform,F,NewValue))).


fmt_call(Pred,Transform,F,NewValue):-flatten([NewValue],ValueList), NewValue\=ValueList,fmt_call(Pred,Transform,F,ValueList).
fmt_call(Pred,Transform,N,[V]):-fmt_call_pred(Pred,Transform,N,V),!.
fmt_call(Pred,Transform,N,[V|VV]):-remove_dupes([V|VV],RVs),reverse(RVs,Vs),fmt_call_pred(Pred,Transform,N,Vs),!.
fmt_call(Pred,Transform,N,V):-fmt_call_pred(Pred,Transform,N,V),!.

fmt_call_pred(Pred,Transform,N,[L|List]):-!, doall((member(V,[L|List]),fmt_call_pred_trans(Pred,Transform,N,V))).
fmt_call_pred(Pred,Transform,N,V0):-fmt_call_pred_trans(Pred,Transform,N,V0).

fmt_call_pred_trans(Pred,Transform,N,V0):-must((debugOnError(call(Transform,V0,V)),!,debugOnError(call(Pred,N,V)))).



remove_dupes(In,Out):-remove_dupes(In,Out,[]).

remove_dupes([],[],_):-!.
remove_dupes([I|In],Out,Shown):-member(I,Shown),!,remove_dupes(In,Out,Shown).
remove_dupes([I|In],[I|Out],Shown):-remove_dupes(In,Out,[I|Shown]).


