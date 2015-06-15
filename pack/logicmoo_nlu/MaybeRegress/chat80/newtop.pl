/*

 _________________________________________________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/



% Chat-80 : A small subset of English for database querying.

/* Control loop */

:-export(hi/0).
hi :-
   hi(user).

:-export(hi/1).
hi(File):- hi(report,File).

:-export(hi/2).
hi(Callback,File):-
    absolute_file_name(File,FOpen),
    call_cleanup((
     open(FOpen,read, Fd, [alias(FOpen)]),
      see(Fd),
    repeat,
      ask80(Fd,P),
      control80(Callback,P), !),end(FOpen)).

ask80(user,P) :- !,
   prompt(_,'Question: '),
   read_sent(P).

ask80(_F,P) :- read_sent(P), doing(P,0),!.

doing([],_) :- !,nl.
doing([X|L],N0) :-
   out(X),  
   advance(X,N0,N),
   doing(L,N),!.

out(nb(X)) :- !,
   write(X).
out(A) :-
   write(A).

advance(X,N0,N) :-
   uses(X,K),
   M is N0+K,
 ( M>72, !,
      nl,
      N is 0;
   N is M+1,
      put(" ")).

uses(nb(X),N) :- !,
   chars(X,N).
uses(X,N) :-
   chars(X,N).

chars(X,N) :- atomic(X), !,
   name(X,L),
   length(L,N).
chars(_,2).

end(user) :- !.
end(F) :- 
   told,seen,
   catch(close(F),_,seen),!.


:-dynamic_multifile_exported(control80/1).
control80(U):-with_assertions(thlocal:tracing80,control80(report,U)).

:-dynamic_multifile_exported(control80/2).

control80(Callback,NotList):-not(is_list(NotList)),to_word_list(NotList,List),NotList \=@= List,!,maplist(string_to_atom,List,ListIn),
   control80(Callback,ListIn).

control80(Callback,[bye,'.']) :- !,
   call(Callback,"Goodbye",'control80',!,call),
   display('Cheerio.'),nl.

control80(Callback,[trace,'.']) :- !,
   assert(thlocal:tracing80),
   call(Callback,assert(thlocal:tracing80),'thlocal:tracing80',true,boolean),
   display('Tracing from now on!'), nl, fail.

control80(Callback,[do,not,trace,'.']) :-
   retract(thlocal:tracing80), !,
   call(Callback,retract(thlocal:tracing80),'thlocal:tracing80',false,boolean),
   display('No longer thlocal:tracing80.'), nl, fail.

control80(Callback,U) :- with_assertions(thlocal:tracing80, call_in_banner(U,(ignore(process_run(Callback,U,_List,_Time))))),fail.
   
:-export(chat80/1).
chat80(U):-
 with_assertions(tracing80,
           with_assertions(thlocal:chat80_interactive,
            with_no_assertions(thlocal:useOnlyExternalDBs,
             with_no_assertions(thglobal:use_cyc_database,
              ignore(control80(U)))))).

:-export(test_chat80/1).
test_chat80(U):-
 with_assertions(tracing80,
           with_assertions(thlocal:chat80_interactive,
            with_no_assertions(thlocal:useOnlyExternalDBs,
             with_no_assertions(thglobal:use_cyc_database,
              ignore(control80(U)))))).
   


get_prev_run_results(U,List,Time):-must_test_801(U,List,Time),!.
get_prev_run_results(U,List,Time):-must_test_80(U,List,Time),!.
get_prev_run_results(_,[],[]).


reportDif(_U,List,BList,_Time,_BTime):-forall(member(N=V,List),ignore((member(N=BV,BList),not(BV = V), format('~n1) ~q = ~q ~~n2) ~q = ~q ~n',[N,V,N,BV])))).

:-dynamic_multifile_exported(process_run_diff/4).
process_run_diff(Callback,U,BList,BTime):-
 call_in_banner(U,( process_run(Callback,U,List,Time),
   ignore((reportDif(U,List,BList,Time,BTime))))),!.
   

:-dynamic_multifile_exported(process_run/4).   
process_run(Callback,U,List,Time):-
  runtime(StartParse),   
  process_run(Callback,StartParse,U,List,Time),!.

:-export(call_in_banner/2).
call_in_banner(U,Call):- p2(begin:U),call_cleanup(Call,p2(end:U)).

process_run(Callback,StartParse,U,List,Time):-    
    process_run_real(Callback,StartParse,U,List,Time) *-> true; process_run_unreal(Callback,StartParse,U,List,Time).
    
   
process_run_unreal(Callback,Start,U,[sent=(U),parse=(E),sem=(error),qplan=(error),answers=(failed)],[time(WholeTime)]):-   
         runtime(Stop),WholeTime is Stop-Start,
         display(Callback - 'Failed after '-WholeTime-' to understand: '+ [sent=(U),parse=(E),sem=(error),qplan=(error),answers=(failed)] ), nl.

if_try(Cond,DoAll):-Cond,!,DoAll.
if_try(_,_):-sleep(1),!.

p3:-format('~n% =========================================================================================================~n',[]).
p2(begin:Term):-!, p3, p4(Term),p1.
p2(end:Term):-!, p1, p4(Term),p3.
p2(Term):- p3, p4(Term), p3.
p1:-format('~n% ---------------------------------------------------------------------------------------------------~n',[]).
p4(Term):-format('~n%                       ~q~n',[Term]).

words_to_w2(U,W2):-var(U),must(W2=U).
words_to_w2([],W2):-must(W2=[]).
words_to_w2(U,W2):-not(is_list(U)),to_word_list(U,List),U \=@= List,!,words_to_w2(List,W2).

words_to_w2(U,W2):-not(compound(U)),must(W2=U).
words_to_w2([W|WL],[W2|W2L]):-w_to_w2(W,W2),words_to_w2(WL,W2L).


sent_to_parsed(U,E):- deepen_pos(sentence(E,U,[],[],[])).

:-export(deepen_pos/1).
:-meta_predicate(deepen_pos(0)).
deepen_pos(Call):- one_must(deepen_pos_0(Call),with_assertions(thlocal:useAltPOS,deepen_pos_0(Call))).
:-export(deepen_pos_0/1).
:-meta_predicate(deepen_pos_0(0)).
deepen_pos_0(Call):-one_must(Call,with_assertions(thlocal:usePlTalk,Call)).


% any_to_string("How many countries are there?",X),splt_words(X,Y,Z),vars_to_ucase(Y,Z),maplist(call,Z)

:-dynamic_multifile_exported(process_run_real/5).
process_run_real(Callback,StartParse,UIn,[sent=(U),parse=(E),sem=(S),qplan=(QP),answers=(Results)],[time(WholeTime)]) :-
   flag(sentenceTrial,_,0),
   ignore((var(Callback),Callback=report)),
   words_to_w2(UIn,U),!,
   call(Callback,U,'Sentence'(Callback),0,expr),
   ignore((var(StartParse),runtime(StartParse))),!,
   (if_try(nonvar(U),sent_to_parsed(U,E)) *-> ignore((U\==UIn,call(Callback,U,'POS Sentence'(Callback),0,expr))); (call(Callback,U,'Rewire Sentence'(Callback),0,expr),!,fail)),
   (flag(sentenceTrial,TZ,TZ), TZ>5 -> (!) ; true),
   once((
   runtime(StopParse),
   ParseTime is StopParse - StartParse,
   call(Callback,E,'Parse',ParseTime,portray),  
   (flag(sentenceTrial,TZ2,TZ2+1), TZ2>5 -> (!,fail) ; true),
   runtime(StartSem))),
   once((if_try(nonvar(E),deepen_pos(logic(E,S))))),
   runtime(StopSem),
   SemTime is StopSem - StartSem,
   call(Callback,S,'Semantics',SemTime,expr),
   runtime(StartPlan),
   once(if_try(nonvar(S),deepen_pos(qplan(S,S1)))),
   copy_term80(S1,QP),
   runtime(StopPlan),
   TimePlan is StopPlan - StartPlan,
   if_try(S\=S1,call(Callback,S1,'Planning',TimePlan,expr)),
   runtime(StartAns),
   nonvar(S1),once(deepen_pos(answer80(S1,Results))), Results\=[],!,
   runtime(StopAns),
   TimeAns is StopAns - StartAns,
   call(Callback,Results,'Reply',TimeAns,expr),
   WholeTime is ParseTime + SemTime + TimePlan + TimeAns,
   p1.


:-dynamic_multifile_exported(test_quiet/4).
test_quiet(_,_,_,_).

:-dynamic_multifile_exported(report/4).
report(Item,Label,Time,Mode) :- thlocal:tracing80, !,
   nl, write(Label), write(': '), write(Time), write('sec.'), nl,
   safely_call_ro(report_item(Mode,Item)),!.
report(_,_,_,_).


call_with_limits(Copy):- (A=_,call_with_limits0(((call(Copy),A=completed))), (A==completed -> true; (trace,Copy))).
call_with_limits0(Copy):-call_with_time_limit(10,call_with_depth_limit(call_with_inference_limit(Copy,10000,_),500,_)).

safely_call_ro(Call):-copy_term(Call,Copy),catchvv(call_with_limits(Copy),E,(dmsg(error_safely_call(E,in,Copy)),!,fail)).
safely_call(Call):-copy_term(Call,Copy),catchvv((Copy,Call=Copy),E,(dmsg(error_safely_call(E,in,Copy)),!,fail)).

report_item(none,_).
report_item(_,Var):-var(Var),!,write('FAILED'),nl.
report_item(portray,Item) :-
   portray_clause((Item:-Item)), nl.
report_item(expr,Item) :-
   write_tree(Item), nl.
report_item(tree,Item) :-
   print_tree(Item), nl.

runtime(Time) :- statistics(runtime,[MSec,_]), Time is MSec/1000,!.

quote(A&R) :-
   atom(A), !,
   quote_amp(R).
quote(_-_).
quote(_--_).
quote(_+_).
quote(verb(_,_,_,_,_,_Kind)).
quote(wh(_)).
quote(nameOf(_)).
quote(prep(_)).
quote(det(_)).
quote(quant(_,_)).
quote(int_det(_)).

quote_amp('$VAR'(_)) :- !.
quote_amp(R) :-
   quote(R).

logic(S0,S) :-
   i_sentence(S0,S1),
   clausify80(S1,S2),
   once(simplify(S2,S)),!.

logic(S0,S) :- 
  thlocal:chat80_interactive,plt,
   must((i_sentence(S0,S1),
   clausify80(S1,S2),
   simplify(S2,S))),!.

simplify(C,C0):-var(C),dmsg(var_simplify(C,C0)),!,fail.
simplify(C,(P:-R)) :- !,
   unequalise(C,(P:-Q)),
   simplify(Q,R,true).

simplify(C,C0,C1):-var(C),dmsg(var_simplify(C,C0,C1)),fail.
simplify(C,C,R):-var(C),!,R=C.
simplify(setof(X,P0,S),R,R0) :- !,
   simplify(P0,P,true),
   revand(R0,setof(X,P,S),R).
simplify((P,Q),R,R0) :-
   simplify(Q,R1,R0),
   simplify(P,R,R1).
simplify(true,R,R) :- !.
simplify(X^P0,R,R0) :- !,
   simplify(P0,P,true),
   revand(R0,X^P,R).
simplify(numberof(X,P0,Y),R,R0) :- !,
   simplify(P0,P,true),
   revand(R0,numberof(X,P,Y),R).
simplify(\+P0,R,R0) :- !,
   simplify(P0,P1,true),
   simplify_not(P1,P),
   revand(R0,P,R).
simplify(P,R,R0) :-
   revand(R0,P,R).

simplify_not(\+P,P) :- !.
simplify_not(P,\+P).

revand(true,P,P) :- !.
revand(P,true,P) :- !.
revand(P,Q,(Q,P)).

unequalise(C0,C) :-
   numbervars80(C0,1,N),
   functor(V,v,N),
   functor(M,v,N),
   inv_map_enter(C0,V,M,C).

inv_map_enter(C0,V,M,C):- catch(inv_map(C0,V,M,C),too_deep(Why),(dmsg(Why),dtrace(inv_map(C0,V,M,C)))).

inv_map(Var,V,M,T) :- stack_depth(X), X> 400, throw(too_deep(inv_map(Var,V,M,T))).
inv_map(Var,V,M,T) :- stack_check(500), var(Var),dmsg(var_inv_map(Var,V,M,T)),!,Var==T.
inv_map('$VAR'(I),V,_,X) :- !,
   arg(I,V,X).
inv_map(A=B,V,M,T) :- !,
   drop_eq(A,B,V,M,T).
inv_map(X^P0,V,M,P) :- !,
   inv_map(P0,V,M,P1),
   exquant(X,V,M,P1,P).
inv_map(A,_,_,A) :- atomic(A), !.
inv_map(T,V,M,R) :-
   functor(T,F,K),
   functor(R,F,K),
   inv_map_list(K,T,V,M,R).

inv_map_list(0,_,_,_,_) :- !.
inv_map_list(K0,T,V,M,R) :-
   arg(K0,T,A),
   arg(K0,R,B),
   inv_map(A,V,M,B),
   K is K0-1,
   inv_map_list(K,T,V,M,R).

drop_eq('$VAR'(I),'$VAR'(J),V,M,true) :- !,
 ( I=\=J, !,
      irev(I,J,K,L), 
      arg(K,M,L),
      arg(K,V,X),
      arg(L,V,X);
   true).
drop_eq('$VAR'(I),T,V,M,true) :- !,
   arg(I,V,T),
   arg(I,M,0).
drop_eq(T,'$VAR'(I),V,M,true) :- !,
   arg(I,V,T),
   arg(I,M,0).
drop_eq(X,Y,_,_,X=Y).

exquant('$VAR'(I),V,M,P0,P) :-
   arg(I,M,U),
 ( var(U), !,
      arg(I,V,X),
       P=(X^P0);
   P=P0).

irev(I,J,I,J) :- I>J, !.
irev(I,J,J,I).

