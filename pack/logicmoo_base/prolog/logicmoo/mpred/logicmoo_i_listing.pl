% =======================================================
/** <module> 
%
%= predicates to examine the state of pfc 
% interactively exploring Pfc justifications.
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% =======================================================


:- multifile
  	user:portray/1,
  	user:prolog_list_goal/1,
  	user:prolog_predicate_name/2,
  	user:prolog_clause_name/2.

:- dynamic
  	user:portray/1.


pfc_queue :- lsting(pfc_queue/2).



pppfc :-
  pp_facts,
  pp_rules,
  pp_triggers,
  pp_supports,
  pfc_queue.

%= pp_facts ...

pp_facts :- pp_facts(_,true).

pp_facts(Pattern) :- pp_facts(Pattern,true).

pp_facts(P,C) :-
  pfc_facts(P,C,L),
  pfc_classify_facts(L,User,Pfc,_Rule),
  draw_line,
  fmt("~N% User added facts:",[]),
  pp_items(Type,User),
  draw_line,
  draw_line,
  fmt("~N% Pfc added facts:",[]),
  pp_items(Type,Pfc),
  draw_line.

draw_line:- (thlocal:print_mode(H)->true;H=unknown),fmt("~N%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n",[]).


pp_items(Type,[]).
pp_items(Type,[H|T]) :-
  pp_item(Type,H),
  pp_items(Type,T).
pp_items(Type,H) :- pp_item(Type,H).

pfc_trace_item(M,H):- (thlocal:pfc_trace_exec-> pp_item(M,H); true).

pp_item(M,H):- flag(show_asserions_offered,X,X+1),thlocal:print_mode(html),!, (\+ \+ pp_item_html(M,H)),!.
pp_item(M,H):- true,(\+ \+ pp_item_html(M,H)),!.
pp_item(M,(H:-true)):-pp_item(M,H).
pp_item(M,spft(W,U,U)):-!,pp_item(M,U:W).
pp_item(M,spft(W,F,U)):- atom(U),!, fmt('~N%~n',[]),pp_item(M,U:W), fmt('~N% rule: ~q~n%~n', [F]),!.
pp_item(M,spft(W,F,U)):-          !,fmt('~N% ~w~n%d:       ~q~n%format:    ~q~n', [M,W,F]),pp_item(M,U).
pp_item(M,U:W):- !,sformat(S,'~w  ~w:',[M,U]),!, pp_item(S,W).
pp_item(M,nt(Trigger,Test,Body)) :- !, fmt('~N%~sn-trigger: ~q~n%test: ~q~n%body: ~q~n', [M,Trigger,Test,Body]).
pp_item(M,pt(F,Body)):-              !,fmt('~N%~sp-trigger: ~q~n~n%body:~n', [M,F]), portray_clause((F:-Body)).
pp_item(M,bt(F,Body)):-              !,fmt('~N%~sb-trigger: ~q~n%body: ~q~n', [M,F,Body]).
pp_item(M,H):- \+ \+ ((  fmt("~N%~s ~q~n",[M,H]))).

pfc_classify_facts([],[],[],[]).

pfc_classify_facts([H|T],User,Pfc,[H|Rule]) :-
  pfc_db_type(H,rule),
  !,
  pfc_classify_facts(T,User,Pfc,Rule).

pfc_classify_facts([H|T],[H|User],Pfc,Rule) :-
  pfc_get_support(H,(u,u)),
  !,
  pfc_classify_facts(T,User,Pfc,Rule).

pfc_classify_facts([H|T],User,[H|Pfc],Rule) :-
  pfc_classify_facts(T,User,Pfc,Rule).


print_db_items(T, I):- 
    draw_line, 
    fmt("~N% ~w ...~n",[T]),
    print_db_items(I),
    draw_line.

print_db_items(F/A):-number(A),!,functor(P,F,A),!,print_db_items(P).
print_db_items(I):- bagof(I,clause_u(I,true),R1),pp_items(Type,R1),!.
print_db_items(I):- listing(I),!,nl,nl.

pp_rules :-
   print_db_items("Forward Rules",(_=>_)),
   print_db_items("Bidirectional Rules",(_<=>_)), 
   print_db_items("Backchaining Rules",(_<=_)),
   print_db_items("Forward Facts",(=>(_))).

pp_triggers :-
     print_db_items("Positive triggers",pt(_,_)),
     print_db_items("Negative triggers", nt(_,_,_)),
     print_db_items("Goal triggers",bt(_,_)).

pp_supports :-
  % temporary hack.
  draw_line,
  fmt("~N% Supports ...~n",[]),
  setof((S > P), pfc_get_support(P,S),L),
  pp_items(Type,L),
  draw_line.



% ======================= pfc_file('pfcwhy').	% interactive exploration of justifications.

%   File   : pfcwhy.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated:
%   Purpose: predicates for interactively exploring Pfc justifications.

% ***** predicates for brousing justifications *****

:-dynamic(whymemory/2).
:-thread_local(thlocal:pfc_interactive_why).

:- use_module(library(lists)).

pfc_interactive_why:- with_assertions(thlocal:pfc_interactive_why,pfc_why).

pfc_why :-
  whymemory(P,_),
  pfc_why(P).

user:why(N) :- pfc_why(N).

pfc_interactive_why(N):- with_assertions(thlocal:pfc_interactive_why,pfc_why(N)).

pfc_why(N) :-
  number(N),
  !,
  whymemory(P,Js),
  pfc_why_command(N,P,Js).

pfc_why(P) :-
  justifications(P,Js),
  retractall_i(whymemory(_,_)),
  assert_i(whymemory(P,Js)),
  pfc_whyBrouse(P,Js).

pfc_why1(P) :-
  justifications(P,Js),
  pfc_whyBrouse(P,Js).

pfc_whyBrouse(P,Js) :-
  pp_justifications(P,Js),
  pfc_interactive_why -> ((
  pfc_ask(' >> ',Answer),
  pfc_why_command(Answer,P,Js))); true.

pfc_why_command(q,_,_) :- !.
pfc_why_command(h,_,_) :-
  !,
  fmt("~N%
Justification Browser Commands:
 q   quit.
 N   focus on Nth justification.
 N.M brouse step M of the Nth justification
 u   up a level
",
[]).

pfc_why_command(N,_P,Js) :-
  float(N),
  !,
  pfc_select_justificationNode(Js,N,Node),
  pfc_why1(Node).

pfc_why_command(u,_,_) :-
  % u=up
  !.

pfc_command(N,_,_) :-
  integer(N),
  !,
  fmt("~N% ~w is a yet unimplemented command.",[N]),
  fail.

pfc_command(X,_,_) :-
 fmt("~N% ~w is an unrecognized command, enter h. for help.",[X]),
 fail.

pp_why(P):- is_list(P),!,maplist(pp_why,P),!.

pp_why(P):-must((
  no_repeats(P,justifications(P,Js)),
      pp_justifications(P,Js))),!.

pp_justifications(P,Js) :-
  fmt("~N% Justifications for ~w:",[P]),
  must(pp_justification1(Js,1)).

pp_justification1([],_).

pp_justification1([J|Js],N) :-
  % show one justification and recurse.
  nl,
  pp_justifications2(J,N,1),
  N2 is N+1,
  pp_justification1(Js,N2).

pp_justifications2([],_,_).

pp_justifications2([C|Rest],JustNo,StepNo) :-
  =(C,CCopy),
  \+ \+ (( %numbervars(CCopy,0,_),
  fmt("~N%     ~w.~w ~w",[JustNo,StepNo,CCopy]))),
  StepNext is 1+StepNo,
  pp_justifications2(Rest,JustNo,StepNext).

pfc_ask(Msg,Ans) :-
  fmt("~N% ~w",[Msg]),
  read(Ans).

pfc_select_justificationNode(Js,Index,Step) :-
  JustNo is integer(Index),
  nth_pfc_call(JustNo,Js,Justification),
  StepNo is 1+ integer(Index*10 - JustNo*10),
  nth_pfc_call(StepNo,Justification,Step).

nth_pfc_call(N,List,Ele):-N2 is N+1,lists:nth0(N2,List,Ele).



show_pred_info(F/A):-integer(A),functor(H,F,A),!,show_pred_info(H).
show_pred_info(Head):-
        doall(show_call(no_repeats(isa(Head,_)))),
        functor(Head,F,_),
        doall(show_call(no_repeats(isa(F,_)))),
     (current_predicate(_,Head) -> show_pred_info_0(Head); wdmsg(cannot_show_pred_info(Head))),!.

show_pred_info_0(Head):- 
        doall(show_call(predicate_property(Head,_))),
        (has_cl(Head)->doall((show_call(clause(Head,_))));hotrace((lsting(Head)))),!.


% ===================================================
% Pretty Print Formula
% ===================================================


print_db_items(Title,Mask,What):-print_db_items(Title,Mask,Mask,What).
print_db_items(Title,Mask,SHOW,What0):-
     get_pi(Mask,H),get_pi(What0,What),
     format(atom(Showing),'~q for ~q...',[Title,What]),
     statistics(cputime,Now),Max is Now + 2,!,
       gripe_time(1.0,
         doall((once(statistics(cputime,NewNow)),NewNow<Max,clause_or_call(H,B),
             hotrace(pfc_contains_term(What,(H:-B))),
             flag(print_db_items,LI,LI+1),
             hotrace(pp_item(Showing,SHOW))))),
     pp_item(Showing,done),!.

pfc_contains_term(What,_):-is_ftVar(What),!.
pfc_contains_term(What,Inside):-compound(What),!,(\+ \+ ((copy_term_nat(Inside,Inside0),numbervars(Inside0),contains_term(What,Inside0)))),!.
pfc_contains_term(What,Inside):- (\+ \+ notrace((subst(Inside,What,foundZadooksy,Diff),Diff \=@= Inside ))),!.

% user:listing_mpred_hook(What):- debugOnError(pfc_listing(What)).

:-thread_local thlocal:pfc_listing_disabled.

pfc_listing(What):-thlocal:pfc_listing_disabled,!.
pfc_listing(What):-loop_check(pfc_listing_nlc(What)).

:-meta_predicate(pfc_listing_nlc(?)).

pfc_listing_nlc(M:What):-atom(M),!,M:pfc_listing(What).
pfc_listing_nlc(What):-loop_check(pfc_listing_0(What),true).

pfc_listing_0(What):-get_pi(What,PI),PI\=@=What,pfc_listing(PI).
pfc_listing_0(What):-nonvar(What),What=neg(Then),!, \+ \+ pfc_listing_1(Then), \+ \+ pfc_listing_1(What).
pfc_listing_0(What):- \+ \+  pfc_listing_1(neg(What)), \+ \+ pfc_listing_1(What).

pfc_listing_types('Triggers').
pfc_listing_types('Instances').
pfc_listing_types('Subclasses').
pfc_listing_types('ArgTypes').
pfc_listing_types('Arity').
pfc_listing_types('Forward').
pfc_listing_types('Bidirectional').
pfc_listing_types('Backchaining').
pfc_listing_types('Negative').
pfc_listing_types('Sources').
pfc_listing_types('Supports').
pfc_listing_types('Edits').

% print_db_items_and_neg(Title,Fact,What):-nonvar(Fact),Fact=neg(_),!,fail.
print_db_items_and_neg(Title,Fact,What):-print_db_items(Title,Fact,What).
print_db_items_and_neg(Title,Fact,What):-print_db_items(Title,neg(Fact),What).

pfc_listing_1(neg(What)):-var(What),!.
pfc_listing_1(neg(_What)):-!.
pfc_listing_1(What):-var(What),!.
pfc_listing_1(What):-
   print_db_items('Supports User',spft_precanonical(P,u,u),spft(P,u,u),What),
   print_db_items('Forward Facts',(=>(F)),F,What),
   print_db_items('Forward Rules',(_=>_),What),
 ignore((What\=neg(_),functor(What,IWhat,_),
   print_db_items_and_neg('Instance Of',isa(IWhat,_),IWhat),
   print_db_items_and_neg('Instances: ',isa(_,IWhat),IWhat),
   print_db_items_and_neg('Subclass Of',genls(IWhat,_),IWhat),
   print_db_items_and_neg('Subclasses: ',genls(_,IWhat),IWhat))),
   print_db_items('PFC Watches', pfcMark(_,_,_,_),What),
   print_db_items('Triggers Negative', nt(_,_,_),What),
   print_db_items('Triggers Goal',bt(_,_),What),
   print_db_items('Triggers Positive',pt(_,_),What),
   print_db_items('Bidirectional Rules',(_<=>_),What), 
   dif(A,B),print_db_items('Supports Deduced',spft_precanonical(P,A,B),spft(P,A,B),What),
   dif(G,u),print_db_items('Supports Nonuser',spft_precanonical(P,G,G),spft(P,G,G),What),
   print_db_items('Backchaining Rules',(_<=_),What),
   % print_db_items('Edits',is_disabled_clause(_),What),
   print_db_items('Edits',is_edited_clause(_,_,_),What),
   print_db_items('Instances',isa(_,_),What),
   print_db_items('Subclasses',genls(_,_),What),
   print_db_items('Negative Facts',neg(_),What),

   print_db_items('ArgTypes',argGenls(_,_,_),What),
   print_db_items('ArgTypes',argIsa(_,_,_),What),
   print_db_items('ArgTypes',argQuotedIsa(_,_,_),What),
   print_db_items('ArgTypes',meta_argtypes(_),What),
   print_db_items('ArgTypes',predicate_property(G,meta_predicate(G)),What),
   print_db_items('ArgTypes',resultGenls(_,_),What),
   print_db_items('ArgTypes',resultIsa(_,_),What),
   print_db_items('Arity',arity(_,_),What),
   print_db_items('Arity',current_predicate(_),What),
   print_db_items('MetaFacts Predicate',predicate_property(_,_),What),
   print_db_items('Sources',module_property(_,_),What),
   print_db_items('Sources',mpred_module(_,_),What),
   print_db_items('Sources',source_file(_,_),What),
   print_db_items('Sources',_:man_index(_,_,_,_,_),What),
   print_db_items('Sources',_:'$pldoc'(_,_,_,_),What),
   print_db_items('Sources',_:'$pred_option'(_,_,_,_),What),
   print_db_items('Sources',_:'$mode'(_,_),What),
   !.     



hide_source_meta.


this_listing(M:F/A):-functor(H,F,A),predicate_property(M:H,number_of_causes(_)),!, forall(clause(M:H,Body),pp_i2tml((M:H :- Body))).
this_listing(M:F/A):-functor(H,F,A),predicate_property(H,number_of_causes(_)),!, forall(clause(H,Body),pp_i2tml((M:H :- Body))).
this_listing(M:F/A):-listing(M:F/A),!.
this_listing(MFA):-listing(MFA).

:-thread_local(pp_i2tml_saved_buffer/2).


pp_i2tml_saved(_,pp_i2tml_saved_buffer(_,_)):-!.
% pp_i2tml_saved(Obj,H):- \+ is_list(H),cyc:pterm_to_sterm(H,S),H\=@=S,!,pp_i2tml_saved(Obj,S).
pp_i2tml_saved(Obj,H):- assert_if_new(pp_i2tml_saved_buffer(Obj,H)),!.
pp_i2tml_saved_done(Obj):-
   findall(H,retract(pp_i2tml_saved_buffer(Obj,H)),List),predsort(head_functor_sort,List,Set),
  forall(member(S,Set),pp_i2tml(S)).

head_functor_sort(Result,H1,H2):- (var(H1);var(H2)),compare(Result,H1,H2),!.
head_functor_sort(Result,H1,H2):- once((get_functor(H1,F1,A1),get_functor(H2,F2,A2))),F1==F2,A1>0,A2>0,arg(1,H1,E1),arg(1,H2,E2),compare(Result,E1,E2),!.
head_functor_sort(Result,H1,H2):- once((get_functor(H1,F1,_),get_functor(H2,F2,_))),F1\==F2,compare(Result,F1,F2),!.
head_functor_sort(Result,H1,H2):-compare(Result,H1,H2),!.

i2tml_hbr(Wall,Obj,H,B,_Ref):- B == true,!,pp_i2tml_r(Wall,Obj,H).
i2tml_hbr(Wall,Obj,H,B,_Ref):- pp_i2tml_r(Wall,Obj,(H:-B)).

pp_i2tml_r(PWall,Obj,H):-
   statistics(walltime,[Wall,_]),
   (((30+ PWall ) > Wall) -> throw(times_up(Obj));pp_i2tml_r(H)).

pp_i2tml_r(USER:HB):-USER==user,!,pp_i2tml_r(HB),!.
pp_i2tml_r((USER:H :- B)):-USER==user,!,pp_i2tml_r((H:-B)),!.
pp_i2tml_r((H :- B)):-B==true,!,pp_i2tml_r((H)),!.
pp_i2tml_r(HB):-pp_i2tml(HB),!.

:-thread_local(thlocal:pp_i2tml_hook/1).

   
:-thread_local(shown_subtype/1).
:-thread_local(shown_clause/1).

section_open(Type):-  once(shown_subtype(Type)->true;((thlocal:print_mode(html)->format('~n</pre><hr>~w<hr><pre>~n<font face="verdana,arial,sans-serif">',[Type]);(draw_line,format('% ~w~n%~n',[Type]))),asserta(shown_subtype(Type)))),!.
section_close(Type):- shown_subtype(Type)->(retractall(shown_subtype(Type)),(thlocal:print_mode(html)->format('</font>\n</pre><hr/><pre>',[]);draw_line));true.

pp_item_html(Type,done):-!,section_close(Type),!.
pp_item_html(_,H):-shown_clause(H),!.
pp_item_html(Type,H):- \+ thlocal:print_mode(html), pp_item_html_now(Type,H),!.
pp_item_html(Type,H):- ignore((flag(matched_assertions,X,X),between(0,5000,X),pp_item_html_now(Type,H))).

pp_item_html_now(Type,H):-    
   flag(matched_assertions,X,X+1),!,
   pp_item_html_if_in_range(Type,H),!,
   assert(shown_clause(H)),!.

pp_item_html_if_in_range(Type,H):- section_open(Type),!,pp_i2tml(H),!,nl.


pp_i2tml(T):-isVarProlog(T),getVarAtom(T,N),format('~w',[N]).
pp_i2tml(done):-!.
pp_i2tml(T):-string(T),format('"~w"',[T]).
pp_i2tml((H:-B)):-B==true, !, pp_i2tml(H).
pp_i2tml(was_chain_rule(H)):- pp_i2tml(H).
pp_i2tml(is_edited_clause(H,B,A)):- pp_i2tml(proplst([(clause)=H,before=B,after=A])).
pp_i2tml(is_disabled_clause(H)):- pp_i2tml((disabled)=H).

pp_i2tml('$was_imported_kb_content$'(_,_)):- hide_source_meta,!.
pp_i2tml(pfcMark(_,_,_,_)):- hide_source_meta,!.

pp_i2tml(FET):-fully_expand(assert,FET,NEWFET),FET\=@=NEWFET,!,pp_i2tml(NEWFET).
pp_i2tml(spft(P,U,U)):- nonvar(U),!, pp_i2tml(P:-asserted_by(U)).
pp_i2tml(spft(P,F,T)):- atom(F),atom(T),!, pp_i2tml(P:-asserted_in(F:T)).
pp_i2tml(spft(P,F,T)):- atom(T),!,  pp_i2tml(((P):-  T:'t-deduced',F)). 
pp_i2tml(spft(P,F,T)):- atom(F),!,  pp_i2tml(((P):-  F:'f-deduced',T)). 
pp_i2tml(spft(P,F,T)):- !, pp_i2tml((P:- ( 'deduced-from'=F,  (rule_why = T)))).
pp_i2tml(nt(Trigger,Test,Body)) :- !, pp_i2tml(proplst(['n-trigger'=Trigger , format=Test  ,  (body = (Body))])).
pp_i2tml(pt(Trigger,Body)):-      pp_i2tml(proplst(['p-trigger'=Trigger , ( body = Body)])).
pp_i2tml(bt(Trigger,Body)):-      pp_i2tml(proplst(['b-trigger'=Trigger ,  ( body = Body)])).
pp_i2tml(proplst([N=V|Val])):- is_list(Val),!, pp_i2tml(N:-([clause=V|Val])).
pp_i2tml(proplst(Val)):-!, pp_i2tml(:-(proplst(Val))).


pp_i2tml(C):- ((\+ \+ thlocal:pp_i2tml_hook(C))),!.
pp_i2tml(C):- ((\+ \+ (if_defined(rok_portray_clause(C),portray_clause(C))))),!.


indent_nbsp(X):-thlocal:print_mode(html),forall(between(0,X,_),format('&nbsp;')),!.
indent_nbsp(X):-forall(between(0,X,_),format('~t',[])),!.

indent_nl:- fresh_line, flag(indent,X,X), indent_nbsp(X).


indent_nbsp(0,''):-!.
indent_nbsp(1,'\n         '):-!.
indent_nbsp(X,Chars):-XX is X -1,!, indent_nbsp(XX,OutP),!,sformat(Chars,'~w   ',[OutP]),!.



