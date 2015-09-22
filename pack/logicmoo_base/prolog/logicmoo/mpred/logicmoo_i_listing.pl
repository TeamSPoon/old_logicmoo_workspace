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

:- include(logicmoo_i_header).
:- style_check(-singleton).

:- multifile
  	user:portray/1,
  	user:prolog_list_goal/1,
  	user:prolog_predicate_name/2,
  	user:prolog_clause_name/2.

:- dynamic
  	user:portray/1.


pfc_queue :- listing(pfc_queue/2).



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
  fmt("User added facts:",[]),
  pp_items(Type,User),
  draw_line,
  draw_line,
  fmt("Pfc added facts:",[]),
  pp_items(Type,Pfc),
  draw_line.

draw_line:- (thlocal:print_mode(H)->true;H=unknown),fmt("~N%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n",[]).


pp_items(Type,[]).
pp_items(Type,[H|T]) :-
  ignore(pp_item(Type,H)),!,
  pp_items(Type,T).
pp_items(Type,H) :- ignore(pp_item(Type,H)).


pfc_trace_item(_,_):- tlbugger:ifHideTrace,!.
pfc_trace_item(M,H):- ignore(thlocal:pfc_trace_exec-> debugOnError(in_cmt(pp_item(M,H))); true).


   
pp_item(M,(H:-B)):- B ==true,pp_item(M,H).
pp_item(M,H):- flag(show_asserions_offered,X,X+1),thlocal:print_mode(html),!, (\+ \+ pp_item_html(M,H)),!.

pp_item(M,spftY(P,F,T,W)):-!,
   with_assertions(thlocal:current_why_source(W),pp_item(M,spft(P,F,T))).

pp_item(M,spft(W,U,U)):-!,pp_item(M,U:W).
pp_item(M,spft(W,F,U)):- atom(U),!,    fmt('~N%~n',[]),pp_item(M,U:W), fmt('rule: ~p~n~n', [F]),!.
pp_item(M,spft(W,F,U)):-          !,   fmt('~w~nd:       ~p~nformat:    ~p~n', [M,W,F]),pp_item(M,U).
pp_item(M,nt(Trigger,Test,Body)) :- !, fmt('~w n-trigger: ~p~ntest: ~p~nbody: ~p~n', [M,Trigger,Test,Body]).
pp_item(M,pt(F,Body)):-              !,fmt('~w p-trigger: ~p~n~nbody:~n', [M,F]), pp_i2tml_now((F:-Body)).
pp_item(M,bt(F,Body)):-              !,fmt('~w b-trigger: ~p~nbody: ~p~n', [M,F,Body]).

pp_item(M,U:W):- !,sformat(S,'~w  ~w:',[M,U]),!, pp_item(S,W).
pp_item(M,H):- \+ \+ (( get_clause_vars_for_print(H,HH),fmt("~w ~p~N",[M,HH]))).

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
    fmt("~w ...~n",[T]),
    print_db_items(I),
    draw_line.

print_db_items(F/A):-number(A),!,functor(P,F,A),!,print_db_items(P).
print_db_items(I):- bagof(I,clause_u(I,true),R1),pp_items(Type,R1),!.
print_db_items(I):- listing(I),!,nl,nl.

pp_rules :-
   print_db_items("Forward Rules",(_ ==> _)),
   print_db_items("Bidirectional Rules",(_ <==> _)), 
   print_db_items("implication Rules",(_ => _)),
   print_db_items("Bi-conditional Rules",(_ <=> _)),
   print_db_items("Backchaining Rules",(_ <- _)),
   print_db_items("Positive Facts",(nesc(_))),
   print_db_items("Negative Facts",(neg(_))).

pp_triggers :-
     print_db_items("Positive hideTriggers",pt(_,_)),
     print_db_items("Negative hideTriggers", nt(_,_,_)),
     print_db_items("Goal hideTriggers",bt(_,_)).

pp_supports :-
  % temporary hack.
  draw_line,
  fmt("Supports ...~n",[]),
  setof((S > P), pfc_get_support(P,S),L),
  pp_items(Type,L),
  draw_line.


loop_check_true(G):-loop_check(G,ignore(arg(1,G,[]))).

% ======================= pfc_file('pfcwhy').	% interactive exploration of justifications.

%   File   : pfcwhy.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated:
%   Purpose: predicates for interactively exploring Pfc justifications.

% ***** predicates for brousing justifications *****

:-dynamic(whymemory/2).
:-thread_local(thlocal:is_pfc_interactive_why/0).

:- use_module(library(lists)).

pfc_interactive_why:- with_assertions(thlocal:is_pfc_interactive_why,pfc_why).

pfc_why :-
  whymemory(P,_),
  pfc_why(P).

user:why(N) :- pfc_why(N).

pfc_interactive_why(N):- with_assertions(thlocal:is_pfc_interactive_why,pfc_why(N)).

pfc_why(N) :-
  number(N),
  !,
  whymemory(P,Js),
  pfc_why_command(N,P,Js).

pfc_why(P) :-
  no_repeats(justifications(P,Js)),
  retractall_i(whymemory(_,_)),
  assert_i(whymemory(P,Js)),!,
  pfc_whyBrouse(P,Js).

pfc_why1(P) :-
  justifications(P,Js),!,
  pfc_whyBrouse(P,Js).

pfc_whyBrouse(P,Js) :-
  pp_justifications(P,Js),!,
  (thlocal:is_pfc_interactive_why -> ((
  pfc_ask(' >> ',Answer),
  pfc_why_command(Answer,P,Js))); true).

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
  fmt("~w is a yet unimplemented command.",[N]),
  fail.

pfc_command(X,_,_) :-
 fmt("~w is an unrecognized command, enter h. for help.",[X]),
 fail.

pp_why(P):- is_list(P),!,maplist(pp_why,P),!.

pp_why(P):-must((
  no_repeats(P,justifications(P,Js)),
      pp_justifications(P,Js))),!.

pp_justifications(P,Js) :-
  fmt("Justifications for ~w:",[P]),
  must(pp_justification1(Js,1)).

pp_justification1([],_).

pp_justification1([J|Js],N) :-
  % show one justification and recurse.
  nl,
  pp_justifications2(J,N,1),
  N2 is N+1,
  loop_check_true(pp_justification1(Js,N2)).

pp_justifications2([],_,_).

pp_justifications2([C|Rest],JustNo,StepNo) :-  
  fmt("    ~w.~w ~w",[JustNo,StepNo,C]),
  StepNext is 1+StepNo,
  loop_check_true(pp_justifications2(Rest,JustNo,StepNext)).

pfc_ask(Msg,Ans) :-
  fmt("~w",[Msg]),
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
        (has_cl(Head)->doall((show_call(clause(Head,_))));hotrace((listing(Head)))),!.


% ===================================================
% Pretty Print Formula
% ===================================================


print_db_items(Title,Mask,What):-print_db_items(Title,Mask,Mask,What).
print_db_items(Title,Mask,SHOW,What0):-
     get_pi(Mask,H),get_pi(What0,What),
     format(atom(Showing),'~p for ~p...',[Title,What]),
     statistics(cputime,Now),Max is Now + 2,!,
       gripe_time(1.0,
         doall((once(statistics(cputime,NewNow)),NewNow<Max,clause_or_call(H,B),
             hotrace(pfc_contains_term(What,(H:-B))),
             flag(print_db_items,LI,LI+1),
             ignore(hotrace(pp_item(Showing,SHOW)))))),
     ignore(pp_item(Showing,done)),!.

pfc_contains_term(What,_):-is_ftVar(What),!.
pfc_contains_term(What,Inside):- compound(What),!,(\+ \+ ((copy_term_nat(Inside,Inside0),snumbervars(Inside0),contains_term(What,Inside0)))),!.
pfc_contains_term(What,Inside):- (\+ \+ once((subst(Inside,What,foundZadooksy,Diff),Diff \=@= Inside ))),!.

% user:listing_mpred_hook(What):- debugOnError(pfc_listing(What)).

:-thread_local thlocal:pfc_listing_disabled.
% listing(L):-with_assertions(thlocal:pfc_listing_disabled,listing(L)).

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
   print_db_items('Forward Facts',(nesc(F)),F,What),
   print_db_items('Forward Rules',(_==>_),What),
 ignore((What\=neg(_),functor(What,IWhat,_),
   print_db_items_and_neg('Instance Of',isa(IWhat,_),IWhat),
   print_db_items_and_neg('Instances: ',isa(_,IWhat),IWhat),
   print_db_items_and_neg('Subclass Of',genls(IWhat,_),IWhat),
   print_db_items_and_neg('Subclasses: ',genls(_,IWhat),IWhat))),
   print_db_items('PFC Watches', pfcMark(_,_,_,_),What),
   print_db_items('Triggers Negative', nt(_,_,_),What),
   print_db_items('Triggers Goal',bt(_,_),What),
   print_db_items('Triggers Positive',pt(_,_),What),
   print_db_items('Bidirectional Rules',(_<==>_),What), 
   dif(A,B),print_db_items('Supports Deduced',spft_precanonical(P,A,B),spft(P,A,B),What),
   dif(G,u),print_db_items('Supports Nonuser',spft_precanonical(P,G,G),spft(P,G,G),What),
   print_db_items('Backchaining Rules',(_<-_),What),
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


% logicmoo_html_needs_debug.
:-multifile shared_hide_data/1.

shared_hide_data('$was_imported_kb_content$'/2):- !,listing_filter(hideMeta).
shared_hide_data(spftY/4):- !,listing_filter(hideTriggers).
shared_hide_data(spft/3):- !,listing_filter(hideTriggers).
shared_hide_data(nt/3):- !,listing_filter(hideTriggers).
shared_hide_data(pt/2):- !, listing_filter(hideTriggers).
shared_hide_data(bt/2):- !, listing_filter(hideTriggers).
shared_hide_data((H:-
 cwc,
        second_order(_,_G19865),
        (   _G19865 = (_G19867,!,_G19871) ->
                call(_G19867),  !,
                call(_G19871)
        ;   CALL
        ))):- CALL=@=call(_G19865).

shared_hide_data(pfcMark/4):- !,listing_filter(hideMeta).


pp_now.

this_listing(M:F/A):-functor(H,F,A),predicate_property(M:H,number_of_causes(_)),!, forall(clause(M:H,Body),pp_i2tml((M:H :- Body))).
this_listing(M:F/A):-functor(H,F,A),predicate_property(H,number_of_causes(_)),!, forall(clause(H,Body),pp_i2tml((M:H :- Body))).
this_listing(M:F/A):-listing(M:F/A),!.
this_listing(MFA):-listing(MFA).

:-thread_local(sortme_buffer/2).


% i2tml_save(Obj,H):- \+ is_list(H),cyc:pterm_to_sterm(H,S),H\=@=S,!,i2tml_save(Obj,S).

pp_i2tml_saved_done(Obj):-pp_now,!,flush_output.
pp_i2tml_saved_done(Obj):-
  findall(H,retract(sortme_buffer(Obj,H)),List),predsort(head_functor_sort,List,Set),
  forall(member(S,Set),pp_i2tml(S)),!.

find_cl_ref(_,none):- thlocal:tl_hide_data(hideClauseInfo),!.
find_cl_ref(clause(H,B,Ref),Ref):-!.
find_cl_ref(clause(H,B),Ref):- clause(H,B,Ref),!.
find_cl_ref((H:-B),Ref):-!, clause(H,B,Ref),clause(HH,BB,Ref),H=@=HH,B=@=BB,!.
find_cl_ref(H,Ref):- clause(H,true,Ref),clause(HH,true,Ref),H=@=HH,!.

find_ref(_,none):- thlocal:tl_hide_data(hideClauseInfo),!.
find_ref(H,Ref):- find_cl_ref(H,Ref),!.
find_ref(This,Ref):- '$was_imported_kb_content$'(A,CALL),arg(1,CALL,This),clause('$was_imported_kb_content$'(A,CALL),true,Ref),!.
find_ref(M:This,Ref):- atom(M),!,find_ref(This,Ref).

head_functor_sort(Result,H1,H2):- (var(H1);var(H2)),compare(Result,H1,H2),!.
head_functor_sort(Result,H1,H2):- once((get_functor(H1,F1,A1),get_functor(H2,F2,A2))),F1==F2,A1>0,A2>0,arg(1,H1,E1),arg(1,H2,E2),compare(Result,E1,E2),Result \== (=),!.
head_functor_sort(Result,H1,H2):- once((get_functor(H1,F1,_),get_functor(H2,F2,_))),F1\==F2,compare(Result,F1,F2),Result \== (=),!.
head_functor_sort(Result,H1,H2):-compare(Result,H1,H2),!.

i2tml_hbr(H,B,Ref):- nonvar(Ref),!,pp_i2tml_save_seen(clause(H,B,Ref)).
i2tml_hbr(H,B,_):- B==true,!, pp_i2tml_save_seen(H).
i2tml_hbr(H,B,_):- !,pp_i2tml_save_seen((H:-B)).

pp_i2tml_save_seen(HB):- pp_now, !,pp_i2tml(HB),!.
pp_i2tml_save_seen(HB):- assertz_if_new(sortme_buffer(Obj,HB)),!.


:-thread_local(thlocal:pp_i2tml_hook/1).

:-thread_local(thlocal:tl_hide_data/1).
   
:-thread_local(shown_subtype/1).
:-thread_local(shown_clause/1).

section_open(Type):-  once(shown_subtype(Type)->true;((thlocal:print_mode(html)->format('~n</pre><hr>~w<hr><pre>~n<font face="verdana,arial,sans-serif">',[Type]);(draw_line,format('% ~w~n~n',[Type]))),asserta(shown_subtype(Type)))),!.
section_close(Type):- shown_subtype(Type)->(retractall(shown_subtype(Type)),(thlocal:print_mode(html)->format('</font>\n</pre><hr/><pre>',[]);draw_line));true.

pp_item_html(_Type,H):-var(H),!.
pp_item_html(Type,done):-!,section_close(Type),!.
pp_item_html(_,H):-shown_clause(H),!.
pp_item_html(_,P):- (listing_filter(P); (compound(P),functor(P,F,A),(listing_filter(F/A);listing_filter(F)))),!.

pp_item_html(Type,H):- \+ thlocal:print_mode(html), pp_item_html_now(Type,H),!.
pp_item_html(Type,H):- ignore((flag(matched_assertions,X,X),between(0,5000,X),pp_item_html_now(Type,H))).

:-dynamic(last_item_offered/1).
last_item_offered(unknonw).


pp_item_html_now(Type,H):-    
   flag(matched_assertions,X,X+1),!,
   pp_item_html_if_in_range(Type,H),!,
   assert(shown_clause(H)),!.


pp_item_html_if_in_range(Type,H):- section_open(Type),!,pp_i2tml(H),!,nl.

:-thread_local(thlocal:last_show_clause_ref/1).
:-thread_local(thlocal:current_clause_ref/1).


show_clause_ref(Ref):- Ref == none,!.
show_clause_ref(Ref):- thlocal:last_show_clause_ref(Ref),!.
show_clause_ref(Ref):- retractall(thlocal:last_show_clause_ref(_)),asserta(thlocal:last_show_clause_ref(Ref)),logOnError(show_clause_ref_now(Ref)),!.

show_clause_ref_now(V):-var(V),!.
show_clause_ref_now(0):-!.
show_clause_ref_now(Ref):- listing_filter(hideClauseRef),!.
show_clause_ref_now(Ref):- listing_filter(showFilenames), \+ clause_property(Ref,predicate(_)),format('~N~p~N',[clref(Ref)]),!.
% write_html(div(class(src_formats),a(href(EditLink), edit)])).
show_clause_ref_now(Ref):- listing_filter(showFilenames),clause_property(Ref,file(File)),ignore(clause_property(Ref,line_count(Line))),
  ignore(clause_property(Ref,module(Module))),
    format('<a href="/swish/filesystem/~w#L~w">@file:~w:~w</a>(~w)~N',[File,Line,File,Line,Module]),
    fail. 
show_clause_ref_now(Ref):- clause_property(Ref,erased),
  ignore(clause_property(Ref,module(Module))),
    format('erased(~w) (~w)~N',[Ref,Module]),!.

pp_i2tml(Done):-Done==done,!.
pp_i2tml(T):-isVarProlog(T),format('~w',[T]),!.
pp_i2tml(T):-string(T),format('"~w"',[T]).
pp_i2tml(clause(H,B,Ref)):- !, with_assertions(thlocal:current_clause_ref(Ref),pp_i2tml_v((H:-B))).
pp_i2tml(HB):- find_ref(HB,Ref),!, must(with_assertions(thlocal:current_clause_ref(Ref),pp_i2tml_v((HB)))).
pp_i2tml(HB):- with_assertions(thlocal:current_clause_ref(none),must(pp_i2tml_v((HB)))).

get_clause_vars_for_print(HB,HB):- ground(HB),!.
get_clause_vars_for_print(I,I):- listing_filter(skipVarnames),!.
get_clause_vars_for_print(H0,MHB):- get_clause_vars_copy(H0,MHB).

numberlist_at(_,[]).
numberlist_at(_,[N|More]):- number(N),!,N2 is N+1,numberlist_at(N2,More),!.
numberlist_at(Was,[N|More]):-var(N),  N is Was+1, N2 is N+1,  numberlist_at(N2,More),!.
numberlist_at(Was,[_|More]):- N2 is Was+2, numberlist_at(N2,More),!.


pp_i2tml_v(HB):- ignore(catch(( \+ \+ ((get_clause_vars_for_print(HB,HB2),pp_i2tml_0(HB2)))),_,true)),!.

pp_i2tml_0(Var):-var(Var),!.
pp_i2tml_0(USER:HB):-USER==user,!,pp_i2tml_0(HB),!.
pp_i2tml_0((H :- B)):-B==true,!,pp_i2tml_0((H)),!.
pp_i2tml_0(((USER:H) :- B)):-USER==user,!,pp_i2tml_0((H:-B)),!.
pp_i2tml_0((H:-B)):-B==true, !, pp_i2tml_0(H).

pp_i2tml_0(P):- listing_filter(P),!.
pp_i2tml_0(was_chain_rule(H)):- pp_i2tml_0(H).
pp_i2tml_0(M:(H)):-M==user, pp_i2tml_0(H).
pp_i2tml_0(is_edited_clause(H,B,A)):- pp_i2tml_0(proplst([(clause)=H,before=B,after=A])).
pp_i2tml_0(is_disabled_clause(H)):- pp_i2tml_0((disabled)=H).


% pp_i2tml_0(FET):-fully_expand(assert,FET,NEWFET),FET\=@=NEWFET,!,pp_i2tml_0(NEWFET).

pp_i2tml_0(spftY(P,F,T,W)):-!,
   with_assertions(thlocal:current_why_source(W),pp_i2tml_0(spft(P,F,T))).

pp_i2tml_0(spft(P,U,U)):- nonvar(U),!, pp_i2tml_1(P:-asserted_by(U)).
pp_i2tml_0(spft(P,F,T)):- atom(F),atom(T),!, pp_i2tml_1(P:-asserted_in(F:T)).
pp_i2tml_0(spft(P,F,T)):- atom(T),!,  pp_i2tml_1(((P):-  T:'t-deduced',F)). 
pp_i2tml_0(spft(P,F,T)):- atom(F),!,  pp_i2tml_1(((P):-  F:'f-deduced',T)). 
pp_i2tml_0(spft(P,F,T)):- !, pp_i2tml_1((P:- ( 'deduced-from'=F,  (rule_why = T)))).
pp_i2tml_0(nt(Trigger,Test,Body)) :- !, pp_i2tml_1(proplst(['n-trigger'=Trigger , format=Test  ,  (body = (Body))])).
pp_i2tml_0(pt(Trigger,Body)):-      pp_i2tml_1(proplst(['p-trigger'=Trigger , ( body = Body)])).
pp_i2tml_0(bt(Trigger,Body)):-      pp_i2tml_1(proplst(['b-trigger'=Trigger ,  ( body = Body)])).

pp_i2tml_0(proplst([N=V|Val])):- is_list(Val),!, pp_i2tml_1(N:-([clause=V|Val])).
pp_i2tml_0(proplst(Val)):-!, pp_i2tml_1(:-(proplst(Val))).


pp_i2tml_0(M:H):- M==user,!,pp_i2tml_1(H).
pp_i2tml_0((M:H:-B)):- M==user,!,pp_i2tml_1((H:-B)).
pp_i2tml_0(HB):-pp_i2tml_1(HB).

if_html(F,A):-thlocal:print_mode(html),!,format(F,[A]).
if_html(F,A):-A.

pp_i2tml_1(H):- 
 once(((last_item_offered(Was);Was=foobar),get_functor(Was,F1,A1),get_functor(H,F2,A2),
   retractall(last_item_offered(Was)),asserta(last_item_offered(H)),
    ((F1 \== F2 -> if_html('~N<hr/>',true);true)))),flush_output,fail.

pp_i2tml_1(_H):- thlocal:current_clause_ref(Ref),
    if_html('<font size="1">~@</font>',show_clause_ref(Ref)),fail.

pp_i2tml_1(H):- thlocal:print_mode(html), 
  term_to_pretty_string(H,ALT)->
   functor_to_color(H,FC)->fmtimg(FC,ALT)->
    format('<input type="checkbox" name="assertion[]" value="~w">',[ALT]),fail.

pp_i2tml_1(H):- \+ \+ pp_i2tml_now(H).

pp_i2tml_now(C):- thlocal:pp_i2tml_hook(C),!.
pp_i2tml_now(C):- if_html('<font size="3">~@</font>~N',if_defined(rok_portray_clause(C),portray_clause(C))).




%  II = 56+TTT, rtrace((url_encode(II,EE),url_decode(EE,OO))),writeq(OO),OO=II.



% url_decode(B,A):- \+ atom(B),!,term_to_atom(B,BB),!,url_encode(BB,O),!,A=O.
url_decode(B,A):- \+ atom(B),A=B.
url_decode(A,B):- atom_concat('#%24%28',_,A) , url_decode_term(A,T),!,T=B.
url_decode(A,B):- url_iri(A,C),!,B=C.

url_decode_term(A,T):- nb_current(A,T),nb_delete(A),!.
url_decode_term(A,T):- url_iri(A,B),
    read_term_from_atom(B,'#$'(T:Vs2),[variable_names(Vs3)]),
    ignore(Vs2=Vs3),!, ignore(Vs2=[]),!.

url_decode_term(A,T):-
    url_iri(A,B),
    read_term_from_atom(B,'#$'(T:Vs2),[variable_names(Vs3)]),
    ignore(Vs2=[]),ignore(Vs2=Vs3),
    merge_key_vals(Before,Vs2,Merge),
    nb_current('$variable_names',Env),
    merge_key_vals(Env,Merge,New),
    b_setval('$variable_names',New),!.




% x(Z+B)

% II = 56+TTT, ((show_call((url_encode(II,EE),var_property(TTT,name(NNN)),url_decode(EE,OO))))),writeq(OO).

url_encode(B,A):- \+ atom(B),!,term_variables(B,Vars),url_encode_term(B,Vars,O),O=A.
url_encode(B,A):- atom_concat('\n',BT,B),!,url_encode(BT,A).
url_encode(B,A):- atom_concat(BT,'\n',B),!,url_encode(BT,A).
url_encode(B,A):- atom_concat(' ',BT,B),!,url_encode(BT,A).
url_encode(B,A):- atom_concat(BT,' ',B),!,url_encode(BT,A).
url_encode(B,A):- url_iri(A,B).

name_the_var(Num,Vs,[],Vs,[]).

name_the_var(Num,Vs,[VIn|More],VsOut,[N=V|Added]):- member_open(N=V,Vs),VIn==V,!,name_the_var(Num,Vs,More,VsOut,Added).
% name_the_var(Num,Vs,[VIn|More],VsOut,[N=VIn|Added]):- \+ is_list(Vs), append(Vs,[N=VIn],NewVs),!, name_the_var(Num,NewVs,More,VsOut,Added).
name_the_var(Num,Vs,[VIn|More],[N=VIn|VsOut],[N=VIn|Added]):- Num2 is Num +1, NV = '$VAR'(Num),
  with_output_to(atom(N),write_term(NV,[portrayed(true),quoted,priority(9)])),
  name_the_var(Num2,Vs,More,VsOut,Added).

url_encode_term(B,[],A):- !, term_to_atom('#$'(B:[]),BB),!,url_iri(O,BB).

url_encode_term(InTerm,VsIn,URL):- fail, with_output_to(atom(IRI),portray_clause('#$'((InTerm:_)))),
  url_iri(URL,IRI),nb_linkval(URL,InTerm),!.

url_encode_term(InTerm,VsIn,URL):-
  nb_current('$variable_names',Prev),
  name_the_var(40,Prev,VsIn,NewVs,Added),
  % (NewVs\==Prev ->  show_call(b_setval('$variable_names',NewVs)) ; true),
  with_output_to(atom(IRI),write_term('#$'(InTerm:Added),[quoted(true),variable_names(Added),quoted,priority(9)])),
  url_iri(URL,IRI),!.

%   b_setval(URL,InTerm).

write_as_url_encoded(_Arg, D):- url_encode(D,U),!,writeq(U).
:-format_predicate('u',write_as_url_encoded(_Arg,_Time)).

term_to_pretty_string(H,HS):-atomic(H),!,with_output_to(atom(HS),writeq(H)).
term_to_pretty_string(H,HS):-
   % igno re(source_variables(X))->ignore(X=[])->
   % numb ervars(HC,0,_)->
  with_output_to(atom(HS),portray_clause(H)).

fmtimg(N,Alt):- thlocal:print_mode(html),!,
 make_quotable(Alt,AltQ),
 url_encode(Alt,AltS),
 format('~N<a href="?call=edit1term&term=~w" target="_parent"><img src="/pixmaps/~w.gif" alt="~w" title="~w"><a>',[AltS,N,AltQ,AltQ]).
fmtimg(_,_).


indent_nbsp(X):-thlocal:print_mode(html),forall(between(0,X,_),format('&nbsp;')),!.
indent_nbsp(X):-forall(between(0,X,_),format('~t',[])),!.

indent_nl:- fresh_line, flag(indent,X,X), indent_nbsp(X).


indent_nbsp(0,''):-!.
indent_nbsp(1,'\n         '):-!.
indent_nbsp(X,Chars):-XX is X -1,!, indent_nbsp(XX,OutP),!,sformat(Chars,'~w   ',[OutP]),!.



tovl([],[],[]).
tovl([K|KL],[V|VL],[K=V|KVL]) :- tovl(KL, VL, KVL).

member_open(C, [B|A]) :-  (nonvar(B),B=C) ; (nonvar(A),member_open(C, A)).

merge_key_vals(Prev,Pairs,NewSave):-var(Prev),!,NewSave=Pairs.
merge_key_vals([],Pairs,NewSave):-!,NewSave=Pairs.
merge_key_vals([K1=V1|Prev],Pairs,NewSave):-
   member_open(K2=V2,Pairs),
   V1==V2, merge_key_vals(Prev,Pairs,NewSave).
merge_key_vals([K1=V1|Prev],Pairs,NewSave):-
   member_open(K2=V2,Pairs),
   K1==K2, V1=V2, merge_key_vals(Prev,Pairs,NewSave).
merge_key_vals([K1=V1|Prev],Pairs,NewSave):-
   merge_key_vals(Prev,[K1=V1|Pairs],NewSave).





functor_to_color(wid(_,_,G),C):-!,functor_to_color(G,C).
functor_to_color(G,C):-compound(G),functor(G,F,A),functor_to_color(G,F,A,C).
functor_to_color(G,green):-!.


functor_to_color(G,isa,_,bug_btn_s).

functor_to_color(G,genls,1,'plus-green').
functor_to_color(G,arity,_,'white').
functor_to_color(G,argIsa,_,'white').
functor_to_color(G,argGenls,_,'white').

functor_to_color(G,F,1,yellow).


functor_to_color(G,_,_,'lightgrey'):-predicate_property(G,foreign).
functor_to_color(G,_,_,'cyc-logo-3-t'):-predicate_property(G,built_in).

functor_to_color(_,-,_,red).
functor_to_color(_,not,_,red).
functor_to_color(_,~,_,red).
functor_to_color(_,neg,_,red).

functor_to_color(G,(<==>),_,'plus-purple').
functor_to_color(G,(<-),_,purple).
functor_to_color(G,(<=),_,'cyc-right-triangle-violet').
functor_to_color(G,(==>),_,'cyc-right-triangle-violet').
functor_to_color(G,(:-),_,red_diam).

functor_to_color(G,(if),_,cy_menu).
functor_to_color(G,(iff),_,cyan).
functor_to_color(G,(all),_,cyan).
functor_to_color(G,(exists),_,blue).

functor_to_color(G,(mudEquals),_,pink).
functor_to_color(G,(skolem),_,pink).
functor_to_color(G,(wid),_,green_yellow).


% :- (thread_property(ID,status(running)),ID=reloader30) -> true; thread_create(((repeat,sleep(30),mmake,fail)),_,[alias(reloader30),detached(true)]).
