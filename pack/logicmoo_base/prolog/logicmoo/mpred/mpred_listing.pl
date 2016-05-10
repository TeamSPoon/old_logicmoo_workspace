% =======================================================
/* 
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
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_list_triggers.pl
%:- if(((current_prolog_flag(xref,true),current_prolog_flag(pldoc_x,true));current_prolog_flag(autoload_logicmoo,true))).
:- module(mpred_listing,
          [ draw_line/0,
            loop_check_just/1,
            
            pp_items/2,
            pp_item/2,
            pp_filtered/1,
            pp_facts/2,
            pp_facts/1,
            pp_facts/0,
            mpred_list_triggers_types/1,
            mpred_list_triggers_nlc/1,
            mpred_list_triggers_1/1,
            mpred_list_triggers_0/1,
            mpred_list_triggers/1,
            mpred_contains_term/2,
            mpred_classify_facts/4,
            lqu/0,
            get_clause_vars_for_print/2,
            %mpred_whyBrouse/2,
            %mpred_why1/1,
            %mpred_why/1,
            %mpred_why/0,
            pp_rules/0,
            pp_supports/0,
            pp_triggers/0,            
            print_db_items/1,
            print_db_items/2,
            print_db_items/3,
            print_db_items/4,
            print_db_items_and_neg/3,
            show_pred_info/1,
            show_pred_info_0/1,
            mpred_listing_file/0
          ]).
%:- endif.

% :- use_module(logicmoo(util/logicmoo_util_preddefs)).

:- include('mpred_header.pi').

:- multifile((
              user:portray/1,
  	user:prolog_list_goal/1,
  	user:prolog_predicate_name/2,
  	user:prolog_clause_name/2)).

:- dynamic
  	user:portray/1.

% :- dynamic(why_buffer/2).



%= 	 	 

%% lqu is semidet.
%
% Lqu.
%
lqu :- listing(que/1).


 

%= 	 	 

%% pp_facts is semidet.
%
% Pretty Print Facts.
%
pp_facts :- pp_facts(_,true).


%= 	 	 

%% pp_facts( ?Pattern) is semidet.
%
% Pretty Print Facts.
%
pp_facts(Pattern) :- pp_facts(Pattern,true).


%= 	 	 

%% pp_facts( ?P, ?C) is semidet.
%
% Pretty Print Facts.
%
pp_facts(P,C) :-
  mpred_facts(P,C,L),
  mpred_classify_facts(L,User,Pfc,_Rule),
  draw_line,
  fmt("User added facts:",[]),
  pp_items(user,User),
  draw_line,
  draw_line,
  fmt("Pfc added facts:",[]),
  pp_items(system,Pfc),
  draw_line.



%= 	 	 

%% pp_items( ?Type, :TermH) is semidet.
%
% Pretty Print Items.
%
pp_items(_Type,[]):-!.
pp_items(Type,[H|T]) :-
  ignore(pp_item(Type,H)),!,
  pp_items(Type,T).
pp_items(Type,H) :- ignore(pp_item(Type,H)).

 

%= 	 	 

%% pp_item( ?MM, :TermH) is semidet.
%
% Pretty Print Item.
%
pp_item(_M,H):-pp_filtered(H),!.
pp_item(MM,(H:-B)):- B ==true,pp_item(MM,H).
pp_item(MM,H):- flag(show_asserions_offered,X,X+1),t_l:print_mode(html), ( \+ \+ if_defined(pp_item_html(MM,H))),!.


pp_item(MM,spft(W0,U,ax)):- W = (_KB:W0),!,pp_item(MM,U:W).
pp_item(MM,spft(W0,F,U)):- W = (_KB:W0),atom(U),!,    fmt('~N%~n',[]),pp_item(MM,U:W), fmt('rule: ~p~n~n', [F]),!.
pp_item(MM,spft(W0,F,U)):- W = (_KB:W0),         !,   fmt('~w~nd:       ~p~nformat:    ~p~n', [MM,W,F]),pp_item(MM,U).
pp_item(MM,nt(Trigger0,Test,Body)) :- Trigger = (_KB:Trigger0), !, fmt('~w n-trigger: ~p~ntest: ~p~nbody: ~p~n', [MM,Trigger,Test,Body]).
pp_item(MM,pt(F0,Body)):- F = (_KB:F0),             !,fmt('~w p-trigger:~n', [MM]), pp_item('',(F:-Body)).
pp_item(MM,bt(F0,Body)):- F = (_KB:F0),             !,fmt('~w b-trigger:~n', [MM]), pp_item('',(F:-Body)).


pp_item(MM,U:W):- !,sformat(S,'~w  ~w:',[MM,U]),!, pp_item(S,W).
pp_item(MM,H):- \+ \+ (( get_clause_vars_for_print(H,HH),fmt("~w ~p~N",[MM,HH]))).


%= 	 	 

%% get_clause_vars_for_print( ?HB, ?HB) is semidet.
%
% Get Clause Variables For Print.
%
get_clause_vars_for_print(HB,HB):- ground(HB),!.
get_clause_vars_for_print(I,I):- listing_filter(skipVarnames),!.
get_clause_vars_for_print(H0,MHB):- get_clause_vars_copy(H0,MHB).


%= 	 	 

%% mpred_classify_facts( :TermH, ?User, :TermPfc, ?H) is semidet.
%
% Managed Predicate Classify Facts.
%
mpred_classify_facts([],[],[],[]).

mpred_classify_facts([H|T],User,Pfc,[H|Rule]) :-
  mpred_db_type(H,rule),
  !,
  mpred_classify_facts(T,User,Pfc,Rule).

mpred_classify_facts([H|T],[H|User],Pfc,Rule) :-
  mpred_get_support(H,(mfl(_,_,_),ax)),
  !,
  mpred_classify_facts(T,User,Pfc,Rule).

mpred_classify_facts([H|T],User,[H|Pfc],Rule) :-
  mpred_classify_facts(T,User,Pfc,Rule).



%= 	 	 

%% print_db_items( ?T, ?I) is semidet.
%
% Print Database Items.
%
print_db_items(T, I):- 
    draw_line, 
    fmt("~N~w ...~n",[T]),
    print_db_items(I),
    draw_line,!.


%= 	 	 

%% print_db_items( ?I) is semidet.
%
% Print Database Items.
%
print_db_items(F/A):-number(A),!,functor(P,F,A),!,print_db_items(P).
print_db_items(I):- bagof(I,with_umt(clause_u(I,true)),R1),pp_items((:),R1),R1\==[],!.
print_db_items(I):- bagof(I,with_umt(clause_i(I,true)),R1),pp_items((:),R1),R1\==[],!.
print_db_items(I):- \+ current_predicate(_,I),!. 
print_db_items(I):- catch( ('$find_predicate'(I,_),call_u(listing(I))),_,true),!,nl,nl.


%= 	 	 

%% pp_rules is semidet.
%
% Pretty Print Rules.
%
pp_rules :-
   print_db_items("Forward Rules",(_ ==> _)),
   print_db_items("Bidirectional Rules",(_ <==> _)), 
   print_db_items("Implication Rules",(_ => _)),
   print_db_items("Bi-conditional Rules",(_ <=> _)),
   print_db_items("Backchaining Rules",(_ <- _)),
   print_db_items("Positive Facts",(==>(_))),
   print_db_items("Negative Facts",(~(_))).


%= 	 	 

%% pp_triggers is semidet.
%
% Pretty Print Triggers.
%
pp_triggers :-
     print_db_items("Positive triggers",pt(_,_,_)),
     print_db_items("Negative triggers", nt(_,_,_,_)),
     print_db_items("Goal triggers",bt(_,_,_)).


%= 	 	 

%% pp_supports is semidet.
%
% Pretty Print Supports.
%
pp_supports :-
  % temporary hack.
  draw_line,
  fmt("Supports ...~n",[]), 
  setof((P =< S), (mpred_get_support(P,S), \+ pp_filtered(P)),L),
  pp_items('Support',L),
  draw_line,!.


pp_filtered(P):-var(P),!,fail.
pp_filtered(_:P):- !, pp_filtered(P).
pp_filtered(P):- functor(P,F,A),F\==(/),!,pp_filtered(F/A).
pp_filtered(F/_):-F==mpred_mark.



%% draw_line is semidet.
%
% Draw Line.
%
draw_line:- (t_l:print_mode(H)->true;H=unknown),fmt("~N%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n",[]),H=H.

 :- meta_predicate loop_check_just(0).

%= 	 	 

%% loop_check_just( :GoalG) is semidet.
%
% Loop Check Justification.
%
loop_check_just(G):-loop_check(G,ignore(arg(1,G,[]))).


%= 	 	 

%% show_pred_info( ?F) is semidet.
%
% Show Predicate Info.
%
show_pred_info(F/A):-integer(A),functor(H,F,A),!,show_pred_info(H).
show_pred_info(Head):-
        doall(show_call(why,no_repeats(isa(Head,_)))),
        functor(Head,F,_),
        doall(show_call(why,no_repeats(isa(F,_)))),
       ((current_predicate(_,M:Head), (\+ predicate_property(M:Head,imported_from(_))))
          -> show_pred_info_0(M:Head); 
             wdmsg(cannot_show_pred_info(M:Head))),!.


%= 	 	 

%% show_pred_info_0( ?Head) is semidet.
%
% show Predicate info  Primary Helper.
%
show_pred_info_0(Head):- 
        doall(show_call(why,predicate_property(Head,_))),
        (has_cl(Head)->doall((show_call(why,clause(Head,_))));hotrace((listing(Head)))),!.


% ===================================================
% Pretty Print Formula
% ===================================================



%= 	 	 

%% print_db_items( ?Title, ?Mask, ?What) is semidet.
%
% Print Database Items.
%
print_db_items(Title,Mask,What):-print_db_items(Title,Mask,Mask,What).

%= 	 	 

%% print_db_items( ?Title, ?Mask, ?SHOW, ?What0) is semidet.
%
% Print Database Items.
%
print_db_items(Title,Mask,SHOW,What0):-
     get_pi(Mask,H),get_pi(What0,What),
     format(atom(Showing),'~p for ~p...',[Title,What]),
     statistics(cputime,Now),Max is Now + 2,!,
       gripe_time(1.0,
         doall((once(statistics(cputime,NewNow)),NewNow<Max,clause_or_call(H,B),
             hotrace(mpred_contains_term(What,(H:-B))),
             flag(print_db_items,LI,LI+1),
             ignore(hotrace(pp_item(Showing,SHOW)))))),
     ignore(pp_item(Showing,done)),!.


%= 	 	 

%% mpred_contains_term( ?What, ?VALUE2) is semidet.
%
% Managed Predicate Contains Term.
%
mpred_contains_term(What,_):-is_ftVar(What),!.
mpred_contains_term(What,Inside):- compound(What),!,(\+ \+ ((copy_term_nat(Inside,Inside0),snumbervars(Inside0),contains_term(What,Inside0)))),!.
mpred_contains_term(What,Inside):- (\+ \+ once((subst(Inside,What,foundZadooksy,Diff),Diff \=@= Inside ))),!.



%= 	 	 

%% hook_mpred_listing( ?What) is semidet.
%
% Hook To [lmconf:hook_mpred_listing/1] For Module Mpred_listing.
% Hook Managed Predicate Listing.
%
lmconf:hook_mpred_listing(What):- on_x_rtrace(mpred_list_triggers(What)).

:- thread_local t_l:mpred_list_triggers_disabled.
% listing(L):-w_tl(t_l:mpred_list_triggers_disabled,listing(L)).


%= 	 	 

%% mpred_list_triggers( ?What) is semidet.
%
% Managed Predicate List Triggers.
%
mpred_list_triggers(_):-t_l:mpred_list_triggers_disabled,!.
mpred_list_triggers(What):-loop_check(mpred_list_triggers_nlc(What)).

:- meta_predicate(mpred_list_triggers_nlc(?)).


%= 	 	 

%% mpred_list_triggers_nlc( ?What) is semidet.
%
% Managed Predicate List Triggers Nlc.
%
mpred_list_triggers_nlc(MM:What):-atom(MM),!,MM:mpred_list_triggers(What).
mpred_list_triggers_nlc(What):-loop_check(mpred_list_triggers_0(What),true).


%= 	 	 

%% mpred_list_triggers_0( ?What) is semidet.
%
% Managed Predicate list triggers  Primary Helper.
%
mpred_list_triggers_0(What):-get_pi(What,PI),PI\=@=What,mpred_list_triggers(PI).
mpred_list_triggers_0(What):-nonvar(What),What= ~(Then),!, \+ \+ mpred_list_triggers_1(Then), \+ \+ mpred_list_triggers_1(What).
mpred_list_triggers_0(What):- \+ \+  mpred_list_triggers_1(~(What)), \+ \+ mpred_list_triggers_1(What).


%= 	 	 

%% mpred_list_triggers_types( ?VALUE1) is semidet.
%
% Managed Predicate list triggers  Types.
%
mpred_list_triggers_types('Triggers').
mpred_list_triggers_types('Instances').
mpred_list_triggers_types('Subclasses').
mpred_list_triggers_types('ArgTypes').
mpred_list_triggers_types('Arity').
mpred_list_triggers_types('Forward').
mpred_list_triggers_types('Bidirectional').
mpred_list_triggers_types('Backchaining').
mpred_list_triggers_types('Negative').
mpred_list_triggers_types('Sources').
mpred_list_triggers_types('Supports').
mpred_list_triggers_types('Edits').

% print_db_items_and_neg(Title,Fact,What):-nonvar(Fact),Fact= ~(_),!,fail.

%= 	 	 

%% print_db_items_and_neg( ?Title, ?Fact, ?What) is semidet.
%
% Print Database Items And Negated.
%
print_db_items_and_neg(Title,Fact,What):-print_db_items(Title,Fact,What).
print_db_items_and_neg(Title,Fact,What):-print_db_items(Title,~(Fact),What).


%= 	 	 

%% mpred_list_triggers_1( ?What) is semidet.
%
% Managed Predicate list triggers  Secondary Helper.
%
mpred_list_triggers_1(~(What)):-var(What),!.
mpred_list_triggers_1(~(_What)):-!.
mpred_list_triggers_1(What):-var(What),!.
mpred_list_triggers_1(What):- 
   print_db_items('Supports User',spft_precanonical(P,mfl(_,_,_),ax),spft(P,mfl(_,_,_),ax),What),
   print_db_items('Forward Facts',(nesc(F)),F,What),
   print_db_items('Forward Rules',(_==>_),What),
 ignore((What\= ~(_),functor(What,IWhat,_),
   print_db_items_and_neg('Instance Of',isa(IWhat,_),IWhat),
   print_db_items_and_neg('Instances: ',isa(_,IWhat),IWhat),
   print_db_items_and_neg('Subclass Of',genls(IWhat,_),IWhat),
   print_db_items_and_neg('Subclasses: ',genls(_,IWhat),IWhat))),
   print_db_items('PFC Watches', mpred_mark(_,_,_),What),
   print_db_items('Triggers Negative', nt(_,_,_,_),What),
   print_db_items('Triggers Goal',bt(_,_,_),What),
   print_db_items('Triggers Positive',pt(_,_,_),What),
   print_db_items('Bidirectional Rules',(_<==>_),What), 
   dif(A,B),print_db_items('Supports Deduced',spft_precanonical(P,A,B),spft(P,A,B),What),
   dif(G,ax),print_db_items('Supports Nonuser',spft_precanonical(P,G,G),spft(P,G,G),What),
   print_db_items('Backchaining Rules',(_<-_),What),
   % print_db_items('Edits',is_disabled_clause(_),What),
   print_db_items('Edits',is_edited_clause(_,_,_),What),
   print_db_items('Instances',isa(_,_),What),
   print_db_items('Subclasses',genls(_,_),What),
   print_db_items('Negative Facts',~(_),What),

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
   print_db_items('Sources',predicateConventionMt(_,_),What),
   print_db_items('Sources',source_file(_,_),What),
   print_db_items('Sources',_:man_index(_,_,_,_,_),What),
   print_db_items('Sources',_:'$pldoc'(_,_,_,_),What),
   print_db_items('Sources',_:'$pred_option'(_,_,_,_),What),
   print_db_items('Sources',_:'$mode'(_,_),What),
   !.     



mpred_listing_file.
