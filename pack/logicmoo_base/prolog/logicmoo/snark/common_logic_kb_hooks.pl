:- module(common_logic_kb_hooks,[kbp_t/1,with_el_holds_disabled/1,noGenlPreds/1,cyckb_t/3,link_to_holds2/2,
           assert_next_queue/1,
            assert_to_db_list/2,
            assertion_f/1,
            assertion_t/1,
            big_kb_ASSERTION/2,
            call_f/3,
            call_f/4,
            call_f/5,
            call_f/6,
            call_f/7,
            call_f/8,
            call_f/9,
            call_mt_f/4,
            call_mt_f/5,
            call_mt_f/6,
            call_mt_f/7,
            call_mt_f/8,
            call_mt_f/9,
            call_mt_f/10,
            call_mt_f/11,
            call_mt_t/4,
            call_mt_t/5,
            call_mt_t/6,
            call_mt_t/7,
            call_mt_t/8,
            call_mt_t/9,
            call_mt_t/10,
            call_mt_t/11,
            call_which_t/3,
            call_which_t/4,
            call_which_t/5,
            call_which_t/6,
            call_which_t/7,
            call_which_t/8,
            call_which_t/9,
            call_whichlist_t/3,
            callable_tf/2,
            convert_easy_strings/0,
            convert_easy_strings2/0,
            cyckb_t/1,
            cyckb_t/3,
            cyckb_t_call/1,
            cyckb_t_implies/2,
            cyckb_t_via_genlPreds/1,
            cyckb_t_via_implies/1,
            drain_assert_next_buffer/0,
            el_holds_DISABLED_KB/0,
            fix_sentence/2,
            get_assertions/2,
            get_b_dnf/2,
            get_dnf_props/6,
            get_props/4,
            get_varsp/2,
            hide_empty_strings/0,
            hide_term_rewrites/0,
            holds_f/1,
            holds_f/2,
            holds_f/3,
            holds_f/4,
            holds_f/5,
            holds_f/6,
            holds_f/7,
            holds_f/8,
            holds_f_p2/2,
            holds_relaxed_0_f/4,
            holds_relaxed_f/3,
            kb_f/1,
            kb_mt/2,
            kb_t/1,
            kb_t/3,
            kbp_t/1,
            kbp_t_list/1,
            kbp_t_list/2,
            kbp_t_list/3,
            kbp_t_list_0/3,
            kbp_t_list_1/3,
            kbp_t_list_prehook/2,
            kbp_to_mpred_0/0,
            kbp_to_mpred_no_more/0,
            kbp_to_mpred_t/0,
            link_to_holds/2,
            link_to_holds/3,
            link_to_holds2/2,
            link_to_holds2/3,
            link_to_holds_DYNAMIC/2,
            link_to_holds_list/2,
            
            move_implied/0,
            move_kb_assertions_matching/4,
            mpred_f/1,
            noGenlPreds/1,
            nv1000/1,
            print_sentence/1,
            proof_from_clause/3,
            prove_calllist/3,
            relax_term/6,
            tiny_kb_ASSERTION/2,
            which_f/1,
            with_el_holds_disabled/1,
            with_el_holds_enabled/1,
            with_kb_assertions_matching/3,
            write_assertions/0,
            xcall_f/1,
            xcall_f/2,
            xcall_f/3,
            xcall_f/4,
            xcall_f/5,
            xcall_f/6,
            xcall_f/7,
            xcall_f/8,
            xcall_f/9,
            xcall_f/10,
            xcall_t/1,
            xcall_t/2,
            xcall_t/3,
            xcall_t/4,
            xcall_t/5,
            xcall_t/6,
            xcall_t/7,
            xcall_t/8,
            xcall_t/9,
            xcall_t/10
          ]).
% :- use_module(logicmoo(util/logicmoo_util_preddefs)).
:- meta_predicate 
      % common_logic_kb_hooks
      call_f(*,1,?),
      % common_logic_kb_hooks
      call_f(*,4,?,?,?,?),
      % common_logic_kb_hooks
      call_f(*,6,?,?,?,?,?,?),
      % common_logic_kb_hooks
      call_mt_f(*,4,?,?,?,?),
      % common_logic_kb_hooks
      call_mt_f(*,6,?,?,?,?,?,?),
      % common_logic_kb_hooks
      call_mt_t(*,2,?,?),
      % common_logic_kb_hooks
      call_mt_t(*,3,?,?,?),
      % common_logic_kb_hooks
      call_mt_t(*,4,?,?,?,?),
      % common_logic_kb_hooks
      call_mt_t(*,5,?,?,?,?,?),
      % common_logic_kb_hooks
      call_mt_t(*,6,?,?,?,?,?,?),
      % common_logic_kb_hooks
      xcall_f(2,?,?),
      % common_logic_kb_hooks
      xcall_f(3,?,?,?),
      % common_logic_kb_hooks
      xcall_f(5,?,?,?,?,?).


:- meta_predicate xcall_f(0).
:- meta_predicate xcall_f(1,?).
:- meta_predicate xcall_f(2,?,?).
:- meta_predicate xcall_f(3,?,?,?).
:- meta_predicate xcall_f(4,?,?,?,?).
:- meta_predicate xcall_f(5,?,?,?,?,?).
:- meta_predicate xcall_f(6,?,?,?,?,?,?).
:- meta_predicate xcall_t(0).
:- meta_predicate xcall_t(1,?).
:- meta_predicate xcall_t(2,?,?).
:- meta_predicate xcall_t(3,?,?,?).
:- meta_predicate xcall_t(4,?,?,?,?).
:- meta_predicate xcall_t(5,?,?,?,?,?).
:- meta_predicate xcall_t(6,?,?,?,?,?,?).
:- meta_predicate call_f(?,1,?).
:- meta_predicate call_f(?,2,?,?).
:- meta_predicate call_f(?,3,?,?,?).
:- meta_predicate call_f(?,4,?,?,?,?).
:- meta_predicate call_f(?,5,?,?,?,?,?).
:- meta_predicate call_f(?,6,?,?,?,?,?,?).
:- meta_predicate call_mt_f(?,2,?,?).
:- meta_predicate call_mt_f(?,3,?,?,?).
:- meta_predicate call_mt_f(?,4,?,?,?,?).
:- meta_predicate call_mt_f(?,5,?,?,?,?,?).
:- meta_predicate call_mt_f(?,6,?,?,?,?,?,?).
:- meta_predicate call_mt_t(?,2,?,?).
:- meta_predicate call_mt_t(?,3,?,?,?).
:- meta_predicate call_mt_t(?,4,?,?,?,?).
:- meta_predicate call_mt_t(?,5,?,?,?,?,?).
:- meta_predicate call_mt_t(?,6,?,?,?,?,?,?).
:- meta_predicate call_which_t(?,1,?).
:- meta_predicate call_which_t(?,2,?,?).
:- meta_predicate call_which_t(?,3,?,?,?).
:- meta_predicate call_which_t(?,4,?,?,?,?).
:- meta_predicate call_which_t(?,5,?,?,?,?,?).
:- meta_predicate call_which_t(?,6,?,?,?,?,?,?).

:- meta_predicate holds_f(5,?,?,?,?,?).
:- meta_predicate holds_f(6,?,?,?,?,?,?).
:- meta_predicate holds_t(5,?,?,?,?,?).
:- meta_predicate holds_t(6,?,?,?,?,?,?).

:- include('../mpred/mpred_header.pi').
:-
            op(1150,fx,(was_dynamic)),
            op(1150,fx,(was_multifile)),
            op(1150,fy,(was_module_transparent)),
            op(1150,fx,(was_export)),
            op(1150,fx,(shared_multifile)).

/*
:- shared_multifile el_assertions:el_holds/4.
:- shared_multifile el_assertions:el_holds/5.
:- shared_multifile el_assertions:el_holds/6.
:- shared_multifile el_assertions:el_holds/7.
:- shared_multifile el_assertions:el_holds/8.
:- shared_multifile el_assertions:el_holds/9.
:- shared_multifile el_assertions:el_holds/10.
:- shared_multifile el_assertions:el_holds/11.
:- shared_multifile el_assertions:el_holds/12.
:- shared_multifile el_assertions:el_holds/13.
:- shared_multifile el_assertions:el_holds/14.



:- was_module_transparent   el_assertions:el_holds/4.
:- was_module_transparent   el_assertions:el_holds/5.
:- was_module_transparent   el_assertions:el_holds/6.
:- was_module_transparent   el_assertions:el_holds/7.
:- was_module_transparent   el_assertions:el_holds/8.
:- was_module_transparent   el_assertions:el_holds/9.
:- was_module_transparent   el_assertions:el_holds/10.
:- was_module_transparent   el_assertions:el_holds/11.
:- was_module_transparent   el_assertions:el_holds/12.
:- was_module_transparent   el_assertions:el_holds/13.
:- was_module_transparent   el_assertions:el_holds/14.




*/


:- meta_predicate call_f(*,2,?,?).
:- meta_predicate call_f(*,3,?,?,?).
:- meta_predicate call_f(*,5,?,?,?,?,?).
:- meta_predicate call_mt_f(*,2,?,?).
:- meta_predicate call_mt_f(*,3,?,?,?).
:- meta_predicate call_mt_f(*,5,?,?,?,?,?).
:- meta_predicate call_which_t(*,5,?,?,?,?,?).
:- meta_predicate call_which_t(*,6,?,?,?,?,?,?).
:- meta_predicate holds_f(1,?).
:- meta_predicate holds_f(5,?,?,?,?,?).
:- meta_predicate holds_f(6,?,?,?,?,?,?).
:- meta_predicate holds_relaxed_0_f(*,4,?,?).
:- meta_predicate xcall_f(0).
:- meta_predicate xcall_f(1,?).
:- meta_predicate xcall_f(4,?,?,?,?).
:- meta_predicate xcall_f(6,?,?,?,?,?,?).
:- meta_predicate xcall_t(0).
:- meta_predicate xcall_t(1,?).
:- meta_predicate xcall_t(2,?,?).
:- meta_predicate xcall_t(3,?,?,?).
:- meta_predicate xcall_t(4,?,?,?,?).
:- meta_predicate xcall_t(5,?,?,?,?,?).
:- meta_predicate xcall_t(6,?,?,?,?,?,?).


:- was_dynamic el_assertions:el_holds/4.
:- was_dynamic el_assertions:el_holds/5.
:- was_dynamic el_assertions:el_holds/6.
:- was_dynamic el_assertions:el_holds/7.
:- was_dynamic el_assertions:el_holds/8.
:- was_dynamic el_assertions:el_holds/9.
:- was_dynamic el_assertions:el_holds/10.
:- was_dynamic el_assertions:el_holds/11.
:- was_dynamic el_assertions:el_holds/12.
:- was_dynamic el_assertions:el_holds/13.
:- was_dynamic el_assertions:el_holds/14.

:- meta_predicate call_whichlist_t(?,0,?).
:- meta_predicate with_kb_assertions_matching(?,?,0).

:- shared_multifile el_assertions:el_holds_pred_impl/1.
:- shared_multifile el_assertions:is_cyckb_t_pred/2.
:- was_dynamic el_assertions:el_holds_pred_impl/1.
:- was_dynamic el_assertions:is_cyckb_t_pred/2.
:- dynamic cyckb_t/3.

:- was_export(kbp_t/1). 




%= 	 	 

%% kbp_t( ?VALUE1) is semidet.
%
% Knowledge Base P- True Stucture.
%
kbp_t(_):- not(loaded_external_kbs),!,fail.
% kbp_t(PLIST):- ground(PLIST),!,no_repeats(call_no_cuts(kbp_t_list_prehook(PLIST,PLISTO))),kbp_t_list(PLISTO).
% kbp_t(PLIST):- kbp_t_list_prehook(PLIST,PLISTO),kbp_t_list(PLISTO).
% TODO RE-ENABLE 
% kbp_t(PLIST):- kbp_t_list(PLIST). % append(PLIST,[_MT,_PROOF],PLISTO), apply(el_assertions:el_holds,PLISTO).  % el_assertions:el_holds has 2 extra args our callers shouldnt be forced to use.. but this is a big slowdown


:- was_dynamic(assertion_f/1).
:- was_export(kb_f/1).

%= 	 	 

%% kb_f( ?X) is semidet.
%
% Knowledge Base False.
%
kb_f(X):-assertion_f(X).



%= 	 	 

%% get_b_dnf( ?DNFA, ?DNFAO) is semidet.
%
% Get Backtackable Disjunctive Normal Form.
%
get_b_dnf([DNFA],DNFA):-!.
get_b_dnf(DNFA,DNFAO):- length(DNFA,L),atom_concat(and,L,Pred),!,DNFAO=..[Pred|DNFA].


%= 	 	 

%% get_dnf_props( ?TRUTH, ?VALUE2, ?DNFC, ?VARS, ?MT, :TermDNFCO) is semidet.
%
% Get Disjunctive Normal Form Props.
%
get_dnf_props(TRUTH,[],DNFC,VARS,MT,[dnf(c,DNFCO)|PROPS]):-!,get_props(TRUTH,VARS,MT,PROPS),get_b_dnf(DNFC,DNFCO).
get_dnf_props(TRUTH,DNFA,[],VARS,MT,[dnf(a,DNFAO)|PROPS]):-!,get_props(TRUTH,VARS,MT,PROPS),get_b_dnf(DNFA,DNFAO).
get_dnf_props(TRUTH,DNFA,DNFC,VARS,MT,[dnf(ca,(DNFCO<DNFAO))|PROPS]):-!,get_props(TRUTH,VARS,MT,PROPS),get_b_dnf(DNFC,DNFCO),get_b_dnf(DNFA,DNFAO).


%= 	 	 

%% get_props( ?TRUTH, ?VARS, ?VALUE3, :TermVARSP) is semidet.
%
% Get Props.
%
get_props(TRUTH,VARS,isMissing,VARSP):-!,get_props(TRUTH,VARS,notmissing,[_|VARSP]),!.
get_props(':TRUE-DEF',VARS,MT,[amt(MT),str(':DEFAULT'),truth(':TRUE')|VARSP]):-get_varsp(VARS,VARSP),!.
get_props(':FALSE-DEF',VARS,MT,[amt(MT),str(':DEFAULT'),truth(':FALSE')|VARSP]):-get_varsp(VARS,VARSP),!.
get_props(':TRUE-MON',VARS,MT,[amt(MT),str(':MONOTONIC'),truth(':TRUE')|VARSP]):-get_varsp(VARS,VARSP),!.
get_props(':FALSE-MON',VARS,MT,[amt(MT),str(':MONOTONIC'),truth(':FALSE')|VARSP]):-get_varsp(VARS,VARSP),!.

%= 	 	 

%% get_varsp( ?VARS, ?VARS) is semidet.
%
% Get Varsp.
%
get_varsp([],[]):-!.
get_varsp(VARS,[vars(VARS)]):-!.


%= 	 	 

%% tiny_kb_ASSERTION( ?PLIST, ?PROPS) is semidet.
%
% Tiny Knowledge Base Assertion.
%
tiny_kb_ASSERTION(_PLIST,_PROPS):-!,fail.
%MAYBE LATER tiny_kb_ASSERTION(PLIST,PROPS):- 'TINYKB-ASSERTION'(TRUTH,[DNFA,DNFC],MT,VARS,PLIST),get_dnf_props(TRUTH,DNFA,DNFC,VARS,MT,PROPS).
%MAYBE LATER tiny_kb_ASSERTION(PLIST,PROPS):- 'TINYKB-ASSERTION'(TRUTH,[DNFA,DNFC],MT,VARS,_HL,PLIST),get_dnf_props(TRUTH,DNFA,DNFC,VARS,MT,PROPS).

%big_kb_ASSERTION(PLIST,[dir(DIR),refcl(A1437)|PROPS]):- 'ASSERTION'(TRUTH, DNF, MT, VARS, A1437, DIR),dnf_to_pnf(DNF,PLIST),get_props(TRUTH,VARS,MT,PROPS).
%big_kb_ASSERTION(PLIST,[dir(DIR),refcl(A1437)|PROPS]):- 'ASSERTION'(TRUTH, _DNF, MT, VARS, A1437, DIR,_,PLIST),get_props(TRUTH,VARS,MT,PROPS).
%big_kb_ASSERTION(PLIST,[dir(DIR),refcl(A1437)|PROPS]):- 'ASSERTION'(TRUTH, _DNF, MT, VARS, A1437, DIR,PLIST),get_props(TRUTH,VARS,MT,PROPS).

%= 	 	 

%% big_kb_ASSERTION( ?VALUE1, ?VALUE2) is semidet.
%
% Big Knowledge Base Assertion.
%
big_kb_ASSERTION(_,_):-fail.

:- was_export(get_assertions/2).
% get_assertions(PLIST,PROPS):-big_kb_ASSERTION(PLISTIn,PROPS),nv1000(PLISTIn-PROPS),fix_sentence(PLISTIn,PLIST).

%= 	 	 

%% get_assertions( ?PLIST, ?PROPS) is semidet.
%
% Get Assertions.
%
get_assertions(PLIST,PROPS):-current_predicate(tiny_kb_ASSERTION/2),!,tiny_kb_ASSERTION(PLISTIn,PROPS),nv1000(PLISTIn-PROPS),fix_sentence(PLISTIn,PLIST).
get_assertions(PLIST,PROPS):-between(2,19,X),length(PLISTIn,X),kbp_t_list(PLISTIn,PROPS,_),nv1000(PLISTIn-PROPS),fix_sentence(PLISTIn,PLIST).



%= 	 	 

%% nv1000( ?S) is semidet.
%
% Nv Secondary Helper Primary Helper Primary Helper Primary Helper.
%
nv1000(S):-numbervars(S,100,_,[singletons(true),attvar(bind)]).


% length(SENT,N),N>1,append(SENT,[MT,Props],PLIST),apply(el_assertions:el_holds,PLIST),member(Var,SENT),var(Var).
% length(SENT,N),N>1,kbp_t_list(SENT,Proof),member(Var,SENT),var(Var).

:- was_export((kb_t/1)).

%= 	 	 

%% kb_t( ?Call) is semidet.
%
% Knowledge Base True Stucture.
%
kb_t(Call):- into_plist(Call,PLIST),[AH|LIST]=PLIST,!, kb_t(AH,LIST,PLIST).



%= 	 	 

%% kb_t( ?AH, ?VALUE2, ?PLIST) is semidet.
%
% Knowledge Base True Stucture.
%
kb_t(AH,_,PLIST):-var(AH),!,kbp_t(PLIST).
kb_t(t,PLIST,_):- !,kbp_t(PLIST).  % t is our versuion of '$holds' or call/N
kb_t(genls,PLIST,_):- !,kbp_t([genls|PLIST]). % rewrite hack for SUMO callers
kb_t(AH,PLIST,_):- is_holds_true(AH),!,kb_t(PLIST). % is_holds_true/1 is temp disabled for speed
kb_t(AH,PLIST,_):- is_holds_false(AH),!,kb_f(PLIST). % is_holds_false(not).
kb_t(_,_,PLIST):- kbp_t(PLIST).


:- was_export(link_to_holds2/2).

%= 	 	 

%% link_to_holds2( ?Pred, ?TargetPred) is semidet.
%
% Link Converted To Holds Extended Helper.
%
link_to_holds2(Pred,Target:TPred):- !,link_to_holds2(Pred,Target,TPred).
link_to_holds2(Pred,TargetPred):- !,link_to_holds2(Pred,user,TargetPred).


%= 	 	 

%% link_to_holds2( ?Pred, ?M, ?TargetPred) is semidet.
%
% Link Converted To Holds Extended Helper.
%
link_to_holds2(Pred,M,TargetPred):- 
  forall(between(2,12,X),((length(PLIST,X),append(PLIST,[_MT],PLISTMT),append(PLISTMT,[_PROOF],PLISTMTPROOF),         
         export(Pred/X),        
         % X2 is X + 2, nop(export(TargetPred/X2)),        
          A=..[Pred|PLIST],
          B=..[TargetPred|PLISTMTPROOF],              
         assertz_if_new((A :- (M:B) ))))).

:- was_export(link_to_holds/2).

%= 	 	 

%% link_to_holds( ?Pred, ?TargetPred) is semidet.
%
% Link Converted To Holds.
%
link_to_holds(Pred,Target:TPred):- !,link_to_holds(Pred,Target,TPred).
link_to_holds(Pred,TargetPred):- !,link_to_holds(Pred,user,TargetPred).

%= 	 	 

%% link_to_holds( ?Pred, ?M, ?TargetPred) is semidet.
%
% Link Converted To Holds.
%
link_to_holds(Pred,M,TargetPred):- 
  doall((between(2,12,X),length(PLIST,X),
         export(Pred/X),          
         nop(export(TargetPred/X)),          
          A=..[Pred|PLIST],
          B=..[TargetPred|PLIST],              
         assertz_if_new((A:- M:B)))).

:- was_export(link_to_holds_DYNAMIC/2).

%= 	 	 

%% link_to_holds_DYNAMIC( ?Pred, ?TargetPred) is semidet.
%
% Link Converted To Holds Dynamic.
%
link_to_holds_DYNAMIC(Pred,TargetPred):- 
  doall((between(2,12,X),length(PLIST,X),
         export(Pred/X),          
         export(TargetPred/X),          
          A=..[Pred|PLIST],
          B=..[TargetPred|PLIST],              
         assertz_if_new((A:-B)))).
:- was_export(link_to_holds_list/2).

%= 	 	 

%% link_to_holds_list( ?Pred, ?TargetPred) is semidet.
%
% Link Converted To Holds List.
%
link_to_holds_list(Pred,TargetPred):- 
  doall((between(2,12,X),length(PLIST,X),
         export(Pred/X),          
         export(TargetPred/1),          
          A=..[Pred|PLIST],
          B=..[TargetPred,PLIST],              
         assertz_if_new((A:-B)))).


/*
cyckb_t(P,A1,A2,A3,A4,A5,A6,A7):- t([P,A1,A2,A3,A4,A5,A6,A7]).
cyckb_t(P,A1,A2,A3,A4,A5,A6):- t([P,A1,A2,A3,A4,A5,A6]).
cyckb_t(P,A1,A2,A3,A4,A5):- t([P,A1,A2,A3,A4,A5]).
cyckb_t(P,A1,A2,A3,A4):- t([P,A1,A2,A3,A4]).
cyckb_t(P,A1,A2,A3):- t([P,A1,A2,A3]).
cyckb_t(P,A1,A2):- t([P,A1,A2]).
cyckb_t(P,A1):- t([P,A1]).
*/

:- was_dynamic(el_holds_DISABLED_KB/0).
:- was_export(el_holds_DISABLED_KB/0).
:- asserta(el_holds_DISABLED_KB).

:- meta_predicate(with_el_holds_enabled(0)).

%= 	 	 

%% with_el_holds_enabled( :GoalGoal) is semidet.
%
% Using El Holds Enabled.
%
with_el_holds_enabled(Goal):-wno_tl(el_holds_DISABLED_KB,Goal).
:- meta_predicate(with_el_holds_disabled(0)).

%= 	 	 

%% with_el_holds_disabled( :GoalGoal) is semidet.
%
% Using El Holds Disabled.
%
with_el_holds_disabled(Goal):-w_tl(el_holds_DISABLED_KB,Goal).

%:- link_to_holds_DYNAMIC(cyckb_t,el_holds_DISABLED_KB).
:- link_to_holds2(cyckb_t,el_assertions:el_holds).

:- was_export(cyckb_t/1).

%= 	 	 

%% cyckb_t( ?Compound) is semidet.
%
% Cyckb True Stucture.
%
cyckb_t([P|LIST]):-!, \+ (el_holds_DISABLED_KB), apply(cyckb_t,[P|LIST]).
cyckb_t(Compound):- \+ (el_holds_DISABLED_KB), Compound=..[F,A|List] , apply(cyckb_t,[F,A|List]).

:- was_export(noGenlPreds/1).

%= 	 	 

%% noGenlPreds( ?X) is semidet.
%
% No Genl Predicates.
%
noGenlPreds(coGenlPreds).
noGenlPreds(isa).
noGenlPreds(genls).
noGenlPreds(X):-not(atom(X)),!.
noGenlPreds(_).

:- link_to_holds_list(cyckb_t,cyckb_t_via_genlPreds).

%= 	 	 

%% cyckb_t_via_genlPreds( :TermGP) is semidet.
%
% Cyckb True Structure Via Genl Predicates.
%
cyckb_t_via_genlPreds([GP|_]):- noGenlPreds(GP),!,fail.
cyckb_t_via_genlPreds([GP,A,B]):- loop_check(cyckb_t(genlInverse,P,GP)), P\=GP, loop_check(cyckb_t([P,B,A])).
cyckb_t_via_genlPreds([GP|LIST]):- loop_check(cyckb_t(genlPreds,P,GP)), P\=GP, loop_check(cyckb_t([P|LIST])).


:- link_to_holds_list(cyckb_t,cyckb_t_via_implies).

%= 	 	 

%% cyckb_t_via_implies( ?CONSEQ) is semidet.
%
% Cyckb True Structure Via Implies.
%
cyckb_t_via_implies(CONSEQ):- fail, loop_check(cyckb_t_implies(ANTE,CONSEQ)), loop_check(cyckb_t_call(ANTE)).


%= 	 	 

%% cyckb_t_call( ?ANTE) is semidet.
%
% Cyckb True Structure Call.
%
cyckb_t_call(ANTE):- nop(cyckb_t_call(ANTE)),!,fail.

%= 	 	 

%% cyckb_t_implies( ?ANTE, ?CONSEQ) is semidet.
%
% Cyckb True Structure Implies.
%
cyckb_t_implies(ANTE,CONSEQ):- nop(cyckb_t_implies(ANTE,CONSEQ)),!,fail.

:- thread_local t_l:useDbase_t/0.


%= 	 	 

%% kbp_t_list_prehook( ?PLIST, ?PLIST) is semidet.
%
% Knowledge Base P- True Structure List Prehook.
%
kbp_t_list_prehook(PLIST,PLIST).

:- was_export(kbp_t_list/1). 

%= 	 	 

%% kbp_t_list( ?PLIST) is semidet.
%
% Knowledge Base P- True Structure List.
%
kbp_t_list(PLIST):- t_l:useDbase_t, t(PLIST).
kbp_t_list(PLIST):- apply(cyckb_t,PLIST).


:- was_export(kbp_t_list/2). 
% kbp_t_list(PLIST,t(PLIST)):- t_l:useDbase_t,  t(PLIST).

%= 	 	 

%% kbp_t_list( ?PLIST, ?Proof) is semidet.
%
% Knowledge Base P- True Structure List.
%
kbp_t_list(PLIST,Proof):- kbp_t_list(PLIST,_A,Proof).

% 
%  current_predicate(F/A),functor(P,F,A),predicate_property(P,number_of_clauses(N)),dif(B,true), clause(P, B, Ref),B\=(!,_), B=true.

:- was_export(kbp_t_list/3). 
kbp_t_list(PLIST,Props):- tiny_kb_ASSERTION(PLIST,Props).

%= 	 	 

%% kbp_t_list( ?PLIST, ?Props, ?Proof) is semidet.
%
% Knowledge Base P- True Structure List.
%
kbp_t_list(PLIST,[amt(t)],Proof):- t_l:useDbase_t,  CallList = [t|PLIST],Call=..CallList,/*Call,*/ clause(Call,true,Ref),clause(Head, Body, Ref),proof_from_clause(Head, Body, Proof).
kbp_t_list(PLIST,Props,Proof):- is_list(PLIST),!,kbp_t_list_1(PLIST,Props,Proof).
kbp_t_list(PLIST,Props,Proof):- kbp_t_list_0(PLIST,Props,Proof).


%= 	 	 

%% kbp_t_list_0( ?PLIST, ?Props, ?Proof) is semidet.
%
% Knowledge Base P- True Structure list  Primary Helper.
%
kbp_t_list_0(PLIST,Props,Proof):- between(3,2,N), length(PLIST,N),kbp_t_list_1(PLIST,Props,Proof).
kbp_t_list_0(PLIST,Props,Proof):- between(4,12,N), length(PLIST,N),kbp_t_list_1(PLIST,Props,Proof).


%= 	 	 

%% kbp_t_list_1( ?PLIST, :TermMT, ?Proof) is semidet.
%
% Knowledge Base P- True Structure list  Secondary Helper.
%
kbp_t_list_1(PLIST,[amt(MT)|PropsV], Proof):- append(PLIST,[MT,PropsV],CallList),!,prove_calllist(el_assertions:el_holds,CallList,Proof).
% kbp_t_list_1(PLIST,[cyckb_t], Proof):- CallList = [cyckb_t|PLIST],prove_calllist(cyckb_t,CallList,Proof).


%= 	 	 

%% prove_calllist( ?Functor, ?CallList, ?Proof) is semidet.
%
% Prove Calllist.
%
prove_calllist(Functor,CallList,Proof):- Call =.. [Functor|CallList], clause(Call, true,Ref),clause(PHead, PBody, Ref),proof_from_clause(PHead, PBody, Proof).
prove_calllist(Functor,CallList,Proof):- dif(Body,true), Head =.. [Functor|CallList],clause(Head, Body, Ref),must_det(not(Body=true)),Body,clause(PHead, PBody, Ref),proof_from_clause(PHead, PBody, Proof).

:- was_export(kb_mt/2).

%= 	 	 

%% kb_mt( ?C, ?MT) is semidet.
%
% Knowledge Base User Microtheory.
%
kb_mt(C,MT):- into_plist(C,PLIST),!,  append([el_assertions:el_holds|PLIST],[MT,_PropsV],CallList),Call=..CallList,Call.
kb_mt(C,t):- t_l:useDbase_t, t(C).



%= 	 	 

%% proof_from_clause( ?Head, ?VALUE2, ?Head) is semidet.
%
% Proof Converted From Clause.
%
proof_from_clause(Head, true, Head):-!.
proof_from_clause(Head, Body, ((Head:- Body))).

:- dynamic assert_next_queue/1.
:- export(assert_next_queue/1).

:- was_export(move_kb_assertions_matching/4).

%= 	 	 

%% move_kb_assertions_matching( ?PLIST, ?Match, ?Replace, ?Where) is semidet.
%
% Move Knowledge Base Assertions Matching.
%
move_kb_assertions_matching(PLIST,Match,Replace,Where):- 
%   dmsg(move_kb_assertions_matching(PLIST,Match,Replace,to(Where))),
        doall((kbp_t_list(PLIST,Call),
           forall(retract(Call),
           (subst(PLIST:Call,Match,Replace,NewPLIST:NewCall),
           assert_to_db_list(Where,[rewrite,NewPLIST,NewCall]))))).



%= 	 	 

%% assert_to_db_list( ?HOLDS, ?PLIST) is semidet.
%
% Assert Converted To Database List.
%
assert_to_db_list(HOLDS,PLIST):- safe_univ(Call,[HOLDS|PLIST]), assert(assert_next_queue(Call)).



%= 	 	 

%% with_kb_assertions_matching( ?PLIST, ?Proof, :GoalCall) is semidet.
%
% Using Knowledge Base Assertions Matching.
%
with_kb_assertions_matching(PLIST,Proof,Call):- doall((kbp_t_list(PLIST, Proof),Call)).
   
:- was_export(kbp_to_mpred_t/0).

%= 	 	 

%% kbp_to_mpred_t is semidet.
%
% Knowledge Base P- Converted To Managed Predicate True Stucture.
%
kbp_to_mpred_t:- must_det(w_tl(t_l:useOnlyExternalDBs,kbp_to_mpred_0)).


%= 	 	 

%% kbp_to_mpred_0 is semidet.
%
% Knowledge Base P- Converted To Managed Predicate  Primary Helper.
%
kbp_to_mpred_0:-!.
% kbp_to_mpred_0:- once(time_call(move_implied)),fail.
kbp_to_mpred_0:- once(time_call(hide_term_rewrites)),fail.
kbp_to_mpred_0:- once(time_call(hide_empty_strings)),fail.
% kbp_to_mpred_0:- once(time_call(convert_easy_strings)),fail.
% kbp_to_mpred_0:- once(time_call(convert_easy_strings2)),fail.
kbp_to_mpred_0:- time_call(drain_assert_next_buffer),!.


%= 	 	 

%% kbp_to_mpred_no_more is semidet.
%
% Knowledge Base P- Converted To Managed Predicate No More.
%
kbp_to_mpred_no_more:- forall((into_plist(_Call,PLIST),kbp_t(PLIST)),assert_to_db_list(_F,PLIST)),
   retractall(lmconf:use_cyc_database),tell('a.txt'),listing(t),listing('ASSERTION'),told,dmsg(done_mpred_t).


:- was_export(move_implied/0).

%= 	 	 

%% move_implied is semidet.
%
% Move Implied.
%
move_implied:-doall((between(2,6,Len),length(PLIST,Len), 
                     Call=..[assertion_holds,implied|PLIST],
                     retract(hl_holds:Call),
                     append(ALIST,[Last],PLIST),NewCall=..[assertion_holds,impliedBy,ALIST,Last],
                     assert(assert_next_queue(hl_holds:NewCall)))),
                     drain_assert_next_buffer.

:- was_export(hide_term_rewrites/0).

%= 	 	 

%% hide_term_rewrites is semidet.
%
% Hide Term Rewrites.
%
hide_term_rewrites :- w_tl(t_l:useOnlyExternalDBs,
 % remove badjuju from the KB (that is unbould slots in the middle of GAFs)
   % hl_holds:retractall(assertion_holds(isa, badjuju, 'Thing')),
   % hl_holds:retractall(el_assertions:el_holds(genls, badjuju, 'AerosolStuff',_,_)), 
   % hl_holds:retractall(assertion_holds(genls, badjuju, 'BiologicalAgentStuff')), 
 % the next few lines will cover the top
   doall((between(2,6,Len),length(PLIST,Len),
     forall(member(vvvar,PLIST),move_kb_assertions_matching(PLIST,vvvar,_,term_rewrites_kb))))),
   drain_assert_next_buffer.

:- was_export(hide_empty_strings/0).

%= 	 	 

%% hide_empty_strings is semidet.
%
% Hide Empty Strings.
%
hide_empty_strings :- w_tl(t_l:useOnlyExternalDBs,
 % remove more badjuju from the KB (that is unbould slots in the middle of GAFs)
 % the next few lines will cover the top
   doall((between(2,6,Len),length(PLIST,Len),
     forall(member('',PLIST),move_kb_assertions_matching(PLIST,'','',term_rewrites_kb))))),
   drain_assert_next_buffer.


:- was_export(convert_easy_strings/0).

%= 	 	 

%% convert_easy_strings is semidet.
%
% Convert Easy Strings.
%
convert_easy_strings:-
   doall((between(2,6,Len),length(PLIST,Len),
     forall(member(string(_),PLIST),
      time_call(with_kb_assertions_matching(PLIST,Proof,must_det(print_sentence(Proof))))))),drain_assert_next_buffer.


%= 	 	 

%% convert_easy_strings2 is semidet.
%
% Convert Easy Strings Extended Helper.
%
convert_easy_strings2:-
   doall((between(2,6,Len),length(PLIST,Len),
     forall(member([_|_],PLIST),
      time_call(with_kb_assertions_matching(PLIST,Proof,must_det(print_sentence(Proof))))))),drain_assert_next_buffer.


%= 	 	 

%% drain_assert_next_buffer is semidet.
%
% Drain Assert Next Buffer.
%
drain_assert_next_buffer:- predicate_property(assert_next_queue(_),number_of_clauses(CL)),dmsg(drain_assert_next_buffer(CL)),
   time_call(doall((retract(assert_next_queue(Call)),asserta_if_new(Call)))).


%= 	 	 

%% write_assertions is semidet.
%
% Write Assertions.
%
write_assertions:-
   tell(holds_all),
   listing(assertion_holds_mworld0),
   listing(assertion_holds),
   listing(term_rewrites_kb),
   told.



%= 	 	 

%% print_sentence( ?Proof) is semidet.
%
% Print Sentence.
%
print_sentence(Proof):- fix_sentence(Proof,New),!,ignore((Proof\=New,!,must_det(retract(Proof)),assert(assert_next_queue(New)))),!.



%= 	 	 

%% fix_sentence( ?X, ?X) is semidet.
%
% Fix Sentence.
%
fix_sentence(X,X).


%= 	 	 

%% relax_term( ?P, ?P, ?Aic, ?Aic, ?Bic, ?Bic) is semidet.
%
% Relax Term.
%
relax_term(P,P,Aic,Aic,Bic,Bic):- !.
/*
relax_term(P,P,A,A,Bi,Bc):- arg(_,v(genls,isa),P),!,fail.
relax_term(P,P,Ai,Ac,Bic,Bic):- when_met(nonvar(Ac), same_arg(same_or(isa),Ac,Ai)),!.
relax_term(P,P,Ai,Ac,Bi,Bc):- is_type(Ai),!,when_met(pred(nonvar,Ac), (same_arg(same_or(genls),Ac,Ai),same_arg(same_or(equals),Bc,Bi))),!.
relax_term(P,P,Ai,Ac,Bi,Bc):- when_met(pred(nonvar,Ac),when_met(pred(nonvar,Bc), (same_arg(same_or(genls),Ac,Ai),same_arg(same_or(equals),Bc,Bi)))).
*/

% ?- member(R,[a,b,c]),when_met(nonvar(Re), dbase:same_arg(same_or(termOfUnit),n,Re)),Re=R,write(chose(R)).



%= 	 	 

%% callable_tf( ?F, ?A) is semidet.
%
% Callable True/false.
%
callable_tf(P,2):- mpred_arity_pred(P),!,fail.
callable_tf(F,A):- functor_safe(P,F,A),predicate_property(P,_),!.



%= 	 	 

%% call_whichlist_t( ?UPARAM1, :GoalGOAL2, ?List) is semidet.
%
% Call Whichlist True Stucture.
%
call_whichlist_t(dac(d,_,_,_),CALL,_):- t(CALL).
call_whichlist_t(dac(_,a,_,_),_,List):- assertion_t(List).
call_whichlist_t(dac(_,_,c,_),CALL,_):- xcall_t(CALL).
call_whichlist_t(dac(_,_,_,holds_t),CALL,_):- holds_t(CALL).


%= 	 	 

%% call_which_t( ?DBS, ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7) is semidet.
%
% Call Which True Stucture.
%
call_which_t(DBS,P,A1,A2,A3,A4,A5,A6,A7):- callable_tf(P,7),List= [P,A1,A2,A3,A4,A5,A6,A7], CALL=..List, call_whichlist_t(DBS,CALL,List).
call_which_t(dac(_,_,_,h),P,A1,A2,A3,A4,A5,A6,A7):- holds_t(P,A1,A2,A3,A4,A5,A6,A7).


%= 	 	 

%% call_which_t( ?UPARAM1, :PRED6P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6) is semidet.
%
% Call Which True Stucture.
%
call_which_t(dac(d,_,_,_),P,A1,A2,A3,A4,A5,A6):- t(P,A1,A2,A3,A4,A5,A6).
call_which_t(dac(_,a,_,_),P,A1,A2,A3,A4,A5,A6):- assertion_t([P,A1,A2,A3,A4,A5,A6]).
call_which_t(dac(_,_,c,_),P,A1,A2,A3,A4,A5,A6):- callable_tf(P,6),xcall_t(P,A1,A2,A3,A4,A5,A6).
call_which_t(dac(_,_,_,holds_t),P,A1,A2,A3,A4,A5,A6):- holds_t(P,A1,A2,A3,A4,A5,A6).


%= 	 	 

%% call_which_t( ?UPARAM1, :PRED5P, ?A1, ?A2, ?A3, ?A4, ?A5) is semidet.
%
% Call Which True Stucture.
%
call_which_t(dac(d,_,_,_),P,A1,A2,A3,A4,A5):- t(P,A1,A2,A3,A4,A5).
call_which_t(dac(_,a,_,_),P,A1,A2,A3,A4,A5):- assertion_t([P,A1,A2,A3,A4,A5]).
call_which_t(dac(_,_,c,_),P,A1,A2,A3,A4,A5):- callable_tf(P,5),xcall_t(P,A1,A2,A3,A4,A5).
call_which_t(dac(_,_,_,holds_t),P,A1,A2,A3,A4,A5):- holds_t(P,A1,A2,A3,A4,A5).


%= 	 	 

%% call_which_t( ?UPARAM1, :PRED4P, ?A1, ?A2, ?A3, ?A4) is semidet.
%
% Call Which True Stucture.
%
call_which_t(dac(d,_,c,_),P,A1,A2,A3,A4):- t(P,A1,A2,A3,A4).
call_which_t(dac(_,a,_,_),P,A1,A2,A3,A4):- assertion_t([P,A1,A2,A3,A4]).
call_which_t(dac(_,_,c,_),P,A1,A2,A3,A4):- callable_tf(P,4),xcall_t(P,A1,A2,A3,A4).
call_which_t(dac(_,_,_,holds_t),P,A1,A2,A3,A4):- holds_t(P,A1,A2,A3,A4).


%= 	 	 

%% call_which_t( ?UPARAM1, :PRED3P, ?A1, ?A2, ?A3) is semidet.
%
% Call Which True Stucture.
%
call_which_t(dac(d,_,_,_),P,A1,A2,A3):- t(P,A1,A2,A3).
call_which_t(dac(_,a,_,_),P,A1,A2,A3):- assertion_t([P,A1,A2,A3]).
call_which_t(dac(_,_,c,_),P,A1,A2,A3):- callable_tf(P,3),xcall_t(P,A1,A2,A3).
call_which_t(dac(_,_,_,holds_t),P,A1,A2,A3):- holds_t(P,A1,A2,A3).


%= 	 	 

%% call_which_t( ?UPARAM1, :PRED2P, ?A1, ?A2) is semidet.
%
% Call Which True Stucture.
%
call_which_t(dac(d,_,_,_),P,A1,A2):- t(P,A1,A2).
call_which_t(dac(_,a,_,_),P,A1,A2):- assertion_t([P,A1,A2]).
call_which_t(dac(_,_,c,_),P,A1,A2):- callable_tf(P,2),xcall_t(P,A1,A2).
call_which_t(dac(_,_,_,holds_t),P,A1,A2):- holds_t(P,A1,A2).


%= 	 	 

%% call_which_t( ?UPARAM1, :PRED1P, ?A1) is semidet.
%
% Call Which True Stucture.
%
call_which_t(dac(d,_,_,_),P,A1):- t(P,A1).
call_which_t(dac(_,a,_,_),P,A1):- assertion_t([P,A1]).
call_which_t(dac(_,_,c,_),P,A1):- callable_tf(P,1),xcall_t(P,A1).
call_which_t(dac(_,_,_,holds_t),P,A1):- holds_t(P,A1).


%= 	 	 

%% call_mt_t( ?VALUE1, ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7, ?A8, ?A9) is semidet.
%
% Call User Microtheory True Stucture.
%
call_mt_t(dac(_,_,_,mt),P,A1,A2,A3,A4,A5,A6,A7,A8,A9):- callable_tf(P,9),CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8,A9],xcall_t(CALL).

%= 	 	 

%% call_mt_t( ?VALUE1, ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7, ?A8) is semidet.
%
% Call User Microtheory True Stucture.
%
call_mt_t(dac(_,_,_,mt),P,A1,A2,A3,A4,A5,A6,A7,A8):- callable_tf(P,8),CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8],xcall_t(CALL).

%= 	 	 

%% call_mt_t( ?VALUE1, ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7) is semidet.
%
% Call User Microtheory True Stucture.
%
call_mt_t(dac(_,_,_,mt),P,A1,A2,A3,A4,A5,A6,A7):- callable_tf(P,7),CALL=..[P,A1,A2,A3,A4,A5,A6,A7],xcall_t(CALL).

%= 	 	 

%% call_mt_t( ?UPARAM1, :PRED6P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6) is semidet.
%
% Call User Microtheory True Stucture.
%
call_mt_t(dac(_,_,_,mt),P,A1,A2,A3,A4,A5,A6):- callable_tf(P,6),xcall_t(P,A1,A2,A3,A4,A5,A6).

%= 	 	 

%% call_mt_t( ?UPARAM1, :PRED5P, ?A1, ?A2, ?A3, ?A4, ?A5) is semidet.
%
% Call User Microtheory True Stucture.
%
call_mt_t(dac(_,_,_,mt),P,A1,A2,A3,A4,A5):- callable_tf(P,5),xcall_t(P,A1,A2,A3,A4,A5).

%= 	 	 

%% call_mt_t( ?UPARAM1, :PRED4P, ?A1, ?A2, ?A3, ?A4) is semidet.
%
% Call User Microtheory True Stucture.
%
call_mt_t(dac(_,_,_,mt),P,A1,A2,A3,A4):- callable_tf(P,4),xcall_t(P,A1,A2,A3,A4).

%= 	 	 

%% call_mt_t( ?UPARAM1, :PRED3P, ?A1, ?A2, ?A3) is semidet.
%
% Call User Microtheory True Stucture.
%
call_mt_t(dac(_,_,_,mt),P,A1,A2,A3):- callable_tf(P,3),xcall_t(P,A1,A2,A3).

%= 	 	 

%% call_mt_t( ?UPARAM1, :PRED2P, ?A1, ?A2) is semidet.
%
% Call User Microtheory True Stucture.
%
call_mt_t(dac(_,_,_,mt),P,A1,A2):- callable_tf(P,3),xcall_t(P,A1,A2).


%= 	 	 

%% xcall_t( ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7, ?A8, ?A9) is semidet.
%
% Extended Call True Stucture.
%
xcall_t(P,A1,A2,A3,A4,A5,A6,A7,A8,A9):- CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8,A9],call(CALL).

%= 	 	 

%% xcall_t( ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7, ?A8) is semidet.
%
% Extended Call True Stucture.
%
xcall_t(P,A1,A2,A3,A4,A5,A6,A7,A8):- CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8],call(CALL).

%= 	 	 

%% xcall_t( ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7) is semidet.
%
% Extended Call True Stucture.
%
xcall_t(P,A1,A2,A3,A4,A5,A6,A7):- CALL=..[P,A1,A2,A3,A4,A5,A6,A7],call(CALL).

%= 	 	 

%% xcall_t( :PRED6P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6) is semidet.
%
% Extended Call True Stucture.
%
xcall_t(P,A1,A2,A3,A4,A5,A6):- call(P,A1,A2,A3,A4,A5,A6).

%= 	 	 

%% xcall_t( :PRED5P, ?A1, ?A2, ?A3, ?A4, ?A5) is semidet.
%
% Extended Call True Stucture.
%
xcall_t(P,A1,A2,A3,A4,A5):- call(P,A1,A2,A3,A4,A5).

%= 	 	 

%% xcall_t( :PRED4P, ?A1, ?A2, ?A3, ?A4) is semidet.
%
% Extended Call True Stucture.
%
xcall_t(P,A1,A2,A3,A4):- call(P,A1,A2,A3,A4).

%= 	 	 

%% xcall_t( :PRED3P, ?A1, ?A2, ?A3) is semidet.
%
% Extended Call True Stucture.
%
xcall_t(P,A1,A2,A3):- call(P,A1,A2,A3).

%= 	 	 

%% xcall_t( :PRED2P, ?A1, ?A2) is semidet.
%
% Extended Call True Stucture.
%
xcall_t(P,A1,A2):- call(P,A1,A2).

%= 	 	 

%% xcall_t( :PRED1P, ?A1) is semidet.
%
% Extended Call True Stucture.
%
xcall_t(P,A1):- call(P,A1).

%= 	 	 

%% xcall_t( :GoalP) is semidet.
%
% Extended Call True Stucture.
%
xcall_t(P):- call(P).

% todo hook into loaded files!
:- was_export(assertion_t/1).

% assertion_t(Call):- t_l:useOnlyExternalDBs,!,lmconf:use_cyc_database,wno_tl(t_l:useOnlyExternalDBs,kb_t(Call)).

%= 	 	 

%% assertion_t( ?Call) is semidet.
%
% Assertion True Stucture.
%
assertion_t(Call):- lmconf:use_cyc_database,!,w_tl(t_l:useOnlyExternalDBs,kb_t(Call)).
% assertion_t(Call):- w_tl(t_l:useOnlyExternalDBs,loop_check(req(Call))).

% ================================================================================
% end holds_t
% ================================================================================

% % :- use_module(logicmoo(plarkc/mpred_cyc_kb)).

% ================================================================================
% begin holds_f
% ================================================================================

%= 	 	 

%% which_f( ?VALUE1) is semidet.
%
% Which False.
%
which_f(dac(d,no_a,no_c,no_mt)).


%= 	 	 

%% holds_f( ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7) is semidet.
%
% Holds False.
%
holds_f(P,A1,A2,A3,A4,A5,A6,A7):- isCycPredArity_ignoreable(P,7),which_f(DBS),(call_f(DBS,P,A1,A2,A3,A4,A5,A6,A7);call_mt_f(DBS,P,A1,A2,A3,A4,A5,A6,A7,_,_);assertion_f([P,A1,A2,A3,A4,A5,A6,A7])).

%= 	 	 

%% holds_f( :PRED6P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6) is semidet.
%
% Holds False.
%
holds_f(P,A1,A2,A3,A4,A5,A6):- isCycPredArity_ignoreable(P,6),which_f(DBS),(call_f(DBS,P,A1,A2,A3,A4,A5,A6);call_mt_f(DBS,P,A1,A2,A3,A4,A5,A6,_,_)).

%= 	 	 

%% holds_f( :PRED5P, ?A1, ?A2, ?A3, ?A4, ?A5) is semidet.
%
% Holds False.
%
holds_f(P,A1,A2,A3,A4,A5):- isCycPredArity_ignoreable(P,5),which_f(DBS),(call_f(DBS,P,A1,A2,A3,A4,A5);call_mt_f(DBS,P,A1,A2,A3,A4,A5,_,_)).

%= 	 	 

%% holds_f( ?P, ?A1, ?A2, ?A3, ?A4) is semidet.
%
% Holds False.
%
holds_f(P,A1,A2,A3,A4):- isCycPredArity_ignoreable(P,4),which_f(DBS),(call_f(DBS,P,A1,A2,A3,A4);call_mt_f(DBS,P,A1,A2,A3,A4,_,_)).

%= 	 	 

%% holds_f( ?P, ?A1, ?A2, ?A3) is semidet.
%
% Holds False.
%
holds_f(P,A1,A2,A3):- isCycPredArity_ignoreable(P,3),which_f(DBS),(call_f(DBS,P,A1,A2,A3);call_mt_f(DBS,P,A1,A2,A3,_,_)).

%= 	 	 

%% holds_f( ?P, ?A1, ?A2) is semidet.
%
% Holds False.
%
holds_f(P,A1,A2):- holds_relaxed_f(P,A1,A2).

%= 	 	 

%% holds_f( :PRED1P, ?A1) is semidet.
%
% Holds False.
%
holds_f(P,A1):- isCycPredArity_ignoreable(P,1),which_f(DBS),(call_f(DBS,P,A1);call_mt_f(DBS,P,A1,_,_)).



%= 	 	 

%% holds_relaxed_f( ?P, ?A1, ?A2) is semidet.
%
% Holds Relaxed False.
%
holds_relaxed_f(P,A1,A2):- isCycPredArity_ignoreable(P,2),which_f(DBS),!,relax_term(P,PR,A1,R1,A2,R2),holds_relaxed_0_f(DBS,PR,R1,R2).

%= 	 	 

%% holds_relaxed_0_f( ?DBS, :PRED4P, ?A1, ?A2) is semidet.
%
% holds relaxed  Primary Helper False.
%
holds_relaxed_0_f(DBS,P,A1,A2):- call_f(DBS,P,A1,A2).
holds_relaxed_0_f(DBS,P,A1,A2):- call_mt_f(DBS,P,A1,A2,_,_).



%= 	 	 

%% holds_f( :TermCALL) is semidet.
%
% Holds False.
%
holds_f([AH,P|LIST]):- is_holds_true(AH),!,holds_f_p2(P,LIST).
holds_f([AH,P|LIST]):- is_holds_false(AH),!,holds_plist_t(P,LIST).
holds_f([P|LIST]):- !, holds_f_p2(P,LIST).
holds_f(CALL):- CALL=..[P|LIST],holds_f([P|LIST]).

%= 	 	 

%% holds_f_p2( ?P, ?LIST) is semidet.
%
% Holds Functor Pred Extended Helper.
%
holds_f_p2(P,LIST):- CALL=..[holds_f,P|LIST],call(CALL).


%= 	 	 

%% mpred_f( ?List) is semidet.
%
% Managed Predicate False.
%
mpred_f(List):- is_list(List),!,Call=..[mpred_f|List],Call.
mpred_f(List):- holds_f(List).



%= 	 	 

%% call_f( ?VALUE1, ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7) is semidet.
%
% Call False.
%
call_f(_,P,A1,A2,A3,A4,A5,A6,A7):- callable_tf(P,7),List= [P,A1,A2,A3,A4,A5,A6,A7], CALL=..List,(assertion_f(List);mpred_f(CALL);xcall_f(CALL)).

%= 	 	 

%% call_f( ?UPARAM1, :PRED6P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6) is semidet.
%
% Call False.
%
call_f(dac(d,_,_,_),P,A1,A2,A3,A4,A5,A6):- mpred_f(P,A1,A2,A3,A4,A5,A6).
call_f(dac(_,a,_,_),P,A1,A2,A3,A4,A5,A6):- assertion_f([P,A1,A2,A3,A4,A5,A6]).
call_f(dac(_,_,c,_),P,A1,A2,A3,A4,A5,A6):- callable_tf(P,6),xcall_f(P,A1,A2,A3,A4,A5,A6).

%= 	 	 

%% call_f( ?UPARAM1, :PRED5P, ?A1, ?A2, ?A3, ?A4, ?A5) is semidet.
%
% Call False.
%
call_f(dac(d,_,_,_),P,A1,A2,A3,A4,A5):- mpred_f(P,A1,A2,A3,A4,A5).
call_f(dac(_,a,_,_),P,A1,A2,A3,A4,A5):- assertion_f([P,A1,A2,A3,A4,A5]).
call_f(dac(_,_,c,_),P,A1,A2,A3,A4,A5):- callable_tf(P,5),xcall_f(P,A1,A2,A3,A4,A5).

%= 	 	 

%% call_f( ?UPARAM1, :PRED4P, ?A1, ?A2, ?A3, ?A4) is semidet.
%
% Call False.
%
call_f(dac(d,_,_,_),P,A1,A2,A3,A4):- mpred_f(P,A1,A2,A3,A4).
call_f(dac(_,a,_,_),P,A1,A2,A3,A4):- assertion_f([P,A1,A2,A3,A4]).
call_f(dac(_,_,c,_),P,A1,A2,A3,A4):- callable_tf(P,4),xcall_f(P,A1,A2,A3,A4).

%= 	 	 

%% call_f( ?UPARAM1, :PRED3P, ?A1, ?A2, ?A3) is semidet.
%
% Call False.
%
call_f(dac(d,_,_,_),P,A1,A2,A3):- mpred_f(P,A1,A2,A3).
call_f(dac(_,a,_,_),P,A1,A2,A3):- assertion_f([P,A1,A2,A3]).
call_f(dac(_,_,c,_),P,A1,A2,A3):- callable_tf(P,3),xcall_f(P,A1,A2,A3).

%= 	 	 

%% call_f( ?UPARAM1, :PRED2P, ?A1, ?A2) is semidet.
%
% Call False.
%
call_f(dac(d,_,_,_),P,A1,A2):- mpred_f(P,A1,A2).
call_f(dac(_,a,_,_),P,A1,A2):- assertion_f([P,A1,A2]).
call_f(dac(_,_,c,_),P,A1,A2):- callable_tf(P,2),xcall_f(P,A1,A2).

%= 	 	 

%% call_f( ?UPARAM1, :PRED1P, ?A1) is semidet.
%
% Call False.
%
call_f(dac(d,_,_,_),P,A1):- mpred_f(P,A1).
call_f(dac(_,a,_,_),P,A1):- assertion_f([P,A1]).
call_f(dac(_,_,c,_),P,A1):- callable_tf(P,1),xcall_f(P,A1).


%= 	 	 

%% call_mt_f( ?VALUE1, ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7, ?A8, ?A9) is semidet.
%
% Call User Microtheory False.
%
call_mt_f(dac(_,_,_,mt),P,A1,A2,A3,A4,A5,A6,A7,A8,A9):- callable_tf(P,9),CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8,A9],xcall_f(CALL).

%= 	 	 

%% call_mt_f( ?VALUE1, ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7, ?A8) is semidet.
%
% Call User Microtheory False.
%
call_mt_f(dac(_,_,_,mt),P,A1,A2,A3,A4,A5,A6,A7,A8):- callable_tf(P,8),CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8],xcall_f(CALL).

%= 	 	 

%% call_mt_f( ?VALUE1, ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7) is semidet.
%
% Call User Microtheory False.
%
call_mt_f(dac(_,_,_,mt),P,A1,A2,A3,A4,A5,A6,A7):- callable_tf(P,7),CALL=..[P,A1,A2,A3,A4,A5,A6,A7],xcall_f(CALL).

%= 	 	 

%% call_mt_f( ?UPARAM1, :PRED6P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6) is semidet.
%
% Call User Microtheory False.
%
call_mt_f(dac(_,_,_,mt),P,A1,A2,A3,A4,A5,A6):- callable_tf(P,6),xcall_f(P,A1,A2,A3,A4,A5,A6).

%= 	 	 

%% call_mt_f( ?UPARAM1, :PRED5P, ?A1, ?A2, ?A3, ?A4, ?A5) is semidet.
%
% Call User Microtheory False.
%
call_mt_f(dac(_,_,_,mt),P,A1,A2,A3,A4,A5):- callable_tf(P,5),xcall_f(P,A1,A2,A3,A4,A5).

%= 	 	 

%% call_mt_f( ?UPARAM1, :PRED4P, ?A1, ?A2, ?A3, ?A4) is semidet.
%
% Call User Microtheory False.
%
call_mt_f(dac(_,_,_,mt),P,A1,A2,A3,A4):- callable_tf(P,4),xcall_f(P,A1,A2,A3,A4).

%= 	 	 

%% call_mt_f( ?UPARAM1, :PRED3P, ?A1, ?A2, ?A3) is semidet.
%
% Call User Microtheory False.
%
call_mt_f(dac(_,_,_,mt),P,A1,A2,A3):- callable_tf(P,3),xcall_f(P,A1,A2,A3).

%= 	 	 

%% call_mt_f( ?UPARAM1, :PRED2P, ?A1, ?A2) is semidet.
%
% Call User Microtheory False.
%
call_mt_f(dac(_,_,_,mt),P,A1,A2):- callable_tf(P,2),xcall_f(P,A1,A2).


%= 	 	 

%% xcall_f( ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7, ?A8, ?A9) is semidet.
%
% Extended Call False.
%
xcall_f(P,A1,A2,A3,A4,A5,A6,A7,A8,A9):- CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8,A9],\+ xcall_t(CALL).

%= 	 	 

%% xcall_f( ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7, ?A8) is semidet.
%
% Extended Call False.
%
xcall_f(P,A1,A2,A3,A4,A5,A6,A7,A8):- CALL=..[P,A1,A2,A3,A4,A5,A6,A7,A8],\+ xcall_t(CALL).

%= 	 	 

%% xcall_f( ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7) is semidet.
%
% Extended Call False.
%
xcall_f(P,A1,A2,A3,A4,A5,A6,A7):- CALL=..[P,A1,A2,A3,A4,A5,A6,A7],\+ xcall_t(CALL).

%= 	 	 

%% xcall_f( :PRED6P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6) is semidet.
%
% Extended Call False.
%
xcall_f(P,A1,A2,A3,A4,A5,A6):- \+ xcall_t(P,A1,A2,A3,A4,A5,A6).

%= 	 	 

%% xcall_f( :PRED5P, ?A1, ?A2, ?A3, ?A4, ?A5) is semidet.
%
% Extended Call False.
%
xcall_f(P,A1,A2,A3,A4,A5):- \+ xcall_t(P,A1,A2,A3,A4,A5).

%= 	 	 

%% xcall_f( :PRED4P, ?A1, ?A2, ?A3, ?A4) is semidet.
%
% Extended Call False.
%
xcall_f(P,A1,A2,A3,A4):- \+ xcall_t(P,A1,A2,A3,A4).

%= 	 	 

%% xcall_f( :PRED3P, ?A1, ?A2, ?A3) is semidet.
%
% Extended Call False.
%
xcall_f(P,A1,A2,A3):- \+ xcall_t(P,A1,A2,A3).

%= 	 	 

%% xcall_f( :PRED2P, ?A1, ?A2) is semidet.
%
% Extended Call False.
%
xcall_f(P,A1,A2):- \+ xcall_t(P,A1,A2).

%= 	 	 

%% xcall_f( :PRED1P, ?A1) is semidet.
%
% Extended Call False.
%
xcall_f(P,A1):- \+ xcall_t(P,A1).

%= 	 	 

%% xcall_f( :GoalP) is semidet.
%
% Extended Call False.
%
xcall_f(P):- \+ xcall_t(P).


%= 	 	 

%% assertion_f( :TermAH) is semidet.
%
% Assertion False.
%
assertion_f([AH,P|LIST]):- is_holds_true(AH),!,assertion_f([P|LIST]).
assertion_f([AH,P|LIST]):- is_holds_false(AH),!,assertion_f([P|LIST]).
% todo hook into loaded files!
assertion_f(_):- not(loaded_external_kbs),!,fail.
%MAYBE LATER assertion_f([P|LIST]):- 'TINYKB-ASSERTION'(':FALSE-DEF',_,_UniversalVocabularyMt,_Vars,/*HL*/[P|LIST]).
%MAYBE LATER assertion_f([P|LIST]):- 'TINYKB-ASSERTION'(':FALSE-MON',_,_UniversalVocabularyMt,_Vars,/*HL*/[P|LIST]).


% ================================================================================
% end holds_f 
% ================================================================================


