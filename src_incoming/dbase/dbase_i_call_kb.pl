

:-module(dbase_i_call_kb,[]).



:- export(kb_f/1).
kb_f(X):-assertion_f(X).


get_props(TRUTH,VARS,missing,VARSP):-!,get_props(TRUTH,VARS,notmissing,[_|VARSP]),!.
get_props(':TRUE-DEF',VARS,MT,[amt(MT)|VARSP]):-get_varsp(VARS,VARSP),!.
get_props(':FALSE-DEF',VARS,MT,[amt(MT),truth(':FALSE')|VARSP]):-get_varsp(VARS,VARSP),!.
get_props(':TRUE-MON',VARS,MT,[amt(MT),str(':MONOTONIC')|VARSP]):-get_varsp(VARS,VARSP),!.
get_props(':FALSE-MON',VARS,MT,[amt(MT),str(':MONOTONIC'),truth(':FALSE')|VARSP]):-get_varsp(VARS,VARSP),!.
get_varsp([],[]):-!.
get_varsp(VARS,[vars(VARS)]):-!.

tiny_kb_ASSERTION(PLIST,PROPS):- 'TINYKB-ASSERTION'(TRUTH,_,MT,VARS,PLIST),get_props(TRUTH,VARS,MT,PROPS).
tiny_kb_ASSERTION(PLIST,PROPS):- 'TINYKB-ASSERTION'(TRUTH,_,MT,VARS,_,PLIST),get_props(TRUTH,VARS,MT,PROPS).

big_kb_ASSERTION(PLIST,[dir(DIR),refcl(A1437)|PROPS]):- 'ASSERTION'(TRUTH, DNF, MT, VARS, A1437, DIR),dnf_to_pnf(DNF,PLIST),get_props(TRUTH,VARS,MT,PROPS).
big_kb_ASSERTION(PLIST,[dir(DIR),refcl(A1437)|PROPS]):- 'ASSERTION'(TRUTH, _DNF, MT, VARS, A1437, DIR,_,PLIST),get_props(TRUTH,VARS,MT,PROPS).
big_kb_ASSERTION(PLIST,[dir(DIR),refcl(A1437)|PROPS]):- 'ASSERTION'(TRUTH, _DNF, MT, VARS, A1437, DIR,PLIST),get_props(TRUTH,VARS,MT,PROPS).

:-export(get_assertions/2).
% get_assertions(PLIST,PROPS):-big_kb_ASSERTION(PLISTIn,PROPS),nv1000(PLISTIn-PROPS),fix_sentence(PLISTIn,PLIST).
get_assertions(PLIST,PROPS):-tiny_kb_ASSERTION(PLISTIn,PROPS),nv1000(PLISTIn-PROPS),fix_sentence(PLISTIn,PLIST).
get_assertions(PLIST,PROPS):-between(2,19,X),length(PLISTIn,X),kbp_t_list(PLISTIn,PROPS,_),nv1000(PLISTIn-PROPS),fix_sentence(PLISTIn,PLIST).

nv1000(S):-numbervars(S,100,_,[singletons(true),attvar(bind)]).


% length(SENT,N),N>1,append(SENT,[MT,Props],PLIST),apply(el_holds,PLIST),member(Var,SENT),var(Var).
% length(SENT,N),N>1,kbp_t_list(SENT,Proof),member(Var,SENT),var(Var).

:-export((kb_t/1)).
kb_t(Call):- into_plist(Call,PLIST),[AH|LIST]=PLIST,!, kb_t(AH,LIST,PLIST).

:-export(into_plist/2).
into_plist(PLIST,PLIST):- var(PLIST),!,between(2,12,X),length(PLIST,X).
into_plist([P|LIST],[P|LIST]):-var(P),!.
into_plist([dbase_t|PLIST],PLIST).  % dbase_t is our versuion of '$holds' or call/N
into_plist(plist(P,LIST),[P|LIST]).
into_plist(Call,PLIST):-Call=..PLIST. % finally the fallthrue

kb_t(AH,_,PLIST):-var(AH),!,kbp_t(PLIST).
kb_t(dbase_t,PLIST,_):- !,kbp_t(PLIST).  % dbase_t is our versuion of '$holds' or call/N
kb_t(subclass,PLIST,_):- !,kbp_t([genls|PLIST]). % rewrite hack for SUMO callers
kb_t(AH,PLIST,_):- is_holds_true(AH),!,kb_t(PLIST). % is_holds_true/1 is temp disabled for speed
kb_t(AH,PLIST,_):- is_holds_false(AH),!,kb_f(PLIST). % is_holds_false(not).
kb_t(_,_,PLIST):- kbp_t(PLIST).

:-export(kbp_t/1). 

kbp_t(_):- not(loaded_external_kbs),!,fail.
% kbp_t(PLIST):- ground(PLIST),!,no_repeats(call_no_cuts(hook:kbp_t_list_prehook(PLIST,PLISTO))),kbp_t_list(PLISTO).
% kbp_t(PLIST):- hook:kbp_t_list_prehook(PLIST,PLISTO),kbp_t_list(PLISTO).
kbp_t(PLIST):- kbp_t_list(PLIST). % append(PLIST,[_MT,_PROOF],PLISTO), apply(el_holds,PLISTO).  % el_holds has 2 extra args our callers shouldnt be forced to use.. but this is a big slowdown


:-export(link_to_holds2/2).
link_to_holds2(Pred,TargetPred):- 
  doall((between(2,12,X),length(PLIST,X),append(PLIST,[_MT],PLISTMT),append(PLISTMT,[_PROOF],PLISTMTPROOF),
         X2 is X + 2,
         dynamic_multifile_exported(Pred/X),        
         nop(dynamic_multifile_exported(TargetPred/X2)),        
          A=..[Pred|PLIST],
          B=..[TargetPred|PLISTMTPROOF],              
         assertz_if_new((A:-B)))).

:-export(link_to_holds/2).
link_to_holds(Pred,TargetPred):- 
  doall((between(2,12,X),length(PLIST,X),
         dynamic_multifile_exported(Pred/X),          
         nop(dynamic_multifile_exported(TargetPred/X)),          
          A=..[Pred|PLIST],
          B=..[TargetPred|PLIST],              
         assertz_if_new((A:-B)))).

:-export(link_to_holds_list/2).
link_to_holds_list(Pred,TargetPred):- 
  doall((between(2,12,X),length(PLIST,X),
         dynamic_multifile_exported(Pred/X),          
         dynamic_multifile_exported(TargetPred/1),          
          A=..[Pred|PLIST],
          B=..[TargetPred,PLIST],              
         assertz_if_new((A:-B)))).

/*
cyckb_t(P,A1,A2,A3,A4,A5,A6,A7):- el_holds(P,A1,A2,A3,A4,A5,A6,A7,_,_).
cyckb_t(P,A1,A2,A3,A4,A5,A6,A7):- dbase_t([P,A1,A2,A3,A4,A5,A6,A7]).
cyckb_t(P,A1,A2,A3,A4,A5,A6):- el_holds(P,A1,A2,A3,A4,A5,A6,_,_).
cyckb_t(P,A1,A2,A3,A4,A5,A6):- dbase_t([P,A1,A2,A3,A4,A5,A6]).
cyckb_t(P,A1,A2,A3,A4,A5):-el_holds(P,A1,A2,A3,A4,A5,_,_).
cyckb_t(P,A1,A2,A3,A4,A5):- dbase_t([P,A1,A2,A3,A4,A5]).
cyckb_t(P,A1,A2,A3,A4):- el_holds(P,A1,A2,A3,A4,_,_).
cyckb_t(P,A1,A2,A3,A4):- dbase_t([P,A1,A2,A3,A4]).
cyckb_t(P,A1,A2,A3):- el_holds(P,A1,A2,A3,_,_).
cyckb_t(P,A1,A2,A3):- dbase_t([P,A1,A2,A3]).
cyckb_t(P,A1,A2):- el_holds(P,A1,A2,_,_).
cyckb_t(P,A1,A2):- dbase_t([P,A1,A2]).
cyckb_t(P,A1):- el_holds(P,A1,_,_).
cyckb_t(P,A1):- dbase_t([P,A1]).
*/

:- link_to_holds2(cyckb_t,el_holds).

:-export(cyckb_t/1).
cyckb_t(PLIST):- apply(cyckb_t,PLIST).

:-export(noGenlPreds/1).
noGenlPreds(coGenlPreds).
noGenlPreds(isa).
noGenlPreds(genls).
noGenlPreds(X):-not(atom(X)),!.
noGenlPreds(_).

:- link_to_holds_list(cyckb_t,cyckb_t_via_genlPreds).
cyckb_t_via_genlPreds([GP|_]):- noGenlPreds(GP),!,fail.
cyckb_t_via_genlPreds([GP,A,B]):- loop_check(cyckb_t(genlInverse,P,GP)), P\=GP, loop_check(cyckb_t([P,B,A])).
cyckb_t_via_genlPreds([GP|LIST]):- loop_check(cyckb_t(genlPreds,P,GP)), P\=GP, loop_check(cyckb_t([P|LIST])).


:- link_to_holds_list(cyckb_t,cyckb_t_via_implies).
cyckb_t_via_implies(CONSEQ):- fail, loop_check(cyckb_t_implies(ANTE,CONSEQ)), loop_check(cyckb_t_call(ANTE)).

cyckb_t_call(ANTE):- nop(cyckb_t_call(ANTE)),!,fail.
cyckb_t_implies(ANTE,CONSEQ):- nop(cyckb_t_implies(ANTE,CONSEQ)),!,fail.

:-thread_local thlocal:useDbase_t/0.

hook:kbp_t_list_prehook(PLIST,PLIST).

:-export(kbp_t_list/1). 
kbp_t_list(PLIST):- thlocal:useDbase_t, dbase_t(PLIST).
kbp_t_list(PLIST):- apply(cyckb_t,PLIST).


:-export(kbp_t_list/2). 
% kbp_t_list(PLIST,dbase_t(PLIST)):- thlocal:useDbase_t,  dbase_t(PLIST).
kbp_t_list(PLIST,Proof):- kbp_t_list(PLIST,_,Proof).

% 
%  current_predicate(F/A),functor(P,F,A),predicate_property(P,number_of_clauses(N)),dif(B,true), clause(P, B, Ref),B\=(!,_), B=true.

:-export(kbp_t_list/3). 
% kbp_t_list(PLIST):- tiny_kb_ASSERTION(PLIST).
kbp_t_list(PLIST,[amt(dbase_t)],Proof):- thlocal:useDbase_t,  CallList = [dbase_t|PLIST],Call=..CallList,/*Call,*/ clause(Call,true,Ref),clause(Head, Body, Ref),proof_from_clause(Head, Body, Proof).
kbp_t_list(PLIST,Props,Proof):- is_list(PLIST),!,kbp_t_list_1(PLIST,Props,Proof).
kbp_t_list(PLIST,Props,Proof):- kbp_t_list_0(PLIST,Props,Proof).

kbp_t_list_0(PLIST,Props,Proof):- between(3,2,N), length(PLIST,N),kbp_t_list_1(PLIST,Props,Proof).
kbp_t_list_0(PLIST,Props,Proof):- between(4,12,N), length(PLIST,N),kbp_t_list_1(PLIST,Props,Proof).

kbp_t_list_1(PLIST,[amt(MT)|PropsV], Proof):- append(PLIST,[MT,PropsV],CallList),!,prove_calllist(el_holds,CallList,Proof).
% kbp_t_list_1(PLIST,[cyckb_t], Proof):- CallList = [cyckb_t|PLIST],prove_calllist(cyckb_t,CallList,Proof).

prove_calllist(Functor,CallList,Proof):- Call =.. [Functor|CallList], clause(Call, true,Ref),clause(PHead, PBody, Ref),proof_from_clause(PHead, PBody, Proof).
prove_calllist(Functor,CallList,Proof):- dif(Body,true), Head =.. [Functor|CallList],clause(Head, Body, Ref),must_det(not(Body=true)),Body,clause(PHead, PBody, Ref),proof_from_clause(PHead, PBody, Proof).

:-export(kb_mt/2).
kb_mt(C,MT):- into_plist(C,PLIST),!,  append([el_holds|PLIST],[MT,_PropsV],CallList),Call=..CallList,Call.
kb_mt(C,dbase_t):- thlocal:useDbase_t, dbase_t(C).


proof_from_clause(Head, true, Head):-!.
proof_from_clause(Head, Body, ((Head:- Body))).

:-dynamic assert_next/1.
:-export assert_next/1.

:-export(move_kb_assertions_matching/4).
move_kb_assertions_matching(PLIST,Match,Replace,Where):- 
%   dmsg(move_kb_assertions_matching(PLIST,Match,Replace,to(Where))),
        doall((kbp_t_list(PLIST,Call),
           forall(retract(Call),
           (subst(PLIST:Call,Match,Replace,NewPLIST:NewCall),
           assert_to_db_list(Where,[rewrite,NewPLIST,NewCall]))))).


assert_to_db_list(HOLDS,PLIST):- safe_univ(Call,[HOLDS|PLIST]), assert(assert_next(Call)).


with_kb_assertions_matching(PLIST,Proof,Call):- doall((kbp_t_list(PLIST, Proof),Call)).
   
:-export(kbp_to_dbase_t/0).

kbp_to_dbase_t:- must_det(with_assertions(thlocal:useOnlyExternalDBs,kbp_to_dbase_0)).

kbp_to_dbase_0:-!.
% kbp_to_dbase_0:- once(time_call(move_implied)),fail.
kbp_to_dbase_0:- once(time_call(hide_term_rewrites)),fail.
kbp_to_dbase_0:- once(time_call(hide_empty_strings)),fail.
% kbp_to_dbase_0:- once(time_call(convert_easy_strings)),fail.
% kbp_to_dbase_0:- once(time_call(convert_easy_strings2)),fail.
kbp_to_dbase_0:- time_call(drain_assert_next_buffer),!.

kbp_to_dbase_no_more:- forall((into_plist(_Call,PLIST),kbp_t(PLIST)),assert_to_db_list(_F,PLIST)),
   retractall(thglobal:use_cyc_database),tell('a.txt'),listing(dbase_t),listing('ASSERTION'),told,dmsg(done_dbase_t).


:-export(move_implied/0).
move_implied:-doall((between(2,6,Len),length(PLIST,Len), 
                     Call=..[assertion_holds,implied|PLIST],
                     retract(hl_holds:Call),
                     append(ALIST,[Last],PLIST),NewCall=..[assertion_holds,impliedBy,ALIST,Last],
                     assert(assert_next(hl_holds:NewCall)))),
                     drain_assert_next_buffer.

:-export(hide_term_rewrites/0).
hide_term_rewrites :- with_assertions(thlocal:useOnlyExternalDBs,
 % remove badjuju from the KB (that is unbould slots in the middle of GAFs)
   % hl_holds:retractall(assertion_holds(isa, badjuju, 'Thing')),
   % hl_holds:retractall(el_holds(genls, badjuju, 'AerosolStuff',_,_)), 
   % hl_holds:retractall(assertion_holds(genls, badjuju, 'BiologicalAgentStuff')), 
 % the next few lines will cover the top
   doall((between(2,6,Len),length(PLIST,Len),
     forall(member(vvvar,PLIST),move_kb_assertions_matching(PLIST,vvvar,_,term_rewrites_kb))))),
   drain_assert_next_buffer.

:-export(hide_empty_strings/0).
hide_empty_strings :- with_assertions(thlocal:useOnlyExternalDBs,
 % remove more badjuju from the KB (that is unbould slots in the middle of GAFs)
 % the next few lines will cover the top
   doall((between(2,6,Len),length(PLIST,Len),
     forall(member('',PLIST),move_kb_assertions_matching(PLIST,'','',term_rewrites_kb))))),
   drain_assert_next_buffer.


:-export(convert_easy_strings/0).
convert_easy_strings:-
   doall((between(2,6,Len),length(PLIST,Len),
     forall(member(string(_),PLIST),
      time_call(with_kb_assertions_matching(PLIST,Proof,must_det(print_sentence(Proof))))))),drain_assert_next_buffer.

convert_easy_strings2:-
   doall((between(2,6,Len),length(PLIST,Len),
     forall(member([_|_],PLIST),
      time_call(with_kb_assertions_matching(PLIST,Proof,must_det(print_sentence(Proof))))))),drain_assert_next_buffer.

drain_assert_next_buffer:- predicate_property(assert_next(_),number_of_clauses(CL)),dmsg(drain_assert_next_buffer(CL)),
   time_call(doall((retract(assert_next(Call)),asserta_if_new(Call)))).

write_assertions:-
   tell(holds_all),
   listing(assertion_holds_mworld0),
   listing(assertion_holds),
   listing(term_rewrites_kb),
   told.


print_sentence(Proof):- fix_sentence(Proof,New),!,ignore((Proof\=New,!,must_det(retract(Proof)),assert(assert_next(New)))),!.



