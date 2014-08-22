

:-module(dbase_i_call_kb,[]).

into_plist(PLIST,PLIST):- var(PLIST),!,between(2,19,X),length(PLIST,X).
into_plist([P|LIST],[P|LIST]).
into_plist(Call,PLIST):-Call=..PLIST.

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

nv1000(S):-numbervars(S,100,_,[singletons(true),attvar(skip)]).

:-export((kb_t/1)).
kb_t(Call):- into_plist(Call,PLIST),[AH|LIST]=PLIST,!, kb_t(AH,LIST,PLIST).

kb_t(AH,_,PLIST):-var(AH),!,kbp_t(PLIST).
kb_t(dbase_t,PLIST,_):- !,kbp_t(PLIST).
kb_t(subclass,PLIST,_):- !,kbp_t([genls|PLIST]).
kb_t(AH,PLIST,_):- is_holds_true(AH),!,kb_t(PLIST).
kb_t(AH,PLIST,_):- is_holds_false(AH),!,kb_f(PLIST).
kb_t(_,_,PLIST):- kbp_t(PLIST).

:-export(kbp_t/1). 
kbp_t(_):- not(loaded_external_kbs),!,fail.
% kbp_t(PLIST):- ground(PLIST),!,no_repeats(call_no_cuts(hook:kbp_t_list_prehook(PLIST,PLISTO))),kbp_t_list(PLISTO).
kbp_t(PLIST):- hook:kbp_t_list_prehook(PLIST,PLISTO),kbp_t_list(PLISTO).


hook:kbp_t_list_prehook(PLIST,PLIST).

:-export(kbp_t_list/1). 
kbp_t_list(PLIST):- !,  append([el_holds|PLIST],[_MT,_PropsV],CallList),Call=..CallList,Call.
% the cut above was intending to comment out the next line
kbp_t_list(PLIST):-kbp_t_list(PLIST,_,_Proof).

:-export(kbp_t_list/2). 

kbp_t_list(PLIST,Proof):- kbp_t_list(PLIST,_,Ref),clause(Head, Body, Ref),proof_from_clause(Head, Body, Proof).

:-export(kbp_t_list/3). 
% kbp_t_list(PLIST):- tiny_kb_ASSERTION(PLIST).
kbp_t_list(PLIST,[amt(MT)|PropsV], Ref):- !,  append([el_holds|PLIST],[MT,PropsV],CallList),Call=..CallList,Call,clause(Call,true,Ref).
% the cut above was intending to comment out these next lines
% kbp_t_list(PLIST,[amt(MT)|PropsV], mworld0:Call):- append([assertion_holds_mworld0|PLIST],[MT,Props],CallList),Call=..CallList,mworld:is_callable(Call), mworld0:Call,get_varsp(Props,PropsV).
% kbp_t_list(PLIST,[], hl_holds:Call):- Call=..[assertion_holds|PLIST],hl_holds:is_callable(Call),hl_holds:Call.
% kbp_t_list(PLIST,[amt('ThoughtTreasureMt')],tt0_00022_cycl:Call):- Call=..[ttholds|PLIST],tt0_00022_cycl:is_callable(Call), tt0_00022_cycl:Call. % '@'(Call,hl_holds).


proof_from_clause(Head, true, Head):-!.
proof_from_clause(Head, Body, ((Head:- Body))).

:-dynamic assert_next/1.
:-export assert_next/1.

:-export(move_kb_assertions_matching/4).
move_kb_assertions_matching(PLIST,Match,Replace,Where):- 
%   dmsg(move_kb_assertions_matching(PLIST,Match,Replace,to(Where))),
        doall((kbp_t_list(PLIST,Call),
           foreach(retract(Call),
           subst(PLIST:Call,Match,Replace,NewPLIST:NewCall),
           assert_to_db_list(Where,[rewrite,NewPLIST,NewCall])))).


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

kbp_to_dbase_no_more:- forall((into_plist(_Call,PLIST),kbp_t(PLIST)),assert_to_db_list(PLIST)),
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



