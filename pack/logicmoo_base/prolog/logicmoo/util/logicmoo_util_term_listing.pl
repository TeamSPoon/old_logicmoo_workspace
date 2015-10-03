

:- swi_export(mstatistics/0).
mstatistics:-
  garbage_collect,
  garbage_collect_atoms,
  statistics,
  statistics(stack,Om),O is Om/1000000,
  statistics(clauses,C),
  statistics(memory,[Tm,_]),T is Tm/1000000,
  statistics(atoms,[A,Mm,0]),AM is Mm/1000000,
  OdC is O/C,
  OdA is (A/1000)*39,
  PerAtom is (Mm/A),
  b_i(L,NA),
  save_atoms,
  fmt((stack/clauses/mem/new + O/C/T-Tm/NA = c(OdC)/a(OdA-AM-PerAtom))),!,
  (NA<1000->fmt(L);true).

current_atom_or_blob(X,atom):-current_atom(X).
current_atom_or_blob(X,blob(T)):-current_blob(X,T).
current_atom_or_blob(X,functor_safe(Y)):-current_functor(X,Y).
current_atom_or_blob(X,key):-current_key(X).
current_atom_or_blob(X,flag):-current_key(X).

blob_info(A,atom,blob(A,text)).
blob_info(A,blob(text),blob(A,text)).
blob_info(A,functor_safe(Y),functor_safe(A,Y)).
blob_info(A,key,key(A,Y)):-findall(V,recorded(A,V),Y).
blob_info(A,flag,flag(A,Y)):-flag(A,Y,Y).
blob_info(A,blob(clause),blob(A,T,Y,H,B)):-T=clause, findall(V,clause_property(A,V),Y),(m_clause(_,H,B,A)->true;H=dead).
blob_info(A,blob(record),blob(A,T,Y)):-T=record,with_output_to(string(Y),print_record_properties(A, current_output)).
% blob_info(A,blob(T),blob(A,T,Y)):-with_output_to(string(Y),prolog_term_view:emit_term(A, [])).
blob_info(A,blob(T),blob(A,T)).

:- swi_export(saved_current_atom/2).
:-dynamic(saved_current_atom/2).
:- swi_export(new_atoms/2).
new_atoms(X,Type):-current_atom_or_blob(X,Type),not(saved_current_atom(X,Type)).
:- swi_export(save_atoms/0).
save_atoms:-forall(new_atoms(X,Type),assert(saved_current_atom(X,Type))).
:- swi_export(b_i/2).
b_i(L,NA):-findall(W,(new_atoms(X,Type),once(blob_info(X,Type,W))),LL),list_to_set(LL,L),length(L,NA).
print_record_properties(Record, Out) :-
	format(Out, 'Record reference ~w~n', [Record]),
	(   recorded(Key, Value, Record)
	->  format(Out, ' Key:   ~p~n', [Key]),
	    format(Out, ' Value: ~p~n', [Value])
	;   format(Out, ' <erased>~n', [])
	).

print_clause_properties(REF, Out) :-
	format(Out, 'Clause reference ~w~n', [REF]),
	(   m_clause(_,Head, Body, REF)
	->  nl(Out),
	    portray_clause(Out, (Head:-Body))
	;   format(Out, '\t<erased>~n', [])
	).

make_searchable_index(PI):- forall(to_pi(PI,H),forall(clause(H,B,Ref),make_searchable_ref((H:-B),Ref))).

m_clause(M,H,B,R):- catch(m_clause0(M,H,B,R),E,R=missing(M,H,B,E)).
m_clause_no_missing(M,H,B,R):- catch(m_clause0(M,H,B,R),_,fail).

m_clause0(M,H,B,R):- atom(M),!, M:clause(H,B,R).
m_clause0(_,H,B,R):- user:clause(H,B,R).
m_clause0(M,H,B,R):- atom(M),!, clause(H,M:B,R).

:-thread_local(thlocal:tl_hide_data/1).

clause_ref(HB,Ref):-as_clause_w_m( HB, M, H, B),m_clause_no_missing(M,H,B,Ref).

make_searchable(HB):- must(clause_ref(HB,Ref)),make_searchable_ref(HB,Ref).
make_searchable_ref(HB,Ref):-searchable_of_clause(HB,List),make_searchable_list_ref(List,Ref).
make_searchable_list_ref(List,Ref):- maplist(save_search_ref(Ref),List).

searchable_of_clause(C,[]):-var(C),!.
searchable_of_clause(M:C,[module(M),M|List]):-atom(M),!,searchable_of_clause(C,List).
searchable_of_clause(':-'(H,B),List):-B==true,!,searchable_of_clause(H,List).
searchable_of_clause(':-'(H,B),List):-!,searchable_of_clause_1(H,[B],List).
searchable_of_clause(H,List):-searchable_of_clause_0(H,List).

searchable_of_clause_0(C,[]):-var(C),!.
searchable_of_clause_0(A,[A]):-atomic(A),!.
searchable_of_clause_0(M:C,[module(M),M|List]):-atom(M),!,searchable_of_clause_0(C,List).
searchable_of_clause_0([H|T],[F|List]):- L=[H|T],!,functor(L,F,_),searchable_of_clause_1(H,T,List).
searchable_of_clause_0(C,[funct(F,A),F|List]):- C=..[F,H|T],functor(C,F,A),searchable_of_clause_1(H,T,List).

searchable_of_clause_1([],[]).
searchable_of_clause_1([H|T],List):-searchable_of_clause_1(H,T,List).

searchable_of_clause_1(H,T,[H|List]):-atomic(H),searchable_of_clause_1(T,List).
searchable_of_clause_1(H,T,List):-searchable_of_clause_0(H,List1),searchable_of_clause_1(T,List2),append(List1,List2,List).

:-dynamic(search_refs_use_recorded/0).
search_refs_use_recorded.

searchable_terms(T):-search_refs_use_recorded,!,current_key(Key),unmake_search_key(Key,T).
searchable_terms(T):-unify_in_thread(searchable_terms_tl(T)).

searchable_terms_tl(T):-use_module(library(dialect/ifprolog),[current_global/1]),current_global(Key),unmake_search_key(Key,T).


make_search_key(E,Atomic):-E=..L,atomic_list_concat(['$search'|L],'%',Atomic).
unmake_search_key(Atomic,E):-atomic_list_concat(['$search'|L],'%',Atomic),E=..L.

get_search_ref(E,Results):-make_search_key(E,Atomic),get_search_ref0(Atomic,Results).

get_search_ref0(Atomic,Results):- search_refs_use_recorded,!,findall(R,recorded(Atomic,R),Results).
get_search_ref0(Atomic,Results):- unify_in_thread(main,once(get_search_ref_tl(Atomic,Results))).


unify_in_thread(Thread,once(Goal)):- 
      message_queue_create(Id),
       call_cleanup((thread_signal(Thread,unify_in_thread_tl(Id,Goal)),thread_get_message(Id,Message),process_unify_in_thread(Message,Goal),!),
         message_queue_destroy(Id)).
unify_in_thread(Thread,Goal):- 
      message_queue_create(Id),
       call_cleanup((thread_signal(Thread,unify_in_thread_tl(Id,Goal)),thread_get_message(Id,Message),process_unify_in_thread(Message,Goal)),
         message_queue_destroy(Id)).

process_unify_in_thread(thrown(Message),_Goal):-!,throw(Message).
process_unify_in_thread(failed(_Message),_Goal):-!,fail.
process_unify_in_thread(result(Goal0),Goal1):-!,Goal0=Goal1.
process_unify_in_thread(Message,Goal):-throw(unknown_process_unify_in_thread(Message,Goal)).

unify_in_thread_tl(Id,Goal):- catch((forall(Goal,thread_send_message(Id,result(Goal))),thread_send_message(Id,failed(Goal))), E,thread_send_message(Id,thrown(E))).

get_search_ref_tl(Atomic,Refs):- nb_current(Atomic,Refs)->true;Refs=[].

save_search_ref(Ref,E):-make_search_key(E,Atomic),save_search_ref_0(Ref,Atomic).

save_search_ref_0(Ref,Atomic):- search_refs_use_recorded,!, save_search_ref_recorded(Ref,Atomic).
save_search_ref_0(Ref,Atomic):- thread_signal(main,save_search_ref_tl(Ref,Atomic)).

save_search_ref_recorded(Ref,Atomic):-recorded(Atomic,Ref),!.
save_search_ref_recorded(Ref,Atomic):-recordz(Atomic,Ref).

save_search_ref_tl(Ref,Atomic):-nb_current(Atomic,Refs),(member(Ref,Refs)->true;nb_setval(Atomic,[Ref|Refs])).
save_search_ref_tl(Ref,Atomic):-nb_setval(Atomic,[Ref]).

% that is    CL=beliefs(we,loves(joe,turkey)), asserta(C,Ref),forall(find_each_atom(CL,A),(asserta_if_new(idexed_atom(A)),asserta(atom_index(A,Ref)))).


:-multifile shared_hide_data/1.

shared_hide_data(varname_info/4):- !,listing_filter(hideMeta).
shared_hide_data(table_bugger:_):- listing_filter(hideMeta).
shared_hide_data(wid):- listing_filter(hideMeta).

listing_filter(P):-notrace(hide_data0(P)).

hide_data0(P):-var(P),!,fail.
hide_data0(neg(_)):-!,fail.
hide_data0(hideMeta):-listing_filter(showAll),!,fail.
hide_data0(P):-thlocal:tl_hide_data(P),!.
hide_data0(P):-shared_hide_data(P),!.
hide_data0(_/_):-!,fail.
hide_data0(P):- compound(P),functor(P,F,A), (hide_data0(F/A);hide_data0(F)).
hide_data0(M:P):- atom(M),(listing_filter(M);hide_data0(P)).


  :- use_module(library(pldoc)).
  :- use_module(library(http/thread_httpd)).
  :- use_module(library(http/http_parameters)).
  :- use_module(library(http/html_write)).
  :- use_module(library(http/mimetype)).
  :- use_module(library(dcg/basics)).
  :- use_module(library(http/http_dispatch)).
  :- use_module(library(http/http_hook)).
  :- use_module(library(http/http_path)).
  :- use_module(library(http/http_wrapper)).
  :- use_module(library(uri)).
  :- use_module(library(debug)).
  :- use_module(library(lists)).
  :- use_module(library(url)).
  :- use_module(library(socket)).
  :- use_module(library(option)).
  :- use_module(library(error)).
  :- use_module(library(www_browser)).
  :- use_module(library(pldoc/doc_process)).
  :- use_module(library(pldoc/doc_htmlsrc)).
  :- use_module(library(pldoc/doc_html)).
  :- use_module(library(pldoc/doc_index)).
  :- use_module(library(pldoc/doc_search)).
  :- use_module(library(pldoc/doc_man)).
  :- use_module(library(pldoc/doc_wiki)).
  :- use_module(library(pldoc/doc_util)).
  :- use_module(library(pldoc/doc_access)).
  :- use_module(library(pldoc/doc_pack)).

% start a unused server
:- use_module(library(doc_http)).
%:- use_module(library(doc_html)).
:- doc_collect(true).


% find normal docs
% prolog:help_hook(help(_M:F/A)):- predicate(F, A, _, _From, _To),!,fail.
% else use a structured comment 
% our smarter matching system (based off listing)
%prolog:help_hook(help(What)):- '$find_predicate'(What, Preds), Preds\==[], Preds\==[What],forall(member(M:F/A,Preds),help(M:F/A)),fail.
prolog:help_hook(help(A)):-  pfc_show_doc(A),fail.

%%	pfc_show_doc(+Object) is det.
%
%	Searches in doc indexes for Object occurances
%
%	@see	help/1.

pfc_show_doc(What):- findall(Infos,pfc_show_doc(What,Infos),LInfos),LInfos\=[],flatten(LInfos,Infos),forall(member(E,Infos),format('~N~w~n',[E])).
pfc_show_doc(What):- '$find_predicate'(What, Preds), Preds\==[], Preds\==[What],forall(member(M:F/A,Preds),pfc_show_doc(M:F/A)).


pfc_show_doc(M:F/A,['$mode'(PI, Det)]):-functor(PI,F,A),M:'$mode'(PI, Det).
pfc_show_doc(M:F/A,[Info,Info2]):-M:'$pldoc'(F/A,_FL,Info,Info2).
pfc_show_doc(Id,[Info,Info2]):- '$pldoc'(Id,_FL,Info,Info2).
pfc_show_doc(M,[Title,Info,Info2]):- pldoc_process:doc_comment(M:module(Title),_FileLines,Info,Info2).


% ?- help(term_expansion/2).
% ?- help(match_regex/2).
:- user:ensure_loaded(library(listing)).
:- user:ensure_loaded(library(check)).
:- user:ensure_loaded(library(make)).


:- meta_predicate user:unify_listing(0).
unify_listing(FileMatch):-functor_safe(FileMatch,F,A),unify_listing(FileMatch,F,A),!.


% unify_listing_header(FileMatch):-functor_safe(FileMatch,F,A),unify_listing_header(FileMatch,F,A),!.

:- meta_predicate user:unify_listing(0,*,*).
unify_listing_header(FileMatch,F,A):- (fmt('~n/* Prediate: ~q / ~q ~n',[F,A,FileMatch])),fail.
unify_listing_header(FileMatch,_F,_A):- printAll(predicate_property(FileMatch,PP),PP),fail.
unify_listing_header(FileMatch,_F,_A):- (fmt('~n ~q. ~n */ ~n',[FileMatch])),fail.
unify_listing_header(FileMatch,F,A):-predicate_property(FileMatch,dynamic),(fmt(':-dynamic(~q).~n',[F/A])),fail.
unify_listing_header(FileMatch,F,A):-predicate_property(FileMatch,multifile),(fmt(':-multifile(~q).~n',[F/A])),fail.
unify_listing_header(_FileMatch,_F,_A).

unify_listing(FileMatch,F,A):- unify_listing_header(FileMatch,F,A), printAll(FileMatch).

printAll(Call):-printAll(Call,Call).
printAll(Call,Print):- flag(printAll,_,0), forall((Call,flag(printAll,N,N+1)),(fmt('~q.~n',[Print]))),fail.
printAll(_Call,Print):- flag(printAll,PA,0),(fmt('~n /* found ~q for ~q. ~n */ ~n',[PA,Print])).


/*
contains_term_unifiable(SearchThis,Find):-Find=SearchThis,!.
contains_term_unifiable(SearchThis,Find):-compound(SearchThis),functor_safe(SearchThis,Func,_),(Func==Find;arg(_,SearchThis,Arg),contains_term_unifiable(Arg,Find)).
*/


:- multifile user:listing_mpred_hook/1.
:- dynamic user:listing_mpred_hook/1.

:-thread_local(thlocal:in_term_listing/1).
:-thread_local(thlocal:in_prolog_listing/1).

:-swi_export((term_listing/1)).
term_listing([]):-!.

term_listing(Match):- \+ \+ thlocal:in_term_listing(Match),!.
term_listing(Match):- thlocal:in_prolog_listing(Match),findall(PI,to_pi(Match,PI),SkipPI),!,term_non_listing(Match,SkipPI),!.
term_listing(f(Match)):- !,term_listing_inner(portray_hbr,Match,[]).
term_listing(Match):- is_list(Match),!,term_listing_inner(portray_hbr,Match,[]).
term_listing(Match):- term_non_listing(Match,[]),!.

plisting(Match):-with_assertions(thlocal:in_term_listing(Match),plisting_0(Match)).
plisting_0(Match):- findall(G,to_pi(Match,G),Gs),forall(member(H,Gs),plisting_1(H)).
plisting_1(H):-plisting_2(H).

plisting_2(H):-synth_clause_for(H,B,R,_SIZE,SYNTH),SYNTH,once(portray_hbr(H,B,R)),fail.
plisting_2(_).




user:listing_mpred_hook(Match):- user:term_listing(Match).

user:term_mpred_listing(Match):- current_predicate(user:listing_mpred_hook/1), once(debugOnError(doall(call_no_cuts(user:listing_mpred_hook(Match))))),!.
 

:-swi_export(term_non_listing/2).
term_non_listing(Match):- term_non_listing(Match,[]).
:-swi_export(term_non_listing/2).
term_non_listing(Match,SkipPI):- 
  with_assertions(thlocal:in_term_listing(Match),
 (
  % format('~N/* term_non_listing(~q) => ~n',[Match]),
   term_listing_inner(portray_hbr,Match,SkipPI),
  % format(' <= term_non_listing(~q) */ ~n',[Match]).
  !)).


get_matcher_code(Match,H,B,MATCHER):-  atom(Match),!, MATCHER= term_matches_unify(99,Match,((H:-B))).
get_matcher_code(Match,H,B,MATCHER):-  MATCHER = term_matches_hb(Match,H,B).


term_listing_inner(Pred,Match,SkipPI):- 
 must_det_l((
   get_matcher_code(Match,H,B,MATCHER),
   PRINT = must(ignore(show_call_failure(once(call(Pred,H,B,Ref))))),   
   PREDZ = ( must(synth_clause_for(H,B,Ref,Size,SYNTH)), \+member(H,SkipPI)),
   forall(PREDZ,
     must(( 
      (listing_filter(wholePreds),Size<100) 
        -> 
          ( \+ \+ ((SYNTH,MATCHER)) -> '@'(forall(SYNTH,PRINT),'user') ; true) 
         ; 

        ('@'(forall(SYNTH,(MATCHER->PRINT;true)),'user'))))))).
      

:- multifile user:prolog_list_goal/1.
% user:prolog_list_goal(Goal):- writeq(hello(prolog_list_goal(Goal))),nl.

:- dynamic(user:no_buggery/0).
user:buggery_ok :- \+ compiling, current_predicate(logicmoo_bugger_loaded/0), \+ user:no_buggery.

:- multifile prolog:locate_clauses/2.
prolog:locate_clauses(A, _) :- 
    current_predicate(logicmoo_bugger_loaded/0),
    buggery_ok,
    current_predicate(user:term_mpred_listing/1),
    with_assertions(thlocal:in_prolog_listing(A),call_no_cuts(user:term_mpred_listing(A))),fail.



:-multifile((synth_clause_for/5)).
:-swi_export((synth_clause_for/5)).

% bookeepingPredicate(M:G):- member(M:F/A,[M:'$exported_op'/3]),current_module(M),functor(G,F,A),once(predicate_property(M:G,_)).
bookeepingPredicateXRef(user:file_search_path(_,_)).
bookeepingPredicateXRef(_:G):-member(F/A,[xref_defined/3,xref_called/3,xref_exported/2]),functor(G,F,A).
predicateUsesCall(_:G):-
  member(F/A,[module_property/2,predicate_property/2,pengine_property/2,current_pengine_application/1,source_file_property/2,
            source_file/2,current_prolog_flag/2,current_op/3]),functor(G,F,A).

sourceTextPredicate(M:G):- source_file((M:el_holds(_,_,_,_,_,_,_)),F) -> source_file(M:G,F).
%sourceTextPredicate(el_assertions:G):- between(4,16,A),functor(G,el_holds,A),current_predicate(_,el_assertions:G).
%sourceTextPredicate(el_assertions:G):- between(4,16,A),functor(G,el_holds_implies,A),current_predicate(_,el_assertions:G).
%sourceTextPredicate(el_assertions:G):- between(4,16,A),functor(G,el_holds_implies_t,A),current_predicate(_,el_assertions:G).
%sourceTextPredicate(el_assertions:G):- between(4,16,A),functor(G,el_holds_t,A),current_predicate(_,el_assertions:G).
sourceTextPredicate(_):-fail.

sourceTextPredicateSource(_):-fail.

:- thread_local(thlocal:large_predicates/2).

plisting_1:-plisting_1(user:spftY(_,_,_,_)).

synth_clause_for(G,true,0,244,SYNTH):-  bookeepingPredicateXRef(G), notrace((\+ listing_filter(hideMeta))), SYNTH=failOnError(G).
synth_clause_for(G,B,Ref,Size,SYNTH):- cur_predicate(_,G), ((notrace(( \+ bookeepingPredicateXRef(G), \+ sourceTextPredicate(G), \+ listing_filter(G))))), 
                                                                SYNTH = (synth_clause_ref(G,B,Ref,Size,SYNTH2),SYNTH2).
synth_clause_for(G,true,0,222, SYNTH):-  sourceTextPredicate(G), \+ listing_filter(G), SYNTH = failOnError(G).
synth_clause_for(G,  B, Ref,Size, SYNTH):- \+ listing_filter(skipLarge), gripe_time(10,synth_clause_for_l2(G,B,Ref,Size,SYNTH)).
 
synth_clause_for_l2(M:H,B,Ref,Size,SYNTH):- 
 (findall((Size- (M:H)),retract(thlocal:large_predicates(M:H,Size)),KeyList),
   keysort(KeyList,KeySorted), 
   format('~N~n% listing larger preds now.. ~q~n',[KeySorted])),!,
   synth_clause_for_large(M:H,B,Ref,KeySorted,Size,SYNTH).

synth_clause_for_large(_,_,_,[],0,fail):-!.
synth_clause_for_large(_,_,_,_,0,fail):- listing_filter(skipLarge),!.
synth_clause_for_large(M:H,B,Ref,KeySorted,Size,SYNTH):-   
      ((must(member( (Size- (M:H)) , KeySorted))),
      \+ listing_filter(M:H),
      SYNTH= must(m_clause(M,H,B,Ref))).

:-swi_export((synth_clause_ref/5)).
synth_clause_ref(_:in_term_listing(_),_B,_Ref, _Size, _CALL):-!,fail.
synth_clause_ref(_:in_prolog_listing(_),_B,_Ref, _Size, _CALL):-!,fail.
synth_clause_ref(_:varname_info(_,_,_,_),_B,_Ref,_Size, _CALL):- \+ listing_filter(showAll),!,fail.

synth_clause_ref(M:H,B,Ref, 250, SYNTH):- \+ listing_filter(hideMeta), SYNTH= (predicate_property(M:H,PP),Ref=0,B=M:predicate_property(H,PP)).
synth_clause_ref(MHG,B,Ref, 213, SYNTH):- predicateUsesCall(MHG),synth_in_listing(MHG), !, SYNTH= (failOnError(MHG),Ref=0,B=predicateUsedCall).
synth_clause_ref(M:H,B,Ref,Size, SYNTH):- predicate_property(M:H,number_of_clauses(Size)),synth_in_listing(M:H),
  (Size > 5000 ->  (\+ listing_filter(skipLarge), asserta(thlocal:large_predicates(M:H,Size)),fail) ; SYNTH = m_clause(M,H,B,Ref)).


synth_in_listing(MH):- ( \+ listing_filter(MH), \+ sourceTextPredicateSource(MH) ),!.

:-swi_export((term_matches_hb/3)).



term_matches_hb(HO,H,B):-term_matches_hb(999,HO,H,B).
term_matches_hb(_,Var,_,_):-var(Var),!.

term_matches_hb(_,[],_,_):-!.
term_matches_hb(D,_,_,_):- D<0,!,fail.
term_matches_hb(_,noinfo,_,info(_)):-!,fail. 
term_matches_hb(D,M:HO,H,B):-!,term_matches_hb(D,module(M);h(HO),H,B).
term_matches_hb(D,[F1],H,B):-!,term_matches_hb(D,F1,H,B),!.
term_matches_hb(D,[F1|FS],H,B):-!,term_matches_hb(D,(F1;FS),H,B).
term_matches_hb(D,[F1+FS],H,B):-!,term_matches_hb(D,(F1,FS),H,B).
term_matches_hb(D,(F1+FS),H,B):-!,term_matches_hb(D,(F1,FS),H,B).
term_matches_hb(D,(F1,FS),H,B):-!,term_matches_hb(D,F1,H,B),term_matches_hb(D,FS,H,B).
term_matches_hb(D,(F1;FS),H,B):-!, (term_matches_hb(D,F1,H,B);term_matches_hb(D,FS,H,B)).
term_matches_hb(D,-(C),H,B):-nonvar(C),!,\+(term_matches_hb(D,C,H,B)).
term_matches_hb(D,not(C),H,B):-nonvar(C),!,\+(term_matches_hb(D,C,H,B)).
term_matches_hb(D,+(C),H,B):-nonvar(C),!, term_matches_hb(D,C,H,B).
term_matches_hb(_,module(M),H,_):-!,predicate_property(H,imported_from(M)).
term_matches_hb(D,h(P),H,_):-!,term_matches_hb(D,P,H,666666).
term_matches_hb(D,b(P),_,B):-!,term_matches_hb(D,P,B,666666).
term_matches_hb(_,string(HO),H,B):- nonvar(HO),any_to_string(HO,HS),!, with_output_to(string(H1B1),write_canonical((H:-B))), (sub_atom_icasechk(HS,_,H1B1);sub_atom_icasechk(H1B1,_,HS)),!.
term_matches_hb(D,unify(HO),H,B):- nonvar(HO),!,term_matches_hb(D,HO,H,B).
term_matches_hb(_,depth(Depth,HO),H,B):- term_matches_hb(Depth,HO,H,B).
term_matches_hb(D,contains(HO),H,B):- !,term_matches_hb(D,string(HO),H,B).
term_matches_hb(D,F/A,H,B):-atom(F),integer(A),!,functor(P,F,A),term_matches_hb(D,(unify(P);same(F/A)),H,B).
term_matches_hb(D,F/A,H,B):-atom(F),var(A),!,term_matches_hb(D,(functor(F);same(F/A)),H,B).
term_matches_hb(D,F/A,H,B):-var(F),integer(A),!,term_matches_hb(D,(arity(A);same(F/A)),H,B).
term_matches_hb(D,HO,H,B):- \+ \+ term_matches_unify(D,HO,(H:-B)).

% ?- term_listing((h(depth(0,pt/2)),same(tBird(A)))).

:-swi_export(term_matches_unify/3).
term_matches_unify(_R,same(HO),V):-HO=@=V.
term_matches_unify(_R,_,V):-var(V),!,fail.
term_matches_unify(_R,V,V).
term_matches_unify(_R,_,V):- \+ compound(V),!,fail.
term_matches_unify(_R,arity(A),H):-functor(H,_,A).
term_matches_unify(_R,functor(F),H):-functor(H,F,_).
term_matches_unify(0,_,_):-!,fail.
term_matches_unify(R,HO,V):- RR is R -1, compound_name_arguments(V,F,ARGS),member(E,[F|ARGS]),term_matches_unify(RR,HO,E).


nonvar_search(V):-var(V),!,fail.
nonvar_search(M:_/A):-nonvar(M),!,nonvar(A).
nonvar_search(M:P):-nonvar(M),nonvar(P).
nonvar_search(F/A):-!,nonvar(F),nonvar(A).
%nonvar_search(F):-atom(F).
%nonvar_search(P):-compound(F).

:-dynamic(cur_predicates/1).
:-swi_export((cur_predicate)/2).

cur_predicate(M:F/A,M:P):-atomic(M),compound(P),!,ignore(functor(P,F,A)).
cur_predicate(M:F/A,M:P):-
   current_predicate(M:F/A),functor(P,F,A),\+ ((predicate_property(M:P,imported_from(_)))).

/*
cur_predicate(M:F/A,MP):-atom(M),var(F),!,cur_predicate(M,F/A,MP).
cur_predicate(MFA,M:P):-atom(M),var(P),!,cur_predicate(M,MFA,P).
cur_predicate(M:FA,MP):-atom(M),!,cur_predicate(M,FA,MP).
cur_predicate(MFA,MP):-cur_predicate(_,MFA,MP).
cur_predicate(M,MFA,MP):-nonvar(M),
 M:call(cur_predicate(MFA,MP)).

cur_predicate(SM,MFA,MP):-
   (

 %nonvar_search(MFA) -> SEARCH=MFA ; 
 %nonvar_search(MP) -> SEARCH=MP ;
    
  nonvar(MFA) ->    (current_predicate(MFA),SEARCH=MFA);
    
  nonvar(MP) ->     (current_predicate(_,MP),SEARCH=MP);
   
   nonvar(SM) ->  (SM:current_predicate(SF/SA),SEARCH=M:SF/SA);
                      
    current_predicate(SEARCH)),
  no_repeats(M:F/A,('$find_predicate'(SEARCH,MFAs),
   
  member(M:F/A,MFAs))),
   debug,
   must_det_l((
  once(two_mfa(MFA, M,F,A)),

       functor(P,F,A),
  once(to_mp(MP,M,P)))).
   ignore(SM=M).


% match_mfa(MFA1,MFA2):-must_det_l((to_mfa(MFA1,M1,F1,A1),to_mfa(MFA2,M2,F2,A2))),A1=A2,F1=F2,!,ignore(M1=M2).


two_mfa(M1:F/A,M2,F,A):-!,ignore(M1=M2).
two_mfa(M1:F,M2,F,_):-atom(F),!,ignore(M1=M2).
two_mfa(M1:FA,M2,F,A):- nonvar(FA),!, get_functor(FA,F,A),ignore(M1=M2).

two_mfa(F,_,F,_):- atom(F),!.
two_mfa(F/A,_,F,A):- !.
to_mp(M1:P,M2,P):-ignore(M1=M2).
to_mp(P,_,P):-nonvar(P),!.
*/

:-swi_export(ok_show/1).
ok_show(F/A):-!,functor(P,F,A),ok_show(P),!.
ok_show(P):-not(bad_pred(P)).



% when we import new and awefull code base (the previous )this can be helpfull
% we redfine list_undefined/1 .. this is the old version
:- swi_export(scansrc_list_undefined/1).
scansrc_list_undefined(_):-!.
scansrc_list_undefined(A):- real_list_undefined(A).


:- swi_export(real_list_undefined/1).
real_list_undefined(A):- 
 merge_options(A, [module_class([user])], B),
        prolog_walk_code([undefined(trace), on_trace(found_undef)|B]),
        findall(C-D, retract(undef(C, D)), E),
        (   E==[]
        ->  true
        ;   print_message(warning, check(undefined_predicates)),
            keysort(E, F),
            group_pairs_by_key(F, G),
            maplist(check:report_undefined, G)
        ).


:-swi_export(mmake/0).
mmake:- thread_main(Main), \+ thread_self(Main), !.
mmake:- thread_main(Main),!,thread_signal(Main,catch(((ignore(update_changed_files), ignore(if_defined(load_mpred_files,true)))),_,true)).
:-swi_export(update_changed_files/0).
update_changed_files:-!,thread_signal(main,update_changed_files0).
update_changed_files0 :- current_main_error_stream(Err),with_output_to(Err,update_changed_files1).
update_changed_files1 :- 
        set_prolog_flag(verbose_load,true),
        ensure_loaded(library(make)),
	findall(File, make:modified_file(File), Reload0),
	list_to_set(Reload0, Reload),
	(   prolog:make_hook(before, Reload)
	->  true
	;   true
	),
	print_message(silent, make(reload(Reload))),
	maplist(make:reload_file, Reload),
	print_message(silent, make(done(Reload))),
	(   prolog:make_hook(after, Reload)
	->  true
	;   
           true %list_undefined,list_void_declarations
	).

:- swi_export(remove_undef_search/0).
% check:list_undefined:-real_list_undefined([]).
remove_undef_search:- ((
 '@'(use_module(library(check)),'user'),
 redefine_system_predicate(check:list_undefined(_)),
 abolish(check:list_undefined/1),
 assert((check:list_undefined(A):- not(thread_self(main)),!, ignore(A=[]))),
 assert((check:list_undefined(A):- reload_library_index,  update_changed_files,call(thread_self(main)),!, ignore(A=[]))),
 assert((check:list_undefined(A):- ignore(A=[]),scansrc_list_undefined(A))))).

% :- remove_undef_search.


mp(M,P,MP):-atom(M),!,(MP=M:P ; MP=P).
mp(_,P,MP):-MP=P.


:-swi_export(bad_pred/1).
bad_pred(M:P):-!,atom(M),bad_pred(P). 
%bad_pred(P):-functor(P,F,A),arg(_,v(cur_predicates/_,db_op/_,db_op00/_,db_op0/_,db_op_loop/_,do_expand_args_l/3),F/A).
%bad_pred(P):-predicate_property(P,autoloaded(_)).
%bad_pred(P):-not(predicate_property(P,number_of_clauses(_))).
%bad_pred(P):-predicate_property(P,imported_from(_)),predicate_property(P,static).
%bad_pred(P):-predicate_property(P,foreign).

:-swi_export(pred_info/2).
pred_info(H,Props):- get_functor(H,F,_),findall(PP,user:mpred_prop(F,PP),Props).

:-swi_export(portray_hbr/3).
portray_hbr(H,B,_):- B==true, !, portray_one_line(H).
portray_hbr(H,B,_):- \+ \+ portray_one_line((H:-B)).

:-swi_export(portray_one_line/1).
:-thread_local(portray_one_line_hook/1).

portray_one_line(H):- no_slow_io,!, writeq(H),write('.'),nl,!.
portray_one_line(H):- notrace((user:loop_check(user:portray_one_line0(H),((portray_clause(H),nl))))).
% portray_one_line(H):- notrace((loop_check(portray_one_line0(H),((writeq(H),write('.'),nl))))).
portray_one_line0(H):- call_not_not((portray_one_line_hook(H))),!.
portray_one_line0(H):- call_not_not(((get_clause_vars(H),portray_clause(H)))),!.
portray_one_line0(H):- writeq(H),write('.'),nl,!.
user:portray(A) :- compound(A),portray_one_line(A),!.

pp_listing(Pred):- functor_safe(Pred,File,A),functor_safe(FA,File,A),listing(File),nl,findall(NV,predicate_property(FA,NV),LIST),writeq(LIST),nl,!.

agg_all_test2(min(X), Goal, Min) :- !,
  State = state(X),
   (  call(Goal),
      arg(1, State, M0),
      M is min(M0,X),
      nb_setarg(1, State, M),
      fail
   ;  arg(1, State, Min),
      nonvar(Min)
   ).
