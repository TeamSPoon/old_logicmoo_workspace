

:- export(mstatistics/0).
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
blob_info(A,blob(clause),blob(A,T,Y,H,B)):-T=clause, findall(V,clause_property(A,V),Y),(clause(H,B,A)->true;H=dead).
blob_info(A,blob(record),blob(A,T,Y)):-T=record,with_output_to(string(Y),print_record_properties(A, current_output)).
% blob_info(A,blob(T),blob(A,T,Y)):-with_output_to(string(Y),prolog_term_view:emit_term(A, [])).
blob_info(A,blob(T),blob(A,T)).

:- export(saved_current_atom/2).
:-dynamic(saved_current_atom/2).
:- export(new_atoms/2).
new_atoms(X,Type):-current_atom_or_blob(X,Type),not(saved_current_atom(X,Type)).
:- export(save_atoms/0).
save_atoms:-forall(new_atoms(X,Type),assert(saved_current_atom(X,Type))).
:- export(b_i/2).
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
	(   clause(Head, Body, REF)
	->  nl(Out),
	    portray_clause(Out, (Head:-Body))
	;   format(Out, '\t<erased>~n', [])
	).




:- user:ensure_loaded(library(listing)).
:- user:ensure_loaded(library(check)).
:- user:ensure_loaded(library(make)).


:- meta_predicate user:unify_listing(0).
unify_listing(FileMatch):-functor_safe(FileMatch,F,A),unify_listing(FileMatch,F,A),!.
unify_listing_header(FileMatch):-functor_safe(FileMatch,F,A),unify_listing_header(FileMatch,F,A),!.


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

:-export((term_listing/1)).
term_listing([]):-!.
term_listing(Match):- 
  '@'(ignore((catchvv(listing(Match),E,wdmsg(E)))),'user'),
  term_non_listing(Match),!.

user:listing_mpred_hook(_):-fail.
user:term_mpred_listing(Match):-
 current_predicate(user:listing_mpred_hook/1), 
 %format('/* listing_mpred_hook(~q) => ~n',[Match]),!,
 once(debugOnError(doall(call_no_cuts(user:listing_mpred_hook(Match))))),
 !. %format(' <= listing_mpred_hook(~q) */ ~n',[Match]).

:-export(term_non_listing/1).
term_non_listing(Match):- 
   format('/* term_non_listing(~q) => ~n',[Match]),
      term_listing_inner(portray_hbr,Match),
   format(' <= term_non_listing(~q) */ ~n',[Match]).

term_listing_inner(Pred,Match):-atom(Match),!,
      '@'(doall((
         synth_clause_for(H,B,Ref),(term_matches_unify(99,Match,((H:-B)))->call(Pred,H,B,Ref)->fail))),'user').
term_listing_inner(Pred,Match):-
      '@'(doall((
         synth_clause_for(H,B,Ref),(term_matches_hb(Match,H,B)->call(Pred,H,B,Ref)->fail))),'user').

:- multifile user:prolog_list_goal/1.
% user:prolog_list_goal(Goal):- writeq(hello(prolog_list_goal(Goal))),nl.

:- dynamic(user:no_buggery/0).
user:buggery_ok :- \+ compiling, current_predicate(logicmoo_bugger_loaded/0), \+ user:no_buggery.

:- multifile prolog:locate_clauses/2.
prolog:locate_clauses(A, _) :- current_predicate(logicmoo_bugger_loaded/0),buggery_ok ,current_predicate(user:term_mpred_listing/1),call_no_cuts(user:term_mpred_listing(A)),fail.



:-multifile((synth_clause_for/3)).
:-export((synth_clause_for/3)).

% bookeepingPredicate(M:G):- member(M:F/A,[M:'$exported_op'/3]),current_module(M),functor(G,F,A),once(predicate_property(M:G,_)).
bookeepingPredicate1(user:file_search_path(_,_)).
% bookeepingPredicate1(G):-member(F/A,[xref_defined/3,xref_called/3,xref_exported/2]),functor(G,F,A).
bookeepingPredicate2(_:G):-
  member(F/A,[module_property/2,predicate_property/2,pengine_property/2,current_pengine_application/1,source_file_property/2,
            source_file/2,current_prolog_flag/2,current_op/3]),functor(G,F,A).

%bookeepingPredicate3(el_assertions:G):- between(4,16,A),functor(G,el_holds_implies_t,A).
%bookeepingPredicate3(el_assertions:G):- between(4,16,A),functor(G,el_holds_t,A).
bookeepingPredicate3(_):-fail.

:- thread_local(thlocal:large_predicates/2).

synth_clause_for(G,true,0):-  bookeepingPredicate1(G),catch(G,_,fail).
synth_clause_for(G,B,Ref):- cur_predicate(_,M:H),synth_clause_ref(M:H,G,B,Ref).
synth_clause_for(G,true,0):-  bookeepingPredicate3(G),catch(G,_,fail).
synth_clause_for(M:H,B,Ref):- (findall((Size-(M:H)),retract(thlocal:large_predicates(M:H,Size)),KeyList),
   keysort(KeyList,KeySorted), pairs_values(KeySorted, ByLength),format('~N~n% listing larger preds now.. ~q~n',[KeySorted]))->
      member(M:H,ByLength),clause(M:H,B,Ref),flush_output.

:-export((synth_clause_ref/3)).
synth_clause_ref(M:H,M:predicate_property(H,B),true,0):- predicate_property(M:H,B).
synth_clause_ref(G,G,true,0):-  bookeepingPredicate2(G),!,catch(G,_,fail).
synth_clause_ref(M:H,M:H,B,Ref):- predicate_property(M:H,number_of_clauses(Size)),!,
    (Size > 5000 -> asserta(thlocal:large_predicates(M:H,Size));M:clause(H,B,Ref)).


%synth_clause_ref(M:H,B,Ref):-M==user,!,synth_clause_ref(H,B,Ref).
% synth_clause_ref(H,(fail,synth_clause_info(Props)),0):- (H\=(_:-_)),once(pred_info(H,Props)), Props\==[].

:-export((term_matches_hb/3)).

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
term_matches_hb(D,HO,H,B):- \+ \+ term_matches_unify(D,HO,(H:-B)).

% ?- term_listing((h(depth(0,pt/2)),same(tBird(A)))).

:-export(term_matches_unify/3).
term_matches_unify(_R,same(HO),V):-HO=@=V.
term_matches_unify(_R,_,V):-var(V),!,fail.
term_matches_unify(_R,V,V).
term_matches_unify(_R,_,V):- \+ compound(V),!,fail.
term_matches_unify(_R,arity(A),H):-functor(H,_,A).
term_matches_unify(_R,functor(F),H):-functor(H,F,_).
term_matches_unify(0,_,_):-!,fail.
term_matches_unify(R,HO,V):- RR is R -1, compound_name_arguments(V,F,ARGS),member(E,[F|ARGS]),term_matches_unify(RR,HO,E).

% match_term_listing(HO,H,B):-!, synth_clause_ref(H,B,_Ref), term_matches_hb(HO,H,B).

nonvar_search(V):-var(V),!,fail.
nonvar_search(M:_/A):-nonvar(M),!,nonvar(A).
nonvar_search(M:P):-nonvar(M),nonvar(P).
nonvar_search(F/A):-!,nonvar(F),nonvar(A).
%nonvar_search(F):-atom(F).
%nonvar_search(P):-compound(F).

:-dynamic(cur_predicates/1).
:-export((cur_predicate)/2).
cur_predicate(M:F/A,M:P):-
   current_predicate(M:F/A),functor(P,F,A),\+ predicate_property(M:P,imported_from(_)).
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

:-export(ok_show/1).
ok_show(F/A):-!,functor(P,F,A),ok_show(P),!.
ok_show(P):-not(bad_pred(P)).



% when we import new and awefull code base (the previous )this can be helpfull
% we redfine list_undefined/1 .. this is the old version
:- export(scansrc_list_undefined/1).
scansrc_list_undefined(_):-!.
scansrc_list_undefined(A):- real_list_undefined(A).


:- export(real_list_undefined/1).
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

:-export(mmake/0).
mmake:- ignore(update_changed_files), ignore(if_defined(load_mpred_files,true)).
:-export(update_changed_files/0).
update_changed_files :-
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

:- export(remove_undef_search/0).
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


:-export(bad_pred/1).
bad_pred(M:P):-!,atom(M),bad_pred(P). 
%bad_pred(P):-functor(P,F,A),arg(_,v(cur_predicates/_,db_op/_,db_op00/_,db_op0/_,db_op_loop/_,do_expand_args_l/3),F/A).
%bad_pred(P):-predicate_property(P,autoloaded(_)).
%bad_pred(P):-not(predicate_property(P,number_of_clauses(_))).
%bad_pred(P):-predicate_property(P,imported_from(_)),predicate_property(P,static).
%bad_pred(P):-predicate_property(P,foreign).

:-export(pred_info/2).
pred_info(H,Props):- get_functor(H,F,_),findall(PP,user:mpred_prop(F,PP),Props).

:-export(portray_hbr/3).
portray_hbr(H,B,_):- B==true, !, portray_one_line(H).
portray_hbr(H,B,_):- portray_one_line((H:-B)).

:-export(portray_one_line/1).
:-thread_local(portray_one_line_hook/1).
portray_one_line(H):- portray_one_line_hook(H),!.
portray_one_line(H):- current_predicate(wdmsg/1),wdmsg(H),!.
portray_one_line(H):- not(not((snumbervars(H),writeq(H),write('.'),nl))),!.
portray_one_line(H):- writeq(H),write('.'),nl,!.

pp_listing(Pred):- functor_safe(Pred,File,A),functor_safe(FA,File,A),listing(File),nl,findall(NV,predicate_property(FA,NV),LIST),writeq(LIST),nl,!.


