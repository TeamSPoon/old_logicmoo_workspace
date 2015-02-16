/** <module> 
% Game loading Utils
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
:-swi_module(dbase_i_loader, []).

:- include(dbase_i_header).

:-dynamic(registered_dbase_file/1).
:-decl_mpred_prolog(declare_load_dbase/1).
declare_load_dbase(Spec):- forall(filematch(Spec,File),show_call(asserta_if_new(registered_dbase_file(File)))).

:-decl_mpred_prolog(load_dbase_files/0).
load_dbase_files :- forall(registered_dbase_file(File),ensure_plmoo_loaded(File)).

:-dynamic thglobal:current_world/1.
thglobal:current_world(current).

:- meta_predicate show_load_call(0).

ensure_dbase_file(Mask):- ensure_plmoo_loaded(Mask).

:-meta_predicate_transparent(load_dbase/1).

load_dbase(File):-thglobal:current_world(World), load_dbase(World,File),!.
load_dbase(World,File):- 
 with_no_assertions(thglobal:use_cyc_database,
    ( world_clear(World),
      retractall(thglobal:loaded_file_world_time(_,_,_)),
      time_call(ensure_plmoo_loaded(File)),!,
      time_call(finish_processing_world))).

/*

path_concat(L,R,LR):-path_concat1(L,R,LR),!.
path_concat(R,L,LR):-path_concat1(L,R,LR),!.
path_concat(L,R,LR):-atom_concat(L,R,LR),!.
path_concat1('',R,R):-!.
path_concat1('./',R,R):-!.
path_concat1('.',R,R):-!.


:-meta_predicate_transparent(files_matching/2).
:-meta_predicate_transparent(files_matching/3).
files_matching(Mask,File1):- files_matching('./',Mask,File1),access_file(File1,read),!.
files_matching(_Prepend,Mask,File1):- compound(Mask),Mask=..[F,A],!,file_search_path(F,NextPrepend),files_matching(NextPrepend,A,File1),access_file(File1,read).
files_matching(Prepend,Mask,File1):- filematch(Prepend,Mask,File1),access_file(File1,read).
% files_matching(Prepend,Mask,File1):- path_concat(Prepend,Mask,PMask),expand_file_name(PMask,Files),Files\=[],!,member(File1,Files),access_file(File1,read).
*/

:-meta_predicate_transparent(ensure_plmoo_loaded/1).
ensure_plmoo_loaded(Mask):-
  forall(filematch(Mask,File1),ensure_plmoo_loaded_each(File1)).

:-dynamic(thglobal:loaded_file_world_time/3).
:-meta_predicate_transparent(thglobal:loaded_file_world_time/3).
:-meta_predicate_transparent(get_last_time_file/3).
get_last_time_file(FileIn,World,LastTime):- absolute_file_name(FileIn,File),thglobal:loaded_file_world_time(File,World,LastTime),!.
get_last_time_file(_,_,0).

:-meta_predicate_transparent(ensure_plmoo_loaded_each/1).
ensure_plmoo_loaded_each(FileIn):-
   absolute_file_name(FileIn,File),
   thglobal:current_world(World),
   time_file_safe(File,NewTime),!,
   get_last_time_file(File,World,LastTime),
   (LastTime<NewTime -> reload_plmoo_file(File) ; true).

:-meta_predicate_transparent(reload_plmoo_file/1).

reload_plmoo_file(FileIn):-
   absolute_file_name(FileIn,File),
   thglobal:current_world(World),
   retractall(thglobal:loaded_file_world_time(File,World,_)),   
   dbase_mod(DBASE),'@'(load_data_file(World,File),DBASE).

:-meta_predicate_transparent(load_data_file/2).
load_data_file(World,FileIn):-  with_assertions(thglobal:current_world(World),load_data_file(FileIn)).

:-thread_local(thlocal:onEndOfFile/2).
:-meta_predicate_transparent(load_data_file/1).
load_data_file(FileIn):-
  absolute_file_name(FileIn,File),
  thglobal:current_world(World),
  time_file_safe(File,NewTime),
  assert(thglobal:loaded_file_world_time(File,World,NewTime)), 
   dmsginfo(loading_data_file(File,World,NewTime)),!,

  catch((with_assertions(thglobal:loading_dbase_file(World,File),
      setup_call_cleanup(see(File),load_dbase_name_stream(World),seen)),
      load_data_file_end(World,File)),
   Error,
    (wdmsg(error(Error,File)),retractall(thglobal:loaded_dbase_file(World,File)),
     retractall(thglobal:loaded_file_world_time(File,World,NewTime)))).

:-export(load_data_file_end/2).
load_data_file_end(World,File):-
   asserta_new(thglobal:loaded_dbase_file(World,File)),
   dmsginfo(info(load_data_file_complete(File))),
   forall(onEndOfFile(File,Call),must((prologCall(Call),retractall(onEndOfFile(File,Call))))).

load_dbase_name_stream(_Name):- do_gc,repeat,read_one_term(Term),myDebugOnError(add_term(Term)),Term == end_of_file,!.
load_dbase_name_stream(_Name,Stream):- do_gc,repeat,read_one_term(Stream,Term),myDebugOnError(add_term(Term)),Term == end_of_file,!.

myDebugOnError(Term):-catch(once(must(prologCall(Term))),E,(dmsg(error(E,start_myDebugOnError(Term))),trace,rtrace(prologCall(Term)),dmsginfo(stop_myDebugOnError(E=Term)),trace)).

read_one_term(Term):- catch(once(( read_term(Term,[double_quotes(string)]))),E,(Term=error(E),dmsg(error(E,read_one_term(Term))))).
read_one_term(Stream,Term):- catch(once(( read_term(Stream,Term,[double_quotes(string)]))),E,(Term=error(E),dmsg(error(E,read_one_term(Term))))).

rescan_mpred_stubs:- doall((mpred_prop(F,prologHybrid),mpred_arity(F,A),A>0,warnOnError(declare_dbase_local_dynamic(moo,F,A)))).


%:-meta_predicate_transparent(finish_processing_world).
%:-meta_predicate(finish_processing_world).

%:- dynamic(finish_processing_world/0).

% :-module_transparent(finish_processing_world()).

% :-meta_predicate_transparent(finish_processing_world).
% :-meta_predicate_transparent(finish_processing_dbase()).
%:-meta_predicate_transparent(rescan_all/0).
:-meta_predicate_transparent(doall_and_fail(0)).

finish_processing_world :- load_dbase_files, loop_check_local(with_assertions(thlocal:do_slow_kb_op_now,doall(finish_processing_dbase)),true).

doall_and_fail(Call):- time_call(once(doall(Call))),fail.


:-decl_mpred_prolog(etrace/0).
etrace:-leash(-all),leash(+exception),trace.

current_filesource(F):-seeing(X),stream_property(X,file_name(F)).

:-export(onEndOfFile/1).
:-dynamic(onEndOfFile/2).
onEndOfFile(Call):-current_filesource(F),asserta(onEndOfFile(F,Call)).

assert_until_end_of_file(Fact):-must_det_l((thread_local(Fact),asserta(Fact),((onEndOfFile(database_op(change( retract,one),Fact)))))).

%:-meta_predicate_transparent(rescan_all/0).
rescan_all:- doall_and_fail(rescan_dbase_ops).
rescan_all:- doall_and_fail(rescan_dbase_facts).
rescan_all:- doall_and_fail(rescan_dbase_loaded).
rescan_all:- doall_and_fail(rescan_dbase_ops).
rescan_all:- doall_and_fail(rescan_dbase_facts).
rescan_all:- doall_and_fail(rescan_default_props).
rescan_all:- doall_and_fail(rescan_slow_kb_ops).
rescan_all:- doall_and_fail(rescan_mpred_props).
rescan_all.

ensure_at_least_one_region:- (isa(_,tRegion)->true;create_instance(oneRegion1,tRegion)),!.

% :-meta_predicate_transparent(finish_processing_dbase).
finish_processing_dbase:- do_gc,dmsginfo(begin_finish_processing_dbase),fail.
finish_processing_dbase:- doall_and_fail(rescan_all).
finish_processing_dbase:- doall_and_fail(ensure_at_least_one_region).
finish_processing_dbase:- doall_and_fail(call_OnEachLoad).
finish_processing_dbase:- dmsginfo(saving_finish_processing_dbase),fail.
finish_processing_dbase:- savedb,fail.
finish_processing_dbase:- do_gc,dmsginfo(end_finish_processing_dbase),fail.
finish_processing_dbase.


%:-meta_predicate_transparent(rescandb/0).
% rescandb:- forall(thglobal:current_world(World),(findall(File,thglobal:loaded_file_world_time(File,World,_),Files),forall(member(File,Files),ensure_plmoo_loaded_each(File)),prologCall(finish_processing_world))).
rescandb:- prologCall(finish_processing_world).



:- style_check(-singleton).
:- style_check(-discontiguous).
% :- style_check(-atom).

% gload:- load_dbase(savedb),!.
gload:- load_dbase(logicmoo('rooms/startrek.all.plmoo')).

%:-meta_predicate_transparent(savedb/0).
savedb:-!.
savedb:- debugOnError(rsavedb),!.
%:-meta_predicate_transparent(rsavedb/0).
rsavedb:-
 debugOnError(rescan_dbase_facts),
 catch((   
   ignore(catch(make_directory('/tmp/lm/'),_,true)),
   ignore(catch(delete_file('/tmp/lm/savedb'),E,(dmsginfo(E:delete_file('/tmp/lm/savedb'))))),   
   tell('/tmp/lm/savedb'),make_db_listing,told),E,dmsginfo(savedb(E))),!.


%:-meta_predicate_transparent(make_db_listing/0).
make_db_listing:-
 % dbase_mod(DBM),
%   listing(dbase_t),
 %  listing(dbase_f),
     listing(_),
     listing(user:_),  
     listing(dbase:_),
     listing(dyn:_),
     listing(moo_loader:_),
     listing(world :_),
     listing(_),!.


detWithSpace(WithSpace,String):-ddeterminer0(String),atom_concat(String,' ',WithSpace).
detWithSpace(WithSpace,String):-ddeterminer1(String),atom_concat(String,' ',WithSpace).

:-meta_predicate_transparent(determinerRemoved/3).
determinerRemoved(S0,Det,S):- fail,detWithSpace(WithSpace,String),string_concat(WithSpace,S,S0),string_lower(String,Det).

:-meta_predicate_transparent(query_description/1).
query_description(mudDescription(I,S)):-  is_asserted(mudDescription(I,S)).
query_description(dbase_t(mudDescription,I,S)):- is_asserted(mudDescription(I,S));is_asserted(mudKeyword(I,S)).


:-meta_predicate_transparent(remove_description/1).
remove_description(mudDescription(I,S)):- dmsg(trace_or_throw(remove_description(mudDescription(I,S)))).

:-meta_predicate_transparent(add_description/1).
add_description(mudDescription(I,S)):-add_description(I,S).

:-meta_predicate_transparent(add_description/2).
add_description(A,S0):-hooked_assertz(mudDescription(A,S0)),fail.
add_description(A,S0):- atomic(S0),string_concat('#$PunchingSomething ',S,S0),!,add_description(A,S).
% add_description(A,S0):-determinerRemoved(S0,String,S),!,add_description(A,S),add(determinerString(A,String)).
add_description(A,S0):-
   any_to_string(S0,S),
   atomic_list_concat(Words,' ',S),
   atomic_list_concat(Sents,'.',S),!,
   length(Words,Ws),
   must_det(add_description(A,S,S0,Ws,Sents,Words)).

% mudBareHandDamage: 10d10+75
add_description(A,S,_S0,Ws,_Sents,_Words):- Ws<3,  
   atomic_list_concat([K,V],': ',S),!,add_description_kv(A,K,V).

add_description(A,S,_S0,Ws,_Sents,_Words):- Ws<3,
   atomic_list_concat([K,V],'=',S),!,add_description_kv(A,K,V).

% "NOBACKSTAB","ACT_STAY_ZONE","MEMORY"
add_description(A,_S,_S0,1,_,[Word]):-add_description_word(A,Word),!.

%#$PunchingSomething ..
add_description(A,S,S0,Ws,Sents,['#$PunchingSomething',B|C]):-add_description(A,S,S0,Ws,Sents,[B|C]).
add_description(A,S,S0,Ws,Sents,[Det,B|C]):-ddeterminer(Det,L),add_description(A,S,S0,Ws,Sents,[B|C]),hooked_assertz(determinerString(A,L)).
add_description(A,S,S0,Ws,_Sents,_Words):-Ws>3,is_here_String(S),text_to_string(S0,String),!,hooked_assertz(descriptionHere(A,String)).
add_description(A,_S,S0,_Ws,_Sents,_Words):- any_to_string(S0,String),hooked_assertz(mudDescription(A,String)).

is_here_String(S):- atomic_list_concat_safe([_,is,_,here,_],S).
is_here_String(S):- atomic_list_concat_safe([_,here],S).
is_here_String(S):- atomic_list_concat_safe([_,is,here,_],S).


ddeterminer1('A').
ddeterminer1('An').
ddeterminer1('The').
ddeterminer0(a).
ddeterminer0(an).
ddeterminer0(the).
ddeterminer(L,L):-ddeterminer0(L).
ddeterminer(U,L):-string_lower(U,L),U\=L,!,ddeterminer0(L).

add_description_word(A,Word):- string_upper(Word,Word),string_lower(Word,Flag),string_to_atom(Flag,Atom),atom_concat(flagged_,Atom,FAtom),add((isa(A,FAtom))).
add_description_word(A,Word):- string_lower(Word,Word),add((mudKeyword(A,Word))).
add_description_word(A,Word):- string_lower(Word,Lower),add((mudKeyword(A,Lower))).


add_description_kv(A,K,V):- atom_concat('#$PunchingSomething ',Key,K),!,add_description_kv(A,Key,V).
add_description_kv(A,K,V):- atom_concat('+',Key,K),!,add_description_kv(A,Key,V).
add_description_kv(A,K,V):-atom_to_value(V,Term),C=..[K,A,Term],show_load_call(add(C)).


% =======================================================

add_term(A):-A==end_of_file.
add_term(pddlPredicates(List)):- !, with_assert_op_override(change(assert,one),maplist(decl_mpred,List)).
% add_term(C):- correctArgsIsa(change(assert,add),C,CC),!, add(CC),!.
add_term(':-'(A)):-(must(show_call((A)))).
add_term(A):-must(add(A)).

show_load_call(C):- logOnFailure(debugOnError(show_call(C))).

