/** <module> 
% Game loading Utils
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% :-swi_module(logicmoo_i_loader, []).

:- include(logicmoo_i_header).


:- export(with_no_mpred_expansions/1).
:- meta_predicate(with_no_mpred_expansions(0)).
with_no_mpred_expansions(Goal):-
  with_assertions(user:prolog_mud_disable_term_expansions,Goal).


current_context_module(Ctx):-user:loading_module_h(Ctx),!.
current_context_module(Ctx):-context_module(Ctx).

% ========================================
% register_module_type/end_module_type
% ========================================
:- module_transparent register_module_type/1.
:- export registered_module_type/2.

register_module_type(Type):-current_context_module(CM),register_module_type(CM,Type).
register_module_type(CM,Types):-is_list(Types),!,forall(member(T,Types),register_module_type(CM,T)).
register_module_type(CM,Type):-asserta_new(registered_module_type(CM,Type)).

:-export(end_module_type/2).
end_module_type(Type):-current_context_module(CM),end_module_type(CM,Type).
end_module_type(CM,Type):-retractall(registered_module_type(CM,Type)).



:-dynamic(registered_mpred_file/1).
:-export(declare_load_dbase/1).
declare_load_dbase(Spec):- forall(no_repeats_old(File,filematch(Spec,File)),show_call(asserta_if_new(registered_mpred_file(File)))).

% :-export((is_compiling_sourcecode/1)).
is_compiling_sourcecode:-is_compiling,!.
is_compiling_sourcecode:-compiling, current_input(X),not((stream_property(X,file_no(0)))),prolog_load_context(source,F),not((thglobal:loading_mpred_file(_,_))),F=user,!.
is_compiling_sourcecode:-compiling,dmsg(system_compiling),!.

:-export(load_mpred_files/0).
load_mpred_files :- forall(registered_mpred_file(File),ensure_plmoo_loaded(File)).

:-dynamic thglobal:current_world/1.
thglobal:current_world(current).

:- meta_predicate show_load_call(0).

ensure_mpred_file(Mask):- ensure_plmoo_loaded(Mask).

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
   must(thglobal:current_world(World)),
   time_file_safe(File,NewTime),!,
   get_last_time_file(File,World,LastTime),
   (LastTime<NewTime -> reload_plmoo_file(File) ; true).

:-meta_predicate_transparent(reload_plmoo_file/1).

reload_plmoo_file(FileIn):-
   absolute_file_name(FileIn,File),
   thglobal:current_world(World),
   retractall(thglobal:loaded_file_world_time(File,World,_)),   
   mpred_mod(DBASE),'@'(load_data_file(World,File),DBASE).

:-meta_predicate_transparent(load_data_file/2).
load_data_file(World,FileIn):- with_assertions(thglobal:current_world(World),load_data_file(FileIn)).


must_locate_file(FileIn,File):-
  must((stream_property(_,file_name(F)),
    absolute_file_name(FileIn,File,
         [file_errors(fail),relative_to(F),expand(true),access(read),extensions(['','plmoo','moo','pl','plt','pro','p'])]))),!.
   

:-thread_local(thlocal:onEndOfFile/2).
:-meta_predicate_transparent(load_data_file/1).
load_data_file(FileIn):- must(load_data_file_now(FileIn)).


load_data_file_now(FileIn):-
   must(must_locate_file(FileIn,File)),
   must(thglobal:current_world(World)),
  time_file_safe(File,NewTime),
  assert(thglobal:loaded_file_world_time(File,World,NewTime)), 
   dmsginfo(loading_data_file(File,World,NewTime)),!,

  catch((with_assertions(thglobal:loading_mpred_file(World,File),
      must(setup_call_cleanup(see(File),load_mpred_name_stream(World),seen))),
      load_data_file_end(World,File)),
   Error,
    (wdmsg(error(Error,File)),retractall(thglobal:loaded_mpred_file(World,File)),
     retractall(thglobal:loaded_file_world_time(File,World,NewTime)))).

:-export(load_data_file_end/2).
load_data_file_end(World,File):-
   asserta_new(thglobal:loaded_mpred_file(World,File)),
   dmsginfo(info(load_data_file_complete(File))),
   forall(onEndOfFile(File,Call),must((mpred_call(Call),retractall(onEndOfFile(File,Call))))).

load_mpred_name_stream(_Name):- do_gc,repeat,read_one_term(Term,Vs),myDebugOnError(add_term(Term,Vs)),Term == end_of_file,!.
load_mpred_name_stream(_Name,Stream):- do_gc,repeat,read_one_term(Stream,Term,Vs),myDebugOnError(add_term(Term,Vs)),Term == end_of_file,!.


add_term(end_of_file,_):-!.
add_term(Term,Vs):- 
   b_setval('$variable_names', Vs),
    add_from_file(Term).


add_from_file(Term):-  
  with_assertions(thlocal:already_in_file_term_expansion,must(add(Term))).

myDebugOnError(Term):-catch(once(must((Term))),E,(dmsg(error(E,start_myDebugOnError(Term))),trace,rtrace((Term)),dmsginfo(stop_myDebugOnError(E=Term)),trace,Term)).
         
read_one_term(Term,Vs):- catch(once(( read_term(Term,[double_quotes(string),variable_names(Vs)]))),E,(Term=error(E),dmsg(error(E,read_one_term(Term))))).
read_one_term(Stream,Term,Vs):- catch(once(( read_term(Stream,Term,[double_quotes(string),variable_names(Vs)]))),E,(Term=error(E),dmsg(error(E,read_one_term(Term))))).

% rescan_mpred_stubs:- doall((user:mpred_prop(F,prologHybrid),arity(F,A),A>0,warnOnError(declare_mpred_local_dynamic(moo,F,A)))).


/*
:-ensure_loaded(logicmoo_i_sexpr_reader).

:- parse_to_source(
  "(documentation instance EnglishLanguage \"An object is an &%instance of a &%SetOrClass if it is included in that &%SetOrClass. 
  An individual may be an instance of many classes, some of which may be subclasses of others. 
  Thus, there is no assumption in the meaning of &%instance about specificity or uniqueness.\")",
  Out),writeq(Out).
*/

assert_kif(_).
assert_kif(String):-must((codelist_to_forms(String,Forms);parse_to_source(string(String),Forms))),dmsg(Forms),!.
assert_kif_dolce(_).

%:-meta_predicate_transparent(finish_processing_world).
%:-meta_predicate(finish_processing_world).

%:- dynamic(finish_processing_world/0).

% :-module_transparent(finish_processing_world()).

% :-meta_predicate_transparent(finish_processing_world).
% :-meta_predicate_transparent(finish_processing_dbase()).
%:-meta_predicate_transparent(rescan_all/0).
:-meta_predicate_transparent(doall_and_fail(0)).

finish_processing_world :- load_mpred_files, loop_check_local(with_assertions(thlocal:agenda_slow_op_do_prereqs,doall(finish_processing_dbase)),true).

doall_and_fail(Call):- time_call(once(doall(Call))),fail.


:-export(etrace/0).
etrace:-leash(-all),leash(+exception),trace.


:-export(onEndOfFile/1).
:-dynamic(onEndOfFile/2).
onEndOfFile(Call):-current_filesource(F),asserta(onEndOfFile(F,Call)).

assert_until_end_of_file(Fact):-must_det_l((thread_local(Fact),asserta(Fact),((onEndOfFile(mpred_op(change( retract,one),Fact)))))).

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
 debugOnError(agenda_mpred_repropigate),
 catch((   
   ignore(catch(make_directory('/tmp/lm/'),_,true)),
   ignore(catch(delete_file('/tmp/lm/savedb'),E,(dmsginfo(E:delete_file('/tmp/lm/savedb'))))),   
   tell('/tmp/lm/savedb'),make_db_listing,told),E,dmsginfo(savedb(E))),!.


%:-meta_predicate_transparent(make_db_listing/0).
make_db_listing:-
 % mpred_mod(DBM),
%   listing(t),
 %  listing(mpred_f),
     listing(_),
     listing(user:_),  
     listing(dbase:_),
     listing(dyn:_),
     listing(moo_loader:_),
     listing(world :_),
     listing(_),!.



% =======================================================

show_load_call(C):- logOnFailure(debugOnError(show_call(C))).


:- meta_predicate locate_moo_file(:,-).

locate_moo_file(I,O):-locate_moo_file0(I,O),!.
locate_moo_file(_:O,O):-!.
locate_moo_file(O,O).
locate_moo_file0(user:SpecPre, Path):-!,locate_moo_file0(SpecPre, Path).
locate_moo_file0(_:SpecPre, Path):-!,locate_moo_file0(SpecPre, Path).
locate_moo_file0(SpecPre, Path) :-
        catch((expand_file_search_path(SpecPre,Spec)),_,fail),
        catch(absolute_file_name(Spec,
                           [ file_type(prolog),
                             access(read)
                           ],
                           Path),_,fail),
        exists_file(Path),!.

:- meta_predicate ensure_moo_loaded(:).

% the once/1s here arte just for dmiles institional memory
ensure_moo_loaded(A) :-
   setup_call_cleanup(once(asserta(must_compile_special_clause_file(_))),
        load_moo_files(A),
        once(retract(must_compile_special_clause_file(asdasdasd)))).

:- meta_predicate load_moo_files(:,+).

load_moo_files(F0):- user:ensure_loaded(F0).
% load_moo_files(F0):-use_module(F0).

load_moo_files(M:F0,List):-!,
  locate_moo_file(M:F0,F),  % scope_settings  expand(true),register(false),
  % 'format'(user_error,'%  ~q + ~q -> ~q.~n',[M,F0,F]),
  load_files(F,[if(not_loaded), must_be_module(true)|List]).
   %load_files(F,[redefine_module(false),if(not_loaded),silent(false),redynamic_multifile_exported(true),must_be_module(true)|List]).   
load_moo_files(M:F0,List):-
  locate_moo_file(M:F0,F),  % scope_settings
  'format'(user_error,'% load_moo_files_M ~q.~n',[M=locate_moo_file(F0,F)]),
   load_files(F,[redefine_module(false),module(M),expand(true),if(not_loaded),redynamic_multifile_exported(true),register(false),silent(false),must_be_module(true)|List]).



hdr_debug(_,_):-!.
hdr_debug(F,A):-'format'(F,A).
:-meta_predicate module_typed_term_expand(?,?).
% :-meta_predicate user:term_expansion(?,?).


module_typed_term_expand(X,_):-not(compound(X)),!,fail.
module_typed_term_expand( ((':-'(_))) , _ ):-!,fail.
module_typed_term_expand(_:B1,B2):-!,module_typed_term_expand(B1,B2),!.
module_typed_term_expand(X,Y):- compound(X),loading_module_h(CM),functor_catch(X,F,A),module_typed_term_expand(CM,X,F,A,Y).

module_typed_term_expand(CM,X,F,A,Y):-findall(Y,term_expand_local_each(CM,X,F,A,Y),Ys), Ys == [],!,fail.  

term_expand_local_each(_,_,F,A,_):- member(F / A,[never_expand]),!,fail.
term_expand_local_each(CM,X,F,A,X):-registered_module_type(CM,utility),export(F/A).
term_expand_local_each(CM,X,F,A,X):-registered_module_type(CM,dynamic),dynamic(F/A).


% user:term_expansion(X,Y):- module_typed_term_expand(X,Y).





% ========================================
% include_moo_files(MASK)
% ========================================

include_moo_files(Mask):- 
     forall(filematch(Mask,E),ensure_moo_loaded(E)).
/*
module(M,Preds):-
    'format'(user_error,'% visting module ~w.~n',[M]),
    forall(member(P,Preds),export(P)).
*/
scan_updates:-thread_property(X,alias(loading_code)),thread_property(X,status(running)),!.
scan_updates:-!.
scan_updates:-ignore(catch(make,_,true)).


do_term_expansions:- context_module(CM), (do_term_expansions(CM)).

do_term_expansions(_):- thread_self(ID),always_expand_on_thread(ID),!.
do_term_expansions(_):- always_transform_heads,not(prevent_transform_moo_preds),!.
do_term_expansions(_):- is_compiling_clause.
do_term_expansions(CM):- must_compile_special_clause_file(CM),!, not(ended_transform_moo_preds), not(prevent_transform_moo_preds).

check_term_expansions:- not(do_term_expansions).

% :- (do_term_expansions(_)->true;throw(not_term_expansions)).


:-  op(1120,fx,export),op(1120,fx,export).

:-export(((current_context_module/1,
    module_typed_term_expand/2,
         register_module_type/1,          
         end_module_type/1))).


% ========================================
% begin/end_transform_moo_preds
% ========================================

:-thread_local is_compiling_clause/0.
is_compiling:-is_compiling_clause;compiling.
:-thread_local ended_transform_moo_preds/0, always_expand_on_thread/1, prevent_transform_moo_preds/0, always_transform_heads/0.
:-module_transparent begin_transform_moo_preds/0, end_transform_moo_preds/0.
:-export(((begin_transform_moo_preds/0,end_transform_moo_preds/0))).
begin_transform_moo_preds:- retractall(ended_transform_moo_preds),context_module(CM),asserta(must_compile_special_clause_file(CM)).
end_transform_moo_preds:- retractall(ended_transform_moo_preds),asserta(ended_transform_moo_preds).


:- style_check(+discontiguous).
:- style_check(-discontiguous).

:-export(thlocal:in_dynamic_reader/1).

:-export(begin_dynamic_reader/0).
begin_dynamic_reader:- must_det(( prolog_load_context(file,Source),asserta(thlocal:in_dynamic_reader(Source)))).
:-export(end_dynamic_reader/0).
end_dynamic_reader:- must_det(( prolog_load_context(file,Source),retract(thlocal:in_dynamic_reader(Source)))).


inside_dynamic_reader :- prolog_load_context(file,Source),test_tl(thlocal:in_dynamic_reader(Source)),!.
inside_dynamic_reader :- prolog_load_context(source,Source),test_tl(thlocal:in_dynamic_reader(Source)),!.


loader_term_expansion(CL,EXP):- 
 % ==== why we assert
  inside_dynamic_reader,!,
% ==== do it
  WHY = '$was_imported_kb_content$'(inside_dynamic_reader,CL),
  %dmsg(WHY),
  add_from_file(CL),
  must(EXP=user:WHY).

loader_term_expansion(CL,WHY):- 
% ==== why we assert
  requires_storage(CL,WhyRS),
% ==== do it
  WHY = '$was_imported_kb_content$'(requires_storage(WhyRS),CL),
  %dmsg(WHY),
  add_from_file(CL),!.


