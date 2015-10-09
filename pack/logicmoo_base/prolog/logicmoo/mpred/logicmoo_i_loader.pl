/** <module> 
% Game loading Utils
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% :-swi_module(logicmoo_i_loader, []).

:- user: use_module(library(shlib)).
:- user: use_module(library(operators)).

:- source_location(F,_),asserta(never_registered_mpred_file(F)).
% filetypes 
%
%  pfc - all terms are sent to mpred_add/1 (the the execeptions previously defined)
%  pl - all terms are sent to compile_clause/1 (the the execeptions previously defined)
%  prolog - all terms are sent to compile_clause/1 (even ones defined conflictingly)
%  dyn - all terms are sent to mpred_add/1 (even ones defined conflictingly)

:- thread_local(t_l:pretend_loading_file/1).
loading_source_file(F):-once(t_l:pretend_loading_file(F);prolog_load_context(source,F);loading_file(F)).


:-dynamic(user: never_reload_file/1).

load_language_file(Name0):- 
 forall(filematch_ext('qlf',Name0,Name),
  ((
   w_tl([(user:term_expansion(_,_):-!,fail),(user:goal_expansion(_,_):-!,fail),(system:term_expansion(_,_):-!,fail),(system:goal_expansion(_,_):-!,fail)],
     gripe_time(1,(user: load_files(Name,[qcompile(auto),register(false),if(not_loaded  )])->asserta(user: never_reload_file(Name));retract(user: never_reload_file(Name)))))))),!.
 


user:prolog_load_file(Module:Spec, Options):- fail,loop_check(prolog_load_file_nlc(Module:Spec, Options)).

prolog_load_file_nlc(Module:Spec, Options):- user: never_reload_file(Spec),
   wdmsg(warn(error(skip_prolog_load_file_nlc(user: never_reload_file(Module:Spec, Options))))),!.

prolog_load_file_nlc(Module:Spec, Options):- thread_self(TID), \+ is_main_thread,
   nop(wdmsg(warn(error(skip_prolog_load_file_nlc(wrong_thread(TID):-thread(Module:Spec, Options)))))),!.

prolog_load_file_nlc(Module:Spec, Options):- thread_self(TID), \+ is_main_thread,
   nop(wdmsg(warn(error(skip_prolog_load_file_nlc(wrong_thread(TID):-thread(Module:Spec, Options)))))),!,fail,dumpST.

prolog_load_file_nlc(Module:Spec, Options):- absolute_file_name(Spec,AFN,[extensions(['pl'])]), 
   (Spec\==AFN),exists_file_safe(AFN),!,prolog_load_file_nlc_0(Module:AFN, Options).

prolog_load_file_nlc(Module:DirName, Options):-  atom(DirName), is_directory(DirName)->
  current_predicate('load_file_dir'/2)->loop_check(show_call(call(load_file_dir,Module:DirName, Options))).

prolog_load_file_nlc(Module:FileName, Options):- exists_file_safe(FileName),!,
   prolog_load_file_nlc_0(Module:FileName, Options).

prolog_load_file_nlc(Module:Spec, Options):- term_to_atom(Spec,String),member(S,['?','*']),sub_atom(String,_,1,_,S),!, 
 foreach(user: filematch(Module:Spec,FileName),
    (loop_check((prolog_load_file_nlc_0(Module:FileName, Options))),TF=true)),!,
  nonvar(TF).

prolog_load_file_nlc_0(Module:Spec, Options):- thread_self(TID), \+ is_main_thread,
   wdmsg(warn(error(skip_prolog_load_file_nlc(wrong_thread(TID):-thread(Module:Spec, Options))))),!.

prolog_load_file_nlc_0(Module:FileName, Options):- 
  '$set_source_module'(SM,SM),
 (source_file_property(FileName,load_context(MC,SubFile:Line)),MC\==user,SM==user),!,
  wdmsg(skipping(prolog_load_file_nlc(Module:FileName, Options):source_file_property(FileName,load_context(MC,SubFile:Line)))),!.

prolog_load_file_nlc_0(Module:FileName, Options):-  
  file_name_extension(_Base,Ext,FileName),
  use_file_type_loader(Ext,Loader)-> loop_check(call(Loader,Module:FileName, Options)).

prolog_load_file_nlc_0(Module:FileName, Options):- fail, fail, fail, fail, fail, fail, fail, fail, fail, fail, fail, fail, 
  file_name_extension(_Base,Ext,FileName),
  guess_file_type_loader(Ext,Loader)-> loop_check(call(Loader,Module:FileName, Options)).

guess_file_type_loader(Ext,Loader):-use_file_type_loader(Ext,Loader).
guess_file_type_loader(Ext,Pred):- atom(Ext),
   (Ext==''->Pred='load_file_some_type';system:atom_concat('load_file_type_,',Ext,Pred)),
   current_predicate(Pred/2).

load_file_dir(Module:DirName, Options):- fail,
  directory_files(DirName,Files),
  foreach((member(F,Files),
            file_name_extension(_,Ext,F),
            guess_file_type_loader(Ext,Loader),
            current_predicate(Loader/2),
            directory_file_path(DirName,F,FileName)),
      (user:prolog_load_file(Module:FileName, Options),TF=true)),
     nonvar(TF).

use_file_type_loader(pfc,ensure_mpred_file_consulted).
use_file_type_loader(pddl,ensure_mpred_file_consulted).
use_file_type_loader(plmoo,ensure_mpred_file_consulted).
% use_file_type_loader(pl,ensure_prolog_file_consulted).

ensure_prolog_file_consulted(M:File,Options):-must(load_files(M:File,Options)),!.

ensure_mpred_file_consulted(M:File,Options):- 
  with_mpred_expansions(w_tl(t_l:pretend_loading_file(File),
              must((file_begin(pfc),
                    load_files(M:File,Options))))),!.

load_file_some_type(M:File,Options):-must(load_files(M:File,Options)),!.

:- include(logicmoo_i_header).
:- multifile(user: xfile_load_form/4).
:- dynamic(user: xfile_load_form/4).

is_elist_functor(isList).
is_elist_functor(ftListfn).
is_elist_functor(isEach).
is_elist_functor(isAnd).

as_list(EC,AL):-compound(EC),EC=..[IsEach|List],is_elist_functor(IsEach),as_list(List,AL),!.
as_list(List,AL):-flatten([List],AL),!.


disable_mpreds_in_current_file:- loading_source_file(F),show_call(asserta((t_l:disable_mpred_term_expansions_locally:-loading_source_file(F),!))).

:- export(with_no_mpred_expansions/1).
:- meta_predicate(with_no_mpred_expansions(0)).
with_no_mpred_expansions(Goal):-
  w_tl(tlbugger:no_buggery_tl,
    w_tl(t_l:disable_mpred_term_expansions_locally,Goal)).


:- export(with_mpred_expansions/1).
:- meta_predicate(with_mpred_expansions(0)).
with_mpred_expansions(Goal):-
  wno_tl(tlbugger:no_buggery_tl,
    wno_tl(t_l:disable_mpred_term_expansions_locally,Goal)).

:- export(ensure_loaded_no_mpreds/1).
:- meta_predicate(ensure_loaded_no_mpreds(0)).
ensure_loaded_no_mpreds(F):-with_no_mpred_expansions(forall(must_locate_file(F,L),ensure_loaded(L))).

use_was_isa(G,I,C):-call((current_predicate(mpred_types_loaded/0),if_defined(was_isa_syntax(G,I,C)))).

current_context_module(Ctx):-user: loading_module_h(Ctx),!.
current_context_module(Ctx):-context_module(Ctx).

% ========================================
% register_module_type/end_module_type
% ========================================
:- module_transparent(register_module_type/1).
:- export(registered_module_type/2).

register_module_type(Type):-current_context_module(CM),register_module_type(CM,Type).
register_module_type(CM,Types):-is_list(Types),!,forall(member(T,Types),register_module_type(CM,T)).
register_module_type(CM,Type):-asserta_new(registered_module_type(CM,Type)).

:-export(end_module_type/2).
end_module_type(Type):-current_context_module(CM),end_module_type(CM,Type).
end_module_type(CM,Type):-retractall(registered_module_type(CM,Type)).

/******

% :- meta_predicate(ensure_mpred_file_loaded(0)).

:- meta_predicate ensure_mpred_file_loaded(:,+).


ensure_mpred_file_loaded(M:F0,List):-!,
  must_locate_file(M:F0,F),  % scope_settings  expand(true),register(false),
  % 'format'(user_output /*e*/,'%  ~q + ~q -> ~q.~n',[M,F0,F]),
  load_files([F],[if(not_loaded), must_be_module(true)|List]).
   %load_files(F,[redefine_module(false),if(not_loaded),silent(false),redynamic_multifile_exported(true),must_be_module(true)|List]).   
ensure_mpred_file_loaded(M:F0,List):-
  must_locate_file(M:F0,F),  % scope_settings
  'format'(user_output /*e*/,'% load_mpred_file_M ~q.~n',[M=must_locate_file(F0,F)]),
   load_files([F],[redefine_module(false),module(M),expand(true),if(not_loaded),redynamic_multifile_exported(true),register(false),silent(false),must_be_module(true)|List]).

******/


:-export(declare_load_dbase/1).
declare_load_dbase(Spec):- forall(no_repeats_old(File,must_locate_file(Spec,File)),show_call(asserta_if_new(registered_mpred_file(File)))).

% :-export((is_compiling_sourcecode/1)).
is_compiling_sourcecode:-is_compiling,!.
is_compiling_sourcecode:-compiling, current_input(X),not((stream_property(X,file_no(0)))),prolog_load_context(source,F),not((lmconf:loading_mpred_file(_,_))),F=user,!.
is_compiling_sourcecode:-compiling,dmsg(system_compiling),!.

:-export(load_mpred_files/0).
load_mpred_files :- forall(registered_mpred_file(File),ensure_mpred_file_loaded(File)).

:-dynamic lmconf:current_world/1.
lmconf:current_world(current).

% =======================================================
% :- meta_predicate show_load_call(0).
% show_load_call(C):- logOnFailure(on_x_rtrace(show_call(C))).



:-dynamic(lmconf:loaded_file_world_time/3).
:-meta_predicate(lmconf:loaded_file_world_time(+,+,+)).
:-meta_predicate(get_last_time_file(+,+,+)).
get_last_time_file(FileIn,World,LastTime):- absolute_file_name(FileIn,File),lmconf:loaded_file_world_time(File,World,LastTime),!.
get_last_time_file(_,_,0).



:- meta_predicate(load_init_world(+,:)).
load_init_world(World,File):- 
 wno_tl(lmconf:use_cyc_database,
    ( world_clear(World),
      retractall(lmconf:loaded_file_world_time(_,_,_)),
      time_call(ensure_mpred_file_loaded(File)),!,
      time_call(finish_processing_world))).


:-meta_predicate(ensure_mpred_file_loaded(:)).

ensure_mpred_file_loaded(_M:FileIn):- \+exists_file_safe(FileIn),
  forall(must_locate_file(FileIn,File),ensure_mpred_file_loaded(File)).
ensure_mpred_file_loaded(_M:File):-
   time_file_safe(File,NewTime),!,
   get_last_time_file(File,_World,LastTime),
   (LastTime<NewTime -> force_reload_mpred_file(File) ; true).

:-meta_predicate(force_reload_mpred_file(?)).

:-meta_predicate(ensure_mpred_file_loaded(+,:)).
ensure_mpred_file_loaded(World,FileIn):- 
  w_tl(lmconf:current_world(World),ensure_mpred_file_loaded(FileIn)).

must_locate_file(FileIn,File):-
  filematch_ext(['','mpred','ocl','moo','plmoo','pl','plt','pro','p','pl.in'],FileIn,File).



force_reload_mpred_file(FileIn):- \+ exists_file_safe(FileIn),
   forall(must_locate_file(FileIn,File),force_reload_mpred_file(File)).

force_reload_mpred_file(_:File):- atomic(File),!,force_reload_mpred_file(File).
force_reload_mpred_file(File):-
   assert_if_new(registered_mpred_file(File)),
   lmconf:current_world(World),
   time_file_safe(File,NewTime),
   retractall(lmconf:loaded_file_world_time(File,World,_)),
   assert(lmconf:loaded_file_world_time(File,World,NewTime)), 
   dmsginfo(loading_mpred_file(File,World,NewTime)),!,
   lmconf:mpred_user_kb(DBASE),'@'(force_reload_mpred_file(World,File),DBASE).

force_reload_mpred_file(World,_:File):- atomic(File),!,force_reload_mpred_file(World,File).
force_reload_mpred_file(World, File):-
 assert_if_new(registered_mpred_file(File)),
 must(lmconf:current_world(World)),  
 wno_tl(t_l:disable_mpred_term_expansions_locally,
  catch((w_tl(lmconf:loading_mpred_file(World,File),     
      ensure_loaded(File)),      
      load_mpred_file_end(World,File)),
   Error,
    (wdmsg(error(Error,File)),retractall(lmconf:loaded_mpred_file(World,File)),
     retractall(lmconf:loaded_file_world_time(File,World,_AnyTime))))).

:-export(load_mpred_file_end/2).
load_mpred_file_end(World,File):-
   asserta_new(lmconf:loaded_mpred_file(World,File)),
   dmsginfo(info(load_mpred_file_complete(File))),
   forall(onEndOfFile(File,Call),must((mpred_call(Call),retractall(onEndOfFile(File,Call))))).

show_load_context:- 
  listing(registered_mpred_file),
  show_call(inside_file(_)),
  show_call(mpred_may_expand),
  show_call(mpred_expand_inside_file_anyways),
  show_call(t_l:mpred_term_expansion_ok),
  show_call(loading_source_file(_)).



%load_mpred_name_stream(_Name):- do_gc,repeat,read_one_term(Term,Vs),myDebugOnError(add_term(Term,Vs)),Term == end_of_file,!.
%load_mpred_name_stream(_Name,Stream):- do_gc,repeat,read_one_term(Stream,Term,Vs),myDebugOnError(add_term(Term,Vs)),Term == end_of_file,!.


add_term(end_of_file,_):-!.
add_term(Term,Vs):- 
   b_setval('$variable_names', Vs),
    add_from_file(Term).


add_from_file(Term):-  
  w_tl(t_l:mpred_already_in_file_expansion(Term),must(add(Term))).

myDebugOnError(Term):-catch(once(must((Term))),E,(dmsg(error(E,start_myDebugOnError(Term))),trace,rtrace((Term)),dmsginfo(stop_myDebugOnError(E=Term)),trace,Term)).
         
read_one_term(Term,Vs):- catch(once(( read_term(Term,[double_quotes(string),variable_names(Vs)]))),E,(Term=error(E),dmsg(error(E,read_one_term(Term))))).
read_one_term(Stream,Term,Vs):- catch(once(( read_term(Stream,Term,[double_quotes(string),variable_names(Vs)]))),E,(Term=error(E),dmsg(error(E,read_one_term(Term))))).

% rescan_mpred_stubs:- doall((user:mpred_prop(F,prologHybrid),arity(F,A),A>0,warnOnError(declare_mpred_local_dynamic(moo,F,A)))).


/*
:-ensure_loaded(plarkc(logicmoo_i_sexpr_reader)).

:- parse_to_source(
  "(documentation instance EnglishLanguage \"An object is an &%instance of a &%SetOrClass if it is included in that &%SetOrClass. 
  An individual may be an instance of many classes, some of which may be subclasses of others. 
  Thus, there is no assumption in the meaning of &%instance about specificity or uniqueness.\")",
  Out),writeq(Out).
*/

is_kif_string([]):- !,fail.
is_kif_string(String):-atomic(String),name(String,Codes), memberchk(40,Codes),memberchk(41,Codes).

from_kif_string(String,Forms) :- must((codelist_to_forms(String,Forms);input_to_forms(string(String),Forms))),!.

assert_kif(String):- from_kif_string(String,Forms),dmsg(warn(assert_kif(Forms))),!.

assert_kif_dolce(String):-from_kif_string(String,Forms),dmsg(warn(assert_kif_dolce(Forms))),!.


:-meta_predicate(doall_and_fail(0)).

finish_processing_world :- load_mpred_files, loop_check(w_tl(t_l:agenda_slow_op_do_prereqs,doall(finish_processing_dbase)),true).

doall_and_fail(Call):- time_call(once(doall(Call))),fail.


:-export(etrace/0).
etrace:-leash(-all),leash(+exception),trace.


:-export(onEndOfFile/1).
:- thread_local(onEndOfFile/2).
onEndOfFile(Call):- (prolog_load_context(source,F),loading_source_file(F)),asserta(onEndOfFile(F,Call)).

assert_until_eof(F):- must_det_l((loading_source_file(File),asserta(F),asserta((onEndOfFile(File,retract(F)))))).

:- style_check(-singleton).
:- style_check(-discontiguous).
% :- style_check(-atom).

% gload:- ensure_mpred_file_loaded(savedb),!.
gload:- ensure_mpred_file_loaded(logicmoo('rooms/startrek.all.mpred')).

%:-meta_predicate(savedb/0).
savedb:-!.
savedb:- on_x_rtrace(rsavedb),!.
%:-meta_predicate(rsavedb/0).
rsavedb:-
 on_x_rtrace(agenda_mpred_repropigate),
 catch((   
   ignore(catch(make_directory('/tmp/lm/'),_,true)),
   ignore(catch(delete_file('/tmp/lm/savedb'),E,(dmsginfo(E:delete_file('/tmp/lm/savedb'))))),   
   tell('/tmp/lm/savedb'),make_db_listing,told),E,dmsginfo(savedb(E))),!.


make_db_listing:-
 % lmconf:mpred_user_kb(DBM),
%   listing(t),
 %  listing(mpred_f),
     listing(_),
     listing(user: _),  
     listing(dbase:_),
     listing(dyn:_),
     listing(moo_loader:_),
     listing(world :_),
     listing(_),!.




hdr_debug(_,_):-!.
hdr_debug(F,A):-'format'(F,A).
:-meta_predicate module_typed_term_expand(?,?).


module_typed_term_expand(X,_):-not(compound(X)),!,fail.
module_typed_term_expand( ((':-'(_))) , _ ):-!,fail.
module_typed_term_expand(_:B1,B2):-!,module_typed_term_expand(B1,B2),!.
module_typed_term_expand(X,Y):- compound(X),loading_module_h(CM),functor_catch(X,F,A),module_typed_term_expand(CM,X,F,A,Y).

module_typed_term_expand(CM,X,F,A,Y):-findall(Y,term_expand_local_each(CM,X,F,A,Y),Ys), Ys == [],!,fail.  

term_expand_local_each(_,_,F,A,_):- member(F / A,[never_expand]),!,fail.
term_expand_local_each(CM,X,F,A,X):-registered_module_type(CM,utility),export(F/A).
term_expand_local_each(CM,X,F,A,X):-registered_module_type(CM,dynamic),dynamic(F/A).





% ========================================
% include_mpred_file(MASK)
% ========================================

include_mpred_files(Mask):- 
     forall(must_locate_file(Mask,E),ensure_mpred_file_loaded(E)).
/*
module(M,Preds):-
    'format'(user_output /*e*/,'% visting module ~w.~n',[M]),
    forall(member(P,Preds),export(P)).
*/
scan_updates:-thread_property(X,alias(loading_code)),thread_property(X,status(running)),!.
scan_updates:-!.
scan_updates:-ignore(catch(make,_,true)).

/*
do_term_expansions:- context_module(CM), (do_term_expansions(CM)).

do_term_expansions(_):- thread_self(ID),user: always_expand_on_thread(ID),!.
%do_term_expansions(_):- always_transform_heads,not(prevent_transform_mpreds),!.
do_term_expansions(_):- is_compiling_clause.
do_term_expansions(CM):- registered_mpred_file(CM),!, not(ended_transform_mpreds), not(prevent_transform_mpreds).

check_term_expansions:- not(do_term_expansions).
*/

% :- (do_term_expansions(_)->true;throw(not_term_expansions)).


:-  op(1120,fx,export),op(1120,fx,export).

:-export(((current_context_module/1,
    module_typed_term_expand/2,
         register_module_type/1,          
         end_module_type/1))).




loader_term_expansion(CL,EXP):- 
 % ==== why we assert
  inside_file(pfc),!,
% ==== do it
  WHY = '$was_imported_kb_content$'(inside_file(pfc),CL),
  %dmsg(WHY),
  mpred_add(CL),
  must(EXP=user: WHY).

loader_term_expansion(CL,WHY):- 
% ==== why we assert
  requires_storage(CL,WhyRS),
% ==== do it
  WHY = '$was_imported_kb_content$'(requires_storage(WhyRS),CL),
  %dmsg(WHY),
  add_from_file(CL),!.


loader_term_expansion(CL,EXP):- 
 % ==== why we assert
  inside_file(dyn),!,
% ==== do it
  WHY = '$was_imported_kb_content$'(inside_file(dyn),CL),
  %dmsg(WHY),
  add_from_file(CL),
  must(EXP=user: WHY).



:- use_module(library(lists)).
:- user: ensure_loaded(library(terms)).
:- user: ensure_loaded(library(base32)).

% :-autoload.

% https://docs.google.com/document/u/0/export?format=txt&id=1yyGne4g8vXKxNPKIKVLOtt0OxIM2kxyfmvjqR1lgbcY
% http_get
:- asserta_if_new(t_l:infForward).

:- dynamic(mpred_skipped_module/1).
mpred_skipped_module(eggdrop).
:-forall(current_module(CM),assert(mpred_skipped_module(CM))).
:-retractall(mpred_skipped_module(pfc)).
% :-show_call(loading_module(X)),retractall(X).

%:-listing(mpred_skipped_module/1).


:-multifile(t_l:into_form_code).
:-thread_local(t_l:into_form_code).
:-thread_local(t_l:disable_mpred_term_expansions_locally /0).
:-dynamic(lmconf:t_l:disable_mpred_term_expansions_locally_globally/0).

%fwc:-true.
%bwc:-true.
cwc:-true.

%is_fc_body(P):- cnotrace(fwc==P ; (compound(P),arg(1,P,E),is_fc_body(E))),!.
%is_bc_body(P):- cnotrace(bwc==P ; (compound(P),arg(1,P,E),is_bc_body(E))),!.
is_code_body(P):- cnotrace(cwc==P ; (compound(P),arg(1,P,E),is_code_body(E))),!.


:- meta_predicate(with_source_module(:,(*))).
with_source_module(M:_,CALL):- !, setup_call_cleanup('$set_source_module'(Old, M),CALL,'$set_source_module'(_, Old)).
with_source_module(M,CALL):- setup_call_cleanup('$set_source_module'(Old, M),CALL,'$set_source_module'(_, Old)).

get_file_type(File,Type):-var(File),!,loading_source_file(File),get_file_type(File,Type).
get_file_type(File,Type):-lmcache:mpred_directive_value(Type,_,File).
get_file_type(File,Type):-file_name_extension(_,Type,File).

is_mpred_file(F):- var(F),!,loading_source_file(F), is_mpred_file(F),!.
is_mpred_file(F):- file_name_extension(_,pfc,F),!.
is_mpred_file(F):- file_name_extension(_,plmoo,F),!.
is_mpred_file(F):- registered_mpred_file(F),!.
is_mpred_file(F):- never_registered_mpred_file(F),!,fail.
is_mpred_file(F):- is_mpred_file0(F),!,asserta(registered_mpred_file(F)),!.
is_mpred_file(F):- asserta(never_registered_mpred_file(F)),!,fail.

decache_file_type(F):- retractall(registered_mpred_file(F)),retractall(never_registered_mpred_file(F)).

is_mpred_file0(F):- filematch(prologmud(**/*),F),!.
is_mpred_file0(F):- inside_file(pfc),!,loading_source_file(F).
is_mpred_file0(F):- file_name_extension(_,WAS,F),WAS\=pl,WAS\= '',WAS\=chr,!.

must_compile_special_clause(:- (_) ):-!,fail.
%must_compile_special_clause(CL):- sanity(nonvar(CL)),not(t_l:into_form_code),not(t_l:mpred_already_in_file_expansion(CL)),not((get_functor(CL,F),expanded_already_functor(F))).
must_compile_special_clause(CL):- \+ t_l:disable_mpred_term_expansions_locally, 
   sanity(nonvar(CL)), \+(t_l:into_form_code),
    \+(t_l:mpred_already_in_file_expansion(CL)),
    \+((get_functor(CL,F),expanded_already_functor(F))),
   mpred_db_type(CL,_),!.

:-multifile(t_l:mpred_module_expansion/1).
:-thread_local(t_l:mpred_module_expansion/1).

mpred_use_module(M):- must(atom(M)),retractall(mpred_skipped_module(M)),show_call(asserta_if_new(t_l:mpred_module_expansion(M))).

% ================================================================================
% DETECT PREDS THAT NEED SPECIAL STORAGE 
% ================================================================================
compile_this(_,_,_,_):- t_l:disable_mpred_term_expansions_locally,!,fail.
compile_this(_,F,M:C,O):-atom(M),!,compile_this(M,F,C,O).

compile_this(M,F,'$was_imported_kb_content$'(_,_),pl):-!.
compile_this(M,F,C,mpred_add):- is_mpred_file(F), \+ mpred_skipped_module(M),!.
compile_this(M,F,C,dyn):- inside_file(dyn),!.
compile_this(M,F,C,O):- (var(M);var(F);var(C)),trace_or_throw(var_compile_this(M,F,C,O)).
compile_this(M,F,C,requires_storage(WHY)):- requires_storage(C,WHY),!.
compile_this(M,F,C,must_compile_special):- must_compile_special_clause(C),t_l:mpred_already_inside_file_expansion(C).
compile_this(_,_,_,pl).

:- module_transparent(mpred_may_expand).
mpred_may_expand:-loading_source_file(F),inside_file(pfc).
mpred_may_expand:-loading_source_file(F),inside_file(mpred).
mpred_may_expand:-must(loading_module(M)),mpred_may_expand_module(M),!,mpred_expand_inside_file_anyways.

mpred_may_expand_module(M):-mpred_skipped_module(M),!,fail.
mpred_may_expand_module(M):-lmcache:mpred_directive_value(pfc,module,M),!.
mpred_may_expand_module(M):-module_property(M,file(F)),is_mpred_file(F).
mpred_may_expand_module(M):-t_l:mpred_module_expansion(M),!.
mpred_may_expand_module(_):-t_l:mpred_module_expansion(*),!.


mpred_expand_inside_file_anyways:- loading_source_file(F),!,mpred_expand_inside_file_anyways(F).

mpred_expand_inside_file_anyways(F):- var(F),!,loading_source_file(F),nonvar(F),mpred_expand_inside_file_anyways(F).
mpred_expand_inside_file_anyways(F):- lmconf:loading_mpred_file(_,F),!.
mpred_expand_inside_file_anyways(F):- registered_mpred_file(F).
mpred_expand_inside_file_anyways(F):- is_mpred_file(F),must(loading_module(M);source_module(M)), (M=user; \+ mpred_skipped_module(M)),!.

was_exported_content(I,CALL,Output):-Output='$was_imported_kb_content$'(I,CALL),!.

:- thread_local(t_l:mpred_term_expansion_ok/0).
:- thread_local(t_l:mpred_already_inside_file_expansion/1).



:-assert_if_new(t_l:mpred_term_expansion_ok).


:-module_transparent(mpred_file_module_term_expansion_4/4).
mpred_file_module_term_expansion_4(F,M,A,BBBO):-compile_this(M,F,A,How),
   must(source_location(F,L)),user: xfile_load_form(How,M,A,B),b_getval('$variable_names', Vs),
   mpred_file_module_term_expansion_1(How,M,A,Vs,F,L,B,BBBO).

:-module_transparent(mpred_file_module_term_expansion_1/7).
mpred_file_module_term_expansion_1(How,M,A,Vs,F,L,B,BBBO):-
   copy_term(A:B:Vs,AA:BB:VVs),mpred_implode_varnames(VVs),INFO=info(M:How,AA->BB,F:L,VVs),
   mpred_file_module_term_expansion_2(How,INFO,F,M,AA,B,BBBO),!.

:-module_transparent( mpred_file_module_term_expansion_2/6).
mpred_file_module_term_expansion_2(How,INFO,F,M,AA,BBB,BBBO):- 
   (BBB = (:- _) -> BBBO = BBB ;
      (How == pl -> BBBO = BBB ;
        (add_from_file(BBB), BBBO = '$was_imported_kb_content$'(AA,INFO)))),!.      

user: xfile_load_form(How,M,P,O):- 
  w_tl(t_l:mpred_already_in_file_expansion(P), 
   ((mpred_file_expansion(P,C),P\=@=C,O=(:- user: (C))))),!.

lmconf:mpred_provide_clauses(H,B,(What)):- !.


show_interesting_cl(Dir,P):- loading_source_file(File),get_file_type(File,Type),
  ((nonvar(Dir),functor(Dir,Type,_))->true;dmsg(Type:cl_assert(Dir,P))).

:-meta_predicate(cl_assert(?,?)).
cl_assert(kif(Dir),P):- show_call(must_det_l(( show_interesting_cl(kif(Dir),P),kif_process(P)))),!.
cl_assert(Dir,P):- show_interesting_cl(Dir,P),mpred_add(P).
cl_assert(pl,P):-  !, show_call(must_det_l((source_location(F,_L), '$compile_aux_clauses'(P,F)))).
cl_assert(_Code,P):- !, show_call(mpred_add(P)).

:- meta_predicate(mpred_file_expansion_cl_assert(?,?,?,?)).
mpred_file_expansion_cl_assert(_,cl_assert(pl,OO),OO,_):-!,show_interesting_cl(pl,OO).
mpred_file_expansion_cl_assert(I,cl_assert(OTHER,OO),OO,I):- inside_file(kif),is_kif_rule(OO),!,mpred_file_expansion_cl_assert(I,cl_assert(kif(OTHER),OO),OO,I).
mpred_file_expansion_cl_assert(I,CALL,OO,O):- (current_predicate(_,CALL) -> ((must(call(CALL)),was_exported_content(I,CALL,OO))); OO=O).

in_include_file:- prolog_load_context(file,F),!, \+ prolog_load_context(source,F).

% ensure we only process onEndOfFile directive at the end of the actual source files
do_end_of_file_actions:- in_include_file,!.
do_end_of_file_actions:- must(loading_source_file(F)),
   GETTER=onEndOfFile(F,TODO),forall(GETTER,((doall(show_call_failure(TODO))),ignore(retract(GETTER)))).

:- export(mpred_file_expansion/2).
:- meta_predicate(mpred_file_expansion(?,?)).
mpred_file_expansion(I,OO):- var(I),!,I=OO.

mpred_file_expansion(I,OO):-
  must(if_defined(current_source_location(FL),source_location(FL,_))),
   w_tl(t_l:current_why_source(FL),mpred_file_expansion0(I,OO)).


mpred_file_expansion0(end_of_file,end_of_file):-once(do_end_of_file_actions),!,fail.
mpred_file_expansion0(I,OO):- (I\=(:-(_))), I\= '$was_imported_kb_content$'(_,_),
   once(loop_check(mpred_file_expansion_0a(I,O))),
   I\=@=O, 
   (((t_l:mpred_term_expansion_ok;mpred_expand_inside_file_anyways)-> nop(wdmsg((mpred_file_expansion(I,O)))) ; ((show_load_context,wdmsg(warning,wanted_mpred_term_expansion(I,O))),fail)),
   ((O=(:-(CALL))) -> 
     mpred_file_expansion_cl_assert(I,CALL,OO,O);      
      (OO = O))).



mpred_implode_varnames([]):-!.
mpred_implode_varnames([N=V|Vs]):-V='$VAR'(N),mpred_implode_varnames(Vs),!.

% mudKeyword("happy","happy") -> mudKeyword(vHappy,"happy").

% must skip already loaded modules (we remember these so make/0 doesnt break)
mpred_maybe_skip(M):- t_l:mpred_module_expansion(N),N==M,!.
mpred_maybe_skip(M):- asserta_if_new(mpred_skipped_module(M)),!.
% :- forall(current_module(M),mpred_maybe_skip(M)).


:- dynamic(lmcache:mpred_directive_value/3).


expanded_already_functor('$was_imported_kb_content$').
expanded_already_functor(was_enabled).
expanded_already_functor(_:NV):-nonvar(NV),!,expanded_already_functor(NV).

% expanded_already_functor(F):-mpred_prop(F,pl).


%:-thread_local is_compiling_clause/0.
%is_compiling:-is_compiling_clause;compiling.


:- multifile(user:term_expansion/2).
:- multifile(system:goal_expansion/2).
% system:goal_expansion(A,_B):-fail,hotrace((source_module(M),(M=mpred_sanity;M=user;M=system),if_defined(pmsg(M:goal_expansion(A)),format(user_output /*e*/,'~N% ~q~n',M:goal_expansion(A))))),fail.
% user:term_expansion(A,_B):-fail,hotrace((source_module(M),(M=mpred_sanity;M=user;M=system),if_defined(pmsg(M:term_expansion(A)),format(user_output /*e*/,'~N% ~q~n',M:term_expansion(A))))),fail.

% system:goal_expansion(N,mpred_prove_neg(P)):-fail,mpred_from_negation_plus_holder(N,P),show_call_failure(mpred_prop(P,pfcControlled)).



:-thread_local(mpred_mpred_add_loaded).

% TODO DISABLE THIS NEXT CLAUSE LATER
mpred_directive_expansion(_,_):- (\+ current_predicate(logicmoo_bugger_loaded/0)),!,fail.
mpred_directive_expansion(_,_):- t_l:disable_mpred_term_expansions_locally,!,fail.

mpred_directive_expansion(mpred_ops, 
           ( op(500,fx,('~')),op(500,fx,('neg')),op(1075,xfx,('<-')), op(1075,xfx,('<==>')),op(1075,xfx,('<-')), op(1100,fx,('nesc')), op(1150,xfx,('::::')))).
mpred_directive_expansion(mpred_dcg,( file_begin(pfc), op(400,yfx,('\\\\')),op(1200,xfx,('-->>')),op(1200,xfx,('--*>>')), op(1200,xfx,('<<--')))).
mpred_directive_expansion(mpred_multifile,
           ( asserta(lmcache:mpred_directive_value(pfc,multifile,M)),
                 decl_mpred_multifile(user),
                 include(logicmoo(mpred/logicmoo_i_header)))):- source_module(M).


mpred_directive_expansion(mpred_module,(asserta(lmcache:mpred_directive_value(pfc,module,M)))):-source_module(M).


decl_mpred_multifile(M):-
                 multifile(M:('<-')/2),
                    multifile(M:('::::')/2),
                 multifile(M:('<==>'/2)),
                 multifile(M:(('==>')/2)),
                 multifile(M:('nesc')/1),
                 multifile(M:('~')/1),
                 multifile(M:('neg')/1),
                 export(M:('<-')/2),
                    export(M:('::::')/2),
                 export(M:('<==>'/2)),
                 export(M:(('==>')/2)),
                 export(M:('nesc')/1),
                 export(M:('~')/1),
                 export(M:('neg')/1).

% ========================================
% begin/end_transform_mpreds
% ========================================
:-dynamic(current_op_alias/2).
:-dynamic(current_lang/1).


~(~G):- nonvar(G),!, mpred_call(~neg(G)).
~(G):- mpred_call(neg(G)).


:-dynamic(always_expand_on_thread/1).
:-thread_local is_compiling_clause/0.
is_compiling:-is_compiling_clause;compiling.

:- style_check(+discontiguous).
:- style_check(-discontiguous).

begin_pfc:-file_begin(pfc).
mpred_begin:-file_begin(pfc).
dyn_begin:-file_begin(dyn).
dyn_end:-file_end(dyn).

enable_mpred_expansion:- (( \+ t_l:disable_mpred_term_expansions_locally) -> true ;
                 (retractall(t_l:disable_mpred_term_expansions_locally),
                 onEndOfFile(asserta_if_new(t_l:disable_mpred_term_expansions_locally)))).

disable_mpred_expansion:- (( t_l:disable_mpred_term_expansions_locally) -> true ;
                 (asserta_if_new(t_l:disable_mpred_term_expansions_locally),
                 onEndOfFile(retractall(t_l:disable_mpred_term_expansions_locally)))).


file_begin(W):-  
  must_det((enable_mpred_expansion, loading_source_file(ISource),
  op_lang(W), assert_until_eof(user: current_lang(W)),
   decache_file_type(ISource),
   assert_until_eof(lmcache:mpred_directive_value(W,file,ISource)))),
   must_det(( loading_source_file(Source),decache_file_type(Source),asserta(lmcache:mpred_directive_value(W,file,Source)))).
file_end(W):- must_det(( loading_source_file(ISource),decache_file_type(ISource),ignore(retract(lmcache:mpred_directive_value(W,file,ISource))))),  
  must_det(( loading_source_file(Source),decache_file_type(Source),ignore(retract(lmcache:mpred_directive_value(W,file,Source))))).

inside_file(W) :- prolog_load_context(file,Source),lmcache:mpred_directive_value(W,_,Source),!.
inside_file(W) :- prolog_load_context(source,Source),lmcache:mpred_directive_value(W,_,Source),!.
inside_file(W) :- loading_source_file(Source),!,lmcache:mpred_directive_value(W,_,Source),!.


user:term_expansion((:- (M:DIR)),O):-atom(M),atom(DIR),with_source_module(M, ((mpred_directive_expansion(DIR,OO),!, must(O=(:- OO))))).
user:term_expansion((:- DIR),O):- atom(DIR), mpred_directive_expansion(DIR,OO),!,must(O=(:- OO)).

:-meta_predicate(mpred_file_expansion_0c(?,?)).
:-meta_predicate(mpred_file_expansion_0a(?,?)).
:-meta_predicate(mpred_file_expansion_z(?,?)).

% Specific "*SYNTAX*" based default

:- ensure_loaded(library(logicmoo/plarkc/dbase_i_sexpr_reader)).


op_alias(OP,OTHER):-retractall(current_op_alias(OP,_)),asserta(current_op_alias(OP,OTHER)).
op_lang(LANG):-retractall(current_op_alias(_,_)),retractall(current_lang(_)),asserta(current_lang(LANG)).

get_op_alias(OP,ALIAS):-current_op_alias(OP,ALIAS).
get_op_alias(OP,ALIAS):-get_lang(LANG),lang_op_alias(LANG,OP,ALIAS).

current_op_alias((<==>),dup(impliesF,(','))).
current_op_alias((=>),==>).
%current_op_alias((not),(neg)).
current_op_alias( not(:-),neg(:-)).
current_op_alias( (:-),(:-)).

get_lang(LANG):-current_lang(LANG),!.
get_lang(pfc).

% pfc
lang_op_alias(pfc,(<==>),(<==>)).
lang_op_alias(pfc,(==>),==>).
lang_op_alias(pfc,(<=>),(<==>)).
lang_op_alias(pfc,(<=),(<-)).
lang_op_alias(pfc,(<-),(<-)).
lang_op_alias(pfc,(not),(neg)).
lang_op_alias(pfc,not(:-),neg(:-)).
lang_op_alias(pfc,(:-),(:-)).
% lang_op_alias(pfc,(A=B),{(A=B)}).
% kif
lang_op_alias(kif,(<==>),(<==>)).
lang_op_alias(kif,(==>),==>).
lang_op_alias(kif,(not),(neg)).
lang_op_alias(kif,(~),(neg)).
lang_op_alias(kif,(=>),(if)).
lang_op_alias(kif,(<=>),(iff)).
lang_op_alias(kif, not(':-'),neg('<-')).
lang_op_alias(kif,(:-),rev(==>)).
% cyc
lang_op_alias(cyc,(<==>),(<==>)).
lang_op_alias(cyc,(==>),==>).
lang_op_alias(cyc,(implies),(if)).
lang_op_alias(cyc,(equiv),(iff)).
lang_op_alias(cyc, not(':-'),neg('<-')).
lang_op_alias(cyc,(:-),rev(==>)).
% prolog
lang_op_alias(prolog,(<==>),(<==>)).
lang_op_alias(prolog,(==>),==>).
lang_op_alias(prolog, not(':-'),neg('<-')).
lang_op_alias(prolog,(:-),(:-)).
lang_op_alias(prolog,(<=),(<=)).
lang_op_alias(prolog,(<-),(<-)).

transform_opers(LANG,PFCM,PFCO):- w_tl(current_lang(LANG),((transitive_lc(transform_opers_0,PFCM,PFC),!, subst(PFC,(not),(neg),PFCO)))).

transform_opers_0(AIS,AIS):- leave_as_is(AIS),!.
transform_opers_0((A/B),C):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]),conjoin_op((/),AA,BB,C).
transform_opers_0(PFCM,PFC):- transform_opers_1(PFCM,PFC),!.
transform_opers_0(=>(A),=>(C)):- !, transform_opers_0(A,C).
transform_opers_0(==>(A),==>(C)):- !, transform_opers_0(A,C).
transform_opers_0(~(A),~(C)):- !, transform_opers_0(A,C).
transform_opers_0(nesc(A),nesc(C)):- !, transform_opers_0(A,C).
transform_opers_0({A},{A}):-!.
transform_opers_0((A;B),C):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]),conjoin_op((;),AA,BB,C).
transform_opers_0((B=>A),(BB=>AA)):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]).
transform_opers_0((B==>A),(BB==>AA)):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]).
transform_opers_0(<=(A,B),<=(AABB)):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]).
transform_opers_0((A<-B),(AA<-BB)):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]).
transform_opers_0((A<=>B),(AA<=>BB)):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]).
transform_opers_0((A<==>B),(AA<==>BB)):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]).
transform_opers_0((A<==>B),(AA<==>BB)):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]).
transform_opers_0(if(A,B),if(AA,BB)):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]).
transform_opers_0(iff(A,B),iff(AA,BB)):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]).
transform_opers_0(implies(A,B),implies(AA,BB)):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]).
transform_opers_0(equiv(A,B),equiv(AA,BB)):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]).
transform_opers_0((B:-A),OUTPUT):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]),=((BB:-AA),OUTPUT).
transform_opers_0(not(A),OUTPUT):- !, must_maplist(transform_opers_0,[A],[AA]),=(not(AA),OUTPUT).
transform_opers_0(not(A),C):- !, transform_opers_0(neg(A),C).
%transform_opers_0((A),OUTPUT):- !, must_maplist(transform_opers_0,[A],[AA]),=((AA),OUTPUT).
transform_opers_0(O,O).

transform_opers_1(not(AB),(BBAA)):- get_op_alias(not(OP),rev(OTHER)), atom(OP),atom(OTHER),AB=..[OP,A,B],!, must_maplist(transform_opers_0,[A,B],[AA,BB]),BBAA=..[OTHER,BB,AA].
transform_opers_1(not(AB),(BOTH)):- get_op_alias(not(OP),dup(OTHER,AND)),atom(OTHER), atom(OP),AB=..[OP,A,B],!, must_maplist(transform_opers_0,[A,B],[AA,BB]),AABB=..[OTHER,AA,BB],BBAA=..[OTHER,BB,AA],BOTH=..[AND,AABB,BBAA].
transform_opers_1(not(AB),neg(NEG)):- get_op_alias(not(OP),neg(OTHER)),atom(OTHER), atom(OP),AB=..[OP|ABL],!, must_maplist(transform_opers_0,ABL,AABB),NEG=..[OTHER|AABB].
transform_opers_1(not(AB),(RESULT)):- get_op_alias(not(OP),(OTHER)), atom(OP),atom(OTHER),AB=..[OP|ABL],!, must_maplist(transform_opers_0,ABL,AABB),RESULT=..[OTHER|AABB].
transform_opers_1((AB),(BBAA)):- get_op_alias(OP,rev(OTHER)), atom(OP),atom(OTHER),AB=..[OP,A,B],!, must_maplist(transform_opers_0,[A,B],[AA,BB]),BBAA=..[OTHER,BB,AA].
transform_opers_1((AB),(BOTH)):- get_op_alias(OP,dup(OTHER,AND)), atom(OP),atom(OTHER),AB=..[OP,A,B],!, must_maplist(transform_opers_0,[A,B],[AA,BB]),AABB=..[OTHER,AA,BB],BBAA=..[OTHER,BB,AA],BOTH=..[AND,AABB,BBAA].
transform_opers_1((AB),(RESULT)):- get_op_alias(OP,(OTHER)),atom(OP), atom(OTHER),AB=..[OP|ABL],!, must_maplist(transform_opers_0,ABL,AABB),RESULT=..[OTHER|AABB].
transform_opers_1(OP,OTHER):- get_op_alias(OPO,OTHER),OPO=OP,!.


mpred_file_expansion_0a(X,Y):- current_predicate(mpred_loader_file/0),current_predicate(mpred_mpred_file/0), expand_term(X,M),!,mpred_file_expansion_0c(M,Y),!.

mpred_file_expansion_0c(I,OO):-
   is_kif_string(I),must_det_l((input_to_forms(atom(I),Wff,Vs),b_setval('$variable_names',Vs),!,
     must((sexpr_sterm_to_pterm(Wff,O),!,\+ is_list(O))))),mpred_file_expansion_0c(O,OO).

mpred_file_expansion_0c(PI,OO):- PI=..[P,I],
   is_kif_string(I),must_det_l((input_to_forms(atom(I),Wff,Vs),b_setval('$variable_names',Vs),!,
     must((sexpr_sterm_to_pterm(Wff,O),!,\+ is_list(O))))),
   PO=..[P,O], mpred_file_expansion_0c(PO,OO).

mpred_file_expansion_0c(C,O):- compound(C), get_op_alias(OP,ALIAS),
  atom(OP),atom(ALIAS),C=..[OP|ARGS],CC=..[ALIAS|ARGS],loop_check(mpred_file_expansion_0c(CC,O)),!.

mpred_file_expansion_0c(C,O):- get_lang(LANG),transform_opers(LANG,C,M),C\=@=M,!,mpred_file_expansion_0c(M,O).
mpred_file_expansion_0c(C,O):- \+ current_predicate(logicmoo_util_varnames_file/0),!, (C=M),mpred_file_expansion_z(M,O).
mpred_file_expansion_0c(C,O):- if_defined(ensure_vars_labled(C,M),(C=M)),mpred_file_expansion_z(M,O).

mpred_file_expansion_z((<=(Q,P)),(:- cl_assert(pfc(bwc),(Q<-P)))).
mpred_file_expansion_z(((P==>Q)),(:- cl_assert(pfc(fwc),(P==>Q)))).
mpred_file_expansion_z((('=>'(Q))),(:- cl_assert(pfc(fwc),('=>'(Q))))).
mpred_file_expansion_z((('==>'(Q))),(:- cl_assert(pfc(fwc),('=>'(Q))))).
mpred_file_expansion_z(((nesc(Q))),(:- cl_assert(pfc(fwc),nesc(Q)))).
mpred_file_expansion_z(('<-'(P,Q)),(:- cl_assert(pfc(bwc),('<-'(P,Q))))).
mpred_file_expansion_z(('<==>'(P,Q)),(:- cl_assert(pfc(bwc),(P<==>Q)))).
mpred_file_expansion_z(neg(Q),(:- cl_assert(pfc(fwc),neg(Q)))).
mpred_file_expansion_z(~(Q),(:- cl_assert(pfc(fwc),~(Q)))).

mpred_file_expansion_z(if(P,Q),(:- cl_assert(kif(fwc),if(P,Q)))).
mpred_file_expansion_z(iff(P,Q),(:- cl_assert(kif(fwc),iff(P,Q)))).
mpred_file_expansion_z(not(Q),(:- cl_assert(kif(fwc),not(Q)))).
mpred_file_expansion_z(exists(V,PQ),(:- cl_assert(kif(fwc),exists(V,PQ)))).
mpred_file_expansion_z(forall(V,PQ),(:- cl_assert(kif(fwc),forall(V,PQ)))).
mpred_file_expansion_z(all(V,PQ),(:- cl_assert(kif(fwc),all(V,PQ)))).


% maybe reverse some rules?
%mpred_file_expansion_z((P==>Q),(:- cl_assert(pfc(fwc),('<-'(Q,P))))).  % speed-up attempt
mpred_file_expansion_z((RuleName :::: Rule),(:- cl_assert(named_rule,(RuleName :::: Rule)))).
mpred_file_expansion_z((==>(P)),(:- cl_assert(pfc(fwc),(==>(P))))).
mpred_file_expansion_z(Fact,(:- cl_assert(pl,Fact))):- get_functor(Fact,F,_A),if_defined(prologDynamic(F)).
mpred_file_expansion_z(Fact,Output):- mpred_file_expansion_1(_Dir,Fact,C),must(mpred_file_expansion_z(C,Output)),!.

      mpred_file_expansion_1(pfc(act),(H:-(Chain,B)),(PFC==>PH)):-cwc, is_action_body(Chain),pl_to_mpred_syntax((Chain,B),PFC),pl_to_mpred_syntax_h(H,PH).
      mpred_file_expansion_1(pfc(awc),(H:-(Chain,B)),(PH==>PFC)):-cwc, has_body_atom(twc,Chain),pl_to_mpred_syntax((Chain,B),PFC),pl_to_mpred_syntax_h(H,PH).
      mpred_file_expansion_1(pfc(fwc),(H:-(Chain,B)),(PFC==>PH)):-cwc, is_fc_body(Chain),pl_to_mpred_syntax((Chain,B),PFC),pl_to_mpred_syntax_h(H,PH),can_be_dynamic(PH),make_dynamic(PH).
      mpred_file_expansion_1(pfc(bwc),(H:-(Chain,B)),(PH<-PFC)):-cwc, is_bc_body(Chain),pl_to_mpred_syntax((Chain,B),PFC),pl_to_mpred_syntax_h(H,PH),can_be_dynamic(PH),make_dynamic(PH).
      mpred_file_expansion_1(Type,In,Out):-mpred_file_expansion_1b(Type,In,Out),!.

      mpred_file_expansion_1b(pfc(act),(H:-Chain,B),(H==>{(Chain,B)})):-cwc, is_action_body(Chain),make_dynamic(H).
      mpred_file_expansion_1b(pfc(fwc),(H:-Chain,B),((Chain,B)==>H)):-cwc, is_fc_body(Chain),make_dynamic(H).
      mpred_file_expansion_1b(pfc(bwc),(H:-Chain,B),(H<-(Chain,B))):-cwc, is_bc_body(Chain),make_dynamic(H).

/*
*/


can_be_dynamic(H):- \+ is_static_pred(H), \+ predicate_property(H,static),  \+ predicate_property(H,meta_predicate(_)).

pl_to_mpred_syntax_h(A,PFC_A):- must(pl_to_mpred_syntax0(A,PFC_A)),!, PFC_A \= '{}'(_).
pl_to_mpred_syntax(A,PFC_A):- must(pl_to_mpred_syntax0(A,PFC_A)),!.

pl_to_mpred_syntax0(A,A):-is_ftVar(A),!.
pl_to_mpred_syntax0((A,B),PFC):-!,pl_to_mpred_syntax(A,PFC_A),pl_to_mpred_syntax(B,PFC_B),conjoin_body(PFC_A,PFC_B,PFC).
pl_to_mpred_syntax0(pfc(A),A):-!.
pl_to_mpred_syntax0(A,{A}):-!.

mpred_file_expansion_z((H:-Chain,B),(H:-(B))):- is_code_body(Chain),!,fail,must(atom(Chain)),make_dynamic(H).


% Specific "*PREDICATE CLASS*" based default
mpred_file_expansion_z(Fact,Fact):- get_functor(Fact,F,A),prologDynamic(F),!.
mpred_file_expansion_z(Fact,(:- ((cl_assert(Dir,Fact))))):- mpred_file_expansion_2(Dir,Fact,_Output),!.

      % Specific "*PREDICATE CLASS*" based default
      mpred_file_expansion_2(pfc(pred_type),Fact,Output):- get_functor(Fact,F,A),if_defined(ttPredType(F)),Output='$was_imported_kb_content$'(Fact,ttPredType(F)),!.
      mpred_file_expansion_2(pfc(func_decl),Fact,Output):- get_functor(Fact,F,A),if_defined(functorDeclares(F)),Output='$was_imported_kb_content$'(Fact,functorDeclares(F)),!.
      mpred_file_expansion_2(pfc(macro_head),Fact,Output):- get_functor(Fact,F,A),if_defined(prologMacroHead(F)),Output='$was_imported_kb_content$'(Fact,prologMacroHead(F)),!.
      mpred_file_expansion_2(pfc(mpred_ctrl),Fact,Output):- get_functor(Fact,F,A),if_defined(pfcControlled(F)),Output='$was_imported_kb_content$'(Fact,pfcControlled(F)),!.
      mpred_file_expansion_2(pfc(hybrid),Fact,Output):- get_functor(Fact,F,A),if_defined(prologHybrid(F)),Output='$was_imported_kb_content$'(Fact,pfcControlled(F)),!.
      mpred_file_expansion_2(pfc(pl),Fact,Output):- get_functor(Fact,F,A),if_defined(prologDynamic(F)),Output='$was_imported_kb_content$'(Fact,pfcControlled(F)),!.


% Specific "*FILE*" based default
mpred_file_expansion_z(Fact,Fact):- inside_file(pl),!.
mpred_file_expansion_z(Fact,(:- ((cl_assert(pfc(mpred_file),Fact))))):- inside_file(pfc),!.
mpred_file_expansion_z(Fact,(:- ((cl_assert(dyn(dyn_file),Fact))))):- inside_file(dyn),!.
mpred_file_expansion_z(Fact,(:- ((cl_assert(mpred(mpreds_file),Fact))))):- inside_file(mpreds),!.
/*
mpred_file_expansion_z(Fact,(:- ((cl_assert(pfc(expand_file),Fact))))):-
    cnotrace(mpred_expand_inside_file_anyways(F)),!,_Output='$was_imported_kb_content$'(Fact,mpred_expand_inside_file_anyways(F)),!.
*/

stream_pos(File:LineNo):-loading_source_file(File),current_input(S),stream_property(S, position(Position)), !,stream_position_data(line_count, Position, LineNo),!.

compile_clause(CL):- make_dynamic(CL),must((assertz_if_new(CL),clause_asserted(CL))).

make_dynamic(C):- compound(C),get_functor(C,F,A),
  functor(P,F,A),
  ( \+predicate_property(P,_) -> dynamic(F/A) ; (predicate_property(P,dynamic)->true;dynamic_safe(P))),!,
  must((predicate_property(P,dynamic))).




varFunctorEscape('?').
varFunctorEscape('\2323\').
varFunctorEscape('&').
varFunctorEscape(A):-atom(A),atom_codes(A,[C]),C>255.

to_var_functors(Outer,In,Out):-  
 varFunctorEscape(VFE),
   \+ compound(In)->In=Out;
  (compound_name_arguments(In,Name,Args),
   (Args==[]->Out=Name;
      ((Name=VFE,Args=[JustOne] )-> (to_var_functors(VFE,JustOne,VOut),(functor(VOut,t,_)->Out=VOut;Out=..[VFE,VOut]));
      ( maplist(to_var_functors(Name),Args,ArgsO),
      ((Name\='[|]',Outer=VFE,atom_codes(Name,[C|_]),code_type(C,prolog_var_start),
         nb_getval('$variable_names', Vs),(member(Name=Var,Vs)->true;b_setval('$variable_names', [Name=Var|Vs])))
           -> Out=..[t,Var|ArgsO];  (Args==ArgsO->(Out=In);compound_name_arguments(Out,Name,ArgsO))))))).

system:term_expansion(I,O):- current_prolog_flag(allow_variable_name_as_functor,true),
   current_predicate(logicmoo_bugger_loaded/0),compound(I),functor(I,VFE,1),varFunctorEscape(VFE),
                     \+ t_l:disable_mpred_term_expansions_locally,
                       w_tl(t_l:disable_mpred_term_expansions_locally,to_var_functors((:-),I,O)),I\=@=O.


system:goal_expansion(I,O):- current_prolog_flag(allow_variable_name_as_functor,true),
   current_predicate(logicmoo_bugger_loaded/0),compound(I),functor(I,VFE,1),varFunctorEscape(VFE),
                     \+ t_l:disable_mpred_term_expansions_locally,
                       to_var_functors((:-),I,O),I\=@=O.


%user:goal_expansion(G,OUT):- \+  t_l:disable_mpred_term_expansions_locally, G\=isa(_,_),(use_was_isa(G,I,C)),!,to_isa_out(I,C,OUT).
%user:term_expansion(G,OUT):- \+  t_l:disable_mpred_term_expansions_locally, hotrace(use_was_isa(G,I,C)),!,to_isa_out(I,C,OUT).
%user:term_expansion(I,O):- \+ t_l:disable_mpred_term_expansions_locally, t_l:consulting_sources, wno_tl(t_l:consulting_sources,add(I)),O=true.



% :-set_prolog_flag(allow_variable_name_as_functor,true).

% :- source_location(S,_),forall(loading_source_file(H,S),ignore(( \+predicate_property(M:H,built_in), functor(H,F,A),M:module_transparent(F/A),M:export(F/A)))).


mpred_process_input_1(':-'(TT)):-!,must(TT),!.
mpred_process_input_1('?-'(TT)):-!,doall(must(TT)),!.
mpred_process_input_1('$was_imported_kb_content$'(_,_)):-!.
mpred_process_input_1(T):-try_save_vars(T),mpred_add(T),!.
mpred_process_input(':-'(T),Vs):- b_setval('$variable_names', Vs),must(T),!.
mpred_process_input(T,Vs):- expand_term(T,TT),b_setval('$variable_names', Vs),mpred_process_input_1(TT),!.

process_this_script:- show_call(prolog_load_context(script,_)), prolog_load_context(stream,S), process_this_script(S).
process_this_script(S):- at_end_of_stream(S),!.
process_this_script(S):- repeat,once(process_this_script0(S)),at_end_of_stream(S).

process_this_script0(S):- at_end_of_stream(S),!.
process_this_script0(S):- peek_string(S,3,W), W="\n\n\n",get_code(S,_),get_code(S,_),!,process_this_script0(S).
process_this_script0(S):- peek_string(S,2,W), W="\r\n",get_code(S,_),!,process_this_script0(S).
process_this_script0(S):- peek_string(S,2,W), W="\n\n",get_code(S,_),!,process_this_script0(S).
process_this_script0(S):- peek_code(S,W),member(W,`\n`),get_code(S,P),put(P),!,process_this_script0(S).
process_this_script0(S):- peek_code(S,W),member(W,` \t\r`),get_code(S,_),!,process_this_script0(S).
process_this_script0(S):- peek_string(S,2,W),W="%=",!,read_line_to_string(S,String),format('~N~s~n',[String]).
process_this_script0(S):- peek_string(S,1,W),W="%",!,read_line_to_string(S,_String).
process_this_script0(S):- read_term(S,T,[variable_names(Vs)]),b_setval('$variable_names', Vs),format('~N~n',[]),portray_one_line(T),format('~N~n',[]),!,mpred_process_input(T,Vs). 



mpred_loader_file.



