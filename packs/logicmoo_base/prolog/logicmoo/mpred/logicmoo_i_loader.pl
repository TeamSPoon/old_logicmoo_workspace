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

is_elist_functor(isList).
is_elist_functor(ftListfn).
is_elist_functor(isEach).
is_elist_functor(isAnd).

as_list(EC,AL):-compound(EC),EC=..[IsEach|List],is_elist_functor(IsEach),as_list(List,AL),!.
as_list(List,AL):-flatten([List],AL),!.


disable_mpreds_in_current_file:- source_file(F,_),show_call(asserta((thlocal:disable_mpred_term_expansions_locally:-source_file(F,_),!))).

:- export(with_no_mpred_expansions/1).
:- meta_predicate(with_no_mpred_expansions(0)).
with_no_mpred_expansions(Goal):-
  with_assertions(thlocal:disable_mpred_term_expansions_locally,Goal).

use_was_isa(G,I,C):-call((current_predicate(mpred_types_loaded/0),if_defined(was_isa_syntax(G,I,C)))).

current_context_module(Ctx):-user:loading_module_h(Ctx),!.
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

:-meta_predicate(load_dbase(?)).

load_dbase(File):-thglobal:current_world(World), load_dbase(World,File),!.
load_dbase(World,File):- 
 with_no_assertions(thglobal:use_cyc_database,
    ( world_clear(World),
      retractall(thglobal:loaded_file_world_time(_,_,_)),
      time_call(ensure_plmoo_loaded(File)),!,
      time_call(finish_processing_world))).


:-meta_predicate(ensure_plmoo_loaded(?)).
ensure_plmoo_loaded(Mask):-
  forall(filematch(Mask,File1),ensure_plmoo_loaded_each(File1)).

:-dynamic(thglobal:loaded_file_world_time/3).
:-meta_predicate(thglobal:loaded_file_world_time(+,+,+)).
:-meta_predicate(get_last_time_file(+,+,+)).
get_last_time_file(FileIn,World,LastTime):- absolute_file_name(FileIn,File),thglobal:loaded_file_world_time(File,World,LastTime),!.
get_last_time_file(_,_,0).

:-meta_predicate(ensure_plmoo_loaded_each(?)).
ensure_plmoo_loaded_each(FileIn):-
   absolute_file_name(FileIn,File),
   must(thglobal:current_world(World)),
   time_file_safe(File,NewTime),!,
   get_last_time_file(File,World,LastTime),
   (LastTime<NewTime -> reload_plmoo_file(File) ; true).

:-meta_predicate(reload_plmoo_file(?)).

reload_plmoo_file(FileIn):-
   absolute_file_name(FileIn,File),
   thglobal:current_world(World),
   retractall(thglobal:loaded_file_world_time(File,World,_)),   
   user:mpred_mod(DBASE),'@'(load_data_file(World,File),DBASE).

:-meta_predicate(load_data_file(+,+)).
load_data_file(World,FileIn):- with_assertions(thglobal:current_world(World),load_data_file(FileIn)).


must_locate_file(FileIn,File):-
  must((stream_property(_,file_name(F)),
    absolute_file_name(FileIn,File,
         [file_errors(fail),relative_to(F),expand(true),access(read),extensions(['','plmoo','moo','pl','plt','pro','p'])]))),!.
   

:-thread_local(thlocal:onEndOfFile/2).
:-meta_predicate(load_data_file(?)).
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


:-meta_predicate(doall_and_fail(0)).

finish_processing_world :- load_mpred_files, loop_check(with_assertions(thlocal:agenda_slow_op_do_prereqs,doall(finish_processing_dbase)),true).

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

%:-meta_predicate(savedb/0).
savedb:-!.
savedb:- debugOnError(rsavedb),!.
%:-meta_predicate(rsavedb/0).
rsavedb:-
 debugOnError(agenda_mpred_repropigate),
 catch((   
   ignore(catch(make_directory('/tmp/lm/'),_,true)),
   ignore(catch(delete_file('/tmp/lm/savedb'),E,(dmsginfo(E:delete_file('/tmp/lm/savedb'))))),   
   tell('/tmp/lm/savedb'),make_db_listing,told),E,dmsginfo(savedb(E))),!.


make_db_listing:-
 % user:mpred_mod(DBM),
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
  % 'format'(user_output /*e*/,'%  ~q + ~q -> ~q.~n',[M,F0,F]),
  load_files(F,[if(not_loaded), must_be_module(true)|List]).
   %load_files(F,[redefine_module(false),if(not_loaded),silent(false),redynamic_multifile_exported(true),must_be_module(true)|List]).   
load_moo_files(M:F0,List):-
  locate_moo_file(M:F0,F),  % scope_settings
  'format'(user_output /*e*/,'% load_moo_files_M ~q.~n',[M=locate_moo_file(F0,F)]),
   load_files(F,[redefine_module(false),module(M),expand(true),if(not_loaded),redynamic_multifile_exported(true),register(false),silent(false),must_be_module(true)|List]).



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
% include_moo_files(MASK)
% ========================================

include_moo_files(Mask):- 
     forall(filematch(Mask,E),ensure_moo_loaded(E)).
/*
module(M,Preds):-
    'format'(user_output /*e*/,'% visting module ~w.~n',[M]),
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





:- use_module(library(lists)).
:- user:ensure_loaded(library(terms)).
:- user:ensure_loaded(library(base32)).

% :-autoload.

% https://docs.google.com/document/u/0/export?format=txt&id=1yyGne4g8vXKxNPKIKVLOtt0OxIM2kxyfmvjqR1lgbcY
% http_get
:- asserta_if_new(thlocal:infForward).

:- dynamic(pfc_skipped_module/1).
pfc_skipped_module(eggdrop).
:-forall(current_module(CM),assert(pfc_skipped_module(CM))).
:-retractall(pfc_skipped_module(pfc)).
% :-show_call(loading_module(X)),retractall(X).

:-lsting(pfc_skipped_module/1).


:-multifile(thlocal:into_form_code).
:-thread_local(thlocal:into_form_code).
:-thread_local(thlocal:disable_mpred_term_expansions_locally/0).
:-dynamic(thglobal:thlocal:disable_mpred_term_expansions_locally_globally/0).

%fwc:-true.
%bwc:-true.
cwc:-true.

%is_fc_body(P):- notrace(fwc==P ; (compound(P),arg(1,P,E),is_fc_body(E))),!.
%is_bc_body(P):- notrace(bwc==P ; (compound(P),arg(1,P,E),is_bc_body(E))),!.
is_code_body(P):- notrace(cwc==P ; (compound(P),arg(1,P,E),is_code_body(E))),!.


:- meta_predicate(with_source_module(:,(*))).
with_source_module(M:_,CALL):- !, setup_call_cleanup('$set_source_module'(Old, M),CALL,'$set_source_module'(_, Old)).
with_source_module(M,CALL):- setup_call_cleanup('$set_source_module'(Old, M),CALL,'$set_source_module'(_, Old)).



pfc_using_file(F):- var(F),!,source_file(F), pfc_using_file(F),!.
pfc_using_file(F):- inside_file(pfc),!.
pfc_using_file(F):- file_name_extension(_,pfc,F),!.
pfc_using_file(F):- file_name_extension(_,plmoo,F),!.
pfc_using_file(F):- file_name_extension(_,WAS,F),WAS\=pl,WAS\= '',WAS\=chr,!.

must_compile_special_clause(:- (_) ):-!,fail.
%must_compile_special_clause(CL):- sanity(nonvar(CL)),not(thlocal:into_form_code),not(thlocal:already_in_file_term_expansion),not((get_functor(CL,F),expanded_already_functor(F))).
must_compile_special_clause(CL):- \+ thlocal:disable_mpred_term_expansions_locally, 
   sanity(nonvar(CL)), \+(thlocal:into_form_code),
    \+(thlocal:already_in_file_term_expansion),
    \+((get_functor(CL,F),expanded_already_functor(F))),
   pfc_db_type(CL,_),!.

:-multifile(thlocal:pfc_module_expansion/1).
:-thread_local(thlocal:pfc_module_expansion/1).

pfc_use_module(M):- must(atom(M)),retractall(pfc_skipped_module(M)),show_call(asserta_if_new(thlocal:pfc_module_expansion(M))).

% ================================================================================
% DETECT PREDS THAT NEED SPECIAL STORAGE 
% ================================================================================
compile_this(_,_,_,_):- thlocal:disable_mpred_term_expansions_locally,!,fail.
compile_this(_,F,M:C,O):-atom(M),!,compile_this(M,F,C,O).
compile_this(M,F,'$was_imported_kb_content$'(_,_),code):-!.
compile_this(M,F,C,pfc_add):- pfc_using_file(F), \+ pfc_skipped_module(M),!.
compile_this(M,F,C,O):- (var(M);var(F);var(C)),trace_or_throw(var_compile_this(M,F,C,O)).
compile_this(M,F,C,requires_storage(WHY)):- requires_storage(C,WHY),!.
compile_this(M,F,C,dynamic_reader):- inside_file(dynamic_reader),!.
compile_this(M,F,C,must_compile_special):- must_compile_special_clause(C),in_file_expansion.
compile_this(_,_,_,code).

:- module_transparent(pfc_may_expand).
pfc_may_expand:-source_location(F,_),inside_file(pfc).
pfc_may_expand:-must(loading_module(M)),pfc_may_expand_module(M),!.

pfc_may_expand_module(M):-pfc_skipped_module(M),!,fail.
pfc_may_expand_module(M):-user:mpred_directive_value(pfc,module,M),!.
pfc_may_expand_module(M):-module_property(M,file(F)),pfc_using_file(F).
pfc_may_expand_module(M):-thlocal:pfc_module_expansion(M),!.
pfc_may_expand_module(_):-thlocal:pfc_module_expansion(*),!.


:-module_transparent(pfc_file_module_term_expansion/4).
pfc_file_module_term_expansion(F,M,A,BBBO):-compile_this(M,F,A,How),   
   user:xfile_load_form(How,M,A,B),must(source_location(F,L)),b_getval('$variable_names', Vs),
   pfc_file_module_term_expansion_1(How,M,A,Vs,F,L,B,BBBO).

:-module_transparent(pfc_file_module_term_expansion_1/7).
pfc_file_module_term_expansion_1(How,M,A,Vs,F,L,B,BBBO):-
   copy_term(A:B:Vs,AA:BB:VVs),pfc_implode_varnames(VVs),INFO=info(M:How,AA->BB,F:L,VVs),
   pfc_file_module_term_expansion_2(How,INFO,F,M,AA,B,BBBO),!.

:-module_transparent( pfc_file_module_term_expansion_2/6).
pfc_file_module_term_expansion_2(How,INFO,F,M,AA,BBB,BBBO):- 
   (BBB = (:- _) -> BBBO = BBB ;
      (How == code -> BBBO = BBB ;
        (add_from_file(BBB), BBBO = '$was_imported_kb_content$'(AA,INFO)))),!.      

xfile_load_form(How,M,P,O):- with_assertions(thlocal:already_in_file_term_expansion, ((pfc_file_expansion(P,C),P\=@=C,O=(:- user:(C))))),!.

mpred_hooks:provide_mpred_clauses(H,B,(What)):- !.

pfc_implode_varnames([]):-!.
pfc_implode_varnames([N=V|Vs]):-V='$VAR'(N),pfc_implode_varnames(Vs),!.



% must skip already loaded modules (we remember these so make/0 doesnt break)
pfc_maybe_skip(M):- thlocal:pfc_module_expansion(N),N==M,!.
pfc_maybe_skip(M):- asserta_if_new(pfc_skipped_module(M)),!.
% :- forall(current_module(M),pfc_maybe_skip(M)).


:- dynamic(user:mpred_directive_value/3).


expanded_already_functor('$was_imported_kb_content$').
expanded_already_functor(was_enabled).
expanded_already_functor(_:NV):-nonvar(NV),!,expanded_already_functor(NV).

% expanded_already_functor(F):-mpred_prop(F,code).


%:-thread_local is_compiling_clause/0.
%is_compiling:-is_compiling_clause;compiling.


:- multifile(user:term_expansion/2).
:- multifile(system:goal_expansion/2).
system:goal_expansion(A,_B):-fail,hotrace((source_module(M),(M=pfc_sanity;M=user;M=system),if_defined(pmsg(M:goal_expansion(A)),format(user_output /*e*/,'~N% ~q~n',M:goal_expansion(A))))),fail.
user:term_expansion(A,_B):-fail,hotrace((source_module(M),(M=pfc_sanity;M=user;M=system),if_defined(pmsg(M:term_expansion(A)),format(user_output /*e*/,'~N% ~q~n',M:term_expansion(A))))),fail.

system:goal_expansion(N,pfc_prove_neg(P)):-fail,pfc_from_negation_plus_holder(N,P),show_call_failure(mpred_prop(P,pfcControlled)).



:-thread_local(mpred_pfc_add_loaded).

pfc_directive_expansion(_,_):- thlocal:disable_mpred_term_expansions_locally,!,fail.

pfc_directive_expansion(pfc_ops, 
           ( op(500,fx,('~')),op(500,fx,('neg')),op(1075,xfx,('=>')), op(1075,xfx,('<=>')),op(1075,xfx,('<=')), op(1100,fx,('=>')), op(1150,xfx,('::::')))).
pfc_directive_expansion(pfc_dcg,( file_begin(pfc), op(400,yfx,('\\\\')),op(1200,xfx,('-->>')),op(1200,xfx,('--*>>')), op(1200,xfx,('<<--')))).
pfc_directive_expansion(pfc_multifile,
           ( asserta(user:mpred_directive_value(pfc,multifile,M)),
                 multifile(('<=')/2),
                 multifile(('<=>'/2)),
                 multifile((('=>')/2)),
                 multifile(('=>')/1),
                 multifile(('~')/1),
                 multifile(('neg')/1),
                 export(('<=')/2),
                 export(('<=>'/2)),
                 export((('=>')/2)),
                 export(('=>')/1),
                 export(('~')/1),
                 export(('neg')/1),
                 include(logicmoo(mpred/logicmoo_i_header)))):- source_module(M).


pfc_directive_expansion(pfc_module,(asserta(user:mpred_directive_value(pfc,module,M)))):-source_module(M).

begin_pfc:-file_begin(pfc).
pfc_begin:-file_begin(pfc).
pfc_end:-file_end(pfc).
dyn_begin:-file_begin(dynamic_reader).
dyn_end:-file_end(dynamic_reader).

file_begin(W):- must_det(( prolog_load_context(file,ISource),asserta(user:mpred_directive_value(W,file,ISource)))),must_det(( prolog_load_context(source,Source),asserta(user:mpred_directive_value(W,file,Source)))).
file_end(W):- must_det(( prolog_load_context(file,ISource),ignore(retract(user:mpred_directive_value(W,file,ISource))))),must_det(( prolog_load_context(source,Source),ignore(retract(user:mpred_directive_value(W,file,Source))))).
inside_file(W) :- prolog_load_context(file,Source),user:mpred_directive_value(W,_,Source),!.
inside_file(W) :- prolog_load_context(source,Source),user:mpred_directive_value(W,_,Source),!.




user:term_expansion((:- (M:DIR)),O):-atom(M),atom(DIR),with_source_module(M, ((pfc_directive_expansion(DIR,OO),!, must(O=(:- OO))))).
user:term_expansion((:- DIR),O):- atom(DIR), pfc_directive_expansion(DIR,OO),!,must(O=(:- OO)).


user:term_expansion(A,BO):- fail, hotrace((A\=(:-_),A\=end_of_file,current_predicate(pfc_file_loaded/0))), 
   hotrace((\+ thlocal:disable_mpred_term_expansions_locally, \+ thlocal:already_in_file_term_expansion,
    \+ (get_functor(A,F),expanded_already_functor(F)))),
   ((source_file(I),must(loading_module(M);source_module(M)))),
   with_no_assertions(thlocal:consulting_sources, 
   with_source_module(M, loop_check(pfc_file_module_term_expansion(I,M,A,B)))),
   must(nonvar(B)),BO=B.



pfc_file_expansion_0((P=>Q),(:- pfc_assert((P=>Q)))).
% maybe reverse some rules?
%pfc_file_expansion_0((P=>Q),(:- pfc_assert(('<='(Q,P))))).  % speed-up attempt
pfc_file_expansion_0(('<='(P,Q)),(:- pfc_assert(('<='(P,Q))))).
pfc_file_expansion_0((P<=>Q),(:- pfc_assert((P<=>Q)))).
pfc_file_expansion_0((RuleName :::: Rule),(:- pfc_assert((RuleName :::: Rule)))).
pfc_file_expansion_0((=>P),(:- pfc_assert((=>P)))).
pfc_file_expansion_0(Fact,Output):- pfc_file_expansion_1(Fact,C),must(pfc_file_expansion_0(C,Output)),!.
pfc_file_expansion_0((H:-Chain,B),(H:-(B))):- is_code_body(Chain),!,fail,must(atom(Chain)),make_dynamic(H).
pfc_file_expansion_0(Fact,(:- ((pfc_assert(Fact))))):- pfc_file_expansion_2(Fact,_Output),!.

pfc_file_expansion_2(Fact,Output):- get_functor(Fact,F,A),if_defined(ttPredType(F)),Output='$was_imported_kb_content$'(Fact,ttPredType(F)),!.
pfc_file_expansion_2(Fact,Output):- get_functor(Fact,F,A),if_defined(functorDeclares(F)),Output='$was_imported_kb_content$'(Fact,functorDeclares(F)),!.
pfc_file_expansion_2(Fact,Output):- get_functor(Fact,F,A),if_defined(prologMacroHead(F)),Output='$was_imported_kb_content$'(Fact,prologMacroHead(F)),!.
pfc_file_expansion_2(Fact,Output):- get_functor(Fact,F,A),if_defined(pfcControlled(F)),Output='$was_imported_kb_content$'(Fact,pfcControlled(F)),!.
pfc_file_expansion_2(Fact,Output):- notrace(pfc_expand_in_file_anyways(F)),!,Output='$was_imported_kb_content$'(Fact,pfc_expand_in_file_anyways(F)),!.

stream_pos(File:C):-source_file(File),current_input_stream(S),line_count(S,C).
compile_clause(CL):- make_dynamic(CL),must((assertz_if_new(CL),clause_asserted(CL))).

make_dynamic(C):- compound(C),get_functor(C,F,A),
  functor(P,F,A),
  ( \+predicate_property(P,_) -> dynamic(F/A) ; (predicate_property(P,dynamic)->true;dynamic_safe(P))),!,
  must((predicate_property(P,dynamic))).


pfc_file_expansion_1((H:-Chain,B),(H=>{B})):- is_action_body(Chain),must(atom(Chain)),make_dynamic(H).
pfc_file_expansion_1((H:-Chain,B),(B=>H)):- is_fc_body(Chain),must(atom(Chain)),make_dynamic(H).
pfc_file_expansion_1((H:-Chain,B),(H<=B)):- is_bc_body(Chain),must(atom(Chain)),make_dynamic(H).


pfc_expand_in_file_anyways(F):- pfc_using_file(F),must(loading_module(M);source_module(M)), (M=user; \+ pfc_skipped_module(M)),!.

was_exported_content(I,CALL,Output):-Output='$was_imported_kb_content$'(I,CALL),!.

:- thread_local(thlocal:pfc_term_expansion_ok/0).
:- thread_local(thlocal:pfc_already_in_file_expansion/1).


:- export(pfc_file_expansion/2).
%:- module_transparent(pfc_file_expansion/2).
pfc_file_expansion(I,OO):- (I\=(:-(_))), I\= '$was_imported_kb_content$'(_,_),
   once(loop_check(pfc_file_expansion_0(I,O))),
   I\=@=O, 
   ((thlocal:pfc_term_expansion_ok -> nop(wdmsg((pfc_file_expansion(I,O)))) ; (wdmsg(warning,wanted_pfc_term_expansion(I,O)),fail)),
   ((O=(:-(CALL))) -> 
      (current_predicate(_,CALL) -> ((must(CALL),was_exported_content(I,CALL,OO))); OO=O);
      (OO = O))).



is_file_clause(I):-compound(I),!.
is_file_clause(I):- dmsg(file_clause(I)).

:- multifile(user:term_expansion/2).
%:- module_transparent(user:term_expansion/2).
user:term_expansion(I,OO):- is_file_clause(I), \+ thlocal:disable_mpred_term_expansions_locally,
  sanity(\+  thlocal:pfc_already_in_file_expansion(I)), 
  pfc:with_assertions(thlocal:pfc_already_in_file_expansion(I),pfc_file_expansion(I,OO)),!,
  nop(dmsg(pfc_file_expansion(I,OO))).



:-assert_if_new(thlocal:pfc_term_expansion_ok).

:-set_prolog_flag(allow_variable_name_as_functor,true).

varFunctorEscape('@').
varFunctorEscape(':').
varFunctorEscape('\262\').
varFunctorEscape(A):-atom(A),atom_codes(A,[C]),C>255.

to_var_functors(Outer,In,Out):-  
 varFunctorEscape(VFE),
   \+ compound(In)->In=Out;
  (compound_name_arguments(In,Name,Args),
   (Args==[]->Out=Name;
      ((Name=VFE,Args=[JustOne] )-> (to_var_functors(VFE,JustOne,VOut),(functor(VOut,t,_)->Out=VOut;Out=..[VFE,VOut]));
      ( maplist(to_var_functors(Name),Args,ArgsO),
      ((Name\='[|]',Outer=VFE,atom_codes(Name,[C|_]),code_type(C,prolog_var_start),
         nb_getval('$variable_names', Vs),(member(Name=Var,Vs)->true;nb_setval('$variable_names', [Name=Var|Vs])))
           -> Out=..[t,Var|ArgsO];  (Args==ArgsO->(Out=In);compound_name_arguments(Out,Name,ArgsO))))))).
  

system:term_expansion(I,O):- compound(I),functor(I,VFE,1),varFunctorEscape(VFE),current_prolog_flag(allow_variable_name_as_functor,true),
                     \+ thlocal:disable_mpred_term_expansions_locally,
                       with_assertions(thlocal:disable_mpred_term_expansions_locally,to_var_functors((:-),I,O)),I\=@=O.

system:goal_expansion(I,O):- compound(I),functor(I,VFE,1),varFunctorEscape(VFE),current_prolog_flag(allow_variable_name_as_functor,true),
                     \+ thlocal:disable_mpred_term_expansions_locally,to_var_functors((:-),I,O),I\=@=O.


:-export(pfc_file_loaded/0).
pfc_file_loaded.


%user:goal_expansion(G,OUT):- \+  thlocal:disable_mpred_term_expansions_locally, G\=isa(_,_),(use_was_isa(G,I,C)),!,to_isa_out(I,C,OUT).
%user:term_expansion(G,OUT):- \+  thlocal:disable_mpred_term_expansions_locally, hotrace(use_was_isa(G,I,C)),!,to_isa_out(I,C,OUT).

%user:term_expansion(I,O):- \+ thlocal:disable_mpred_term_expansions_locally, thlocal:consulting_sources, with_no_assertions(thlocal:consulting_sources,add(I)),O=true.
user:goal_expansion(ISA,G) :- \+ thlocal:disable_mpred_term_expansions_locally, compound(ISA),thlocal:is_calling,use_was_isa(ISA,I,C),to_isa_out(I,C,OUT),G=no_repeats(OUT).



% :- source_location(S,_),forall(source_file(H,S),ignore(( \+predicate_property(M:H,built_in), functor(H,F,A),M:module_transparent(F/A),M:export(F/A)))).

