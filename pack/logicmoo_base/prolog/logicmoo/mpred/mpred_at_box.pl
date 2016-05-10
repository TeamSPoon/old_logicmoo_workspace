/* 
% ===================================================================
% File 'mpred_db_preds.pl'
% Purpose: Emulation of OpenCyc for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface.pl' 1.0.0
% Revision:  $Revision: 1.9 $
% Revised At:   $Date: 2002/06/27 14:13:20 $
% ===================================================================
% File used as storage place for all predicates which change as
% the world is run.
%
%
% Dec 13, 2035
% Douglas Miles
*/
% :- if((\+ current_prolog_flag(common_logic_at_box_true,true),set_prolog_flag(common_logic_at_box_true,true))).
%:- if(((current_prolog_flag(xref,true),current_prolog_flag(pldoc_x,true));current_prolog_flag(autoload_logicmoo,true))).
:- module(mpred_at_box,[
         assert_setting01/1,
         make_module_name_local/2,
         make_module_name_local0/2,
         (make_shared_multifile)/3,
         (make_shared_multifile)/4,
         (shared_multifile)/1,
         add_import_predicate/3,
         autoload_library_index/4,
         baseKB_hybrid_support/2,
         uses_predicate/2,
         uses_predicate/4,
         correct_module/3,
         correct_module/5,
         defaultAssertMt/1,
         abox:defaultTBoxMt/1,
         ensure_imports/1,
         fileAssertMt/1,
         in_mpred_kb_module/0,
         istAbove/2,
         make_as_dynamic/4,
         makeConstant/1,
         mtCanAssertMt/1,
         registerCycPred/4,
         registerCycPred/5,
         retry_undefined/3,
         set_defaultAssertMt/1,
         set_fileAssertMt/1,
         transitive_path/3,
         which_file/1,
         user_m_check/1,

         add_abox_module/1,

         ensure_tbox/1,


         fixup_modules/0,
         import_predicate/2,
         skip_user/1,
         inherit_into_module/2,
         box_type/3,
         make_reachable/2,
         fixup_module/2,
         is_undefaulted/1,
         ensure_imports_tbox/2,
         map_inheritance/1,


         which_file/1
    ]).
%:- endif.
:- set_prolog_flag(retry_undefined,false).

:- include('mpred_header.pi').

user_m_check(_Out).

:- meta_predicate make_shared_multifile(+,+,+).
:- meta_predicate make_shared_multifile(*,*,*,*).


:- meta_predicate shared_multifile(:).

:- meta_predicate transitive_path(2,*,*).

:- use_module(mpred_pfc).

add_abox_module(ABox):- baseKB:mtLocal(ABox),!.
add_abox_module(baseKB):-!.
add_abox_module(ABox):- 
  must(mtCanAssertMt(ABox)),
  asserta(baseKB:mtLocal(ABox)),
  forall(current_module(M),
    (((import_module(M,abox),(M\==ABox))->add_import_module(M,ABox,start);true),
     ((import_module(abox,M),(M\==ABox)->add_import_module(ABox,M,start);true)))),  
    ((M==abox)->true;(ignore(system:delete_import_module(ABox,abox)),add_import_module(abox,ABox,start))).

:- dynamic(baseKB:mtSharedPrologCodeOnly/1).
:- dynamic(baseKB:mtNoPrologCode/1).
baseKB:mtNoPrologCode(mpred_userkb).

baseKB:mtSharedPrologCodeOnly(Mt):- module_property(Mt,class(library)).
baseKB:mtSharedPrologCodeOnly(Mt):-
      arg(_,v( 
         common_logic_boxlog,
         common_logic_compiler,
         common_logic_kb_hooks,
         common_logic_sexpr,
         common_logic_skolem,
         common_logic_snark,
         lmconf,
         lmcache,
         t_l,
         lmcode,
         logicmoo_base_file,
         logicmoo_util_attvar_reader,
         logicmoo_util_bugger,
         logicmoo_util_catch,
         logicmoo_util_ctx_frame,
         logicmoo_util_database,
         logicmoo_util_dmsg,
         logicmoo_util_dra,
         logicmoo_util_dumpst,
         logicmoo_util_engines,
         logicmoo_util_filestreams,
         logicmoo_util_first,
         logicmoo_util_help,
         logicmoo_util_loop_check,
         logicmoo_util_no_repeats,
         logicmoo_util_preddefs,
         logicmoo_util_prolog_frames,
         logicmoo_util_prolog_streams,
         logicmoo_util_rtrace,
         logicmoo_util_shared_dynamic,
         logicmoo_util_strings,
         logicmoo_util_term_listing,
         logicmoo_util_terms,
         logicmoo_util_varnames,
         logicmoo_util_with_assertions,
         logicmoo_utils_file,
         lmcode,
         mpred_agenda,
         mpred_at_box,
         mpred_expansion,
         mpred_hooks,
         mpred_kb_ops,
         mpred_listing,
         mpred_loader,
         mpred_pfc,
         mpred_props,
         mpred_storage,
         mpred_stubs,
         mpred_stubs_file_module,
         mpred_type_args,
         mpred_type_constraints,
         mpred_type_isa,
         mpred_type_naming,
         mpred_type_wff,
         prolog_statistics,
         system,
         user
       ),Mt).

:- multifile(lmcache:has_pfc_database_preds/1).
:- dynamic(lmcache:has_pfc_database_preds/1).


%% assert_setting01( ?X) is semidet.
assert_setting01(M:P):-functor(P,_,A),duplicate_term(P,DP),setarg(A,DP,_),system:retractall(M:DP),system:asserta(M:P).
            

%% which_file( ?F) is semidet.
%
% Which File.
%
which_file(F):- prolog_load_context(source,F) -> true; once(loading_source_file(F)).

:- module_transparent
 
         assert_setting01/1,
         make_module_name_local/2,
         make_module_name_local0/2,
         
         defaultAssertMt/1,
         set_defaultAssertMt/1,
         
         which_file/1,
         fileAssertMt/1,
         set_fileAssertMt/1,
                          
         correct_module/3,
         correct_module/5,
         ensure_imports/1,
         in_mpred_kb_module/0,
         which_file/1,
         user_m_check/1 .


%% in_mpred_kb_module is semidet.
%
% In Managed Predicate Knowledge Base Module.
%
in_mpred_kb_module:- source_context_module(MT),defaultAssertMt(MT2),!,MT==MT2.


map_inheritance(Child):-forall(import_module(Child,Parent),inherit_into_module(Child,Parent)).


%% box_type( ?F, ?A, ?VALUE3) is semidet.
%
% Datalog Type.
%
box_type(F,A,tbox):-current_predicate(baseKB:F/A).
box_type(_,_,abox).




:- thread_local(t_l:current_defaultAssertMt/1).
:- dynamic(lmconf:file_to_module/2).

:- multifile(abox:defaultTBoxMt/1).
:- dynamic(abox:defaultTBoxMt/1).
abox:defaultTBoxMt(baseKB).


%% defaultAssertMt(-Ctx) is det.
%
% ABox is an "assertion component" Prolog Module
% within a knowledge base.
%
% not just user modules
defaultAssertMt(ABox):- nonvar(ABox),defaultAssertMt(ABoxVar),!,ABox=@=ABoxVar.
defaultAssertMt(ABox):- 
    (t_l:current_defaultAssertMt(ABox);
    ((('$current_source_module'(ABox);
    '$current_typein_module'(ABox);
     abox:defaultTBoxMt(ABox))), 
           mtCanAssertMt(ABox))),!.

defaultAssertMt(ABox):- fileAssertMt(ABox).

%% fileAssertMt(-ABox) is det.
%
% Gets ABox is an "assertion component" Prolog Module
% within a knowledge base.
%
% not just user modules
fileAssertMt(ABox):- nonvar(ABox), fileAssertMt(ABoxVar),!,ABox=@=ABoxVar.
fileAssertMt(ABox):- which_file(File)->current_module(ABox),module_property(ABox,file(File)),File\==ABox,!.
fileAssertMt(ABox):-
 (t_l:current_defaultAssertMt(ABox);
    ((('$current_source_module'(ABox))),ABox\==user)),!.
fileAssertMt(ABox):- which_file(File)->make_module_name_local(File,ABox),File\==ABox,!.
fileAssertMt(ABox):-
 (((('$current_typein_module'(ABox);
     abox:defaultTBoxMt(ABox))),mtCanAssertMt(ABox))),!.
fileAssertMt(baseKB).

mtCanAssertMt(ABox):- \+ baseKB:mtSharedPrologCodeOnly(ABox).



% baseKB:mtGlobal
% mtCore



makeConstant(_Mt).


%:- (system:trace, rtrace, trace,cls ).
%:- (break,notrace,nortrace).



%% set_defaultAssertMt( ?ABox) is semidet.
%
% Sets Current Module.
%
set_defaultAssertMt(abox):-!.
set_defaultAssertMt(M):- baseKB:mtSharedPrologCodeOnly(M),!,setup_module_ops(M).
set_defaultAssertMt(ABox):- defaultAssertMt(QABox)->QABox==ABox,!.
set_defaultAssertMt(ABox):- 
  must_det_l((
    must(mtCanAssertMt(ABox)),
    must(abox:defaultTBoxMt(TBox)),
    asserta_if_new(ABox:defaultTBoxMt(TBox)),
    (TBox==ABox->true;assert_setting(t_l:current_defaultAssertMt(ABox))),
    '$set_source_module'(ABox),'$set_typein_module'(ABox),                        
    setup_module_ops(ABox), 
    inherit_into_module(ABox,TBox))).

%% set_fileAssertMt( ABox) is semidet.
%
% Sets the File''s Module.
%
set_fileAssertMt(ABox):- 
 must_det_l((
   must(mtCanAssertMt(ABox)),
   abox:defaultTBoxMt(TBox),
   TBox:ensure_abox(ABox),
   '$current_typein_module'(CM),
   '$current_source_module'(SM),
   defaultAssertMt(Was),

   set_defaultAssertMt(ABox),
   which_file(File),assert_setting(lmconf:file_to_module(File,ABox)),
   assert_until_eof(lmcache:mpred_directive_value(File,module,ABox)),

   onEndOfFile(set_defaultAssertMt(Was)),
   onEndOfFile('$set_source_module'(SM)),
   onEndOfFile('$set_typein_module'(CM)))).



make_module_name_local(A,B):- make_module_name_local0(A,B), \+ exists_file(B).

make_module_name_local0(Source,KB):- baseKB:mtGlobal(Source),defaultAssertMt(KB).
make_module_name_local0(Source,SetName):- lmconf:file_to_module(Source,SetName),!.
make_module_name_local0(Source,Source):- lmcache:has_pfc_database_preds(Source).
make_module_name_local0(Source,GetName):- make_module_name(Source,GetName).


ensure_tbox(_ABox).


%% mtCore( ?VALUE1) is semidet.
%
% If Is A Datalog System Core Microtheory.
%
:- dynamic(baseKB:mtCore/1).
baseKB:mtCore(baseKB).


%% baseKB:mtGlobal(M,Box).
%
% Boot Modules.
%
%baseKB:mtGlobal(mpred_loader).

:- dynamic(baseKB:mtGlobal/1).
baseKB:mtGlobal(baseKB).
baseKB:mtGlobal(boot_system).
baseKB:mtGlobal(system_markers).
baseKB:mtGlobal(system_singleValued).
baseKB:mtGlobal(system_genls).
baseKB:mtGlobal(system_if_missing).
baseKB:mtGlobal(common_logic_clif).
baseKB:mtGlobal(system_mdefault).


is_undefaulted(user).




%% ensure_imports( ?M) is semidet.
%
% Ensure Imports.
%
ensure_imports(baseKB):-!.
ensure_imports(M):-ensure_imports_tbox(M,baseKB).

:-multifile(lmcache:is_ensured_imports_tbox/2).
:-dynamic(lmcache:is_ensured_imports_tbox/2).


%% skip_user( ?M) is semidet.
%
% Skip over 'user' module and still see 'system'.
%
skip_user(Mt):- import_module(Mt,system), \+ default_module(Mt,user), !.
skip_user(Mt):- !, add_import_module(Mt,system,start),ignore(delete_import_module(Mt,user)),
  forall((import_module(Mt,X),default_module(X,user)),skip_user(X)).
  
inherit_into_module(Child,Parent):- ==(Child,Parent),!.
inherit_into_module(Child,Parent):-ain(baseKB:genlMt(Child,Parent)).

%% ensure_imports_tbox( ?M, ?TBox) is semidet.
%
% Ensure Imports Tbox.
%
ensure_imports_tbox(M,TBox):-M==TBox,!.
ensure_imports_tbox(M,TBox):-
  lmcache:is_ensured_imports_tbox(M,TBox),!.
ensure_imports_tbox(M,TBox):-
  asserta(lmcache:is_ensured_imports_tbox(M,TBox)),
  
  must_det((
   %forall((system:current_module(IM), \+ lmconf:is_box_module(IM,_)),inherit_into_module(M,IM)),
   %forall((system:current_module(IM), \+ lmconf:is_box_module(IM,_)),inherit_into_module(TBox,IM)),
   skip_user(TBox),
   ignore(maybe_delete_import_module(M,TBox)),
   ignore(maybe_delete_import_module(TBox,M)),
   forall((user:current_predicate(_,TBox:P),
      \+ predicate_property(TBox:P,imported_from(_))),
      add_import_predicate(M,P,TBox)),
   inherit_into_module(M,user),
   skip_user(M),
   ignore(maybe_delete_import_module(M,user)),
   inherit_into_module(user,M),
   ignore(maybe_delete_import_module(user,system)), % gets from M now
   !)).



% :- inherit_into_module(logicmoo_user,lmcode).

fixup_module(system,_).
fixup_module(M,_L):-baseKB:mtGlobal(M),skip_user(M).
fixup_module(lmcode,_L):-skip_user(lmcode).
fixup_module(_,[user]).
fixup_module(M,_L):- skip_user(M).


fixup_modules:- 
   doall((current_module(M),once((findall(I,import_module(M,I),L))),once(fixup_module(M,L)))).

% :- autoload([verbose(false)]).
:- set_prolog_flag(retry_undefined,true).

% :- fixup_modules.


% :- endif.

















% ============================================

%% correct_module( ?M, ?X, ?T) is semidet.
%
% Correct Module.
%
correct_module(M,G,T):-functor(G,F,A),quietly_must(correct_module(M,G,F,A,T)),!.

%% correct_module( ?M, ?Goal, ?F, ?A, ?T) is semidet.
%
% Correct Module.
%
correct_module(abox,G,F,A,T):- !, defaultAssertMt(M),correct_module(M,G,F,A,T).
correct_module(tbox,G,F,A,T):- !, abox:defaultTBoxMt(M),correct_module(M,G,F,A,T).
correct_module(user,G,F,A,T):- fail,!,defaultAssertMt(M),correct_module(M,G,F,A,T).
correct_module(HintMt,Goal,_,_,OtherMt):- predicate_property_nt(HintMt:Goal,imported_from(OtherMt)).
correct_module(_,Goal,_,_,OtherMt):- predicate_property_nt(Goal,imported_from(OtherMt)).
correct_module(HintMt,_,_,_,HintMt):- mtExact(HintMt).
correct_module(HintMt,Goal,_,_,HintMt):- predicate_property_nt(HintMt:Goal,exported).
correct_module(_,Goal,_,_,OtherMt):- var(OtherMt),!, predicate_property_nt(OtherMt:Goal,file(_)).
correct_module(_,Goal,_,_,OtherMt):- baseKB:mtGlobal(OtherMt), predicate_property_nt(OtherMt:Goal,file(_)).
correct_module(MT,_,_,_,MT):-!.



:- dynamic(lmcache:how_registered_pred/4).
:- module_transparent(lmcache:how_registered_pred/4).

add_import_predicate(Mt,Goal,OtherMt):- fail,
   baseKB:mtGlobal(Mt),
   baseKB:mtGlobal(OtherMt),
   \+ import_module(OtherMt,Mt),
   catch(add_import_module(Mt,OtherMt,end),
       error(permission_error(add_import,module,baseKB),
       context(system:add_import_module/3,'would create a cycle')),fail),
   must(predicate_property_nt(Mt:Goal,imported_from(OtherMt))),!.

add_import_predicate(Mt,Goal,OtherMt):- catch(Mt:import(OtherMt:Goal),_,fail),!.
add_import_predicate(Mt,Goal,OtherMt):- 
   functor(Goal,F,A),
   make_as_dynamic(imported_from(OtherMt),Mt,F,A),
   assert_if_new(( Mt:Goal :- OtherMt:Goal)).
  
make_as_dynamic(Reason,Mt,F,A):- dynamic( Mt:F/A),
   functor(Goal,F,A),
   assert_if_new(( Mt:Goal :- (fail,infoF(Reason)))).



registerCycPred(Mt,_,F,A):-
   lmcache:how_registered_pred(_,Mt,F,A),!.

registerCycPred(Mt,Goal,F,A):-
   correct_module(Mt,Goal,OtherMt), 
   sanity(Mt \== OtherMt),
   must(Mt \== OtherMt),
   ain(tMicrotheory(Mt)),
   registerCycPred(Mt,Goal,F,A,OtherMt),!.

registerCycPred(Mt,Goal,F,A):-
   dynamic(Mt:F/A), % (1 is random(180)->dump_break;true),
   assert_if_new(( Mt:Goal :- istAbove(Mt,Goal))).


transitive_path(F,[Arg1,Arg2],Arg2):-
  dif(Arg1,Arg2),call(F,Arg1,Arg2),!.
transitive_path(F,[Arg1,SecondNodeMt|REST],Arg2):-
  dif(Arg1,Arg2),dif(Arg1,SecondNodeMt),
  call(F,Arg1,SecondNodeMt),stack_check,
  transitive_path(F,[SecondNodeMt|REST],Arg2).

registerCycPred(Mt,Goal,_Pred,_Arity,OtherMt):- 
  baseKB:mtGlobal(OtherMt),
  add_import_predicate(Mt,Goal,OtherMt),!.

  /*
registerCycPred(Mt,Goal,F,A,OtherMt):- fail,
   transitive_path(genlMt,[Mt,SecondNodeMt|_],OtherMt),
   make_as_dynamic(genlMt(Mt,OtherMt),Mt,F,A),
   assert_if_new(( Mt:Goal :- SecondNodeMt:call(Goal))),!.

registerCycPred(Mt,_Goal,F,A,OtherMt):-
  dump_break,
  make_as_dynamic(need_genlMt(Mt,OtherMt),Mt,F,A),!.
   */


autoload_library_index(F,A,PredMt,File):- functor(P,F,A),'$autoload':library_index(P,PredMt,File).


:- multifile(baseKB:hybrid_support/2).
:- dynamic(baseKB:hybrid_support/2).
baseKB_hybrid_support(F,A):-wsh_w:wrap_shared(F,A,_).
baseKB_hybrid_support(F,A):-baseKB:hybrid_support(F,A).
baseKB:hybrid_support(arity,2).
baseKB:hybrid_support(predicateConventionMt,2).
baseKB:hybrid_support(functorDeclares,1).
baseKB:hybrid_support(spft,3).
baseKB:hybrid_support(mtLocal,1).
baseKB:hybrid_support(genlMt,2).


istAbove(Mt,Query):- Mt \== baseKB, Mt \== lmcode, genlMt(Mt,MtAbove),MtAbove:Query.



uses_predicate(M:F/A,R):- uses_predicate(M,F,A,R).
uses_predicate(F/A,R):- uses_predicate(user,F,A,R).


uses_predicate(Module,Name,Arity,Action) :- 
      current_prolog_flag(autoload, true),
	'$autoload'(Module, Name, Arity), !,
	Action = retry.
uses_predicate(CallerMt,'$pldoc',4,retry):- multifile(CallerMt:'$pldoc'/4),discontiguous(CallerMt:'$pldoc'/4),dynamic(CallerMt:'$pldoc'/4),!.
uses_predicate(M,F,A,error):- lmcache:tried_to_retry_undefined(_SM:M,F,A),!.
uses_predicate(CallerMt,F,A,_):-
   '$current_source_module'(SM),
   wdmsg(uses_predicate(SM,CallerMt,F,A)),
   assert(lmcache:tried_to_retry_undefined(SM:CallerMt,F,A)),fail.
uses_predicate(_, (/), _, error) :- !,dumpST,break.
uses_predicate(_, (//), _, error) :- !,dumpST,break.
uses_predicate(_, (:), _, error) :- !,dumpST,break.
% uses_predicate(_, '>>',  4, error) :- !,dumpST,break.
uses_predicate(_, '[|]', _, error) :- !,dumpST,break.
uses_predicate(Module, Name, Arity, Action) :- fail,
	current_prolog_flag(autoload, true),
	'$autoload'(Module, Name, Arity), !,
	Action = retry.

uses_predicate(CallerMt,F,A,retry):- 
    loop_check(retry_undefined(CallerMt,F,A),dump_break).


:- dynamic(lmcache:tried_to_retry_undefined/3).


% Module defines the type
% retry_undefined(M:F/A):- lmcache:tried_to_retry_undefined(M,F,A),!.
% retry_undefined(M:F/A):- assert(lmcache:tried_to_retry_undefined(M,F,A)),fail.


% Every module has it''s own
retry_undefined(CallerMt,'$pldoc',4):- multifile(CallerMt:'$pldoc'/4),discontiguous(CallerMt:'$pldoc'/4),dynamic(CallerMt:'$pldoc'/4),!.

retry_undefined(lmconf,F,A):-multifile(lmconf:F/A),dynamic(lmconf:F/A),!.
retry_undefined(lmcache,F,A):-multifile(lmcache:F/A),volatile(lmcache:F/A),dynamic(lmcache:F/A),!.
retry_undefined(t_l,F,A):-multifile(t_l:F/A),thread_local(t_l:F/A),!.

retry_undefined(baseKB, F, A):-  baseKB_hybrid_support(F,A),
   dynamic(baseKB:F/A),
   multifile(baseKB:F/A),
   dynamic(baseKB:F/A),
   discontiguous(baseKB:F/A),!.

retry_undefined(CallerMt,F,A):- baseKB_hybrid_support(F,A),find_and_call(mtLocalized(CallerMt)),!,
   functor(Goal,F,A),assert_if_new(( CallerMt:Goal :- istAbove(CallerMt,Goal))).

retry_undefined(CallerMt,F,A):- current_predicate(lmcode:F/A), current_module(M),M\=lmcode,
  current_predicate(M:F/A),functor(P,F,A),predicate_property(M:P,defined),\+predicate_property(M:P,imported_from(_)),
  CallerMt:import(M:F/A).

retry_undefined(CallerMt,F,A):- 
    autoload_library_index(F,A,_PredMt,File),
    use_module(CallerMt:File),!.
   
% Autoloads importing the entire other module
retry_undefined(CallerMt,F,A):-
       autoload_library_index(F,A,PredMt,File),
       asserta(lmcache:how_registered_pred(PredMt:use_module(CallerMt:File),CallerMt,F,A)),
       use_module(lmcode:File),!.
       % system:add_import_module(CallerMt,lmcode,start).



% System-like Autoloads
retry_undefined(CallerMt,debug,1):- use_module(CallerMt:library(debug)),!.
retry_undefined(CallerMt,debugging,1):- use_module(CallerMt:library(debug)),!.
retry_undefined(CallerMt,member,2):- use_module(CallerMt:library(lists)),!.
retry_undefined(CallerMt,directory_file_path,3):- use_module(CallerMt:library(filesex)),!.


retry_undefined(CallerMt,F,A):- fail,
       autoload_library_index(F,A,_,File),
       load_files(CallerMt:File,[if(true),imports([F/A]),register(false),silent(false)]),!.

% Autoloads importing the entire other module
retry_undefined(CallerMt,F,A):-
       autoload_library_index(F,A,PredMt,File),
       asserta(lmcache:how_registered_pred(PredMt:use_module(CallerMt:File),CallerMt,F,A)),
       use_module(CallerMt:File),!.

/*
retry_undefined(CallerMt,F,A):-
      autoload_library_index(F,A,PredMt,File),
      ((current_module(PredMt),current_predicate(PredMt:F/A))
       -> add_import_module(CallerMt,PredMt,start) ;
       (PredMt:ensure_loaded(PredMt:File),add_import_module(CallerMt,PredMt,start))),!.
*/

%retry_undefined(PredMt:must/1) :- add_import_module(PredMt,logicmoo_util_catch,start),!.
%retry_undefined(PredMt:debugm/2) :- add_import_module(PredMt,logicmoo_util_dmsg,start),!.


:-module_transparent(make_shared_multifile/3).
:- export((make_shared_multifile)/3).




:- module_transparent((shared_multifile)/1).
:- meta_predicate((shared_multifile(+))).

%= 	 	 

%% shared_multifile( +PI) is semidet.
%
% Shared Multifile.
%
shared_multifile(PredMt:MPI):-
   context_module_of_file(CallerMt),!,
   with_pfa_group(make_shared_multifile,CallerMt,PredMt, MPI),!.

shared_multifile(PI):- kb_dynamic(PI).


%% make_shared_multifile( ?CallerMt, ?PredMt, :TermPI) is semidet.
%
% Make Shared Multifile.
%
make_shared_multifile(CallerMt,   M,FA):- get_fa(FA,F,A), make_shared_multifile(CallerMt, PredMt,F,A),!.

make_shared_multifile(CallerMt,    t_l,F,A):-!,CallerMt:thread_local(t_l:F/A),CallerMt:multifile(t_l:F/A).
make_shared_multifile(CallerMt,lmconf ,F,A):-!,CallerMt:multifile(lmconf:F/A),CallerMt:dynamic(lmconf:F/A),!.
make_shared_multifile(CallerMt,lmcache,F,A):-!,CallerMt:multifile(lmcache:F/A),CallerMt:volatile(lmcache:F/A),CallerMt:dynamic(lmcache:F/A),!.

make_shared_multifile(CallerMt,PredMt,F,A):- 
  functor(Goal,F,A),
  correct_module(PredMt,Goal,F,A,HomeM),
  HomeM\==PredMt,!,
  make_shared_multifile(CallerMt,HomeM,F,A).

make_shared_multifile(CallerMt,Home,F,A):- baseKB:mtSharedPrologCodeOnly(Home),!,
     wdmsg(mtSharedPrologCodeOnly_make_shared_multifile(CallerMt,Home:F/A)),!.

make_shared_multifile(_CallerMt, baseKB,F,A):-  kb_dynamic(F,A),!.

make_shared_multifile(_CallerMt,PredMt,F,A):-!,
 debug(make_shared_multifile,'~p',make_shared_multifile(PredMt:F/A)),
 w_tl(set_prolog_flag(access_level,system),
  PredMt:( 
   sanity( \+ ((PredMt:F/A) = (qrTBox:p/1))),
   PredMt:check_never_assert(declared(PredMt:F/A)),
   decl_shared(PredMt:F/A))).




%% make_reachable( ?UPARAM1, ?Test) is semidet.
%
% Make Reachable.
%
make_reachable(_,Test):- \+ \+ ((Test= (_:F/_), is_ftVar(F))),!.
make_reachable(CM,M:F/A):-  atom(CM),ignore(CM=M),quietly_must(atom(CM)),quietly_must(atom(M)), 
   functor(G,F,A),
   correct_module(M,G,F,A,TT), !,import_predicate(CM,TT:F/A).



%% import_predicate( ?CM, :TermM) is semidet.
%
% Import Predicate.
%
import_predicate(CM,M:_):- CM==M,!.
import_predicate(CM,M:_):- default_module(CM,M),!.
import_predicate(CM,M:F/A):- show_call(nop(CM:z333import(M:F/A))),CM:multifile(M:F/A),
  icatch(CM:discontiguous(M:F/A)).

