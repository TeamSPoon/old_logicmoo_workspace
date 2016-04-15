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
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl
:- if((prolog_load_context(file,F), prolog_load_context(source,F))).
:- module(mpred_prolog_file,[ ]).
:- include('mpred_header.pi').
:- else.
:- endif.












%% mpred_process_input_1( :TermT) is semidet.
%
% Managed Predicate process input  Secondary Helper.
%
mpred_process_input_1('?-'(TT)):-!,doall(printAll(TT)),!.
mpred_process_input_1('$si$':'$was_imported_kb_content$'(_,_)):-!.
mpred_process_input_1(':-'(TT)):-!,with_umt(TT).
mpred_process_input_1(T):-try_save_vars(T),trace,with_umt(ain(T)),!.



%% mpred_process_input( ?T, ?Vs) is semidet.
%
% Managed Predicate Process Input.
%
mpred_process_input(T,Vs):- must(expand_term(T,TT))->put_variable_names( Vs)->mpred_process_input_1(TT),!.




%% process_this_script is semidet.
%
% Process This Script.
%
process_this_script:- current_prolog_flag(xref,true),!.
process_this_script:- ignore(show_call(why,prolog_load_context(script,_))), prolog_load_context(stream,S), !, must(process_this_script(S)).




%% process_this_script( ?S) is semidet.
%
% Process This Script.
%
process_this_script(_):- current_prolog_flag(xref,true),!.
process_this_script(S):- at_end_of_stream(S),!.
process_this_script(S):- repeat,once(process_this_script0(S)),at_end_of_stream(S).




%% process_this_script0( ?S) is semidet.
%
% Process This Script Primary Helper.
%
process_this_script0(_):- current_prolog_flag(xref,true),!.
process_this_script0(_):- bad_idea,!.
process_this_script0(S):- at_end_of_stream(S),!.
process_this_script0(S):- peek_string(S,3,W), W="\n\n\n",get_code(S,_),get_code(S,_),!,process_this_script0(S).
process_this_script0(S):- peek_string(S,2,W), W="\r\n",get_code(S,_),!,process_this_script0(S).
process_this_script0(S):- peek_string(S,2,W), W="\n\n",get_code(S,_),!,process_this_script0(S).
process_this_script0(S):- peek_code(S,W),member(W,`\n`),get_code(S,P),put(P),!,process_this_script0(S).
process_this_script0(S):- peek_code(S,W),member(W,` \t\r`),get_code(S,_),!,process_this_script0(S).
process_this_script0(S):- peek_string(S,2,W),W="%=",!,read_line_to_string(S,String),format('~N~s~n',[String]).
process_this_script0(S):- peek_string(S,1,W),W="%",!,read_line_to_string(S,_String).
process_this_script0(S):- 
 with_umt((
  read_term(S,T,[variable_names(Vs)]),put_variable_names( Vs),
  format('~N~n',[]),portray_one_line(T),format('~N~n',[]),!,
  must(mpred_process_input(T,Vs)))).

















%% never_load_special( :TermARG1, ?Options) is semidet.
%
% Never Load Special.
%
never_load_special(_, Options) :-memberchk(must_be_module(true),Options).
never_load_special(_Module:Spec, _) :- atom(Spec), atomic_list_concat(M,'.',Spec),length(M,L),L>7.
never_load_special(_Module:library(Atom), Options) :- atom(Atom),member(must_be_module(true),Options),member(if(_),Options).
% [if(true),imports([mpred_database_term/2]),register(false),silent(false)]
never_load_special(_Module:_Spec, Options) :- member(must_be_module(true),Options),member(if(not_loaded),Options),member(imports([_/_]),Options).   


% :- use_module(library(logicmoo/util/logicmoo_util_filesystem)).
:- dynamic(prolog_load_file_loop_checked/2).

% probably an autoload (SKIP)



%% prolog_load_file_loop_checked( ?ModuleSpec, ?Options) is semidet.
%
% Prolog Load File Loop Checked.
%
prolog_load_file_loop_checked(ModuleSpec, Options) :- never_load_special(ModuleSpec, Options),!,fail.
prolog_load_file_loop_checked(ModuleSpec, Options) :- loop_check(show_success(prolog_load_file,prolog_load_file_loop_checked_0(ModuleSpec, Options))).




%% prolog_load_file_loop_checked_0( ?ModuleSpec, ?Options) is semidet.
%
% prolog load file loop checked  Primary Helper.
%
prolog_load_file_loop_checked_0(ModuleSpec, Options) :- current_predicate(_,_:exists_file_safe(_)),
   catch(prolog_load_file_nlc_pre(ModuleSpec, Options),E,(nop((trace,prolog_load_file_nlc(ModuleSpec, Options))),throw(E))).


prolog_load_file_nlc_pre(Module:Spec, Options) :- 
  call_with_module(Module,prolog_load_file_nlc(Module:Spec, Options)).

%% prolog_load_file_nlc( :TermModule, ?Options) is semidet.
%
% Prolog Load File Nlc.
%
prolog_load_file_nlc(Module:Spec, Options) :- 
   filematch(Module:Spec,Where1),Where1\=Spec,!,forall(filematch(Module:Spec,Where),Module:load_files(Module:Where,Options)).

prolog_load_file_nlc(Module:Spec, Options):- lmconf:never_reload_file(Spec),
   wdmsg(warn(error(skip_prolog_load_file_nlc(lmconf:never_reload_file(Module:Spec, Options))))),!.

prolog_load_file_nlc(Module:Spec, Options):- thread_self(TID), \+ is_main_thread,
   nop(wdmsg(warn(error(skip_prolog_load_file_nlc(wrong_thread(TID):-thread(Module:Spec, Options)))))),!.

prolog_load_file_nlc(Module:Spec, Options):- thread_self(TID), \+ is_main_thread,
   nop(wdmsg(warn(error(skip_prolog_load_file_nlc(wrong_thread(TID):-thread(Module:Spec, Options)))))),!,fail,dumpST.

prolog_load_file_nlc(Module:DirName, Options):-  atom(DirName), is_directory(DirName)->
  current_predicate(_,_:'load_file_dir'/2)->loop_check(show_call(why,call(load_file_dir,Module:DirName, Options))).

prolog_load_file_nlc(Module:Spec, Options):- absolute_file_name(Spec,AFN,[extensions(['pl'])]), 
   (Spec\==AFN),exists_file_safe(AFN),!,prolog_load_file_nlc(Module:AFN, Options).

prolog_load_file_nlc(Module:FileName, Options):- exists_file_safe(FileName),!,
   prolog_load_file_nlc_0(Module:FileName, Options).

prolog_load_file_nlc(Module:Spec, Options):- term_to_atom(Spec,String),member(S,['?','*']),sub_atom(String,_,1,_,S),!, 
 foreach(lmconf:filematch(Module:Spec,FileName),
    (loop_check((prolog_load_file_nlc_0(Module:FileName, Options))),TF=true)),!,
  nonvar(TF).




%% prolog_load_file_nlc_0( :TermModule, ?Options) is semidet.
%
% prolog load file nlc  Primary Helper.
%
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




%% guess_file_type_loader( ?Ext, ?Loader) is semidet.
%
% Guess File Type Loader.
%
guess_file_type_loader(Ext,Loader):-use_file_type_loader(Ext,Loader).
guess_file_type_loader(Ext,Pred):- atom(Ext),
   (Ext==''->Pred='load_file_some_type';system:atom_concat('load_file_type_,',Ext,Pred)),
   current_predicate(Pred/2).




%% load_file_dir( :TermModule, ?Options) is semidet.
%
% Load File Dir.
%
load_file_dir(Module:DirName, Options):- fail,
  directory_files(DirName,Files),
  foreach((member(F,Files),
            file_name_extension(_,Ext,F),
            guess_file_type_loader(Ext,Loader),
            current_predicate(_,_:Loader/2),
            directory_file_path(DirName,F,FileName)),
      (user:prolog_load_file(Module:FileName, Options),TF=true)),
     nonvar(TF).

:- source_location(S,_),forall(source_file(H,S),(functor(H,F,A),export(F/A),module_transparent(F/A))).





%% use_file_type_loader( ?VALUE1, ?VALUE2) is semidet.
%
% Use File Type Loader.
%
use_file_type_loader(pfc,ensure_mpred_file_consulted).
use_file_type_loader(pddl,ensure_mpred_file_consulted).
use_file_type_loader(plmoo,ensure_mpred_file_consulted).
% use_file_type_loader(pl,ensure_prolog_file_consulted).




%% ensure_prolog_file_consulted( :TermM, ?Options) is semidet.
%
% Ensure Prolog File Consulted.
%
ensure_prolog_file_consulted(M:File,Options):-must(load_files(M:File,Options)),!.




%% ensure_mpred_file_consulted( :TermM, ?Options) is semidet.
%
% Ensure Managed Predicate File Consulted.
%
ensure_mpred_file_consulted(M:File,Options):- 
 call_with_module(M,
  with_mpred_expansions(w_tl(t_l:pretend_loading_file(File),
              must((file_begin(pfc),
                    load_files(M:File,Options)))))),!.




%% load_file_some_type( :TermM, ?Options) is semidet.
%
% Load File Some Type.
%
load_file_some_type(M:File,Options):-call_with_module(M,must(load_files(M:File,Options))),!.



:- multifile(user:prolog_load_file/2).
:- dynamic(user:prolog_load_file/2).


%% prolog_load_file( ?ModuleSpec, ?Options) is semidet.
%
% Hook To [user:prolog_load_file/2] For Module Mpred_loader.
% Prolog Load File.
%


user:prolog_load_file(ModuleSpec, Options):- current_predicate(_,_:mpred_loader_file),
  \+ never_load_special(ModuleSpec, Options),  catch(prolog_load_file_loop_checked(ModuleSpec, Options),E,
    ((wdmsg(E),trace,prolog_load_file_loop_checked(ModuleSpec, Options),throw(E)))).



