/** <module> Logicmoo Path Setups
*/
:-module(logicmoo_util_all,[if_flag_true/2,add_to_search_path/2,add_to_search_path_first/2,prolog_file_dir/2,if_startup_script/1,if_startup_script/0]).
:- set_prolog_flag(generate_debug_info, true).
:-export(prolog_file_dir/1).
prolog_file_dir(Here):- prolog_load_context(file, HereF),file_directory_name(HereF,Here).
prolog_file_dir(Here):- working_directory(Here,Here).
:-export(prolog_file_dir/2).
prolog_file_dir(Rel,ABSF):-prolog_file_dir(Here),absolute_file_name(Rel,ABSF,[relative_to(Here),file_type(directory),expand(true)]),!.
prolog_file_dir(Rel,ABSF):-prolog_file_dir(Here),absolute_file_name(Rel,ABSF,[relative_to(Here),expand(true)]),!.

add_to_search_path_first(Alias, Abs) :- asserta(user:file_search_path(Alias, Abs)).

add_to_search_path(Alias, Abs) :- is_absolute_file_name(Abs) ->
    assertz(user:file_search_path(Alias, Abs)) 
   ; (prolog_file_dir(Abs,ABSF),assertz(user:file_search_path(Alias, ABSF))).

:- add_to_search_path(logicmoo,'./../').


% Was this our startup file?
if_startup_script:- prolog_load_context(source, HereF),current_prolog_flag(associated_file,HereF),!.
if_startup_script:- prolog_load_context(source, HereF),file_base_name(HereF,HereFB),
   current_prolog_flag(os_argv,List),!,member(Arg,List),file_base_name(Arg,ArgFB),atom_concat(ArgFB,_,HereFB),!.

:- meta_predicate(if_startup_script(0)).
if_startup_script(Call):-if_startup_script->Call;true.

% this is a backwards compatablity block for SWI-Prolog 6.6.6
:- dynamic(double_quotes_was/1).
:- multifile(double_quotes_was/1).
:- current_prolog_flag(double_quotes,WAS),asserta(double_quotes_was(WAS)).
:- retract(double_quotes_was(WAS)),set_prolog_flag(double_quotes,WAS).
:- current_prolog_flag(double_quotes,WAS),asserta(double_quotes_was(WAS)).
:- set_prolog_flag(double_quotes,string).

:- meta_predicate if_flag_true(0,0).
:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

%logicmoo_util_all:if_flag_true(Flag,Goal):- catch(Flag,_,fail)->catch(Goal,E,throw(if_flag_true(E)));true.
logicmoo_util_all:if_flag_true(Flag,Goal):- catch(Flag,E,(dmsg(E:Flag),fail)) -> must(Goal); true.

join_path33(A,B,C):-exists_directory(B)->B=C;directory_file_path(A,B,C).

:-dynamic(logicmoo_runtime_dir/1).

% ======================================================
% Save a location of *this* file into logicmoo_runtime_dir/1
% And adds the local directories to file search path of logicmoo(..)
% ======================================================
:- source_location(File,_Line),
    file_directory_name(File, RunDir),
    retractall(logicmoo_runtime_dir(RunDir)),
    asserta(logicmoo_runtime_dir(RunDir)).


clip_dir_sep('/','/'):-!.
clip_dir_sep(Where,WhereF2):-atom_concat(WhereF2,'/',Where),!.
clip_dir_sep(Where,Where):-!.

:-export(normalize_path/2).
normalize_path(Where,WhereF3):-absolute_file_name(Where,WhereF),prolog_to_os_filename(WhereF,WhereF1),prolog_to_os_filename(WhereF2,WhereF1),!,clip_dir_sep(WhereF2,WhereF3).
normalize_path(Where,WhereF2):-clip_dir_sep(Where,WhereF2).

:-export(enumerate_files/2).

enumerate_files(Spec,Result):-no_repeats([Result],((enumerate_files0(Spec,NResult),normalize_path(NResult,Result)))),exists_file_or_dir(Result).

enumerate_files0(Spec,Result):- expand_file_name_safe(Spec,ResultList),ResultList\=[],member(NResult,ResultList),enumerate_files2(NResult,Result).
enumerate_files0(Spec,Result):- enumerate_files1(Spec,M),enumerate_files2(M,Result).

enumerate_files2(Spec,Result):-sub_atom(Spec,_,1,_,'*') -> enumerate_files1(Spec,Result);Spec=Result.

enumerate_files1(Spec,Result):- exists_file_or_dir(Spec),!,Result=Spec.
enumerate_files1(P/C,Result):- !,concat_paths(P,C,Result).
enumerate_files1(Spec,Result):- expand_file_name_safe(Spec,ResultList),member(Result,ResultList).
enumerate_files1(Spec,Result):- file_search_path(Spec,Result).
enumerate_files1(Spec,Result):- expand_file_search_path(Spec,Result).
enumerate_files1(Spec,Result):- absolute_file_name(Spec,Result).
enumerate_files1(Atom,Result):- atomic(Atom),not(is_absolute_file_name(Atom)),atomic_list_concat(List,'/',Atom),!,concat_paths(List,Result).
enumerate_files1(Atom,Result):- atomic(Atom),once((member(Sep,['/**/','/**','**']),atomic_list_concat([B,A|R],Sep,Atom))),concat_paths([B,'**',A|R],Result).

expand_file_name_safe(I,O):-catch(expand_file_name(I,O),_,fail),O\=[].
expand_file_name_safe(I,[O]):-catch(expand_file_search_path(I,O),_,fail).
expand_file_name_safe(I,L):- findall(O,absolute_file_name(I,O,[expand(true),solutions(all)]);
 absolute_file_name(I,O,[expand(true),solutions(all),file_type(directory)]),L).

:-export(exists_file_or_dir/1).
exists_file_or_dir(X):-atomic(X),once((catch(exists_file(X),E,(fmt(E:X),fail));is_directory(X))).
:-export(is_directory/1).
is_directory(X):-exists_directory(X).

:-export(concat_paths/3).
concat_paths(A,'',A).
concat_paths(A,'/',A).
concat_paths(ParentIn,'**',Result):-!, member(Child,['./','./*/','./*/*/','./*/*/*','./*/*/*/*','./*/*/*/*/*']),concat_paths(ParentIn,Child,Result).
concat_paths(ParentIn,Child,Result):- enumerate_files(ParentIn,Parent),
   once(is_directory(Parent) -> directory_file_path(Parent,Child,Joined) ; atom_concat(Parent,Child,Joined)),
   enumerate_files(Joined,Result).

:-export(concat_paths/2).
concat_paths([Joined],Result):- !,enumerate_files(Joined,Result).
concat_paths([ParentIn,Child|MORE],Result):- concat_paths(ParentIn,Child,ResultM),concat_paths([ResultM|MORE],Result).


/*
:- 
    source_location(File,_Line),
    file_directory_name(File, RunDir),
    atom_concat(RunDir,'/../library',RelDir),
    my_absolute_file_name(RelDir,A),
   'format'(' ~q. ~n',[user:file_search_path(library, A)]),
   asserta(user:file_search_path(library, A)).
*/

% Add the locations that the MUD source files will be picked up by the system
local_directory_search('../..').
%local_directory_search('~logicmoo-mud/cynd/startrek'). % home dir CynD world
% local_directory_search('.').
local_directory_search('..'). 
local_directory_search('../runtime'). 
local_directory_search('../src_game'). % for user overrides and uploads
local_directory_search('../src_assets').  % for non uploadables (downloadables)
local_directory_search('../src_mud').  % for vetted src of the MUD
local_directory_search('../src_modules'). % for big modules
local_directory_search('../src_webui').  % for web UI modules
local_directory_search('../src_lib'). % shared_library preds
local_directory_search('../externals/XperiMental/src_incoming').  % areeba underlay

:- current_prolog_flag(windows,true)->
   setenv('PATH_INDIGOLOG','../../indigolog');
   setenv('PATH_INDIGOLOG','../../indigolog').

my_absolute_file_name(F,A):-catch(expand_file_name(F,[A]),_,fail),F\=A,!.
my_absolute_file_name(F,A):-catch(absolute_file_name(F,A),_,fail),!.

% register search path hook
user:file_search_path(library,ATLIB):-getenv('PATH_INDIGOLOG',AT),atom_concat(AT,'/lib',ATLIB).
user:file_search_path(indigolog,AT):-getenv('PATH_INDIGOLOG',AT).
user:file_search_path(logicmoo,Dir):- 
   local_directory_search(Locally),
   locally_to_dir(Locally,Dir).

locally_to_dir(Locally,Dir):-logicmoo_runtime_dir(RunDir), join_path33(RunDir,Locally,Directory),my_absolute_file_name(Directory,Dir),exists_directory(Dir),!.
locally_to_dir(Directory,Dir):-my_absolute_file_name(Directory,Dir),exists_directory(Dir),!.

:- '@'( use_module(logicmoo(logicmoo_util/logicmoo_util_bugger_new)), 'user').
:- '@'( use_module(logicmoo(logicmoo_util/logicmoo_util_bugger_catch)), 'user').
:- '@'( use_module(logicmoo(logicmoo_util/logicmoo_util_bugger)), 'user').
:- '@'( use_module(logicmoo(logicmoo_util/logicmoo_util_library)), 'user').
:- '@'( use_module(logicmoo(logicmoo_util/logicmoo_util_ctx_frame)), 'user').
:- '@'( use_module(logicmoo(logicmoo_util/logicmoo_util_strings)), 'user').
:- '@'( use_module(logicmoo(logicmoo_util/logicmoo_util_terms)), 'user').
:- '@'( use_module(logicmoo(logicmoo_util/logicmoo_util_dcg)), 'user').
:- '@'( use_module(logicmoo(logicmoo_util/logicmoo_util_library)), 'user').
:- '@'( use_module(logicmoo(logicmoo_util/logicmoo_util_coroutining_was)), 'user').
:- '@'( use_module(logicmoo(logicmoo_util/logicmoo_util_coroutining_iz)), 'user').

/*
win_fork(G,SERVIO,PID):-atom_concat('swipl-win.exe ',G,AC),writeq(win_fork(AC,SERVIO)),nl,
      win_exec(AC,showdefault),PID = 0.

:- current_prolog_flag(windows,true)->
   asserta((fork(G):-win_fork(G)));
   use_module(library(unix)).

% fork(G):-writeq(fork(G)),nl.
*/


show_file_search_path:-'format'('% ~q.~n',[forall(user:file_search_path(_,_))]), 
  forall(user:file_search_path(A,B),'format'('% ~q.~n',[user:file_search_path(A,B)])).
% :-show_file_search_path.

% :- list_undefined.

:- logicmoo_util_dcg:do_dcg_util_tests.


% this is a backwards compatablity block for SWI-Prolog 6.6.6
:- retract(double_quotes_was(WAS)),set_prolog_flag(double_quotes,WAS).


