/** <module> Logicmoo Path Setups
*/
:-module(logicmoo_util_all,[if_flag_true/2,add_to_search_path/2,add_to_search_path_first/2,prolog_file_dir/2,if_startup_script/1,if_startup_script/0]).
:- set_prolog_flag(generate_debug_info, true).
:- export(normalize_path/2).

:- dynamic(double_quotes_was/1).
:- multifile(double_quotes_was/1).
:- current_prolog_flag(double_quotes,WAS),asserta(double_quotes_was(WAS)).
:- retract(double_quotes_was(WAS)),set_prolog_flag(double_quotes,WAS).
:- current_prolog_flag(double_quotes,WAS),asserta(double_quotes_was(WAS)).
:- set_prolog_flag(double_quotes,string).

% have to load this module here so we dont take ownership of prolog_exception_hook/4.
:- set_prolog_flag(access_level,system).
:- set_prolog_flag(verbose_autoload, true).
:- set_prolog_flag(generate_debug_info, true).
% have to load this module here so we dont take ownership of prolog_exception_hook/4.
:- user:ensure_loaded(library(ansi_term)).
:- user:ensure_loaded(library(check)).
:- user:ensure_loaded(library(debug)).
:- user:ensure_loaded(library(listing)).
:- user:ensure_loaded(library(lists)).
:- user:ensure_loaded(library(make)).
:- user:ensure_loaded(library(prolog_stack)).
:- user:ensure_loaded(library(system)).

:- export(with_stream_pos/2).
:- meta_predicate(with_stream_pos(+,0)).
with_stream_pos(In,Call):-
    stream_property(In, position(InitalPos)),
    PS = position(InitalPos),
    (Call *-> 
       (stream_property(In, position(NewPos)),nb_setarg(1,PS,NewPos)) ; 
       ((arg(1,PS,Pos),set_stream_position(In, Pos)),!,fail)).


:-export(l_open_input/2).
:-export(l_open_input0/2).
:-export(l_open_input1/2).
l_open_input(InS,In):-once(must(l_open_input0(InS,In))).

l_open_input0(In,InS):-l_open_input1(In,InS),!.
l_open_input0(InS,In):-string(InS),!,open_string(InS,In).
l_open_input0(Filename,In) :- \+ is_list(Filename),nonvar(Filename),filematch(Filename,File), catch(see(File),_,fail),current_input(In).
l_open_input0(InS,In):-!,open_string(InS,In).

l_open_input1([V|_],_):-var(V),V=zzzzzzzzzzzzz,!,throw(error(l_open_input/2,'Arguments are not sufficiently instantiated (l_open_input)')).
l_open_input1(InS,In):-is_stream(InS),!,In=InS.
l_open_input1(file(Filename),In) :- filematch(Filename,File), catch(see(File),_,fail),current_input(In).
l_open_input1(alias(Filename),In) :-  catch(see(Filename),_,fail),current_input(In).
l_open_input1(string(string(InS)),In):-!,text_to_string_safe(InS,Str),string_codes(Str,Codes),open_chars_stream(Codes,In).
l_open_input1(string(InS),In):-!,open_string(InS,In).
l_open_input1(atom(InS),In):-!,open_string(InS,In).
l_open_input1(codes(InS),In):-!,open_string(InS,In).
l_open_input1(chars(InS),In):-!,open_string(InS,In).


:- meta_predicate if_flag_true(0,0).
:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

%logicmoo_util_all:if_flag_true(Flag,Goal):- catch(Flag,_,fail)->catch(Goal,E,throw(if_flag_true(E)));true.
logicmoo_util_all:if_flag_true(Flag,Goal):- catch(Flag,E,(dmsg(E:Flag),fail)) -> must(Goal); true.

join_path33(A,B,C):-exists_directory(B)->B=C;directory_file_path(A,B,C).

:-dynamic(logicmoo_runtime_dir/1).
normalize_path(Where,WhereF3):-absolute_file_name(Where,WhereF),prolog_to_os_filename(WhereF,WhereF1),prolog_to_os_filename(WhereF2,WhereF1),!,clip_dir_sep(WhereF2,WhereF3).
normalize_path(Where,WhereF2):-clip_dir_sep(Where,WhereF2).


:-export(current_filesource/1).
:-export(current_filedir/1).
current_filedir(D):- no_repeats([D],(current_filesource(F),file_directory_name(F,D))).
current_filesource(F):-seeing(X),is_stream(X),stream_property(X,file_name(F)).
current_filesource(F):-stream_property(_,file_name(F)).

:- export(filematch/2).
:- meta_predicate(filematch(:,-)).
filematch(Spec,Result):-  enumerate_files(Spec,Result).


:- thread_local(thlocal:file_ext/1).
:- meta_predicate(filematch_ext(+,:,-)).
:- export(filematch_ext/3).
filematch_ext(Ext,FileIn,File):-
  with_assertions(thlocal:file_ext(Ext),filematch(FileIn,File)).

:- meta_predicate(enumerate_files(:,-)).
:- export(enumerate_files/2).
enumerate_files(M:Spec,Result):-
   user:no_repeats_old([Result],((user:enumerate_m_files(M,Spec,NResult),user:normalize_path(NResult,Result),exists_file_or_dir(Result)))).

:- meta_predicate(enumerate_files(:,-)).
:- export(enumerate_m_files/3).
enumerate_m_files(user, Mask,File1):-!,enumerate_files0(Mask,File1).
enumerate_m_files(M, Mask,File1):- 
  findall(thlocal:search_first_dir(Dir),
   (((M\=user,file_search_path(M,SP),expand_file_search_path(SP,Dir));((module_property(M, file(File)),directory_file_path(Dir,_,File)))),
   exists_directory(Dir)),List),list_to_set(List,Set),with_assertions(Set,enumerate_files0(Mask,File1)).

:- export(enumerate_files0/2).
enumerate_files0(Mask,File1):- absolute_file_name(Mask,X,[expand(true),file_errors(fail),solutions(all)]),expand_file_name(X,Y),member(File1,Y).
enumerate_files0(Mask,File1):- one_must(filematch3('./',Mask,File1),(current_filedir(D),filematch3(D,Mask,File1))).
enumerate_files0(Spec,Result):- enumerate_files00(Spec,Result).
enumerate_files0(Spec,Result):- to_filename(Spec,Result).
enumerate_files0(Mask,File1):-  (current_dirs(D),filematch3(D,Mask,File1)).

enumerate_files00(Spec,Result):- expand_file_name_safe(Spec,ResultList),ResultList\=[],!,member(NResult,ResultList),enumerate_files2(NResult,Result).
enumerate_files00(Spec,Result):- enumerate_files1(Spec,M),enumerate_files2(M,Result).


:- export(filematch3/3).
filematch3(RelativeTo,Mask,File1):-
   findall(Ext,thlocal:file_ext(Ext),EXTs),flatten([EXTs,'','pl.in'],Flat),
   absolute_file_name(Mask,File1Matched,[extensions(Flat),
   expand(true),file_errors(fail),solutions(all),relative_to(RelativeTo),access(read)]),expand_file_name(File1Matched,File1S),member(File1,File1S).
filematch3(RelativeTo,Mask,File1):-absolute_file_name(Mask,File1Matched,[file_type(directory),
   expand(true),file_errors(fail),solutions(all),relative_to(RelativeTo),access(read)]),expand_file_name(File1Matched,File1S),member(File1,File1S).

:- export(enumerate_files2/2).
enumerate_files2(Spec,Result):-sub_atom(Spec,_,1,_,'*') -> enumerate_files1(Spec,Result);Spec=Result.

:- export(enumerate_files1/2).
enumerate_files1(Atom,Result):- atomic(Atom),not(is_absolute_file_name(Atom)),atomic_list_concat(List,'/',Atom),!,concat_paths(List,Result).
enumerate_files1(Spec,Result):- exists_file_or_dir(Spec),!,Result=Spec.
enumerate_files1(P/C,Result):- !,concat_paths(P,C,Result).
enumerate_files1(Spec,Result):- expand_file_name_safe(Spec,ResultList),member(Result,ResultList).
enumerate_files1(Spec,Result):- file_search_path(Spec,Result).
enumerate_files1(Spec,Result):- expand_file_search_path(Spec,Result).
enumerate_files1(Spec,Result):- absolute_file_name(Spec,Result).
enumerate_files1(Atom,Result):- atomic(Atom),once((member(Sep,['/**/','/**','**']),atomic_list_concat([B,A|R],Sep,Atom))),concat_paths([B,'**',A|R],Result).

:- export(expand_file_name_safe/2).
expand_file_name_safe(I,O):-catch(expand_file_name(I,O),_,fail),O\=[],!.
expand_file_name_safe(I,[O]):-catch(expand_file_search_path(I,O),_,fail),!.
expand_file_name_safe(I,L):- findall(O,absolute_file_name(I,O,[expand(true),solutions(all)]);
 absolute_file_name(I,O,[expand(true),solutions(all),file_type(directory)]),L),!.

:- export(exists_file_or_dir/1).
exists_file_or_dir(X):- nonvar(X),( X=(_:F)->exists_file_or_dir(F); (atomic(X),once((catch(exists_file(X),E,(fmt(E:X),fail));is_directory(X))))).
:- export(is_directory/1).
is_directory(X):-exists_directory(X).

:- export(concat_paths/3).
concat_paths(A,'',A).
concat_paths(A,'/',A).
concat_paths(ParentIn,'**',Result):-!, member(Child,['./','./*/','./*/*/','./*/*/*/','./*/*/*/*/','./*/*/*/*/*/']),concat_paths(ParentIn,Child,Result).
concat_paths(ParentIn,Child,Result):- filematch(ParentIn,Parent),
   once(is_directory(Parent) -> directory_file_path(Parent,Child,Joined) ; atom_concat(Parent,Child,Joined)),
   filematch(Joined,Result).

:- export(concat_paths/2).
concat_paths([Joined],Result):- !,filematch(Joined,Result).
concat_paths([ParentIn,Child|MORE],Result):- concat_paths(ParentIn,Child,ResultM),concat_paths([ResultM|MORE],Result).


:-thread_local(thlocal:search_first_dir/1).

current_dirs(DO):- no_repeats(DO,(current_dirs0(D),(atom_concat(DO,'/',D)->true;DO=D))).
current_dirs0(D):- thlocal:search_first_dir(D).
current_dirs0(D):- prolog_load_context(directory,D).
current_dirs0(D):- working_directory(D,D).
current_dirs0(D):- current_stream(_,read,Y), stream_property(Y,file_name(FN)), file_directory_name(FN,D).
current_dirs0(D):- stream_property(_,file_name(FN)), file_directory_name(FN,D).

%current_dirs0(D):- expand_file_name('*/',X),member(E,X),absolute_file_name(E,D),exists_directory(D).
%current_dirs0(D):- expand_file_name('*/*/',X),member(E,X),absolute_file_name(E,D),exists_directory(D).
%current_dirs0(D):- expand_file_name('*/*/*/',X),member(E,X),absolute_file_name(E,D),exists_directory(D).
current_dirs0(D):- source_file_property(FN, modified(_)), file_directory_name(FN,D).
current_dirs0('.').

:-export(to_filename/2).
to_filename( FileName, FileName ) :- atomic(FileName),exists_file(FileName),!.
to_filename( FileName, AFN ) :-
 (((if_defined(default_extension( Ext ));Ext='.tlp';Ext='';Ext='.pl'), 
     current_dirs(D),
     member(TF,[false,true]),
        absolute_file_name(FileName,AFN,[solutions(all),expand(TF),access(read),relative_to(D),file_errors(fail),extensions(['',Ext,'.pl','.tlp','.clp','.P'])]),
        exists_file(AFN))),!.



:-export(with_vars/2).
:-module_transparent(with_vars/2). 
:- meta_predicate with_vars(*,0).
with_vars([],Stuff):- !, Stuff.
with_vars([V|Vs],Stuff):- !,
  b_getval('$variable_names', VsOrig),
  append([V|Vs],VsOrig,Temp),
  b_setval('$variable_names', Temp),!,Stuff.
with_vars(_,Stuff):- Stuff.

:- export(prolog_file_dir/1).
prolog_file_dir(Here):- prolog_load_context(file, HereF),file_directory_name(HereF,Here).
prolog_file_dir(Here):- working_directory(Here,Here).
:- export(prolog_file_dir/2).
prolog_file_dir(Rel,ABSF):-prolog_file_dir(Here),absolute_file_name(Rel,ABSF,[relative_to(Here),file_type(directory),expand(true)]),!.
prolog_file_dir(Rel,ABSF):-prolog_file_dir(Here),absolute_file_name(Rel,ABSF,[relative_to(Here),expand(true)]),!.

add_to_search_path_first(Alias, Abs) :- asserta_new(user:file_search_path(Alias, Abs)).

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







/*
:- 
    source_location(File,_Line),
    file_directory_name(File, RunDir),
    atom_concat(RunDir,'/../library',RelDir),
    my_absolute_file_name(RelDir,A),
   'format'(' ~q. ~n',[user:file_search_path(library, A)]),
   asserta(user:file_search_path(library, A)).
*/

:-multifile(local_directory_search/1).
:-dynamic(local_directory_search/1).
% Add the locations that the MUD source files will be picked up by the system
%local_directory_search('../..').
%local_directory_search('~logicmoo-mud/cynd/startrek'). % home vtDirection CynD world
% local_directory_search('.').
%local_directory_search('..'). 
%local_directory_search('../runtime'). 
%local_directory_search('../src_game'). % for user overrides and uploads
%local_directory_search('../src_assets').  % for non uploadables (downloadables)
%local_directory_search('../src_modules'). % for big modules
%local_directory_search('../src_webui').  % for web UI modules
local_directory_search('../src'). % shared_library preds
local_directory_search('../src_lib').
local_directory_search('../src_mud').  % for vetted src of the MUD
%local_directory_search('../externals/XperiMental/src_incoming').  % areeba underlay

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


:- '@'( ensure_loaded((logicmoo_util_bugger_new)), 'user').
:- '@'( ensure_loaded((logicmoo_util_bugger_catch)), 'user').
:- '@'( ensure_loaded((logicmoo_util_bugger)), 'user').
:- '@'( ensure_loaded((logicmoo_util_strings)), 'user').
:- '@'( ensure_loaded((logicmoo_util_library)), 'user').
:- '@'( use_module((logicmoo_util_ctx_frame)), 'user').
:- '@'( use_module((logicmoo_util_terms)), 'user').
:- '@'( use_module((logicmoo_util_dcg)), 'user').
:- '@'( use_module((logicmoo_util_coroutining_was)), 'user').
:- '@'( use_module((logicmoo_util_coroutining_iz)), 'user').


/*
win_fork(G,SERVIO,PID):-atom_concat('swipl-win.exe ',G,AC),writeq(win_fork(AC,SERVIO)),nl,
      win_exec(AC,showdefault),PID = 0.

:- current_prolog_flag(windows,true)->
   asserta((fork(G):-win_fork(G)));
   use_module(library(unix)).

% fork(G):-writeq(fork(G)),nl.
*/


show_file_search_path:- % 'format'('% ~q.~n',[forall(user:file_search_path(_,_))]), 
  forall(user:file_search_path(A,B),'format'('% ~q.~n',[user:file_search_path(A,B)])).

:- show_file_search_path.

% :- list_undefined.

:- logicmoo_util_dcg:call(do_dcg_util_tests).


% this is a backwards compatablity block for SWI-Prolog 6.6.6
:- retract(double_quotes_was(WAS)),set_prolog_flag(double_quotes,WAS).


