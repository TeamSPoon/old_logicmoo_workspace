/** <module> Logicmoo Path Setups
*/
:- set_prolog_flag(generate_debug_info, true).
:- '@'( ensure_loaded(library(filesex)), 'user').

:- meta_predicate(with_filematch(0)).
with_filematch(G):- expand_wfm(G,GG),!,GG.
with_filematches(G):- forall(expand_wfm(G,GG),GG).

expand_wfm(G,GG):- must((sub_term(Sub, G),compound(Sub),Sub=wfm(F))),
   (filematch(F,M),subst(G,wfm(F),M,GG),y_must(with_filematch(G), (G\=@=GG))).


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

:-dynamic(logicmoo_runtime_dir/1).

:- export(normalize_path/2).
normalize_path(Where,WhereF3):-absolute_file_name(Where,WhereF),prolog_to_os_filename(WhereF,WhereF1),prolog_to_os_filename(WhereF2,WhereF1),!,clip_dir_sep(WhereF2,WhereF3).
normalize_path(Where,WhereF2):-clip_dir_sep(Where,WhereF2).

clip_dir_sep('/','/'):-!.
clip_dir_sep(Where,WhereF2):-atom_concat(WhereF2,'/',Where),!.
clip_dir_sep(Where,Where):-!.


