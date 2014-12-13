
:- set_prolog_flag(file_loading,verbose).
:- use_module('../../../src_lib/logicmoo_util/logicmoo_util_all.pl').
:- abolish(dbase_rosprolog,1).
:- dynamic(dbase_rosprolog/1).
:- prolog_load_context(file, Here),asserta(dbase_rosprolog(Here)).

:- dynamic(ros_package_initialized/1).

% enumerate_files(Spec,Result):- \+ exists_file_or_dir(Spec),!,expand_file_search_path(Spec,Result).
enumerate_files(Spec,Result):-expand_file_name(Spec,ResultList),member(NResult,ResultList),normalize_path(NResult,Result).

absolute_file_name_or_dir_opts(Spec,File,Options):-absolute_file_name(Spec,File,[file_type(directory),file_errors(fail)|Options])*->true;absolute_file_name(Spec,File,Options).

absolute_file_name_or_dir(Spec,File):-absolute_file_name(Spec,File,[file_type(directory),file_errors(fail)])*->true;absolute_file_name(Spec,File).

exists_file_or_dir(X):-exists_file(X),!.
exists_file_or_dir(X):-is_directory(X).
is_directory(X):-exists_directory(X).

concat_paths(ParentIn,Child,Result):- enumerate_files(ParentIn,Parent),
   (is_directory(Parent) -> directory_file_path(Parent,Child,Joined) ; atom_concat(Parent,Child,Joined)),
   enumerate_files(Joined,Result).

concat_paths([Joined],Result):- !,enumerate_files(Joined,Result).
concat_paths([ParentIn,Child|MORE],Result):- concat_paths(ParentIn,Child,ResultM),concat_paths([ResultM|MORE],Result).

dbase_rosprolog_dir(ADIR):- dbase_rosprolog(Here),file_directory_name(Here,DIR),absolute_file_name_or_dir(DIR,ADIR),!.

user:file_search_path(pack, knowrob).
user:file_search_path(knowrob,Where):- dbase_rosprolog(Here),file_directory_name(Here,DIR),concat_paths(DIR,'../../*/',OOO),enumerate_files(OOO,Where).

% :-asserta((library_directory(Where):-ros_library_directory(Where))).
user:library_directory(Where):-ros_library_directory(Where).

ros_library_directory(WhereF2):-user:file_search_path(knowrob,W),!,once((atom_concat(W,'/*/prolog/',O),
  expand_file_name(O,ListE))),member(Where,ListE),normalize_path(Where,WhereF2).

normalize_path(Where,WhereF2):- absolute_file_name(Where,WhereF),prolog_to_os_filename(WhereF,WhereF1),prolog_to_os_filename(WhereF2,WhereF1),!.
normalize_path(Where,Where):-!.

ros_package(B):-ros_library_directory(WhereF2),concat_paths(WhereF2,(..),O),file_base_name(O,B).

ros_package_initfile(PackagePath,InitFile):-
  concat_paths([PackagePath,'./prolog/init{.pl,.pl.in,}'],Where),expand_file_name(Where,[InitFile|_]).
ros_package_initfile(RosPack,InitFile):-
  expand_file_search_path(knowrob(RosPack/prolog/'init{.pl,.pl.in,}'),Where),expand_file_name(Where,[InitFile|_]).

ros_package_dir(RosPack,Dir):- ros_package_initfile(RosPack,InitFile),concat_paths([InitFile,fdn,'../'],Dir).




:- use_module(library('process')).

rospack_package_path(Package, Path) :- ros_package_dir(Package, Path).
rospack_package_path(Package, Path) :- expand_file_search_path(knowrob(Package),Path),exists_directory(Path).
rospack_package_path(Package, Path) :-
  must(nonvar(Package)),trace,
  process_create(path('rospack'), ['find', Package], [stdout(pipe(RospackOutput)), process(_PID)]),
  read_line_to_codes(RospackOutput, C),
  string_to_list(Path, C).

init_ros_package( PackagePath ) :-
 % atom_concat(PackagePath, 'init.pl', InitFile),
 ros_package_initfile(PackagePath,InitFile),
  exists_file(InitFile),
  consult(InitFile), !.

init_ros_package( _ ).

register_ros_package( Package, _ ) :-
  current_predicate(ros_package_initialized/1),
  ros_package_initialized(Package), !.

register_ros_package( Package, AbsoluteDirectory ) :-
  rospack_package_path(Package, PackagePath),
  nonvar(PackagePath),
  atom_concat(PackagePath, '/prolog/', AbsoluteDirectory),
  asserta(library_directory(AbsoluteDirectory)),
  assert(user:file_search_path(ros, AbsoluteDirectory)),
  assert( ros_package_initialized(Package) ),
  add_ros_package_to_classpath(Package),
  init_ros_package( AbsoluteDirectory ).

% do_not_register_ros_package( semweb ) :-!.
do_not_register_ros_package( jpl ) :-!.
do_not_register_ros_package( thea ) :-!.
do_not_register_ros_package( rosprolog ) :-!.

register_ros_package( Package ) :- do_not_register_ros_package( Package )  ,!.
register_ros_package( Package ) :-
  register_ros_package( Package, _ ).

use_ros_module( Package, FilePath ) :-
  register_ros_package(Package, AbsoluteDirectory),
  atom_concat(AbsoluteDirectory, FilePath, AbsoluteFilePath),
  use_module( AbsoluteFilePath ).

% adds java dependencies to classpath
add_ros_package_to_classpath(Package):-
	rospack_package_classpath(Package, Path),
	atom_concat(':',Path,PackagePath),
	concat_env("CLASSPATH",PackagePath).


% calculates java dependencies for classpath
rospack_package_classpath(Package, Path) :- ros_package_dir(Package, PackPath),atom_concat(PackPath,'**/*.jar',O),!,
  expand_file_name(O,ListE),atomic_list_concat(ListE,':',Path).
% calculates java dependencies for classpath
rospack_package_classpath(Package, Path) :-
  nonvar(Package),
  process_create(path('rospack'), ['export', '--lang=java', '--attrib=classpath', Package], [stdout(pipe(RospackOutput)), process(_PID)]),
  read_line_to_codes(RospackOutput, C),
  string_to_list(String, C),
  concat_atom(List,' ',String),% split string at ' '
  concat_atom(List,':',Path).  % concat list elements with seperator ':'

% concat a value to an environment varible
% please note: delimiters have to be set within Val, e.g.:
% ':/path/to/lib:/path/to/lib2'
concat_env(Var,Val):-
	(getenv(Var,OldVal)
	->  (atom_concat(OldVal,Val,NewVal)) ;
        (NewVal = Val)),
	setenv(Var,NewVal).

:- show_call(user:file_search_path(knowrob,_Where)).

:-prolog.
:- init_ros_package(knowrob_common).
:- init_ros_package(knowrob_actions).
:- init_ros_package(ias_knowledge_base).
:- init_ros_package(comp_semantic_map).
:- forall(ros_package(B),init_ros_package(B)).


