/** <module> 
% Very simple... but kept separate to maintain modularity
%
% Project Logicmoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
%
:- module(scansrc, []).

:- include(logicmoo(vworld/moo_header)).

:- register_module_type(command).

% when we import new and awefull code base (the previous )this can be helpfull
% we redfine list_undefined/1 .. this is the old version
real_list_undefined(A):- 
 merge_options(A, [module_class([user])], B),
        prolog_walk_code([undefined(trace), on_trace(found_undef)|B]),
        findall(C-D, retract(undef(C, D)), E),
        (   E==[]
        ->  true
        ;   print_message(warning, check(undefined_predicates)),
            keysort(E, F),
            group_pairs_by_key(F, G),
            maplist(check:report_undefined, G)
        ).


remove_undef_search:- ((
 '@'(use_module(library(check)),'user'),
 redefine_system_predicate(check:list_undefined(_)),
 abolish(check:list_undefined/1),
 assert((check:list_undefined(A):- not(thread_self(main)),!, ignore(A=[]))),
 assert((check:list_undefined(A):- ignore(A=[]),real_list_undefined(A))))).


moo:action_info(scansrc,"Scan for sourcecode modifed on filesystem and TeamSPoon. NOTE: only new files with this mask (src_incoming/*/?*.pl) are picked up on").
moo:agent_call_command(Agent,scansrc):-  '@'(agent_call_safely(Agent,scansrc),'user').

:-export(scansrc/0).
scansrc :- 
 ensure_loaded(library(make)),
 debugOnError((
  reload_library_index,
  %remove_undef_search,
  update_changed_files,
  include_moo_files_not_included('../src_incoming/*/?*.pl'),
   
   % autoload,
   % include_moo_files_not_included('../src_incoming/*/*/?*.pl'),
   % make,
   % include_moo_files_not_included('../src_incoming/*/?*.plmoo'),
   !)). 

include_moo_files_not_included(Mask):- 
   expand_file_name(Mask,X),
     forall(member(E,X),include_moo_file_ni(E)).

include_moo_file_ni(M):-absolute_file_name(M,EX,[expand(true),access(read),file_type(prolog)]),include_moo_file_ni_1(EX).


update_changed_files :-
	findall(File, make:modified_file(File), Reload0),
	list_to_set(Reload0, Reload),
	(   prolog:make_hook(before, Reload)
	->  true
	;   true
	),
	print_message(silent, make(reload(Reload))),
	maplist(make:reload_file, Reload),
	print_message(silent, make(done(Reload))),
	(   prolog:make_hook(after, Reload)
	->  true
	;   
           true %list_undefined,list_void_declarations
	).


include_moo_file_ni_1(M):- atomic_list_concat([_,_|_],'_i_',M),!.
include_moo_file_ni_1(M):- atomic_list_concat([_,_|_],'_c_',M),!.
include_moo_file_ni_1(M):- source_file_property(M,_),!.
include_moo_file_ni_1(M):- source_file_property(_,includes(M)),!.

include_moo_file_ni_1(M):- '@'(ensure_loaded(M),user).


:- include(logicmoo(vworld/moo_footer)).
