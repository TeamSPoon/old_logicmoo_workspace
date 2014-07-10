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
 use_module(library(check)),
 redefine_system_predicate(check:list_undefined(_)),
 abolish(check:list_undefined/1),
 assert((check:list_undefined(A):- not(thread_self(main)),!, ignore(A=[]))),
 assert((check:list_undefined(A):- ignore(A=[]),real_list_undefined(A))))).


moo:action_info(scansrc,"Scan for sourcecode modifed on filesystem and TeamSPoon").

scansrc :- include_moo_files('../src_incoming/*/?*.pl'),remove_undef_search,make.

:- include(logicmoo(vworld/moo_footer)).
