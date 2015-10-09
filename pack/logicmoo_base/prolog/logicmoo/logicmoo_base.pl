/** <module> 
% File used as storage place for all predicates which change as
% the world is run.
%
% props(Obj,height(ObjHt)) == k(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt)) == padd(height,Obj,ObjHt,...) == add(QueryForm)
% kretract[all](Obj,height(ObjHt)) == kretract[all](Obj,height,ObjHt) == pretract[all](height,Obj,ObjHt) == del[all](QueryForm)
% keraseall(AnyTerm).
%
%
% Dec 13, 2035
% Douglas Miles
*/



:- meta_predicate with_kb_assertions(:,0).
:- meta_predicate with_kb_assertions_matching(?,?,0).
:- meta_predicate with_assert_op_override(?,0).


:- multifile lmconf:startup_option/2. 
:- dynamic lmconf:startup_option/2. 

lmconf:startup_option(datalog,sanity). %  Run datalog sanity tests while starting
lmconf:startup_option(clif,sanity). %  Run datalog sanity tests while starting

:- dynamic   file_search_path/2.
:- multifile file_search_path/2.
:- prolog_load_context(directory,Dir),
   %Dir = (DirThis/planner),
   DirFor = logicmoo,
   (( \+ file_search_path(DirFor,Dir)) ->asserta(file_search_path(DirFor,Dir));true),
   absolute_file_name('../../../',Y,[relative_to(Dir),file_type(directory)]),
   (( \+ file_search_path(pack,Y)) ->asserta(file_search_path(pack,Y));true).
:- attach_packs.
:- initialization(attach_packs).

% [Required] Load the Logicmoo Library Utils
:- ensure_loaded(library(logicmoo/logicmoo_utils)).

:- prolog_load_context(directory,Dir),asserta(file_search_path(logicmoo,Dir)).
:- dynamic(isa_pred_now_locked/0).

:- include(mpred/logicmoo_i_header).



/*
:- meta_predicate call_mpred_body(*,0).
:- meta_predicate decl_mpred_hybrid_ilc_0(*,*,0,*).
:- meta_predicate assert_isa_hooked(0,*).
*/
:- meta_predicate t(7,?,?,?,?,?,?,?).
:- meta_predicate t(6,?,?,?,?,?,?).
:- meta_predicate t(5,?,?,?,?,?).
:- meta_predicate t(3,?,?,?).
:- meta_predicate t(4,?,?,?,?).
:- meta_predicate t(2,?,?).


% ========================================
% user:mpred_mod/1
% ========================================

% TODO uncomment the next line without breaking it all!
% thglobal:use_cyc_database.

:-asserta(thglobal:pfcManageHybrids).

:- export(user:mpred_mod/1).
:- dynamic user:mpred_mod/1.
user:mpred_mod(user).


% [Manditory] define how we interact with the module system
:-if(not(current_predicate(swi_module/2))).
:-export(swi_module/2).
swi_module(M,E):-dmsg(swi_module(M,E)).
:-endif.





% ================================================
% Debugging settings
% ================================================

:-export(is_stable/0).

is_stable:-fail.

:- if(current_prolog_flag(optimise,true)).
is_recompile.
:- else.
is_recompile:-fail.
:- endif.

fast_mud.
xperimental:-fail.
xperimental_big_data:-fail.

simple_code :- fail.
save_in_mpred_t:-true.
not_simple_code :- \+ simple_code.
type_error_checking:-false.
% slow_sanity(A):-nop(A).
:- meta_predicate xtreme_debug(0).
xtreme_debug(P):- is_release,!,nop(P).
xtreme_debug(P):- not_is_release, verify_sanity(P).
xtreme_debug(_).

:- meta_predicate verify_sanity(0).
verify_sanity(P):- \+ is_recompile, (true; is_release),!,nop(P).
verify_sanity(P):- on_x_rtrace(hotrace(P)),!.
verify_sanity(P):- dmsg('$ERROR_incomplete_SANITY'(P)),!.
:-meta_predicate(when_debugging(+,0)).
when_debugging(What,Call):- debugging(What),!,Call.
when_debugging(_,_).

% :- asserta(tlbugger:no_colors).
% :- asserta(tlbugger:show_must_go_on).

:- set_prolog_flag(double_quotes, atom).
:- set_prolog_flag(double_quotes, string).

% ================================================
% DBASE_T System
% ================================================
:- gripe_time(40,user:ensure_loaded(logicmoo(mpred_online/logicmoo_i_www))).

:- ensure_loaded(mpred/logicmoo_i_wff).
:- ensure_loaded(mpred/logicmoo_i_listing).
:- ensure_loaded(mpred/logicmoo_i_pfc).
:- ensure_loaded(mpred/logicmoo_i_loader).
:- ensure_loaded(mpred/logicmoo_i_naming).
:- ensure_loaded(mpred/logicmoo_i_term_expansion).
:- ensure_loaded(mpred/logicmoo_i_types).

:- ensure_loaded(mpred/logicmoo_i_mpred_props).
:- ensure_loaded(mpred/logicmoo_i_agenda).
:- ensure_loaded(mpred/logicmoo_i_call).
:- ensure_loaded(mpred/logicmoo_i_coroutining).
:- ensure_loaded(mpred/logicmoo_i_hooks).
:- ensure_loaded(mpred/logicmoo_i_store).
:- ensure_loaded(mpred/logicmoo_i_mpred_stubs).
:- ensure_loaded(mpred/logicmoo_i_argtypes).

:-dynamic(t_l:mpred_already_in_file_expansion/1).

:- asserta(t_l:disable_mpred_term_expansions_locally).

% user:goal_expansion(ISA,G) :- compound(ISA),t_l:is_calling,use_was_isa(ISA,I,C),to_isa_out(I,C,OUT),G=no_repeats(OUT).
:- meta_predicate(lmbase_record_transactions(?,?)).
:- meta_predicate(lmbase_record_transactions_maybe(?,?)).
:- meta_predicate(mpred_file_expansion(?,?)).


lmbase_record_transactions(I,OO):-thread_self(X),X\==main,!,I=OO.

% not actual function
lmbase_record_transactions(I,OO):- nonvar(I),current_predicate(mpred_loader_file/0),current_predicate(logicmoo_bugger_loaded/0), 
  ( \+ t_l:mpred_already_in_file_expansion(I) ),
  w_tl(t_l:mpred_already_in_file_expansion(I),if_defined(lmbase_record_transactions_maybe(I,OO))),!,I\=@=OO,
  nop(dmsg(mpred_file_expansion(I,OO))).


lmbase_record_transactions_maybe(I,Supposed):- 
  t_l:verify_side_effect_buffer,!,
   sanity(var(ActualSupposed)),
    push_predicates(t_l:side_effect_buffer/3,STATE),
    mpred_file_expansion(I,Supposed),
    current_source_location(Why),
    collect_expansions(Why,I,Actual),
    convert_side_effect(suppose(Supposed),S),
    conjoin(S, Actual,ActualSupposed),
    conjuncts_to_list(ActualSupposed,Readable),
    assert(actual_side_effect(I,Readable)),
    pop_predicates(t_l:side_effect_buffer/3,STATE),!.



lmbase_record_transactions_maybe(I,ActualSupposed):- 
  t_l:use_side_effect_buffer,!,trace,
   sanity(var(ActualSupposed)),
    push_predicates(t_l:side_effect_buffer/3,STATE),
    mpred_file_expansion(I,Supposed),
    current_source_location(Why),
    collect_expansions(Why,I,Actual),
    conjoin(Actual,Supposed,ActualSupposed),
    pop_predicates(t_l:side_effect_buffer/3,STATE),!.

lmbase_record_transactions_maybe(I,OO):- mpred_file_expansion(I,OO),!.


collect_expansions(_Why,I,I):- \+ t_l:side_effect_buffer(_Op,_Data,_),!.
collect_expansions(NWhy,_I, TODO):- findall(ReproduceSWhy, 
  ( retract(t_l:side_effect_buffer(Op, Data, Why)),
    must_det_l(convert_side_effect(Op, Data,Reproduce)),
    must(simplify_why_r(Reproduce,Why,NWhy,ReproduceSWhy))), TODOs),
   must_det_l( list_to_conjuncts(TODOs,TODO)).

simplify_why_r(Reproduce,Why,NWhy,   Reproduce):- Why==NWhy, !.
simplify_why_r(Reproduce,Why,_,Reproduce:SWhy):-simplify_why(Why,SWhy),!.
 
% aliases
:-meta_predicate(convert_side_effect(?,+,-)).

simplify_why(Why,SWhy):-var(Why),!,Why=SWhy.
simplify_why(Why:0,SWhy):-!,simplify_why(Why,SWhy).
simplify_why(Why:N,SWhy:N):-!,simplify_why(Why,SWhy).
simplify_why(Why,SWhy):- atom(Why),!,directory_file_path(_,SWhy,Why).
simplify_why(Why,Why).

convert_side_effect(M:C,A,SE):- Call=..[C,A],!,convert_side_effect(M:Call,SE).
convert_side_effect(C,A,SE):- Call=..[C,A],!,convert_side_effect(Call,SE).

convert_side_effect(suppose(OO), suppose(Result)):- convert_side_effect_0a(OO,Result),!.
convert_side_effect(I,OO):-convert_side_effect_0c(I,O),((O=(N-_V),number(N))->OO=O;OO=O),!.

convert_side_effect_0a(asserta(Data), (  a(DataR))):-convert_side_effect_0a(Data,DataR).
convert_side_effect_0a(assertz(Data), (  (DataR))):-convert_side_effect_0a(Data,DataR).
convert_side_effect_0a(retract(Data), (  r(DataR))):-convert_side_effect_0a(Data,DataR).
convert_side_effect_0a(cl_assert(Why,Data), (  cl_assert(Why,DataR))):-convert_side_effect_0a(Data,DataR).
convert_side_effect_0a(attvar_op(Why,Data),Reproduce):-!,convert_side_effect(Why,Data,Reproduce),!.
convert_side_effect_0a(I,O):-convert_side_effect_0b(I,O),!.
convert_side_effect_0a(I,I).

convert_side_effect_0b((OpData:-TRUE),Result):- is_true(TRUE),!,convert_side_effect_0a(OpData,Result),!.
convert_side_effect_0b(suppose(OpData),Result):-!,convert_side_effect_0a(OpData,Result),!.
convert_side_effect_0b(user:OpData,Reproduce):- !,convert_side_effect_0a(OpData,Reproduce),!.
convert_side_effect_0b(( :- OpData),( ( (Result)))):-!,convert_side_effect_0a(OpData,Result),!.
convert_side_effect_0b('$was_imported_kb_content$'(_, OO),Result):-!,convert_side_effect_0a(OO,Result),!.
convert_side_effect_0b(asserta_if_new(Data),Result):-!,convert_side_effect_0a(asserta(Data),Result).
convert_side_effect_0b(assertz_if_new(Data),Result):-!,convert_side_effect_0a(assertz(Data),Result).
convert_side_effect_0b(assert_if_new(Data),Result):-!,convert_side_effect_0a(assertz(Data),Result).
convert_side_effect_0b(assert(Data),Result):-!,convert_side_effect_0a(assertz(Data),Result).

convert_side_effect_0c(OpData,Reproduce):- convert_side_effect_0b(OpData,Reproduce),!.
convert_side_effect_0c(OpData,Reproduce):- show_call_success(convert_side_effect_buggy(OpData,Reproduce)),!.
convert_side_effect_0c(OpData,Reproduce):- trace_or_throw(unknown_convert_side_effect(OpData,Reproduce)),!.

% todo
convert_side_effect_buggy(erase(clause(H,B,_Ref)), (e(HB))):- convert_side_effect_0a((H:-B),HB).
convert_side_effect_buggy(retract(Data), (r(DataR))):-convert_side_effect_0a(Data,DataR).
convert_side_effect_buggy(retractall(Data), (c(DataR))):-convert_side_effect_0a(Data,DataR).
convert_side_effect_buggy(OpData,( (  error_op(OpData)))):-dmsg(unknown_convert_side_effect(OpData)).


clear_predicates(M:H):- forall(M:clause(H,_,Ref),erase(Ref)).
push_predicates(M:F/A,STATE):- functor(H,F,A),findall((H:-B), (M:clause(H,B,Ref),erase(Ref)), STATE).
pop_predicates(M:F/A,STATE):- functor(H,F,A),forall(member((H:-B),STATE),M:assert((H:-B))).



user:term_expansion(I,OO):- (I==end_of_file->(must(do_end_of_file_actions),fail);
                                 (\+ t_l:disable_mpred_term_expansions_locally, 
                                     if_defined(lmbase_record_transactions(I,OO)),I\=@=OO)).

:-export(mpred_file_loaded/0).

mpred_file_loaded.

% :- read_source_files.
% logicmoo_html_needs_debug.
:- if((lmconf:startup_option(www,sanity),if_defined(logicmoo_html_needs_debug))).
:- write(ready),nl,flush_output.
:- prolog.
:- endif.
:-  user:call(with_mfa_of(user: (dynamic_safe)),user,user,boxlog_to_compile(_D,_E,_F),boxlog_to_compile/3).
:- retractall(t_l:disable_mpred_term_expansions_locally).

:- ensure_mpred_file_loaded(mpred/logicmoo_i_builtin).
:- w_tl(tlbugger:ifHideTrace,(ensure_mpred_file_loaded(mpred/logicmoo_i_builtin))).

:- list_undefined.
