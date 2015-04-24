/** <module> pfc (wint pfc_)
% Provides a prolog database replacent that uses PFC
%
% For the Logicmoo Project
% Maintainer: Douglas Miles
% Dec 13, 2035
%   File   : pfc
%   Author : Tim Finin, finin@umbc.edu
%   Updated: 10/11/87, ...
%   Purpose: Provides a prolog database replacent that uses PFC
%
*/
:- user:ensure_loaded(library(terms)).
:- user:ensure_loaded(library(base32)).

% :-autoload.

:- dynamic(thglobal:use_cyc_database/0).
:- thread_local(thlocal:agenda_slow_op_do_prereqs/0).
:- thread_local(thlocal:already_in_file_term_expansion/0).
:- thread_local(thlocal:assert_op_override/1).
:- thread_local(thlocal:caller_module/2).
:- thread_local(thlocal:consulting_sources/0).
:- thread_local(thlocal:deduceArgTypes/1).
:- thread_local(thlocal:enable_src_loop_checking/0).
:- thread_local(thlocal:in_prolog_source_code/0).
:- thread_local(thlocal:infMustArgIsa/0).
:- thread_local(thlocal:infSkipArgIsa/0).
:- thread_local(thlocal:infSkipFullExpand/0).
:- thread_local(thlocal:is_calling/0).
:- thread_local(thlocal:mpred_opcall/2).
:- thread_local(thlocal:mpred_pfc_add_loaded/0).
:- thread_local(thlocal:noDBaseHOOKS/1).
:- thread_local(thlocal:noDBaseMODs/1).
:- thread_local(thlocal:pfc_loads_file/0).
:- thread_local(thlocal:with_callMPred/1).
:- dynamic(user:isa_pred_now_locked/0).
:- dynamic(pfc_manages_unknowns/0).

:- asserta(thlocal:infForward).

:- dynamic(pfc_skipped_module/1).
:-forall(current_module(CM),assert(pfc_skipped_module(CM))).
:-retractall(pfc_skipped_module(pfc)).
:-show_call(loading_module(X)),retractall(X).

:-listing(pfc_skipped_module/1).


:-multifile(thlocal:into_form_code).
:-thread_local(thlocal:into_form_code).
:-thread_local(thlocal:disable_mpred_term_expansions_locally/0).
:-dynamic(thglobal:thlocal:disable_mpred_term_expansions_locally_globally/0).

%fwc:-true.
%bwc:-true.
cwc:-true.

%is_fc_body(P):- (fwc==P ; (compound(P),arg(1,P,E),is_fc_body(E))),!.
%is_bc_body(P):- (bwc==P ; (compound(P),arg(1,P,E),is_bc_body(E))),!.
is_code_body(P):- (cwc==P ; (compound(P),arg(1,P,E),is_code_body(E))),!.


:- meta_predicate(with_source_module(:,(*))).
with_source_module(M:_,CALL):- !, setup_call_cleanup('$set_source_module'(Old, M),CALL,'$set_source_module'(_, Old)).
with_source_module(M,CALL):- setup_call_cleanup('$set_source_module'(Old, M),CALL,'$set_source_module'(_, Old)).



pfc_using_file(F):- var(F),!,source_file(F), pfc_using_file(F),!.
pfc_using_file(F):- inside_file(pfc),!.
pfc_using_file(F):- file_name_extension(_,pfc,F),!.


must_compile_special_clause(CL):- \+ thlocal:disable_mpred_term_expansions_locally,  (CL \= :-(_)), 
   sanity(nonvar(CL)), \+(thlocal:into_form_code),
    \+(thlocal:already_in_file_term_expansion),
    \+((get_functor(CL,F),expanded_already_functor(F))),
   pfc_db_type(CL,_),!.

:-multifile(thlocal:pfc_module_expansion/1).
:-thread_local(thlocal:pfc_module_expansion/1).

pfc_use_module(M):- must(atom(M)),retractall(pfc_skipped_module(M)),show_call(asserta_if_new(thlocal:pfc_module_expansion(M))).

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
expanded_already_functor('$was_imported_kb_content$').
expanded_already_functor(_:NV):-nonvar(NV),!,expanded_already_functor(NV).

% expanded_already_functor(F):-mpred_prop(F,code).


%:-thread_local is_compiling_clause/0.
%is_compiling:-is_compiling_clause;compiling.




pfc_prove_neg(G):-trace, \+ pfc_bc_caching(G), \+ pfc_fact(G).

:- multifile(system:term_expansion/2).
:- multifile(system:goal_expansion/2).
system:goal_expansion(A,_B):-fail,notrace((source_module(M),(M=pfc_sanity;M=user;M=system),if_defined(pmsg(M:goal_expansion(A)),format(user_error,'~N% ~q~n',M:goal_expansion(A))))),fail.
system:term_expansion(A,_B):-fail,notrace((source_module(M),(M=pfc_sanity;M=user;M=system),if_defined(pmsg(M:term_expansion(A)),format(user_error,'~N% ~q~n',M:term_expansion(A))))),fail.

system:goal_expansion(N,pfc_prove_neg(P)):-fail,pfc_from_negation_plus_holder(N,P),show_call_failure(mpred_prop(P,pfc_control)).



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

pfc_directive_expansion(pfc_begin,file_begin(pfc)):- must(source_location(S,_);context_module(S)).
pfc_directive_expansion(pfc_end,file_end(pfc)):- must(source_location(S,_);context_module(S)).
pfc_directive_expansion(dyn_begin,file_begin(dynamic_reader)):- must(source_location(S,_);context_module(S)).
pfc_directive_expansion(dyn_end,file_end(dynamic_reader)):- must(source_location(S,_);context_module(S)).


file_begin(W):- must_det(( prolog_load_context(file,Source),asserta(user:mpred_directive_value(W,file,Source)))).
file_end(W):- must_det(( prolog_load_context(file,Source),retract(user:mpred_directive_value(W,file,Source)))).
inside_file(W) :- prolog_load_context(file,Source),user:mpred_directive_value(W,file,Source),!.
inside_file(W) :- prolog_load_context(source,Source),user:mpred_directive_value(W,file,Source),!.




system:term_expansion((:- (M:DIR)),O):-atom(M),atom(DIR),with_source_module(M, ((pfc_directive_expansion(DIR,OO),!, must(O=(:- OO))))).
system:term_expansion((:- DIR),O):- atom(DIR), pfc_directive_expansion(DIR,OO),!,must(O=(:- OO)).


system:term_expansion(A,BO):- fail, notrace((A\=(:-_),A\=end_of_file,current_predicate(pfc_file_loaded/0))), 
   notrace((\+ thlocal:disable_mpred_term_expansions_locally, \+ thlocal:already_in_file_term_expansion,\+ (get_functor(A,F),expanded_already_functor(F)))),
   ((source_file(I),must(loading_module(M);source_module(M)))),
   with_no_assertions(thlocal:consulting_sources, 
   with_source_module(M, loop_check(pfc_file_module_term_expansion(I,M,A,B)))),
   must(nonvar(B)),BO=B.


:-export(pfc_file_loaded/0).
pfc_file_loaded.


end_of_file.


% ISA QUERY
system:goal_expansion(ISA,GO) :- \+ thlocal:disable_mpred_term_expansions_locally, once((compound(ISA),was_isa(ISA,I,C))),thlocal:is_calling,show_call(GO=no_repeats(isa(I,C))).
% ISA GOAL
% pfc_system_goal_expansion(G,GO):-G\=isa(_,_),was_isa(G,I,C),GO=isa(I,C).
% ISA EVER
%pfc_term_expansion(G,GO):-  \+ thlocal:disable_mpred_term_expansions_locally,was_isa(G,I,C),GO=isa(I,C).





:-if(current_module(pfc)).
:-throw(loaded(pfc)).
:-else.
:-if(fail).
:- module(pfc,[pfc_add/1,pfc_rem/1,
          op(500,fx,('~')),op(1075,xfx,('=>')),
          op(1075,xfx,('<=>')),op(1075,xfx,('<=')),
          op(1100,fx,('=>')),
          op(1150,xfx,('::::')),          
         (<=>)/2,(=>)/2,(<=)/2,(=>)/1,(~)/1,
      %   op(400,yfx,'\\\\'),op(1200,xfx,'-->>'),op(1200,xfx,'--*>>'), op(1200,xfx,'<<--'),
          pfc_add/2]).
:-endif.

:- op(500,fx,('~')),op(1075,xfx,('=>')),
          op(1075,xfx,('<=>')),op(1075,xfx,('<=')),
          op(1100,fx,('=>')),
          op(1150,xfx,('::::')).


:- dynamic(user:isa/2).
:- multifile(user:isa/2).
:- user:export(user:isa/2).
:- import(user:isa/2).

:- dynamic(user:is_storage_functor/2).
:- multifile(user:is_storage_functor/2).
:- user:export(user:is_storage_functor/2).
:- import(user:is_storage_functor/2).

:- dynamic(user:is_meta_functor_hooked/2).
:- multifile(user:is_meta_functor_hooked/2).
:- user:export(user:is_meta_functor_hooked/2).
:- import(user:is_meta_functor_hooked/2).

:- dynamic(user:is_literal/2).
:- multifile(user:is_literal/2).
:- user:export(user:is_liteeral/2).
:- import(user:is_literal/2).

:- dynamic(user:pfc_is_negated_functor/3).
:- multifile(user:pfc_is_negated_functor/3).
:- user:export(user:pfc_is_negated_functor/3).
:- import(user:pfc_is_negated_functor/3).

:- dynamic(user:arity/2).
:- multifile(user:arity/2).
:- user:export(user:arity/2).
:- import(user:arity/2).

:- user:dynamic(user:ruleRewrite/2).
:- user:multifile(user:ruleRewrite/2).
%:- user:export(user:ruleRewrite/2).
%:- user:import(user:ruleRewrite/2).

:- dynamic(user:isa/2).
:- multifile(user:isa/2).
:- user:export(user:isa/2).
:- import(user:isa/2).

:-dynamic('$was_imported_kb_content$'/2).
:-multifile('$was_imported_kb_content$'/2).
:-user:export('$was_imported_kb_content$'/2).
:-dynamic('$was_imported_kb_content$'/2).


:- user:ensure_loaded(library(logicmoo/util/logicmoo_util_bugger)).
:- user:ensure_loaded(library(logicmoo/util/logicmoo_util_bugger_iza)).
:- user:ensure_loaded(library(logicmoo/util/logicmoo_util_bugger_waz)).
:- user:ensure_loaded(library(logicmoo/logicmoo_util_strings)).


:- include(logicmoo(mpred/logicmoo_props)).
:- include(logicmoo(mpred/logicmoo_form)).
% :- use_module(logicmoo(mpred/logicmoo_hooks)).
% :- include(logicmoo(mpred/logicmoo_call)).
% :- include(logicmoo(mpred/logicmoo_database)).

/*
bugger:was/2
mpred_form:add/1
alt_calls/1
argIsa_ft/3
was_isa/3,
*/

potrace(C):-C.
source_user(u):-!.
source_user(M):-(source_module(M)),!.
source_user(M):-context_module(M).

mpred_prop(A,B,C):-mpred_get(A,B,C).

:- 
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
   export(('neg')/1).

  
:-
   dynamic(('<=')/2),
   dynamic(('<=>'/2)),
   dynamic((('=>')/2)),
   dynamic(('=>')/1),
   dynamic(('~')/1),
   dynamic(('neg')/1).

pmsg(F,A):-implode_varnames_copy(A,AA),sformat(S,F,AA),pmsg(S).
pmsg(S):-implode_varnames_copy(S,SC),wdmsg(pfc(SC)).

:- set_prolog_flag(generate_debug_info, true).
:- op(500,fx,('~')),op(1075,xfx,('=>')), op(1075,xfx,('<=>')),op(1075,xfx,('<=')),op(1100,fx,('=>')),op(1150,xfx,('::::')),!.


default_module(user).

:- thread_local(user:pfc_uses/2).


:- dynamic(('<=')/2).
:- dynamic(('<=>')/2).
:- dynamic(('=>')/2).
:- dynamic(('=>')/1).
:- dynamic(('~')/1).
:- dynamic(('bt')/2).
:- dynamic(('nt')/3).
:- dynamic(('pt')/2).
:- dynamic(('pfc_t_pkey')/3).
:- dynamic(('::::')/2).
:- dynamic((pfc_action)/2).
:- dynamic((pfc_haltSignal)/1).
:- dynamic((pfc_queue)/2).
:- dynamic((pfc_select)/2).
:- dynamic((pfc_undo_method)/2).
:- dynamic((spft)/3).
:- dynamic((pfc_ignored)/1).

:-style_check(-singleton).
:-style_check(-discontiguous).



:- meta_predicate(logicmoo_util_strings:convert_members(2,?,?)).
:- meta_predicate(user:db_expand_maplist(2,*,*,*,*)).
:- meta_predicate(user:with_logical_functor(*,*,1)).
:- meta_predicate(user:mpred_pa_call(*,*,(*))).
:- meta_predicate(user:whenAnd((*),(*))).
:- meta_predicate(user:simply_functors(2,*,*)).
:- meta_predicate(user:must_op(*,(*))).
:- meta_predicate(user:with_fail_mpred_asserted(*,(*))).
:- meta_predicate(user:compare_op(*,2,?,?)).
:- meta_predicate(user:mpred_call(*,(*))).
:- meta_predicate(bugger:within_user((*))).
:- meta_predicate(bugger:no_repeats_av(*,(*))).
:- meta_predicate(bugger:must_not_repeat((*))).
:- meta_predicate(bugger:time_call((*))).
:- meta_predicate(bugger:transitive1(2,?,?)).
:- meta_predicate(bugger:once_if_ground((*),*)).
:- meta_predicate(bugger:once_if_ground((*))).
:- meta_predicate(bugger:test_tl((*),*,*)).
:- meta_predicate(bugger:moo_withpred_all(2,?)).
:- meta_predicate(bugger:transitive_na(2,?,?,*)).
:- meta_predicate(bugger:no_repeats_av_l(*,(*))).
:- meta_predicate(bugger:with_all_dmsg((*))).
:- meta_predicate(bugger:with_vars(*,(*))).
:- meta_predicate(bugger:test_tl(1,*)).
:- meta_predicate(bugger:no_repeats_av((*))).
:- meta_predicate(user:transitive_P_l_r(3,?,?,?)).
:- meta_predicate(user:callOr(1,?,?)).
:- meta_predicate(user:transitive_P_r_l(3,?,?,?)).
:- meta_predicate(user:transitive_P(3,?,?,?)).
% Restarting analysis ...
% Found new meta-predicates in iteration 2 ((*).245 sec)
:- meta_predicate(bugger:transitive(2,?,?)).
:- meta_predicate(bugger:transitive_na(2,?,?)).
:- meta_predicate(user:h(7,?,?,?,?,?,?,?)).
:- meta_predicate(user:h(2,?,?)).
:- meta_predicate(user:h(3,?,?,?)).
:- meta_predicate(user:h(4,?,?,?,?)).
:- meta_predicate(user:h(5,?,?,?,?,?)).
:- meta_predicate(user:h(6,?,?,?,?,?,?)).

:- meta_predicate(brake(?)).
:- meta_predicate(log_and_continue_on_failure(?)).
:- meta_predicate(pfc_add_minfo(?,?)).
:- meta_predicate(pfc_add_minfo(?,?)).
:- meta_predicate(pfc_add_minfo_2(?,?)).
:- meta_predicate(pfc_addPreTermExpansion(?,?)).
:- meta_predicate(pfc_compile(+,+)).
:- meta_predicate(pfc_do_once(?)).
:- meta_predicate(pfc_xform(+,+,+,-)).
:- meta_predicate(pfc_forEach(?,?)).
:- meta_predicate(pfc_lambda(?,?,?)).
:- meta_predicate(pfc_lambda(?,?,?,?)).
:- meta_predicate(pfc_lambda(?,?,?,?,?)).
:- meta_predicate(pfc_lambda(?,?,?,?,?,?)).
:- meta_predicate(pfc_loop_check(?)).
:- meta_predicate(pfc_must(?)).
:- meta_predicate(pfc_negate(+,?,?)).
:- meta_predicate(pfc_nf_negation(?,?)).
:- meta_predicate(pfc_not_too_deep(?,?)).
:- meta_predicate(pfc_post1(+,?)).
:- meta_predicate(pfc_rem(?)).
:- meta_predicate(pfc_rem1(?,?)).
:- meta_predicate(pfc_rem2(?,?)).
:- meta_predicate(pfc_rem2_user(?)).
:- meta_predicate(pfc_rem2a(?,?)).
:- meta_predicate(pfc_rem3(?)).
:- meta_predicate(pfc_removeIfUnsupported(?)).
:- meta_predicate(pfc_transform_neck(+,+,-)).
:- meta_predicate(pfc_transform_neck(+,-)).
:- meta_predicate(pfc_undo(?)).
:- meta_predicate(resolveConflict(?)).
:- meta_predicate(unused_pfc_addSome(?)).
:- meta_predicate(unused_pfc_addType(?,?)).


:- multifile('=>'/2).
:- multifile('<=>'/2).
:- multifile('<='/2).
:- discontiguous('=>'/2).
:- discontiguous('<=>'/2).
:- discontiguous('<='/2).

:- dynamic('=>'/2).
:- dynamic('<=>'/2).
:- dynamic('<='/2).

%pfc_sformat(F,F,[]):-!.
pfc_sformat(S,F,A):-sformat(S,F,A).

pfc_must(G):-show_call_failure(must((G))).
log_and_continue_on_failure(G):-ignore(show_call_failure(G)).
% log_and_continue_on_failure(G):-ignore((G)).

pfc_is_unbound(V):- is_ftVar(V).
pfc_is_bound(V):-  not_ftVar(V).

/*
db_retractall(X):- retractall(X).
db_retract(X):-  retract(X).
db_assertz(X):- assertz(X).
db_asserta(X):- asserta(X).
db_assert(X):- assert(X).
*/

mpred_h_un(X,M,XX):-pfc_data_type(X,_),unnumbervars(X,XX),M=user.
mpred_h_un(X,M,XX):- mpred_home_module(X,M0),unnumbervars(X,XX),(M0==pfc->M=user;M=M0).

db_retractall(X):- mpred_h_un(X,M,XX),M:retractall(XX).
db_retract(X):-  mpred_h_un(X,M,XX),M:retract(XX).
db_assertz(X):- mpred_h_un(X,M,XX),assertz_if_new(M:XX).
db_asserta(X):- mpred_h_un(X,M,XX),asserta_new(M:XX).
db_assert(X):- mpred_h_un(X,M,XX),assert_if_new(M:XX).
db_uclause_ref(H,Y,Ref):- mpred_home_module(X,M),M:clause(X,Y,Ref).
db_uclause(H,B):- must(pfc_local(H)),
   (current_predicate(_,H) -> (has_clauses(H) -> clause(H,B) ; B = mpred_call(H)); % simulates a body for system predicates TODO rethink
                                             B = mpred_call(H)).

/*
db_retractall(X):- retractall(X).
db_retract(X):-  retract(X).
db_assertz(X):- assertz_if_new(X).
db_asserta(X):- asserta_new(X).
db_assert(X):- assert_if_new(X).
db_uclause_ref(X,Y,Ref):- clause(X,Y,Ref).
db_uclause(H,B):- must(pfc_local(H)),
   (current_predicate(_,H) -> (has_clauses(H) -> clause(H,B) ; B = mpred_call(H)); % simulates a body for system predicates TODO rethink
                                             B = mpred_call(H)).
*/

has_clauses(H):-mpred_ccount(H,C).
mpred_ccount(H,C):-predicate_property(H,number_of_clauses(C)).

% HACK FOR NOW
pfc_warn_local(X):-must(pfc_local(X)).


pfc_not_asserted_unify((Head:-Tail)) :- pfc_warn_local(Head), !, \+ db_uclause(Head,Tail). 
pfc_not_asserted_unify((Head:-Tail)) :-  !, \+ mpred_uclause(Head,Tail). 
pfc_not_asserted_unify(P) :- pfc_warn_local(P), !, \+ db_uclause(P,true).
pfc_not_asserted_unify(P) :- \+ mpred_uclause(P,true).

db_not_asserted_unify((Head:-Tail)) :-  !, \+ db_uclause(Head,Tail). 
db_not_asserted_unify(P) :- !, \+ db_uclause(P,true).

pfc_retractall(X):-pfc_warn_local(X),!,db_retractall(X).
pfc_retractall(X):-pfc_mpred_op(retract(all),X).
pfc_retract(X):- pfc_warn_local(X),!,db_retract(X).
pfc_retract(X):-pfc_mpred_op(retract(one),X).
pfc_assertz(X):-pfc_warn_local(X),!,db_assertz(X).
pfc_assertz(X):-pfc_mpred_op(assert(z),X).
pfc_asserta(X):-pfc_warn_local(X),!,db_asserta(X).
pfc_asserta(X):-pfc_mpred_op(assert(a),X).
pfc_assert(X):-pfc_warn_local(X),!,db_assert(X).
pfc_assert(X):-pfc_mpred_op(assert(z),X).
pfc_call(CALL):-pfc_call(nonPfC,CALL).
pfc_call(Why,(G1)):-var(G1),!,pfc_var_call(Why,G1).
pfc_call(Why,'$VAR'(N)):-!,trace_or_throw(ftVar_pfc_call(Why,'$VAR'(N))).
pfc_call(Why,(G1;G3)):-var(G1),!,(pfc_call(Why,G1)->pfc_call(Why,G2);pfc_call(Why,G3)).
pfc_call(Why,(P=..List)):- !, pfc_univ_call(Why,P,List).
pfc_call(Why,(G1->G2;G3)):- !,(pfc_call(Why,G1)->pfc_call(Why,G2);pfc_call(Why,G3)).
pfc_call(Why,(G1*->G2;G3)):- !,(pfc_call(Why,G1)*->pfc_call(Why,G2);pfc_call(Why,G3)).
pfc_call(Why,(G1->G2)):-!,(pfc_call(Why,G1)->pfc_call(Why,G2)).
pfc_call(Why,(G1*->G2)):-!,(pfc_call(Why,G1)*->pfc_call(Why,G2)).
pfc_call(Why,(G2/G1)):-!,(pfc_call(Why,G1),pfc_call(Why,G2)).
pfc_call(Why,(G1;G2)):-!,pfc_call(Why,G1);pfc_call(Why,G2).
pfc_call(Why,'~'(G1)):-!,(pfc_call(Why,G1)->fail;true).
pfc_call(Why,\+(G1)):-!,(pfc_call(Why,G1)->fail;true).
pfc_call(Why,'not'(G1)):-!,trace,pfc_call(Why,'neg'(G1)).
pfc_call(Why,'call'(G1)):-!,pfc_call(Why,G1).
pfc_call(_Why,CALL):- pfc_warn_local(CALL),!,if_defined(CALL,if_defined((mpred_call((!,fail,trace,CALL))))).
pfc_call(_Why,CALL):- if_defined(CALL,if_defined((mpred_call((!,fail,trace,CALL))))).

pfc_uclause_ref(X,Y,Ref):-pfc_warn_local(X),!,db_uclause_ref(X,Y,Ref).
pfc_uclause_ref(X,Y,Ref):-pfc_mpred_op(mpred_asserted,clause(X,Y,Ref)).

pfc_uclause(X,Y):- pfc_warn_local(X),!,mpred_uclause(X,Y).
pfc_uclause(X,Y):-pfc_mpred_op(mpred_asserted,clause(X,Y)).
mpred_uclause(H,B):-
   (current_predicate(_,H) -> (has_clauses(H) -> db_uclause(H,B) ; B = mpred_call(H)); % simulates a body for system predicates TODO rethink
                                             B = mpred_call(H)).



% HACK FOR NOW
mpred_uclause(X,Y):-db_uclause(X,Y).

pfc_var_call(Why,G):-pfc_fact(G)*->true;pfc_var_call_0(Why,G).
pfc_var_call_0(Why,G):-var(G),!,mpred_arity_var_call(F,A),functor(G,F,A),pfc_call(Why,G).

mpred_arity_var_call(F,A):-arity(F,A).

pfc_univ_call(Why,P,List):-nonvar(List),!,pfc_univ_call_0(Why,P,List).
pfc_univ_call(Why,P,List):-nonvar(P),!,functor(P,F,A),P=..List.
pfc_univ_call(Why,P,List):-mpred_arity_var_call(F,A),functor(P,F,A),P=..List.

pfc_univ_call_0(Why,P,[F|Right]):-var(F),!,mpred_arity_var_call(F,A),functor(P,F,A),P=..[F|Right].
pfc_univ_call_0(Why,P,['$VAR'(F)|Right]):- !, fail, trace_or_throw(ftVar_pfc_univ_call(Why,(P=..['$VAR'(F)|Right]))).
pfc_univ_call_0(Why,P,List):-is_list(List),!,P=..List.
pfc_univ_call_0(Why,P,[F|Right]):- !, F=..[FF|Left],arity(FF,A),functor(P,FF,A),P=..[FF|ALL],append(Left,Right,ALL).


pfc_mpred_op(OP,TERM):-if_defined(mpred_op(OP,TERM),trace_or_throw(pfc_mpred_op(OP,TERM))).


%pfc_compile(_,HB):-pfc_transform_neck(HB,AS),mpred_get(add,HB,How), How==code->pfc_assertz(How);pfc_call(How,AS).
pfc_compile(M,HB):-pfc_xform(load,(M,M),HB,AS),pfc_compile2(HB,AS).
pfc_compile2(_HB,(:-Call)):-!,pfc_call(Call).
pfc_compile2(code,HB):-dtrace,!,pfc_assertz(HB).
pfc_compile2(_HB,HB):-add_from_file(HB).



pfc_transform_neck(HB,O):-mpred_get(neck,HB,Type),hotrace(pfc_transform_neck(HB,Type,O)),(HB\=@=O->pmsg(pfc_transform_neck(HB,Type,O));HB=Type),!.

pfc_transform_neck(HB,=,HB):-!.
pfc_transform_neck(HB,(:-),O):- pfc_get_head_body(HB,H,B),mpred_reduce_hb(((H:-B)),O).
pfc_transform_neck(HB,[],HB):-!.
pfc_transform_neck(HB,[P|List],O):- is_list(List),!,pfc_transform_neck(HB,P,O1),pfc_transform_neck(O1,List,O).
pfc_transform_neck(HB,Pred,HB):- atom(Pred),HB=..[Pred|_],!.
pfc_transform_neck(HB,pfc_state,HB).
pfc_transform_neck((H:-B),pfc_is_fwc,(B=>H)).
pfc_transform_neck((H:-B),pfc_is_bc,(H<=B)).
pfc_transform_neck(HB,(<=),(HB<=true)).
pfc_transform_neck(HB,(=>),(true=>HB)).
pfc_transform_neck(HB,call(Pred),O):- !,call(HB,O),!.
pfc_transform_neck(HB,Pred,O):- current_predicate(Pred/2),call(HB,O),!.
pfc_transform_neck(HB,TYPE,O):- mpred_db(neck,TYPE,Type),TYPE\=@=Type, loop_check(pfc_transform_neck(HB,Type,O)).
pfc_transform_neck((H:-B),Pred,O):- atom(Pred),!,O=..[Pred,H,B].
pfc_transform_neck(HB,_,HB).

pfc_preferedDir(H,B,(B=>H)):-mpred_prop(H,pfc_is_fc).
pfc_preferedDir(H,B,(H<=B)):-mpred_prop(H,pfc_is_bc).
pfc_preferedDir(H,B,(H:-B)).

pfc_prefered_neck(H,FB):-pfc_prefered_neck_fb(H,FB)*->true;mpred_get(neck,H,FB).
pfc_prefered_neck_fb(H,(<=)):- once(db_uclause(H,pfc_bc_only(H));db_uclause(H,infoF(H <= _));db_uclause(H <= _,true)).
pfc_prefered_neck_fb(H,(=>)):- once(db_uclause(H,infoF(_ => H));db_uclause(_ => H,true)).
pfc_prefered_neck_fb(H,(:-)):- once(db_uclause(H,infoF(H:-_));(db_uclause(H,B),\+is_true(B),\+pfc_is_info(B))).




pfc_is_neck_other(H,Type) :- pfc_prefered_neck(H,T),T\=Type,!.

pfc_set_neck(H,Type):- mpred_guess_functor(H,F,A), \+ current_predicate(F/A), decl_mpred(neck,H,Type).
pfc_set_neck(H,Type):- \+ pfc_is_neck_other(H,Type), !, decl_mpred(neck,H,Type).
pfc_set_neck(H,Type):- 
  findall((H:-B),(pfc_uclause_ref(H,B,_),B\==fail),ToAssert),
  must(mpred_home_module(H,M);M=user),!,
  forall(member((H:-B),ToAssert),pfc_rem2(H)),
  decl_mpred(H,neck(Type)),
  forall(member(HB,ToAssert),pfc_compile(M,HB)).


% todo record the module mpred_home_module(F/A,M).
% asserted_pfc_assume_dynamic(G) => {pfc_assume_dynamic(G)}.


pfc_assume_dynamic(G) :- get_functor(G,F,A),pfc_assume_dynamic(G,F/A).

pfc_assume_dynamic(G,F/A):-predicate_property(G,dynamic),mpred_home_module(G,M),!,export(M:F/A),asserta_if_new(user:arity(F,A)),decl_mpred_home_module(G,M).
pfc_assume_dynamic(G,F/A):-predicate_property(G,_),mpred_home_module(G,M),!,export(M:F/A),asserta_if_new(user:arity(F,A)),decl_mpred_home_module(G,M).
pfc_assume_dynamic(G,F/A):-show_call(dynamic_safe(F/A)),mpred_home_module(G,M),!,export(M:F/A),asserta_if_new(user:arity(F,A)),decl_mpred_home_module(G,M).

pfc_dynamic_safe(M,F,A):- functor(C,F,A),predicate_property(C,imported_from(system)),!,pmsg(warn(predicate_property(M:C,imported_from(system)))).
pfc_dynamic_safe(M,F,A):- (static_predicate(M,F,A) -> (pmsg(warn(not(M:dynamic(M:F/A))))) ; logOnErrorIgnore(M:dynamic(M:F/A))). % , warn_module_dupes(M,F,A).

% ===========================================================================
% Change other settings 
% ===========================================================================

:-thread_local pfc_slow_search/0.

% ===========================================================================
% reduction 
% ===========================================================================

% maybe comment out
pfc_into_mpred(Type,B,A):- must(loop_check(call((if_defined(into_mpred_form(Type,B,A),expand_goal(B,A)))),A=B)),!.

% maybe uncomment (should never expand here. if it does we issue a warning)
% pfc_warn_expand(_,B,A):- 0 is random(2),!,B=A.
pfc_warn_expand(Type,B,A):-pfc_into_mpred(Type,B,A),(B\=@=A->pmsg(error(pfc_warn_expand(Type,B,A)));true),!.


has_numvars(V):-compound(V), (V='$VAR'(_);(arg(_,V,E),has_numvars(E))),!.
must_no_numvars(V):-must(\+has_numvars(V)).
must_numvars(P):- must(ground(P);has_numvars(P)),!.

pfc_loop_check(G):-no_repeats(loop_check(G)).
% :-abolish(system:not/1),abolish(not/1),dynamic(not/1),db_assert((not(P):- pfc_is_bound(P), \+ P)).




%add(X):-pfc_add(X).
%rem(X):-pfc_rem1(X).

:-multifile mpred_hooks:provide_canonical_forms/3.
:-dynamic mpred_hooks:provide_canonical_forms/3.
:-export(mpred_hooks:provide_canonical_forms/3).

mpred_hooks:provide_canonical_forms(How,(~({X})),O):-pfc_is_bound(X),wrap_in_neg_functor(How,X,O).
mpred_hooks:provide_canonical_forms(How,(-({X})),O):-pfc_is_bound(X),wrap_in_neg_functor(How,X,O).
mpred_hooks:provide_canonical_forms(How,(not({X})),O):-pfc_is_bound(X),wrap_in_neg_functor(How,X,O).
mpred_hooks:provide_canonical_forms(How,(notz({X})),O):-pfc_is_bound(X),wrap_in_neg_functor(How,X,O).
mpred_hooks:provide_canonical_forms(How,(assertable_not({X})),O):-pfc_is_bound(X),wrap_in_neg_functor(How,X,O).
mpred_hooks:provide_canonical_forms(How,(\+({X})),O):-pfc_is_bound(X),wrap_in_neg_functor(How,X,O).

wrap_in_neg_functor(db_uclause,X,neg(X)).
wrap_in_neg_functor(mpred,X,not(X)).
wrap_in_neg_functor(callable,X, (\+(X))).

pfc_head_expansion(_,V,V ):-pfc_is_unbound(V),!.
pfc_head_expansion(How,G,GG):-mpred_hooks:provide_canonical_forms(How,G,GG),!.
pfc_head_expansion(_,V,V).

% =================================================
% Run tiggers automatically
% =================================================
:-export(pfc_expectedClauseCount_db/3). 
:-dynamic(pfc_expectedClauseCount_db/3). 
pfc_getExpectedClauseCount(F,A,C):- (pfc_expectedClauseCount_db(F,A,C);C=0).
pfc_getActualClauseCount(F,A,C):-make_functor(P,F,A),mpred_ccount(P,C).
pfcIsClauseCountWrong(F,A):-pfc_getExpectedClauseCount(F,A,E),pfc_getActualClauseCount(F,A,C),!,C\=E.

% pfc_checkClauseCounts =>{ forall((mpred_prop(F/A,pfc_is_watching),~mpred_prop(F/A,pfc_control)),pfc_updateClauses(F,A)) }.


pfc_updateClauses(F,A):- \+(pfcIsClauseCountWrong(F,A)),!.
pfc_updateClauses(F,A):-make_functor(P,F,A),forall((db_uclause(P,T),is_true(T)),pfc_asserta(P)),db_retractall(pfc_expectedClauseCount_db(F,A,_)),
   mpred_ccount(P,C),
   asserta(pfc_expectedClauseCount_db(F,A,C)).

:-if(current_predicate(onEachLoad/1)).
:-onEachLoad(pfc_checkClauseCounts).
:-endif.


:- export(pfc_maptree/3).
:- module_transparent(pfc_maptree/3).
pfc_maptree(_,Ex,_) :- Ex == [],!.
pfc_maptree(Pred,H,S):-pfc_is_unbound(H),!,apply(Pred,[H|S]).
pfc_maptree(Pred,[H|T],S):-!, apply(Pred,[H|S]), pfc_maptree(Pred,T,S).
pfc_maptree(Pred,(H,T),S):-!, apply(Pred,[H|S]), pfc_maptree(Pred,T,S).
pfc_maptree(Pred,H,S):-apply(Pred,[H|S]).


:- export(pfc_lambda/3).
:- export(pfc_lambda/4).
:- export(pfc_lambda/5).
:- export(pfc_lambda/6).
:- module_transparent(pfc_lambda/3).
:- module_transparent(pfc_lambda/4).
:- module_transparent(pfc_lambda/5).
:- module_transparent(pfc_lambda/6).
pfc_lambda([A1],Body,A1):-Body.
pfc_lambda([A1,A2],Body,A1,A2):-Body.
pfc_lambda([A1,A2,A3],Body,A1,A2,A3):-Body.
pfc_lambda([A1,A2,A3,A4],Body,A1,A2,A3,A4):-Body.
% :-call(pfc_lambda([E],writeln(E)),hello_lambda).


deny_pfc_Permission_to_remove(pfcInternal,_,_):-!,fail. %allow
deny_pfc_Permission_to_remove(_,P,not(pfc_control)):-get_functor(P,F,A), \+(pfc_local(P,F,A);mpred_prop(F,pfc_control)).

pfc_pre_expansion_each(A,B):- (pfc_pre_expansion_each0(A,B)).
pfc_pre_expansion_each0(X,X):- \+ compound(X),!.
%HOOK pfc_pre_expansion_each0(X,X):-if_defined(as_is_term(X)),!.
%HOOK pfc_pre_expansion_each0(X,mpred_prop(I,C)):- if_defined(was_isa(X,I,C)),!,( \+ \+ maybe_hybrid(C/1)).
pfc_pre_expansion_each0(Sent,OUT):-Sent=..[And|C12],if_defined(is_logical_functor(And)),!,maplist(pfc_pre_expansion_each0,C12,O12),OUT=..[And|O12],!.
pfc_pre_expansion_each0(C12,OUT):-is_list(C12),!,maplist(pfc_pre_expansion_each0,C12,OUT),!.
pfc_pre_expansion_each0(X,X):-!.
pfc_pre_expansion_each0(X,X):- \+ \+ must(pfc_warn_local(X)),!.



% by returning true we veto the assertion  (fail accepts assertion)
pfc_throw_on_bad_fact(G):- call((pfc_is_bad_fact(G,Why), pmsg(((pfc_throw_on_bad_fact(Why,G)))),!,fail)).

pfc_is_bad_fact(G,singletons(HS)):-   
  pfc_head_singletons_g(G,HS),get_functor(HS,F),!,\+(mpred_prop(F,predCanHaveSingletons)),fail.

pfc_head_singletons_g(G,HS):- pfc_get_head_body(G,H,B), pfc_head_singletons_hb(H,B,HS),!.
  
pfc_head_singletons_hb(HN,BN,H):- unnumbervars(HN:BN,H:B),
  term_variables(H,HV),
  numbervars((H:B),66,_,[singletons(true)]),!,
    member('$VAR'('_'),HV).

pfc_manages_unknowns:-!.
pfc_manages_unknowns :- mpred_prop(*,pfc_control).



% pfc_file('thlocal').	% predicates to manipulate database.


%   File   : thlocal.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Author :  Dan Corpron
%   Updated: 10/11/87, ...
%   Purpose: predicates to manipulate a pfc database (e.g. save,
%=	restore, reset, etc.0 )

% pfc_state(P/A) is true iff P/A is something that pfc adds to
% the database and should not be present in an empty pfc database


% :-decl_mpred(pfc_state/1,pfc_col).

pfc_state(spft/3).
pfc_state(pfc_t_pkey/3).
pfc_state(pt/3).
pfc_state(bt/2).
pfc_state(nt/3).
pfc_state('=>'/2).
pfc_state('<=>'/2).
pfc_state('<='/2).
pfc_state(pfc_queue/2).
% CANT BE HERE OR IT DISABLED FWD CHAINING pfc DatabaseTerm(pfc Default/1).

:-forall(pfc_state(F/A),((dynamic(F/A),export(F/A)))).

% pfc_local/1 is used to indicate that pfc local database is where the predicates are asserted
pfc_local(G):- call((get_functor(G,F,A),pfc_local(G,F,A))).
pfc_local(_,F,A):-pfc_state(F/A),!.
pfc_local(_,F,_):-mpred_prop(F,code),!.
pfc_local(_,F,_):-mpred_prop(F,pfc_is_notlocal),!,fail.
pfc_local(_,F,_):-mpred_prop(F,pfc_dont_expand).
pfc_local(G,_,_):-pfc_manages_unknowns,!,pfc_mark_W(G),!,pfc_mark_C(G).
pfc_local(G,_,_):-  \+(current_predicate(mpred_assertz/1)),!,pfc_mark_C(G).
pfc_local(_,_,_).

pfc_add_minfo(How,(H:-True)):-is_true(True),must(pfc_is_bound(H)),!,pfc_add_minfo(How,H).
pfc_add_minfo(How,(H<=B)):- !,pfc_add_minfo(How,(H:-infoF(H<=B))),!,pfc_add_minfo(How,(H:-pfc_bc_only(H))),pfc_add_minfo_2(How,(B:-infoF(H<=B))).
pfc_add_minfo(How,(B=>H)):- !,pfc_add_minfo(How,(H:-infoF(B=>H))),!,pfc_add_minfo_2(How,(B:-infoF(B=>H))).
pfc_add_minfo(How,(B<=>H)):- !,pfc_add_minfo(How,(H:-infoF(B<=>H))),!,pfc_add_minfo(How,(B:-infoF(B<=>H))),!.
pfc_add_minfo(How,((A,B):-INFOC)):-pfc_is_info(INFOC),(pfc_is_bound(A);pfc_is_bound(B)),!,pfc_add_minfo(How,((A):-INFOC)),pfc_add_minfo(How,((B):-INFOC)),!.
pfc_add_minfo(How,((A;B):-INFOC)):-pfc_is_info(INFOC),(pfc_is_bound(A);pfc_is_bound(B)),!,pfc_add_minfo(How,((A):-INFOC)),pfc_add_minfo(How,((B):-INFOC)),!.
pfc_add_minfo(How,(~(A):-infoF(C))):-pfc_is_bound(C),pfc_is_bound(A),!,pfc_add_minfo(How,((A):-infoF((C)))). % call(How,(~(A):-infoF(C))).
pfc_add_minfo(How,(neg(A):-infoF(C))):-pfc_is_bound(C),pfc_is_bound(A),!,pfc_add_minfo(How,((A):-infoF((C)))). % call(How,(~(A):-infoF(C))).
pfc_add_minfo(How,(A:-INFOC)):-nonvar(INFOC),INFOC= pfc_bc_only(A),!,call(How,(A:-INFOC)),!.
pfc_add_minfo(How,(A:-INFOC)):-pfc_is_info(INFOC),!,pfc_rewrap_h(A,AA),call(How,(AA:-INFOC)),!.
pfc_add_minfo(How,bt(H,_)):-!,call(How,(H:-pfc_bc_only(H))).
pfc_add_minfo(How,nt(H,Test,Body)):-!,call(How,(H:-fail,nt(H,Test,Body))).
pfc_add_minfo(How,pt(H,Body)):-!,call(How,(H:-fail,pt(H,Body))).
%pfc_add_minfo(How,G):-pmsg(skipped_add_meta_facts(How,G)).
pfc_add_minfo(_,_).

:-export(pfc_add_minfo_2/2).
pfc_add_minfo_2(How,G):-pfc_add_minfo(How,G).

pfc_is_info(pfc_bc_only(C)):-pfc_is_bound(C),!.
pfc_is_info(infoF(C)):-pfc_is_bound(C),!.

{}(X):-pfc_call(X).


:-export(pfc_wont_assert/1).
pfc_wont_assert(F/A):-number(A),!,make_functor(H,F,A),pfc_wont_assert(H).
pfc_wont_assert(A):-current_predicate(_,A), \+ predicate_property(A,dynamic).
:-dynamic(pfc_info_head/1).
pfc_rewrap_h(A,F):-pfc_is_bound(A),\+ pfc_wont_assert(A), functor(A,F,_),!.
% pfc_rewrap_h(A,F):-pfc_is_bound(A),\+ pfc_wont_assert(A), functor(A,F,N),functor(AA,F,N),!,numbervars(AA,0,_,[singletons(false),functor_name('*')]).
pfc_rewrap_h(A,pfc_info_head(A)):-!.

% used to annotate a predciate to indicate PFC support
infoF(_):-fail.

pfc_into_prolog(G,GGG):-must((pfc_warn_expand(pfc_into_prolog,G,GG))),!,unnumbervars(GG,GGG).



:-thread_local ntd_max_depth/2.

pfc_not_too_deep(_,G):- skipWrapper,!,G.
pfc_not_too_deep(Key,G):-stack_depth(CD),
  (ntd_max_depth(Key,MD)->
      ( (CD > MD) -> (!,fail) ; G) ; 
    (MD is CD+200,call_cleanup(asserta(ntd_max_depth(Key,MD),REF),G,erase(REF)))).

% :- set_prolog_flag(unknown,fail).



pfc_get_head(Head,HeadO):-pfc_get_head_body_0(Head,HeadO,_),!.

pfc_get_head_body(Head,HeadO,BodyO):-pfc_get_head_body_0(Head,HeadO,Body),!,pfc_get_head(Body,BodyO).

pfc_get_head_body_0(Head,HeadO,true):-pfc_is_unbound(Head),!,HeadO=Head.
pfc_get_head_body_0((Head1,Head2),HeadO,BodyO):-!,
   pfc_get_head_body(Head1,Head1O,Body1),
   pfc_get_head_body(Head2,Head2O,Body2),
   pfc_conjoin(Head1O,Head2O,HeadO),
   pfc_conjoin(Body1,Body2,BodyO).
pfc_get_head_body_0((BM => Head),HeadO,BodyO):-nonvar(BM),BM=(Body/Precond),!,pfc_get_head_body((Body => (Head/Precond)),HeadO,BodyO).
pfc_get_head_body_0((Head <= BM),HeadO,BodyO):-nonvar(BM),BM=(Body/Precond),!,pfc_get_head_body(((Head/Precond) <= Body),HeadO,BodyO).
pfc_get_head_body_0('=>'(Head),HeadO,Body2):-!,pfc_get_head_body(Head,HeadO,Body2).
pfc_get_head_body_0('=>'(Body1,Head),HeadO,(Body1,Body2)):-!,pfc_get_head_body(Head,HeadO,Body2).
pfc_get_head_body_0((Head<=Body1),HeadO,(Body1,Body2)):-!,pfc_get_head_body(Head,HeadO,Body2).
pfc_get_head_body_0((L<=>R),HeadO,Body):-!,pfc_get_head_body(((L<=R),(R<=L)),HeadO,Body).
pfc_get_head_body_0(_::::Head,HeadO,Body2):-!,pfc_get_head_body_0(Head,HeadO,Body2).
pfc_get_head_body_0(bt(Head,Body1),HeadO,(Body1,Body2)):-!,pfc_get_head_body(Head,HeadO,Body2).
pfc_get_head_body_0(pt(Body1,Head),HeadO,(Body1,Body2)):-!,pfc_get_head_body(Head,HeadO,Body2).
pfc_get_head_body_0(pfc_t_pkey(_,Head,Body1b),HeadO,(Body1b,Body2)):-!,pfc_get_head_body(Head,HeadO,Body2).
pfc_get_head_body_0(nt(_,Head,Body1b),HeadO,(Body1b,Body2)):-!,pfc_get_head_body(Head,HeadO,Body2).
pfc_get_head_body_0(spft(Head,Body1a,Body1b),HeadO,(Body1a,Body1b,Body2)):-!,pfc_get_head_body(Head,HeadO,Body2).
pfc_get_head_body_0(pfc_queue(Head,S),HeadO,Body2):-!,pfc_get_head_body(Head,HeadO,Body2).
pfc_get_head_body_0('~'(Head),'~'(HeadO),Body):-!,pfc_get_head_body(Head,HeadO,Body).
pfc_get_head_body_0('neg'(Head),'neg'(HeadO),Body):-!,pfc_get_head_body(Head,HeadO,Body).
pfc_get_head_body_0('not'(Head),'not'(HeadO),Body):-!,pfc_get_head_body(Head,HeadO,Body).
pfc_get_head_body_0((Head:-Body1),HeadO,(Body1,Body2)):-!,pfc_get_head_body(Head,HeadO,Body2).
pfc_get_head_body_0((Head:-Body),Head,Body):-!.
pfc_get_head_body_0(Head,Head,true).


pfcVersion(1.2).

% pfc_file('pfcsyntax').	% operator declarations.

%   File   : pfcsyntax.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Purpose: syntactic sugar for Pfc - operator definitions and term expansions.

/*
pfc_f_expand(M,':-'(_),_):-!,fail.
pfc_f_expand(M,(=>P),(:- pfc_mark_F(P),pfc_add((P)))):-pfc_is_bound(P).
pfc_f_expand(M,(P=>Q),(:- pfc_mark_F(Q),pfc_add((P=>Q)))).
%pfc_f_expand(M,(P=>Q),(:- pfc_add(('<='(Q,P))))).  % DO NOT USE speed-up attempt
pfc_f_expand(M,('<='(P,Q)),(:- pfc_mark_B(P),pfc_add(('<='(P,Q))))).
pfc_f_expand(M,(P<=>Q),(:- pfc_mark_C(P),pfc_mark_C(Q),pfc_add((P<=>Q)))).
pfc_f_expand(M,(RuleName :::: Rule),(:- pfc_add((RuleName :::: Rule)))).
pfc_f_expand(M,'fwc'((Q)),(:- pfc_mark_F(Q),pfc_add(=>Q))):-pfc_is_bound(Q).

pfc_f_expand(M,((Q:-P)),(:- (pfc_mark_F(Q),pfc_mark_C(P=>Q),pfc_add(P=>Q)))):- mpred_prop(Q,pfc_is_fc),!.

pfc_f_expand(M,((Q:-P)),(:- (pfc_mark_F(Q),pfc_mark_C(P=>Q),pfc_add(P=>Q)))):- pfc_is_bound(P),P\==true,pfc_is_bound(Q),is_fc_body(P),!.
pfc_f_expand(M,((Q:-P)),(:- (pfc_mark_B(Q),pfc_mark_C(Q),pfc_add(Q<=P)))):- pfc_is_bound(P),P\==true,pfc_is_bound(Q),is_bc_body(P),!.

%pfc_f_expand(M,((Q:-P)),(:- pfc_mark_B(Q),pfc_add(Q<=P))):- pfc_is_bound(P),pfc_is_bound(Q),P\==true,\+(is_fc_body(P)),pfc_control(Q),!.
pfc_f_expand(M,P,(:- pfc_add(P))):- mpred_prop(P,pfc_is_fc),!.
pfc_f_expand(M,P,(:- pfc_add(P))):- mpred_prop(P,pfc_control),!.
pfc_f_expand(M,P,O):- pfc_transform_neck(P,H), H \=@= P, must(pfc_f_expand(M,H,O)).
pfc_f_expand(M,P,(:- pfc_add(P))):- pfc_neg_holder(P),!.
pfc_f_expand(M,B,A):- pfc_f_expand2(M,B,A).
pfc_f_expand2(M,P,(:- CALL)):- transitive(mpred_get(load),P,How),P\=@=How,How\==(=),atom(How),current_predicate(How/1),CALL=..[How,P],!.
pfc_f_expand2(M,B,A):- source_user(U),pfc_xform_add((U,U),B,BA),B\=@=BA,pfc_f_expand(M,BA,A),!.
*/

may_never_deduce_bc_change.

may_use_head(_):-may_never_deduce_bc_change,!,fail.
may_use_head(Q):-pfc_is_unbound(Q),!,fail.
may_use_head(_:_):-!,fail.
may_use_head(Q):-Q \= (F/A),!, get_functor(Q,F,A),!,may_use_head(F/A).
% may_use_head(F/_):- atom_contains(F,'_'),!,fail.
may_use_head(F/_):- mpred_prop(F,pfc_dont_expand),!,fail.
may_use_head(F/_):- current_predicate(F/A),make_functor(G,F,A),real_builtin_predicate(G),!,fail.
may_use_head(F/A):- make_functor(G,F,A),real_builtin_predicate(G),!,fail.
may_use_head(_/1):-!,fail.
may_use_head(_/2).
% may_use_head(_/_).


pfc_addPreTermExpansion((I1,I2)):-!,pfc_addPreTermExpansion(I1),pfc_addPreTermExpansion(I2).
pfc_addPreTermExpansion(Info):-pfc_f_expand(M,Info,What),!,What=(:-Call),show_call(Call).


pfc_xform_add(S,P,O):- pfc_xform_add0(S,P,O),!.
%pfc_xform_add(_,(H:-B),(H<=B)) :- head_singletons_hb(H,B,_),!,pmsg("adding pfc_bC instead of Neck ~q",[pfc_add1(H<=B)]).
%pfc_xform_add(_,(B=>H),(H<=B)) :-head_singletons_hb(H,B,_),!,pmsg("adding pfc_bC instead of pfc_fWC ~q",[pfc_add1(H<=B)]).
pfc_xform_add(S,(=>(P)),P):- pfc_is_bound(P).
pfc_xform_add(S,((P<= TRUE)),P):- pfc_is_bound(P),is_true(TRUE).
pfc_xform_add(_,(H:-B),(G)) :-pfc_preferedDir(H,B,G),G\=(:-(_,_)),pmsg(error("adding ~q",[pfc_preferedDir(H,B,G)])),!.
pfc_xform_add(S,P,P):- pfc_is_already_false(P,S,_),!.
pfc_xform_add(S,P,O):- pfc_xform_add_args(S,P,O),!.


pfc_xform_add0(S,P,P):- (\+ \+ P='$VAR'(_)),!.
pfc_xform_add0(S,P,P):- pfc_is_unbound(P),!.
pfc_xform_add0(S,P,O):- user:ruleRewrite(P,O),!.
                       
pfc_xform_add_args(S,P,P):- pfc_is_unbound(P),!.
pfc_xform_add_args(S,P,O):-pfc_xform_add0(S,P,M).
pfc_xform_add_args(_,pfc_literal,pfc_literal):-!.
pfc_xform_add_args(_,holds,h):-!.
pfc_xform_add_args(_,holds,pfc_t):-!.
pfc_xform_add_args(_,pfc_HaltSignal(X),pfc_haltSignal(X)):-!.
pfc_xform_add_args(_,add,pfc_add):-!.
pfc_xform_add_args(_,rem,pfc_rem):-!.
%pfc_xform_add_args(_,-,~):-!.
pfc_xform_add_args(_,not,neg):-!.
pfc_xform_add_args(S,P,P):- atomic(P),!.
pfc_xform_add_args(_Type,B,A):- catch(expand_goal(B,A),_,fail),B\=@=A,!.
pfc_xform_add_args(Type,B,A):- ((compound(B),B=..[F|Args],maplist(transitive(pfc_xform_add_args(Type)),[F|Args],OUT),A=..OUT)),B\=A.
pfc_xform_add_args(S,P,P0):- pfc_slow_search, pfc_warn_expand(change(db_assert,pfc_xform_add),P,P0), P0\=@=P.
pfc_xform_add_args(_,P,P).

:-export(pfc_xform_lc/4).
pfc_xform_lc(A,B,C,D):-loop_check(pfc_xform(A,B,C,D),C=D).

:-export(pfc_xform/4).
pfc_xform(A,B,C,D):-(var(A);var(B)),trace_or_throw(var_pfc_xform(A,B,C,D)),!,fail.

pfc_xform(T,S,P,NV):-nonvar(NV),!,pfc_xform(T,S,P,V),!,must(V=VV).
pfc_xform(callable,S,P,P):-pfc_is_unbound(P),!.
pfc_xform(assertable,S,P,P):-pfc_is_unbound(P),!.
pfc_xform(load,S,P,P):-pfc_is_unbound(P),!.
pfc_xform(argsof(_),S,P,P):-pfc_is_unbound(P),!.
pfc_xform(arg,S,P,P):-pfc_is_unbound(P),!.
pfc_xform(_,_,P,P):-atom(P),!.
pfc_xform(_,S,P,P):-pfc_is_unbound(P),!.
pfc_xform(_,S,(Q:-P),(Q:-P)):- is_code_body(P),!.
pfc_xform(functorof(_,_),S,P,h(P)):-pfc_is_unbound(P),!.
pfc_xform(argsof(_),S,P,O):-pfc_xform_lc(arg,S,P,O).
pfc_xform(_,S,P,P):-pfc_is_unbound(P),!.

pfc_xform(FMT,S,B,A):- nonvar(B),pfc_to_negation(Type,BB,B),functor(B,Neg,_),pfc_xform_lc(Type,S,BB,BBB),(atom(Neg)->A=..[Neg,BBB];pfc_to_negation(FMT,BBB,A)),!.


pfc_xform(load,S,P,(  user:pfc_add(P,S))):-pfc_is_unbound(P),!.
pfc_xform(FMT,S,P,O):- user:ruleRewrite(P,PO),P\=@=PO,pfc_xform_lc(FMT,S,PO,O).
pfc_xform(load,S,(=>P),(   user:pfc_mark_F(P),user:pfc_add(P,S))):-pfc_is_bound(P).
pfc_xform(load,S,(P=>Q),(   user:pfc_mark_F(Q),user:pfc_add((P=>Q),S))).
%pfc_xform_lc(load,S,(P=>Q),(   user:pfc_add(('<='(Q,P),S)))).  % DO NOT USE speed-up attemptpfc_xform(load,S,('<='(P,Q)),(   user:pfc_mark_B(P),user:pfc_add(S,('<='(P,Q),S)))).
pfc_xform(load,S,(P<=>Q),(   user:pfc_mark_C(P),user:pfc_mark_C(Q),user:pfc_add((P<=>Q),S))).
pfc_xform(load,S,(RuleName :::: Rule),(   user:pfc_add((RuleName :::: Rule),S))).
pfc_xform(load,S,'fwc'((Q)),(   user:pfc_mark_F(Q),user:pfc_add(=>Q,S))):-pfc_is_bound(Q).
pfc_xform(load,S,((Q:-P)),(   (user:pfc_mark_F(Q),user:pfc_mark_C(P=>Q),user:pfc_add(P=>Q,S)))):- mpred_prop(Q,pfc_is_fc),!.
pfc_xform(load,S,((Q:-P)),(   (user:pfc_mark_F(Q),user:pfc_mark_C(P=>Q),user:pfc_add(P=>Q,S)))):- pfc_is_bound(P),P\==true,pfc_is_bound(Q),is_fc_body(P),!.
pfc_xform(load,S,((Q:-P)),(   (user:pfc_mark_B(Q),user:pfc_mark_C(Q),user:pfc_add(Q<=P,S)))):- pfc_is_bound(P),P\==true,pfc_is_bound(Q),is_bc_body(P),!.

%pfc_xform_lc(load,S,((Q:-P)),(   user:pfc_mark_B(Q),user:pfc_add(Q<=P,S))):- pfc_is_bound(P),pfc_is_bound(Q),P\==true,\+(is_fc_body(P)),pfc_control(Q),!.
pfc_xform(load,S,P,(   user:pfc_add(P,S))):- mpred_prop(P,pfc_is_fc),!.
pfc_xform(load,S,P,(   user:pfc_add(P,S))):- mpred_prop(P,pfc_control),!.
pfc_xform(load,S,P,O):- loop_check(pfc_transform_neck(P,H)), H \=@= P, must(pfc_xform_lc(load,S,H,O)).
pfc_xform(load,S,(P1,P2),O):- !,pfc_xform_lc(load,S,P1,O1),pfc_xform_lc(load,S,P2,O2),pfc_conjoin(O1,O2,O).
pfc_xform(load,S,P,(   user:pfc_add(P,S))):- pfc_neg_holder(P),!.
pfc_xform(load,S,P,(:- user:CALL )):- transitive(mpred_get(load),P,How),must(P\=@=How),How\==(=),atom(How),current_predicate(How/1),CALL=..[How,P],!.
pfc_xform(load,S,B,A):- pfc_xform_lc(load,S,B,BA), (B\=@=BA -> pfc_xform_lc(load,S,BA,A) ; (A = B)).

% ?- pfc_xform(argsof(assertable(assertable)), (pfc, pfc), concat_atom([result, _G5388], '', _G5400), _G10127) 

%pfc_xformc(FMT,S,(H:-B),(H<=B)) :- head_singletons_hb(H,B,_),!,pmsg("adding pfc_bC instead of Neck ~q",[pfc_add1(H<=B)]).
%pfc_xformc(FMT,S,(B=>H),(H<=B)) :-head_singletons_hb(H,B,_),!,pmsg("adding pfc_bC instead of pfc_fWC ~q",[pfc_add1(H<=B)]).
pfc_xform(FMT,S,(P1,P2),O):- !,pfc_xform_lc(FMT,S,P1,O1),pfc_xform_lc(FMT,S,P2,O2),pfc_conjoin(O1,O2,O).
pfc_xform(FMT,S,(P1;P2),O):- !,pfc_xform_lc(FMT,S,P1,O1),pfc_xform_lc(FMT,S,P2,O2),pfc_op_join((';'),O1,O2,O).
pfc_xform(FMT,S,(=>(P)),O):- pfc_is_bound(P),!,pfc_xform_lc(FMT,S,P,O).
pfc_xform(FMT,S,(TRUE=>P),O):- is_true(TRUE),pfc_is_bound(P),!,pfc_xform_lc(FMT,S,P,O).
pfc_xform(FMT,S,(P<=TRUE),O):- is_true(TRUE),pfc_is_bound(P),!,pfc_xform_lc(FMT,S,P,O).
pfc_xform(FMT,S,(P:-TRUE),O):- is_true(TRUE),pfc_is_bound(P),!,pfc_xform_lc(FMT,S,P,O).
pfc_xform(_,S,ISA,isa(I,C)):- was_isa(ISA,I,C).
pfc_xform(assertable,S,P,P):- pfc_is_already_false(P,S,Why),!,pmsg("adding a FALSE ~q",[add(P,S,Why)]).
                
pfc_xform(FMT,S,P,P):- atomic(P),!.
pfc_xform(FMT,S,(=>(P)),O):- pfc_is_bound(P),!,pfc_xform_lc(FMT,S,P,O).
pfc_xform(FMT,S,(P=>Q),(PP=>QQ)):- pfc_xform_lc(askable(FMT),S,P,PP),pfc_xform_lc(assertable(FMT),S,Q,QQ).
pfc_xform(FMT,S,(Q<=P),(Q<=P)):- pfc_xform_lc(askable(FMT),S,P,PP),pfc_xform_lc(assertable(FMT),S,Q,QQ).
pfc_xform(FMT,S,(Q<=>P),O):- pfc_xform_lc(FMT,S,((Q=>P),(P=>Q)),O).
pfc_xform(FMT,S,(H:-B),(GO)) :-loop_check(pfc_preferedDir(H,B,G)),G\=(:-(_,_)),pmsg(error("adding ~q",[pfc_preferedDir(H,B,G)])),!,pfc_xform_lc(FMT,S,(G),(GO)).
pfc_xform(FMT,S,(Q:-P),(QQ:-PP)):- pfc_xform_lc(callable(FMT),S,P,PP),pfc_xform_lc(assertable(FMT),S,Q,QQ).
pfc_xform(FMT,S,B,A):- loop_check(catch(expand_goal(B,A),_,fail)),B\=@=AB,!,pfc_xform_lc(FMT,S,AB,A).
pfc_xform(FMT,S,B,A):- compound(B),must(pfc_xform_c(FMT,S,B,A)).

pfc_xform_c(FMT,S,B,A):- 
       B=..[F|Args], 
       must(pfc_xform_lc(functorof(FMT,Args),S,F,FO)),
       must(maplist(user:pfc_xform_lc(argsof(FMT),S),Args,OUT)),
       must(atom(FO)-> (must(A=..[FO|OUT]),!);((must((FO =.. TFA, append(TFA,OUT,NOUT),A=..NOUT))))).


pfc_is_already_false(P,S,Why):-call((pfc_is_bound(P),pfc_negate_for_add(P,N),P\=@=N,pfc_get_support(N,Why),!,pmsg(warn(want(P,S),but,N -> Why)))),!,dtrace.


pfc_to_negation(callable,A, \+(A)).
pfc_to_negation(assertable,A, neg(A)).
pfc_to_negation(askable,A, '~'(A)).
pfc_to_negation(holder,A, 'neg'(A)).
pfc_to_negation(askable,A, naf(A)).
pfc_to_negation(holder,A, not(A)).


% pfc_file('pfccore').	% core of Pfc.

%   File   : pfccore.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated: 10/11/87, ...
%            4/2/91 by R. McEntire: added calls to valid_dbref as a
%                                   workaround for the Quintus 3.1
%                                   bug in the recorded database.
%   Purpose: core Pfc predicates.

%= pfc_add/2 and pfc_post/2 are the main ways to db_assert new clauses into the
%= database and have forward reasoning done.

%= pfc_add(P,S) asserts P into the dataBase with support from S.

% pfc_add(/*to_exp*/((:-export number/1))):-trace_or_throw(crazy_pfc_add(/*to_exp*/((:-export number/1)))).
pfc_add(P) :- source_user(U), pfc_add(P,U,U),!.
pfc_add(P,S):- pfc_maptree(pfc_add1,P,[S]),!.
pfc_add(P,F,T):-pfc_add(P,(F,T)),!.

expand_for_add(S,P,O):-pfc_xx_form(assertable,S,P,O).
pfc_xx_form(FMT,S,P,P0):- must(transitive(pfc_xform(FMT,S),P,P0)),!.
% pfc_xx_form(FMT,S,P,P0):- must(loop_check(pfc_xform(FMT,S,P,P0),P=P0)),!.

pfc_add1('=>'(P),S):- nonvar(P),!, pfc_add1(P,S).
pfc_add1(P0,S):- 
     % transitive(pfc_xform_add(S),P,P0), (P=@=P0 -> true; pfc_warn("pfc_xform_add Changed pfc_add1 ~q",[P->P0])),
     transitive(pfc_xx_form(assertable,S),P0,P1), (P0=@=P1 -> true; nop(pfc_warn("pfc_xx_form Changed pfc_add1 ~q",[P0->P1]))),
     pfc_add2(P1,S).


% pfc_add2(P,S):-  once(fully_expand(change(assert,S),P,P0)), P\=@=P0 ,!, pfc_warn("fully_expand Changed pfc_add1 ~q",[P->P0]),pfc_add1(P0,S).

% pfc_add1(P,_):-pfc_data_type(P,T),T==support,!,db_assert(P),!.
% % pfc_add1a(P,S) :- pfc_is_already_false(P,S,_),!.
pfc_add2(P,_):- call(pfc_ignored(P)),!.

pfc_simu_support(P0,U0,U0,O) :- spft(P0,U1,U1),(U0==U1->O=sup_id(P0,U0);O=sup_users(P0,U0,U1)).
pfc_simu_support(P0,F0,T0,sup_ident(P0)) :- spft(P0,S0,T0),S0\=S1.
pfc_simu_support(P0,F0,T0,sup_t(P0,F1,F0)) :- spft(P0,F1,T0),T0\=T1.
pfc_simu_support(P0,F0,T0,sup_f(P0,T1,T0)) :- spft(P0,F0,T1),T0\=T1.
pfc_simu_support(P0,F0,T0,sup_ft(P0,F1,F0,T1,T0)):- spft(P0,F1,T1).

pfc_add2(P0,S):-S=(F0,T0),!,pfc_add1(P0,S,F0,T0).

pfc_add1(P0,F0,T0):-pfc_add1(P0,(F0,T0),F0,T0),!.
pfc_add1(P0,S0,F0,T0):-pfc_simu_support(P0,F0,T0,How),!,nop(pmsg(sim(How))),pfc_add_fast(P0,How,S0,F0,T0).
pfc_add1(P0,S0,F0,T0):-pfc_pre_expansion_each(P0,P1),!,pfc_add_fast(P1,none,S0,F0,T0).

pfc_add_fast(P) :- source_user(U), pfc_add_fast(P,fast,(U,U),U,U).
pfc_add_fast(P,S) :- must(S = (F,T)),pfc_add_fast(P,fast,S,F,T).
pfc_add_fast(pfc_t(F,A,B),How,S0,F0,T0) :- atom(F), P=..[F,A,B],!,pfc_add_fast(P,How,S0,F0,T0).
pfc_add_fast(h(F,A,B),How,S0,F0,T0) :- atom(F), P=..[F,A,B],!,pfc_add_fast(P,How,S0,F0,T0).
pfc_add_fast(P,How,S,FT,FT) :-!,
  must((copy_term(P,P2),
  pfc_post(P2,S),
  %sanity(variant(P,P2)),
  pfc_run)),!.
pfc_add_fast(P,How,S,F,T) :-
  copy_term(P,P2),
  pfc_post(P2,S),
 % sanity(variant(P,P2)),
  pfc_run,!.

%pfc_add1(_,_).
pfc_add_fast(P,How,S,F,T) :- pfc_error("pfc_add(~w,~w) failed",[P,S]),!,fail.


% OLD VERSION
pfc_add_BTRule(P,S):- !, must(pfc_post(P,S)), pfc_run.

% USE UNTIL IT DONT WORK
pfc_add_BTRule(P,S):- !,
  pfc_addSupport(P,S),
  pfc_assert(P).

% MAYBE VERSION
pfc_add_BTRule(P,S):-
  %= db pfc_addDbToHead(P,P2),
  % pfc_removeOldVersion(P),
  sanity(copy_term(P,PC)),
  must((pfc_addSupport(P,S),nop(sanity(PC=@=P)))),
  pfc_not_asserted_unify(P),
  sanity(PC=@=P),
  assertz_if_new(P),
  % pfc_trace_add(P,S),
   !,
  % would never be ran? 
  pfc_enqueue_fc(P,S,breadth),!. 

pfc_add_BTRule(P,S) :- pfc_error("pfc_add(~w,~w) failed",[P,S]),!,fail.



% pfc_post(+Ps,+S) tries to add a hybrid_fact or set of hybrid_fact to the database.  For
% each hybrid_fact (or the singelton) pfc_post1 is called. It always succeeds.

pfc_post(Each,S) :- pfc_maptree(pfc_post1,Each,[S]).

% pfc_post1(+P,+S) tries to add a hybrid_fact to the database, and, if it succeeded,
% adds an entry to the pfc queue for subsequent forward chaining.
% It always succeeds.

:- meta_predicate(pfc_post2(?,*)).
pfc_post1(P,S):- P\=neg(_),pfc_from_negation_plus_holder(P,NP),!,pfc_post1(neg(NP),S).
pfc_post1(P,_):- pfc_ignored(P),!.
pfc_post1(P,S) :- call(unnumbervars(P:S,PP:SS)),expand_for_add(S,PP,PPP),pfc_post2(PPP,SS).
pfc_post2(P,S) :- 
  %= db pfc_addDbToHead(P,P2),
  % pfc_removeOldVersion(P),
  copy_term(P,PC),
  must((pfc_addSupport(P,S),sanity(PC=@=P))),
  pfc_not_asserted_unify(P), % sanity(PC=@=P),  
  must(user:pfc_assert(P)), % was simply db_assert(P),
   get_functor(P,F,A),export(F/A),
   must(pfc_trace_add(P,S)),
   !,
   must(pfc_enqueue_fc(P,S)),
   !.

pfc_post2(P,S) :- (\+ must(\+ pfc_not_asserted_unify(P))),pfc_error("(maybe ERROR?!) pfc_add(~w,~w) failed",[P,S]),!.
pfc_post2(_,_).
pfc_post2(P,S) :-  pfc_error("pfc_add(~w,~w) failed",[P,S]).


pfc_repropagate(P) :-
  forall(must(pfc_get_support(P,S)), pfc_repropagate(P,S)).

pfc_repropagate(P,S) :-
  (\+ \+ must(pfc_assert(P))), % was simply db_assert(P),
  pfc_trace_add(P,S),
  !,
  pfc_enqueue_fc(P,S),
  !.



%=
%= pfc_addDbToHead(+P,-NewP) talkes a hybrid_fact P or a conditioned hybrid_fact
%= (P:-C) and adds the Db context.
%=
/*
pfc_addDbToHead(P,NewP) :-
  pfc_currentDb(Db),
  (Db=true        -> NewP = P;
   P=(Head:-Body) -> NewP = (Head :- (Db,Body));
   otherwise      -> NewP = (P :- Db)).
*/


pfc_enqueue_fc(P,S):-
 mpred_get(pfc_search,P,Mode) -> pfc_enqueue_fc(P,S,Mode) ; pfc_warn("No pfc_search mode").

pfc_enqueue_fc(P,S,Mode) :-
     (Mode=direct  -> pfc_fwd(P,S) ;
	Mode=depth   -> pfc_int_add_to_front(pfc_queue(P,S),S) ;
	Mode=breadth -> pfc_int_add_last(pfc_queue(P,S),S) ;
	otherwise    -> pfc_error("Unrecognized pfc_search mode: ~w", Mode)).
    


% if there is a hybrid_rule of the form Identifier ::: Rule then delete it.

pfc_removeOldVersion((Identifier::::Body)) :-
  % this should never happen.
  pfc_is_unbound(Identifier),
  !,
  pfc_error("variable used as an  hybrid_rule name in ~w :::: ~w",
          [Identifier,Body]).

  
pfc_removeOldVersion((Identifier::::Body)) :-
  pfc_is_bound(Identifier),
  db_uclause((Identifier::::OldBody),_),
  \+(Body=OldBody),
  pfc_rem1((Identifier::::OldBody)),
  !.
pfc_removeOldVersion(_).



% EOF

% pfc_run compute the deductive closure of the current database. 
% How this is done depends on the searching mode:
%    direct -  fc has already done the job.
%    depth or breadth - use the pfc_queue mechanism.

pfc_run :-
  (\+ mpred_get(pfc_search,*,direct)),
  pfc_runSteps.

pfc_run :- pfc_runQueueNoHalt.


pfc_runSteps:- 
  pfc_step,
  pfc_runSteps.


pfc_runQueueNoHalt:- doall(( pfc_queue(Q,S), \+ pfc_haltSignal(_), db_retract(pfc_queue(Q,S)),pfc_do_once(pfc_fwd(Q,S)))).
  

pfc_step :- \+ pfc_haltSignal(_),
  % draw immediate conclusions from the next hybrid_fact to be considered.
  % fails iff the queue is empty.
  get_next_fact(P,S),
  pfc_do_once(pfc_fwd(P,S)),
  !.
% pfc_step removes one entry from the pfc_queue and reasons from it.
pfc_step :-  
  % if pfc_haltSignal is true, reset it and fail, thereby stopping inferencing.
  db_retract(pfc_haltSignal(W)),
  pfc_warn(pfc_haltSignal(W)),
  !, 
  fail.

get_next_fact(P,S) :-
  %identifies the nect hybrid_fact to fc from and removes it from the queue.
  select_next_fact(P,S),
  remove_selection(P,S).

remove_selection(P,S) :- 
  db_retract(pfc_queue(P,S)),
  pfc_removeSupportsQuietly(pfc_queue(P,S)),
  !.
remove_selection(P,S) :-
  brake(pmsg("get_next_fact - selected hybrid_fact not on Queue: ~w (~w)", [P,S])).


% select_next_fact(P) identifies the next hybrid_fact to reason from.  
% It tries the u defined predicate first and, failing that, 
%  the default mechanism.

select_next_fact(P,S) :- 
  pfc_select(P,S),
  !.  
select_next_fact(P,S) :- 
  defaultpfc_select(P,S),
  !.  

% the default selection predicate takes the item at the froint of the queue.
defaultpfc_select(P,S) :- pfc_queue(P,S),!.

% pfc_halt stops the forward chaining.
pfc_halt :-  pfc_halt("",[]).

pfc_halt(Format) :- pfc_halt(Format,[]).

pfc_halt(Format,Args) :- 
  pfc_sformat(ThisTime,Format,Args),
  pmsg(pfc_halt(ThisTime)),
  (pfc_haltSignal(Why) -> 
       pfc_warn("pfc_halt finds pfc_haltSignal already set becasue of ~w",[Why])
     ; db_assert(pfc_haltSignal(ThisTime))).


% EOF
%=
%= predicates for manipulating triggers
%=


pfc_addTrigger(pt(Trigger,Body),Support) :-
  !,
  pfc_trace_msg('      Adding positive trigger ~q~n',
		[pt(Trigger,Body)]),  
  pfc_int_add_last(pt(Trigger,Body),Support),
  copy_term(pt(Trigger,Body),Tcopy),
  pfc_bc_caching(Trigger),
  pfc_evalLHS(Body,(Trigger,Tcopy)),
  fail.

pfc_addTrigger(pfc_t_pkey(Trigger,Test,Body),Support) :- trace,
       !,
       pfc_trace_msg('      Adding Pos3 trigger: ~q~n       test: ~q~n       body: ~q~n',
                     [Trigger,Test,Body]),
       copy_term(Trigger,TriggerCopy),
       pfc_int_add_last(pfc_t_pkey(TriggerCopy,Test,Body),Support),
       Test,
       pfc_evalLHS(Body,((Trigger),pfc_t_pkey(TriggerCopy,Test,Body))).


pfc_addTrigger(nt(Trigger,Test,Body),Support) :-
  !,
  pfc_trace_msg('      Adding negative trigger: ~q~n       test: ~q~n       body: ~q~n',
		[Trigger,Test,Body]),
  copy_term(Trigger,TriggerCopy),
  pfc_int_add_last(nt(TriggerCopy,Test,Body),Support),
  \+Test,
  pfc_evalLHS(Body,((\+Trigger),nt(TriggerCopy,Test,Body))).


pfc_addTrigger(bt(Trigger,Body),Support) :-
  !,
  pfc_int_add_last(bt(Trigger,Body),Support),
  pfc_btPtCombine(Trigger,Body,Support).

pfc_addTrigger(X,Support) :-
  pfc_error("Unrecognized trigger to pfc_addtrigger: ~w",[trigger(X,Support)]).


pfc_btPtCombine(Head,Body,Support) :- 
  %= a backward trigger (bt) was just added with head and Body and support Support
  %= find any pt's with unifying heads and add the instantied bt body.
  pfc_getTriggerQuick(pt(Head,_PtBody)),
  pfc_evalLHS(Body,Support),
  fail.
pfc_btPtCombine(_,_,_) :- !.

pfc_getTriggerQuick(Trigger) :-  db_uclause(Trigger,true).

pfc_getTrigger(Trigger):-pfc_getTriggerQuick(Trigger).

% EOF
%=
%= predicates for manipulating action traces.
%=

pfc_addActionTrace(Action,Support) :- 
  % adds an action trace and it's support.
  pfc_addSupport(pfc_action(Action),Support).

pfc_remActionTrace(pfc_action(A)) :-
  pfc_undo_method(A,M),
  M,
  !.


% EOF
%= predicate to remove pfc facts, triggers, action traces, and queue items
%= from the database.
%=

unused_pfc_retractOrWarn(X) :- 
  %= remove an arbitrary thing.
  pfc_type(X,Type),
  unused_pfc_retractType(Type,X),
  !.

unused_pfc_retractType(hybrid_rule,X) :- 
  %= db  pfc_addDbToHead(X,X2),  db_retract(X2).
  pfc_retract(X).

unused_pfc_retractType(trigger,X) :- 
  db_retract(X)
    -> unFc(X)
     ; pfc_warn("Trigger not found to remove: ~w",[X]).

unused_pfc_retractType(action,X) :- pfc_remActionTrace(X).

unused_pfc_retractType(support,X) :-  db_retract(X).

unused_pfc_retractType(hybrid_fact,X) :-   
  %= db pfc_addDbToHead(X,X2), remove(pfcInternal,X2). 
  pfc_retract(X).

%= unused_pfc_addSome(X) adds item X to some database

unused_pfc_addSome(X) :-
  % what type of X do we have?
  pfc_type(X,Type),
  % call the appropriate predicate.
  unused_pfc_addType(Type,X).

unused_pfc_addType(support,X) :- 
  db_assert(X),!.
unused_pfc_addType(hybrid_rule,X) :- 
  pfc_not_asserted_unify(X), 
  pfc_assert(X),!.
unused_pfc_addType(trigger,X) :- 
  db_assert(X).
unused_pfc_addType(action,_Action) :- !.
unused_pfc_addType(hybrid_fact,X) :- 
  must(pfc_not_asserted_unify(X)), 
  pfc_assert(X),!,
  if_defined(run_database_hooks(change(db_assert,z),X),true).



  

%= pfc_rem1(P,S) removes support S from P and checks to see if P is still supported.
%= If it is not, then the hybrid_fact is retreactred from the database and any support
%= relationships it participated in removed.
pfc_rem1(P) :- 
  % pfc_rem1/1 is the user's interface - it withdraws user support for P.
  pfc_maptree(pfc_lambda([E],pfc_rem(E)),P,[]).

pfc_rem(E):- source_user(U), pfc_rem1(E,(U,U)).


pfc_rem1(P,S) :-
  % pfc_debug(pmsg("removing support ~w from ~w",[S,P])),
  pfc_trace_msg('Removing support: ~q from ~q~n',[S,P]),  
  pfc_rem_support(P,S)
     -> pfc_removeIfUnsupported(P)
      ; pfc_warn("pfc_rem1/2 Could not find support ~w to remove from hybrid_fact ~w", [S,P]).

%=
%= pfc_rem2 is like pfc_rem1, but if P is still in the DB after removing the
%= user's support, it is retracted by more forceful means (e.g. pfc_rem3/1).
%=

pfc_rem2(P) :- 
  % pfc_rem2/1 is the user's interface - it withdraws user support for P.
  pfc_maptree(pfc_lambda([E],pfc_rem2_user(E)),P,[]).

pfc_rem2_user(E):- source_user(U), pfc_rem2(E,(U,U)).


pfc_rem2(P,S) :- pfc_rem2a(P,S)-> must(\+ pfc_bc_caching(P)) ; ((must(\+ pfc_bc_caching(P))),!,fail).

pfc_rem2a(P,S) :-
  pfc_rem1(P,S),
  pfc_bc_caching(P)
     -> pfc_rem3(P) 
      ; true.

%=
%= pfc_rem3(+F) retracts hybrid_fact F from the DB and removes any dependent facts */
%=

pfc_rem3(F) :- 
  pfc_removeSupports(F),
  pfc_undo(F).


% removes any remaining supports for hybrid_fact F, complaining as it goes.

pfc_removeSupports(F) :- 
  pfc_rem_support(F,S),
  pfc_warn("~w was still supported by ~w",[F,S]),
  fail.
pfc_removeSupports(_).

pfc_removeSupportsQuietly(F) :- 
  pfc_rem_support(F,_),
  fail.
pfc_removeSupportsQuietly(_).

% pfc_undo(X) undoes X.


pfc_undo(List):- is_list(List),!,maplist(pfc_undo,List).
pfc_undo(N):- pfc_from_negation(N,P),!,pfc_undo(neg(P)).

pfc_undo(nt(Head,Condition,Body)) :-  
  % undo a negative trigger.
  !,
  (db_retract(nt(Head,Condition,Body))
    -> unFc(nt(Head,Condition,Body))
     ; pfc_error("Trigger not found to remove: ~w",[nt(Head,Condition,Body)])).

pfc_undo(Fact) :-
  % undo a random hybrid_fact, printing out the trace, if relevant.
  pfc_retract(Fact),!,
  pfc_traceRem(Fact),
  unFc1(Fact).

pfc_undo(Fact) :- pmsg(no_pfc_undo(Fact)),sanity((functor(Fact,F,_),\+((atom_concat(_,'Fn',F),dtrace)))).


%= unFc(P) "un-forward-chains" from hybrid_fact f.  That is, hybrid_fact F has just
%= been removed from the database, so remove all support relations it
%= participates in and check the things that they support to see if they
%= should stayu in the database or should also be removed.


unFc(F) :- 
  pfc_remove_SR(F),
  unFc1(F).

unFc1(F) :-
  pfc_unFcCheckTriggers(F),
  % is this really the right place for pfc_run<?
  pfc_run.


pfc_unFcCheckTriggers(F) :-
  pfc_type(F,hybrid_fact),
  copy_term(F,Fcopy),
  nt(Fcopy,Condition,Action),
  (\+ pfc_call(Condition)),
  pfc_evalLHS(Action,((\+F),nt(F,Condition,Action))),
  fail.
pfc_unFcCheckTriggers(_).

pfc_remove_SR(Fact) :-
  pfc_type(Fact,Type),
  (Type=trigger -> pfc_rem_support(P,_,Fact); pfc_rem_support(P,Fact,_)),
  pfc_removeIfUnsupported(P),
  fail.
pfc_remove_SR(_).



%= pfc_removeIfUnsupported(+P) checks to see if P is supported and removes
%= it from the DB if it is not.

pfc_removeIfUnsupported(P) :- 
   pfc_tms_supported(P) -> true ;  pfc_undo(P).


%= pfc_tms_supported(+P) succeeds if P is "supported". What this means
%= depends on the TMS mode selected.

pfc_tms_supported(P) :- 
  mpred_get(pfc_tms,P,Mode),
  pfc_wff_support(Mode,P).

pfc_wff_support(local,P) :- !, pfc_get_support(P,A),!,A\=fail.
pfc_wff_support(cycles,P) :-  !, wellFounded(P).
pfc_wff_support(full,P) :-  !, wellFounded(P).
pfc_wff_support(_,_P) :- true.




% EOF
%= a hybrid_fact is well founded if it is supported by the user
%= or by a set of facts and a rules, all of which are well founded.
%=
wellFounded(Fact) :- pfc_wFF(Fact,[]).

pfc_wFF(F,_) :-
  % supported by user (pfc_axiom) or an "absent" hybrid_fact (assumption/Assumable).
  (pfc_axiom(F) ; pfc_assumptionBase(F)),
  !.

pfc_wFF(F,Descendants):- 
   pfc_wFF_Descendants(F,Descendants).

pfc_wFF_Descendants(F,Descendants) :-
  % first make sure we aren't in a loop.
  (\+ memberchk(F,Descendants)),
  % find a justification.
  supportsForWhy(F,Supporters),
  % all of whose members are well founded.
  pfc_wFF_L(Supporters,[F|Descendants]),
  !.

%= pfc_wFF_L(L) simply maps pfc_wFF over the list.

pfc_wFF_L([],_).
pfc_wFF_L([X|Rest],L) :-
  pfc_wFF(X,L),
  pfc_wFF_L(Rest,L).


% supports(+F,-ListofSupporters) where ListOfSupports is a list of the
% supports for one justification for hybrid_fact F -- i.e. a list of facts which,
% together allow one to deduce F.  One of the facts will typically be a hybrid_rule.
% The supports for a user-defined hybrid_fact are: [u/*(Original)*/].

supportsForWhy(F,[Fact|MoreFacts]) :-
  pfc_get_support(F,(Fact,Trigger)),
  triggerSupports(Trigger,MoreFacts).

triggerSupports(U,[]) :- source_user(U),!.
triggerSupports(Trigger,[Fact|MoreFacts]) :-
  pfc_get_support(Trigger,(Fact,AnotherTrigger)),
  triggerSupports(AnotherTrigger,MoreFacts).



% EOF
%=
%= pfc_fwd(X,S) forward chains from a hybrid_fact or a list of facts X.
%=
pfc_fwd(X,S):-
  with_assertions(thlocal:pfc_no_mark,pfc_maptree(pfc_fwd1,X,[S])).
  %copy_term(X,X1),
  % pfc_maptree(pfc_fwd1,X1,[]).

%=
%= pfc_fwd1(+Fact,+S) forward chains for a single hybrid_fact.
%=
pfc_fwd1(Fact,S) :-
   call(pfc_get_head(Fact,Head)),!,
   must(pfc_is_bound(Head)->true;pmsg(pfc_fwd1(Fact - Head,S))),
    loop_check_term(pfc_fwd1_newoutcome(Fact,S),        Head,
       nop(pmsg(looped_pfc_ruleHeadHead(Fact,Head,S)))),!.

pfc_fwd1_newoutcome(Fact,S) :-
  must(pfc_rule_check(Fact)),
  call(pfc_add_minfo(assertz_if_new,Fact)),
  copy_term(Fact,F),
  % check positive triggers
  must(pfc_runPT(Fact,F)),
  % check negative triggers
  must(pfc_runNT(Fact,F)).


%=
%= pfc_rule_check(P) does some special, built in forward chaining if P is 
%= a hybrid_rule.
%= 

pfc_rule_check((P=>Q)) :-  
  !,  
  pfc_processRule(P,Q,(P=>Q)).
pfc_rule_check((Name::::P=>Q)) :- 
  !,  
  pfc_processRule(P,Q,(Name::::P=>Q)).
pfc_rule_check((P<=>Q)) :- 
  !, 
  pfc_processRule(P,Q,(P<=>Q)), 
  pfc_processRule(Q,P,(P<=>Q)).
pfc_rule_check((Name :::: P <=> Q)) :- 
  !, 
  pfc_processRule(P,Q,((Name::::P<=>Q))), 
  pfc_processRule(Q,P,((Name::::P<=>Q))).

pfc_rule_check(('<='(P,Q))) :-
  !,
  pfc_defineBcRule(P,Q,('<='(P,Q))).

pfc_rule_check(_).

pfc_good_rhs(rhs([Var])):-pfc_is_unbound(Var),!,fail.
pfc_good_rhs(_).

pfc_runPT(Fact,F) :- 
  pfc_getTriggerQuick(pt(F,Body)),
  pfc_trace_msg('      Found positive trigger: ~q~n       body: ~q~n',
		[F,Body]),
  must(pfc_good_rhs(Body)),
  pfc_not_too_deep(pfc_runPT, pfc_evalLHS(Body,(Fact,pt(F,Body)))),
  fail.

%pfc_runPT(Fact,F) :- 
%  pfc_getTriggerQuick(pt(presently(F),Body)),
%  pfc_evalLHS(Body,(presently(Fact),pt(presently(F),Body))),
%  fail.

pfc_runPT(_,_).


pfc_runNT(_Fact,F) :-
  spft(X,_,nt(F,Condition,Body)),
  pfc_call(Condition),
  pfc_rem1(X,(_,nt(F,Condition,Body))),
  fail.
pfc_runNT(_,_).


%=
%= pfc_defineBcRule(+Head,+Body,+ParentRule) - defines a backeard
%= chaining hybrid_rule and adds the corresponding bt triggers to the database.
%=

pfc_defineBcRule(Head,_Body,ParentRule) :-
  potrace((\+ pfc_is_literal(Head))),
  pfc_warn("Malformed backward chaining hybrid_rule.  ~w not atomic.",[Head]),
  pfc_warn("hybrid_rule: ~w",[ParentRule]),
  !,
  fail.

pfc_defineBcRule(Head,Body,ParentRule) :-
  copy_term(ParentRule,ParentRuleCopy),
  pfc_buildRhs(Head,Rhs),
  must(pfc_good_rhs(rhs(Rhs))),
  source_user(U),
  pfc_forEach(pfc_nf(Body,Lhs),
          (pfc_buildTrigger(Lhs,rhs(Rhs),Trigger),
           pfc_add_BTRule(bt(Head,Trigger),(ParentRuleCopy,U)))).
 


% EOF
%=
%= eval something on the LHS of a hybrid_rule.
%=

 
pfc_evalLHS((Test->Body),Support) :-  
  !, 
  (pfc_call(pfc_test,Test) -> pfc_evalLHS(Body,Support)),
  !.

pfc_evalLHS(rhs(X),Support) :-
  !,
  pfc_eval_rhs(X,Support),
  !.

pfc_evalLHS(X,Support) :-
  pfc_db_type(X,trigger),
  !,
  pfc_addTrigger(X,Support),
  !.

%pfc_evalLHS(snip(X),Support) :- 
%  snip(Support),
%  pfc_evalLHS(X,Support).

pfc_evalLHS(X,_) :-
  pfc_error("Unrecognized item found in trigger body, namely ~w.",[X]).


%=
%= eval something on the RHS of a hybrid_rule.
%=

pfc_eval_rhs([],_) :- !.
pfc_eval_rhs([Head|Tail],Support) :- 
  pfc_eval_rhs1(Head,Support),
  pfc_eval_rhs(Tail,Support).



pfc_eval_rhs1(XXrest,Support) :- is_list(XXrest),
 % embedded sublist.
 !, pfc_eval_rhs(XXrest,Support).


pfc_eval_rhs1({Action},Support) :-
 % evaluable Prolog code.
 !,
 pfc_evalAction(Action,Support).

pfc_eval_rhs1(P,_Support) :-
 % predicate to remove.
 pfc_negated_lit(P),
 !,
 pfc_rem1(P).

pfc_eval_rhs1(Assertion,Support) :-
 % an assertion to be added.
 pfc_post1(Assertion,Support).

pfc_eval_rhs1(X,_) :-
  pfc_error("Malformed rhs of a hybrid_rule: ~w",[X]).


%=
%= evaluate an action found on the rhs of a hybrid_rule.
%=

pfc_evalAction(Action,Support) :-
  (pfc_call(pfc_evalAction,Action)), 
  (pfc_undoable(Action) 
     -> pfc_addActionTrace(Action,Support) 
      ; true).


%=
%= 
%=

unused_pfc_trigger_the_trigger(Trigger,Body,_Support) :-
 unused_trigger_trigger1(Trigger,Body).
unused_pfc_trigger_the_trigger(_,_,_).


%unused_trigger_trigger1(presently(Trigger),Body) :-
%  !,
%  copy_term(Trigger,TriggerCopy),
%  pfc_bc_caching(Trigger),
%  pfc_evalLHS(Body,(presently(Trigger),pt(presently(TriggerCopy),Body))),
%  fail.

unused_trigger_trigger1(Trigger,Body) :-
  copy_term(Trigger,TriggerCopy),
  pfc_bc_caching(Trigger),
  pfc_evalLHS(Body,(Trigger,pt(TriggerCopy,Body))),
  fail.


%=
%= The predicate pfc_bc_caching/1 is the proper way to access terms in the Pfc database. pfc(P) succeeds if P is a term
%= in the current pfc database after invoking any backward chaining rules or is provable by Prolog.
%= pfc_bc_caching(F) is true iff F is a hybrid_fact available for forward (backward?) chaining.
%= Note that this has the side effect of catching unsupported facts and
%= assigning them support from God.
%=
pfc_bc_caching(F):-hotrace(pfc_loop_check(pfc_bC_Cache(F))).

pfc_bC_Cache(not(P)) :-nonvar(P),!,pfc_bC_Cache(neg(P)).
pfc_bC_Cache(P) :-
  % trigger any bc rules.
  bt(P,Trigger),
  pfc_get_support(bt(P,Trigger),S),
  pfc_evalLHS(Trigger,S),
  pfc_maybeSupport(P,S),
  fail.


pfc_bC_Cache(F) :-
  %= this is probably not advisable due to extreme inefficiency.
  pfc_is_unbound(F)    ->  pfc_fact(F) ;
  ( \+ current_predicate(_,F)) -> pfc_call(F) ;
  % check for system predicates as well.
  \+(has_clauses(F)) -> pfc_call(systemPred,F) ; 
  otherwise ->  (pfc_uclause(F,Condition), 
    pfc_call(neck(F),Condition), 
       ignore((ground(F),(\+(pfc_is_asserted(F)), pfc_maybeSupport(F,(g,g)))))).


pfc_maybeSupport(P,_):-pfc_ignored(P),!.
pfc_maybeSupport(P,S):-( \+ ground(P)-> true;
  (predicate_property(P,dynamic)->pfc_add_fast(P,S);true)).


%=
%= pfc_bC_NoFacts(F) is true iff F is a hybrid_fact available for backward chaining ONLY.
%= Note that this has the side effect of catching unsupported facts and
%= assigning them support from God.
%= this Predicate should hide Facts from pfc_bc_only/1
%=
pfc_bc_only(G):- pfc_from_negation(G,Pos),!,show_call( \+(pfc_bc_only(Pos))),!.
pfc_bc_only(G):- pfc_loop_check(pfc_bC_NoFacts(G)).

pfc_bC_NoFacts(F):- pfc_bC_NoFacts_TRY(F)*-> true ; (pfc_slow_search,pfc_bC_Cache(F)).

pfc_bC_NoFacts_TRY(F) :- pfc_is_bound(F),
 (
  %= this is probably not advisable due to extreme inefficiency.
  pfc_is_unbound(F)    ->  pfc_fact(F) ;
  ( \+ current_predicate(_,F)) -> pfc_call(F) ;
  % check for system predicates as well.
  \+(has_clauses(F)) -> pfc_call(systemPred,F) ; 
  otherwise -> pfc_bC_NoFacts_TRY2(F)).

pfc_ruleBackward(F,Condition):-pfc_ruleBackward0(F,Condition),Condition\=pfc_call(F).
pfc_ruleBackward0(F,Condition):-db_uclause(F,Condition),not(is_true(Condition);pfc_is_info(Condition)).
pfc_ruleBackward0(F,Condition):-'<='(F,Condition),\+(is_true(Condition);pfc_is_info(Condition)).

pfc_bC_NoFacts_TRY2(F) :- no_repeats(pfc_ruleBackward(F,Condition)),
  pfc_call(neck(F),Condition),\+ db_uclause(F,true),
  pfc_maybeSupport(F,(g,g)).


% an action is pfc_undoable if there exists a method for undoing it.
pfc_undoable(A) :- pfc_undo_method(A,_).



% EOF
%=
%= defining fc rules 
%=

%= pfc_nf(+In,-Out) maps the LHR of a pfc hybrid_rule In to one normal form 
%= Out.  It also does certain optimizations.  Backtracking into this
%= predicate will produce additional clauses.


pfc_nf(LHS,List) :-
  pfc_nf1(LHS,List2),
  pfc_nf_negations(List2,List).


%= pfc_nf1(+In,-Out) maps the LHR of a pfc hybrid_rule In to one normal form
%= Out.  Backtracking into this predicate will produce additional clauses.

% handle a variable.

pfc_nf1(P,[P]) :- pfc_is_unbound(P), !.

% these next two rules are here for upward compatibility and will go 
% away eventually when the P/Condition form is no longer used anywhere.

pfc_nf1(P/Cond,[(\+P)/Cond]) :- pfc_negated_lit(P), !.

pfc_nf1(P/Cond,[P/Cond]) :-  pfc_literal(P), !.

%= handle a negated form

pfc_nf1(NegTerm,NF) :-
  pfc_from_negation(NegTerm,Term),
  !,
  must(pfc_nf1_negation(Term,NF)).

%= disjunction.

pfc_nf1((P;Q),NF) :- 
  !,
  (pfc_nf1(P,NF) ;   pfc_nf1(Q,NF)).


%= conjunction.

pfc_nf1((P,Q),NF) :-
  !,
  pfc_nf1(P,NF1),
  pfc_nf1(Q,NF2),
  append(NF1,NF2,NF).

%= handle a random atom.

pfc_nf1(P,[P]) :- 
  pfc_literal(P),  %= DMILES TODO unaddNeg
  !.

%=% shouln't we have something to catch the rest as errors?
pfc_nf1(Term,[Term]) :-
  pfc_error("pfc_nf doesn't know how to normalize ~w",[Term]).




%= pfc_nf1_negation(P,NF) is true if NF is the normal form of \+P.
pfc_nf1_negation((P/Cond),[(\+(P))/Cond]) :- !.

pfc_nf1_negation((P;Q),NF) :-
  !,
 must(( pfc_nf1_negation(P,NFp),
  pfc_nf1_negation(Q,NFq),
  append(NFp,NFq,NF))).

pfc_nf1_negation((P,Q),NF) :- 
  % this code is not correct! twf.  
  !,
  must((
   pfc_nf1_negation(P,NF) ,
   pfc_warn("leftHandSideDisjunct: ~w",[NF])
  ))
   ;
 must(( 
   pfc_nf1(P,Pnf),
   pfc_nf1_negation(Q,Qnf),
   append(Pnf,Qnf,NF),
   % so we'll warn it! logicmoo
   pfc_warn("pfc_rightHandSideDisjunct: ~w",[NF])
  )).

pfc_nf1_negation(P,[\+P]).




%= pfc_nf_negations(List2,List) sweeps through List2 to produce List,
%= changing ~{...} to {\+...}
%=% ? is this still needed? twf 3/16/90

pfc_nf_negations(X,X) :- !.  % I think not! twf 3/27/90

pfc_nf_negations(X,X) :- pfc_is_unbound(X),!.

pfc_nf_negations([],[]).

pfc_nf_negations([H1|T1],[H2|T2]) :-
  pfc_nf_negation(H1,H2),
  pfc_nf_negations(T1,T2).

pfc_nf_negation(Form,{PLNeg}) :-  
  pfc_is_bound(Form),
  into_mpred_canonical_form(callable,Form,PLNeg),
  !.
pfc_nf_negation(X,X).


%=
%= pfc_buildRhs(+Conjunction,-Rhs)
%=

pfc_buildRhs(X,[X]) :- 
  pfc_is_unbound(X),
  !.

pfc_buildRhs((A,B),[A2|Rest]) :- 
  !, 
  pfc_compileRhsTerm(A,A2),
  pfc_buildRhs(B,Rest).

pfc_buildRhs(X,[X2]) :-
   pfc_compileRhsTerm(X,X2).

pfc_compileRhsTerm((P/C),((P:-C))) :- !.
pfc_compileRhsTerm(P,P).

pfc_negate_for_add(NQ,NQ):-pfc_is_unbound(NQ),!.
pfc_negate_for_add(','(_,Q),NQ):-!,pfc_negate_for_add(Q,NQ).
pfc_negate_for_add('<=>'(P,Q),'<=>'(P,NQ)):-!,pfc_negate_for_add(Q,NQ).
pfc_negate_for_add(In,Out):-pfc_negate(pfc_negate_for_add,In,Out).

pfc_negate(C,'=>'(P,Q),'=>'(P,NQ)):-!,call(C,Q,NQ).
pfc_negate(C,'<='(Q,P),'<='(NQ,P)):-!,call(C,Q,NQ).
pfc_negate(C,':-'(Q,P),':-'(NQ,P)):-!,call(C,Q,NQ).
pfc_negate(_,N,P):-call(pfc_negated_lit(N)),!,pfc_from_negation(N,P).
pfc_negate(_,P,N):-call(pfc_positive_lit(P)),!,N=neg(P).

%= pfc_from_negation(N,P) is true if N is a negated term and P is the term
%= with the negation operator stripped.

pfc_from_negation((~P),P).
pfc_from_negation((-P),P).
pfc_from_negation((\+(P)),P).
pfc_from_negation((naf(P)),P).
% NEVER not or neg! pfc_from_negation(not(P)),P).
pfc_from_negation(NP,PP):-call(pfc_loop_check(pfc_negation0(NP,PP))).
pfc_negation0(NP,PP):- compound(NP), NP=..[NF,A|RGS], if_defined(is_negated_functor(NF,PF,_FormName)),!,PP=..[PF,A|RGS].

pfc_from_negation_plus_holder(neg(P),P):-!.
pfc_from_negation_plus_holder(not(P),P):-!.
pfc_from_negation_plus_holder(N,P):-pfc_from_negation(N,P).


pfc_negated_lit(P) :- 
  call((pfc_from_negation(P,Q),
  pfc_positive_lit(Q))),!.

pfc_literal(X):-pfcAtom_0(X).

pfcAtom_0(X):-pfc_is_literal(X).
pfcAtom_0(V):-pfc_neg_holder_0(V).
pfcAtom_0(V):-pfc_is_unbound(V),!.

pfc_neg_holder(NEG):-call(pfc_neg_holder_0(NEG)).

pfc_neg_holder_0(neg(X)):-pfc_is_literal(X),!.
pfc_neg_holder_0(NEG):- mpred_prop(NEG,pfc_is_neg_store).

pfc_is_literal(X) :- pfc_negated_lit(X),!.
pfc_is_literal(X) :- pfc_positive_lit(X),!.

pfc_positive_lit(X) :- pfc_is_bound(X), 
  functor(X,F,_), 
  \+ pfc_is_operator(F).

pfc_is_operator(';').
pfc_is_operator(',').
pfc_is_operator('/').
pfc_is_operator('|').
pfc_is_operator(('=>')).
pfc_is_operator(('<=')).
pfc_is_operator('<=>').

pfc_is_operator('-').
pfc_is_operator('~').
pfc_is_operator(('\\+')).

% TODO create a way to have negative and not negations
pfc_is_operator(neg):-!,fail. 
pfc_is_operator(F):- call(mpred_get(pfc_is_neg_store,F,true)),!,fail.
pfc_is_operator(F):- call(mpred_get(pfc_is_operator,F,true)),!.

% :- asserta((is_logical_functor(F,F):-pfc_is_operator(F))).

pfc_processRule(Lhs,Rhs,ParentRule) :-  
  copy_term(ParentRule,ParentRuleCopy),
  pfc_buildRhs(Rhs,Rhs2),
  must(pfc_good_rhs(rhs(Rhs2))),
  source_user(U),
  pfc_forEach(pfc_nf(Lhs,Lhs2), 
          ((pfc_build1Rule(Lhs2,rhs(Rhs2),(ParentRuleCopy,U))))).

pfc_build1Rule(Lhs,Rhs,Support) :-
  must(pfc_buildTrigger(Lhs,Rhs,Trigger)),
  pfc_evalLHS(Trigger,Support).

pfc_buildTrigger([],Consequent,Consequent).

pfc_buildTrigger([V|Triggers],Consequent,pt(V,X)) :-
  pfc_is_unbound(V),
  !, 
  pfc_buildTrigger(Triggers,Consequent,X).

pfc_buildTrigger([(T1/Test)|Triggers],Consequent,nt(T2,Test2,X)) :- 
  pfc_from_negation(T1,T2),
  !,
  pfc_buildNtTest(T2,Test,Test2),
  pfc_buildTrigger(Triggers,Consequent,X).

pfc_buildTrigger([(T1)|Triggers],Consequent,nt(T2,Test,X)) :- 
  pfc_from_negation(T1,T2),
  !,
  pfc_buildNtTest(T2,true,Test),
  pfc_buildTrigger(Triggers,Consequent,X).

pfc_buildTrigger([{Test}|Triggers],Consequent,(Test->X)) :-
  !,
  pfc_buildTrigger(Triggers,Consequent,X).

pfc_buildTrigger([T/Test|Triggers],Consequent,pt(T,X)) :-
  !, 
  pfc_buildTest(Test,Test2),
  pfc_buildTrigger([{Test2}|Triggers],Consequent,X).


%pfc_buildTrigger([snip|Triggers],Consequent,snip(X)) :-
%  !,
%  pfc_buildTrigger(Triggers,Consequent,X).

pfc_buildTrigger([T|Triggers],Consequent,pt(T,X)) :-
  !, 
  pfc_buildTrigger(Triggers,Consequent,X).

%=
%= pfc_buildNtTest(+,+,-).
%=
%= builds the test used in a negative trigger (nt/3).  This test is a
%= conjunction of the check than no matching facts are in the db and any
%= additional test specified in the hybrid_rule attached to this ~ term.
%=

pfc_buildNtTest(T,Testin,Testout) :-
  pfc_buildTest(Testin,Testmid),
  pfc_conjoin(pfc_bc_caching(T),Testmid,Testout).

  
% this just strips away any currly brackets.

pfc_buildTest({Test},Test) :- !,pfc_mark_W(Test).
pfc_buildTest(Test,Test):-pfc_mark_W(Test).

% EOF




pfc_data_type(spft(_,_,_),support).

pfc_data_type((H:-B),Type):-is_true(B),!,pfc_data_type(H,Type).
pfc_data_type(G,Type) :- pfc_type(G,Type).

%= simple typeing for pfc objects
pfc_db_type(('=>'(_,_)),hybrid_rule).
pfc_db_type(('<=>'(_,_)),hybrid_rule).
pfc_db_type(('<='(_,_)),hybrid_rule).
pfc_db_type(pfc_t_pkey(_,_,_),trigger).
pfc_db_type(pt(_,_),trigger).
pfc_db_type(nt(_,_,_),trigger).
pfc_db_type(bt(_,_),trigger).
pfc_db_type(pfc_action(_),action).
pfc_db_type(('=>'(_)),hybrid_fact).
pfc_db_type(('~'(_)),hybrid_fact).
pfc_db_type(('neg'(_)),hybrid_fact).
pfc_db_type((('::::'(_,X))),Type):-pfc_is_bound(X),pfc_type(X,Type).
%= if it's not one of the above, it must be a hybrid_fact!
pfc_type(P,T):- (pfc_db_type(P,T0)*->((pfc_is_unbound(T)->T0=T;T==T0)*->true);T=hybrid_fact).
 



pfc_int_add_last(P,Support) :- 
  assertz_if_new(P),
  !,
  pfc_addSupport(P,Support).

pfc_int_add_to_front(P,Support) :-
  asserta_if_new(P),
  !,
  pfc_addSupport(P,Support).


pfc_is_asserted((Head :- Body)) :-
  !,
  copy_term(Head,Head_copy),
  copy_term(Body,Body_copy),
  pfc_uclause(Head,Body),
  variant(Head,Head_copy),
  variant(Body,Body_copy).

pfc_is_asserted(Head) :-
  % find a unit db_uclause identical to Head by finding one which unifies,
  % and then checking to see if it is identical
  copy_term(Head,Head_copy),
  pfc_uclause(Head_copy,true),
  variant(Head,Head_copy).

pfc_forEach(Binder,Body) :- Binder,pfc_do_once(Body),fail.
pfc_forEach(_,_).

% pfc_do_once(X) executes X once and always succeeds.
pfc_do_once(X) :- X,!.
pfc_do_once(_).


%= pfc_union(L1,L2,L3) - true if set L3 is the result of appending sets
%= L1 and L2 where sets are represented as simple lists.

pfc_union([],L,L).
pfc_union([Head|Tail],L,Tail2) :-  
  memberchk(Head,L),
  !,
  pfc_union(Tail,L,Tail2).
pfc_union([Head|Tail],L,[Head|Tail2]) :-  
  pfc_union(Tail,L,Tail2).


%= pfc_conjoin(+Conjunct1,+Conjunct2,?Conjunction).
%= arg3 is a simplified expression representing the conjunction of
%= args 1 and 2.

pfc_conjoin(TRUE,X,X) :- is_true(TRUE),!.
pfc_conjoin(X,TRUE,X) :- is_true(TRUE),!.
pfc_conjoin(X,Y,O):- X==Y,!,O=X.
pfc_conjoin(C1,C2,(C1,C2)).


% pfc_op_join(OP,TRUE,X,X) :- is_true(TRUE),!.
pfc_op_join(OP,X,TRUE,X) :- is_true(TRUE),!.
pfc_op_join(OP,C1,C2,R)  :-R=..[OP,C1,C2].

% pfc_file('pfcsupport').	% support maintenance

% EOF
%=
%= predicates for manipulating support relationships
%=

:-dynamic(spft/3).

%= pfc_addSupport(+Fact,+Support)
pfc_addSupport(P,(Fact,Trigger)) :-
  assertz_if_new(spft(P,Fact,Trigger)).

pfc_get_support_OR(P,OR,Support):- (pfc_get_support(P,Support) *-> true ; Support = OR).

% for a litteral
%pfc_get_support(P,(S,S)):-pfc_is_unbound(P),!,S=if_was_asserted(P1).
pfc_get_support(P,S):- 
  (pfc_get_support_lit(P,S)*-> true; 
  (pfc_get_support_neg(P,S)*-> true; 
   pfc_get_support_cnj(P,S))).

pfc_get_support_lit(P,(F,T)):- spft(P,F,T).
pfc_get_support_neg(N,S):- pfc_from_negation(N,P),!,pfc_get_support(neg(P),S).
pfc_get_support_cnj((P1,P2),((F1,F2),(T1,T2))):-!,pfc_get_support(P1,(F1,T1)),pfc_get_support(P2,(F2,T2)).
% pfc_get_support_cnj([P1|P2],[S1|S2]):-!,pfc_get_support_OR(P1,if_was_asserted(P1),S1),pfc_get_support(P2,S2).


% TODO pack the T1 into T2 return value is still a (Fact,Trigger) pair
pfc_why(G,Proof):-spft(G,F,T),(F==T -> Proof=asserted(T,G);Proof=deduced(F,T)).

pfc_removeSupportItems(P,Types):-
  pfc_get_support(P,Support),
  show_call(pfc_filterSupports(Support,Types,Results)),
  pfc_maptree(pfc_rem1,Results,[]).

pfc_typeFilter_l(ResultsO,Filter,ResultsO):-pfc_typeFilter(ResultsO,Filter).
pfc_typeFilter_l((Body,More),Filter,ResultsO):-!,pfc_typeFilter_l(Body,Filter,BodyO),pfc_typeFilter_l((More),Filter,(ResultsM)),pfc_conjoin(BodyO,ResultsM,ResultsO).
pfc_typeFilter_l(_,_Filter,!).

pfc_filterSupports(Support,Filter,ResultsO):- 
  pfc_get_head_body(Support,_,Body),
  pfc_typeFilter_l(Body,Filter,ResultsO),!.

pfc_filterSupports(Support,Filter,ResultsO):-
  findall(Term, ((sub_term(Term,Support),pfc_literal(Term),compound(Term),pfc_typeFilter(Term,Filter))),Results),
  list_to_set(Results,ResultsO).

pfc_typeFilter(Term,Filter):- not(is_list(Filter)),!,pfc_typeFilter(Term,[Filter]).
pfc_typeFilter(Term,FilterS):- pfc_data_type(Term,Type),memberchk(Type,FilterS),!.
pfc_typeFilter(Term,FilterS):- member(Filter,FilterS),append_term(Filter,Term,Call),current_predicate(_,Call),debugOnError(Call),!.

% There are three of these to try to efficiently handle the cases
% where some of the arguments are not bound but at least one is.
pfc_is_support_goal(spft(_,_,_)).


pfc_rem_support(P,Fact,Trigger) :- must_no_numvars(v(P,Fact,Trigger)),(db_retract(spft(P,Fact,Trigger))).
pfc_rem_support(P,(Fact,Trigger)) :- must_no_numvars(v(P,Fact,Trigger)), (db_retract(spft(P,Fact,Trigger))).

pfc_collect_supports(Tripples) :-
  bagof(Tripple, pfc_support_relation(Tripple), Tripples),
  !.
pfc_collect_supports([]).

pfc_support_relation((P,F,T)) :-
  spft(P,F,T).

unused_pfc_make_supports((P,S1,S2)) :- 
  pfc_addSupport(P,(S1,S2)),
  (unused_pfc_addSome(P); true),
  !.

%= pfc_triggerKey(+Trigger,-Key) 
%=
%= Arg1 is a trigger.  Key is the best term to index it on.

/*
%pfc_triggerKey(pt(Key,_),Key).
pfc_triggerKey(pfc_t_pkey(Key,_,_),Key).
pfc_triggerKey(nt(Key,_,_),Key).
pfc_triggerKey(Key,Key).
*/


%=^L
%= Get a key from the trigger that will be used as the first argument of
%= the trigger pfc_bases_I_L db_uclause that stores the trigger.
%=

pfc_trigger_key(X,X) :- pfc_is_unbound(X), !.
pfc_trigger_key(chart(word(W),_L),W) :- !.
pfc_trigger_key(chart(stem([Char1|_Rest]),_L),Char1) :- !.
pfc_trigger_key(chart(Concept,_L),Concept) :- !.
pfc_trigger_key(X,X).



% removes all forward chaining rules and justifications from db.

pfc_reset :-
  db_uclause(spft(P,F,Trigger),true),
  log_and_continue_on_failure(pfc_retract(P)),
  log_and_continue_on_failure(db_retract(spft(P,F,Trigger))),
  fail.
pfc_reset :-
  pfc_databaseItem(T),
  pfc_error("Pfc database not empty after pfc_reset, e.g., ~p.~n",[T]).
pfc_reset.

% true if there is some pfc crud still in the database.
pfc_databaseItem(Term) :-
  pfc_state(P/A),
  functor(Term,P,A),
  db_uclause(Term,_).


% pfc_file('pfcdebug').	% debugging aids (e.g. tracing).


%   File   : pfcdebug.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: provides predicates for examining the database and debugginh 
%   for Pfc.

:- decl_mpred(warnings,*,true).

%= predicates to examine the state of pfc

pfc_queue :- listing(pfc_queue/2).

pfc_printDB :- current_predicate(must_det_l/1),!,
  must_det_l([
  pfc_printFacts,
  pfc_printRules,
  pfc_printTriggers,
   pfc_printSupports,
   pfc_queue]),!.

pfc_printDB :-
   pfc_printFacts,
   pfc_printRules,
   pfc_printTriggers,
  pfc_printSupports,
  pfc_queue,!.

%= pfc_printFacts ..

pfc_printFacts :- pfc_printFacts(_,true).

pfc_printFacts(Pattern) :- pfc_printFacts(Pattern,true).

pfc_printFacts(P,C) :-
  pfc_facts(P,C,L),
  must(pfc_classifyFacts(L,User,Pfc,_Rule)),!,
  fmt("User added facts:",[]),
  must(pfc_printitems(User)),
  fmt("Pfc added facts:",[]),
  must(pfc_printitems(Pfc)).

pfc_sup_user(P,U):- spft(P,U,U).

%= printitems clobbers it's arguments - beware (fixed .. it no longer clobers)!

pfc_printitems(HIn):-copy_term(HIn,H),pfc_maptree(pfc_lambda([E],(numbervars(E,0,_),fmt(" ~q.~n",[E]))),H,[]).

pfc_classifyFacts([],[],[],[]).

pfc_classifyFacts([H|T],User,Pfc,[H|Rule]) :-
  pfc_type(H,hybrid_rule),
  !,
  pfc_classifyFacts(T,User,Pfc,Rule).

pfc_classifyFacts([H|T],[H|User],Pfc,Rule) :-
  source_user(U),pfc_sup_user(H,U),
  !,
  pfc_classifyFacts(T,User,Pfc,Rule).

pfc_classifyFacts([H|T],User,[H|Pfc],Rule) :-
  pfc_classifyFacts(T,User,Pfc,Rule).


printHeadItems(Head):-ignore((bagof(Head,db_uclause(Head,true),R1),pfc_printitems(R1))).
printHeadCallItems(Head):-ignore((bagof(Head,db_uclause(Head,true),R1),pfc_printitems(R1))).

pfc_printRules :-
  printHeadItems((P=>Q)),printHeadItems((P<=>Q)),printHeadItems((P<=Q)).

pfc_printTriggers :-
  fmt("% Positive triggers...~n",[]),
     printHeadCallItems(pfc_getTrigger(pt(_,_))),
  fmt("% Negative triggers...~n",[]),
     printHeadCallItems(pfc_getTrigger(nt(_,_,_))),
  fmt("% Goal triggers...~n",[]),
     printHeadCallItems(pfc_getTrigger(bt(_,_))),!.

pfc_printSupports :- 
  % temporary hack.
  setof((S > P), pfc_get_support(P,S),L),
  pfc_printitems(L).

%example pfcVerifyMissing(mpred_prop(I,D), mpred_prop(I,C), ((mpred_prop(I,C), {D==C});~mpred_prop(I,C))). 
%example pfcVerifyMissing(mudColor(I,D), mudColor(I,C), ((mudColor(I,C), {D==C});~mudColor(I,C))). 

pfcVerifyMissing(GC, GO, ((GO, {D==C});~GO) ):- 
       GC=..[F,A|Args],append(Left,[D],Args),append(Left,[C],NewArgs),GO=..[F,A|NewArgs],!.

%example pfc_freeLastArg(mpred_prop(I,C),neg(mpred_prop(I,C))):-pfc_is_bound(C),!.
%example pfc_freeLastArg(mpred_prop(I,C),(mpred_prop(I,F),C\=F)):-!.
pfc_freeLastArg(G,GG):- G=..[F,A|Args],append(Left,[_],Args),append(Left,[_],NewArgs),GG=..[F,A|NewArgs],!.
pfc_freeLastArg(_G,false).

%= pfc_fact(P) is true if hybrid_fact P was asserted into the database via add.
pfc_fact(P) :- no_repeats(pfc_fact(P,true)).

%= pfc_fact(P,C) is true if hybrid_fact P was asserted into the database via
%= add and condition C is satisfied.  For example, we might do:
%= 
%=  pfc_fact(X,pfc_user_fact(X))
%=

pfc_fact(P,C) :- no_repeats(pfc_fact0(P,C)).

pfc_fact0(P,C) :- 
  pfc_get_support(P,_),
  pfc_type(P,hybrid_fact),
  pfc_call(hybrid_fact,C).

%= pfc_facts(-ListofPfcFacts) returns a list of facts added.

pfc_facts(L) :- pfc_facts(_,true,L).

pfc_facts(P,L) :- pfc_facts(P,true,L).

%= pfc_facts(Pattern,Condition,-ListofPfcFacts) returns a list of facts added.

pfc_facts(P,C,L) :- setof(P,pfc_fact(P,C),L).

brake(X) :-  X, break.

% EOF
%=
%= predicates providing a simple tracing facility
%=

pfc_trace_add(P) :- 
  % this is here for upward compat. - should go away eventually.
  pfc_trace_add(P,(o,o)).

pfc_trace_add(pt(_,_),_) :-
  % hack for now - never trace triggers.
  !.
pfc_trace_add(pfc_t_pkey(_,_,_),_) :-
  % hack for now - never trace triggers.
  !.
pfc_trace_add(nt(_,_,_),_) :-
  % hack for now - never trace triggers.
  !.

pfc_trace_add(P,S) :-
   pfc_trace_addPrint(P,S),
   pfc_traceBreak(P,S).

%example pfc_is_crazy_bad_fact(mpred_prop(not,tCol)).
%example pfc_is_crazy_bad_fact(mpred_prop(_,not)).
pfc_is_crazy_bad_fact(mpred_prop(_,pred_argtypes)).

pfc_trace_addPrint(P,S) :- pfc_is_crazy_bad_fact(P),retractall(tlbugger:show_must_go_on),!,trace_or_throw(crazy_pfc_trace_addPrint(P,S)).

pfc_trace_addPrint(P,S) :-
  \+ \+ mpred_prop(P,tracing),
  !,
  copy_term(P,Pcopy),
  numbervars(Pcopy,0,_),
  ((S = (W1,W2),W1==W2)
       -> pmsg(";  Adding (~w) ~q",[W1,P])
        ; pmsg(";  Adding (:) ~q   === ~q",[Pcopy,S])).

pfc_trace_addPrint(_,_).


pfc_traceBreak(P,_S) :-
  mpred_prop(P,add(spying)) -> 
   (copy_term(P,Pcopy),
    numbervars(Pcopy,0,_),
    pmsg("Breaking on pfc_add(~w)",[Pcopy]),
    break)
   ; true.

pfc_traceRem(pt(_,_)) :-
  % hack for now - never trace triggers.
  !.

pfc_traceRem(P) :-
  (mpred_prop(P,tracing) 
     -> pmsg('Removing ~w.',[P])
      ; true),
  (mpred_prop(P,rem(spying))
   -> (pmsg("Breaking on pfc_rem1(~w)",[P]),
       break)
   ; true).


pfc_trace :- pfc_trace(_).

pfc_trace(Form) :-
  decl_mpred(tracing,Form,true).

pfc_trace(Form,Condition) :- mpred_add_condition(tracing,Form,true,Condition).

pfc_spy(Form) :- pfc_spy(Form,[add,rem],true).

pfc_spy(Form,Modes) :- pfc_spy(Form,Modes,true).

pfc_spy(Form,[add,rem],Condition) :-
  !,
  pfc_spy1(Form,add,Condition),
  pfc_spy1(Form,rem,Condition).

pfc_spy(Form,Mode,Condition) :-
  pfc_spy1(Form,Mode,Condition).

pfc_spy1(Form,Mode,Condition) :-
  mpred_add_condition(spying,Form,Mode, Condition).

pfc_nospy :- pfc_nospy(_,_,_).

pfc_nospy(Form) :- pfc_nospy(Form,_,_).


pfc_nospy(Form,Mode,Condition) :- mpred_remove_condition(spying,Form,Mode,Condition).

pfc_notrace :- pfc_untrace.
pfc_untrace :- pfc_untrace(_).
pfc_untrace(Form) :- mpred_unset(Form,tracing).

% needed:  pfc_traceRule(Name)  ...


% if the correct flag is set, trace exection of Pfc
pfc_trace_msg(Msg,Args) :- user:mpred_prop(*,trace_exec), !,pmsg(Msg, Args).
% pfc_trace_msg(_Msg,_Args).
pfc_trace_msg(W):- pfc_trace_msg('% ~q',[W]),!.

pfc_watch :- decl_mpred(trace_exec,__,true).

pfc_noWatch :- decl_mpred(trace_exec,__,false).

pfc_error(Msg) :-  pfc_error(Msg,[]).

pfc_error(Msg,Args) :- 
  sformat(S,Msg,Args),
  pmsg("ERROR/Pfc: ~s",[S]),  dtrace(S),
  !,
  trace_or_throw(S),!.


%%
%= These control whether or not warnings are printed at all.
%=   pfc_warn.
%=   nopfc_warn.
%%
%= These print a warning message if the flag pfc_warnings is set.
%=   pfc_warn(+Message)
%=   pfc_warn(+Message,+ListOfArguments)
%%

pfc_warn :- decl_mpred(warnings,*,true).

nopfc_warn :- decl_mpred(warnings,*,false).
 
pfc_warn(Msg) :-  pfc_warn(Msg,[]).

pfc_warn(Msg,Args) :- 
  mpred_get(warnings,*,true),
  !,
  sformat(S,Msg,Args),
  pmsg("WARNING/Pfc: ~s",[S]), % dtrace(S),
  !. 

pfc_warn(Msg,Args) :- true,
  sformat(S,Msg,Args),
   pmsg("warning/Pfc: ~s",[S]), % dtrace(S),

pfc_warn(_,_).

%=
%= pfc_warnings/0 sets flag to cause pfc warning messages to print.
%= pfc_no_warnings/0 sets flag to cause pfc warning messages not to print.
%=

pfc_warnings :- mpred_unset(*,warnings), decl_mpred(*,warnings).

pfc_no_warnings :- mpred_unset(*,warnings).



% pfc_file('pfcjust').	% predicates to manipulate justifications.


%   File   : pfcjust.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: predicates for accessing Pfc Justifications.
%   Status: more or less working.
%   Bugs:

%= *** predicates for exploring supports of a hybrid_fact *****


:- use_module(library(lists)).

pfc_just_I_I(F,J) :- pfc_loop_check(pfc_get_support(F,J)).

pfc_just_I_L(L,LJs):-is_list(L),!,maplist(pfc_just_I_L,L,LJs).
pfc_just_I_L(F,Js) :- bagof(J,pfc_just_I_I(F,J),Js).

%= pfc_bases_I_L(P,L) - is true iff L is a list of "base" facts which, taken
%= together, allows us to deduce P.  A pfc_bases_I_L hybrid_fact is an pfc_axiom (a hybrid_fact 
%= added by the user or a raw Prolog hybrid_fact (i.e. one w/o any support))
%= or an assumption.
pfc_bases_I_L(F,[F]) :- (pfc_axiom(F) ; pfc_assumptionBase(F)),!.

pfc_bases_I_L(F,L) :-
  % i.e. (reduce 'append (map 'pfc_bases_I_L (justification f)))
  pfc_just_I_L(F,Js),
  pfc_bases_L_L(Js,L).


%= pfc_bases_L_L(L1,L2) is true if list L2 represents the union of all of the 
%= facts on which some conclusion in list L1 is based.

pfc_bases_L_L([],[]).
pfc_bases_L_L([X|Rest],L) :-
  pfc_bases_I_L(X,Bx),
  pfc_bases_L_L(Rest,Br),
  pfc_union(Bx,Br,L).
	
pfc_axiom(P) :- spft(P,FT,FT).

%= an pfc_assumptionBase/1''s G was a failed goal, i.e. were assuming that our failure to 
%= prove P is a proof of not(P)

pfc_assumptionBase(P) :- pfc_from_negation(P,_).
   
%= pfc_assumption_I_L(X,As) if As is a set of assumptions which underly X.

pfc_assumption_I_L(X,[X]) :- pfc_assumptionBase(X).
pfc_assumption_I_L(X,[]) :- pfc_axiom(X).
pfc_assumption_I_L(X,L) :-
  pfc_just_I_L(X,Js),
  pfc_assumption_L_L(Js,L).

pfc_assumption_L_L([],[]).
pfc_assumption_L_L([X|Rest],L) :-
  pfc_assumption_I_L(X,Bx),
  pfc_assumption_L_L(Rest,Br),
  pfc_union(Bx,Br,L).  


%= pfc_proofTree(P,T) the proof tree for P is T where a proof tree is
%= of the form
%=
%=     [P , J1, J2, ;;; Jn]         each Ji is an independent P justifier.
%=          ^                         and has the form of
%=          [J11, J12,... J1n]      a list of proof trees.


% pfc_child(P,Q) is true iff P is an immediate justifier for Q.
% mode: pfc_child(+,?)

pfc_child(P,Q) :-
  pfc_get_support(Q,(P,_)).

pfc_child(P,Q) :-
  pfc_get_support(Q,(_,Trig)),
  pfc_type(Trig,trigger),
  pfc_child(P,Trig).

pfc_children(P,L) :- bagof(C,pfc_child(P,C),L).

% pfc_descendant(P,Q) is true iff P is a justifier for Q.

pfc_descendant(P,Q) :- 
   pfc_descendant1(P,Q,[]).

pfc_descendant1(P,Q,Seen) :-
  pfc_child(X,Q),
  (\+ member(X,Seen)),
  (P=X ; pfc_descendant1(P,X,[X|Seen])).
  
pfc_descendants(P,L) :- 
  bagof(Q,pfc_descendant1(P,Q,[]),L).



% pfc_file('pfcwhy').	% interactive exploration of justifications.



%   File   : pfcwhy.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated:
%   Purpose: predicates for interactively exploring Pfc justifications.

% ***** predicates for brousing justifications *****

:-dynamic(pfc_whyMemory1/2).

pfc_why :- 
  pfc_whyMemory1(P,_),
  pfc_why(P).


pfc_why(N) :-
  number(N),
  !,
  pfc_whyMemory1(P,Js),
  pfc_whyCommand(N,P,Js).

pfc_why(P) :-
  must((pfc_just_I_L(P,Js),
  retractall(pfc_whyMemory1(_,_)),
  db_assert(pfc_whyMemory1(P,Js)))),
  pfc_whyBrouse(P,Js).

pfc_why1(P) :-
  pfc_just_I_L(P,Js),
  pfc_whyBrouse(P,Js).

pfc_whyBrouse(P,Js) :-
  pfc_showJustifications(P,Js),
  pfc_askUser(' >> ',Answer),
  pfc_whyCommand(Answer,P,Js).

pfc_whyCommand(q,_,_) :- !.
pfc_whyCommand(h,_,_) :- 
  !,
  fmt("~n
Justification Brouser Commands:
 q   quit.
 N   focus on Nth pfc_just_I_L.
 N.M brouse step M of the Nth pfc_just_I_L
 u   up a level
",
[]).

pfc_whyCommand(N,_P,Js) :-
  float(N),
  !,
  pfc_selectJustificationNode(Js,N,Node),
  pfc_why1(Node).

pfc_whyCommand(u,_,_) :-
  % u=up
  !.

pfc_command(N,_,_) :-
  integer(N),
  !,
  fmt("~w is a yet unimplemented command.",[N]),
  fail.

pfc_command(X,_,_) :-
 fmt("~w is an unrecognized command, enter h. for help.",[X]),
 fail.
  
pfc_showJustifications(P,Js) :-
  fmt("Justifications for ~w:",[P]),
  must(pfc_showJustification1(Js,1)).

pfc_showJustification1([],_).

pfc_showJustification1([J|Js],N) :-
  % show one justification and recurse.
  nl,
  must(pfc_showJustifications2(J,N,1)),
  N2 is N+1,
  pfc_showJustification1(Js,N2).

pfc_showJustifications2([],_,_).

pfc_showJustifications2([C|Rest],JustNo,StepNo) :- 
  copy_term(C,CCopy),
  numbervars(CCopy,0,_),
  fmt0("    ~w.~w ~w",[JustNo,StepNo,CCopy]),
  StepNext is 1+StepNo,
  pfc_showJustifications2(Rest,JustNo,StepNext).

pfc_askUser(Msg,Ans) :-
  fmt0(Msg),
  read(Ans).

pfc_selectJustificationNode(Js,Index,Step) :-
  JustNo is integer(Index),
  lists:nth1(JustNo,Js,Justification),
  StepNo is 1+ integer(Index*10 - JustNo*10),
  lists:nth1(StepNo,Justification,Step).


 
%   File   : compute_resolve.pl
%   Author : Douglas Mies
%   Updated:  Today
%   Purpose: predicates for interactively exploring Pfc justifications.



compute_resolve(NewerP,OlderQ,S1,S2,(pfc_rem3(OlderQ),pfc_add(NewerP),pfc_rem1(conflict(NewerP)))):-wdmsg(compute_resolve(S1>S2)).

compute_resolve(NewerP,OlderQ,Resolve):-
   pfc_just_I_L(NewerP,S1),
   pfc_just_I_L(OlderQ,S2),
   compute_resolve(NewerP,OlderQ,S1,S2,Resolve).


pfc_why_non_interactive(P):- pfc_maptree(pfc_why_non_interactive1,P,[]).
pfc_why_non_interactive1(P):-
% retractall(pfc_whyMemory1(_,_)),
  must((pfc_just_I_L(P,Js),
  db_assert(pfc_whyMemory1(P,Js)))),
  pfc_showJustifications([P],[Js]).






%pfc_use_file:-


pfc_is_rule_or_fact(G):-pfc_data_type(G,T),!,member(T,[hybrid_rule,hybrid_fact]).

:-thread_local(thlocal:pfc_no_mark/0).
:-export(thlocal:pfc_no_mark/0).
pfc_mark(_, _):-thlocal:pfc_no_mark,!.
pfc_mark(G,As):-call(must(pfc_maptree(pfc_mark1,G,[As]))),!.
pfc_mark1(G,_):-pfc_is_unbound(G),!.
pfc_mark1(G,_):-is_true(G),!.
pfc_mark1(G,_):-pfc_is_unbound(G),!.
pfc_mark1(F/A,As):- atom(F),!,decl_mpred(F/A,As),!.
pfc_mark1(neg(G),As):- pfc_is_bound(G),!,pfc_mark1(G,As).
pfc_mark1(not(G),As):- pfc_is_bound(G),!,pfc_mark1(G,As).
pfc_mark1(G,code):-!,get_functor(G,F,A),must(decl_mpred(core,F/A,true)).
pfc_mark1(G,pfc_state):-!,get_functor(G,F,A),decl_mpred(F/A,pfc_state).
pfc_mark1({Code},As):- !,pfc_mark_P(Code).
pfc_mark1(F,As):-atom(F),mpred_prop(F,pfc_dont_expand),!,pmsg(todo(warn(wont_pfc_change1(F,As)))).
pfc_mark1(F,As):-atom(F),!,ignore((user:arity(F,A),!,pfc_mark(F/A,As))).
pfc_mark1(G,_):- \+(compound(G)),!.
pfc_mark1(G,_):- \+ pfc_is_rule_or_fact(G),!.
pfc_mark1(forall(G1,G2),As):-!,pfc_mark(G1,As),pfc_mark(G2,As).
pfc_mark1(F/A,As):-!,pfc_maptree(As,pfc_lambda((pfc_mark(F/A),ARG),ARG),[As]).
pfc_mark1(G,As):-pfc_get_head_body(G,H,B),H\=@=G,pfc_mark(H,As),!,pfc_mark(B,pfc_is_watching),!.
pfc_mark1(G,As):-decl_mpred(As,G,true).

:- op(0,fx,pfc_mark_S).
pfc_mark_S(G):-(pfc_mark(G,pfc_state)).
pfc_mark_P(G):-(pfc_mark(G,code)).

pfc_mark_W(G):-(pfc_mark(G,pfc_is_watching)).
pfc_mark_C(G):-(pfc_mark(G,pfc_control)).
pfc_mark_F(G):-(pfc_mark(G,pfc_is_fwc)). 
pfc_mark_B(G):-(pfc_mark(G,pfc_is_bc)).
:- op(1150,fx,pfc_mark_S).



pfc_ify(F/A):- export(F/A),((pfc_wont_assert(F/A)->pfc_mark_P(F/A);pfc_mark_S(F/A))),!. % module_transparent(F/A).


% :- (context_module(pfc)->must((if_startup_script,pfc_use_module(pfc));(loading_module(M, use_module(pfc)),pfc_use_module(M)));true).

% * = any and all
% -,0 = code directive
% +,1 = code hybrid_fact
% :,2 = code hybrid_rule
%  ?  = any prolog code
%  ^  = non prolog code 
%  arity1Pred 

% specify how code loads {code_compile;code_directive;pfc_add;callable(1) }
:- decl_mpred(load,code,code_compile).
:- decl_mpred(load,*,code_compile).
:- decl_mpred(load,(:),*).
:- decl_mpred(load,(+),*).
:- decl_mpred(load,(-),*).
:- decl_mpred(load,(^),pfc_add).
:- decl_mpred(load,pfc_is_fwc,pfc_add).
:- decl_mpred(load,pfc_is_bc,pfc_add).
:- decl_mpred(load,pfc_is_controlled,pfc_add).
:- decl_mpred(load,member/2,(code_compile)).
:- decl_mpred(load,(=>),pfc_add).
:- decl_mpred(load,(<=)/2,pfc_add).
:- decl_mpred(load,(:-)/1,(^)).
:- decl_mpred(load,(:-)/2,(:)).

% specify how to tranform a neck when asserting/retracting
%   {(=);(=>);(<=);(<=>);(callable(2)) }
:- decl_mpred(neck,*,(=)).
:- decl_mpred(neck,code,(:-)).
:- decl_mpred(neck,(:),*).
:- decl_mpred(neck,(+),*).
:- decl_mpred(neck,(-),*).
:- decl_mpred(neck,pfc_is_fwc,(=>)).
:- decl_mpred(neck,pfc_is_bc,(<=)).
:- decl_mpred(neck,member/2,(=)).
:- decl_mpred(neck,lists:member/2,(=)).

% All predciates that are in pfc module are 'pfc_home'ed
% :- decl_mpred(user:_, pfc_home).

%== initialization of global assertons 

%= pfc_tms is one of {none,local,cycles} and controles the tms alg.
:- decl_mpred(pfc_tms,*,cycles).

% Pfc Search strategy. (pfc_search,*,X) where X is one of {direct,depth,breadth}
:- decl_mpred(pfc_search,*,direct).

% :-show_module(pfc_consulting).

% :- (M=pfc,forall(module_predicate(M,_,F,A),doall((\+((current_module(O),(O \= M),module_predicate(O,_,F,A))),M:export(M:F/A))))).

% :- asserta(tlbugger:no_colors).


% :-forall((current_predicate(F/A),atom_concat('pfc_',_,F)),pfc_ify(F/A)).

% :- pfc_mark_S(pfc_database/1).
%:- pfc_mark_S(pfc Default/1).
:- pfc_mark_P((pfc_local/1)).
:- pfc_mark_S(('bt')/2).
:- pfc_mark_S(('nt')/3).
:- pfc_mark_S(('pfc_t_pkey')/2).
:- pfc_mark_S(('pt')/2).
:- pfc_mark_S(('::::')/2).
:- pfc_mark_S(('<=')/2).
:- pfc_mark_S(('=>')/1).
:- pfc_mark_S(('<=>')/2).
:- pfc_mark_S(('=>')/2).
:- pfc_mark_S(('=>')/1).
:- pfc_mark_S(pfc_action/2).
:- pfc_mark_S(pfc_haltSignal/1).
:- pfc_mark_S(pfc_queue/2).
:- pfc_mark_S(pfc_select/2).
:- pfc_mark_S(pfc_undo_method/2).
:- pfc_mark_S(spft/3).
:- pfc_mark_S(mpred_db/3).
:- pfc_mark_S(user:hasInstance/2).

:-decl_mpred_hybrid(user:ruleRewrite/2).
:-decl_mpred_hybrid(user:hasInstance/2).

was_isa(G,I,C):-call((current_predicate(mpred_types_loaded/0),if_defined(was_isa(G,I,C)))).


:- forall(context_module(Mod),forall(module_predicate(Mod,_,F,A),pfc_ify(F/A))).

:- forall(pfc_db_type(F/A,hybrid_rule),decl_mpred(F/A,pfc_state)).

:- forall(pfc_db_type(H, hybrid_rule),forall(db_uclause(H,true),pfc_add(H))).


% :- pfc_add((mpred_db(pfc_is_bc,F/A,true) => {make_functor(PHead,F,A),assertz_if_new(PHead:-pfc_bc_only(PHead)),pfc_assume_dynamic(F/A)})).
% :- pfc_add((mpred_db(Prop,Form,true)=>{decl_mpred(Form,Prop)})).

% :- if_startup_script(with_assertions(thlocal:pfc_module_expansion(_),prolog)).

% a pretty basic conflict.
:- pfc_add(({pfc_literal(P)}, neg(P), P) => conflict(P)).

% a conflict triggers a Prolog action to resolve it.
:- pfc_add((conflict(C) => {must(resolveConflict(C))})).

:-multifile(resolveConflict/1).
:-dynamic(resolveConflict/1).
:-multifile(resolveConflict/1).
:-dynamic(resolverConflict_robot/1).
:-export(resolverConflict_robot/1).
% this isnt written yet.
resolveConflict(C) :- forall(pfc_must(pfc_nf1_negation(C,N)),pfc_must(pfc_why_non_interactive(N))),pfc_must(pfc_why_non_interactive(C)), if_defined(resolverConflict_robot(C)),!.
resolveConflict(C) :- forall(pfc_must(pfc_nf1_negation(C,N)),forall(compute_resolve(C,N,TODO),debugOnError(TODO))),!.
resolveConflict(C) :- forall(pfc_must(pfc_nf1_negation(C,N)),forall(compute_resolve(C,N,TODO),debugOnError((TODO)))),!.
resolveConflict(C) :- pfc_must((pfc_rem3(C),format("~nRem-3 with conflict ~w~n", [C]),pfc_run)).
resolveConflict(C) :-
  format("~NHalting with conflict ~w~n", [C]),   
 pfc_must(pfc_halt(conflict(C))).



:- style_check(+discontiguous).
:- style_check(-discontiguous).



:- source_location(S,_),forall(source_file(H,S),(functor(H,F,A),export(F/A),module_transparent(F/A))).


:- decl_mpred(*,pfc_control).

trace_all:- forall((M=prolog,current_predicate(M:F/A)),trace(M:F/A,+all)),
    forall((M=system,current_predicate(M:F/A)),trace(M:F/A,+all)).

% with_assertions(thlocal:consulting_sources..)

:- ensure_loaded(logicmoo(mpred/logicmoo_types)).
:- ensure_loaded(logicmoo(mpred/logicmoo_argtypes)).
:- ensure_loaded(logicmoo(mpred/logicmoo_call)).
:- ensure_loaded(logicmoo(mpred/logicmoo_database)).

:-ensure_loaded(library(statistics)).

:- user:ensure_loaded(library('mpred/mpred_builtins.pfc')).


:-pfc_begin.
(default(P)/pfc_literal(P))  =>  (~neg(P) => P).

default((P => Q))/pfc_literal(Q) => (P, ~neg(Q) => Q).

% birds fly by default.
=> default((bird(X) => fly(X))).

% here's one way to do an isa hierarchy.
% isa = subclass.
:-(pfc_add((
pisa(C1,C2) =>
  {P1 =.. [C1,X],
    P2 =.. [C2,X]},
  (P1 => P2)))).

:-(pfc_add((=> pisa(canary,bird)))).
=> pisa(penguin,bird).

% penguins do not fly.
penguin(X) => neg(fly(X)).

% tweety is a canary.
=> canary(tweety).

% chilly is a penguin.
=> penguin(chilly).

=> fly(chilly).

:-endif.

end_of_file.

