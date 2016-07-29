/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_preddefs.pl
:- module(logicmoo_util_preddefs,
          [ call_if_defined/1,
            convert_to_dynamic/1,
            convert_to_dynamic/3,
            current_predicate_module/2,
            context_module_of_file/1,
          call_from_module/2,
          with_source_module/2,
            def_meta_predicate/3,
            dynamic_if_missing/1,
            dynamic_multifile/1,
            dump_break/0,
            (dynamic_safe)/1,
            (dynamic_safe)/3,
            op_safe/3,
            dynamic_transparent/1,
            fill_args/2,
            get_module_of/2,
            get_module_of_4/4,
            get_pi/2,
            only_3rd/4,
            make_transparent/4,
            multi_transparent/1,
            must_pi/1,
            p_predicate_property/2,
            to_canonical_mpi/2,
            get_fa/3,
            pred_prop/3,
            pred_prop/4,
            rebuild_as_dyn/4,
            rebuild_pred_into/3,
            rebuild_pred_into/4,
            remove_pred/3,
            is_static_predicate/1,
            is_static_predicate_3/3,
            save_was/4,
             (was_dynamic)/1,
             (was_multifile)/1,
             (was_module_transparent)/1,
             (was_export)/1,
            with_mfa/2,            
            with_pfa/2,
            add_mi/3,
            with_pfa/4,
            make_module_name00/2,

            is_static_why/5,
            defined_predicate/1,
            m_m_fa_to_m_p_fa/4,
            m_fa_to_m_p_fa/2,
            with_pfa_single/4,
            with_pfa_group/4,
            with_mfa_of/5,
            with_pi/2,
            with_pi_selected/4,
            with_pi_stub/5
          ]).
:- meta_predicate
        call_if_defined(0),
        def_meta_predicate(0, +, +),
        ( (with_pfa_group(3,+,+,+))),
        ( (dynamic_safe(+)) ),
         with_pfa_single(3,?,?,?),
        ( dynamic_safe(+, +, +) ),
        get_module_of(0, -),
        get_module_of_4(0, +, +, -),
        must_pi(0),
        rebuild_pred_into(0, 1, ?),
        rebuild_pred_into(0, 0, 1, ?),
        is_static_predicate_3(+, +, +),
        is_static_predicate(+),
        with_mfa(0, 3),
        with_mfa_of(3, +, +, +, +),
        with_pi(0, 4),
        with_pi_selected(+, +, +, +),
        with_pi_stub(+, +, +, +, 0),
        context_module_of_file(-),
         with_pfa(1,*,+,+),
         with_pfa(1,+),
         only_3rd(1,*,*,*).


:- module_transparent
        convert_to_dynamic/1,
        convert_to_dynamic/3,
        current_predicate_module/2,
   call_from_module/2,
   with_source_module/2,
        dynamic_if_missing/1,
        dynamic_multifile/1,
        dynamic_transparent/1,
        only_3rd/4,
        fill_args/2,
        get_pi/2,
        make_transparent/4,
        multi_transparent/1,
        p_predicate_property/2,
        pred_prop/3,
        pred_prop/4,
        rebuild_as_dyn/4,
        remove_pred/3,
        is_static_predicate/1.


:- include('logicmoo_util_header.pi').

:- use_module(logicmoo_util_shared_dynamic).

%% with_source_module( +NewModule, :GoalGoal) is semidet.
%
% Call Using Source Module.
%
:- meta_predicate with_source_module(+,0).
with_source_module(OldSModule, Goal ):- '$current_source_module'(OldSModule),!,OldSModule:Goal.
with_source_module(NewModule,Goal):-  
   '$current_source_module'(OldModule),
    NewModule:setup_call_cleanup_each(
      '$set_source_module'(NewModule),
          Goal, 
      '$set_source_module'(OldModule)).

%% call_from_module( +NewModule, :GoalGoal) is semidet.
%
% Call Using Module.
%
:- meta_predicate call_from_module(+,+).

call_from_module(NewModule,( H:-B ) ):- !, must(nonvar(H)),call_from_module(NewModule, clause_asserted_call(H,B) ).
call_from_module(OldSModule, Goal ):- '$current_source_module'(OldSModule),!,OldSModule:Goal.
% call_from_module(NewModule,Goal):- sanity((atom(NewModule),nonvar(Goal))),fail.
call_from_module(NewModule,Goal):-
   '$current_typein_module'(OldModule),
   '$current_source_module'(OldSModule),
   strip_module(Goal,_,Call),
    setup_call_cleanup_each(
      ('$set_typein_module'(NewModule),'$set_source_module'(NewModule)), 
      NewModule:Call,
      ('$set_source_module'(OldSModule),'$set_typein_module'(OldModule))).



dump_break:- prolog_stack:backtrace(8000),dtrace. % system:dbreak.


:- meta_predicate only_3rd(1,*,*,*).
:- meta_predicate with_pfa(1,+).
:- meta_predicate with_pfa(1,+,+,+).

:- export(op_safe/3).
op_safe(A,B,C):-!, (current_op(_,B,C)->true;op(A,B,C)).


:- reexport(logicmoo_util_with_assertions).
:- reexport(logicmoo_util_dmsg).

% ----------
:- export(with_pi/2).
:- module_transparent(with_pi/2).
% = :- meta_predicate(with_pi(0,4)).


%= 	 	 

%% with_pi( :GoalP, :PRED4Pred3) is semidet.
%
% Using Predicate Indicator.
%
with_pi(PredMt:(P1,P2),Pred3):-!,'@'(( with_pi(PredMt:P1,Pred3),with_pi(PredMt:P2,Pred3) ),PredMt).
with_pi(PredMt:P,Pred3):-source_context_module(CallerMt),!,with_pi_selected(CallerMt,PredMt,P,Pred3),!.
with_pi([],_):-!.
with_pi((P1,P2),Pred3):-!, with_pi(P1,Pred3),with_pi(P2,Pred3).
with_pi(P,Pred3):-source_context_module(CallerMt),with_pi_selected(CallerMt,CallerMt,P,Pred3),!.

:- export(with_pi_selected/4).
% = :- meta_predicate(with_pi_selected(+,+,+,+)).

%= 	 	 

%% with_pi_selected( +CallerMt, +PredMt, +P, +Pred3) is semidet.
%
% Using Predicate Indicator Selected.
%

% 11 will be our signal of "any arity" (naughty)
with_pi_selected(CallerMt,PredMt, F/A ,Pred3):- var(A),!,
  forall(between(1,11,A),must(with_pi_selected(CallerMt,PredMt, F/A ,Pred3))).

with_pi_selected(CallerMt,PredMt, F//A ,Pred3):- var(A),!,
  forall(between(1,11,A),must(with_pi_selected(CallerMt,PredMt, F//A ,Pred3))).


with_pi_selected(CallerMt, PredMt,[P|L],Pred3):-!,with_pi_selected(CallerMt,PredMt,P,  Pred3),with_pi_selected(CallerMt,PredMt,L,Pred3).
with_pi_selected(CallerMt,PredMt,(P,L),Pred3):-!,with_pi_selected(CallerMt,PredMt,P,  Pred3),with_pi_selected(CallerMt,PredMt,L,Pred3).
with_pi_selected(_CallerMt,_M,[]  ,_Pred3):-!.
with_pi_selected(CallerMt,PredMt,[P]  ,Pred3):-!,with_pi_selected(CallerMt,PredMt,P,  Pred3).
with_pi_selected(CallerMt,_, PredMt:F//A,Pred3):-Ap2 is A+2, !,with_pi_selected(CallerMt,PredMt,F/Ap2,Pred3).
with_pi_selected(CallerMt,PredMt, F//A ,Pred3):-Ap2 is A+2,!,functor_safe(P,F,A),  with_pi_stub(CallerMt,PredMt,P,F/Ap2,Pred3).
with_pi_selected(CallerMt,_, PredMt:F/A,Pred3):-!,with_pi_selected(CallerMt,PredMt,F/A,Pred3).
with_pi_selected(CallerMt,_, PredMt:P ,Pred3):-!,with_pi_selected(CallerMt,PredMt,P,  Pred3).
with_pi_selected(CallerMt,PredMt, F/A ,Pred3):-!,functor_safe(P,F,A),  with_pi_stub(CallerMt,PredMt,P,F/A,Pred3).
with_pi_selected(CallerMt,PredMt, P ,Pred3):-  functor_safe(P,F,A), with_pi_stub(CallerMt,PredMt,P,F/A,Pred3).



% ----------

% = :- meta_predicate(must_pi(0)).

%= 	 	 

%% must_pi( :GoalX) is semidet.
%
% Must Be Successfull Predicate Indicator.
%
must_pi(X):-X,!.
must_pi(X):-dtrace,X,!.

:- export(with_pi_stub/5).
% = :- meta_predicate(with_pi_stub(+,+,+,+,0)).

%= 	 	 

%% with_pi_stub( +CallerMt, +PredMt, +P, +FA, :GoalPred3) is semidet.
%
% Using Predicate Indicator Stub.
%
with_pi_stub(CallerMt, PredMt,P, F//A , PM:Pred3):- ((integer(A),atom(PredMt),atom(F),Ap2 is A+2, functor_safe(P,F,Ap2))),must_pi(PM:call(Pred3,CallerMt, PredMt,P,F/Ap2)),!.
with_pi_stub(CallerMt, PredMt,P, F/A , PM:Pred3):- ((integer(A),atom(PredMt),atom(F),functor_safe(P,F,A))),
  must_pi(PM:call(Pred3,CallerMt, PredMt,P,F/A)),!.
with_pi_stub(CallerMt, PredMt,P, F//A , Pred3):- ((integer(A),atom(PredMt),atom(F),Ap2 is A+2,functor_safe(P,F,Ap2))),must_pi(call(Pred3,CallerMt, PredMt,P,F/Ap2)),!.
with_pi_stub(CallerMt, PredMt,P, F/A , Pred3):- ((integer(A),atom(PredMt),atom(F),functor_safe(P,F,A))),
   must_pi(call(Pred3,CallerMt, PredMt,P,F/A)),!.
%with_pi_stub(CallerMt, PredMt,P, F/A ,_: Pred3):- ((integer(A),atom(PredMt),atom(F),functor_safe(P,F,A))),  must_pi(call(Pred3,CallerMt, PredMt,P,F/A)),!.
%with_pi_stub(CallerMt, PredMt,P, F/A ,CP: Pred3):-!, must_pi(CP:call(Pred3,CallerMt, PredMt,P,F/A)),!.
%with_pi_stub(CallerMt, PredMt,P, F/A , Pred3):-!, must_pi(call(Pred3,CallerMt, PredMt,P,F/A)),!.

with_pi_stub(CallerMt, PredMt,P,FA,Pred3):- trace_or_throw(invalide_args(CallerMt, PredMt,P,FA,Pred3)).
% ----------

:- export(with_mfa/2).
:- module_transparent(with_mfa/2).
% = :- meta_predicate(with_mfa(0,3)).

%= 	 	 

%% with_mfa( :GoalP, :PRED3Pred3) is semidet.
%
% Using Module-functor-arity.
%
with_mfa(P  ,Pred3):- with_pi(P,with_mfa_of(Pred3)).

:- module_transparent(with_mfa_of/5).
% = :- meta_predicate(with_mfa_of(3,+,+,+,+)).

%= 	 	 

%% with_mfa_of( :PRED3Pred3, +CallerMt, +PredMt, +P, +F) is semidet.
%
% Using Module-functor-arity Of.
%
with_mfa_of(Pred3,_CallerMt,PredMt,_P,F//A):- Ap2 is A+2, PredMt:call(Pred3,PredMt,F,Ap2).
with_mfa_of(Pred3,_CallerMt,PredMt,_P,F/A):-PredMt:call(Pred3,PredMt,F,A).

% ----------


:- module_transparent(make_transparent/4).
:- export(make_transparent/4).


%= 	 	 

%% make_transparent( ?CallerMt, ?PredMt, ?PI, :TermF) is semidet.
%
% Make Transparent.
%
make_transparent(_CallerMt,PredMt,_PI,F/0):-!, compound_name_arity(C,F,0), PredMt:meta_predicate(C).
make_transparent(CallerMt,_,PI,PredMt:F/A):-!,make_transparent(CallerMt,PredMt,PI,F/A).
make_transparent(_CallerMt,PredMt,PI,F/A):-
   (((var(PI)->functor_safe(PI,F,A);true),
   PredMt:module_transparent(F/A),
   fill_args(PI,('?')),!,
   dbgsubst(PI, (^),(^),PI1),
   dbgsubst(PI1,(0),(0),PI2),
   dbgsubst(PI2,(:),(:),PI3),
   (compound(PI3) -> PredMt:meta_predicate(PI3) ; true))).

% ----------


:-module_transparent(context_module_of_file/1).

%= 	 	 

%% context_module_of_file( -CallerMt) is semidet.
%
% Context Module Of File.
%
context_module_of_file(CallerMt):- prolog_load_context(source,_), prolog_load_context(module,CallerMt).
context_module_of_file(CallerMt):- prolog_load_context(source,F), make_module_name00(F,CallerMt),current_module(CallerMt0),CallerMt==CallerMt0,!.
context_module_of_file(CallerMt):-  '$set_source_module'(CallerMt,CallerMt),!.
context_module_of_file(CallerMt):- source_context_module(CallerMt),!.

%% make_module_name00( ?P, ?Module) is semidet.
%
% Make Module Name.
%
make_module_name00(P,Module):- module_property(Module,file(P)),!.

make_module_name00(P,O):-atom(P),!,file_base_name(P,F),file_name_extension(PredMt,_Ext,F),(PredMt\==F->make_module_name00(PredMt,O);O=PredMt).
make_module_name00(mpred/P,PredMt):-nonvar(P),!,make_module_name00(P,PredMt).
make_module_name00(util/P,PredMt):-nonvar(P),!,make_module_name00(P,PredMt).
make_module_name00(P,PredMt):-must(filematch(P,F)),F\=P,!,make_module_name00(F,PredMt).

:- op(1150,fx,baseKB:dynamic_safe).

%= 	 	 

%% was_dynamic( ?PI) is semidet.
%
% Was Dynamic.
%
was_dynamic(PI):- context_module_of_file(CallerMt),with_pfa_group(save_was(dynamic),CallerMt, baseKB, PI).

%= 	 	 

%% was_export( ?PI) is semidet.
%
% Was Export.
%
was_export(PI):- context_module_of_file(CallerMt),with_pfa_group(save_was(export),CallerMt, baseKB, PI).

%= 	 	 

%% was_module_transparent( ?PI) is semidet.
%
% Was Module Transparent.
%
was_module_transparent(PI):- context_module_of_file(CallerMt),with_pfa_group(save_was(module_transparent),CallerMt, baseKB, PI).

%= 	 	 

%% was_multifile( ?PI) is semidet.
%
% Was Multifile.
%
was_multifile(PI):- context_module_of_file(CallerMt),with_pfa_group(save_was(multifile),CallerMt, baseKB, PI).

:-dynamic(was_was:was_was_once/4).
:-export(was_was:was_was_once/4).
:-multifile(was_was:was_was_once/4).
:-dynamic(was_was:skip_def/2).
:-export(was_was:skip_def/2).
:-multifile(was_was:skip_def/2).


%= 	 	 

%% save_was( ?Was, ?CallerMt, ?PredMt, :TermP) is semidet.
%
% Save Was.
%
save_was(_,_,_,_).
save_was(_,_, PredMt, F/A):- was_was:skip_def(F/A,PredMt),!.
save_was(export,_, PredMt, F/A):- !,retractall(was_was:was_was_once(F/A,PredMt,_,_)),!,assert_if_new(was_was:skip_def(F/A,PredMt)),!.
save_was(module_transparent,_, _, _):- !.
save_was(_,CallerMt, PredMt, F/A):-  
  on_x_cont(PredMt:dynamic(F/A)), on_x_cont(CallerMt:dynamic(F/A)), on_x_cont(PredMt:multifile(F/A)), on_x_cont(CallerMt:multifile(F/A)),fail.
save_was(Was,CallerMt, PredMt, F/A):- !, once(source_location(File,_);File=CallerMt),assert_if_new(was_was:was_was_once(F/A,PredMt,File,Was)),!.
save_was(Was,CallerMt, PredMt, P):- functor(P,F,A), save_was(Was,CallerMt, PredMt, F/A).


:-module_transparent(with_pfa/2).
:-export(with_pfa/2).

%= 	 	 mudDescription kb_dynamic

%% with_pfa( :PRED1With, +PI) is semidet.
%
% Using Pfa.
%
with_pfa(With, PI):- 
  must((context_module_of_file(CallerMt)->with_pfa_group(only_3rd(With),CallerMt, CallerMt, PI))).

:-module_transparent(with_pfa/4).

%= 	 	 

%% with_pfa( :PRED1With, +CallerMt, +PredMt, +PI) is semidet.
%
% Using Pfa.
%
with_pfa(With,CallerMt, PredMt, PI):- context_module_of_file(CallerMt)->with_pfa_group(only_3rd(With),CallerMt, PredMt, PI).

:-module_transparent(m_m_fa_to_m_p_fa/4).

%= 	 	 

%% m_m_fa_to_m_p_fa( ?Decl_mpred_hybrid, ?CallerMt, ?PredMt, ?PI) is semidet.
%
% Module Module Functor-arity Converted To Module F Functor-arity.
%

m_m_fa_to_m_p_fa(Decl_mpred_hybrid,CallerMt,PredMt,FA):- ignore(CallerMt=PredMt),
   var(CallerMt),!,must(call(ereq,defaultAssertMt(CallerMt))),
   m_m_fa_to_m_p_fa(Decl_mpred_hybrid,CallerMt,PredMt,FA).
   

m_m_fa_to_m_p_fa(Decl_mpred_hybrid,CallerMt,PredMt,F/A):- var(A),atom(F),!,
   forall(between(1,11,A),(functor(PI,F,A),CallerMt:call(Decl_mpred_hybrid,PredMt,PI,F/A))).   
m_m_fa_to_m_p_fa(Decl_mpred_hybrid,CallerMt,PredMt,F/A):-
   atom(F),sanity(integer(A)),functor(PI,F,A),CallerMt:call(Decl_mpred_hybrid,PredMt,PI,F/A).
m_m_fa_to_m_p_fa(Decl_mpred_hybrid,CallerMt,PredMt,PI):-functor(PI,F,A),CallerMt:call(Decl_mpred_hybrid,PredMt,PI,F/A).

:-module_transparent(m_fa_to_m_p_fa/2).

%= 	 	 

%% m_fa_to_m_p_fa( ?Decl_mpred_hybrid, ?FA) is semidet.
%
% Module Functor-arity Converted To Module F Functor-arity.
%
m_fa_to_m_p_fa(Decl_mpred_hybrid,PredMt:PredMt:FA):- !, m_m_fa_to_m_p_fa(Decl_mpred_hybrid,PredMt,PredMt,FA).
m_fa_to_m_p_fa(Decl_mpred_hybrid,PredMt:FA):- !, m_m_fa_to_m_p_fa(Decl_mpred_hybrid,PredMt,PredMt,FA).
m_fa_to_m_p_fa(Decl_mpred_hybrid,FA):- call(ereq,defaultAssertMt(PredMt)), m_m_fa_to_m_p_fa(Decl_mpred_hybrid,PredMt,PredMt,FA).

 
:-module_transparent(only_3rd/4).

%= 	 	 

%% only_3rd( :PRED1With, ?CallerMt, ?PredMt, ?PI) is semidet.
%
% Only 3rd.
%
only_3rd(With,CallerMt, PredMt, PI):- 
   var(CallerMt),nonvar(PredMt),!,
   must(\+ \+ only_3rd(With,PredMt, PredMt, PI)).
only_3rd([],_CallerMt, _M, _PI):- !.
only_3rd([With|List],CallerMt, PredMt, PI):- is_list(List),!,only_3rd(With,CallerMt, PredMt, PI),only_3rd(List,CallerMt, PredMt, PI).
only_3rd(With,user, user, PI):-!, show_call(with_pi,call(With,PI)).
only_3rd(WithList,CallerMt, MOD, F/A):- !,only_3rd(WithList,CallerMt, MOD , MOD:F/A).
only_3rd(With,_, _, CallerMt:PI):-!, show_call(with_pi,call(With,CallerMt:PI)).
only_3rd(With,CallerMt, user, PI):-!, show_call(with_pi,call(With,CallerMt:PI)).
% only_3rd(With,user, PredMt, PI):-!, show_call(with_pi,call(With,PredMt:PI)).
only_3rd(With,CallerMt, PredMt, PI):- CallerMt:call(With,PredMt:PI).

:- multifile(baseKB:mpred_is_decl_called/4).
:- dynamic(baseKB:mpred_is_decl_called/4).

:- meta_predicate(with_pfa_group(3,+,+,+)).
:- module_transparent(with_pfa_group/4).
:- export((with_pfa_group)/4).


%= 	 	 

%% to_canonical_mpi( :TermP, ?MPI) is semidet.
%
% Converted To Canonical Module Predicate Indicator.
%
to_canonical_mpi(PredMt:FA,MPI):-atom(PredMt),!,to_canonical_mpi(FA,PI),add_mi(PredMt,PI,MPI).
to_canonical_mpi((PredMt:F)/A,MPI):- integer(A),!,functor(PI,F,A),add_mi(PredMt,PI,MPI).
to_canonical_mpi((PredMt:F)//A2,MPI):-integer(A),!,A is A2 + 2, functor(PI,F,A),add_mi(PredMt,PI,MPI).
to_canonical_mpi(F/A,MPI):- functor(P,F,A), functor(P,F,A),strip_module(P,PredMt,PI),add_mi(PredMt,PI,MPI).
to_canonical_mpi(F//A2,MPI):- A is A2 + 2, functor(P,F,A),strip_module(P,PredMt,PI),add_mi(PredMt,PI,MPI).
to_canonical_mpi(P,MPI):- strip_module(P,PredMt,PI),add_mi(PredMt,PI,MPI).



%% get_fa( +PI, ?F, ?A) is semidet.
%
% Get Functor-arity.
%
get_fa(FA,F,A):- var(FA),!,trace_or_throw(var_get_fa(FA,F,A)).
get_fa(_:FA,F,A):-!,get_fa(FA,F,A).
get_fa(F/A,F,A):-!.
get_fa(F//A2,F,A):-A is A2+2.
get_fa(PI,PI,0):- atomic(PI),!.
%get_fa(FA,F,A):- functor(FA,F,A),!.
get_fa(Mask,F,A):-get_functor(Mask,F,A).


%= 	 	 

%% add_mi( ?PredMt, ?P, :TermM) is semidet.
%
% Add Mi.
%
add_mi(PredMt,P,PredMt:PI):-strip_module(P,_,PI).


%= 	 	 

%% with_pfa_group( :PRED3With, +CallerMt, +PredMt, +F) is semidet.
%
% Using Pfa Group.
%
with_pfa_group(With,CallerMt, _, _:(PredMt:F)/A ):- must(atom(F)), !,with_pfa_group(With,CallerMt, PredMt,F/A ).
with_pfa_group(With,CallerMt, _, (PredMt:F)/A ):- must(atom(F)), !,with_pfa_group(With,CallerMt, PredMt,F/A ).
with_pfa_group(With,CallerMt, _, PredMt:F/A ):- must(atom(F)), !,with_pfa_group(With,CallerMt, PredMt,F/A ).
with_pfa_group(With,CallerMt, _, PredMt:PI ):- must(nonvar(PI)),!, with_pfa_group(With,CallerMt,PredMt,PI).
with_pfa_group(With,CallerMt, PredMt, [A] ):-!,with_pfa_group(With,CallerMt,PredMt, A ).
with_pfa_group(With,CallerMt, PredMt, [A|B] ):-!,with_pfa_group(With,CallerMt,PredMt, A ),with_pfa_group(With,CallerMt,PredMt, B ).
with_pfa_group(With,CallerMt, PredMt, (A,B) ):-!,with_pfa_group(With,CallerMt,PredMt, A ),with_pfa_group(With,CallerMt,PredMt, B ).
with_pfa_group(With,CallerMt, PredMt, ([F1|FL])/A):- !,with_pfa_single(With,CallerMt, PredMt, F1/A),with_pfa_group(With,CallerMt, PredMt, FL/A).
with_pfa_group(With,CallerMt, PredMt, (F1,FL)/A):- !,with_pfa_single(With,CallerMt, PredMt, F1/A),with_pfa_group(With,CallerMt, PredMt, FL/A).

with_pfa_group(With,CallerMt, PredMt, F):- atom(F),!,must(with_pfa_single(With,CallerMt, PredMt, F/0)).
with_pfa_group(With,CallerMt, PredMt, F/A):- !,must(with_pfa_single(With,CallerMt, PredMt, F/A)).
with_pfa_group(With,CallerMt, PredMt, PI):- must(with_pfa_single(With,CallerMt, PredMt, PI)).

:-export(with_pfa_single/4).
:-module_transparent(with_pfa_single/4).

%= 	 	 

%% with_pfa_single( :PRED3With, ?CallerMt, ?PredMt, ?FA) is semidet.
%
% Using Pfa Single.
%
with_pfa_single(With,CallerMt, PredMt, FA):- baseKB:mpred_is_decl_called(With,CallerMt, PredMt, FA),!.
% with_pfa_single(With,_CallerMt, PredMt, FA):- to_canonical_mpi(FA,P), \+ \+ current_predicate(_,_:P), ignore(once((must((current_predicate(_,RM:P),\+ predicate_property(RM:P,imported_from(_)), PredMt==RM))))),fail.
with_pfa_single([], _CallerMt, _M, _FA):-!.
with_pfa_single([With|List],CallerMt, PredMt, FA):- is_list(List),!,with_pfa_single(With,CallerMt, PredMt, FA),!,with_pfa_single(List,CallerMt, PredMt, FA).
with_pfa_single(With,CallerMt, PredMt, FA):- baseKB:mpred_is_decl_called(With,CallerMt0, M0, FA),M0\==PredMt, dmsg(with_pfa_single(With,CallerMt->CallerMt0, PredMt->M0, FA)),!,asserta(baseKB:mpred_is_decl_called(With,CallerMt, PredMt, FA)),!.
with_pfa_single(With,CallerMt, PredMt, FA):- asserta(baseKB:mpred_is_decl_called(With,CallerMt, PredMt, FA)), must(call(With,CallerMt, PredMt, FA)).


% ----------


:- export(fill_args/2).

%= 	 	 

%% fill_args( ?PI, ?With) is semidet.
%
% Fill Arguments.
%
fill_args([Arg|More],With):-!,ignore(With=Arg),fill_args(More,With).
fill_args([],_).
fill_args(PI,With):-compound_name_arguments(PI,_,ARGS),fill_args(ARGS,With).

% = :- meta_predicate(meta_predicate(0)).


:- export(def_meta_predicate/3).
% = :- meta_predicate((def_meta_predicate(0,+,+))).


%= 	 	 

%% def_meta_predicate( :GoalF, +S, +E) is semidet.
%
% Def Meta Predicate.
%
def_meta_predicate(PredMt:F,S,E):-!,PredMt:doall(((between(S,E,N),make_list('?',N,List),compound_name_arguments(CALL,F,List),'@'(meta_predicate(CALL),PredMt)))).
def_meta_predicate(F,S,E):- trace_or_throw(def_meta_predicate(F,S,E)).



:- export(remove_pred/3).

%= 	 	 

%% remove_pred( ?VALUE1, ?F, ?A) is semidet.
%
% Remove Predicate.
%
% remove_pred(_,_,_):- !.
remove_pred(_,F,A):-member(_:F/A,[_:delete_common_prefix/4]),!.
remove_pred(M,F,A):- 
 on_x_log_cont((
  w_tl(set_prolog_flag(access_level,system),
    ((functor(P,F,A),unlock_predicate(M:P),
    redefine_system_predicate(M:F/A),redefine_system_predicate(F/A),
    M:redefine_system_predicate(P),M:redefine_system_predicate(M:P),
    abolish(M:F,A),
  M:asserta((M:P:- wdmsg(permission_error(P)),throw(permission_error(M:F/A)))),
  lock_predicate(M:P)))))),!.

% = :- meta_predicate(call_if_defined(0)).
:- export(call_if_defined/1).

%= 	 	 

%% call_if_defined( :GoalG) is semidet.
%
% Call If Defined.
%
call_if_defined(G):-current_predicate(_,G)->call(G).


:- module_transparent(p_predicate_property/2).

%= 	 	 

%% p_predicate_property( :TermP, ?PP) is semidet.
%
% F Predicate Property.
%
p_predicate_property(P,PP) :- predicate_property(P,PP).
p_predicate_property(_:P,PP) :- predicate_property(P,PP).
%current_bugger_predicate(PredMt:FF/FA):-nonvar(FF),!,current_predicate(PredMt:FF,FA).
%current_bugger_predicate(FF/FA):-nonvar(FF),!,!,current_predicate(FF/FA).
:- module_transparent(current_predicate_module/2).

%= 	 	 

%% current_predicate_module( :TermP, ?PredMt) is semidet.
%
% Current Predicate Module.
%
current_predicate_module(P,PredMt):-var(P),!,current_predicate(F/A),functor_safe(P,F,A),(nonvar(PredMt)->true;p_predicate_property(P,imported_from(PredMt))).
current_predicate_module(OM:F/A,PredMt):-!,functor_safe(P,F,A),(current_predicate(PredMt:F/A);(current_predicate(OM:F/A),PredMt=OM);current_predicate(F/A)),(nonvar(PredMt)->true;p_predicate_property(P,imported_from(PredMt))).
current_predicate_module(OM:P,PredMt):-!,functor_safe(P,F,A),(current_predicate(PredMt:F/A);(current_predicate(OM:F/A),PredMt=OM);current_predicate(F/A)),(nonvar(PredMt)->true;p_predicate_property(P,imported_from(PredMt))).
current_predicate_module(F/A,PredMt):-!,functor_safe(P,F,A),(current_predicate(PredMt:F/A);(current_predicate(OM:F/A),PredMt=OM);current_predicate(F/A)),(nonvar(PredMt)->true;p_predicate_property(P,imported_from(PredMt))).
current_predicate_module(P,PredMt):-!,functor_safe(P,F,A),(current_predicate(PredMt:F/A);current_predicate(F/A)),(nonvar(PredMt)->true;p_predicate_property(P,imported_from(PredMt))).



%= 	 	 

%% dynamic_multifile( :TermPred) is semidet.
%
% Dynamic Multifile.
%
dynamic_multifile(F/N):-
   dynamic(F/N),
   multifile(F/N),
   module_transparent(F/N).
dynamic_multifile(M:F/N):-
   dynamic(M:F/N),
   multifile(M:F/N),
   module_transparent(M:F/N).


%= 	 	 

%% dynamic_transparent( :TermX) is semidet.
%
% Dynamic Transparent.
%
dynamic_transparent([]):-!.
dynamic_transparent([X]):-dynamic_transparent(X),!.
dynamic_transparent([X|Xs]):-!,dynamic_transparent(X),dynamic_transparent(Xs),!.
dynamic_transparent(PredMt:F/A):-!, module_transparent(PredMt:F/A),dynamic(PredMt:F/A).
dynamic_transparent(F/A):-!,multi_transparent(baseKB:F/A).
dynamic_transparent(X):-functor_catch(X,F,A),dynamic_transparent(F/A),!.





%% multi_transparent( :TermX) is semidet.
%
% Multi Transparent.
%
multi_transparent([]):-!.
multi_transparent([X]):-multi_transparent(X),!.
multi_transparent([X|Xs]):-!,multi_transparent(X),multi_transparent(Xs),!.
multi_transparent(PredMt:F/A):-!, module_transparent(PredMt:F/A),dynamic(PredMt:F/A),multifile(PredMt:F/A).
multi_transparent(F/A):-!,multi_transparent(baseKB:F/A).
multi_transparent(X):-functor_catch(X,F,A),multi_transparent(F/A),!.




%= 	 	 

%% dynamic_if_missing( :TermF) is semidet.
%
% Dynamic If Missing.
%
dynamic_if_missing(F/A):-functor_safe(X,F,A),predicate_property(X,_),!.
dynamic_if_missing(F/A):-dynamic([F/A]).


%= 	 	 

%% get_pi( ?PI, ?PI) is semidet.
%
% Get Predicate Indicator.
%
get_pi(PI,PI):-var(PI),!.
get_pi(F/A,PI):-!,functor(PI,F,A).
get_pi(PI,PI):- atomic(PI),!.
get_pi(PI,PI):- compound(PI),!.
get_pi(Mask,PI):-get_functor(Mask,F,A),functor(PI,F,A),!.


:- meta_predicate get_module_of_4(0,+,+,-).
:- meta_predicate with_pi_selected(+,+,*,0).
:- meta_predicate dynamic_safe(0).

%= 	 	 

%% get_module_of_4( :GoalP, +F, +A, -ModuleName) is semidet.
%
% Get Module Of Helper Number 4..
%
get_module_of_4(_P,F,A,ModuleName):- current_module(ModuleName),module_property(ModuleName, exports(List)),member(F/A,List),!.
get_module_of_4(_P,F,A,PredMt):- current_predicate(M0:F0/A0),F0=F,A0=A,!,PredMt=M0.
get_module_of_4(P,F,A,PredMt):- trace_or_throw((get_module_of_4(P,F,A,PredMt))).

/*
get_module_of_4(_P,F,A,PredMt):- current_predicate(F0/A0),F0=F,A0=A,!,call(ereq,defaultAssertMt(PredMt)).
get_module_of_4(_P,F,A,_M):-dtrace, isCycPredArity(F,A),!,fail.
get_module_of_4(P,F,A,PredMt):- dtrace, debugCall(get_module_of_4(P,F,A,PredMt)).
*/

:- meta_predicate get_module_of(0,-).

%= 	 	 

%% get_module_of( :GoalV, -PredMt) is semidet.
%
% Get Module Of.
%
get_module_of(V,PredMt):-var(V),!,current_module(PredMt).
get_module_of(F/A,PredMt):-!,functor_catch(P,F,A),!,get_module_of(P,PredMt).
get_module_of(P,PredMt):-predicate_property(P,imported_from(PredMt)),!.
get_module_of(P,PredMt):-predicate_property(_:P,imported_from(PredMt)),!.
get_module_of(MM:_,PredMt):-!,MM=PredMt.
get_module_of(P,PredMt):-functor_catch(P,F,A),get_module_of_4(P,F,A,PredMt).



% ----------
:- export(is_static_predicate_3/3).
% = :- meta_predicate(is_static_predicate_3(+,+,+)).

%= 	 	 

%% is_static_predicate( +PredMt, +F, +A) is semidet.
%
% Static Predicate.
%
is_static_predicate_3(PredMt,F,A):- 
  functor_safe(FA,F,A),  
  PredMt:once(predicate_property(FA,_)),
  \+ predicate_property(FA,dynamic),
    \+ ((predicate_property(PredMt:FA,imported_from(Where)),
    Where \== PredMt)).


%= 	 	 

is_dynamic_module(user).
is_dynamic_module(baseKB).
is_dynamic_module(baseKB).
is_dynamic_module(lmcache).
is_dynamic_module(t_l).
is_dynamic_module(prolog).
is_dynamic_module(M):- ereq(mtCycL(M)).

is_static_module(system).
is_static_module(M):- is_dynamic_module(M),!,fail.
is_static_module(M):- module_property(M,class(library)),!.
is_static_module(M):- module_property(M,class(system)),!.

%% is_static_predicate( :TermA) is semidet.
%
% Static Predicate.
%
is_static_predicate(Var):- var(Var),!,trace_or_throw(var_is_static_predicate(Var)).
is_static_predicate(M:Var):- var(Var),!,trace_or_throw(var_is_static_predicate(M:Var)).
is_static_predicate(M:M:H):-!,nonvar(H),is_static_predicate(M:H).
is_static_predicate((H:-_)):-!,nonvar(H),is_static_predicate(H).
is_static_predicate(~(H)):-!,nonvar(H),is_static_predicate(H).
is_static_predicate(M:'~'(H)):-!,nonvar(H),is_static_predicate(M:H).
is_static_predicate(M:(H:-_)):-!,nonvar(H),!,is_static_predicate(M:H).
is_static_predicate(M:F/A):-!,atom(F),current_predicate(M:F/A),!,functor(FA,F,A),is_static_predicate(M:FA).
is_static_predicate(M:F//A2):-A is A2+2, !,atom(F),current_predicate(M:F/A),!,functor(FA,F,A),is_static_predicate(M:FA).
is_static_predicate(M:F):-atom(F),predicate_property(M:F,static),!,predicate_property(F,number_of_clauses(_)),\+ predicate_property(F,dynamic).
is_static_predicate((M:F)//A2):-A is A2+2, !,atom(F),current_predicate(M:F/A),!,functor(FA,F,A),is_static_predicate(M:FA).
is_static_predicate((M:F)/A):-!,atom(F),current_predicate(M:F/A),!,functor(FA,F,A),is_static_predicate(M:FA).
is_static_predicate(F/A):-!,atom(F),current_predicate(F/A),!,functor(FA,F,A),is_static_predicate(FA).
is_static_predicate(F//A2):-A is A2+2, !,atom(F),current_predicate(F/A),!,functor(FA,F,A),is_static_predicate(FA).

is_static_predicate(FA):- predicate_property(FA,dynamic),!,fail.
is_static_predicate(FA):- predicate_property(FA,undefined),!,fail.
% is_static_predicate(M:F):-!,atom(F),between(1,11,A),current_predicate(M:F/A),functor(FA,F,A),is_static_predicate(M:FA),!.
is_static_predicate(F):- F\=(_:_),!,prolog_load_context(module,M),!,is_static_predicate(M:F).

is_static_predicate(FA):-predicate_property(FA,static),!,predicate_property(FA,number_of_clauses(_)), 
  catch(dynamic(FA),_,true),
  \+ predicate_property(FA,dynamic),
  catch(multifile(FA),_,true).
is_static_predicate(FA):- once(predicate_property(FA,_)),
    catch(dynamic(FA),_,true),
    \+ predicate_property(FA,dynamic),
    catch(multifile(FA),_,true).



:- export((((dynamic_safe)/1))).
% = :- meta_predicate(dynamic_safe(+)).
:- module_transparent((((dynamic_safe)/1))).

%= 	 	 

%% dynamic_safe( +MFA) is semidet.
%
% Dynamic Safely Paying Attention To Corner Cases.
%
dynamic_safe(MFA):- with_mfa(MFA,dynamic_safe).

:- export((((dynamic_safe)/3))).
% = :- meta_predicate(dynamic_safe(+,+,+)).
:- module_transparent((((dynamic_safe)/3))).


%= 	 	 

%% convert_to_dynamic( ?FA) is semidet.
%
% Convert Converted To Dynamic.
%
convert_to_dynamic(PredMt:FA):- !, get_functor(FA,F,A),convert_to_dynamic(PredMt,F,A).
convert_to_dynamic(FA):- strip_module(FA,PredMt,FA0), get_functor(FA0,F,A), convert_to_dynamic(PredMt,F,A).


%= 	 	 

%% convert_to_dynamic( ?PredMt, ?F, ?A) is semidet.
%
% Convert Converted To Dynamic.
%
convert_to_dynamic(PredMt,F,A):-  functor(C,F,A), predicate_property(PredMt:C,dynamic),!.
convert_to_dynamic(PredMt,F,A):-  functor(C,F,A),\+ predicate_property(PredMt:C,_),if_defined(kb_dynamic(PredMt:C),(PredMt:((dynamic(PredMt:F/A),multifile(PredMt:F/A),export(PredMt:F/A))))),!.
convert_to_dynamic(PredMt,F,A):-  functor(C,F,A),findall((C:-B),clause(C,B),List),rebuild_as_dyn(PredMt,C,F,A),maplist(assertz,List),!.

% kb_dynamic = 



%= 	 	 

%% rebuild_as_dyn( ?PredMt, ?C, ?VALUE3, ?VALUE4) is semidet.
%
% Rebuild Converted To Dyn.
%
rebuild_as_dyn(PredMt,C,_,_):- predicate_property(PredMt:C,dynamic),!.
rebuild_as_dyn(PredMt,C,F,A):- redefine_system_predicate(PredMt:C),PredMt:abolish(F,A),dynamic(PredMt:F/A),multifile(PredMt:F/A),export(F/A),!.


%= 	 	 

%% dynamic_safe( +PredMt, +F, +A) is semidet.
%
% Dynamic Safely Paying Attention To Corner Cases.
%
dynamic_safe(PredMt,F,A):- functor(C,F,A),predicate_property(C,imported_from(system)),!,dmsg(warn(predicate_property(PredMt:C,imported_from(system)))).
dynamic_safe(PredMt,F,A):- (is_static_predicate(PredMt:F/A) 
  -> show_call(why,convert_to_dynamic(PredMt,F,A)) ; on_x_log_cont((dynamic(PredMt:F/A),multifile(PredMt:F/A)))). % , warn_module_dupes(PredMt,F,A).
:- op(1150,fx,baseKB:dynamic_safe).


% pred_prop(Spec,DO,TEST,DONT)

%= 	 	 

%% pred_prop( :TermM, :TermDO, ?TEST, ?VALUE4) is semidet.
%
% Predicate Prop.
%
pred_prop((PredMt:F/A),DO,TEST,true):-pred_prop(PredMt:F/A,DO,TEST).
pred_prop(PredMt:F/A,(lock_predicate(PredMt:F/A)),(built_in),unlock_predicate(PredMt:F/A)).
pred_prop(PredMt:F/A, (dynamic(PredMt:F/A)) ,(dynamic), show_call(why,compile_predicates([F/A]))).


%% is_static_why( ?PredMt, ?P, ?VALUE3, ?VALUE4, ?VALUE5) is semidet.
%
% If Static F, Generate a Proof.
%
:- module_transparent(is_static_why/5).
is_static_why(PredMt,P,_,_,_):- predicate_property(PredMt:P,dynamic),!,fail.
is_static_why(PredMt,P,F,A,WHY):- show_success(predicate_property(PredMt:P,static)),!,WHY=static(PredMt:F/A).

defined_predicate(PredMt:P):- (current_module(PredMt),
   current_predicate(_,PredMt:P),( \+ predicate_property(PredMt:P,imported_from(_)))).

%= 	 	 

%% pred_prop( :TermARG1, :TermSpec, ?Spec) is semidet.
%
% Predicate Prop.
%
pred_prop(_,(meta_predicate Spec),(meta_predicate Spec)).
pred_prop(PredMt:F/A,multifile(PredMt:F/A)	       ,(multifile)).
pred_prop(PredMt:F/A,module_transparent(PredMt:F/A) ,(transparent)).
pred_prop(PredMt:F/A,discontiguous(PredMt:F/A) ,(discontiguous)).
pred_prop(PredMt:F/A,volatile(PredMt:F/A)	  ,(volatile)).
pred_prop(PredMt:F/A,public(PredMt:F/A)     ,(public)).
pred_prop(PredMt:F/A,thread_local(PredMt:F/A),(thread_local)).
pred_prop(PredMt:F/A,noprofile(PredMt:F/A)	    , (noprofile)).
pred_prop(PredMt:F/A,'$iso'(PredMt:F/A) ,(iso)).


:- thread_local(tlbugger:rbuild_pred_impl_cache_pp/2).
:- thread_local(tlbugger:rbuild_pred_impl_cache/2).

%  rebuild_pred_into(OMC,NMC,AssertZ,[+dynamic,-built_in,+volatile, etc]).

% = :- meta_predicate(rebuild_pred_into(0,1,?)).

%= 	 	 

%% rebuild_pred_into( :GoalC, :PRED1AssertZ, ?OtherTraits) is semidet.
%
% Rebuild Predicate Converted To.
%
rebuild_pred_into(C,AssertZ,OtherTraits):-rebuild_pred_into(C,C,AssertZ,OtherTraits).

% = :- meta_predicate(rebuild_pred_into(0,0,1,?)).

%= 	 	 

%% rebuild_pred_into( :GoalGOAL1, :GoalNMC, :PRED1AssertZ, ?UPARAM4) is semidet.
%
% Rebuild Predicate Converted To.
%
rebuild_pred_into(_,NMC,AssertZ,_):-tlbugger:rbuild_pred_impl_cache(NMC,AssertZ),!.
rebuild_pred_into(OMC,NMC,AssertZ,OtherTraits):-
  listing(OMC),
  asserta(tlbugger:rbuild_pred_impl_cache(NMC,AssertZ)),
  show_call(rebuild_pred_into,(predicate_property(OMC,number_of_clauses(_)))),
  strip_module(OMC, OM, OC),
  strip_module(NMC, NM, NC),
   must_det_l((
      '$set_source_module'(Before, OM),
      functor(NC,NF,A), functor(OC,OF,A),
      (show_call(why,predicate_property(OMC,number_of_clauses(_)))),
      must_pi(show_failure(why,predicate_property(OMC,number_of_clauses(_)))),
      forall(predicate_property(OC,PP),asserta(tlbugger:rbuild_pred_impl_cache_pp(NC,PP))),
      findall((OC:-B),((clause(OC,B),assertz(pp_clauses((OC:-B))))),List),
      '$set_source_module'( NM),
      forall(member(-PP,OtherTraits),retractall(tlbugger:rbuild_pred_impl_cache_pp(NC,PP))),
      forall(member(+PP,OtherTraits),asserta(tlbugger:rbuild_pred_impl_cache_pp(NC,PP))),
      once(tlbugger:rbuild_pred_impl_cache_pp(NC,(built_in))->(redefine_system_predicate(NF/A),unlock_predicate(NF/A));true),
      show_call(why,must_pi(abolish(NF/A))),
      show_call(why,must_pi(abolish(NF/A))),
      garbage_collect_clauses,
      ignore(convert_to_dynamic(NM,NF,A)),
      garbage_collect_clauses,
      %must_pi( \+ predicate_property(NMC,_)),
      %once(memberchk(CC,List)->true;(CC=((NC:-fail,1234)))),
      %convert_to_dynamic(NM,NF,A),
      %ignore(on_x_log_throw(tlbugger:rbuild_pred_impl_cache_pp(NC,(dynamic))->dynamic(NF/A);true)),
      %ignore(once(tlbugger:rbuild_pred_impl_cache_pp(NC,(multifile))->multifile(NF/A);true)),
      must_pi(((tlbugger:rbuild_pred_impl_cache_pp(NC,file(File)),tlbugger:rbuild_pred_impl_cache_pp(NC,line_count(_Line))))
        ->
            must_pi(('$compile_aux_clauses'(CC, File),retractall(CC)));
            must_pi(dmsg(noFileFor(NC)))),
      forall(pred_prop(NM:NF/A,TODO,PP,ELSE),(tlbugger:rbuild_pred_impl_cache_pp(NC,PP)->must_pi(TODO);must_pi(ELSE))),
      (tlbugger:rbuild_pred_impl_cache_pp(NC,meta_predicate(NC))->meta_predicate(NC);true),
      dbgsubst(List,OF,NF,ListO),maplist(AssertZ,ListO),!,

      retractall(tlbugger:rbuild_pred_impl_cache(NMC,_)),
      asserta(tlbugger:rbuild_pred_impl_cache(NMC,AssertZ)),
      '$set_source_module'( Before),
      listing(NMC),
      retractall(tlbugger:rbuild_pred_impl_cache_pp(NC,_))
      )).

