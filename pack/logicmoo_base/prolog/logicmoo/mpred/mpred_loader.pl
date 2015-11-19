/* 
% Game loading Utils
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_loader.pl
:- module(mpred_loader,
          [ add_from_file/1,
            add_term/2,
            assert_kif/1,
            import_module_to_user/1,
            assert_kif_dolce/1,
            assert_until_eof/1,
            decl_user_abox/1,
            set_current_module/1,
            set_mpred_module/1,
            import_shared_pred/3,
            import_to_user0/3,
            import_to_user_mfa0/4,
            set_user_abox/1,
            predicate_is_undefined_fa/2,
            get_user_abox/1,
            get_user_tbox/1,
            get_user_sbox/1,
            is_box_module/2,
            import_to_user/1,
            best_module/2,
            not_boot_module/1,
            maybe_add_import_module/3,
            to_sbox/2,
            to_tbox/2,
            to_abox/2,
            ensure_imports/1,
            correct_module/3,
            correct_module/4,
            is_system_box/1,
            which_file/1,
            begin_pfc/0,
            call_file_command/4,
            call_with_module/2,
            call_with_source_module/2,
            %set_user_tbox/1,
            can_be_dynamic/1,
            cl_assert/2,
            clear_predicates/1,
            collect_expansions/3,
            compile_clause/1,
            compile_this/4,
            convert_side_effect/2,
            convert_side_effect/3,
            convert_side_effect_buggy/2,
            current_context_module/1,
            % cwc/0,
            decache_file_type/1,
            mpred_process_input_1/1,
            mpred_ops/0,
            declare_load_dbase/1,
            disable_mpred_expansion/0,
            disable_mpreds_in_current_file/0,
            do_end_of_file_actions/4,
            do_end_of_file_actions_real/0,
            doall_and_fail/1,
            dyn_begin/0,
            dyn_end/0,
            enable_mpred_expansion/0,
            end_module_type/1,
            end_module_type/2,
            ensure_loaded_no_mpreds/1,
            ensure_tbox_module/1,
            ensure_mpred_file_consulted/2,
            ensure_mpred_file_loaded/1,
            ensure_mpred_file_loaded/2,
            ensure_prolog_file_consulted/2,
            etrace/0,
            expand_in_mpred_kb_module/2,
            expanded_already_functor/1,
            file_begin/1,
            file_end/1,
            finish_processing_world/0,
            force_reload_mpred_file/1,
            force_reload_mpred_file/2,
            from_kif_string/2,
            convert_if_kif_string/4,
            get_file_type/2,
            get_lang/1,
            get_last_time_file/3,
            get_op_alias/2,
            gload/0,
            guess_file_type_loader/2,
            hdr_debug/2,
            in_include_file/0,
            in_mpred_kb_module/0,
            include_mpred_files/1,
            inside_file/1,
            is_code_body/1,
            is_compiling/0,
            is_compiling_sourcecode/0,
            is_kif_string/1,
            is_prolog_xform/1,
            is_mpred_file/1,
            lang_op_alias/3,
            load_file_dir/2,
            load_file_some_type/2,
            load_file_term_to_command_or_fail/2,
            mpred_expander_now_physically/4,
            load_init_world/2,
            load_language_file/1,
            load_mpred_files/0,
            load_mpred_on_file_end/2,
            loader_side_effect_capture_only/2,
            loader_side_effect_verify_only/2,
            loader_term_expansion/2,
            loading_source_file/1,
            make_db_listing/0,
            make_dynamic/1,
            with_ukb/2,
            module_typed_term_expand/2,
            module_typed_term_expand/5,
            mpred_begin/0,
            mpred_directive_expansion/2,
            mpred_expand_inside_file_anyways/0,
            mpred_expand_inside_file_anyways/1,
            mpred_expander/4,
            mpred_expander_now/2,
            mpred_expander_now_one/4,
            mpred_expander_now_one_cc/4,
            mpred_implode_varnames/1,
            mpred_loader_file/0,
            mpred_prolog_only_file/1,
            mpred_may_expand/0,
            mpred_may_expand_module/1,
            mpred_maybe_skip/1,
            mpred_process_input/2,
            lmconf:mpred_skipped_module/1,
            mpred_term_expansion/2,
            mpred_use_module/1,
            must_compile_special_clause/1,
            must_load_file_term_to_command/2,
            must_locate_file/2,
            myDebugOnError/1,
            onEndOfFile/1,
            op_alias/2,
            op_lang/1,
            pl_to_mpred_syntax/2,
            pl_to_mpred_syntax_h/2,
            pop_predicates/2,
            process_this_script/0,
            process_this_script/1,
            process_this_script0/1,
            prolog_load_file_loop_checked/2,
            prolog_load_file_nlc/2,
            push_predicates/2,
            read_one_term/2,
            read_one_term/3,
            register_module_type/1,
            register_module_type/2,
            rsavedb/0,
            savedb/0,
            scan_updates/0,
            show_bool/1,
            show_interesting_cl/2,
            show_load_context/0,
            simplify_why/2,
            simplify_why_r/4,
            stream_pos/1,
            term_expand_local_each/5,
            to_box_type0/3,
            transform_opers/3,
            use_file_type_loader/2,
            use_was_isa/3,
            was_exported_content/3,
            with_mpred_expansions/1,
            with_no_mpred_expansions/1,
            with_source_module/2,
            lmcache:mpred_directive_value/3,
%            get_user_abox/1,
            lmconf:loaded_file_world_time/3,
            lmconf:mpred_provide_clauses/3,
            lmconf:never_reload_file/1,
            mpred_loader:always_expand_on_thread/1,
            t_l:current_lang/1,
            kb_dynamic/1,
            make_declared/2,
            make_reachable/2,
            import_predicate/2,
            lmconf:mpred_skipped_module/1,
            prolog_load_file_loop_checked/2,
%            registered_module_type/2,
            %t_l:disable_mpred_term_expansions_globally/0,
            %t_l:into_form_code/0,
            %t_l:mpred_module_expansion/1,
            user:prolog_load_file/2,
            user:term_expansion/2,
            mpred_loader_module_transparent/1,
            convert_side_effect_0a/2, convert_side_effect_0b/2, 
            convert_side_effect_0c/2, is_mpred_file0/1, load_file_term_to_command_0c/2, load_file_term_to_command_1/3, 
            load_file_term_to_command_1b/3, load_file_term_to_command_2/3, mpred_process_input_1/1, must_mpred_term_expansion_2/2, pl_to_mpred_syntax0/2, process_this_script0/1, prolog_load_file_loop_checked_0/2, prolog_load_file_nlc_0/2, transform_opers_0/2, transform_opers_1/2, xfile_module_term_expansion_pass_3/7
          ]).
 :- meta_predicate % cmt :-

         kb_dynamic(?),
        make_declared(?,-),
        make_reachable(?,?),
        call_file_command(?, ?, ?, ?),
        call_with_module(+, 0),
        call_with_source_module(+, 0),
        with_ukb(+, 0),
        cl_assert(?, ?),
        convert_side_effect(?, +, -),
        doall_and_fail(0),
        ensure_loaded_no_mpreds(0),
        ensure_mpred_file_loaded(:),
        ensure_mpred_file_loaded(+, :),
        force_reload_mpred_file(?),
        get_last_time_file(+, +, +),
        load_file_term_to_command_0c(?, ?),
        mpred_expander_now_physically(?, ?, ?, ?),
        load_init_world(+, :),
        module_typed_term_expand(?, ?),
        mpred_expander(+, +, +, -),
        mpred_expander_now(+, -),
        mpred_term_expansion(?, ?),
        myDebugOnError(0),        
        with_mpred_expansions(0),
        with_no_mpred_expansions(0),
        mpred_loader_module_transparent(?),
        with_source_module(:, ?),
        lmconf:loaded_file_world_time(+, +, +).
:- (multifile t_l:into_form_code/0, t_l:mpred_module_expansion/1, user:prolog_load_file/2, user:term_expansion/2).
:- (dynamic is_box_module/2, user:prolog_load_file/2, user:term_expansion/2).
% :- (module_transparent add_from_file/1, add_term/2, assert_kif/1, assert_kif_dolce/1, assert_until_eof/1, begin_pfc/0, call_file_command/4, call_with_module/2, call_with_source_module/2, can_be_dynamic/1, cl_assert/2, clear_predicates/1, collect_expansions/3, compile_clause/1, compile_this/4, convert_side_effect/2, convert_side_effect/3, convert_side_effect_0a/2, convert_side_effect_0b/2, convert_side_effect_0c/2, convert_side_effect_buggy/2, current_context_module/1, current_op_alias/2, cwc/0, decache_file_type/1, decl_user_abox/1, declare_load_dbase/1, disable_mpred_expansion/0, disable_mpreds_in_current_file/0, do_end_of_file_actions/4, do_end_of_file_actions_real/0, doall_and_fail/1, dyn_begin/0, dyn_end/0, enable_mpred_expansion/0, end_module_type/1, end_module_type/2, ensure_loaded_no_mpreds/1, ensure_mpred_file_consulted/2, ensure_mpred_file_loaded/1, ensure_mpred_file_loaded/2, ensure_prolog_file_consulted/2, etrace/0, expand_in_mpred_kb_module/2, expanded_already_functor/1, file_begin/1, file_end/1, finish_processing_world/0, force_reload_mpred_file/1, force_reload_mpred_file/2, from_kif_string/2, get_file_type/2, get_lang/1, get_last_time_file/3, get_op_alias/2, gload/0, guess_file_type_loader/2, hdr_debug/2, in_include_file/0, in_mpred_kb_module/0, include_mpred_files/1, inside_file/1, is_code_body/1, is_compiling/0, is_compiling_sourcecode/0, is_kif_string/1, is_mpred_file/1, is_mpred_file0/1, lang_op_alias/3, load_file_dir/2, load_file_some_type/2, load_file_term_to_command_0c/2, load_file_term_to_command_1/3, load_file_term_to_command_1b/3, load_file_term_to_command_2/3, load_file_term_to_command_or_fail/2, load_file_term_to_command_or_fail/4, load_init_world/2, load_language_file/1, load_mpred_files/0, load_mpred_on_file_end/2, loader_side_effect_capture_only/2, loader_side_effect_verify_only/2, loader_term_expansion/2, loading_source_file/1, make_db_listing/0, make_dynamic/1, module_typed_term_expand/2, module_typed_term_expand/5, mpred_begin/0, mpred_directive_expansion/2, mpred_expand_inside_file_anyways/0, mpred_expand_inside_file_anyways/1, mpred_expander/4, mpred_expander_now/2, mpred_expander_now_one/4, mpred_implode_varnames/1, mpred_loader_file/0, mpred_may_expand/0, mpred_may_expand_module/1, mpred_maybe_skip/1, mpred_process_input/2, mpred_process_input_1/1, lmconf:mpred_skipped_module/1, mpred_term_expansion/2, mpred_use_module/1, must_compile_special_clause/1, must_load_file_term_to_command/2, must_locate_file/2, must_mpred_term_expansion_2/2, myDebugOnError/1, onEndOfFile/1, op_alias/2, op_lang/1, pl_to_mpred_syntax/2, pl_to_mpred_syntax0/2, pl_to_mpred_syntax_h/2, pop_predicates/2, process_this_script/0, process_this_script/1, process_this_script0/1, prolog_load_file_loop_checked/2, prolog_load_file_loop_checked_0/2, prolog_load_file_nlc/2, prolog_load_file_nlc_0/2, push_predicates/2, read_one_term/2, read_one_term/3, register_module_type/1, register_module_type/2, rsavedb/0, savedb/0, scan_updates/0, show_bool/1, show_interesting_cl/2, show_load_context/0, simplify_why/2, simplify_why_r/4, stream_pos/1, term_expand_local_each/5, transform_opers/3, transform_opers_0/2, transform_opers_1/2, use_file_type_loader/2, use_was_isa/3, was_exported_content/3, with_mpred_expansions/1, with_no_mpred_expansions/1, with_source_module/2, xfile_module_term_expansion_pass_3/7,  (~)/1, baseKB:cl_assert/2, baseKB:cwc/0, lmconf:mpred_provide_clauses/3, mpred_loader:always_expand_on_thread/1, mpred_loader:t_l:current_lang/1, mpred_loader:current_op_alias/2, mpred_loader:get_user_abox/1, mpred_loader:disable_mpred_term_expansions_globally/0, lmconf:loaded_file_world_time/3, mpred_loader:mpred_directive_value/3, mpred_loader:lmconf:mpred_skipped_module/1, mpred_loader:never_reload_file/1, mpred_loader:prolog_load_file_loop_checked/2, mpred_loader:registered_module_type/2).
:- module_transparent mpred_ops/0. % ,set_user_tbox/1.
:- (thread_local t_l:into_form_code/0, t_l:mpred_module_expansion/1).
%:- (volatile t_l:into_form_code/0, t_l:mpred_module_expansion/1).
%:- was_export((convert_side_effect_0a/2, convert_side_effect_0b/2, convert_side_effect_0c/2, is_mpred_file0/1, load_file_term_to_command_0c/2, load_file_term_to_command_1/3, load_file_term_to_command_1b/3, load_file_term_to_command_2/3, mpred_process_input_1/1, must_mpred_term_expansion_2/2, pl_to_mpred_syntax0/2, process_this_script0/1, prolog_load_file_loop_checked_0/2, prolog_load_file_nlc_0/2, transform_opers_0/2, transform_opers_1/2, xfile_module_term_expansion_pass_3/7)).
%:- dynamic((registered_module_type/2, current_op_alias/2, lmconf:mpred_skipped_module/1, prolog_load_file_loop_checked/2, lmcache:mpred_directive_value/3, get_user_abox/1, lmconf:loaded_file_world_time/3, lmconf:never_reload_file/1, mpred_loader:always_expand_on_thread/1, mpred_loader:t_l:current_lang/1, mpred_loader:current_op_alias/2, mpred_loader:get_user_abox/1, mpred_loader:disable_mpred_term_expansions_globally/0, lmconf:loaded_file_world_time/3, mpred_loader:mpred_directive_value/3, mpred_loader:lmconf:mpred_skipped_module/1, mpred_loader:never_reload_file/1, mpred_loader:prolog_load_file_loop_checked/2, mpred_loader:registered_module_type/2, t_l:disable_mpred_term_expansions_globally/0, user:prolog_load_file/2, user:term_expansion/2)).
%:- dynamic(registered_module_type/2).        


:- lmconf:dynamic((lmconf:registered_mpred_file/1,lmconf:never_registered_mpred_file/1,lmconf:registered_module_type/2)).
:- multifile((lmconf:registered_mpred_file/1,lmconf:never_registered_mpred_file/1,lmconf:registered_module_type/2)).







%% kb_dynamic( ?P) is semidet.
%
% Knowledge Base Dynamic.
%
kb_dynamic(_:FA):- is_ftVar(FA),!,fail.
kb_dynamic(_:F/_):- is_ftVar(F),!.
kb_dynamic(F/A):- !, get_user_abox(KB),!,kb_dynamic(KB:F/A).
kb_dynamic([FA1|FA2]):-!,kb_dynamic(FA1),kb_dynamic(FA2).
kb_dynamic((FA1,FA2)):-!,kb_dynamic(FA1),kb_dynamic(FA2).
% kb_dynamic(CM:M:FA):- atom(CM),atom(M),!,(CM==M -> kb_dynamic(M:FA);(CM:kb_dynamic(M:FA))).
% kb_dynamic(CM:M:F/A):- atom(CM),atom(M),!,(CM==M -> kb_dynamic(M:FA);(CM:kb_dynamic(M:F/A))).
kb_dynamic(M:(FA1,FA2)):-!,kb_dynamic(M:FA1),kb_dynamic(M:FA2).
kb_dynamic(M:[FA1|FA2]):-!,kb_dynamic(M:FA1),kb_dynamic(M:FA2).
kb_dynamic(M:F/A):-!,must((make_declared(M:F/A,T),get_user_abox(CM),make_reachable(CM,T:F/A))).
kb_dynamic(M:P):-functor(P,F,A),!,kb_dynamic(M:F/A).
kb_dynamic(P):-functor(P,F,A),!,kb_dynamic(F/A).





%% chop_box( ?Chop, ?Was) is semidet.
%
% Chop Datalog.
%
chop_box(Chop,Was):-atom_concat(Was,'ABox',Chop),!.
chop_box(Chop,Was):-atom_concat(Was,'TBox',Chop),!.
chop_box(Chop,Was):-atom_concat(Was,'SBox',Chop),!.
chop_box(Chop,Chop).




%% maybe_add_import_module( ?A, ?A, ?VALUE3) is semidet.
%
% Maybe Add Import Module.
%
maybe_add_import_module(_,baseKBTBox,end):-!,dumpST,dtrace,fail.
maybe_add_import_module(A,A,_):-!.
maybe_add_import_module(basePFC,_,end):-!.
maybe_add_import_module(baseKB,_,end):-!.
maybe_add_import_module(logicmoo_user, baseKB, end):-!.
maybe_add_import_module(user, baseKB, end):-!.
maybe_add_import_module(A,B,C):- catch(add_import_module(A,B,C),_,logicmoo_util_dmsg:dmsg(failed(maybe_add_import_module(A,B,C)))),!.




%% is_system_box( ?VALUE1) is semidet.
%
% If Is A System Datalog.
%
is_system_box(baseKB).
is_system_box(logicmoo_user).
is_system_box(basePFC).
% is_system_box(user).




%% no_separate_boxes is semidet.
%
% No Separate Boxes.
%
no_separate_boxes.




%% to_tbox( ?A, ?T) is semidet.
%
% Converted To Tbox.
%
to_tbox(A,T):-sanity((nonvar(A),var(T))),is_system_box(A),!,T=baseKB.
to_tbox(pqr,pqrTBox).
to_tbox(pqrABox,pqrTBox).
to_tbox(pqrSBox,pqrTBox).
to_tbox(pqrTBox,pqrTBox).
to_tbox(Chop,Chop):- no_separate_boxes,!.
to_tbox(Chop,Add):-chop_box(Chop,Was),atom_concat(Was,'TBox',Add).





%% to_sbox( ?A, ?T) is semidet.
%
% Converted To Sbox.
%
to_sbox(A,T):-sanity((nonvar(A),var(T))),is_system_box(A),!,T=baseKB.
to_sbox(pqr,pqrSBox).
to_sbox(pqrABox,pqrSBox).
to_sbox(pqrTBox,pqrSBox).
to_sbox(pqrSBox,pqrSBox).
to_sbox(Chop,Chop):- no_separate_boxes,!.
to_sbox(Chop,Add):-chop_box(Chop,Was),atom_concat(Was,'SBox',Add).





%% to_abox( ?A, ?A) is semidet.
%
% Converted To Abox.
%
to_abox(A,A):-is_system_box(A).
to_abox(Chop,Chop):- no_separate_boxes,!.
to_abox(pqr,pqr).
to_abox(pqrABox,pqr).
to_abox(pqrTBox,pqr).
to_abox(pqrSBox,pqr).

to_abox(pqr,pqrABox).
to_abox(pqrABox,pqrABox).
to_abox(pqrTBox,pqrABox).
to_abox(pqrSBox,pqrABox).
to_abox(Chop,Add):-chop_box(Chop,Add),!.
to_abox(Chop,Add):-chop_box(Chop,Was),atom_concat(Was,'ABox',Add).

:- export(to_box_type0/3).



%% to_box_type( ?M, ?B, ?T) is semidet.
%
% Converted To Datalog Type.
%
to_box_type(M,B,T):-must(to_box_type0(M,B,TT)),!,T=TT.



%% to_box_type0( ?M, ?VALUE2, ?T) is semidet.
%
% Converted To Datalog Type Primary Helper.
%
to_box_type0(M,abox,T):-!,to_abox(M,T).
to_box_type0(M,tbox,T):-!,to_tbox(M,T).
to_box_type0(M,sbox,T):-!,to_sbox(M,T).




%% correct_module( ?M, ?X, ?T) is semidet.
%
% Correct Module.
%
correct_module(M,X,T):-functor(X,F,A),must(correct_module(M,F,A,T)),!.




%% correct_module( ?M, ?F, ?A, ?T) is semidet.
%
% Correct Module.
%
correct_module(abox,F,A,T):- !,get_user_abox(M),!,correct_module(M,F,A,T).
correct_module(tbox,F,A,T):- !,get_user_tbox(M),!,correct_module(M,F,A,T).
correct_module(sbox,F,A,T):- !,get_user_sbox(M),!,correct_module(M,F,A,T).
correct_module(M,F,A,T):- box_type(F,A,Type),!,to_box_type(M,Type,T).
correct_module(MT,_,_,MT):-!.





%% box_type( ?F, ?A, ?VALUE3) is semidet.
%
% Datalog Type.
%
box_type(F,A,tbox):-current_predicate(baseKB:F/A).
% box_type(F,A,sbox):-current_predicate(basePFC:F/A).




%% make_declared( ?Test, -IN2) is semidet.
%
% Make Declared.
%
make_declared(Test,_):- \+ \+ ((Test= (_:F/_), is_ftVar(F))),!.
make_declared(F/_,_):- is_ftVar(F),!.
make_declared(M:F/A,T):- !,correct_module(M,F,A,T),!,make_declared_now(T:F/A).
make_declared(F/A,T):- !,correct_module(abox,F,A,T),!,make_declared_now(T:F/A).




%% make_declared_now( :TermM) is semidet.
%
% Make Declared Now.
%
make_declared_now(M:F/A):-!,
   debug(make_declared,'~p',make_declared_now(M:F/A)),
   sanity( \+ ((M:F/A) = (qrTBox:p/1))),
   check_never_assert(declared(M:F/A)),
   discontiguous(M:F/A),
   functor(P,F,A),(predicate_property(P,dynamic)->true;M:dynamic(M:F/A)),
   M:multifile(M:F/A),M:module_transparent(M:F/A),
   import_to_user(M:F/A),  !.
make_declared_now(M:P):-functor(P,F,A),!,make_declared_now(M:F/A). 




%% make_reachable( ?UPARAM1, ?Test) is semidet.
%
% Make Reachable.
%
make_reachable(_,Test):- \+ \+ ((Test= (_:F/_), is_ftVar(F))),!.
make_reachable(CM,M:F/A):-  must(atom(CM)),must(atom(M)), make_declared(M:F/A,TT), !,import_predicate(CM,TT:F/A).



%% make_reachable( :TermF) is semidet.
%
% Make Reachable.
%
make_reachable(F/A):- dumpST,trace, source_context_module(CM),make_declared(CM:F/A,_).




%% import_predicate( ?CM, :TermM) is semidet.
%
% Import Predicate.
%
import_predicate(CM,M:_):- CM==M,!.
import_predicate(CM,M:_):- default_module(CM,M),!.
import_predicate(CM,M:F/A):- show_call(nop(CM:z333import(M:F/A))),CM:multifile(M:F/A),CM:discontiguous(M:F/A).



:- include('mpred_header.pi').
:- include('mpred_prolog_file').




%% with_ukb( +KB, :GoalG) is semidet.
%
% Using Ukb.
%
with_ukb(KB,G):-w_tl(t_l:user_abox(KB),G).

% TODO uncomment the next line without breaking it all!
% lmconf:use_cyc_database.




%% mpred_loader_module_transparent( ?F) is semidet.
%
% Managed Predicate Loader Module Transparent.
%
mpred_loader_module_transparent(mpred_loader:F/A):-!,mpred_loader_module_transparent(F/A).
mpred_loader_module_transparent(M:F/A):-!, M:module_transparent(M:F/A),trace, system:import(M:F/A).
mpred_loader_module_transparent(F/A):-!, mpred_loader:module_transparent(F/A).

:- module_property(mpred_loader, exports(List)),maplist(mpred_loader_module_transparent,List).

:- thread_local(t_l:mpred_already_in_file_expansion/1).




%% mpred_prolog_only_file( ?File) is semidet.
%
% Managed Predicate Prolog Only File.
%
mpred_prolog_only_file(File):- file_name_extension(_,pi,File),!.
mpred_prolog_only_file(File):- file_name_extension(_,pl,File), \+ (lmcache:mpred_directive_value(pfc,file,File)).
mpred_prolog_only_file(File):- lmconf:never_registered_mpred_file(File),!.

%:- use_module(library(logicmoo/util/logicmoo_util_help)).
%:- use_module(library(logicmoo/util/logicmoo_util_varnames)).

:- meta_predicate mpred_expander(+,+,+,-).
% mpred_expander(_,_,I,OO):-thread_self(X),X\==main,!,I=OO.
% not actual function



%% mpred_expander( +OUT1, +OUT2, +I, -IN4) is semidet.
%
% Managed Predicate Expander.
%
mpred_expander(_,_,I,_):-var(I),!,fail.
mpred_expander(_,_,(_ --> _),_):-!,fail.
mpred_expander(_,_,_,_):- notrace( current_prolog_flag(xref,true);t_l:disable_px),!,fail.
mpred_expander(Type,_DefMod,_I,_O):-  (Type \== term,Type \= _:term ),!,fail.
mpred_expander(Type,DefMod,end_of_file,O):- !,Type = term, DefMod = user, do_end_of_file_actions(Type,DefMod,end_of_file,O),!,fail.
mpred_expander(Type,LoaderMod,I,OO):- \+ t_l:disable_px, on_x_debug(mpred_expander0(Type,LoaderMod,I,O)),must(nonvar(O)),O=OO.




%% mpred_expander0( ?Type, ?LoaderMod, ?I, ?OO) is semidet.
%
% Managed Predicate Expander Primary Helper.
%
mpred_expander0(Type,LoaderMod,I,OO):-
  I\= '$si$':'$was_imported_kb_content$'(_,_),  
   '$set_source_module'(M,M),  
   source_location(F,L),
   \+ mpred_prolog_only_file(F),
   must(\+ mpred_prolog_only_module(M)),
  '$module'(UM,M),
   

  call_cleanup(((
  make_key(mpred_expander_key(F,L,M,UM,Type,LoaderMod,I),Key),
  w_tl(t_l:current_why_source(F),((
  ( \+ t_l:mpred_already_in_file_expansion(Key) ),
  w_tl(t_l:mpred_already_in_file_expansion(Key),
        (( % trace,
           fully_expand(change(assert,ain),I,II),
           mpred_expander_now_one_cc(F,M,II,O))))))))),
    '$module'(_,UM)),
  !,I\=@=O,O=OO.




%% mpred_expander_now_one_cc( ?VALUE1, ?VALUE2, :TermA, :TermA) is semidet.
%
% Managed Predicate Expander Now One Cc.
%
mpred_expander_now_one_cc(_,_,A,A):-var(A),!.
mpred_expander_now_one_cc(_,_,(:-(G)),(:-(G))):-!.
mpred_expander_now_one_cc(_,_,(?-(G)),(?-(G))):-!.
mpred_expander_now_one_cc(F,M,II,O):-show_if_debug(mpred_expander_now_one(F,M,II,O)).
  

%mpred_expander_now_one(F,M,(:- G),GGG):- !, nonvar(G), once(fully_expand(call,G,GG)),G\=@=GG,GGG=(:- GG),!.



%% mpred_expander_now_one( ?VALUE1, ?VALUE2, ?I, ?O) is semidet.
%
% Managed Predicate Expander Now One.
%
mpred_expander_now_one(_,_,I,O):- t_l:verify_side_effect_buffer,!,loader_side_effect_verify_only(I,O).
mpred_expander_now_one(_,_,I,O):- t_l:use_side_effect_buffer,!,loader_side_effect_capture_only(I,O).
mpred_expander_now_one(F,M,I,O):- mpred_expander_now_physically(F,M,I,O).




%% mpred_expander_now_physically( ?F, ?M, ?I, ?OO) is semidet.
%
% Managed Predicate Expander Now Physically.
%
mpred_expander_now_physically(_F,M,I,OO):-   
 '$set_source_module'(Old,M),
 call_cleanup(M:((
   must((source_context_module(CM),CM\==mpred_pfc,CM\==mpred_loader)),
   loop_check(must(load_file_term_to_command_or_fail(I,O))),!,
   I\=@=O,
  (((t_l:mpred_term_expansion_ok;mpred_expand_inside_file_anyways)-> true ; ((show_load_context,wdmsg(warning,wanted_mpred_term_expansion(I,O))),fail)),
   ((O=(:-(CALL))) ->  M:call_file_command(I,CALL,OO,O); 
        (OO = O))))),'$set_source_module'(_,Old)).
    

:- module_transparent( xfile_module_term_expansion_pass_3/6).



%% xfile_module_term_expansion_pass_3( ?How, ?INFO, ?F, ?M, ?AA, ?O, ?OO) is semidet.
%
% Xfile Module Term Expansion Pass Helper Number 3..
%
xfile_module_term_expansion_pass_3(How,INFO,_F,_M,AA,O,OO):- 
   (O = (:- _) -> OO = O ;
      (How == pl -> OO = O ;
        (add_from_file(O), OO = '$si$':'$was_imported_kb_content$'(AA,INFO)))),!.      

:- thread_local((t_l:use_side_effect_buffer , t_l:verify_side_effect_buffer)).




%% mpred_expander_now( +I, -O) is semidet.
%
% Managed Predicate Expander Now.
%
mpred_expander_now(I,O):- 
 '$set_source_module'(M,M),
  must(if_defined(current_source_location(F),source_location(F,_))),
   w_tl(t_l:current_why_source(F), 
     mpred_expander_now_one(F,M,I,O)).




%% show_bool( :GoalG) is semidet.
%
% Show Bool.
%
show_bool(G):-ignore(show_call(why,G)).




%% show_load_context is semidet.
%
% Show Load Context.
%
show_load_context:- 
  listing(lmconf:registered_mpred_file),
  show_bool(inside_file(_)),
  show_bool(mpred_may_expand),
  show_bool(in_mpred_kb_module),
  show_bool(mpred_expand_inside_file_anyways),
  show_bool(t_l:mpred_term_expansion_ok),
  show_bool(loading_source_file(_)).





%% add_term( ?Term, ?Vs) is semidet.
%
% Add Term.
%
add_term(end_of_file,_):-!.
add_term(Term,Vs):- 
   put_variable_names( Vs),
    add_from_file(Term).





%% add_from_file( ?Term) is semidet.
%
% Add Converted From File.
%
add_from_file(Term):-  
  w_tl(t_l:mpred_already_in_file_expansion(Term),must(ain(Term))).




%% myDebugOnError( :GoalTerm) is semidet.
%
% My Debug Whenever Error.
%
myDebugOnError(Term):-catch(once(must((Term))),E,(dmsg(error(E,start_myDebugOnError(Term))),dumpST,trace,rtrace((Term)),dmsginfo(stop_myDebugOnError(E=Term)),trace,Term)).
         



%% read_one_term( ?Term, ?Vs) is semidet.
%
% Read One Term.
%
read_one_term(Term,Vs):- catch(once(( read_term(Term,[double_quotes(string),variable_names(Vs)]))),E,(Term=error(E),dmsg(error(E,read_one_term(Term))))).



%% read_one_term( ?Stream, ?Term, ?Vs) is semidet.
%
% Read One Term.
%
read_one_term(Stream,Term,Vs):- catch(once(( read_term(Stream,Term,[double_quotes(string),variable_names(Vs)]))),E,(Term=error(E),dmsg(error(E,read_one_term(Term))))).

% rescan_mpred_stubs:- doall((mpred_isa(F,prologHybrid),arity(F,A),A>0,warnOnError(declare_mpred_local_dynamic(moo,F,A)))).



:- was_export(etrace/0).



%% etrace is semidet.
%
% E Trace.
%
etrace:-leash(-all),leash(+exception),trace.


:- thread_local(t_l:on_eof/2).
:- export(t_l:on_eof/2).




%% which_file( ?F) is semidet.
%
% Which File.
%
which_file(F):- prolog_load_context(source,F) -> true; once(loading_source_file(F)).

:- export(onEndOfFile/1).



%% onEndOfFile( ?Call) is semidet.
%
% Whenever End Of File.
%
onEndOfFile(Call):- which_file(F), asserta(t_l:on_eof(F,Call)).




%% assert_until_eof( ?F) is semidet.
%
% Assert Until Eof.
%
assert_until_eof(F):- must_det_l((loading_source_file(File),asserta(F),asserta((t_l:on_eof(File,ignore(retract(F))))))).

:- style_check(-singleton).
:- style_check(-discontiguous).
% :- style_check(-atom).

% gload:- ensure_mpred_file_loaded(savedb),!.



%% gload is semidet.
%
% Gload.
%
gload:- ensure_mpred_file_loaded(logicmoo('rooms/startrek.all.mpred')).

%:-meta_predicate(savedb/0).



%% savedb is semidet.
%
% Savedb.
%
savedb:-!.
savedb:- on_x_rtrace(rsavedb),!.
%:-meta_predicate(rsavedb/0).



%% rsavedb is semidet.
%
% Rsavedb.
%
rsavedb:-
 on_x_rtrace(agenda_mpred_repropigate),
 catch((   
   ignore(catch(make_directory('/tmp/lm/'),_,true)),
   ignore(catch(delete_file('/tmp/lm/savedb'),E,(dmsginfo(E:delete_file('/tmp/lm/savedb'))))),   
   tell('/tmp/lm/savedb'),make_db_listing,told),E,dmsginfo(savedb(E))),!.





%% make_db_listing is semidet.
%
% Make Database Listing.
%
make_db_listing:-
 % get_user_abox(DBM),
%   listing(t),
 %  listing(mpred_f),
     listing(_),
     listing(lmconf:_),  
     listing(dbase:_),
     listing(dyn:_),
     listing(moo_loader:_),
     listing(world :_),
     listing(_),!.







%% hdr_debug( ?F, ?A) is semidet.
%
% Hdr Debug.
%
hdr_debug(_,_):-!.
hdr_debug(F,A):-'format'(F,A).
:- meta_predicate module_typed_term_expand(?,?).





%% module_typed_term_expand( ?X, ?UPARAM2) is semidet.
%
% Module Typed Term Expand.
%
module_typed_term_expand(X,_):-not(compound(X)),!,fail.
module_typed_term_expand( ((':-'(_))) , _ ):-!,fail.
module_typed_term_expand(_:B1,B2):-!,module_typed_term_expand(B1,B2),!.
module_typed_term_expand(X,Y):- compound(X),loading_module(CM),functor_catch(X,F,A),module_typed_term_expand(CM,X,F,A,Y).




%% module_typed_term_expand( ?CM, ?X, ?F, ?A, ?Y) is semidet.
%
% Module Typed Term Expand.
%
module_typed_term_expand(CM,X,F,A,Y):-findall(Y,term_expand_local_each(CM,X,F,A,Y),Ys), Ys == [],!,fail.  




%% term_expand_local_each( ?VALUE1, ?VALUE2, ?F, ?A, ?VALUE5) is semidet.
%
% Term Expand Local Each.
%
term_expand_local_each(_,_,F,A,_):- member(F / A,[never_expand]),!,fail.
term_expand_local_each(CM,X,F,A,X):-lmconf:registered_module_type(CM,utility),export(F/A).
term_expand_local_each(CM,X,F,A,X):-lmconf:registered_module_type(CM,dynamic),dynamic(F/A).





% ========================================
% include_mpred_file(MASK)
% ========================================




%% include_mpred_files( ?Mask) is semidet.
%
% Include Managed Predicate Files.
%
include_mpred_files(Mask):- 
     forall(must_locate_file(Mask,E),ensure_mpred_file_loaded(E)).
/*
module(M,Preds):-
    'format'(user_output /*e*/,'% visting module ~w.~n',[M]),
    forall(member(P,Preds),export(P)).
*/



%% scan_updates is semidet.
%
% Scan Updates.
%
scan_updates:-thread_property(X,alias(loading_code)),thread_property(X,status(running)),!.
scan_updates:-!.
scan_updates:-ignore(catch(make,_,true)).

/*
do_term_expansions:- source_context_module(CM), (do_term_expansions(CM)).

do_term_expansions(_):- thread_self(ID),lmconf:always_expand_on_thread(ID),!.
%do_term_expansions(_):- always_transform_heads,not(prevent_transform_mpreds),!.
do_term_expansions(_):- is_compiling_clause.
do_term_expansions(CM):- lmconf:registered_mpred_file(CM),!, not(ended_transform_mpreds), not(prevent_transform_mpreds).

check_term_expansions:- not(do_term_expansions).
*/

% :- (do_term_expansions(_)->true;throw(not_term_expansions)).


:- op(1120,fx,export),op(1120,fx,export).

:- was_export(((current_context_module/1,
    module_typed_term_expand/2,
         register_module_type/1,          
         end_module_type/1))).







%% loader_term_expansion( ?CL, ?EXP) is semidet.
%
% Loader Term Expansion.
%
loader_term_expansion(CL,EXP):- 
 % ==== why we assert
  inside_file(pfc),!,
% ==== do it
  WHY = '$si$':'$was_imported_kb_content$'(inside_file(pfc),CL),
  %dmsg(WHY),
  ain(CL),
  must(EXP=lmconf:WHY).

loader_term_expansion(CL,WHY):- 
% ==== why we assert
  requires_storage(CL,WhyRS),
% ==== do it
  WHY = '$si$':'$was_imported_kb_content$'(requires_storage(WhyRS),CL),
  %dmsg(WHY),
  add_from_file(CL),!.


loader_term_expansion(CL,EXP):- 
 % ==== why we assert
  inside_file(dyn),!,
% ==== do it
  WHY = '$si$':'$was_imported_kb_content$'(inside_file(dyn),CL),
  %dmsg(WHY),
  add_from_file(CL),
  must(EXP=lmconf:WHY).



:- use_module(library(lists)).
:- use_module(library(terms)).
:- use_module(library(base32)).

% :-autoload.

% https://docs.google.com/document/u/0/export?format=txt&id=1yyGne4g8vXKxNPKIKVLOtt0OxIM2kxyfmvjqR1lgbcY
% http_get
:- asserta_if_new(t_l:infForward).

:- dynamic(lmconf:mpred_skipped_module/1).



%% mpred_skipped_module( ?VALUE1) is semidet.
%
% Hook To [lmconf:mpred_skipped_module/1] For Module Mpred_loader.
% Managed Predicate Skipped Module.
%
lmconf:mpred_skipped_module(eggdrop).
:- forall(current_module(CM),assert(lmconf:mpred_skipped_module(CM))).
:- retractall(lmconf:mpred_skipped_module(pfc)).
% :-show_call(why,loading_module(X)),retractall(X).

%:-listing(lmconf:mpred_skipped_module/1).


:- shared_multifile(t_l:into_form_code).
:- thread_local(t_l:into_form_code).
:- thread_local(t_l:disable_px /0).
% :- dynamic(lmconf:disable_mpred_term_expansions_globally/0).

%fwc:-true.
%bwc:-true.

%is_fc_body(P):- cnotrace(fwc==P ; (compound(P),arg(1,P,E),is_fc_body(E))),!.
%is_bc_body(P):- cnotrace(bwc==P ; (compound(P),arg(1,P,E),is_bc_body(E))),!.



%% is_code_body( ?P) is semidet.
%
% If Is A Code Body.
%
is_code_body(P):- cnotrace(cwc==P ; (compound(P),arg(1,P,E),is_code_body(E))),!.


:- meta_predicate(with_source_module(:,(*))).



%% with_source_module( ?M, ?CALL) is semidet.
%
% Using Source Module.
%
with_source_module(M:_,CALL):- !, setup_call_cleanup('$set_source_module'(Old, M),CALL,'$set_source_module'(_, Old)).
with_source_module(M,CALL):- setup_call_cleanup('$set_source_module'(Old, M),CALL,'$set_source_module'(_, Old)).




%% get_file_type( ?File, ?Type) is semidet.
%
% Get File Type.
%
get_file_type(File,Type):-var(File),!,loading_source_file(File),get_file_type(File,Type).
get_file_type(File,Type):-lmcache:mpred_directive_value(Type,_,File).
get_file_type(File,Type):-file_name_extension(_,Type,File).




%% is_mpred_file( ?F) is semidet.
%
% If Is A Managed Predicate File.
%
is_mpred_file(F):- var(F),!,loading_source_file(F), is_mpred_file(F),!.
is_mpred_file(F):- file_name_extension(_,pfc,F),!.
is_mpred_file(F):- atom_concat(_,'.pfc.pl',F),!.
is_mpred_file(F):- file_name_extension(_,plmoo,F),!.
is_mpred_file(F):- lmconf:registered_mpred_file(F),!.
is_mpred_file(F):- lmconf:never_registered_mpred_file(F),!,fail.
is_mpred_file(F):- is_mpred_file0(F),!,asserta(lmconf:registered_mpred_file(F)),!.
is_mpred_file(F):- asserta(lmconf:never_registered_mpred_file(F)),!,fail.




%% decache_file_type( ?F) is semidet.
%
% Decache File Type.
%
decache_file_type(F):- 
  retractall(lmconf:mpred_is_impl_file(F)),
  retractall(lmconf:registered_mpred_file(F)),
  retractall(lmconf:never_registered_mpred_file(F)).




%% is_mpred_file0( ?F) is semidet.
%
% If Is A Managed Predicate File Primary Helper.
%
is_mpred_file0(F):- filematch(prologmud(**/*),F),!.
is_mpred_file0(F):- inside_file(pfc),!,loading_source_file(F).
is_mpred_file0(F):- file_name_extension(_,WAS,F),WAS\=pl,WAS\= '',WAS\=chr,!.




%% must_compile_special_clause( ?CL) is semidet.
%
% Must Be Successfull Compile Special Clause.
%
must_compile_special_clause(:- (_) ):-!,fail.
%must_compile_special_clause(CL):- sanity(nonvar(CL)),not(t_l:into_form_code),not(t_l:mpred_already_in_file_expansion(CL)),not((get_functor(CL,F),expanded_already_functor(F))).
must_compile_special_clause(CL):- \+ t_l:disable_px, 
   sanity(nonvar(CL)), \+(t_l:into_form_code),
    \+(t_l:mpred_already_in_file_expansion(CL)),
    \+((get_functor(CL,F),expanded_already_functor(F))),
   mpred_db_type(CL,_),!.

:- shared_multifile(t_l:mpred_module_expansion/1).
:- thread_local(t_l:mpred_module_expansion/1).




%% mpred_use_module( ?M) is semidet.
%
% Managed Predicate Use Module.
%
mpred_use_module(M):- must(atom(M)),retractall(lmconf:mpred_skipped_module(M)),show_call(why,asserta_if_new(t_l:mpred_module_expansion(M))).

% ================================================================================
% DETECT PREDS THAT NEED SPECIAL STORAGE 
% ================================================================================



%% compile_this( ?M, ?F, ?C, ?O) is semidet.
%
% Compile This.
%
compile_this(_,_,_,_):- t_l:disable_px,!,fail.
compile_this(_,F,M:C,O):-atom(M),!,compile_this(M,F,C,O).

compile_this(M,F,'$si$':'$was_imported_kb_content$'(_,_),pl):-!.
compile_this(M,F,C,ain):- is_mpred_file(F), \+ lmconf:mpred_skipped_module(M),!.
compile_this(M,F,C,dyn):- inside_file(dyn),!.
compile_this(M,F,C,O):- (var(M);var(F);var(C)),trace_or_throw(var_compile_this(M,F,C,O)).
compile_this(M,F,C,requires_storage(WHY)):- requires_storage(C,WHY),!.
compile_this(M,F,C,must_compile_special):- must_compile_special_clause(C),t_l:mpred_already_inside_file_expansion(C).
compile_this(_,_,_,pl).

:- was_module_transparent(mpred_may_expand).



%% mpred_may_expand is semidet.
%
% Managed Predicate May Expand.
%
mpred_may_expand:-loading_source_file(_F),inside_file(pfc).
mpred_may_expand:-loading_source_file(_F),inside_file(mpred).
mpred_may_expand:-must(loading_module(M)),mpred_may_expand_module(M),!,mpred_expand_inside_file_anyways.




%% mpred_may_expand_module( ?M) is semidet.
%
% Managed Predicate May Expand Module.
%
mpred_may_expand_module(M):-lmconf:mpred_skipped_module(M),!,fail.
mpred_may_expand_module(M):-lmcache:mpred_directive_value(pfc,module,M),!.
mpred_may_expand_module(M):-module_property(M,file(F)),is_mpred_file(F).
mpred_may_expand_module(M):-t_l:mpred_module_expansion(M),!.
mpred_may_expand_module(_):-t_l:mpred_module_expansion(*),!.






%% mpred_expand_inside_file_anyways is semidet.
%
% Managed Predicate Expand Inside File Anyways.
%
mpred_expand_inside_file_anyways:- loading_source_file(F),!,mpred_expand_inside_file_anyways(F).




%% mpred_expand_inside_file_anyways( ?F) is semidet.
%
% Managed Predicate Expand Inside File Anyways.
%
mpred_expand_inside_file_anyways(F):- var(F),!,loading_source_file(F),nonvar(F),mpred_expand_inside_file_anyways(F).
mpred_expand_inside_file_anyways(F):- t_l:loading_mpred_file(_,F),!.
mpred_expand_inside_file_anyways(F):- lmconf:registered_mpred_file(F).
mpred_expand_inside_file_anyways(F):- is_mpred_file(F),must(loading_module(M);source_module(M)), (M=user; \+ lmconf:mpred_skipped_module(M)),!.




%% was_exported_content( ?I, ?CALL, ?Output) is semidet.
%
% Was Exported Content.
%
was_exported_content(I,CALL,Output):-Output='$si$':'$was_imported_kb_content$'(I,CALL),!.

:- thread_local(t_l:mpred_term_expansion_ok/0).
:- thread_local(t_l:mpred_already_inside_file_expansion/1).

:- assert_if_new(t_l:mpred_term_expansion_ok).





%% mpred_provide_clauses( ?H, ?B, ?What) is semidet.
%
% Hook To [lmconf:mpred_provide_clauses/3] For Module Mpred_loader.
% Managed Predicate Provide Clauses.
%
lmconf:mpred_provide_clauses(H,B,(What)):- !.




%% show_interesting_cl( ?Dir, ?VALUE2) is semidet.
%
% Show Interesting Clause.
%
show_interesting_cl(_Dir,_).
show_interesting_cl(Dir,P):- loading_source_file(File),get_file_type(File,Type),
  ((nonvar(Dir),functor(Dir,Type,_))->true;dmsg(Type:cl_assert(Dir,P))).

:- meta_predicate(cl_assert(?,?)).



%% cl_assert( ?Dir, ?P) is semidet.
%
% Clause Assert.
%
cl_assert(kif(Dir),P):- show_if_debug(must_det_l(( show_interesting_cl(kif(Dir),P),kif_process(P)))),!.
cl_assert(Dir,P):- show_interesting_cl(Dir,P),ain(P).
cl_assert(pl,P):-  !, show_if_debug(must_det_l((source_location(F,_L), '$compile_aux_clauses'(P,F)))).
cl_assert(_Code,P):- !, show_if_debug(ain(P)).

:- meta_predicate(call_file_command(?,?,?,?)).
%call_file_command(_,cl_assert(pl,OO),OO,_):-!,show_interesting_cl(pl,OO).



%% call_file_command( ?I, ?CALL, ?OO, ?O) is semidet.
%
% Call File Command.
%
call_file_command(I,cl_assert(OTHER,OO),OO,I):- inside_file(kif),is_kif_rule(OO),!,call_file_command(I,cl_assert(kif(OTHER),OO),OO,I).
call_file_command(I,CALL,OO,O):- (current_predicate(_,CALL) -> ((must(call(CALL)),was_exported_content(I,CALL,OO))); OO=O).




%% in_include_file is semidet.
%
% In Include File.
%
in_include_file:- prolog_load_context(file,F),!, \+ prolog_load_context(source,F).

% ensure we only process onEndOfFile directive at the end of the actual source files



%% do_end_of_file_actions( ?VALUE1, ?VALUE2, ?VALUE3, ?VALUE4) is semidet.
%
% Do End Of File Actions.
%
do_end_of_file_actions(_,_,_,_):- in_include_file,!.
do_end_of_file_actions(term,_,_,_):- must(loading_source_file(F)),do_end_of_file_actions_real.




%% do_end_of_file_actions_real is semidet.
%
% Do End Of File Actions Real.
%
do_end_of_file_actions_real:- which_file(F),
   GETTER=t_l:on_eof(F,TODO),forall(GETTER,((doall(show_failure(why,TODO))),ignore(retract(GETTER)))).





%% mpred_implode_varnames( :TermN) is semidet.
%
% Managed Predicate Implode Varnames.
%
mpred_implode_varnames([]):-!.
mpred_implode_varnames([N=V|Vs]):-V='$VAR'(N),mpred_implode_varnames(Vs),!.

% mudKeyword("happy","happy") -> mudKeyword(vHappy,"happy").

% must skip already loaded modules (we remember these so make/0 doesnt break)



%% mpred_maybe_skip( ?M) is semidet.
%
% Managed Predicate Maybe Skip.
%
mpred_maybe_skip(M):- t_l:mpred_module_expansion(N),N==M,!.
mpred_maybe_skip(M):- asserta_if_new(lmconf:mpred_skipped_module(M)),!.
% :- forall(current_module(M),mpred_maybe_skip(M)).


:- dynamic(lmcache:mpred_directive_value/3).





%% expanded_already_functor( :TermARG1) is semidet.
%
% Expanded Already Functor.
%
expanded_already_functor('$si$':'$was_imported_kb_content$').
expanded_already_functor(was_enabled).
expanded_already_functor(_:NV):-nonvar(NV),!,expanded_already_functor(NV).

% expanded_already_functor(F):-mpred_isa(F,pl).


%:- thread_local is_compiling_clause/0.
%is_compiling:-is_compiling_clause;compiling.

%:- shared_multifile(user:term_expansion/2).
%:- shared_multifile(system:goal_expansion/2).
% system:goal_expansion(A,_B):-fail,hotrace((source_module(M),(M=mpred_sanity;M=user;M=system),if_defined(pmsg(M:goal_expansion(A)),format(user_output /*e*/,'~N% ~q~n',M:goal_expansion(A))))),fail.
% user:term_expansion(A,_B):-fail,hotrace((source_module(M),(M=mpred_sanity;M=user;M=system),if_defined(pmsg(M:term_expansion(A)),format(user_output /*e*/,'~N% ~q~n',M:term_expansion(A))))),fail.

% system:goal_expansion(N,mpred_prove_neg(P)):-fail,mpred_from_negation_plus_holder(N,P),show_failure(why,mpred_isa(P,pfcControlled)).




%% mpred_ops is semidet.
%
% Managed Predicate Oper.s.
%
mpred_ops:-  op(500,fx,('~')),op(500,fx,('~')),op(1075,xfx,('==>')), op(1075,xfx,('<==>')),op(1075,xfx,('<-')), op(1100,fx,('nesc')), op(1150,xfx,('::::')).



%% mpred_dcg_ops is semidet.
%
% Managed Predicate Dcg Oper.s.
%
mpred_dcg_ops:-  op(400,yfx,('\\\\')),op(1200,xfx,('-->>')),op(1200,xfx,('--*>>')), op(1200,xfx,('<<--')).

:- thread_local(mpred_ain_loaded).

% TODO DISABLE THIS NEXT CLAUSE LATER



%% mpred_directive_expansion( ?VALUE1, ?M) is semidet.
%
% Managed Predicate Directive Expansion.
%
mpred_directive_expansion(_,_):- (\+ current_predicate(logicmoo_bugger_loaded/0)),!,fail.
mpred_directive_expansion(_,_):- t_l:disable_px,!,fail.

mpred_directive_expansion(mpred_ops,mpred_ops). 
mpred_directive_expansion(mpred_dcg,( file_begin(pfc),mpred_dcg_ops)).
mpred_directive_expansion(mpred_module,(asserta(lmcache:mpred_directive_value(pfc,module,M)))):-context_module_of_file(M).
mpred_directive_expansion(mpred_multifile,include(logicmoo(mpred/'mpred_header.pi'))):- context_module_of_file(M),set_mpred_multifle(M).


set_mpred_module(M):- asserta(lmcache:mpred_directive_value(pfc,module,M)),'$set_source_module'(_,M).


%% set_mpred_multifle( ?M) is semidet.
%
% Set Managed Predicate Multifle.
%
set_mpred_multifle(M):- set_user_abox(M), asserta(lmcache:mpred_directive_value(pfc,multifile,M)).
  



% ========================================
% get_user_abox/1
% ========================================




%% get_user_tbox( ?T) is semidet.
%
% Get User Tbox.
%
get_user_tbox(T):-get_user_abox(M),to_tbox(M,T).



%% get_user_sbox( ?T) is semidet.
%
% Get User Sbox.
%
get_user_sbox(T):-get_user_abox(M),to_sbox(M,T).

:- thread_local(t_l:user_abox/1).



%% get_user_abox( ?Ctx) is semidet.
%
% Get User Abox.
%
get_user_abox(Ctx):- (t_l:user_abox(Out)),user_m_check(Out),!,must(Ctx=Out).
get_user_abox(Ctx):- current_context_module(Out),user_m_check(Out),!,must(Ctx=Out),set_user_abox(Ctx).


user_m_check(_Out).



%% set_user_abox( ?M) is semidet.
%
% Set User Abox.
%
set_user_abox(M):- user_m_check(M), (t_l:user_abox(Prev)->true;Prev=M),
   decl_user_abox(M),assert_until_eof(t_l:user_abox(M)),onEndOfFile(reset_user_abox(Prev)),!.




%% reset_user_abox( ?M) is semidet.
%
% Reset User Abox.
%
reset_user_abox(M):- ignore(show_failure(M\=user)),retractall(t_l:user_abox(Prev)),decl_user_abox(M),!,asserta(t_l:user_abox(M)).


% set_user_tbox(SM):- get_user_abox(M),ensure_support_module(SM),set_user_tbox(M,SM).





%% ensure_support_module( ?SM) is semidet.
%
% Ensure Support Module.
%
ensure_support_module(SM):-
   multifile(((SM:bt/3),(SM:nt/4),(SM:pk/4),(SM:pt/3),(SM:spft/5),(SM:tms/1),(SM:hs/1),(SM:qu/3),(SM:sm/1))),
     dynamic(((SM:bt/3),(SM:nt/4),(SM:pk/4),(SM:pt/3),(SM:spft/5),(SM:tms/1),(SM:hs/1),(SM:qu/3),(SM:sm/1))),
     (is_system_box(SM) -> true ; maybe_add_import_module(SM,basePFC,end)).





%% set_user_tbox( ?M, ?SM) is semidet.
%
% Set User Tbox.
%
set_user_tbox(M,SM):- 
   ( is_system_box(M) -> true ; maybe_add_import_module(M,SM,end)).





%% decl_user_abox( ?M) is semidet.
%
% Declare User Abox.
%
decl_user_abox(M):-     sanity(atom(M)),
                        asserta_if_new(mpred_loader:is_box_module(M,abox)),
                        to_tbox(M,T),
                        asserta_if_new(mpred_loader:is_box_module(T,tbox)),
                        ensure_tbox_module(T),
                        to_sbox(M,S),
                        asserta_if_new(mpred_loader:is_box_module(S,sbox)),!.
                        
                        





%% ensure_tbox_module( ?M) is semidet.
%
% Ensure Tbox Module.
%
ensure_tbox_module(M):- sanity(atom(M)),M== logicmoo_user,!.

ensure_tbox_module(baseKB) :- !.
ensure_tbox_module(user)   :- !.
ensure_tbox_module(M):-  (M==baseKB->trace_or_throw(bad_ensure_tbox_module(M));true),
  M:((
            multifile(M:('<-')/2),
            multifile(M:('::::')/2),
            multifile(M:('<==>'/2)),
            multifile(M:(('==>')/2)),
            multifile(M:('==>')/1),
            multifile(M:('~')/1),
            multifile(M:('nesc')/1),
            
            
            dynamic(M:('<-')/2),
            dynamic(M:('::::')/2),
            dynamic(M:('<==>'/2)),                   
            dynamic(M:(('==>')/2)),
            dynamic(M:('==>')/1),
            dynamic(M:('~')/1),
            dynamic(M:('nesc')/1),

            /*
            export(M:('<-')/2),
            export(M:('::::')/2),
            export(M:('<==>'/2)),                   
            export(M:(('==>')/2)),
            export(M:('==>')/1),
            export(M:('~')/1),
            export(M:('nesc')/1),
            */

            op(1199,fx,M:('==>')), % assert
            op(1199,fx,M:('?->')), % ask
            op(1190,xfy,M:('::::')), % Name something
            op(1180,xfx,M:('==>')), % Forward chaining
            op(1170,xfx,M:('<==>')), % Forward and backward chaining
            op(1160,xfx,M:('<==')), % backward chain PFC sytle
            op(1160,xfx,M:('<-')), % backward chain PTTP sytle (currely really PFC)
            op(1160,xfx,M:('<=')), % backward chain DRA sytle
            op(1150,xfx,M:('=>')), % Logical implication
            op(1130,xfx,(M:'<=>')), % Logical bi-implication
            op(600,yfx,(M:'&')), 
            op(600,yfx,(M:'v')),
            op(400,fx,(M:'~')),
            op(300,fx,(M:'-')),
            op(350,xfx,(M:'xor')))).



% ========================================
% begin/end_transform_mpreds
% ========================================
:- dynamic(current_op_alias/2).
:- dynamic(t_l:current_lang/1).


:- dynamic(always_expand_on_thread/1).
:- thread_local is_compiling_clause/0.



%% is_compiling is semidet.
%
% If Is A Compiling.
%
is_compiling:-is_compiling_clause;compiling.

:- style_check(+discontiguous).
:- style_check(-discontiguous).




%% begin_pfc is semidet.
%
% Begin Prolog Forward Chaining.
%
begin_pfc:-file_begin(pfc).



%% mpred_begin is semidet.
%
% Managed Predicate Begin.
%
mpred_begin:-file_begin(pfc).



%% dyn_begin is semidet.
%
% Dyn Begin.
%
dyn_begin:-file_begin(dyn).



%% dyn_end is semidet.
%
% Dyn End.
%
dyn_end:-file_end(dyn).




%% enable_mpred_expansion is semidet.
%
% Enable Managed Predicate Expansion.
%
enable_mpred_expansion:- (( \+ t_l:disable_px) -> true ;
                 (retractall(t_l:disable_px),
                 onEndOfFile(asserta_if_new(t_l:disable_px)))).




%% disable_mpred_expansion is semidet.
%
% Disable Managed Predicate Expansion.
%
disable_mpred_expansion:- (( t_l:disable_px) -> true ;
                 (asserta_if_new(t_l:disable_px),
                 onEndOfFile(retractall(t_l:disable_px)))).




%% is_box_module( ?M, ?VALUE2) is semidet.
%
% If Is A Datalog Module.
%
is_box_module(M,tbox):- is_system_box(M).
is_box_module(user,abox).





%% import_shared_pred( ?M, ?BaseKB, ?P) is semidet.
%
% Import Shared Predicate.
%
import_shared_pred(baseKB,_,_):-!.
import_shared_pred(M,BaseKB,P):- 
  functor(P,F,A),
  %dynamic(BaseKB:F/A),
  user:catch(mpred_op_prolog(pain,((M:P:- user:BaseKB:P))),E,dmsg(import_shared_pred(M:F/A:-BaseKB:F/A)=E)),
  must( \+ predicate_property(BaseKB:P,exported)),
  import_to_user(M:P).


predicate_is_undefined_fa(F,A):-
  \+ current_predicate(_:F/A),
  \+ predicate_property(_:F/A,exported),
  \+ predicate_property(_:F/A,static).


%% import_to_user( ?P) is semidet.
%
% Import Converted To User.
%
import_to_user(P):- '$module'(MM,MM),'$set_source_module'(SM,SM),must(import_to_user0(MM,SM,P)).

import_to_user0(user,user,M:FA):- must(M\==user),!, call_from_module(M,import_to_user(M:FA)).
import_to_user0(M,SM, user:FA):- M\==SM,dmsg(warn(import_to_user0(M,SM, user:FA))),fail.
import_to_user0(MM,SM,M:F/A):- !,functor(P,F,A),import_to_user_mfa0(MM,SM,P,M:F/A).
import_to_user0(MM,SM,M:P):-!,functor(P,F,A),import_to_user_mfa0(MM,SM,P,M:F/A).
import_to_user0(MM,SM,P):- t_l:user_abox(M),import_to_user0(MM,SM,M:P).


import_to_user_mfa0(_MM,_SM,_P,_M:F/A):- current_predicate(system:F/A),!.
import_to_user_mfa0(_MM,_SM,P,_M:_F/_A):- predicate_property(P,static),!.
import_to_user_mfa0(_MM,_SM,P,_M:_F/_A):- predicate_property(P,exported),!.
import_to_user_mfa0(_MM,_SM,P,_M:_F/_A):- predicate_property(P,imported_from(_)),!.
import_to_user_mfa0(_MM,_SM,_P,M:F/A):- current_predicate(check_never_assert/1),check_never_assert(declared(M:F/A)),fail.
import_to_user_mfa0(_MM,_SM,decl_type(A),baseKB:decl_type/1):-trace_or_throw(basd_import_to_user_mfa0(baseKB:decl_type/1)).



import_to_user_mfa0(_MM,_SM,_,M:F/A):- functor(P,F,A), 
 U=logicmoo_user,
 Rule = ((U:P:- user:loop_check_nr(M:P))),
 (clause_asserted(Rule)-> true; 
  ((
   user:catch(mpred_op_prolog(pain,Rule),E,dmsg(import_shared_pred(U:F/A:-M:F/A)=E)),
   user:export(U:F/A),
   catch(user:import(U:F/A),_,true)))).



%% import_module_to_user( ?M) is semidet.
%
% Import Module Converted To User.
%
import_module_to_user(M):- default_module(user,M),!.
import_module_to_user(M):- ignore(delete_import_module(M,user)),
                           add_import_module(M,system,end),
                           add_import_module(user,M,end),
                           % find system thru M
                           ignore(delete_import_module(user,system)).




%% ensure_imports( ?M) is semidet.
%
% Ensure Imports.
%
ensure_imports(baseKB):-!.
ensure_imports(M):-ensure_imports_tbox(M,baseKB).

:-multifile(lmcache:is_ensured_imports_tbox/2).
:-dynamic(lmcache:is_ensured_imports_tbox/2).




%% skip_user( ?M) is semidet.
%
% Skip User.
%
skip_user(M):-
  system:add_import_module(M,system,end),  
  ignore(system:delete_import_module(M,user)).
  %ignore(system:delete_import_module(user,system)).
  %asserta((M:import(P):-system:import(P))),
  



%% ensure_imports_tbox( ?M, ?BaseKB) is semidet.
%
% Ensure Imports Tbox.
%
ensure_imports_tbox(M,BaseKB):-M==BaseKB,!.
ensure_imports_tbox(M,BaseKB):-
  lmcache:is_ensured_imports_tbox(M,BaseKB),!.
ensure_imports_tbox(M,BaseKB):-
  asserta(lmcache:is_ensured_imports_tbox(M,BaseKB)),
  mpred_loader:
  must_det((
   %maybe_add_import_module(BaseKB,mpred_loader,end),
   %maybe_add_import_module(M,mpred_loader,end),
   forall((system:current_module(IM), \+ mpred_loader:is_box_module(IM,_)),maybe_add_import_module(M,IM,end)),
   forall((system:current_module(IM),\+ mpred_loader:is_box_module(IM,_)),maybe_add_import_module(BaseKB,IM,end)),
   % mpred_loader:skip_user(BaseKB),
   %ignore(system:delete_import_module(user,BaseKB)),
   %ignore(system:delete_import_module(BaseKB,user)),
   ignore(system:delete_import_module(M,BaseKB)),
   ignore(system:delete_import_module(BaseKB,M)),
   forall((prolog:current_predicate(_,BaseKB:P),\+predicate_property(BaseKB:P,imported_from(_))),mpred_loader:import_shared_pred(M,BaseKB,P)),
   % maybe_add_import_module(user,BaseKB,end),
   % maybe_add_import_module(BaseKB,system,end),
  %= maybe_add_import_module(M,user,end),
   %maybe_add_import_module(BaseKB,M,end),
   %mpred_loader:skip_user(M),
   %=ignore(system:delete_import_module(M,user)),
   %=system:add_import_module(user,M,end),
   %=ignore(system:delete_import_module(user,system)), % gets from M now
   !)).

:-multifile(lmconf:locked_baseKB/0).
:-dynamic(lmconf:locked_baseKB/0).




%% not_boot_module( ?ABox) is semidet.
%
% Not Boot Module.
%
not_boot_module(ABox):-
  ABox\==mpred_loader,
  ABox\==boot_system,
  ABox\==system_markers,
  ABox\==system_singleValued,
  ABox\==system_genls,
  ABox\==system_if_missing,
  ABox\==common_logic_clif,
  ABox\==system_mdefault.




%% best_module( ?List, ?ABox) is semidet.
%
% Best Module.
%
best_module(List,ABox):-member(ABox,List),ABox\==user,not_boot_module(ABox), \+ is_system_box(ABox),!.
best_module(List,ABox):-member(ABox,List),ABox\==user,not_boot_module(ABox),!.
best_module(_List,baseKB):-!.




%% file_begin( ?W) is semidet.
%
% File Begin.
%
file_begin(W):-
 must_det_l((
   '$module'(CM,CM),
   '$set_source_module'(SM,SM),
   onEndOfFile(module(CM)),
   onEndOfFile('$set_source_module'(_,SM)),
   onEndOfFile('$module'(_,CM)),
   (loading_source_file(Source)->true;Source=CM),
   make_module_name(Source,FM),
   context_module_of_file(M),
   (t_l:user_abox(AM)->true;AM=SM),
   best_module([AM,SM,CM,FM,M],ABox),
   dmsg(best_module(W-[AM,SM,CM,FM,M],ABox)),
   must(set_user_abox(ABox)),
   set_current_module(ABox),
   ABox:mpred_ops,
   op_lang(W),   
   assert_until_eof(lmcache:mpred_directive_value(W,file,Source)),
   decache_file_type(Source),
   ensure_imports(ABox),
   enable_mpred_expansion,
   SM:mpred_ops)).




%% set_current_module( ?ABox) is semidet.
%
% Set Current Module.
%
set_current_module(user):- set_mpred_module(baseKB),!.
set_current_module(ABox):- module(ABox),'$module'(_,ABox),'$set_source_module'(_,ABox).




%% file_end( ?W) is semidet.
%
% File End.
%
file_end(W):- must_det(( loading_source_file(ISource),decache_file_type(ISource),ignore(retract(lmcache:mpred_directive_value(W,file,ISource))))),  
  must_det(( loading_source_file(Source),decache_file_type(Source),ignore(retract(lmcache:mpred_directive_value(W,file,Source))))).




%% inside_file( ?W) is semidet.
%
% Inside File.
%
inside_file(W) :- prolog_load_context(file,Source),lmcache:mpred_directive_value(W,_,Source),!.
inside_file(W) :- prolog_load_context(source,Source),lmcache:mpred_directive_value(W,_,Source),!.
inside_file(W) :- loading_source_file(Source),!,lmcache:mpred_directive_value(W,_,Source),!.





%% term_expansion( :TermFDecl, ?Clause) is semidet.
%
% Hook To [system:term_expansion/2] For Module Mpred_loader.
% Term Expansion.
%
user:term_expansion((:- (M:DIR)),O):-atom(M),atom(DIR),with_source_module(M, ((mpred_directive_expansion(DIR,OO),!, must(O=(:- OO))))).
user:term_expansion((:- DIR),O):- atom(DIR), mpred_directive_expansion(DIR,OO),!,must(O=(:- OO)).

:- meta_predicate(load_file_term_to_command_0c(?,?)).
:- meta_predicate(mpred_term_expansion(?,?)).
:- meta_predicate(mpred_term_expansion(?,?)).

% Specific "*SYNTAX*" based default

% :- ensure_loaded(logicmoo(snark/common_logic_sexpr)).




%% op_alias( ?OP, ?OTHER) is semidet.
%
% Oper. Alias.
%
op_alias(OP,OTHER):-retractall(current_op_alias(OP,_)),asserta(current_op_alias(OP,OTHER)).



%% op_lang( ?LANG) is semidet.
%
% Oper. Language.
%
op_lang(LANG):-retractall(current_op_alias(_,_)), retract(t_l:current_lang(Was)),!,assert_until_eof(t_l:current_lang(LANG)), onEndOfFile(asserta(t_l:current_lang(Was))).
op_lang(LANG):- assert_until_eof(t_l:current_lang(LANG)).




%% get_op_alias( ?OP, ?ALIAS) is semidet.
%
% Get Oper. Alias.
%
get_op_alias(OP,ALIAS):-current_op_alias(OP,ALIAS).
get_op_alias(OP,ALIAS):-get_lang(LANG),lang_op_alias(LANG,OP,ALIAS).

% current_op_alias((<==>),dup(impliesF,(','))).
% current_op_alias((=>),==>).
% current_op_alias((not),(~)).



%% current_op_alias( ?VALUE1, ?VALUE2) is semidet.
%
% Current Oper. Alias.
%
current_op_alias( not(:-),~(:-)).
current_op_alias( (:-),(:-)).




%% get_lang( ?LANG) is semidet.
%
% Get Language.
%
get_lang(LANG):-t_l:current_lang(LANG),!.
get_lang(pfc).

% pfc



%% lang_op_alias( ?VALUE1, ?VALUE2, ?VALUE3) is semidet.
%
% Language Oper. Alias.
%
lang_op_alias(pfc,(<==>),(<==>)).
lang_op_alias(pfc,(==>),==>).
% lang_op_alias(pfc,(<=>),(<==>)).
lang_op_alias(pfc,(<=),(<-)).
lang_op_alias(pfc,(<-),(<-)).
lang_op_alias(pfc,(not),(~)).
lang_op_alias(pfc,not(:-),~(:-)).
lang_op_alias(pfc,(:-),(:-)).
% lang_op_alias(pfc,(A=B),{(A=B)}).
% kif
lang_op_alias(kif,(<==>),(<==>)).
lang_op_alias(kif,(==>),==>).
lang_op_alias(kif,(not),(~)).
lang_op_alias(kif,(~),(~)).
lang_op_alias(kif,(=>),(if)).
lang_op_alias(kif,(<=>),(iff)).
lang_op_alias(kif, not(':-'),~('<-')).
lang_op_alias(kif,(:-),rev(==>)).
% cyc
lang_op_alias(cyc,(<==>),(<==>)).
lang_op_alias(cyc,(==>),==>).
lang_op_alias(cyc,(implies),(if)).
lang_op_alias(cyc,(equiv),(iff)).
lang_op_alias(cyc, not(':-'),~('<-')).
lang_op_alias(cyc,(:-),rev(==>)).
% prolog
lang_op_alias(prolog,(<==>),(<==>)).
lang_op_alias(prolog,(==>),==>).
lang_op_alias(prolog, not(':-'),~('<-')).
lang_op_alias(prolog,(:-),(:-)).
lang_op_alias(prolog,(<=),(<=)).
lang_op_alias(prolog,(<-),(<-)).




%% transform_opers( ?LANG, ?PFCM, ?PFCO) is semidet.
%
% Transform Opers.
%
transform_opers(LANG,PFCM,PFCO):- w_tl(t_l:current_lang(LANG),((transitive_lc(transform_opers_0,PFCM,PFC),!, subst(PFC,(not),(~),PFCO)))).

:- op(1199,fx,('==>')).
:- op(1190,xfx,('::::')).
:- op(1180,xfx,('==>')).
:- op(1170,xfx,'<==>').
:- op(1160,xfx,('<-')).
:- op(1150,xfx,'=>').
:- op(1140,xfx,'<=').
:- op(1130,xfx,'<=>').
:- op(1100,fx,('nesc')).
:- op(300,fx,'-').
:- op(300,fx,'~').
:- op(600,yfx,'&'). 
:- op(600,yfx,'v').
:- op(1075,xfx,'<-').
:- op(350,xfx,'xor').
:- op(1100,fx,(shared_multifile)).




%% transform_opers_0( ?AIS, ?AIS) is semidet.
%
% transform opers  Primary Helper.
%
transform_opers_0(AIS,AIS):- if_defined(leave_as_is(AIS)),!.
transform_opers_0((A/B),C):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]),conjoin_op((/),AA,BB,C).
transform_opers_0(PFCM,PFC):- transform_opers_1(PFCM,PFC),!.
transform_opers_0(=>(A),=>(C)):- !, transform_opers_0(A,C).
transform_opers_0(==>(A),==>(C)):- !, transform_opers_0(A,C).
transform_opers_0(~(A),~(C)):- !, transform_opers_0(A,C).
transform_opers_0(nesc(A),nesc(C)):- !, transform_opers_0(A,C).
transform_opers_0({A},{A}):-!.
transform_opers_0((A;B),C):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]),conjoin_op((;),AA,BB,C).
transform_opers_0((B=>A),(BB=>AA)):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]).
transform_opers_0((B==>A),(BB==>AA)):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]).
transform_opers_0(<=(A,B),<=(AABB)):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]).
transform_opers_0((A<-B),(AA<-BB)):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]).
transform_opers_0((A<=>B),(AA<=>BB)):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]).
transform_opers_0((A<==>B),(AA<==>BB)):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]).
transform_opers_0((A<==>B),(AA<==>BB)):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]).
transform_opers_0(if(A,B),if(AA,BB)):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]).
transform_opers_0(iff(A,B),iff(AA,BB)):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]).
transform_opers_0(implies(A,B),implies(AA,BB)):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]).
transform_opers_0(equiv(A,B),equiv(AA,BB)):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]).
transform_opers_0((B:-A),OUTPUT):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]),=((BB:-AA),OUTPUT).
transform_opers_0(not(A),OUTPUT):- !, must_maplist(transform_opers_0,[A],[AA]),=(not(AA),OUTPUT).
transform_opers_0(not(A),C):- !, transform_opers_0(~(A),C).
%transform_opers_0((A),OUTPUT):- !, must_maplist(transform_opers_0,[A],[AA]),=((AA),OUTPUT).
transform_opers_0(O,O).




%% transform_opers_1( ?AB, ?BBAA) is semidet.
%
% transform opers  Secondary Helper.
%
transform_opers_1(not(AB),(BBAA)):- get_op_alias(not(OP),rev(OTHER)), atom(OP),atom(OTHER),AB=..[OP,A,B],!, must_maplist(transform_opers_0,[A,B],[AA,BB]),BBAA=..[OTHER,BB,AA].
transform_opers_1(not(AB),(BOTH)):- get_op_alias(not(OP),dup(OTHER,AND)),atom(OTHER), atom(OP),AB=..[OP,A,B],!, must_maplist(transform_opers_0,[A,B],[AA,BB]),AABB=..[OTHER,AA,BB],BBAA=..[OTHER,BB,AA],BOTH=..[AND,AABB,BBAA].
transform_opers_1(not(AB),~(NEG)):- get_op_alias(not(OP),~(OTHER)),atom(OTHER), atom(OP),AB=..[OP|ABL],!, must_maplist(transform_opers_0,ABL,AABB),NEG=..[OTHER|AABB].
transform_opers_1(not(AB),(RESULT)):- get_op_alias(not(OP),(OTHER)), atom(OP),atom(OTHER),AB=..[OP|ABL],!, must_maplist(transform_opers_0,ABL,AABB),RESULT=..[OTHER|AABB].
transform_opers_1((AB),(BBAA)):- get_op_alias(OP,rev(OTHER)), atom(OP),atom(OTHER),AB=..[OP,A,B],!, must_maplist(transform_opers_0,[A,B],[AA,BB]),BBAA=..[OTHER,BB,AA].
transform_opers_1((AB),(BOTH)):- get_op_alias(OP,dup(OTHER,AND)), atom(OP),atom(OTHER),AB=..[OP,A,B],!, must_maplist(transform_opers_0,[A,B],[AA,BB]),AABB=..[OTHER,AA,BB],BBAA=..[OTHER,BB,AA],BOTH=..[AND,AABB,BBAA].
transform_opers_1((AB),(RESULT)):- get_op_alias(OP,(OTHER)),atom(OP), atom(OTHER),AB=..[OP|ABL],!, must_maplist(transform_opers_0,ABL,AABB),RESULT=..[OTHER|AABB].
transform_opers_1(OP,OTHER):- get_op_alias(OPO,OTHER),OPO=OP,!.

%% Possibly should term expand since we are in the userKb modules



%% in_mpred_kb_module is semidet.
%
% In Managed Predicate Knowledge Base Module.
%
in_mpred_kb_module:- source_context_module(MT),fail,get_user_abox(MT).




%% load_file_term_to_command_or_fail( ?I, ?Y) is semidet.
%
% Load File Term Converted To Command Or Fail.
%
load_file_term_to_command_or_fail(I,Y):- 
   once(fully_expand(change(assert,ain),I,X)),
   expand_term(X,M),!,
   must_load_file_term_to_command(M,Y),!.




%% is_prolog_xform( :TermV) is semidet.
%
% If Is A Prolog Xform.
%
is_prolog_xform((:-(V))):-!,nonvar(V).
is_prolog_xform((?-(V))):-!,nonvar(V).
is_prolog_xform((:-(V,_))):-!,nonvar(V).
is_prolog_xform(_:(:-(V,_))):-!,nonvar(V).




%% must_load_file_term_to_command( ?I, ?O) is semidet.
%
% Must Be Successfull Load File Term Converted To Command.
%
must_load_file_term_to_command(I,O):- in_mpred_kb_module,!, must((expand_in_mpred_kb_module(I,O),is_prolog_xform(O))),!.
must_load_file_term_to_command(I,O):- must(load_file_term_to_command_0c(I,O)),!.





%% expand_in_mpred_kb_module( ?I, ?O) is semidet.
%
% Expand In Managed Predicate Knowledge Base Module.
%
expand_in_mpred_kb_module(I,O):-is_prolog_xform(I),must(I=O),!.
expand_in_mpred_kb_module(I,OO):- load_file_term_to_command_0c(I,O),!,
   ( is_prolog_xform(O) -> (OO = O); OO=  (:- cl_assert(pfc(in_mpred_kb_module),O))),!.





%% load_file_term_to_command_0c( ?I, ?OO) is semidet.
%
% Load File Term Converted To Command 0c.
%
load_file_term_to_command_0c(I,OO):-
   convert_if_kif_string(I,Wff,Vs,O),
   must(must_load_file_term_to_command(O,OO)).

load_file_term_to_command_0c(PI,OO):- PI=..[P,I],
   convert_if_kif_string(I,Wff,Vs,O),
   PO=..[P,O], must(must_load_file_term_to_command(PO,OO)).

load_file_term_to_command_0c(C,O):- compound(C), get_op_alias(OP,ALIAS),
  atom(OP),atom(ALIAS),C=..[OP|ARGS],CC=..[ALIAS|ARGS],loop_check(must_load_file_term_to_command(CC,O)),!.

load_file_term_to_command_0c(C,O):- get_lang(LANG),transform_opers(LANG,C,M),C\=@=M,!,must_load_file_term_to_command(M,O).

load_file_term_to_command_0c(C,O):- hotrace(ensure_vars_labled(C,M)), must_mpred_term_expansion_2(M,O),!.




%% must_mpred_term_expansion_2( ?M, ?O) is semidet.
%
% Must Be Successfull Managed Predicate term expansion  Extended Helper.
%
must_mpred_term_expansion_2(M,O):- mpred_term_expansion(M,O),!.
must_mpred_term_expansion_2(M,(:-  cl_assert(pfc(in_mpred_kb_module),(M)))):- !. % in_mpred_kb_module,!.
must_mpred_term_expansion_2(M,O):- must(mpred_term_expansion(M,O)),!.





%% mpred_term_expansion( ?Fact, ?Output) is semidet.
%
% Managed Predicate Term Expansion.
%
mpred_term_expansion((<=(Q,P)),(:- cl_assert(pfc(bwc),(Q<-P)))).
mpred_term_expansion(((P==>Q)),(:- cl_assert(pfc(fwc),(P==>Q)))).
mpred_term_expansion((('=>'(Q))),(:- cl_assert(pfc(fwc),('=>'(Q))))).
mpred_term_expansion((('==>'(Q))),(:- cl_assert(pfc(fwc),('=>'(Q))))).
mpred_term_expansion(((nesc(Q))),(:- cl_assert(pfc(fwc),nesc(Q)))).
mpred_term_expansion(('<-'(P,Q)),(:- cl_assert(pfc(bwc),('<-'(P,Q))))).
mpred_term_expansion(('<==>'(P,Q)),(:- cl_assert(pfc(bwc),(P<==>Q)))).
mpred_term_expansion(~(Q),(:- cl_assert(pfc(fwc),~(Q)))).
mpred_term_expansion(~(Q),(:- cl_assert(pfc(fwc),~(Q)))).

mpred_term_expansion(if(P,Q),(:- cl_assert(kif(fwc),if(P,Q)))).
mpred_term_expansion(iff(P,Q),(:- cl_assert(kif(fwc),iff(P,Q)))).
mpred_term_expansion(not(Q),(:- cl_assert(kif(fwc),not(Q)))).
mpred_term_expansion(exists(V,PQ),(:- cl_assert(kif(fwc),exists(V,PQ)))).
mpred_term_expansion(forall(V,PQ),(:- cl_assert(kif(fwc),forall(V,PQ)))).
mpred_term_expansion(all(V,PQ),(:- cl_assert(kif(fwc),all(V,PQ)))).


% maybe reverse some rules?
%mpred_term_expansion((P==>Q),(:- cl_assert(pfc(fwc),('<-'(Q,P))))).  % speed-up attempt
mpred_term_expansion((RuleName :::: Rule),(:- cl_assert(named_rule,(RuleName :::: Rule)))).
mpred_term_expansion((==>(P)),(:- cl_assert(pfc(fwc),(==>(P))))).
mpred_term_expansion(Fact,(:- cl_assert(pl,Fact))):- get_functor(Fact,F,_A),(a(prologDynamic,F)).
mpred_term_expansion(Fact,Output):- load_file_term_to_command_1(_Dir,Fact,C),must(mpred_term_expansion(C,Output)),!.




%% load_file_term_to_command_1( ?Type, :TermIn, :TermOut) is semidet.
%
% load file term Converted To command  Secondary Helper.
%
      load_file_term_to_command_1(pfc(act),(H:-(Chain,B)),(PFC==>PH)):-cwc, is_action_body(Chain),pl_to_mpred_syntax((Chain,B),PFC),pl_to_mpred_syntax_h(H,PH).
      load_file_term_to_command_1(pfc(awc),(H:-(Chain,B)),(PH==>PFC)):-cwc, has_body_atom(twc,Chain),pl_to_mpred_syntax((Chain,B),PFC),pl_to_mpred_syntax_h(H,PH).
      load_file_term_to_command_1(pfc(fwc),(H:-(Chain,B)),(PFC==>PH)):-cwc, is_fc_body(Chain),pl_to_mpred_syntax((Chain,B),PFC),pl_to_mpred_syntax_h(H,PH),can_be_dynamic(PH),make_dynamic(PH).
      load_file_term_to_command_1(pfc(bwc),(H:-(Chain,B)),(PH<-PFC)):-cwc, is_bc_body(Chain),pl_to_mpred_syntax((Chain,B),PFC),pl_to_mpred_syntax_h(H,PH),can_be_dynamic(PH),make_dynamic(PH).
      load_file_term_to_command_1(Type,In,Out):-load_file_term_to_command_1b(Type,In,Out),!.




%% load_file_term_to_command_1b( ?VALUE1, :TermH, :TermH) is semidet.
%
% Load File Term Converted To Command 1b.
%
      load_file_term_to_command_1b(pfc(act),(H:-Chain,B),(H==>{(Chain,B)})):-cwc, is_action_body(Chain),make_dynamic(H).
      load_file_term_to_command_1b(pfc(fwc),(H:-Chain,B),((Chain,B)==>H)):-cwc, is_fc_body(Chain),make_dynamic(H).
      load_file_term_to_command_1b(pfc(bwc),(H:-Chain,B),(H<-(Chain,B))):-cwc, is_bc_body(Chain),make_dynamic(H).
      

/*
*/





%% can_be_dynamic( ?H) is semidet.
%
% Can Be Dynamic.
%
can_be_dynamic(H):- \+ is_static_pred(H), \+ predicate_property(H,static),  \+ predicate_property(H,meta_predicate(_)).




%% pl_to_mpred_syntax_h( ?A, ?PFC_A) is semidet.
%
% Pl Converted To Managed Predicate Syntax Head.
%
pl_to_mpred_syntax_h(A,PFC_A):- must(pl_to_mpred_syntax0(A,PFC_A)),!, PFC_A \= '{}'(_).



%% pl_to_mpred_syntax( ?A, ?PFC_A) is semidet.
%
% Pl Converted To Managed Predicate Syntax.
%
pl_to_mpred_syntax(A,PFC_A):- must(pl_to_mpred_syntax0(A,PFC_A)),!.




%% pl_to_mpred_syntax0( ?A, ?A) is semidet.
%
% Pl Converted To Managed Predicate Syntax Primary Helper.
%
pl_to_mpred_syntax0(A,A):-is_ftVar(A),!.
pl_to_mpred_syntax0((A,B),PFC):-!,pl_to_mpred_syntax(A,PFC_A),pl_to_mpred_syntax(B,PFC_B),conjoin_body(PFC_A,PFC_B,PFC).
pl_to_mpred_syntax0(pfc(A),A):-!.
pl_to_mpred_syntax0(A,{A}):-!.

% mpred_term_expansion((H:-Chain,B),(H:-(B))):- is_code_body(Chain),!,fail,must(atom(Chain)),make_dynamic(H).


% Specific "*PREDICATE CLASS*" based default
mpred_term_expansion(Fact,Fact):- get_functor(Fact,F,A),(a(prologDynamic,F)),!.
mpred_term_expansion(Fact,(:- ((cl_assert(Dir,Fact))))):- load_file_term_to_command_2(Dir,Fact,_Output),!.


      % Specific "*PREDICATE CLASS*" based default



%% load_file_term_to_command_2( ?VALUE1, ?Fact, ?Output) is semidet.
%
% load file term Converted To command  Extended Helper.
%
      load_file_term_to_command_2(pfc(pred_type),Fact,Output):- get_functor(Fact,F,A),req(ttPredType(F)),Output='$si$':'$was_imported_kb_content$'(Fact,ttPredType(F)),!.
      load_file_term_to_command_2(pfc(func_decl),Fact,Output):- get_functor(Fact,F,A),req(functorDeclares(F)),Output='$si$':'$was_imported_kb_content$'(Fact,functorDeclares(F)),!.
      load_file_term_to_command_2(pfc(macro_head),Fact,Output):- get_functor(Fact,F,A),req(prologMacroHead(F)),Output='$si$':'$was_imported_kb_content$'(Fact,prologMacroHead(F)),!.
      load_file_term_to_command_2(pfc(mpred_ctrl),Fact,Output):- get_functor(Fact,F,A),req(pfcControlled(F)),Output='$si$':'$was_imported_kb_content$'(Fact,pfcControlled(F)),!.
      load_file_term_to_command_2(pfc(hybrid),Fact,Output):- get_functor(Fact,F,A),req(prologHybrid(F)),Output='$si$':'$was_imported_kb_content$'(Fact,pfcControlled(F)),!.
      load_file_term_to_command_2(pfc(pl),Fact,Output):- get_functor(Fact,F,A),(a(prologDynamic,F)),Output='$si$':'$was_imported_kb_content$'(Fact,pfcControlled(F)),!.
      load_file_term_to_command_2(mpred(in_mpred_kb_module),Fact,Output):- in_mpred_kb_module,Output=Fact,!.


% Specific "*FILE*" based default
mpred_term_expansion(Fact,Fact):- inside_file(pl),!.
mpred_term_expansion(Fact,(:- ((cl_assert(pfc(mpred_file),Fact))))):- inside_file(pfc),!.
mpred_term_expansion(Fact,(:- ((cl_assert(dyn(dyn_file),Fact))))):- inside_file(dyn),!.
mpred_term_expansion(Fact,(:- ((cl_assert(mpred(mpreds_file),Fact))))):- inside_file(mpreds),!.
mpred_term_expansion(Fact,(:- ((cl_assert(mpred(in_mpred_kb_module),Fact))))):- in_mpred_kb_module,!.

/*
mpred_term_expansion(Fact,(:- ((cl_assert(pfc(expand_file),Fact))))):-
    cnotrace(mpred_expand_inside_file_anyways(F)),!,_Output='$si$':'$was_imported_kb_content$'(Fact,mpred_expand_inside_file_anyways(F)),!.
*/




%% stream_pos( :TermFile) is semidet.
%
% Stream Pos.
%
stream_pos(File:LineNo):-loading_source_file(File),current_input(S),stream_property(S, position(Position)), !,stream_position_data(line_count, Position, LineNo),!.




%% compile_clause( ?CL) is semidet.
%
% Compile Clause.
%
compile_clause(CL):- must((make_dynamic(CL),assertz_if_new(CL),clause_asserted(CL))).




%% make_dynamic( ?C) is semidet.
%
% Make Dynamic.
%
make_dynamic(C):- compound(C),get_functor(C,F,A),must(F\=='$VAR'),
  functor(P,F,A),
  ( \+predicate_property(P,_) -> kb_dynamic(F/A) ; (predicate_property(P,dynamic)->true;dynamic_safe(P))),!,
  import_to_user(F/A),
  must((predicate_property(P,dynamic))).

% once(lmconf:mpred_is_impl_file(F);asserta(lmconf:mpred_is_impl_file(F))).

%user:goal_expansion(G,OUT):- \+  t_l:disable_px, G\=isa(_,_),(use_was_isa(G,I,C)),!,to_isa_out(I,C,OUT).
%user:term_expansion(G,OUT):- \+  t_l:disable_px, hotrace(use_was_isa(G,I,C)),!,to_isa_out(I,C,OUT).
%user:term_expansion(I,O):- \+ t_l:disable_px, t_l:consulting_sources, wno_tl(t_l:consulting_sources,ain(I)),O=true.



% :-set_prolog_flag(allow_variable_name_as_functor,true).

% :- source_location(S,_),forall(loading_source_file(H,S),ignore(( \+predicate_property(M:H,built_in), functor(H,F,A),M:module_transparent(F/A),M:export(F/A)))).





%% mpred_process_input_1( :TermT) is semidet.
%
% Managed Predicate process input  Secondary Helper.
%
mpred_process_input_1(':-'(TT)):-!,must(TT),!.
mpred_process_input_1('?-'(TT)):-!,doall(must(TT)),!.
mpred_process_input_1('$si$':'$was_imported_kb_content$'(_,_)):-!.
mpred_process_input_1(T):-try_save_vars(T),ain(T),!.



%% mpred_process_input( ?T, ?Vs) is semidet.
%
% Managed Predicate Process Input.
%
mpred_process_input(':-'(T),Vs):- put_variable_names( Vs),must(T),!.
mpred_process_input(T,Vs):- expand_term(T,TT),put_variable_names( Vs),mpred_process_input_1(TT),!.




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
process_this_script0(S):- at_end_of_stream(S),!.
process_this_script0(S):- peek_string(S,3,W), W="\n\n\n",get_code(S,_),get_code(S,_),!,process_this_script0(S).
process_this_script0(S):- peek_string(S,2,W), W="\r\n",get_code(S,_),!,process_this_script0(S).
process_this_script0(S):- peek_string(S,2,W), W="\n\n",get_code(S,_),!,process_this_script0(S).
process_this_script0(S):- peek_code(S,W),member(W,`\n`),get_code(S,P),put(P),!,process_this_script0(S).
process_this_script0(S):- peek_code(S,W),member(W,` \t\r`),get_code(S,_),!,process_this_script0(S).
process_this_script0(S):- peek_string(S,2,W),W="%=",!,read_line_to_string(S,String),format('~N~s~n',[String]).
process_this_script0(S):- peek_string(S,1,W),W="%",!,read_line_to_string(S,_String).
process_this_script0(S):- read_term(S,T,[variable_names(Vs)]),put_variable_names( Vs),format('~N~n',[]),portray_one_line(T),format('~N~n',[]),!,
  must(mpred_process_input(T,Vs)).


:- use_module(library(shlib)).
:- use_module(library(operators)).

:- source_location(F,_),asserta(lmconf:never_registered_mpred_file(F)).
% filetypes 
%
%  pfc - all terms are sent to ain/1 (the the execeptions previously defined)
%  pl - all terms are sent to compile_clause/1 (the the execeptions previously defined)
%  prolog - all terms are sent to compile_clause/1 (even ones defined conflictingly)
%  dyn - all terms are sent to ain/1 (even ones defined conflictingly)

:- thread_local(t_l:pretend_loading_file/1).



%% loading_source_file( ?F) is semidet.
%
% Loading Source File.
%
loading_source_file(F):-once(t_l:pretend_loading_file(F);prolog_load_context(source,F);loading_file(F);'$module'(F,F)).


:- dynamic(lmconf:never_reload_file/1).




%% load_language_file( ?Name0) is semidet.
%
% Load Language File.
%
load_language_file(Name0):- 
 forall(filematch_ext('qlf',Name0,Name),
  ((
   w_tl([(user:term_expansion(_,_):-!,fail),(user:goal_expansion(_,_):-!,fail),(system:term_expansion(_,_):-!,fail),(system:goal_expansion(_,_):-!,fail)],
     gripe_time(1,(lmconf:load_files(Name,[qcompile(auto),register(false),if(not_loaded  )])->asserta(lmconf:never_reload_file(Name));retract(lmconf:never_reload_file(Name)))))))),!.
 





%% never_load_special( :TermARG1, ?Options) is semidet.
%
% Never Load Special.
%
never_load_special(_, Options) :-memberchk(must_be_module(true),Options).
never_load_special(_Module:Spec, _) :- atom(Spec), atomic_list_concat(M,'.',Spec),length(M,L),L>7.
never_load_special(_Module:library(Atom), Options) :- atom(Atom),member(must_be_module(true),Options),member(if(not_loaded),Options).
never_load_special(_Module:_Spec, Options) :- member(must_be_module(true),Options),member(if(not_loaded),Options),member(imports([_/_]),Options).   


:- use_module(logicmoo(util/logicmoo_util_filesystem)).
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
   catch(prolog_load_file_nlc(ModuleSpec, Options),E,(nop((trace,prolog_load_file_nlc(ModuleSpec, Options))),throw(E))).




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
  with_mpred_expansions(w_tl(t_l:pretend_loading_file(File),
              must((file_begin(pfc),
                    load_files(M:File,Options))))),!.




%% load_file_some_type( :TermM, ?Options) is semidet.
%
% Load File Some Type.
%
load_file_some_type(M:File,Options):-must(load_files(M:File,Options)),!.







%% disable_mpreds_in_current_file is semidet.
%
% Disable Managed Predicates In Current File.
%
disable_mpreds_in_current_file:- loading_source_file(F),show_call(why,asserta((t_l:disable_px:-loading_source_file(F),!))).

:- thread_local(tlbugger:no_buggery_tl/0).
:- was_export(with_no_mpred_expansions/1).
:- meta_predicate(with_no_mpred_expansions(0)).



%% with_no_mpred_expansions( :GoalGoal) is semidet.
%
% Using No Managed Predicate Expansions.
%
with_no_mpred_expansions(Goal):-
  w_tl(tlbugger:no_buggery_tl,
    w_tl(t_l:disable_px,Goal)).


:- was_export(with_mpred_expansions/1).
:- meta_predicate(with_mpred_expansions(0)).



%% with_mpred_expansions( :GoalGoal) is semidet.
%
% Using Managed Predicate Expansions.
%
with_mpred_expansions(Goal):-
  wno_tl(tlbugger:no_buggery_tl,
    wno_tl(t_l:disable_px,Goal)).

:- was_export(ensure_loaded_no_mpreds/1).
:- meta_predicate(ensure_loaded_no_mpreds(0)).



%% ensure_loaded_no_mpreds( :GoalF) is semidet.
%
% Ensure Loaded No Managed Predicates.
%
ensure_loaded_no_mpreds(F):-with_no_mpred_expansions(forall(must_locate_file(F,L),ensure_loaded(L))).




%% use_was_isa( ?G, ?I, ?C) is semidet.
%
% use was  (isa/2).
%
use_was_isa(G,I,C):-call((current_predicate(_,_:mpred_types_loaded/0),if_defined(was_isa_syntax(G,I,C)))).




%% current_context_module( ?Ctx) is semidet.
%
% Current Context Module.
%
current_context_module(Ctx):-notrace((lmconf:loading_module(Ctx))),!.
current_context_module(Ctx):-notrace((source_context_module(Ctx))).

% ========================================
% register_module_type/end_module_type
% ========================================
:- was_module_transparent(lmconf:register_module_type/1).
:- shared_multifile(lmconf:registered_module_type/2).




%% register_module_type( ?Type) is semidet.
%
% Register Module Type.
%
register_module_type(Type):-current_context_module(CM),register_module_type(CM,Type).



%% register_module_type( ?CM, ?Types) is semidet.
%
% Register Module Type.
%
register_module_type(CM,Types):-is_list(Types),!,forall(member(T,Types),register_module_type(CM,T)).
register_module_type(CM,Type):-asserta_new(lmconf:registered_module_type(CM,Type)).

:- was_export(end_module_type/2).



%% end_module_type( ?Type) is semidet.
%
% End Module Type.
%
end_module_type(Type):-current_context_module(CM),end_module_type(CM,Type).



%% end_module_type( ?CM, ?Type) is semidet.
%
% End Module Type.
%
end_module_type(CM,Type):-retractall(lmconf:registered_module_type(CM,Type)).

/******

% :- meta_predicate(ensure_mpred_file_loaded(0)). 

:- meta_predicate ensure_mpred_file_loaded(:,+).


ensure_mpred_file_loaded(M:F0,List):-!,
  must_locate_file(M:F0,F),  % scope_settings  expand(true),register(false),
  % 'format'(user_output /*e*/,'%  ~q + ~q -> ~q.~n',[M,F0,F]),
  load_files([F],[if(not_loaded), must_be_module(true)|List]).
   %load_files(F,[redefine_module(false),if(not_loaded),silent(false),exported(true),must_be_module(true)|List]).   
ensure_mpred_file_loaded(M:F0,List):-
  must_locate_file(M:F0,F),  % scope_settings
  'format'(user_output /*e*/,'% load_mpred_file_M ~q.~n',[M=must_locate_file(F0,F)]),
   load_files([F],[redefine_module(false),module(M),expand(true),if(not_loaded),exported(true),register(false),silent(false),must_be_module(true)|List]).

******/


:- was_export(declare_load_dbase/1).



%% declare_load_dbase( ?Spec) is semidet.
%
% Declare Load Dbase.
%
declare_load_dbase(Spec):- forall(no_repeats_old(File,must_locate_file(Spec,File)),show_call(why,asserta_if_new(lmconf:registered_mpred_file(File)))).

% :- was_export((is_compiling_sourcecode/1)).



%% is_compiling_sourcecode is semidet.
%
% If Is A Compiling Sourcecode.
%
is_compiling_sourcecode:-is_compiling,!.
is_compiling_sourcecode:-compiling, current_input(X),not((stream_property(X,file_no(0)))),prolog_load_context(source,F),\+((t_l:loading_mpred_file(_,_))),F=user,!.
is_compiling_sourcecode:-compiling,dmsg(system_compiling),!.

:- was_export(load_mpred_files/0).



%% load_mpred_files is semidet.
%
% Load Managed Predicate Files.
%
load_mpred_files :- forall(lmconf:registered_mpred_file(File),ensure_mpred_file_loaded(File)).


% =======================================================
% :- meta_predicate show_load_call(0).
% show_load_call(C):- logOnFailure(on_x_rtrace(show_call(why,C))).



:- dynamic(lmconf:loaded_file_world_time/3).
:- meta_predicate(lmconf:loaded_file_world_time(+,+,+)).
:- meta_predicate(get_last_time_file(+,+,+)).



%% get_last_time_file( +FileIn, +World, +LastTime) is semidet.
%
% Get Last Time File.
%
get_last_time_file(FileIn,World,LastTime):- absolute_file_name(FileIn,File),lmconf:loaded_file_world_time(File,World,LastTime),!.
get_last_time_file(_,_,0).



:- meta_predicate(load_init_world(+,:)).



%% load_init_world( +World, ?File) is semidet.
%
% Load Init World.
%
load_init_world(World,File):- 
 wno_tl(lmconf:use_cyc_database,
    ( world_clear(World),
      retractall(lmconf:loaded_file_world_time(_,_,_)),
      time_call(ensure_mpred_file_loaded(File)),!,
      time_call(finish_processing_world))).


:- meta_predicate(ensure_mpred_file_loaded(:)).




%% ensure_mpred_file_loaded( ?MFileIn) is semidet.
%
% Ensure Managed Predicate File Loaded.
%
ensure_mpred_file_loaded(MFileIn):- strip_module(MFileIn,M,_), 
 forall(must_locate_file(MFileIn,File),
   must_det_l((time_file_safe(File,NewTime),!,
   get_last_time_file(File,_World,LastTime),
   (LastTime<NewTime -> force_reload_mpred_file(M:File) ; true)))).

:- meta_predicate(force_reload_mpred_file(?)).

:- meta_predicate(ensure_mpred_file_loaded(+,:)).



%% ensure_mpred_file_loaded( +World, ?FileIn) is semidet.
%
% Ensure Managed Predicate File Loaded.
%
ensure_mpred_file_loaded(World,FileIn):- 
  w_tl(get_user_abox(World),ensure_mpred_file_loaded(FileIn)).




%% must_locate_file( ?FileIn, ?File) is semidet.
%
% Must Be Successfull Locate File.
%
must_locate_file(FileIn,File):-
  must(filematch_ext(['','mpred','ocl','moo','plmoo','pl','plt','pro','p','pl.in','pfc','pfct'],FileIn,File)).


:- meta_predicate call_from_module(+,0).



%% call_from_module( +NewModule, :GoalGoal) is semidet.
%
% Call Converted From Module.
%
call_from_module(NewModule,Goal):-
   '$module'(OldCModule, NewModule),
   '$set_source_module'(OldModule, NewModule),
   call_with_module(NewModule,call_cleanup('@'(Goal,NewModule),('$set_source_module'(_, OldModule),'$module'(_, OldCModule)))).

:- meta_predicate call_with_source_module(+,0).



%% call_with_source_module( +NewModule, :GoalGoal) is semidet.
%
% Call Using Source Module.
%
call_with_source_module(NewModule,Goal):-
   '$set_source_module'(OldModule, NewModule),
   call_with_module(NewModule,call_cleanup('@'(Goal,NewModule),'$set_source_module'(_, OldModule))).
:- meta_predicate call_with_module(+,0).



%% call_with_module( +NewModule, :GoalGoal) is semidet.
%
% Call Using Module.
%
call_with_module(NewModule,Goal):-
   '$module'(OldModule, NewModule),
   call_cleanup('@'(Goal,NewModule),'$module'(_, OldModule)).

:- meta_predicate(ensure_mpred_file_loaded(:)).



%% force_reload_mpred_file( ?FileIn) is semidet.
%
% Force Reload Managed Predicate File.
%
force_reload_mpred_file(FileIn):- 
  must((get_user_abox(World),force_reload_mpred_file(World,FileIn))).




%% force_reload_mpred_file( ?World, ?MFileIn) is semidet.
%
% Force Reload Managed Predicate File.
%
force_reload_mpred_file(World,MFileIn):- strip_module(MFileIn,NewModule,_), 
 call_with_source_module(NewModule,((
 NewModule:ensure_loaded(logicmoo(mpred/mpred_userkb)),
 forall(must_locate_file(MFileIn,File),
   must_det_l((
   once(show_success(prolog_load_file,get_user_abox(DBASE));DBASE=NewModule),
   sanity(exists_file(File)),sanity(get_user_abox(World)),
   mpred_remove_file_support(File),
   assert_if_new(lmconf:registered_mpred_file(File)),
   must(time_file_safe(File,NewTime)),
   retractall(lmconf:loaded_file_world_time(File,World,_)),
   assert(lmconf:loaded_file_world_time(File,World,NewTime)),    
   DBASE = DBASE,
   wno_tl(t_l:disable_px,
     show_call((call_with_source_module(NewModule,load_files(NewModule:File, [module(NewModule)]))))),
   catch((w_tl(t_l:loading_mpred_file(World,File),     
      load_mpred_on_file_end(World,File))),
    Error,
    (wdmsg(error(Error,File)),retractall(lmconf:loaded_mpred_file(World,File)),
     retractall(lmconf:loaded_file_world_time(File,World,_AnyTime)))))))))).

:- was_export(load_mpred_on_file_end/2).



%% load_mpred_on_file_end( ?World, ?File) is semidet.
%
% Load Managed Predicate Whenever File End.
%
load_mpred_on_file_end(World,File):-
   must(atom(File)),
   asserta_new(lmconf:loaded_mpred_file(World,File)),
   dmsginfo(info(load_mpred_file_complete(File))),
   forall(t_l:on_eof(File,Call),must((on_f_log_ignore(Call),retractall(t_l:on_eof(File,Call))))).










%load_mpred_name_stream(_Name):- do_gc,repeat,read_one_term(Term,Vs),myDebugOnError(add_term(Term,Vs)),Term == end_of_file,!.
%load_mpred_name_stream(_Name,Stream):- do_gc,repeat,read_one_term(Stream,Term,Vs),myDebugOnError(add_term(Term,Vs)),Term == end_of_file,!.


/*
:- ensure_loaded(plarkc(mpred_sexpr_reader)).

:- parse_to_source(
  "(documentation instance EnglishLanguage \"An object is an &%instance of a &%SetOrClass if it is included in that &%SetOrClass. 
  An individual may be an instance of many classes, some of which may be subclasses of others. 
  Thus, there is no assumption in the meaning of &%instance about specificity or uniqueness.\")",
  Out),writeq(Out).
*/




%% is_kif_string( ?String) is semidet.
%
% If Is A Knowledge Interchange Format String.
%
is_kif_string([]):- !,fail.
is_kif_string(String):-atomic(String),name(String,Codes), memberchk(40,Codes),memberchk(41,Codes).




%% convert_if_kif_string( ?I, ?Wff, ?Vs, ?O) is semidet.
%
% Convert If Knowledge Interchange Format String.
%
convert_if_kif_string(I,Wff,Vs,O):-is_kif_string(I),must_det_l((input_to_forms(atom(I),Wff,Vs),put_variable_names(Vs),!,must((sexpr_sterm_to_pterm(Wff,O),!,\+ is_list(O))))).




%% from_kif_string( ?String, ?Forms) is semidet.
%
% Converted From Knowledge Interchange Format String.
%
from_kif_string(String,Forms) :- must((codelist_to_forms(String,Forms);input_to_forms(string(String),Forms))),!.




%% assert_kif( ?String) is semidet.
%
% Assert Knowledge Interchange Format.
%
assert_kif(String):- from_kif_string(String,Forms),dmsg(warn(assert_kif(Forms))),!.




%% assert_kif_dolce( ?String) is semidet.
%
% Assert Knowledge Interchange Format Dolce.
%
assert_kif_dolce(String):-from_kif_string(String,Forms),dmsg(warn(assert_kif_dolce(Forms))),!.


:- meta_predicate(doall_and_fail(0)).




%% finish_processing_world is semidet.
%
% Finish Processing World.
%
finish_processing_world :- load_mpred_files, loop_check(w_tl(t_l:agenda_slow_op_do_prereqs,doall(finish_processing_dbase)),true).




%% doall_and_fail( :GoalCall) is semidet.
%
% Doall And Fail.
%
doall_and_fail(Call):- time_call(once(doall(Call))),fail.






%% loader_side_effect_verify_only( ?I, ?Supposed) is semidet.
%
% Loader Side Effect Verify Only.
%
loader_side_effect_verify_only(I,Supposed):-   
   sanity(var(ActualSupposed)),
    push_predicates(t_l:side_effect_buffer/3,STATE),
    load_file_term_to_command_or_fail(I,Supposed),
    current_source_location(Why),
    collect_expansions(Why,I,Actual),
    convert_side_effect(suppose(Supposed),S),
    conjoin(S, Actual,ActualSupposed),
    conjuncts_to_list(ActualSupposed,Readable),
    assert(actual_side_effect(I,Readable)),
    pop_predicates(t_l:side_effect_buffer/3,STATE),!.




%% loader_side_effect_capture_only( ?I, ?ActualSupposed) is semidet.
%
% Loader Side Effect Capture Only.
%
loader_side_effect_capture_only(I,ActualSupposed):-   
   sanity(var(ActualSupposed)),
    push_predicates(t_l:side_effect_buffer/3,STATE),
    load_file_term_to_command_or_fail(I,Supposed),
    current_source_location(Why),
    collect_expansions(Why,I,Actual),
    conjoin(Actual,Supposed,ActualSupposed),
    pop_predicates(t_l:side_effect_buffer/3,STATE),!.





%% collect_expansions( ?Why, ?I, ?I) is semidet.
%
% Collect Expansions.
%
collect_expansions(_Why,I,I):- \+ t_l:side_effect_buffer(_Op,_Data,_),!.
collect_expansions(NWhy,_I, TODO):- findall(ReproduceSWhy, 
  ( retract(t_l:side_effect_buffer(Op, Data, Why)),
    must_det_l(convert_side_effect(Op, Data,Reproduce)),
    must(simplify_why_r(Reproduce,Why,NWhy,ReproduceSWhy))), TODOs),
   must_det_l( list_to_conjuncts(TODOs,TODO)).




%% simplify_why_r( ?Reproduce, ?Why, ?NWhy, :TermReproduce) is semidet.
%
% Simplify Generation Of Proof R.
%
simplify_why_r(Reproduce,Why,NWhy,   Reproduce):- Why==NWhy, !.
simplify_why_r(Reproduce,Why,_,Reproduce:SWhy):-simplify_why(Why,SWhy),!.
 
% aliases
:- meta_predicate(convert_side_effect(?,+,-)).




%% simplify_why( ?Why, ?SWhy) is semidet.
%
% Simplify Generation Of Proof.
%
simplify_why(Why,SWhy):-var(Why),!,Why=SWhy.
simplify_why(Why:0,SWhy):-!,simplify_why(Why,SWhy).
simplify_why(Why:N,SWhy:N):-!,simplify_why(Why,SWhy).
simplify_why(Why,SWhy):- atom(Why),!,directory_file_path(_,SWhy,Why).
simplify_why(Why,Why).




%% convert_side_effect( ?C, +A, -SE) is semidet.
%
% Convert Side Effect.
%
convert_side_effect(M:C,A,SE):- Call=..[C,A],!,convert_side_effect(M:Call,SE).
convert_side_effect(C,A,SE):- Call=..[C,A],!,convert_side_effect(Call,SE).




%% convert_side_effect( ?I, ?OO) is semidet.
%
% Convert Side Effect.
%
convert_side_effect(suppose(OO), suppose(Result)):- convert_side_effect_0a(OO,Result),!.
convert_side_effect(I,OO):-convert_side_effect_0c(I,O),((O=(N-_V),number(N))->OO=O;OO=O),!.




%% convert_side_effect_0a( ?I, ?O) is semidet.
%
% Convert Side Effect 0a.
%
convert_side_effect_0a(asserta(Data), (  a(DataR))):-convert_side_effect_0a(Data,DataR).
convert_side_effect_0a(assertz(Data), (  (DataR))):-convert_side_effect_0a(Data,DataR).
convert_side_effect_0a(retract(Data), (  r(DataR))):-convert_side_effect_0a(Data,DataR).
convert_side_effect_0a(cl_assert(Why,Data), (  cl_assert(Why,DataR))):-convert_side_effect_0a(Data,DataR).
convert_side_effect_0a(attvar_op(Why,Data),Reproduce):-!,convert_side_effect(Why,Data,Reproduce),!.
convert_side_effect_0a(I,O):-convert_side_effect_0b(I,O),!.
convert_side_effect_0a(I,I).




%% convert_side_effect_0b( :TermOpData, ?Result) is semidet.
%
% Convert Side Effect 0b.
%
convert_side_effect_0b((OpData:-TRUE),Result):- is_true(TRUE),!,convert_side_effect_0a(OpData,Result),!.
convert_side_effect_0b(suppose(OpData),Result):-!,convert_side_effect_0a(OpData,Result),!.
convert_side_effect_0b(lmconf:OpData,Reproduce):- !,convert_side_effect_0a(OpData,Reproduce),!.
convert_side_effect_0b(( :- OpData),( ( (Result)))):-!,convert_side_effect_0a(OpData,Result),!.
convert_side_effect_0b('$si$':'$was_imported_kb_content$'(_, OO),Result):-!,convert_side_effect_0a(OO,Result),!.
convert_side_effect_0b(asserta_if_new(Data),Result):-!,convert_side_effect_0a(asserta(Data),Result).
convert_side_effect_0b(assertz_if_new(Data),Result):-!,convert_side_effect_0a(assertz(Data),Result).
convert_side_effect_0b(assert_if_new(Data),Result):-!,convert_side_effect_0a(assertz(Data),Result).
convert_side_effect_0b(assert(Data),Result):-!,convert_side_effect_0a(assertz(Data),Result).




%% convert_side_effect_0c( ?OpData, ?Reproduce) is semidet.
%
% Convert Side Effect 0c.
%
convert_side_effect_0c(OpData,Reproduce):- convert_side_effect_0b(OpData,Reproduce),!.
convert_side_effect_0c(OpData,Reproduce):- show_success(convert_side_effect,convert_side_effect_buggy(OpData,Reproduce)),!.
convert_side_effect_0c(OpData,Reproduce):- trace_or_throw(unknown_convert_side_effect(OpData,Reproduce)),!.

% todo



%% convert_side_effect_buggy( ?H, ?HB) is semidet.
%
% Convert Side Effect Buggy.
%
convert_side_effect_buggy(erase(clause(H,B,_Ref)), (e(HB))):- convert_side_effect_0a((H:-B),HB).
convert_side_effect_buggy(retract(Data), (r(DataR))):-convert_side_effect_0a(Data,DataR).
convert_side_effect_buggy(retractall(Data), (c(DataR))):-convert_side_effect_0a(Data,DataR).
convert_side_effect_buggy(OpData,( (  error_op(OpData)))):-dmsg(unknown_convert_side_effect(OpData)).





%% clear_predicates( :TermM) is semidet.
%
% Clear Predicates.
%
clear_predicates(M:H):- forall(M:clause(H,_,Ref),erase(Ref)).



%% push_predicates( :TermM, ?STATE) is semidet.
%
% Push Predicates.
%
push_predicates(M:F/A,STATE):- functor(H,F,A),findall((H:-B), (M:clause(H,B,Ref),erase(Ref)), STATE).



%% pop_predicates( :TermM, ?STATE) is semidet.
%
% Pop Predicates.
%
pop_predicates(M:F/A,STATE):- functor(H,F,A),forall(member((H:-B),STATE),M:assert((H:-B))).

:- source_location(S,_),forall(source_file(H,S),(functor(H,F,A),export(F/A),module_transparent(F/A))).

:- multifile(user:prolog_load_file/2).
:- dynamic(user:prolog_load_file/2).




%% prolog_load_file( ?ModuleSpec, ?Options) is semidet.
%
% Hook To [user:prolog_load_file/2] For Module Mpred_loader.
% Prolog Load File.
%
user:prolog_load_file(ModuleSpec, Options):- current_predicate(_,_:mpred_loader_file),
  \+ never_load_special(ModuleSpec, Options),  catch(prolog_load_file_loop_checked(ModuleSpec, Options),E,((trace,prolog_load_file_loop_checked(ModuleSpec, Options),throw(E)))).





%% mpred_loader_file is semidet.
%
% Managed Predicate Loader File.
%
mpred_loader_file.

:- asserta_if_new((user:term_expansion(I,O):- mpred_expander(term,user,I,O))).
:- asserta_if_new((system:goal_expansion(I,O):- mpred_expander(goal,system,I,O))).



