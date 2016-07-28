/* 
% Game loading Utils
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_loader.pl
%:- if(((current_prolog_flag(xref,true),current_prolog_flag(pldoc_x,true));current_prolog_flag(autoload_logicmoo,true))).
:- module(mpred_loader,
          [ add_from_file/1,
          % unused_assertion/1,
          assert_until_eof/2,
          assert_until_eof/1,
          mpred_ops/0,setup_module_ops/1,
          set_file_lang/1,
          pfc_dcg/0,
          get_original_term_source/1,

          set_lang/1,
           simplify_language_name/2,
           %is_undefaulted/1,
          must_map_preds/3,

          current_op_alias/2,
            show_load_call/1,
            add_term/2,
            assert_kif/1,
            % system:import_module_to_user/1,
            assert_kif_dolce/1,
            
            make_file_command/3,
            % import_shared_pred/3,
            % import_to_user0/3,
            % import_to_user_mfa0/4,

            predicate_is_undefined_fa/2,
            
            same_language/2,
            call_file_command0/4,
            is_compiling_clause/0,
            to_prolog_xform/2,
            mpred_ain_loaded/0,


            begin_pfc/0,
            call_file_command/4,
            can_be_dynamic/1,
            cl_assert/2,
            clear_predicates/1,
            collect_expansions/3,
            compile_clause/1,
            mpred_term_expansion_by_storage_type/3,
            convert_side_effect/2,
            convert_side_effect/3,
            convert_side_effect_buggy/2,
            current_context_module/1,
            % cwc/0,
            decache_file_type/1,
            mpred_ops/0,
            setup_module_ops/1,
            mpred_op_each/1,
            mpred_op_unless/4,
            declare_load_dbase/1,
            disable_mpred_expansion/0,
            disable_mpreds_in_current_file/0,
            do_end_of_file_actions/4,
            do_end_of_file_actions_real/0,
            dyn_begin/0,
            dyn_end/0,
            enable_mpred_expansion/0,
            end_module_type/1,
            end_module_type/2,
            ensure_loaded_no_mpreds/1,

            % ensure_prolog_file_consulted/2, ensure_mpred_file_consulted/2,
            ensure_mpred_file_loaded/1,
            ensure_mpred_file_loaded/2,
            
            etrace/0,
            expand_in_mpred_kb_module/2,
            expanded_already_functor/1,
            file_begin/1,
            file_end/1,
            finish_processing_world/0,
            force_reload_mpred_file/1,
            force_reload_mpred_file/2,
            force_reload_mpred_file2/2,
            from_kif_string/2,
            convert_from_kif/2,
            convert_if_kif_string/2,
            get_file_type/2,
            get_lang/1,
            get_lang0/1,
            get_last_time_file/3,
            get_op_alias/2,
            gload/0,
            hdr_debug/2,
            include_mpred_files/1,
            include_prolog_files/1,
            get_lang0/1,
            is_code_body/1,
            is_compiling/0,
            is_compiling_sourcecode/0,
            is_kif_string/1,
            is_directive_form/1,
            is_mpred_file/1,
            lang_op_alias/3,
            expand_term_to_load_calls/2,
            mpred_expander_now_physically/3,
            load_init_world/2,
            load_language_file/1,
            load_mpred_files/0,
            load_mpred_on_file_end/2,
            loader_side_effect_capture_only/2,
            loader_side_effect_verify_only/2,
            must_expand_term_to_command/2,
            loading_source_file/1,
            make_db_listing/0,
            make_dynamic/1,
           (make_dynamic_ilc)/1,
            module_typed_term_expand/2,
            module_typed_term_expand/5,
            mpred_begin/0,
            mpred_expand_inside_file_anyways/0,
            mpred_expand_inside_file_anyways/1,
            mpred_te/6,
            mpred_file_term_expansion/4,
            dont_term_expansion/2,
            mpred_file_term_expansion/4,
            
            mpred_expand_file_module_clause/4,
            mpred_expand_file_module_clause/4,
            mpred_implode_varnames/1,
            
            mpred_prolog_only_file/1,
            mpred_may_expand/0,
            mpred_may_expand_module/1,
            mpred_maybe_skip/1,
            
            baseKB:mpred_skipped_module/1,
            mpred_term_expansion/2,
            mpred_use_module/1,
            must_compile_special_clause/1,
            expand_term_to_load_calls/2,
            must_locate_file/2,
            maybe_locate_file/2,
            myDebugOnError/1,
            onEndOfFile/1,
            op_alias/2,
            op_lang/1,
            pl_to_mpred_syntax/2,
            pl_to_mpred_syntax_h/2,
            pop_predicates/2,
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
            
            transform_opers/3,
            
            use_was_isa/3,
            was_exported_content/3,
            with_mpred_expansions/1,
            with_no_mpred_expansions/1,
            lmcache:mpred_directive_value/3,

            baseKB:loaded_file_world_time/3,
            baseKB:mpred_provide_clauses/3,
            baseKB:never_reload_file/1,
            always_expand_on_thread/1,
            t_l:current_lang/1,

            baseKB:mpred_skipped_module/1,
            %prolog_load_file_loop_checked/2,
%            registered_module_type/2,
            %t_l:disable_mpred_term_expansions_globally/0,
            %t_l:into_form_code/0,
            %t_l:mpred_module_expansion/1,
            
            user:term_expansion/2,
            mpred_loader_module_transparent/1,
            convert_side_effect_0a/2, convert_side_effect_0b/2, 
            convert_side_effect_0c/2, guess_if_mpred_file0/1, expand_term_to_load_calls/2, load_file_term_to_command_1/3, 
            load_file_term_to_command_1b/3, mpred_term_expansion_by_pred_class/3, 
            must_expand_term_to_command/2, pl_to_mpred_syntax0/2, 
            
            transform_opers_0/2, transform_opers_1/2,
            mpred_loader_file/0
          ]).
%:- endif.

 :- module_transparent((load_file_term_to_command_1b/3,pfc_dcg/0, mpred_term_expansion_by_pred_class/3,
   must_expand_term_to_command/2, pl_to_mpred_syntax0/2, 
    
    transform_opers_0/2, transform_opers_1/2)).

 :- meta_predicate
        % make_reachable(?,?),
        call_file_command(?, ?, ?, ?),
        cl_assert(?, ?),
        show_bool(0),
        convert_side_effect(?, +, -),
        
        ensure_loaded_no_mpreds(0),
        ensure_mpred_file_loaded(:),
        ensure_mpred_file_loaded(+, :),
        force_reload_mpred_file(?),
        force_reload_mpred_file2(+,+),
        get_last_time_file(+, +, +),
        expand_term_to_load_calls(?, ?),
        mpred_expander_now_physically(?, ?, ?),
        load_init_world(+, :),
        module_typed_term_expand(?, ?),
        mpred_te(+, +, +,+, -,-),
        mpred_op_each(3),
        mpred_term_expansion(?, ?),
        myDebugOnError(0),        
        with_mpred_expansions(0),
        with_no_mpred_expansions(0),
        mpred_loader_module_transparent(?),       
        baseKB:loaded_file_world_time(+, +, +).
:- multifile((t_l:into_form_code/0, t_l:mpred_module_expansion/1, user:term_expansion/2)).
:- (dynamic   user:term_expansion/2).
% :- (module_transparent add_from_file/1, add_term/2, assert_kif/1, assert_kif_dolce/1, assert_until_eof/2,assert_until_eof/1, begin_pfc/0, call_file_command/4, 
% call_from_module/2, with_source_module/2, can_be_dynamic/1, cl_assert/2, clear_predicates/1, collect_expansions/3, compile_clause/1,
%  mpred_term_expansion_by_storage_type/3, convert_side_effect/2, convert_side_effect/3, convert_side_effect_0a/2, convert_side_effect_0b/2, convert_side_effect_0c/2, 
% convert_side_effect_buggy/2, current_context_module/1, current_op_alias/2, cwc/0, decache_file_type/1, ensure_abox/1, declare_load_dbase/1, 
% disable_mpred_expansion/0, disable_mpreds_in_current_file/0, do_end_of_file_actions/4, do_end_of_file_actions_real/0, dyn_begin/0, dyn_end/0, enable_mpred_expansion/0, end_module_type/1, end_module_type/2, ensure_loaded_no_mpreds/1, ensure_mpred_file_consulted/2, ensure_mpred_file_loaded/1, ensure_mpred_file_loaded/2, ensure_prolog_file_consulted/2, etrace/0, expand_in_mpred_kb_module/2, expanded_already_functor/1, file_begin/1, file_end/1, finish_processing_world/0, force_reload_mpred_file/1, 
%  force_reload_mpred_file2/2, force_reload_mpred_file/2, from_kif_string/2, get_file_type/2, get_lang/1, get_last_time_file/3, get_op_alias/2, gload/0, guess_file_type_loader/2, hdr_debug/2, in_include_file/0, in_mpred_kb_module/0, include_mpred_files/1, get_lang/1, is_code_body/1, is_compiling/0, is_compiling_sourcecode/0, is_kif_string/1, is_mpred_file/1, guess_if_mpred_file0/1, lang_op_alias/3, load_file_dir/2, load_file_some_type/2, expand_term_to_load_calls/2, load_file_term_to_command_1/3, load_file_term_to_command_1b/3, mpred_term_expansion_by_pred_class/3, expand_term_to_load_calls/2, expand_term_to_load_calls/4, load_init_world/2, load_language_file/1, load_mpred_files/0, load_mpred_on_file_end/2, loader_side_effect_capture_only/2, loader_side_effect_verify_only/2, expand_term_to_command/2, loading_source_file/1, make_db_listing/0, make_dynamic/1, module_typed_term_expand/2, module_typed_term_expand/5, mpred_begin/0,  mpred_expand_inside_file_anyways/0, mpred_expand_inside_file_anyways/1, mpred_te/4, mpred_expander_now/2, mpred_expand_file_module_clause/4, mpred_implode_varnames/1, mpred_loader_file/0, mpred_may_expand/0, mpred_may_expand_module/1, mpred_maybe_skip/1, mpred_process_input/2, mpred_process_input_1/1, baseKB:mpred_skipped_module/1, mpred_term_expansion/2, mpred_use_module/1, must_compile_special_clause/1, expand_term_to_load_calls/2, must_locate_file/2, must_expand_term_to_command/2, myDebugOnError/1, onEndOfFile/1, op_alias/2, op_lang/1, pl_to_mpred_syntax/2, pl_to_mpred_syntax0/2, pl_to_mpred_syntax_h/2, pop_predicates/2, process_this_script/0, process_this_script/1, process_this_script0/1, prolog_load_file_loop_checked/2, prolog_load_file_loop_checked_0/2, prolog_load_file_nlc/2, prolog_load_file_nlc_0/2, push_predicates/2, read_one_term/2, read_one_term/3, register_module_type/1, register_module_type/2, rsavedb/0, savedb/0, scan_updates/0, show_bool/1, show_interesting_cl/2, show_load_context/0, simplify_why/2, simplify_why_r/4, stream_pos/1, term_expand_local_each/5, transform_opers/3, transform_opers_0/2, transform_opers_1/2, use_file_type_loader/2, use_was_isa/3, was_exported_content/3, with_mpred_expansions/1, with_no_mpred_expansions/1, with_source_module/2, xfile_module_term_expansion_pass_3/7,  (~)/1, baseKB:cl_assert/2, baseKB:cwc/0, baseKB:mpred_provide_clauses/3, always_expand_on_thread/1, t_l:current_lang/1, current_op_alias/2, defaultAssertMt/1, disable_mpred_term_expansions_globally/0, baseKB:loaded_file_world_time/3, mpred_directive_value/3, baseKB:mpred_skipped_module/1, 
%   never_reload_file/1, prolog_load_file_loop_checked/2, registered_module_type/2).
:- module_transparent 
            mpred_ops/0,setup_module_ops/1.

:- (thread_local t_l:into_form_code/0, t_l:mpred_module_expansion/1).
%:- (volatile t_l:into_form_code/0, t_l:mpred_module_expansion/1).
%:-  /**/ export((convert_side_effect_0a/2, convert_side_effect_0b/2, convert_side_effect_0c/2, guess_if_mpred_file0/1, expand_term_to_load_calls/2, load_file_term_to_command_1/3, load_file_term_to_command_1b/3, mpred_term_expansion_by_pred_class/3, mpred_process_input_1/1, must_expand_term_to_command/2, pl_to_mpred_syntax0/2, process_this_script0/1, prolog_load_file_loop_checked_0/2, prolog_load_file_nlc_0/2, transform_opers_0/2, transform_opers_1/2, xfile_module_term_expansion_pass_3/7)).
%:- dynamic((registered_module_type/2, current_op_alias/2, baseKB:mpred_skipped_module/1, prolog_load_file_loop_checked/2, lmcache:mpred_directive_value/3, defaultAssertMt/1, baseKB:loaded_file_world_time/3, baseKB:never_reload_file/1, always_expand_on_thread/1, t_l:current_lang/1, current_op_alias/2, defaultAssertMt/1, disable_mpred_term_expansions_globally/0, baseKB:loaded_file_world_time/3, mpred_directive_value/3, baseKB:mpred_skipped_module/1, never_reload_file/1, prolog_load_file_loop_checked/2, registered_module_type/2, t_l:disable_mpred_term_expansions_globally/0, user:prolog_load_file/2, user:term_expansion/2)).
%:- dynamic(registered_module_type/2).        


:- multifile((baseKB:registered_mpred_file/1,baseKB:ignore_file_mpreds/1,baseKB:registered_module_type/2)).
:-   dynamic((baseKB:registered_mpred_file/1,baseKB:ignore_file_mpreds/1,
   baseKB:registered_module_type/2)).


:- include('mpred_header.pi').


% TODO uncomment the next line without breaking it all!
% baseKB:use_cyc_database.




%% mpred_loader_module_transparent( ?F) is det.
%
% Managed Predicate Loader Module Transparent.
%
mpred_loader_module_transparent(F/A):-!,mpred_loader_module_transparent(F/A).
mpred_loader_module_transparent(M:F/A):-!, M:module_transparent(M:F/A),dtrace, system:import(M:F/A).
mpred_loader_module_transparent(F/A):-!, module_transparent(F/A).

% :- module_property(mpred_loader, exports(List)),maplist(mpred_loader_module_transparent,List).

:- thread_local(t_l:mpred_already_in_file_expansion/1).




%% mpred_prolog_only_file( ?File) is det.
%
% Managed Predicate Prolog Only File.
%
mpred_prolog_only_file(File):- var(File),!,fail.
mpred_prolog_only_file(File):- baseKB:ignore_file_mpreds(File),!.
mpred_prolog_only_file(File):- lmcache:mpred_directive_value(File,language,pl),!.
mpred_prolog_only_file(File):- file_name_extension(File,_,pfc),!,fail.
mpred_prolog_only_file(File):- lmcache:mpred_directive_value(File,language,pfc),!,fail.
mpred_prolog_only_file(_).

% :- use_module(library(logicmoo_utils)).
%:- use_module(mpred_expansion).
%:- use_module(library(logicmoo/util/logicmoo_util_attvar_reader)).
%:- use_module(library(logicmoo/util/logicmoo_util_varnames)).

% mpred_te(_,_,I,OO):-thread_self(X),X\==main,!,I=OO.
% not actual function



%% mpred_te( +OUT1, +OUT2, +I, +Pos, -IN4, -POS4) is det.
%
% Managed Predicate Expander.
%

:- prolog_load_context(directory,Dir),asserta(baseKB:mpred_loader_dir(Dir)).

mpred_te(Type,_,I,_,_,_):- quietly(dont_term_expansion(Type,I)),!,fail.
mpred_te(_Type,_Module,I,PosI,O,PosI):- expand_kif_string_or_fail(pl_te,I,O),!.
mpred_te(_Type,_Module,I,PosI,O,PosI):- expand_isEach_or_fail(I,O),!.
mpred_te(Type,Module,I,PosI,O,PosO):- 
  \+ current_prolog_flag(mpred_te,false),
   % prolog_load_context(file,S),prolog_load_context(source,S),   
   mpred_file_term_expansion(Type,Module,I,O)->PosO=PosI.

dont_term_expansion(Type,I):- 
   current_prolog_flag(lm_expanders,false);
   var(I);
   I=(_ --> _) ;    
   current_prolog_flag(xref,true);
   (prolog_load_context(directory,Dir), baseKB:mpred_loader_dir(Dir));
   I= '$si$':'$was_imported_kb_content$'(_,_); 
   (Type \== term , Type \= _:term ) ; 
   t_l:disable_px.




%% mpred_file_term_expansion( ?Type, ?LoaderMod, ?I, ?OO) is det.
%
% Managed Predicate Expander Primary Helper.
%
:- meta_predicate mpred_file_term_expansion(+,+,+,-).
% mpred_file_term_expansion(_,_,_,_):- \+ current_predicate(_,_:mpred_loader_file),!,fail.
mpred_file_term_expansion(_,_,I,_):- is_directive_form(I),!,fail.
mpred_file_term_expansion(_,_,I,_):- is_ftVar(I),!,fail.
mpred_file_term_expansion(Type,DefMod,end_of_file,O):- !, Type = term, DefMod = user,             
      do_end_of_file_actions(Type,DefMod,end_of_file,O),!,fail.
% mpred_file_term_expansion(_,_,_,_):- get_lang(pl),!,fail.
% mpred_file_term_expansion(Type,LoaderMod,(I:-B),OO):-B==true,!,mpred_file_term_expansion(Type,LoaderMod,I,OO).
% mpred_file_term_expansion(_Type,_LoaderMod,I,( :- must(ain(I)))):-!.

mpred_file_term_expansion(Type,LoaderMod,I,OO):- !,
   no_loop_check(mpred_file_term_expansion0(Type,LoaderMod,I,OO)).

% Ensure rule macro predicates are being used checked just before assert/query time
mpred_file_term_expansion0(Type,LoaderMod,I,O):- 
  sanity((ground(Type:LoaderMod),nonvar(I),var(O))),
  quietly_must(get_source_ref1(mfl(MF,F,L))),!,
  % \+ mpred_prolog_only_file(F),
  call_u(baseKB:mtCycL(MT1)),
  must((proper_source_mod([LoaderMod,MF,MT1],AM))),
  b_getval('$term',TermWas), TermWas == I,
  call_cleanup(
        w_tl(t_l:current_why_source(mfl(AM,F,L)),
        (( get_original_term_source(Orig), 
           b_setval('$orig_term',Orig),
           b_setval('$term',[]),
           (O= (:- must(mpred_ain(I,(mfl(AM,F,L),ax)))))))),
    b_setval('$term',TermWas)),!, wdmsg(I-->O).


proper_source_mod(List,AM):- member(AM,List),call_u(mtCycL(AM)),!.
proper_source_mod(List,AM):- member(AM,List),call_u(mtCanAssert(AM)),!.

%% mpred_expand_file_module_clause( +File, +Module, +:Term, -:Expanded) is det.
%
% Managed Predicate Expander Now One Cc.
%
%mpred_expand_file_module_clause(_,_,I,O):- var(I),!,quietly_must(I=O).

%mpred_expand_file_module_clause(_,_,(?-(G0)),(?-(G1))):-!,quietly_must(fully_expand_goal(change(assert,ain),G0,G1)).
%mpred_expand_file_module_clause(F,M,I,O):- is_directive_form(I),!,quietly_must(fully_expand(change(assert,load(F,M)),I,O)).
%mpred_expand_file_module_clause(F,M,(H:-B),O):- get_lang(pl),!,quietly_must(fully_expand(change(assert,load(F,M)),(H:-B),O)).
%mpred_expand_file_module_clause(_,_,I,O):- t_l:verify_side_effect_buffer,!,loader_side_effect_verify_only(I,O).
%mpred_expand_file_module_clause(_,_,I,O):- t_l:use_side_effect_buffer,!,loader_side_effect_capture_only(I,O).
mpred_expand_file_module_clause(_,M,I,O):- mpred_expander_now_physically(M,I,O).
  


%% mpred_expander_now_physically( ?M, ?I, ?OO) is det.
%
% Managed Predicate Expander Now Physically.
%
mpred_expander_now_physically(M,I,OO):-  
 '$set_source_module'(Old,M),
 call_cleanup(M:((
   quietly_must((source_context_module(CM),CM\==mpred_pfc,CM\==mpred_loader)),
   quietly_must(loop_check(expand_term_to_load_calls(I,O),trace_or_throw(in_loop(expand_term_to_load_calls(I,O))))),!,
   quietly_must(I\=@=O),
  (((t_l:mpred_term_expansion_ok;mpred_expand_inside_file_anyways)-> true ; 
    ((show_load_context,wdmsg(warning,wanted_mpred_term_expansion(I,O))),fail)),
   ((O=(:-(CALL))) ->  quietly_must((M:call_file_command(I,CALL,OO,O))); 
        (OO = O))))),'$set_source_module'(Old)).
    



%% show_bool( :GoalG) is det.
%
% Show Bool.
%
show_bool(G):- must(forall((G*->wdmsg(true=G);wdmsg(false=G)),true)).




%% show_load_context is det.
%
% Show Load Context.
%
show_load_context:- 
  must((
  listing(baseKB:registered_mpred_file),
  show_bool(mpred_may_expand),
  show_bool(in_mpred_kb_module),
  show_bool(mpred_expand_inside_file_anyways),
  show_bool(t_l:mpred_term_expansion_ok),
  show_bool(loading_source_file(_)),
  show_bool(nb_current('$term',_)),
  show_bool(nb_current('$orig_term',_)),
  show_bool(get_lang(_)))).





%% add_term( ?Term, ?Vs) is det.
%
% Add Term.
%
add_term(end_of_file,_):-!.
add_term(Term,Vs):- 
   put_variable_names( Vs),
    add_from_file(Term).





%% add_from_file( ?Term) is det.
%
% Add Converted From File.
%
add_from_file(Term):-  
  w_tl(t_l:mpred_already_in_file_expansion(Term),quietly_must(ain(Term))).




%% myDebugOnError( :GoalTerm) is det.
%
% My Debug Whenever Error.
%
myDebugOnError(Term):-catch(once(quietly_must((Term))),E,(dmsg(error(E,start_myDebugOnError(Term))),dumpST,dtrace,rtrace((Term)),dmsginfo(stop_myDebugOnError(E=Term)),dtrace,Term)).
         



%% read_one_term( ?Term, ?Vs) is det.
%
% Read One Term.
%
read_one_term(Term,Vs):- catch(once(( read_term(Term,[double_quotes(string),variable_names(Vs)]))),E,(Term=error(E),dmsg(error(E,read_one_term(Term))))).



%% read_one_term( ?Stream, ?Term, ?Vs) is det.
%
% Read One Term.
%
read_one_term(Stream,Term,Vs):- catch(once(( read_term(Stream,Term,[double_quotes(string),variable_names(Vs)]))),E,(Term=error(E),dmsg(error(E,read_one_term(Term))))).

% rescan_mpred_stubs:- doall((mpred_isa(F,prologHybrid),arity(F,A),A>0,warnOnError(declare_mpred_local_dynamic(moo,F,A)))).



:-  /**/ export(etrace/0).



%% etrace is det.
%
% E Trace.
%
etrace:-leash(+all),leash(+exception),dtrace.


:- thread_local(t_l:on_eof/2).
:- export(t_l:on_eof/2).




%% onEndOfFile( ?Call) is det.
%
% Whenever End Of File.
%
onEndOfFile(Call):- which_file(F), asserta(t_l:on_eof(F,Call)).




%% assert_until_eof( ?F) is det.
%
% Assert Until Eof.
%
assert_until_eof(F):- must_det_l((loading_source_file(File),assert_until_eof(File,F))).

assert_until_eof(File,F):-dmsg(assert_until_eof(File,F)),must_det_l((asserta(F,Ref),asserta((t_l:on_eof(File,ignore(erase(Ref))))))).

:- style_check(+singleton).
:- style_check(-discontiguous).
% :- style_check(-atom).

% gload:- ensure_mpred_file_loaded(savedb),!.



%% gload is det.
%
% Gload.
%
gload:- ensure_mpred_file_loaded(logicmoo('rooms/startrek.all.mpred')).

%:-meta_predicate(savedb/0).



%% savedb is det.
%
% Savedb.
%
savedb:-!.
savedb:- on_x_rtrace(rsavedb),!.
%:-meta_predicate(rsavedb/0).



%% rsavedb is det.
%
% Rsavedb.
%
rsavedb:-
 on_x_rtrace(agenda_mpred_repropigate),
 catch((   
   ignore(catch(make_directory('/tmp/lm/'),_,true)),
   ignore(catch(delete_file('/tmp/lm/savedb'),E,(dmsginfo(E:delete_file('/tmp/lm/savedb'))))),   
   tell('/tmp/lm/savedb'),make_db_listing,told),E,dmsginfo(savedb(E))),!.





%% make_db_listing is det.
%
% Make Database Listing.
%
make_db_listing:-
 % defaultAssertMt(DBM),
%   listing(t),
 %  listing(mpred_f),
     listing(_),
     listing(baseKB:_),  
     listing(dbase:_),
     listing(dyn:_),
     listing(moo_loader:_),
     listing(world :_),
     listing(_),!.







%% hdr_debug( ?F, ?A) is det.
%
% Hdr Debug.
%
hdr_debug(_,_):-!.
hdr_debug(F,A):-'format'(F,A).
:- meta_predicate module_typed_term_expand(?,?).





%% module_typed_term_expand( ?X, ?UPARAM2) is det.
%
% Module Typed Term Expand.
%
module_typed_term_expand(X,_):-not(compound(X)),!,fail.
module_typed_term_expand( ((':-'(_))) , _ ):-!,fail.
module_typed_term_expand(_:B1,B2):-!,module_typed_term_expand(B1,B2),!.
module_typed_term_expand(X,CvtO):- compound(X),loading_module(CM),functor_catch(X,F,A),module_typed_term_expand(CM,X,F,A,CvtO).




%% module_typed_term_expand( ?CM, ?X, ?F, ?A, ?CvtO) is det.
%
% Module Typed Term Expand.
%
module_typed_term_expand(CM,X,F,A,CvtO):-findall(CvtO,term_expand_local_each(CM,X,F,A,CvtO),Ys), Ys == [],!,fail.  




%% term_expand_local_each( ?VALUE1, ?VALUE2, ?F, ?A, ?VALUE5) is det.
%
% Term Expand Local Each.
%
term_expand_local_each(_,_,F,A,_):- member(F / A,[never_expand]),!,fail.
term_expand_local_each(CM,X,F,A,X):-baseKB:registered_module_type(CM,utility),export(F/A).
term_expand_local_each(CM,X,F,A,X):-baseKB:registered_module_type(CM,dynamic),dynamic(F/A).





% ========================================
% include_mpred_file(MASK)
% ========================================




%% include_mpred_files( ?Mask) is det.
%
% Include Managed Predicate Files.
%
include_mpred_files(Mask):- 
     forall(maybe_locate_file(Mask,E),ensure_mpred_file_loaded(E)).

:- module_transparent(include_prolog_files/1).
include_prolog_files(Mask):- 
     forall(maybe_locate_file(Mask,E),ensure_loaded(E)).
/*
module(M,Preds):-
    'format'(user_output /*e*/,'% visting module ~w.~n',[M]),
    forall(member(P,Preds),export(P)).
*/



%% scan_updates is det.
%
% Scan Updates.
%
scan_updates:-thread_property(X,alias(loading_code)),thread_property(X,status(running)),!.
scan_updates:-!.
scan_updates:-ignore(catch(make,_,true)).

/*
do_term_expansions:- source_context_module(CM), (do_term_expansions(CM)).

do_term_expansions(_):- thread_self(ID),baseKB:always_expand_on_thread(ID),!.
%do_term_expansions(_):- always_transform_heads,not(prevent_transform_mpreds),!.
do_term_expansions(_):- is_compiling_clause.
do_term_expansions(CM):- baseKB:registered_mpred_file(CM),!, not(ended_transform_mpreds), not(prevent_transform_mpreds).

check_term_expansions:- not(do_term_expansions).
*/

% :- (do_term_expansions(_)->true;throw(not_term_expansions)).


:- op(1120,fx,export),op(1120,fx,export).

:-  /**/ export(((current_context_module/1,
    module_typed_term_expand/2,
         register_module_type/1,          
         end_module_type/1))).










% :- system:use_module(library(base32)).

% :-autoload.

% https://docs.google.com/document/u/0/export?format=txt&id=1yyGne4g8vXKxNPKIKVLOtt0OxIM2kxyfmvjqR1lgbcY
% http_get
:- asserta_if_new(t_l:infForward).

:- dynamic(baseKB:mpred_skipped_module/1).



%% mpred_skipped_module( ?VALUE1) is det.
%
% Hook To [baseKB:mpred_skipped_module/1] For Module Mpred_loader.
% Managed Predicate Skipped Module.
%
% :-show_call(why,loading_module(X)),retractall(X).

%:-listing(baseKB:mpred_skipped_module/1).


:- thread_local(t_l:into_form_code).
:- thread_local(t_l:into_form_code).
:- thread_local(t_l:disable_px /0).
% :- dynamic(baseKB:disable_mpred_term_expansions_globally/0).

%fwc:-true.
%bwc:-true.

%is_fc_body(P):- cnotrace(fwc==P ; (compound(P),arg(1,P,E),is_fc_body(E))),!.
%is_bc_body(P):- cnotrace(bwc==P ; (compound(P),arg(1,P,E),is_bc_body(E))),!.



%% is_code_body( ?P) is det.
%
% If Is A Code Body.
%
is_code_body(P):- cnotrace(cwc==P ; (compound(P),arg(1,P,E),is_code_body(E))),!.


% :- meta_predicate(with_source_module(:,(*))).



%% get_file_type( ?File, ?Type) is det.
%
% Get File Type.
%
get_file_type(File,Type):-var(File),!,quietly_must(loading_source_file(File)),get_file_type(File,Type).
get_file_type(File,Type):-lmcache:mpred_directive_value(File,language,Type).
get_file_type(File,pfc):-file_name_extension(_,'.pfc.pl',File).
get_file_type(File,Type):-file_name_extension(_,Type,File).




%% is_mpred_file( ?F) is det.
%
% If Is A Managed Predicate File.
%
is_mpred_file(F):- var(F),!,quietly_must(loading_source_file(F)),F\==user,!, is_mpred_file(F),!.
is_mpred_file(F):- baseKB:registered_mpred_file(F),!.
is_mpred_file(F):- baseKB:ignore_file_mpreds(F),!,fail.
is_mpred_file(F):- guess_if_mpred_file0(F),!,guess_if_mpred_file0(F),asserta(baseKB:registered_mpred_file(F)),!.
%is_mpred_file(F):- asserta(baseKB:ignore_file_mpreds(F)),!,fail.

%% guess_if_mpred_file0( ?F) is det.
%
% If Is A Managed Predicate File Primary Helper.
%
guess_if_mpred_file0(F):- file_name_extension(_,pfc,F),!.
guess_if_mpred_file0(F):- atom_concat(_,'.pfc.pl',F),!.
guess_if_mpred_file0(F):- file_name_extension(_,plmoo,F),!.
% guess_if_mpred_file0(F):- filematch(prologmud(**/*),F0),F0=F.
guess_if_mpred_file0(F):- loop_check(get_lang(pfc)),!,loop_check(loading_source_file(F0)),F0=F.
guess_if_mpred_file0(F):- atom(F),exists_file(F), file_name_extension(_,WAS,F),WAS\=pl,WAS\= '',WAS\=chr,!.



%% decache_file_type( ?F) is det.
%
% Decache File Type.
%
decache_file_type(F):-
  retractall(baseKB:registered_mpred_file(F)),
  retractall(baseKB:ignore_file_mpreds(F)).




%% must_compile_special_clause( ?CL) is det.
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

:- thread_local(t_l:mpred_module_expansion/1).




%% mpred_use_module( ?M) is det.
%
% Managed Predicate Use Module.
%
mpred_use_module(M):- \+ atom(M),!.
mpred_use_module(M):- atom(M),quietly_must(atom(M)),retractall(baseKB:mpred_skipped_module(M)),show_call(why,asserta_if_new(t_l:mpred_module_expansion(M))).

% ================================================================================
% DETECT PREDS THAT NEED SPECIAL STORAGE 
% ================================================================================


%% mpred_may_expand is det.
%
% Managed Predicate May Expand.
%
mpred_may_expand:-loading_source_file(_F),get_lang(pfc).
mpred_may_expand:-loading_source_file(_F),get_lang(mpred).
mpred_may_expand:-quietly_must(loading_module(M)),mpred_may_expand_module(M),!,mpred_expand_inside_file_anyways.




%% mpred_may_expand_module( ?M) is det.
%
% Managed Predicate May Expand Module.
%
mpred_may_expand_module(M):-baseKB:mpred_skipped_module(M),!,fail.
mpred_may_expand_module(M):-module_property(M,file(F)),is_mpred_file(F).
mpred_may_expand_module(M):-t_l:mpred_module_expansion(M),!.
mpred_may_expand_module(_):-t_l:mpred_module_expansion(*),!.




%% mpred_expand_inside_file_anyways is det.
%
% Managed Predicate Expand Inside File Anyways.
%
mpred_expand_inside_file_anyways:- loading_source_file(F),!,mpred_expand_inside_file_anyways(F).




%% mpred_expand_inside_file_anyways( ?F) is det.
%
% Managed Predicate Expand Inside File Anyways.
%
mpred_expand_inside_file_anyways(F):- var(F),!,loading_source_file(F),nonvar(F),mpred_expand_inside_file_anyways(F).
mpred_expand_inside_file_anyways(F):- t_l:loading_mpred_file(_,F),!.
mpred_expand_inside_file_anyways(F):- baseKB:registered_mpred_file(F).
mpred_expand_inside_file_anyways(F):- is_mpred_file(F),quietly_must(loading_module(M);source_module(M)), (M=user; \+ baseKB:mpred_skipped_module(M)),!.




%% was_exported_content( ?I, ?CALL, ?Output) is det.
%
% Was Exported Content.
%
was_exported_content(I,CALL,'$si$':'$was_imported_kb_content$'(I,CALL)).

:- thread_local(t_l:mpred_term_expansion_ok/0).
:- thread_local(t_l:mpred_already_inside_file_expansion/1).

:- assert_if_new(t_l:mpred_term_expansion_ok).





%% mpred_provide_clauses( ?H, ?B, ?What) is det.
%
% Hook To [baseKB:mpred_provide_clauses/3] For Module Mpred_loader.
% Managed Predicate Provide Clauses.
%
baseKB:mpred_provide_clauses(_H,_B,_What):- fail.




%% show_interesting_cl( ?Dir, ?VALUE2) is det.
%
% Show Interesting Clause.
%
show_interesting_cl(_Dir,_).
show_interesting_cl(Dir,P):- loading_source_file(File),get_file_type(File,Type),
  ((nonvar(Dir),functor(Dir,Type,_))->true;dmsg(Type:cl_assert(Dir,P))).

:- meta_predicate(cl_assert(?,?)).



%% cl_assert( ?Dir, ?P) is det.
%
% Clause Assert.
%
cl_assert(kif(Dir),P):- show_if_debug(must_det_l(( show_interesting_cl(kif(Dir),P),kif_process(P)))),!.
cl_assert(Dir,P):- show_interesting_cl(Dir,P),ain(P),!.
cl_assert(pl,P):-  !, show_if_debug(must_det_l((source_location(F,_L), '$compile_aux_clauses'(P,F)))).
cl_assert(_Code,P):- !, show_if_debug(ain(P)).

:- meta_predicate(call_file_command(?,?,?,?)).
%call_file_command(_,cl_assert(pl,OO),OO,_):-!,show_interesting_cl(pl,OO).


get_original_term_source(Orig):- nb_current('$orig_term',Orig),!. 
get_original_term_source(Orig):- nb_current('$term',Orig),Orig\==[],!. 
get_original_term_source(true).

make_file_command(IN,(:- CALL),OUT):- nonvar(CALL),!, must(make_file_command(IN,CALL,OUT)).

make_file_command(_IN,cl_assert(pfc(WHY),PFC),(NEWSOURCE:-true)):- 
  current_why(CY),
  CMD = mpred_ain(PFC,(CY,ax)),
  get_original_term_source(Orig),
  was_exported_content(Orig,WHY,NEWSOURCE),!,
  show_call(quietly_must((CMD))).


make_file_command(_IN,cl_assert(pfc(WHY),PFC),[(:- CMD), NEWSOURCE]):- 
  current_why(CY),
  CMD = ain(PFC,CY),
  get_original_term_source(Orig),
  was_exported_content(Orig,WHY,NEWSOURCE),!.
  

make_file_command(IN,cl_assert(WHY,NEWISH),OUT):- get_lang(kif),is_kif_clause(NEWISH),!,must(make_file_command(IN,cl_assert(kif(WHY),NEWISH),OUT)).
make_file_command(_IN,cl_assert(WHY,CMD2),SET):- 
  get_original_term_source(Orig),
  was_exported_content(Orig,WHY,NEWSOURCE),list_to_set([(:- cl_assert(WHY,CMD2)), NEWSOURCE],SET).
 
make_file_command(IN,cl_assert(WHY,CMD2),[CMD2, (:- cl_assert(WHY,CMD2)), NEWSOURCE ]):- was_exported_content(WHY,IN,NEWSOURCE),!.

make_file_command(_IN,'$si$':'$was_imported_kb_content$'(IN2,WHY),'$si$':'$was_imported_kb_content$'(IN2,WHY)).


%% call_file_command( ?I, ?CALL, ?OO, ?O) is det.
%
% Call File Command.
%
call_file_command(I,CALL,OO,O):- call_file_command0(I,CALL,OO,O),wdmsg(call_file_command(I,CALL,OO,O)).

call_file_command0(I,cl_assert(OTHER,OO),OO,I):- get_lang(kif),is_kif_clause(OO),!,call_file_command(I,cl_assert(kif(OTHER),OO),OO,I).
call_file_command0(I,CALL,[(:- quietly_must(CALL2)),(:- quietly_must(CALL)),OO],(:-CALL2)):- CALL2\=@=CALL, 
  was_exported_content(I,CALL,OO),!.
call_file_command0(I,CALL,[(:- quietly_must(CALL)),OO],(:-CALL)):- was_exported_content(I,CALL,OO),!.
% call_file_command(I,CALL,OO,O):- (current_predicate(_,CALL) -> ((quietly_must(call(CALL)),was_exported_content(I,CALL,OO))); OO=[O,:-CALL]).






% ensure we only process onEndOfFile directive at the end of the actual source files



%% do_end_of_file_actions( ?VALUE1, ?VALUE2, ?VALUE3, ?VALUE4) is det.
%
% Do End Of File Actions.
%
do_end_of_file_actions(_,_,_,_):- in_include_file,!.
do_end_of_file_actions(term,_,_,_):- quietly_must(loading_source_file(_F)),do_end_of_file_actions_real.




%% do_end_of_file_actions_real is det.
%
% Do End Of File Actions Real.
%
do_end_of_file_actions_real:- which_file(F),
   GETTER=t_l:on_eof(F,TODO),
       forall(GETTER,((doall(show_failure(why,TODO))),ignore(retract(GETTER)))).





%% mpred_implode_varnames( :TermN) is det.
%
% Managed Predicate Implode Varnames.
%
mpred_implode_varnames([]):-!.
mpred_implode_varnames([N=V|Vs]):-V='$VAR'(N),mpred_implode_varnames(Vs),!.

% mudKeyword("happy","happy") -> mudKeyword(vHappy,"happy").

% quietly_must skip already loaded modules (we remember these so make/0 doesnt dbreak)



%% mpred_maybe_skip( ?M) is det.
%
% Managed Predicate Maybe Skip.
%
mpred_maybe_skip(M):- t_l:mpred_module_expansion(N),N==M,!.
mpred_maybe_skip(M):- asserta_if_new(baseKB:mpred_skipped_module(M)),!.
% :- forall(current_module(M),mpred_maybe_skip(M)).


:- dynamic(lmcache:mpred_directive_value/3).





%% expanded_already_functor( :TermARG1) is det.
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




%% setup_module_ops is det.
%
% Managed Predicate Oper.s.
%
mpred_ops:-  prolog_load_context(module,M),setup_module_ops(M).

:- export(mpred_op_unless/4).

setup_module_ops(M):- mpred_op_each(mpred_op_unless(M)).

mpred_op_unless(M,A,B,C):- op_safe(A,B,M:C).

mpred_op_each(OpEach):-
            call(OpEach,1199,fx,('==>')), % assert
            call(OpEach,1199,fx,('?->')), % ask
            call(OpEach,1190,xfy,('::::')), % Name something
            call(OpEach,1180,xfx,('==>')), % Forward chaining
            call(OpEach,1170,xfx,('<==>')), % Forward and backward chaining
            call(OpEach,1160,xfx,('<==')), % backward chain PFC sytle
            call(OpEach,1160,xfx,('<-')), % backward chain PTTP sytle (currely really PFC)
            call(OpEach,1160,xfx,('<=')), % backward chain DRA sytle
            call(OpEach,1150,xfx,('=>')), % Logical implication
            call(OpEach,1130,xfx,('<=>')), % Logical bi-implication
            call(OpEach,600,yfx,('&')), 
            call(OpEach,600,yfx,('v')),
            call(OpEach,400,fx,('~')),
            % call(OpEach,300,fx,('-')),
            call(OpEach,350,xfx,('xor')).





%% pfc_dcg is det.
%
% Managed Predicate Dcg Oper.s.
%
pfc_dcg:- file_begin(pfc), op(400,yfx,('\\\\')),op(1200,xfx,('-->>')),op(1200,xfx,('--*>>')), op(1200,xfx,('<<--')).

:- thread_local(mpred_ain_loaded).






% ========================================
% begin/end_transform_mpreds
% ========================================
:- dynamic(t_l:current_lang/1).


:- dynamic(always_expand_on_thread/1).
:- thread_local is_compiling_clause/0.

%% is_compiling is det.
%
% If Is A Compiling.
%
is_compiling:-is_compiling_clause;compiling.

:- style_check(+discontiguous).
:- style_check(-discontiguous).


unload_this_file(File):- 
   ignore((
   source_file(M:P,File),
   copy_term(P,PP),
   clause(M:P,_,Ref),
   clause_property(Ref,file(File)),
   erase(Ref),
   \+ clause(M:PP,_,_),
   abolish(M:PP),fail)),
   unload_file(File).


:- export(clause_count/2).
:- module_transparent(clause_count/2).

clause_count(Mask,N):- 
     flag(clause_count,_,0),
      ignore((current_module(M),clause(M:Mask,_,Ref),
         (clause_property(Ref,module(MW))->must(M==MW);true),
         flag(clause_count,X,X+1),fail)),flag(clause_count,N,0).

check_clause_counts:- notrace((forall(checked_clause_count(Mask),check_clause_count(Mask)))),fail.
check_clause_counts.

:- dynamic(checked_clause_count/2).

checked_clause_count(isa(_,_)).
checked_clause_count(~(_)).
checked_clause_count(t(_,_)).
checked_clause_count(t(_,_,_)).
checked_clause_count(arity(_,_)).
checked_clause_count(argIsa(_,_,_)).
checked_clause_count(argQuotedIsa(_,_,_)).
checked_clause_count(tCol(_)).
checked_clause_count(resultIsa(_,_)).
checked_clause_count(genls(_,_)).
checked_clause_count((_ <- _)).
checked_clause_count((_ ==> _)).
checked_clause_count((_ <==> _)).
checked_clause_count(spft(_,_,ax)).
checked_clause_count(agent_command(_,_)).


:- dynamic(lmcache:last_clause_count/2).

% check_clause_count(_):-current_prolog_flag(unsafe_speedups,true),!.
check_clause_count(Mask):- swc,
 clause_count(Mask,N),
    (retract(lmcache:last_clause_count(Mask,Was)) -> true ; Was=0),
     (assert(lmcache:last_clause_count(Mask,N)),
     Diff is N - Was ), 
     (Diff ==0 -> true;
     (Diff == -1 -> true;
     (Diff<0 -> trace_or_throw(bad_count(Mask,(Was --> N))) ; dmsg(good_count(Mask,(Was --> N)))))).

%% begin_pfc is det.
%
% Begin Prolog Forward Chaining.
%
begin_pfc:-file_begin(pfc).



%% mpred_begin is det.
%
% Managed Predicate Begin.
%
mpred_begin:-file_begin(pfc).



%% dyn_begin is det.
%
% Dyn Begin.
%
dyn_begin:-file_begin(dyn).



%% dyn_end is det.
%
% Dyn End.
%
dyn_end:-file_end(dyn).

        


%% enable_mpred_expansion is det.
%
% Enable Managed Predicate Expansion.
%
enable_mpred_expansion:- set_prolog_flag(lm_expanders,true), set_prolog_flag(mpred_te,true),
     (( \+ t_l:disable_px) -> true ;
                 (retractall(t_l:disable_px),
                 onEndOfFile(asserta_if_new(t_l:disable_px)))).




%% disable_mpred_expansion is det.
%
% Disable Managed Predicate Expansion.
%
disable_mpred_expansion:- 
            set_prolog_flag(mpred_te,false),
             (( t_l:disable_px) -> true ;
                 (asserta_if_new(t_l:disable_px),
                 onEndOfFile(retractall(t_l:disable_px)))).



predicate_is_undefined_fa(F,A):-
  \+ current_predicate(_:F/A),
  functor(P,F,A),
  \+ predicate_property(_:P,exported),
  \+ predicate_property(_:P,static).


:-multifile(baseKB:locked_baseKB/0).
:-dynamic(baseKB:locked_baseKB/0).

simplify_language_name(W,W2):-var(W),!,W2=W.
simplify_language_name(mpred,pfc).
simplify_language_name(plmoo,pfc).
simplify_language_name(prolog,pl).
simplify_language_name(code,pl).
simplify_language_name(W,W).

%% file_begin( ?W) is det.
%
% File Begin.
%
file_begin(WIn):- 
 must_det_l((   
   simplify_language_name(WIn,W),
   set_lang(W),
   set_file_lang(W),   
   fileAssertMt(Mt),
   set_fileAssertMt(Mt),
   wdmsg(fileAssertMt(Mt)),
   op_lang(W),
   set_prolog_flag(retry_undefined,true),
   enable_mpred_expansion)),!,
   must(get_lang(W)).



set_file_lang(W):-
   %assert_until_eof(t_l:current_lang(W)),
   forall((prolog_load_context(file,Source);which_file(Source);prolog_load_context(source,Source)),
   ignore((  % \+ lmcache:mpred_directive_value(Source,language,W),
   decache_file_type(Source),

   wdmsg(lmcache:mpred_directive_value(Source,language,W)),
   (Source = '/root/lib/swipl/pack/logicmoo_base/prolog/logicmoo/pfc/system_common.pfc.pl'-> must(W=pfc);true),
   assert_until_eof(Source,lmcache:mpred_directive_value(Source,language,W))))),
   must(get_lang(W)).


set_lang(W):-
    assert_until_eof(t_l:current_lang(W)).
    

%% file_end( ?W) is det.
%
% File End.
%
file_end(WIn):- 
 must_det((
  simplify_language_name(WIn,W),
  loading_source_file(ISource),decache_file_type(ISource),
  ignore(show_failure(retract(lmcache:mpred_directive_value(ISource,language,W)))))),!.


%% get_lang( ?LANG) is det.
%
% Get Language.
% Inside File.
%
get_lang(LANG):- ((get_lang0(LANGVAR)->same_language(LANG,LANGVAR))).

same_language(LANG,LANGVAR):- 
    simplify_language_name(LANGVAR,LANGVARS),
    simplify_language_name(LANG,LANGS),!,
    LANGS=LANGVARS.

:-thread_local( t_l:current_lang/1).

get_lang0(W) :- t_l:current_lang(W),!.
get_lang0(W) :- prolog_load_context(file,Source)->lmcache:mpred_directive_value(Source,language,W).
get_lang0(W) :- prolog_load_context(source,Source)->lmcache:mpred_directive_value(Source,language,W).
get_lang0(W) :- loading_source_file(Source)->lmcache:mpred_directive_value(Source,language,W).
get_lang0(pfc):- loading_source_file(F)->is_mpred_file(F),!.
get_lang0(pl).





:- meta_predicate(expand_term_to_load_calls(?,?)).
:- meta_predicate(mpred_term_expansion(?,?)).

% Specific "*SYNTAX*" based default

% :- ensure_loaded(logicmoo(snark/common_logic_sexpr)).




%% op_alias( ?OP, ?OTHER) is det.
%
% Oper. Alias.
%
op_alias(OP,OTHER):-retractall(current_op_alias(OP,_)),asserta(current_op_alias(OP,OTHER)).



%% op_lang( ?LANG) is det.
%
% Oper. Language.
%
op_lang(_LANG):- !.




%% get_op_alias( ?OP, ?ALIAS) is det.
%
% Get Oper. Alias.
%
get_op_alias(OP,ALIAS):-current_op_alias(OP,ALIAS).
get_op_alias(OP,ALIAS):-get_lang(LANG),lang_op_alias(LANG,OP,ALIAS).

% current_op_alias((<==>),dup(impliesF,(','))).
% current_op_alias((=>),==>).
% current_op_alias((not),(~)).



%% current_op_alias( ?VALUE1, ?VALUE2) is det.
%
% Current Oper. Alias.
%
:- dynamic(current_op_alias/2).
current_op_alias( not(:-),~(:-)).
current_op_alias( (:-),(:-)).



%% lang_op_alias( ?VALUE1, ?VALUE2, ?VALUE3) is det.
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
% prolog - pl
lang_op_alias(pl,(<==>),(<==>)).
lang_op_alias(pl,(==>),==>).
lang_op_alias(pl, not(':-'),~('<-')).
lang_op_alias(pl,(:-),(:-)).
lang_op_alias(pl,(<=),(<=)).
lang_op_alias(pl,(<-),(<-)).




%% transform_opers( ?LANG, ?PFCM, ?PFCO) is det.
%
% Transform Opers.
%
transform_opers(LANG,PFCM,PFCO):- hotrace((w_tl(t_l:current_lang(LANG),((transitive_lc(transform_opers_0,PFCM,PFC),!, subst(PFC,(not),(~),PFCO)))))).

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




%% transform_opers_0( ?AIS, ?AIS) is det.
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
transform_opers_0(<=(A,B),<=(AA,BB)):- !, must_maplist(transform_opers_0,[A,B],[AA,BB]).
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




%% transform_opers_1( ?AB, ?BBAA) is det.
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



%% to_prolog_xform(+Clause,-Command) is det.
%
% Convert an input clause to a call that will have assumed it is loaded
%
to_prolog_xform(O,OO):-
    ( is_directive_form(O) -> (OO = O); OO=  (:- cl_assert(pfc(to_prolog_xform),O))),!.



%% is_directive_form( :TermV) is det.
%
% If Is A Prolog Xform.
%
is_directive_form((:-(V))):-!,nonvar(V).
is_directive_form((?-(V))):-!,nonvar(V).
is_directive_form(List):-is_list(List),!,member(E,List),is_directive_form(E).
%is_directive_form((:-(V,_))):-!,nonvar(V).
%is_directive_form(_:(:-(V,_))):-!,nonvar(V).





%% expand_in_mpred_kb_module( ?I, ?O) is det.
%
% Expand In Managed Predicate Knowledge Base Module.
%
expand_in_mpred_kb_module(I,O):- is_directive_form(I),quietly_must(I=O),!.
expand_in_mpred_kb_module(I,OO):- quietly_must(expand_term_to_load_calls(I,O)),!,quietly_must(to_prolog_xform(O,OO)).


%% expand_term_to_load_calls( ?I, ?OO) is det.
%
% Load File Term Converted To Command 0c.
%
expand_term_to_load_calls(I,OO):- convert_if_kif_string(I,O),!,
   quietly_must(expand_term_to_load_calls(O,OO)).

expand_term_to_load_calls(PI,OO):- PI=..[P,I], convert_if_kif_string(I,O),!,
   quietly_must((PO=..[P,O], expand_term_to_load_calls(PO,OO))).

expand_term_to_load_calls((H:-B),O):- B==true,!,quietly_must(expand_term_to_load_calls(H,O)).

expand_term_to_load_calls(HB,O):- strip_module(HB,M,(H:-B)),B==true,(H:-B)\=@=HB,!,quietly_must(expand_term_to_load_calls(M:H,O)).

expand_term_to_load_calls(C,O):- fail,  hotrace((get_lang(LANG),show_success((quietly_must(transform_opers(LANG,C,M)),C\=@=M)))),!,
   quietly_must(expand_term_to_load_calls(M,O)).

expand_term_to_load_calls(C,O):- fail,show_success(hotrace((compound(C), get_op_alias(OP,ALIAS),
  atom(OP),atom(ALIAS),C=..[OP|ARGS]))),CC=..[ALIAS|ARGS],quietly_must(loop_check(must_expand_term_to_command(CC,O))),!.

expand_term_to_load_calls(C,O):- must_expand_term_to_command(C,O)->quietly_must(is_directive_form(O)).
expand_term_to_load_calls(O,(:-compile_clause(O))):- get_lang(pl),!.


%% must_expand_term_to_command( ?M, ?O) is det.
%
% Must Be Successfull Managed Predicate term expansion  Extended Helper.
%
must_expand_term_to_command(C,O):- mpred_term_expansion(C,O),C\=@=O,quietly_must(is_directive_form(O)),!.
must_expand_term_to_command(O,(:-compile_clause(O))):- get_lang(pl),!.

%% mpred_term_expansion( ?Fact, ?Output) is det.
%
% Managed Predicate Term Expansion.
%

mpred_term_expansion(((P==>Q)),(:- cl_assert(pfc(fwc),(P==>Q)))).
mpred_term_expansion((('=>'(Q))),(:- cl_assert(pfc(fwc),('=>'(Q))))).
mpred_term_expansion((('==>'(Q))),(:- cl_assert(pfc(fwc),('=>'(Q))))).
mpred_term_expansion(((nesc(Q))),(:- cl_assert(pfc(fwc),nesc(Q)))).
mpred_term_expansion(~(Q),(:- cl_assert(pfc(fwc),~(Q)))).
mpred_term_expansion(('<-'(P,Q)),(:- cl_assert(pfc(bwc),('<-'(P,Q))))).
mpred_term_expansion(('<==>'(P,Q)),(:- cl_assert(pfc(bwc),(P<==>Q)))).
mpred_term_expansion((<=(Q,P)),(:- cl_assert(pfc(bwc),(Q<-P)))).



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
mpred_term_expansion(Fact,Output):- load_file_term_to_command_1(_Dir,Fact,C),quietly_must(mpred_term_expansion(C,Output)),!.


%% load_file_term_to_command_1( ?Type, :TermIn, :TermOut) is det.
%
% load file term Converted To command  Secondary Helper.
%
      load_file_term_to_command_1(pfc(act),(H:-(Chain,B)),(PFC==>PH)):-cwc, is_action_body(Chain),pl_to_mpred_syntax((Chain,B),PFC),pl_to_mpred_syntax_h(H,PH).
      load_file_term_to_command_1(pfc(awc),(H:-(Chain,B)),(PH==>PFC)):-cwc, has_body_atom(twc,Chain),pl_to_mpred_syntax((Chain,B),PFC),pl_to_mpred_syntax_h(H,PH).
      load_file_term_to_command_1(pfc(fwc),(H:-(Chain,B)),(PFC==>PH)):-cwc, is_fc_body(Chain),pl_to_mpred_syntax((Chain,B),PFC),pl_to_mpred_syntax_h(H,PH),can_be_dynamic(PH),make_dynamic(PH).
      load_file_term_to_command_1(pfc(bwc),(H:-(Chain,B)),(PH<-PFC)):-cwc, is_bc_body(Chain),pl_to_mpred_syntax((Chain,B),PFC),pl_to_mpred_syntax_h(H,PH),can_be_dynamic(PH),make_dynamic(PH).


mpred_term_expansion(Fact,Output):- load_file_term_to_command_1b(_Dir,Fact,C),!,quietly_must(mpred_term_expansion(C,Output)),!.

%% load_file_term_to_command_1b( ?VALUE1, :TermH, :TermH) is det.
%
% Load File Term Converted To Command 1b.
%
      load_file_term_to_command_1b(pfc(act),(H:-Chain,B),(H==>{(Chain,B)})):-cwc, is_action_body(Chain),make_dynamic(H).
      load_file_term_to_command_1b(pfc(fwc),(H:-Chain,B),((Chain,B)==>H)):-cwc, is_fc_body(Chain),make_dynamic(H).
      load_file_term_to_command_1b(pfc(bwc),(H:-Chain,B),(H<-(Chain,B))):-cwc, is_bc_body(Chain),make_dynamic(H).


% mpred_term_expansion((H:-Chain,B),(H:-(B))):- atom(Chain),is_code_body(Chain),!,quietly_must(atom(Chain)),make_dynamic(H).



  
mpred_term_expansion_by_storage_type(_M,'$si$':'$was_imported_kb_content$'(_,_),pl):-!.
mpred_term_expansion_by_storage_type(M,( \+ C ),HOW):- nonvar(C), !,mpred_term_expansion_by_storage_type(M,C,HOW).
mpred_term_expansion_by_storage_type(_M,C,compile_clause(static)):- is_static_predicate(C).
mpred_term_expansion_by_storage_type(_M,C,requires_storage(WHY)):- requires_storage(C,WHY),!.
mpred_term_expansion_by_storage_type(_M,C,must_compile_special):- must_compile_special_clause(C),t_l:mpred_already_inside_file_expansion(C).


mpred_term_expansion(Fact,Fact):- get_functor(Fact,F,_A),(a(prologDynamic,F)),!.
mpred_term_expansion(Fact,(:- ((cl_assert(Dir,Fact))))):- mpred_term_expansion_by_pred_class(Dir,Fact,_Output),!.

mpred_term_expansion(MC,(:- cl_assert(ct(How),MC))):- fail, strip_module(MC,M,C),hotrace(mpred_rule_hb(C,H,_B)),
  (mpred_term_expansion_by_storage_type(M,H,How)->true;(C \= (_:-_),mpred_term_expansion_by_storage_type(M,C,How))),!.


mpred_term_expansion((Fact:- BODY),(:- ((cl_assert(Dir,Fact:- BODY))))):- nonvar(Fact),
   mpred_term_expansion_by_pred_class(Dir,Fact,_Output),!.

mpred_term_expansion((M:Fact:- BODY),(:- ((cl_assert(Dir,M:Fact:- BODY))))):- nonvar(Fact),
   mpred_term_expansion_by_pred_class(Dir,Fact,_Output),!.

%% mpred_term_expansion_by_pred_class( ?VALUE1, ?Fact, ?Output) is det.
%
% load file term Converted To command  Extended Helper.
% Specific to the "*PREDICATE CLASS*" based default
%
      mpred_term_expansion_by_pred_class(_,Fact,Output):- get_functor(Fact,F,_A),lookup_u(prologOnly(F)),Output='$si$':'$was_imported_kb_content$'(Fact,pfcControlled(F)),!,fail.
      mpred_term_expansion_by_pred_class(pfc(pred_type),Fact,Output):- get_functor(Fact,F,_A),lookup_u(ttPredType(F)),Output='$si$':'$was_imported_kb_content$'(Fact,ttPredType(F)),!.
      mpred_term_expansion_by_pred_class(pfc(func_decl),Fact,Output):- get_functor(Fact,F,_A),lookup_u(functorDeclares(F)),Output='$si$':'$was_imported_kb_content$'(Fact,functorDeclares(F)),!.
      mpred_term_expansion_by_pred_class(pfc(macro_head),Fact,Output):- get_functor(Fact,F,_A),lookup_u(prologMacroHead(F)),Output='$si$':'$was_imported_kb_content$'(Fact,prologMacroHead(F)),!.
      mpred_term_expansion_by_pred_class(pfc(mpred_ctrl),Fact,Output):- get_functor(Fact,F,_A),lookup_u(pfcControlled(F)),Output='$si$':'$was_imported_kb_content$'(Fact,pfcControlled(F)),!.
      mpred_term_expansion_by_pred_class(pfc(hybrid),Fact,Output):- get_functor(Fact,F,_A),lookup_u(prologHybrid(F)),Output='$si$':'$was_imported_kb_content$'(Fact,pfcControlled(F)),!.
      mpred_term_expansion_by_pred_class(pfc(pl),Fact,Output):- get_functor(Fact,F,_A),(a(prologDynamic,F)),Output='$si$':'$was_imported_kb_content$'(Fact,pfcControlled(F)),!.
      % mpred_term_expansion_by_pred_class(pfc(in_mpred_kb_module),Fact,Output):- in_mpred_kb_module,Output=Fact,!.


% Specific "*FILE*" based default
mpred_term_expansion(Fact,(:- ((cl_assert(dyn(get_lang(dyn)),Fact))))):- get_lang(dyn),!.
mpred_term_expansion(Fact,(:- ((cl_assert(kif(get_lang(kif)),Fact))))):- get_lang(kif),!.
%mpred_term_expansion(Fact,(:- ((cl_assert(pfc(in_mpred_kb_module),Fact))))):- in_mpred_kb_module,!.
%mpred_term_expansion(Fact,(:- ((cl_assert(pfc(get_lang(pl)),Fact))))):- get_lang(pl),!.
mpred_term_expansion(Fact,Fact):- get_lang(pl),!.
%mpred_term_expansion(Fact,(:- ((cl_assert(pfc(get_lang(pfc)),Fact))))):- get_lang(pfc),!.

/*
mpred_term_expansion(Fact,(:- ((cl_assert(pfc(expand_file),Fact))))):-
    cnotrace(mpred_expand_inside_file_anyways(F)),!,_Output='$si$':'$was_imported_kb_content$'(Fact,mpred_expand_inside_file_anyways(F)),!.
*/




%% can_be_dynamic( ?H) is det.
%
% Can Be Dynamic.
%
can_be_dynamic(H):- predicate_property(H,dynamic),!.
can_be_dynamic( \+ H):- nonvar(H), predicate_property(H,dynamic),!.
can_be_dynamic(H):- \+ is_static_pred(H), \+ predicate_property(H,static),  \+ predicate_property(H,meta_predicate(_)).




%% pl_to_mpred_syntax_h( ?A, ?PFC_A) is det.
%
% Pl Converted To Managed Predicate Syntax Head.
%
pl_to_mpred_syntax_h(A,PFC_A):- quietly_must(pl_to_mpred_syntax0(A,PFC_A)),!, PFC_A \= '{}'(_).



%% pl_to_mpred_syntax( ?A, ?PFC_A) is det.
%
% Pl Converted To Managed Predicate Syntax.
%
pl_to_mpred_syntax(A,PFC_A):- quietly_must(pl_to_mpred_syntax0(A,PFC_A)),!.




%% pl_to_mpred_syntax0( ?A, ?A) is det.
%
% Pl Converted To Managed Predicate Syntax Primary Helper.
%
pl_to_mpred_syntax0(A,A):-is_ftVar(A),!.
pl_to_mpred_syntax0((A,B),PFC):-!,pl_to_mpred_syntax(A,PFC_A),pl_to_mpred_syntax(B,PFC_B),conjoin_body(PFC_A,PFC_B,PFC).
pl_to_mpred_syntax0(pfc(A),A):-!.
pl_to_mpred_syntax0(A,{A}):-!.



%% stream_pos( :TermFile) is det.
%
% Stream Pos.
%
stream_pos(File:LineNo):-loading_source_file(File),current_input(S),stream_property(S, position(Position)), !,stream_position_data(line_count, Position, LineNo),!.




%% compile_clause( ?CL) is det.
%
% Compile Clause.
%
compile_clause(CL):- quietly_must((make_dynamic(CL),assertz_if_new(CL),!,clause_asserted(CL))).




%% make_dynamic( ?C) is det.
%
% Make Dynamic.
%
make_dynamic((H:-_)):- sanity(nonvar(H)),!,must(make_dynamic(H)).
make_dynamic(M:(H:-_)):- sanity(nonvar(H)),!,must(make_dynamic(M:H)).
make_dynamic(C):- loop_check(make_dynamic_ilc(C),true).

make_dynamic_ilc(C):- trace_or_throw(make_dynamic_ilc(C)),
   compound(C),strip_module(C,MIn,_),get_functor(C,F,A),quietly_must(F\=='$VAR'),
  (\+ a(mtCycL,MIn) -> must(defaultAssertMt(M)) ; MIn =M),
  functor(P,F,A),

  ( \+predicate_property(M:P,_) -> kb_dynamic(M:F/A) ; 
    (predicate_property(M:P,dynamic)->true;dynamic_safe(M:P))),!,
  kb_dynamic(M:F/A),
  quietly_must((predicate_property(M:P,dynamic))).

% once(baseKB:mpred_is_impl_file(F);asserta(baseKB:mpred_is_impl_file(F))).

%user:goal_expansion(G,OUT):- \+  t_l:disable_px, G\=isa(_,_),(use_was_isa(G,I,C)),!,to_isa_out(I,C,OUT).
%user:term_expansion(G,OUT):- \+  t_l:disable_px, hotrace(use_was_isa(G,I,C)),!,to_isa_out(I,C,OUT).
%user:term_expansion(I,O):- \+ t_l:disable_px, t_l:consulting_sources, wno_tl(t_l:consulting_sources,ain(I)),O=true.



% :-set_prolog_flag(allow_variable_name_as_functor,true).

% :- source_location(S,_),forall(loading_source_file(H,S),ignore(( \+predicate_property(M:H,built_in), functor(H,F,A),M:module_transparent(F/A),M:export(F/A)))).



%:- system:use_module(library(shlib)).
%:- system:use_module(library(operators)).

:- source_location(F,_),asserta(baseKB:ignore_file_mpreds(F)).

% filetypes 
%
%  pfc - all terms are sent to ain/1 (the the execeptions previously defined)
%  pl - all terms are sent to compile_clause/1 (the the execeptions previously defined)
%  prolog - all terms are sent to compile_clause/1 (even ones defined conflictingly)
%  dyn - all terms are sent to ain/1 (even ones defined conflictingly)

:- thread_local(t_l:pretend_loading_file/1).



%% loading_source_file( ?F) is det.
%
% Loading Source File.
%
loading_source_file(F):-once(t_l:pretend_loading_file(F);prolog_load_context(source,F);loading_file(F);call('$module'(F,F))).


:- dynamic(baseKB:never_reload_file/1).




%% load_language_file( ?Name0) is det.
%
% Load Language File.
%
load_language_file(Name0):- 
 forall(filematch_ext('qlf',Name0,Name),
  ((
   w_tl([set_prolog_flag(lm_expanders,false),
         set_prolog_flag(read_attvars,false),
         (user:term_expansion(_,_):-!,fail),
         (user:term_expansion(_,_,_,_):-!,fail),
         (user:goal_expansion(_,_):-!,fail),
         (user:goal_expansion(_,_,_,_):-!,fail),
         (system:term_expansion(_,_):-!,fail),
         (system:term_expansion(_,_,_,_):-!,fail),
         (system:goal_expansion(_,_,_,_):-!,fail),
         (system:goal_expansion(_,_):-!,fail)],
     gripe_time(1,(baseKB:load_files(Name,[qcompile(auto),register(false),if(not_loaded  )])->asserta(baseKB:never_reload_file(Name));retract(baseKB:never_reload_file(Name)))))))),!.
 


%% disable_mpreds_in_current_file is det.
%
% Disable Managed Predicates In Current File.
%
disable_mpreds_in_current_file:- loading_source_file(F),show_call(why,asserta((t_l:disable_px:-loading_source_file(F),!))).

:- thread_local(tlbugger:no_buggery_tl/0).
:-  /**/ export(with_no_mpred_expansions/1).
:- meta_predicate(with_no_mpred_expansions(0)).



%% with_no_mpred_expansions( :GoalGoal) is det.
%
% Using No Managed Predicate Expansions.
%
with_no_mpred_expansions(Goal):-
  w_tl(tlbugger:no_buggery_tl,
    w_tl(t_l:disable_px,Goal)).


:-  /**/ export(with_mpred_expansions/1).
:- meta_predicate(with_mpred_expansions(0)).



%% with_mpred_expansions( :GoalGoal) is det.
%
% Using Managed Predicate Expansions.
%
with_mpred_expansions(Goal):-
  wno_tl(tlbugger:no_buggery_tl,
    wno_tl(t_l:disable_px,Goal)).

:-  /**/ export(ensure_loaded_no_mpreds/1).
:- meta_predicate(ensure_loaded_no_mpreds(0)).



%% ensure_loaded_no_mpreds( :GoalF) is det.
%
% Ensure Loaded No Managed Predicates.
%
ensure_loaded_no_mpreds(F):-with_no_mpred_expansions(forall(must_locate_file(F,L),ensure_loaded(L))).




%% use_was_isa( ?G, ?I, ?C) is det.
%
% use was  (isa/2).
%
use_was_isa(G,I,C):-call((current_predicate(_,_:mpred_types_loaded/0),if_defined(was_mpred_isa(G,I,C)))).




%% current_context_module( ?Ctx) is det.
%
% Current Context Module.
%
current_context_module(Ctx):-hotrace((loading_module(Ctx))),!.
current_context_module(Ctx):-hotrace((source_context_module(Ctx))).

% ========================================
% register_module_type/end_module_type
% ========================================
%:- was_module_transparent(baseKB:register_module_type/1).
% :- op(1100,fx,(shared_multifile)).




%% register_module_type( ?Type) is det.
%
% Register Module Type.
%
register_module_type(Type):-current_context_module(CM),register_module_type(CM,Type).



%% register_module_type( ?CM, ?Types) is det.
%
% Register Module Type.
%
:- multifile(baseKB:registered_module_type/2).
register_module_type(CM,Types):-is_list(Types),!,forall(member(T,Types),register_module_type(CM,T)).
register_module_type(CM,Type):-asserta_new(baseKB:registered_module_type(CM,Type)).

:-  /**/ export(end_module_type/2).



%% end_module_type( ?Type) is det.
%
% End Module Type.
%
end_module_type(Type):-current_context_module(CM),end_module_type(CM,Type).



%% end_module_type( ?CM, ?Type) is det.
%
% End Module Type.
%
end_module_type(CM,Type):-retractall(baseKB:registered_module_type(CM,Type)).

/******

% :- meta_predicate(ensure_mpred_file_loaded(0)). 

:- meta_predicate ensure_mpred_file_loaded(:,+).


ensure_mpred_file_loaded(M:F0,List):-!,
  must_locate_file(M:F0,F),  % scope_settings  expand(true),register(false),
  % 'format'(user_error /*e*/,'%  ~q + ~q -> ~q.~n',[M,F0,F]),
  load_files([F],[if(not_loaded), must_be_module(true)|List]).
   %load_files(F,[redefine_module(false),if(not_loaded),silent(false),exported(true),must_be_module(true)|List]).   
ensure_mpred_file_loaded(M:F0,List):-
  must_locate_file(M:F0,F),  % scope_settings
  'format'(user_error /*e*/,'% load_mpred_file_M ~q.~n',[M=must_locate_file(F0,F)]),
   load_files([F],[redefine_module(false),module(M),expand(true),if(not_loaded),exported(true),register(false),silent(false),must_be_module(true)|List]).

******/


:-  /**/ export(declare_load_dbase/1).



%% declare_load_dbase( ?Spec) is det.
%
% Declare Load Dbase.
%
declare_load_dbase(Spec):- forall(no_repeats_old(File,must_locate_file(Spec,File)),show_call(why,asserta_if_new(baseKB:registered_mpred_file(File)))).

% :-  /**/ export((is_compiling_sourcecode/1)).



%% is_compiling_sourcecode is det.
%
% If Is A Compiling Sourcecode.
%
is_compiling_sourcecode:-is_compiling,!.
is_compiling_sourcecode:-compiling, current_input(X),not((stream_property(X,file_no(0)))),prolog_load_context(source,F),\+((t_l:loading_mpred_file(_,_))),F=user,!.
is_compiling_sourcecode:-compiling,dmsg(system_compiling),!.

:-  /**/ export(load_mpred_files/0).



%% load_mpred_files is det.
%
% Load Managed Predicate Files.
%
load_mpred_files :- forall(baseKB:registered_mpred_file(File),ensure_mpred_file_loaded(File)).


% =======================================================
:- meta_predicate show_load_call(0).
show_load_call(C):- must(on_x_rtrace(show_call(why,C))).



:- dynamic(baseKB:loaded_file_world_time/3).
:- meta_predicate(baseKB:loaded_file_world_time(+,+,+)).
:- meta_predicate(get_last_time_file(+,+,+)).



%% get_last_time_file( +FileIn, +World, +LastTime) is det.
%
% Get Last Time File.
%
get_last_time_file(FileIn,World,LastTime):- absolute_file_name(FileIn,File),baseKB:loaded_file_world_time(File,World,LastTime),!.
get_last_time_file(_,_,0).



:- meta_predicate(load_init_world(+,:)).



%% load_init_world( +World, ?File) is det.
%
% Load Init World.
%
load_init_world(World,File):- 
 wno_tl(baseKB:use_cyc_database,
    ( world_clear(World),
      retractall(baseKB:loaded_file_world_time(_,_,_)),
      time_call(ensure_mpred_file_loaded(File)),!,
      time_call(finish_processing_world))).


:- meta_predicate(ensure_mpred_file_loaded(:)).



%% ensure_mpred_file_loaded( ?MFileIn) is det.
%
% Ensure Managed Predicate File Loaded.
%
:- meta_predicate(ensure_mpred_file_loaded(:)).

ensure_mpred_file_loaded(MFileIn):- strip_module(MFileIn,M,_), 
 forall(must_locate_file(MFileIn,File),
   must_det_l((time_file_safe(File,NewTime),!,
   get_last_time_file(File,_World,LastTime),
   (LastTime<NewTime -> force_reload_mpred_file(M:File) ; true)))).

:- meta_predicate(force_reload_mpred_file(?)).

:- meta_predicate(ensure_mpred_file_loaded(+,:)).



%% ensure_mpred_file_loaded( +World, ?FileIn) is det.
%
% Ensure Managed Predicate File Loaded.
%
ensure_mpred_file_loaded(World,FileIn):- 
  with_umt(World,ensure_mpred_file_loaded(FileIn)).




%% must_locate_file( ?FileIn, ?File) is det.
%
% Must Be Successfull Locate File.
%
must_locate_file(FileIn,File):-
  quietly_must(filematch_ext(['','mpred','ocl','moo','plmoo','pl','plt','pro','p','pl.in','pfc','pfct'],FileIn,File)).

maybe_locate_file(FileIn,File):-
  quietly(filematch_ext(['','mpred','ocl','moo','plmoo','pl','plt','pro','p','pl.in','pfc','pfct'],FileIn,File)).







%% force_reload_mpred_file( ?FileIn) is det.
%
% Force Reload Managed Predicate File.
%
force_reload_mpred_file(MFileIn):- 
 strip_module(MFileIn,M,FileIn),
 (FileIn==MFileIn->defaultAssertMt(World);World=M),
  quietly_must(force_reload_mpred_file(World,FileIn)).




%% force_reload_mpred_file( ?World, ?MFileIn) is det.
%
% Force Reload Managed Predicate File.
%
force_reload_mpred_file(World,MFileIn):- 
 without_varname_scan(force_reload_mpred_file2(World,MFileIn)).

%% force_reload_mpred_file2( ?World, ?MFileIn) is det.
%
% Helper for Force Reloading of a Managed Predicate File.
%
force_reload_mpred_file2(WorldIn,MFileIn):- 
 must(call_u(baseKB:mtCycL(WorldIn)->World=WorldIn;defaultAssertMt(World))),
 strip_module(MFileIn,_MaybeNewModule,_),
 NewModule = World,
 with_source_module(NewModule,((
 % NewModule:ensure_loaded(logicmoo(mpred/mpred_userkb)),
 forall(must_locate_file(MFileIn,File),
   must_det_l((
   once(show_success(prolog_load_file,defaultAssertMt(DBASE));DBASE=NewModule),
   sanity(exists_file(File)),
   sanity((true,defaultAssertMt(World))),
   nop(mpred_remove_file_support(File)),
   assert_if_new(baseKB:registered_mpred_file(File)),
   quietly_must(time_file_safe(File,NewTime)),
   retractall(baseKB:loaded_file_world_time(File,World,_)),
   system:assert(baseKB:loaded_file_world_time(File,World,NewTime)),    
   DBASE = DBASE,
   wno_tl(t_l:disable_px,
     w_tl(set_prolog_flag(lm_expanders,true),
      w_tl(set_prolog_flag(mpred_te,true),
     show_call((with_source_module(NewModule,load_files(NewModule:File, [module(NewModule)]))))))),
   catch((w_tl(t_l:loading_mpred_file(World,File),     
      load_mpred_on_file_end(World,File))),
    Error,
    (wdmsg(error(Error,File)),retractall(baseKB:loaded_mpred_file(World,File)),
     retractall(baseKB:loaded_file_world_time(File,World,_AnyTime)))))))))).




%% load_mpred_on_file_end( ?World, ?File) is det.
%
% Load Managed Predicate Whenever File End.
%
:- export(load_mpred_on_file_end/2).
load_mpred_on_file_end(World,File):- atom(File),
   quietly_must(atom(File)),
   asserta_new(baseKB:loaded_mpred_file(World,File)),
   dmsginfo(info(load_mpred_file_complete(File))),
   forall(t_l:on_eof(File,Call),quietly_must((on_f_log_ignore(Call),retractall(t_l:on_eof(File,Call))))).










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




%% is_kif_string( ?String) is det.
%
% If Is A Knowledge Interchange Format String.
%
is_kif_string([]):- !,fail.
is_kif_string(String):-atomic(String),name(String,Codes), memberchk(40,Codes),memberchk(41,Codes).




%% convert_if_kif_string( ?I, ?O) is det.
%
% Convert If Knowledge Interchange Format String.
%
convert_if_kif_string(I, O):-is_kif_string(I),convert_from_kif(I,O),!, \+ is_list(O).


convert_from_kif(I,O):- from_kif_string(I,Wff),quietly_must((sexpr_sterm_to_pterm(Wff,O))),!.


%% from_kif_string( ?String, ?Forms) is det.
%
% Converted From Knowledge Interchange Format String.
%
from_kif_string(I,Wff) :- input_to_forms(I,Wff,Vs),put_variable_names(Vs),!.
from_kif_string(String,Forms) :- quietly_must((codelist_to_forms(String,Forms);input_to_forms(string(String),Forms))),!.
from_kif_string(Wff,Wff).


:- module_transparent(must_map_preds/3).
must_map_preds([],IO,IO):-!.
must_map_preds([one(Pred)|ListOfPreds],IO,Out):- must(call(Pred,IO)),!,
   must_map_preds(ListOfPreds,IO,Out).
must_map_preds([Pred|ListOfPreds],In,Out):- must(call(Pred,In,Mid)),!,
   must_map_preds(ListOfPreds,Mid,Out),!.


%% assert_kif( ?String) is det.
%
% Assert Knowledge Interchange Format.
%
assert_kif(D):-
         must_det_l((must_map_preds([
           convert_from_kif,
           maybe_ruleRewrite,
           cyc_to_clif,
           unnumbervars_and_save,
           sexpr_sterm_to_pterm,
           fully_expand,
           sexpr_sterm_to_pterm],D,CycLOut),

         format('~q.~n',[(CycLOut)]),
         call_u((ground(CycLOut)->ain((CycLOut));ain(tinyKB8(CycLOut)))))).




%% assert_kif_dolce( ?String) is det.
%
% Assert Knowledge Interchange Format Dolce.
%
assert_kif_dolce(String):-convert_from_kif(String,Forms),dmsg(warn(assert_kif_dolce(Forms))),!,ain(tinyKB8(Forms)).



%% finish_processing_world is det.
%
% Finish Processing World.
%
finish_processing_world :- load_mpred_files, loop_check(w_tl(t_l:agenda_slow_op_do_prereqs,doall(finish_processing_dbase)),true).




%% loader_side_effect_verify_only( ?I, ?Supposed) is det.
%
% Loader Side Effect Verify Only.
%
loader_side_effect_verify_only(I,Supposed):-   
   sanity(var(ActualSupposed)),
    push_predicates(t_l:side_effect_buffer/3,STATE),
    prolog_load_context(module,M),
    mpred_expander_now_physically(M,I,Supposed),
    get_source_ref1(Why),
    collect_expansions(Why,I,Actual),
    convert_side_effect(suppose(Supposed),S),
    conjoin(S, Actual,ActualSupposed),
    conjuncts_to_list(ActualSupposed,Readable),
    system:assert(t_l:actual_side_effect(I,Readable)),
    pop_predicates(t_l:side_effect_buffer/3,STATE),!.




%% loader_side_effect_capture_only( ?I, ?ActualSupposed) is det.
%
% Loader Side Effect Capture Only.
%
loader_side_effect_capture_only(I,ActualSupposed):-   
   sanity(var(ActualSupposed)),
    push_predicates(t_l:side_effect_buffer/3,STATE),
    prolog_load_context(module,M),
    mpred_expander_now_physically(M,I,Supposed),
    get_source_ref1(Why),
    collect_expansions(Why,I,Actual),
    conjoin(Actual,Supposed,ActualSupposed),
    pop_predicates(t_l:side_effect_buffer/3,STATE),!.





%% collect_expansions( ?Why, ?I, ?I) is det.
%
% Collect Expansions.
%
collect_expansions(_Why,I,I):- \+ t_l:side_effect_buffer(_Op,_Data,_),!.
collect_expansions(NWhy,_I, TODO):- findall(ReproduceSWhy, 
  ( retract(t_l:side_effect_buffer(Op, Data, Why)),
    must_det_l(convert_side_effect(Op, Data,Reproduce)),
    quietly_must(simplify_why_r(Reproduce,Why,NWhy,ReproduceSWhy))), TODOs),
   must_det_l( list_to_conjuncts(TODOs,TODO)).




%% simplify_why_r( ?Reproduce, ?Why, ?NWhy, :TermReproduce) is det.
%
% Simplify Generation Of Proof R.
%
simplify_why_r(Reproduce,Why,NWhy,   Reproduce):- Why==NWhy, !.
simplify_why_r(Reproduce,Why,_,Reproduce:SWhy):-simplify_why(Why,SWhy),!.
 
% aliases
:- meta_predicate(convert_side_effect(?,+,-)).




%% simplify_why( ?Why, ?SWhy) is det.
%
% Simplify Generation Of Proof.
%
simplify_why(Why,SWhy):-var(Why),!,Why=SWhy.
simplify_why(Why:0,SWhy):-!,simplify_why(Why,SWhy).
simplify_why(Why:N,SWhy:N):-!,simplify_why(Why,SWhy).
simplify_why(Why,SWhy):- atom(Why),!,directory_file_path(_,SWhy,Why).
simplify_why(Why,Why).




%% convert_side_effect( ?C, +A, -SE) is det.
%
% Convert Side Effect.
%
convert_side_effect(M:C,A,SE):- Call=..[C,A],!,convert_side_effect(M:Call,SE).
convert_side_effect(C,A,SE):- Call=..[C,A],!,convert_side_effect(Call,SE).




%% convert_side_effect( ?I, ?OO) is det.
%
% Convert Side Effect.
%
convert_side_effect(suppose(OO), suppose(Result)):- convert_side_effect_0a(OO,Result),!.
convert_side_effect(I,OO):-convert_side_effect_0c(I,O),((O=(N-_V),number(N))->OO=O;OO=O),!.




%% convert_side_effect_0a( ?I, ?O) is det.
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




%% convert_side_effect_0b( :TermOpData, ?Result) is det.
%
% Convert Side Effect 0b.
%
convert_side_effect_0b((OpData:-TRUE),Result):- is_true(TRUE),!,convert_side_effect_0a(OpData,Result),!.
convert_side_effect_0b(suppose(OpData),Result):-!,convert_side_effect_0a(OpData,Result),!.
convert_side_effect_0b(baseKB:OpData,Reproduce):- !,convert_side_effect_0a(OpData,Reproduce),!.
convert_side_effect_0b(( :- OpData),( ( (Result)))):-!,convert_side_effect_0a(OpData,Result),!.
convert_side_effect_0b('$si$':'$was_imported_kb_content$'(_, OO),Result):-!,convert_side_effect_0a(OO,Result),!.
convert_side_effect_0b(asserta_if_new(Data),Result):-!,convert_side_effect_0a(asserta(Data),Result).
convert_side_effect_0b(assertz_if_new(Data),Result):-!,convert_side_effect_0a(assertz(Data),Result).
convert_side_effect_0b(assert_if_new(Data),Result):-!,convert_side_effect_0a(assertz(Data),Result).
convert_side_effect_0b(assert(Data),Result):-!,convert_side_effect_0a(assertz(Data),Result).


% unused_assertion('$was_imported_kb_content$'([], A)):-atom(A).


%% convert_side_effect_0c( ?OpData, ?Reproduce) is det.
%
% Convert Side Effect 0c.
%
convert_side_effect_0c(OpData,Reproduce):- convert_side_effect_0b(OpData,Reproduce),!.
convert_side_effect_0c(OpData,Reproduce):- show_success(convert_side_effect,convert_side_effect_buggy(OpData,Reproduce)),!.
convert_side_effect_0c(OpData,Reproduce):- trace_or_throw(unknown_convert_side_effect(OpData,Reproduce)),!.

% todo



%% convert_side_effect_buggy( ?H, ?HB) is det.
%
% Convert Side Effect Buggy.
%
convert_side_effect_buggy(erase(clause(H,B,_Ref)), (e(HB))):- convert_side_effect_0a((H:-B),HB).
convert_side_effect_buggy(retract(Data), (r(DataR))):-convert_side_effect_0a(Data,DataR).
convert_side_effect_buggy(retractall(Data), (c(DataR))):-convert_side_effect_0a(Data,DataR).
convert_side_effect_buggy(OpData,( (  error_op(OpData)))):-dmsg(unknown_convert_side_effect(OpData)).





%% clear_predicates( :TermM) is det.
%
% Clear Predicates.
%
clear_predicates(M:H):- forall(M:clause(H,_,Ref),erase(Ref)).



%% push_predicates( :TermM, ?STATE) is det.
%
% Push Predicates.
%
push_predicates(M:F/A,STATE):- functor(H,F,A),findall((H:-B), (M:clause(H,B,Ref),erase(Ref)), STATE).



%% pop_predicates( :TermM, ?STATE) is det.
%
% Pop Predicates.
%
pop_predicates(M:F/A,STATE):- functor(H,F,A),forall(member((H:-B),STATE),M:assert((H:-B))).



mpred_loader_file.
system:term_expansion(end_of_file,_):-must(check_clause_counts),fail.
