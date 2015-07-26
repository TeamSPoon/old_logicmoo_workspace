   % NOTICE: %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   %                                                                      %
   %  COPYRIGHT (2009) University of Dallas at Texas.                     %
   %                                                                      %
   %  Developed at the Applied Logic, Programming Languages and Systems   %
   %  (ALPS) Laboratory at UTD by Feliks Kluzniak.                        %
   %                                                                      %
   %  Permission is granted to modify this file, and to distribute its    %
   %  original or modified contents for non-commercial purposes, on the   %
   %  condition that this notice is included in all copies in its         %
   %  original form.                                                      %
   %                                                                      %
   %  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,     %
   %  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES     %
   %  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, TITLE AND     %
   %  NON-INFRINGEMENT. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR        %
   %  ANYONE DISTRIBUTING THE SOFTWARE BE LIABLE FOR ANY DAMAGES OR       %
   %  OTHER LIABILITY, WHETHER IN CONTRACT, TORT OR OTHERWISE, ARISING    %
   %  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR       %
   %  OTHER DEALINGS IN THE SOFTWARE.                                     %
   %                                                                      %
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%  Predicates specific to SWI Prolog.                                      %%%
%%%                                                                          %%%
%%%  Written by Feliks Kluzniak at UTD (February, August 2009)               %%%
%%%                                                                          %%%

:- ensure_loaded( higher_order ).
:- ensure_loaded( library( lists ) ). % An SWI library, for reverse/2.
:- ensure_loaded( utilities ).


%%------------------------------------------------------------------------------
%% Identify the system.

lp_system( 'SWI Prolog' ).


%%------------------------------------------------------------------------------
%% The standard streams.

std_input_stream(   user_input  ).
std_output_stream(  user_output ).
std_error_stream(   user_error  ).
std_warning_stream( user_error  ).


%%------------------------------------------------------------------------------
%% getchar( + input stream, - character in the form of an atom ):
%% This is introduced because the built-in get_char/2 returns strings on
%% Eclipse and atoms on Sicstus.

getchar( Stream, Atom ) :-
        get_char( Stream, Atom ).


%%------------------------------------------------------------------------------
%% name_chars( +- atom or number,
%%             -+ list of characters (codes) that form its name
%%           ):
%% Used because Eclipse complains about name/2 being obsolete.

name_chars( Atomic, NameCharCodes ) :-
        name( Atomic, NameCharCodes ).


%%------------------------------------------------------------------------------
%% clause_in_module( + module name, +- clause head, - clause body ):
%% Like clause/2, but from the named module.
%%
%% NOTE: For the module 'interpreted' we provide special treatment.  Instead of
%%       asserting clauses, we record them in the database, to avoid the
%%       overhead of decompilation.
%%       "Head :- Body" is recorded as "interpreted_clause( Head, Body )"
%%       under the key "Head" (i.e., effectively under the name and arity of the
%%       predicate). The effective key is also recorded, under the key
%%       "interpreted_clause_key".
%%   ->  This has been commented out for the time being, as the win is not all
%%       that clear.  Just search for "interpreted" and uncomment to get back to
%%       that version, but NOTE that it is unclear whether essence_hook/2 would
%%       then work properly.

% clause_in_module( interpreted, Head, Body ) :-
%        !,
%        recorded( Head, interpreted_clause( Head, Body ) ).

clause_in_module( ModuleName, Head, Body ) :-
        clause( ModuleName : Head, Body ).


%%------------------------------------------------------------------------------
%% current_predicate_in_module( + module name, +- predicate specification ):
%% Like current_predicate/2, but from the named module.
%%
%% NOTE: See the note to clause_in_module/3 above.

% current_predicate_in_module( interpreted, PredSpec ) :-
%        !,
%        recorded( interpreted_clause_key, PredSpec ).

current_predicate_in_module( ModuleName, PredSpec ) :-
        current_predicate( ModuleName : PredSpec ).


%%------------------------------------------------------------------------------
%% assert_in_module( + module name, + clause ):
%% Like assert/1, but into this module.
%%
%% NOTE: See the note to clause_in_module/3 above.

assert_in_module( Module, Clause ) :-
        assertz_in_module( Module, Clause ).


%%------------------------------------------------------------------------------
%% assertz_in_module( + module name, + clause ):
%% Like assertz/1, but into this module.
%%
%% NOTE: See the note to clause_in_module/3 above.

% assertz_in_module( interpreted, Clause ) :-
%         !,
%         (
%             Clause = (Head :- Body)
%         ->
%             true
%         ;
%             Head = Clause,
%             Body = true
%         ),
%         recordz( Head, interpreted_clause( Head, Body ) ),
%         functor( Head, Name, Arity ),
%         (
%             recorded( interpreted_clause_key, Name / Arity )
%         ->
%             true
%         ;
%             recordz( interpreted_clause_key, Name / Arity )
%         ).

assertz_in_module( Module, Clause ) :-
        assertz( Module : Clause ).


%%------------------------------------------------------------------------------
%% retractall_in_module( + module name, + head pattern ):
%% Like retractall/1, but into this module.
%%
%% NOTE: See the note to clause_in_module/3 above.

% retractall_in_module( interpreted, Head ) :-
%         recorded( Head, interpreted_clause( Head, _ ), RefClause ),
%         erase( RefClause ),
%         fail.

% retractall_in_module( interpreted, _ ) :-
%         !.

retractall_in_module( Module, Head ) :-
        retractall( Module : Head ).


%%------------------------------------------------------------------------------
%% call_in_module( + module name, + head pattern ):
%% Like call/1, but into this module.

call_in_module( Module, Head ) :-
        call( Module : Head ).


%%------------------------------------------------------------------------------
%% export_from_module( + module name, + predicate specification ):
%% For Sicstus this is a no-op.

export_from_module( _, _ ).


%%------------------------------------------------------------------------------
%% dynamic_in_module( + module name, + predicate specification ):
%% For Sicstus this is a no-op.

dynamic_in_module( _, _ ).


%%------------------------------------------------------------------------------
%% compile_to_module( + module name, + file name ):
%% Compile the program in this file into this module.

compile_to_module( Module, FileName ) :-
        compile( Module : FileName ).


%%------------------------------------------------------------------------------
%% copy_term2( + term, - term ):
%% Same as copy_term/2, but safe for cyclic terms.
%% In the case of Sicstus there are no problems.

copy_term2( Term, Copy ) :-
        copy_term( Term, Copy ).


%%------------------------------------------------------------------------------
%% are_variants( + term, + term ) :
%%    Succeeds only if both arguments are variants of each other.
%%    Does not instantiate any variables.
%% NOTE:
%%   If variant/2 turns out to be broken, replace the last call with the
%%    following three:
%%        check( T1 = T2 ),                   % quickly weed out obvious misfits
%%        copy_term( T2, CT2 ),
%%        check( (numbervars( T1, 0, N ), numbervars( CT2, 0, N ), T1 = CT2) ).

are_variants( T1, T2 ) :-
        variant( T1, T2 ).


%%------------------------------------------------------------------------------
%% write_shallow( + output stream, + term, + maximum depth ):
%% Like write/2, but only to a limited print depth.

write_shallow( OutputStream, Term, MaxDepth ) :-
       write_term( OutputStream, Term, [ max_depth( MaxDepth ) ] ).


%%------------------------------------------------------------------------------
%% is_built_in( +- goal ):
%% Does this goal call a built-in predicate?  Or generate a built-in goal.

is_swi_builtin( Pred ) :-
        ( \+ ( \+ predicate_property( Pred, built_in ))).


%%------------------------------------------------------------------------------
%% ordered_term_variables( + term, - list of variables ):
%% Produce the set of variables in this term in the order of their occurrence.
%% (term_variables/2 does it in that order in Sicstus, but in reverse order in
%%  Eclipse.)

ordered_term_variables( Term, Variables ) :-
        term_variables( Term, Variables ).


%%------------------------------------------------------------------------------
%% readvar( + input stream, - term, - variable dictionary  ):
%% Simulates Eclipse's readvar/3.  The variable dictionary will be in the format
%% used by Eclipse, not by Sicstus (i.e., an entry has the form
%% "[ name | Variable ]" rather than "name = variable".

readvar( InputStream, Term, EclipseVarDict ) :-
        read_term( InputStream, Term, [ variable_names( SicstusVarDict ) ] ),
        map( translate_vardict_entry, SicstusVarDict, EclipseVarDict ),
        !.

%
translate_vardict_entry( N = V, [ N | V ] ).



%%------------------------------------------------------------------------------
%% erase_module( + module name ):
%% Simulates Eclipse's erase_module/1.
%%
%% NOTE: See the note to clause_in_module/3 above.

% erase_module( interpreted ) :-
%         recorded( interpreted_clause_key,  Name / Arity, RefKey ),
%         erase( RefKey ),
%         functor( Key, Name, Arity ),
%         recorded( Key, interpreted_clause( _, _ ), RefClause ),
%         erase( RefClause ),
%         fail.

% erase_module( interpreted ) :-
%        !.

erase_module( Module ) :-
        current_predicate( Module : PredSpec ),
        PredSpec = PredName / PredArity,
        mk_pattern( PredName, PredArity, PredPattern ),
        \+ predicate_property( Module : PredPattern, built_in ),
        abolish( Module : PredSpec ),
        fail.

erase_module( _ ).


%%------------------------------------------------------------------------------
%% setval( + name, + value ):
%% Set this counter to this value.
%%
%% NOTE: Since DRA uses global variables to store only integers, we use the
%%       flag/3 facility of SWI Prolog.  For more general values we would have
%%       to use nb_setval/nb_getval.  See also getval/2 and incval/1 below.

setval( Name, Value ) :-
        flag( Name, _Old, Value ).


%%------------------------------------------------------------------------------
%% getval( + name, - value ):
%% Get the value associated with this counter.

getval( Name, Value ) :-
        flag( Name, Value, Value ).


%%------------------------------------------------------------------------------
%% incval( + name ):
%% Increment this counter by 1.
incval( Name ) :- !, flag( Name, Value, Value+1 ).
incval( Name ) :-
        getval( Name, Value ),
        NewValue is Value + 1,
        setval( Name, NewValue ).


%%------------------------------------------------------------------------------
%% writeclause( + output stream, + clause ):
%% Given an open output stream, write the clause onto it.

writeclause( OutputStream, (:- Directive) ) :-
        !,
        write( OutputStream, ':- ' ),
        write_term( OutputStream, Directive, [ quoted( true ) ] ),
        write( OutputStream, '.' ),
        nl( OutputStream ).

writeclause( OutputStream, (?- Query) ) :-
        !,
        write( OutputStream, '?- ' ),
        write_term( OutputStream, Query, [ quoted( true ) ] ),
        write( OutputStream, '.' ),
        nl( OutputStream ).

writeclause( OutputStream, Clause ) :-
        write_term( OutputStream, Clause, [ quoted( true ) ] ),
        write( OutputStream, '.' ),
        nl( OutputStream ).


%%------------------------------------------------------------------------------
%% writeln( + output stream, + term ):
%% Write the term, followed by a newline.

writeln( OutputStream, Term ) :-
        write( OutputStream, Term ),
        nl(    OutputStream ).


%%------------------------------------------------------------------------------
%% writeln( + term ):
%% Write the term to the standard output stream, followed by a newline.

writeln( Term ) :-
        std_output_stream( OutputStream ),
        writeln( OutputStream, Term ).


%%------------------------------------------------------------------------------
%% concat_atoms( + atom, + atom, - atom ):
%% Return an atom whose name is the concatenation of the names of the first two
%% atoms.

concat_atoms( A, B, AB ) :-
        name( A, AChars ),
        name( B, BChars ),
        append( AChars, BChars, ABChars ),
        name( AB, ABChars ).

%%------------------------------------------------------------------------------
