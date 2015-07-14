% ---------------------------------------------------------------------------
% Menu 
% ---------------------------------------------------------------------------

:- dynamic 
     haltSystem/0.

main :- haltSystem,halt.
main :- 
        \+(haltSystem),
        on_exception(_,
         (prompt(_,'> '), prompt( InputS1 ),
          process( InputS1, InputS2 ), 
          (  InputS2=[], Input=[], main
           ; append( Input, [10], InputS2 ),
             (  command_read( Input )
              ; command_debug( Input )
              ; command_deftree( Input )
              ; command_type( Input )
              ; command_help( Input )
              ; command_cd( Input )
              ; command_pwd( Input )
              ; command_exit( Input )
              ; nopass,command_solve( Input ) ), 
             !,main ))
        ,main)
        .

prompt( Input ) :-
        get0(X),
        ( (X =:= 10 ; X =:= 13), !, Input = []
         ;Input = [X|Input2], prompt(Input2) ).

command_solve( "" ) :- !.
command_solve( Input ) :- %No known command accepted
        ( append( ":load ",  _, Input )
         ;append( ":l ",     _, Input )
         ;append( ":debug ", _, Input )
         ;append( ":d ",     _, Input )
         ;append( ":deftree ", _, Input )
         ;append( ":e ", _, Input )
         ;append( ":type ", _, Input )
         ;append( ":t ", _, Input )
         ;append( ":help ", _, Input )
         ;append( ":h ", _, Input )
         ;append( ":cd ", _, Input )
         ;append( ":c ", _, Input )
         ;append( ":type ", _, Input )
         ;append( ":t ", _, Input )
         ;append( ":quit ", _, Input )
         ;append( ":q ", _, Input )      ),
        !.
command_solve( Input ) :-
        cleanVars,
        exprIni( Exp, Input, []),
        sCheckExpr( Exp, _, [], _),!,
        ( solve( Exp )
         ;write('') ).
command_solve( Text ) :- 
        name( Atom, Text ),
        write('ERROR: Incorrect expression '), write(Atom), write(' found'),nl.

command_read( Input ) :-
        ( append( ":load ", Str, Input )
         ;append( ":l ",    Str, Input ) ), !,
        Str = [34|Rest],
        get_ToQuote( Rest, Name, [] ),
        atom_chars( File, Name ),
        parser( File, Source ),
        checkType( Source, Rules ),
        deftrees(Rules).

command_debug( Input ) :-
        ( append( ":debug ", Expression, Input )
         ;append( ":d ",     Expression, Input ) ),!,
        ( pass,
          command_solve( Expression ),
          nopass
         ;nopass).

command_deftree( Input ) :-
        ( append( ":deftree ", Function, Input )
         ;append( ":e ",       Function, Input ) ),!,
        ( append( Name, [32], Function )
         ;Name=Function ),
        name(F,Name),
        (  dt(F,_,DT),!, format("~a:\n",[F]), write2(DT), nl
         ; \+(dt(F,_,DT)), format("There is no definitional tree\n",[]) ).

command_type( Input ) :-
        ( append( ":type ", ExpS, Input )
         ;append( ":t ",    ExpS, Input ) ),!,
        ( exprIni(Exp,ExpS,[])
        ; functionInfixID(F,_,_,ExpS,[]), Exp=f(F,2,[]) ),
        sCheckExpr( Exp, TypeExpr, [], Susts ),
        applySusts(Susts,TypeExpr,Type), simpType(Type,ShowType),
        write2(ShowType),nl.

command_help( Input ) :- 
        ( Input = ":help "
         ;Input = ":h "   ),!,
        nl,
        write('Commands:'),nl,
        write('   :load "<File>"      - Load Curry file in interpreter [:l]'),nl,
        write('   <Expression>        - Evaluate expression'),nl,
        write('   :debug <Expression> - Debug expression evaluation    [:d]'),nl,
        write('   :deftree <Function> - Show function deftree          [:e]'),nl,
        write('   :type <Expression>  - Show expression type           [:t]'),nl,
        write('   :cd "<Directory>"   - Change working directory       [:c]'),nl,
        write('   :pwd                - Show working directory         [:p]'),nl,
        write('   :help               - This help menu                 [:h]'),nl,
        write('   :quit               - Exit interpreter               [:q]'),nl,
        nl,
        write(' :? refers to the short form of each command (only first character)'),
        nl,
        nl.

command_cd( Input ) :-
        ( append( ":cd ", Str, Input )
         ;append( ":c ",  Str, Input ) ), !,
        Str = [34|Rest],
        get_ToQuote( Rest, Name, [] ),
        atom_chars( Dir, Name ),
        working_directory(_,Dir).

command_pwd( Input ) :-
        ( Input = ":pwd "
         ;Input = ":p "   ),!,
        working_directory(Dir,Dir),
        write(Dir),nl.

command_exit( Input ) :-
        ( Input = ":quit "
         ;Input = ":q "   ),!,
        assert(haltSystem).

