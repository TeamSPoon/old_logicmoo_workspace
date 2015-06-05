:-include('logicmoo_utils_header.pl').
% ===========================================================
% Invoke Prolog Command
% ===========================================================
clientEvent(Channel,Agent,Options):- %memberchk(interp='prolog',Options),!,
        memberchk(submit=command,Options),!, make,
        ensureMooOption(Agent,sf=surf,Prolog),
        ignore(parse_prolog_cmd(Prolog)).

parse_prolog_cmd(Prolog):-
        writeDebugWindow,
	fmt('<pre>') ,
        catch(atom_to_term(Prolog,CMD,Vars),E,
        (message_to_string(E,S),fmt('\nCall "~w" could not be read.  \nError: ~s\n',[Prolog,S]))),!,
        callFromWeb(Prolog,CMD,Vars),!.
parse_prolog_cmd(Prolog):-fmt('\nCall "~w" failed',[Prolog]),!.
callFromWeb(Prolog,CMD,Vars):-var(CMD),!.
callFromWeb(Prolog,CMD,Vars):-
        thread_self(Id),
        ensureMooOption(Agent,'$socket_out',user_output,Out),
        ensureMooOption(Agent,'$socket_in',user_input,In),
        invokePrologCommandRDF(Id,In,Out,CMD,Vars,Results),
        fmt('\n       Results: ~w\n',[Results]),!.


clientEvent(Channel,Agent,[file='prolog.moo'|List]):-
      ignore(member(pformula=Call,List)),
      ignore(Call='member(A,[1,2,3])'),!,
      writeHTMLStdHeader('Prolog Interactor'),
      fmt('
      <form method="GET">
	<p><textarea rows="9" name="pformula" cols="40">~w</textarea><br>
	<input type="submit" value="Call" name="submit">&nbsp;<input type="reset" value="Reset" name="resetButton"></p>
      </form>',[Call]),
      toCallable(Call,PrologGoal,ToplevelVars),
        getMooOption(Agent,'$socket_in',In),
        getMooOption(Agent,'$socket_out',Out),!,
	fmt('<pre>'),
	 invokePrologCommandRDF(foo,In,Out,PrologGoal,ToplevelVars,Returns),!,
	fmt('Returns = ~w',[Returns]),
	fmt('</pre>'),
      writeHTMLStdFooter.

toCallable(true,true,[]):-!.
toCallable(C,C,[]):-compound(C),!.
toCallable(A,T,V):-atom(A),catch(atom_to_term(A,T,V),_,fail).
toCallable(A,fmt('could not parse ~w.',[A]),V):-!.


clientEvent(Channel,Agent,Options):- %memberchk(interp='prolog',Options),!,
        memberchk(cmd='Compile Demo',Options),!,autoload,
        (unsetMooOption(Agent,client=html)),qsave_program('moo_server',[goal=(initializeMooServerData)]),(setMooOption(Agent,client=html)).

build_exe_and_halt:- !,autoload,
        (unsetMooOption(Agent,client=html)),qsave_program('moo_server',[goal=(initializeMooServerData)]).


clientEvent(Channel,Agent,english(phrase(List,Codes),_)):-
		once(getRegistered(Channel,Agent,execute)),
		catch((atom_codes(Atom,Codes),atom_to_term(Atom,Term,Vars)),_,fail),
		catch(once(Term),_,fail).


