% kernel.pl
% May 18, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
/** <module> 
% This module defineds the moduule typers that we use
% It is the basic heart of the simulator, 
% the forward chaining code in in 'actr' which handles the run_tests.
%
*/

% user is the reserved top level module. All predicates considered to be exported
:- module(kernel, 
   [
         term_expansion_local/2,
         register_module_type/1, 
         registered_module_type/2, 
         end_module_type/1,
         register_timer_thread/3,
         end_module_type/2         
         ]).


:-dynamic registered_module_type/2.


register_timer_thread(Name,_Seconds,_OnTick):-current_thread(Name,_Status).
register_timer_thread(Name,Seconds,OnTick):-
   thread_create(tick_every(Name,Seconds,OnTick),_ID,[alias(Name)]). 

tick_every(Name,Seconds,OnTick):-repeat,sleep(Seconds),catch(OnTick,E,dmsg(caused(Name,OnTick,E))),fail.
   
end_module_type(Type):-current_context_module(CM),end_module_type(CM,Type).
end_module_type(CM,Type):-retractall(registered_module_type(CM,Type)).

register_module_type(Type):-current_context_module(CM),register_module_type(CM,Type).

register_module_type(CM,Types):-is_list(Types),!,forall(member(T,Types),register_module_type(CM,T)).
register_module_type(CM,Type):-asserta_new(registered_module_type(CM,Type)).

registered_module_type(Type):-current_context_module(CM),registered_module_type(CM,Type).

:-meta_predicate term_expansion_local(?,?),term_expansion_local0(?,?).
% :-meta_predicate user:term_expansion(?,?).

term_expansion_local(X,_):-var(X),!,fail.
term_expansion_local( ((':-'(_))) , _ ):-!,fail.

term_expansion_local(_:B1,B2):-!,term_expansion_local(B1,B2),!.

term_expansion_local(((H1:-B1)),H2B2):- !,
   nonvar(B1),  current_context_module(CM),!,        
      functor(H1,F,A), atom_concat(P,'_hook',F),!,atomic_list_concat([_,_|_],'_',P),
      H1=..[F|ARGS], H2=..[P|ARGS],
      B2 = '@'((nop(CM), B1), CM ),
      module_transparent((P/A)),
      % copy_term(H2,META), meta_predicate(META),
      multifile(P/A),
      export(P/A),
      ignore(H2B2 = ((moo:H2 :- B2))),!.

term_expansion_local(X,Y):- loading_module_h(CM),functor(X,F,A),term_expand_local(CM,X,F,A,Y).



term_expand_local(CM,X,F,A,Y):-findall(Y,term_expand_local_each(CM,X,F,A,Y),Ys), Ys == [],!,fail.  

term_expand_local_each(_,_,F,A,_):- member(F/A,[never_expand]),!,fail.
term_expand_local_each(CM,X,F,A,X):-registered_module_type(CM,utility),export(F/A).
term_expand_local_each(CM,X,F,A,X):-registered_module_type(CM,dynamic),dynamic(F/A).


term_expansion_local0(A,B):-term_expansion_local(A,B),!.
term_expansion_local0(A,A).


% user:term_expansion(X,Y):- term_expansion_local0(X,Y).

:- include(logicmoo('vworld/vworld_header.pl')).

:- register_module_type(utility).

moo:agent_text_command(_Agent,_Text,_AgentTarget,_Cmd):-fail.

:- include(logicmoo('vworld/vworld_footer.pl')).

