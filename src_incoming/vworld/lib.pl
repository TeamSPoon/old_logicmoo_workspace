% library.pl
% July 7, 1996
% John Eikenberry
% Dec 13, 2035
% Douglas Miles
%
/** <module> 
% General library of predicates not always portable between prolog impls
*/
:- module(lib,
	[
        % random_premutation/2,
          maybe/1, 
          with_assertions/2,
         nth_member/3,
         edit_out/3]).

:- module_transparent  maybe/1.
:- meta_predicate with_assertions(:,0).
:- meta_predicate forall_member(?,?,0).

:- include(logicmoo('vworld/vworld_header.pl')).
:- register_module_type(utility).

with_assertions(With,Call):-setup_call_cleanup(asserta(With,Ref),Call,erase(Ref)).



% Edit out 
edit_out([],[]):-!.
edit_out([Agent|Agents],Agent^Test,NewAgents) :-
   edit_out(Agents,Agent^Test,TempAgents),
    (call(Test) -> NewAgents=TempAgents ; NewAgents=[Agent|TempAgents]).


% Random_Permutation defined for SWI... comment out for quintus!
% Randomizes two lists.
% random_permutation([],[]).
% random_permutation(Old,New) :-
% 	length(Old,N), 
% 	R is (random(N) + 1), 
%	nth_member(R,Elem,Old),
%	select(Old,Elem,Middle),
%	random_permutation(Middle,RP),
%	append(RP,[Elem],New).


% Goal done once and only once
% not use anymore 
% one(Goal) :- once(Goal).
% Goal,
% 	!.


% Quintus version
% member(X,[X|_]).
% member(X,[_|Y]) :-
%	member(X,Y).

% Quintus version
%ticks(T) :-
%	!,
%	time(time(H,M,S)),
%	T is 360*H + 60*M + S.

% SWI mabey pred... not needed for quintus
% Used only in kernel. Placed here since it isn't needed for quintus, so
% it can be easily found and commented out.
maybe(Maybe) :-
    Temp is random(100),
    Temp < Maybe -> true ; fail.


% Grab certain member of a list as denoted by its place in that list.
nth_member(1,X,[X|_]) :-
	!.
nth_member(N,X,[_|T]) :-
	N > 1,
	M is N - 1,
	!,
	nth_member(M,X,T).


:- include(logicmoo('vworld/vworld_footer.pl')).
