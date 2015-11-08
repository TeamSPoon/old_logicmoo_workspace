% -*-Prolog-*-

:- use_module(library(logicmoo_user)).

%% a simple Pfc example - the one bulb problem (see DeKleer and
%% Williams, IJCAI89)
%%
%% Tim Finin, finin@prc.unisys.com, 8/89


% a conflict triggers a Prolog action to resolve it.
conflict(C) ==> {resolveConflict(C)}.

% this isn't written yet.
resolveConflict(C) :-
  format("~NHalting with conflict ~w", [C]),
  mpred_halt.

% meta rules to schedule inferencing.

% resolve conflicts asap
mpred_select(conflict(X),S) :- mpred_queue(conflict(X),S).
  
% a pretty basic conflict.
( ~P), P ==> conflict(P).


% Devices behave as intended unless they are faulty.
isa(X,Class), ~faulty(X) ==> behave(X,Class).

% assume an observation is true.
observed(P), ~false_observation(P) ==> P.

% connecting two terminals means their voltages are equal.
con(T1,T2) ==> (volt(T1,V) <==> volt(T2,V)).

% a wire behaves by connecting its two terminals.
behave(X,wire) ==> con(t1(X),t2(X)).

% a battery's behaviour
behave(X,battery)
  ==>
 volt(t1(X),1.5),
 volt(t2(X),0).

% a bulb's behaviour.
behave(X,bulb), 
 volt(t1(X),V1),
 volt(t2(X),V2), 
 {V1\==V2} 
==> lit(X).

% It is a conflict if a terminal has two different voltages.
% volt(T,V1), volt(T,V2)/( \+V1=:=V2) ==> conflict(two_voltages(T,V1,V2)).

%% ***** here is a particular test case. *****


% here is a particular circuit - a gizmo.

isa(X,gizmo) ==>
  isa(battery(X),battery),
  isa(bulb(X),bulb),

  isa(w1(X),wire),
  isa(w2(X),wire),
 
  con(t1(battery(X)),t1(w1(X))),
  con(t2(battery(X)),t1(w2(X))),
  con(t2(w1(X)),t1(bulb(X))),
  con(t2(bulb(X)),t2(w2(X))).

%% here is a diagnostic problem for a gizmo.

test_b1(X) :- 
  add([isa(X,gizmo),
       observed(( ~lit(bulb(X))))]).



