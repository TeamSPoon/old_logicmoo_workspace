

:- use_module('look.pl').   % get_percepts(Agent,[list of internal traits],[list of percepts]).
:- use_module('move.pl').   % move(Dir). Dir is one of 8 cardinal directions
:- use_module('sit.pl').    % sit. Don't do anything.
:- use_module('take.pl').   % take(Object). take an object in the same atloc as agent
:- use_module('drop.pl').   % drop(Object). drop an object in agent's possession
:- use_module('climb.pl').  % climb(Dir). climb up on an object in direction Dir
:- use_module('push.pl').   % push(Dir). push an object in direction Dir
:- use_module('eat.pl').    % eat(Object). eat/destroy object in possesion
:- use_module('attack.pl'). % attack(Dir). attack another agent in direction Dir



