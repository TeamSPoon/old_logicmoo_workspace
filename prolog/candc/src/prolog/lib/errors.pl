
:- module(errors,[warning/2,error/2,inform/2]).

:- use_module(semlib(options),[candc_option/2]).

warning(S,V):-
   candc_option('--warnings',true), !,
   format(user_error,'\033[33mWARNING: ',[]),
   format(user_error,S,V),
   format(user_error,'\033[0m~n',[]).

warning(_S,_V):-
   candc_option('--warnings',false).

inform(S,V):-
   candc_option('--info',true), !,
   format(user_error,'\033[34mINFO: ',[]),
   format(user_error,S,V),
   format(user_error,'\033[0m~n',[]).

inform(_S,_V):-
   candc_option('--info',false).

error(S,V):-
   format(user_error,'\033[31mERROR: ',[]),
   format(user_error,S,V),
   format(user_error,'\033[0m~n',[]).

