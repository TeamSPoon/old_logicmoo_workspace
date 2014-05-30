/** <module> 
% Test worlds for vacuum world can be defined by a grid like the
% one below. The grid must be rectangular (ie. same number of
% columns for each row). It can be of any size.
%
% To look at the world, use the show_world/0 command. 
%
% The two letter codes used below are defined in map.objects.pl
%
% predator.map.pl
% July 10, 1996
% John Eikenberry
%
%
% Dec 13, 2035
% Douglas Miles
%
*/

:- module(vacuum_map,[]).

moo:grid_key( ed='edge of world').
moo:grid_key( hw='high wall').
moo:grid_key( lw='low wall').
moo:grid_key( hb='high box').
moo:grid_key( lb='low box').
moo:grid_key( dt=dirt).
moo:grid_key( ot=outlet).



moo:type_grid(storage,1, [ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed]).
moo:type_grid(storage,2, [ed,dt,dt,--,--,--,--,--,--,hw,hw,--,--,--,--,--,--,--,--,ed]).
moo:type_grid(storage,3, [ed,dt,dt,--,--,--,--,--,--,hw,hw,--,--,--,--,--,--,--,--,ed]).
moo:type_grid(storage,4, [ed,dt,dt,dt,dt,--,--,--,--,hw,hw,--,--,--,--,dt,dt,--,--,ed]).
moo:type_grid(storage,5, [ed,hw,hw,lw,lw,ot,--,--,--,hw,hw,--,--,dt,dt,dt,--,--,--,ed]).
moo:type_grid(storage,6, [ed,--,--,--,lw,lw,lw,--,--,hw,hw,--,lb,lb,dt,dt,--,--,--,ed]).
moo:type_grid(storage,7, [ed,--,--,--,--,--,--,--,--,hw,hw,--,--,lb,lb,dt,--,--,--,ed]).
moo:type_grid(storage,8, [ed,--,--,--,--,--,--,--,--,lw,lw,--,--,--,--,--,--,--,--,ed]).
moo:type_grid(storage,9, [ed,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,ed]).
moo:type_grid(storage,10,[ed,--,--,--,dt,--,--,--,--,--,--,--,--,--,--,--,--,--,--,ed]).
moo:type_grid(storage,11,[ed,--,--,--,dt,--,--,--,--,--,--,--,--,--,--,--,--,--,--,ed]).
moo:type_grid(storage,12,[ed,--,--,--,--,dt,dt,--,--,--,--,dt,dt,dt,dt,dt,dt,--,--,ed]).
moo:type_grid(storage,13,[ed,--,--,--,--,--,dt,--,--,--,--,--,dt,dt,dt,dt,dt,--,--,ed]).
moo:type_grid(storage,14,[ed,--,--,--,--,lb,lb,--,--,--,--,--,--,--,dt,dt,--,--,--,ed]).
moo:type_grid(storage,15,[ed,--,--,--,--,hb,hb,hb,hb,--,--,--,--,dt,dt,--,--,ot,--,ed]).
moo:type_grid(storage,16,[ed,--,--,--,--,--,--,--,--,--,--,--,dt,dt,--,--,--,--,--,ed]).
moo:type_grid(storage,17,[ed,--,--,--,--,--,ot,--,--,--,--,dt,dt,--,--,--,--,--,--,ed]).
moo:type_grid(storage,18,[ed,--,--,--,--,--,--,--,--,--,dt,dt,--,--,--,--,--,--,--,ed]).
moo:type_grid(storage,19,[ed,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,ed]).
moo:type_grid(storage,20,[ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed]).


