% maze.map.pl
% July 10, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
/** <module> 
% Test world for maze world can be defined by a grid like the
% one below. The grid must be rectangular (ie. same number of
% columns for each row). It can be of any size.
%
% To look at the world, once this file is consulted,  use the show_world/0 command. 
%
% The two letter codes used below are defined in maze.objects.pl
%
*/


moo:grid_key(ed='edge of world').
moo:grid_key(wl=wall).
moo:grid_key(lg=ledge).
moo:grid_key(dr=door).
moo:grid_key(gd=gold).
moo:grid_key(fd=food).
moo:grid_key(pt=pit).
moo:grid_key(el='elixer of health').

moo:type_grid(maze,1, [ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed]).
moo:type_grid(maze,2, [ed,--,--,--,wl,fd,--,fd,--,fd,--,fd,--,fd,--,--,fd,wl,gd,ed]).
moo:type_grid(maze,3, [ed,--,wl,--,wl,fd,--,--,--,--,--,--,--,--,--,wl,--,wl,--,ed]).
moo:type_grid(maze,4, [ed,--,wl,--,wl,fd,--,--,--,--,--,--,--,wl,--,wl,--,wl,--,ed]).
moo:type_grid(maze,5, [ed,--,wl,--,wl,wl,wl,--,wl,wl,wl,wl,--,wl,--,wl,--,wl,--,ed]).
moo:type_grid(maze,6, [ed,el,wl,--,--,--,--,--,--,--,--,--,--,wl,--,wl,--,wl,--,ed]).
moo:type_grid(maze,7, [ed,gd,wl,wl,wl,wl,wl,--,wl,wl,wl,--,wl,wl,--,--,--,--,--,ed]).
moo:type_grid(maze,8, [ed,wl,wl,--,--,--,--,--,--,--,--,--,--,wl,wl,wl,--,wl,--,ed]).
moo:type_grid(maze,9, [ed,--,--,--,wl,wl,wl,wl,wl,wl,wl,--,--,--,--,--,--,wl,--,ed]).
moo:type_grid(maze,10,[ed,--,wl,--,wl,--,--,--,--,gd,wl,--,wl,--,wl,--,--,wl,--,ed]).
moo:type_grid(maze,11,[ed,--,wl,--,wl,--,wl,--,wl,fd,wl,--,wl,fd,wl,--,--,wl,--,ed]).
moo:type_grid(maze,12,[ed,--,--,--,--,--,wl,--,wl,fd,wl,--,wl,wl,wl,--,--,wl,--,ed]).
moo:type_grid(maze,13,[ed,--,wl,--,wl,--,wl,--,wl,--,--,--,--,--,--,--,--,wl,--,ed]).
moo:type_grid(maze,14,[ed,fd,wl,--,wl,wl,wl,--,wl,wl,wl,wl,wl,wl,wl,wl,--,wl,--,ed]).
moo:type_grid(maze,15,[ed,--,wl,--,--,wl,--,--,--,--,--,--,--,--,pt,wl,--,--,--,ed]).
moo:type_grid(maze,16,[ed,fd,wl,fd,--,wl,--,wl,wl,wl,wl,wl,wl,--,wl,wl,wl,--,fd,ed]).
moo:type_grid(maze,17,[ed,--,wl,fd,--,--,--,--,--,--,--,--,--,--,--,--,wl,--,fd,ed]).
moo:type_grid(maze,18,[ed,fd,wl,wl,wl,wl,wl,wl,wl,--,wl,--,wl,wl,wl,--,wl,--,fd,ed]).
moo:type_grid(maze,19,[ed,pt,fd,--,fd,--,fd,--,fd,--,wl,--,--,--,--,--,--,--,fd,ed]).
moo:type_grid(maze,20,[ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed]).


