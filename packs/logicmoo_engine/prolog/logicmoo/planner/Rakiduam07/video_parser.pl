:- module(video_parser,[analyze/4],[dcg]).

:- use_module(library(lists)).
:- use_module(library(file_utils)).
:- use_module(doraemon_tokenizer).
:- use_module(configuration).
:- use_module(ambiente,[pelota_anterior/3,jugador_prev/1]).
:- use_module(trigonometry,[ajuste_angulo/2,radians_to_degrees/2]).


% main([File],Pelota,Jugadores):-
% 	file_to_string(File,S),
% 	analize(S,L),
% 	member(robot(pelota,0,pos(XP,YP,ZP,_)),L),
% 	delete_non_ground(L,robot(pelota,0,pos(XP,YP,ZP,_)),Jugadores),
% 	Pelota = pos(XP,YP,ZP).
% %	close(Stream).

% Pelota = pos(X,Y,Z).
% Jugadores = [robot(propio | contrario,NumeroJugador,pos(X,Y,Z,R)),...]



analyze(S,Pelota,Jugadores,Estado):-
	get_environment(real),
	analize(S,L),
	Estado = estado(0,0),
	member(ball(Ball,pos(XP,YP,ZP)),L),
	delete_non_ground(L,ball(Ball,pos(XP,YP,ZP)),Jugadores),
	Pelota = pos(XP,YP,ZP).

analyze(S,Pelota,Jugadores,Estado):-
	get_environment(simulado),
	posxyz(S,Pelota,MensajeR),
	jugadores(MensajeR,Jugadores,MensajeP),
	estado(MensajeP,Estado,_MensajeN),!.



analize(S,L):-
	salida(L,S,_E).
%	estructurar(E,
%	display(E).
	

salida(Estado) --> init(Estado).

init(Estado) --> 
		%nextLine(pos(1,1),I1),
	           firstline(N,pos(1,1),I1),
		   nextLine(I1,I2),
		   parseLines(N,I2,_O,Estado).
%		   parseLine(I2,_O,Estado).

firstline(N,I,O) --> nextToken(I,I1,Number), 
	nextToken(I1,I2,_), 
	nextToken(I2,O,_), 
	{number_codes(N,Number)}.

parseLines(1,I,O,[Estado]) --> parseLine(I,O,Estado).
parseLines(N,I,O,[Estado|Rest]) --> 
	{ N1 is N - 1 }, 
	  parseLine(I,I1,Estado) , 
	  parseLines(N1,I1,O,Rest).

parseLine(I,O,robot(Equipo,Jug,pos(X,Y,Z,Orientation))) --> 
	                  nextToken(I,I1,ObjectTypeS), 
			  {number_codes(ObjectType,ObjectTypeS),
			  get_player_id(ObjectType)},
			  nextToken(I1,I2,ObjectNameS),
			         {atom_codes(ObjectName,ObjectNameS),
				 get_player(ObjectName,Jug,Equipo)},
			  nextToken(I2,I3,Found),
			  nextToken(I3,I4,Xs),
			  nextToken(I4,I5,Ys),
			  nextToken(I5,I6,Zs),
			  nextToken(I6,I7,OrientationS),
			  nextToken(I7,I8,_VelocityX),
			  nextToken(I8,O,_VelocityY),
			  {
			      (
				  Found=="Found" ->
				  number_codes(X,Xs),
				  number_codes(Y,Ys),
				  number_codes(Z,Zs),
				  number_codes(OrientRad,OrientationS),
				  radians_to_degrees(OrientRad,OrientDgr),
				  ajuste_angulo(OrientDgr,Orientation)
			      ;
				  jugador_prev(robot(Equipo,Jug,pos(X,Y,Z,Orientation)))
			      )
				  
			   }.
			  

parseLine(I,O,ball(ObjectName,pos(X,Y,Z))) --> 
	                  nextToken(I,I1,ObjectTypeS), 
			  {number_codes(ObjectType,ObjectTypeS),get_ball_id(ObjectType)},
			  nextToken(I1,I2,ObjectNameS),
			         {atom_codes(ObjectName,ObjectNameS),
				 get_ball_name(ObjectName)},
			  nextToken(I2,I3,Found),
			  nextToken(I3,I4,Xs),
			  nextToken(I4,I5,Ys),
			  nextToken(I5,I6,Zs),
			  nextToken(I6,I7,_Orientation),
			  nextToken(I7,I8,_VelocityX),
			  nextToken(I8,O,_VelocityY),
			  {
			      (
				  Found=="Found" ->
				  number_codes(X,Xs),
				  number_codes(Y,Ys),
				  number_codes(Z,Zs)
			      ;
				  pelota_anterior(X,Y,Z)
			      )
			  }.



%funciones de parser in (simulado)
next_line([],[],[]).
next_line([32|C],[],C).
next_line([10|C],[],C).
next_line([X|C],[X|CL],CR):-
	next_line(C,CL,CR).

%Resuelve el problema de números negativos
numero_string(X,[45|R]):-
	number_codes(X1,R),
	X is -X1.
numero_string(X,R):-
	number_codes(X,R).
next_number([],0,[]).
next_number(Mensaje,X,MensajeS):-
	next_line(Mensaje,StrX,MensajeS),
	%display(StrX),write_string(StrX),nl,%hasta StrXesta bien
	numero_string(X,StrX), %error en la función number_codes para números negativos
	%display(X),
	!.
posxyzr(Stream,pos(X,Y,Z,R),StreamQ):-
	next_number(Stream, X ,StreamY),
        next_number(StreamY, Y,StreamZ),
        next_number(StreamZ, Z,StreamR),
        next_number(StreamR, R,StreamQ).
equipos(Stream,'contrario',[robot('contrario',5,X)],5,StreamS):-
	posxyzr(Stream,X,StreamS).
equipos(Stream,'propio',[robot('propio',5,X)|L],5,StreamS):-
	posxyzr(Stream,X,StreamS1),
	equipos(StreamS1,'contrario',L,1,StreamS). 
equipos(Stream,Equipo,[robot(Equipo,N,X)|L],N,StreamS):-
	posxyzr(Stream,X,StreamS1),
	N2 is N+1,
	equipos(StreamS1,Equipo,L,N2,StreamS).
 jugadores(Stream,ListaJugadores,StreamS):-
	equipos(Stream,'propio',ListaJugadores,1,StreamS).

posxyz(Stream,pos(X,Y,Z),StreamR):-
	next_number(Stream, X,StreamY),
        next_number(StreamY, Y,StreamZ),
        next_number(StreamZ, Z,StreamR).
write_pos(pos(X,Y,Z)):-
	display('('),display(X),display(','),display(Y),display(','),display(Z),display(')').
write_jugadores([]):-nl.
write_jugadores([robot(E,N,pos(X,Y,Z,R))|C]):-
	display('Robot '),display(E),display(' '),display(N), display('('),display(X),display(','),display(Y),display(','),display(Z),display(','),display(R),display(')'),
	write_jugadores(C).
estado(Stream,estado(Estado,TienePelota),StreamQ):-
	next_number(Stream, Estado ,StreamY),
        next_number(StreamY, TienePelota,StreamQ).
write_estado(estado(1,Y)):-
	display('FREE_BALL '),
	write_tienepelota(Y).
write_estado(estado(2,Y)):-
	display('PLACE_KICK '),
	write_tienepelota(Y).
write_estado(estado(3,Y)):-
	display('PENALTY_KICK '),
	write_tienepelota(Y).
write_estado(estado(4,Y)):-
	display('FREE_KICK '),
	write_tienepelota(Y).
write_estado(estado(5,Y)):-
	display('GOAL_KICK '),
	write_tienepelota(Y).
write_estado(estado(_,Y)):-
	display('INDEFINIDO '),
	write_tienepelota(Y).
write_tienepelota(0):-
	display('ANYONES_BALL').
write_tienepelota(1):-
	display('BLUE_BALL').
write_tienepelota(2):-
	display('YELLOW_BALL').
write_tienepelota(_):-
	display('INDEFINIDO').

%// gameState
%const long FREE_BALL = 1;
%const long PLACE_KICK = 2;
%const long PENALTY_KICK = 3;
%const long FREE_KICK = 4;
%const long GOAL_KICK = 5;
%
%// whosBall
%const long ANYONES_BALL = 0;
%const long BLUE_BALL = 1;
%const long YELLOW_BALL = 2;

% Doraemon's Output

% 7 6188 0.000290976
% ; 7=#objects specified in config file. 
% ; 6188=frame #.  
% ; rest - time diff in sec between prev frame and this one
% 1605.82 -708.394 1321.44
% ; x, y, z coordinates of camera wrt the coordinate system you calibrated
% ; you need these for 3d settings, but not for 2d worlds)
% ; what follows now are a set of recognized objects for each item in the 
% ; configuration file

% 2 spot1 Found 1232.5 416.374 0 0 0 0
% ; first digit - 0=car; 1=ball; 2=spot
% ; then id; then Found/NoFnd, then X/Y/Z within coordinate system
% ; (remember you laid down the 0,0 point when you set the rotation matrix)
% ; next item is orientation in rads from the 0,0 coordinate of rotation matrix
% ; last 2 are velocity in x and y dimensions, respectively, in mm/sec
% 2 spot2 Found 1559.22 417.359 0 0 0 0
% 2 spot3 Found 1260.55 812.189 0 0 0 0
% 2 spot4 Found 902.726 1002.43 0 0 0 0
% 2 spot5 Found 746.045 735.631 0 0 0 0
% 1 ball1 Found 1677.99 1205.55 50 0 -2.75769 1.19908
% 0 car54 Found 1783.53 873.531 100 2.63944 1.47684 -6.49056


