
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Copyright 2007 Pablo Kogan, Guillermo Torres, Mario Moya		    %
    % 									    %
    % This file is part of Rakiduam.					    %
    % 									    %
    % Rakiduam is free software; you can redistribute it and/or modify	    %
    % it under the terms of the GNU General Public License as published by  %
    % the Free Software Foundation; either version 3 of the License, or	    %
    % (at your option) any later version.				    %
    % 									    %
    % Rakiduam is distributed in the hope that it will be useful,	    %
    % but WITHOUT ANY WARRANTY; without even the implied warranty of	    %
    % MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the	    %
    % GNU General Public License for more details.			    %
    % 									    %
    % You should have received a copy of the GNU General Public License	    %
    % along with this program.  If not, see <http://www.gnu.org/licenses/>. %
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module(configuration,[get_anchoArea/1,get_altoArea/1,get_anchoAreaChica/1,get_altoAreaChica/1,get_arco_alto/4,get_arco_bajo/4,get_field/4,get_environment/1,get_video_host/1,get_video_port/1,get_command_host/1,get_command_port/1,get_numplayers/1,get_player/3,get_role/2,players_names/1, get_ball_id/1,get_ball_name/1,get_player_id/1],[assertions]).

:- use_package(xml_path).
:- use_package(pillow).
:- use_module(library(file_utils)).

:- initialization(start).

:- comment(title, "Configuration module ").

:- comment(author, "Mario Moya").

:- comment(module, "Esté módulo lee los datos parametrizables de un
	archivo externo XML, e inserta en la base de conocimientos los
	predicados con sus valores, que luego serán consultados por el
	resto de los módulos").


xml_file('config.xml').



fetch_xml(Terms) :-
	xml_file(Xml),
	file_to_string(Xml,Content),
	xml2terms(Content,Terms).

get_element_number(Terms,Clave,Valor):-
	member(env(config,[],L),Terms),
	member(env(Clave,[],[ValorS]),L),
	number_codes(Valor,ValorS).


get_element(Terms,Clave,Valor):-
%	Query = config::(Clave)::(Valor),
%	xml_query(Query, Term).
	member(env(config,[],L),Terms),
	member(env(Clave,[],[ValorS]),L),
	atom_codes(Valor,ValorS).	

get_all_roles(Terms,Nombre,Role):-
	Query = config::players::player@(val(role,RoleStr))::(NombreS),
	xml_query(Query, Terms),
 	atom_codes(Nombre,NombreS),
 	atom_codes(Role,RoleStr).


get_player_data(Terms,Nombre,Jug,Equipo):-
	Query = config::players::player@(val(number,JugS),val(team,EquipoS))::(NombreS),
	xml_query(Query, Terms),
 	atom_codes(Nombre,NombreS),
 	number_codes(Jug,JugS),
 	atom_codes(Equipo,EquipoS).

%obtiene los nombres de los jugadores propios
get_players_names(Terms,Nombre):-
	Query = config::players::player@(val(team,"propio"))::(NombreS),
	xml_query(Query, Terms),
 	atom_codes(Nombre,NombreS).


get_host_and_port(Terms,Element,Hostname,Port):-
	Query = config::(Element)@(val(hostname,HostnameS),val(port,PortS)),
	xml_query(Query, Terms),
 	atom_codes(Hostname,HostnameS),
 	number_codes(Port,PortS).


get_coord(Terms,Elem,X1,Y1,X2,Y2):-
	Query = config::(Elem)@(val('X1',X1s),val('X2',X2s),val('Y1',Y1s),val('Y2',Y2s)),
	xml_query(Query, Terms),
	number_codes(X1,X1s),	
	number_codes(X2,X2s),	
	number_codes(Y1,Y1s),	
	number_codes(Y2,Y2s).

get_players_id(Terms,Id) :-
	Query = config::players@(val(doraemon_id,IdS)),
	xml_query(Query, Terms),
	number_codes(Id,IdS).
	
get_ball_id_and_name(Terms,Id,Name) :-
	Query = config::ball@(val(doraemon_id,IdS))::(NameS),
	xml_query(Query, Terms),
	number_codes(Id,IdS),	
 	atom_codes(Name,NameS).	

start:-
	fetch_xml(Terms),
	get_element_number(Terms,anchoArea,VanchoArea),
	asserta_fact(get_anchoArea(VanchoArea)),
	get_element_number(Terms,altoArea,ValtoArea),
	asserta_fact(get_altoArea(ValtoArea)),
	get_element_number(Terms,anchoAreaChica,VanchoAreaChica),
	asserta_fact(get_anchoAreaChica(VanchoAreaChica)),
	get_element_number(Terms,altoAreaChica,ValtoAreaChica),
	asserta_fact(get_altoAreaChica(ValtoAreaChica)),
	get_coord(Terms,arco_alto,X1aa,Y1aa,X2aa,Y2aa),
	asserta_fact(get_arco_alto(X1aa,Y1aa,X2aa,Y2aa)),
	get_coord(Terms,arco_bajo,X1ab,Y1ab,X2ab,Y2ab),
	asserta_fact(get_arco_bajo(X1ab,Y1ab,X2ab,Y2ab)),
	get_coord(Terms,field,X1f,Y1f,X2f,Y2f),
	asserta_fact(get_field(X1f,Y1f,X2f,Y2f)),
	get_element(Terms,environment,Venvironment),
	asserta_fact(get_environment(Venvironment)),
	get_host_and_port(Terms,video_server,VideoHost,VideoPort),
	asserta_fact(get_video_host(VideoHost)),
	asserta_fact(get_video_port(VideoPort)),
	get_host_and_port(Terms,command_server,CmmdHost,CmmdPort),
	asserta_fact(get_command_host(CmmdHost)),
	asserta_fact(get_command_port(CmmdPort)),
	get_element_number(Terms,numplayers,NumP),
	asserta_fact(get_numplayers(NumP)),
        assert_players(Terms),
	assert_players_names,
	assert_roles(Terms),
	get_players_id(Terms,PlyId),
	asserta_fact(get_player_id(PlyId)),
	get_ball_id_and_name(Terms,BallId,BallName),
	asserta_fact(get_ball_id(BallId)),
	asserta_fact(get_ball_name(BallName)).


players_names([]). 

assert_players(Terms):-
	get_player_data(Terms,Nombre,Jug,Equipo),
	asserta_fact(get_player(Nombre,Jug,Equipo)),
% 	asserta_fact(players_names([Plyrs|Nombre])),
	fail.

assert_players(_).


assert_players_names:-
	get_numplayers(N),
	opt_assert_players_names(N,[]).

opt_assert_players_names(0,L):-
 	set_fact(players_names(L)).
opt_assert_players_names(N,L):-
	get_player(Nombre,N,propio),
	N1 is N - 1,
	opt_assert_players_names(N1,[Nombre|L]).	

assert_roles(Terms):-
	get_all_roles(Terms,Nombre,Role),
	asserta_fact(get_role(Nombre,Role)),
	fail.

assert_roles(_).



	

:- data [
	get_anchoArea/1,
	get_altoArea/1,
	get_anchoAreaChica/1,
	get_altoAreaChica/1,
	get_arco_alto/4,
	get_arco_bajo/4,
	get_field/4,
	get_environment/1,
	get_video_host/1,
	get_video_port/1,
	get_command_host/1,
	get_command_port/1,
	get_numplayers/1,
	get_player/3,
	get_role/2,
	players_names/1,
	get_ball_id/1,
	get_ball_name/1,
	get_player_id/1
	].


