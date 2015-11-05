/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_filestreams.pl
:- module(logicmoo_util_filestreams,
          [ copy_stream/2,
            file_to_stream/2,
            file_to_stream_ssl_verify/5,
            l_open_input/2,
            l_open_input0/2,
            l_open_input1/2,
      %      make_socket/3,
      %      negotiate_http_connect/2,
       %     ssl_failed/3,
       %     ssl_protocol_hook/4,
            text_to_stream/2,
            with_stream_pos/2
          ]).
:- multifile
        accept_hook/2,
        make_socket_hook/3,
        open_client_hook/5,
        open_options/2,
        package_path/2.
:- meta_predicate
        with_stream_pos(+, 0).
:- module_transparent
        copy_stream/2,
        file_to_stream/2,
        file_to_stream_ssl_verify/5,
        l_open_input/2,
        l_open_input0/2,
        l_open_input1/2,
     %   make_socket/3,
     %   negotiate_http_connect/2,
        package_path/2,
     %   ssl_failed/3,
     %   ssl_protocol_hook/4,
        text_to_stream/2.


:- include('logicmoo_util_header.pi').

:- export(with_stream_pos/2).
% = :- meta_predicate(with_stream_pos(+,0)).

% 	 	 
%% with_stream_pos( +In, :GoalCall) is semidet.
%
% Using Stream Pos.
%
with_stream_pos(In,Call):-
    stream_property(In, position(InitalPos)),
    PS = position(InitalPos),
    (Call *-> 
       (stream_property(In, position(NewPos)),nb_setarg(1,PS,NewPos)) ; 
       ((arg(1,PS,Pos),set_stream_position(In, Pos)),!,fail)).


:- export(l_open_input/2).
:- export(l_open_input0/2).
:- export(l_open_input1/2).

% 	 	 
%% l_open_input( ?InS, ?In) is semidet.
%
% (list Version) Open Input.
%
l_open_input(InS,In):-once(must(l_open_input0(InS,In))).


% 	 	 
%% l_open_input0( ?In, ?InS) is semidet.
%
% (list Version) Open Input Primary Helper.
%
l_open_input0(In,InS):-l_open_input1(In,InS),!.
l_open_input0(InS,In):-string(InS),!,open_string(InS,In).
l_open_input0(Filename,In) :- \+ is_list(Filename),nonvar(Filename),filematch(Filename,File), catch(see(File),_,fail),current_input(In).
l_open_input0(InS,In):-!,open_string(InS,In).


% 	 	 
%% l_open_input1( :TermInS, ?In) is semidet.
%
% (list Version) Open Input Secondary Helper.
%
l_open_input1([V|_],_):-var(V),V=zzzzzzzzzzzzz,!,throw(error(l_open_input/2,'Arguments are not sufficiently instantiated (l_open_input)')).
l_open_input1(InS,In):-is_stream(InS),!,In=InS.
l_open_input1(file(Filename),In) :- filematch(Filename,File), catch(see(File),_,fail),current_input(In).
l_open_input1(alias(Filename),In) :-  catch(see(Filename),_,fail),current_input(In).
l_open_input1(string(string(InS)),In):-!,dmsg_text_to_string_safe(InS,Str),string_codes(Str,Codes),open_chars_stream(Codes,In).
l_open_input1(string(InS),In):-!,open_string(InS,In).
l_open_input1(atom(InS),In):-!,open_string(InS,In).
l_open_input1(codes(InS),In):-!,open_string(InS,In).
l_open_input1(chars(InS),In):-!,open_string(InS,In).


:- use_module(library(url)).

/*
:- use_module(library(http/http_ssl_plugin)).
*/

% :- module(http_ssl_plugin, []).
:- use_module(library(ssl),[]).


:- use_module(library(socket),[]).
:- use_module(library(debug),[]).
:- use_module(library(option),[]).
:- use_module(library(http/thread_httpd),[]).
:- use_module(library(http/http_header)).

/* Part of LogicMOO Base SSL plugin for HTTP libraries

This  module  can  be   loaded    next   to   library(thread_httpd)  and
library(http_open) to provide secure HTTP   (HTTPS)  services and client
access.

An example secure server using self-signed  certificates can be found in
the <plbase>/doc/packages/examples/ssl/https.pl, where <plbase>   is the
SWI-Prolog installation directory.
*/

:- multifile
	thread_httpd:make_socket_hook/3,
	thread_httpd:accept_hook/2,
	thread_httpd:open_client_hook/5,
        http:http_protocol_hook/5,
	http:open_options/2,
	http:http_connection_over_proxy/6.


		 /*******************************
		 *	    SERVER HOOKS	*
		 *******************************/

%%	thread_httpd:make_socket_hook(?Port, :OptionsIn, -OptionsOut)
%%								is semidet.
%
%	Hook into http_server/2 to create an   SSL  server if the option
%	ssl(SSLOptions) is provided.
%
%	@see thread_httpd:accept_hook/2 handles the corresponding accept


% 	 	 
%% thread_httpd:make_socket_hook( ?VALUE1, :Term_G21452, ?VALUE3) is semidet.
%
% Hook To [thread_httpd:make_socket_hook/3] For Module Logicmoo_util_filestreams.
% Make Socket Hook.
%
thread_httpd:make_socket_hook(Port, M:Options0, Options) :-
	memberchk(ssl(SSLOptions), Options0), !,
	make_socket(Port, Socket, Options0),
	ssl_context(server,
                    SSL,
                    M:[ port(Port),
                        close_parent(true)
                      | SSLOptions
                      ]),
	atom_concat('httpsd', Port, Queue),
	Options = [ queue(Queue),
                    tcp_socket(Socket),
		    ssl_instance(SSL)
		  | Options0
		  ].


% 	 	 
%% thread_httpd:make_socket( ?Port, ?Options0, ?Options) is semidet.
%
% Hook To [thread_httpd:make_socket/3] For Module Logicmoo_util_filestreams.
% Make Socket.
%
make_socket(_Port, Socket, Options) :-
	option(tcp_socket(Socket), Options), !.
make_socket(Port, Socket, _Options) :-
	tcp_socket(Socket),
	tcp_setopt(Socket, reuseaddr),
	tcp_bind(Socket, Port),
	tcp_listen(Socket, 5).


%%	thread_httpd:accept_hook(:Goal, +Options) is semidet.
%
%	Implement the accept for HTTPS connections.


% 	 	 
%% thread_httpd:accept_hook( ?Goal, ?Options) is semidet.
%
% Hook To [thread_httpd:accept_hook/2] For Module Logicmoo_util_filestreams.
% Accept Hook.
%
thread_httpd:accept_hook(Goal, Options) :-
	memberchk(ssl_instance(SSL), Options), !,
	memberchk(queue(Queue), Options),
        memberchk(tcp_socket(Socket), Options),
        tcp_accept(Socket, Client, Peer),
	debug(http(connection), 'New HTTPS connection from ~p', [Peer]),
	http_enough_workers(Queue, accept, Peer),
	thread_send_message(Queue, ssl_client(SSL, Client, Goal, Peer)).


% 	 	 
%% thread_httpd:open_client_hook( :Term_G13743, ?VALUE2, ?VALUE3, ?VALUE4, ?VALUE5) is semidet.
%
% Hook To [thread_httpd:open_client_hook/5] For Module Logicmoo_util_filestreams.
% Open Client Hook.
%
thread_httpd:open_client_hook(ssl_client(SSL, Client, Goal, Peer),
			      Goal, In, Out,
			      [peer(Peer), protocol(https)]) :-
        tcp_open_socket(Client, Read, Write),
	catch(ssl_negotiate(SSL, Read, Write, In, Out),
	      E,
	      ssl_failed(Read, Write, E)).


% 	 	 
%% ssl_failed( ?Read, ?Write, ?E) is semidet.
%
% Ssl Failed.
%
ssl_failed(Read, Write, E) :-
	close(Write, [force(true)]),
	close(Read,  [force(true)]),
	throw(E).


		 /*******************************
		 *	   CLIENT HOOKS		*
		 *******************************/

%%	http:http_protocol_hook(+Scheme, +Parts, +PlainStreamPair,
%%				-StreamPair, +Options) is semidet.
%
%	Hook for http_open/3 to connect  to   an  HTTPS (SSL-based HTTP)
%	server.   This   plugin   also   passes   the   default   option
%	`cacert_file(system(root_certificates))` to ssl_context/3.


% 	 	 
%% http:http_protocol_hook( ?VALUE1, ?VALUE2, ?VALUE3, ?VALUE4, ?VALUE5) is semidet.
%
% Hook To [http:http_protocol_hook/5] For Module Logicmoo_util_filestreams.
% Http Protocol Hook.
%
http:http_protocol_hook(https, Parts, PlainStreamPair, StreamPair, Options):-
	ssl_protocol_hook(Parts, PlainStreamPair, StreamPair, Options).


% 	 	 
%% ssl_protocol_hook( ?Parts, ?PlainStreamPair, ?StreamPair, ?Options) is semidet.
%
% Ssl Protocol Hook.
%
ssl_protocol_hook(Parts, PlainStreamPair, StreamPair, Options) :-
        memberchk(host(Host), Parts),
        option(port(Port), Parts, 443),
	ssl_context(client, SSL, [ host(Host),
                                   port(Port),
                                   close_parent(true)
				 | Options
				 ]),
        stream_pair(PlainStreamPair, PlainIn, PlainOut),
        catch(ssl_negotiate(SSL, PlainIn, PlainOut, In, Out),
              Exception,
              ( ssl_exit(SSL), throw(Exception)) ),
        stream_pair(StreamPair, In, Out).

%%	http:open_options(Parts, Options) is nondet.
%
%	Implementation of the multifile hook http:open_options/2 used by
%	library(http/http_open). By default, we use   the system trusted
%	root certificate database for validating an SSL certificate.


% 	 	 
%% http:open_options( ?Parts, ?Options) is semidet.
%
% Hook To [http:open_options/2] For Module Logicmoo_util_filestreams.
% Open Options.
%
http:open_options(Parts, Options) :-
	memberchk(scheme(https), Parts),
	Options = [cacert_file(system(root_certificates))].

%%	http:http_connection_over_proxy(+Proxy, +Parts, +HostPort, -StreamPair,
%%					+OptionsIn, -OptionsOut)
%
%	Facilitate an HTTPS connection via a   proxy using HTTP CONNECT.
%	Note that most proxies will only  support this for connecting on
%	port 443


% 	 	 
%% http:http_connection_over_proxy( :Term_G20607, ?VALUE2, :Term_G21243, ?VALUE4, ?VALUE5, ?VALUE6) is semidet.
%
% Hook To [http:http_connection_over_proxy/6] For Module Logicmoo_util_filestreams.
% Http Connection Over Proxy.
%
http:http_connection_over_proxy(proxy(ProxyHost, ProxyPort), Parts,
				Host:Port, StreamPair, Options, Options) :-
        memberchk(scheme(https), Parts), !,
        tcp_connect(ProxyHost:ProxyPort, StreamPair, [bypass_proxy(true)]),
        catch(negotiate_http_connect(StreamPair, Host:Port),
              Error,
              ( close(StreamPair, [force(true)]),
                throw(Error)
              )).

:- use_module(library(http/http_open),[]).


% 	 	 
%% negotiate_http_connect( ?StreamPair, ?Address) is semidet.
%
% Negotiate Http Connect.
%
negotiate_http_connect(StreamPair, Address):-
        format(StreamPair, 'CONNECT ~w HTTP/1.1\r\n\r\n', [Address]),
        flush_output(StreamPair),
        http_read_reply_header(StreamPair, Header),
        memberchk(status(_, Status, Message), Header),
        (   Status == ok
	->  true
        ;   throw(error(proxy_rejection(Message), _))
        ).


:- multifile(package_path/2).

% 	 	 
%% package_path( ?Pkg, ?PkgPath) is semidet.
%
% Package Path.
%
package_path(Pkg,PkgPath):-expand_file_search_path(pack(Pkg),PkgPathN),exists_directory(PkgPathN),normalize_path(PkgPathN,PkgPath).
package_path(Pkg,PkgPath):-atom(Pkg),T=..[Pkg,'.'],expand_file_search_path(T,PkgPathN),exists_directory(PkgPathN),normalize_path(PkgPathN,PkgPath).


% 	 	 
%% file_to_stream_ssl_verify( ?SSL, ?ProblemCert, ?AllCerts, ?FirstCert, ?Error) is semidet.
%
% File Converted To Stream Ssl Verify.
%
file_to_stream_ssl_verify(_SSL, _ProblemCert, _AllCerts, _FirstCert, _Error) :- !.
:- export(text_to_stream/2).

% 	 	 
%% text_to_stream( ?Text, ?Stream) is semidet.
%
% Text Converted To Stream.
%
text_to_stream(Text,Stream):-text_to_string(Text,String),string_codes(String,Codes),open_codes_stream(Codes,Stream).
:- export(file_to_stream/2).

% 	 	 
%% file_to_stream( :TermStreamIn, ?Stream) is semidet.
%
% File Converted To Stream.
%
file_to_stream((StreamIn),Stream):-is_stream(StreamIn),!,copy_stream(StreamIn,Stream).
file_to_stream(stream(StreamIn),Stream):-copy_stream(StreamIn,Stream).
file_to_stream('$socket'(Sock),Stream):-tcp_open_socket('$socket'(Sock),StreamIn),copy_stream(StreamIn,Stream).
file_to_stream(ftTerm(Text),Stream):-term_to_string(Text,String),string_codes(String,Codes),open_codes_stream(Codes,Stream).
file_to_stream(text(Text),Stream):-text_to_stream(Text,Stream).
file_to_stream(codes(Text),Stream):-text_to_stream(Text,Stream).
file_to_stream(chars(Text),Stream):-text_to_stream(Text,Stream).
file_to_stream(atom(Text),Stream):-text_to_stream(Text,Stream).
file_to_stream(string(Text),Stream):-text_to_stream(Text,Stream).
file_to_stream(file(Spec),Stream):-file_to_stream(Spec,Stream).
file_to_stream(exfile(File),Stream):-!,read_file_to_codes(File,Codes,[expand(true)]),open_codes_stream(Codes,Stream).
file_to_stream(match(Spec),Stream):-!,filematch(Spec,File),exists_file(File),!,file_to_stream(exfile(File),Stream).
file_to_stream(package(Pkg,LocalPath),Stream) :-!,
   package_path(Pkg,PkgPath),
   % build global path
   atomic_list_concat([PkgPath|LocalPath], '/',  GlobalPath),file_to_stream(GlobalPath,Stream).
file_to_stream(Spec,Stream):-compound(Spec),!,file_to_stream(match(Spec),Stream).
file_to_stream(URL,Stream):-sub_string(URL,_,_,_,":/"),sub_string(URL,0,4,_,'http'), !, if_defined_else(http_open:http_open(URL,HTTP_Stream,[ cert_verify_hook(file_to_stream_ssl_verify)]),fail),copy_stream(HTTP_Stream,Stream),!.
file_to_stream(URL,Stream):-atom_concat('file://', File, URL),!,file_to_stream(File,Stream).
file_to_stream(URL,Stream):-atom_concat('file:', File, URL),!,file_to_stream(File,Stream).
file_to_stream(URL,Stream):-on_x_fail(atomic_list_concat(['package://',Pkg,'/', Path], URL)),file_to_stream(package(Pkg,Path),Stream).
file_to_stream(URL,Stream):-on_x_fail(atomic_list_concat([Pkg,'://',Path],URL)),file_to_stream(package(Pkg,Path),Stream).
file_to_stream(Spec,Stream):-file_to_stream(match(Spec),Stream).

:- export(copy_stream/2).

% 	 	 
%% copy_stream( ?HTTP_Stream, ?Stream) is semidet.
%
% Copy Stream.
%
copy_stream(HTTP_Stream,Stream):-read_stream_to_codes(HTTP_Stream,Codes),catch(close(HTTP_Stream),_,true),open_codes_stream(Codes,Stream).


