
:- export(with_stream_pos/2).
:- meta_predicate(with_stream_pos(+,0)).
with_stream_pos(In,Call):-
    stream_property(In, position(InitalPos)),
    PS = position(InitalPos),
    (Call *-> 
       (stream_property(In, position(NewPos)),nb_setarg(1,PS,NewPos)) ; 
       ((arg(1,PS,Pos),set_stream_position(In, Pos)),!,fail)).


:-export(l_open_input/2).
:-export(l_open_input0/2).
:-export(l_open_input1/2).
l_open_input(InS,In):-once(must(l_open_input0(InS,In))).

l_open_input0(In,InS):-l_open_input1(In,InS),!.
l_open_input0(InS,In):-string(InS),!,open_string(InS,In).
l_open_input0(Filename,In) :- \+ is_list(Filename),nonvar(Filename),filematch(Filename,File), catch(see(File),_,fail),current_input(In).
l_open_input0(InS,In):-!,open_string(InS,In).

l_open_input1([V|_],_):-var(V),V=zzzzzzzzzzzzz,!,throw(error(l_open_input/2,'Arguments are not sufficiently instantiated (l_open_input)')).
l_open_input1(InS,In):-is_stream(InS),!,In=InS.
l_open_input1(file(Filename),In) :- filematch(Filename,File), catch(see(File),_,fail),current_input(In).
l_open_input1(alias(Filename),In) :-  catch(see(Filename),_,fail),current_input(In).
l_open_input1(string(string(InS)),In):-!,text_to_string_safe(InS,Str),string_codes(Str,Codes),open_chars_stream(Codes,In).
l_open_input1(string(InS),In):-!,open_string(InS,In).
l_open_input1(atom(InS),In):-!,open_string(InS,In).
l_open_input1(codes(InS),In):-!,open_string(InS,In).
l_open_input1(chars(InS),In):-!,open_string(InS,In).



:- use_module(library(url)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).

file_to_stream_ssl_verify(_SSL, _ProblemCert, _AllCerts, _FirstCert, _Error) :- !.
:- export(text_to_stream/2).
text_to_stream(Text,Stream):-text_to_string(Text,String),string_codes(String,Codes),open_codes_stream(Codes,Stream).
:- export(file_to_stream/2).
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
   user:package_path(Pkg,PkgPath),
   % build global path
   atomic_list_concat([PkgPath|LocalPath], '/',  GlobalPath),file_to_stream(GlobalPath,Stream).
file_to_stream(Spec,Stream):-compound(Spec),!,file_to_stream(match(Spec),Stream).
file_to_stream(URL,Stream):-atom_contains(URL,":/"),sub_string(URL,0,4,_,'http'), !, http_open(URL,HTTP_Stream,[ cert_verify_hook(file_to_stream_ssl_verify)]),copy_stream(HTTP_Stream,Stream),!.
file_to_stream(URL,Stream):-atom_concat('file://', File, URL),!,file_to_stream(File,Stream).
file_to_stream(URL,Stream):-atom_concat('file:', File, URL),!,file_to_stream(File,Stream).
file_to_stream(URL,Stream):-atomic_list_concat_safe(['package://',Pkg,'/', Path], URL),file_to_stream(package(Pkg,Path),Stream).
file_to_stream(URL,Stream):-atomic_list_concat_safe([Pkg,'://',Path],URL),file_to_stream(package(Pkg,Path),Stream).
file_to_stream(Spec,Stream):-file_to_stream(match(Spec),Stream).

:- export(copy_stream/2).
copy_stream(HTTP_Stream,Stream):-read_stream_to_codes(HTTP_Stream,Codes),catch(close(HTTP_Stream),_,true),open_codes_stream(Codes,Stream).


