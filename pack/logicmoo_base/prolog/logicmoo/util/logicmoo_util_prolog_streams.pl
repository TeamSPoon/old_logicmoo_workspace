% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_prolog_streams.pl

:- module(logicmoo_util_prolog_streams,
          [ with_err_to_pred/2,
            with_input_from_pred/2,
            with_output_to_pred/2,
            with_output_to_stream_pred/4
          ]).
:- meta_predicate
        with_err_to_pred(1, 0),
        with_input_from_pred(1, 0),
        with_output_to_pred(1, 0),
        on_x_fail_priv(0),
        with_output_to_stream_pred(1, -, 0, 0).
:- module_transparent
        buffer_chars/1,
        read_received/1,
        some_test/0,
        test1_0/1,
        test2/1.


on_x_fail_priv(Goal):- catch(Goal,_,fail).

:- use_module(library(prolog_stream)).

% Writeq/1s a term the user_error and flushes
%dmsg(M):-!.
%dmsg(M):-format(user_error,'~N~n% dmsg: ~q.~n',[M]),flush_output(user_error).

:- discontiguous some_test/0.

:- multifile(lmconf:is_prolog_stream/1).
:- dynamic(lmconf:is_prolog_stream/1).
:- export(lmconf:is_prolog_stream/1).

%=	open_prolog_stream(+Module, +Mode, -Stream, +Options)
%
%	Create  a  new  stream  that  implements   its  I/O  by  calling
%	predicates in Module.  The called predicates are:
%
%	The current implementation only  deals   with  text streams. The
%	stream uses the =wchar_t= encoding. The   buffer  size must be a
%	multiple of =wchar_t=, i.e., a multiple of four for portability.
%	The _newline_ mode of the stream   is  =posix= on all platforms,
%	disabling the translation `"\n" --> "\r\n"`.
%
%	@arg Options is currently ignored.
%	@bug	Futher versions might require additional callbacks.  As we
%		demand all callbacks to be defined, existing code needs
%		to implement the new callbacks.


%	  - Module:stream_close(+Stream)
%	  Called when the stream is closed.  This predicate must
%	  succeed.  The callback can be used to cleanup associated
%	  resources.

:- thread_local(tl_with_prolog_streams:stream_close/1).
:- meta_predicate(tl_with_prolog_streams:stream_close(-)).

%	  - Module:stream_write(+Stream, +String)
%	  Called for a `Mode = write` stream if data is available.
%	  String contains the (textual) data that is written
%	  to Stream.  The callback is called if the buffer of
%	  Stream overflows, the user calls flush_output(Stream)
%	  or Stream is closed and there is buffered data.

:- thread_local(tl_with_prolog_streams:stream_write/2).
:- meta_predicate(tl_with_prolog_streams:stream_write(?,?)).

:- meta_predicate(with_output_to_pred(1,0)).
with_output_to_pred(Callback,Goal):-
  current_output(Prev),
    with_output_to_stream_pred(Callback,Stream,
      (set_stream(Stream,  alias(user_output)),
       set_stream(Stream,  alias(current_output)),
            with_output_to(Stream,Goal)),
       (set_stream(Prev,  alias(user_output)),
       set_stream(Prev,  alias(current_output)))).
 
with_err_to_pred(Callback,Goal):-
  thread_current_error_stream(Err),
    with_output_to_stream_pred(Callback,Stream,
      (set_stream(Stream, alias(user_error)), Goal),
       set_stream(Err, alias(user_error))).

some_test :- dynamic(received_chars/1).


with_output_to_stream_pred(Callback,Stream,Goal,Exit):- 
  open_prolog_stream(tl_with_prolog_streams, write, Stream, []),
  call_cleanup((
   asserta(((tl_with_prolog_streams:stream_write(Stream,Data):- (ignore(on_x_fail_priv(call(Callback,Data)))))),Ref),
   asserta(((tl_with_prolog_streams:stream_close(Stream):- (ignore(on_x_fail_priv(call(Callback,end_of_file)))))),Ref2),
   asserta(((lmconf:is_prolog_stream(Stream))),Ref3),
    % catch so we will not exception on a closed stream
    % set_stream(Stream, buffer(line)),
    set_stream(Stream, buffer(false)),
    % set_stream(Stream, buffer_size(0)),    
    % set_stream(Stream, close_on_exec(false)),    
    call_cleanup((call_cleanup(Goal,catch(flush_output(Stream),_,true))),(erase(Ref),erase(Ref2),erase(Ref3)))),
  Exit). % Not decided that a with_output_to_pred/2 should call close of not (flush gets the job done equally as well as closing)


% test predciate to receive char codes
buffer_chars(end_of_file):-!,assertz(received_chars(end_of_file)).
buffer_chars(N):-number(N),!,char_code(C,N),assertz(received_chars(C)).
buffer_chars(C):-name(C,Chars),maplist(buffer_chars,Chars).


some_test :- with_output_to_pred(buffer_chars,write('hello. ')).
some_test :- with_output_to_pred(buffer_chars,write('World.\n')).
% lets just be nasy to ourselves here (confirms we handle closing of current output)
some_test :- with_output_to_pred(buffer_chars,told).
some_test :- with_output_to_pred(buffer_chars,(current_output(Out),close(Out))).
% Not bad !

some_test :- listing(received_chars/1).

/*
received_chars(h).
received_chars(e).
received_chars(l).
received_chars(l).
received_chars(o).
received_chars('.').
received_chars(' ').
received_chars('W').
received_chars(o).
received_chars(r).
received_chars(l).
received_chars(d).
received_chars('.').
received_chars('\n').
received_chars(end_of_file).
received_chars(end_of_file).

Looks good !
*/


some_test :- with_output_to_pred(dmsg, (current_output(Out),forall(stream_property(Out,Prop),dmsg(stream_property(Out,Prop))))).

% dmsg: stream_property(<stream>(0x232b8a0),mode(write)).
% dmsg: stream_property(<stream>(0x232b8a0),output).
% dmsg: stream_property(<stream>(0x232b8a0),position('$stream_position'(0,1,0,0))).
% dmsg: stream_property(<stream>(0x232b8a0),eof_action(eof_code)).
% dmsg: stream_property(<stream>(0x232b8a0),reposition(false)).
% dmsg: stream_property(<stream>(0x232b8a0),type(text)).
% dmsg: stream_property(<stream>(0x232b8a0),buffer(full)).
% dmsg: stream_property(<stream>(0x232b8a0),buffer_size(0)).
% dmsg: stream_property(<stream>(0x232b8a0),close_on_abort(true)).
% dmsg: stream_property(<stream>(0x232b8a0),encoding(wchar_t)).
% dmsg: stream_property(<stream>(0x232b8a0),locale(default)).
% dmsg: stream_property(<stream>(0x232b8a0),newline(posix)).
% dmsg: stream_property(<stream>(0x232b8a0),representation_errors(error)).

l_prolog_streams.




%	  - Module:stream_read(+Stream, -Term)
%	  Called for a `Mode == read` stream to get new data.  On
%	  success the stream extracts text from the provided Term.
%	  Term is typically a string, atom, code or character list.
%	  If term is not one of the above, it is handed to writeq/1.
%	  To signal end-of-file, unify stream with an empty text,
%	  e.g., `stream_read(Stream, "")`.

:- thread_local(tl_with_prolog_streams:stream_read/2).
:- meta_predicate(tl_with_prolog_streams:stream_read(?,?)).
:- meta_predicate(prolog_stream:open_prolog_stream(?,?,?,?)).
with_input_from_pred(Callback,Goal):-
 current_input(Old),
 call_cleanup((
   must(var(Stream)),
    tl_with_prolog_streams:open_prolog_stream(tl_with_prolog_streams, read, StreamO, []),
    StreamO = Stream,
   asserta(((tl_with_prolog_streams:stream_read(Stream,Data):- ((call(Callback,Data))))),Ref),
   asserta(((tl_with_prolog_streams:stream_close(Stream):- (ignore(call(Callback,end_of_file))))),Ref2),
   set_stream(Stream,  alias(current_input)),
   set_stream(Stream,  alias(user_input)),
   setup_call_cleanup(see(Stream), Goal,(seen,erase(Ref2),erase(Ref)))),
 set_input(Old)).

% our test callback
read_received(A):- A == end_of_file,!. %  Dmiles was lazy
read_received(C):- retract(received_chars(A)), (A==end_of_file -> C="" ; C =A).
read_received("").

% Test wtih read/1 works awesome
some_test :- with_input_from_pred(read_received, (read(X), writeln(read(X)))).
% dmsg: read(hello)

% This test is just for deciding the scope .. (about asking callback to understand peeking or not)
some_test :- with_input_from_pred(read_received, (peek_char(X), writeln(peek_char(X)))).
% dmsg: peek_char('W')

some_test :- listing(received_chars).

/*
some_test :- dynamic received_chars/1.

received_chars(o).
received_chars(r).
received_chars(l).
received_chars(d).
received_chars('.').
received_chars('\n').
received_chars(end_of_file).
received_chars(end_of_file).
*/

some_test :- with_input_from_pred(=(""), (current_input(In),forall(stream_property(In,Prop),dmsg(stream_property(In,Prop))))).

% dmsg: stream_property(<stream>(0x9cfd70),mode(read)).
% dmsg: stream_property(<stream>(0x9cfd70),input).
% dmsg: stream_property(<stream>(0x9cfd70),position('$stream_position'(0,1,0,0))).
% dmsg: stream_property(<stream>(0x9cfd70),end_of_stream(not)).
% dmsg: stream_property(<stream>(0x9cfd70),eof_action(eof_code)).
% dmsg: stream_property(<stream>(0x9cfd70),reposition(false)).
% dmsg: stream_property(<stream>(0x9cfd70),type(text)).
% dmsg: stream_property(<stream>(0x9cfd70),buffer(full)).
% dmsg: stream_property(<stream>(0x9cfd70),buffer_size(0)).
% dmsg: stream_property(<stream>(0x9cfd70),close_on_abort(true)).
% dmsg: stream_property(<stream>(0x9cfd70),encoding(wchar_t)).
% dmsg: stream_property(<stream>(0x9cfd70),locale(default)).
% dmsg: stream_property(<stream>(0x9cfd70),newline(posix)).
% dmsg: stream_property(<stream>(0x9cfd70),representation_errors(error)).
% dmsg: stream_property(<stream>(0x9cfd70),timeout(infinite)).


% Passes
some_test :- \+ with_input_from_pred(=(""), \+ at_end_of_stream(current_input)).

% Passes
some_test :- with_input_from_pred(=(hi), \+ at_end_of_stream(current_input)).



% Test 1
test1_0(In) :-
        repeat,
               get_char(In, Char),
               dmsg(getch(In,Char)),
               Char == end_of_file.
           

% Passes
some_test :- with_input_from_pred(read_received, test1_0(current_input)).


% setup for test 2 
% :- with_output_to_pred(buffer_chars, format('Read it all\n',[])).
% :- listing(received_chars/1).


% Test 2 is indeed asks much, but still is reasonable
test2(In) :-
        repeat,
            (   at_end_of_stream(In)
            ->  !
            ;   read_pending_input(In, Chars, []),
                dmsg(read_pending_input(In, Chars, [])),
                fail
            ).

% TODO
% :- with_input_from_pred(read_received, test2(current_input)).


:- source_location(S,_),forall(source_file(H,S),(functor(H,F,A),export(F/A),module_transparent(F/A))).
