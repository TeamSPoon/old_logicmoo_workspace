%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: Delivery/main_swi.pl
%
% Legolog delivery robot program
%
% This file contains system dependent predicates
% It is written for SWI Prolog (http://www.swi.psy.uva.nl/projects/SWI-Prolog/)
% running under Linux
%
% WRITTEN BY: Maurice Pagnucco and Hector J. Levesque
% REVISED: June 15, 2000
% TESTED: SWI Prolog 3.3.6 under RedHat Linux 6.2
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                             June 15, 2000
%
% This software was developed by the Cognitive Robotics Group under the
% direction of Hector Levesque and Ray Reiter.
%
%        Do not distribute without permission.
%        Include this notice in any copy made.
%
%
%         Copyright (c) 2000 by The University of Toronto,
%                        Toronto, Ontario, Canada.
%
%                          All Rights Reserved
%
% Permission to use, copy, and modify, this software and its
% documentation for non-commercial research purpose is hereby granted
% without fee, provided that the above copyright notice appears in all
% copies and that both the copyright notice and this permission notice
% appear in supporting documentation, and that the name of The University
% of Toronto not be used in advertising or publicity pertaining to
% distribution of the software without specific, written prior
% permission.  The University of Toronto makes no representations about
% the suitability of this software for any purpose.  It is provided "as
% is" without express or implied warranty.
% 
% THE UNIVERSITY OF TORONTO DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
% SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
% FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TORONTO BE LIABLE FOR ANY
% SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
% RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
% CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% This is the top-level file for a Legolog program.
% It consults the necessary Legolog prolog files and
% defines any additional system-dependent predicates that are required.
%
% For this example this file defines the following predicates:
% -- initializeExog: perform any initialization of other sources of
%      exogenous actions that is required
% -- finalizeExog: things to do for other sources of exogenous actions
%      at end of program
% -- checkOtherExog(-ExogList): check whether a request has been
%      entered via keyboard
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DYNAMIC DECLARATIONS
%  These are not required by all Prologs so we include them here so as
%  to make the system-independent files as independent as possible
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% IndiGolog
:- dynamic(indi_exog/1).
:- dynamic(currently/2).
:- dynamic(temp/2).

% Delivery example

% keyboardStream(Stream): Saves the file descriptor currently referring 
%     to the open tty receiving keyboard input
:- dynamic(keyboardStream/1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSULT NECESSARY FILES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Consult Lego subsystem plus IndiGolog interpreter

:- consult('../Main/lego_swi'). % System dependent Prolog/RCX communication
                                %     predicates
:- consult('../Main/legorcx').  % Generic Prolog/RCX communication predicates
:- consult('../Main/golog').    % IndiGolog interpreter
:- consult(delivery).           % Golog delivery program

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MAIN PREDICATE - evaluate this to run demo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%main: Gets IndiGolog to evaluate main control procedure
main :- indigolog(control).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PREDICATES WITH SYSTEM DEPENDENT CODE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXOGENOUS ACTIONS - sources other than RCX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% initializeExog: initialization for sources of exogenous actions other
%     than the RCX
initializeExog :-
    keyboardFile(KbFile),
    keyboardShellScript(KbShellScript),  % Shell script to execute
    concat_atom(['xterm -e ', KbShellScript, ' ', KbFile, '&'], Command),
    shell(Command),
    sleep(1),   % Sleep for 1 second allowing time for creation of input file
    printKbInstructions,
    open(KbFile, read, KbStream, []),
    retractall(keyboardStream(_)),
    assert(keyboardStream(KbStream)).

% finalizeExog: Things to do for sources of exogenous actions other
%      than the RCX at end of program
finalizeExog :-
    keyboardStream(KbStream),
    close(KbStream).

% checkOtherExog(-ExogList): Check whether exogenous actions have occurred
%     other than those detected by the RCX
checkOtherExog(ExogList) :-
    checkKeyboardExog(ExogList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Exogenous actions from keyboard
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% keyboardFile(+File): Name of TTY or pipe from which keyboard input
%     will be read.
keyboardFile('./keyin').

% keyboardShellScript(+ShellScript): Name of shell script to execute
keyboardShellScript('./keybdexog.sh').

% keyboardWait(+Time): Time in seconds to wait for input from keyboard
keyboardWait(0.02).

% printKbInstructions: Print instructions on how to enter keyboard input
printKbInstructions :-
    write('*********************************************************'), nl,
    write('* NOTE: Keyboard Input - delivery/cancel requests'), nl,
    write('*   An xterm will appear on your screen.'), nl,
    write('*   Enter exogenous delivery command: +/-(From, To).'), nl,
    write('*   For example, request delivery from station 3 to 5: "+(3, 5)."'), nl,
    write('*   Type ctl-C in window when you are done.'), nl,
    write('*********************************************************'), nl, nl.

% checkKeyboardExog(-KbExogList): Looks for exogenous events entered
%     via keyboard and returns them in KbExogList. Uses system-dependent
%     I/O routines
checkKeyboardExog([KbExog]) :-
    keyboardWait(Time),
    keyboardStream(KbStream),
    wait_for_input([KbStream], ReadyList, Time),
    ReadyList \== [],
    read(KbStream, Term), write('Read: '), write(Term), nl,
    ((Term = +(From, To), KbExog = request_delivery(From, To));
        (Term = -(From, To), KbExog = cancel_request(From, To))).

checkKeyboardExog([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MISCELLANEOUS PREDICATES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% None required

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: Delivery/main_swi.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
