                            Legolog - README File

                           Version 1 - 05/06/2000

            Written by: Hector J. Levesque and Maurice Pagnucco

  ------------------------------------------------------------------------

                                  Contents

   * License

   * Introduction

   * Obtaining Legolog

   * Basic Legolog Philosophy

   * Example Scenario - A Delivery Robot

   * Legolog Files

        o MAIN_SWI.PL, MAIN_ECL.PL, MAIN_LPA.DEC Files

        o GOLOG.PL

        o DELIVERY.PL

        o LEGORCX.PL

        o LEGO_SWI.PL, LEGO_ECL.PL, LEGO_LPA.DEC

        o CONTROL.NQC

        o DELIVERY.NQH, DELIVERY.NQC

        o KEYBDEXOG.SH

   * Using Legolog with another Planner

   * References

   * Questions, Problems, Comments, ...

  ------------------------------------------------------------------------

                                   License

                                June 15, 2000

This software was developed by the Cognitive Robotics Group under the
direction of Hector Levesque and Ray Reiter.

     Do not distribute without permission.
     Include this notice in any copy made.

              Copyright (c) 2000 by The University of Toronto,

                          Toronto, Ontario, Canada.

                             All Rights Reserved

Permission to use, copy, and modify, this software and its documentation for
non-commercial research purpose is hereby granted without fee, provided that
the above copyright notice appears in all copies and that both the copyright
notice and this permission notice appear in supporting documentation, and
that the name of The University of Toronto not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission. The University of Toronto makes no representations
about the suitability of this software for any purpose. It is provided "as
is" without express or implied warranty.

THE UNIVERSITY OF TORONTO DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
IN NO EVENT SHALL THE UNIVERSITY OF TORONTO BE LIABLE FOR ANY SPECIAL,
INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.

Introduction

This file describes the software needed to run Legolog, an implementation of
Golog for controlling the LEGO® MINDSTORMSTM Robotics Invention System. It
does not describe the Golog language, the LEGO® RCX computer, nor the NQC
language. See [Levesque & Pagnucco 00] for an overview of the whole system
and for references on these other topics. Legolog is intended to run on
various Prolog implementations (with support here for SWI Prolog and ECLiPSe
Prolog under Linux, and LPA Prolog under MS-DOS on a HP200LX).
Note: For MS-DOS we have only sucessfully tested Legolog on the HP200LX
using LPA DOS-Prolog 3.83. Attempts to run Legolog reliably on Windows
machines using various Prologs have encountered problems with the serial
port. It is recommended that users adopt one of the methods above.

Obtaining Legolog

The Legolog web page is located at:
                http://www.cs.toronto.edu/~cogrobo/Legolog/

The Legolog distribution and this README, along with other information, can
be obtained from there.

Basic Legolog Philosophy

The basic philosophy behind Legolog is that the RCX is programmed with
simple behaviours that correspond to primitive actions. The RCX can also
monitor for exogenous actions that need to be reported to the Golog
interpreter. The RCX does no reasoning whatsoever. Reasoning is performed by
a planner like Golog. It decides what primitive actions to perform which are
subsequently executed by the RCX. Golog also monitors for reports of
exogenous actions and sensing information from the RCX. The Golog
interpreter runs under Prolog on a standalone machine. All communication
between Golog and the RCX is done via the infrared tower. Moreover, all
communication is initiated by Prolog.

Example Scenario - A Delivery Robot

In this distribution the included sample application is one of a robot
delivery system. The LEGO® robot lives on a surface where black tape
indicates the road it can follow, and bright markers indicate way stations
(numbered 1-6) on the road where it can stop. The road is a single line
although it can have curves and bends; it does not have any junctions. To go
backwards on the road, the robot needs to turn around and then go forward.
Requests are made from the keyboard to pick up a "package" at one way
station and deliver it to another way station. (Delivery requests can also
be cancelled.) On arriving at a way station where a pickup or dropoff is
needed, the robot signals its arrival and waits until someone either takes
or places a package on its tray, and pushes its "continue" (or "go") button.
When there are no more deliveries left to perform, the robot heads towards
way station 1, and then awaits further requests once it arrives.

In the particular files included here, at the outset the robot is located at
the third way station and is facing towards way station 4. It has deliveries
pending for way stations 4 and 6.

Delivery and cancellation requests can be received at any time. They are
typed into a window waiting to receive exogenous requests via the keyboard.
Delivery requests are of the form "+(From, To).", e.g., +(2, 3). is a
request to collect an object from way station 2 and deliver it to way
station 3. In similar fashion, cancellation requests have the form "-(From,
To).". A cancellation must be made before the robot has collected the object
from the "From" way station in order to have effect.
Note: The final period `.' is important as the input must be in the form of
a Prolog term.

Legolog Files

A Legolog application is made up of 8 files. Some of these files are
application independent (these are prefixed with an asterisk (*) in the list
below) and can be used in applications other than the delivery task demo
included here. For convenience, in the Legolog distribution, application
independent file are located in a directory called Main while the
application dependent files - in this case, for the delivery task - are
located in the Delivery directory. Briefly, the files are as follows:

     main_XXX.pl
          (where XXX is one of "swi", "ecl", or "lpa"): a short Prolog
          program, that loads the rest of the Prolog files and defines
          any special implementation/application dependent predicates.
          In this instance we have included some predicates to show how
          exogenous requests from the keyboard may be handled.

          Note: LPA Prolog uses a .dec extension rather than .pl. The
          LPA files are therefore suffixed accordingly. To run the demo
          under LPA Prolog, you will need to rename "delivery.pl",
          "golog.pl" and "legorcx.pl" to have a .dec extension.

     delivery.pl
          the application program, written in Golog.

     (*) golog.pl
          the Golog interpreter, in Prolog.

     (*) legorcx.pl
          high-level routines for communication between the interpreter
          and the LEGO® RCX, in Prolog.

     (*) lego_XXX.pl
          (where XXX is one of "swi", "ecl", or "lpa"): a short Prolog
          file defining the implementation dependent communication and
          timing routines.

     (*) control.nqh
          application independent NQC include file containing the
          communication routines and control procedures for the RCX
          side.

     delivery.nqh
          application dependent NQC include file containing
          definitions.

     delivery.nqc
          the primitive application behaviours to be run by the RCX
          computer, written in NQC.

     Note: For the Linux versions running under ECLiPSe and SWI Prolog,
     there is an additional shell script:

     keybdexog.sh
          shell script that creates a named pipe under unix, if
          necessary, from where exogenous keyboard input may be taken.

     This will run in a new xterm window whose object is to take
     delivery and cancellation requests from the keyboard. The LPA
     version splits the screen into two "virtual" windows: one for
     accepting delivery and cancellation requests from the keyboard,
     the other for outputting information about which actions are being
     executed.

To run the sample delivery application, do the following:

        * construct robot and track (the track should consist of a
          single "road" with six way stations)

        * download NQC program: nqc -d delivery.nqc
          (It may be necessary to calibrate the light sensor threshold
          settings beforehand.)

        * consult "main_XXX.pl" in Prolog

        * position the robot at the third way station, pointing towards
          the fourth way station, and press the start button on the RCX

        * evaluate the "main/0" predicate in Prolog.

Every attempt was made to keep the Legolog system modular. Beyond the given
sample application, here are some other possible uses of parts of the
system:

       1. to run a different Golog application program on the robot:
          change the files "delivery.pl", "delivery.nqh" and
          "delivery.nqc" and possibly "main_XXX.pl".

       2. to run Golog without any LEGO® connection: use "golog.pl" and
          a variant of "delivery.pl" or some other Golog program (to
          see how one might do this, there are sections that can be
          uncommented in the "delivery.pl" file).

       3. to port Legolog to a new Prolog implementation: redefine the
          low-level communication and timing predicates in the
          "lego_XXX.pl" file (to run the delivery application, check
          "main_XXX.pl" for any Prolog implementation specific
          predicates).

       4. to interact with the LEGO® RCX without using Golog: replace
          "golog.pl" and "delivery.pl" by some other high-level
          controller written in Prolog (e.g. a STRIPS planner, an event
          or fluent calculus one) that calls the predicate
          "sendRcxActionNumber/2"in "legorcx.pl" to perform actions on
          the robot.

In what follows, we describe the 8 files above, mainly in terms of their
interface assumptions and other points of interest. Other notes are
available in the internal comments.

MAIN_SWI.PL, MAIN_ECL.PL, MAIN_LPA.DEC

These are the top-level files for the various Prologs. They consult all of
the necessary files and make requisite dynamic declarations. There is a
"main/0" predicate that calls "indigolog/1" to execute the main control
procedure of the Golog program. This file also takes care of dealing with
exogenous events that do not originate from the RCX; delivery and
cancellation requests entered from the keyboard for the delivery
application. For this purpose "checkOtherExog/1" must be defined here along
with "initializeExog/1" and "finalizeExog/1". In this application, the
keyboard is the only other source of exogenous events.

GOLOG.PL

This file defines the Golog language, and in particular, the IndiGolog
variant (incremental deterministic Golog) used. The top level predicate is
"indigolog/1", which executes a Golog program. It handles exogenous events,
sensing actions, programmed concurrency, interrupts, and the search
operator. The interpreter performs rolling forward to bound the length of
the history of actions. Rolling forward is controlled by the predicate
"roll_parameters/3".

Golog expects to see a number of predicates defined in a Golog application
(like "delivery.pl"). Each of these required predicates mentioned here and
below need to be defined by at least one clause, even if they always fail.

The following predicates are needed to define the Golog application:

     prim_fluent(fluent)
          for each primitive fluent

     prim_action(action)
          for each primitive action

     exog_action(action)
          for each exogenous action

     senses(action, fluent)
          for each sensing action

     poss(action, cond)
          when cond is true, action is executable

     initially(fluent, value)
          fluent has value in S0 (the initial situation)

     causes_val(action, fluent, value, cond)
          when cond holds, doing act causes fluent to have value

     proc(name, program)
          Golog complex actions

Golog calls the two predicates below before and after running a program to
do any application dependent initialization and cleanup:

     initialize
          run at start of programs

     finalize
          run at end of programs

During execution, Golog produces a sequence of calls to this predicate:

     execute(action, history, result)
          do the action, return the sensing result; the history is
          passed for diagnostic or other use, the (sensing) result is
          ignored unless action is a sensing action

Also, Golog repeatedly calls

     exog_occurs(list-of-actions)
          return a list of exogenous actions that have occurred since
          the last time it was called. The predicate can fail or return
          [] when there are none.

to check to see if anything has happened exogenously since the last time.

DELIVERY.PL

This file defines the robot delivery application. There are three parts: the
declarative part (the Prolog version of the axioms of an action theory),
procedural part (the definition of the control programs), and the glue
linking Golog to the RCX.

For the declarative part we specify axioms for the fluents "direction",
"location", "motion", "delivery_requested(from)" and
"holding_delivery_for(to)", and for the primitive actions "turnaround",
"start_to_next_station", "signal_arrival", as well as the exogenous actions
"request_delivery(from, to)", "cancel_request(from, to)", "stop_abnormally",
"arrive_at_station", and "push_go_button". The first two exogenous actions
occur at the keyboard when the user makes or cancels requests; the last
three occur at the robot.

Note that in this application, there is no primitive action for "going to a
location"; this is handled by "start_to_next_station" which starts the
motion in the direction the robot is facing; one of the exogenous actions
"arrive_at_next_station" or "stop_abnormally" will eventually occur, which
signals the (normal or abnormal) termination of the motion. In fact, the
robot has no concept of its locations; this is maintained by Golog.

The procedural part involves defining a top level program called "control"
which is invoked by "main/0" in "main_XXX.pl". Here the control program is a
set of prioritized interrupts which do the following:

        * if the robot has stopped abnormally, then recover somehow

        * else if the robot is moving, then wait

        * else if (the robot is stopped and) the location is one where
          a pickup or dropoff is needed, then signal_arrival and wait

        * else if there is a next place to go to, then head towards it

        * else if the robot is not yet at home, then head towards home
          (way station 1)

        * else wait

Note that heading towards a location may involve asking the robot to turn
around. The robot itself does not keep track of its location or what
direction it is facing.

To properly interface with the RCX, the Golog application defines
"initialize/0" to call "initializeRcx/0" (defined in "legorcx.pl") to get
the RCX communication ready and "initializeExog/0" (defined in
"main_XXX.pl") to get the keyboard and screen ready. The predicate
"finalize/0" is similar.

To perform a primitive action, "execute/3" is defined to send an appropriate
action message to the RCX telling it to do some behaviour. The predicate
"exog_occurs/1" is defined to check if the RCX reports an exogenous action
or if the user typed a delivery request or cancellation on the keyboard.
Each action to be sent to or received from the RCX is assigned a unique
number by the predicate "actionNumber/2". Then the predicates
"sendRcxActionNumber/2" and "receiveRcxActionNumber/1" (defined in
"legorcx.pl") are invoked to do the actual communication.

In the version of delivery.pl supplied, we have included some additional
predicates that allow for some simple error recovery. The predicate
"debugRcx/3" takes an action and history and prints the value of all fluents
after the actions in the history are executed. This predicate is called if
"execute/3" fails (usually due to a failure in communicating with the RCX).
If the user so wishes, the action can be re-attempted and a sensing value
returned as if the problem had never occurred.

While the delivery application does not make use of any actions that perform
sensing, Legolog is designed to allow for such actions. Each action must
return a sensing value although this can be an arbitrary value (we use 0
here) for non-sensing actions (in any case, such values are ignored by
Golog). Such values must be returned by the RCX within 3.5 seconds of having
invoked a primitive action otherwise Prolog will time out (as noted below)
and attempt to resend the action to the RCX. However, the RCX can also
return a special message requesting further time to return a sensing value.
Refer to [Levesque & Pagnucco 00] for further details.

LEGORCX.PL

There are two types of communication required by Legolog: Golog will send
the number of a primitive action to be performed by the RCX, and the RCX
will reply with a sensing value (which may be a fixed value); Golog will ask
the RCX if any exogenous action has occurred, and the RCX will reply with
the number of an action, or a special number indicating none.

The main predicates defined in "legorcx.pl" are

     sendRcxActionNumber(number, result)
          send action number to the RCX computer and fail if it does
          not return a result

     receiveRcxActionNumber(list-of-numbers)
          get a list of exogenous action numbers from the RCX and fail
          if there is no reply

These are called by "execute/3" and "exog_occurs/1" in "delivery.pl".

The LEGO® RCX computer communicates with a host computer over infrared using
a LEGO® provided infrared tower that plugs into the host's serial port. The
infrared tower must be activated by an outgoing message from the host. It
times out after about 4 seconds and is unable to received input if it is not
further activated. Therefore, all communication must originate from Golog.

Apart from a number of predefined message types (used, for example, by NQC
to download programs to the RCX), the RCX can send or receive a "user
message" which is a single number in the range 1-255. To allow for
arbitrarily large action numbers and sensing values, special Legolog
messages (like a panic / abort message, exogenous request message, etc.), as
well as to allow for multiple RCX computers, Legolog divides the user
messages 1-255 into ranges. For example, only messages in the range 32-63
are used for action numbers (for a first RCX computer). An action number x
in the range 0-15 can be sent as the user message 32+x, but larger action
numbers are sent as multiple user messages, each of which must be
acknowledged by the receiver before the next part is sent. If no reply or
return value arrives, the message is resent after a delay. Sensing values
returned by the RCX to Golog are dealt with in a similar manner however the
message range used is different so that the two types of messages are not
confused. Refer to [Levesque & Pagnucco 00] for further details.

All of the communication work in the predicates "sendRcxActionNumber/2" and
"receiveRcxActionNumber/1" is done by "sendRcxMessage/3". It sends a single
user message out and tries to get a single user message reply from the RCX.
If nothing returns within 3.5 seconds, it tries again, eventually failing if
the given time runs out. To send a single user message, a stream of 9 bytes
must be written to the serial port using "putRcxMess/1"; to receive a single
user message, a stream of 9 bytes must be read from the serial port using
"getRcxMess/2". In case the 9 bytes received are not the ones being sought
(because of other unrelated communication or noise), "getRcxMess/2" can
backtrack and read additional bytes.

LEGO_SWI.PL, LEGO_ECL.PL, LEGO_LPA.DEC

These files are used to define the lowest level communication and timing
predicates for the various Prolog implementations. These are:

     initRcx()
          initialize serial port, baud, parity etc. for communication
          with the RCX via the infrared tower

     openRcxRead()
          open the serial port for reading

     openRcxWrite()
          open the serial port for writing

     closeRcx()
          close the serial port

     getRcxByte(Ascii)
          read a byte sent from the RCX

     putRcxByte(Ascii)
          write a byte to the RCX

     eofRcx()
          succeed if there is no byte from RCX waiting to be read

     currentTime(Time)
          get current time in 100ths of seconds

     waitUntilRcx(Endtime)
          optionally wait until time is Endtime or until input arrives
          from RCX. Fail if Endtime has already passed.

These predicates are only called from within "legorcx.pl". Note that all
communication is at 2400 baud, odd parity, 8 data bits and 1 stop bit.

CONTROL.NQH

The NQC code that runs on the RCX consists of an application independent
portion that takes care of communication with Golog and monitors the status
of the RCX and an application dependent portion that executes behaviours and
monitors for exogenous events (e.g., button presses etc.). The control.nqh
include file takes care of the application independent part. It is
essentially an endless event loop in the main task. It monitors for incoming
messages from Golog requesting the execution of an action or querying the
occurrence of exogenous events. These messages are decoded and, in the case
of actions, the function "startBehaviour(number)" is called with number
having the value of the relevant action to be executed. In the case of a
query for exogenous events, the RCX will return the value of the global
variable "exogAction" if an exogenous event has occurred and otherwise
return a special message number indicating no exogenous event has occurred.
Note that exogenous events are not queued, only one can be stored at a time.
Functions are also provided for sending messages to and receiving messages
from Golog. In the following section we detail what functions and constants
this file requires to be provided in the application dependent code.

DELIVERY.NQH, DELIVERY.NQC

These two files specify the application dependent portion of the NQC code.
In this instance, these files contain code for accomplishing the delivery
task.

The delivery.nqh include file should define the constants ACTION_MESG_LOW,
VALUE_MESG_LOW, ACTION_MESG_BASE and VALUE_MESG_BASE required by the
send/receive functions in control.nqh for communicating with Golog. These
values must correspond to those of "rcxMessageRange/4" in legorcx.pl.

The delivery.nqc file contains code for all the behaviours and code that
monitors for the occurrence of exogenous actions (e.g., button press,
arrival at way station, etc.). In addition, the following functions must be
provided:

void initialize()
     perform any necessary initialization of RCX, e.g., define input/output
     types, display sensor values, start exogenous action monitors, etc.

void startBehaviour(int BehNum)
     start action corresponding to BehNum. Once this returns the value of
     the global variable "sensorVal" will be returned to Golog as the
     sensing value of the action.

void stopAllBehaviours()
     terminate all currently running behaviour tasks. Variables can be reset
     here too.

void panicAction()
     what to do when the status of the RCX is PANIC (i.e., when Prolog is
     not accepting transmissions).

When actions are executed they must complete within 3.5 seconds otherwise
Golog will resend the action request. This usually means that actions are
coded using NQC functions or subroutines. Before termination, these must set
the value of the global variable "sensorVal" which will be returned by the
RCX to Golog as the sensing result of the action. For actions that might
have a long duration (e.g., line following in the delivery task), the
primitive action is a request to initiate this action and the subroutine or
function can start a task (which runs concurrently on the RCX) to accomplish
this. The RCX will need to signal the occurrence of an exogenous event once
this task completes. See the "task goToNextStation()" for an example. Action
functions also have the option of returning a special message that is a
request for an additional 3.5 seconds. However, they must eventually return
a sensing value. See the "void turnAround()" function for an example of this
type of action.

Exogenous events are returned to the RCX by setting the value of the global
variable "exogAction". This may be set by any task, subroutine or function.
Note that these are not queued however, so that only one exogenous action
can be returned at a time.

Calibrating thresholds

The delivery.nqc file contains some line following code. This is loosely
based on a program in Chapter 8 of Dave Baum's Definitive Guide to LEGO
MINDSTORMS. This code uses threshold values to determine when the robot is
on a line and when it has located a landmark. These values will vary
depending on the materials you use for the surface on which the robot is to
move. In our experiments we have used brown cardboard for the surface, black
electrical tape for the line and shiny aluminum foil for the landmarks. The
black tape gives the lower light sensor reading, the foil gives the higher
while the cardboard gives an intermediate reading. As a rule of thumb, use
the RCX to measure the value of the light sensor when it is over the line,
add 3 or 4 to this and make the result the value of the constant
"LINE_THRESHOLD". Similarly, measure the value of the light sensor when it
is over a landmark, subtract 3 or 4 and make this the value of the constant
"STOPPER_THRESHOLD". Some variation of these values may be required to
improve the line following behaviour.

KEYBDEXOG.SH

This Bourne shell script is used by Prologs running under Linux. In the
main_XXX.pl files for these Prologs, an xterm is created to allow the user
to enter delivery and cancellation requests via the keyboard as indicated
above. Characters typed into this window are directed to a named Unix pipe
which is created by this script, if necessary. The script removes the pipe
once it is done. The xterm should be killed once you are finished by typing
ctl-C in it.

Using Legolog with another Planner

It is quite a simple task to replace Golog with a different planning
language and use the Legolog communication predicates to control the RCX. We
have alluded to this above and the description of the various Legolog files
should give a good indication of how this would be achieved. Here we give a
brief synopsis. The NQC code that runs on the RCX will remain the same so we
do not discuss it at all.

You will need to retain the files legorcx.pl and lego_XXX.pl and essentially
provide replacements for golog.pl (your own planner should take its place),
delivery.pl, and main_XXX.pl. You must first initialize the serial port to
which the infrared tower is connected by executing "initializeRcx/0". When
your planner determines an action that should be executed by the RCX this
needs to be communicated to the RCX by translating the action into the
appropriate number (we use the predicate "actionNum/2" for this purpose) and
send it to the RCX using "sendRcxActionNumber/2". This predicate will return
a sensing value for the action which you can choose to deal with or
disregard as you please. If your planner generates a list of actions to be
executed, these can be executed sequentially by successive calls to
"sendRcxActionNumber/2". Exogenous actions detected by the RCX must be
explicitly requested by your planner using the "receiveRcxActionNumber/3"
predicate as all communication between the planner and the RCX must be
initiated by the planner. This means that you may need to alter your planner
so as to monitor for exogenous actions on a regular basis. Failure to do so
may result in the loss of such events. You must also take care of monitoring
other sources of exogenous actions as we do with delivery and cancellation
requests typed at the keyboard in main_XXX.pl. When you are done, execute
"finalizeRcx/0" to tidy things up.

References

[Levesque & Pagnucco 00]
     Levesque, H. J., and Pagnucco, M., Legolog: Inexpensive Experiments in
     Cognitive Robotics, In Proceedings of the Second International
     Cognitive Robotics Workshop, Berlin, Germany, August 21-22, 2000.

Questions, Problems, Comments, ...

Please contact:

     Maurice Pagnucco/Hector J. Levesque
     Cognitive Robotics Group
     Department of Computer Science
     D. L. Pratt Building
     6 King's College Road
     University of Toronto
     Toronto, Ontario, CANADA, M5S 3G4

     Email: {morri | hector}@cs.toronto.edu.

  ------------------------------------------------------------------------
Maurice Pagnucco (morri@cs.toronto.edu) and Hector J. Levesque
(hector@cs.toronto.edu)

Last modified: Mon Aug 14 12:10:22 2000
