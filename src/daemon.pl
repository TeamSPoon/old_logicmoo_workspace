%!/usr/bin/swipl -f

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ClioPatria is prepared to be combined with the SWI-Prolog library
library(http/http_unix_daemon).  ClioPatria is started as a deamon
using

  % ./daemon.pl -- port=Port [option ...]

See library(http/http_unix_daemon) for details.  The SWI-Prolog
distribution contains a file debian-init-script, which can be
used as a skeleton for managing the server from /etc/init.d.  To
do so, follow these steps:

  1. copy debian-init-script to /etc/init.d/<server>
  2. Edit /etc/init.d/<server>, setting DIR to the the working
     directory of the ClioPatria server.
  3. Edit the remainder of the configuration section to suit
     your needs.
  4. Run
       % update-rc.d <server> defaults
       % /etc/init.d/<server> start
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- set_prolog_flag(verbose, silent).
:- use_module(library(http/http_unix_daemon)).
:- [run].
