#!/bin/bash

command -v git >/dev/null 2>&1 || { echo >&2 "I require git but it's not installed.  Aborting."; exit 1; }

cd /opt
git clone --recursive  https://github.com/TeamSPoon/PrologMUD 
cd PrologMUD
git fetch --recurse-submodules
git submodule init
git submodule update

# return 0
[ $PS1 ]&&return||exit;

---------------------------------------------------------------------
Example output
---------------------------------------------------------------------

root@c3po:/opt#  source <(curl -s https://raw.githubusercontent.com/TeamSPoon/PrologMUD/master/scripts/web_install.sh)
The program 'curl' is currently not installed. You can install it by typing:
apt-get install curl
root@c3po:/opt# apt-get install curl
Reading package lists... Done
Building dependency tree
Reading state information... Done
The following package was automatically installed and is no longer required:
  libunwind8
Use 'apt-get autoremove' to remove it.
The following extra packages will be installed:
  libcurl3 libcurl4-openssl-dev
Suggested packages:
  libcurl4-doc libcurl3-dbg
The following NEW packages will be installed:
  curl
The following packages will be upgraded:
  libcurl3 libcurl4-openssl-dev
2 upgraded, 1 newly installed, 0 to remove and 743 not upgraded.
Need to get 541 kB of archives.
After this operation, 314 kB of additional disk space will be used.
Do you want to continue? [Y/n]
Get:1 http://us.archive.ubuntu.com/ubuntu/ trusty-updates/main libcurl4-openssl-dev amd64 7.35.0-1ubuntu2.5 [245 kB]
Get:2 http://us.archive.ubuntu.com/ubuntu/ trusty-updates/main libcurl3 amd64 7.35.0-1ubuntu2.5 [173 kB]
Get:3 http://us.archive.ubuntu.com/ubuntu/ trusty-updates/main curl amd64 7.35.0-1ubuntu2.5 [123 kB]
Fetched 541 kB in 2s (263 kB/s)
(Reading database ... 266780 files and directories currently installed.)
Preparing to unpack .../libcurl4-openssl-dev_7.35.0-1ubuntu2.5_amd64.deb ...
Unpacking libcurl4-openssl-dev:amd64 (7.35.0-1ubuntu2.5) over (7.35.0-1ubuntu2.2) ...
Preparing to unpack .../libcurl3_7.35.0-1ubuntu2.5_amd64.deb ...
Unpacking libcurl3:amd64 (7.35.0-1ubuntu2.5) over (7.35.0-1ubuntu2.2) ...
Selecting previously unselected package curl.
Preparing to unpack .../curl_7.35.0-1ubuntu2.5_amd64.deb ...
Unpacking curl (7.35.0-1ubuntu2.5) ...
Processing triggers for man-db (2.6.7.1-1ubuntu1) ...
Setting up libcurl3:amd64 (7.35.0-1ubuntu2.5) ...
Setting up libcurl4-openssl-dev:amd64 (7.35.0-1ubuntu2.5) ...
Setting up curl (7.35.0-1ubuntu2.5) ...
Processing triggers for libc-bin (2.19-0ubuntu6.4) ...
root@c3po:/opt# source <(curl -s https://raw.githubusercontent.com/TeamSPoon/PrologMUD/master/scripts/web_install.sh)
Cloning into 'PrologMUD'...
remote: Counting objects: 27126, done.
remote: Compressing objects: 100% (144/144), done.
remote: Total 27126 (delta 69), reused 0 (delta 0), pack-reused 26982
Receiving objects: 100% (27126/27126), 212.83 MiB | 6.37 MiB/s, done.
Resolving deltas: 100% (14491/14491), done.
Checking connectivity... done.
Checking out files: 100% (15153/15153), done.
Submodule 'pack/ClioPatria' (https://github.com/TeamSPoon/ClioPatria) registered for path 'pack/ClioPatria'
Submodule 'pack/MUD_ircbot' (https://github.com/TeamSPoon/MUD_ircbot.git) registered for path 'pack/MUD_ircbot'
Submodule 'pack/hMUD' (https://github.com/TeamSPoon/hmud.git) registered for path 'pack/hMUD'
Submodule 'pack/pldata_larkc' (https://gitlab.prologmoo.com/prologmud_data/pldata_larkc.git) registered for path 'pack/pldata_larkc'
Submodule 'pack/swish' (https://github.com/TeamSPoon/swish.git) registered for path 'pack/swish'
Submodule 'pack/trill_on_swish' (https://github.com/TeamSPoon/trill_on_swish.git) registered for path 'pack/trill_on_swish'
Cloning into 'pack/ClioPatria'...
remote: Counting objects: 7674, done.
remote: Total 7674 (delta 0), reused 0 (delta 0), pack-reused 7674
Receiving objects: 100% (7674/7674), 2.01 MiB | 2.33 MiB/s, done.
Resolving deltas: 100% (4751/4751), done.
Checking connectivity... done.
Submodule path 'pack/ClioPatria': checked out '25fa91c75514d15b9793d9fe54d5055a2a54c1f2'
Submodule 'test/Tests/sparql-1.1' (git://eculture.cs.vu.nl/home/git/misc/sparql-1.1-test-cases.git) registered for path 'test/Tests/sparql-1.1'
Submodule 'web/FlintSparqlEditor' (https://github.com/TSO-Openup/FlintSparqlEditor.git) registered for path 'web/FlintSparqlEditor'
Submodule 'web/yasqe' (https://github.com/YASGUI/YASQE.git) registered for path 'web/yasqe'
Submodule 'web/yasr' (https://github.com/YASGUI/YASR.git) registered for path 'web/yasr'
Cloning into 'test/Tests/sparql-1.1'...
fatal: unable to connect to eculture.cs.vu.nl:
eculture.cs.vu.nl[0: 130.37.193.11]: errno=Connection refused

Clone of 'git://eculture.cs.vu.nl/home/git/misc/sparql-1.1-test-cases.git' into submodule path 'test/Tests/sparql-1.1' failed
Cloning into 'pack/MUD_ircbot'...
remote: Counting objects: 83, done.
remote: Total 83 (delta 0), reused 0 (delta 0), pack-reused 83
Unpacking objects: 100% (83/83), done.
Checking connectivity... done.
Submodule path 'pack/MUD_ircbot': checked out '64defd512aff40ae1201bd7ac44ea7359764cf42'
Cloning into 'pack/hMUD'...
remote: Counting objects: 117, done.
remote: Total 117 (delta 0), reused 0 (delta 0), pack-reused 117
Receiving objects: 100% (117/117), 102.60 KiB | 0 bytes/s, done.
Resolving deltas: 100% (48/48), done.
Checking connectivity... done.
Submodule path 'pack/hMUD': checked out 'ab56eb9b757b6575cefa69aa3ed1c6a7e23adc2f'
Cloning into 'pack/pldata_larkc'...
remote: Counting objects: 17, done.
remote: Compressing objects: 100% (10/10), done.
fatal: The remote end hung up unexpectedly
fatal: early EOF
fatal: unpack-objects failed
Clone of 'https://gitlab.prologmoo.com/prologmud_data/pldata_larkc.git' into submodule path 'pack/pldata_larkc' failed
Fetching submodule pack/ClioPatria
Fetching submodule pack/MUD_ircbot
Fetching submodule pack/hMUD
Cloning into 'pack/pldata_larkc'...
remote: Counting objects: 17, done.
remote: Compressing objects: 100% (10/10), done.
remote: Total 17 (delta 1), reused 0 (delta 0)
Unpacking objects: 100% (17/17), done.
Checking connectivity... done.
Submodule path 'pack/pldata_larkc': checked out '17d108cd1bd039b3b89dcc24b2ac26fef3a4a71e'
Cloning into 'pack/swish'...
remote: Counting objects: 1508, done.
remote: Total 1508 (delta 0), reused 0 (delta 0), pack-reused 1508
Receiving objects: 100% (1508/1508), 970.86 KiB | 0 bytes/s, done.
Resolving deltas: 100% (1008/1008), done.
Checking connectivity... done.
Submodule path 'pack/swish': checked out '7356d37a83e4f2f80e2250f159af10a27daa25c4'
Cloning into 'pack/trill_on_swish'...
remote: Counting objects: 2166, done.
remote: Total 2166 (delta 0), reused 0 (delta 0), pack-reused 2166
Receiving objects: 100% (2166/2166), 8.49 MiB | 4.09 MiB/s, done.
Resolving deltas: 100% (655/655), done.
Checking connectivity... done.
Submodule path 'pack/trill_on_swish': checked out 'dc6109fff4778eead4bdc6e9225ce24128a51266'
root@c3po:/opt/PrologMUD#
