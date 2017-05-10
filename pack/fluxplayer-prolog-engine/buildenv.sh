# This file contains the environment that can be used to
# build the foreign pack outside Prolog.  This file must
# be loaded into a bourne-compatible shell using
#
#   $ source buildenv.sh

PATH='/usr/local/lib/swipl-7.5.3/bin/x86_64-linux:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/snap/bin:/usr/lib/jvm/java-8-oracle/bin:/usr/lib/jvm/java-8-oracle/db/bin:/usr/lib/jvm/java-8-oracle/jre/bin:/usr/lib/jvm/java-8-oracle/bin:/usr/lib/jvm/java-8-oracle/db/bin:/usr/lib/jvm/java-8-oracle/jre/bin:/usr/share/logtalk/tools/lgtdoc/xml:/usr/share/logtalk/scripts:/usr/share/logtalk/integration'
SWIPL='/usr/local/lib/swipl-7.5.3/bin/x86_64-linux/swipl'
SWIPLVERSION='70503'
SWIHOME='/usr/local/lib/swipl-7.5.3'
SWIARCH='x86_64-linux'
PACKSODIR='lib/x86_64-linux'
SWISOLIB=''
SWILIB='-lswipl'
CC='gcc'
LD='gcc'
CFLAGS='-pthread -fPIC  -I"/usr/local/lib/swipl-7.5.3/include"'
LDSOFLAGS='-rdynamic -O3 -pthread -Wl,-rpath=/usr/local/lib/swipl-7.5.3/lib/x86_64-linux  -shared'
SOEXT='so'
USER='root'
HOME='/root'

export  PATH SWIPL SWIPLVERSION SWIHOME SWIARCH PACKSODIR SWISOLIB SWILIB CC LD CFLAGS LDSOFLAGS SOEXT USER HOME
