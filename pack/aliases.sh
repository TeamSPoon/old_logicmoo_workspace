
#export LD_LIBRARY_PATH=/usr/lib/jvm/java-8-oracle/jre/lib/amd64/server/:/usr/lib/jvm/java-9-oracle/lib/amd64/server/:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/amd64:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib:/usr/lib/swi-prolog/lib/x86_64-linux

#export LD_LIBRARY_PATH=/usr/lib/jvm/java-8-oracle/jre/lib/amd64/server/:/usr/lib/jvm/java-8-oracle/lib/amd64/server

#export LD_LIBRARY_PATH=/usr/lib/jvm/java-8-oracle/jre/lib/amd64/server:/usr/lib/jvm/java-8-oracle/jre/lib/amd64
#export LD_PRELOAD_NEXT=/usr/lib/swi-prolog/lib/amd64/socket.so:/usr/lib/swi-prolog/lib/amd64/libjpl.so

export JAVA_HOME=/usr/lib/jvm/java-8-oracle/jre
export LD_LIBRARY_PATH=$JAVA_HOME/lib/amd64/server/:$JAVA_HOME/lib/amd64:$JAVA_HOME/lib:/usr/lib/swi-prolog/lib/x86_64-linux

export CLASSPATH=
export RLWRAP_HOME=$HOME/.local/share/rlwrap

export LM_WORKSPACE="/home/prologmud_server/lib/swipl/pack"
alias cdp='cd $LM_WORKSPACE'
alias cds='cd $LM_WORKSPACE/prologmud_samples/prolog/prologmud_sample_games'
alias gdbexec0='gdb -return-child-result  -ex "set pagination off" -ex "handle SIGXCPU SIG33 SIG35 SIGPWR SIGINT SIGALRM SIGWINCH SIG2 SIGCHILD pass nostop noprint" -ex run -ex "thread apply all bt all" -ex "quit" --args '
alias gdbexec='gdb -return-child-result -ex "set pagination off" -ex run -ex quit --args '
alias ss="cls ; killall -9 swipl xterm prolicyd perl ; gdbexec swipl "
echo cd /mnt/dddd/workspace/runtime
echo cd /mnt/dddd/workspace/phase02-jrtl/platform
echo export DISPLAY=`who am i --ips|awk '{print $5}' `:0.0
alias ls='ls -A --color '
alias abcl='/mnt/dddd/workspace/runtime/abcl.sh '
export RLWRAP_HOME=$HOME/.local/share/rlwrap
pushd "${LM_WORKSPACE}/logicmoo_base/t/examples/fol"
pushd "${LM_WORKSPACE}/prologmud_samples/prolog/prologmud_sample_games"
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
export GOPATH=$HOME/gopath
export PATH=$GOPATH:$GOPATH/bin:$PATH
alias cant='rm -rf build dist ~/.cache/ ;  ant ; ant ; LCD=$PWD ; cd ../old-ansi-tests ; $LCD/abcl --load doit.lsp ; cd -'

function _cd {
    # typing just `_cd` will take you $HOME ;)
    if [ "$1" == "" ]; then
        pushd "$HOME" > /dev/null

    # use `_cd -` to visit previous directory
    elif [ "$1" == "-" ]; then
        pushd $OLDPWD > /dev/null

    # use `_cd -n` to go n directories back in history
    elif [[ "$1" =~ ^-[0-9]+$ ]]; then
        for i in `seq 1 ${1/-/}`; do
            popd > /dev/null
        done

    # use `_cd -- <path>` if your path begins with a dash
    elif [ "$1" == "--" ]; then
        shift
        pushd -- "$@" > /dev/null

    # basic case: move to a dir and add it to history
    else
        pushd "$@" > /dev/null
    fi
}

# replace standard `cd` with enhanced version, ensure tab-completion works
alias cd=_cd
complete -d cd

complete -d cdp
complete -d cds


