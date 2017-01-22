
cls ; killall -9 swipl xterm prolicyd perl ; swipl -g "system:ensure_loaded(logicmoo_repl),qsave_program(lm_repl,[goal(prolog)]),halt"

