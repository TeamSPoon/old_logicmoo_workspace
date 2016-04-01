#!/bin/sh

# The perf framework should set the ECLIPSE environment variable
# Try "eclipse" if ECLIPSE is not defined
ECLIPSE="${ECLIPSE:-eclipse}"

GAME_FILE=$1
OUTPUT_FILE=$2
SECONDS_TO_RUN=$3

${ECLIPSE} -g 512M -b "bin/main_perf" -e "main_perf:run_perf_test(\"${GAME_FILE}\", \"${OUTPUT_FILE}\", ${SECONDS_TO_RUN})"
