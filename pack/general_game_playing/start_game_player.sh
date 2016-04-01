#!/bin/sh

ECLIPSE="eclipse -g 512M"

${ECLIPSE} -b "bin/main" -e "main:start_game_player"

