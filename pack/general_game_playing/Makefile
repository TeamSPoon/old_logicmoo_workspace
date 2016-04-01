# Copyright (C) 2008 Stephan Schiffel <stephan.schiffel@gmx.de>
# 
# This file is part of the GGP starter code.
# 
# The GGP starter code is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# The GGP starter code is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with the GGP starter code.  If not, see <http://www.gnu.org/licenses/>.

ECLIPSE=eclipse

SOURCE_DIR=src
DOC_DIR=doc
BIN_DIR=bin

##############################################################
PROLOG_SOURCES:=$(wildcard $(SOURCE_DIR)/*.ecl)
PROLOG_MODULES:=$(basename $(notdir $(PROLOG_SOURCES)))
ECIS:=$(addprefix $(DOC_DIR)/, $(addsuffix .eci, $(PROLOG_MODULES)))
ECOS:=$(addprefix $(BIN_DIR)/, $(addsuffix .eco, $(PROLOG_MODULES)))

.PHONY: all doc dist run clean distclean

all: dist

run: dist
	./start_game_player.sh

dist: $(ECOS)

$(BIN_DIR)/%.eco : $(SOURCE_DIR)/%.ecl
	@mkdir -p $(BIN_DIR)
	$(ECLIPSE) -e "lib(fcompile), fcompile(\"$<\",[outdir:\"$(BIN_DIR)\"])"

doc: $(DOC_DIR)/index.html

$(DOC_DIR)/index.html : $(ECIS)
	@mkdir -p $(DOC_DIR)
	$(ECLIPSE) -e "lib(document), ecis_to_htmls(\"$(DOC_DIR)\",\"$(DOC_DIR)\", \"\", \"General Game Player\")"

$(DOC_DIR)/%.eci : $(SOURCE_DIR)/%.ecl
	@mkdir -p $(DOC_DIR)
	$(ECLIPSE) -e "lib(document), icompile(\"$<\",\"$(DOC_DIR)\")"

clean:
	rm -rf bin/

distclean: clean
	rm -rf logs/ doc/
