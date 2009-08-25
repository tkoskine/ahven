#
# Copyright (c) 2009 Reto Buerki <buerki@swiss-it.ch>
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
#

# project settings

PROJECT := example-project
EMAIL   := developer@example.net

FORMATS := html txt pdf
TARGETS := $(foreach fmt,$(FORMATS),$(PROJECT).$(fmt))

SRCDIR  := ../../results
DOCDIR  := docs
XSLDIR  := xsl
HTMLDIR := html

RESULTFILE := result.xml
HEADERFILE := result-header.xml
ENTITYFILE := data.ent

SOURCES := $(wildcard $(SRCDIR)/*.xml) \
	$(ENTITYFILE) \
	$(RESULTFILE) \
	$(HEADERFILE)

TRANSXSL := $(XSLDIR)/transform.xsl
HTMLXSL  := $(XSLDIR)/html.xsl
TXTXSL   := $(XSLDIR)/txt.xsl

EXT := docbook

# testresult input produced by ahven xml runner

TESTS := $(shell cd $(SRCDIR); ls TEST-*.xml)

# testenv info

DATE        := $(shell date -R)
OS          := $(shell uname -o)
KERNEL_NAME := $(shell uname -s)
KERNEL_REL  := $(shell uname -r)
KERNEL_VER  := $(shell uname -v)
CPU_NR      := $(shell getconf _NPROCESSORS_ONLN)

DBLATEX := dblatex --style=db2latex
XP      := xsltproc --nonet --novalid --xinclude

all: $(TARGETS)

index.html: $(SOURCES)
	@mkdir -p $(HTMLDIR)
	@$(XP) -o $(HTMLDIR)/ $(HTMLXSL) index.xml

$(PROJECT).html: index.html

$(PROJECT).txt: $(SOURCES)
	@$(XP) $(TXTXSL) index.xml | w3m -cols 65 -dump -T text/html > $@

$(PROJECT).pdf: $(SOURCES)
	@$(DBLATEX) index.xml -o $@

$(RESULTFILE): to-docbook
	@cp $(HEADERFILE) $(RESULTFILE)
	@for DOC in `ls $(DOCDIR)/TEST-*.$(EXT)`; do \
		echo "<xi:include href='$$DOC' xmlns:xi='http://www.w3.org/2003/XInclude'/>" \
	   		>> $(RESULTFILE); \
	done
	@echo "</chapter>" >> $(RESULTFILE)

to-docbook:
	@mkdir -p $(DOCDIR)
	@for SOURCE in $(TESTS); do \
		$(XP) $(TRANSXSL) $(SRCDIR)/$$SOURCE > $(DOCDIR)/$$SOURCE.$(EXT); \
	done

$(ENTITYFILE):
	@echo '<!ENTITY project "$(PROJECT)">'           >> $@
	@echo '<!ENTITY email "$(EMAIL)">'               >> $@
	@echo '<!ENTITY date "$(DATE)">'                 >> $@
	@echo '<!ENTITY os "$(OS)">'                     >> $@
	@echo '<!ENTITY kernel_name "$(KERNEL_NAME)">'   >> $@
	@echo '<!ENTITY kernel_release "$(KERNEL_REL)">' >> $@
	@echo '<!ENTITY kernel_version "$(KERNEL_VER)">' >> $@
	@echo '<!ENTITY cpu_nr "$(CPU_NR)">'             >> $@

clean:
	@rm -f $(PROJECT).*
	@rm -f $(RESULTFILE)
	@rm -f $(ENTITYFILE)
	@rm -rf $(DOCDIR)
	@rm -rf $(HTMLDIR)

.PHONY: all $(PROJECT).html clean
