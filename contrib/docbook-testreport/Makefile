PROJECT := example-project
FORMATS := html txt pdf
TARGETS := $(foreach fmt,$(FORMATS),$(PROJECT).$(fmt))

SRCDIR  := ../../results
DOCDIR  := docs
XSLDIR  := xsl
HTMLDIR := html

RESULTFILE := result.xml
HEADERFILE := result-header.xml
ENTITYFILE := data.ent

SOURCES := $(wildcard $(SRCDIR)/*.xml) $(ENTITYFILE) $(RESULTFILE) $(HEADERFILE)

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
		echo "<xi:include href='$$DOC' xmlns:xi='http://www.w3.org/2003/XInclude'/>" >> $(RESULTFILE); \
	done
	@echo "</chapter>" >> $(RESULTFILE)

to-docbook:
	@mkdir -p $(DOCDIR)
	@for SOURCE in $(TESTS); do \
		$(XP) $(TRANSXSL) $(SRCDIR)/$$SOURCE > $(DOCDIR)/$$SOURCE.$(EXT); \
	done

$(ENTITYFILE):
	@echo '<!ENTITY project "$(PROJECT)">'           >> $@
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
