XML_FILES=ahven-manual.xml using.xml installing.xml introduction.xml

XSL_FO_FILE=ahven.xsl
XSL_HTML_FILE=/usr/local/share/xsl/docbook/html/docbook.xsl

# Params:
#    --stringparam draft.mode yes|no
#    --stringparam fop.extensions 1
#    --stringparam paper.type A4
#    --stringparam glossentry.show.acronym primary
#
PARAMS=--stringparam draft.mode no --stringparam fop.extensions 1 \
	--stringparam paper.type A4 \
	--stringparam glossentry.show.acronym no \
	--stringparam tablecolumns.extension 1
FOP=$(HOME)/java/utilities/fop-0.94/fop

default: ahven-manual.fo

all: ahven-manual.pdf

ahven-manual.pdf: ahven-manual.fo
	$(FOP) -fo ahven-manual.fo -pdf ahven-manual.pdf

ahven-manual.fo: $(XML_FILES) $(XSL_FO_FILE)
	xsltproc --output ahven-manual.fo $(PARAMS) $(XSL_FO_FILE) ahven-manual.xml

ahven-manual.html: $(XML_FILES) $(XSL_HTML_FILE)
	xsltproc --output ahven-manual.html $(XSL_HTML_FILE) ahven-manual.xml

validate:
	xmllint --valid ahven-manual.xml

clean:
	rm -f ahven-manual.fo ahven-manual.pdf

pdf: ahven-manual.pdf

