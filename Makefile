#
# Copyright (c) 2007-2014 Tero Koskinen <tero.koskinen@iki.fi>
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

PREFIX?=/usr/local
OPTS?="GNAT_BUILDER=gnatmake" OS_VERSION=unix prefix=$(PREFIX)

default: base

all:
	cd gnat && $(MAKE) $(OPTS) all

check:
	cd gnat && $(MAKE) $(OPTS) check

check_xml:
	cd gnat && $(MAKE) $(OPTS) check_xml

check_tap:
	cd gnat && $(MAKE) $(OPTS) check_tap


html:
	cd gnat && $(MAKE) html

install:
	cd gnat && $(MAKE) $(OPTS) install

base:
	cd gnat && $(MAKE) $(OPTS) base

clean:
	cd gnat && $(MAKE) $(OPTS) clean

README.html: README.rst
	rst2html --stylesheet-path=css/html4css1.css,css/my-docutils.css README.rst > README.html

control:
	rm -f objects/*.adt objects/*.ali
	cd objects && adactl -f ../rules/ahven.aru ../src/*.ad[bs] ../test/*.ad[bs] ../src/unix/*.ad[bs]
	rm -f objects/*.adt objects/*.ali
