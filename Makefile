#
# Copyright (c) 2007, 2008 Tero Koskinen <tero.koskinen@iki.fi>
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

PREFIX?=$(HOME)/libraries/ahven
INSTALL=install
OS_VERSION?=unix

SOURCES=src/ahven-framework.adb src/ahven-framework.ads \
	src/ahven-listeners-basic.adb src/ahven-listeners-basic.ads \
	src/ahven-listeners.ads src/ahven-results.adb \
	src/ahven-results.ads src/ahven-runner.adb \
	src/ahven-runner.ads src/ahven-text_runner.adb \
	src/ahven-text_runner.ads src/ahven.adb \
	src/ahven.ads \
	src/ahven-temporary_output.adb \
	src/ahven-temporary_output.ads \
	src/ahven-parameters.adb src/ahven-parameters.ads \
	src/ahven-xml_runner.adb src/ahven-xml_runner.ads \
	src/${OS_VERSION}/ahven-compat.adb src/ahven-compat.ads

ALI_FILES=lib/ahven.ali \
	lib/ahven-framework.ali lib/ahven-listeners-basic.ali \
	lib/ahven-listeners.ali lib/ahven-results.ali \
	lib/ahven-runner.ali lib/ahven-text_runner.ali \
	lib/ahven-temporary_output.ali \
	lib/ahven-parameters.ali \
	lib/ahven-compat.ali lib/ahven-xml_runner.ali

SO_LIBRARY=libahven.so.14.0
GPR_FILE=ahven.gpr

default: build_all

build_all: build_lib build_tests

build_lib:
	OS_VERSION=$(OS_VERSION) gnatmake -Pahven_lib

build_tests: build_lib
	OS_VERSION=$(OS_VERSION) gnatmake -Pahven_tests

clean: clean_lib clean_tests clean_docs

clean_lib:
	OS_VERSION=$(OS_VERSION) gnatclean -Pahven_lib

clean_tests:
	OS_VERSION=$(OS_VERSION) gnatclean -Pahven_tests
	rm -rf results

clean_docs:
	rm -f doc/api/*.html

install: install_lib

install_lib:
	mkdir -p $(PREFIX)/include/ahven
	mkdir -p $(PREFIX)/lib/ahven
	mkdir -p $(PREFIX)/lib/gnat
	$(INSTALL) -m 644 $(SOURCES) $(PREFIX)/include/ahven
	$(INSTALL) -m 444 $(ALI_FILES) $(PREFIX)/lib/ahven
	$(INSTALL) -m 644 lib/$(SO_LIBRARY) $(PREFIX)/lib/ahven
	ln -sf $(PREFIX)/lib/ahven/$(SO_LIBRARY) $(PREFIX)/lib/libahven.so
	$(INSTALL) -m 644 $(GPR_FILE) $(PREFIX)/lib/gnat

check: build_tests
	./tester -c

check_xml: build_tests
	-mkdir -p results
	./tester -c -x -d results

check_tap: build_tests
	./tap_tester

control:
	rm -f objects/*.adt objects/*.ali
	cd objects && adactl -f ../rules/ahven.aru ../src/*.ad[bs] ../test/*.ad[bs]
	rm -f objects/*.adt objects/*.ali

docs:
	adabrowse -c adabrowse.conf -i -I src/ -f@ahven.specs -o doc/api/

