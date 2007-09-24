
PREFIX?=$(HOME)/libraries/ahven
INSTALL=install

SOURCES=src/ahven-double_linked_list.adb src/ahven-double_linked_list.ads \
	src/ahven-framework.adb src/ahven-framework.ads \
	src/ahven-listeners-basic.adb src/ahven-listeners-basic.ads \
	src/ahven-listeners.ads src/ahven-results.adb \
	src/ahven-results.ads src/ahven-runner.adb \
	src/ahven-runner.ads src/ahven-text_runner.adb \
	src/ahven-text_runner.ads src/ahven.adb \
	src/ahven.ads

ALI_FILES=lib/ahven.ali lib/ahven-double_linked_list.ali \
	lib/ahven-framework.ali lib/ahven-listeners-basic.ali \
	lib/ahven-listeners.ali lib/ahven-results.ali \
	lib/ahven-runner.ali lib/ahven-text_runner.ali

SO_LIBRARY=lib/libahven.so
GPR_FILE=ahven_lib.gpr

default: build_all

build_all: build_lib build_tests

build_lib:
	gnatmake -Pahven

build_tests: build_lib
	gprmake -Pahven_tests

clean: clean_lib clean_tests clean_docs

clean_lib:
	gnatclean -Pahven

clean_tests:
	gnatclean -Pahven_tests

clean_docs:
	rm -f doc/*.html

install: install_lib

install_lib:
	mkdir -p $(PREFIX)/include/ahven
	mkdir -p $(PREFIX)/lib/ahven
	$(INSTALL) -m 644 $(SOURCES) $(PREFIX)/include/ahven
	$(INSTALL) -m 644 $(ALI_FILES) $(PREFIX)/lib/ahven
	$(INSTALL) -m 644 $(SO_LIBRARY) $(PREFIX)/lib/ahven
	$(INSTALL) -m 644 $(GPR_FILE) $(PREFIX)/lib/gnat

test: build_tests

docs:
	adabrowse -c adabrowse.conf -i -I src/ -f@ahven.specs -o doc/

