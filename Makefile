
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

test: build_tests

docs:
	adabrowse -c adabrowse.conf -i -I src/ -f@ahven.specs -o doc/

