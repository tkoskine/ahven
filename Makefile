
default: build_all

build_all: build_lib build_tests

build_lib:
	gnatmake -Pahven

build_tests: build_lib
	gprmake -Pahven_tests

clean: clean_lib clean_tests

clean_lib:
	gnatclean -Pahven

clean_tests:
	gnatclean -Pahven_tests

test: build_tests
	

