GNAT_BUILDER ?= gnatmake

include comfignat.mk

export OS_VERSION ?= unix

build_GPRs = build_ahven.gpr
usage_GPRs = ahven.gpr

options = development_build
development_build = false

html: ${stage_miscdocdir}/ahven/
	${MAKE} html --directory=${srcdir}/../doc/manual/en BUILDDIR=${objdir}/sphinx
	cp -RHpf ${objdir}/sphinx/html ${stage_miscdocdir}/ahven

build: html

tests: base ahven_tests.gpr
	${build_GPR}

${stage_miscdocdir}/ahven:
	mkdir -p ${stage_miscdocdir}/ahven

check: tests
	./tester -c

check_xml: tests
	-mkdir -p results
	./tester -c -x -s .Test -d results

check_tap: tests
	./tap_tester

clean::
	rm -f ${builddir}/*tester
