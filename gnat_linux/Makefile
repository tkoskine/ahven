GNAT_BUILDER ?= gprbuild

include comfignat.mk

export OS_VERSION ?= unix

build_GPRs = build_ahven.gpr
usage_GPRs = ahven.gpr

options = development_build
development_build = false

html: ${stage_miscdocdir}/ahven/
	${MAKE} html --directory=${srcdir}/../doc/manual/en BUILDDIR=${objdir}/sphinx
	cp -RHpf ${objdir}/sphinx/html ${stage_miscdocdir}/ahven

docs: html

# The documentation is built as part of make all, but not in the default build.
all: html

tests: base ahven_tests.gpr
	${build_GPR}

${stage_miscdocdir}/ahven:
	mkdir -p ${stage_miscdocdir}/ahven

check: tests
	${builddir}/tester -c

check_xml: tests
	-mkdir -p results
	./tester -c -x -s .Test -d results

check_tap: tests
	./tap_tester

clean::
	rm -f ${builddir}/*tester
