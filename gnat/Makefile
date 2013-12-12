include comfignat.mk

build_GPRs = build_ahven.gpr
usage_GPRs = ahven.gpr

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

clean::
	rm -f ${builddir}/*tester
