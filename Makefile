export project=${CURDIR}/gpr_tools
-include Makefile.config

ifeq (${OS},Windows_NT)
EXE=.exe
endif

all:compile test

Makefile.config : Makefile  #IGNORE
	@echo "PREFIX=$(dir $(shell dirname $(shell which gnatls)))" >$@
	@echo "export PATH=${CURDIR}/bin:${PATH}" >>$@
	@echo "export GPR_PROJECT_PATH=${CURDIR}" >>$@

pretty:
	gnatpp -rf -P ${project}.gpr

compile:
	gprbuild  -s -p -P ${project}.gpr

clean:
	gprclean -P${project}.gpr
	rm -rf Makefile.config gpr_tools-*
	rm -rf `find * -name "*~"`

install:
	gprinstall -f -v -p -P ${project}.gpr --mode=usage  --prefix=${DESTDIR}${PREFIX} -XDevelopment=False

test:compile
	${MAKE} -C tests project=${project}
	rm -rf tss/*
	bin/pkg2gpr  /usr/share/pkgconfig/*.pc  -O tss
	bin/pkg2gpr  /usr/lib64/pkgconfig/*.pc  -O tss
	cd tss; for i in *.gpr; do gprbuild -P  $$i ; done


dist: project:=$(notdir ${project})
dist: test
	if [[ ! -z ` git status --porcelain`  ]] ; then \
		echo "Workfolder is not clean";\
		git status ;\
		exit -1;\
	fi
	rm -rf ${project}-$(shell bin/gprinfo --version) *.tgz

	git tag ${project}-$(shell bin/gprinfo --version) -f
	git clone . ${project}-$(shell bin/gprinfo --version)
	rm ${project}-$(shell bin/gprinfo --version)/.git -rf
	tar -czf ${project}-$(shell bin/gprinfo --version).tgz ${project}-$(shell bin/gprinfo --version)
	rm ${project}-$(shell bin/gprinfo --version) -rf
