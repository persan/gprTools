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
	gprbuild  -s -p -P ${project}-util.gpr

clean:
	git clean -dfqx

install:
	gprinstall -f -p -P ${project}.gpr --mode=usage  --prefix=${DESTDIR}${PREFIX} -XDevelopment=False

test:compile
	${MAKE} -C tests project=${project}
	rm -rf tss/*
	pkg2gpr  /usr/share/pkgconfig/*.pc  -O tss
	pkg2gpr  /usr/lib64/pkgconfig/*.pc  -O tss
	cd tss; for i in *.gpr; do gprbuild -P  $$i ; done


tag: project:=$(notdir ${project})
tag: test
	check_version
	if [[ ! -z ` git status --porcelain`  ]] ; then \
		echo "Workfolder is not clean";\
		git status ;\
		exit -1;\
	fi
	check_tags  v`check_version`
	git tag -a v`check_version` "-mAuto tag ${project}-v$(shell bin/gprinfo --version)"
	git push --all
	git push --tag
