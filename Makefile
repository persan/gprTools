project=gpr_tools

sinclude Makefile.config

ifeq (${OS},Windows_NT)
EXE=.exe
endif

all:compile

bin/version${EXE}: #IGNORE
	gprbuild  -p -P ${project} version.adb

Makefile.config : Makefile  bin/version${EXE} #IGNORE
	echo PREFIX=$(dir $(shell dirname $(shell which gnatls))) >$@
	echo VERSION=$(shell bin/version${EXE}) >>$@

compile:
	gprbuild  -s -p -P ${project}.gpr

clean:
	gprclean -P${project}.gpr
	rm -rf Makefile.config gpr_tools-*
install:
	gprinstall -f -v -p -P ${project}.gpr --mode=usage  --prefix=${DESTDIR}${PREFIX} -XDevelopment=False

test:compile
	bin/gprinfo -P ${project}.gpr --languages >_language_list.out
	cmp _language_list.out _language_list.golden
	bin/gprinfo -P test_1.gpr --missing >_missing.out
	cmp _missing.out _missing.golden

	@(if ( bin/gprinfo -P test_1.gpr --Missing >_missing.out ) ; then \
           exit -1;\
         fi )
	cmp _missing.out _missing.golden

	bin/gprinfo -P gpr_tools --imports --basename    >_direct_imports.out
	cmp _direct_imports.out _direct_imports.golden

	bin/gprinfo -P gpr_tools --imports --basename -r >_recursive_imports.out
	cmp _recursive_imports.out _recursive_imports.golden

dist:test
	if [[ ! -z ` git status --porcelain`  ]] ; then \
		echo "Workfolder is not clean";\
		git status ;\
		exit -1;\
	fi
	rm -rf ${project}-${VERSION}
	git tag ${project}-${VERSION} -f
	git clone . ${project}-${VERSION}
	rm ${project}-${VERSION}/.git -rf
	tar -czf ${project}-${VERSION}.tgz ${project}-${VERSION}
	rm ${project}-${VERSION} -rf




