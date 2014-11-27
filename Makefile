project=gpr_tools

sinclude Makefile.config

ifeq (${OS},Windows_NT)
EXE=.exe
endif

all:compile

Makefile.config : Makefile  bin/version${EXE} #IGNORE
	echo PREFIX=$(dir $(shell dirname $(shell which gnatls))) >$@

compile:
	gprbuild  -s -p -P ${project}.gpr

clean:
	gprclean -P${project}.gpr
	rm -rf Makefile.config gpr_tools-*
	rm -rf `find * -name "*~"`

install:
	gprinstall -f -v -p -P ${project}.gpr --mode=usage  --prefix=${DESTDIR}${PREFIX} -XDevelopment=False

test:compile
	bin/gprinfo -P ${project}.gpr --languages >_language_list.out
	@cmp _language_list.out _language_list.golden

	bin/gprinfo -P test_1.gpr --missing >_missing.out
	@cmp _missing.out _missing.golden

	@(if ( bin/gprinfo -P test_1.gpr --Missing >_missing.out ) ; then \
	   echo "Not error exit.";\
           exit -1;\
         fi )
	@cmp _missing.out _missing.golden

	bin/gprinfo -P gpr_tools --imports --basename    >_direct_imports.out
	@cmp _direct_imports.out _direct_imports.golden

	bin/gprinfo -P gpr_tools --imports --basename -r >_recursive_imports.out
	@cmp _recursive_imports.out _recursive_imports.golden

	bin/gprinfo -P gpr_tools --attribute=exec_dir >_attributes.out
	bin/gprinfo -P gpr_tools --attribute=object_dir >>_attributes.out
	bin/gprinfo -P gpr_tools --attribute=Source_Dirs >>_attributes.out
	bin/gprinfo -P gpr_tools "--attribute=Binder.Default_Switches(Ada)" >>_attributes.out
	@cmp _attributes.out _attributes.golden

dist:test
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
