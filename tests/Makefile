
test:
	gprinfo -P ${project} --languages >_language_list.out
	@cmp _language_list.out _language_list.golden

	gprinfo -P test_1.gpr --missing >_missing.out
	@cmp _missing.out _missing.golden

	@(if ( gprinfo -P test_1.gpr --Missing >_missing.out ) ; then \
	   echo "Not error exit.";\
           exit -1;\
         fi )
	@cmp _missing.out _missing.golden

	gprinfo -P ${project} --imports --basename    >_direct_imports.out
	@cmp _direct_imports.out _direct_imports.golden

	gprinfo -P ${project} --imports --basename -r >_recursive_imports.out
	@cmp _recursive_imports.out _recursive_imports.golden

	gprinfo -P ${project} --attribute=exec_dir >_attributes.out
	gprinfo -P ${project} --attribute=object_dir >>_attributes.out
	gprinfo -P ${project} --attribute=Source_Dirs >>_attributes.out
	gprinfo -P ${project} "--attribute=Binder.Default_Switches(Ada)" >>_attributes.out
	@cmp _attributes.out _attributes.golden

