
test:
	gprinfo -P ${project} --languages >out/_language_list
	@cmp out/_language_list golden/_language_list

	gprinfo -P test_1.gpr --missing >out/_missing
	@cmp out/_missing golden/_missing

	@(if ( gprinfo -P test_1.gpr --Missing >out/_missing ) ; then \
	   echo "Not error exit.";\
           exit -1;\
         fi )
	@cmp out/_missing golden/_missing

	gprinfo -P ${project} --imports --basename    >out/_direct_imports
	@cmp out/_direct_imports golden/_direct_imports

	gprinfo -P ${project} --imports --basename -r >out/_recursive_imports
	@cmp out/_recursive_imports golden/_recursive_imports

	gprinfo -P ${project} --attribute=exec_dir >out/_attributes
	gprinfo -P ${project} --attribute=object_dir >>out/_attributes
	gprinfo -P ${project} --attribute=Source_Dirs >>out/_attributes
	gprinfo -P ${project} "--attribute=Binder.Default_Switches(Ada)" >>out/_attributes
	@cmp out/_attributes golden/_attributes

	gprinfo -P  ag.gpr --attribute=Source_Dirs  >out/_agg_sourcdirs
	gprinfo -P  ag.gpr --attribute=Object_Dir   >>out/_agg_sourcdirs
	@cmp out/_agg_sourcdirs golden/_agg_sourcdirs
