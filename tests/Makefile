
project?=$(dir ${CURDIR})gpr_tools.gpr
export PATH:=$(dir ${CURDIR})bin:${PATH}
test:
	@rm -rf .obj
	echo ${PATH}
	gprinfo -P ${project} --languages >out/_language_list
	@cmp out/_language_list golden/_language_list

	@rm -rf .obj
	gprinfo -P test_1.gpr --missing >out/_missing
	@cmp out/_missing golden/_missing

	@rm -rf .obj
	@(if ( gprinfo -P test_1.gpr --Missing >out/_missing ) ; then \
	   echo "Not error exit.";\
           exit -1;\
         fi )
	@cmp out/_missing golden/_missing

	@rm -rf .obj
	gprinfo -P ${project} --imports --basename    >out/_direct_imports
	@cmp out/_direct_imports golden/_direct_imports

	@rm -rf .obj
	gprinfo -P ${project} --imports --basename -r >out/_recursive_imports
	@cmp out/_recursive_imports golden/_recursive_imports

	@rm -rf .obj
	gprinfo -P ${project} --attribute=exec_dir >out/_attributes
	@rm -rf .obj
	gprinfo -P ${project} --attribute=object_dir >>out/_attributes
	@rm -rf .obj
	gprinfo -P ${project} --attribute=Source_Dirs >>out/_attributes
	@rm -rf .obj
	gprinfo -P ${project} "--attribute=Binder.Default_Switches(Ada)" >>out/_attributes
	@cmp out/_attributes golden/_attributes

	@rm -rf .obj
	gprinfo -P  ag.gpr --attribute=Source_Dirs  >out/_agg_sourcdirs
	@rm -rf .obj
	gprinfo -P  ag.gpr --attribute=Object_Dir   >>out/_agg_sourcdirs
	@cmp out/_agg_sourcdirs golden/_agg_sourcdirs

	@rm -rf .obj
	gprinfo -P  p5.gpr -r --contains-lang=c++ | sed "s#${CURDIR}/##" >out/_p5_c++
	@cmp out/_p5_c++ golden/_p5_c++

	@rm -rf .obj
	gprinfo -P  p5.gpr -r --contains-lang=C | sed "s#${CURDIR}/##" >out/_p5_c
	@cmp out/_p5_c golden/_p5_c

	gprinfo -P  p5.gpr -r --contains-lang=ADA | sed "s#${CURDIR}/##" >out/_p5_ada
	@cmp out/_p5_ada golden/_p5_ada
