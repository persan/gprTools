gprTools
========
Some handy commandline utilities for .gprfiles.

Version: 1.4.1
- Added command pkg2gpr A tool that generates .gpr-files from .pc files
   to simlplify use of  preinstalled sw.

Version: 1.3.1
- Added support for aggregate projects.

Version: 1.2.0
- Added capability to print any attribute.

Version: 1.1.0
- Added capability to echo/exec several commands.
- Added multiple exclude patterns.
- Iterate the project tree in buildorder and reverse buildorder.

Version: 1.0.5
Added project iterators.

Version: 1.0.4
Build on Linux as well (end of line conventions)

Version: 1.0.2
More tests.

Version: 1.0.1
Added capability to find missing projects

Version: 1.0.0
Initial Version

gpr_tools:
This project contains small utilities to make it simpler to work with
.gpr files and makefiles in concert.
The tool gprinfo displays various Attributes such as:
 - Source_Dirs
 - Main
 - Object_Dir(s)
 - Exec_Dir(s)
 - Languages
 - Imported projects
 - Missing imports.
 - Calculate buildorder for a .gpr project tree.
 the results could ether be echod to standard outup and in
 some cases executed direct.
