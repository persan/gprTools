with GNAT.Spitbol;
with Ada.Directories;
with Ada.Command_Line;
package GPR_Tools.Gprslaves.Configuration is
   use GNAT.Spitbol;

   Nameserver : VString := V (Default_Nameserver);
   Command    : constant String := Ada.Directories.Base_Name (Ada.Command_Line.Command_Name);

   type Verbose_Level is range 0 .. 2;
   Verbosity  : Verbose_Level := 0;

   procedure Trace_Log (Level : Verbose_Level; Message : VString);
   procedure Trace_Log (Level : Verbose_Level; Message : String);

end GPR_Tools.Gprslaves.Configuration;
