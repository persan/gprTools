with Ada.Text_IO;
with GNAT.OS_Lib;
package body Gprslaves.Configuration is

   HOME : GNAT.OS_Lib.String_Access := GNAT.OS_Lib.Getenv ("HOME");
   Config_File_Path : constant String := Ada.Directories.Compose (HOME.all, Config_File_Name);
   ---------------
   -- Trace_Log --
   ---------------

   procedure Trace_Log (Level : Verbose_Level; Message : VString) is
   begin
      Trace_Log (Level => Level, Message =>  S (Message));
   end Trace_Log;

   ---------------
   -- Trace_Log --
   ---------------

   procedure Trace_Log (Level : Verbose_Level; Message : String) is
   begin
      if Level > Verbosity then
         Ada.Text_IO.Put_Line (Message);
      end if;
   end Trace_Log;
   procedure Read (F : String) is

begin
   if Exists (Config_File_Name) then
      Read (Config_File_Name);
   elsif Exists (Config_File_Path)  then
      Read (Config_File_Path);
   end if;
end Gprslaves.Configuration;
