with GNAT.OS_Lib;
with Ada.Directories; use Ada.Directories;
with Ada.Command_Line;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.Strings;
with Ada.Text_IO;use Ada.Text_IO;
package body Gprslaves.Configuration is

   HOME         : constant GNAT.Strings.String_Access := GNAT.OS_Lib.Getenv ("HOME");
   CONFIGFOLDER : constant String := Ada.Directories.Compose (HOME.all, ".gprslaves");
   URL_PATH     :    constant String := Ada.Directories.Compose (CONFIGFOLDER, "URL");


   procedure Q_GNAT is
      FD          : File_Descriptor;
      Name        : String_Access;
      Args        : String_Access_List (1 .. 1) := (new String'("-v"));
      Return_Code : Integer;

   begin
      Create_Temp_File (Fd, Name);
      GNAT.OS_Lib.Spawn ("gnatls", Args, Fd, Return_Code);
      Close (Fd);
   end;
   ---------
   -- URL --
   ---------

   function URL return String is
   begin
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         if Index (Ada.Command_Line.Argument (I), "--url=") = Ada.Command_Line.Argument (I)'First then
            return Ada.Command_Line.Argument (I) (Ada.Command_Line.Argument (I)'First + 6 .. Ada.Command_Line.Argument (I)'Last);
         end if;
      end loop;
      if Exists (URL) then
         declare
            F      : Ada.Text_IO.File_Type;
            Buffer : String (1 .. 1024);
            Last   : Natural;
         begin
            Open (F, In_File, URL_PATH);
            while not End_Of_File (F) loop
               Get_Line (F, Buffer, Last);
               if Index (Buffer (Buffer'First .. Last), "http") = Buffer'First then
                  return Buffer (Buffer'First .. Last);
               end if;
            end loop;
         end;
      end if;
      return "http://localhost:8023";
   end URL;
end Gprslaves.Configuration;
