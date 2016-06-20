with GNAT.Command_Line;
use GNAT.Command_Line;
with GNAT.Strings;
with Ada.Directories;
use Ada.Directories;
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
procedure GPR_Tools.Pkg2gpr.Main is
   OutputFolder : aliased GNAT.Strings.String_Access := new String'(Ada.Directories.Current_Directory);
   Version      : constant String := $VERSION;
   Command_Name : constant String := Ada.Directories.Base_Name (Ada.Command_Line.Command_Name);
   procedure Show (S : String) is
      D : Descr;
   begin
      D.Read_Pkg (S);
      D.Write_GPR (Ada.Directories.Compose (OutputFolder.all, D.Get_GPR));
   end Show;
   procedure Print_Help is
      use ASCII;
   begin
      Put_Line
        (Command_Name & " "  & Version & LF &
           "Syntax:" & LF &
           " " & Command_Name & " [OPtions] pkg-FILEs" & LF &
           "Options:" & LF &
           " -OFolder       Define output folder." & LF &
           " --version      Printversion and exit." & LF &
           " -h -? --help   Print this text.");
   end Print_Help;

begin
   loop
      case GNAT.Command_Line.Getopt ("O: ? h -help") is
         when ASCII.NUL => exit;

         when '?' | 'h' =>
            Print_Help;
            return;
         when '-' =>
            if Full_Switch = "-version" then
               Put_Line (Version);
               return;
            elsif Full_Switch = "-help" then
               Print_Help;
               return;
            end if;
         when 'O' =>
            OutputFolder := new String'(GNAT.Command_Line.Parameter);

         when others =>
            raise Program_Error; -- cannot occur
      end case;
   end loop;
   if not Exists (OutputFolder.all) then
      Create_Path (OutputFolder.all);
   end if;
   loop
      declare
         S : constant String := Get_Argument (Do_Expansion => True);
      begin
         exit when S'Length = 0;
         Show (S);
      end;
   end loop;

exception
   when Invalid_Switch    => Put_Line ("Invalid Switch " & Full_Switch);
   when Invalid_Parameter => Put_Line ("No parameter for " & Full_Switch);
end GPR_Tools.Pkg2gpr.Main;
