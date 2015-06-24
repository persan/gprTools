with GNAT.Command_Line;
use GNAT.Command_Line;
with GNAT.Strings;
with Ada.Directories;
use Ada.Directories;
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
procedure Pkg2gpr.Main is
   OutputFolder : aliased GNAT.Strings.String_Access := new String'(Ada.Directories.Current_Directory);
   Version      : constant String :=  "1.0.0";
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
           " " & Command_Name & " {OPtions} pkg-FILEs" & LF &
           "Options:" & LF &
           "-OFolder   Define output folder" & LF &
           "-h -?      Print this text.");
   end Print_Help;

begin
   loop
      case GNAT.Command_Line.Getopt ("O: ? h") is  -- Accepts '-a', '-ad', or '-b argument'
         when ASCII.NUL => exit;

         when '?' | 'h' =>
            Print_Help;
            return;
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
end Pkg2gpr.Main;
