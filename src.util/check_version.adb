with Ada.Calendar;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with GNAT.Calendar.Time_IO;
with GNAT.Calendar;

with GPR_Tools;

procedure Check_Version is
   pragma Compile_Time_Error (GPR_Tools.VERSION /= $VERSION,
                              "Version missmatch between source and project");
   --  Check that the version number i GPR_Tools.gpr and
   --  GPR_Tools.ads matches.
   --  -------------------------------------------------------------------------

   use Ada.Command_Line;
   use Ada.Strings.Fixed;
   use Ada.Text_IO;
   use Ada.Calendar;
   use GNAT.Calendar.Time_IO;

   Exit_Status : Ada.Command_Line.Exit_Status := Ada.Command_Line.Success;

   pragma Warnings (Off, "condition is always False");
   pragma Warnings (Off, "condition is always True");

   F           : File_Type;
   VERSION_OK  : Boolean := False;
   DATE_OK     : Boolean := False;
   Date        : constant String := Image (Clock, ISO_Date);

   procedure Check (Line : String) is
   begin
      if Index (Line, GPR_Tools.VERSION) > 0 then
         VERSION_OK := True;
         if Index (Line, Date) > 0 then
            DATE_OK := True;
         end if;
      end if;
   end Check;

begin
   Open (F, In_File, "README.md");
   while not End_Of_File (F) loop
      Check (Get_Line (F));
   end loop;
   if (not VERSION_OK) or (not DATE_OK) then
      Exit_Status := Failure;
      if not VERSION_OK then
         Put_Line (Standard_Error, "README.md:1: Version missmatch " & GPR_Tools.VERSION & " not found");
      elsif not DATE_OK then
         Put_Line (Standard_Error, "README.md:1: Date missmatch: " & Image (Clock, ISO_Date) & " not Found");
      end if;
   end if;
   Close (F);

   if Exit_Status = Success then
      Put_Line (GPR_Tools.VERSION);
   end if;

   Ada.Command_Line.Set_Exit_Status (Exit_Status);
end  Check_Version;
