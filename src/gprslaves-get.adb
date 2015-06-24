with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Gprslaves.DB;
with Gprslaves.DB.JSON;
with GNATCOLL.JSON;
with GNAT.Command_Line;
use GNAT.Command_Line;
with GNAT.String_Split;
with GNAT.Spitbol; use GNAT.Spitbol;
with GNAT.Spitbol.Table_VString; use GNAT.Spitbol.Table_VString;
with Gprslaves.Configuration;
use Gprslaves.Configuration;
procedure Gprslaves.Get is
   use GNAT.String_Split;
   use DB;
   procedure Put (Self : GNAT.Spitbol.Table_VString.Table) is
      J : GNATCOLL.JSON.JSON_Array;
      V : constant GNATCOLL.JSON.JSON_Value := GNATCOLL.JSON.Create (J);
   begin
      Ada.Text_IO.Put_Line (GNATCOLL.JSON.Write (V, False));
   end Put;
   Keys : GNAT.Spitbol.Table_VString.Table (32);
   procedure Help is
   begin
      Put_Line (Configuration.Command & " " & VERSION &
                  "-v --version displa");
   end Help;
begin
   Set (Keys, V ("GNAT"), V (Get_Gnat_Version));
   loop
      case Getopt ("D! -help h ? " &
                     "-version " &
                     "n= -nameserver= " &
                     "v") is
         when ASCII.NUL => exit;

         when '-' =>
            if Full_Switch = "-version" then
               Put_Line (VERSION);
            elsif Full_Switch = "-nameserver" then
               Configuration.Nameserver := V (Parameter);
            end if;
         when 'D' =>
            declare
               S : GNAT.String_Split.Slice_Set;
            begin
               Create (S, Parameter, "=");
               if Slice_Count (S) = 2 then
                  Set (Keys, V (Slice (S, 1)), V (Slice (S, 2)));
               end if;
            end;
         when 'v' =>
            Configuration.Verbosity := Configuration.Verbosity + 1;
         when 'n' =>
            Configuration.Nameserver := V (Parameter);
         when 'h' | '?' =>
            Help;
            return;
         when others =>
            null;
      end case;
   end loop;
   Put_Line (S (Configuration.Nameserver));
   Put (Keys);
end Gprslaves.Get;
