with GNAT.Command_Line;
with Gprslaves.DB;
with GNAT.String_Split;
with GNAT.Spitbol; use GNAT.Spitbol;
with GNAT.Sockets;
with Gprslaves.Configuration;
use Gprslaves.Configuration;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Gprslaves.Launcher is
   use Ada.Strings.Unbounded;
   use GNAT.String_Split;
   use GNAT.Command_Line;
   Self          : DB.Info_Struct;
   Args          : Unbounded_String;
   Cmd           : Unbounded_String := V ("gprslave");
   Print_Version : Boolean := False;
   Print_Help    : Boolean := False;
begin
   Self.Initialize;
   loop
      case Getopt ("? h -help " &
                     "v V -version " &
                     "j: -jobs= " &
                     "h: -hostname= " &
                     "r: -response-handler= " &
                     "d: -direcory= " &
                     "p: -port= " &
                     "v -verbose " &
                     "vv -debug " &
                     "-cmd= " &
                     "n: -nameserver= " &
                     "D!") is
         when ASCII.NUL => exit;

         when '-' =>
            if Full_Switch = "-version" then
               Append (Args, " --version");
            elsif Full_Switch = "-response-handler" then
               Append (Args, "--response-handler=" & Parameter);
            elsif Full_Switch = "-hostname" then
               Self.Host.Name := V (Parameter);
            elsif Full_Switch = "-direcory" then
               Append (Args, "--direcory=" & Parameter);
            elsif Full_Switch = "-port" then
               Self.Host.Port := DB.Get_Free_Port (GNAT.Sockets.Port_Type'Value (Parameter));
            elsif Full_Switch = "-verbose" then
               Append (Args, "--verbose");
               Configuration.Verbosity := 1;
            elsif Full_Switch = "-debug" then
               Append (Args, "--debug");
               Configuration.Verbosity := 2;
            elsif Full_Switch = "-nameserver" then
               Configuration.Nameserver := V (Parameter);
            elsif Full_Switch = "-cmd" then
               Cmd := V (Parameter);
            elsif Full_Switch = "-help" then
               Append (Args, "--help");
               Print_Help := True;
            end if;

         when 'v' | 'V' =>
            if Full_Switch = "-version" then
               Append (Args, "-v");
               Print_Version := True;
            elsif Full_Switch = "vv" then
               Append (Args, "-vv");
               Configuration.Verbosity := 2;
            end if;
         when 'j' =>
            Append (Args, "-j" & Parameter);
         when 'r' =>
            Append (Args, "-r" & Parameter);
         when 'd' =>
            Append (Args, "-d" & Parameter);
         when 'n' =>
            Configuration.Nameserver := V (Parameter);
         when 'p' =>
            Self.Host.Port := DB.Get_Free_Port (GNAT.Sockets.Port_Type'Value (Parameter));
         when 'D' =>
            declare
               S : GNAT.String_Split.Slice_Set;
            begin
               Create (S, Parameter, "=");
               if Slice_Count (S) = 2 then
                  Self.Append (Slice (S, 1), Slice (S, 2));
               end if;
            end;
         when '?' | 'h' =>
            Append (Args, "-h");
            Print_Help := True;
         when others =>
            raise Program_Error; -- cannot occur
      end case;
   end loop;

   Append (Args, "-p" & Self.Host.Port'Img);
   if Print_Help then
      Put_Line (Configuration.Command & " " & VERSION);
      Put_Line ("-DKey=Value             Defines a key/Value pair for matching");
      Put_Line ("-n   --nameserver=URL   Defines a nameserver (default is read from file '~/.gprslaves').");
      Put_Line ("-h    -hostname=name    Defines to hostname to register.");
   elsif Print_Version then
      Put_Line (Configuration.Command & " " & VERSION);
   end if;
   Trace_Log (1, Cmd & " " & Args);


end Gprslaves.Launcher;
