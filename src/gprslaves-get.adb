with Gprslaves.Nameserver_Service.Client;
with Gprslaves.Configuration;
with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
procedure Gprslaves.Get is
   Conf : Ada.Strings.Unbounded.Unbounded_String;

begin
   Ada.Text_IO.Put_Line ("--distributed=" & Nameserver_Service.Client.Find (To_String (Conf), Gprslaves.Configuration.URL));
end Gprslaves.Get;
