with GNAT.Spitbol;
with AWS.Status; use AWS.Status;
with Gprslaves.DB.JSON;
with GNATCOLL.JSON;
with AWS.Messages;
package body Gprslaves.Nameserver.Server is
   Store : Gprslaves.DB.Table;
   --------------
   -- Register --
   --------------

   procedure Register (Server : DB.Info_Struct) is
   begin
      Store.Register (Server.Host, Server.Keys);
   end Register;

   function Find (Keys : GNAT.Spitbol.Table_VString.Table) return DB.Host_Info_Vectors.Vector is
   begin
      return Store.Find (Keys);
   end Find;

   function Request (Request : AWS.Status.Data) return AWS.Response.Data is
   begin
      case AWS.Status.Method (Request) is
         when AWS.Status.PUT =>
            Register (Gprslaves.DB.JSON.Get (GNATCOLL.JSON.Read (AWS.Status.Binary_Data (Request))));
            return AWS.Response.Build ("", "OK");
         when AWS.Status.GET =>
            declare
               Keys : GNAT.Spitbol.Table_VString.Table;
               Hosts : Host_Info_Vectors.Vector;
            begin

                := Find (Gprslaves.DB.JSON.Get (GNATCOLL.JSON.Read (AWS.Status.Binary_Data (Request))));
               return AWS.Response.Build
              ("", "");
            end;
         when others => null;
            return AWS.Response.Build ("", "FAIL", Status_Code => AWS.Messages.S400);
      end case;
   exception
      when others =>
         return AWS.Response.Build ("", "FAIL", Status_Code => AWS.Messages.S400);
   end Request;

end Gprslaves.Nameserver.Server;
