with GNAT.Spitbol;
with AWS.Status; use AWS.Status;
with Gprslaves.DB.JSON;
with GNATCOLL.JSON;
with AWS.Messages;
with Gprslaves.DB; use Gprslaves.DB;
package body GPR_Tools.Gprslaves.Nameserver.Server is
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
      Hosts : Host_Info_Vectors.Vector;
   begin
      case AWS.Status.Method (Request) is
         when AWS.Status.PUT =>
            Register (Gprslaves.DB.JSON.Get (GNATCOLL.JSON.Read (AWS.Status.Binary_Data (Request))));
            return AWS.Response.Build ("", "OK");
         when AWS.Status.GET =>
            Hosts := Find (DB.JSON.Get (GNATCOLL.JSON.Read (AWS.Status.Binary_Data (Request))));
            return AWS.Response.Build ("", String'(GNATCOLL.JSON.Write (DB.JSON.Create (Hosts))));
         when others => null;
            return AWS.Response.Build ("", "FAIL", Status_Code => AWS.Messages.S400);
      end case;
   exception
      when others =>
         return AWS.Response.Build ("", "FAIL", Status_Code => AWS.Messages.S400);
   end Request;

end GPR_Tools.Gprslaves.Nameserver.Server;
