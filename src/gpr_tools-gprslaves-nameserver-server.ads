with Gprslaves.DB;
with GNAT.Spitbol.Table_VString;
with AWS.Status;
with AWS.Response;
package GPR_Tools.Gprslaves.Nameserver.Server is

   procedure Register (Server : DB.Info_Struct);
   function Find (Keys : GNAT.Spitbol.Table_VString.Table) return DB.Host_Info_Vectors.Vector;
   function Request (Request : AWS.Status.Data) return AWS.Response.Data;

end GPR_Tools.Gprslaves.Nameserver.Server;
