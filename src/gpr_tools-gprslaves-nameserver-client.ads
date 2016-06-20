with Gprslaves.DB;
with GNAT.Spitbol.Table_VString;
package GPR_Tools.Gprslaves.Nameserver.Client is

   procedure Register (Server : DB.Info_Struct);

   function Find (Keys : GNAT.Spitbol.Table_VString.Table) return DB.Host_Info_Vectors.Vector;

end GPR_Tools.Gprslaves.Nameserver.Client;
