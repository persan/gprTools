with Gprslaves.DB;
with GNAT.Spitbol.Table_VString;
package Gprslaves.Nameserver.client is

   procedure Register (Server : DB.Info_Struct);

   function Find (Keys : GNAT.Spitbol.Table_VString.Table) return DB.Host_Info_Vectors.Vector;

end Gprslaves.Nameserver.client;
