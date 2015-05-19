package Gprslaves.Nameserver is

   procedure Register (Server : DB.Info_Struct);

   function Find (KeyValues : String) return String;

end Gprslaves.Nameserver;
