package Gprslaves.Nameserver is

   procedure Register (Host : String; Port : Natural; Key, Name : String);

   function Find (KeyValues : String) return String;
end Gprslaves.Nameserver;
