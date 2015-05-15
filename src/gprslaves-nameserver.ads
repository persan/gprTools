package Gprslaves.Nameserver is
   subtype Key_String is String (1 .. 64);
   subtype Value_String is String (1 .. 128);
   type KeyValue is record
      Key   : Key_String;
      Value : Value_String;
   end record;
   procedure Register (Host : String; Port : Natural; Key, Name : String);

   function Find (KeyValues : String) return String;

end Gprslaves.Nameserver;
