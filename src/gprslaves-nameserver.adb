with GNAT.String_Split;
package body Gprslaves.Nameserver is

   --------------
   -- Register --
   --------------

   procedure Register (Host : String; Port : Natural; Key, Name : String) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Register unimplemented");
      raise Program_Error with "Unimplemented procedure Register";
   end Register;

   function Find (KeyValues : String) return String is

   begin
      return "localhost:5555";
   end Find;

end Gprslaves.Nameserver;
