with GNAT.Sockets;
with GNAT.Spitbol.Table_VString;
with Ada.Containers.Vectors;
package Gprslaves.DB is

   type Host_Address is record
      Name : GNAT.Spitbol.VString;
      Port : GNAT.Sockets.Port_Type;
   end record;

   package Host_Info_Vectors is new
     Ada.Containers.Vectors (Natural, Host_Address);

   type Info_Struct is tagged record
      Host : Host_Address;
      Keys : GNAT.Spitbol.Table_VString.Table (32);
   end  record;

   function Get_Free_Port (Default : GNAT.Sockets.Port_Type := 8484) return GNAT.Sockets.Port_Type;

   function Get_Gnat_Version return String;

   procedure Initialize (Self     : in out Info_Struct;
                         HostName : String := GNAT.Sockets.Host_Name;
                         Port     : GNAT.Sockets.Port_Type := Get_Free_Port);

   procedure Append (Self : in out Info_Struct;
                     Key_Name  : String;
                     Key_Value : String);

   type Table is tagged private;

   procedure Register (Self   : in out Table;
                       Host   : Host_Address;
                       Keys   : GNAT.Spitbol.Table_VString.Table);

   function Find (Self   : Table;
                  Keys   : GNAT.Spitbol.Table_VString.Table)
                  return Host_Info_Vectors.Vector;
private
   package Host_Vectors is new Ada.Containers.Vectors (Natural, Info_Struct);
   type Table is tagged record
      Hosts : Host_Vectors.Vector;
   end record;

end Gprslaves.DB;
