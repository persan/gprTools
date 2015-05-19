with GNATCOLL.JSON;
package Gprslaves.DB.JSON is

   function Create (Item : Info_Struct) return GNATCOLL.JSON.JSON_Value;
   function Create (Item : Host_Address) return GNATCOLL.JSON.JSON_Value;
   function Create (Item : GNAT.Spitbol.Table_VString.Table) return GNATCOLL.JSON.JSON_Array;
   function Create (Item : Host_Info_Vectors.Vector) return GNATCOLL.JSON.JSON_Array;


   function Get (Item : GNATCOLL.JSON.JSON_Value) return Info_Struct;
   function Get (Item : GNATCOLL.JSON.JSON_Value) return Host_Address;
   function Get (Item : GNATCOLL.JSON.JSON_Array) return GNAT.Spitbol.Table_VString.Table;
   function Get (Item : GNATCOLL.JSON.JSON_Array) return Host_Info_Vectors.Vector;

end Gprslaves.DB.JSON;
