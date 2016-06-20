with GNATCOLL.JSON;
package GPR_Tools.Gprslaves.DB.JSON is

   function Create (Item : Info_Struct) return GNATCOLL.JSON.JSON_Value;
   function Create (Item : Host_Address) return GNATCOLL.JSON.JSON_Value;
   function Create (Item : GNAT.Spitbol.Table_VString.Table) return GNATCOLL.JSON.JSON_Value;
   function Create (Item : Host_Info_Vectors.Vector) return GNATCOLL.JSON.JSON_Value;
   function Create (Item : GNAT.Spitbol.Table_VString.Table_Entry) return GNATCOLL.JSON.JSON_Value;

   function Get (Item : GNATCOLL.JSON.JSON_Value) return Info_Struct;
   function Get (Item : GNATCOLL.JSON.JSON_Value) return Host_Address;
   function Get (Item : GNATCOLL.JSON.JSON_Value) return GNAT.Spitbol.Table_VString.Table;
   function Get (Item : GNATCOLL.JSON.JSON_Value) return Host_Info_Vectors.Vector;
   function Get (Item : GNATCOLL.JSON.JSON_Value) return GNAT.Spitbol.Table_VString.Table_Entry;

end GPR_Tools.Gprslaves.DB.JSON;
