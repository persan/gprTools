with GNAT.Spitbol; use GNAT.Spitbol;
with Interfaces; use Interfaces;
package body Gprslaves.DB.JSON is
   use GNATCOLL.JSON;
   ------------
   -- Create --
   ------------

   function Create (Item : Info_Struct) return GNATCOLL.JSON.JSON_Value is
   begin
      return Ret : constant JSON_Value := Create_Object do
         Ret.Set_Field ("Host", Create (Item.Host));
         Ret.Set_Field ("Keys", Create (Item.Keys));
      end return;
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Item : Host_Address) return GNATCOLL.JSON.JSON_Value is
   begin
      return Ret : constant JSON_Value := Create_Object do
         Ret.Set_Field ("Name", Create (Item.Name));
         Ret.Set_Field ("Port", Create (Integer (Item.Port)));
      end return;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Item : GNAT.Spitbol.Table_VString.Table)
      return GNATCOLL.JSON.JSON_Value
   is
      MyArr : JSON_Array := Empty_Array;
   begin
      for Key of GNAT.Spitbol.Table_VString.Convert_To_Array (Item) loop
         Append (MyArr, Create (Key));
      end loop;
      return Create (MyArr);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Item : Host_Info_Vectors.Vector)
      return GNATCOLL.JSON.JSON_Value
   is
      MyArr : JSON_Array := Empty_Array;
   begin
      for Value of Item loop
         Append (MyArr, Create (Value));
      end loop;
      return Create (MyArr);
   end Create;



   function Get (Item : GNATCOLL.JSON.JSON_Value) return Info_Struct is
   begin
      return Ret : Info_Struct do
         Ret.Host :=  Get (Get (Item, "Host"));
      end return;
   end Get;

   function Get (Item : GNATCOLL.JSON.JSON_Value) return Host_Address is
   begin
      return Ret : Host_Address do
         Ret.Name :=  V (String'(Get (Item, "Name")));
         Ret.Port :=  GNAT.Sockets.Port_Type (Natural'(Get (Item, "Port")));
      end return;
   end Get;


   function Get (Item : GNATCOLL.JSON.JSON_Value) return GNAT.Spitbol.Table_VString.Table_Entry is
   begin
      return Ret : GNAT.Spitbol.Table_VString.Table_Entry do
         Ret.Name :=  V (String'(Get (Item, "Name")));
         Ret.Value :=  V (String'(Get (Item, "Value")));
      end return;
   end Get;

   function Get (Item : GNATCOLL.JSON.JSON_Value) return GNAT.Spitbol.Table_VString.Table is
      Arr : constant GNATCOLL.JSON.JSON_Array := Get (Item);
   begin
      return Ret : GNAT.Spitbol.Table_VString.Table (Interfaces.Unsigned_32 (Length (Arr))) do
         for Ix in 1 .. Length (Arr) loop
            declare
               V : constant GNAT.Spitbol.Table_VString.Table_Entry := Get (Get (Arr, Ix));
            begin
               GNAT.Spitbol.Table_VString.Set (Ret, V.Name, V.Value);
            end;
         end loop;
      end return;
   end Get;

   function Get (Item : GNATCOLL.JSON.JSON_Value) return Host_Info_Vectors.Vector is
      Arr : constant GNATCOLL.JSON.JSON_Array := Get (Item);
   begin
      return Ret : Host_Info_Vectors.Vector do
         for Ix in 1 .. Length (Arr) loop
            Ret.Append (Host_Address'(Get (Get (Arr, Ix))));
         end loop;
      end return;
   end Get;

   function Create (Item : GNAT.Spitbol.Table_VString.Table_Entry) return GNATCOLL.JSON.JSON_Value is
   begin
      return raise Program_Error;
   end Create;

end Gprslaves.DB.JSON;
