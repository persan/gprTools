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
      return GNATCOLL.JSON.JSON_Array
   is
   begin
      return MyArr : JSON_Array := Empty_Array do
         for Keys of GNAT.Spitbol.Table_VString.Convert_To_Array (Item) loop
            declare
               E : constant JSON_Value := Create_Object;
            begin
               E.Set_Field ("Name", Keys.Name);
               E.Set_Field ("Value", Keys.Value);
               Append (MyArr, E);
            end;
         end loop;
      end return;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Item : Host_Info_Vectors.Vector)
      return GNATCOLL.JSON.JSON_Array
   is
   begin
      return MyArr : JSON_Array := Empty_Array do
         for Value of Item loop
            Append (MyArr, Create (Value));
         end loop;
      end return;
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

   function Get (Item : GNATCOLL.JSON.JSON_Array) return GNAT.Spitbol.Table_VString.Table is
   begin
      return Ret : GNAT.Spitbol.Table_VString.Table (Interfaces.Unsigned_32 (Length (Item))) do
         for Ix in 1 .. Length (Item) loop
            declare
               V : constant GNAT.Spitbol.Table_VString.Table_Entry := Get (Get (Item, Ix));
            begin
               GNAT.Spitbol.Table_VString.Set (Ret, V.Name, V.Value);
            end;
         end loop;
      end return;
   end Get;

   function Get (Item : GNATCOLL.JSON.JSON_Array) return Host_Info_Vectors.Vector is
   begin
      return Ret : Host_Info_Vectors.Vector do
         for Ix in 1 .. Length (Item) loop
            Ret.Append (Get (Get (Item, Ix)));
         end loop;
      end return;
   end Get;
end Gprslaves.DB.JSON;
