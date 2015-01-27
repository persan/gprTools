with Ada.Strings.Fixed;
package body Gprslaves is
   use Ada.Streams;
   use Ada.Strings.Fixed;
   use Ada.Strings;
   -----------
   -- Clear --
   -----------

   procedure Clear (Item : out Request) is
   begin
      for I in Item.Ids'Range loop
         Item.Ids (I) := (Key => (others => ' '), Value => (others => ' '));
      end loop;
   end Clear;

   -----------
   -- Image --
   -----------

   function Image (Item : Request) return String is
      pragma Unreferenced (Item);
   begin
      return "jjj";
   end Image;

   ---------
   -- "=" --
   ---------

   function "=" (L, R : Request) return Boolean is
   begin
      for I in L.Ids'Range loop
         if Trim (L.Ids (I).Key, Both) /= "" then
            exit;
         end if;
      end loop;
      return raise Program_Error;
   end "=";

   ------------
   -- Append --
   ------------

   procedure Append (Item : in out Request; Key : String; Value : String) is
      OK : Boolean := False;
   begin
      for I in Item.Ids'Range loop
         if Trim (Item.Ids (I).Key, Both) = "" then
            Ada.Strings.Fixed.Move (Key, Item.Ids (I).Key);
            Ada.Strings.Fixed.Move (Value, Item.Ids (I).Value);
            OK := True;
            exit;
         end if;
      end loop;
      if not OK then
         raise Constraint_Error with "Unable to append " & Key & " => " & Value;
      end if;
   end Append;


   procedure Read (S : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Request) is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. Item'Size / 8) with Address => Item'Address;
   begin
      Ada.Streams.Stream_Element_Array'Read (S, Buffer);
   end Read;
   procedure Write (S : not null access Ada.Streams.Root_Stream_Type'Class; Item : in Request) is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. Item'Size / 8) with Address => Item'Address;
   begin
      Ada.Streams.Stream_Element_Array'Write (S, Buffer);
   end Write;

   procedure Output (S : not null access Ada.Streams.Root_Stream_Type'Class; Item : in Request) is
   begin
      Write (S, Item);
   end Output;
   function Input (S : not null access Ada.Streams.Root_Stream_Type'Class) return  Request is
   begin
      return Ret : Request do
         Read (S, Ret);
      end return;
   end Input;

end Gprslaves;
