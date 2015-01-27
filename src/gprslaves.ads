with GNAT.Sockets; use GNAT.Sockets;
with Ada.Streams;
package Gprslaves is
   DEFAULT_PORT : constant := 55555;
   Multicast_Group        : constant String := "224.0.0.1";
   type Response is record
      Address : Sock_Addr_Type;
   end record;
   type Id_Type is record
      Key : String (1 .. 32) := (others => ' ');
      Value : String (1 .. 64) := (others => ' ');
   end record;
   type ID_Array is array (Natural range <>) of aliased Id_Type;
   type Request is record
      Ids : ID_Array (1 .. 1500 / (Id_Type'Size / 8) - 1);
   end record with Read => Read, Input => Input, Write => Write, Output => Output;
   procedure Read (S : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Request);
   procedure Write (S : not null access Ada.Streams.Root_Stream_Type'Class; Item : in Request);
   procedure Output (S : not null access Ada.Streams.Root_Stream_Type'Class; Item : in Request);
   function Input (S : not null access Ada.Streams.Root_Stream_Type'Class) return  Request;

   procedure Clear (Item : out Request);
   function Image (Item : Request) return String;
   function "="   (L, R : Request) return Boolean;
   procedure Append (Item : in out Request; Key : String; Value : String);
end Gprslaves;
