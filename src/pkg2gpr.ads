with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
package Pkg2gpr is
   package String_Vectors is new Ada.Containers.Vectors (Natural, Unbounded_String);
   package String_Maps    is new Ada.Containers.Hashed_Maps (Unbounded_String, Unbounded_String, Hash, "=", "=");
   type Descr is tagged record
      Variables        : String_Maps.Map;
      Name             : Unbounded_String;
      Description      : Unbounded_String;
      URL              : Unbounded_String;
      Version          : Unbounded_String;
      Requires         : String_Vectors.Vector;
      Requires_Private : String_Vectors.Vector;
      Conflicts        : String_Vectors.Vector;
      Cflags           : String_Vectors.Vector;
      Libs             : String_Vectors.Vector;
      Libs_Private     : String_Vectors.Vector;
   end record;

   procedure Read_Pkg  (Item : in out Descr; From : String);
   procedure Write_GPR (Item : Descr; To   : String);
   function Get_GPR    (Item : Descr) return String;
end Pkg2gpr;
