with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Text_IO;
package Pkg2gpr is

   use Ada.Strings.Unbounded;
   package String_Vectors is new Ada.Containers.Vectors (Natural, Unbounded_String);
   package String_Maps    is new Ada.Containers.Hashed_Maps (Unbounded_String, Unbounded_String, Hash, "=", "=");

   type Descr is tagged record
      Default_Include  : String_Vectors.Vector;
      Default_Libs     : String_Vectors.Vector;
      Variables        : String_Maps.Map;
      Name             : Unbounded_String;
      FName            : Unbounded_String;
      SrcFile          : Unbounded_String;
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
   procedure Write_GPR (Item : Descr; Output  : Ada.Text_IO.File_Type);


   function Get_Source_Dirs   (Item : Descr) return String_Vectors.Vector;
   function Get_GPR    (Item : Descr) return String;
   function Get_GPR (Item : String) return String;
   function Image (Item : String_Vectors.Vector) return String;
   function Linker_Options (Item : Descr) return String;
   function Get_Include (Cpp : String := "cpp") return String_Vectors.Vector;
   function Get_Libs (Gcc : String := "gcc") return String_Vectors.Vector;
   function Compiler_Default_Switches (Item : Descr) return String_Vectors.Vector;

end Pkg2gpr;
