with Ada.Text_IO;
with GNAT.Regpat;
with GNAT.Case_Util;
with GNAT.String_Split;
package body Pkg2gpr is
   use Ada.Text_IO;
   use GNAT.Regpat;
   use GNAT.String_Split;
   --------------
   -- Read_Pkg --
   --------------
   Matcher    : constant GNAT.Regpat.Pattern_Matcher :=
                  GNAT.Regpat.Compile ("^((\w+)=(\S+))" &
                                                    "|(Name):(.+)" &
                                         "|(Description):\s*(\S.+)" &
                                         "|(URL):\s*(\S.+)" &
                                         "|(Version):\s*(\S.+)" &
                                         "|(Requires):\s*(\S.+)" &
                                         "|(Requires.private):\s*(\S.+)" &
                                         "|(Conflicts):\s*(\S.+)" &
                                         "|(Cflags):\s*(\S.+)" &
                                         "|(Libs):\s*(\S.+)" &
                                         "|(Libs.private):\s*(\S.+)");
   VarMatcher : GNAT.Regpat.Pattern_Matcher :=
                  GNAT.Regpat.Compile ("${(\w+)}");

   procedure Read_Pkg (Item : in out Descr; From : String) is

      function Expand (Src : String) return String is
      begin
         return  Src;
      end Expand;

      function Expand (Src : String) return Unbounded_String is
      begin
         return To_Unbounded_String (Expand (Src));
      end Expand;

      function Expand (Src : String) return String_Vectors.Vector is
         S : GNAT.String_Split.Slice_Set;
      begin
         return Ret : String_Vectors.Vector do
            GNAT.String_Split.Create (S, Src, " ");
            for I in 1 .. GNAT.String_Split.Slice_Count (S) loop
               Ret.Append (Unbounded_String'(Expand (GNAT.String_Split.Slice (S, I))));
            end loop;
         end return;
      end Expand;


      procedure Parse (L : String) is
         Matches : Match_Array (0 .. Paren_Count (Matcher));
      begin
         Match (Matcher, Data => L, Matches => Matches);
         if Matches (0) /= No_Match then
            for I in Matches'Range loop
               if Matches (I) /= No_Match then
                  Put_Line
                    (I'Img & "=> (" & Matches (I).First'Img & ","
                     & Matches (I).Last'Img & ")" &
                       L (Matches (I).First .. Matches (I).Last));
               end if;
            end loop;
            if Matches (2) /= No_Match then -- Variable
               Item.Variables.Include
                 (Key      => To_Unbounded_String (L (Matches (2).First .. Matches (2).Last)),
                  New_Item => Expand (L (Matches (3).First .. Matches (3).Last)));

            elsif Matches (4) /= No_Match then -- Name
               Item.Name := Expand (L (Matches (5).First .. Matches (5).Last));
            elsif Matches (6) /= No_Match then -- Description
               Item.Description := Expand (L (Matches (7).First .. Matches (7).Last));
            elsif Matches (8) /= No_Match then -- URL
               Item.URL := Expand (L (Matches (9).First .. Matches (9).Last));
            elsif Matches (10) /= No_Match then -- Version
               Item.Version := Expand (L (Matches (11).First .. Matches (11).Last));
            elsif Matches (12) /= No_Match then -- Requires
               Item.Requires := Expand (L (Matches (13).First .. Matches (13).Last));
            elsif Matches (14) /= No_Match then -- Requires.private
               Item.Requires_Private := Expand (L (Matches (15).First .. Matches (15).Last));
            elsif Matches (16) /= No_Match then -- Conflicts
               Item.Conflicts := Expand (L (Matches (17).First .. Matches (17).Last));
            elsif Matches (18) /= No_Match then -- Cflags
               Item.Cflags := Expand (L (Matches (19).First .. Matches (19).Last));
            elsif Matches (20) /= No_Match then -- Libs
               Item.Libs := Expand (L (Matches (21).First .. Matches (21).Last));
            elsif Matches (22) /= No_Match then -- Libs.private
               Item.Libs_Private := Expand (L (Matches (23).First .. Matches (23).Last));
            end if;
         end if;
      end Parse;

      F : Ada.Text_IO.File_Type;
   begin
      Open (F, In_File, From);
      while not End_Of_File (F) loop
         Parse (Get_Line (F));
      end loop;
      Close (F);
   end Read_Pkg;

   function Pkg2Ada_Name (Src : Unbounded_String) return String is
      S      : constant String := To_String (Src);
      Ret    : String (S'Range);
      Cursor : Natural := Ret'First;
   begin
      for C of S  loop
         if C in '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_' then
            Ret (Cursor) := C;
            Cursor := Cursor + 1;
         elsif Cursor > Ret'First and then Ret (Cursor - 1) /= '_' then
            Ret (Cursor) := '_';
            Cursor := Cursor + 1;
         end if;
      end loop;
      if Cursor > Ret'First and then Ret (Cursor - 1) = '_' then
         Cursor :=  Cursor - 1;
      end if;
      return Ret (Ret'First .. Cursor - 1);
   end Pkg2Ada_Name;
   ---------------
   -- Write_GPR --
   ---------------

   procedure Write_GPR (Item : Descr; To   : String) is

   begin
      Put_Line (" < " & To & " >");
      Put_Line (Standard_Output, "project " & Pkg2Ada_Name (Item.Name) & " is");
      if Length (Item.Description) /= 0 then
         Put_Line (Standard_Output, "  Description := """ & To_String (Item.Description) & """;");
      end if;
      if Length (Item.URL) /= 0 then
         Put_Line (Standard_Output, "  URL := """ & To_String (Item.URL) & """;");
      end if;
      if Length (Item.Version) /= 0 then
         Put_Line (Standard_Output, "  Version := """ & To_String (Item.Version) & """;");
      end if;
      Put_Line (Standard_Output, "end " & Pkg2Ada_Name (Item.Name) & ";");
   end Write_GPR;

   -------------
   -- Get_GPR --
   -------------

   function Get_GPR (Item : Descr) return String is
      Ret : String := Pkg2Ada_Name (Item.Name);
   begin
      GNAT.Case_Util.To_Lower (Ret);
      return Ret & ".gpr";
   end Get_GPR;

end Pkg2gpr;
