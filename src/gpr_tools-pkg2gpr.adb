with GNAT.Regpat;
with GNAT.Case_Util;
with GNAT.String_Split;
with GNATCOLL.Templates;
with GNAT.OS_Lib;
with Ada.Strings.Fixed;
with Ada.Directories;
use Ada.Directories;
with Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
package body GPR_Tools.Pkg2gpr is
   use Ada.Text_IO;
   use GNAT.Regpat;
   use GNAT.String_Split;
   use Ada.Containers;
   use GNAT.OS_Lib;
   use Ada.Strings;
   use Ada.Strings.Fixed;
   --------------
   -- Read_Pkg --
   --------------
   Matcher    : constant GNAT.Regpat.Pattern_Matcher :=
                  GNAT.Regpat.Compile ("^((\w+)\s*=\s*(.*))" &
                                                    "|^(Name):(.+)" &
                                         "|^(Description):\s*(\S.+)" &
                                         "|^(URL):\s*(\S.+)" &
                                         "|^(Version):\s*(\S.+)" &
                                         "|^(Requires):\s*(\S.+)" &
                                         "|^(Requires.private):\s*(\S.+)" &
                                         "|^(Conflicts):\s*(\S.+)" &
                                         "|^(Cflags):\s*(\S.+)" &
                                         "|^(Libs):\s*(\S.+)" &
                                         "|^(Libs.private):\s*(\S.+)");

   function Compiler_Default_Switches (Item : Descr) return String_Vectors.Vector is

   begin
      return Ret : String_Vectors.Vector do
         for I of Item.Cflags loop
            if Index (I, "-I") /= 1 then
               Ret.Append (I);
            end if;
         end loop;
      end return;
   end Compiler_Default_Switches;

   function Linker_Options (Item : Descr) return String is
      Ret : Unbounded_String;
      function Is_Std (S : String) return Boolean is
      begin
         if Index (S, "-L") = S'First then
            if Item.Default_Libs.Contains (To_Unbounded_String (S (S'First + 2 .. S'Last))) then
               return True;
            end if;
         end if;
         return False;
      end Is_Std;

   begin
      Append (Ret, "(");
      for I in Item.Libs.First_Index ..  Item.Libs.Last_Index loop
         if not Is_Std (To_String (Item.Libs.Element (Integer (I)))) then
            Append (Ret, """");
            Append (Ret, Item.Libs.Element (Integer (I)));
            Append (Ret, """");
            if I < Item.Libs.Last_Index then
               Append (Ret, ", ");
            end if;
         end if;
      end loop;
      Append (Ret, ")");
      return To_String (Ret);
   end Linker_Options;


   function Image (Item : String_Vectors.Vector) return String is
      Ret : Unbounded_String;
   begin
      Append (Ret, "(");
      for I in Item.First_Index ..  Item.Last_Index loop
         Append (Ret, """");
         Append (Ret, Item.Element (Integer (I)));
         Append (Ret, """");
         if I < Item.Last_Index then
            Append (Ret, ", ");
         end if;
      end loop;
      Append (Ret, ")");
      return To_String (Ret);
   end Image;

   procedure Read_Pkg (Item : in out Descr; From : String) is

      function Expand (S : String) return String is
         Vars   : GNATCOLL.Templates.Substitution_Array (1 .. Integer (Item.Variables.Length));
         Cursor : Natural := 1;
         Src    : String := S;
      begin
         while Index (Src, "\""") > 0 loop
            Src (Index (Src, "\""")) := '"';
         end loop;
         for C in Item.Variables.Iterate loop
            Vars (Cursor) := (Name  => new String'(To_String (String_Maps.Key (C))),
                              Value => new String'(To_String (String_Maps.Element (C))));
            Cursor := Cursor + 1;
         end loop;
         return Ret : constant String := GNATCOLL.Templates.Substitute (Src, Vars, Delimiter => '$', Recursive => True) do
            GNATCOLL.Templates.Free (Vars);
         end return;
      end Expand;

      function Expand (Src : String) return Unbounded_String is
      begin
         return To_Unbounded_String (Expand (Trim (Trim (Src, To_Set ("""'"), To_Set ("""'")), To_Set (" "), To_Set (" "))));
      end Expand;

      function Expand (Src : String) return String_Vectors.Vector is
         S    : GNAT.String_Split.Slice_Set;
         Temp : constant String := Trim (Src, To_Set ("""'"), To_Set ("""'"));
         function Strip_Quotes (S : Unbounded_String) return Unbounded_String is
            Ret : Unbounded_String;
            C   : Character;
         begin
            for Ix  in 1 ..  Length (S) loop
               C := Element (S, Ix);
               if C not in ''' | '"' | '#' then
                  Append (Ret, C);
               end if;
            end loop;
            return Ret;
         end Strip_Quotes;
      begin
         return Ret : String_Vectors.Vector do
            GNAT.String_Split.Create (S, Expand (Temp), " ,");
            for I in 1 .. GNAT.String_Split.Slice_Count (S) loop
               declare
                  Name : constant Unbounded_String := Expand (GNAT.String_Split.Slice (S, I));
               begin
                  if Length (Name) > 0 then
                     Ret.Append (Strip_Quotes (Name));
                  end if;
               end;
            end loop;
         end return;
      end Expand;


      procedure Parse (L : String) is
         Matches : Match_Array (0 .. Paren_Count (Matcher));
      begin
         Match (Matcher, Data => L, Matches => Matches);
         if Matches (0) /= No_Match then
            if Matches (2) /= No_Match then -- Variable
               Item.Variables.Include
                 (Key      => To_Unbounded_String (L (Matches (2).First .. Matches (2).Last)),
                  New_Item => Expand (L (Matches (3).First .. Matches (3).Last)));

            elsif Matches (4) /= No_Match then -- Name
               Item.Name := Trim (Expand (L (Matches (5).First .. Matches (5).Last)), Both);
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
      Item.Variables.Include (To_Unbounded_String ("pcfiledir"), To_Unbounded_String (Containing_Directory (From)));
      Item.Default_Include := Get_Include;
      Item.Default_Libs    := Get_Libs;
      Item.FName           := To_Unbounded_String (Base_Name (From));
      Item.SrcFile         := To_Unbounded_String (From);
      Open (F, In_File, From);
      while not End_Of_File (F) loop
         Parse (Get_Line (F));
      end loop;
      Close (F);
   end Read_Pkg;


   function Pkg2Ada_Name (Src : String) return String is
      Ret    : String (Src'Range);
      Cursor : Natural := Ret'First;
   begin
      for C of Src  loop
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
   function Pkg2Ada_Name (Src : Unbounded_String) return String is
   begin
      return Pkg2Ada_Name (To_String (Src));
   end Pkg2Ada_Name;

   ---------------
   -- Write_GPR --
   ---------------
   procedure Write_GPR (Item : Descr; To   : String) is
      Outf : Ada.Text_IO.File_Type;
   begin
      Create (Outf, Out_File, To);
      Item.Write_GPR (Outf);
      Close (Outf);
   end Write_GPR;

   procedure Write_GPR (Item : Descr; Output  : Ada.Text_IO.File_Type) is

   begin

      if Item.Requires.Length /= 0 then
         for I in Item.Requires.First_Index ..  Item.Requires.Last_Index loop
            declare
               Name : constant String := To_String (Item.Requires.Element (Integer (I)));
            begin
               if Name'Length > 1 and then Name (Name'First) not in '<' | '>' | '=' | '0' .. '9' then
                  Put (Output, "with """);
                  Put (Output, Get_GPR (Name));
                  Put_Line (Output, """;");
               end if;
            end;
         end loop;
         New_Line (Output);
      end if;
      Put_Line (Output, "project " & Pkg2Ada_Name (Item.FName) & " is");
      New_Line (Output);
      Put_Line (Output, "   for Languages use (""C"");");
      New_Line (Output);

      if Length (Item.Name) /= 0 then
         Put_Line (Output, "   Name           := """ & To_String (Item.Name) & """;");
      end if;


      if Length (Item.Description) /= 0 then
         Put_Line (Output, "   Description    := """ & To_String (Item.Description) & """;");
      end if;
      if Length (Item.Version) /= 0 then
         Put_Line (Output, "   Version        := """ & To_String (Item.Version) & """;");
      end if;

      if Length (Item.URL) /= 0 then
         Put_Line (Output, "   URL            := """ & To_String (Item.URL) & """;");
      end if;
      New_Line (Output);

      if Length (Item.SrcFile) /= 0 then
         Put_Line (Output, "   Source_Pc_File := """ & To_String (Item.SrcFile) & """;");
      end if;
      New_Line (Output);

      if Item.Variables.Length > 0 then

         declare
            Max_Name_Length : Natural := 0;
         begin
            for C in Item.Variables.Iterate loop
               Max_Name_Length := Natural'Max (Max_Name_Length, Length (String_Maps.Key (C)));
            end loop;

            Put_Line (Output, "   package Variables is");
            for C in Item.Variables.Iterate loop
               declare
                  Name  : constant String := To_String (String_Maps.Key (C));
                  Value : constant String := To_String (String_Maps.Element (C));
               begin
                  Put_Line (Output,
                            "      V_" &  Name & ((Max_Name_Length - Name'Length) * ' ') &
                              " := """ & Value  & """;");
               end;
            end loop;
            Put_Line (Output, "   end Variables;");
            New_Line (Output);
         end;
      end if;

      if Get_Source_Dirs (Item).Length /= 0 then
         Put_Line (Output, "   for Source_Dirs use " & Image (Get_Source_Dirs (Item)) & ";");
         New_Line (Output);
      else
         Put_Line (Output, "   for Source_Dirs use ();");
         New_Line (Output);
      end if;


      Put_Line (Output, "   for Externally_Built use ""True"";");
      New_Line (Output);

      Put_Line (Output, "   package Compiler is");
      if Item.Compiler_Default_Switches.Length > 0 then
         Put_Line (Output, "      for Default_Switches(""C"") use " & Image (Compiler_Default_Switches (Item)) & ";");
         New_Line (Output);
      end if;
      for I of Item.Requires loop
         if Length (I) > 1 and then Element (I, 1) not in '<' | '>' | '=' | '0' .. '9' then
            Put_Line (Output, "      for Default_Switches(""C"") use Compiler'Default_Switches(""C"") & ");
            Put_Line (Output, "        " & Pkg2Ada_Name (I) & ".Compiler'Default_Switches(""C"");");
            New_Line (Output);
         end if;
      end loop;
      Put_Line (Output, "   end Compiler;");

      if Item.Libs.Length /= 0 then
         New_Line (Output);
         Put_Line (Output, "   package Linker is");
         Put_Line (Output, "      for Linker_Options use " & Linker_Options (Item) & ";");
         Put_Line (Output, "   end Linker;");
         New_Line (Output);
      end if;

      Put_Line (Output, "end " & Pkg2Ada_Name (Item.FName) & ";");
   end Write_GPR;

   function Get_Source_Dirs   (Item : Descr) return String_Vectors.Vector is
      Temp : Unbounded_String;
   begin
      return Ret : String_Vectors.Vector do
         for I of Item.Cflags loop
            if Index (I, "-I") > 0 then
               Temp := Unbounded_Slice (I, 3, Length (I));
               if not Item.Default_Include.Contains (Temp) then
                  if Exists (To_String (Temp)) then
                     Ret.Append (To_Unbounded_String (Normalize_Pathname (To_String (Temp))));
                  end if;
               end if;
            end if;
         end loop;
      end return;
   end Get_Source_Dirs;

   -------------
   -- Get_GPR --
   -------------

   function Get_GPR (Item : String) return String is
      Ret : String := Pkg2Ada_Name (Item);
   begin
      GNAT.Case_Util.To_Lower (Ret);
      return Ret & ".gpr";
   end Get_GPR;

   function Get_GPR (Item : Descr) return String is
   begin
      return Get_GPR (To_String (Item.FName));
   end Get_GPR;

   function Get_Include (Cpp : String := "cpp") return String_Vectors.Vector is
      Args         : GNAT.OS_Lib.Argument_List_Access := new GNAT.OS_Lib.Argument_List'
        (new String'("-v"),
         (new String'("/dev/null")));
      Success      : Boolean;
      Return_Code  : Integer;
      F            : File_Type;
      Ret          : String_Vectors.Vector;
      Exe          : GNAT.OS_Lib.String_Access := GNAT.OS_Lib.Locate_Exec_On_Path (Cpp);
   begin
      GNAT.OS_Lib.Spawn (Exe.all, Args.all, "_dummy", Success, Return_Code);
      Open (F, In_File, "_dummy");
      while not End_Of_File (F) loop
         declare
            S : constant String := Ada.Strings.Fixed.Trim (Get_Line (F), Side => Ada.Strings.Both);
         begin
            if Exists (S) then
               Ret.Append (To_Unbounded_String (GNAT.OS_Lib.Normalize_Pathname (S)));
            end if;
         end;
      end loop;
      Close (F);
      Free (Exe);
      Free (Args);
      return Ret;
   end Get_Include;

   function Get_Libs (Gcc : String := "gcc") return String_Vectors.Vector is
      Args         : GNAT.OS_Lib.Argument_List_Access := new GNAT.OS_Lib.Argument_List'
        (new String'("-print-search-dirs"),
         (new String'("/dev/null")));
      Success      : Boolean;
      Return_Code  : Integer;
      F            : File_Type;
      Ret          : String_Vectors.Vector;
      Exe          : GNAT.OS_Lib.String_Access := GNAT.OS_Lib.Locate_Exec_On_Path (Gcc);
   begin
      GNAT.OS_Lib.Spawn (Exe.all, Args.all, "_dummy", Success, Return_Code);
      Open (F, In_File, "_dummy");

      while not End_Of_File (F) loop
         declare
            S : constant String := Ada.Strings.Fixed.Trim (Get_Line (F), Side => Ada.Strings.Both);
            F : GNAT.String_Split.Slice_Set;
         begin
            if Index (S, "libraries: =") > 0 then
               GNAT.String_Split.Create (F, S (Index (S, "=") + 1 .. S'Last), ":");
               for I in 1 .. GNAT.String_Split.Slice_Count (F) loop
                  declare
                     Name : constant String := GNAT.OS_Lib.Normalize_Pathname (GNAT.String_Split.Slice (F, I));
                  begin
                     if Exists (Name) then
                        if not Ret.Contains (To_Unbounded_String (Name)) then
                           Ret.Append (To_Unbounded_String (Name));
                        end if;
                     end if;
                  end;
               end loop;
            end if;
         end;
      end loop;
      Close (F);
      Free (Exe);
      Free (Args);
      return Ret;
   end Get_Libs;

end GPR_Tools.Pkg2gpr;
