pragma Ada_2012;

with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Strings.Fixed.Equal_Case_Insensitive;
with Ada.Strings.Unbounded;

with GNAT.Command_Line;
with GNAT.Exception_Traces;
with GNAT.IO; use GNAT.IO;
with GNAT.OS_Lib;
with GNAT.Regexp;
with GNAT.Regpat;
with GNAT.Spitbol.Table_Boolean;
with GNAT.String_Split;
with GNAT.Strings;
with GNAT.Traceback.Symbolic;
with GNAT.Traceback;

with GNATCOLL.Arg_Lists;
with GNATCOLL.Projects;
with GNATCOLL.Templates;
with GNATCOLL.VFS;
with Ada.Containers.Indefinite_Ordered_Sets;
with GNAT.Directory_Operations;
with GPR.Opt;
procedure GPR_Tools.Gprinfo is
   use Ada.Containers;
   use Ada.Directories;
   use Ada.Strings.Fixed;

   use GNAT.Command_Line;
   use GNAT.Regpat;
   use GNAT.Spitbol.Table_Boolean;
   use GNAT.Strings;

   use GNATCOLL.Arg_Lists;
   use GNATCOLL.Projects;
   use GNATCOLL.Templates;
   use GNATCOLL.VFS;


   type Command_Kind is (Do_Echo, Do_Exec);
   type Command_Type is  record
      Kind  : Command_Kind := Do_Echo;
      Cmd   : GNAT.Strings.String_Access;
   end record;

   GPR_PROJECT_PATH_ORIG : constant GNAT.Strings.String_Access := GNAT.OS_Lib.Getenv ("GPR_PROJECT_PATH");

   package Command_Vectors is new Ada.Containers.Vectors (Natural, Command_Type);
   package String_Vectors is new Ada.Containers.Vectors (Natural, GNAT.Strings.String_Access);
   package String_Sets is new Ada.Containers.Indefinite_Ordered_Sets (Element_Type => String);

   GPR_PROJECT_PATH_LOCAL      : String_Vectors.Vector;
   GPR_PROJECT_PATH_SUBPROCESS : String_Vectors.Vector;
   Version                     : constant String := $VERSION;
   BaseName                    : Boolean := False;
   Commands                    : Command_Vectors.Vector;
   Cwd                         : Boolean := False;
   DirName                     : Boolean := False;
   Direct_Imports              : Boolean := False;
   Displayed_Folders           : GNAT.Spitbol.Table_Boolean.Table (512);

   Env                      : Project_Environment_Access;
   Exclude_Externally_Built : Boolean := True;
   Exclude_No_Source        : Boolean := True;
   Exclude_Patterns         : String_Vectors.Vector;
   Exclude_RTS              : Boolean := True;

   Exec_Dir                      : Boolean := False;
   GNAT_Version                  : GNAT.Strings.String_Access;
   Gnatls                        : GNAT.Strings.String_Access := new String'("gnatls");
   Help                          : Boolean := False;
   Languages                     : Boolean := False;
   Library_Dir                   : Boolean := False;
   Missing                       : Boolean := False;
   Missing_Fail                  : Boolean := False;
   No_Duplicates                 : Boolean := False;
   Object_Dir                    : Boolean := False;
   Proj                          : Project_Tree;
   Project_File                  : GNAT.Strings.String_Access;
   Recursive                     : Boolean := False;
   Rts_Root                      : GNAT.Strings.String_Access;
   Show_Version                  : Boolean := False;
   Source_Dirs                   : Boolean := False;
   Source_Dirs_I                 : Boolean := False;
   Source_Files                  : Boolean := False;
   Verbose                       : Boolean := False;
   Exit_Status                   : Ada.Command_Line.Exit_Status := Ada.Command_Line.Success;
   Reverse_Order                 : Boolean := False;
   Execute_Commands              : Boolean := False;
   Max_Iterations                : Positive := 1;
   Default_Max_Iterations        : constant := 16;
   Attribute                     : GNAT.Strings.String_Access;
   Query_Languages               : GNAT.Strings.String_Access;
   Reload_Project_After_Warnings : Boolean := True;
   function Image (Item : String_Vectors.Vector) return String is
      Ret : Ada.Strings.Unbounded.Unbounded_String;
   begin
      for I of Item loop
         if Ada.Strings.Unbounded.Length (Ret) /= 0 then
            Ada.Strings.Unbounded.Append (Ret, GNAT.OS_Lib.Path_Separator);
         end if;
         Ada.Strings.Unbounded.Append (Ret, I.all);
      end loop;
      return Ada.Strings.Unbounded.To_String (Ret);
   end Image;




   procedure Output (Full_Name : String;
                     Base_Name : String;
                     Dir_Name  : String;
                     Name      : String;
                     Continue  : out Boolean) is

      Subst                    : GNATCOLL.Templates.Substitution_Array
        := ((new String'("full_name"), new String'(Full_Name)),
            (new String'("base_name"), new String'(Base_Name)),
            (new String'("dir_name"), new String'(Dir_Name)),
            (new String'("name"), new String'(Name)));
      Args                     : GNAT.OS_Lib.Argument_List_Access;
      Cmd                      : GNAT.OS_Lib.String_Access;
   begin
      Continue := True;

      if No_Duplicates then
         if not Present (Displayed_Folders, Dir_Name) then
            Set (Displayed_Folders, Dir_Name, True);
         else
            return;
         end if;
      end if;

      if Exclude_RTS and then Ada.Strings.Fixed.Index (Dir_Name, Rts_Root.all) = Dir_Name'First then
         return;
      end if;

      for Exclude_Pattern of Exclude_Patterns  loop
         if GNAT.Regexp.Match (Full_Name, GNAT.Regexp.Compile (Exclude_Pattern.all)) then
            return;
         end if;
      end loop;

      for C of Commands loop
         declare
            Command                  : GNATCOLL.Arg_Lists.Arg_List;
            Commands_Template        : GNATCOLL.Arg_Lists.Arg_List;
            use type Ada.Command_Line.Exit_Status;
         begin
            Commands_Template := GNATCOLL.Arg_Lists.Parse_String (C.Cmd.all, Separate_Args);
            for I in 0 .. Args_Length (Commands_Template) loop
               Append_Argument (C        => Command,
                                Argument => Substitute (Str        => Nth_Arg (Commands_Template, I),
                                                        Substrings => Subst),
                                Mode     => One_Arg);
            end loop;

            case C.Kind is
            when Do_Echo =>
               GNAT.IO.Put_Line (GNATCOLL.Arg_Lists.To_Display_String (Command));
            when Do_Exec =>
               if Cwd then
                  if Verbose then
                     GNAT.IO.Put_Line ("<cwd>  " & Dir_Name);
                  end if;
                  Ada.Directories.Set_Directory (Dir_Name);
               end if;

               if Verbose then
                  GNAT.IO.Put_Line ("<exec> " & GNATCOLL.Arg_Lists.To_Display_String (Command));
               end if;

               Args := new GNAT.OS_Lib.Argument_List'(GNATCOLL.Arg_Lists.To_List (Command, False));
               Cmd := GNAT.OS_Lib.Locate_Exec_On_Path (GNATCOLL.Arg_Lists.Get_Command (Command));
               Exit_Status := Ada.Command_Line.Exit_Status (GNAT.OS_Lib.Spawn (Cmd.all, Args.all));
               if Exit_Status /= 0 then
                  Continue := False;
               end if;
               Free (Cmd);
               Free (Args);
            end case;
         end;
      end loop;
      GNATCOLL.Templates.Free (Subst);
   end Output;


   procedure Locate_Proj is
      Found : Boolean := False;
      procedure  Process (Directory_Entry : Directory_Entry_Type) is
      begin
         if not Found then
            Project_File := new String'(Full_Name (Directory_Entry));
            Found := True;
         end if;
      end Process;
   begin
      declare
         Name : constant String := Get_Argument (True);
      begin
         if Name'Length /= 0 then
            Project_File := new String'(Full_Name (Name));
            return;
         end if;
      end;
      Search (".", "*.gpr", Process => Process'Access);
   end Locate_Proj;


   procedure Print_Attribute (P : Project_Type; Attribute : String) is
      Matcher : constant Pattern_Matcher := Compile ("((\w+)\.|)(\w+)(\((\w+)\)|)");
      Matches : Match_Array (1 .. Paren_Count (Matcher));
      Pkg_IX  : constant := 2;
      Attr_IX : constant := 3;
      IX_IX   : constant := 5;
      function Get (S : String; Slice : Match_Location) return String is
      begin
         if Slice = No_Match then
            return "";
         else
            return S (Slice.First .. Slice.Last);
         end if;
      end Get;
   begin
      Match (Matcher, Attribute, Matches);
      if Matches (Attr_IX) /= No_Match then
         declare
            Attr_L : constant String_List_Access := P.Attribute_Value
              (Attribute => Build (Get (Attribute, Matches (Pkg_IX)), Get (Attribute, Matches (Attr_IX))),
               Index     => Get (Attribute, Matches (IX_IX)));
            First  : Boolean := True;
         begin
            for I of Attr_L.all loop
               if not First then
                  GNAT.IO.Put (" ");
               end if;
               GNAT.IO.Put (I.all);
               First := False;
            end loop;
            GNAT.IO.New_Line;
         end;
      end if;
   end Print_Attribute;

   procedure Display (P : Project_Type) is
   begin
      if Exclude_Externally_Built and then Equal_Case_Insensitive (P.Attribute_Value (Build ("", "externally_built")), "true") then
         return;
      elsif Exclude_No_Source and then P.Direct_Sources_Count = 0 then
         return;
      elsif Languages then
         for I of  P.Languages loop
            GNAT.IO.Put_Line (I.all);
         end loop;
      elsif Attribute /= null then
         Print_Attribute (P, Attribute.all);
      elsif Source_Dirs or Source_Dirs_I then
         for I of  P.Source_Dirs (Recursive) loop
            GNAT.IO.Put_Line ((if Source_Dirs_I then "-I " else "") & (+Full_Name (I)));
         end loop;
      elsif Source_Files then
         for I of  GNATCOLL.VFS.File_Array_Access'(P.Source_Files (Recursive)).all loop
            GNAT.IO.Put_Line (+Full_Name (I));
         end loop;
      elsif Object_Dir then
         for I of  P.Object_Path (Recursive) loop
            GNAT.IO.Put_Line (+Full_Name (I));
         end loop;
      elsif Exec_Dir then
         GNAT.IO.Put_Line (+Full_Name (P.Executables_Directory));
      elsif Library_Dir then
         GNAT.IO.Put_Line (+Full_Name (P.Library_Directory));
      elsif Recursive or Direct_Imports then
         declare
            Iter     : Project_Iterator;
            Continue : Boolean;
         begin
            if Reverse_Order then
               Iter := P.Start_Reversed (Recursive, Direct_Imports);
            else
               Iter := P.Start (Recursive, Direct_Imports);
            end if;

            loop
               exit when Current (Iter) = No_Project;
               if DirName then
                  declare
                     Name : constant String := +Dir_Name (Current (Iter).Project_Path);
                  begin
                     if No_Duplicates and then not Present (Displayed_Folders, Name) then
                        GNAT.IO.Put_Line (Name);
                        Set (Displayed_Folders, Name, True);
                     end if;
                  end;
               elsif BaseName then
                  GNAT.IO.Put_Line (+Base_Name (Current (Iter).Project_Path));
               elsif Commands.Length /= 0  then
                  Output (Full_Name => +Full_Name (Current (Iter).Project_Path),
                          Base_Name => +Base_Name (Current (Iter).Project_Path),
                          Dir_Name  => +Dir_Name (Current (Iter).Project_Path),
                          Name      => Current (Iter).Name,
                          Continue  => Continue);

               else
                  if Query_Languages = null then
                     GNAT.IO.Put_Line (+Full_Name (Current (Iter).Project_Path));
                  else
                     for I of  Current (Iter).Languages loop
                        if Ada.Strings.Fixed.Equal_Case_Insensitive (I.all, Query_Languages.all) then
                           GNAT.IO.Put_Line (+Full_Name (Current (Iter).Project_Path));
                        end if;
                     end loop;
                  end if;
               end if;
               Next (Iter);
            end loop;

         end;
      end if;
   end Display;


   procedure Handle_Error_Report (Msg : String) is
      Matcher  : constant GNAT.Regpat.Pattern_Matcher := Compile ("^((\w+\.gpr):\d+:\d+: unknown project file: ""(.+)"")");
      Matches  : GNAT.Regpat.Match_Array (1 .. GNAT.Regpat.Paren_Count (Matcher));
      Continue : Boolean;
   begin
      Match (Matcher, Msg, Matches);
      if Matches (3) /= No_Match then
         if Missing_Fail then
            Exit_Status := Ada.Command_Line.Failure;
         end if;
         if Commands.Length /= 0  then
            Output (Full_Name => Msg (Matches (3).First .. Matches (3).Last),
                    Base_Name => Msg (Matches (2).First .. Matches (2).Last),
                    Dir_Name  => Msg (Matches (3).First .. Matches (3).Last),
                    Name      => Msg (Matches (3).First .. Matches (3).Last),
                    Continue  => Continue);
         else
            GNAT.IO.Put_Line (Msg (Matches (3).First .. Matches (3).Last));
         end if;
      end if;
   end Handle_Error_Report;

   procedure Handle_Error_Report_Suppress_Warnings (Msg : String) is
      Matcher  : constant GNAT.Regpat.Pattern_Matcher := Compile ("^(\w+)\.gpr:\d+:\d+: warning: object directory ""(.+)"" not found");
      Matches  : GNAT.Regpat.Match_Array (1 .. GNAT.Regpat.Paren_Count (Matcher));
   begin
      Match (Matcher, Msg, Matches);
      if Matches (2) /= No_Match then
         Reload_Project_After_Warnings := True;
      else
         GNAT.IO.Put_Line (Msg);
      end if;
   end Handle_Error_Report_Suppress_Warnings;

   --  -------------------------------------------------------------------------
   --  -------------------------------------------------------------------------
   procedure Print_Help is
      use ASCII;
   begin
      Put_Line ("Displays various aspects of .gpr-project files. " & ASCII.LF &
                  "could also excute commands on the resulting project set." & ASCII.LF &
                  "The usage usage is multiple:" & ASCII.LF &
                  " * Extract aspects of a single project file to be used in scripts." & ASCII.LF &
                  " * Iterate over project-trees." & LF &
                  " * Find missing projects in a project tree.");

      Put_Line ("-P=proj                      Use Project File proj");
      Put_Line ("-aP=dir                      Add directory dir to project search path (not propageted to subproceses).");
      Put_Line ("-Ap=dir                      Add directory dir to project search path (only propageted to subproceses).");
      Put_Line ("-AP=dir                      Add directory dir to project search path global.");
      Put_Line ("--dirname                    Show directorynames of projects.");
      Put_Line ("--basename                   Show basenames of projects.");
      Put_Line ("-m  --missing                Show missing projects substitution is %name is the missing file and %base_name is the importing file.");
      Put_Line ("-M  --Missing                Show missing projects substitution same as in ""-m"" (exit with error if not complete).");
      Put_Line ("-x{num}                      Repeats load of missing projects max ""num"" times (default is" & Default_Max_Iterations'Img & ") does only apply on ""-exec"".");
      Put_Line ("--exec-dir                   Print exec dir.");
      Put_Line ("--object-dir                 Print object dir.");
      Put_Line ("--source-dirs                Print source dirs.");
      Put_Line ("--source-dirs-include        Print source dirs with ""-I "" before each entry.");
      Put_Line ("--source-files               Print source files.");
      Put_Line ("--imports                    Print direct imports.");
      Put_Line ("--library-dir                Print library dir.");
      Put_Line ("--languages                  Print project languages.");
      Put_Line ("--contains-lang=lang         Print projects containing languages lang (implies recursive)");
      Put_Line ("--gnatls={gnatls}            Use as gnatls default=>'" & Gnatls.all & "' .");
      Put_Line ("-r --recursive               Show recursive on all projects in tree in buildorder.");
      Put_Line ("--reverse                    Show recursive on all projects in tree in reverse buildorder.");
      Put_Line ("--echo=""command line""      Echo the argument with substitution %full_name %base_Name %dir_name %name.");
      Put_Line ("--exec=""command line""      Execute the argument with substitution %full_name %base_Name %dir_name %name.");
      Put_Line ("--cwd                        Change dir to projects enclosing dir before executing command.");
      Put_Line ("--rts                        Include projects from runtimes.");
      Put_Line ("--externally-built           Include externally built procjects.");
      Put_Line ("--empty-sources              Include projects that dont have source files.");
      Put_Line ("--no-duplicates              Don't duplicate folders when showing dirnames.");
      Put_Line ("--exclude-pattern=regexp     Exclude project paths matching regexp.");
      Put_Line ("--attribute={pkg.}attr{(ix)} Prints the attribute attr.");
      Put_Line ("-Xnm=val                     Specify an external reference for Project Files.");
      Put_Line ("-v  --version                Print version and then exit.");
      Put_Line ("--verbose                    Be verbose.");
      Put_Line ("--exceptions                 Trace all exceptions.");
      Put_Line ("-? -h --help                 Display this text.");
   end Print_Help;

   procedure Parse_Command_Line is
      Opt : Character;
   begin
      loop
         Opt :=  Getopt ("P= " &
                           "AP= " &
                           "aP= " &
                           "-dirname " &
                           "-basename " &
                           "m -missing " &
                           "M -Missing " &
                           "-exec-dir " &
                           "-object-dir " &
                           "-source-dirs " &
                           "-source-dirs-include " &
                           "-source-files " &
                           "-imports " &
                           "-library-dir " &
                           "-languages " &
                           "-contains-lang= " &
                           "-gnatls= " &
                           "x? " &
                           "X! " &
                           "r -recursive " &
                           "-reverse " &
                           "-echo= " &
                           "-exec= " &
                           "-cwd " &
                           "-rts " &
                           "-externally-built " &
                           "-empty-sources " &
                           "-no-duplicates " &
                           "-exclude-pattern " &
                           "v -verbose " &
                           "-version " &
                           "-attribute= " &
                           "-exceptions " &
                           "? h -help");
         case Opt is
            when ASCII.NUL => exit;
            when 'P' =>
               Project_File := new String'(Parameter);
            when 'a' =>
               if Full_Switch = "aP" then
                  GPR_PROJECT_PATH_LOCAL.Append (new String'(Parameter));
               else
                  raise Program_Error with "invalid switch => '" & Full_Switch  & "'";
               end if;
            when 'A' =>
               if Full_Switch = "Ap" then
                  GPR_PROJECT_PATH_SUBPROCESS.Append (new String'(Parameter));
               elsif Full_Switch = "AP" then
                  GPR_PROJECT_PATH_LOCAL.Append (new String'(Parameter));
                  GPR_PROJECT_PATH_SUBPROCESS.Append (new String'(Parameter));
               else
                  raise Program_Error with "invalid switch => '" & Full_Switch  & "'";
               end if;
            when 'm' =>
               Missing := True;
            when 'M' =>
               Missing_Fail := True;
            when 'r' =>
               Recursive  := True;
            when 'v' =>
               Show_Version  := True;
            when 'x' =>
               if Parameter'Length = 0 then
                  Max_Iterations := Default_Max_Iterations;
               else
                  Max_Iterations := Integer'Value (Parameter);
               end if;
            when 'X' =>
               declare
                  S : GNAT.String_Split.Slice_Set; use GNAT.String_Split;
               begin
                  Create (S, Parameter, "=");
                  if Slice_Count (S) = 2 then
                     Change_Environment (Env.all, Slice (S, 1), Slice (S, 2));
                  else
                     Put_Line ("Invalid argument for ""-X"" """ & Parameter & """.");
                     Help := True;
                  end if;
               end;
            when '?' | 'h' =>
               Help := True;
            when '-' =>
               if Full_Switch = "-dirname" then
                  DirName := True;
               elsif Full_Switch = "-basename" then
                  BaseName := True;
               elsif Full_Switch = "-missing" then
                  Missing := True;
               elsif Full_Switch = "-Missing" then
                  Missing := True;
                  Missing_Fail := True;
               elsif Full_Switch = "-object-dir" then
                  Object_Dir := True;
               elsif Full_Switch = "-object-dir" then
                  Object_Dir := True;
               elsif Full_Switch = "-source-dirs" then
                  Source_Dirs := True;
               elsif Full_Switch = "-source-dirs-include" then
                  Source_Dirs_I := True;
               elsif Full_Switch = "-source-files" then
                  Source_Files := True;
               elsif Full_Switch = "-imports" then
                  Direct_Imports := True;
               elsif Full_Switch = "-exec-dir" then
                  Exec_Dir := True;
               elsif Full_Switch = "-library-dir" then
                  Library_Dir := True;
               elsif Full_Switch = "-languages" then
                  Languages := True;
               elsif Full_Switch = "-contains-lang" then
                  Query_Languages := new String'(Parameter);
                  Recursive  := True;
               elsif Full_Switch = "-gnatls" then
                  Gnatls :=  new String'(Parameter);

               elsif Full_Switch = "-reverse" then
                  Recursive := True;
                  Reverse_Order := True;
               elsif Full_Switch = "-recursive" then
                  Recursive := True;

               elsif Full_Switch = "-cwd" then
                  Cwd := True;
               elsif Full_Switch = "-echo" then
                  Commands.Append ((Do_Echo, new String'(Parameter)));
               elsif Full_Switch = "-exec" then
                  Commands.Append ((Do_Exec, new String'(Parameter)));
                  Execute_Commands := True;
               elsif Full_Switch = "-rts" then
                  Exclude_RTS := False;
               elsif Full_Switch = "-externally-built" then
                  Exclude_Externally_Built := False;
               elsif Full_Switch = "-empty-sources" then
                  Exclude_No_Source := False;
               elsif Full_Switch = "-exclude-pattern" then
                  Exclude_Patterns.Append (new String'(Parameter));
               elsif Full_Switch = "-no-duplicates" then
                  No_Duplicates := True;
               elsif Full_Switch = "-version" then
                  Show_Version := True;
               elsif Full_Switch = "-verbose" then
                  Verbose := True;
               elsif Full_Switch = "-attribute" then
                  Attribute := new String'(Parameter);
               elsif Full_Switch = "-exceptions" then
                  GNAT.Exception_Traces.Set_Trace_Decorator (GNAT.Traceback.Symbolic.Symbolic_Traceback'Access);
                  GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);

               elsif Full_Switch = "-help" then
                  Help := True;
               else
                  raise Program_Error with "invalid switch => '" & Full_Switch  & "'";
               end if;
            when others =>
               raise Program_Error with "invalid switch => '" & Full_Switch  & "'";
         end case;
      end loop;
   exception
      when GNAT.Command_Line.Invalid_Switch =>
         GNAT.IO.Put_Line ("Invalid switch: " &  Full_Switch);
         Print_Help;
         raise;
   end Parse_Command_Line;

begin
   Initialize (Env);
   Parse_Command_Line;

   if Help then
      Print_Help;
      return;
   elsif Show_Version then
      GNAT.IO.Put_Line (Version);
      return;
   end if;


   if Direct_Imports then
      Recursive := True;
   end if;

   --  ============
   --  Set up paths
   --  ============
   if GPR_PROJECT_PATH_ORIG /= null and then GPR_PROJECT_PATH_ORIG.all /= "" then
      declare
         S : GNAT.String_Split.Slice_Set;
      begin
         GNAT.String_Split.Create (S, GPR_PROJECT_PATH_ORIG.all, GNAT.OS_Lib.Path_Separator & "", GNAT.String_Split.Multiple);
         for I in 1 .. GNAT.String_Split.Slice_Count (S) loop
            GPR_PROJECT_PATH_SUBPROCESS.Append (new String'(GNAT.String_Split.Slice (S, I)));
            GPR_PROJECT_PATH_LOCAL.Append (new String'(GNAT.String_Split.Slice (S, I)));
         end loop;
      end;
   end if;

   declare
      Realgnatls : GNAT.Strings.String_Access;
   begin
      Realgnatls := GNAT.OS_Lib.Locate_Exec_On_Path (Gnatls.all);
      Rts_Root   := new String'(Containing_Directory (Containing_Directory (Realgnatls.all)));
      Free (Realgnatls);
   end;

   Env.Set_Path_From_Gnatls (Gnatls.all, GNAT_Version, GNAT.IO.Put_Line'Access);

   --  ======================
   --  Locate the root-project
   --  ======================
   if Project_File = null then
      Locate_Proj;
   elsif Project_File.all = "" then
      Free (Project_File);
      Locate_Proj;
   end if;
   if not Exists (Name => Project_File.all) then
      if Exists (Name => Project_File.all & ".gpr") then
         Project_File := new String'(Project_File.all & ".gpr");
      end if;
   end if;

   --  ================
   --  Do the real work
   --  ================
   if GPR_PROJECT_PATH_LOCAL.Length /= 0 then
      GNAT.OS_Lib.Setenv ("GPR_PROJECT_PATH", Image (GPR_PROJECT_PATH_LOCAL));
   end if;

   if Missing or Missing_Fail then
      Load_Loop : for I in  1 .. (if Execute_Commands then Max_Iterations else 1) loop
         begin
            Proj.Load (Create (Filesystem_String (Project_File.all)), Env,
                       Errors         => Handle_Error_Report'Unrestricted_Access,
                       Recompute_View => False);
            exit Load_Loop;
         exception
            when others   =>
               null;
         end;
      end loop Load_Loop;
   else
      GPR.Opt.Setup_Projects := True;
      GPR.Opt.Unchecked_Shared_Lib_Imports := True;
      GPR.Opt.Full_Path_Name_For_Brief_Errors := True;

      Proj.Load (Create (Filesystem_String (Project_File.all)), Env, Errors => Handle_Error_Report_Suppress_Warnings'Unrestricted_Access);
      if Reload_Project_After_Warnings then
         Proj.Load (Create (Filesystem_String (Project_File.all)), Env, Errors => Handle_Error_Report_Suppress_Warnings'Unrestricted_Access);
      end if;
      if GPR_PROJECT_PATH_SUBPROCESS.Length /= 0 then
         GNAT.OS_Lib.Setenv ("GPR_PROJECT_PATH", Image (GPR_PROJECT_PATH_SUBPROCESS));
      end if;

      if Proj.Root_Project.Is_Aggregate_Project or Proj.Root_Project.Is_Aggregate_Library then
         Recursive := False;
         declare
            Projects : String_Sets.Set;

            procedure Process (Directory_Entry : Directory_Entry_Type) is
               P        : constant Project_Type := Project_From_Path (Proj, Create_From_UTF8 (Ada.Directories.Full_Name (Directory_Entry)));
               Iter     : Project_Iterator;
            begin
               Projects.Include (P.Name);
               Iter := P.Start (True, False, True);
               while Current (Iter) /= No_Project loop
                  Projects.Include (Current (Iter).Name);
                  Next (Iter);
               end loop;
            end Process;
            Project_Files : constant GNAT.Strings.String_List_Access := Proj.Root_Project.Attribute_Value  (Build ("", "Project_Files"));
         begin
            Locate_All_Projects : for I of  Project_Files.all loop
               Ada.Directories.Search (GNAT.Directory_Operations.Dir_Name (I.all),
                                       GNAT.Directory_Operations.Base_Name (I.all),
                                       (False, True, False),
                                       Process'Access);
            end loop Locate_All_Projects;
            for I of Projects loop
               Display (Proj.Project_From_Name (I));
            end loop;
         end;
      else
         Display (Proj.Root_Project);
      end if;
   end if;
   Ada.Command_Line.Set_Exit_Status (Exit_Status);
exception
   when others =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end GPR_Tools.Gprinfo;
