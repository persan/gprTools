pragma Ada_2012;
with GNAT.Command_Line; use GNAT.Command_Line;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNAT.Strings; use GNAT.Strings;
with Gnat.IO;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with Ada.Directories; use Ada.Directories;
with GNAT.Regpat; use GNAT.Regpat;
with Ada.Command_Line;
with GNAT.Spitbol.Table_Boolean;
with GNATCOLL.Templates; use GNATCOLL.Templates;
with GNATCOLL.Arg_Lists; use GNATCOLL.Arg_Lists;
with GNAT.OS_Lib;
with GNAT.Regexp;
with Ada.Strings.Fixed.Equal_Case_Insensitive;
with ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with GNAT.String_Split;
procedure Gprinfo is

   use Ada.Strings.Fixed;
   use GNAT.Spitbol.Table_Boolean;
   use Ada.Containers;


   type command_kind is (do_echo, do_exec);
   type command_type is  record
      kind  : command_kind := do_echo;
      cmd   : GNAT.Strings.String_Access;
   end record;

   GPR_PROJECT_PATH_ORIG : constant GNAT.Strings.String_Access := GNAT.OS_Lib.Getenv ("GPR_PROJECT_PATH");

   package Command_Vectors is new Ada.Containers.Vectors (Natural, command_type);
   package String_Vectors is new Ada.Containers.Vectors (Natural, GNAT.Strings.String_Access);

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

   Exec_Dir                 : Boolean := False;
   GNAT_Version             : GNAT.Strings.String_Access;
   Gnatls                   : GNAT.Strings.String_Access := new String'("gnatls");
   Help                     : Boolean := False;
   Languages                : Boolean := False;
   Library_Dir              : Boolean := False;
   Missing                  : Boolean := False;
   Missing_Fail             : Boolean := False;
   No_Duplicates            : Boolean := False;
   Object_Dir               : Boolean := False;
   Proj                     : Project_Tree;
   Project_File             : GNAT.Strings.String_Access;
   Recursive                : Boolean := False;
   Rts_Root                 : GNAT.Strings.String_Access;
   Show_Version             : Boolean := False;
   Source_Dirs              : Boolean := False;
   Source_Dirs_I            : Boolean := False;
   Source_Files             : Boolean := False;
   Verbose                  : Boolean := False;
   Exit_Status              : Ada.Command_Line.Exit_Status := Ada.Command_Line.Success;
   Reverse_Order            : Boolean := False;
   execute_Commands         : Boolean := False;
   Max_Iterations           : Positive := 1;
   Default_Max_Iterations   : constant := 16;

   function image (item : String_Vectors.Vector) return string is
      ret : ada.Strings.Unbounded.Unbounded_String;
   begin
      for i in 1 .. item.Length loop
         ada.Strings.Unbounded.append (ret, String_Vectors.Element (item, Natural (i)).all);
         if i /= item.Length then
            ada.Strings.Unbounded.append (ret, GNAT.OS_Lib.Path_Separator);
         end if;
      end loop;
      return ada.Strings.Unbounded.To_String (ret);
   end;

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
         if GNAT.Regexp.Match ( Full_Name, GNAT.Regexp.Compile (Exclude_Pattern.all)) then
            return;
         end if;
      end loop;

      for c of Commands loop
         declare
            Command                  : GNATCOLL.Arg_Lists.Arg_List;
            Commands_Template        : GNATCOLL.Arg_Lists.Arg_List;
         begin
            Commands_Template := GNATCOLL.Arg_Lists.Parse_String (c.cmd.all, Separate_Args);
            for I in 0 .. Args_Length (Commands_Template) loop
               Append_Argument (C        => Command,
                                Argument => Substitute (Str        => Nth_Arg (Commands_Template, I),
                                                        Substrings => Subst),
                                Mode     => One_Arg);
            end loop;

            case c.kind is
            when do_echo =>
               GNAT.Io.Put_Line (GNATCOLL.Arg_Lists.To_Display_String (Command));
            when do_exec =>
               if Cwd then
                  if Verbose then
                     GNAT.Io.Put_Line ("<cwd>  " & Dir_Name);
                  end if;
                  Ada.Directories.Set_Directory (Dir_Name);
               end if;

               if Verbose then
                  GNAT.Io.Put_Line ("<exec> " & GNATCOLL.Arg_Lists.To_Display_String (Command));
               end if;

               Args := new GNAT.OS_Lib.Argument_List'(GNATCOLL.Arg_Lists.To_List (Command, False));
               cmd := GNAT.OS_Lib.Locate_Exec_On_Path (GNATCOLL.Arg_Lists.Get_Command (Command));
               if GNAT.OS_Lib.Spawn (Cmd.all, Args.all) /= 0 then
                  Continue := False;
               end if;
               Free (Cmd);
               Free (Args);
            end case;
         end;
      end loop;
      GNATCOLL.Templates.free (Subst);
   end Output;


   procedure Locate_Proj is
      Found : Boolean := False;
      procedure  Process (Directory_Entry : Directory_Entry_Type) is
      begin
         if not Found then
            Project_File := new String'(Full_Name (Directory_Entry));
            Found := True;
         end if;
      end;
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
   end;


   procedure Display (P : Project_Type ) is
   begin
      if Exclude_Externally_Built and then Equal_Case_Insensitive (P.Attribute_Value (Build ("", "externally_built")), "true") then
         return;
      end if;

      if Exclude_No_Source and then P.Direct_Sources_Count = 0 then
         return;
      end if;
      if Languages then
         for I of  P.Languages loop
            Gnat.IO.Put_Line (I.all);
         end loop;
      elsif Source_Dirs or Source_Dirs_I then
         for I of  P.Source_Dirs (Recursive) loop
            Gnat.IO.Put_Line (( if Source_Dirs_I then "-I " else "") & (+Full_Name (I)));
         end loop;
      elsif Source_Files then
         for I of  P.Source_Files (Recursive).all loop
            Gnat.IO.Put_Line (+Full_Name (I));
         end loop;
      elsif Object_Dir then
         for I of  P.Object_Path (Recursive) loop
            Gnat.IO.Put_Line (+Full_Name (I));
         end loop;
      elsif Exec_Dir then
         Gnat.IO.Put_Line (+Full_Name (P.Executables_Directory));
      elsif Library_Dir then
         Gnat.IO.Put_Line (+Full_Name (P.Library_Directory));
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
                        Gnat.IO.Put_Line (Name);
                        Set (Displayed_Folders, Name, True);
                     end if;
                  end;
               elsif Basename then
                  Gnat.IO.Put_Line (+Base_Name (Current (Iter).Project_Path));
               elsif Commands.Length /= 0  then
                  Output (Full_Name => +Full_Name (Current (Iter).Project_Path),
                          Base_Name => +Base_Name (Current (Iter).Project_Path),
                          Dir_Name  => +Dir_Name (Current (Iter).Project_Path),
                          Name      => Current (Iter).Name,
                          Continue  => Continue);

               else
                  Gnat.IO.Put_Line (+Full_Name (Current (Iter).Project_Path));
               end if;
               Next (Iter);
            end loop;

         end;
      end if;
   end;


   procedure Handle_Error_Report (Msg : String) is
      Matcher  : constant GNAT.Regpat.Pattern_Matcher := Compile ("^((\w+\.gpr):\d+:\d+: unknown project file: ""(.+)"")");
      Matches  : GNAT.Regpat.Match_Array (1 .. GNAT.Regpat.Paren_Count (Matcher));
      Continue : Boolean;
   begin
      Match (Matcher, Msg, Matches);
      if Matches (3) /= No_Match then
         if Missing_Fail then
            Exit_Status := ADA.Command_Line.Failure;
         end if;
         if Commands.length /= 0  then
            Output (Full_Name => Msg (Matches (3).First .. Matches (3).Last),
                    Base_Name => Msg (Matches (2).First .. Matches (2).Last),
                    Dir_Name  => Msg (Matches (3).First .. Matches (3).Last),
                    Name      => Msg (Matches (3).First .. Matches (3).Last),
                    Continue  => Continue);
         else
            GNAt.Io.Put_Line (Msg (Matches (3).First .. Matches (3).Last));
         end if;
      end if;
   end;

   --  -------------------------------------------------------------------------
   --  -------------------------------------------------------------------------
   procedure print_help is
      use gnat.io;
      use ASCII;
   begin
      Put_Line ( "Displays various aspects of .gpr-project files. " & ASCII.LF &
                   "could also excute commands on the resulting project set." & ASCII.LF &
                   "The usage usage is multiple:" & ASCII.LF &
                   " * Exctract aspects of a single project file to be used in scripts." & ASCII.LF &
                   " * Iterate over project-trees." & LF &
                   " * Find missing projects in a project tree.");

      Put_Line ("-P={proj}                 Use Project File proj");
      Put_Line ("-aP=dir                   Add directory dir to project search path (not propageted to subproceses).");
      Put_Line ("-Ap=dir                   Add directory dir to project search path (propageted to subproceses).");
      Put_Line ("-AP=dir                   Add directory dir to project search path global.");
      Put_Line ("--dirname                 Show directorynames of projects.");
      Put_Line ("--basename                Show basenames of projects.");
      Put_Line ("-m  --missing             Show missing projects substitution is %name is the missing file and %base_name is the importing file.");
      Put_Line ("-M  --Missing             Show missing projects substitution same as in ""-m"" (exit with error if not complete).");
      Put_Line ("-x{num}                   Repeats load of missing projects max ""num"" times (default is" & Default_Max_Iterations'Img & ") does only apply on ""-exec"".");
      Put_Line ("--exec-dir                Print exec dir.");
      Put_Line ("--object-dir              Print object dir.");
      Put_Line ("--source-dirs             Print source dirs.");
      Put_Line ("--source-dirs-include     Print source dirs with ""-I "" before each entry.");
      Put_Line ("--source-files            Print source files.");
      Put_Line ("--imports                 Print direct imports.");
      Put_Line ("--library-dir             Print library dir.");
      Put_Line ("--languages               Print project languages.");
      Put_Line ("--gnatls={gnatls}         Use as gnatls default=>'" & Gnatls.all & "' .");
      Put_Line ("-r --recursive            Show recursive on all projects in tree in buildorder.");
      Put_Line ("--reverse                 Show recursive on all projects in tree in reverse buildorder.");
      Put_Line ("--echo=""command line""     Echo the argument with substitution %full_name %base_Name %dir_name %name.");
      Put_Line ("--exec=""command line""     Execute the argument with substitution %full_name %base_Name %dir_name %name.");
      Put_Line ("--cwd                     Change dir to projects enclosing dir before executing command.");
      Put_Line ("--rts                     Include projects from runtimes.");
      Put_Line ("--externally-built        Include externally built procjects.");
      Put_Line ("--empty-sources           Include projects that dont have source files.");
      Put_Line ("--no-duplicates           Don't duplicate folders when showing dirnames.");
      Put_Line ("--exclude-pattern=regexp  Exclude project paths matching regexp.");
      Put_Line ("-v  --version             Print version and then exit.");
      Put_Line ("--verbose                 Be verbose.");
      Put_Line ("-? -h --help              Display this text.");
   end;
   procedure parse_command_line is
      opt : Character;
   begin
      loop
         Opt :=  getopt ("P= " &
                           "AP=" &
                           "aP=" &
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
                           "-gnatls= " &
                           "x? " &
                           "r -recursive " &
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
                           "? h -help");
         case Opt is
            when ASCII.NUL => exit;
            when 'P' =>
               Project_File := new string'(Parameter);
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
               Recursive  := True;
            when 'x' =>
               if parameter'length = 0 then
                  Max_Iterations := Default_Max_Iterations;
               else
                  Max_Iterations := Integer'value (Parameter);
               end if;
            when '?' | 'h' =>
               help := true;
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
               elsif Full_Switch = "-gnatls" then
                  Gnatls :=  new String'(Parameter);

               elsif Full_Switch = "-reverse" then
                  recursive := True;
                  Reverse_Order := True;
               elsif Full_Switch = "-recursive" then
                  recursive := True;

               elsif Full_Switch = "-cwd" then
                  cwd := True;
               elsif Full_Switch = "-echo" then
                  Commands.Append ((do_echo, new String'(Parameter)));
               elsif Full_Switch = "-exec" then
                  Commands.Append ((do_exec, new String'(Parameter)));
                  execute_Commands := True;
               elsif Full_Switch = "-rts" then
                  Exclude_RTS := False;
               elsif Full_Switch = "-externally-built" then
                  Exclude_Externally_Built := False;
               elsif Full_Switch = "-empty-sources" then
                  Exclude_No_Source := False;
               elsif Full_Switch = "-exclude-pattern" then
                  Exclude_Patterns.append (new String'(Parameter));
               elsif Full_Switch = "-no-duplicates" then
                  No_Duplicates := True;
               elsif Full_Switch = "-version" then
                  Show_Version := True;
               elsif Full_Switch = "-verbose" then
                  Verbose := True;
               elsif Full_Switch = "-Help" then
                  Help := True;
               else
                  raise Program_Error with "invalid switch => '" & Full_Switch  & "'";
               end if;
            when others =>
               raise Program_Error with "invalid switch => '" & Full_Switch  & "'";
         end case;
      end loop;
   end;

begin

   parse_command_line;

   if Help then
      print_help;
      return;
   elsif Show_Version then
      GNAT.Io.Put_Line (Version);
      return;
   end if;


   if Direct_Imports then
      Recursive := True;
   end if;

   --
   -- Set up paths
   --
   Initialize (Env);
   if GPR_PROJECT_PATH_ORIG /= null and then GPR_PROJECT_PATH_ORIG.all /= "" then
      declare
         s : GNAT.String_Split.Slice_Set;
      begin
         GNAT.String_Split.Create (S, GPR_PROJECT_PATH_ORIG.all, GNAT.OS_Lib.Path_Separator & "", GNAT.String_Split.Multiple);
         for i in 1 .. GNAT.String_Split.Slice_Count (s) loop
            GPR_PROJECT_PATH_SUBPROCESS.Append (New STring'(GNAT.String_Split.Slice (s, i)));
            GPR_PROJECT_PATH_LOCAL.Append (New STring'(GNAT.String_Split.Slice (s, i)));
         end loop;
      end;
   end if;

   Env.Set_Path_From_Gnatls (Gnatls.all, GNAT_Version);
   declare
      realgnatls : GNAT.Strings.String_Access;
   begin
      Realgnatls := GNAT.OS_Lib.Locate_Exec_On_Path (Gnatls.all);
      Rts_Root   := new String'(Containing_Directory (Containing_Directory (Realgnatls.all)));
      Free (Realgnatls);
   end;

   --
   -- Locate the root-project
   --
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

   --
   -- Do the real work
   --
   if GPR_PROJECT_PATH_LOCAL.length /= 0 then
      GNAT.OS_Lib.Setenv ("GPR_PROJECT_PATH", image (GPR_PROJECT_PATH_LOCAL));
   end if;

   if Missing or Missing_Fail then
      Load_Loop : for i in  1 .. (if Execute_Commands then Max_Iterations else 1) loop
         begin
            Proj.Load (Create (Filesystem_String (Project_File.all)), Env,
                       Errors         => Handle_Error_Report'Unrestricted_Access,
                       Recompute_View => False);
            exit load_Loop;
         exception
            when others   =>
               null;
         end;
      end loop Load_Loop;
   else
      Proj.Load (Create (Filesystem_String (Project_File.all)), Env);
      if GPR_PROJECT_PATH_SUBPROCESS.length /= 0 then
         GNAT.OS_Lib.Setenv ("GPR_PROJECT_PATH", image (GPR_PROJECT_PATH_SUBPROCESS));
      end if;
      Display (Proj.Root_Project);
   end if;


   Ada.Command_Line.Set_Exit_Status (Exit_Status);

end Gprinfo;
