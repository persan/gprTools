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
with GNATCOLL.Templates;use GNATCOLL.Templates;
with GNATCOLL.Arg_Lists;use GNATCOLL.Arg_Lists;
with ada.Strings.Fixed.Equal_Case_Insensitive;
with GNAT.OS_Lib;
with GNAT.Regexp;
procedure Gprinfo is
   function "=" (L, R : String ) return Boolean renames Ada.Strings.Fixed.Equal_Case_Insensitive;
   use GNAT.Spitbol.Table_Boolean;
   Version      : constant String := $VERSION;

   Config       : Command_Line_Configuration;

   Recursive                : aliased Boolean;
   Object_Dir               : aliased Boolean;
   Source_Dirs              : aliased Boolean;
   Exec_Dir                 : aliased Boolean;
   Library_Dir              : aliased Boolean;
   Source_Files             : aliased Boolean;
   Languages                : aliased Boolean;
   Env                      : aliased Project_Environment_Access;
   GNAT_Version             : aliased GNAT.Strings.String_Access;
   Proj                     : aliased Project_Tree;
   Project_File             : aliased GNAT.Strings.String_Access;
   Gnatls                   : aliased GNAT.Strings.String_Access := new String'("gnatls");
   Show_Version             : aliased Boolean;
   Direct_Imports           : aliased Boolean;
   DirName                  : aliased Boolean;
   BaseName                 : aliased Boolean;
   Source_Dirs_I            : aliased Boolean;
   No_Duplicates            : aliased Boolean;
   Displayed_Folders        : GNAT.Spitbol.Table_Boolean.Table (512);
   Exclude_RTS              : aliased Boolean := False;
   Exclude_Externally_Built : aliased Boolean := False;
   Exclude_No_Source        : aliased Boolean := False;
   Echo                     : aliased GNAT.Strings.String_Access;
   Exec                     : aliased GNAT.Strings.String_Access;
   Cwd                      : aliased Boolean;
   Verbose                  : aliased Boolean;
   Rts_Root                 : aliased GNAT.Strings.String_Access;
   Exclude_Pattern          : aliased GNAT.Strings.String_Access;

   procedure Output (Full_Name : String;
                     Base_Name : String;
                     Dir_Name  : String;
                     Name      : String;
                     missing   : String;
                     Continue  : out Boolean) is

      Command         : GNATCOLL.Arg_Lists.Arg_List;
      Commands_Template        : GNATCOLL.Arg_Lists.Arg_List;
      Subst           : GNATCOLL.Templates.Substitution_Array
        := ((new String'("full_name"), new String'(Full_Name)),
            (new String'("base_name"), new String'(Base_Name)),
            (new String'("dir_name"), new String'(Dir_Name)),
            (new String'("missing"), new String'(missing)),
            (new String'("name"), new String'(Name)));
      Args : GNAT.OS_Lib.Argument_List_Access;
      Cmd  : GNAT.OS_Lib.String_Access;
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
      if Exclude_Pattern /= null and then Exclude_Pattern.all /= "" then
         if GNAT.Regexp.Match ( Full_Name, GNAT.Regexp.Compile (Exclude_Pattern.all)) then
            return;
         end if;
      end if;

      if Echo /= null and then Echo.all /= "" then

         Commands_Template := GNATCOLL.Arg_Lists.Parse_String (Echo.all,Separate_Args);
         for I in 0 .. Args_Length (Commands_Template) loop
            Append_Argument (C => Command,
                             Argument => Substitute (Str => Nth_Arg (Commands_Template, I),
                                                     Substrings => Subst),
                             Mode     => One_Arg);
         end loop;
         GNAT.Io.Put_Line (GNATCOLL.Arg_Lists.To_Display_String (Command));
      end if;

      if Exec /= null and then Exec.all /= "" then
         Commands_Template := Parse_String (Exec.all, Separate_Args);
         for I in 0 .. Args_Length (Commands_Template) loop
            Append_Argument (C => Command,
                             Argument => Substitute (Str => Nth_Arg (Commands_Template, I),
                                                     Substrings => Subst),
                             Mode     => One_Arg);
         end loop;
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
         cmd := GNAT.OS_Lib.Locate_Exec_On_Path(GNATCOLL.Arg_Lists.Get_Command (Command));
         if GNAT.OS_Lib.Spawn (Cmd.all, Args.all) /= 0 then
            Continue := False;
         end if;
         Free (Cmd);
         Free (Args);
      end if;

      GNATCOLL.Templates.free(Subst);
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
      if Exclude_Externally_Built and then P.Attribute_Value (Build ("", "externally_built")) = "true" then
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
            Iter : Project_Iterator := P.Start (Recursive, Direct_Imports);
            Continue : Boolean;
         begin
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
               elsif echo /= null  or exec /= null  then
                  Output (Full_Name => +Full_Name (Current (Iter).Project_Path),
                          Base_Name => +Base_Name (Current (Iter).Project_Path),
                          Dir_Name  => +Dir_Name (Current (Iter).Project_Path),
                          Name      => Current (Iter).Name,
                          Missing   => "",
                          Continue  => Continue);

               else
                  Gnat.IO.Put_Line (+Full_Name (Current (Iter).Project_Path));
               end if;
               Next (Iter);
            end loop;

         end;
      end if;
   end;

   Matcher : constant GNAT.Regpat.Pattern_Matcher := Compile ("^((\w+\.gpr):\d+:\d+: unknown project file: ""(.+)"")");

   procedure Error_Report (Msg : String) is
      Matches : GNAT.Regpat.Match_Array (1 .. GNAT.Regpat.Paren_Count (Matcher));
      Continue : Boolean;
   begin
      Match (Matcher, Msg, Matches);
      if Matches (3) /= No_Match then
         if (Exec /= null and then Exec.all /= "") or (Echo /= null and then Echo.all /= "") then
            Output (Full_Name => "",
                    Base_Name => Msg (Matches (2).First .. Matches (2).Last),
                    Dir_Name  => "",
                    Name      => "",
                    Missing   => Msg (Matches (3).First .. Matches (3).Last),
                    Continue  => Continue);
         else
            GNAt.Io.Put_Line (Msg (Matches(3).First .. Matches(3).Last));
         end if;
      end if;
   end;

   Missing_Fail : aliased Boolean;
   Missing  : aliased Boolean;
begin
   Set_Usage (Config, Help => "Displays various aspects of .gpr-project files. " & ASCII.LF &
                "could also excute commands on the resulting project set." & ASCII.LF &
                "The usage usage is dual:" & ASCII.LF &
                " * Exctract aspects of a single project file to be used in scripts." & ASCII.LF &
                " * Iterate over project-trees." );
   Define_Switch (Config, Project_File'Access, "-P=", Help => "Use Project File proj", Argument => "proj");
   Define_Switch (Config, Recursive'Access, "-r",    Long_Switch => "--recursive" ,     Help => "Show recursive.");
   Define_Switch (Config, DirName'Access,            Long_Switch => "--dirname" ,       Help => "Show directorynames of projects.");
   Define_Switch (Config, No_Duplicates'Access,      Long_Switch => "--no-duplicates" , Help => "Don't duplicate folders when showing dirnames.");
   Define_Switch (Config, BaseName'Access,           Long_Switch => "--basename" ,  Help => "Show basenames of projects.");
   Define_Switch (Config, Missing'Access, "-m",      Long_Switch => "--missing" ,  Help => "Show missing projects.");
   Define_Switch (Config, Missing_Fail'Access, "-M", Long_Switch => "--Missing" ,  Help => "Show missing projects (exit with error if not complete).");
   Define_Switch (Config, Object_Dir'Access,         Long_Switch => "--object-dir" ,  Help => "Print object dir.");
   Define_Switch (Config, Source_Dirs'Access,        Long_Switch => "--source-dirs" , Help => "Print source dirs.");
   Define_Switch (Config, Source_Dirs_I'Access,      Long_Switch => "--source-dirs-include" , Help => "Print source dirs with ""-I "" before each entry.");
   Define_Switch (Config, Source_Files'Access, Long_Switch => "--source-files" , Help => "Print source files.");
   Define_Switch (Config, Direct_Imports'Access, Long_Switch =>      "--imports" , Help => "Print direct imports.");
   Define_Switch (Config, Exec_Dir'Access, Long_Switch => "--exec-dir" , Help => "Print exec dir.");
   Define_Switch (Config, Library_Dir'Access, Long_Switch => "--library-dir" , Help => "Print library dir.");
   Define_Switch (Config, Languages'Access, Long_Switch => "--languages" , Help => "Print project languages.");
   Define_Switch (Config, Gnatls'Access, Long_Switch => "--gnatls=" , Help => "Use as gnatls default=>'" & Gnatls.all & "' .", Argument => "gnatls");
   Define_Switch (Config, cwd'Access,  Long_Switch => "--cwd" , Help => "Change dir to projects enclosing dir before executing command.");
   Define_Switch (Config, Echo'Access,  Long_Switch => "--echo=" , Help => "Echo the argument with substitution %full_name %base_Name %dir_name %name %missing.", Argument => """Command line""");
   Define_Switch (Config, Exec'Access,  Long_Switch => "--exec=" , Help => "Execute the argument with substitution %full_name %base_Name %dir_name %name %missing.", Argument => """Command line""");
   Define_Switch (Config, Exclude_RTS'Access,  Long_Switch => "--no-rts" , Help => "Dont show projects from runtimes.");
   Define_Switch (Config, Exclude_Externally_Built'Access,  Long_Switch => "--no-externally-built" , Help => "Excludes externally built prokjects.");
   Define_Switch (Config, Exclude_No_Source'Access,  Long_Switch => "--no-empty-sources" , Help => "Excludes projects that dont have source files.");
   Define_Switch (Config, Exclude_Externally_Built'Access,  Long_Switch => "--no-externally-built" , Help => "Exclude Externally built projects");
   Define_Switch (Config, Exclude_Pattern'Access,  Long_Switch => "--exclude-pattern=" , Help => "Exclude project paths matching regexp.", Argument => "regexp");

   Define_Switch (Config, Show_Version'Access, "-v", Long_Switch => "--version" , Help => "Print version and then exit.");
   Define_Switch (Config, Verbose'Access,  Long_Switch => "--verbose" , Help => "Be verbose.");

   Getopt (Config);
   if Show_Version then
      GNAT.Io.Put_Line (Version);
      return;
   end if;
   Initialize (Env);

   if Direct_Imports then
      Recursive := True;
   end if;
   Env.Set_Path_From_Gnatls (Gnatls.all, GNAT_Version);
   declare
      realgnatls : GNAT.Strings.String_Access;
   begin
      Realgnatls := GNAT.OS_Lib.Locate_Exec_On_Path (Gnatls.all);
      Rts_Root   := new String'(Containing_Directory (Containing_Directory (Realgnatls.all)));
      Free (Realgnatls);
   end;

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

   if Missing or Missing_Fail then
      begin
         Proj.Load (Create (Filesystem_String (Project_File.all)), Env,  Errors => Error_Report'Unrestricted_Access, Recompute_View => False);
      exception
         when GNATCOLL.PROJECTS.INVALID_PROJECT  =>
            if Missing_Fail then
               ADA.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
            end if;
      end;
      return;
   else
      Proj.Load (Create (Filesystem_String (Project_File.all)), Env);
   end if;
   Display (Proj.Root_Project);
exception
   when GNAT.COMMAND_LINE.EXIT_FROM_COMMAND_LINE =>
      null;

end Gprinfo;
