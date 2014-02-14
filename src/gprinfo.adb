pragma Ada_2012;
with GNAT.Command_Line; use GNAT.Command_Line;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNAT.Strings; use GNAT.Strings;
with Gnat.IO;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with Ada.Directories; use Ada.Directories;
with GNAT.Regpat; use GNAT.Regpat;
with Ada.Command_Line;
procedure Gprinfo is

   Version      : constant String := $VERSION;

   Config       : Command_Line_Configuration;

   Recursive        : aliased Boolean;
   Object_Dir       : aliased Boolean;
   Source_Dirs      : aliased Boolean;
   Exec_Dir         : aliased Boolean;
   Library_Dir      : aliased Boolean;
   Source_Files     : aliased Boolean;
   Languages        : aliased Boolean;
   Env              : aliased Project_Environment_Access;
   GNAT_Version     : aliased GNAT.Strings.String_Access;
   Proj             : aliased Project_Tree;
   Project_File     : aliased GNAT.Strings.String_Access;
   Gnatls           : aliased GNAT.Strings.String_Access := new String'("gnatls");
   Show_Version     : aliased Boolean;
   Imported         : aliased Boolean;
   DirName          : aliased Boolean;
   BaseName         : aliased Boolean;
   Source_Dirs_I    : aliased Boolean;

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
      elsif Imported then
         declare
            Iter : Project_Iterator := P.Start (True, not Recursive);
         begin
            loop
               exit when Current (Iter) = No_Project;
               if DirName then
                  Gnat.IO.Put_Line (+Dir_Name (Current (Iter).Project_Path));
               elsif Basename then
                  Gnat.IO.Put_Line (+Base_Name (Current (Iter).Project_Path));
               else
                  Gnat.IO.Put_Line (+Full_Name (Current (Iter).Project_Path));
               end if;
               Next (Iter);
            end loop;
         end;
      end if;
   end;


   Matcher : constant GNAT.Regpat.Pattern_Matcher := Compile ("^(\w+\.gpr:\d+:\d+: unknown project file: ""(.+)"")");

   procedure Error_Report (Msg : String) is
      Matches : GNAT.Regpat.Match_Array (1 .. GNAT.Regpat.Paren_Count (Matcher));
      Ix      : Match_Location renames Matches (2);
   begin
      Match (Matcher, Msg, Matches);
      if Matches (2) /= No_Match then
         GNAt.Io.Put_Line (Msg (Ix.First .. Ix.Last));
      end if;
   end;
   Missing_Fail : aliased Boolean;
   Missing  : aliased Boolean;
begin

   Define_Switch (Config, Project_File'Access, "-P=", Help => " Use Project File proj", Argument => "proj");
   Define_Switch (Config, Recursive'Access, "-r", Long_Switch => "--recursive" ,  Help => "Show recursive.");
   Define_Switch (Config, DirName'Access,  Long_Switch => "--dirname" ,  Help => "Show directorynames of projects.");
   Define_Switch (Config, BaseName'Access,  Long_Switch => "--basename" ,  Help => "Show basenames of projects.");
   Define_Switch (Config, Missing'Access, "-m", Long_Switch => "--missing" ,  Help => "Show missing projects.");
   Define_Switch (Config, Missing_Fail'Access, "-M", Long_Switch => "--Missing" ,  Help => "Show missing projects (exit with error if not complete).");
   Define_Switch (Config, Object_Dir'Access, Long_Switch => "--object-dir" ,  Help => "Print object dir.");
   Define_Switch (Config, Source_Dirs'Access, Long_Switch => "--source-dirs" , Help => "Print source dirs.");
   Define_Switch (Config, Source_Dirs_I'Access, Long_Switch => "--source-dirs-include" , Help => "Print source dirs with ""-I "" before each entry.");
   Define_Switch (Config, Source_Files'Access, Long_Switch => "--source-files" , Help => "Print source files.");
   Define_Switch (Config, Exec_Dir'Access, Long_Switch => "--exec-dir" , Help => "Print exec dir.");
   Define_Switch (Config, Library_Dir'Access, Long_Switch => "--library-dir" , Help => "Print library dir.");
   Define_Switch (Config, Languages'Access, Long_Switch => "--languages" , Help => "Print project languages.");
   Define_Switch (Config, Gnatls'Access, Long_Switch => "--gnatls=" , Help => "Use as gnatls.", Argument => "gnatls");
   Define_Switch (Config, Imported'Access, Long_Switch => "--imported" , Help => "Show first level of imported projects.");
   Define_Switch (Config, Show_Version'Access, "-v", Long_Switch => "--version" , Help => "Print version and then exit.");

   Getopt (Config);
   if Show_Version then
      GNAT.Io.Put_Line (Version);
      return;
   end if;
   Initialize (Env);


   Env.Set_Path_From_Gnatls (Gnatls.all, GNAT_Version);

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
end Gprinfo;
