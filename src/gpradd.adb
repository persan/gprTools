with Ada.Command_Line;
with Ada.Text_IO;

with GNAT.OS_Lib;
with GNAT.Strings;

with GNATCOLL.Projects;
with GNATCOLL.VFS;
with Ada.Directories;

procedure Gpradd is

   use GNATCOLL.VFS;
   use GNAT.Strings;
   use GNATCOLL.Projects;
   use Ada.Directories;
   Env          : Project_Environment_Access;
   GNAT_Version : GNAT.Strings.String_Access;
   New_Path     : GNATCOLL.VFS.File_Array_Access;
   Sys_Path     : GNATCOLL.VFS.File_Array_Access;
   Found        : Boolean;

begin
   Initialize (Env);

   declare --  Read new entries from the command line.
      F : Virtual_File;
   begin
      for I in 1 .. Ada.Command_Line.Argument_Count loop

         if Exists (Ada.Command_Line.Argument (I))
           and then (Kind (Ada.Command_Line.Argument (I)) = Directory)
         then
            F :=
              Create
                (Filesystem_String
                   (Ada.Directories.Full_Name
                      (Ada.Command_Line.Argument (I))));
            if New_Path /= null then
               Found := False;
               for I of New_Path.all loop
                  if I = F then
                     Found := True;
                  end if;
               end loop;
            end if;
            if not Found then
               Append (New_Path, F);
            end if;
         end if;
      end loop;
   end;

   begin --  Get the original Project-PATH
      Env.all.Set_Path_From_Gnatls ("gnatls", GNAT_Version);
      Append (New_Path, Env.all.Predefined_Project_Path);
   end;

   begin --  Remove all system entries.
      GNAT.OS_Lib.Setenv ("ADA_PROJECT_PATH", "");
      GNAT.OS_Lib.Setenv ("GPR_PROJECT_PATH", "");
      Env.all.Set_Path_From_Gnatls ("gnatls", GNAT_Version);
      Append (Sys_Path, Env.all.Predefined_Project_Path);
      for I of Sys_Path.all loop
         Remove (New_Path, I);
      end loop;
      Unchecked_Free (Sys_Path);
   end;

   begin --  Build a new path with uniqe entries.
      for I of New_Path.all loop
         Found := False;
         if Sys_Path /= null then
            for J of Sys_Path.all loop
               if J = I then
                  Found := True;
               end if;
            end loop;
         end if;
         if not Found then
            Append (Sys_Path, I);
         end if;
      end loop;
   end;

   begin --  print the result.
      Ada.Text_IO.Put_Line
        ("export GPR_PROJECT_PATH=" & String (To_Path (Sys_Path.all)));
      Ada.Text_IO.Put_Line ("export ADA_PROJECT_PATH=");
   end;

   Unchecked_Free (New_Path);
end Gpradd;
