with GNAT.Spitbol; use GNAT.Spitbol;
with Ada.Strings.Unbounded;
with GNAT.Expect;
with GNATCOLL.Projects;
with GNATCOLL.Utils;
with GNAT.OS_Lib; use GNAT.OS_Lib;
use GNAT.Expect;
with Ada.Unchecked_Deallocation;
package body Gprslaves.DB is
   use GNAT.Spitbol.Table_VString;
   use type Ada.Strings.Unbounded.Unbounded_String;
   --------------
   -- Register --
   --------------

   procedure Register
     (Self   : in out Table;
      Host   : Host_Address;
      Keys   : GNAT.Spitbol.Table_VString.Table)
   is
   begin
      for I of Self.Hosts loop
         if I.Host = Host then
            I.Keys := Keys;
            return;
         end if;
      end loop;
      Self.Hosts.Append (Info_Struct'(Host, Keys));
   end Register;

   ----------
   -- Find --
   ----------

   function Find
     (Self   : Table;
      Keys   : GNAT.Spitbol.Table_VString.Table)
      return Host_Info_Vectors.Vector
   is
   begin
      return Ret : Host_Info_Vectors.Vector do
         for Candidate of Self.Hosts loop
            declare
               Search_Keys : constant Table_Array := Convert_To_Array (Keys);
               OK          : Boolean := True;
            begin
               for I of Search_Keys loop
                  if Present (Candidate.Keys, I.Name) then
                     if Get (Candidate.Keys, I.Name) /= I.Value then
                        OK := False;
                     end if;
                  else
                     OK := False;
                  end if;
                  if OK then
                     Ret.Append (Candidate.Host);
                  end if;
               end loop;
            end;
         end loop;
      end return;
   end Find;


   procedure Append (Self      : in out Info_Struct;
                     Key_Name  : String;
                     Key_Value : String) is
   begin
      Set (Self.Keys, Key_Name, V (Key_Value));
   end Append;

   function Get_Free_Port (Default : GNAT.Sockets.Port_Type := 8484) return GNAT.Sockets.Port_Type is
      S : GNAT.Sockets.Socket_Type;
      A : GNAT.Sockets.Sock_Addr_Type;
   begin
      A.Addr := GNAT.Sockets.Any_Inet_Addr;
      A.Port := Default;
      GNAT.Sockets.Create_Socket (S);
      begin
         GNAT.Sockets.Bind_Socket (S, A);
         GNAT.Sockets.Close_Socket (S);
      exception
         when others =>
            A.Port := GNAT.Sockets.Any_Port;
            GNAT.Sockets.Bind_Socket (S, A);
            A := GNAT.Sockets.Get_Socket_Name (S);
            GNAT.Sockets.Close_Socket (S);
      end;
      return A.Port;
   end Get_Free_Port;

   function Get_Gnat_Version return String is
         Env          : GNATCOLL.Projects.Project_Environment_Access;
      Fd           : GNAT.Expect.Process_Descriptor_Access;
      Gnatls_Args    : GNAT.OS_Lib.Argument_List_Access :=
                         Argument_String_To_List ("gnatls" & " -v");
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Process_Descriptor'Class, Process_Descriptor_Access);
   begin
      GNATCOLL.Projects.Initialize (Env);
      GNATCOLL.Projects.Spawn_Gnatls (Self => Env.all, Fd => Fd, Gnatls_Args => Gnatls_Args, Errors => null);
      if Fd /= null then
         declare
            S : constant String := GNATCOLL.Utils.Get_Command_Output (Fd);
            Index : Integer := S'First;
            First : Integer;
            Last : Integer;
         begin
            GNATCOLL.Utils.Skip_To_String (S, Index, "GNATLS");
            while S (Index) /= ' ' loop
               Index := Index + 1;
            end loop;
            GNATCOLL.Utils.Skip_Blanks (S, Index);
            First := Index;
            Last  := GNATCOLL.Utils.Line_End (S, Index);
            Unchecked_Free (Fd);
            return S (First .. Last);
         end;
      end if;
      Free (Gnatls_Args);
      return "--";
   end Get_Gnat_Version;


   procedure Initialize (Self     : in out Info_Struct;
                         HostName : String := GNAT.Sockets.Host_Name;
                         Port     : GNAT.Sockets.Port_Type := Get_Free_Port) is

   begin
      Self.Host := (V (HostName), Port);
      Self.Append ("GNAT", Get_Gnat_Version);
   end Initialize;


end Gprslaves.DB;
