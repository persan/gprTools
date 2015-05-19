with GNAT.Command_Line;
with Gprslaves.DB;
procedure Gprslaves.Launcher is

begin
   Self.Initialize;
   loop
      case Getopt ("? h -help " &
                     "v V -version " &
                     "j: -jobs= " &
                     "h: -hostname= " &
                     "r: -response-handler= " &
                     "d: -direcory= " &
                     "p: -port= " &
                     "v -verbose " &
                     "vv -debug " &
                     "n: -nameserver= "
                     "D!") is  -- Accepts '-a', '-ad', or '-b argument'
         when ASCII.NUL => exit;

         when '-' =>
            if Full_Switch = "-version" then
               Put_Line ("Got a");
            elsif Full_Switch = "-response-handler" then
               Put_Line ("Got ad");
            elsif Full_Switch = "-direcory" then
               Put_Line ("Got ad");
            elsif Full_Switch = "-port" then
               Put_Line ("Got ad");
            elsif Full_Switch = "-verbose" then
               Put_Line ("Got ad");
            elsif Full_Switch = "-debug" then
               null;
            elsif Full_Switch = "-nameserver" then
               Put_Line ("Got ad");
            elsif Full_Switch = "-help" then
               Put_Line ("Got ad");
            end if;

         when 'v' | 'V' => null;
         when 'j' => null;
         when 'r' => null;
         when 'd' => null;
         when 'n' => null;
         when 'p' => null;
         when 'D' => null;
         when '?' | "h" =>
            null;

         when others =>
            raise Program_Error; -- cannot occur
      end case;
   end loop;

end Gprslaves.Launcher;
