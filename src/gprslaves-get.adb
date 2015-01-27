with GNAT.Sockets;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with GNAT.Exception_Traces;
procedure Gprslaves.Get is
   use GNAT.Sockets;
   Address  : Sock_Addr_Type;
   Socket   : Socket_Type;
   Channel  : Stream_Access;
   R        : Request;
begin
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
   GNAT.Exception_Traces.Set_Trace_Decorator (GNAT.Traceback.Symbolic.Symbolic_Traceback'Access);
   Clear (R);
   Create_Socket (Socket, Mode => Socket_Datagram);
   Set_Socket_Option
     (Socket,
      Socket_Level,
      (Reuse_Address, True));

   Set_Socket_Option
     (Socket,
      IP_Protocol_For_IP_Level,
      (Multicast_TTL, 1));

   Set_Socket_Option
     (Socket,
      IP_Protocol_For_IP_Level,
      (Multicast_Loop, True));
   Set_Socket_Option
     (Socket,
      IP_Protocol_For_IP_Level,
      (Receive_Timeout, 0.5));

   Address.Addr := Inet_Addr (Multicast_Group);
   Address.Port := DEFAULT_PORT;

   Channel := Stream (Socket, Address);

   --  Send message to server Pong

   Request'Output (Channel, R);

   --  Receive and print message from server Pong
   loop
      declare
         Message : constant Response := Response'Input (Channel);

      begin
         Address := Get_Address (Channel);
         Ada.Text_IO.Put_Line ("from " & Image (Address)  & "Service => " & Image (Message.Address));
      exception
         when others =>
            exit;
      end;
   end loop;
   Close_Socket (Socket);
end Gprslaves.Get;
