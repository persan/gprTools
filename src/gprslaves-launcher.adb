with GNAT.Sockets;
with Ada.Text_IO;
procedure Gprslaves.Launcher is
   use GNAT.Sockets;
   Address  : Sock_Addr_Type;
   Socket   : Socket_Type;
   Channel  : Stream_Access;
   Service  : Response;
begin
   Service.Address.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);
   Service.Address.Port := 5876;

   Create_Socket (Socket, Mode => Socket_Datagram);
   Address.Port := DEFAULT_PORT;
   Bind_Socket (Socket  => Socket,
                Address => Address);
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

   Channel := Stream (Socket);

   declare
      Message : constant Request := Request'Input (Channel);
      Response_Channel  : Stream_Access;

   begin
      Response_Channel := Stream (Socket, Get_Address (Channel));
      Response'Output (Response_Channel, Service);
      Ada.Text_IO.Put_Line (Image (Message) & " from " & Image (Get_Address (Channel)));
   end;

   Close_Socket (Socket);
end Gprslaves.Launcher;
