--  wsdl2aws SOAP Generator v2.4.0
--
--  AWS 3.3.0w - SOAP 1.5.0
--  This file was generated on Friday 15 May 2015 at 14:44:08
--
--  $ wsdl2aws gprslaves-nameserver.ads.wsdl -f -main gprslaves-server
--  -timeouts 1 -cb -spec gprslaves.nameserver

with AWS.Config.Set;
pragma Elaborate_All (AWS.Config.Set);
with AWS.Server;
pragma Elaborate_All (AWS.Server);
with AWS.Status;
pragma Elaborate_All (AWS.Status);
with AWS.Response;
pragma Elaborate_All (AWS.Response);
with SOAP.Dispatchers.Callback;
pragma Elaborate_All (SOAP.Dispatchers.Callback);

with Gprslaves.Nameserver_Service.CB;
pragma Elaborate_All (Gprslaves.Nameserver_Service.CB);
with Gprslaves.Nameserver_Service.Server;
pragma Elaborate_All (Gprslaves.Nameserver_Service.Server);

procedure Gprslaves.server is

   use AWS;

   function CB (Request : Status.Data) return Response.Data is
      R : Response.Data;
   begin
      return R;
   end CB;

   WS   : AWS.Server.HTTP;
   Conf : Config.Object;
   Disp : Gprslaves.Nameserver_Service.CB.Handler;

begin
   Config.Set.Server_Port (Conf, Gprslaves.Nameserver_Service.Server.Port);
   Disp :=
     SOAP.Dispatchers.Callback.Create
       (CB'Unrestricted_Access,
        Gprslaves.Nameserver_Service.CB.SOAP_CB'Access);

   AWS.Server.Start (WS, Disp, Conf);

   AWS.Server.Wait (AWS.Server.Forever);
end Gprslaves.server;
