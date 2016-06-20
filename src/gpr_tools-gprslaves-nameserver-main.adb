with AWS.Server;
with Gprslaves.Nameserver.Server;
with AWS.Config;
procedure GPR_Tools.Gprslaves.Nameserver.Main is
   Web_Server : AWS.Server.HTTP;
   Config     : AWS.Config.Object;
begin
   AWS.Server.Start (Web_Server => Web_Server,
                     Callback => Server.Request'Access,
                     Config => Config);

end GPR_Tools.Gprslaves.Nameserver.Main;
