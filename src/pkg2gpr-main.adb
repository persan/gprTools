procedure Pkg2gpr.Main is
   D : Descr;
begin
   D.Read_Pkg ("/usr/lib64/pkgconfig/omniConnectionMgmt4.pc");
   D.Write_GPR (D.Get_GPR);
end Pkg2gpr.Main;
