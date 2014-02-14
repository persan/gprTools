package Api is
   type Cb is abstract tagged  private;
   procedure Notify (Self : not null access Cb; Msg : String);


   type Appl is tagged  private;
   procedure Start (Self : not null access Appl; N : not null access Cb'Class);

private
   type cb is tagged null record;
   type Appl is tagged null record;
end api;
