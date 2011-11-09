with Ada.Text_IO;

procedure Read
is
   File : Ada.Text_IO.File_Type;
begin
   Ada.Text_IO.Open (File, Mode => Ada.Text_IO.In_File, Name => "read.adb"); -- line 7
   Ada.Text_IO.Set_Line (File, 7);
   declare
      Stored_Line : String := Ada.Text_IO.Get_Line (File);
   begin
      Ada.Text_IO.Put_Line (Stored_Line);
   end;
   Ada.Text_IO.Close (File);
exception
   when others =>
      Ada.Text_IO.Put_Line ("Error while reading file!");
end Read;
