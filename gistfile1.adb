with Ada.Text_IO;
with Ada.IO_Exceptions;

procedure Read
is
   File : Ada.Text_IO.File_Type;
begin  -- this is line 7
   Ada.Text_IO.Open (File, Mode => Ada.Text_IO.In_File, Name => "read.adb");
   Ada.Text_IO.Set_Line (File, 7);
   declare
      Stored_Line : String := Ada.Text_IO.Get_Line (File);
   begin
      Ada.Text_IO.Put_Line (Stored_Line);
   end;
   Ada.Text_IO.Close (File);
exception
   when Ada.IO_Exceptions.End_Error =>
      Ada.Text_IO.Put_Line("read.adb has less than 7 lines");
      if Ada.Text_IO.Is_Open (File) then
         Ada.Text_IO.Close (File);
      end if;
   when others =>
      Ada.Text_IO.Put_Line("Error while trying to read file: read.adb");
      if Ada.Text_IO.Is_Open (File) then
         Ada.Text_IO.Close (File);
      end if;
end Read;
