with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Read
is
   package TIO renames Ada.Text_IO;
   package US renames Ada.Strings.Unbounded; -- this is line 7

   File : TIO.File_Type;
   Stored_Line : US.Unbounded_String;
begin
   TIO.Open (File, Mode => Ada.Text_IO.In_File, Name => "read.adb");
   TIO.Set_Line (File, 7);
   Stored_Line := US.To_Unbounded_String (TIO.Get_Line (File));
   TIO.Put_Line ( US.To_String (Stored_Line));
   TIO.Close (File);
end Read;
