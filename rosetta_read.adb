with Ada.Text_IO;
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Command_Line;

procedure Rosetta_Read
is
   File : Ada.Text_IO.File_Type;
begin

   if Ada.Command_Line.Argument_Count = 0 then
      Ada.Text_IO.Put_Line (File => Ada.Text_IO.Standard_Error,
                            Item => "Usage: " &
                              Ada.Command_Line.Command_Name &
                              " file_name");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   -- Make sure the target exists and is a file (could be a directory etc.)
   declare
      use Ada.Command_Line; -- Argument, Set_Exit_Status, Failure
      use Ada.Directories; -- Exists, Kind
   begin
      if not Exists( Argument (Number => 1))
        or else not (Kind (Argument (Number => 1)) = Ordinary_File) then
         Ada.Text_IO.Put_Line (File => Ada.Text_IO.Standard_Error,
                               Item => "File " &
                                 Argument (Number => 1) &
                                 " does not exist or is not a regular file");
         Set_Exit_Status (Failure);
         return;
   end if;
   end;

   Ada.Text_IO.Open (File,
                     Mode => Ada.Text_IO.In_File,
                     Name => Ada.Command_Line.Argument (Number => 1));
   Ada.Text_IO.Set_Line (File, 7);

   begin

      declare
         Stored_Line : constant String := Ada.Text_IO.Get_Line (File);
      begin
         -- We assume that lines containing
         -- only whitespace are *not* empty.
         if Stored_Line = "" then
            Ada.Text_IO.Put_Line ("Line 7 in " &
                                    Ada.Command_Line.Argument (Number => 1) &
                                    " is empty");
         else
            Ada.Text_IO.Put_Line (Stored_Line);
         end if;
      end;
   exception
      when Standard.Storage_Error =>
         Ada.Text_IO.Put_Line (File => Ada.Text_IO.Standard_Error,
                               Item => "Line 7 of " &
                                 Ada.Command_Line.Argument (Number => 1) &
                                 " too long to store in memory available to this program");
         if Ada.Text_IO.Is_Open (File) then
            Ada.Text_IO.Close (File);
         end if;
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;
   end;

   Ada.Text_IO.Close (File);
exception
   when Ada.IO_Exceptions.End_Error =>
      Ada.Text_IO.Put_Line (File => Ada.Text_IO.Standard_Error,
                            Item => Ada.Command_Line.Argument (Number => 1) &
                              " has fewer than 7 lines");
      if Ada.Text_IO.Is_Open (File) then
         Ada.Text_IO.Close (File);
      end if;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   when others =>
      Ada.Text_IO.Put_Line (File => Ada.Text_IO.Standard_Error,
                            Item => "Error while trying to read file: " &
                           Ada.Command_Line.Argument (Number => 1));
      if Ada.Text_IO.Is_Open (File) then
         Ada.Text_IO.Close (File);
      end if;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Rosetta_Read;
