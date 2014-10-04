with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.IO_Exceptions;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;

procedure Tab_Remover is

-- Written by Syth Ryan
--
-- This program Changes all tabs to a specified amount of spaces and
-- echos the data to a new file.
--
-- Input From keyboard:
-- None
--
-- Arguments from the command line:
-- Input file
-- Output file
-- Spaces per tab (optional, default = 3)
--
-- Output:
-- A processed output file
-- Errors
-- Number of tabs in a file
-- Number of lines process in a file
--

   procedure Process_Line (In_File   : in out Ada.Text_IO.File_Type;
                           Out_File  : in out Ada.Text_IO.File_Type;
                           Tab_Count : in out Natural) is

      Char    : Character;
      N       : Natural := 3;

   begin
      if Ada.Command_Line.Argument_Count > 2 then
         N := Natural'Value (Ada.Command_Line.Argument (3));
      end if;

      -- Process one line
      -- Each iteration, process one character
      loop
         exit when Ada.Text_IO.End_Of_Line (In_File);
         Ada.Text_IO.Get (File => In_File,
                          Item => Char);

         -- Replace tab with spaces
         if Char = Ada.Characters.Latin_1.HT then
            Ada.Text_IO.Put (File => Out_File,
                             Item => N * ' ');
            Tab_Count := Tab_Count + 1;
         else
            Ada.Text_IO.Put (File => Out_File,
                             Item => Char);
         end if;
      end loop;
   end Process_Line;

-------------------------------------------------------------------------------

   Out_File      : Ada.Text_IO.File_Type;
   In_File       : Ada.Text_IO.File_Type;
   Tab_Count     : Natural := 0;
   Line_Count    : Natural := 0;

begin
   begin
      Ada.Text_IO.Open (File => In_File,
                        Mode => Ada.Text_IO.In_File,
                        Name => Ada.Command_Line.Argument (1));

      Ada.Text_IO.Create (File => Out_File,
                          Mode => Ada.Text_IO.Out_File,
                          Name => Ada.Command_Line.Argument (2));
      -- Process a whole text file
      -- Each iteration, process one line
      loop
         exit when Ada.Text_IO.End_Of_File (In_File);

         Process_Line (In_File   => In_File,
                       Out_File  => Out_File,
                       Tab_Count => Tab_Count);
         Line_Count := Line_Count + 1;
         if not Ada.Text_IO.End_Of_File (In_File) then
            Ada.Text_IO.Skip_Line (In_File);
         end if;

         Ada.Text_IO.New_Line (Out_File);
      end loop;
      -- Exceptions
   exception
      when Constraint_Error =>
         Ada.Text_IO.Put ("Constraint_Error, invalid range");
         Ada.Text_IO.New_Line;

      when Ada.IO_Exceptions.Use_Error =>
         Ada.Text_IO.Put ("Use_Error, reopening shared file");
         Ada.Text_IO.New_Line;

      when Ada.IO_Exceptions.Status_Error =>
         Ada.Text_IO.Put ("Status_Error, file not open");
         Ada.Text_IO.New_Line;

      when Ada.IO_Exceptions.Name_Error =>
         Ada.Text_IO.Put_Line ("Name_Error, file does not exist.");
         Ada.Text_IO.New_Line;
   end;

   Ada.Text_IO.Put ("Number of tab characters found and replaced: ");
   Ada.Integer_Text_IO.Put (Tab_Count, 1);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("Number of lines proccessed: ");
   Ada.Integer_Text_IO.Put (Line_Count, 1);
   Ada.Text_IO.New_Line (2);

   -- Close files
   Ada.Text_IO.Close (In_File);
   Ada.Text_IO.Close (Out_File);
end Tab_Remover;