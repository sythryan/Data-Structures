with Unbounded_Word_IO; use Unbounded_Word_IO;
with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
procedure My_Test is

   Empty_File : File_Type;
   Unopened   : File_Type;
   One_Word   : File_Type;
   Two_Words  : File_Type;
   Test_Word  : Unbounded_String;

begin

   -- Test End_Of_File
   Ada.Text_IO.Put_Line ("End of file tests:");
   Ada.Text_IO.Put ("Empty file, expected True:   ");
   Open (File => Empty_File,
         Name => "Empty.txt");
   if End_Of_File (Empty_File) then
      Ada.Text_IO.Put_Line ("True");
   else
      Ada.Text_IO.Put_Line ("False");
   end if;
   Close (Empty_File);

   Ada.Text_IO.Put ("one word file, expected False :   ");
   Open (File => One_Word,
         Name => "One_Word.txt");
   if End_Of_File (One_Word) then
      Ada.Text_IO.Put_Line ("True");
   else
      Ada.Text_IO.Put_Line ("False");
   end if;
   Close (One_Word);

   begin
      Ada.Text_IO.Put ("unopened file, expected status_error:   ");
      Close (Unopened);
      if End_Of_File (Unopened) then
         Ada.Text_IO.Put_Line ("No Error");
      end if;
   exception
      when Ada.Text_IO.Status_Error =>
         Ada.Text_IO.Put_Line ("Status_Error");
   end;
   Ada.Text_IO.New_Line (2);

   -- Test Get
   Ada.Text_IO.Put_Line ("Get Tests: ");
   Ada.Text_IO.Put ("No words in file, expected End_Error:   ");
   begin
      Open (File => Empty_File,
            Name => "Empty.txt");

      Get (File => Empty_File,
           Item => Test_Word);
      Ada.Text_IO.Put_Line ("No_Error");
   exception
      when END_ERROR =>
         Ada.Text_IO.Put_Line ("End_Error");
   end;
   Close (Empty_File);
   begin

      Ada.Text_IO.Put ("File not open, expected Status_Error:   ");
      Close (Unopened);
      Open (File => Unopened,
            Name => "Not_Open.txt");

      Get (File => Unopened,
           Item => Test_Word);
      Ada.Text_IO.Put_Line ("No_Error");
   exception
      when Ada.Text_IO.Status_Error =>
         Ada.Text_IO.Put_Line ("Status_Error");
   end;
   Open (File => Two_Words,
         Name => "Two_Words.txt");
   Ada.Text_IO.Put ("Two words in file, expected Hello Goodbye:   ");
   Get (File => Two_Words,
        Item => Test_Word);
   Ada.Text_IO.Put_Line (To_String (Test_Word));
   Ada.Text_IO.New_Line (2);

   -- Test Unget
   Ada.Text_IO.Put_Line ("Unget Tests:");
   Ada.Text_IO.Put ("Unget, expected Hello Goodbye:   ");
   Unget (File => Two_Words,
          Word => Test_Word);
   Get (File => Two_Words,
        Item => Test_Word);
   Ada.Text_IO.Put_Line (To_String (Test_Word));


end My_Test;



   