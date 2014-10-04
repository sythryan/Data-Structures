with Ada.Text_IO;
with Ada.Strings.Bounded;
with Word_IO;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;

procedure prelab is

   package My_Strings is new
      Ada.Strings.Bounded.Generic_Bounded_Length (Max => 32);

   package My_Word_IO is new
      Word_IO (Max_Input_Line_Length => 1024,
               Word_Strings          => My_Strings);

   subtype My_String_Type is String (1 .. 32);

   Data        : My_Word_IO.File_Type;
   File_Name   : My_String_Type;
   Name_Length     : Natural;
   Word       : My_Strings.Bounded_String;
   Average    : Float   := 0.0;
   Word_Count : Natural := 0;

begin
   Ada.Text_IO.Put_Line ("Enter file name:");
   Ada.Text_IO.Get_Line (Item => File_Name,
                         Last => Name_Length);
   My_Word_IO.Open (File => Data,
                    Name => File_Name (1 .. Name_Length));
   loop
      exit when My_Word_IO.End_Of_File (Data);
      My_Word_IO.Get (File => Data,
                      Item => Word);
      Word_Count := Word_Count + 1;
      Average    := Float (My_Strings.Length (Word)) + Average;
   end loop;
   Average := Average / Float (Word_Count);
   Ada.Text_IO.Put ("Number of words: ");
   Ada.Integer_Text_IO.Put (Word_Count, 1);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("Average number of letters per word: ");
   Ada.Float_Text_IO.Put (Item => Average,
                          Fore => 2,
                          Aft  => 3,
                          Exp  => 0);

end prelab;