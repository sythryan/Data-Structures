with Ada.Text_IO;
with Ada.Strings.Bounded;
with Word_IO;

procedure prelab is

   package My_Strings is new
      Ada.Strings.Bounded.Generic_Bounded_Length (Max => 32);

   package My_Word_IO is new
      Word_IO (File_Type => My_Strings);

   Data       : Ada.Text_IO.File_Type;
   File_Name  : My_Strings.Bounded_String;
   Word       : My_Strings.Bounded_String;
   Average    : Float   := 0;
   Word_Count : Natural := 0;

begin
   My_Word_IO.Get (File_Name);
   My_Word_IO.Open (File => Data,
                    Name => File_Name);
   loop
      exit when My_Word_IO.End_Of_File (Data);
      My_Word_IO.Get (File => File_Name,
                      Item => Word);
      Word_Count := Word_Count + 1;
      Average    := Float (Word'Length) + Average;
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