with Ada.Text_IO;   use Ada.Text_IO;
with Vector_ADT;    use  type Vector_ADT.Vector;
with Vector_ADT.Vector_IO;

procedure Vector_Test is

   subtype My_String is String (1 .. 100);

   ------------------------------------------------------------
   procedure Put_Dashed_Line (Length : in Natural) is
   -- Display a line of Length dashes
   begin
      for Count in 1 .. Length loop
         Put ('-');
      end loop;
      New_Line;
   end Put_Dashed_Line;

   ------------------------------------------------------------
   procedure Display_Results (Vector_1 : Vector_ADT.Vector;
                              Vector_2 : Vector_ADT.Vector;
                              Expected : Vector_ADT.Vector;
                              Actual   : Vector_ADT.Vector;
                              Operator : Character) is
   -- This procedure displays the result of a test of a vector operation

   begin
      Vector_ADT.Vector_IO.Put (Item => Vector_1,
                                Fore => 1,
                                Aft  => 2,
                                Exp  => 0,
                                Raw  => False);
      Put (' ' & Operator & ' ');
      Vector_ADT.Vector_IO.Put (Item => Vector_2,
                                Fore => 1,
                                Aft  => 2,
                                Exp  => 0,
                                Raw  => False);

      New_Line (2);
      Put ("Expect ");
      Vector_ADT.Vector_IO.Put (Item => Expected,
                                Fore => 1,
                                Aft  => 2,
                                Exp  => 0,
                                Raw  => False);
      New_Line;
      Put ("Actual ");
      Vector_ADT.Vector_IO.Put (Item => Actual,
                                Fore => 1,
                                Aft  => 2,
                                Exp  => 0,
                                Raw  => False);
      New_Line;
   end Display_Results;


   ------------------------------------------------------------
   -- Variables for input data file
   File_Name  : My_String;
   My_File    : Ada.Text_IO.File_Type;

   Goal : My_String; -- Goal of the current test

   Last : Natural;   -- For input string lengths

   Vector_1   : Vector_ADT.Vector;
   Vector_2   : Vector_ADT.Vector;
   Expected   : Vector_ADT.Vector;
   Actual     : Vector_ADT.Vector;
   Operator   : Character;

begin
   -- Prepare the data file
   Ada.Text_IO.Put_Line ("Enter the name of the text file with the test data");
   Ada.Text_IO.Get_Line (Item => File_Name,
                         Last => Last);
   New_Line (2);
   Ada.Text_IO.Open (File => My_File,
                     Mode => Ada.Text_IO.In_File,
                     Name => File_Name (1 .. Last));

   loop
      exit when End_Of_File (My_File);

      Get_Line (File => My_File,
                Item => Goal,                       -- Get the current test goal
                Last => Last);

      Get (File => My_File,                         -- Get the operator
           Item => Operator);

      Vector_ADT.Vector_IO.Get (File => My_File,    -- Get first vector
                                Item => Vector_1);

      Vector_ADT.Vector_IO.Get (File => My_File,    -- Get second vector
                                Item => Vector_2);

      Vector_ADT.Vector_IO.Get (File => My_File,    -- Get expected result
                                Item => Expected);

      if not End_Of_File (My_File) then
         Skip_Line (My_File);
      end if;

      Put_Dashed_Line (Last);
      Put_Line (Goal (1 .. Last));
      New_Line;

      begin
         -- Carry out the operation
         case Operator is
            when '+' =>
               Actual := Vector_1 + Vector_2;
            when '-' =>
               Actual := Vector_1 - Vector_2;
            when '*' =>
               Actual := Vector_1 * Vector_2;
            when others =>
               Put_Line ("Invalid operator entered");
               Actual := (others => 0.0);  -- assign an arbitrary result
         end case;

      exception
         when Constraint_Error =>
            Put_Line ("A component in the result is out of range.");
            Actual := (others => 0.0);  -- assign an arbitrary result
      end;

      Display_Results (Vector_1, Vector_2, Expected, Actual, Operator);

      New_Line (2);
   end loop;

   Ada.Text_IO.Close (My_File);
end Vector_Test;