with Discrete_Set;
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
procedure Assign3 is
--
-- Written by Syth Ryan
--
-- This program processes a file with many sets of hobbies
--
-- Input:
-- File Name
--
-- Output:
-- Prompt for file name
-- Name and number of hobbies for said person
-- Set of hobbies everbody participates in
-- Set of hobbies in which at least one person participates in
-- Set of hobbies for which one person participates but not all
-- Set of hobbies no one participates in
--
-- Assumptions:
-- A line terminator immediately follows the last hobby on the line in the file
-- All the data in the file is valid
--

   type Hobby_Type is (Skiing, Skating, Tennis, Baseball, Soccer, Football,
                       Basketball, Hearts, Old_Maid, Five_Hundred, Bridge,
                       Dancing, Music, Photography, Painting, Gardening,
                       Mountain_Climbing, Hiking, Fishing, Hunting, Sewing,
                       Knitting, Reading, Net_Surfing, Woodworking);
   pragma Warnings (Off, Hobby_Type);

   package  Hobby_Set is new Discrete_Set (Element_Type => Hobby_Type);
   use type Hobby_Set.Set_Type;

   package Hobby_IO is new Ada.Text_IO.Enumeration_IO (Hobby_Type);

   subtype Name_String is String (1 .. 10);
   subtype File_String is String (1 .. 100);

   ----------------------------------------------------------------------------
   function Size (Set : in Hobby_Set.Set_Type) return Natural is
   -- Preconditions  : none
   --
   -- Postconditions : Returns the number of items in Set
      Result : Natural;
   begin
      Result := 0;
      -- Add all the hobbies in the set of hobbies
      -- Each iteration, check one hobby
      for Hobby in Hobby_Type loop
         if Hobby_Set.Is_Member (Set => Set, Element => Hobby) then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Size;

   ----------------------------------------------------------------------------
   procedure Put (Item        : in Hobby_Set.Set_Type) is
   -- Output a set of hobbies to the screen
   --
   -- Preconditions  : none
   --
   -- Postconditions : All hobbies in Item are displayed on separate lines

   begin
      -- Display all of the hobbies in the set of hobbies
      -- Each iteration, process one potential hobby
      for Hobby in Hobby_Type loop
         if Hobby_Set.Is_Member (Set => Item, Element => Hobby) then
            Hobby_IO.Put (Item => Hobby);
            New_Line;
         end if;
      end loop;
   end Put;

   ----------------------------------------------------------------------------
   procedure Get_Hobbies (File : in Ada.Text_IO.File_Type;
                          Set  : out Hobby_Set.Set_Type) is
   --
   -- Get a set of hobbies for one person
   --
   -- preconditions  : File is open
   --
   -- postconditions : All hobbies for one person are added to set
   --
      Hobby : Hobby_Type;
   begin
      Set := Hobby_Set.Empty_Set;
      -- Get all hobbies for one person
      -- Each iteration, get one hobby
      loop
         exit when Ada.Text_IO.End_Of_Line (File);
         Hobby_IO.Get (File => File,
                       Item => Hobby);
         Set := Set + Hobby;
      end loop;
   end Get_Hobbies;

-------------------------------------------------------------------------------
   -- The data file
   Data      : Ada.Text_IO.File_Type;
   File_Name : File_String;
   Last      : Natural;               -- Position of Last characater in File_Name

   -- Current Person
   Name         : Name_String;
   Name_Last    : Natural;
   Current_Set  : Hobby_Set.Set_Type;

   -- Overall sets
   Every_Set        : Hobby_Set.Set_Type;
   No_Set           : Hobby_Set.Set_Type;
   Someone_Set      : Hobby_Set.Set_Type;
   Only_Someone_Set : Hobby_Set.Set_Type;

begin
   -- Initialize Sets
   Every_Set        := Hobby_Set.Empty_Set;
   No_Set           := Hobby_Set.Universal_Set;
   Only_Someone_Set := Hobby_Set.Empty_Set;
   Someone_Set      := Hobby_Set.Empty_Set;

   -- Get the file name and open the file
   Put_Line ("Enter the name of the data file");
   Get_Line (Item => File_Name,
             Last => Last);
   New_Line;
   Open (File => Data,
         Mode => In_File,
         Name => File_Name (1 .. Last));
   -- Process all persons and their hobbies in the file
   -- Each iteration, process one person and hobbies
   loop
      exit when End_Of_File (Data);
      -- Get Name
      Get_Line (File => Data,
                Item => Name,
                Last => Name_Last);

      -- Get hobbies
      Get_Hobbies (File => Data,
                   Set  => Current_Set);
      if not End_Of_File (Data) then
         Skip_Line (Data);
      end if;
      -- Add hobbies to totals
      Every_Set   := Someone_Set * Current_Set;
      No_Set      := No_Set - Current_Set;
      Someone_Set := Someone_Set + Current_Set;

      -- Put Current Person
      Put (Name (1 .. Name_Last));
      Put ("participates in ");
      Put (Item => Size (Current_Set),
          Width => 1);
      Put (" hobbies");
      New_Line (2);

   end loop;


   New_Line (2);
   Close (Data);

   Put_Line ("Hobbies in which everyone participates");
   Put (Item => Every_Set);
   New_Line (2);
   Put_Line ("Hobbies in which at least one person participates");
   Put (Item => Someone_Set);
   New_Line (2);
   Only_Someone_Set := Someone_Set - Every_Set;
   Put_Line ("Hobbies in which someone but not everyone participates");
   Put (Item => Only_Someone_Set);
   New_Line (2);
   Put_Line ("Hobbies in which no one participates");
   Put (Item => No_Set);
   New_Line (2);

end Assign3;
