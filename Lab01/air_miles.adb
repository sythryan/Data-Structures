with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.IO_Exceptions;

procedure Air_Miles is
--
-- Written by Syth Ryan
--
-- This program computes a file of employee flight data
-- and displays each employee's data along with totals
-- and most flights and miles flown
--
-- Input
-- File of employee flight data
--
-- Output
-- A table of employee flight data
--
-- Assumptions
-- The user enters a valid file name containing no more than 80 characters.
-- Each employee name is on a line by itself,
-- each set of flight information is on a line by itself,
-- and the word "Done" is on a line by itself.
-- No employee name contains more than 20 characters.
-- Employee names and the sentinel value "Done" are spelled correctly.
-- There are no characters between the last data item and the end of file marker.
-- There are no ties for either most flights or most miles flown
--
   type    Input_Range is (January, February, March, April, May, June, July,
                           August, September, October, November, December, Done);
   subtype Month_Range is Input_Range range January .. December;
   package Month_IO    is new Ada.Text_IO.Enumeration_IO (Enum => Input_Range);

   type Distance_Array is array (Month_Range range <>) of Natural;

   -- Types and constants for table output
   type Tab_Array   is array (Positive range <>) of Ada.Text_IO.Positive_Count;
   type Width_Array is array (Positive range <>) of Natural;
   Tab   : constant Tab_Array   := (1, 22, 31, 41, 54, 67);  -- six column
   Width : constant Width_Array := (20, 5, 6, 7, 7, 5);      -- table

   ----------------------------------------------------------------------------
   function Average (Values : in Distance_Array) return Natural is
   --
   -- Preconditions  : Values'Length > 0
   --
   -- Postconditions : Returns the average of the numbers in Values
   --

      Sum : Float;  -- Sum of all numbers in Values

   begin
      Sum := 0.0;
      -- Add up all numbers in Values
      -- Each iteration, add one value to the sum
      for Index in Values'Range loop
         Sum := Sum + Float (Values (Index));
      end loop;

      -- Return the average
      return Integer (Sum / Float (Values'Length));
   end Average;


   ----------------------------------------------------------------------------
   procedure Put_Dashed_Line (Length : in Natural) is

   -- Preconditions  : none
   --
   -- Postconditions : A line of Length dashes is displayed

   begin
      -- Display a line of dashes
      -- Each iteration, display one dash
      for Dash_Count in 1 .. Length loop
         Ada.Text_IO.Put ('-');
      end loop;
      Ada.Text_IO.New_Line;
   end Put_Dashed_Line;


   ----------------------------------------------------------------------------
   procedure Display_Headings is

   -- Preconditions  : none
   --
   -- Postconditions : Table headings are displayed

   begin
      Ada.Text_IO.New_Line;

      -- First line
      Ada.Text_IO.Set_Col (To => Tab (1));
      Ada.Text_IO.Put ("Name");
      Ada.Text_IO.Set_Col (To => Tab (2));
      Ada.Text_IO.Put ("Flights");
      Ada.Text_IO.Set_Col (To => Tab (3));
      Ada.Text_IO.Put ("Distance");
      Ada.Text_IO.Set_Col (To => Tab (4));
      Ada.Text_IO.Put ("Average 1st");
      Ada.Text_IO.Set_Col (To => Tab (5));
      Ada.Text_IO.Put ("Average 2nd");
      Ada.Text_IO.Set_Col (To => Tab (6));
      Ada.Text_IO.Put ("Bad Data");
      Ada.Text_IO.New_Line;

      -- Second line
      Ada.Text_IO.Set_Col (To => Tab (3));
      Ada.Text_IO.Put ("Flown");
      Ada.Text_IO.Set_Col (To => Tab (4));
      Ada.Text_IO.Put ("Half Year");
      Ada.Text_IO.Set_Col (To => Tab (5));
      Ada.Text_IO.Put ("Half Year");
      Ada.Text_IO.Set_Col (To => Tab (6));
      Ada.Text_IO.Put ("Entries");
      Ada.Text_IO.New_Line;

      -- Third line
      Ada.Text_IO.Set_Col (To => Tab (4));
      Ada.Text_IO.Put ("miles/month");
      Ada.Text_IO.Set_Col (To => Tab (5));
      Ada.Text_IO.Put ("miles/month");
      Ada.Text_IO.New_Line;

      Put_Dashed_Line (Length => Natural (Tab (Tab'Last)) + 8);
      Ada.Text_IO.New_Line;
   end Display_Headings;



   ---------------------------------------------------------------------------
   procedure Display_Totals (Flights : in Natural;
                             Miles   : in Natural) is
   --
   -- Preconditions  : none
   --
   -- Postconditions : The given values are displayed
   --
   begin
      Ada.Text_IO.New_Line;
      Put_Dashed_Line (Length => Natural (Tab (Tab'Last)) + 8);
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Set_Col (To => Tab (1));
      Ada.Text_IO.Put ("Totals");
      Ada.Text_IO.Set_Col (To => Tab (2));
      Ada.Integer_Text_IO.Put (Item  => Flights,
                               Width => Width (2));
      Ada.Text_IO.Set_Col (To => Tab (3));
      Ada.Integer_Text_IO.Put (Item  => Miles,
                               Width => Width (3));
      Ada.Text_IO.New_Line (2);
   end Display_Totals;


   ---------------------------------------------------------------------------
   procedure Display_Extremes (Name_Max_Flights : in String;
                               Max_Flights      : in Natural;
                               Name_Max_Miles   : in String;
                               Max_Miles        : in Natural) is
   --
   -- Preconditions  : none
   --
   -- Postconditions : The given values are displayed
   --
   begin
      Ada.Text_IO.Put ("The most flights (");
      Ada.Integer_Text_IO.Put (Item => Max_Flights, Width => 1);
      Ada.Text_IO.Put (") were flown by ");
      Ada.Text_IO.Put_Line (Name_Max_Flights);
      Ada.Text_IO.Put ("The most miles (");
      Ada.Integer_Text_IO.Put (Item => Max_Miles, Width => 1);
      Ada.Text_IO.Put (") were flown by ");
      Ada.Text_IO.Put_Line (Name_Max_Miles);
   end Display_Extremes;
   ----------------------------------------------------------------------------
   procedure Display_Employee (Name           : in String;
                               Distance       : in Natural;
                               Flights        : in Natural;
                               First_Average  : in Natural;
                               Second_Average : in Natural;
                               Bad_Entries    : in Natural) is
   --
   -- Preconditions  : none
   --
   -- Postconditions : The given values are displayed
   --
   begin
      Ada.Text_IO.Set_Col (To => Tab (1));
      Ada.Text_IO.Put (Name);
      Ada.Text_IO.Set_Col (To => Tab (2));
      Ada.Integer_Text_IO.Put (Item  => Flights,
                               Width => 5);
      Ada.Text_IO.Set_Col (To => Tab (3));
      Ada.Integer_Text_IO.Put (Item  => Distance,
                               Width => 6);
      Ada.Text_IO.Set_Col (To => Tab (4));
      Ada.Integer_Text_IO.Put (Item  => First_Average,
                               Width => 7);
      Ada.Text_IO.Set_Col (To => Tab (5));
      Ada.Integer_Text_IO.Put (Item  => Second_Average,
                               Width => 7);
      Ada.Text_IO.Set_Col (To => Tab (6));
      Ada.Integer_Text_IO.Put (Item  => Bad_Entries,
                               Width => 5);
      Ada.Text_IO.New_Line;
   end Display_Employee;

   ----------------------------------------------------------------------------
   procedure Process_Employee (Flight_File : in  Ada.Text_IO.File_Type;
                               Flights     : out Natural;
                               Distance    : out Natural;
                               Name        : out String;
                               Length      : out Natural) is
   --
   -- Preconditions  : File is open
   --                  Reading marker is on next employee
   --
   -- Postconditions : One employee's data is displayed
   --                  Number of flights and miles are added to current total
   --


      -- Variables for output
      Miles_Array    : Distance_Array (January .. December);    -- Number of miles for each month
      Month          : Input_Range;                             -- Month to add miles to
      First_Average  : Natural;                                 -- Miles/month first half of year
      Second_Average : Natural;                                 -- Miles/month second half of year
      Bad_Entries    : Natural;                                 -- Bad data entries count
      Current_Miles  : Natural;                                 -- Miles for one flight

   begin
      -- Initialize values
      Bad_Entries    := 0;
      Flights        := 0;  -- reset total flights and
      Distance       := 0;  -- miles for current employee
      Miles_Array    := (others => 0);

      -- Get name
      Ada.Text_IO.Get_Line (File => Flight_File,
                            Item => Name,
                            Last => Length);
      -- Process all flight data
      loop   -- Each iteration, process one flight data
         -- Get month
         Validation_Block :
         begin
            Month_IO.Get (File => Flight_File,
                          Item => Month);
            exit when Month = Done;
            -- Get distance
            Ada.Integer_Text_IO.Get (File => Flight_File,
                                     Item => Current_Miles);
            -- Check for valid mile range
            if Current_Miles < 100 or Current_Miles > 12500 then           -- out of valid miles range
               Bad_Entries := Bad_Entries + 1;                             -- increment bad entry
            else
               Miles_Array (Month) := Miles_Array (Month) + Current_Miles; -- Add current trip to month total
               Flights             := Flights + 1;                         -- increment flights
               Distance            := Distance + Current_Miles;            -- add to current total distance
            end if;
         exception
            when Ada.IO_Exceptions.Data_Error =>      -- incorrect month
               Bad_Entries := Bad_Entries + 1;
               Ada.Text_IO.Skip_Line (Flight_File);
         end Validation_Block;
      end loop;
      -- Calculate Averages
      First_Average  := Average (Miles_Array (January .. June));
      Second_Average := Average (Miles_Array (July .. December));
      -- Display employee's data
      Display_Employee (Name           => Name (Name'First .. Length),
                        Flights        => Flights,
                        Distance       => Distance,
                        First_Average  => First_Average,
                        Second_Average => Second_Average,
                        Bad_Entries    => Bad_Entries);

   end Process_Employee;
-------------------------------------------------------------------------------

   subtype File_String is String (1 .. 80);
   subtype Name_String is String (1 .. 20);

   -- Variables for the data file
   Flight_File        : Ada.Text_IO.File_Type;  -- File with employee flight information
   File_Name          : File_String;            -- Name of the file
   File_Length        : Natural;                -- Length of the file name

   -- Variables for  totals
   Flight_Sum         : Natural;                -- Number of overall flights
   Distance_Sum       : Natural;                -- Overall distance flown

   -- Variables for Max
   Name_Max_Flights   : Name_String;            -- Name of employee with greatest flights
   Max_Flights_Length : Natural;                -- and its length
   Max_Flights        : Natural;                -- Greatest Flights Flown
   Name_Max_Miles     : Name_String;            -- Name of employee with greatest miles
   Max_Miles_Length   : Natural;                -- and it's length
   Max_Miles          : Natural;                -- Greatest Miles Flown

   -- Variables for current employee data
   Flights            : Natural;                -- Current flights to check if greater
   Distance           : Natural;                -- Current miles to check if greater
   Name               : Name_String;            -- Name of employee with data being checked
   Name_Length        : Natural;                -- and it's length



begin  -- Air_Miles

   -- Initialize Values
   Max_Flights  := 0;
   Max_Miles    := 0;
   Flight_Sum   := 0;
   Distance_Sum := 0;
   Flights      := 0;
   Distance     := 0;

   -- Open the data file
   Ada.Text_IO.Put_Line ("Enter the name of the file with flight information.");
   Ada.Text_IO.Get_Line (Item => File_Name,
                         Last => File_Length);
   Ada.Text_IO.Open     (File => Flight_File,
                         Name => File_Name (1 .. File_Length),
                         Mode => Ada.Text_IO.In_File);
   Display_Headings;
   -- Process all employees' data
   loop -- Each iteration, process one employee
      exit when Ada.Text_IO.End_Of_File (Flight_File);
      -- Process employee data
      Process_Employee (Flight_File => Flight_File,
                        Flights     => Flights,
                        Distance    => Distance,
                        Length      => Name_Length,
                        Name        => Name);
      if not Ada.Text_IO.End_Of_File (Flight_File) then
         Ada.Text_IO.Skip_Line (Flight_File);
      end if;
      -- Check if greater values
      if Flights > Max_Flights then
         Max_Flights        := Flights;
         Name_Max_Flights   := Name;
         Max_Flights_Length := Name_Length;
      end if;
      if Distance > Max_Miles then
         Max_Miles        := Distance;
         Name_Max_Miles   := Name;
         Max_Miles_Length := Name_Length;
      end if;
      -- Add employee data to totals
      Flight_Sum   := Flight_Sum + Flights;
      Distance_Sum := Distance_Sum + Distance;
   end loop;
   -- Display total sums
   Display_Totals (Flights => Flight_Sum,
                   Miles   => Distance_Sum);
   -- Display extremes
   if Flight_Sum /= 0 then      -- Check if empty file
      Display_Extremes (Name_Max_Flights => Name_Max_Flights (1 .. Max_Flights_Length),
                        Max_Flights      => Max_Flights,
                        Name_Max_Miles   => Name_Max_Miles (1 .. Max_Miles_Length),
                        Max_Miles        => Max_Miles);
   end if;
   -- Close File
   Ada.Text_IO.Close (Flight_File);
end Air_Miles;