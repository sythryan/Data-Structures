with Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Integer_Text_IO;
with Heart;
with Ada.Numerics.Elementary_Functions;
use  Ada.Numerics.Elementary_Functions;
procedure Cardioverter is

-- This program simulates the action of a Cardioverter-Defibrillator, a device that
-- is implanted in a patient to check and maintain correct heart rhythms.
--
-- Written by Syth Ryan
--
-- Input
--       From Keyboard    Patient's last name
--                        Lower bound of healthy zero crossings
--                        Upper bound of healthy zero crossings
--                        Number of readings to use in activity calculations
--
--       From Patient     Heart Muscle Activity
--
-- Output
--       Monitor initiation and termination messages.
--       Number of zero crossings for each heart monitoring cycle.
--       Statistics (mean and standard deviation of heart activity readings)
--       Status of heart for each heart monitoring cycle
--       Sensor failure messages.
--
-- Assumptions
--       1.  Patient's last name contains no more than 60 characters
--       2.  User enters a valid patient name
--       3.  User enters valid lower and upper bound values
--       4.  User enters valid number of readings for calculations


   subtype Name_String is String (1 .. 60);

   type Array_Type is array (Integer range <>) of Integer;

   -------------------------------------------------------------------------------
   function Average_Of (Values : in Array_Type) return Float is
   -- Find the average of a group of numbers
   --
   -- Preconditions  : Values'Length > 0
   --
   -- Postconditions : The average of Values is returned

      Sum : Float;  -- The sum of the Values
   begin
      Sum := 0.0;
      -- Sum the numbers in Values
      -- Each iteration, one value is added to the sum
      for Index in Values'Range loop
         Sum := Sum + Float (Values (Index));
      end loop;
      -- Calculate and return the average
      return Sum / Float (Values'Length);
   end Average_Of;


   -------------------------------------------------------------------------------
   function Standard_Deviation_Of (Values : in Array_Type) return Float is
   -- Find the standard deviation of a group of numbers
   --
   -- Preconditions  : Values'Length > 1
   --
   -- Postconditions : The standard deviation of Values is returned

      Average        : Float;  -- The average of Values
      Sum_Of_Squares : Float;  -- Sum of the squared variances of Values
   begin
      Sum_Of_Squares := 0.0;
      Average := Average_Of (Values);
      -- Sum the squares of the variances in Values
      -- Each iteration, add one square of a variance to the sum
      for Index in Values'Range loop
         Sum_Of_Squares := Sum_Of_Squares + (Float (Values (Index)) - Average) ** 2;
      end loop;
      -- Calculate and return the standard deviation
      return Sqrt (Sum_Of_Squares / Float (Values'Length - 1));
   end Standard_Deviation_Of;


   ---------------------------------------------------------------------------------
   function Convert (Num_Readings : in Positive;
                     Percent      : in Positive) return Natural is
   -- Converts a whole number to a percent and return the number of
   --    readings rounded to the nearest whole number
   --
   -- Preconditions  : none
   --
   -- Postconditions : The percent of the number of readings is returned
      Float_Readings : Float;
   begin
      Float_Readings := Float (Percent) / 100.0 * Float (Num_Readings);
      return Integer (Float_Readings);
   end Convert;

   ---------------------------------------------------------------------------------
   function negative (Num : in Heart.Reading) return Boolean is
   -- Test wether a Num is negative
   --
   -- Preconditions  : none
   --
   -- Postconditions : True is returned if Num is negative else false is returned
   begin
      return Integer (Num) < 0;
   end negative;

   ---------------------------------------------------------------------------------
   procedure Process (Initial_Reading : in out Heart.Reading;
                      Num_Readings    : in  Positive;
                      ZCC             : out Natural;
                      Value_Array     : out Array_Type) is
      Current_Read  : Heart.Reading;   -- Current reading
      Previous_Read : Heart.Reading;   -- Previous reading
      Count         : Integer;         -- Number of Readings so far
   begin
      Value_Array := (others => 0);
      Previous_Read := Initial_Reading;
      Count := 0;
      ZCC   := 0;
      loop
         exit when Count = Num_Readings;
         Count := Count + 1;
         Current_Read := Heart.Sense;
         if (negative (Previous_Read) and not negative (Current_Read)) or
               (not negative (Previous_Read) and negative (Current_Read)) then
            ZCC := ZCC + 1;
         end if;
         Value_Array (Count) := Integer (Current_Read);
         Previous_Read := Current_Read;
      end loop;
      Initial_Reading := Current_Read;
   end Process;

------------------------------------------------------------------------------------
   -- Patient data source
   Patient_Name : Name_String;             -- The patient's last name
   Length       : Positive;                -- Number of characters in Name

   Lower_Bound     : Positive range 10 .. 30; -- Upper Percent as a whole number
   Upper_Bound     : Positive range 70 .. 95; -- Lower Percent as a whole number
   Num_Readings    : Positive range 12 .. 48; -- Number of readings
   Initial_Reading : Heart.Reading;           -- Initial and previous reading
   Low_Amount      : Natural;                 -- Lower amount of readings
   Up_Amount       : Natural;                 -- Upper amount of readings
   ZCC             : Natural;                 -- Zero Cross Count
   Value_Array     : Array_Type (1 .. 48);    -- Array of current readings

begin
   -- Prepare the patient
   Ada.Text_IO.Put_Line ("Enter the patient's name");
   Ada.Text_IO.Get_Line (Item => Patient_Name, Last => Length);
   Heart.Initialize (Patient_Name (1 .. Length));

   -- Set up the parameters of the Cardioverter-Defibrillator
   Ada.Text_IO.Put_Line ("Enter the lower bound (percent)");
   Ada.Integer_Text_IO.Get (Lower_Bound);
   Ada.Text_IO.Put_Line ("Enter the upper bound (percent)");
   Ada.Integer_Text_IO.Get (Upper_Bound);
   Ada.Text_IO.Put_Line ("Enter number of readings used in a calculation");
   Ada.Integer_Text_IO.Get (Num_Readings);
   Initial_Reading := 0;

   -- Convert percentages into a number of readings (rounded to nearest reading)
   Low_Amount := Convert (Num_Readings, Lower_Bound);
   Up_Amount  := Convert (Num_Readings, Upper_Bound);

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Beginning patient heart monitoring");
   Ada.Text_IO.New_Line;

   loop
      exit when not Heart.Insured;
      begin
         Process (Initial_Reading => Initial_Reading,
                  Num_Readings    => Num_Readings,
                  ZCC             => ZCC,
                  Value_Array     => Value_Array);
         Ada.Integer_Text_IO.Put (ZCC, 5);
         Ada.Float_Text_IO.Put (Item => Average_Of (Value_Array (Value_Array'First .. Num_Readings)),
                                Fore => 5,
                                Aft  => 1,
                                Exp  => 0);
         Ada.Float_Text_IO.Put
                      (Item => Standard_Deviation_Of (Value_Array (Value_Array'First .. Num_Readings)),
                       Fore => 4,
                       Aft  => 2,
                       Exp  => 0);
         if ZCC > Up_Amount then
            Ada.Text_IO.Put (" Fibrillation!");
            Heart.Shock;
         elsif ZCC < Low_Amount then
            Ada.Text_IO.Put (" Cardiac Arrest!");
            Heart.Shock;
         else
            Ada.Text_IO.Put (" Normal Heart Rhythm");
         end if;
      exception
         when Heart.SENSE_ERROR =>
            Ada.Text_IO.Put ("Sensor Failure!");
      end;
      Ada.Text_IO.New_Line;
   end loop;

   Ada.Text_IO.New_Line (2);
   Ada.Text_IO.Put_Line ("Insurance terminated!");
   Ada.Text_IO.Put_Line ("Patient's Cardioverter-Defibrillator removed.");

end Cardioverter;


