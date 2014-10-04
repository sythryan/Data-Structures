with Ada.Unchecked_Deallocation;
package body Big_Natural is

-- Written by Syth Ryan

   procedure Recycle is new Ada.Unchecked_Deallocation (Object => Digit_Array,
                                                        Name   => Number_Ptr);

   ----------------------------------------------------------------------------
   -- Local (helper) operations
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Remove_Leading_Zeros (Item : in out Number_Ptr) is
   -- Removes any leading zeros from Item
   -- Preconditions  : Item in not null
   -- Postconditions : Item designates any array of digits with no leading zeros

      Ptr   : Number_Ptr;
      Index : Integer;
      Zeros : Natural;

   begin
      Index := Item.all'Last;
      Zeros := 0;
      -- Check for number of leading zeros
      -- Each iteration check one digit
      loop
         exit when Item.all (Index) /= 0 or
                        Index = Item.all'First;
         Index := Index - 1;
         Zeros := Zeros + 1;
      end loop;

      -- Remove leading zeros
      Ptr := Item;
      Item := new Digit_Array'(Item.all (0 .. Item.all'Last - Zeros));
      Recycle (Ptr);
   end Remove_Leading_Zeros;

   ----------------------------------------------------------------------------
   function "*" (Left : in Big_Natural; Right : in Digit) return Big_Natural is
   -- Returns the product of a big natural and a single digit.  Written
   -- as a helper function for the multiplication of two big natural numbers
   -- Preconditions  : Left has at least one digit
   -- Postconditions : The product of Left and Right is returned

      Result  : Big_Natural;
      Carry   : Natural;
      Product : Natural;

   begin
      -- Allocate new memory for Result
      Result.Reference := new Digit_Array (0 .. Left.Reference.all'Last + 1);

      Carry := 0;
      -- Process all but last of Result
      -- Each itereation, process one index of Result
      for Index in Left.Reference.all'First .. Left.Reference.all'Last loop
         Product := Left.Reference.all (Index) * Right + Carry;
         Carry := Product / 10;
         Result.Reference.all (Index) := Product rem 10;
      end loop;

      -- Put value into last index of Result
      if Carry > 0 then
         if Product rem 10 + Carry > 9 then
            Result.Reference.all (Result.Reference.all'Last) := Carry + 1;
         else
            Result.Reference.all (Result.Reference.all'Last) := Carry;
         end if;
      else
         Result.Reference.all (Result.Reference.all'Last) := 0;
      end if;

      -- Remove leading zeros
      Remove_Leading_Zeros (Result.Reference);
      return Result;
   end "*";

   ----------------------------------------------------------------------------
   -- Bodies of operations in specification
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function To_Big_Natural (Item : in Natural) return Big_Natural is
   -- Returns a Big_Natural with the same
   -- value as Item

      Result   : Big_Natural;   -- Result of Item to big natural
      New_Item : Natural;       -- Copy of Item
      Index    : Natural;       -- Index of Result
      Count    : Natural;       -- size of Item
   begin

      -- Determine size
      New_Item := Item;
      Count    := 1;
      loop
         exit when New_Item < 10;
         New_Item := New_Item / 10;
         Count := Count + 1;
      end loop;

      -- Allocate memory
      Result.Reference := new Digit_Array (0 .. Count - 1);

      -- Assign Item to digit array
      New_Item := Item;
      Index := 0;
      loop
         Result.Reference.all (Index) := New_Item rem 10;
         exit when New_Item < 10;
         New_Item := New_Item / 10;
         Index := Index + 1;
      end loop;
      return Result;
   end To_Big_Natural;


   ----------------------------------------------------------------------------
   function To_Natural (Item : in Big_Natural) return Natural is
   -- Returns a Natural with the same
   -- value as Item
   -- Exceptions  : Contraint_Error - When Item is Greater than
   --                                 Natural'Last return
   --                                 Natural'Last

      Result     : Natural;  -- Natural being returned
      Multiplyer : Integer;  -- The place of the digit (10, 100, 1000)
   begin
      begin
         Result := 0;
         Multiplyer := 1;
         for Index in Item.Reference'Range loop
            Result := Item.Reference.all (Index) * Multiplyer + Result;
            Multiplyer := Multiplyer * 10;
         end loop;
      exception
         when Constraint_Error =>   -- Item is Greater than Natural'Last
         Result := Natural'Last;
      end;
      return Result;
   end To_Natural;


   ----------------------------------------------------------------------------
   function "+" (Left : in Big_Natural; Right : in Big_Natural) return Big_Natural is
   -- Returns a Big_Natual of Left + Right

      subtype Sum_Range   is Integer range 0 .. 19;
      subtype Carry_Range is Integer range 0 .. 1;

      Sum    : Sum_Range;   -- Result of adding two digits and possible carry
      Carry  : Carry_Range; -- Carry from adding two digits
      Large  : Big_Natural; -- Alias of largest lengthed Big_Natural
      Small  : Big_Natural; -- Alias of Smallest lengthed Big_Natural
      Result : Big_Natural; -- Result of Left + Right

   begin
      -- Check For Smallest and Largest lengths
      if Left.Reference.all'Last = Right.Reference.all'Last then   -- if equal lengths, set left to Large
         Large := Left;
         Small := Right;
      elsif Left.Reference.all'Last > Right.Reference.all'Last then
         Large := Left;
         Small := Right;
      else
         Large := Right;
         Small := Left;
      end if;

      -- Allocate memory for Result
      Result.Reference := new Digit_Array (Large.Reference.all'First .. Large.Reference.all'Last + 1);

      Carry := 0;
      Sum   := 0;
      -- Process Left + Right for the size of Small
      -- Each iteration process one "place"
      for Index in Small.Reference.all'First .. Small.Reference.all'Last loop
         Sum := Large.Reference.all (Index) + Small.Reference.all (Index) + Carry;
         if Sum > 9 then
            Carry := 1;
            Result.Reference.all (Index) := Sum - 10;
         else
            Result.Reference.all (Index) := Sum;
            Carry := 0;
         end if;
      end loop;

      -- Process The Rest of Large
      -- Each iteration process one "place"
      for Index in Small.Reference.all'Last + 1 .. Large.Reference.all'Last loop
         Sum := Large.Reference.all (Index) + Carry;
         if Sum > 9 then
            Carry := 1;
            Result.Reference.all (Index) := 0;  -- has to be zero
         else
            Result.Reference.all (Index) := Sum;
            Carry := 0;
         end if;
      end loop;

      -- Assign value to last of Result
      if Carry = 1 then
         Result.Reference.all (Large.Reference.all'Last + 1) := 1;
      else
         Result.Reference.all (Large.Reference.all'Last + 1) := 0;
      end if;

      -- Remove leading zeros
      Remove_Leading_Zeros (Result.Reference);

      return Result;
   end "+";

   --------------------------
   function "+" (Left : in Big_Natural; Right : in Natural) return Big_Natural is
   begin
      return Left + To_Big_Natural (Right);
   end "+";

   --------------------------
   function "+" (Left : in Natural; Right : in Big_Natural) return Big_Natural is
   begin
      return To_Big_Natural (Left) + Right;
   end "+";


   ----------------------------------------------------------------------------
   function "-" (Left : in Big_Natural; Right : in Big_Natural) return Big_Natural is
   -- Returns a Big_Natual of Left - Right

      subtype Borrow_Range is Integer range 0 .. 1;

      Difference : Digit;         -- Result of subtracting two digits
      Borrow     : Borrow_Range;  -- Borrow from next column
      Result     : Big_Natural;   -- Result of Left - Right

   begin

      -- Allocate memory for Result
      Result.Reference := new Digit_Array (Left.Reference.all'First .. Left.Reference.all'Last);

      -- Process the difference of Left and Right for the range of Right
      -- Each iteration, Process one "place"
      Borrow := 0;
      for Index in Right.Reference.all'First .. Right.Reference.all'Last loop
         if Borrow = 0 then
            if Left.Reference.all (Index) < Right.Reference.all (Index) then
               Borrow     := 1;
               Difference := (Left.Reference.all (Index) + 10)  - Right.Reference.all (Index);
            else
               Borrow     := 0;
               Difference := Left.Reference.all (Index) - Right.Reference.all (Index);
            end if;
         else  -- Borrow = 1
            if Left.Reference.all (Index) - 1 < Right.Reference.all (Index) then
               Borrow     := 1;
               Difference := (Left.Reference.all (Index) + 9)  - Right.Reference.all (Index);
            else
               Borrow     := 0;
               Difference := (Left.Reference.all (Index)  - Right.Reference.all (Index)) - 1;
            end if;
         end if;
         -- Assign Difference to Result
         Result.Reference.all (Index) := Difference;
      end loop;

      -- Process remaining "places"
      -- Each iteration process one "place"
      for Index in Right.Reference.all'Last + 1 .. Left.Reference.all'Last loop
         if Borrow = 1 then
            Borrow := 0;
            Result.Reference.all (Index) := Left.Reference.all (Index) - 1;
         else   -- Borrow = 0
            Result.Reference.all (Index) := Left.Reference.all (Index);
         end if;
      end loop;

      if Borrow = 1 then             -- If Left is smaller than Right
         raise Constraint_Error;
      else
         Remove_Leading_Zeros (Result.Reference);
         return Result;
      end if;
   end "-";

   --------------------------
   function "-" (Left : in Big_Natural; Right : in Natural) return Big_Natural is
   begin
      return Left - To_Big_Natural (Right);
   end "-";

   --------------------------
   function "-" (Left : in Natural; Right : in Big_Natural) return Big_Natural is
   begin
      return To_Big_Natural (Left) - Right;
   end "-";


   ----------------------------------------------------------------------------
   function "*" (Left : in Big_Natural; Right : in Big_Natural) return Big_Natural is
      -- Returns the Product of Left and Right
      Result : Big_Natural;   -- Total of the sums (from multiplication)
      Large  : Big_Natural;   -- Alias of Largest lengthed parameter
      Small  : Big_Natural;   -- Alias of Smallest lengthed parameter
   begin
      -- Check For Smallest and Largest lengths
      if Left.Reference.all'Length < Right.Reference.all'Length then  -- if equal lengths, set left to Large
         Large := Left;
         Small := Right;
      elsif Left.Reference.all'Length > Right.Reference.all'Length then
         Large := Left;
         Small := Right;
      else
         Large := Right;
         Small := Left;
      end if;
      -- Initialize to zero
      Result := To_Big_Natural (0);

      -- Process all digits of Right
      -- Each iteration process one digit of right
      for Index in Small.Reference.all'First .. Small.Reference.all'Last loop
         Result := Result + Shift_Left (Number => Large * Small.Reference.all (Index),
                                        By     => Index);
      end loop;
      return Result;
   end "*";

   ----------------------------------------------------------------------------
   function "*" (Left : in Natural; Right : in Big_Natural) return Big_Natural is
   begin
      return To_Big_Natural (Left) * Right;
   end "*";

   ----------------------------------------------------------------------------
   function "/" (Dividend : in Big_Natural; Divisor : in Big_Natural) return Big_Natural is
      -- The following two pragmas disable warnings for not using the parameters.
      -- If you implement this extra credit function, delete these two lines
      pragma Warnings (Off, Dividend);
      pragma Warnings (Off, Divisor);

      Quotient : Big_Natural;
   begin

--      if Dividend < Divider then
--         Quotient := 0;
--      elsif Dividend = Divider then
--         Quotient := 1;
--      else
--         for Index in reverse Dividend.Reference.all'First .. Dividend.Reference.all'Last loop
--            Quotient := Dividend.Reference.all (Index) / Divisor.Reference.all

      Quotient := To_Big_Natural (0);
      return Quotient;
   end "/";

   --------------------------
   function "/" (Dividend : in Big_Natural; Divisor : in Natural) return Big_Natural is
   begin
      return Dividend / To_Big_Natural (Divisor);
   end "/";

   ----------------------------------------------------------------------------
   function Shift_Left (Number : in Big_Natural;
                        By     : in Natural := 1) return Big_Natural is
   -- Returns a Big_Natural shifted left By times.

      Result : Big_Natural;

   begin
      if By = 0 or else Number.Reference.all'Length = 0 then
         return Number;
      else
         Result.Reference := new Digit_Array (Number.Reference.all'First .. Number.Reference.all'Last + By);
         Result.Reference.all := (0 .. By - 1 => 0) & Number.Reference.all;
      end if;
      return Result;
   end Shift_Left;

   ----------------------------------------------------------------------------
   function Shift_Right (Number : in Big_Natural;
                         By     : in Natural := 1) return Big_Natural is
   -- Returns a Big_Natural shifted right By times.

      Result : Big_Natural;

   begin
      if Number.Reference.all'Length - By <= 0 then
         return To_Big_Natural (0);
      elsif By = 0 then
         return Number;
      else
         Result.Reference := new Digit_Array (0 .. Number.Reference.all'Last - By);
         Result.Reference.all := Number.Reference.all (0 + By .. Number.Reference.all'Last);
      end if;
      return Result;
   end Shift_Right;

   ----------------------------------------------------------------------------
   function "=" (Left : in Big_Natural; Right : in Big_Natural) return Boolean is
   begin
      return Left.Reference.all = Right.Reference.all;
   end "=";

   function "=" (Left : in Big_Natural; Right : in Natural) return Boolean is
   begin
      return Left = To_Big_Natural (Right);
   end "=";

   function "=" (Left : in Natural;  Right : in Big_Natural) return Boolean is
   begin
      return To_Big_Natural (Left) = Right;
   end "=";


   ----------------------------------------------------------------------------
   function "<" (Left : in Big_Natural; Right : in Big_Natural) return Boolean is
   begin
      if Left.Reference.all'Length /= Right.Reference.all'Length then
         return Left.Reference.all'Length < Right.Reference.all'Length;
      else
         for Index in reverse Left.Reference.all'Range loop
            if Left.Reference.all (Index) /= Right.Reference.all (Index) then
               return Left.Reference.all (Index) < Right.Reference.all (Index);
            end if;
         end loop;
      end if;
      return false; -- If we've made it here they equal each other
   end "<";

   function "<" (Left : in Big_Natural; Right : in Natural) return Boolean is
   begin
      return Left < To_Big_Natural (Right);
   end "<";

   function "<" (Left : in Natural;  Right : in Big_Natural) return Boolean is
   begin
      return To_Big_Natural (Left) < Right;
   end "<";


   ----------------------------------------------------------------------------
   function "<=" (Left : in Big_Natural; Right : in Big_Natural) return Boolean is
   begin
      return Left = Right or else Left < Right;
   end "<=";

   function "<=" (Left : in Big_Natural; Right : in Natural) return Boolean is
   begin
      return Left <= To_Big_Natural (Right);
   end "<=";

   function "<=" (Left : in Natural;  Right : in Big_Natural) return Boolean is
   begin
      return To_Big_Natural (Left) <= Right;
   end "<=";


   ----------------------------------------------------------------------------
   function ">" (Left : in Big_Natural; Right : in Big_Natural) return Boolean is
   begin
      return not (Left <= Right);
   end ">";

   function ">" (Left : in Big_Natural; Right : in Natural) return Boolean is
   begin
      return Left > To_Big_Natural (Right);
   end ">";

   function ">" (Left : in Natural;  Right : in Big_Natural) return Boolean is
   begin
      return To_Big_Natural (Left) > Right;
   end ">";

   ----------------------------------------------------------------------------
   function ">=" (Left : in Big_Natural; Right : in Big_Natural) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

   function ">=" (Left : in Big_Natural; Right : in Natural) return Boolean is
   begin
      return Left >= To_Big_Natural (Right);
   end ">=";

   function ">=" (Left : in Natural;  Right : in Big_Natural) return Boolean is
   begin
      return To_Big_Natural (Left) >= Right;
   end ">=";



   ----------------------------------------------------------------------------
   -- Bodies that override the two operations of the parent class (Controlled)
   ----------------------------------------------------------------------------


   ----------------------------------------------------------------------------
   procedure Finalize (Object : in out Big_Natural) is
   -- This procedure is automatically called when an instance of a Big_Natural
   -- is no longer needed.  It provides "garbage collection" for Big_Naturals
   begin
      Recycle (Object.Reference);
   end Finalize;

   ---------------------------------------------------------------------------
   procedure Adjust (Object : in out Big_Natural) is
   -- This procedure is automatically called after an assignment of a value to
   -- Object as in "Object := Other_Object;"  At this point Object and Other_Object
   -- are aliases.  This procedure will make Object a copy (clone) of Other_Object
   -- instead of an alias.
   begin
      Object.Reference := new Digit_Array'(Object.Reference.all);
   end Adjust;

end Big_Natural;