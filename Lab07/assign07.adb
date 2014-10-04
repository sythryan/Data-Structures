with Stack;
with Ada.Strings.Maps;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

procedure Assign07 is

-- Written by Syth Ryan
--
-- This program converts an infix expression to a post fix expression
--
-- Input  :   An infix expression
--
-- Output :   Prompts
--            The converted expression
--            The original expression
--
-- Assumptions  : The only operators are +, -, *, /
--                Operands may contain any graphic characters any graphic
--                   other than the operators, parentheses, and blanks
--                All infix expressions entered are Valid


   package My_Stack is new Stack (Element_Type => Unbounded_String);

   ----------------------------------------------------------------------------
   function P_Value (Item : in Unbounded_String) return Natural is
   -- Purpose  : Determine precedence
   -- Preconditions  : none
   -- Postconditions : The precedence value of item is returned
   begin
      if Item = "(" or Item = ")" then
         return 1;
      elsif Item = "+" or Item = "-"  then
         return 2;
      elsif Item = "*" or Item = "/" then
         return 3;
      else                -- Not an operator
         return 0;
      end if;
   end P_Value;


   ----------------------------------------------------------------------------
   procedure Process_Slice (Slice   : in     Unbounded_String;
                            Stack   : in out My_Stack.Stack_Type;
                            Result  :    out Unbounded_String) is
   -- Purpose : place Slice either in the stack or appened to Result
   -- Preconditions  : Stack must have at least one element and is not a blank
   -- Postconditions : Slice is either placed on the stack or to the end
   --                  of Result

      Popped : Unbounded_String;  -- the element to be check and or appended to Result
   begin
      My_Stack.Pop (Stack => Stack,
                    Popped_Element => Popped);
      if P_Value (Slice) > 1 then                -- if an operator
         if P_Value (Slice) <= P_Value (Popped) then
            Append (Source => Result,
                    New_Item => Popped & ' ');
         else
            My_Stack.Push (Stack => Stack,
                           New_Element => Popped);
         end if;
         My_Stack.Push (Stack => Stack,
                        New_Element => Slice);
      elsif Slice = "(" then
         My_Stack.Push (Stack => Stack,
                        New_Element => Popped);
         My_Stack.Push (Stack => Stack,
                        New_Element => Slice);
      elsif Slice = ")" then
         loop
            exit when Popped = "(";
            Append (Source => Result,
                    New_Item => Popped & ' ');
            My_Stack.Pop (Stack => Stack,
                       Popped_Element => Popped);
         end loop;
      else                                       -- Operand
         Append (Source => Result,
                 New_Item => Slice & ' ');
         My_Stack.Push (Stack => Stack,
                        New_Element => Popped);
      end if;
   end Process_Slice;

   ----------------------------------------------------------------------------
   procedure Convert (Input  : in     Unbounded_String;
                      Result :    out Unbounded_String) is
   -- Purpose : Convert a Infix expression to a postfix expression
   -- Preconditions  : none
   -- Postconditions : Result is a postfix expression with the same
   --                  Value as the infix expression of Input

      Temp   : Unbounded_String; -- Copy of Input, allows us to change it without Changing Input
      Slice  : Unbounded_String; -- operator or operand
      Popped : Unbounded_String; -- Slice to appened to result
      First  : Natural;          -- Index of beginning of element
      Last   : Natural;          -- Index of end of token or operator
      Stack  : My_Stack.Stack_Type (10);  -- Stack of operators

   begin
      My_Stack.Clear (Stack);
      Temp  := Input;

      -- Process the Input String
      -- Each iteration, Process one token
      loop
         First := 1;
         Last  := 1;
         exit when Length (Temp) = 0;
         -- Take off any leading blanks
         Trim (Source => Temp,
               Side   => Ada.Strings.Left);
         -- Slice off First character (non blank)
         Unbounded_Slice (Source => Temp,
                          Target => Slice,
                          Low    => First,
                          High   => First);
         if P_Value (Slice) = 0 then
            -- find operand
            Find_Token (Source => Temp,
                        Set    => Ada.Strings.Maps.To_Set ("()*/-+ "),
                        Test   => Ada.Strings.Outside,
                        First  => First,
                        Last   => Last);
            -- slice operand
            Unbounded_Slice (Source => Temp,
                             Target => Slice,
                             Low    => First,
                             High   => Last);
         end if;


         if My_Stack.Empty (Stack) then
            if P_Value (Slice) > 0 or Slice = "(" then
               My_Stack.Push (Stack => Stack,
                              New_Element => Slice);
            else
               Append (Source => Result,
                       New_Item => Slice & ' ');
            end if;
         else
            Process_Slice (Slice  => Slice,
                           Stack  => Stack,
                           Result => Result);
         end if;

         Delete (Source => Temp,
                 From   => First,
                 Through => Last);
      end loop;

      -- Finish appending the remaining operators
      -- Each iteration, Append one operator
      loop
         exit when My_Stack.Empty (Stack);
         My_Stack.Pop (Stack          => Stack,
                       Popped_Element => Popped);
         Append       (Source   => Result,
                       New_Item => Popped & ' ');
      end loop;
   end Convert;


-------------------------------------------------------------------------------

   Infix   : Unbounded_String;      -- Expression read from the user
   Result  : Unbounded_String;      -- Expression after conversion

begin

   -- Convert Infix to Postfix until a blank or null string is entered
   -- Each iteration convert one expression
   loop
      Result := Null_Unbounded_String;
      -- Read in an infix expression
      Put_Line ("Please enter an infix expression");
      Get_Line (Infix);
      Trim (Source => Infix,
            Side   => Ada.Strings.Both);
      exit when Infix = Null_Unbounded_String;         -- exit when Null
      begin
         -- Convert the expression
         Convert (Input  => Infix,
                  Result => Result);
         -- Display the result
         Put_Line ("The converted expression:");
         Put_Line (Result);
         -- Display original Infix expression
         Put_Line ("The original expression:");
         Put_Line (Infix);
         New_Line;
      exception
         when My_Stack.OVERFLOW =>
            Put_Line ("The expression is too long to convert");
            New_Line;
      end;
   end loop;
end Assign07;