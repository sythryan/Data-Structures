package body Vector_ADT is

-- Written by Syth Ryan

   ---------
   -- "+" --
   ---------

   function "+" (Left : in Vector; Right : in Vector) return Vector is
      Result : Vector;
   begin
      Result (1) := Left (1) + Right (1);
      Result (2) := Left (2) + Right (2);
      Result (3) := Left (3) + Right (3);
      return Result;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left : in Vector; Right : in Vector) return Vector is
      Result : Vector;
   begin
      Result (1) := Left (1) - Right (1);
      Result (2) := Left (2) - Right (2);
      Result (3) := Left (3) - Right (3);
      return Result;
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (Left : in Vector; Right : in Vector) return Vector is
      Result : Vector;
   begin
      Result (1) := Left (2) * Right (3) - Left (3) * Right (2);
      Result (2) := Left (3) * Right (1) - Left (1) * Right (3);
      Result (3) := Left (1) * Right (2) - Left (2) * Right (1);
      return Result;
   end "*";

end Vector_ADT;
