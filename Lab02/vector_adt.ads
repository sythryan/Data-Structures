package Vector_ADT is

-- This data abstraction package implements three basic operations
-- for vectors in 3-dimensional space.

   -- Types that define the domain of a vector

   subtype Component_Type is Float;   -- Vector components

   subtype Index_Type is Integer range 1 .. 3;
   type    Vector     is array (Index_Type) of Component_Type;


   -- Operations for vectors
   -- Any operation will raise Constraint_Error if a resulting
   -- component is out of the range of Component_Type

   function "+" (Left : in Vector; Right : in Vector) return Vector;
   -- Returns the sum of two vectors

   function "-" (Left : in Vector; Right : in Vector) return Vector;
   -- Returns the difference of two vectors

   function "*" (Left : in Vector; Right : in Vector) return Vector;
   -- Returns the cross product of two vectors

end Vector_ADT;