with Ada.Text_IO;
package Vector_ADT.Vector_IO is

-- This is a child package of Vector_ADT that provides input and output
-- operations for vectors.


   ----------------------------------------------------------------------------
   procedure Get (File : in  Ada.Text_IO.File_Type;
                  Item : out Vector);
   -- Get 3 components from File and return them in Item
   --
   -- Preconditions  : File is open for input
   --                  There are three values remaining in the file
   --
   -- Postconditions : Vector is made up of the 3 components obtained
   --                  from the file
   --
   -- Exceptions     : Standard I/O exceptions for floating point numbers


   ----------------------------------------------------------------------------
   procedure Get (Item : out Vector);
   -- Get 3 components from Standard Input and return them in Item
   --
   -- Preconditions  : None
   --
   -- Postconditions : Vector is made up of the 3 components obtained
   --                  from the Standard Input file
   --
   -- Exceptions     : Standard I/O exceptions for floating point numbers

   ----------------------------------------------------------------------------
   procedure Put (File : in Ada.Text_IO.File_Type;
                  Item : in Vector;
                  Fore : in Ada.Text_IO.Field;
                  Aft  : in Ada.Text_IO.Field;
                  Exp  : in Ada.Text_IO.Field;
                  Raw  : in Boolean);
   -- Puts a Vector to a File
   --
   -- Preconditions  : File is open for output
   --
   -- Postconditions : The three vector components are put to the File using the
   --                     formatting information specified in Fore, Aft, and Exp.
   --                  If Raw then
   --                     Only the 3 components are put
   --                  Else
   --                     The components are separated by a comma and one blank.
   --                     The three components are enclosed in parentheses.
   --                     Example output   (13.25,  -8.66,  4.03)


   ----------------------------------------------------------------------------
   procedure Put (Item : in Vector;
                  Fore : in Ada.Text_IO.Field;
                  Aft  : in Ada.Text_IO.Field;
                  Exp  : in Ada.Text_IO.Field;
                  Raw  : in Boolean);
   -- Puts a Vector to the standard output file
   --
   -- Preconditions  : None
   --
   -- Postconditions : The three vector components are put using the
   --                     formatting information specified in Fore, Aft, and Exp.
   --                  If Raw then
   --                     Only the 3 components are put
   --                  Else
   --                     The components are separated by a comma and one blank.
   --                     The three components are enclosed in parentheses.
   --                     Example output   (13.25,  -8.66,  4.03)

end Vector_ADT.Vector_IO;