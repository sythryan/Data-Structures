with Ada.Float_Text_IO;
package body Vector_ADT.Vector_IO is

   ---------
   -- Get --
   ---------

   procedure Get
     (File : in  Ada.Text_IO.File_Type;
      Item : out Vector) is
   begin
      for Index in Item'Range loop
         Ada.Float_Text_IO.Get (File => File,
                                Item => Item (Index));
      end loop;

   end Get;

   ---------
   -- Get --
   ---------

   procedure Get (Item : out Vector) is
   begin
      Get (File => Ada.Text_IO.Standard_Input,
           Item => Item);
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (File : in Ada.Text_IO.File_Type;
      Item : in Vector;
      Fore : in Ada.Text_IO.Field;
      Aft  : in Ada.Text_IO.Field;
      Exp  : in Ada.Text_IO.Field;
      Raw  : in Boolean)
   is
   begin
      for Index in Item'Range loop
         Ada.Float_Text_IO.Put (File => File,
                                Item => Item (Index),
                                Fore => Fore,
                                Aft  => Aft,
                                Exp  => Exp);
      end loop;
      if Raw then
         for Index in Item'Range loop
            Ada.Float_Text_IO.Put (Item => Item (Index),
                                   Fore => Fore,
                                   Aft  => Aft,
                                   Exp  => Exp);
         end loop;
      else
         Ada.Text_IO.Put ("(");
         for Index in Item'Range loop
            Ada.Float_Text_IO.Put (Item => Item (Index),
                                   Fore => Fore,
                                   Aft  => Aft,
                                   Exp  => Exp);
            Ada.Text_IO.Put (", ");
         end loop;
         Ada.Text_IO.Put (")");
      end if;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (Item : in Vector;
      Fore : in Ada.Text_IO.Field;
      Aft  : in Ada.Text_IO.Field;
      Exp  : in Ada.Text_IO.Field;
      Raw  : in Boolean)
   is
   begin
      Put (File => Ada.Text_IO.Standard_Output,
           Item => Item,
           Fore => Fore,
           Aft  => Aft,
           Exp  => Exp,
           Raw  => Raw);
   end Put;

end Vector_ADT.Vector_IO;
