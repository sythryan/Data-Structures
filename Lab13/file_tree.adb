with Ada.Direct_IO;
with Ada.Sequential_IO;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.IO_Exceptions;
package body File_Tree is

   subtype Node_Ptr is Natural;

   Nil : constant Node_Ptr := 0;

   type Node_Type is
      record
         Info  : Element_Type;
         Left  : Node_Ptr := Nil;
         Right : Node_Ptr := Nil;
      end record;

   -- For Input and Output of Tree nodes
   package Node_IO is new Ada.Direct_IO (Element_Type => Node_Type);

   -- For Input and Output of bookkeeping information
   package Header_IO is new Ada.Sequential_IO (Element_Type => Node_Ptr);

   -- Package Variables (used globally)

   Header_File : Header_IO.File_Type;       -- Keep header information between runs
   Tree_File   : Node_IO.File_Type;         -- Storage for the Tree's nodes

   Root    : Node_Ptr := Nil;           -- Pointer to the Tree's root node
   Current : Node_Ptr := Nil;           -- Pointer to the Tree's current node
   Free    : Node_Ptr := Nil;           -- Head of a list of available nodes


   -- Local Procedures

   ----------------------------------------------------------------------------
   procedure Read_Node (From : in  Node_Ptr;
                        Node : out Node_Type) is
   -- Reads a node from the Tree file
   -- Preconditions  : Tree_File is open for input
   -- Postconditions : Node is a copy of the node at position From in the file
   begin
      Node_IO.Read (File => Tree_File,
                    Item => Node,
                    From => Node_IO.Positive_Count (From));
   end Read_Node;

   ----------------------------------------------------------------------------
   procedure Write_Node (To   : in Node_Ptr;
                         Node : in Node_Type) is
   -- Writes a node to the Tree file
   -- Preconditions  : Tree File is open for output
   -- Postconditions : Node is written to Tree_File at position To
   begin
      Node_IO.Write (File => Tree_File,
                     Item => Node,
                     To   => Node_IO.Positive_Count (To));
   end Write_Node;

   -----------------------------------------------------------------------------
   function New_Node return Node_Ptr is
   -- Find a free node in the file (memory allocation)
   -- Preconditions  : Tree_File is open for input
   -- Postconditions : Returns the location of an unused element in Tree_File
      Result : Node_Ptr;
      Node   : Node_Type;
   begin
      if Free = Nil then
         return Node_Ptr (Node_IO.Size (Tree_File)) + 1;
      else
         Result := Free;
         Read_Node (From => Result,
                    Node => Node);
         Free := Node.Left;
         return Result;
      end if;
   end New_Node;

   ----------------------------------------------------------------------------
   procedure Free_Node (X : in out Node_Ptr) is
   -- Deallocate the disk memory used by a node
   -- Preconditions  : Tree File is open for output
   -- Postconditions : The given file location is made available for future use
   --                  X is Nil
      Node : Node_Type;
   begin
      Node.Left := Free;
      Free := X;
      Write_Node (To   => Free,
                  Node => Node);
      X := Nil;   -- Reset pointer
   end Free_Node;

   ----------------------------------------------------------------------------
   procedure Search_Tree (Key      : in  Key_Type;
                          Parent   : out Node_Ptr;
                          Location : out Node_Ptr) is
   -- Searches for the location of Key in the binary search tree
   -- Preconditions  : None
   -- Postconditions : If Key is in the tree
   --                     Location designates the element with Key
   --                     Parent designates the parent of the node designated by
   --                        Location.  If Location designates the root of the
   --                        tree, Parent is Nil.
   --                  else
   --                     Location is Nil.
   --                     if the tree is not empty then
   --                        Parent designates the node in the tree that is
   --                        the logical parent of a node containing Key.
   --                     else
   --                        Parent is Nil
      Node : Node_Type;
   begin
      Location := Root;
      Parent   := Nil;
      loop
         exit when Location = Nil;
         Read_Node (From => Location,
                    Node => Node);
         exit when Key = Key_Of (Node.Info);
         Parent := Location;
         if Key < Key_Of (Node.Info) then
            Location := Node.Left;
         else
            Location := Node.Right;
         end if;
      end loop;
   end Search_Tree;

   ----------------------------------------------------------------------------
   procedure Find_And_Unlink_Max (Root    : in out Node_Ptr;
                                  Max_Ptr :    out Node_Ptr) is
   -- Finds and unlinks the node with the maximum key from a tree
   -- Preconditions  : Root is not Nil
   -- Postconditions : Max_Ptr designates the node containing the largest key
   --                     in the tree whose Root is given
   --                  The node designated by Max_Ptr is unlinked from the
   --                     tree rooted at Root

      Parent      : Node_Ptr;
      Parent_Node : Node_Type;
      Max_Node    : Node_Type;
   begin
      Max_Ptr := Root;   -- Start at the root
      Parent  := Nil;

      -- Read in the root
      Read_Node (From => Max_Ptr,
                 Node => Max_Node);

      -- Check for maximum node
      -- Each iteration check one node
      loop
         exit when Max_Node.Right = Nil;  -- exit when there's no more right children
         Parent  := Max_Ptr;              -- Set the parent of the next node to be checked
         Max_Ptr := Max_Node.Right;       -- Set the next node to be checked

         -- Read in the Right Child of Max_Ptr
         Read_Node (From => Max_Node.Right,
                    Node => Max_Node);
      end loop;

      -- Unlink the max node
      if Parent = Nil then
         Ada.Text_IO.Put_Line ("F1");
         Root := Max_Node.Left;
      else
         Ada.Text_IO.Put_Line ("F2");
         -- Read in the Parent node
         Read_Node (From => Parent,
                    Node => Parent_Node);
         -- Unlink
         Parent_Node.Right := Max_Node.Left;
         -- Write in changed node
         Write_Node (To   => Parent,
                     Node => Parent_Node);
      end if;


   end Find_And_Unlink_Max;


   ----------------------------------------------------------------------------
   procedure Delete_Root (Root : in out Node_Ptr) is
   -- Delete the root node from a tree
   -- Preconditions  : Root is not Nil
   -- Postconditions : The node designated by Root is deleted

      To_Recycle : Node_Ptr;   -- For recycling file elements
      Pred_Ptr   : Node_Ptr;   -- Pointer to the root node's predecessor

      Root_Node  : Node_Type;  -- Local copy of root node
      Pred_Node  : Node_Type;  -- Local copy of the root node's predecessor
   begin
      -- Copy in the root node
      Read_Node (From => Root,
                 Node => Root_Node);

      -- Free the Root
      if Root_Node.Left = Nil and Root_Node.Right = Nil then
         Ada.Text_IO.Put_Line ("DR1");
         -- Root node has no children
         Free_Node (Root);                 -- tree is now empty
      elsif Root_Node.Left = Nil then
         Ada.Text_IO.Put_Line ("DR2");
         -- Root has only a right child
         To_Recycle := Root;                -- Save for later deallocation
         Read_Node (From => Root_Node.Right,
                    Node => Pred_Node);     -- Unlink the root node
         Root_Node.Info := Pred_Node.Info;
         Free_Node (To_Recycle);            -- Deallocate former root node
      elsif Root_Node.Right = Nil then
         Ada.Text_IO.Put_Line ("DR4");
         -- Root has only a Left child
         To_Recycle := Root;               -- Save for later deallocation
         Read_Node (From => Root_Node.Left,
                    Node => Pred_Node);    -- Unlink the root node
         Root_Node.Info := Pred_Node.Info;
         Free_Node (To_Recycle);           -- Deallocate former root node
      else   -- Root node has two children
         -- Find and unlink the logical predecessor
         Ada.Text_IO.Put_Line ("DR5");
         Find_And_Unlink_Max (Root    => Root_Node.Left,
                              Max_Ptr => Pred_Ptr);
         -- Read in predecessor
         Read_Node (From => Pred_Ptr,
                    Node => Pred_Node);
         Root_Node.Info := Pred_Node.Info;  -- Copy Info from predecessor
         Free_Node (Pred_Ptr);              -- Deallocate predecessor

      end if;

      if Root /= Nil then    -- if root is not empty
         -- Write in the new Root
         Write_Node (To   => Root,
                     Node => Root_Node);
      end if;
   end Delete_Root;



   -- Operations defined in the specification

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      Node_IO.Delete (Tree_File);               -- Delete the file
      Node_IO.Create (File => Tree_File,        -- Create a new empty file
                      Name => Tree_File_Name,
                      Mode => Node_IO.Inout_File);
      Root    := Nil;
      Current := Nil;
      Free    := Nil;
   end Clear;

   ------------
   -- Insert --
   ------------

   procedure Insert (Item : in Element_Type) is
      Parent : Node_Ptr;   -- Pointers to the parent
      Child  : Node_Ptr;   -- and child nodes

      Child_Node  : Node_Type;  -- Local copies of the file nodes for
      Parent_Node : Node_Type;  -- the child and parent
   begin
      -- Search for the insertion place
      Search_Tree (Key      => Key_Of (Item),
                   Parent   => Parent,
                   Location => Child);

      if Child /= Nil then
         raise DUPLICATE_KEY;
      else

         -- Set up the node for the new child
         Child := New_Node;
         Child_Node := (Item, Nil, Nil);
         -- Attach the new child to the tree (change either Root or parent pointer)

         if Parent = Nil then
            Root := Child;
            Write_Node (To => Child,
                        Node => Child_Node);
         else
            Read_Node (From => Parent,
                       Node => Parent_Node);
            if Key_Of (Child_Node.Info) < Key_Of (Parent_Node.Info) then
               Parent_Node.Left := Child;
            else
               Parent_Node.Right := Child;
            end if;

            -- Put the new element into the file
            Write_Node (To => Parent,
                        Node => Parent_Node);
            Write_Node (To => Child,
                        Node => Child_Node);
         end if;
      end if;
      Current := Child;  -- Set the current element to designate the new node

   end Insert;

   ------------
   -- Delete --
   ------------

   procedure Delete (Key : in  Key_Type) is
      Parent   : Node_Ptr;   -- Pointers to the parent
      Location : Node_Ptr;   -- and the node to be deleted

      Parent_Node : Node_Type;  -- Local copy of the Parent node
   begin
      Current := Nil;  -- The current element is no longer defined

      -- Search for the node we want to delete
      Search_Tree (Key      => Key,
                   Parent   => Parent,
                   Location => Location);

      Ada.Text_IO.Put_Line ("Delete");

      if Location = Nil then
         raise KEY_ERROR;
      end if;

      if Parent = Nil then
         Ada.Text_IO.Put_Line ("D1");
         Delete_Root (Location);
      else
         Read_Node (From => Parent,
                    Node => Parent_Node);
         if Key < Key_Of (Parent_Node.Info) then
            Ada.Text_IO.Put_Line ("D2");
            Delete_Root (Parent_Node.Left);
         else
            Ada.Text_IO.Put_Line ("D3");
            Delete_Root (Parent_Node.Right);
         end if;
         Write_Node (To   => Parent,
                     Node => Parent_Node);
      end if;



   end Delete;

   ------------
   -- Modify --
   ------------

   procedure Modify (Element : in Element_Type) is
      Current_Node : Node_Type;
   begin
      if Current = Nil then
         raise CURRENT_UNDEFINED;
      end if;

      Read_Node (From => Current, Node => Current_Node);
      if Key_Of (Element) /= Key_Of (Current_Node.Info) then
         raise KEY_ERROR;
      end if;
      Current_Node.Info := Element;
      Write_Node (To => Current, Node => Current_Node);

   end Modify;

   ----------
   -- Find --
   ----------

   procedure Find (Key : in  Key_Type) is
      Parent : Node_Ptr;
   begin
      Search_Tree (Key      => Key,
                   Parent   => Parent,
                   Location => Current);
   end Find;

   -----------
   -- Close --
   -----------

   procedure Close is
   begin
      Header_IO.Create (File => Header_File,
                        Name => Header_File_Name);
      Header_IO.Write (File => Header_File, Item => Root);
      Header_IO.Write (File => Header_File, Item => Current);
      Header_IO.Write (File => Header_File, Item => Free);
      Header_IO.Close (Header_File);
      Node_IO.Close (Tree_File);
   end Close;

   ---------------------
   -- Current_Defined --
   ---------------------

   function Current_Defined return Boolean is
   begin
      return Current /= Nil;
   end Current_Defined;

   -----------
   -- Empty --
   -----------

   function Empty return Boolean is
   begin
      return Root = Nil;
   end Empty;

   --------------
   -- Retrieve --
   --------------

   procedure Retrieve (Element : out Element_Type) is
      Current_Node : Node_Type;
   begin
      if Current = Nil then
         raise CURRENT_UNDEFINED;
      end if;

      Read_Node (From => Current, Node => Current_Node);
      Element := Current_Node.Info;
   end Retrieve;

   ------------
   -- Height --
   ------------

   function Height return Natural is
      function Recursive_Height (Root : in Node_Ptr) return Natural is
         Node : Node_Type;
      begin
         if Root = Nil then
            return 0;
         else
            Read_Node (From => Root,
                       Node => Node);
            return 1 + Natural'Max (Recursive_Height (Node.Left),
                                    Recursive_Height (Node.Right));
         end if;
      end Recursive_Height;
   begin
      return Recursive_Height (Root);
   end Height;

   ----------------------
   -- InOrder_Traverse --
   ----------------------

   procedure InOrder_Traverse
     (Process : not null access procedure (Element : in out Element_Type)) is

      procedure Inorder (Root : in Node_Ptr) is
         Node : Node_Type;
      begin
         if Root /= Nil then
            Read_Node (From => Root,
                       Node => Node);
            Inorder (Node.Left);
            Process (Node.Info);
            Write_Node (To   => Root,
                        Node => Node);
            Inorder (Node.Right);
         end if;
      end Inorder;
   begin
      Inorder (Root);
   end InOrder_Traverse;


begin  -- Initialize the tree
   Node_IO.Open (File => Tree_File,
                 Mode => Node_IO.Inout_File,
                 Name => Tree_File_Name);
   Header_IO.Open (File => Header_File,
                   Mode => Header_IO.In_File,
                   Name => Header_File_Name);
   Header_IO.Read (File => Header_File, Item => Root);
   Header_IO.Read (File => Header_File, Item => Current);
   Header_IO.Read (File => Header_File, Item => Free);
   Header_IO.Close (Header_File);

exception
   when Ada.IO_Exceptions.Name_Error =>   -- One of the two open statements failed
      if not Node_IO.Is_Open (Tree_File) then
         --The tree data file does not exist, create an empty one
         Node_IO.Create (File => Tree_File,
                         Mode => Node_IO.Inout_File,
                         Name => Tree_File_Name);
         Root    := Nil;
         Current := Nil;
         Free    := Nil;
      else
         -- The tree data file exists but the header data file does not
         -- A big problem.  Don't destroy the data file.  End the program.
         Node_IO.Close (Tree_File);
         raise; -- propogate the exception to halt the program
      end if;
end File_Tree;