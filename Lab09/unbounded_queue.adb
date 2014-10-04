with Ada.Unchecked_Deallocation;
package body Unbounded_Queue is

   -- Instantiate a procedure to recycle node memory
   procedure Free is new Ada.Unchecked_Deallocation (Object => Node_Type,
                                                     Name   => Node_Ptr);
   --------------------------------------------------------------------------
   function Size (Queue : in Queue_Type) return Natural is
      Result  : Natural;
      Temp    : Node_Ptr;
   begin
      Result := 0;
      Temp := Queue.Front;
      loop
         exit when Temp = null;
         Result := Result + 1;
         Temp := Temp.all.Next;
      end loop;
      return Result;
   end Size;

   --------------------------------------------------------------------------
   function Value_At (Queue    : in Queue_Type;
                      Position : in Positive) return Element_Type is
      Count : Natural;
      Temp  : Node_Ptr;
   begin
      if Queue.Empty then
         raise UNDERFLOW;
      end if;

      Temp  := Queue.Front;
      Count := 1;
      loop
         exit when Count = Position;
         Temp := Temp.all.Next;
         if Temp = null then
            raise QUEUE_CONSTRAINT_ERROR;
         end if;
         Count := Count + 1;
      end loop;

      return Temp.all.Info;
   end Value_At;


   ----------------------------------------------------------------------------
   procedure Clear (Queue : in out Queue_Type) is
      To_Recycle : Node_Ptr;               -- For recycling nodes
   begin
      loop
         exit when Queue.Front = null;
         To_Recycle  := Queue.Front;           -- Unlink the
         Queue.Front := Queue.Front.all.Next;  --    front node
         Free (To_Recycle);                    -- Recycle the node
      end loop;
      Queue.Rear := null;                      -- Clean up Rear pointer
   end Clear;

   ----------------------------------------------------------------------------
   procedure Enqueue (Queue : in out Queue_Type;
                      Item  : in     Element_Type) is
   begin
      if Full (Queue) then
         raise OVERFLOW;
      end if;
      if Queue.Front = null then    -- Is the queue empty?
         -- Yes, Front and Back should both designate the new node
         Queue.Front := new Node_Type'(Info => Item,  Next => null);
         Queue.Rear  := Queue.Front;
      else
         -- No, link a new node to the rear of the existing queue.
         Queue.Rear.all.Next := new Node_Type'(Info => Item,  Next => null);
         Queue.Rear := Queue.Rear.all.Next;
      end if;
   exception
      when STORAGE_ERROR =>
         raise OVERFLOW;
   end Enqueue;

   ----------------------------------------------------------------------------
   procedure Dequeue (Queue : in out Queue_Type;
                      Item  :    out Element_Type) is
      To_Recycle : Node_Ptr;           -- For recycling nodes
   begin
      if Queue.Front = null then
         raise UNDERFLOW;
      end if;
      Item := Queue.Front.all.Info;        -- Get the value from the front node
      To_Recycle  := Queue.Front;          -- Save access to old front
      Queue.Front := Queue.Front.all.Next; -- Change the front
      Free (To_Recycle);                   -- Recycle the memory
      if Queue.Front = null then           -- Is the queue now empty?
         Queue.Rear := null;               -- Set Rear to null as well
      end if;
   end Dequeue;

   --------------------------------------------------------------------------
   function Full (Queue : in Queue_Type) return Boolean is
   begin
      return Size (Queue) = Queue.Max_Size;
   end Full;

   --------------------------------------------------------------------------
   function Empty (Queue : in Queue_Type) return Boolean is
   begin
      return Queue.Front = null;
   end Empty;

end Unbounded_Queue;
