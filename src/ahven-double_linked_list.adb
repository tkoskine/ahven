--
-- Copyright (c) 2006, 2007 Tero Koskinen <tero.koskinen@iki.fi>
--
-- Permission to use, copy, modify, and distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--

with Ada.Unchecked_Deallocation;

package body Ahven.Double_Linked_List is

   procedure Remove (Ptr : in Node_Access) is
      procedure Free is new Ada.Unchecked_Deallocation (Node, Node_Access);
      My_Ptr : Node_Access := Ptr;
   begin
      Ptr.Next := null;
      Ptr.Prev := null;
      Free (My_Ptr);
   end Remove;

   procedure Remove (This_List : in out List; Iter : Iterator) is
      Temp_Node : Node_Access;
   begin
      if This_List.Size = 0 then
         raise List_Empty;
      elsif Iter = null then
         raise Invalid_Iterator;
      end if;
      if Node_Access (Iter) = This_List.First then
         Remove_First (This_List);
      elsif Node_Access (Iter) = This_List.Last then
         Remove_Last (This_List);
      else
         Temp_Node := Node_Access (Iter);
         Iter.Prev.Next := Iter.Next;
         Iter.Next.Prev := Iter.Prev;
         Remove (Temp_Node);
      end if;
   end Remove;

   procedure Prepend (This_List : in out List; Node_Data : Data_Type) is
      New_Node : Node_Access  := null;
   begin
      New_Node := new Node'(Data => Node_Data,
         Next => This_List.First, Prev => null);

      if This_List.First = null then
         This_List.Last := New_Node;
      else
         This_List.First.Prev := New_Node;
      end if;

      This_List.First := New_Node;
      This_List.Size := This_List.Size + 1;
   end Prepend;

   procedure Append (This_List : in out List; Node_Data : Data_Type) is
      New_Node : Node_Access  := null;
   begin
      New_Node := new Node'(Data => Node_Data,
         Next => null, Prev => This_List.Last);

      if This_List.Last = null then
         This_List.Last := New_Node;
         This_List.First := New_Node;
      else
         This_List.Last.Next := New_Node;
         This_List.Last := New_Node;
      end if;

      This_List.Size := This_List.Size + 1;
   end Append;

   procedure Remove_All (This_List : in out List) is
      Current_Node : Node_Access := This_List.First;
      Next_Node : Node_Access := null;
   begin
      while Current_Node /= null loop
         Next_Node := Current_Node.Next;
         Remove (Current_Node);
         Current_Node := Next_Node;
      end loop;

      This_List.First := null;
      This_List.Last := null;
      This_List.Size := 0;
   end Remove_All;

   procedure Remove_First (This_List : in out List) is
      Temp_Node : Node_Access;
   begin
      if This_List.Size = 0 then
         raise List_Empty;
      end if;

      Temp_Node := This_List.First;
      This_List.First := This_List.First.Next;
      This_List.First.Prev := null;

      Remove (Temp_Node);
      This_List.Size := This_List.Size - 1;
   end Remove_First;

   procedure Remove_Last (This_List : in out List) is
      Temp_Node : Node_Access;
   begin
      if This_List.Size = 0 then
         raise List_Empty;
      end if;

      Temp_Node := This_List.Last;
      This_List.Last := This_List.Last.Prev;
      This_List.Last.Next := null;

      Remove (Temp_Node);
      This_List.Size := This_List.Size - 1;
   end Remove_Last;

   function Empty (This_List : in List) return Boolean is
   begin
      if This_List.Size = 0 then
         return True;
      end if;
      return False;
   end Empty;

   function First (This_List : in List) return Iterator is
   begin
      if This_List.Size = 0 then
         return null;
      end if;

      return Iterator (This_List.First);
   end First;

   function Last (This_List : in List ) return Iterator is
   begin
      if This_List.Size = 0 then
         return null;
      end if;

      return Iterator (This_List.Last);
   end Last;

   function Next (Iter : in Iterator) return Iterator is
   begin
      if Iter = null then
         raise Invalid_Iterator;
      end if;
      return Iterator (Iter.Next);
   end Next;

   function Prev (Iter : in Iterator) return Iterator is
   begin
      if Iter = null then
         raise Invalid_Iterator;
      end if;
      return Iterator (Iter.Prev);
   end Prev;

   function Data (Iter : in Iterator) return Data_Type is
   begin
      return Iter.Data;
   end Data;

   function Data (Iter : in Node_access) return Data_Type is
   begin
      return Iter.Data;
   end Data;

   function Size (This_List : in List) return Natural is
   begin
      return This_List.Size;
   end Size;

   procedure Join (Target : in out List; Addition : in out List) is
   begin
      if Target.Size = 0 then
         Target.First := Addition.First;
      else
         Target.Last.Next := Addition.First;
         Addition.First.Prev := Target.Last;
      end if;

      Target.Last := Addition.Last;
      Target.Size := Target.Size + Addition.Size;

      -- No need to release Addition's memory
      -- because all nodes are transferred to Target
      Addition.Last := null;
      Addition.First := null;
      Addition.Size := 0;
   end Join;

   procedure Initialize (Object : in out List) is
   begin
      Object.Last := null;
      Object.First := null;
      Object.Size := 0;
   end Initialize;

   procedure Finalize (Object : in out List) is
   begin
      Remove_All (Object);
   end Finalize;

   procedure Adjust (Object : in out List) is
      Target_Last : Node_Access := null;
      Target_First : Node_Access := null;
      Current : Node_Access := Object.First;
      New_Node : Node_Access;
   begin
      while Current /= null loop
         New_Node := new Node'(Data => Current.Data,
           Next => null, Prev => Target_Last);

         if Target_Last = null then
            Target_Last := New_Node;
            Target_First := New_Node;
         else
            Target_Last.Next := New_Node;
            Target_Last := New_Node;
         end if;

         Current := Current.Next;
      end loop;
      Object.First := Target_First;
      Object.Last := Target_Last;
   end Adjust;

end Ahven.Double_Linked_List;
