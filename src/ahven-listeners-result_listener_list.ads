--
-- Copyright (c) 2006, 2007, 2008 Tero Koskinen <tero.koskinen@iki.fi>
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

with Ada.Finalization;

package Ahven.Listeners.Result_Listener_List is
   type Iterator is private;

   List_Empty : exception;
   Out_Of_Range : exception;
   Invalid_Iterator : exception;

   type List is new Ada.Finalization.Controlled with private;
   -- List is a Controlled type. You can safely copy it.
   -- Although, notice that if you have a list of pointers (access types),
   -- only the pointers are copied, not the objects they point at.

   Empty_List : constant List;
   -- A list with zero elements.
   -- For example, can be used for initialization.

   procedure Append (This_List : in out List;
                     Node_Data : Listeners.Result_Listener_Class_Access);
   -- Append an element at the end of the list.

   procedure Remove_All (This_List : in out List);
   -- Remove all elements from the list.

   function Empty (This_List : List) return Boolean;
   -- Is the list empty?

   function First (This_List : List) return Iterator;
   -- Return an iterator to the first element of the list.

   function Next (Iter : Iterator) return Iterator;
   -- Move the iterator to point to the next element on the list.

   function Data (Iter : Iterator)
     return Listeners.Result_Listener_Class_Access;
   -- Return element pointed by the iterator.

   function Size (This_List : List) return Natural;
   -- Return the size of the list.

   function Is_Valid (Iter : Iterator) return Boolean;
   -- Is Iterator still valid or out of range?
private
   type Node;
   type Node_Access is access Node;
   type Iterator is new Node_Access;

   procedure Remove (Ptr : Node_Access);
   -- A procedure to release memory pointed by Ptr.

   function Data (Iter : Node_Access)
     return Listeners.Result_Listener_Class_Access;

   type Node is record
      Data : Listeners.Result_Listener_Class_Access;
      Next : Node_Access := null;
      Prev : Node_Access := null;
   end record;

   type List is new Ada.Finalization.Controlled with record
      First : Node_Access := null;
      Last  : Node_Access := null;
      Size  : Natural := 0;
   end record;

   procedure Initialize (Object : in out List);
   procedure Finalize   (Object : in out List);
   procedure Adjust     (Object : in out List);

   Empty_List : constant List :=
     (Ada.Finalization.Controlled with First => null,
                                       Last  => null,
                                       Size  => 0);

end Ahven.Listeners.Result_Listener_List;
