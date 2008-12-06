--
-- Copyright (c) 2008 Tero Koskinen <tero.koskinen@iki.fi>
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

generic
   type Element_Type is private;
package Ahven.SList is
      type List is new Ada.Finalization.Controlled with private;
      type Iterator is private;

      subtype Count_Type is Natural;

      Invalid_Iterator : exception;

      Empty_List : constant List;

      procedure Append (Target : in out List; Node_Data : Element_Type);
      -- Append an element at the end of the list.

      procedure Remove_All (Target : in out List);
      -- Remove all elements from the list.

      function First (Target : List) return Iterator;
      -- Return an iterator to the first element of the list.

      function Next (Iter : Iterator) return Iterator;
      -- Move the iterator to point to the next element on the list.

      function Data (Iter : Iterator) return Element_Type;
      -- Return element pointed by the iterator.

      function Is_Valid (Iter : Iterator) return Boolean;

      function Length (Target : List) return Count_Type;

   private
      type Node;
      type Node_Access is access Node;
      type Iterator is new Node_Access;

      procedure Remove (Ptr : Node_Access);
      -- A procedure to release memory pointed by Ptr.

      type Node is record
         Next : Node_Access := null;
         Data : Element_Type;
      end record;

      type List is new Ada.Finalization.Controlled with record
         First : Node_Access := null;
         Last  : Node_Access := null;
         Size  : Count_Type  := 0;
      end record;

      procedure Initialize (Target : in out List);
      procedure Finalize   (Target : in out List);
      procedure Adjust     (Target : in out List);

      Empty_List : constant List :=
        (Ada.Finalization.Controlled with First => null,
                                          Last  => null,
                                          Size  => 0);
end Ahven.SList;
