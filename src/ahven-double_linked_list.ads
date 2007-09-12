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

with Ada.Finalization;

generic
   type Data_Type is private;
package Ahven.Double_Linked_List is

   type Node is private;
   type Node_Access is access Node;
   type Iterator is new Node_Access;

   List_Empty : exception;
   Out_Of_Range : exception;
   Invalid_Iterator : exception;

   type List is new Ada.Finalization.Controlled with private;

   Empty_List : constant List;

   procedure Append  (This_List : in out List; Node_Data : Data_Type);
   procedure Prepend (This_List : in out List; Node_Data : Data_Type);
   procedure Remove  (This_List : in out List; Iter : Iterator);
   procedure Remove_All   (This_List : in out List);
   procedure Remove_First (This_List : in out List);
   procedure Remove_Last  (This_List : in out List);

   function Empty (This_List : in List) return Boolean;
   function First (This_List : in List ) return Iterator;
   function Last  (This_List : in List) return Iterator;
   function Next  (Iter      : in Iterator) return Iterator;
   function Prev  (Iter      : in Iterator) return Iterator;

   function Data (Iter : in Iterator) return Data_Type;

   function Size (This_List : in List) return Natural;

   procedure Join (Target : in out List; Addition : in out List);

private
   procedure Remove (Ptr : Node_Access);

   type Node is record
      Data : Data_Type;
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

end Ahven.Double_Linked_List;

