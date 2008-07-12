--
-- Copyright (c) 2007 Tero Koskinen <tero.koskinen@iki.fi>
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

with Ahven.Doubly_Linked_List;

use Ahven;

package body List_Tests is
   package Integer_Linked_List is
     new Ahven.Doubly_Linked_List (Data_Type => Integer);

   procedure Test_Append is
      use Integer_Linked_List;

      My_List : List := Empty_List;
   begin
      Append ( My_List, 1);
      Append ( My_List, 2);
      Assert (2 = Data (Last (My_List)),
         "Append does not work properly (last /= 2)");
      Assert (1 = Data (First (My_List)),
         "Append does not work properly (first /= 1)");

      Remove_All (My_List);
   end Test_Append;

   procedure Test_Remove is
      use Integer_Linked_List;

      My_List : List;
      Iter : Iterator;
   begin
      Append (My_List, 1);
      Append (My_List, 2);
      Append (My_List, 3);
      Append (My_List, 4);
      Append (My_List, 5);
      Iter := Next (First (My_List));
      Remove (My_List, Iter);
      Iter := Next (First (My_List));

      Assert (3 = Data (Iter),
         "Remove does not work properly, data item 3 in a wrong place");
      Assert (5 = Data (Last (My_List)),
         "Remove does not work properly, data item 5 in a wrong place");
      Assert (4 = Size (My_List),
         "Remove does not work properly, Size /= 4");
      Remove_All (My_List);
   end Test_Remove;

   procedure Test_Move is
      use Integer_Linked_List;

      My_List : List;
      Extra_List : List;
      Iter : Iterator;
   begin
      Append (My_List, 1);
      Append (My_List, 2);
      Append (Extra_List, 3);
      Append (Extra_List, 4);
      Append (Extra_List, 5);
      Move (My_List, Extra_List);
      Assert (Size (My_List) = 5, "Size of My_List does not match");
      Assert (Size (Extra_List) = 0, "Size of Extra_List does not match");

      -- Testing forward iteration
      Iter := First (My_List);
      for A in Integer range 1 .. 5 loop
         Assert (Is_Valid (Iter), "Iterator went to null");
         Assert (Data (Iter) = A, "Data does not match");
         Iter := Next (Iter);
      end loop;

      -- And backward iteration
      Iter := Last (My_List);
      for A in reverse Integer range 1 .. 5 loop
         Assert (Is_Valid (Iter), "Iterator went to null");
         Assert (Data (Iter) = A, "Data does not match");
         Iter := Prev (Iter);
      end loop;

   end Test_Move;

   procedure Test_Assignment is
      use Integer_Linked_List;

      My_List : List;
      Second_List : List;
      Third_List : List;
   begin
      Append (My_List, 1);
      Append (My_List, 2);

      Second_List := My_List;
      Assert (Size (Second_List) = 2, "Size of Second_List does not match");

      Third_List := My_List;
      Assert (Size (Third_List) = 2, "Size of Third_List does not match");

      Remove (My_List, Last (My_List));
      Assert (Size (My_List)     = 1, "Size of My_List does not match");
      Assert (Size (Second_List) = 2, "Size of Second_List does not match");
      Assert (Size (Third_List)  = 2, "Size of Third_List does not match");

      declare
         Local_List : List := My_List;
      begin
         Assert (Size (Local_List) = 1, "Size of Local_List does not match");
         Append (Local_List, 3);
         Assert (Size (Local_List) = 2, "Size of Local_List does not match");
      end;

      Assert (Size (My_List)     = 1, "Size of My_List does not match");
      Assert (Size (Second_List) = 2, "Size of Second_List does not match");
      Assert (Size (Third_List)  = 2, "Size of Third_List does not match");
   end Test_Assignment;

   procedure Test_Forward_Iterator is
      use Integer_Linked_List;

      My_List : List;
      Iter : Iterator;
      Count : Natural := 0;
   begin
      Append (My_List, 1);
      Iter := First (My_List);
      Assert (Is_Valid (Iter), "First returned null!");
      Iter := Next (Iter);
      Assert (not Is_Valid (Iter), "Next after First returned non-null!");

      Append (My_List, 2);
      Append (My_List, 3);

      Iter := First (My_List);
      loop
         exit when not Is_Valid (Iter);
         Iter := Next (Iter);
         Count := Count + 1;
      end loop;
      Assert (Count = 3, "Iteration loop did not loop all items!");

   end Test_Forward_Iterator;

   procedure Test_Reverse_Iterator is
      use Integer_Linked_List;

      My_List : List;
      Iter : Iterator;
      Count : Natural := 0;
   begin
      Append (My_List, 1);
      Iter := Last (My_List);
      Assert (Is_Valid (Iter), "Last returned null!");
      Assert (Data (Iter) = 1, "Iterator points to invalid item!");

      Iter := Next (Iter);
      Assert (not Is_Valid (Iter), "Next after First returned non-null!");

      Append (My_List, 2);
      Append (My_List, 3);

      Iter := Last (My_List);
      loop
         exit when not Is_Valid (Iter);
         Count := Count + 1;
         Assert (Data (Iter) = (4 - Count), "Iterator points to wrong item!");
         Iter := Prev (Iter);
      end loop;
      Assert (Count = 3, "Iteration loop did not loop all items!");
   end Test_Reverse_Iterator;

   procedure Test_Move_Empty_List is
      use Integer_Linked_List;

      My_List : List;
      Extra_List : List;
   begin
      Append (My_List, 3);
      Move (Target => My_List, Source => Extra_List);
   end Test_Move_Empty_List;

   procedure Initialize (T: in out Test_Case) is
      use Ahven.Framework;
   begin
      Set_Name (T, "Ahven.Doubly_Linked_List");
      Add_Test_Routine (T, Test_Append'Access, "Append");
      Add_Test_Routine (T, Test_Remove'Access, "Remove");
      Add_Test_Routine (T, Test_Move'Access, "Move");
      Add_Test_Routine (T, Test_Move_Empty_List'Access,
                        "Move (Empty Source)");
      Add_Test_Routine (T, Test_Assignment'Access, "Assignment");
      Add_Test_Routine (T, Test_Reverse_Iterator'Access,
                        "Reverse Iterator");
      Add_Test_Routine (T, Test_Forward_Iterator'Access,
                        "Forward Iterator");
   end Initialize;
end List_Tests;
