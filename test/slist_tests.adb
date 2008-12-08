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

with Ahven;
with Ahven.SList;

use Ahven;

package body SList_Tests is
   type Simple_Type is record
      Value : Integer := 0;
   end record;

   package Integer_List is new Ahven.SList (Element_Type => Integer);

   package Simple_List is new Ahven.SList (Element_Type => Simple_Type);

   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Ahven.SList");

      Framework.Add_Test_Routine (T, Test_Append_Elementary'Access,
                                  "Test_Append (Elementary)");
      Framework.Add_Test_Routine (T, Test_Append_Record'Access,
                                  "Test_Append (Record)");
      Framework.Add_Test_Routine (T, Test_Remove_All'Access,
                                  "Test_Remove_All");
      Framework.Add_Test_Routine (T, Test_First'Access,
                                  "Test_First");
      Framework.Add_Test_Routine (T, Test_Next'Access,
                                  "Test_Data");
      Framework.Add_Test_Routine (T, Test_Length'Access,
                                  "Test_Length");
      Framework.Add_Test_Routine (T, Test_Copy'Access,
                                  "Test_Copy");
   end Initialize;

   procedure Test_Append_Elementary is
      use Integer_List;

      My_List : List;
      Iter    : Iterator;
   begin
      Append (My_List, 1);
      Assert (Length (My_List) = 1, "Length does not match (1st append)");
      Append (My_List, 2);
      Assert (Length (My_List) = 2, "Length does not match (2nd append)");

      Iter := First (My_List);
      Assert (Data (Iter) = 1, "Value of 1st item does not match");
      Iter := Next (Iter);
      Assert (Data (Iter) = 2, "Value of 2nd item does not match");
   end Test_Append_Elementary;

   procedure Test_Append_Record is
      use Simple_List;

      My_List : List;
      Obj_1   : constant Simple_Type := Simple_Type'(Value => 1);
      Obj_2   : constant Simple_Type := Simple_Type'(Value => 2);
      Iter    : Iterator;
   begin
      Append (My_List, Obj_1);
      Assert (Length (My_List) = 1, "Length does not match (1st append)");
      Append (My_List, Obj_2);
      Assert (Length (My_List) = 2, "Length does not match (2nd append)");

      Iter := First (My_List);
      Assert (Data (Iter).Value = Obj_1.Value,
              "Value of 1st item does not match");
      Iter := Next (Iter);
      Assert (Data (Iter).Value = Obj_2.Value,
              "Value of 2nd item does not match");
   end Test_Append_Record;

   procedure Test_Remove_All is
      use Simple_List;

      My_List : List;
      Obj_1   : constant Simple_Type := Simple_Type'(Value => 1);
   begin
      Append (My_List, Obj_1);
      Append (My_List, Obj_1);
      Append (My_List, Obj_1);

      Remove_All (My_List);
      Assert (Length (My_List) = 0, "List not empty after Remove_All!");
   end Test_Remove_All;

   procedure Test_First is
      use Simple_List;

      My_List : List;
      Obj_1   : constant Simple_Type := Simple_Type'(Value => 1);
      Iter    : Iterator;
   begin
      Iter := First (My_List);
      Assert (not Is_Valid (Iter), "First (empty) returned valid iterator!");

      Append (My_List, Obj_1);
      Iter := First (My_List);
      Assert (Is_Valid (Iter), "First (nto empty) returned invalid iterator!");
   end Test_First;

   procedure Test_Next is
      use Simple_List;

      Max_Count : constant := 10;

      My_List : List;
      Counter : Count_Type := 0;
      Iter    : Iterator;
   begin
      for A in Integer range 1 .. Max_Count loop
         Append (My_List, Simple_Type'(Value => A));
      end loop;

      Iter := First (My_List);
      loop
         exit when not Is_Valid (Iter);
         Iter := Next (Iter);
         Counter := Counter + 1;
      end loop;
      Assert (Counter = Max_Count, "Invalid counter value: " &
              Count_Type'Image (Counter));
   end Test_Next;

   procedure Test_Data is
      use Simple_List;

      My_List : List;
      Obj_1   : constant Simple_Type := Simple_Type'(Value => 1);
      Iter    : Iterator;
   begin
      Append (My_List, Obj_1);
      Iter := First (My_List);
      Assert (Data (Iter) = Obj_1,
              "Item in the list does not match original item");
   end Test_Data;

   procedure Test_Length is
      use Simple_List;

      My_List : List;
      Obj_1   : constant Simple_Type := Simple_Type'(Value => 1);
   begin
      Assert (Length (My_List) = 0, "Invalid initial length: " &
              Count_Type'Image (Length (My_List)));
      Append (My_List, Obj_1);
      Assert (Length (My_List) = 1, "Invalid length after 1st append: " &
              Count_Type'Image (Length (My_List)));
      Remove_All (My_List);
      Assert (Length (My_List) = 0, "Invalid length after Remove_All: " &
              Count_Type'Image (Length (My_List)));
   end Test_Length;

   procedure Test_Copy is
      use Simple_List;
      Object_Amount : constant := 4;

      My_List : List;
      Copy    : List;
      Obj_1   : constant Simple_Type := Simple_Type'(Value => 1);
      Obj_2   : constant Simple_Type := Simple_Type'(Value => 2);
      Obj_3   : constant Simple_Type := Simple_Type'(Value => 3);

      Iter_1  : Iterator;
      Iter_2  : Iterator;
   begin
      Append (My_List, Obj_1);
      Append (My_List, Obj_2);
      Append (My_List, Obj_3);

      Copy := My_List;
      Assert (Length (Copy) = Length (My_List), "Size does not match!");

      Iter_1 := First (My_List);
      Iter_2 := First (Copy);
      Assert (Data (Iter_1) = Data (Iter_2), "First items not equal!");

      Iter_1 := Next (Iter_1);
      Iter_2 := Next (Iter_2);
      Assert (Data (Iter_1) = Data (Iter_2), "Second items not equal!");

      Iter_1 := Next (Iter_1);
      Iter_2 := Next (Iter_2);
      Assert (Data (Iter_1) = Data (Iter_2), "Third items not equal!");

      declare
         Another_Copy : constant List := My_List;
         --## rule off IMPROPER_INITIALIZATION
         Yet_Another  : List := Copy;
      begin
         Assert (Length (Another_Copy) = Length (My_List),
                 "Size does not match!");
         Iter_1 := First (My_List);
         Iter_2 := First (Another_Copy);
         Assert (Data (Iter_1) = Data (Iter_2),
                 "First items not equal! (Another_Copy)");

         Append (My_List, Obj_1);
         Yet_Another := My_List;

         Iter_1 := First (My_List);
         Iter_2 := First (Yet_Another);
         Assert (Data (Iter_1) = Data (Iter_2),
                 "First items not equal! (Yet_Another)");
      end;

      Assert (Length (My_List) = Object_Amount, "Invalid size: " &
              Count_Type'Image (Length (My_List)));
   end Test_Copy;
end SList_Tests;
