--
-- Copyright (c) 2008-2009 Tero Koskinen <tero.koskinen@iki.fi>
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
with Ahven.Results;
with Ahven.AStrings;

use Ahven;
use Ahven.AStrings;

package body Results_Tests is
   procedure Assert_Eq_Int is
     new Ahven.Assert_Equal (Data_Type => Integer,
                             Image     => Integer'Image);

   procedure Initialize (T : in out Test) is
      use Ahven.Framework;
   begin
      Set_Name (T, "Ahven.Results");
      Add_Test_Routine (T, Test_Count_Children'Access, "Test Count Children");
      Add_Test_Routine (T, Test_Direct_Count'Access, "Test Direct Count");
      Add_Test_Routine (T, Test_Result_Iterator'Access,
                        "Test Result Iterator");
      Add_Test_Routine (T, Test_Add_Pass'Access, "Test Add Pass");
      Add_Test_Routine (T, Test_Add_Failure'Access, "Test Add Failure");
      Add_Test_Routine (T, Test_Add_Error'Access, "Test Add Error");
   end Initialize;

   procedure Test_Count_Children is
      use Ahven.Results;

      Coll     : Result_Collection;
      Coll_Dyn : Result_Collection_Access;
      Info     : constant Result_Info := Empty_Result_Info;
   begin
      Coll_Dyn := new Result_Collection;
      Add_Error (Coll, Info);
      Add_Pass (Coll_Dyn.all, Info);

      Add_Child (Coll, Coll_Dyn);
      Assert_Eq_Int (Actual   => Test_Count (Coll),
                     Expected => 2,
                     Message  => "test count");
   end Test_Count_Children;

   procedure Test_Direct_Count is
      use Ahven.Results;

      Coll     : Result_Collection;
      Coll_Dyn : Result_Collection_Access;
      Info     : constant Result_Info := Empty_Result_Info;
      Expected_Test_Count : constant := 3;
   begin
      Coll_Dyn := new Result_Collection;
      Add_Error (Coll, Info);
      Add_Failure (Coll, Info);
      Add_Pass (Coll, Info);

      -- This should not be counted in direct test count
      Add_Pass (Coll_Dyn.all, Info);

      Add_Child (Coll, Coll_Dyn);
      Assert_Eq_Int (Actual => Direct_Test_Count (Coll),
                     Expected => Expected_Test_Count,
                     Message => "test count");
      Assert_Eq_Int (Actual => Direct_Test_Count (Coll_Dyn.all),
                     Expected => 1,
                     Message => "test count (dyn)");
   end Test_Direct_Count;

   procedure Test_Result_Iterator is
      use Ahven.Results;

      Coll : Result_Collection;
      Info : Result_Info := Empty_Result_Info;
      Iter : Result_Info_Cursor;
      Msg  : constant Bounded_String := To_Bounded_String ("hello");
      Count : Natural;
   begin
      Set_Message (Info, Msg);
      Add_Error (Coll, Info);
      Add_Failure (Coll, Info);
      Add_Pass (Coll, Info);

      Iter := First_Pass (Coll);
      Count := 0;
      loop
         exit when not Is_Valid (Iter);
         Assert (Get_Message (Data (Iter)) = To_String (Msg),
                 "Invalid message in the item");
         Iter := Next (Iter);
         Count := Count + 1;
      end loop;
      Assert_Eq_Int (Actual   => Count,
                     Expected => 1,
                     Message  => "pass amount");

      Iter := First_Failure (Coll);
      Count := 0;
      loop
         exit when not Is_Valid (Iter);
         Assert (Get_Message (Data (Iter)) = To_String (Msg),
                 "Invalid message in the item");
         Iter := Next (Iter);
         Count := Count + 1;
      end loop;
      Assert_Eq_Int (Actual   => Count,
                     Expected => 1,
                     Message  => "failure amount");

      Iter := First_Error (Coll);
      Count := 0;
      loop
         exit when not Is_Valid (Iter);
         Assert (Get_Message (Data (Iter)) = To_String (Msg),
                 "Invalid message in the item");
         Iter := Next (Iter);
         Count := Count + 1;
      end loop;
      Assert_Eq_Int (Actual   => Count,
                     Expected => 1,
                     Message  => "error amount");
   end Test_Result_Iterator;

   procedure Test_Add_Pass is
      use Ahven.Results;

      Coll : Result_Collection;
      Info : constant Result_Info := Empty_Result_Info;
   begin
      Add_Pass (Coll, Info);
      Assert (Pass_Count (Coll) = 1, "Pass was not added!");
   end Test_Add_Pass;

   procedure Test_Add_Failure is
      use Ahven.Results;

      Coll : Result_Collection;
      Info : constant Result_Info := Empty_Result_Info;
   begin
      Add_Failure (Coll, Info);
      Assert (Failure_Count (Coll) = 1, "Failure was not added!");
   end Test_Add_Failure;

   procedure Test_Add_Error is
      use Ahven.Results;

      Coll : Result_Collection;
      Info : constant Result_Info := Empty_Result_Info;
   begin
      Add_Error (Coll, Info);
      Assert (Error_Count (Coll) = 1, "Error was not added!");
   end Test_Add_Error;

end Results_Tests;
