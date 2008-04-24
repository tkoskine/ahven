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
with Ada.Strings.Unbounded;
with Ahven;
with Ahven.Results;

use Ada.Strings.Unbounded;
use Ahven;

package body Results_Tests is

   procedure Initialize (T : in out Test) is
      use Ahven.Framework;
   begin
      Set_Name (T, "Ahven.Results");
      Add_Test_Routine (T, Test_Count_Children'Access, "Test Count Children");
      Add_Test_Routine (T, Test_Direct_Count'Access, "Test Direct Count");
      Add_Test_Routine (T, Test_Result_Iterator'Access,
                        "Test Result Iterator");
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
      Assert (2 = Test_Count (Coll), "Invalid test count");
   end Test_Count_Children;

   procedure Test_Direct_Count is
      use Ahven.Results;

      Coll     : Result_Collection;
      Coll_Dyn : Result_Collection_Access;
      Info     : constant Result_Info := Empty_Result_Info;
   begin
      Coll_Dyn := new Result_Collection;
      Add_Error (Coll, Info);
      Add_Failure (Coll, Info);
      Add_Pass (Coll, Info);
      Add_Pass (Coll_Dyn.all, Info);

      Add_Child (Coll, Coll_Dyn);
      Assert (3 = Direct_Test_Count (Coll), "Invalid test count: "
              & Integer'Image (Direct_Test_Count (Coll)));
      Assert (1 = Direct_Test_Count (Coll_Dyn.all), "Invalid test count: "
              & Integer'Image (Direct_Test_Count (Coll_Dyn.all)));
   end Test_Direct_Count;

   procedure Test_Result_Iterator is
      use Ahven.Results;

      Coll : Result_Collection;
      Info : Result_Info := Empty_Result_Info;
      Iter : Result_Info_Iterator;
      Msg  : constant Unbounded_String := To_Unbounded_String ("hello");
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
         Assert (Get_Message (Data (Iter)) = Msg,
                 "Invalid message in the item");
         Iter := Next (Iter);
         Count := Count + 1;
      end loop;
      Assert (Count = 1, "Invalid amount of passes");

      Iter := First_Failure (Coll);
      Count := 0;
      loop
         exit when not Is_Valid (Iter);
         Assert (Get_Message (Data (Iter)) = Msg,
                 "Invalid message in the item");
         Iter := Next (Iter);
         Count := Count + 1;
      end loop;
      Assert (Count = 1, "Invalid amount of failures");

      Iter := First_Error (Coll);
      Count := 0;
      loop
         exit when not Is_Valid (Iter);
         Assert (Get_Message (Data (Iter)) = Msg,
                 "Invalid message in the item");
         Iter := Next (Iter);
         Count := Count + 1;
      end loop;
      Assert (Count = 1, "Invalid amount of errors");
   end Test_Result_Iterator;
end Results_Tests;
