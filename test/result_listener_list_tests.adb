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
with Ahven.Listeners.Result_Listener_List;
with Ahven.Listeners.Basic;
with Ahven.Listeners;

package body Result_Listener_List_Tests is
   use Ahven;
   use Ahven.Framework;
   use Ahven.Listeners;

   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Ahven.Listeners.Result_Listener_List");
      Add_Test_Routine (T, Test_Append'Access, "Test Append");
   end Initialize;

   procedure Test_Append is
      use Result_Listener_List;

      Listener : Basic.Basic_Listener_Class_Access := Basic.Create;
      My_List : List;
      List_Size : Natural;
   begin
      Append (My_List, Result_Listener_Class_Access (Listener));
      List_Size := Size (My_List);

      Assert (List_Size = 1, "Size of list invalid: " &
              Integer'Image (List_Size));
      Basic.Free (Listener);
   end Test_Append;
end Result_Listener_List_Tests;
