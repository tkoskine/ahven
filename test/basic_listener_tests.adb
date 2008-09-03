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
with Ahven.Listeners.Basic;
with Ahven.Results;

use Ahven;
use Ahven.Results;

package body Basic_Listener_Tests is

   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Ahven.Listeners.Basic");
      Framework.Add_Test_Routine
        (T, Test_Single_Pass'Access, "Test Single Pass");
   end Initialize;

   procedure Test_Single_Pass is
      Listener : Listeners.Basic.Basic_Listener;
      Info     : Result_Info := Empty_Result_Info;
   begin
      Set_Test_Name (Info, "testname");
      Set_Routine_Name (Info, "routine");

      Listeners.Basic.Start_Test (Listener, Info);
      Listeners.Basic.Add_Pass (Listener, Info);
      Listeners.Basic.End_Test (Listener, Info);

      Assert (Test_Count (Listener.Main_Result) = 1, "Invalid test count: " &
              Integer'Image (Test_Count (Listener.Main_Result)));
   end Test_Single_Pass;
end Basic_Listener_Tests;
