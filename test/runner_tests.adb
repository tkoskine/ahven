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

with Ada.Command_Line;
with Ahven;
with Ahven.Results;
with Ahven.Parameters;
with Ahven.Runner;

with Dummy_Tests;

package body Runner_Tests is
   use Ahven;
   use Ahven.Framework;

   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Ahven.Runner");
      Add_Test_Routine (T, Test_Run_Suite'Access, "Test Run_Suite");
   end Initialize;

   procedure Report (Test_Results : Results.Result_Collection;
                     Args         : Parameters.Parameter_Info);

   procedure Report (Test_Results : Results.Result_Collection;
                     Args         : Parameters.Parameter_Info) is
      use Ahven.Results;
      use Dummy_Tests;
   begin
      -- Collection should be filled with results from Dummy_Tests.
      Assert (Pass_Count (Test_Results) = Dummy_Passes,
              "Invalid amount of passes.");
      Assert (Error_Count (Test_Results) = Dummy_Errors,
              "Invalid amount of errors.");
      Assert (Failure_Count (Test_Results) = Dummy_Failures,
              "Invalid amount of failures.");
   end Report;

   procedure Test_Run_Suite is
      My_Suite : Framework.Test_Suite := Framework.Create_Suite ("My suite");
   begin
      Framework.Add_Test (My_Suite, new Dummy_Tests.Test);
      Runner.Run_Suite (My_Suite, Report'Access);
      -- As a side effect, Run_Suite sets the global exit status and
      -- since Dummy_Tests contains failures and errors, we need to
      -- reset the status.
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   end Test_Run_Suite;
end Runner_Tests;
