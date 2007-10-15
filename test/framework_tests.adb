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

with Ada.Unchecked_Deallocation;
with Ahven.Framework;
with Ahven.Listeners;
with Ahven.Results;
with Simple_Listener;
with Dummy_Tests;

package body Framework_Tests is
   use Ahven;
   procedure Free is new Ada.Unchecked_Deallocation
     (Simple_Listener.Listener, Simple_Listener.Listener_Access);
   
   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Framework tests");
      Framework.Add_Test_Routine (T, Test_Set_Up'Access, "Set_Up");

      Framework.Add_Test_Routine (T, Test_Test_Result_Add_Pass'Access,
                                  "Test_Result: Add_Pass");
      Framework.Add_Test_Routine (T, Test_Test_Result_Add_Pass'Access,
                                  "Test_Result: Add_Pass");
      Framework.Add_Test_Routine (T, Test_Test_Result_Add_Failure'Access,
                                  "Test_Result: Add_Failure");
      Framework.Add_Test_Routine (T, Test_Test_Result_Add_Error'Access,
                                  "Test_Result: Add_Error");
      Framework.Add_Test_Routine (T, Test_Add_Listener_Null'Access,
                                  "Test_Result: Add_Listener (null)");
      Framework.Add_Test_Routine (T, Test_Test_Case_Run'Access,
                                  "Test_Case: Run");
      Framework.Add_Test_Routine (T, Test_Test_Suite_Run'Access,
                                  "Test_Suite: Run");
   end Initialize;
   
   procedure Set_Up (T : in out Test) is
   begin
      T.Value := 2;
   end Set_Up;

   procedure Tear_Down (T : in out Test) is
   begin
      T.Value := -3;
   end Tear_Down;
   
   procedure Test_Set_Up (T : in out Ahven.Framework.Test_Case'Class) is
   begin
      Assert (Test (T).Value = 2, "Set_Up not called!");
   end Test_Set_Up;
   
   procedure Test_Test_Result_Add_Pass is
      Result : Framework.Test_Result;
      My_Listener : Simple_Listener.Listener_Access :=
        new Simple_Listener.Listener;
      Place : Results.Result_Place;
   begin
      Ahven.Framework.Add_Listener
        (Result, Listeners.Result_Listener_Class_Access (My_Listener));
      Framework.Add_Pass (Result, Place);

      Assert (My_Listener.Passes = 1, "Invalid amount of passes.");
      Assert (My_Listener.Errors = 0, "Invalid amount of errors.");
      Assert (My_Listener.Failures = 0, "Invalid amount of failures.");

      Free (My_Listener);
   end Test_Test_Result_Add_Pass;

   procedure Test_Test_Result_Add_Failure is
      Result : Framework.Test_Result;
      My_Listener : Simple_Listener.Listener_Access :=
        new Simple_Listener.Listener;
      Place : Results.Result_Place;
   begin
      Framework.Add_Listener
        (Result, Listeners.Result_Listener_Class_Access (My_Listener));
      Framework.Add_Failure (Result, Place);

      Assert (My_Listener.Passes = 0, "Invalid amount of passes.");
      Assert (My_Listener.Errors = 0, "Invalid amount of errors.");
      Assert (My_Listener.Failures = 1, "Invalid amount of failures.");

      Free (My_Listener);
   end Test_Test_Result_Add_Failure;

   procedure Test_Test_Result_Add_Error is
      Result : Framework.Test_Result;
      My_Listener : Simple_Listener.Listener_Access :=
        new Simple_Listener.Listener;
      Place : Results.Result_Place;
   begin
      Framework.Add_Listener
        (Result, Listeners.Result_Listener_Class_Access (My_Listener));
      Framework.Add_Error (Result, Place);

      Assert (My_Listener.Passes = 0, "Invalid amount of passes.");
      Assert (My_Listener.Errors = 1, "Invalid amount of errors.");
      Assert (My_Listener.Failures = 0, "Invalid amount of failures.");

      Free (My_Listener);
   end Test_Test_Result_Add_Error;
   
   procedure Test_Test_Case_Run is
      Result : Framework.Test_Result;
      My_Listener : Simple_Listener.Listener_Access :=
        new Simple_Listener.Listener;
      My_Test : Dummy_Tests.Test;
   begin
      Framework.Add_Listener
        (Result, Listeners.Result_Listener_Class_Access (My_Listener));

      Dummy_Tests.Run (My_Test, Result);

      Assert (My_Listener.Passes = 1, "Invalid amount of passes.");
      Assert (My_Listener.Errors = 1, "Invalid amount of errors.");
      Assert (My_Listener.Failures = 1, "Invalid amount of failures.");

      Free (My_Listener);
   end Test_Test_Case_Run;
   
   procedure Test_Test_Suite_Run is
      Result : Framework.Test_Result;
      My_Listener : Simple_Listener.Listener_Access :=
        new Simple_Listener.Listener;
      My_Suite : Framework.Test_Suite_Access;
   begin
      My_Suite := Framework.Create_Suite ("My suite");
      Framework.Add_Test (My_Suite.all, new Dummy_Tests.Test);
      Framework.Add_Listener
        (Result, Listeners.Result_Listener_Class_Access (My_Listener));

      Framework.Run (My_Suite.all, Result);

      Assert (My_Listener.Passes = 1, "Invalid amount of passes.");
      Assert (My_Listener.Errors = 1, "Invalid amount of errors.");
      Assert (My_Listener.Failures = 1, "Invalid amount of failures.");

      Free (My_Listener);
      Framework.Release_Suite (My_Suite);
   end Test_Test_Suite_Run;
   
   procedure Test_Add_Listener_Null is
      Result : Framework.Test_Result;
   begin
      begin
         Framework.Add_Listener (Result, null);
         Fail ("No exception thrown");
      exception
         when Ahven.Framework.Parameter_Error =>
            null; -- Ok, this was expected
      end;
   end Test_Add_Listener_Null;

end Framework_Tests;
