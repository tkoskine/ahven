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
with Ahven.Listeners;
with Ahven.Results;
with Simple_Listener;
with Dummy_Tests;

package body Framework_Tests is
   use Ahven;

   Dummy_Passes     : constant := 2;
   Dummy_Failures   : constant := 1;
   Dummy_Errors     : constant := 1;
   Dummy_Test_Count : constant := Dummy_Passes + Dummy_Failures + Dummy_Errors;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Simple_Listener.Listener,
      Name   => Simple_Listener.Listener_Access);

   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Ahven.Framework");

      Framework.Add_Test_Routine (T, Test_Test_Result_Add_Pass'Access,
                                  "Test_Result: Add_Pass");
      Framework.Add_Test_Routine (T, Test_Test_Result_Add_Failure'Access,
                                  "Test_Result: Add_Failure");
      Framework.Add_Test_Routine (T, Test_Test_Result_Add_Error'Access,
                                  "Test_Result: Add_Error");
      Framework.Add_Test_Routine (T, Test_Add_Listener_Null'Access,
                                  "Test_Result: Add_Listener (null)");
      Framework.Add_Test_Routine (T, Test_Set_Up'Access, "Test_Case: Set_Up");
      T.Value := INITIALIZED;
      Framework.Add_Test_Routine (T, Test_Tear_Down'Access,
                                  "Test_Case: Tear_Down");
      Framework.Add_Test_Routine (T, Test_Test_Case_Run'Access,
                                  "Test_Case: Run");
      Framework.Add_Test_Routine (T, Test_Call_End_Test'Access,
                                  "Test_Case: Run (Call End_Test)");
      Framework.Add_Test_Routine (T, Test_Test_Suite_Run'Access,
                                  "Test_Suite: Run");
      Framework.Add_Test_Routine (T, Test_Test_Suite_Inside_Suite'Access,
                                  "Test_Suite: Suite inside another");
   end Initialize;

   procedure Set_Up (T : in out Test) is
   begin
      T.Value := SETUP_DONE;
   end Set_Up;

   procedure Tear_Down (T : in out Test) is
   begin
      T.Value := TEARDOWN_DONE;
   end Tear_Down;

   procedure Test_Set_Up (T : in out Ahven.Framework.Test_Case'Class) is
   begin
      Assert (Test (T).Value = SETUP_DONE, "Set_Up not called!");
   end Test_Set_Up;

   procedure Test_Tear_Down is
      use type Dummy_Tests.Test_State;

      Result : Framework.Test_Result;
      My_Test : Dummy_Tests.Test;
   begin
      Dummy_Tests.Run (My_Test, Result);
      Assert (My_Test.State = Dummy_Tests.DOWN, "Tear_Down not called!");
   end Test_Tear_Down;

   procedure Test_Test_Result_Add_Pass is
      Result : Framework.Test_Result;
      My_Listener : Simple_Listener.Listener_Access :=
        new Simple_Listener.Listener;
      Place : Results.Result_Info;
   begin
      Framework.Add_Listener
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
      Place : Results.Result_Info;
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
      Place : Results.Result_Info;
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

      Assert (My_Listener.Passes = Dummy_Passes, "Invalid amount of passes.");
      Assert (My_Listener.Errors = Dummy_Errors, "Invalid amount of errors.");
      Assert
        (My_Listener.Failures = Dummy_Failures, "Invalid amount of failures.");
      Assert (My_Listener.Level = 0, "Start_Test /= End_Test");
      Assert (My_Listener.Start_Calls = Dummy_Test_Count,
              "Start_Test calls: " &
              Integer'Image (My_Listener.Start_Calls));

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

      Assert
        (My_Listener.Passes = Dummy_Passes, "Invalid amount of passes.");
      Assert
        (My_Listener.Errors = Dummy_Errors, "Invalid amount of errors.");
      Assert
        (My_Listener.Failures = Dummy_Failures, "Invalid amount of failures.");
      Assert (My_Listener.Level = 0, "Start_Test /= End_Test");
      Assert (My_Listener.Start_Calls = (Dummy_Test_Count + 1),
              "Start_Test calls: " & Integer'Image (My_Listener.Start_Calls));

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
         when Framework.Parameter_Error =>
            null; -- Ok, this was expected
      end;
   end Test_Add_Listener_Null;

   procedure Test_Call_End_Test is
      Result : Framework.Test_Result;
      My_Listener : Simple_Listener.Listener_Access :=
        new Simple_Listener.Listener;
      My_Test : Dummy_Tests.Test;
   begin
      Framework.Add_Listener
        (Result, Listeners.Result_Listener_Class_Access (My_Listener));

      Dummy_Tests.Run (My_Test, Result);

      Assert (My_Listener.Level = 0, "Start_Test /= End_Test");
      Assert (My_Listener.End_Calls = Dummy_Test_Count,
              "End_Test calls: " & Integer'Image (My_Listener.End_Calls));

      Free (My_Listener);
   end Test_Call_End_Test;

   procedure Test_Test_Suite_Inside_Suite is
      Result : Framework.Test_Result;
      My_Listener : Simple_Listener.Listener_Access :=
        new Simple_Listener.Listener;
      Child : Framework.Test_Suite_Access;
      Parent : Framework.Test_Suite;
   begin
      Child := Framework.Create_Suite ("Child suite");
      Framework.Add_Test (Child.all, new Dummy_Tests.Test);

      Parent := Framework.Create_Suite ("Parent suite");
      Framework.Add_Test (Parent, Child);

      Framework.Add_Listener
        (Result, Listeners.Result_Listener_Class_Access (My_Listener));

      Framework.Run (Parent, Result);

      Assert
        (My_Listener.Passes = Dummy_Passes, "Invalid amount of passes.");
      Assert
        (My_Listener.Errors = Dummy_Errors, "Invalid amount of errors.");
      Assert
        (My_Listener.Failures = Dummy_Failures, "Invalid amount of failures.");
      Assert (My_Listener.Level = 0, "Start_Test /= End_Test");
      Assert (My_Listener.Start_Calls = (Dummy_Test_Count + 2),
              "Start_Test calls: " &
              Integer'Image (My_Listener.Start_Calls));

      Free (My_Listener);
   end Test_Test_Suite_Inside_Suite;
end Framework_Tests;
