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
with Simple_Listener;
with Dummy_Tests;

package body Framework_Tests is
   use Ahven;

   procedure Assert_Eq_Count is
     new Ahven.Assert_Equal (Data_Type => Framework.Test_Count_Type,
                             Image     => Framework.Test_Count_Type'Image);

   procedure Assert_Eq_Nat is
     new Ahven.Assert_Equal (Data_Type => Natural,
                             Image     => Natural'Image);
   
   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Simple_Listener.Listener,
      Name   => Simple_Listener.Listener_Access);

   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Ahven.Framework");

      Framework.Add_Test_Routine (T, Test_Set_Up'Access, "Test_Case: Set_Up");
      T.Value := INITIALIZED;
      Framework.Add_Test_Routine (T, Test_Tear_Down'Access,
                                  "Test_Case: Tear_Down");
      Framework.Add_Test_Routine (T, Test_Test_Case_Run'Access,
                                  "Test_Case: Run");
      Framework.Add_Test_Routine (T, Test_Test_Case_Test_Count'Access,
                                  "Test_Case: Test_Count");
      Framework.Add_Test_Routine (T, Test_Call_End_Test'Access,
                                  "Test_Case: Run (Call End_Test)");
      Framework.Add_Test_Routine (T, Test_Test_Suite_Run'Access,
                                  "Test_Suite: Run");
      Framework.Add_Test_Routine (T, Test_Test_Suite_Static_Run'Access,
                                  "Test_Suite: Run (Static)");
      Framework.Add_Test_Routine (T, Test_Test_Suite_Name_Run'Access,
                                  "Test_Suite: Run (Name)");
      Framework.Add_Test_Routine (T, Test_Test_Suite_Inside_Suite'Access,
                                  "Test_Suite: Suite inside another");
      Framework.Add_Test_Routine (T, Test_Test_Suite_Test_Count'Access,
                                  "Test_Suite: Test Count");
      Framework.Add_Test_Routine (T, Test_Test_Suite_Test_Static_Count'Access,
                                  "Test_Suite: Test Count (Static)");
      Framework.Add_Test_Routine (T, Test_Test_Suite_Test_Name_Count'Access,
                                  "Test_Suite: Test Count (Name)");
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

      My_Test : Dummy_Tests.Test;
      My_Listener : Simple_Listener.Listener;
   begin
      Dummy_Tests.Run (My_Test, My_Listener);
      Assert (My_Test.State = Dummy_Tests.DOWN, "Tear_Down not called!");
   end Test_Tear_Down;

   procedure Test_Test_Case_Run is
      use Dummy_Tests;

      My_Listener : Simple_Listener.Listener_Access :=
        new Simple_Listener.Listener;
      My_Test : Dummy_Tests.Test;
   begin
      Dummy_Tests.Run (My_Test, My_Listener.all);

      Assert_Eq_Nat (My_Listener.Passes, Dummy_Passes, "Pass count");
      Assert_Eq_Nat (My_Listener.Errors, Dummy_Errors, "Error count");
      Assert_Eq_Nat (My_Listener.Failures, Dummy_Failures, "Failure count");
      Assert_Eq_Nat (My_Listener.Level, 0, "Listener.Level");
      Assert_Eq_Nat (My_Listener.Start_Calls, Dummy_Test_Count,
              "Start_Test calls");

      Free (My_Listener);
   end Test_Test_Case_Run;

   procedure Test_Test_Case_Test_Count is
      use type Framework.Test_Count_Type;

      Dummy_Test  : Dummy_Tests.Test;
   begin
      Assert_Eq_Count (Dummy_Tests.Test_Count (Dummy_Test),
                       Dummy_Tests.Dummy_Test_Count,
                       "Test Count");
   end Test_Test_Case_Test_Count;

   procedure Test_Test_Suite_Run is
      use Dummy_Tests;

      My_Listener : Simple_Listener.Listener_Access :=
        new Simple_Listener.Listener;
      My_Suite : Framework.Test_Suite_Access;
   begin
      My_Suite := Framework.Create_Suite ("My suite");
      Framework.Add_Test (My_Suite.all, new Dummy_Tests.Test);

      Framework.Run (My_Suite.all, My_Listener.all);

      Assert_Eq_Nat (My_Listener.Passes, Dummy_Passes, "Pass count");
      Assert_Eq_Nat (My_Listener.Errors, Dummy_Errors, "Error count");
      Assert_Eq_Nat (My_Listener.Failures, Dummy_Failures, "Failure count");
      Assert_Eq_Nat (My_Listener.Level, 0, "Listener.Level");
      Assert_Eq_Nat (My_Listener.Start_Calls, (Dummy_Test_Count + 1),
              "Start_Test calls");

      Free (My_Listener);
      Framework.Release_Suite (My_Suite);
   end Test_Test_Suite_Run;

   procedure Test_Test_Suite_Static_Run is
      use Dummy_Tests;

      My_Listener : Simple_Listener.Listener;
      My_Suite : Framework.Test_Suite := Framework.Create_Suite ("My suite");
      Dummy_Test : Dummy_Tests.Test;
   begin
      Framework.Add_Static_Test (My_Suite, Dummy_Test);

      Framework.Run (My_Suite, My_Listener);

      Assert_Eq_Nat (My_Listener.Passes, Dummy_Passes, "Pass count");
      Assert_Eq_Nat (My_Listener.Errors, Dummy_Errors, "Error count");
      Assert_Eq_Nat (My_Listener.Failures, Dummy_Failures, "Failure count");
      Assert_Eq_Nat (My_Listener.Level, 0, "Listener.Level");
      Assert_Eq_Nat (My_Listener.Start_Calls, (Dummy_Test_Count + 1),
              "Start_Test calls");
   end Test_Test_Suite_Static_Run;

   procedure Test_Test_Suite_Name_Run is
      use Dummy_Tests;

      My_Listener : Simple_Listener.Listener;
      My_Suite : Framework.Test_Suite := Framework.Create_Suite ("My suite");
      Dummy_Test : Dummy_Tests.Test;
   begin
      Framework.Add_Static_Test (My_Suite, Dummy_Test);

      Framework.Run (My_Suite, "Failure", My_Listener);

      Assert_Eq_Nat (My_Listener.Passes, 0, "Pass count");
      Assert_Eq_Nat (My_Listener.Errors, 0, "Error count");
      Assert_Eq_Nat (My_Listener.Failures, Dummy_Failures, "Failure count");
      Assert_Eq_Nat (My_Listener.Level, 0, "Listener.Level");
      Assert_Eq_Nat (My_Listener.Start_Calls, (Dummy_Failures + 1),
              "Start_Test calls");
   end Test_Test_Suite_Name_Run;

   procedure Test_Call_End_Test is
      use Dummy_Tests;

      My_Listener : Simple_Listener.Listener_Access :=
        new Simple_Listener.Listener;
      My_Test     : Dummy_Tests.Test;
   begin
      Dummy_Tests.Run (My_Test, My_Listener.all);

      Assert_Eq_Nat (My_Listener.Level, 0, "Listener.Level");
      Assert_Eq_Nat (My_Listener.End_Calls, Dummy_Test_Count,
              "End_Test calls");

      Free (My_Listener);
   end Test_Call_End_Test;

   procedure Test_Test_Suite_Inside_Suite is
      use Dummy_Tests;

      My_Listener : Simple_Listener.Listener_Access :=
        new Simple_Listener.Listener;
      Child       : Framework.Test_Suite_Access;
      Parent      : Framework.Test_Suite;
   begin
      Child := Framework.Create_Suite ("Child suite");
      Framework.Add_Test (Child.all, new Dummy_Tests.Test);

      Parent := Framework.Create_Suite ("Parent suite");
      Framework.Add_Test (Parent, Child);

      Framework.Run (Parent, My_Listener.all);

      Assert_Eq_Nat (My_Listener.Passes, Dummy_Passes, "Amount of passes.");
      Assert_Eq_Nat (My_Listener.Errors, Dummy_Errors, "Amount of errors.");
      Assert_Eq_Nat
        (My_Listener.Failures, Dummy_Failures, "Amount of failures.");
      Assert_Eq_Nat (My_Listener.Level, 0, "Start_Test /= End_Test");
      Assert_Eq_Nat (My_Listener.Start_Calls, (Dummy_Test_Count + 2),
              "Start_Test calls");

      Free (My_Listener);
   end Test_Test_Suite_Inside_Suite;

   -- Test that Test_Count works for dynamic test cases
   procedure Test_Test_Suite_Test_Count is
      use Dummy_Tests;

      Child       : Framework.Test_Suite_Access;
      Parent      : Framework.Test_Suite;
   begin
      Child := Framework.Create_Suite ("Child suite");
      Framework.Add_Test (Child.all, new Dummy_Tests.Test);

      Parent := Framework.Create_Suite ("Parent suite");
      Framework.Add_Test (Parent, Child);

      Assert_Eq_Count
        (Framework.Test_Count (Parent), Dummy_Test_Count, "Test Count");
   end Test_Test_Suite_Test_Count;

   -- Test that Test_Count works for static test cases
   procedure Test_Test_Suite_Test_Static_Count is
      use Dummy_Tests;
      use type Framework.Test_Count_Type;

      Child       : Framework.Test_Suite;
      Parent      : Framework.Test_Suite;
      Dummy_Test  : Dummy_Tests.Test;
   begin
      Child := Framework.Create_Suite ("Child suite");
      Framework.Add_Static_Test (Child, Dummy_Test);

      Parent := Framework.Create_Suite ("Parent suite");
      Framework.Add_Static_Test (Parent, Child);

      Assert_Eq_Count
        (Framework.Test_Count (Parent), Dummy_Test_Count, "Test Count");
   end Test_Test_Suite_Test_Static_Count;

   procedure Test_Test_Suite_Test_Name_Count is
      use Dummy_Tests;
      use type Framework.Test_Count_Type;

      Child       : Framework.Test_Suite;
      Parent      : Framework.Test_Suite;
      Dummy_Test  : Dummy_Tests.Test;
   begin
      Child := Framework.Create_Suite ("Child suite");
      Framework.Add_Static_Test (Child, Dummy_Test);

      Parent := Framework.Create_Suite ("Parent suite");
      Framework.Add_Static_Test (Parent, Child);

      Assert_Eq_Count
        (Framework.Test_Count (Parent, "Failure"), 1, "Test Count");
   end Test_Test_Suite_Test_Name_Count;
end Framework_Tests;
