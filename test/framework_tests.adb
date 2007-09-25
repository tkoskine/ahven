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

package body Framework_Tests is
   use Ahven;
   procedure Free is new Ada.Unchecked_Deallocation
     (Simple_Listener.Listener, Simple_Listener.Listener_Access);
   
   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Framework tests");
      Framework.Register_Routine (T, Test_Set_Up'Access, "Set_Up");

      Framework.Register_Routine (T, Test_Test_Result_Add_Pass'Access,
                                  "Test_Result: Add_Pass");
      Framework.Register_Routine (T, Test_Test_Result_Add_Pass'Access,
                                  "Test_Result: Add_Pass");
      Framework.Register_Routine (T, Test_Test_Result_Add_Failure'Access,
                                  "Test_Result: Add_Failure");
      Framework.Register_Routine (T, Test_Test_Result_Add_Error'Access,
                                  "Test_Result: Add_Error");
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
      Ahven.Framework.Add_Failure (Result, Place);
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

end Framework_Tests;
