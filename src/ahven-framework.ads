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

with Ada.Finalization;
with Ada.Strings.Unbounded;
with System.Address_To_Access_Conversions;

with Ahven.Double_Linked_List;
with Ahven.Results;
with Ahven.Listeners;

use Ada.Strings.Unbounded;

use Ahven.Results;

package Ahven.Framework is

   type Test_Result is private;
   -- A place where the test results are reported. Test_Result
   -- does not store the results, but calls the listeners instead.
   -- It is the responsibility of the listeners to store the results.

   procedure Add_Failure (Result : in out Test_Result; P : Result_Place);
   -- Add a test failure to the result.
   -- P tells which test failed.

   procedure Add_Error (Result : in out Test_Result; P : Result_Place);
   -- Add a test error to the result.
   -- P tells which test had the error.

   procedure Add_Pass (Result : in out Test_Result; P : Result_Place);
   -- Add a successful test to the result.
   -- P tells which test was ok.

   procedure Start_Test (Result : in out Test_Result; Place : Result_Place);
   -- Informs result that the test is about to start.

   procedure End_Test (Result: in out Test_Result; Place : Result_Place);
   -- Informs the result that the test has ended.

   procedure Add_Listener (Result   : in out Test_Result;
                           Listener : Listeners.Result_Listener_Class_Access);
   -- Add a new listener to the Result.

   type Test is abstract new Ada.Finalization.Controlled with null record;
   -- A type, which provides the base for Test_Case and
   -- Test_Suite types.

   type Test_Class_Access is access all Test'Class;

   type Test_Access is access Test;

   procedure Set_Up (T : in out Test);
   -- Set_Up is called before executing the test procedure.

   procedure Tear_Down (T : in out Test);
   -- Tear_Down is called after the test procedure is executed.

   function Name (T : Test) return Unbounded_String is abstract;
   -- Return the name of the test.

   procedure Run (T      : in out Test;
                  Result : in out Test_Result) is abstract;
   -- Run the test and place the test result to Result.

   procedure Finalize (T : in out Test);
   -- Finalize procedure of Test.

   procedure Execute (T : in out Test'Class;
                      Result : in out Test_Result);
   -- Call Test class' Run method and place the test outcome to Result.

   type Test_Case is abstract new Test with private;
   -- The base type for other test cases.

   type Test_Case_Access is access all Test_Case;

   type Test_Case_Class_Access is access all Test_Case'Class;

   function Name (T : Test_Case) return Unbounded_String;
   -- Return the name of the test case.

   procedure Run (T      : in out Test_Case;
                  Result : in out Test_Result);
   -- Run Test_Case's test routines.

   procedure Finalize (T : in out Test_Case);
   -- Finalize procedure of the Test_Case.

   procedure Set_Name (T : in out Test_Case; Name : String);
   -- Set Test_Case's name.

   type Object_Test_Routine_Access is
     access procedure (T : in out Test_Case'Class);
   -- A pointer to a test routine which takes Test_Case'Class object
   -- as an argument.
   --
   -- For this kind of test routines, the framework will
   -- call Set_Up and Tear_Down routines before and after
   -- test routine execution.

   type Simple_Test_Routine_Access is access procedure;
   -- A pointer to a test routine which does not take arguments.

   procedure Add_Test_Routine (T       : in out Test_Case'Class;
                               Routine : Object_Test_Routine_Access;
                               Name    : String);
   -- Register a test routine to the Test_Case.
   -- Routine must have signature
   --  "procedure R (T : in out Test_Case'Class)".

   procedure Add_Test_Routine (T       : in out Test_Case'Class;
                               Routine : Simple_Test_Routine_Access;
                               Name    : String);
   -- Register a simple test routine to the Test_Case.
   -- Routine must have signature "procedure R".

   type Test_Suite is new Test with private;
   -- A collection of Tests.

   type Test_Suite_Access is access all Test_Suite;

   type Test_Suite_Class_Access is access Test_Suite'Class;

   function Create_Suite (Suite_Name : String)
     return Test_Suite_Access;
   -- Create a new Test_Suite.
   -- Caller must free the returned Test_Suite using Release_Suite.

   procedure Add_Test (Suite : in out Test_Suite; T : Test_Class_Access);
   -- Add a Test to the suite. The suite frees the Test automatically
   -- when it is no longer needed.

   function Name (T : Test_Suite) return Unbounded_String;
   -- Return the name of Test_Suite.

   procedure Run (T      : in out Test_Suite;
                  Result : in out Test_Result);
   -- Run Test_Suite's Test_Cases.

   procedure Finalize (T : in out Test_Suite);
   -- Finalize procedure of Test_Suite. Frees all added Tests.

   procedure Release_Suite (T : in out Test_Suite_Access);
   -- Release the memory of Test_Suite.
   -- All added tests are released automatically.

private
   type Test_Result is record
      Listeners : Ahven.Listeners.Result_Listener_List.List;
   end record;

   type Test_Command is abstract tagged record
      Name : Unbounded_String;
   end record;

   procedure Run (Command : Test_Command) is abstract;

   type Test_Command_Access is access all Test_Command;
   type Test_Command_Class_Access is access Test_Command'Class;

   package Test_Command_List is
     new Double_Linked_List (Test_Command_Class_Access);

   type Test_Case is abstract new Test with record
      Routines : Test_Command_List.List := Test_Command_List.Empty_List;
      Name : Unbounded_String := Null_Unbounded_String;
   end record;

   package Address_To_Access_Conversions is
     new System.Address_To_Access_Conversions (Test_Case'Class);

   type Test_Object_Command is new Test_Command with record
      Routine : Object_Test_Routine_Access;
      Object  : Address_To_Access_Conversions.Object_Pointer;
   end record;

   type Test_Object_Command_Access is access all Test_Object_Command;

   procedure Run (Command : Test_Object_Command);

   type Test_Simple_Command is new Test_Command with record
      Routine : Simple_Test_Routine_Access;
   end record;

   type Test_Simple_Command_Access is access all Test_Simple_Command;

   procedure Run (Command : Test_Simple_Command);

   package Test_List is new Double_Linked_List (Test_Class_Access);

   type Test_Suite is new Test with record
      Suite_Name : Unbounded_String;
      Test_Cases : Test_List.List;
   end record;

end Ahven.Framework;
