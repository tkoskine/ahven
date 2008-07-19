--
-- Copyright (c) 2007, 2008 Tero Koskinen <tero.koskinen@iki.fi>
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

with Ahven.Doubly_Linked_List;
with Ahven.Results;
with Ahven.Listeners;

use Ada.Strings.Unbounded;

use Ahven.Results;

pragma Elaborate_All (Ahven.Doubly_Linked_List);

package Ahven.Framework is

   Parameter_Error : exception;

   type Test_Result is private;
   -- A place where the test results are reported. Test_Result
   -- does not store the results, but calls the listeners instead.
   -- It is the responsibility of the listeners to store the results.

   procedure Add_Failure (Result : in out Test_Result; I : Result_Info);
   -- Call Add_Failure for every listener of the Result.
   -- I tells which test failed.

   procedure Add_Error (Result : in out Test_Result; I : Result_Info);
   -- Call Add_Error for every listener of the Result.
   -- I tells which test had an error.

   procedure Add_Pass (Result : in out Test_Result; I : Result_Info);
   -- Call Add_Pass for every listener of the Result.
   -- I tells which test was ok.

   procedure Start_Test (Result : in out Test_Result; Info : Result_Info);
   -- Informs result that the test is about to start.

   procedure End_Test (Result: in out Test_Result; Info : Result_Info);
   -- Informs the result that the test has ended.

   procedure Add_Listener (Result   : in out Test_Result;
                           Listener : Listeners.Result_Listener_Class_Access);
   -- Add a new listener to the Result.
   -- Passing null as Listener raises Parameter_Error.

   type Test is abstract new Ada.Finalization.Controlled with null record;
   -- A type, which provides the base for Test_Case and
   -- Test_Suite types.

   type Test_Class_Access is access all Test'Class;

   procedure Set_Up (T : in out Test);
   -- Set_Up is called before executing the test procedure.

   procedure Tear_Down (T : in out Test);
   -- Tear_Down is called after the test procedure is executed.

   function Get_Name (T : Test) return Unbounded_String is abstract;
   -- Return the name of the test.

   procedure Run (T      : in out Test;
                  Result : in out Test_Result) is abstract;
   -- Run the test and place the test result to Result.

   procedure Run (T         : in out Test;
                  Test_Name :        String;
                  Result    : in out Test_Result) is abstract;
   -- Run the test with given name and place the test result to Result.
   -- Notice: If multiple tests have same name this might call all of
   -- them.

   procedure Execute (T      : in out Test'Class;
                      Result : in out Test_Result);
   -- Call Test class' Run method and place the test outcome to Result.
   -- The procedure calls Start_Test of every listener before calling
   -- the Run procedure and End_Test after calling the Run procedure.

   procedure Execute (T         : in out Test'Class;
                      Test_Name :        String;
                      Result    : in out Test_Result);
   -- Same as Execute above, but call the Run procedure which
   -- takes Test_Name parameter.

   type Test_Case is abstract new Test with private;
   -- The base type for other test cases.

   type Test_Case_Access is access Test_Case'Class;

   function Get_Name (T : Test_Case) return Unbounded_String;
   -- Return the name of the test case.

   procedure Run (T      : in out Test_Case;
                  Result : in out Test_Result);
   -- Run Test_Case's test routines.

   procedure Run (T         : in out Test_Case;
                  Test_Name :        String;
                  Result    : in out Test_Result);
   -- Run Test_Case's test routine which matches to the Name.

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
                               Routine :        Object_Test_Routine_Access;
                               Name    :        String);
   -- Register a test routine to the Test_Case.
   -- The routine must have signature
   --  "procedure R (T : in out Test_Case'Class)".

   procedure Add_Test_Routine (T       : in out Test_Case'Class;
                               Routine :        Simple_Test_Routine_Access;
                               Name    :        String);
   -- Register a simple test routine to the Test_Case.
   -- The routine must have signature
   --  "procedure R".

   type Test_Suite is new Test with private;
   -- A collection of Tests.

   type Test_Suite_Access is access all Test_Suite;

   function Create_Suite (Suite_Name : String)
     return Test_Suite_Access;
   -- Create a new Test_Suite.
   -- Caller must free the returned Test_Suite using Release_Suite.

   function Create_Suite (Suite_Name : String)
     return Test_Suite;
   -- Create a new Test_Suite. The suite and its children are
   -- released automatically.

   procedure Add_Test (Suite : in out Test_Suite; T : Test_Class_Access);
   -- Add a Test to the suite. The suite frees the Test automatically
   -- when it is no longer needed.

   procedure Add_Test (Suite : in out Test_Suite; T : Test_Suite_Access);
   -- Add a Test suite to the suite. The suite frees the Test automatically
   -- when it is no longer needed.
   -- This is a helper function, which internally calls
   -- Add_Test (Suite : in out Test_Suite; T : Test_Class_Access).

   function Get_Name (T : Test_Suite) return Unbounded_String;
   -- Return the name of Test_Suite.

   procedure Run (T      : in out Test_Suite;
                  Result : in out Test_Result);
   -- Run Test_Suite's Test_Cases.

   procedure Run (T         : in out Test_Suite;
                  Test_Name :        String;
                  Result    : in out Test_Result);
   -- Run test suite's child which matches to the given name.

   procedure Finalize (T : in out Test_Suite);
   -- Finalize procedure of Test_Suite. Frees all added Tests.

   procedure Release_Suite (T : Test_Suite_Access);
   -- Release the memory of Test_Suite.
   -- All added tests are released automatically.

private
   type Test_Result is record
      Listeners : Ahven.Listeners.Result_Listener_List.List;
   end record;
   -- A container for test result listeners.
   -- In theory, this type could hold also the test results
   -- but currently it just notifies the listeners.

   type Command_Object_Enum is (SIMPLE, OBJECT);

   type Test_Command (Command_Kind : Command_Object_Enum) is record
      Name : Unbounded_String;
      case Command_Kind is
         when SIMPLE =>
            Simple_Routine : Simple_Test_Routine_Access;
         when OBJECT =>
            Object_Routine : Object_Test_Routine_Access;
      end case;
   end record;
   -- Name attribute tells the name of the test routine.

   type Test_Command_Access is access Test_Command;

   procedure Run (Command : Test_Command; T : in out Test_Case'Class);
   -- Run the specified command.
   -- Calls Set_Up and Tear_Down if necessary.

   package Test_Command_List is
     new Doubly_Linked_List (Data_Type => Test_Command_Access);

   type Test_Case is abstract new Test with record
      Routines : Test_Command_List.List := Test_Command_List.Empty_List;
      Name : Unbounded_String := Null_Unbounded_String;
   end record;
   -- Our test case type. It holds a list of test routines
   -- (test command objects) and the name of the test case.

   procedure Run_Command (Command :        Test_Command;
                          Info    :        Result_Info;
                          Result  : in out Test_Result;
                          T       : in out Test_Case'Class);
   -- Handle dispatching to the right Run (Command : Test_Command)
   -- procedure and record test routine result to the Result object.

   package Test_List is
     new Doubly_Linked_List (Data_Type => Test_Class_Access);

   type Test_Suite is new Test with record
      Suite_Name : Unbounded_String;
      Test_Cases : Test_List.List;
   end record;
   -- A suite type which holds a list of test cases and the name
   -- of the suite.

end Ahven.Framework;
