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

use Ada.Strings.Unbounded;

package Ahven.Framework is

   type Result_Place is record
      Test_Name : Unbounded_String;
      Routine_Name : Unbounded_String;
   end record;

   package Result_List is
     new Ahven.Double_Linked_List (Result_Place);

   type Result_Listener is abstract tagged null record;
   type Result_Listener_Access is access Result_Listener;
   type Result_Listener_Class_Access is access Result_Listener'Class;

   procedure Add_Pass (Listener : Result_Listener; Place : Result_Place)
     is abstract;
   -- Called after test passes.

   procedure Add_Failure (Listener : Result_Listener; Place : Result_Place)
     is abstract;
   -- Called after test fails.

   procedure Add_Error (Listener : Result_Listener; Place : Result_Place)
     is abstract;
   -- Called after there is an error in the test.

   procedure Start_Test (Listener : Result_Listener; Place : Result_Place)
     is abstract;
   -- Called before the test begins.

   procedure End_Test (Listener : Result_Listener; Place : Result_Place)
     is abstract;
   -- Called after the test ends. Add_* procedures are called before this.

   package Result_Listener_List is
     new Ahven.Double_Linked_List (Result_Listener_Class_Access);

   type Test_Result is record
      Error_Results   : Result_List.List;
      Failure_Results : Result_List.List;
      Pass_Results    : Result_List.List;
      Listeners       : Result_Listener_List.List;
   end record;

   procedure Add_Failure (Result : in out Test_Result; P : Result_Place);
   -- Add a test failure to the result.
   -- P tells which test failed.

   procedure Add_Error   (Result : in out Test_Result; P : Result_Place);
   -- Add a test error to the result.
   -- P tells which test had the error.

   procedure Add_Pass    (Result : in out Test_Result; P : Result_Place);
   -- Add a successful test to the result
   -- P tells which test was ok.

   procedure Start_Test (Result : in out Test_Result ; Place : Result_Place);
   -- Informs result that the test is about to start.

   procedure End_Test (Result: in out Test_Result; Place : Result_Place);
   -- Informs the result that the test has ended.

   procedure Add_Listener(Result : in out Test_Result;
                          Listener : Result_Listener_Class_Access);

   type Test is abstract new Ada.Finalization.Controlled with null record;
   type Test_Class_Access is access all Test'Class;
   type Test_Access is access Test;

   procedure Set_Up    (T : in out Test);
   -- Set_Up is called before executing the test procedure.

   procedure Tear_Down (T : in out Test);
   -- Tear_Down is called after the test procedure is executed.

   function  Name      (T : Test) return Unbounded_String is abstract;
   -- Return the name of the test.

   procedure Run       (T      : in out Test;
                        Result : in out Test_Result) is abstract;
   -- Run the test and place the test result to Result.

   procedure Register_Routines (T : in out Test) is abstract;
   procedure Execute   (T : Test_Class_Access;
                        Result : in out Test_Result);

   procedure Finalize  (T : in out Test) is null;

   type Test_Case is abstract new Test with private;
   type Test_Case_Access is access all Test_Case;
   type Test_Case_Class_Access is access all Test_Case'Class;

   function Name (T : Test_Case) return Unbounded_String;
   procedure Run (T      : in out Test_Case;
                  Result : in out Test_Result);
   procedure Finalize (T : in out Test_Case);
   procedure Set_Name (T : in out Test_Case; Name : String);

   type Object_Test_Routine_Access is
     access procedure (T : in out Test_Case'Class);
   type Simple_Test_Routine_Access is access procedure;

   procedure Register_Routine (T       : in out Test_Case'Class;
                               Routine : Object_Test_Routine_Access;
                               Name    : String);
   procedure Register_Routine (T       : in out Test_Case'Class;
                               Routine : Simple_Test_Routine_Access;
                               Name    : String);
   procedure Register_Routines (T : in out Test_Case) is abstract;

   type Test_Suite is new Test with private;
   type Test_Suite_Access is access all Test_Suite;
   type Test_Suite_Class_Access is access Test_Suite'Class;

   function Create_Suite (Suite_Name : String)
     return Test_Suite_Access;
   procedure Add_Test (Suite : in out Test_Suite; T : Test_Class_Access);
   function  Name     (T : Test_Suite) return Unbounded_String;
   procedure Run (T      : in out Test_Suite;
                  Result : in out Test_Result);
   procedure Finalize  (T : in out Test_Suite);
   procedure Release_Suite (T : in out Test_Suite_Access);
   procedure Register_Routines (T : in out Test_Suite);

private
   type Test_Command is abstract tagged record
      Name : Unbounded_String;
   end record;

   procedure Run (Command : Test_Command) is abstract;

   type Test_Command_Access is access all Test_Command;
   type Test_Command_Class_Access is access Test_Command'Class;

   package Test_Command_List is
     new Ahven.Double_Linked_List (Test_Command_Class_Access);

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

   package Test_List is new Ahven.Double_Linked_List (Test_Class_Access);

   type Test_Suite is new Test with record
      Suite_Name : Unbounded_String;
      Test_Cases : Test_List.List;
   end record;

end Ahven.Framework;
