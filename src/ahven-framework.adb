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

with Ada.Unchecked_Deallocation;
with Ada.Exceptions;
with Ada.Calendar;

package body Ahven.Framework is
   use Test_Case_Address_Conversion;

   procedure Add_Failure_L (L : access Listeners.Result_Listener'Class;
                            I : Result_Info);
   procedure Add_Error_L (L : access Listeners.Result_Listener'Class;
                          I : Result_Info);
   procedure Add_Pass_L (L : access Listeners.Result_Listener'Class;
                         I : Result_Info);

   type Add_Procedure is access
     procedure (L : access Listeners.Result_Listener'Class; I : Result_Info);

   procedure Add_Result (Result : in out Test_Result; I : Result_Info;
                         Add : Add_Procedure);

   procedure Add_Failure_L (L : access Listeners.Result_Listener'Class;
                          I : Result_Info) is
   begin
      Listeners.Add_Failure (L.all, I);
   end Add_Failure_L;

   procedure Add_Error_L (L : access Listeners.Result_Listener'Class;
                        I : Result_Info) is
   begin
      Listeners.Add_Error (L.all, I);
   end Add_Error_L;

   procedure Add_Pass_L (L : access Listeners.Result_Listener'Class;
                       I : Result_Info) is
   begin
      Listeners.Add_Pass (L.all, I);
   end Add_Pass_L;

   procedure Add_Result (Result : in out Test_Result; I : Result_Info;
                         Add : Add_Procedure) is
      use Listeners.Result_Listener_List;

      Iter : Iterator := First (Result.Listeners);
   begin
      loop
         exit when not Is_Valid (Iter);
         Add (Data (Iter), I);
         Iter := Next (Iter);
      end loop;
   end Add_Result;

   procedure Add_Failure (Result : in out Test_Result; I : Result_Info) is
   begin
      Add_Result (Result, I, Add_Failure_L'Access);
   end Add_Failure;

   procedure Add_Error (Result : in out Test_Result; I : Result_Info) is
   begin
      Add_Result (Result, I, Add_Error_L'Access);
   end Add_Error;

   procedure Add_Pass (Result : in out Test_Result; I : Result_Info) is
   begin
      Add_Result (Result, I, Add_Pass_L'Access);
   end Add_Pass;

   -- Notify listeners that the test is about to begin.
   --
   -- Execute procedure calls this for testsuites and testcases.
   -- Test case's Run procedure should call this for the test case's
   -- test routines.
   procedure Start_Test
     (Result : in out Test_Result ; Info : Result_Info) is
      use Listeners.Result_Listener_List;

      Iter : Iterator := First (Result.Listeners);
   begin
      loop
         exit when not Is_Valid (Iter);
         Listeners.Start_Test (Data (Iter).all, Info);
         Iter := Next (Iter);
      end loop;
   end Start_Test;

   -- Same as Start_Test, but for the end of the test.
   procedure End_Test (Result: in out Test_Result; Info : Result_Info) is
      use Listeners.Result_Listener_List;

      Iter : Iterator := First (Result.Listeners);
   begin
      loop
         exit when not Is_Valid (Iter);
         Listeners.End_Test (Data (Iter).all, Info);
         Iter := Next (Iter);
      end loop;
   end End_Test;

   procedure Add_Listener
     (Result : in out Test_Result;
      Listener : Listeners.Result_Listener_Class_Access) is

      use type Listeners.Result_Listener_Class_Access;
   begin
      if Listener = null then
         raise Parameter_Error;
      else
         Listeners.Result_Listener_List.Append (Result.Listeners, Listener);
      end if;
   end Add_Listener;

   procedure Set_Up (T : in out Test) is
      pragma Unreferenced (T);
   begin
      null;
   end Set_Up;

   procedure Tear_Down (T : in out Test) is
      pragma Unreferenced (T);
   begin
      null;
   end Tear_Down;

   procedure Execute (T      :        Test'Class;
                      Result : in out Test_Result) is
      N : constant Unbounded_String := Get_Name (T);
      Info : Result_Info   := Empty_Result_Info;
   begin
      Set_Test_Name (Info, N);

      -- This Start_Test here is called for Test_Suites and Test_Cases.
      -- Info includes only the name of the test suite/case.
      --
      -- There is a separate Start_Test/End_Test pair for test routines
      -- in the Run (T : in out Test_Case; ...) procedure.
      Start_Test (Result, Info);

      Run (T, Result);

      -- Like Start_Test, only for Test_Suites and Test_Cases.
      End_Test (Result, Info);
   end Execute;

   procedure Execute (T           :        Test'Class;
                      Test_Name   :        String;
                      Result      : in out Test_Result) is
      N : constant Unbounded_String := Get_Name (T);
      Info : Result_Info   := Empty_Result_Info;
   begin
      Set_Test_Name (Info, N);

      -- Like in the Ececute procedure above.
      Start_Test (Result, Info);
      Run (T, Test_Name, Result);
      End_Test (Result, Info);
   end Execute;

   procedure Add_Test_Routine (T       : in out Test_Case'Class;
                               Routine : Object_Test_Routine_Access;
                               Name    : String) is
      Command : constant Test_Command_Class_Access :=
        new Test_Object_Command'(Name => To_Unbounded_String (Name),
                                 Object => To_Pointer (T'Address),
                                 Routine => Routine);

   begin
      Test_Command_List.Append (T.Routines, Command);
   end Add_Test_Routine;

   procedure Add_Test_Routine (T       : in out Test_Case'Class;
                               Routine : Simple_Test_Routine_Access;
                               Name    : String) is
      Command : constant Test_Command_Class_Access :=
        new Test_Simple_Command'(Name => To_Unbounded_String (Name),
                                 Routine => Routine);
   begin
      Test_Command_List.Append (T.Routines, Command);
   end Add_Test_Routine;

   -- The heart of the package.
   -- Run one test routine (well, Command at this point) and
   -- store the result to the Result object.
   procedure Run_Command (Command : Test_Command_Class_Access;
                          Info    : Result_Info;
                          Result  : in out Test_Result) is
      Passed  : Boolean := False; --## rule line off IMPROPER_INITIALIZATION
      My_Info : Result_Info := Info;
   begin
      Run (Command.all);
      Passed := True;
      Add_Pass (Result, My_Info);
   exception
      when E : Assertion_Error =>
         Results.Set_Message (My_Info, Ada.Exceptions.Exception_Message (E));
         Add_Failure (Result, My_Info);

      when E : others =>
         -- Did the exception come from the test (Passed = False) or
         -- from the library routines (Passed = True)?
         if Passed then
            raise;
         else
            Set_Message (My_Info, Ada.Exceptions.Exception_Name (E));
            Set_Long_Message
              (My_Info, Ada.Exceptions.Exception_Message (E));
            Add_Error (Result, My_Info);
         end if;
   end Run_Command;

   function Get_Name (T : Test_Case) return Unbounded_String is
   begin
      return T.Name;
   end Get_Name;

   -- Run procedure for Test_Case.
   --
   -- Loops over the test routine list, executes the routines,
   -- and calculates the time spent in the routine.
   procedure Run (T      :        Test_Case;
                  Result : in out Test_Result) is
      use Test_Command_List;
      use type Ada.Calendar.Time;

      Iter : Test_Command_List.Iterator :=
        Test_Command_List.First (T.Routines);
      Info : Result_Info := Empty_Result_Info;
      Start_Time, End_Time : Ada.Calendar.Time;
   begin
      Set_Test_Name (Info, Get_Name (T));
      loop
         exit when not Is_Valid (Iter);
         Set_Routine_Name (Info, Test_Command_List.Data (Iter).Name);

         Start_Test (Result, Info);
         Start_Time := Ada.Calendar.Clock;

         Run_Command (Test_Command_List.Data (Iter), Info, Result);

         End_Time := Ada.Calendar.Clock;
         Set_Execution_Time (Info, End_Time - Start_Time);
         End_Test (Result, Info);

         Iter := Test_Command_List.Next (Iter);
      end loop;
   end Run;

   -- Purpose of the procedure is to run all
   -- test routines with name Test_Name.
   --
   -- The procedure also tracks the execution time of the
   -- test routines and records them to the Result_Info.
   procedure Run (T         :        Test_Case;
                  Test_Name :        String;
                  Result    : in out Test_Result) is
      use Test_Command_List;
      use type Ada.Calendar.Time;

      Iter : Test_Command_List.Iterator :=
        Test_Command_List.First (T.Routines);
      Info : Result_Info := Empty_Result_Info;
      Start_Time, End_Time : Ada.Calendar.Time;
   begin
      Set_Test_Name (Info, Get_Name (T));
      loop
         exit when not Is_Valid (Iter);
         if To_String (Test_Command_List.Data (Iter).Name) = Test_Name then
            Set_Routine_Name (Info, Test_Command_List.Data (Iter).Name);

            Start_Test (Result, Info);
            Start_Time := Ada.Calendar.Clock;

            Run_Command (Test_Command_List.Data (Iter), Info, Result);

            End_Time := Ada.Calendar.Clock;
            Set_Execution_Time (Info, End_Time - Start_Time);
            End_Test (Result, Info);
         end if;

         Iter := Test_Command_List.Next (Iter);
      end loop;
   end Run;

   procedure Finalize  (T : in out Test_Case) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Test_Command'Class,
                                        Test_Command_Class_Access);

      use Test_Command_List;
      Ptr  : Test_Command_Class_Access := null;
      Iter : Iterator := First (T.Routines);
   begin
      loop
         exit when not Is_Valid (Iter);
         Ptr := Data (Iter);
         Free (Ptr);
         Iter := Next (Iter);
      end loop;
      Remove_All (T.Routines);
   end Finalize;

   procedure Set_Name (T : in out Test_Case; Name : String) is
   begin
      T.Name := To_Unbounded_String (Name);
   end Set_Name;

   function Create_Suite (Suite_Name : String)
     return Test_Suite_Access is
      Suite : constant Test_Suite_Access :=
        new Test_Suite'(Ada.Finalization.Controlled with
                        Suite_Name => To_Unbounded_String (Suite_Name),
                        Test_Cases => Test_List.Empty_List);
   begin
      return Suite;
   end Create_Suite;

   procedure Add_Test (Suite : in out Test_Suite; T : Test_Class_Access) is
   begin
      Test_List.Append (Suite.Test_Cases, T);
   end Add_Test;

   function Get_Name (T : Test_Suite) return Unbounded_String is
   begin
      return T.Suite_Name;
   end Get_Name;

   procedure Run (T      :        Test_Suite;
                  Result : in out Test_Result) is
      use Test_List;

      Iter : Iterator := First (T.Test_Cases);
   begin
      loop
         exit when not Is_Valid (Iter);
         Execute (Data (Iter).all, Result);
         Iter := Next (Iter);
      end loop;
   end Run;

   procedure Run (T         :        Test_Suite;
                  Test_Name :        String;
                  Result    : in out Test_Result) is
      use Test_List;

      Iter : Iterator := First (T.Test_Cases);
   begin
      if Test_Name = To_String (T.Suite_Name) then
         Run (T, Result);
      else
         loop
            exit when not Is_Valid (Iter);

            if To_String (Get_Name (Data (Iter).all)) = Test_Name then
               Execute (Data (Iter).all, Result);
            else
               Execute (Data (Iter).all, Test_Name, Result);
            end if;
            Iter := Next (Iter);
         end loop;
      end if;
   end Run;

   procedure Finalize  (T : in out Test_Suite) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Test'Class, Test_Class_Access);
      use Test_List;

      Ptr  : Test_Class_Access := null;
      Iter : Iterator := First (T.Test_Cases);
   begin
      loop
         exit when not Is_Valid (Iter);
         Ptr := Data (Iter);
         Free (Ptr);
         Iter := Next (Iter);
      end loop;
      Remove_All (T.Test_Cases);
   end Finalize;

   procedure Release_Suite (T : Test_Suite_Access) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Test_Suite, Test_Suite_Access);
      Ptr : Test_Suite_Access := T;
   begin
      Free (Ptr);
   end Release_Suite;

   procedure Run (Command : Test_Object_Command) is
   begin
      Set_Up (Command.Object.all);
      Command.Routine.all (Command.Object.all);
      Tear_Down (Command.Object.all);
   end Run;

   procedure Run (Command : Test_Simple_Command) is
   begin
      Command.Routine.all;
   end Run;
end Ahven.Framework;
