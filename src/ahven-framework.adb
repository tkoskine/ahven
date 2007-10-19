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
with Ada.Exceptions;
with Ada.Calendar;

package body Ahven.Framework is
   use Address_To_Access_Conversions;

   procedure Add_Failure (Result : in out Test_Result; P : Result_Place) is
      use Listeners.Result_Listener_List;

      Iter : Iterator := First (Result.Listeners);
   begin
      loop
         exit when Iter = null;
         Listeners.Add_Failure (Data (Iter).all, P);
         Iter := Next (Iter);
      end loop;

   end Add_Failure;

   procedure Add_Error (Result : in out Test_Result; P : Result_Place) is
      use Listeners.Result_Listener_List;

      Iter : Iterator := First (Result.Listeners);
   begin
      loop
         exit when Iter = null;
         Listeners.Add_Error (Data (Iter).all, P);
         Iter := Next (Iter);
      end loop;
   end Add_Error;

   procedure Add_Pass (Result : in out Test_Result; P : Result_Place) is
      use Listeners.Result_Listener_List;

      Iter : Iterator := First (Result.Listeners);
   begin
      loop
         exit when Iter = null;
         Listeners.Add_Pass (Data (Iter).all, P);
         Iter := Next (Iter);
      end loop;
   end Add_Pass;

   procedure Start_Test
     (Result : in out Test_Result ; Place : Result_Place) is
      use Listeners.Result_Listener_List;

      Iter : Iterator := First (Result.Listeners);
   begin
      loop
         exit when Iter = null;
         Listeners.Start_Test (Data (Iter).all, Place);
         Iter := Next (Iter);
      end loop;
   end Start_Test;

   procedure End_Test (Result: in out Test_Result; Place : Result_Place) is
      use Listeners.Result_Listener_List;

      Iter : Iterator := First (Result.Listeners);
   begin
      loop
         exit when Iter = null;
         Listeners.End_Test (Data (Iter).all, Place);
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

   procedure Finalize (T : in out Test) is
      pragma Unreferenced (T);
   begin
      null;
   end Finalize;

   procedure Execute (T : in out Test'Class;
                      Result : in out Test_Result) is
      N : Unbounded_String := Name (T);
      Place : Result_Place;
   begin
      Set_Test_Name (Place, N);
      Start_Test (Result, Place);
      Run (T, Result);
      End_Test (Result, Place);
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

   procedure Run_Command (Command : Test_Command_Class_Access;
                          Place   : Result_Place;
                          Result  : in out Test_Result) is
      Passed : Boolean := False;
      My_Place : Result_Place := Place;
   begin
      Run (Command.all);
      Passed := True;
      Add_Pass (Result, My_Place);
   exception
      when E : Assertion_Error =>
         Results.Set_Message (My_Place, Ada.Exceptions.Exception_Message (E));
         Add_Failure (Result, My_Place);
      when E : others =>
         if Passed = False then
            Results.Set_Message (My_Place, Ada.Exceptions.Exception_Name (E));
            Add_Error (Result, My_Place);
         else
            raise;
         end if;
   end Run_Command;

   function Name (T : Test_Case) return Unbounded_String is
   begin
      return T.Name;
   end Name;

   procedure Run (T      : in out Test_Case;
                  Result : in out Test_Result) is
      use type Test_Command_List.Iterator;
      use type Ada.Calendar.Time;

      Iter : Test_Command_List.Iterator :=
        Test_Command_List.First (T.Routines);
      Place : Result_Place;
      Start_Time, End_Time : Ada.Calendar.Time;
   begin
      Set_Test_Name (Place, Name (T));
      loop
         exit when Iter = null;
         Set_Routine_Name (Place, Test_Command_List.Data (Iter).Name);

         Start_Test (Result, Place);
         Start_Time := Ada.Calendar.Clock;

         Run_Command (Test_Command_List.Data (Iter), Place, Result);

         End_Time := Ada.Calendar.Clock;
         Set_Execution_Time (Place, End_Time - Start_Time);
         End_Test (Result, Place);

         Iter := Test_Command_List.Next (Iter);
      end loop;
   end Run;

   procedure Finalize  (T : in out Test_Case) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Test_Command'Class,
                                        Test_Command_Class_Access);

      use type Test_Command_List.Iterator;
      use Test_Command_List;
      Ptr : Test_Command_Class_Access;

      Iter : Iterator := First (T.Routines);
   begin
      loop
         exit when Iter = null;
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

   function Name (T : Test_Suite) return Unbounded_String is
   begin
      return T.Suite_Name;
   end Name;

   procedure Run (T      : in out Test_Suite;
                  Result : in out Test_Result) is
      use type Test_List.Iterator;

      Iter : Test_List.Iterator := Test_List.First (T.Test_Cases);
   begin
      loop
         exit when Iter = null;

         Execute (Test_List.Data (Iter).all, Result);
         Iter := Test_List.Next (Iter);
      end loop;
   end Run;

   procedure Finalize  (T : in out Test_Suite) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Test'Class, Test_Class_Access);
      use Test_List;

      Ptr  : Test_Class_Access := null;
      Iter : Iterator := First (T.Test_Cases);
   begin
      loop
         exit when Iter = null;
         Ptr := Data (Iter);
         Free (Ptr);
         Iter := Next (Iter);
      end loop;
      Remove_All (T.Test_Cases);
   end Finalize;

   procedure Release_Suite (T : in out Test_Suite_Access) is
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
