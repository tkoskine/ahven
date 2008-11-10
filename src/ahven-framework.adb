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

   -- Few local wrapper functions, so we can use Add_Result procedure
   -- for Passes, Failures, and Errors.
   procedure Run_Internal
     (T            : in out Test_Case;
      Listener     : in out Listeners.Result_Listener'Class;
      Command      : Test_Command;
      Test_Name    : Unbounded_String;
      Routine_Name : Unbounded_String);

   procedure Set_Up (T : in out Test) is
   begin
      null;
   end Set_Up;

   procedure Tear_Down (T : in out Test) is
   begin
      null;
   end Tear_Down;

   procedure Execute (T        : in out Test'Class;
                      Listener : in out Listeners.Result_Listener'Class) is
      Info : Result_Info := Empty_Result_Info;
   begin
      Set_Test_Name (Info, Get_Name (T));

      -- This Start_Test here is called for Test_Suites and Test_Cases.
      -- Info includes only the name of the test suite/case.
      --
      -- There is a separate Start_Test/End_Test pair for test routines
      -- in the Run (T : in out Test_Case; ...) procedure.
      Listeners.Start_Test (Listener, Info);

      Run (T, Listener);

      -- Like Start_Test, only for Test_Suites and Test_Cases.
      Listeners.End_Test (Listener, Info);
   end Execute;

   procedure Execute (T           : in out Test'Class;
                      Test_Name   :        String;
                      Listener    : in out Listeners.Result_Listener'Class) is
      Info : Result_Info := Empty_Result_Info;
   begin
      Set_Test_Name (Info, Get_Name (T));

      -- Like in the Ececute procedure above.
      Listeners.Start_Test (Listener, Info);
      Run (T, Test_Name, Listener);
      Listeners.End_Test (Listener, Info);
   end Execute;

   procedure Add_Test_Routine (T       : in out Test_Case'Class;
                               Routine :        Object_Test_Routine_Access;
                               Name    :        String)
   is
      Command : constant Test_Command :=
        Test_Command'(Command_Kind   => OBJECT,
                      Name           => To_Unbounded_String (Name),
                      Object_Routine => Routine);
   begin
      Test_Command_List.Append (T.Routines, Command);
   end Add_Test_Routine;

   procedure Add_Test_Routine (T       : in out Test_Case'Class;
                               Routine :        Simple_Test_Routine_Access;
                               Name    :        String)
   is
      Command : constant Test_Command :=
        Test_Command'(Command_Kind   => SIMPLE,
                      Name           => To_Unbounded_String (Name),
                      Simple_Routine => Routine);
   begin
      Test_Command_List.Append (T.Routines, Command);
   end Add_Test_Routine;

   -- The heart of the package.
   -- Run one test routine (well, Command at this point) and
   -- store the result to the Result object.
   procedure Run_Command (Command  :        Test_Command;
                          Info     :        Result_Info;
                          Listener : in out Listeners.Result_Listener'Class;
                          T        : in out Test_Case'Class) is
      Passed  : Boolean := False; --## rule line off IMPROPER_INITIALIZATION
      My_Info : Result_Info := Info;
   begin
      Run (Command, T);
      Passed := True;
      Listeners.Add_Pass (Listener, My_Info);
   exception
      when E : Assertion_Error =>
         Results.Set_Message (My_Info, Ada.Exceptions.Exception_Message (E));
         Listeners.Add_Failure (Listener, My_Info);

      when E : others =>
         -- Did the exception come from the test (Passed = False) or
         -- from the library routines (Passed = True)?
         if Passed then
            raise;
         else
            Set_Message (My_Info, Ada.Exceptions.Exception_Name (E));
            Set_Long_Message
              (My_Info, Ada.Exceptions.Exception_Message (E));
            Listeners.Add_Error (Listener, My_Info);
         end if;
   end Run_Command;

   function Get_Name (T : Test_Case) return Unbounded_String is
   begin
      return T.Name;
   end Get_Name;

   procedure Run_Internal
     (T            : in out Test_Case;
      Listener     : in out Listeners.Result_Listener'Class;
      Command      :        Test_Command;
      Test_Name    :        Unbounded_String;
      Routine_Name :        Unbounded_String)
   is
      use type Ada.Calendar.Time;

      Info       : Result_Info := Empty_Result_Info;
      Start_Time : Ada.Calendar.Time;
      End_Time   : Ada.Calendar.Time;
   begin
      Set_Test_Name (Info, Test_Name);
      Set_Routine_Name (Info, Routine_Name);

      Listeners.Start_Test (Listener, Info);
      Start_Time := Ada.Calendar.Clock;

      Run_Command (Command  => Command,
                   Info     => Info,
                   Listener => Listener,
                   T        => T);

      End_Time := Ada.Calendar.Clock;
      Set_Execution_Time (Info, End_Time - Start_Time);
      Listeners.End_Test (Listener, Info);
   end Run_Internal;

   -- Run procedure for Test_Case.
   --
   -- Loops over the test routine list, executes the routines,
   -- and calculates the time spent in the routine.
   procedure Run (T        : in out Test_Case;
                  Listener : in out Listeners.Result_Listener'Class)
   is
      use Test_Command_List;

      Iter : Iterator := First (T.Routines);
   begin
      loop
         exit when not Is_Valid (Iter);
         Run_Internal (T            => T,
                       Listener     => Listener,
                       Command      => Data (Iter),
                       Test_Name    => Get_Name (T),
                       Routine_Name => Data (Iter).Name);
         Iter := Next (Iter);
      end loop;
   end Run;

   -- Purpose of the procedure is to run all
   -- test routines with name Test_Name.
   --
   -- The procedure also tracks the execution time of the
   -- test routines and records them to the Result_Info.
   procedure Run (T         : in out Test_Case;
                  Test_Name :        String;
                  Listener  : in out Listeners.Result_Listener'Class)
   is
      use Test_Command_List;

      Iter : Iterator    := First (T.Routines);
   begin
      loop
         exit when not Is_Valid (Iter);
         if To_String (Data (Iter).Name) = Test_Name then
            Run_Internal (T            => T,
                          Listener     => Listener,
                          Command      => Data (Iter),
                          Test_Name    => Get_Name (T),
                          Routine_Name => Data (Iter).Name);
         end if;

         Iter := Next (Iter);
      end loop;
   end Run;

   function Test_Count (T : Test_Case) return Test_Count_Type is
      use Test_Command_List;

      Iter : Iterator := First (T.Routines);
      Counter : Test_Count_Type := 0;
   begin
      loop
         exit when not Is_Valid (Iter);
         Counter := Counter + 1;
         Iter := Next (Iter);
      end loop;
      return Counter;
   end Test_Count;

   function Test_Count (T : Test_Case; Test_Name : String)
     return Test_Count_Type
   is
      use Test_Command_List;

      Iter : Iterator := First (T.Routines);
      Counter : Test_Count_Type := 0;
   begin
      loop
         exit when not Is_Valid (Iter);
         if To_String (Data (Iter).Name) = Test_Name then
            Counter := Counter + 1;
         end if;
         Iter := Next (Iter);
      end loop;

      return Counter;
   end Test_Count;

   procedure Finalize (T : in out Test_Case) is
   begin
      Test_Command_List.Remove_All (T.Routines);
   end Finalize;

   procedure Set_Name (T : in out Test_Case; Name : String) is
   begin
      T.Name := To_Unbounded_String (Name);
   end Set_Name;

   function Create_Suite (Suite_Name : String)
     return Test_Suite_Access is
   begin
      return
        new Test_Suite'(Ada.Finalization.Controlled with
                        Suite_Name        => To_Unbounded_String (Suite_Name),
                        Test_Cases        => Test_List.Empty_List,
                        Static_Test_Cases => Indefinite_Test_List.Empty_List);
   end Create_Suite;

   function Create_Suite (Suite_Name : String)
     return Test_Suite is
   begin
      return (Ada.Finalization.Controlled with
              Suite_Name        => To_Unbounded_String (Suite_Name),
              Test_Cases        => Test_List.Empty_List,
              Static_Test_Cases => Indefinite_Test_List.Empty_List);
   end Create_Suite;

   procedure Add_Test (Suite : in out Test_Suite; T : Test_Class_Access) is
   begin
      Test_List.Append (Suite.Test_Cases, T);
   end Add_Test;

   procedure Add_Test (Suite : in out Test_Suite; T : Test_Suite_Access) is
   begin
      Add_Test (Suite, Test_Class_Access (T));
   end Add_Test;

   procedure Add_Static_Test
     (Suite : in out Test_Suite; T : Test'Class) is
   begin
      Indefinite_Test_List.Append (Suite.Static_Test_Cases, T);
   end Add_Static_Test;

   function Get_Name (T : Test_Suite) return Unbounded_String is
   begin
      return T.Suite_Name;
   end Get_Name;

   procedure Run (T        : in out Test_Suite;
                  Listener : in out Listeners.Result_Listener'Class)
   is
      -- Some nested procedure exercises here.
      --
      -- Execute_Cases is for normal test list
      -- and Execute_Static_Cases is for indefinite test list.
      --
      -- Normal test list does not have For_Each procedure,
      -- so we need to loop manually.

      procedure Execute_Cases (Cases : Test_List.List);
      -- Run Execute procedure for all tests in the Cases list.

      procedure Execute_Test (Current : in out Test'Class);
      -- A helper procedure which runs Execute for the given test.

      procedure Execute_Cases (Cases : Test_List.List) is
         use Test_List;

         Iter : Iterator := First (Cases);
      begin
         loop
            exit when not Is_Valid (Iter);
            Execute_Test (Data (Iter).all);
            Iter := Next (Iter);
         end loop;
      end Execute_Cases;

      procedure Execute_Test (Current : in out Test'Class) is
      begin
         Execute (Current, Listener);
      end Execute_Test;

      procedure Execute_Static_Cases is
        new Indefinite_Test_List.For_Each (Action => Execute_Test);
   begin
      Execute_Cases (T.Test_Cases);
      Execute_Static_Cases (T.Static_Test_Cases);
   end Run;

   procedure Run (T         : in out Test_Suite;
                  Test_Name :        String;
                  Listener  : in out Listeners.Result_Listener'Class)
   is
      use Test_List;

      procedure Execute_Test (Current : in out Test'Class);

      procedure Execute_Test (Current : in out Test'Class) is
      begin
         if To_String (Get_Name (Current)) = Test_Name then
            Execute (Current, Listener);
         else
            Execute (Current, Test_Name, Listener);
         end if;
      end Execute_Test;

      Iter : Iterator := First (T.Test_Cases);
   begin
      if Test_Name = To_String (T.Suite_Name) then
         Run (T, Listener);
      else
         loop
            exit when not Is_Valid (Iter);
            Execute_Test (Test'Class (Data (Iter).all));
            Iter := Next (Iter);
         end loop;
      end if;
   end Run;

   function Test_Count (T : Test_Suite) return Test_Count_Type is
      Counter : Test_Count_Type := 0;
   begin
      declare
         use Test_List;

         Iter : Iterator := First (T.Test_Cases);
      begin
         loop
            exit when not Is_Valid (Iter);
            Counter := Counter + Test_Count (Data (Iter).all);
            Iter := Next (Iter);
         end loop;
      end;

      declare
         use Indefinite_Test_List;

         Iter : Iterator := First (T.Static_Test_Cases);
      begin
         loop
            exit when not Is_Valid (Iter);
            Counter := Counter + Test_Count (Data (Iter));
            Iter := Next (Iter);
         end loop;
      end;

      return Counter;
   end Test_Count;

   function Test_Count (T : Test_Suite; Test_Name : String)
     return Test_Count_Type is
      Counter : Test_Count_Type := 0;
   begin
      if Test_Name = To_String (T.Suite_Name) then
         return Test_Count (T);
      end if;

      declare
         use Test_List;
         Iter : Iterator := First (T.Test_Cases);
      begin
         loop
            exit when not Is_Valid (Iter);
            if To_String (Get_Name (Data (Iter).all)) = Test_Name then
               Counter := Counter + Test_Count (Data (Iter).all);
            end if;
            Iter := Next (Iter);
         end loop;
      end;

      declare
         use Indefinite_Test_List;
         Iter : Iterator := First (T.Static_Test_Cases);
      begin
         loop
            exit when not Is_Valid (Iter);
            if To_String (Get_Name (Data (Iter))) = Test_Name then
               Counter := Counter + Test_Count (Data (Iter));
            end if;
            Iter := Next (Iter);
         end loop;
      end;

      return Counter;
   end Test_Count;

   procedure Finalize  (T : in out Test_Suite) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Test'Class,
                                        Name   => Test_Class_Access);
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
        new Ada.Unchecked_Deallocation (Object => Test_Suite,
                                        Name   => Test_Suite_Access);
      Ptr : Test_Suite_Access := T;
   begin
      Free (Ptr);
   end Release_Suite;

   procedure Run (Command : Test_Command; T : in out Test_Case'Class) is
   begin
      case Command.Command_Kind is
         when SIMPLE =>
            Command.Simple_Routine.all;
         when OBJECT =>
            Set_Up (T);
            Command.Object_Routine.all (T);
            Tear_Down (T);
      end case;
   end Run;

   package body Test_Command_List is
      procedure Remove (Ptr : Node_Access) is
         procedure Free is
           new Ada.Unchecked_Deallocation (Object => Node,
                                           Name   => Node_Access);
         My_Ptr : Node_Access := Ptr;
      begin
         Ptr.Next := null;
         Free (My_Ptr);
      end Remove;

      procedure Append (Target : in out List;
                        Node_Data : Test_Command) is
         New_Node : Node_Access  := null;
      begin
         New_Node := new Node'(Data => Node_Data, Next => null);

         if Target.Last = null then
            Target.Last := New_Node;
            Target.First := New_Node;
         else
            Target.Last.Next := New_Node;
            Target.Last := New_Node;
         end if;
      end Append;

      procedure Remove_All (Target : in out List) is
         Current_Node : Node_Access := Target.First;
         Next_Node : Node_Access := null;
      begin
         while Current_Node /= null loop
            Next_Node := Current_Node.Next;
            Remove (Current_Node);
            Current_Node := Next_Node;
         end loop;

         Target.First := null;
         Target.Last := null;
      end Remove_All;

      function First (Target : List) return Iterator is
      begin
         return Iterator (Target.First);
      end First;

      function Next (Iter : Iterator) return Iterator is
      begin
         if Iter = null then
            raise Invalid_Iterator;
         end if;
         return Iterator (Iter.Next);
      end Next;

      function Data (Iter : Iterator) return Test_Command is
      begin
         return Iter.Data;
      end Data;

      function Is_Valid (Iter : Iterator) return Boolean is
      begin
         return Iter /= null;
      end Is_Valid;

      procedure Initialize (Target : in out List) is
      begin
         Target.Last := null;
         Target.First := null;
      end Initialize;

      procedure Finalize (Target : in out List) is
      begin
         Remove_All (Target);
      end Finalize;

      procedure Adjust (Target : in out List) is
         Target_Last : Node_Access := null;
         Target_First : Node_Access := null;
         Current : Node_Access := Target.First;
         New_Node : Node_Access;
      begin
         while Current /= null loop
            New_Node := new Node'(Data => Current.Data, Next => null);

            if Target_Last = null then
               Target_Last := New_Node;
               Target_First := New_Node;
            else
               Target_Last.Next := New_Node;
               Target_Last := New_Node;
            end if;

            Current := Current.Next;
         end loop;
         Target.First := Target_First;
         Target.Last := Target_Last;
      end Adjust;
   end Test_Command_List;

   package body Test_List is
      procedure Remove (Ptr : Node_Access) is
         procedure Free is
           new Ada.Unchecked_Deallocation (Object => Node,
                                           Name   => Node_Access);
         My_Ptr : Node_Access := Ptr;
      begin
         Ptr.Next := null;
         Free (My_Ptr);
      end Remove;

      procedure Append (Target : in out List;
                        Node_Data : Test_Class_Access) is
         New_Node : Node_Access  := null;
      begin
         New_Node := new Node'(Data => Node_Data, Next => null);

         if Target.Last = null then
            Target.Last := New_Node;
            Target.First := New_Node;
         else
            Target.Last.Next := New_Node;
            Target.Last := New_Node;
         end if;
      end Append;

      procedure Remove_All (Target : in out List) is
         Current_Node : Node_Access := Target.First;
         Next_Node : Node_Access := null;
      begin
         while Current_Node /= null loop
            Next_Node := Current_Node.Next;
            Remove (Current_Node);
            Current_Node := Next_Node;
         end loop;

         Target.First := null;
         Target.Last := null;
      end Remove_All;

      function First (Target : List) return Iterator is
      begin
         return Iterator (Target.First);
      end First;

      function Next (Iter : Iterator) return Iterator is
      begin
         if Iter = null then
            raise Invalid_Iterator;
         end if;
         return Iterator (Iter.Next);
      end Next;

      function Data (Iter : Iterator) return Test_Class_Access is
      begin
         return Iter.Data;
      end Data;

      function Is_Valid (Iter : Iterator) return Boolean is
      begin
         return Iter /= null;
      end Is_Valid;

      procedure Initialize (Target : in out List) is
      begin
         Target.Last := null;
         Target.First := null;
      end Initialize;

      procedure Finalize (Target : in out List) is
      begin
         Remove_All (Target);
      end Finalize;

      procedure Adjust (Target : in out List) is
         Target_Last : Node_Access := null;
         Target_First : Node_Access := null;
         Current : Node_Access := Target.First;
         New_Node : Node_Access;
      begin
         while Current /= null loop
            New_Node := new Node'(Data => Current.Data, Next => null);

            if Target_Last = null then
               Target_Last := New_Node;
               Target_First := New_Node;
            else
               Target_Last.Next := New_Node;
               Target_Last := New_Node;
            end if;

            Current := Current.Next;
         end loop;
         Target.First := Target_First;
         Target.Last := Target_Last;
      end Adjust;
   end Test_List;

   package body Indefinite_Test_List is
      procedure Remove (Ptr : Node_Access) is
         procedure Free is
           new Ada.Unchecked_Deallocation (Object => Node,
                                           Name   => Node_Access);
         procedure Free_Test is
           new Ada.Unchecked_Deallocation (Object => Test'Class,
                                           Name   => Test_Class_Access);
         My_Ptr : Node_Access := Ptr;
      begin
         Ptr.Next := null;
         Free_Test (My_Ptr.Data);
         My_Ptr.Data := null;
         Free (My_Ptr);
      end Remove;

      procedure Append (Target : in out List;
                        Node_Data : Test'Class) is
         New_Node : Node_Access  := null;
      begin
         New_Node := new Node'(Data => new Test'Class'(Node_Data),
                               Next => null);

         if Target.Last = null then
            Target.Last := New_Node;
            Target.First := New_Node;
         else
            Target.Last.Next := New_Node;
            Target.Last := New_Node;
         end if;
      end Append;

      procedure Remove_All (Target : in out List) is
         Current_Node : Node_Access := Target.First;
         Next_Node : Node_Access := null;
      begin
         while Current_Node /= null loop
            Next_Node := Current_Node.Next;
            Remove (Current_Node);
            Current_Node := Next_Node;
         end loop;

         Target.First := null;
         Target.Last := null;
      end Remove_All;

      function First (Target : List) return Iterator is
      begin
         return Iterator (Target.First);
      end First;

      function Next (Iter : Iterator) return Iterator is
      begin
         if Iter = null then
            raise Invalid_Iterator;
         end if;
         return Iterator (Iter.Next);
      end Next;

      function Data (Iter : Iterator) return Test'Class is
      begin
         return Iter.Data.all;
      end Data;

      function Is_Valid (Iter : Iterator) return Boolean is
      begin
         return Iter /= null;
      end Is_Valid;

      procedure For_Each (Target : List) is
         Current_Node : Node_Access := Target.First;
      begin
         while Current_Node /= null loop
            Action (Current_Node.Data.all);
            Current_Node := Current_Node.Next;
         end loop;
      end For_Each;

      procedure Initialize (Target : in out List) is
      begin
         Target.Last := null;
         Target.First := null;
      end Initialize;

      procedure Finalize (Target : in out List) is
      begin
         Remove_All (Target);
      end Finalize;

      procedure Adjust (Target : in out List) is
         Target_Last : Node_Access := null;
         Target_First : Node_Access := null;
         Current : Node_Access := Target.First;
         New_Node : Node_Access;
      begin
         while Current /= null loop
            New_Node := new Node'(Data => new Test'Class'(Current.Data.all),
                                  Next => null);

            if Target_Last = null then
               Target_Last := New_Node;
               Target_First := New_Node;
            else
               Target_Last.Next := New_Node;
               Target_Last := New_Node;
            end if;

            Current := Current.Next;
         end loop;
         Target.First := Target_First;
         Target.Last := Target_Last;
      end Adjust;
   end Indefinite_Test_List;
end Ahven.Framework;
