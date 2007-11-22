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
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Command_Line;

with Ahven.Runner;
with Ahven.Results;
with Ahven.Listeners.Output_Capture;
with Ahven.Framework;
with Ahven.Listeners;

use Ada.Text_IO;
use Ada.Strings.Unbounded;
use Ada.Strings.Fixed;

use Ahven.Results;
use Ahven.Framework;
use Ahven.Listeners.Output_Capture;

package body Ahven.Text_Runner is

   -- Local procedures
   procedure Pad (Level : Natural);
   procedure Pad (Output : in out Unbounded_String; Level : Natural);
   procedure Print_Test     (Place  : Results.Result_Info;
                             Level  : Natural;
                             Result : String);
   procedure Print_Failures (Result : in out Results.Result_Collection;
                             Level  : Natural);
   procedure Print_Errors   (Result : in out Results.Result_Collection;
                             Level  : Natural);
   procedure Print_Passes   (Result : in out Results.Result_Collection;
                             Level  : Natural);
   procedure Report_Results (Result  : in out Results.Result_Collection;
                             Verbose : Boolean := False);
   procedure Print_Log_File (Filename : String);

   procedure Pad (Level : Natural) is
   begin
      for A in Integer range 0 .. Level loop
         Put (" ");
      end loop;
   end Pad;

   procedure Pad (Output : in out Unbounded_String; Level : Natural) is
   begin
      for A in Integer range 0 .. Level loop
         Append (Output, " ");
      end loop;
   end Pad;

   procedure Print_Test (Place : Results.Result_Info;
                         Level : Natural;
                         Result : String) is
      use Ada.Strings;

      Msg : Unbounded_String := Message (Place);
      Output : Unbounded_String := Null_Unbounded_String;
      Result_Out : String (1 .. 7) := (others => ' ');
      Time_Out   : String (1 .. 12) := (others => ' ');
   begin
      Pad (Output, Level + 1);
      -- Put (To_String (Data (Iter).Test_Name) & " - ");
      Append (Output, Routine_Name (Place));
      if Length (Msg) > 0 then
         Append (Output, " - ");
         Append (Output, Msg);
      end if;

      if Length (Output) < 50 then
         Pad (Output, 50 - Length (Output));
      end if;

      Put (To_String (Output));

      -- If we know the name of the routine, we print it,
      -- the result, and the execution time.
      if Length (Routine_Name (Place)) > 0 then
         Move (Source => Result,
               Target => Result_Out,
               Drop => Right,
               Justify => Right,
               Pad => ' ');
         Move (Source => Duration'Image (Execution_Time (Place)),
               Target => Time_Out,
               Drop => Right,
               Justify => Right,
               Pad => ' ');
         Put (" " & Result_Out);
         Put (" " & Time_Out & "s");
      end if;
      new_Line;
   end Print_Test;

   procedure Print_Failures (Result : in out Results.Result_Collection;
                             Level : Natural) is
      End_Flag : Boolean := False;
      Place : Results.Result_Info;
      Child : Results.Result_Collection_Access := null;
   begin
      if Length (Results.Test_Name (Result)) > 0 then
         Pad (Level);
         Put_Line (To_String (Results.Test_Name (Result)) & ":");
      end if;

      Failure_Loop:
      loop
         Next_Failure (Result, Place, End_Flag);
         exit Failure_Loop when End_Flag;
         Print_Test (Place, Level, "FAIL");
         if Length (Output_File (Place)) > 0 then
            Print_Log_File (To_String (Output_File (Place)));
         end if;
      end loop Failure_Loop;

      loop
         Next_Child (Result, Child, End_Flag);
         exit when End_Flag;
         if Failure_Count (Child.all) > 0 then
            Print_Failures (Child.all, Level + 1);
         end if;
      end loop;
   end Print_Failures;

   procedure Print_Errors (Result : in out Results.Result_Collection;
                           Level : Natural) is
      End_Flag : Boolean := False;
      Place : Results.Result_Info;
      Child : Results.Result_Collection_Access := null;
   begin
      if Length (Results.Test_Name (Result)) > 0 then
         Pad (Level);
         Put_Line (To_String (Results.Test_Name (Result)) & ":");
      end if;

      Error_Loop:
      loop
         Next_Error (Result, Place, End_Flag);
         exit Error_Loop when End_Flag;
         Print_Test (Place, Level, "ERROR");
         if Length (Output_File (Place)) > 0 then
            Print_Log_File (To_String (Output_File (Place)));
         end if;
      end loop Error_Loop;

      loop
         Next_Child (Result, Child, End_Flag);
         exit when End_Flag;
         if Error_Count (Child.all) > 0 then
            Print_Errors (Child.all, Level + 1);
         end if;
      end loop;

   end Print_Errors;

   procedure Print_Passes (Result : in out Results.Result_Collection;
                           Level : Natural) is
      End_Flag : Boolean := False;
      Place : Results.Result_Info;
      Child : Results.Result_Collection_Access := null;
   begin
      if Length (Test_Name (Result)) > 0 then
         Pad (Level);
         Put_Line (To_String (Test_Name (Result)) & ":");
      end if;

      Pass_Loop:
      loop
         Next_Pass (Result, Place, End_Flag);
         exit Pass_Loop when End_Flag;
         Print_Test (Place, Level, "PASS");
      end loop Pass_Loop;

      loop
         Next_Child (Result, Child, End_Flag);
         exit when End_Flag;
         if Pass_Count (Child.all) > 0 then
            Print_Passes (Child.all, Level + 1);
         end if;
      end loop;
   end Print_Passes;

   procedure Report_Results (Result  : in out Results.Result_Collection;
                             Verbose : Boolean := False) is
   begin
      Put_Line ("Passed : " & Integer'Image (Pass_Count (Result)));
      if Verbose then
         Print_Passes (Result, 0);
      end if;
      New_Line;
      if Failure_Count (Result) > 0 then
         Put_Line ("Failed : " & Integer'Image (Failure_Count (Result)));
         Print_Failures (Result, 0);
         New_Line;
      end if;
      if Error_Count (Result) > 0 then
         Put_Line ("Errors : " & Integer'Image (Error_Count (Result)));
         Print_Errors (Result, 0);
      end if;
   end Report_Results;

   procedure Print_Log_File (Filename : String) is
      Handle : File_Type;
      Char   : Character;
      First  : Boolean := True;
   begin
      Open (Handle, In_File, Filename);
      loop
         exit when End_Of_File (Handle);
         Get (Handle, Char);
         if First then
            Put_Line ("===== Output =======");
            First := False;
         end if;
         Put (Char);
         if End_Of_Line (Handle) then
            New_Line;
         end if;
      end loop;
      Close (Handle);
      if First = False then
         -- New_Line;
         Put_Line ("====================");
      end if;
   end Print_Log_File;

   procedure Run (Suite : Framework.Test_Suite_Access) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Output_Capture_Listener, Output_Capture_Listener_Access);

      Test : constant Ahven.Framework.Test_Class_Access :=
        Framework.Test_Class_Access (Suite);
      Result : Ahven.Framework.Test_Result;
      Listener : Output_Capture_Listener_Access :=
        new Output_Capture_Listener;
   begin
      Framework.Add_Listener
        (Result, Listeners.Result_Listener_Class_Access (Listener));
      if Ada.Command_Line.Argument_Count > 0 then
         Runner.Run (Test, Ada.Command_Line.Argument (1), Result);
      else
         Runner.Run (Test, Result);
      end if;
      Report_Results (Listener.Main_Result, True);
      Free (Listener);
   end Run;

end Ahven.Text_Runner;
