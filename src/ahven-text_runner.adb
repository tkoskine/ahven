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

with Ahven.Runner;
with Ahven.Results;
with Ahven.Listeners.Basic;
with Ahven.Framework;

use Ada.Text_IO;
use Ada.Strings.Unbounded;

use Ahven.Results;
use Ahven.Framework;
use Ahven.Listeners.Basic;

package body Ahven.Text_Runner is

   procedure Pad (Level : Natural) is
   begin
      for A in Integer range 0 .. Level loop
         Put (" ");
      end loop;
   end Pad;

   procedure Print_Test (Place : Results.Result_Place;
                         Level : Natural) is
   begin
      Pad (Level + 1);
      -- Put (To_String (Data (Iter).Test_Name) & " - ");
      Put_Line (To_String (Routine_Name (Place)));
   end Print_Test;

   procedure Print_Failures (Result : in out Results.Result_Collection;
                             Level : Natural) is
      End_Flag : Boolean := False;
      Place : Results.Result_Place;
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
         Print_Test (Place, Level);
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
      Place : Results.Result_Place;
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
         Print_Test (Place, Level);
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
      Place : Results.Result_Place;
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
         Print_Test (Place, Level);
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

   procedure Run (Suite : Framework.Test_Suite_Access) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Basic_Listener, Basic_Listener_Access);
      R : Runner.Test_Runner;
      Listener : Listeners.Basic.Basic_Listener_Access :=
        new Listeners.Basic.Basic_Listener;
   begin
      R.Suite := Framework.Test_Class_Access (Suite);
      Framework.Add_Listener
        (R.Result, Framework.Result_Listener_Class_Access (Listener));
      Runner.Run (R);
      Report_Results (Listener.Main_Result, True);
      Free (Listener);
   end Run;

end Ahven.Text_Runner;

