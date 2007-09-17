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

   procedure Print_Tests (Iter : Result_Place_List.Iterator;
                          Level : Natural) is
      use Result_Place_List;

      Current : Result_Place_List.Iterator := Iter;
   begin
      loop
         exit when Current = null;
         Pad (Level + 1);
         -- Put (To_String (Data (Iter).Test_Name) & " - ");
         Put_Line (To_String (Data (Iter).Routine_Name));
         Current := Next (Current);
      end loop;
   end Print_Tests;

   procedure Print_Failed (Result : Results.Result; Level : Natural) is
      use Result_List;

      Iter : Iterator := First (Result.Children);
   begin
      if Length (Result.Test_Name) > 0 then
         Pad (Level);
         Put_Line (To_String (Result.Test_Name) & ":");
      end if;
      Print_Tests (Result_Place_List.First (Result.Failures), Level);
      loop
         exit when Iter = null;
         if Failure_Count (Data (Iter).all) > 0 then
            Print_Failed (Data (Iter).all, Level + 1);
         end if;
         Iter := Next (Iter);
      end loop;
   end Print_Failed;

   procedure Print_Errors (Result : Results.Result; Level : Natural) is
      use Result_List;

      Iter : Iterator := First (Result.Children);
   begin
      if Length (Result.Test_Name) > 0 then
         Pad (Level);
         Put_Line (To_String (Result.Test_Name) & ":");
      end if;
      Print_Tests (Result_Place_List.First (Result.Errors), Level);
      loop
         exit when Iter = null;
         if Error_Count (Data (Iter).all) > 0 then
            Print_Errors (Data (Iter).all, Level + 1);
         end if;
         Iter := Next (Iter);
      end loop;
   end Print_Errors;

   procedure Print_Passes (Result : Results.Result; Level : Natural) is
      use Result_List;

      Iter : Iterator := First (Result.Children);
   begin
      if Length (Result.Test_Name) > 0 then
         Pad (Level);
         Put_Line (To_String (Result.Test_Name) & ":");
      end if;
      Print_Tests (Result_Place_List.First (Result.Passes), Level);
      loop
         exit when Iter = null;
         if Pass_Count (Data (Iter).all) > 0 then
            Print_Passes (Data (Iter).all, Level + 1);
         end if;
         Iter := Next (Iter);
      end loop;
   end Print_Passes;

   procedure Report_Results (Result  : Results.Result;
                             Verbose : Boolean := False) is
   begin
      Put_Line ("Passed : " & Integer'Image (Pass_Count (Result)));
      if Verbose then
         Print_Passes (Result, 0);
      end if;
      New_Line;
      if Failure_Count (Result) > 0 then
         Put_Line ("Failed : " & Integer'Image (Failure_Count (Result)));
         Print_Failed (Result, 0);
      end if;
      New_Line;
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

