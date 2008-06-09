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

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Characters.Latin_1;

with Ahven.Runner;
with Ahven.XML_Runner;

use Ada.Text_IO;
use Ada.Strings.Unbounded;
use Ada.Strings.Fixed;

package body Ahven.Text_Runner is
   use Ahven.Results;
   use Ahven.Framework;

   -- Local procedures
   procedure Pad (Level : Natural);
   procedure Pad (Output : in out Unbounded_String; Level : Natural);
   procedure Multiline_Pad (Output : in out Unbounded_String;
                            Input  :        String;
                            Level  :        Natural);

   procedure Print_Test (Info   : Result_Info;
                         Level  : Natural;
                         Result : String);

   procedure Print_Failures (Result : Result_Collection;
                             Level  : Natural);

   procedure Print_Errors (Result : Result_Collection;
                           Level  : Natural);

   procedure Print_Passes (Result : Result_Collection;
                           Level  : Natural);

   procedure Report_Results (Result  : Result_Collection;
                             Verbose : Boolean := False);

   procedure Print_Log_File (Filename : String);

   procedure Pad (Level : Natural) is
   begin
      for A in Integer range 1 .. Level loop
         Put (" ");
      end loop;
   end Pad;

   procedure Pad (Output : in out Unbounded_String; Level : Natural) is
   begin
      for A in Integer range 1 .. Level loop
         Append (Output, " ");
      end loop;
   end Pad;

   procedure Multiline_Pad (Output : in out Unbounded_String;
                            Input  :        String;
                            Level  :        Natural) is
   begin
      Pad (Output, Level);
      for A in Input'Range loop
         Append (Output, Input (A));
         if (Input (A) = Ada.Characters.Latin_1.LF) and (A /= Input'Last) then
            Pad (Output, Level);
         end if;
      end loop;
   end Multiline_Pad;

   procedure Print_Test (Info   : Result_Info;
                         Level  : Natural;
                         Result : String) is
      use Ada.Strings;

      Msg        : constant Unbounded_String := Get_Message (Info);
      Output     : Unbounded_String := Null_Unbounded_String;
      Result_Out : String (1 .. 7) := (others => ' ');
      Time_Out   : String (1 .. 12) := (others => ' ');
   begin
      Pad (Output, Level + 1);
      Append (Output, Get_Routine_Name (Info));
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
      if Length (Get_Routine_Name (Info)) > 0 then
         Move (Source => Result,
               Target => Result_Out,
               Drop => Right,
               Justify => Right,
               Pad => ' ');
         Move (Source => Duration'Image (Get_Execution_Time (Info)),
               Target => Time_Out,
               Drop => Right,
               Justify => Right,
               Pad => ' ');
         Put (" " & Result_Out);
         Put (" " & Time_Out & "s");
      end if;
      if Length (Get_Long_Message (Info)) > 0 then
         New_Line;
         Output := Null_Unbounded_String;
         Multiline_Pad
           (Output, To_String (Get_Long_Message (Info)), Level + 2);
         Put (To_String (Output));
      end if;

      New_Line;
   end Print_Test;

   procedure Print_Failures (Result : Result_Collection;
                             Level  : Natural) is
      Iter       : Result_Info_Iterator;
      Child_Iter : Result_Collection_Iterator;
   begin
      if Length (Get_Test_Name (Result)) > 0 then
         Pad (Level);
         Put_Line (To_String (Get_Test_Name (Result)) & ":");
      end if;

      Iter := First_Failure (Result);
      Failure_Loop:
      loop
         exit Failure_Loop when not Is_Valid (Iter);
         Print_Test (Data (Iter), Level, "FAIL");
         if Length (Get_Output_File (Data (Iter))) > 0 then
            Print_Log_File (To_String (Get_Output_File (Data (Iter))));
         end if;
         Iter := Next (Iter);
      end loop Failure_Loop;

      Child_Iter := First_Child (Result);
      loop
         exit when not Is_Valid (Child_Iter);
         if Failure_Count (Data (Child_Iter).all) > 0 then
            Print_Failures (Data (Child_Iter).all, Level + 1);
         end if;
         Child_Iter := Next (Child_Iter);
      end loop;
   end Print_Failures;

   procedure Print_Errors (Result : Result_Collection;
                           Level  : Natural) is
      Iter       : Result_Info_Iterator;
      Child_Iter : Result_Collection_Iterator;
   begin
      if Length (Get_Test_Name (Result)) > 0 then
         Pad (Level);
         Put_Line (To_String (Get_Test_Name (Result)) & ":");
      end if;

      Iter := First_Error (Result);
      Error_Loop:
      loop
         exit Error_Loop when not Is_Valid (Iter);
         Print_Test (Data (Iter), Level, "ERROR");
         if Length (Get_Output_File (Data (Iter))) > 0 then
            Print_Log_File (To_String (Get_Output_File (Data (Iter))));
         end if;
         Iter := Next (Iter);
      end loop Error_Loop;

      Child_Iter := First_Child (Result);
      loop
         exit when not Is_Valid (Child_Iter);
         if Error_Count (Data (Child_Iter).all) > 0 then
            Print_Errors (Data (Child_Iter).all, Level + 1);
         end if;
         Child_Iter := Next (Child_Iter);
      end loop;

   end Print_Errors;

   procedure Print_Passes (Result : Result_Collection;
                           Level  : Natural) is
      Iter       : Result_Info_Iterator;
      Child_Iter : Result_Collection_Iterator;
   begin
      if Length (Get_Test_Name (Result)) > 0 then
         Pad (Level);
         Put_Line (To_String (Get_Test_Name (Result)) & ":");
      end if;

      Iter := First_Pass (Result);
      Pass_Loop:
      loop
         exit Pass_Loop when not Is_Valid (Iter);
         Print_Test (Data (Iter), Level, "PASS");
         Iter := Next (Iter);
      end loop Pass_Loop;

      Child_Iter := First_Child (Result);
      loop
         exit when not Is_Valid (Child_Iter);
         if Pass_Count (Data (Child_Iter).all) > 0 then
            Print_Passes (Data (Child_Iter).all, Level + 1);
         end if;
         Child_Iter := Next (Child_Iter);
      end loop;
   end Print_Passes;

   procedure Report_Results (Result  : Result_Collection;
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
      Char   : Character := ' ';
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
      if not First then
         Put_Line ("====================");
      end if;
   end Print_Log_File;

   procedure Do_Report (Test_Results : Results.Result_Collection;
                        Args         : Parameters.Parameter_Info) is
   begin
      if Parameters.XML_Results (Args) then
         XML_Runner.Report_Results
           (Test_Results, Parameters.Result_Dir (Args));
      else
         Report_Results (Test_Results, Parameters.Verbose (Args));
      end if;
   end Do_Report;

   procedure Run (Suite : Framework.Test_Suite'Class) is
   begin
      Runner.Run_Suite (Suite, Do_Report'Access);
   end Run;

   procedure Run (Suite : Framework.Test_Suite_Access) is
   begin
      Run (Suite.all);
   end Run;
end Ahven.Text_Runner;
