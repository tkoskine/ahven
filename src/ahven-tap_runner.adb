--
-- Copyright (c) 2008 Tero Koskinen <tero.koskinen@iki.fi>
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

with Ahven.Tap_Parameters;

use Ada.Text_IO;
use Ada.Strings.Unbounded;
use Ada.Strings.Fixed;

use Ahven;

package body Ahven.Tap_Runner is
   use Ahven.Framework;

   -- Local procedures
   procedure Print_Data (Message : String; Prefix : String);

   procedure Print_Info (Info : Result_Info);

   procedure Print_Info_13 (Info : Result_Info; Severity : String);

   procedure Print_Log_File (Filename : String; Prefix : String);

   procedure Report_Not_Ok (Listener : in out Tap_Listener;
                            Info     :        Result_Info;
                            Severity :        String);

   procedure Print_Data (Message : String; Prefix : String) is
      Start_Of_Line : Boolean := True;
   begin
      for I in Message'Range loop
         if Start_Of_Line then
            Put(Prefix);
            Start_Of_Line := False;
         end if;
         Put (Message (I));
         if Message (I) = Ada.Characters.Latin_1.LF then
            New_Line;
            Start_Of_Line := True;
         end if;
      end loop;
   end Print_Data;

   procedure Run (Suite : in out Framework.Test_Suite'Class) is
      Listener : Tap_Listener;
      Params   : Tap_Parameters.Parameter_Info;
   begin
      Tap_Parameters.Parse_Parameters (Params);

      if Tap_Parameters.Use_Tap_13 (Params) then
         Put_Line ("TAP version 13");
      end if;
      Listener.Tap_13 := Tap_Parameters.Use_Tap_13 (Params);
      Listener.Verbose := Tap_Parameters.Verbose (Params);
      Listener.Capture_Output := Tap_Parameters.Capture (Params);

      if Tap_Parameters.Single_Test (Params) then
         Put_Line ("1.." & Ada.Strings.Fixed.Trim
           (Test_Count_Type'Image (Test_Count
             (Suite, Tap_Parameters.Test_Name (Params))), Ada.Strings.Both));
         Framework.Execute
           (Suite, Tap_Parameters.Test_Name (Params), Listener);
      else
         Put_Line ("1.." & Ada.Strings.Fixed.Trim
           (Test_Count_Type'Image(Test_Count (Suite)), Ada.Strings.Both));
         Framework.Execute (Suite, Listener);
      end if;
   exception
      when Tap_Parameters.Invalid_Parameter =>
         Tap_Parameters.Usage;
   end Run;

   procedure Print_Info (Info : Result_Info) is
   begin
      Print_Data (Message => Get_Message (Info), Prefix => "# ");
      New_Line;
      if Get_Long_Message (Info)'Length > 0 then
         Print_Data (Message => Get_Long_Message (Info), Prefix => "# ");
         New_Line;
      end if;
   end Print_Info;

   procedure Print_Info_13 (Info : Result_Info; Severity : String) is
   begin
      Put_Line ("  ---");
      Print_Data (Message => "message: " & '"' & Get_Message (Info) & '"',
                  Prefix => "  ");
      New_Line;
      Print_Data (Message => "severity: " & Severity, Prefix => "  ");
      New_Line;
      if Get_Long_Message (Info)'Length > 0 then
         Print_Data (Message => "long_message: " & Get_Long_Message (Info),
                     Prefix => "  ");
         New_Line;
      end if;
      Put_Line ("  ...");
   end Print_Info_13;

   procedure Print_Log_File (Filename : String; Prefix : String) is
      Handle : File_Type;
      Char   : Character := ' ';
      First  : Boolean := True;
      Start_Of_Line : Boolean := True;
   begin
      Open (Handle, In_File, Filename);
      loop
         exit when End_Of_File (Handle);
         Get (Handle, Char);
         if First then
            Put_Line (Prefix & "===== Output =======");
            First := False;
         end if;
         if Start_Of_Line then
            Put(Prefix);
            Start_Of_Line := False;
         end if;
         Put (Char);
         if End_Of_Line (Handle) then
            New_Line;
            Start_Of_Line := True;
         end if;
      end loop;
      Close (Handle);
      if not First then
         -- New_Line;
         Put_Line (Prefix & "====================");
      end if;
   exception
      when Name_Error =>
         -- Missing output file is ok.
         Put_Line (Prefix & "no output");
   end Print_Log_File;

   procedure Add_Pass (Listener : in out Tap_Listener;
                       Info     :        Result_Info) is
      use Ada.Strings;
   begin
      if Listener.Capture_Output then
         Temporary_Output.Restore_Output;
         Temporary_Output.Close_Temp (Listener.Output_File);
      end if;

      Put ("ok ");
      Put (Trim (Test_Count_Type'Image (Listener.Current_Test), Both) & " ");
      Put (To_String (Get_Test_Name (Info) & ": " & Get_Routine_Name (Info)));
      New_Line;
   end Add_Pass;

   procedure Report_Not_Ok (Listener : in out Tap_Listener;
                            Info     :        Result_Info;
                            Severity :        String) is
      use Ada.Strings;
   begin
      if Listener.Capture_Output then
         Temporary_Output.Restore_Output;
         Temporary_Output.Close_Temp (Listener.Output_File);
      end if;

      Put ("not ok ");
      Put (Trim
        (Test_Count_Type'Image (Listener.Current_Test), Both) & " ");
      Put (To_String
        (Get_Test_Name (Info) & ": " & Get_Routine_Name (Info)));
      New_Line;

      if Listener.Verbose then
         if Listener.Tap_13 then
            Print_Info_13 (Info, Severity);
            -- For version 1.3 we do not support log file yet.
         else
            Print_Info (Info);
            if Listener.Capture_Output then
               Print_Log_File
                 (Filename => Temporary_Output.Get_Name (Listener.Output_File),
                  Prefix   => "# ");
            end if;
         end if;
      end if;
   end Report_Not_Ok;

   procedure Add_Failure (Listener : in out Tap_Listener;
                          Info     :        Result_Info) is
   begin
      Report_Not_Ok (Listener, Info, "fail");
   end Add_Failure;

   procedure Add_Error (Listener : in out Tap_Listener;
                        Info     :        Result_Info) is
   begin
      Report_Not_Ok (Listener, Info, "error");
   end Add_Error;

   procedure Start_Test (Listener : in out Tap_Listener;
                         Info     :        Result_Info) is
   begin
      if Length (Get_Routine_Name (Info)) > 0 then
         Listener.Current_Test := Listener.Current_Test + 1;
         if Listener.Capture_Output then
            Temporary_Output.Create_Temp (Listener.Output_File);
            Temporary_Output.Redirect_Output (Listener.Output_File);
         end if;
      end if;
   end Start_Test;

   procedure End_Test (Listener : in out Tap_Listener;
                       Info     :        Result_Info) is
      Handle : Ada.Text_IO.File_Type;
   begin
      if Listener.Capture_Output then
         Ada.Text_IO.Open (Handle, Ada.Text_IO.Out_File,
                           Temporary_Output.Get_Name (Listener.Output_File));
         Ada.Text_IO.Delete (Handle);
      end if;
   exception
      when Name_Error =>
         -- Missing file is safe to ignore, we are going to delete it anyway
         null;
   end End_Test;
end Ahven.Tap_Runner;