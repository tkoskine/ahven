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

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

with Ahven.Runner;
with Ahven.Results;
with Ahven.Listeners.Output_Capture;
with Ahven.Listeners.Basic;
with Ahven.Framework;
with Ahven.Listeners;
with Ahven.Parameters;

with Ahven.Compat;

use Ada.Text_IO;
use Ada.Strings.Unbounded;
use Ada.Strings.Fixed;

use Ahven.Results;
use Ahven.Framework;
use Ahven.Listeners.Output_Capture;
use Ahven.Listeners.Basic;

package body Ahven.XML_Runner is
   function Filter_String (Str : String) return String;

   procedure Print_Test_Pass (File : in out File_Type;
                              Parent_Test : String;
                              Info : Result_Info);
   procedure Print_Test_Failure (File : in out File_Type;
                                 Parent_Test : String;
                                 Info : Result_Info);
   procedure Print_Test_Error (File : in out File_Type;
                               Parent_Test : String;
                               Info : Result_Info);

   procedure Print_Test_Case (Result : in out Result_Collection;
                              Dir : String);

   procedure Report_Results (Result : in out Result_Collection;
                             Dir    : String);

   procedure Print_Log_File (File : in out File_Type; Filename : String);

   procedure Print_Attribute (File : in out File_Type; Attr : String;
                              Value : String);

   procedure Print_Testcase_Tag (File : in out File_Type;
                                 Parent : String; Name : String;
                                 Execution_Time : String);

   function Create_Name (Dir : String; Name : String) return String;

   function Filter_String (Str : String) return String is
      Result : String (Str'Range);
   begin
      for I in Str'Range loop
         if Str (I) = ''' then
            Result (I) := '_';
         else
            Result (I) := Str (I);
         end if;
      end loop;

      return Result;
   end Filter_String;

   procedure Print_Attribute (File : in out File_Type; Attr : String;
                              Value : String) is
   begin
      Put (File, Attr & "=" & '"' & Value & '"');
   end Print_Attribute;

   procedure Print_Testcase_Tag (File : in out File_Type;
                                 Parent : String; Name : String;
                                 Execution_Time : String) is
   begin
      Put (File, "<testcase ");
      Print_Attribute (File, "classname", Filter_String (Parent));
      Put (File, " ");
      Print_Attribute (File, "name", Filter_String (Name));
      Put (File, " ");
      Print_Attribute (File, "time", Filter_String (Execution_Time));
      Put_Line (File, ">");
   end Print_Testcase_Tag;

   function Create_Name (Dir : String; Name : String) return String is
      function Filename (Test : String) return String;
      -- Create a filename for the test.

      function Filename (Test : String) return String is
      begin
         return "TEST-" & Filter_String (Test) & ".xml";
      end Filename;
   begin
      if Dir'Length > 0 then
         return Dir & Ahven.Compat.Directory_Separator & Filename (Name);
      else
         return Filename (Name);
      end if;
   end Create_Name;

   procedure Print_Test_Pass (File : in out File_Type;
                              Parent_Test : String;
                              Info : Result_Info) is
   begin
      Print_Testcase_Tag (File, Parent_Test,
        To_String (Get_Routine_Name (Info)),
        Trim (Duration'Image (Get_Execution_Time (Info)), Ada.Strings.Both));
      if Length (Get_Output_File (Info)) > 0 then
         Put (File, "<system-out>");
         Print_Log_File (File, To_String (Get_Output_File (Info)));
         Put_Line (File, "</system-out>");
      end if;
      Put_Line (File, "</testcase>");
   end Print_Test_Pass;

   procedure Print_Test_Failure (File : in out File_Type;
                                 Parent_Test : String;
                                 Info : Result_Info) is
   begin
      Print_Testcase_Tag (File, Parent_Test,
        To_String (Get_Routine_Name (Info)),
        Trim (Duration'Image (Get_Execution_Time (Info)), Ada.Strings.Both));
      if Length (Get_Output_File (Info)) > 0 then
         Put (File, "<system-out>");
         Print_Log_File (File, To_String (Get_Output_File (Info)));
         Put_Line (File, "</system-out>");
      end if;
      Put_Line (File, "</testcase>");
   end Print_Test_Failure;

   procedure Print_Test_Error (File : in out File_Type;
                               Parent_Test : String;
                               Info : Result_Info) is
   begin
      Print_Testcase_Tag (File, Parent_Test,
        To_String (Get_Routine_Name (Info)),
        Trim (Duration'Image (Get_Execution_Time (Info)), Ada.Strings.Both));
      if Length (Get_Output_File (Info)) > 0 then
         Put (File, "<system-out>");
         Print_Log_File (File, To_String (Get_Output_File (Info)));
         Put_Line (File, "</system-out>");
      end if;
      Put_Line (File, "</testcase>");
   end Print_Test_Error;

   procedure Print_Test_Case (Result : in out Result_Collection;
                              Dir : String) is
      End_Flag : Boolean;
      Info     : Result_Info;
      File     : Ada.Text_IO.File_Type;
   begin
      Create (File => File, Mode => Ada.Text_IO.Out_File,
              Name => Create_Name (Dir, To_String (Get_Test_Name (Result))));

      Put_Line (File, "<?xml version=" & '"' & "1.0" & '"' &
                " encoding=" & '"' & "iso-8859-1" & '"' &
                "?>");
      Put (File, "<testsuite ");
      Print_Attribute (File, "errors",
           Trim (Integer'Image (Error_Count (Result)), Ada.Strings.Both));
      Put (File, " ");
      Print_Attribute (File, "failures",
           Trim (Integer'Image (Failure_Count (Result)), Ada.Strings.Both));
      Put (File, " ");
      Print_Attribute (File, "tests",
           Trim (Integer'Image (Test_Count (Result)), Ada.Strings.Both));
      Put (File, " ");
      Print_Attribute (File, "time", "0.0001");
      Put (File, " ");
      Print_Attribute (File, "name", To_String (Get_Test_Name (Result)));
      Put_Line (File, ">");

      Error_Loop:
      loop
         Next_Error (Result, Info, End_Flag);
         exit Error_Loop when End_Flag;
         Print_Test_Error (File, To_String (Get_Test_Name (Result)), Info);
      end loop Error_Loop;

      Failure_Loop:
      loop
         Next_Failure (Result, Info, End_Flag);
         exit Failure_Loop when End_Flag;
         Print_Test_Failure (File, To_String (Get_Test_Name (Result)), Info);
      end loop Failure_Loop;

      Pass_Loop:
      loop
         Next_Pass (Result, Info, End_Flag);
         exit Pass_Loop when End_Flag;
         Print_Test_Pass (File, To_String (Get_Test_Name (Result)), Info);
      end loop Pass_Loop;

      Put_Line (File, "</testsuite>");
      Ada.Text_IO.Close (File);
   end Print_Test_Case;

   procedure Report_Results (Result : in out Result_Collection;
                             Dir    : String) is
      Child : Result_Collection_Access := null;
      End_Flag : Boolean;
   begin
      loop
         Next_Child (Result, Child, End_Flag);
         exit when End_Flag;
         if Child_Depth (Child.all) = 0 then
            Print_Test_Case (Child.all, Dir);
         else
            Report_Results (Child.all, Dir);

            -- Handle the test cases in this collection
            if Direct_Test_Count (Result) > 0 then
               Print_Test_Case (Result, Dir);
            end if;
         end if;
      end loop;
   end Report_Results;

   procedure Print_Log_File (File : in out File_Type; Filename : String) is
      Handle : File_Type;
      Char   : Character := ' ';
      First  : Boolean := True;
   begin
      Open (Handle, In_File, Filename);
      loop
         exit when End_Of_File (Handle);
         Get (Handle, Char);
         if First then
            Put (File, "<![CDATA[");
            First := False;
         end if;
         Put (File, Char);
         if End_Of_Line (Handle) then
            New_Line (File);
         end if;
      end loop;
      Close (Handle);
      if not First then
         Put_Line (File, "]]>");
      end if;
   end Print_Log_File;

   procedure Run (Suite : Framework.Test_Suite'Class) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Listeners.Result_Listener'Class,
         Listeners.Result_Listener_Class_Access);

      Result   : Test_Result;
      Listener : Listeners.Result_Listener_Class_Access;
      Params   : Parameters.Parameter_Info;
   begin
      Parameters.Parse_Parameters (Params);
      if Parameters.Capture (Params) then
         Listener := Listeners.Output_Capture.Create;
      else
         Listener := Listeners.Basic.Create;
      end if;

      Add_Listener (Result, Listener);
      if Parameters.Single_Test (Params) then
         Runner.Run (Suite, Parameters.Test_Name (Params), Result);
      else
         Runner.Run (Suite, Result);
      end if;

      Report_Results (Basic_Listener (Listener.all).Main_Result,
                      Parameters.Result_Dir (Params));
      if Error_Count (Basic_Listener (Listener.all).Main_Result) > 0 or
         Failure_Count (Basic_Listener (Listener.all).Main_Result) > 0 then
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;
      Free (Listener);
   exception
      when Parameters.Invalid_Parameter =>
         Parameters.Usage;
   end Run;

   procedure Run (Suite : Framework.Test_Suite_Access) is
   begin
      Run (Suite.all);
   end Run;
end Ahven.XML_Runner;
