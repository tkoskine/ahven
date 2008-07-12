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
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

with Ahven.Runner;

with Ahven.Compat;

use Ada.Text_IO;
use Ada.Strings.Unbounded;
use Ada.Strings.Fixed;

package body Ahven.XML_Runner is
   use Ahven.Results;
   use Ahven.Framework;

   function Filter_String (Str : String) return String;

   procedure Print_Test_Pass (File : File_Type;
                              Parent_Test : String;
                              Info : Result_Info);
   procedure Print_Test_Failure (File : File_Type;
                                 Parent_Test : String;
                                 Info : Result_Info);
   procedure Print_Test_Error (File : File_Type;
                               Parent_Test : String;
                               Info : Result_Info);

   procedure Print_Test_Case (Collection : Result_Collection;
                              Dir : String);

   procedure Print_Log_File (File : File_Type; Filename : String);

   procedure Print_Attribute (File : File_Type; Attr : String;
                              Value : String);

   procedure Start_Testcase_Tag (File : File_Type;
                                 Parent : String; Name : String;
                                 Execution_Time : String);

   procedure End_Testcase_Tag (File : File_Type);

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

   procedure Print_Attribute (File : File_Type; Attr : String;
                              Value : String) is
   begin
      Put (File, Attr & "=" & '"' & Value & '"');
   end Print_Attribute;

   procedure Start_Testcase_Tag (File : File_Type;
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
   end Start_Testcase_Tag;

   procedure End_Testcase_Tag (File : File_Type) is
   begin
      Put_Line (File, "</testcase>");
   end End_Testcase_Tag;

   function Create_Name (Dir : String; Name : String) return String
   is
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

   procedure Print_Test_Pass (File : File_Type;
                              Parent_Test : String;
                              Info : Result_Info) is
      Exec_Time : constant String :=
        Trim (Duration'Image (Get_Execution_Time (Info)), Ada.Strings.Both);
   begin
      Start_Testcase_Tag
        (File           => File,
         Parent         => Parent_Test,
         Name           => To_String (Get_Routine_Name (Info)),
         Execution_Time => Exec_Time);

      if Length (Get_Output_File (Info)) > 0 then
         Put (File, "<system-out>");
         Print_Log_File (File, To_String (Get_Output_File (Info)));
         Put_Line (File, "</system-out>");
      end if;
      End_Testcase_Tag (File);
   end Print_Test_Pass;

   procedure Print_Test_Failure (File : File_Type;
                                 Parent_Test : String;
                                 Info : Result_Info) is
      Exec_Time : constant String :=
        Trim (Duration'Image (Get_Execution_Time (Info)), Ada.Strings.Both);
   begin
      Start_Testcase_Tag
        (File           => File,
         Parent         => Parent_Test,
         Name           => To_String (Get_Routine_Name (Info)),
         Execution_Time => Exec_Time);

      Put (File, "<failure ");
      Print_Attribute (File, "type",
        Trim (To_String (Get_Message (Info)), Ada.Strings.Both));
      Put (File, ">");
      Put_Line (File, To_String (Get_Message (Info)));
      Put_Line (File, "</failure>");
      if Length (Get_Output_File (Info)) > 0 then
         Put (File, "<system-out>");
         Print_Log_File (File, To_String (Get_Output_File (Info)));
         Put_Line (File, "</system-out>");
      end if;
      End_Testcase_Tag (File);
   end Print_Test_Failure;

   procedure Print_Test_Error (File : File_Type;
                               Parent_Test : String;
                               Info : Result_Info) is
      Exec_Time : constant String :=
        Trim (Duration'Image (Get_Execution_Time (Info)), Ada.Strings.Both);
   begin
      Start_Testcase_Tag
        (File           => File,
         Parent         => Parent_Test,
         Name           => To_String (Get_Routine_Name (Info)),
         Execution_Time => Exec_Time);

      Put (File, "<error ");
      Print_Attribute (File, "type",
        Trim (To_String (Get_Message (Info)), Ada.Strings.Both));
      Put (File, ">");
      Put_Line (File, To_String (Get_Message (Info)));
      Put_Line (File, "</error>");
      if Length (Get_Output_File (Info)) > 0 then
         Put (File, "<system-out>");
         Print_Log_File (File, To_String (Get_Output_File (Info)));
         Put_Line (File, "</system-out>");
      end if;
      End_Testcase_Tag (File);
   end Print_Test_Error;

   procedure Print_Test_Case (Collection : Result_Collection;
                              Dir : String) is
      procedure Print (Output : File_Type;
                       Result : Result_Collection);
      -- Internal procedure to print the testcase into given file.

      procedure Print (Output : File_Type;
                       Result : Result_Collection) is
         Iter : Result_Info_Iterator;
      begin
         Put_Line (Output, "<?xml version=" & '"' & "1.0" & '"' &
                   " encoding=" & '"' & "iso-8859-1" & '"' &
                   "?>");
         Put (Output, "<testsuite ");
         Print_Attribute (Output, "errors",
           Trim (Integer'Image (Error_Count (Result)), Ada.Strings.Both));
         Put (Output, " ");
         Print_Attribute (Output, "failures",
           Trim (Integer'Image (Failure_Count (Result)), Ada.Strings.Both));
         Put (Output, " ");
         Print_Attribute (Output, "tests",
           Trim (Integer'Image (Test_Count (Result)), Ada.Strings.Both));
         Put (Output, " ");
         Print_Attribute (Output, "time",
           Trim (Duration'Image (Get_Execution_Time (Result)),
                 Ada.Strings.Both));
         Put (Output, " ");
         Print_Attribute (Output,
           "name", To_String (Get_Test_Name (Result)));
         Put_Line (Output, ">");

         Iter := First_Error (Result);
         Error_Loop:
         loop
            exit Error_Loop when not Is_Valid (Iter);
            Print_Test_Error (Output,
              To_String (Get_Test_Name (Result)), Data (Iter));
            Iter := Next (Iter);
         end loop Error_Loop;

         Iter := First_Failure (Result);
         Failure_Loop:
         loop
            exit Failure_Loop when not Is_Valid (Iter);
            Print_Test_Failure (Output,
              To_String (Get_Test_Name (Result)), Data (Iter));
            Iter := Next (Iter);
         end loop Failure_Loop;

         Iter := First_Pass (Result);
         Pass_Loop:
         loop
            exit Pass_Loop when not Is_Valid (Iter);
            Print_Test_Pass (Output,
              To_String (Get_Test_Name (Result)), Data (Iter));
            Iter := Next (Iter);
         end loop Pass_Loop;
         Put_Line (Output, "</testsuite>");
      end Print;

      File : File_Type;
   begin
      if Dir = "-" then
         Print (Standard_Output, Collection);
      else
         Create (File => File, Mode => Ada.Text_IO.Out_File,
           Name => Create_Name (Dir, To_String (Get_Test_Name (Collection))));
         Print (File, Collection);
         Ada.Text_IO.Close (File);
      end if;
   end Print_Test_Case;

   procedure Report_Results (Result : Result_Collection;
                             Dir    : String) is
      Iter : Result_Collection_Iterator;
   begin
      Iter := First_Child (Result);
      loop
         exit when not Is_Valid (Iter);
         if Child_Depth (Data (Iter).all) = 0 then
            Print_Test_Case (Data (Iter).all, Dir);
         else
            Report_Results (Data (Iter).all, Dir);

            -- Handle the test cases in this collection
            if Direct_Test_Count (Result) > 0 then
               Print_Test_Case (Result, Dir);
            end if;
         end if;
         Iter := Next (Iter);
      end loop;
   end Report_Results;

   procedure Print_Log_File (File : File_Type; Filename : String) is
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

   procedure Do_Report (Test_Results : Results.Result_Collection;
                        Args         : Parameters.Parameter_Info) is
   begin
      Report_Results (Test_Results,
                      Parameters.Result_Dir (Args));
   end Do_Report;

   procedure Run (Suite : in out Framework.Test_Suite'Class) is
   begin
      Runner.Run_Suite (Suite, Do_Report'Access);
   end Run;

   procedure Run (Suite : Framework.Test_Suite_Access) is
   begin
      Run (Suite.all);
   end Run;
end Ahven.XML_Runner;
