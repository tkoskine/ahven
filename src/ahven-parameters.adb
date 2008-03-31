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

with Ada.Command_Line;
with Ada.Text_IO;

use Ada.Command_Line;
use Ada.Text_IO;

package body Ahven.Parameters is
   procedure Parse_Options (Info : in out Parameter_Info; Option : String;
                            Dir_Next : out Boolean);

   -- Possible options:
   --  -c : capture output
   --  -d : result directory
   --  -q : quiet mode
   --  -v : verbose mode (default)
   --  -x : XML output
   --
   procedure Parse_Options (Info : in out Parameter_Info; Option : String;
                            Dir_Next : out Boolean) is
   begin
      Dir_Next := False;
      for A in Option'Range loop
         case Option (A) is
            when 'c' =>
               Info.Capture_Output := True;
            when 'd' =>
               Dir_Next := True;
            when 'v' =>
               Info.Verbose_Output := True;
            when 'q' =>
               Info.Verbose_Output := False;
            when 'x' =>
               Info.Xml_Output := True;
            when others =>
               raise Invalid_Parameter;
         end case;
      end loop;
   end Parse_Options;

   -- Recognize command line parameters.
   -- Option "--" can be used to separate options and test names.
   --
   procedure Parse_Parameters (Info : out Parameter_Info) is
      procedure Handle_Parameter (P : in out Parameter_Info; Arg : String);
      -- Parse one parameter and update P if necessary.

      Files_Only : Boolean := False;
      Dir_Next   : Boolean := False;

      procedure Handle_Parameter (P : in out Parameter_Info; Arg : String) is
      begin
         if Dir_Next then
            P.Result_Dir := To_Unbounded_String (Arg);
            Dir_Next := False;
         elsif Arg = "--" then
            Files_Only := True;
         elsif Arg'Size > 1 then
            if (not Files_Only) and (Arg (Arg'First) = '-') then
               Parse_Options (P, Arg (Arg'First + 1 .. Arg'Last), Dir_Next);
            else
               P.Test_Name := To_Unbounded_String (Arg);
            end if;
         end if;
      end Handle_Parameter;
   begin
      -- Default values: verbose mode, no xml, no capture
      Info := (Verbose_Output => True,
               Xml_Output     => False,
               Capture_Output => False,
               Test_Name => Null_Unbounded_String,
               Result_Dir => Null_Unbounded_String);
      for A in Natural range 1 .. Argument_Count loop
         Handle_Parameter (Info, Argument (A));
      end loop;
      if Dir_Next then
         raise Invalid_Parameter;
      end if;
   end Parse_Parameters;

   procedure Usage is
   begin
      Put_Line ("Possible parameters: [-cqv] [--] [testname]");
      Put_Line ("   -c    : capture and report test outputs");
      Put_Line ("   -d    : directory for test results");
      Put_Line ("   -q    : quiet results");
      Put_Line ("   -v    : verbose results (default)");
      Put_Line ("   -x    : output in XML format");
      Put_Line ("   --    : end of parameters (optional)");
   end Usage;

   function Capture (Info : Parameter_Info) return Boolean is
   begin
      return Info.Capture_Output;
   end Capture;

   function Verbose (Info : Parameter_Info) return Boolean is
   begin
      return Info.Verbose_Output;
   end Verbose;

   function XML_Results (Info : Parameter_Info) return Boolean is
   begin
      return Info.Xml_Output;
   end XML_Results;

   function Single_Test (Info : Parameter_Info) return Boolean is
   begin
      return (Length (Info.Test_Name) /= 0);
   end Single_Test;

   function Test_Name (Info : Parameter_Info) return String is
   begin
      return To_String (Info.Test_Name);
   end Test_Name;

   function Result_Dir (Info : Parameter_Info) return String is
   begin
      return To_String (Info.Result_Dir);
   end Result_Dir;
end Ahven.Parameters;
