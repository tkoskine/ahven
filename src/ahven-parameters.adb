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
   procedure Parse_Options (Info : in out Parameter_Info; Option : String);

   -- Possible options:
   --  -c : capture output
   --  -q : quiet mode
   --  -v : verbose mode (default)
   --
   procedure Parse_Options (Info : in out Parameter_Info; Option : String) is
   begin
      for A in Option'Range loop
         case Option (A) is
            when 'c' =>
               Info.Capture_Output := True;
            when 'v' =>
               Info.Verbose_Output := True;
            when 'q' =>
               Info.Verbose_Output := False;
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

      procedure Handle_Parameter (P : in out Parameter_Info; Arg : String) is
      begin
         if Arg = "--" then
            Files_Only := True;
         elsif Arg'Size > 1 then
            if Files_Only = False and Arg (Arg'First) = '-' then
               Parse_Options (P, Arg (Arg'First + 1 .. Arg'Last));
            else
               P.Test_Name := To_Unbounded_String (Arg);
            end if;
         end if;
      end Handle_Parameter;
   begin
      -- Default values: verbose mode, no capture
      Info := (Verbose_Output => True,
               Capture_Output => False,
               Test_Name => Null_Unbounded_String);
      for A in Natural range 1 .. Argument_Count loop
         Handle_Parameter (Info, Argument (A));
      end loop;
   end Parse_Parameters;

   procedure Usage is
   begin
      Put_Line ("Possible parameters: [-cqv] [--] [testname]");
      Put_Line ("   -c    : capture and report test outputs");
      Put_Line ("   -v    : verbose results (default)");
      Put_Line ("   -q    : quiet results");
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

   function Single_Test (Info : Parameter_Info) return Boolean is
   begin
      return not (Length (Info.Test_Name) = 0);
   end Single_Test;

   function Test_Name (Info : Parameter_Info) return String is
   begin
      return To_String (Info.Test_Name);
   end Test_Name;
end Ahven.Parameters;
