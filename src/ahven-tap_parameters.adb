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

package body Ahven.Tap_Parameters is
   procedure Parse_Options (Info : in out Parameter_Info; Option : String);

   -- Possible options:
   --  -c : capture output
   --  -n : new protocol version (use 1.3 instead of 1.2)
   --  -q : quiet mode
   --  -v : verbose mode (default)
   --
   procedure Parse_Options (Info : in out Parameter_Info; Option : String) is
   begin
      for A in Option'Range loop
         case Option (A) is
            when 'c' =>
               Info.Capture_Output := True;
            when 'n' =>
               Info.Tap_13 := True;
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
      procedure Handle_Parameter (P     : in out Parameter_Info;
                                  Arg   :        String;
                                  Index :        Positive);
      -- Parse one parameter and update P if necessary.

      Files_Only : Boolean := False;

      procedure Handle_Parameter (P     : in out Parameter_Info;
                                  Arg   :        String;
                                  Index :        Positive)
      is
      begin
         if Arg = "--" then
            Files_Only := True;
         elsif Arg'Size > 1 then
            if (not Files_Only) and (Arg (Arg'First) = '-') then
               Parse_Options (P, Arg (Arg'First + 1 .. Arg'Last));
            else
               P.Test_Name := Index;
            end if;
         end if;
      end Handle_Parameter;
   begin
      -- Default values
      Info := (Tap_13         => False,
               Verbose_Output => True,
               Capture_Output => False,
               Test_Name      => 0);
      for A in Positive range 1 .. Argument_Count loop
         Handle_Parameter (Info, Argument (A), A);
      end loop;
   end Parse_Parameters;

   procedure Usage is
   begin
      Put_Line ("Possible parameters: [-cnqv] [--] [testname]");
      Put_Line ("   -c    : capture and report test outputs");
      Put_Line ("   -n    : use tap 1.3 instead of tap 1.2");
      Put_Line ("   -q    : quiet results");
      Put_Line ("   -v    : verbose results (default)");
      Put_Line ("   --    : end of parameters (optional)");
   end Usage;

   function Use_Tap_13 (Info : Parameter_Info) return Boolean is
   begin
      return Info.Tap_13;
   end Use_Tap_13;

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
      return (Info.Test_Name /= 0);
   end Single_Test;

   function Test_Name (Info : Parameter_Info) return String is
   begin
      if Info.Test_Name = 0 then
         return "";
      else
         return Argument (Info.Test_Name);
      end if;
   end Test_Name;
end Ahven.Tap_Parameters;
