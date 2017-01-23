--
-- Copyright (c) 2008-2015 Tero Koskinen <tero.koskinen@iki.fi>
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

with Ahven.AStrings;

use Ada.Command_Line;
use Ada.Text_IO;

package body Ahven.Parameters is
   type Parser_State is
     (NONE, DIR_NEXT, TIMEOUT_NEXT, SUFFIX_NEXT, IGNORE_REST);

   -- Possible options:
   --  -c : capture output
   --  -d : result directory
   --  -q : quiet mode
   --  -s : test class suffix in XML files
   --  -t : timeout
   --  -v : verbose mode (default)
   --  -x : XML output
   --
   procedure Parse_Options (Info     :  in out Parameter_Info;
                            Mode     :         Parameter_Mode;
                            Option   :         String;
                            State    :     out Parser_State) is
      procedure Check_Invalid (C : Character) is
      begin
         case Mode is
            when NORMAL_PARAMETERS =>
               if C = 'n' then
                  raise Invalid_Parameter;
               end if;
            when TAP_PARAMETERS =>
               if (C = 'd') or (C = 'x') or (C = 's') then
                  raise Invalid_Parameter;
               end if;
         end case;
      end Check_Invalid;
   begin
      State := NONE;

      Option_Loop:
      for A in Option'Range loop
         Check_Invalid (Option (A));
         case Option (A) is
            when 'c' =>
               Info.Capture_Output := True;
            when 'd' =>
               State := DIR_NEXT;
            when 't' =>
               State := TIMEOUT_NEXT;
            when 'v' =>
               Info.Verbose_Output := True;
            when 'q' =>
               Info.Verbose_Output := False;
            when 'x' =>
               Info.Xml_Output := True;
            when 's' =>
               State := SUFFIX_NEXT;
            when 'i' =>
               State := IGNORE_REST;
               exit Option_Loop;
            when others =>
               raise Invalid_Parameter;
         end case;
      end loop Option_Loop;
   end Parse_Options;

   -- Recognize command line parameters.
   -- Option "--" can be used to separate options and test names.
   --
   procedure Parse_Parameters (Mode :     Parameter_Mode;
                               Info : out Parameter_Info) is

      State : Parser_State := NONE;
      Files_Only  : Boolean := False;

      procedure Handle_Parameter (P     : in out Parameter_Info;
                                  Arg   :        String;
                                  Index :        Positive)
      -- Parse one parameter and update P if necessary.
      is
      begin
         if (State = IGNORE_REST) and (Arg = "--") then
            State := NONE;
            Files_Only := True;
         elsif State = IGNORE_REST then
            null; -- Do nothing
         elsif State = DIR_NEXT then
            P.Result_Dir := Index;
            State := NONE;
         elsif State = TIMEOUT_NEXT then
            P.Timeout := Framework.Test_Duration'Value (Arg);
            State := NONE;
         elsif State = SUFFIX_NEXT then
            P.Test_Suffix := Index;
            State := NONE;
         elsif Arg = "--" then
            Files_Only := True;
         elsif Arg'Size > 1 then
            if (not Files_Only) and (Arg (Arg'First) = '-') then
               Parse_Options
                 (Info   => P,
                  Mode   => Mode,
                  Option => Arg (Arg'First + 1 .. Arg'Last),
                  State  => State);
            else
               Name_List.Append (P.Test_Names,
                 AStrings.To_Bounded_String
                   (Ada.Command_Line.Argument (Index)));
            end if;
         end if;
      end Handle_Parameter;
   begin
      -- Default values
      Info := (Verbose_Output => True,
               Xml_Output     => False,
               Capture_Output => False,
               Test_Names     => Name_List.Empty_List,
               Result_Dir     => 0,
               Test_Suffix    => 0,
               Timeout        => 0.0);
      for A in Positive range 1 .. Argument_Count loop
         Handle_Parameter (Info, Argument (A), A);
      end loop;
      if (State /= NONE) and (State /= IGNORE_REST) then
         raise Invalid_Parameter;
      end if;
   end Parse_Parameters;

   procedure Usage (Mode : Parameter_Mode := NORMAL_PARAMETERS) is
   begin
      case Mode is
         when NORMAL_PARAMETERS =>
            Put_Line
              ("Possible parameters: [-cqvxi] [-t timeout ] " &
               "[-s suffix] " &
               "[-d directory] [--]" &
               " [testname] .. [testname]");
            Put_Line ("   -d    : directory for test results");
            Put_Line ("   -x    : output in XML format");
            Put_Line ("   -s    : Test name suffix in XML files");
         when TAP_PARAMETERS =>
            Put_Line ("Possible parameters: [-cqvi] [-t timeout] [--] " &
               "[testname] .. [testname]");
      end case;
      Put_Line ("   -c    : capture and report test outputs");
      Put_Line ("   -q    : quiet results");
      Put_Line ("   -t    : test timeout, infinite(0) default");
      Put_Line ("   -v    : verbose results (default)");
      Put_Line ("   -i    : ignore remaining parameters up to ""--""");
      Put_Line ("   --    : test names follow (optional)");
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
      return (Name_List.Length (Info.Test_Names) /= 0);
   end Single_Test;

   function Test_Name (Info : Parameter_Info) return String is
      use Ahven.AStrings;
   begin
      if Name_List.Length (Info.Test_Names) = 0 then
         return "";
      else
         return To_String (Name_List.Data (Name_List.First (Info.Test_Names)));
      end if;
   end Test_Name;

   function Test_Names (Info : Parameter_Info)
     return Ahven.Name_List.List is
   begin
      return Info.Test_Names;
   end Test_Names;

   function Result_Dir (Info : Parameter_Info) return String is
   begin
      if Info.Result_Dir = 0 then
         return "";
      else
         return Argument (Info.Result_Dir);
      end if;
   end Result_Dir;

   function Timeout (Info : Parameter_Info) return Framework.Test_Duration is
   begin
      return Info.Timeout;
   end Timeout;

   function Test_Class_Suffix (Info : Parameter_Info) return String is
   begin
      if Info.Test_Suffix = 0 then
         return "";
      else
         return Argument (Info.Test_Suffix);
      end if;
   end Test_Class_Suffix;
end Ahven.Parameters;
