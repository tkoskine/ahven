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
-- SPDX-License-Identifier: ISC
--

with Ahven.Framework;
with Ahven.Name_List;

-- A package for handling command line parameters
--
-- Parameters are handled for normal and TAP test runners
-- and the used mode is specified by giving either
-- NORMAL_PARAMETERS or TAP_PARAMETERS parameter to
-- Parse_Parameters procedure.
package Ahven.Parameters is

   Invalid_Parameter : exception;

   type Parameter_Info is private;

   type Parameter_Mode is (NORMAL_PARAMETERS, TAP_PARAMETERS);

   procedure Parse_Parameters (Mode :     Parameter_Mode;
                               Info : out Parameter_Info);
   -- Parse Ada.Command_Line parameters and put the results
   -- to the Info parameter. Raises Invalid_Parameter if
   -- some parameter is invalid.

   procedure Usage (Mode : Parameter_Mode := NORMAL_PARAMETERS);
   -- Print usage.

   function Capture (Info : Parameter_Info) return Boolean;
   -- Capture Ada.Text_IO output?

   function Verbose (Info : Parameter_Info) return Boolean;
   -- Use verbose mode?

   function XML_Results (Info : Parameter_Info) return Boolean;
   -- Output XML?

   function Single_Test (Info : Parameter_Info) return Boolean;
   -- Run a single test (case/suite/routine) only?

   function Test_Name (Info : Parameter_Info) return String;
   -- Return the name of the test passed as a parameter.

   function Test_Names (Info : Parameter_Info)
     return Ahven.Name_List.List;
   -- Return all the test names passed as a parameter

   function Result_Dir (Info : Parameter_Info) return String;
   -- Return the directory for XML results.

   function Timeout (Info : Parameter_Info) return Framework.Test_Duration;
   -- Return the timeout value for a test.

   function Test_Class_Suffix (Info : Parameter_Info) return String;

private
   type Parameter_Info is record
      Verbose_Output : Boolean := True;
      Xml_Output     : Boolean := False;
      Capture_Output : Boolean := False;

      Test_Names     : Name_List.List;
      -- Names of the wanted tests

      Result_Dir     : Natural := 0;
      -- Position of results dir in the argument array

      Test_Suffix    : Natural := 0;
      -- Position of test class name suffix in the argument array

      Timeout        : Framework.Test_Duration := 0.0;
   end record;
end Ahven.Parameters;
