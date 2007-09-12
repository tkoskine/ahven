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

with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Ahven.Runner;

use Ada.Text_IO;
use Ada.Strings.Unbounded;

package body Ahven.Text_Runner is

   procedure Report_Results (Result : Framework.Test_Result;
                             Verbose : Boolean := False) is
      use Framework.Result_List;

      Iter : Iterator;
      Test_Amount : Integer := 0;
   begin
      Iter := First (Result.Failure_Results);

      Test_Amount := Size (Result.Pass_Results) +
                     Size (Result.Failure_Results) +
                     Size (Result.Error_Results);

      Put ("Tests:  ");
      Put_Line (Integer'Image (Test_Amount));

      if Size (Result.Failure_Results) > 0 then
         Put ("Failed: ");
         Put_Line (Integer'Image (Size (Result.Failure_Results)));
         if Verbose then
            loop
               exit when Iter = null;
               Put (" " & To_String (Data (Iter).Test_Name) & " : ");
               Put_Line (To_String (Data (Iter).Routine_Name));
               Iter := Next (Iter);
            end loop;
         end if;
      end if;

      if Size (Result.Error_Results) > 0 then
         Put ("Errors: ");
         Put_Line (Integer'Image (Size (Result.Error_Results)));
         if Verbose then
            Iter := First (Result.Error_Results);
            loop
               exit when Iter = null;
               Put (" " & To_String (Data (Iter).Test_Name) & " : ");
               Put_Line (To_String (Data (Iter).Routine_Name));
               Iter := Next (Iter);
            end loop;
         end if;
      end if;
   end Report_Results;

   procedure Run (Suite : Framework.Test_Suite_Access) is
      R : Runner.Test_Runner;
   begin
      R.Suite := Framework.Test_Class_Access (Suite);
      Runner.Run (R);
      Report_Results (R.Result, True);
   end Run;

end Ahven.Text_Runner;

