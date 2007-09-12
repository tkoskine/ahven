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

with Ada.Unchecked_Deallocation;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

use Ada.Text_IO;
use Ada.Strings.Unbounded;

package body Ahven.Runner is

   procedure Finalize (R : in out Test_Runner) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Framework.Test'Class, Framework.Test_Class_Access);
   begin
      -- Free (R.Suite);
      null;
   end Finalize;

   procedure Run (R : in out Test_Runner) is
      use Framework.Result_List;

      P : Framework.Result_Place;
      Iter : Iterator;
   begin
      P.Test_Name := Framework.Name (R.Suite.all);
      begin
         Framework.Execute (R.Suite, R.Result);
      exception
         when Assertion_Error =>
            Framework.Add_Failure (R.Result, P);
         when others =>
            Framework.Add_Error (R.Result, P);
      end;

      Iter := First (R.Result.Failure_Results);

      Put ("Passed: ");
      Put_Line (Integer'Image (Size (R.Result.Pass_Results)));
      Put ("Failed: ");
      Put_Line (Integer'Image (Size (R.Result.Failure_Results)));
      loop
         exit when Iter = null;
         Put (" " & To_String (Data (Iter).Test_Name) & " : ");
         Put_Line (To_String (Data (Iter).Routine_Name));
         Iter := Next (Iter);
      end loop;
      Put ("Errors: ");
      Put_Line (Integer'Image (Size (R.Result.Error_Results)));
      Iter := First (R.Result.Error_Results);
      loop
         exit when Iter = null;
         Put (" " & To_String (Data (Iter).Test_Name) & " : ");
         Put_Line (To_String (Data (Iter).Routine_Name));
         Iter := Next (Iter);
      end loop;
   end Run;

end Ahven.Runner;
