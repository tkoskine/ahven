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

with Ahven.Results;

use Ada.Text_IO;
use Ada.Strings.Unbounded;

package body Ahven.Runner is

   procedure Run (R : in out Test_Runner) is
      P : Results.Result_Place;
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
   end Run;

end Ahven.Runner;
