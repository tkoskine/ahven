--
-- Copyright (c) 2017 Tero Koskinen <tero.koskinen@iki.fi>
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
use Ada.Text_IO;

with Ahven; use Ahven;

package body Fail1 is
   type Test_Access is access all Test_Case;

   procedure Initialize (T : in out Test_Case) is
   begin
      Set_Name (T, "Fail1");

      Ahven.Framework.Add_Test_Routine
        (T, Test_Fail'Access, "Fail");
   end Initialize;
   
   procedure Test_Fail is
   begin
      Fail ("DOES NOT WORK");
   end Test_Fail;
end Fail1;
