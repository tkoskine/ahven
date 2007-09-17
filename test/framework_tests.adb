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

with Ahven.Framework;

package body Framework_Tests is
   use Ahven;
   
   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Framework tests");
      Ahven.Framework.Register_Routine (T, Test_Set_Up'Access, "Test Set_Up");
   end Initialize;
   
   procedure Set_Up (T : in out Test) is
   begin
      T.Value := 2;
   end Set_Up;

   procedure Tear_Down (T : in out Test) is
   begin
      T.Value := -3;
   end Tear_Down;
   
   procedure Test_Set_Up (T : in out Ahven.Framework.Test_Case'Class) is
   begin
      Assert (Test (T).Value = 2, "Set_Up not called!");
   end Test_Set_Up;

end Framework_Tests;
