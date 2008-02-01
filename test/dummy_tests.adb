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

with Ahven;

package body Dummy_Tests is
   procedure Initialize (T : in out Test) is
      procedure Register (T : in out Ahven.Framework.Test_Case'Class;
                          Routine : Ahven.Framework.Simple_Test_Routine_Access;
                          Name    : String)
        renames Ahven.Framework.Add_Test_Routine;
   begin
      Register (T, This_Test_Fails'Access, "Failure");
      Register (T, This_Test_Passes'Access, "Pass");
      Register (T, This_Test_Raises_Error'Access, "Error");
   end Initialize;

   procedure This_Test_Fails is
   begin
      Ahven.Fail ("Failure");
   end This_Test_Fails;

   procedure This_Test_Passes is
   begin
      Ahven.Assert (True, "True was not true!");
   end This_Test_Passes;

   procedure This_Test_Raises_Error is
   begin
      raise Constraint_Error;
   end This_Test_Raises_Error;

end Dummy_Tests;
