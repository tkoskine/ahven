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

package Framework_Tests is
   type Test is new Ahven.Framework.Test_Case with record
      Value : Integer := -1;
   end record;

   procedure Initialize (T : in out Test);

   procedure Set_Up (T : in out Test);

   procedure Tear_Down (T : in out Test);

   procedure Test_Set_Up (T : in out Ahven.Framework.Test_Case'Class);

   procedure Test_Tear_Down;

   procedure Test_Test_Result_Add_Pass;

   procedure Test_Test_Result_Add_Failure;

   procedure Test_Test_Result_Add_Error;

   procedure Test_Test_Case_Run;

   procedure Test_Test_Suite_Run;

   procedure Test_Add_Listener_Null;

   procedure Test_Call_End_Test;

   procedure Test_Test_Suite_Inside_Suite;

end Framework_Tests;
