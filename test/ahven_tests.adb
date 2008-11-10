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

with Ahven.Text_Runner;
with Ahven.Framework;

with Framework_Tests;
with Derived_Tests;
with Results_Tests;
with Basic_Listener_Tests;
with Assertion_Tests;
with Static_Test_Case_Tests;

use Ahven;

package body Ahven_Tests is
   --## rule off DIRECTLY_ACCESSED_GLOBALS
   --## rule off Removable
   Static_Test    : Static_Test_Case_Tests.Test;
   Assertion_Test : Assertion_Tests.Test;
   Framework_Test : Framework_Tests.Test;
   Derived_Test   : Derived_Tests.Test;
   Results_Test   : Results_Tests.Test;
   Listener_Test  : Basic_Listener_Tests.Test;

   procedure Run_Tests is
      S : Framework.Test_Suite := Framework.Create_Suite ("All");
   begin
      Framework.Add_Static_Test (S, Assertion_Test);
      Framework.Add_Static_Test (S, Framework_Test);
      Framework.Add_Static_Test (S, Derived_Test);
      Framework.Add_Static_Test (S, Results_Test);
      Framework.Add_Static_Test (S, Listener_Test);
      Framework.Add_Static_Test (S, Static_Test);
      Text_Runner.Run (S);
   end Run_Tests;
end Ahven_Tests;
