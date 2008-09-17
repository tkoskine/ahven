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

with Ahven.Text_Runner;
with Ahven.Framework;

with Framework_Tests;
with Derived_Tests;
with Results_Tests;
with Runner_Tests;
with Basic_Listener_Tests;
with Assertion_Tests;

use Ahven;

procedure Tester is
   S : Framework.Test_Suite := Framework.Create_Suite ("All");
begin
   Framework.Add_Test (S, new Assertion_Tests.Test);
   Framework.Add_Test (S, new Framework_Tests.Test);
   Framework.Add_Test (S, new Derived_Tests.Test);
   Framework.Add_Test (S, new Results_Tests.Test);
   Framework.Add_Test (S, new Runner_Tests.Test);
   Framework.Add_Test (S, new Basic_Listener_Tests.Test);
   Text_Runner.Run (S);
end Tester;
