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
with List_Tests;
with Derived_Tests;

use Ahven;

procedure Tester is
   S : Framework.Test_Suite_Access := Framework.Create_Suite ("All");
begin
   Framework.Add_Test (S.all, new Framework_Tests.Test);
   Framework.Add_Test (S.all, new Derived_Tests.Test);
   Framework.Add_Test (S.all, new List_Tests.Test_Case);
   Text_Runner.Run (S);
   Framework.Release_Suite (S);
end Tester;
