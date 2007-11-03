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

with Ada.Exceptions;

with Ahven.Results;

package body Ahven.Runner is

   procedure Run (T      : Ahven.Framework.Test_Class_Access;
                  Result : in out Ahven.Framework.Test_Result) is
      P : Results.Result_Place;
   begin
      Results.Set_Test_Name (P, Framework.Name (T.all));
      begin
         Framework.Execute (T.all, Result);
      exception
         when E : Assertion_Error =>
            Results.Set_Message (P, Ada.Exceptions.Exception_Message (E));
            Framework.Add_Failure (Result, P);
         when E : others =>
            Results.Set_Message (P, Ada.Exceptions.Exception_Name (E));
            Framework.Add_Error (Result, P);
      end;
   end Run;

   procedure Run (T         : Ahven.Framework.Test_Class_Access;
                  Test_Name : String;
                  Result    : in out Ahven.Framework.Test_Result) is
      P : Results.Result_Place;
   begin
      Results.Set_Test_Name (P, Framework.Name (T.all));
      begin
         Framework.Execute (T.all, Test_Name, Result);
      exception
         when E : Assertion_Error =>
            Results.Set_Message (P, Ada.Exceptions.Exception_Message (E));
            Framework.Add_Failure (Result, P);
         when E : others =>
            Results.Set_Message (P, Ada.Exceptions.Exception_Name (E));
            Framework.Add_Error (Result, P);
      end;
   end Run;

end Ahven.Runner;
