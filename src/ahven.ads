-- Ahven Unit Test Library
--
-- Copyright (c) 2007-2009 Tero Koskinen <tero.koskinen@iki.fi>
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
-- SPDX-License-Identifier: ISC
--

package Ahven is
   Max_String_Len : constant := 160;
   Max_Long_String_Len : constant := 1024;
   Count_Max : constant := (2**31) - 1;

   Assertion_Error : exception;
   -- Exception, raised when Assert fails.

   Test_Skipped_Error : exception;
   -- Exception, raised when test is skipped

   subtype Test_Count_Type is Long_Integer range 0 .. Count_Max;
   -- Type for the test count. Long_Integer might be bigger
   -- on some platforms, but the upper limit is something
   -- what most compilers support.
   --
   -- In practice, when adding tests the limit is not explicitly checked.

   procedure Assert (Condition : Boolean; Message : String);
   -- If Condition is false, Assert raises Assertion_Error
   -- with given Message.

   generic
      type Data_Type is private;
      with function Image (Item : Data_Type) return String is <>;
   procedure Assert_Equal (Actual   : Data_Type;
                           Expected : Data_Type;
                           Message  : String);
   -- If Expected /= Actual, Assert raises Assertion_Error
   -- with given Message + represenation of expected and acutal values

   procedure Fail (Message : String);
   -- Fail always raises Assertion_Error with given Message.

   procedure Skip (Message : String);
   -- Skip always raises Test_Skipped_Error with given Message.
end Ahven;
