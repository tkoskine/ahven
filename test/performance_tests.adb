--
-- Copyright (c) 2008-2010 Tero Koskinen <tero.koskinen@iki.fi>
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

with Simple_Listener;

use Ahven;

package body Performance_Tests is
   procedure Assert_Eq_Nat is
     new Ahven.Assert_Equal (Data_Type => Natural,
                             Image     => Natural'Image);

   procedure Initialize (T : in out Test) is
      use Ahven.Framework;
   begin
      Set_Name (T, "Run_Tests_X_Times");

      Add_Test_Routine (T, Test_100K_Tests'Access, "Test 100K tests");
      Add_Test_Routine (T, Test_1M_Tests'Access, "Test 1M tests");
      Add_Test_Routine (T, Test_10M_Tests'Access, "Test 10M tests");
      Add_Test_Routine (T, Test_100M_Tests'Access, "Test 100M tests");
   end Initialize;

   procedure Dummy_Test_Routine is
   begin
      null;
   end Dummy_Test_Routine;

   type T is new Ahven.Framework.Test_Case with record
      null;
   end record;

   procedure Run_Tests_X_Times (X : Natural) is
      use Ahven.Framework;

      My_Test : T;
      My_Listener : Simple_Listener.Listener;
   begin
      My_Listener.Passes := 0;

      for I in Natural range 1 .. X loop
         Add_Test_Routine
           (My_Test, Dummy_Test_Routine'Access, "passed test");
      end loop;
      Ahven.Framework.Run (Ahven.Framework.Test_Case (My_Test), My_Listener);
      Assert_Eq_Nat (Actual   => My_Listener.Start_Calls,
                     Expected => X,
                     Message  => "start calls");
      Assert_Eq_Nat (Actual   => My_Listener.End_Calls,
                     Expected => X,
                     Message  => "end calls");
      Assert_Eq_Nat (Actual   => My_Listener.Passes,
                     Expected => X,
                     Message  => "all passed");
   end Run_Tests_X_Times;

   procedure Test_100K_Tests is
   begin
      Run_Tests_X_Times (X => 100_000); --## rule line off STYLE
   end Test_100K_Tests;

   function Get_Test_Suite return Ahven.Framework.Test_Suite is
      S : Framework.Test_Suite := Framework.Create_Suite ("Performance");

      Perf_Test : Test;
   begin
      Framework.Add_Static_Test (S, Perf_Test);
      return S;
   end Get_Test_Suite;

   procedure Test_1M_Tests is
   begin
      Run_Tests_X_Times (X => 1_000_000); --## rule line off STYLE
   end Test_1M_Tests;

   procedure Test_10M_Tests is
   begin
      Run_Tests_X_Times (X => 10_000_000); --## rule line off STYLE
   end Test_10M_Tests;

   procedure Test_100M_Tests is
   begin
      Skip ("Requires too much memory");
      Run_Tests_X_Times (X => 100_000_000); --## rule line off STYLE
   end Test_100M_Tests;
end Performance_Tests;
