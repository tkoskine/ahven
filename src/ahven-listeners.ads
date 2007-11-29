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

with Ada.Finalization;
with Ahven.Doubly_Linked_List;
with Ahven.Results;

use Ahven.Results;

package Ahven.Listeners is
   type Result_Listener is
     abstract new Ada.Finalization.Limited_Controlled with null record;
   -- Result_Listener is a listener for test results.
   -- Whenever a test is run, the framework calls
   -- registered listeners and tells them the result of the test.

   type Result_Listener_Access is access Result_Listener;
   type Result_Listener_Class_Access is access all Result_Listener'Class;

   procedure Add_Pass (Listener : in out Result_Listener;
                       Info     :        Result_Info) is abstract;
   -- Called after test passes.

   procedure Add_Failure (Listener : in out Result_Listener;
                          Info     :        Result_Info) is abstract;
   -- Called after test fails.

   procedure Add_Error (Listener : in out Result_Listener;
                        Info     :        Result_Info) is abstract;
   -- Called after there is an error in the test.

   procedure Start_Test (Listener : in out Result_Listener;
                         Info     :        Result_Info) is abstract;
   -- Called before the test begins.

   procedure End_Test (Listener : in out Result_Listener;
                       Info     :        Result_Info) is abstract;
   -- Called after the test ends. Add_* procedures are called before this.

   package Result_Listener_List is
     new Ahven.Doubly_Linked_List (Result_Listener_Class_Access);
   -- A package for Result_Listener list.

end Ahven.Listeners;
