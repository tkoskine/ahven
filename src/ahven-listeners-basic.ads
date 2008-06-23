--
-- Copyright (c) 2007, 2008 Tero Koskinen <tero.koskinen@iki.fi>
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

with Ada.Strings.Unbounded;

use Ada.Strings.Unbounded;

package Ahven.Listeners.Basic is
   type Result_Type is (NO_RESULT, PASS_RESULT, FAILURE_RESULT, ERROR_RESULT);

   type Basic_Listener is new Result_Listener with record
      Main_Result       : Result_Collection;
      Current_Result    : Result_Collection_Access;
      Last_Test_Result  : Result_Type      := NO_RESULT;
      Last_Test_Message : Unbounded_String := Null_Unbounded_String;
      Last_Test_Long_Message : Unbounded_String := Null_Unbounded_String;
   end record;

   type Basic_Listener_Class_Access is access Basic_Listener'Class;

   function Create return Basic_Listener_Class_Access;

   procedure Add_Pass (Listener : in out Basic_Listener;
                       Info     :        Result_Info);
   -- New implementation for Listeners.Add_Pass

   procedure Add_Failure (Listener : in out Basic_Listener;
                          Info     :        Result_Info);
   -- New implementation for Listeners.Add_Failure

   procedure Add_Error (Listener : in out Basic_Listener;
                        Info     :        Result_Info);
   -- New implementation for Listeners.Add_Error

   procedure Start_Test (Listener : in out Basic_Listener;
                         Info     :        Result_Info);
   -- New implementation for Listeners.Start_Test

   procedure End_Test (Listener : in out Basic_Listener;
                       Info     :        Result_Info);
   -- New implementation for Listeners.End_Test

private
   procedure Set_Last_Test_Info (Listener : in out Basic_Listener;
                                 Info     : Result_Info;
                                 Result   : Result_Type);
end Ahven.Listeners.Basic;
