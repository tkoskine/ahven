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

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package body Ahven.Listeners.Basic is

   procedure Add_Pass (Listener : in out Basic_Listener;
                       Place : Result_Place) is
   begin
      Add_Pass (Listener.Current_Result.all, Place);
   end Add_Pass;

   procedure Add_Failure (Listener : in out Basic_Listener;
                          Place : Result_Place) is
   begin
      Add_Failure (Listener.Current_Result.all, Place);
   end Add_Failure;

   procedure Add_Error (Listener : in out Basic_Listener;
                        Place : Result_Place) is
   begin
      Add_Error (Listener.Current_Result.all, Place);
   end Add_Error;

   procedure Start_Test (Listener : in out Basic_Listener;
                         Place : Result_Place) is
      R : Result_Collection_Access := null;
   begin
      if Routine_Name (Place) = Null_Unbounded_String then
         R := new Result_Collection;
         Set_Name (R.all, Test_Name (Place));
         if Listener.Current_Result = null then
            Add_Child (Listener.Main_Result, R);
            Listener.Current_Result := R;
         else
            Add_Child (Listener.Current_Result.all, R);
         end if;
         Set_Parent (R.all, Listener.Current_Result);
         Listener.Current_Result := R;
      end if;
   end Start_Test;

   procedure End_Test (Listener : in out Basic_Listener;
                       Place : Result_Place) is
      pragma Unreferenced (Place);
   begin
      if Listener.Current_Result /= null then
         Listener.Current_Result := Parent (Listener.Current_Result.all);
      end if;
   end End_Test;

end Ahven.Listeners.Basic;
