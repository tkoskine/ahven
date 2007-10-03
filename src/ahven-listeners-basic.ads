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

with Ahven.Results;

use Ahven.Results;

package Ahven.Listeners.Basic is
   type Basic_Listener is new Result_Listener with record
      Main_Result    : Result_Collection;
      Current_Result : Result_Collection_Access;
   end record;

   type Basic_Listener_Access is access all Basic_Listener;

   procedure Add_Pass (Listener : in out Basic_Listener;
                       Place    :        Result_Place);

   procedure Add_Failure (Listener : in out Basic_Listener;
                          Place    :        Result_Place);

   procedure Add_Error (Listener : in out Basic_Listener;
                        Place    :        Result_Place);

   procedure Start_Test (Listener : in out Basic_Listener;
                         Place    :        Result_Place);

   procedure End_Test (Listener : in out Basic_Listener;
                       Place    :        Result_Place);

private

end Ahven.Listeners.Basic;
